use rustc_abi::{Align, BackendRepr, Size};
use rustc_codegen_c_ast::expr::{CExpr, CValue};
use rustc_codegen_c_ast::ty::{CFloatTy, CIntTy, CTy, CUintTy};
use rustc_codegen_ssa::common::IntPredicate;
use rustc_codegen_ssa::mir::operand::OperandRef;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    BuilderMethods, ConstCodegenMethods, IntrinsicCallBuilderMethods, LayoutTypeCodegenMethods,
    OverflowOp,
};
use rustc_middle::ty::layout::{LayoutOf, TyAndLayout};
use rustc_middle::ty::{GenericArgsRef, Instance};
use rustc_span::sym;

use crate::builder::Builder;

impl<'tcx, 'mx> Builder<'_, 'tcx, 'mx> {
    fn materialize_typed_expr(&mut self, ty: CTy<'mx>, expr: CExpr<'mx>) -> CValue<'mx> {
        let value = self.bb.func.0.next_local_var();
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(value, ty, Some(expr))));
        self.record_value_ty(value, ty);
        value
    }

    fn casted_integer_literal(&mut self, ty: CTy<'mx>, literal: &'static str) -> CValue<'mx> {
        let expr = self.mcx.cast(ty, self.mcx.raw(literal));
        self.materialize_typed_expr(ty, expr)
    }

    fn saturating_integer_bounds(&mut self, ty: CTy<'mx>) -> (CValue<'mx>, CValue<'mx>) {
        match ty {
            CTy::UInt(_) => {
                let min = self.casted_integer_literal(ty, "0");
                let max = self.casted_integer_literal(ty, ty.max_value());
                (min, max)
            }
            CTy::Int(_) => {
                let max = self.casted_integer_literal(ty, ty.max_value());
                let neg_max = self.mcx.unary("-", self.mcx.value(max));
                let one = self.mcx.cast(ty, self.mcx.raw("1"));
                let min = self.materialize_typed_expr(ty, self.mcx.binary(neg_max, one, "-"));
                (min, max)
            }
            _ => panic!("saturating intrinsic expects integer backend type, got {ty:?}"),
        }
    }

    fn unary_float_intrinsic_spec(
        &self,
        name: rustc_span::Symbol,
    ) -> Option<(&'static str, CFloatTy)> {
        Some(match name {
            sym::fabsf32 => ("__builtin_fabsf", CFloatTy::F32),
            sym::fabsf64 => ("__builtin_fabs", CFloatTy::F64),
            sym::sqrtf32 => ("__builtin_sqrtf", CFloatTy::F32),
            sym::sqrtf64 => ("__builtin_sqrt", CFloatTy::F64),
            sym::floorf32 => ("__builtin_floorf", CFloatTy::F32),
            sym::floorf64 => ("__builtin_floor", CFloatTy::F64),
            sym::ceilf32 => ("__builtin_ceilf", CFloatTy::F32),
            sym::ceilf64 => ("__builtin_ceil", CFloatTy::F64),
            sym::truncf32 => ("__builtin_truncf", CFloatTy::F32),
            sym::truncf64 => ("__builtin_trunc", CFloatTy::F64),
            sym::round_ties_even_f32 => ("__builtin_rintf", CFloatTy::F32),
            sym::round_ties_even_f64 => ("__builtin_rint", CFloatTy::F64),
            _ => return None,
        })
    }

    fn codegen_unary_float_intrinsic(
        &mut self,
        builtin_name: &'static str,
        ret_float_ty: CFloatTy,
        arg: CValue<'mx>,
        llresult: PlaceRef<'tcx, CValue<'mx>>,
    ) {
        let ret_ty = CTy::Float(ret_float_ty);
        let arg_expr = match self.value_ty(arg) {
            Some(arg_ty) if arg_ty != ret_ty => self.mcx.cast(ret_ty, self.mcx.value(arg)),
            _ => self.mcx.value(arg),
        };
        let call = self.mcx.call(self.mcx.raw(builtin_name), vec![arg_expr]);
        let out = self.materialize_typed_expr(ret_ty, call);
        self.store(out, llresult.val.llval, llresult.val.align);
    }

    fn raw_eq_use_fast_path(&self, layout: TyAndLayout<'tcx>, pointer_size: u64) -> bool {
        match layout.backend_repr {
            BackendRepr::Scalar(_) | BackendRepr::ScalarPair(_, _) => true,
            BackendRepr::Memory { .. } => layout.size.bytes() <= pointer_size * 2,
            BackendRepr::SimdVector { .. } => false,
        }
    }

    fn pick_chunk(remaining: u64, align_bytes: u64, offset: u64) -> u64 {
        for chunk in [8, 4, 2, 1] {
            if chunk <= remaining && align_bytes % chunk == 0 && offset % chunk == 0 {
                return chunk;
            }
        }
        1
    }

    fn raw_eq_fast_compare_chunks(
        &mut self,
        lhs_ptr: CValue<'mx>,
        rhs_ptr: CValue<'mx>,
        size_bytes: u64,
        align: Align,
    ) -> CValue<'mx> {
        if size_bytes == 0 {
            return self.const_bool(true);
        }

        let mut offset = 0u64;
        let mut all_equal = None;
        while offset < size_bytes {
            let remaining = size_bytes - offset;
            let chunk = Self::pick_chunk(remaining, align.bytes() as u64, offset);
            let chunk_ty = match chunk {
                8 => CTy::UInt(CUintTy::U64),
                4 => CTy::UInt(CUintTy::U32),
                2 => CTy::UInt(CUintTy::U16),
                1 => CTy::UInt(CUintTy::U8),
                _ => unreachable!(),
            };

            let lhs_chunk_ptr = if offset == 0 {
                lhs_ptr
            } else {
                self.inbounds_ptradd(lhs_ptr, self.const_usize(offset))
            };
            let rhs_chunk_ptr = if offset == 0 {
                rhs_ptr
            } else {
                self.inbounds_ptradd(rhs_ptr, self.const_usize(offset))
            };
            let chunk_align = align.restrict_for_offset(Size::from_bytes(offset));
            let lhs_chunk = self.load(chunk_ty, lhs_chunk_ptr, chunk_align);
            let rhs_chunk = self.load(chunk_ty, rhs_chunk_ptr, chunk_align);
            let eq = self.icmp(IntPredicate::IntEQ, lhs_chunk, rhs_chunk);

            all_equal = Some(match all_equal {
                Some(prev) => self.and(prev, eq),
                None => eq,
            });
            offset += chunk;
        }

        all_equal.unwrap_or_else(|| self.const_bool(true))
    }

    fn raw_eq_memcmp(
        &mut self,
        lhs_ptr: CValue<'mx>,
        rhs_ptr: CValue<'mx>,
        size_bytes: u64,
    ) -> CValue<'mx> {
        self.ensure_alloca_byte_array_decl(lhs_ptr);
        self.ensure_alloca_byte_array_decl(rhs_ptr);

        let void_ptr = self.pointer_to(CTy::Void);
        let lhs_ptr = self.mcx.cast(void_ptr, self.mcx.value(lhs_ptr));
        let rhs_ptr = self.mcx.cast(void_ptr, self.mcx.value(rhs_ptr));
        let size = self.mcx.value(self.const_usize(size_bytes));
        let cmp = self.mcx.call(self.mcx.raw("__builtin_memcmp"), vec![lhs_ptr, rhs_ptr, size]);
        let cmp = self.mcx.cast(CTy::Int(CIntTy::I32), cmp);
        let cmp_val = self.bb.func.0.next_local_var();
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(
            cmp_val,
            CTy::Int(CIntTy::I32),
            Some(cmp),
        )));
        self.record_value_ty(cmp_val, CTy::Int(CIntTy::I32));
        self.icmp(IntPredicate::IntEQ, cmp_val, self.const_i32(0))
    }

    fn simd_int_binop_op(name: rustc_span::Symbol) -> Option<&'static str> {
        Some(match name {
            sym::simd_add => "+",
            sym::simd_sub => "-",
            sym::simd_mul => "*",
            sym::simd_shl => "<<",
            sym::simd_shr => ">>",
            sym::simd_and => "&",
            sym::simd_or => "|",
            sym::simd_xor => "^",
            _ => return None,
        })
    }

    fn codegen_simd_int_binop(
        &mut self,
        name: rustc_span::Symbol,
        fn_args: GenericArgsRef<'tcx>,
        args: &[OperandRef<'tcx, CValue<'mx>>],
        llresult: PlaceRef<'tcx, CValue<'mx>>,
        span: rustc_span::Span,
    ) {
        let Some(op) = Self::simd_int_binop_op(name) else {
            panic!("expected SIMD integer binop intrinsic, got {name}");
        };

        let lhs_ty = fn_args.type_at(0);
        let lhs_layout = self.layout_of(lhs_ty);
        let ret_layout = llresult.layout;
        if !matches!(lhs_layout.backend_repr, BackendRepr::SimdVector { .. }) {
            self.tcx.sess.dcx().span_fatal(
                span,
                format!(
                    "invalid monomorphization of `{name}` intrinsic: expected SIMD input type, found non-SIMD `{lhs_ty:?}`"
                ),
            );
        }
        if !matches!(ret_layout.backend_repr, BackendRepr::SimdVector { .. }) {
            self.tcx.sess.dcx().span_fatal(
                span,
                format!(
                    "invalid monomorphization of `{name}` intrinsic: expected SIMD return type, found `{}`",
                    ret_layout.ty
                ),
            );
        }
        if ret_layout.ty != lhs_ty {
            self.tcx.sess.dcx().span_fatal(
                span,
                format!(
                    "invalid monomorphization of `{name}` intrinsic: arg={lhs_ty:?}, ret={:?}",
                    ret_layout.ty
                ),
            );
        }

        // First SIMD integer binop batch supports only Int/Uint lanes.
        let (_lanes, lane_ty) = lhs_ty.simd_size_and_type(self.tcx);
        if !matches!(
            lane_ty.kind(),
            rustc_type_ir::TyKind::Int(_) | rustc_type_ir::TyKind::Uint(_)
        ) {
            self.tcx.sess.dcx().span_fatal(
                span,
                format!(
                    "`{name}` intrinsic currently supports only Int/Uint SIMD lanes in rustc_codegen_c, got `{lane_ty}`"
                ),
            );
        }

        let vec_backend_ty = self.cx.immediate_backend_type(lhs_layout);
        let lhs = args[0].immediate();
        let rhs = args[1].immediate();
        let lhs_expr = match self.value_ty(lhs) {
            Some(ty) if ty != vec_backend_ty => self.mcx.cast(vec_backend_ty, self.mcx.value(lhs)),
            _ => self.mcx.value(lhs),
        };
        let rhs_expr = match self.value_ty(rhs) {
            Some(ty) if ty != vec_backend_ty => self.mcx.cast(vec_backend_ty, self.mcx.value(rhs)),
            _ => self.mcx.value(rhs),
        };

        let out = self.materialize_typed_expr(vec_backend_ty, self.mcx.binary(lhs_expr, rhs_expr, op));
        self.store(out, llresult.val.llval, llresult.val.align);
    }
}

impl<'tcx, 'mx> IntrinsicCallBuilderMethods<'tcx> for Builder<'_, 'tcx, 'mx> {
    fn codegen_intrinsic_call(
        &mut self,
        instance: Instance<'tcx>,
        args: &[OperandRef<'tcx, Self::Value>],
        llresult: PlaceRef<'tcx, Self::Value>,
        span: rustc_span::Span,
    ) -> Result<(), Instance<'tcx>> {
        let _ = span;
        let name = self.tcx.item_name(instance.def_id());
        let fn_args = instance.args;

        match name {
            sym::copy_nonoverlapping => {
                let elem_ty = fn_args.type_at(0);
                let elem_layout = self.layout_of(elem_ty);
                let src = args[0].immediate();
                let dst = args[1].immediate();
                let count = args[2].immediate();
                let size = self.mul(count, self.const_usize(elem_layout.size.bytes()));
                self.memcpy(
                    dst,
                    elem_layout.align.abi,
                    src,
                    elem_layout.align.abi,
                    size,
                    rustc_codegen_ssa::MemFlags::empty(),
                );
                Ok(())
            }
            sym::volatile_copy_nonoverlapping_memory => {
                let elem_ty = fn_args.type_at(0);
                let elem_layout = self.layout_of(elem_ty);
                let dst = args[0].immediate();
                let src = args[1].immediate();
                let count = args[2].immediate();
                let size = self.mul(count, self.const_usize(elem_layout.size.bytes()));
                self.memcpy(
                    dst,
                    elem_layout.align.abi,
                    src,
                    elem_layout.align.abi,
                    size,
                    rustc_codegen_ssa::MemFlags::VOLATILE,
                );
                Ok(())
            }
            sym::volatile_copy_memory => {
                let elem_ty = fn_args.type_at(0);
                let elem_layout = self.layout_of(elem_ty);
                let dst = args[0].immediate();
                let src = args[1].immediate();
                let count = args[2].immediate();
                let size = self.mul(count, self.const_usize(elem_layout.size.bytes()));
                self.memmove(
                    dst,
                    elem_layout.align.abi,
                    src,
                    elem_layout.align.abi,
                    size,
                    rustc_codegen_ssa::MemFlags::VOLATILE,
                );
                Ok(())
            }
            sym::write_bytes => {
                let elem_ty = fn_args.type_at(0);
                let elem_layout = self.layout_of(elem_ty);
                let dst = args[0].immediate();
                let fill = args[1].immediate();
                let count = args[2].immediate();
                let size = self.mul(count, self.const_usize(elem_layout.size.bytes()));
                self.memset(
                    dst,
                    fill,
                    size,
                    elem_layout.align.abi,
                    rustc_codegen_ssa::MemFlags::empty(),
                );
                Ok(())
            }
            sym::volatile_set_memory => {
                let elem_ty = fn_args.type_at(0);
                let elem_layout = self.layout_of(elem_ty);
                let dst = args[0].immediate();
                let fill = args[1].immediate();
                let count = args[2].immediate();
                let size = self.mul(count, self.const_usize(elem_layout.size.bytes()));
                self.memset(
                    dst,
                    fill,
                    size,
                    elem_layout.align.abi,
                    rustc_codegen_ssa::MemFlags::VOLATILE,
                );
                Ok(())
            }
            sym::black_box => {
                args[0].val.store(self, llresult);

                let ptr =
                    self.mcx.cast(self.pointer_to(CTy::Void), self.mcx.value(llresult.val.llval));
                let size = self.const_usize(llresult.layout.size.bytes());
                let observe = self.mcx.call(
                    self.mcx.raw("__rust_black_box_observe"),
                    vec![ptr, self.mcx.value(size)],
                );
                self.bb.func.0.push_stmt(self.mcx.expr_stmt(observe));
                Ok(())
            }
            name if self.unary_float_intrinsic_spec(name).is_some() => {
                let (builtin_name, expected_float_ty) = self.unary_float_intrinsic_spec(name).unwrap();
                let arg = args[0].immediate();
                self.codegen_unary_float_intrinsic(builtin_name, expected_float_ty, arg, llresult);
                Ok(())
            }
            sym::ctpop => {
                let ret_ty = self.cx.immediate_backend_type(llresult.layout);
                let (_, bits, _) = self
                    .integer_shape(ret_ty)
                    .unwrap_or_else(|| panic!("unsupported ctpop return type: {ret_ty:?}"));

                let arg = args[0].immediate();
                let (builtin_name, builtin_arg_ty) = if bits <= 32 {
                    ("__builtin_popcount", CTy::UInt(CUintTy::U32))
                } else if bits <= 64 {
                    ("__builtin_popcountll", CTy::UInt(CUintTy::U64))
                } else if bits <= 128 {
                    ("__rust_popcount_u128", CTy::UInt(CUintTy::U128))
                } else {
                    panic!("unsupported ctpop width: {bits}");
                };

                let arg_expr = self.mcx.cast(builtin_arg_ty, self.mcx.value(arg));
                let popcnt = self.mcx.call(self.mcx.raw(builtin_name), vec![arg_expr]);
                let res = self.bb.func.0.next_local_var();
                let init = self.mcx.cast(ret_ty, popcnt);
                self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(res, ret_ty, Some(init))));
                self.record_value_ty(res, ret_ty);
                self.store(res, llresult.val.llval, llresult.val.align);
                Ok(())
            }
            sym::saturating_add | sym::saturating_sub => {
                let int_ty = fn_args.type_at(0);
                let backend_ty = match int_ty.kind() {
                    rustc_type_ir::TyKind::Int(int) => self.mcx.get_int_type(*int),
                    rustc_type_ir::TyKind::Uint(uint) => self.mcx.get_uint_type(*uint),
                    _ => self.tcx.sess.dcx().span_fatal(
                        span,
                        format!("{name} intrinsic expects an integer type, got {int_ty:?}"),
                    ),
                };

                let lhs = args[0].immediate();
                let rhs = args[1].immediate();
                let overflow_op = match name {
                    sym::saturating_add => OverflowOp::Add,
                    sym::saturating_sub => OverflowOp::Sub,
                    _ => unreachable!(),
                };
                let (val, has_overflow) = self.checked_binop(overflow_op, int_ty, lhs, rhs);
                let (min, max) = self.saturating_integer_bounds(backend_ty);

                let saturated = match backend_ty {
                    CTy::UInt(_) => match name {
                        sym::saturating_add => self.select(has_overflow, max, val),
                        sym::saturating_sub => self.select(has_overflow, min, val),
                        _ => unreachable!(),
                    },
                    CTy::Int(_) => {
                        let zero = self.casted_integer_literal(backend_ty, "0");
                        let rhs_ge_zero = self.icmp(IntPredicate::IntSGE, rhs, zero);
                        let sat_on_overflow = match name {
                            sym::saturating_add => self.select(rhs_ge_zero, max, min),
                            sym::saturating_sub => self.select(rhs_ge_zero, min, max),
                            _ => unreachable!(),
                        };
                        self.select(has_overflow, sat_on_overflow, val)
                    }
                    _ => unreachable!(),
                };

                self.store(saturated, llresult.val.llval, llresult.val.align);
                Ok(())
            }
            name if Self::simd_int_binop_op(name).is_some() => {
                self.codegen_simd_int_binop(name, fn_args, args, llresult, span);
                Ok(())
            }
            sym::compare_bytes => {
                let a_ptr = args[0].immediate();
                let b_ptr = args[1].immediate();
                let size = args[2].immediate();

                self.ensure_alloca_byte_array_decl(a_ptr);
                self.ensure_alloca_byte_array_decl(b_ptr);

                let void_ptr = self.pointer_to(CTy::Void);
                let a_ptr = self.mcx.cast(void_ptr, self.mcx.value(a_ptr));
                let b_ptr = self.mcx.cast(void_ptr, self.mcx.value(b_ptr));
                let size = match self.value_ty(size) {
                    Some(CTy::UInt(CUintTy::Usize)) => self.mcx.value(size),
                    _ => self.mcx.cast(CTy::UInt(CUintTy::Usize), self.mcx.value(size)),
                };

                let cmp = self.mcx.call(self.mcx.raw("__builtin_memcmp"), vec![a_ptr, b_ptr, size]);
                let ret_ty = CTy::Int(CIntTy::I32);
                let ret = self.bb.func.0.next_local_var();
                let cmp = self.mcx.cast(ret_ty, cmp);
                self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ret_ty, Some(cmp))));
                self.record_value_ty(ret, ret_ty);
                self.store(ret, llresult.val.llval, llresult.val.align);
                Ok(())
            }
            sym::raw_eq => {
                let pointee_ty = fn_args.type_at(0);
                let layout = self.layout_of(pointee_ty);
                if !layout.is_sized() {
                    self.tcx.sess.dcx().span_fatal(span, "raw_eq requires a sized pointee type");
                }

                let lhs_ptr = args[0].immediate();
                let rhs_ptr = args[1].immediate();
                let size_bytes = layout.size.bytes();

                let is_equal = if size_bytes == 0 {
                    self.const_bool(true)
                } else {
                    let pointer_size = self.tcx.data_layout.pointer_size().bytes();
                    if self.raw_eq_use_fast_path(layout, pointer_size) {
                        self.raw_eq_fast_compare_chunks(
                            lhs_ptr,
                            rhs_ptr,
                            size_bytes,
                            layout.align.abi,
                        )
                    } else {
                        self.raw_eq_memcmp(lhs_ptr, rhs_ptr, size_bytes)
                    }
                };

                self.store(is_equal, llresult.val.llval, llresult.val.align);
                Ok(())
            }
            sym::catch_unwind => {
                // With panic=abort there is no unwind path. Execute the body and report success.
                let try_fn = args[0].immediate();
                let data = args[1].immediate();
                let _catch_fn = args[2].immediate();

                let call = self.mcx.call(self.mcx.value(try_fn), vec![self.mcx.value(data)]);
                self.bb.func.0.push_stmt(self.mcx.expr_stmt(call));
                self.store(self.const_i32(0), llresult.val.llval, llresult.val.align);
                Ok(())
            }
            _ => Err(instance),
        }
    }

    fn abort(&mut self) {
        self.unreachable();
    }

    fn assume(&mut self, val: Self::Value) {
        let _ = val;
    }

    fn expect(&mut self, cond: Self::Value, expected: bool) -> Self::Value {
        let _ = expected;
        cond
    }

    fn type_checked_load(
        &mut self,
        llvtable: Self::Value,
        vtable_byte_offset: u64,
        typeid: Self::Value,
    ) -> Self::Value {
        todo!()
    }

    fn va_start(&mut self, val: Self::Value) -> Self::Value {
        val
    }

    fn va_end(&mut self, val: Self::Value) -> Self::Value {
        val
    }
}
