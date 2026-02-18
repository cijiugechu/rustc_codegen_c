use rustc_abi::{Align, BackendRepr, Size};
use rustc_codegen_c_ast::expr::CValue;
use rustc_codegen_c_ast::ty::{CIntTy, CTy, CUintTy};
use rustc_codegen_ssa::common::IntPredicate;
use rustc_codegen_ssa::mir::operand::OperandRef;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    BuilderMethods, ConstCodegenMethods, IntrinsicCallBuilderMethods, LayoutTypeCodegenMethods,
};
use rustc_middle::ty::layout::{LayoutOf, TyAndLayout};
use rustc_middle::ty::Instance;
use rustc_span::sym;

use crate::builder::Builder;

impl<'tcx, 'mx> Builder<'_, 'tcx, 'mx> {
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
        self.bb
            .func
            .0
            .push_stmt(self.mcx.decl_stmt(self.mcx.var(cmp_val, CTy::Int(CIntTy::I32), Some(cmp))));
        self.record_value_ty(cmp_val, CTy::Int(CIntTy::I32));
        self.icmp(IntPredicate::IntEQ, cmp_val, self.const_i32(0))
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
                    self.tcx
                        .sess
                        .dcx()
                        .span_fatal(span, "raw_eq requires a sized pointee type");
                }

                let lhs_ptr = args[0].immediate();
                let rhs_ptr = args[1].immediate();
                let size_bytes = layout.size.bytes();

                let is_equal = if size_bytes == 0 {
                    self.const_bool(true)
                } else {
                    let pointer_size = self.tcx.data_layout.pointer_size().bytes();
                    if self.raw_eq_use_fast_path(layout, pointer_size) {
                        self.raw_eq_fast_compare_chunks(lhs_ptr, rhs_ptr, size_bytes, layout.align.abi)
                    } else {
                        self.raw_eq_memcmp(lhs_ptr, rhs_ptr, size_bytes)
                    }
                };

                self.store(is_equal, llresult.val.llval, llresult.val.align);
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
