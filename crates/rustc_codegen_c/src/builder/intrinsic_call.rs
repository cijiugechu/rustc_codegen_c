use rustc_codegen_ssa::mir::operand::OperandRef;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_c_ast::ty::{CTy, CUintTy};
use rustc_codegen_ssa::traits::{
    BuilderMethods, ConstCodegenMethods, IntrinsicCallBuilderMethods, LayoutTypeCodegenMethods,
};
use rustc_middle::ty::Instance;
use rustc_middle::ty::layout::LayoutOf;
use rustc_span::sym;

use crate::builder::Builder;

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
