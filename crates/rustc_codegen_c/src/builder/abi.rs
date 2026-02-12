use rustc_abi::BackendRepr;
use rustc_codegen_c_ast::expr::CValue;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    AbiBuilderMethods, ArgAbiBuilderMethods, BuilderMethods, ConstCodegenMethods,
};
use rustc_middle::ty::Ty;
use rustc_target::callconv::{ArgAbi, PassMode};

use crate::builder::Builder;

impl<'tcx, 'mx> AbiBuilderMethods for Builder<'_, 'tcx, 'mx> {
    fn get_param(&mut self, index: usize) -> Self::Value {
        // Params are first n variables in the function
        CValue::Local(index)
    }
}

impl<'tcx, 'mx> ArgAbiBuilderMethods<'tcx> for Builder<'_, 'tcx, 'mx> {
    fn store_fn_arg(
        &mut self,
        arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        idx: &mut usize,
        dst: PlaceRef<'tcx, Self::Value>,
    ) {
        match arg_abi.mode {
            PassMode::Ignore => {}
            PassMode::Direct(_) => {
                let val = self.get_param(*idx);
                *idx += 1;
                self.store_arg(arg_abi, val, dst);
            }
            PassMode::Pair(_, _) => {
                let a = self.get_param(*idx);
                let b = self.get_param(*idx + 1);
                *idx += 2;

                let (a_scalar, b_scalar) = match dst.layout.backend_repr {
                    BackendRepr::ScalarPair(a, b) => (a, b),
                    _ => {
                        panic!("pair arg with non-scalar-pair destination layout: {:?}", dst.layout)
                    }
                };
                let b_offset = a_scalar.size(self).align_to(b_scalar.align(self).abi);

                self.store(a, dst.val.llval, dst.val.align);
                let second_ptr =
                    self.inbounds_ptradd(dst.val.llval, self.const_usize(b_offset.bytes()));
                self.store(b, second_ptr, dst.val.align.restrict_for_offset(b_offset));
            }
            PassMode::Cast { .. } | PassMode::Indirect { .. } => {
                panic!("store_fn_arg does not support pass mode {:?} yet", arg_abi.mode)
            }
        }
    }

    fn store_arg(
        &mut self,
        arg_abi: &ArgAbi<'tcx, Ty<'tcx>>,
        val: Self::Value,
        dst: PlaceRef<'tcx, Self::Value>,
    ) {
        match arg_abi.mode {
            PassMode::Ignore => {}
            PassMode::Direct(_) => {
                self.store(val, dst.val.llval, dst.val.align);
            }
            PassMode::Pair(_, _) => {
                let (a_scalar, b_scalar) = match dst.layout.backend_repr {
                    BackendRepr::ScalarPair(a, b) => (a, b),
                    _ => {
                        panic!("pair arg with non-scalar-pair destination layout: {:?}", dst.layout)
                    }
                };
                let b_offset = a_scalar.size(self).align_to(b_scalar.align(self).abi);

                let first = self.extract_value(val, 0);
                let second = self.extract_value(val, 1);
                self.store(first, dst.val.llval, dst.val.align);
                let second_ptr =
                    self.inbounds_ptradd(dst.val.llval, self.const_usize(b_offset.bytes()));
                self.store(second, second_ptr, dst.val.align.restrict_for_offset(b_offset));
            }
            PassMode::Cast { .. } | PassMode::Indirect { .. } => {
                panic!("store_arg does not support pass mode {:?} yet", arg_abi.mode)
            }
        }
    }
}
