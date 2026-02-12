use rustc_abi::BackendRepr;
use rustc_codegen_c_ast::expr::CValue;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{
    AbiBuilderMethods, ArgAbiBuilderMethods, BuilderMethods, ConstCodegenMethods,
};
use crate::rustc_codegen_ssa::traits::LayoutTypeCodegenMethods;
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
            PassMode::Cast { ref cast, pad_i32 } => {
                if pad_i32 {
                    *idx += 1;
                }
                for (offset, _) in self.cx.cast_target_to_c_abi_pieces(cast) {
                    let piece = self.get_param(*idx);
                    *idx += 1;
                    let piece_ptr = if offset == 0 {
                        dst.val.llval
                    } else {
                        self.inbounds_ptradd(dst.val.llval, self.const_usize(offset as u64))
                    };
                    let piece_align =
                        dst.val.align.restrict_for_offset(rustc_abi::Size::from_bytes(offset as u64));
                    self.store(piece, piece_ptr, piece_align);
                }
            }
            PassMode::Indirect { meta_attrs: None, .. } => {
                let src_ptr = self.get_param(*idx);
                *idx += 1;
                let pointee_ty = self.cx.backend_type(arg_abi.layout);
                let loaded = self.load(pointee_ty, src_ptr, dst.val.align);
                self.store(loaded, dst.val.llval, dst.val.align);
            }
            PassMode::Indirect { meta_attrs: Some(_), .. } => {
                panic!("store_fn_arg does not support unsized indirect arguments yet")
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
            PassMode::Cast { ref cast, pad_i32: _ } => {
                let tuple_ty = self
                    .value_ty(val)
                    .unwrap_or_else(|| panic!("cast return value without type metadata: {val:?}"));
                for (i, (offset, piece_ty)) in
                    self.cx.cast_target_to_c_abi_pieces(cast).into_iter().enumerate()
                {
                    let field = self.cx.abi_tuple_field_name(tuple_ty, i);
                    let expr = self.mcx.member(self.mcx.value(val), field);
                    let piece = self.bb.func.0.next_local_var();
                    self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(piece, piece_ty, Some(expr))));
                    self.record_value_ty(piece, piece_ty);

                    let piece_ptr = if offset == 0 {
                        dst.val.llval
                    } else {
                        self.inbounds_ptradd(dst.val.llval, self.const_usize(offset as u64))
                    };
                    let piece_align =
                        dst.val.align.restrict_for_offset(rustc_abi::Size::from_bytes(offset as u64));
                    self.store(piece, piece_ptr, piece_align);
                }
            }
            PassMode::Indirect { meta_attrs: None, .. } => {
                let pointee_ty = self.cx.backend_type(arg_abi.layout);
                let loaded = self.load(pointee_ty, val, dst.val.align);
                self.store(loaded, dst.val.llval, dst.val.align);
            }
            PassMode::Indirect { meta_attrs: Some(_), .. } => {
                panic!("store_arg does not support unsized indirect values yet")
            }
        }
    }
}
