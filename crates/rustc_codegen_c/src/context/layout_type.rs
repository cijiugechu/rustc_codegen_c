use rustc_abi::{BackendRepr, Integer, Primitive};
use rustc_codegen_c_ast::ty::{CIntTy, CTy, CTyKind, CUintTy};
use rustc_codegen_ssa::traits::LayoutTypeCodegenMethods;
use rustc_data_structures::intern::Interned;
use rustc_middle::ty::layout::{LayoutOf, TyAndLayout};
use rustc_middle::ty::Ty;
use rustc_target::callconv::FnAbi;
use rustc_type_ir::TyKind;

use crate::context::CodegenCx;

impl<'tcx, 'mx> LayoutTypeCodegenMethods<'tcx> for CodegenCx<'tcx, 'mx> {
    fn backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        match layout.ty.kind() {
            TyKind::Int(_) | TyKind::Uint(_) => self.immediate_backend_type(layout),
            TyKind::Array(elem_ty, _) => {
                let elem_layout = self.layout_of(*elem_ty);
                let elem = self.immediate_backend_type(elem_layout);
                let array = self.mcx.arena().alloc(CTyKind::Array(elem, layout.fields.count()));
                CTy::Ref(Interned::new_unchecked(array))
            }
            _ => todo!(),
        }
    }

    fn cast_backend_type(&self, ty: &rustc_target::callconv::CastTarget) -> Self::Type {
        todo!()
    }

    fn fn_decl_backend_type(&self, fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        CTy::Void
    }

    fn fn_ptr_backend_type(&self, fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        todo!()
    }

    fn reg_backend_type(&self, ty: &rustc_abi::Reg) -> Self::Type {
        todo!()
    }

    fn immediate_backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        match layout.ty.kind() {
            TyKind::Int(int) => self.mcx.get_int_type(*int),
            TyKind::Uint(uint) => self.mcx.get_uint_type(*uint),
            _ => todo!(),
        }
    }

    fn is_backend_immediate(&self, layout: TyAndLayout<'tcx>) -> bool {
        match layout.backend_repr {
            BackendRepr::Scalar(_) | BackendRepr::SimdVector { .. } => true,
            BackendRepr::ScalarPair(..) | BackendRepr::Memory { .. } => false,
        }
    }

    fn is_backend_scalar_pair(&self, layout: TyAndLayout<'tcx>) -> bool {
        matches!(layout.backend_repr, BackendRepr::ScalarPair(..))
    }

    fn scalar_pair_element_backend_type(
        &self,
        layout: TyAndLayout<'tcx>,
        index: usize,
        immediate: bool,
    ) -> Self::Type {
        let (a, b) = match layout.backend_repr {
            BackendRepr::ScalarPair(a, b) => (a, b),
            _ => panic!("layout is not scalar pair: {layout:?}"),
        };

        let scalar = match index {
            0 => a,
            1 => b,
            _ => panic!("invalid scalar pair index: {index}"),
        };

        match scalar.primitive() {
            Primitive::Int(int, signed) => match (int, signed) {
                (Integer::I8, true) => CTy::Int(CIntTy::I8),
                (Integer::I16, true) => CTy::Int(CIntTy::I16),
                (Integer::I32, true) => CTy::Int(CIntTy::I32),
                (Integer::I64, true) => CTy::Int(CIntTy::I64),
                (Integer::I8, false) => CTy::UInt(CUintTy::U8),
                (Integer::I16, false) => CTy::UInt(CUintTy::U16),
                (Integer::I32, false) => CTy::UInt(CUintTy::U32),
                (Integer::I64, false) => CTy::UInt(CUintTy::U64),
                (Integer::I128, _) => todo!(),
            },
            Primitive::Pointer(_) => CTy::UInt(CUintTy::Usize),
            Primitive::Float(_) => todo!(),
        }
    }
}
