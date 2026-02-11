use rustc_abi::BackendRepr;
use rustc_codegen_c_ast::ty::CTy;
use rustc_codegen_ssa::traits::LayoutTypeCodegenMethods;
use rustc_middle::ty::layout::TyAndLayout;
use rustc_middle::ty::Ty;
use rustc_target::callconv::FnAbi;
use rustc_type_ir::TyKind;

use crate::context::CodegenCx;

impl<'tcx, 'mx> LayoutTypeCodegenMethods<'tcx> for CodegenCx<'tcx, 'mx> {
    fn backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        todo!()
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
        todo!()
    }

    fn scalar_pair_element_backend_type(
        &self,
        layout: TyAndLayout<'tcx>,
        index: usize,
        immediate: bool,
    ) -> Self::Type {
        todo!()
    }
}
