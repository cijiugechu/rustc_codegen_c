use rustc_abi::Align;
use rustc_codegen_ssa::traits::StaticCodegenMethods;
use rustc_hir::def_id::DefId;

use crate::context::CodegenCx;

impl<'tcx, 'mx> StaticCodegenMethods for CodegenCx<'tcx, 'mx> {
    fn static_addr_of(&self, cv: Self::Value, align: Align, kind: Option<&str>) -> Self::Value {
        todo!()
    }

    fn codegen_static(&mut self, def_id: DefId) {
        todo!()
    }
}
