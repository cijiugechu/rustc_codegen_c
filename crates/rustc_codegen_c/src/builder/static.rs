use rustc_codegen_c_ast::expr::CValue;
use rustc_codegen_ssa::traits::StaticBuilderMethods;
use rustc_hir::def_id::DefId;

use crate::builder::Builder;

impl<'tcx, 'mx> StaticBuilderMethods for Builder<'_, 'tcx, 'mx> {
    fn get_static(&mut self, def_id: DefId) -> Self::Value {
        let symbol = self.cx.static_symbol(def_id);
        let expr = self.mcx.alloc_str(&format!("((uint8_t *)&{symbol})"));
        CValue::Func(expr)
    }
}
