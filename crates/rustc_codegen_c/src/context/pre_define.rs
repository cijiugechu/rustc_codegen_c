use rustc_codegen_c_ast::func::CFuncKind;
use rustc_codegen_ssa::traits::PreDefineCodegenMethods;
use rustc_data_structures::intern::Interned;
use rustc_hir::attrs::Linkage;
use rustc_hir::def_id::DefId;
use rustc_middle::mir::mono::Visibility;
use rustc_middle::ty::layout::FnAbiOf;
use rustc_middle::ty::{self, Instance};

use crate::context::CodegenCx;

fn is_always_false_intrinsic(symbol_name: &str) -> bool {
    symbol_name.contains("is_val_statically_known")
}

impl<'tcx, 'mx> PreDefineCodegenMethods<'tcx> for CodegenCx<'tcx, 'mx> {
    fn predefine_static(
        &mut self,
        def_id: DefId,
        linkage: Linkage,
        visibility: Visibility,
        symbol_name: &str,
    ) {
        let _ = (linkage, visibility);
        self.register_static_symbol(def_id, symbol_name);
    }

    fn predefine_fn(
        &mut self,
        instance: Instance<'tcx>,
        linkage: Linkage,
        visibility: Visibility,
        symbol_name: &str,
    ) {
        let fn_abi = self.fn_abi_of_instance(instance, ty::List::empty());
        let mut signature = self.fn_abi_to_c_signature(fn_abi);
        let is_printf = self.apply_known_symbol_signature_overrides(symbol_name, &mut signature);
        let args = signature.param_tys();
        let (symbol_name, link_name) = self.declaration_symbol_names(symbol_name);
        let func = CFuncKind::new(self.mcx.alloc_str(&symbol_name), signature.ret, args)
            .with_link_name(link_name.as_ref().map(|name| self.mcx.alloc_str(name)));
        let func = Interned::new_unchecked(self.mcx.func(func));

        if is_always_false_intrinsic(symbol_name.as_str()) {
            func.0.push_stmt(
                self.mcx.ret(Some(self.mcx.value(rustc_codegen_c_ast::expr::CValue::Scalar(0)))),
            );
        }

        if !is_printf {
            self.mcx.module().push_func(func);
        }
        self.function_instances.borrow_mut().insert(instance, func);
    }
}
