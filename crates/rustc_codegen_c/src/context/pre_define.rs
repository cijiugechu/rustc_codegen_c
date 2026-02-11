use crate::rustc_codegen_ssa::traits::LayoutTypeCodegenMethods;
use rustc_codegen_c_ast::func::CFuncKind;
use rustc_codegen_ssa::traits::PreDefineCodegenMethods;
use rustc_data_structures::intern::Interned;
use rustc_hir::attrs::Linkage;
use rustc_hir::def_id::DefId;
use rustc_middle::mir::mono::Visibility;
use rustc_middle::ty::layout::FnAbiOf;
use rustc_middle::ty::{self, Instance};

use crate::context::CodegenCx;

impl<'tcx, 'mx> PreDefineCodegenMethods<'tcx> for CodegenCx<'tcx, 'mx> {
    fn predefine_static(
        &mut self,
        def_id: DefId,
        linkage: Linkage,
        visibility: Visibility,
        symbol_name: &str,
    ) {
        todo!()
    }

    fn predefine_fn(
        &mut self,
        instance: Instance<'tcx>,
        linkage: Linkage,
        visibility: Visibility,
        symbol_name: &str,
    ) {
        let fn_abi = self.fn_abi_of_instance(instance, ty::List::empty());

        let args = fn_abi.args.iter().map(|arg| self.immediate_backend_type(arg.layout));
        let ret = self.immediate_backend_type(fn_abi.ret.layout);

        let func = CFuncKind::new(self.mcx.alloc_str(symbol_name), ret, args);
        let func = Interned::new_unchecked(self.mcx.func(func));
        self.mcx.module().push_func(func);
        self.function_instances.borrow_mut().insert(instance, func);
    }
}
