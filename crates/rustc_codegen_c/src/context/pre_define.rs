use crate::rustc_codegen_ssa::traits::LayoutTypeCodegenMethods;
use rustc_codegen_c_ast::func::CFuncKind;
use rustc_codegen_ssa::traits::PreDefineCodegenMethods;
use rustc_data_structures::intern::Interned;
use rustc_hir::attrs::Linkage;
use rustc_hir::def_id::DefId;
use rustc_middle::mir::mono::Visibility;
use rustc_middle::ty::layout::FnAbiOf;
use rustc_middle::ty::{self, Instance};
use rustc_target::callconv::PassMode;

use crate::context::CodegenCx;

fn is_valid_c_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };

    if !(first == '_' || first.is_ascii_alphabetic()) {
        return false;
    }

    chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}

fn sanitize_symbol_name(symbol_name: &str) -> String {
    if is_valid_c_identifier(symbol_name) {
        return symbol_name.to_string();
    }

    let mut out = String::from("__rcgenc_");
    for byte in symbol_name.bytes() {
        if byte.is_ascii_alphanumeric() {
            out.push(byte as char);
        } else {
            use std::fmt::Write;
            let _ = write!(&mut out, "_{byte:02X}");
        }
    }
    out
}

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

        let args = fn_abi.args.iter().flat_map(|arg| match arg.mode {
            PassMode::Ignore => Vec::new(),
            PassMode::Direct(_) => vec![self.immediate_backend_type(arg.layout)],
            PassMode::Pair(_, _) => vec![
                self.scalar_pair_element_backend_type(arg.layout, 0, true),
                self.scalar_pair_element_backend_type(arg.layout, 1, true),
            ],
            PassMode::Cast { .. } | PassMode::Indirect { .. } => {
                panic!("predefine_fn does not support arg pass mode {:?} yet", arg.mode)
            }
        });
        let ret = match fn_abi.ret.mode {
            PassMode::Ignore => rustc_codegen_c_ast::ty::CTy::Void,
            PassMode::Direct(_) => self.immediate_backend_type(fn_abi.ret.layout),
            PassMode::Pair(_, _) | PassMode::Cast { .. } | PassMode::Indirect { .. } => {
                panic!("predefine_fn does not support return pass mode {:?} yet", fn_abi.ret.mode)
            }
        };

        let symbol_name = sanitize_symbol_name(symbol_name);
        let func = CFuncKind::new(self.mcx.alloc_str(&symbol_name), ret, args);
        let func = Interned::new_unchecked(self.mcx.func(func));
        self.mcx.module().push_func(func);
        self.function_instances.borrow_mut().insert(instance, func);
    }
}
