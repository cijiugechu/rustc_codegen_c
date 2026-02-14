use rustc_codegen_c_ast::func::CFuncKind;
use rustc_codegen_ssa::traits::PreDefineCodegenMethods;
use rustc_data_structures::intern::Interned;
use rustc_hir::attrs::Linkage;
use rustc_hir::def_id::DefId;
use rustc_middle::mir::mono::Visibility;
use rustc_middle::ty::layout::FnAbiOf;
use rustc_middle::ty::{self, Instance};

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

fn declaration_symbol_names<'a>(symbol_name: &'a str) -> (String, Option<&'a str>) {
    let sanitized = sanitize_symbol_name(symbol_name);
    let link_name = if sanitized == symbol_name { None } else { Some(symbol_name) };
    (sanitized, link_name)
}

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
        let (mut ret, mut args) = self.fn_abi_to_c_signature(fn_abi);

        if symbol_name == "malloc" {
            ret =
                rustc_codegen_c_ast::ty::CTy::Ref(Interned::new_unchecked(self.mcx.arena().alloc(
                    rustc_codegen_c_ast::ty::CTyKind::Pointer(rustc_codegen_c_ast::ty::CTy::Void),
                )));
            args =
                vec![rustc_codegen_c_ast::ty::CTy::UInt(rustc_codegen_c_ast::ty::CUintTy::Usize)];
        } else if symbol_name == "realloc" {
            ret =
                rustc_codegen_c_ast::ty::CTy::Ref(Interned::new_unchecked(self.mcx.arena().alloc(
                    rustc_codegen_c_ast::ty::CTyKind::Pointer(rustc_codegen_c_ast::ty::CTy::Void),
                )));
            args = vec![
                rustc_codegen_c_ast::ty::CTy::Ref(Interned::new_unchecked(self.mcx.arena().alloc(
                    rustc_codegen_c_ast::ty::CTyKind::Pointer(rustc_codegen_c_ast::ty::CTy::Void),
                ))),
                rustc_codegen_c_ast::ty::CTy::UInt(rustc_codegen_c_ast::ty::CUintTy::Usize),
            ];
        } else if symbol_name == "free" {
            ret = rustc_codegen_c_ast::ty::CTy::Void;
            args = vec![rustc_codegen_c_ast::ty::CTy::Ref(Interned::new_unchecked(
                self.mcx.arena().alloc(rustc_codegen_c_ast::ty::CTyKind::Pointer(
                    rustc_codegen_c_ast::ty::CTy::Void,
                )),
            ))];
        } else if symbol_name == "printf" {
            ret = rustc_codegen_c_ast::ty::CTy::Int(rustc_codegen_c_ast::ty::CIntTy::I32);
            args = vec![];
        }

        let is_printf = symbol_name == "printf";
        let (symbol_name, link_name) = declaration_symbol_names(symbol_name);
        let func = CFuncKind::new(self.mcx.alloc_str(&symbol_name), ret, args)
            .with_link_name(link_name.map(|name| self.mcx.alloc_str(name)));
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
