use std::cell::RefCell;

use rustc_codegen_c_ast::expr::CValue;
use rustc_codegen_c_ast::func::{CFunc, CFuncKind};
use rustc_codegen_c_ast::ty::{CIntTy, CTy, CTyKind};
use rustc_codegen_ssa::traits::MiscCodegenMethods;
use rustc_data_structures::intern::Interned;
use rustc_hash::FxHashMap;
use rustc_middle::ty::layout::{FnAbiOf, HasTypingEnv};
use rustc_middle::ty::{ExistentialTraitRef, Instance, Ty};
use rustc_span::DUMMY_SP;

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

fn function_signature_from_type<'mx>(fn_type: CTy<'mx>) -> (CTy<'mx>, Vec<CTy<'mx>>) {
    match fn_type {
        CTy::Ref(kind) => match kind.0 {
            CTyKind::Function { ret, params } => (*ret, params.clone()),
            CTyKind::Pointer(pointee) => match *pointee {
                CTy::Ref(inner) => match inner.0 {
                    CTyKind::Function { ret, params } => (*ret, params.clone()),
                    _ => panic!("declare_c_main expects function type, got pointer to {inner:?}"),
                },
                _ => panic!("declare_c_main expects pointer-to-function type, got {fn_type:?}"),
            },
            _ => panic!("declare_c_main expects function type, got {fn_type:?}"),
        },
        _ => panic!("declare_c_main expects function type, got {fn_type:?}"),
    }
}

impl<'tcx, 'mx> MiscCodegenMethods<'tcx> for CodegenCx<'tcx, 'mx> {
    fn vtables(
        &self,
    ) -> &RefCell<FxHashMap<(Ty<'tcx>, Option<ExistentialTraitRef<'tcx>>), Self::Value>> {
        &self.vtables
    }

    fn get_fn(&self, instance: Instance<'tcx>) -> Self::Function {
        if let Some(func) = self.function_instances.borrow().get(&instance).copied() {
            return func;
        }

        let fn_abi = self.fn_abi_of_instance(instance, rustc_middle::ty::List::empty());
        let (mut ret, mut args) = self.fn_abi_to_c_signature(fn_abi);

        let symbol_name = self.tcx.symbol_name(instance).name;
        let is_printf = symbol_name == "printf";
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

        let (symbol_name, link_name) = declaration_symbol_names(symbol_name);

        let func: CFunc<'mx> = Interned::new_unchecked(
            self.mcx.func(
                CFuncKind::new(self.mcx.alloc_str(&symbol_name), ret, args)
                    .with_link_name(link_name.map(|name| self.mcx.alloc_str(name))),
            ),
        );
        if !is_printf {
            self.mcx.module().push_func(func);
        }
        self.function_instances.borrow_mut().insert(instance, func);
        func
    }

    fn get_fn_addr(&self, instance: Instance<'tcx>) -> Self::Value {
        let func = self.get_fn(instance);
        CValue::Func(func.0.name)
    }

    fn eh_personality(&self) -> Self::Function {
        if let Some(func) = *self.eh_personality_fn.borrow() {
            return func;
        }

        let func = if let Some(def_id) = self.tcx.lang_items().eh_personality() {
            let instance = Instance::expect_resolve(
                self.tcx,
                self.typing_env(),
                def_id,
                rustc_middle::ty::List::empty(),
                DUMMY_SP,
            );
            let existing = { self.function_instances.borrow().get(&instance).copied() };
            if let Some(func) = existing {
                func
            } else {
                let symbol_name = self.tcx.symbol_name(instance).name;
                let (symbol_name, link_name) = declaration_symbol_names(symbol_name);
                let func =
                    CFuncKind::new(self.mcx.alloc_str(&symbol_name), CTy::Int(CIntTy::I32), [])
                        .with_link_name(link_name.map(|name| self.mcx.alloc_str(name)));
                let func = Interned::new_unchecked(self.mcx.func(func));
                self.mcx.module().push_func(func);
                self.function_instances.borrow_mut().insert(instance, func);
                func
            }
        } else {
            let func = CFuncKind::new(
                self.mcx.alloc_str("rust_eh_personality"),
                CTy::Int(CIntTy::I32),
                [],
            );
            let func = Interned::new_unchecked(self.mcx.func(func));
            self.mcx.module().push_func(func);
            func
        };

        *self.eh_personality_fn.borrow_mut() = Some(func);
        func
    }

    fn sess(&self) -> &rustc_session::Session {
        self.tcx.sess
    }

    fn set_frame_pointer_type(&self, _llfn: Self::Function) {
        // no-op for C backend
    }

    fn apply_target_cpu_attr(&self, _llfn: Self::Function) {
        // no-op for C backend
    }

    fn declare_c_main(&self, fn_type: Self::Type) -> Option<Self::Function> {
        let entry_name = self.sess().target.entry_name.as_ref();
        if self.mcx.module().funcs.borrow().iter().any(|func| func.0.name == entry_name) {
            return None;
        }

        let (ret, mut params) = function_signature_from_type(fn_type);
        if self.sess().target.main_needs_argc_argv
            && !self.sess().target.os.contains("uefi")
            && params.len() == 2
        {
            let char_ptr = CTy::Ref(Interned::new_unchecked(
                self.mcx.arena().alloc(CTyKind::Pointer(CTy::Char)),
            ));
            let argv_ty = CTy::Ref(Interned::new_unchecked(
                self.mcx.arena().alloc(CTyKind::Pointer(char_ptr)),
            ));
            params[1] = argv_ty;
        }
        let func = Interned::new_unchecked(self.mcx.func(CFuncKind::new(
            self.mcx.alloc_str(entry_name),
            ret,
            params,
        )));
        self.mcx.module().push_func(func);
        Some(func)
    }
}
