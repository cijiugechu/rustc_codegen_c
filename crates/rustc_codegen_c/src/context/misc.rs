use std::cell::RefCell;

use rustc_codegen_c_ast::expr::CValue;
use rustc_codegen_c_ast::func::CFuncKind;
use rustc_codegen_c_ast::ty::{CIntTy, CTy};
use rustc_codegen_ssa::traits::MiscCodegenMethods;
use rustc_data_structures::intern::Interned;
use rustc_hash::FxHashMap;
use rustc_middle::ty::layout::HasTypingEnv;
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

impl<'tcx, 'mx> MiscCodegenMethods<'tcx> for CodegenCx<'tcx, 'mx> {
    fn vtables(
        &self,
    ) -> &RefCell<FxHashMap<(Ty<'tcx>, Option<ExistentialTraitRef<'tcx>>), Self::Value>> {
        todo!()
    }

    fn get_fn(&self, instance: Instance<'tcx>) -> Self::Function {
        *self.function_instances.borrow().get(&instance).unwrap()
    }

    fn get_fn_addr(&self, instance: Instance<'tcx>) -> Self::Value {
        if let Some(func) = self.function_instances.borrow().get(&instance).copied() {
            return CValue::Func(func.0.name);
        }

        let symbol_name = self.tcx.symbol_name(instance).name;
        let symbol_name = sanitize_symbol_name(symbol_name);
        CValue::Func(self.mcx.alloc_str(&symbol_name))
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
            if let Some(func) = self.function_instances.borrow().get(&instance).copied() {
                func
            } else {
                let symbol_name = sanitize_symbol_name(self.tcx.symbol_name(instance).name);
                let func = CFuncKind::new(
                    self.mcx.alloc_str(&symbol_name),
                    CTy::Int(CIntTy::I32),
                    [],
                );
                let func = Interned::new_unchecked(self.mcx.func(func));
                self.mcx.module().push_func(func);
                self.function_instances.borrow_mut().insert(instance, func);
                func
            }
        } else {
            let func =
                CFuncKind::new(self.mcx.alloc_str("rust_eh_personality"), CTy::Int(CIntTy::I32), []);
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

    fn set_frame_pointer_type(&self, llfn: Self::Function) {
        todo!()
    }

    fn apply_target_cpu_attr(&self, llfn: Self::Function) {
        todo!()
    }

    fn declare_c_main(&self, fn_type: Self::Type) -> Option<Self::Function> {
        todo!()
    }
}
