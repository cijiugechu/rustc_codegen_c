use std::cell::RefCell;

use rustc_codegen_c_ast::expr::CValue;
use rustc_codegen_c_ast::func::{CFunc, CFuncKind};
use rustc_codegen_c_ast::symbol::CLinkage;
use rustc_codegen_c_ast::ty::{CIntTy, CTy, CTyKind, CUintTy};
use rustc_codegen_ssa::traits::MiscCodegenMethods;
use rustc_data_structures::intern::Interned;
use rustc_hash::FxHashMap;
use rustc_middle::ty::layout::{FnAbiOf, HasTypingEnv};
use rustc_middle::ty::{ExistentialTraitRef, Instance, Ty};
use rustc_span::DUMMY_SP;
use rustc_target::callconv::FnAbi;

use crate::context::{CAbiSignature, CodegenCx};

fn is_always_false_intrinsic(symbol_name: &str) -> bool {
    symbol_name.contains("is_val_statically_known")
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

impl<'tcx, 'mx> CodegenCx<'tcx, 'mx> {
    fn declare_c_func_with_signature(
        &self,
        name: &str,
        link_name: Option<String>,
        signature: &CAbiSignature<'mx>,
        linkage: CLinkage,
    ) -> CFunc<'mx> {
        let params = signature.param_tys();
        Interned::new_unchecked(
            self.mcx.func(
                CFuncKind::new(self.mcx.alloc_str(name), signature.ret, params)
                    .with_linkage(linkage)
                    .with_link_name(link_name.as_deref().map(|name| self.mcx.alloc_str(name))),
            ),
        )
    }

    fn declare_indirect_return_bridge(
        &self,
        symbol_name: &str,
        fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        lowered_signature: &CAbiSignature<'mx>,
    ) -> CFunc<'mx> {
        let native_signature = self.fn_abi_to_native_c_signature(fn_abi);
        let (decl_name, _) = self.declaration_symbol_names(symbol_name);
        let link_name = Some(self.symbol_object_link_name(symbol_name));

        let native_decl_name =
            format!("__rcgenc_native_{}_{}", decl_name, self.next_synthetic_type_id());
        let native_func = self.declare_c_func_with_signature(
            &native_decl_name,
            link_name,
            &native_signature,
            CLinkage::External,
        );
        self.mcx.module().push_func(native_func);

        let bridge_name =
            format!("__rcgenc_sret_bridge_{}_{}", decl_name, self.next_synthetic_type_id());
        let bridge_func = self.declare_c_func_with_signature(
            &bridge_name,
            None,
            lowered_signature,
            CLinkage::External,
        );

        let call_args = lowered_signature
            .params
            .iter()
            .enumerate()
            .skip(1)
            .map(|(idx, _)| self.mcx.value(CValue::Local(idx)))
            .collect::<Vec<_>>();
        let call_expr = self.mcx.call(self.mcx.value(CValue::Func(native_func.0.name)), call_args);
        let call_tmp = bridge_func.0.next_local_var();
        bridge_func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(
            call_tmp,
            native_signature.ret,
            Some(call_expr),
        )));

        let ret_size = fn_abi.ret.layout.size.bytes();
        if ret_size != 0 {
            let u8_ptr_ty = CTy::Ref(Interned::new_unchecked(
                self.mcx.arena().alloc(CTyKind::Pointer(CTy::UInt(CUintTy::U8))),
            ));
            let dst_ptr = self.mcx.cast(u8_ptr_ty, self.mcx.value(CValue::Local(0)));
            let src_ptr = self.mcx.cast(u8_ptr_ty, self.mcx.unary("&", self.mcx.value(call_tmp)));
            let copy_expr = self.mcx.call(
                self.mcx.value(CValue::Func("__builtin_memcpy")),
                vec![dst_ptr, src_ptr, self.mcx.value(CValue::Scalar(ret_size as i128))],
            );
            bridge_func.0.push_stmt(self.mcx.expr_stmt(copy_expr));
        }

        bridge_func.0.push_stmt(self.mcx.ret(None));
        self.mcx.module().push_func(bridge_func);
        bridge_func
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
        let symbol_name = self.tcx.symbol_name(instance).name;
        let mut signature = self.fn_abi_to_c_signature(fn_abi);
        let is_printf = self.apply_known_symbol_signature_overrides(symbol_name, &mut signature);

        let (func, already_emitted) = if signature.has_struct_return() {
            (self.declare_indirect_return_bridge(symbol_name, fn_abi, &signature), true)
        } else {
            let (decl_name, link_name) = self.declaration_symbol_names(symbol_name);
            let linkage = if is_always_false_intrinsic(symbol_name) {
                // This synthesized intrinsic can be instantiated in multiple CGUs.
                CLinkage::Weak
            } else {
                CLinkage::External
            };
            (self.declare_c_func_with_signature(&decl_name, link_name, &signature, linkage), false)
        };

        if is_always_false_intrinsic(symbol_name) {
            func.0.push_stmt(self.mcx.ret(Some(self.mcx.value(CValue::Scalar(0)))));
        }

        if !is_printf && !already_emitted {
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
                let (symbol_name, link_name) = self.declaration_symbol_names(symbol_name);
                let mut func_kind =
                    CFuncKind::new(self.mcx.alloc_str(&symbol_name), CTy::Int(CIntTy::I32), [])
                        .with_link_name(link_name.as_ref().map(|name| self.mcx.alloc_str(name)));
                if symbol_name == "rust_eh_personality" {
                    // Avoid duplicate global `_rust_eh_personality` definitions across CGUs.
                    func_kind = func_kind.with_linkage(CLinkage::Weak);
                }
                let func = func_kind;
                let func = Interned::new_unchecked(self.mcx.func(func));
                if is_always_false_intrinsic(symbol_name.as_str()) {
                    func.0.push_stmt(self.mcx.ret(Some(self.mcx.value(CValue::Scalar(0)))));
                }
                self.mcx.module().push_func(func);
                self.function_instances.borrow_mut().insert(instance, func);
                func
            }
        } else {
            let func = CFuncKind::new(
                self.mcx.alloc_str("rust_eh_personality"),
                CTy::Int(CIntTy::I32),
                [],
            )
            .with_linkage(CLinkage::Weak);
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
