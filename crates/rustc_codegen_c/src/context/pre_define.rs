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

        let mut args = Vec::new();
        if matches!(fn_abi.ret.mode, PassMode::Indirect { .. }) {
            args.push(self.indirect_ptr_ty_for_layout(fn_abi.ret.layout));
        }
        for arg in fn_abi.args.iter() {
            match arg.mode {
                PassMode::Ignore => {}
                PassMode::Direct(_) => args.push(self.immediate_backend_type(arg.layout)),
                PassMode::Pair(_, _) => {
                    args.push(self.scalar_pair_element_backend_type(arg.layout, 0, true));
                    args.push(self.scalar_pair_element_backend_type(arg.layout, 1, true));
                }
                PassMode::Cast { ref cast, pad_i32 } => {
                    if pad_i32 {
                        args.push(rustc_codegen_c_ast::ty::CTy::UInt(
                            rustc_codegen_c_ast::ty::CUintTy::U32,
                        ));
                    }
                    args.extend(
                        self.cast_target_to_c_abi_pieces(cast).into_iter().map(|(_, ty)| ty),
                    );
                }
                PassMode::Indirect { meta_attrs: None, .. } => {
                    args.push(self.indirect_ptr_ty_for_layout(arg.layout));
                }
                PassMode::Indirect { meta_attrs: Some(_), .. } => {
                    args.push(self.indirect_ptr_ty_for_layout(arg.layout));
                    args.push(rustc_codegen_c_ast::ty::CTy::Ref(Interned::new_unchecked(
                        self.mcx.arena().alloc(rustc_codegen_c_ast::ty::CTyKind::Pointer(
                            rustc_codegen_c_ast::ty::CTy::UInt(
                                rustc_codegen_c_ast::ty::CUintTy::U8,
                            ),
                        )),
                    )));
                }
            }
        }
        let mut ret = match fn_abi.ret.mode {
            PassMode::Ignore | PassMode::Indirect { .. } => rustc_codegen_c_ast::ty::CTy::Void,
            PassMode::Direct(_) => self.immediate_backend_type(fn_abi.ret.layout),
            PassMode::Pair(_, _) => self.abi_tuple_ty(&[
                self.scalar_pair_element_backend_type(fn_abi.ret.layout, 0, true),
                self.scalar_pair_element_backend_type(fn_abi.ret.layout, 1, true),
            ]),
            PassMode::Cast { ref cast, pad_i32: _ } => self.cast_backend_type(cast),
        };

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
        let symbol_name = sanitize_symbol_name(symbol_name);
        let func = CFuncKind::new(self.mcx.alloc_str(&symbol_name), ret, args);
        let func = Interned::new_unchecked(self.mcx.func(func));
        if !is_printf {
            self.mcx.module().push_func(func);
        }
        self.function_instances.borrow_mut().insert(instance, func);
    }
}
