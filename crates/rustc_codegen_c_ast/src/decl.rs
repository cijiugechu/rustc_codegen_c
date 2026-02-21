//! This module defines AST nodes for C declarations.

use crate::expr::{CExpr, CValue};
use crate::pretty::{Print, PrinterCtx, INDENT};
use crate::symbol::{CLinkage, CVisibility};
use crate::ty::{print_declarator, CTy, CTyKind};
use crate::ModuleCtx;

/// C declarations.
pub type CDecl<'mx> = &'mx CDeclKind<'mx>;

/// C declaration kinds.
#[derive(Debug, Clone)]
pub enum CDeclKind<'mx> {
    /// Variable declaration consisting of a name, type, and optional initializer.
    ///
    /// Example:
    /// - `int foo;` // `ty val`
    /// - `int foo = bar` `ty val = expr`
    Var {
        name: CValue<'mx>,
        ty: CTy<'mx>,
        init: Option<CExpr<'mx>>,
        storage_class: CStorageClass,
        linkage: CLinkage,
        visibility: CVisibility,
        link_name: Option<&'mx str>,
        thread_local: bool,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CStorageClass {
    None,
    Extern,
}

impl Default for CStorageClass {
    fn default() -> Self {
        Self::None
    }
}

impl<'mx> ModuleCtx<'mx> {
    /// Create a new declaration.
    pub fn decl(self, decl: CDeclKind<'mx>) -> CDecl<'mx> {
        self.arena().alloc(decl)
    }

    /// Create a new variable declaration.
    pub fn var(self, name: CValue<'mx>, ty: CTy<'mx>, init: Option<CExpr<'mx>>) -> CDecl<'mx> {
        self.var_with_attrs(name, ty, init, CLinkage::External, CVisibility::Default)
    }

    /// Create a new variable declaration with explicit linkage and visibility.
    pub fn var_with_attrs(
        self,
        name: CValue<'mx>,
        ty: CTy<'mx>,
        init: Option<CExpr<'mx>>,
        linkage: CLinkage,
        visibility: CVisibility,
    ) -> CDecl<'mx> {
        self.var_with_symbol_attrs(name, ty, init, linkage, visibility, None, false)
    }

    pub fn extern_var_with_attrs(
        self,
        name: CValue<'mx>,
        ty: CTy<'mx>,
        visibility: CVisibility,
        link_name: Option<&'mx str>,
        thread_local: bool,
    ) -> CDecl<'mx> {
        self.decl(CDeclKind::Var {
            name,
            ty,
            init: None,
            storage_class: CStorageClass::Extern,
            linkage: CLinkage::External,
            visibility,
            link_name,
            thread_local,
        })
    }

    pub fn var_with_symbol_attrs(
        self,
        name: CValue<'mx>,
        ty: CTy<'mx>,
        init: Option<CExpr<'mx>>,
        linkage: CLinkage,
        visibility: CVisibility,
        link_name: Option<&'mx str>,
        thread_local: bool,
    ) -> CDecl<'mx> {
        if type_contains_incomplete_array(ty) {
            panic!("incomplete array is declaration-only and requires extern storage class");
        }
        self.decl(CDeclKind::Var {
            name,
            ty,
            init,
            storage_class: CStorageClass::None,
            linkage,
            visibility,
            link_name,
            thread_local,
        })
    }
}

fn type_contains_incomplete_array(ty: CTy<'_>) -> bool {
    match ty {
        CTy::Ref(kind) => match kind.0 {
            CTyKind::IncompleteArray(_) => true,
            CTyKind::Pointer(inner) => type_contains_incomplete_array(*inner),
            CTyKind::Array(inner, _) => type_contains_incomplete_array(*inner),
            CTyKind::Function { ret, params } => {
                type_contains_incomplete_array(*ret)
                    || params.iter().any(|param| type_contains_incomplete_array(*param))
            }
            CTyKind::Struct(_) | CTyKind::Union(_) => false,
        },
        _ => false,
    }
}

impl Print for CDecl<'_> {
    fn print_to(&self, ctx: &mut PrinterCtx) {
        match self {
            CDeclKind::Var {
                name,
                ty,
                init,
                storage_class,
                linkage,
                visibility,
                link_name,
                thread_local,
            } => {
                ctx.ibox(INDENT, |ctx| {
                    if *storage_class == CStorageClass::Extern {
                        ctx.word("extern ");
                    } else if *linkage == CLinkage::Internal {
                        ctx.word("static ");
                    }
                    if *linkage == CLinkage::Weak {
                        ctx.word("__attribute__((weak)) ");
                    }
                    if *thread_local {
                        ctx.word("_Thread_local ");
                    }
                    if *visibility == CVisibility::Hidden {
                        ctx.word("__attribute__((visibility(\"hidden\"))) ");
                    }
                    print_declarator(*ty, Some(*name), ctx);
                    if let Some(link_name) = link_name {
                        ctx.word(" __asm__(");
                        ctx.word(c_string_literal(link_name));
                        ctx.word(")");
                    }
                    if let Some(init) = init {
                        ctx.word(" =");
                        ctx.softbreak();
                        init.print_to(ctx);
                    }
                    ctx.word(";");
                });
            }
        }
    }
}

fn c_string_literal(raw: &str) -> String {
    use std::fmt::Write;

    let mut out = String::with_capacity(raw.len() + 2);
    out.push('"');
    for byte in raw.bytes() {
        match byte {
            b'\\' => out.push_str("\\\\"),
            b'"' => out.push_str("\\\""),
            0x20..=0x7e => out.push(byte as char),
            _ => {
                let _ = write!(&mut out, "\\{:03o}", byte);
            }
        }
    }
    out.push('"');
    out
}

#[cfg(test)]
mod tests {
    use crate::decl::CStorageClass;
    use crate::expr::CValue;
    use crate::pretty::{Print, PrinterCtx};
    use crate::symbol::{CLinkage, CVisibility};
    use crate::ty::{CTy, CTyKind, CIntTy, CUintTy};
    use crate::{ModuleArena, ModuleCtx};
    use rustc_data_structures::intern::Interned;

    fn render_decl<'mx>(decl: crate::decl::CDecl<'mx>) -> String {
        let mut ctx = PrinterCtx::new();
        decl.print_to(&mut ctx);
        ctx.finish()
    }

    #[test]
    fn prints_extern_var_declaration() {
        let arena = ModuleArena::new("");
        let mcx = ModuleCtx(&arena);
        let decl = mcx.extern_var_with_attrs(
            CValue::Func("g"),
            CTy::UInt(CUintTy::U8),
            CVisibility::Default,
            None,
            false,
        );
        assert_eq!(render_decl(decl), "extern uint8_t g;");
    }

    #[test]
    fn prints_extern_var_with_link_name() {
        let arena = ModuleArena::new("");
        let mcx = ModuleCtx(&arena);
        let decl = mcx.extern_var_with_attrs(
            CValue::Func("__rcgenc__5Ffoo"),
            CTy::UInt(CUintTy::U8),
            CVisibility::Default,
            Some("foo$bar"),
            false,
        );
        assert_eq!(
            render_decl(decl),
            "extern uint8_t __rcgenc__5Ffoo __asm__(\"foo$bar\");"
        );
    }

    #[test]
    fn prints_extern_incomplete_array_with_link_name() {
        let arena = ModuleArena::new("");
        let mcx = ModuleCtx(&arena);
        let ty = CTy::Ref(Interned::new_unchecked(&CTyKind::IncompleteArray(CTy::UInt(CUintTy::U8))));
        let decl = mcx.extern_var_with_attrs(
            CValue::Func("__rcgenc_x"),
            ty,
            CVisibility::Default,
            Some("x$y"),
            false,
        );
        assert_eq!(
            render_decl(decl),
            "extern uint8_t __rcgenc_x[] __asm__(\"x$y\");"
        );
    }

    #[test]
    fn prints_thread_local_extern_var_with_visibility() {
        let arena = ModuleArena::new("");
        let mcx = ModuleCtx(&arena);
        let decl = mcx.extern_var_with_attrs(
            CValue::Func("TLS"),
            CTy::UInt(CUintTy::U8),
            CVisibility::Hidden,
            None,
            true,
        );
        assert_eq!(
            render_decl(decl),
            "extern _Thread_local __attribute__((visibility(\"hidden\"))) uint8_t TLS;"
        );
    }

    #[test]
    fn prints_internal_var_without_extern_storage() {
        let arena = ModuleArena::new("");
        let mcx = ModuleCtx(&arena);
        let decl = mcx.var_with_symbol_attrs(
            CValue::Func("x"),
            CTy::UInt(CUintTy::U8),
            None,
            CLinkage::Internal,
            CVisibility::Default,
            None,
            false,
        );
        assert_eq!(render_decl(decl), "static uint8_t x;");
        match decl {
            crate::decl::CDeclKind::Var { storage_class, .. } => {
                assert_eq!(*storage_class, CStorageClass::None);
            }
        }
    }

    #[test]
    fn prints_weak_var_without_static_storage() {
        let arena = ModuleArena::new("");
        let mcx = ModuleCtx(&arena);
        let decl = mcx.var_with_symbol_attrs(
            CValue::Func("weak_sym"),
            CTy::UInt(CUintTy::U8),
            None,
            CLinkage::Weak,
            CVisibility::Default,
            None,
            false,
        );
        assert_eq!(render_decl(decl), "__attribute__((weak)) uint8_t weak_sym;");
    }

    #[test]
    #[should_panic(expected = "incomplete array is declaration-only")]
    fn rejects_non_extern_incomplete_array_declaration() {
        let arena = ModuleArena::new("");
        let mcx = ModuleCtx(&arena);
        let ty = CTy::Ref(Interned::new_unchecked(&CTyKind::IncompleteArray(CTy::Int(CIntTy::I32))));
        let _ = mcx.var_with_symbol_attrs(
            CValue::Func("bad"),
            ty,
            None,
            CLinkage::Internal,
            CVisibility::Default,
            None,
            false,
        );
    }
}
