//! This module defines AST nodes for C declarations.

use crate::expr::{CExpr, CValue};
use crate::pretty::{Print, PrinterCtx, INDENT};
use crate::symbol::{CLinkage, CVisibility};
use crate::ty::{print_declarator, CTy};
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
        linkage: CLinkage,
        visibility: CVisibility,
    },
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
        self.decl(CDeclKind::Var { name, ty, init, linkage, visibility })
    }
}

impl Print for CDecl<'_> {
    fn print_to(&self, ctx: &mut PrinterCtx) {
        match self {
            CDeclKind::Var { name, ty, init, linkage, visibility } => {
                ctx.ibox(INDENT, |ctx| {
                    if *linkage == CLinkage::Internal {
                        ctx.word("static ");
                    }
                    if *visibility == CVisibility::Hidden {
                        ctx.word("__attribute__((visibility(\"hidden\"))) ");
                    }
                    print_declarator(*ty, Some(*name), ctx);
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
