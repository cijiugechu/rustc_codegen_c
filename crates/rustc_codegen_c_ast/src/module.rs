//! This module defines AST nodes for C modules.

use std::cell::RefCell;

use crate::cstruct::CStructDef;
use crate::decl::CDecl;
use crate::func::{print_func_decl, CFunc};
use crate::include::{canonicalize_include, render_include_block, IncludeKind, IncludeSet};
use crate::pretty::{Print, PrinterCtx};

/// C module definition.
#[derive(Debug, Clone)]
pub struct Module<'mx> {
    /// Includes. Only the file name is recorded, without the angle brackets.
    pub includes: RefCell<IncludeSet>,
    /// A piece of helper code to be included at the beginning of the file.
    pub helper: &'static str,
    /// Declarations.
    pub decls: RefCell<Vec<CDecl<'mx>>>,
    /// Struct declarations.
    pub structs: RefCell<Vec<CStructDef<'mx>>>,
    /// Function definitions.
    pub funcs: RefCell<Vec<CFunc<'mx>>>,
}

impl<'mx> Module<'mx> {
    /// Make a new module definition.
    pub fn new(helper: &'static str) -> Self {
        Self {
            includes: RefCell::new(IncludeSet::new()),
            helper,
            decls: RefCell::new(Vec::new()),
            structs: RefCell::new(Vec::new()),
            funcs: RefCell::new(Vec::new()),
        }
    }

    /// Require a system include (`#include <...>`).
    pub fn require_system_include(&self, include: &str) {
        self.includes.borrow_mut().insert(canonicalize_include(include, IncludeKind::System));
    }

    /// Require a local include (`#include "..."`).
    pub fn require_local_include(&self, include: &str) {
        self.includes.borrow_mut().insert(canonicalize_include(include, IncludeKind::Local));
    }

    /// Push a declaration to the end of the declarations list.
    pub fn push_decl(&self, decl: CDecl<'mx>) {
        self.decls.borrow_mut().push(decl);
    }

    /// Push a struct declaration to the end of the declarations list.
    pub fn push_struct(&self, def: CStructDef<'mx>) {
        self.structs.borrow_mut().push(def);
    }

    /// Push a function definition to the end of the function definitions list.
    pub fn push_func(&self, func: CFunc<'mx>) {
        self.funcs.borrow_mut().push(func);
    }
}

impl Print for Module<'_> {
    fn print_to(&self, ctx: &mut PrinterCtx) {
        ctx.cbox(0, |ctx| {
            let include_block = {
                let includes = self.includes.borrow();
                render_include_block(&includes)
            };
            if !include_block.is_empty() {
                for line in include_block.lines() {
                    ctx.word(line.to_string());
                    ctx.hardbreak();
                }
                ctx.hardbreak();
            }

            ctx.word(self.helper);

            for def in self.structs.borrow().iter() {
                ctx.hardbreak();
                ctx.hardbreak();
                def.print_to(ctx);
            }

            for &func in self.funcs.borrow().iter() {
                ctx.hardbreak();
                print_func_decl(func, ctx);
            }

            for &decl in self.decls.borrow().iter() {
                ctx.hardbreak();
                ctx.hardbreak();
                decl.print_to(ctx);
            }

            for &func in self.funcs.borrow().iter() {
                if func.0.body.borrow().is_empty() && func.0.name != "rust_eh_personality" {
                    continue;
                }
                ctx.hardbreak();
                ctx.hardbreak();
                func.print_to(ctx);
            }

            ctx.hardbreak();
        });
    }
}
