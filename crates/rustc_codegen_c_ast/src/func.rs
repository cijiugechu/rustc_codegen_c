//! This module defines AST nodes for C functions.

use std::cell::{Cell, RefCell};
use std::collections::HashSet;

use rustc_data_structures::intern::Interned;

use crate::expr::CValue;
use crate::pretty::{Print, PrinterCtx};
use crate::stmt::{print_compound, CStmt};
use crate::symbol::{CLinkage, CVisibility};
use crate::ty::{print_declarator, CTy};
use crate::ModuleCtx;

/// C functions definition.
pub type CFunc<'mx> = Interned<'mx, CFuncKind<'mx>>;

/// C function definition.
#[derive(Debug, Clone)]
pub struct CFuncKind<'mx> {
    /// Function name.
    pub name: &'mx str,
    /// Optional link symbol name emitted on declarations.
    pub link_name: Option<&'mx str>,
    /// Linkage for this symbol.
    pub linkage: CLinkage,
    /// Visibility for this symbol.
    pub visibility: CVisibility,
    /// Return type.
    pub ty: CTy<'mx>,
    /// Function parameters.
    pub params: Vec<(CTy<'mx>, CValue<'mx>)>,
    /// Function body.
    pub body: RefCell<Vec<CStmt<'mx>>>,
    /// A counter for local variables, for generating unique names.
    local_var_counter: Cell<usize>,
    /// A counter for basic block labels.
    block_counter: Cell<usize>,
    /// Labels already emitted in the function body.
    emitted_labels: RefCell<HashSet<&'mx str>>,
}

impl<'mx> CFuncKind<'mx> {
    /// Make a new function definition.
    pub fn new(name: &'mx str, ty: CTy<'mx>, params: impl IntoIterator<Item = CTy<'mx>>) -> Self {
        let params = params
            .into_iter()
            .enumerate()
            .map(|(i, ty)| (ty, CValue::Local(i)))
            .collect::<Vec<_>>();
        let local_var_counter = Cell::new(params.len());
        let block_counter = Cell::new(0);
        let emitted_labels = RefCell::new(HashSet::new());

        Self {
            name,
            link_name: None,
            linkage: CLinkage::External,
            visibility: CVisibility::Default,
            ty,
            params,
            body: RefCell::new(Vec::new()),
            local_var_counter,
            block_counter,
            emitted_labels,
        }
    }

    /// Set the link symbol name used for declarations.
    pub fn with_link_name(mut self, link_name: Option<&'mx str>) -> Self {
        self.link_name = link_name;
        self
    }

    /// Set linkage for this function.
    pub fn with_linkage(mut self, linkage: CLinkage) -> Self {
        self.linkage = linkage;
        self
    }

    /// Set visibility for this function.
    pub fn with_visibility(mut self, visibility: CVisibility) -> Self {
        self.visibility = visibility;
        self
    }

    /// Push a statement to the end of the function body.
    pub fn push_stmt(&self, stmt: CStmt<'mx>) {
        self.body.borrow_mut().push(stmt);
    }

    /// Get a new unique local variable.
    pub fn next_local_var(&self) -> CValue<'_> {
        let val = CValue::Local(self.local_var_counter.get());
        self.local_var_counter.set(self.local_var_counter.get() + 1);
        val
    }

    /// Get a new unique basic block id.
    pub fn next_block_id(&self) -> usize {
        let id = self.block_counter.get();
        self.block_counter.set(id + 1);
        id
    }

    /// Emit a block label once.
    pub fn emit_label_once(&self, mcx: ModuleCtx<'mx>, label: &'mx str) {
        if self.emitted_labels.borrow_mut().insert(label) {
            self.push_stmt(mcx.label_stmt(label));
        }
    }
}

impl<'mx> ModuleCtx<'mx> {
    /// Create a new function definition.
    pub fn func(&self, func: CFuncKind<'mx>) -> &'mx CFuncKind<'mx> {
        self.arena().alloc(func)
    }
}

impl Print for CFunc<'_> {
    fn print_to(&self, ctx: &mut PrinterCtx) {
        ctx.ibox(0, |ctx| {
            print_signature(*self, ctx, false);
            ctx.softbreak(); // I don't know how to avoid a newline here
            print_compound(&self.0.body.borrow(), ctx);
        })
    }
}

pub(crate) fn print_func_decl(func: CFunc, ctx: &mut PrinterCtx) {
    print_signature(func, ctx, true);
    ctx.word(";");
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

fn print_signature(func: CFunc, ctx: &mut PrinterCtx, with_link_name: bool) {
    ctx.ibox(0, |ctx| {
        if func.0.linkage == CLinkage::Internal {
            ctx.word("static ");
        }
        if func.0.visibility == CVisibility::Hidden {
            ctx.word("__attribute__((visibility(\"hidden\"))) ");
        }
        print_declarator(func.0.ty, Some(CValue::Func(func.0.name)), ctx);

        ctx.valign_delim(("(", ")"), |ctx| {
            ctx.seperated(",", &func.0.params, |ctx, (ty, name)| {
                ctx.ibox(0, |ctx| {
                    print_declarator(*ty, Some(*name), ctx);
                })
            })
        });

        if with_link_name {
            if let Some(link_name) = func.0.link_name {
                ctx.word(" __asm__(");
                ctx.word(c_string_literal(link_name));
                ctx.word(")");
            }
        }
    });
}
