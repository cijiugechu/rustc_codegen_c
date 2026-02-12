//! This module defines AST nodes for C functions.

use std::cell::{Cell, RefCell};
use std::collections::HashSet;

use rustc_data_structures::intern::Interned;

use crate::expr::CValue;
use crate::pretty::{Print, PrinterCtx};
use crate::stmt::{print_compound, CStmt};
use crate::ty::{print_declarator, CTy};
use crate::ModuleCtx;

/// C functions definition.
pub type CFunc<'mx> = Interned<'mx, CFuncKind<'mx>>;

/// C function definition.
#[derive(Debug, Clone)]
pub struct CFuncKind<'mx> {
    /// Function name.
    pub name: &'mx str,
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
            ty,
            params,
            body: RefCell::new(Vec::new()),
            local_var_counter,
            block_counter,
            emitted_labels,
        }
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
            print_signature(*self, ctx);
            ctx.softbreak(); // I don't know how to avoid a newline here
            print_compound(&self.0.body.borrow(), ctx);
        })
    }
}

pub(crate) fn print_func_decl(func: CFunc, ctx: &mut PrinterCtx) {
    print_signature(func, ctx);
    ctx.word(";");
}

fn print_signature(func: CFunc, ctx: &mut PrinterCtx) {
    ctx.ibox(0, |ctx| {
        print_declarator(func.0.ty, Some(CValue::Func(func.0.name)), ctx);

        ctx.valign_delim(("(", ")"), |ctx| {
            ctx.seperated(",", &func.0.params, |ctx, (ty, name)| {
                ctx.ibox(0, |ctx| {
                    print_declarator(*ty, Some(*name), ctx);
                })
            })
        });
    });
}
