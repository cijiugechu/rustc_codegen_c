//! This module defines the AST nodes for C statements.

use crate::decl::CDecl;
use crate::expr::CExpr;
use crate::pretty::{Print, PrinterCtx, INDENT};
use crate::ModuleCtx;

/// C statement.
pub type CStmt<'mx> = &'mx CStmtKind<'mx>;

/// C statement.
#[derive(Debug, Clone)]
pub enum CStmtKind<'mx> {
    /// Compound statement, which is a sequence of statements enclosed in braces.
    Compound(Vec<CStmt<'mx>>),
    /// Return statement.
    Return(Option<CExpr<'mx>>),
    /// Declaration statement, e.g. `int x = 42;`.
    Decl(CDecl<'mx>),
    /// Expression statement, e.g. `foo(x + 1);`.
    Expr(CExpr<'mx>),
    /// Label statement, e.g. `bb0:`.
    Label(&'mx str),
    /// Goto statement, e.g. `goto bb1;`.
    Goto(&'mx str),
    /// Conditional goto statement.
    IfGoto { cond: CExpr<'mx>, then_label: &'mx str, else_label: &'mx str },
    /// Switch statement with goto targets.
    Switch { expr: CExpr<'mx>, cases: Vec<(u128, &'mx str)>, default: &'mx str },
}

impl<'mx> ModuleCtx<'mx> {
    /// Create a new statement.
    pub fn stmt(self, stmt: CStmtKind<'mx>) -> CStmt<'mx> {
        self.arena().alloc(stmt)
    }

    /// Create a compound statement.
    pub fn compound(self, stmts: Vec<CStmt<'mx>>) -> CStmt<'mx> {
        self.stmt(CStmtKind::Compound(stmts))
    }

    /// Create a return statement.
    pub fn ret(self, expr: Option<CExpr<'mx>>) -> CStmt<'mx> {
        self.stmt(CStmtKind::Return(expr))
    }

    /// Create a declaration statement.
    pub fn decl_stmt(self, decl: CDecl<'mx>) -> CStmt<'mx> {
        self.stmt(CStmtKind::Decl(decl))
    }

    /// Create an expression statement.
    pub fn expr_stmt(self, expr: CExpr<'mx>) -> CStmt<'mx> {
        self.stmt(CStmtKind::Expr(expr))
    }

    /// Create a label statement.
    pub fn label_stmt(self, label: &'mx str) -> CStmt<'mx> {
        self.stmt(CStmtKind::Label(label))
    }

    /// Create a goto statement.
    pub fn goto_stmt(self, label: &'mx str) -> CStmt<'mx> {
        self.stmt(CStmtKind::Goto(label))
    }

    /// Create a conditional goto statement.
    pub fn if_goto_stmt(
        self,
        cond: CExpr<'mx>,
        then_label: &'mx str,
        else_label: &'mx str,
    ) -> CStmt<'mx> {
        self.stmt(CStmtKind::IfGoto { cond, then_label, else_label })
    }

    /// Create a switch statement.
    pub fn switch_stmt(
        self,
        expr: CExpr<'mx>,
        cases: Vec<(u128, &'mx str)>,
        default: &'mx str,
    ) -> CStmt<'mx> {
        self.stmt(CStmtKind::Switch { expr, cases, default })
    }
}

fn format_switch_case_literal(value: u128) -> String {
    if value <= i64::MAX as u128 {
        return value.to_string();
    }

    if value <= u64::MAX as u128 {
        return format!("{value}ULL");
    }

    let hi = (value >> 64) as u64;
    let lo = value as u64;
    format!("__rust_u128_from_parts({hi}ULL, {lo}ULL)")
}

impl Print for CStmt<'_> {
    fn print_to(&self, ctx: &mut PrinterCtx) {
        match self {
            CStmtKind::Compound(stmts) => print_compound(stmts, ctx),
            CStmtKind::Return(ret) => {
                ctx.ibox(INDENT, |ctx| {
                    ctx.word("return");
                    if let Some(ret) = ret {
                        ctx.softbreak();
                        ret.print_to(ctx);
                    }
                    ctx.word(";");
                });
            }
            CStmtKind::Decl(decl) => decl.print_to(ctx),
            CStmtKind::Expr(expr) => {
                expr.print_to(ctx);
                ctx.word(";");
            }
            CStmtKind::Label(label) => {
                ctx.word(label.to_string());
                ctx.word(":");
            }
            CStmtKind::Goto(label) => {
                ctx.word("goto");
                ctx.nbsp();
                ctx.word(label.to_string());
                ctx.word(";");
            }
            CStmtKind::IfGoto { cond, then_label, else_label } => {
                ctx.ibox(INDENT, |ctx| {
                    ctx.word("if");
                    ctx.nbsp();
                    ctx.word("(");
                    cond.print_to(ctx);
                    ctx.word(")");
                    ctx.nbsp();
                    ctx.word("goto");
                    ctx.nbsp();
                    ctx.word(then_label.to_string());
                    ctx.word(";");
                    ctx.nbsp();
                    ctx.word("else");
                    ctx.nbsp();
                    ctx.word("goto");
                    ctx.nbsp();
                    ctx.word(else_label.to_string());
                    ctx.word(";");
                });
            }
            CStmtKind::Switch { expr, cases, default } => {
                ctx.ibox(INDENT, |ctx| {
                    ctx.word("switch");
                    ctx.nbsp();
                    ctx.word("(");
                    expr.print_to(ctx);
                    ctx.word(")");
                    ctx.nbsp();
                    ctx.cbox_delim(INDENT, ("{", "}"), 1, |ctx| {
                        if let Some((first, rest)) = cases.split_first() {
                            ctx.ibox(INDENT, |ctx| {
                                ctx.word(format!("case {}:", format_switch_case_literal(first.0)));
                                ctx.nbsp();
                                ctx.word("goto");
                                ctx.nbsp();
                                ctx.word(first.1.to_string());
                                ctx.word(";");
                            });
                            for (val, label) in rest {
                                ctx.hardbreak();
                                ctx.ibox(INDENT, |ctx| {
                                    ctx.word(format!("case {}:", format_switch_case_literal(*val)));
                                    ctx.nbsp();
                                    ctx.word("goto");
                                    ctx.nbsp();
                                    ctx.word(label.to_string());
                                    ctx.word(";");
                                });
                            }
                            ctx.hardbreak();
                        }
                        ctx.ibox(INDENT, |ctx| {
                            ctx.word("default:");
                            ctx.nbsp();
                            ctx.word("goto");
                            ctx.nbsp();
                            ctx.word(default.to_string());
                            ctx.word(";");
                        });
                    });
                });
            }
        }
    }
}

/// Print a compound statement.
pub(crate) fn print_compound(stmts: &[CStmt], ctx: &mut PrinterCtx) {
    ctx.cbox_delim(INDENT, ("{", "}"), 1, |ctx| {
        if let Some((first, rest)) = stmts.split_first() {
            first.print_to(ctx);
            for stmt in rest {
                ctx.hardbreak();
                stmt.print_to(ctx);
            }
        }
    });
}

#[cfg(test)]
mod tests {
    use super::{format_switch_case_literal, CStmtKind};
    use crate::expr::CValue;
    use crate::pretty::{Print, PrinterCtx};
    use crate::{ModuleArena, ModuleCtx};

    #[test]
    fn switch_case_formats_large_u128_literal_with_parts() {
        let value = (1u128 << 64) + 7;
        assert_eq!(format_switch_case_literal(value), "__rust_u128_from_parts(1ULL, 7ULL)");
    }

    #[test]
    fn switch_statement_prints_formatted_large_case_literal() {
        let arena = ModuleArena::new("");
        let mcx = ModuleCtx(&arena);
        let stmt = mcx.stmt(CStmtKind::Switch {
            expr: mcx.value(CValue::Local(0)),
            cases: vec![((1u128 << 64) + 9, "bb1")],
            default: "bb0",
        });

        let mut ctx = PrinterCtx::new();
        stmt.print_to(&mut ctx);
        let rendered = ctx.finish();
        assert!(rendered.contains("case __rust_u128_from_parts(1ULL, 9ULL):"));
    }
}
