//! This module defines the AST nodes for C expressions.

use crate::pretty::{Print, PrinterCtx, INDENT};
use crate::ty::{print_declarator, CTy};
use crate::ModuleCtx;

/// Represents the values of C variables, parameters, and scalars.
///
/// There are two variants to distinguish between constants and variables,
/// as is done in LLVM IR. We follow the `rustc_codegen_ssa` convention for this representation.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum CValue<'mx> {
    /// A constant scalar
    Scalar(i128),
    /// A constant real literal represented as textual C expression.
    RealLiteral(&'mx str),
    /// A constant scalar with a unique identity so backend metadata can be tracked per value.
    ScalarTyped(i128, u64),
    /// A local variable indexed by a number, in the form `_0`, `_1`, etc.
    Local(usize),
    /// A function name
    Func(&'mx str),
}

/// C expressions.
pub type CExpr<'mx> = &'mx CExprKind<'mx>;

/// C expressions.
#[derive(Debug, Clone)]
pub enum CExprKind<'mx> {
    /// A "raw" C expression, simply a string of C code, which is printed as-is.
    Raw(&'static str),
    /// A value, such as a constant, variable, or function name.
    Value(CValue<'mx>),
    /// A binary operation expression, e.g. `lhs + rhs`.
    Binary { lhs: CExpr<'mx>, rhs: CExpr<'mx>, op: &'static str },
    /// A ternary expression, e.g. `cond ? then_expr : else_expr`.
    Ternary { cond: CExpr<'mx>, then_expr: CExpr<'mx>, else_expr: CExpr<'mx> },
    /// A type cast expression, e.g. `(int) x`.
    Cast { ty: CTy<'mx>, expr: CExpr<'mx> },
    /// A function call expression, e.g. `foo(x, y)`.
    Call { callee: CExpr<'mx>, args: Vec<CExpr<'mx>> },
    /// A member access expression, e.g. `foo.bar` or `foo->bar`.
    Member {
        expr: CExpr<'mx>,
        /// Whether to use the `->` operator instead of `.`.
        arrow: bool,
        field: &'mx str,
    },
    /// A unary operation expression, e.g. `*x` or `&x`.
    Unary { op: &'static str, expr: CExpr<'mx> },
    /// An array indexing expression, e.g. `foo[1]`.
    Index { expr: CExpr<'mx>, index: CExpr<'mx> },
}

impl<'mx> ModuleCtx<'mx> {
    /// Create a new expression.
    pub fn expr(&self, expr: CExprKind<'mx>) -> CExpr<'mx> {
        self.arena().alloc(expr)
    }

    /// Create a new raw expression.
    pub fn raw(&self, raw: &'static str) -> CExpr<'mx> {
        self.expr(CExprKind::Raw(raw))
    }

    /// Create a new value expression.
    pub fn value(&self, value: CValue<'mx>) -> CExpr<'mx> {
        self.expr(CExprKind::Value(value))
    }

    /// Create a new binary expression.
    pub fn binary(&self, lhs: CExpr<'mx>, rhs: CExpr<'mx>, op: &'static str) -> CExpr<'mx> {
        self.expr(CExprKind::Binary { lhs, rhs, op })
    }

    /// Create a new ternary expression.
    pub fn ternary(
        &self,
        cond: CExpr<'mx>,
        then_expr: CExpr<'mx>,
        else_expr: CExpr<'mx>,
    ) -> CExpr<'mx> {
        self.expr(CExprKind::Ternary { cond, then_expr, else_expr })
    }

    /// Create a new cast expression.
    pub fn cast(&self, ty: CTy<'mx>, expr: CExpr<'mx>) -> CExpr<'mx> {
        self.expr(CExprKind::Cast { ty, expr })
    }

    /// Create a new function call expression.
    pub fn call(&self, callee: CExpr<'mx>, args: Vec<CExpr<'mx>>) -> CExpr<'mx> {
        self.expr(CExprKind::Call { callee, args })
    }

    /// Create a new member access expression.
    pub fn member(&self, expr: CExpr<'mx>, field: &'mx str) -> CExpr<'mx> {
        self.expr(CExprKind::Member { expr, field, arrow: false })
    }

    /// Create a new pointer member access expression.
    pub fn member_arrow(&self, expr: CExpr<'mx>, field: &'mx str) -> CExpr<'mx> {
        self.expr(CExprKind::Member { expr, field, arrow: true })
    }

    /// Create a new unary expression.
    pub fn unary(&self, op: &'static str, expr: CExpr<'mx>) -> CExpr<'mx> {
        self.expr(CExprKind::Unary { op, expr })
    }

    /// Create a new indexing expression.
    pub fn index(&self, expr: CExpr<'mx>, index: CExpr<'mx>) -> CExpr<'mx> {
        self.expr(CExprKind::Index { expr, index })
    }
}

fn format_scalar_constant(value: i128) -> String {
    if value >= i64::MIN as i128 && value <= i64::MAX as i128 {
        return value.to_string();
    }

    if value >= 0 && value <= u64::MAX as i128 {
        return format!("{value}ULL");
    }

    let bits = value as u128;
    let hi = (bits >> 64) as u64;
    let lo = bits as u64;
    format!("__rust_i128_from_parts({hi}ULL, {lo}ULL)")
}

impl Print for CValue<'_> {
    fn print_to(&self, ctx: &mut PrinterCtx) {
        match self {
            CValue::Scalar(i) => ctx.word(format_scalar_constant(*i)),
            CValue::RealLiteral(i) => ctx.word(i.to_string()),
            CValue::ScalarTyped(i, _) => ctx.word(format_scalar_constant(*i)),
            CValue::Local(i) => ctx.word(format!("_{}", i)),
            CValue::Func(name) => ctx.word(name.to_string()),
        }
    }
}

impl Print for CExpr<'_> {
    fn print_to(&self, ctx: &mut PrinterCtx) {
        match self {
            CExprKind::Raw(raw) => ctx.word(*raw),
            CExprKind::Value(value) => value.print_to(ctx),
            CExprKind::Binary { lhs, rhs, op } => ctx.ibox_delim(INDENT, ("(", ")"), 0, |ctx| {
                ctx.ibox(-INDENT, |ctx| lhs.print_to(ctx));

                ctx.softbreak();
                ctx.word(*op);
                ctx.nbsp();

                rhs.print_to(ctx);
            }),
            CExprKind::Ternary { cond, then_expr, else_expr } => {
                ctx.ibox_delim(INDENT, ("(", ")"), 0, |ctx| {
                    cond.print_to(ctx);
                    ctx.softbreak();
                    ctx.word("?");
                    ctx.nbsp();
                    then_expr.print_to(ctx);
                    ctx.softbreak();
                    ctx.word(":");
                    ctx.nbsp();
                    else_expr.print_to(ctx);
                })
            }
            CExprKind::Cast { ty, expr } => ctx.ibox(INDENT, |ctx| {
                ctx.word("(");
                print_declarator(*ty, None, ctx);
                ctx.word(")");

                ctx.nbsp();
                expr.print_to(ctx);
            }),
            CExprKind::Call { callee, args } => ctx.ibox(INDENT, |ctx| {
                let need_callee_parens = matches!(
                    callee,
                    CExprKind::Binary { .. }
                        | CExprKind::Ternary { .. }
                        | CExprKind::Cast { .. }
                        | CExprKind::Unary { .. }
                );
                if need_callee_parens {
                    ctx.word("(");
                    callee.print_to(ctx);
                    ctx.word(")");
                } else {
                    callee.print_to(ctx);
                }
                ctx.cbox_delim(INDENT, ("(", ")"), 0, |ctx| {
                    ctx.seperated(",", args, |ctx, arg| arg.print_to(ctx));
                });
            }),
            CExprKind::Member { expr, arrow, field } => ctx.cbox(INDENT, |ctx| {
                expr.print_to(ctx);
                ctx.zerobreak();
                if *arrow {
                    ctx.word("->");
                } else {
                    ctx.word(".");
                }
                ctx.word(field.to_string());
            }),
            CExprKind::Unary { op, expr } => ctx.cbox(INDENT, |ctx| {
                ctx.word(*op);
                expr.print_to(ctx);
            }),
            CExprKind::Index { expr, index } => ctx.cbox(INDENT, |ctx| {
                expr.print_to(ctx);
                ctx.word("[");
                index.print_to(ctx);
                ctx.word("]");
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{format_scalar_constant, CValue};
    use crate::pretty::{Print, PrinterCtx};

    #[test]
    fn scalar_prints_decimal_within_i64_range() {
        assert_eq!(format_scalar_constant(-42), "-42");
        assert_eq!(format_scalar_constant(i64::MAX as i128), i64::MAX.to_string());
    }

    #[test]
    fn scalar_prints_ull_suffix_for_u64_range() {
        let value = (i64::MAX as u128 + 7) as i128;
        assert_eq!(format_scalar_constant(value), format!("{value}ULL"));
    }

    #[test]
    fn scalar_prints_split_parts_for_large_128_values() {
        assert_eq!(
            format_scalar_constant(i128::MIN),
            "__rust_i128_from_parts(9223372036854775808ULL, 0ULL)"
        );

        let mut ctx = PrinterCtx::new();
        CValue::ScalarTyped(i128::MIN, 0).print_to(&mut ctx);
        assert_eq!(ctx.finish(), "__rust_i128_from_parts(9223372036854775808ULL, 0ULL)");
    }
}
