use crate::pretty::{Print, PrinterCtx, INDENT};
use crate::ty::{print_declarator, CTy};

#[derive(Debug, Clone)]
pub struct CUnionField<'mx> {
    pub name: &'mx str,
    pub ty: CTy<'mx>,
}

#[derive(Debug, Clone)]
pub struct CUnionDef<'mx> {
    pub name: &'mx str,
    pub fields: Vec<CUnionField<'mx>>,
}

impl Print for CUnionDef<'_> {
    fn print_to(&self, ctx: &mut PrinterCtx) {
        ctx.ibox(0, |ctx| {
            ctx.word("union");
            ctx.nbsp();
            ctx.word(self.name.to_string());
            ctx.nbsp();
            ctx.cbox_delim(INDENT, ("{", "}"), 1, |ctx| {
                if let Some((first, rest)) = self.fields.split_first() {
                    ctx.ibox(INDENT, |ctx| {
                        print_declarator(first.ty, None, ctx);
                        ctx.nbsp();
                        ctx.word(first.name.to_string());
                        ctx.word(";");
                    });
                    for field in rest {
                        ctx.hardbreak();
                        ctx.ibox(INDENT, |ctx| {
                            print_declarator(field.ty, None, ctx);
                            ctx.nbsp();
                            ctx.word(field.name.to_string());
                            ctx.word(";");
                        });
                    }
                }
            });
            ctx.word(";");
        });
    }
}
