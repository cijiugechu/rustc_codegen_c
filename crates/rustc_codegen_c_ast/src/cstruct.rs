use crate::pretty::{Print, PrinterCtx, INDENT};
use crate::ty::{print_declarator, CTy};

#[derive(Debug, Clone)]
pub struct CStructField<'mx> {
    pub name: &'mx str,
    pub ty: CTy<'mx>,
}

#[derive(Debug, Clone)]
pub struct CStructDef<'mx> {
    pub name: &'mx str,
    pub fields: Vec<CStructField<'mx>>,
}

impl Print for CStructDef<'_> {
    fn print_to(&self, ctx: &mut PrinterCtx) {
        ctx.ibox(0, |ctx| {
            ctx.word("struct");
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
