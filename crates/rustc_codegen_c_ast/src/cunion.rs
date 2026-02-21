use crate::expr::CValue;
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
                        print_declarator(first.ty, Some(CValue::Func(first.name)), ctx);
                        ctx.word(";");
                    });
                    for field in rest {
                        ctx.hardbreak();
                        ctx.ibox(INDENT, |ctx| {
                            print_declarator(field.ty, Some(CValue::Func(field.name)), ctx);
                            ctx.word(";");
                        });
                    }
                }
            });
            ctx.word(";");
        });
    }
}

#[cfg(test)]
mod tests {
    use super::{CUnionDef, CUnionField};
    use crate::pretty::{Print, PrinterCtx};
    use crate::ty::{CTy, CTyKind, CUintTy};
    use rustc_data_structures::intern::Interned;

    #[test]
    fn union_fields_keep_array_brackets_after_identifier() {
        let bytes_array = CTy::Ref(Interned::new_unchecked(&CTyKind::Array(
            CTy::UInt(CUintTy::U8),
            4,
        )));
        let def = CUnionDef {
            name: "BytesOrWord",
            fields: vec![
                CUnionField { name: "bytes", ty: bytes_array },
                CUnionField { name: "word", ty: CTy::UInt(CUintTy::U32) },
            ],
        };
        let mut ctx = PrinterCtx::new();
        def.print_to(&mut ctx);
        let printed = ctx.finish();

        assert!(printed.contains("uint8_t bytes[4];"), "{printed}");
        assert!(printed.contains("uint32_t word;"), "{printed}");
    }
}
