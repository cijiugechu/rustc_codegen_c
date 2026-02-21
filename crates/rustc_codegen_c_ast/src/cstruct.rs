use crate::expr::CValue;
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
    use super::{CStructDef, CStructField};
    use crate::pretty::{Print, PrinterCtx};
    use crate::ty::{CIntTy, CTy, CTyKind, CUintTy};
    use rustc_data_structures::intern::Interned;

    #[test]
    fn struct_fields_keep_array_brackets_after_identifier() {
        let bytes_array = CTy::Ref(Interned::new_unchecked(&CTyKind::Array(
            CTy::UInt(CUintTy::U8),
            4,
        )));
        let def = CStructDef {
            name: "Packet",
            fields: vec![
                CStructField { name: "bytes", ty: bytes_array },
                CStructField { name: "tag", ty: CTy::Int(CIntTy::I32) },
            ],
        };
        let mut ctx = PrinterCtx::new();
        def.print_to(&mut ctx);
        let printed = ctx.finish();

        assert!(printed.contains("uint8_t bytes[4];"), "{printed}");
        assert!(printed.contains("int32_t tag;"), "{printed}");
    }
}
