use rustc_ast::{InlineAsmOptions, InlineAsmTemplatePiece};
use rustc_codegen_ssa::traits::{AsmBuilderMethods, InlineAsmOperandRef};
use rustc_middle::ty::{Instance, TyCtxt};
use rustc_span::Span;

use crate::builder::Builder;

const INLINE_ASM_UNSUPPORTED_MSG: &str =
    "rustc_codegen_c currently doesn't support inline assembly.";
const INLINE_ASM_UNWIND_UNSUPPORTED_MSG: &str =
    "rustc_codegen_c doesn't support unwinding from inline assembly.";

fn validate_inline_asm_policy(
    options: InlineAsmOptions,
    has_catch_funclet: bool,
) -> &'static str {
    if options.contains(InlineAsmOptions::MAY_UNWIND) || has_catch_funclet {
        INLINE_ASM_UNWIND_UNSUPPORTED_MSG
    } else {
        INLINE_ASM_UNSUPPORTED_MSG
    }
}

fn inline_asm_diag_span(line_spans: &[Span], instance: Instance<'_>, tcx: TyCtxt<'_>) -> Span {
    line_spans.first().copied().unwrap_or_else(|| tcx.def_span(instance.def_id()))
}

impl<'tcx, 'mx> AsmBuilderMethods<'tcx> for Builder<'_, 'tcx, 'mx> {
    fn codegen_inline_asm(
        &mut self,
        template: &[InlineAsmTemplatePiece],
        operands: &[InlineAsmOperandRef<'tcx, Self>],
        options: InlineAsmOptions,
        line_spans: &[rustc_span::Span],
        instance: Instance<'_>,
        dest: Option<Self::BasicBlock>,
        catch_funclet: Option<(Self::BasicBlock, Option<&Self::Funclet>)>,
    ) {
        let _ = (template, operands, dest);

        let msg = validate_inline_asm_policy(options, catch_funclet.is_some());
        let span = inline_asm_diag_span(line_spans, instance, self.cx.tcx);
        self.cx.tcx.sess.dcx().span_fatal(span, msg);
    }
}

#[cfg(test)]
mod tests {
    use super::{
        INLINE_ASM_UNSUPPORTED_MSG, INLINE_ASM_UNWIND_UNSUPPORTED_MSG, validate_inline_asm_policy,
    };
    use rustc_ast::InlineAsmOptions;

    #[test]
    fn validate_inline_asm_policy_rejects_non_unwind_inline_asm() {
        assert_eq!(
            validate_inline_asm_policy(InlineAsmOptions::empty(), false),
            INLINE_ASM_UNSUPPORTED_MSG
        );
    }

    #[test]
    fn validate_inline_asm_policy_rejects_may_unwind_inline_asm() {
        assert_eq!(
            validate_inline_asm_policy(InlineAsmOptions::MAY_UNWIND, false),
            INLINE_ASM_UNWIND_UNSUPPORTED_MSG
        );
    }

    #[test]
    fn validate_inline_asm_policy_rejects_catch_funclet_inline_asm() {
        assert_eq!(
            validate_inline_asm_policy(InlineAsmOptions::empty(), true),
            INLINE_ASM_UNWIND_UNSUPPORTED_MSG
        );
    }
}
