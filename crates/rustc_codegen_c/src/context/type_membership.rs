use rustc_codegen_ssa::traits::TypeMembershipCodegenMethods;

use crate::context::CodegenCx;

impl<'tcx, 'mx> TypeMembershipCodegenMethods<'tcx> for CodegenCx<'tcx, 'mx> {}
