#![feature(rustc_private)]

extern crate rustc_abi;
extern crate rustc_ast;
extern crate rustc_codegen_ssa;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_fluent_macro;
extern crate rustc_hash;
extern crate rustc_hir;
extern crate rustc_metadata;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_symbol_mangling;
extern crate rustc_target;
extern crate rustc_type_ir;
extern crate tracing;

use std::path::PathBuf;
use std::sync::Arc;

use rustc_ast::expand::allocator::AllocatorKind;
use rustc_codegen_ssa::back::lto::{SerializedModule, ThinModule};
use rustc_codegen_ssa::back::write::{
    CodegenContext, FatLtoInput, ModuleConfig, OngoingCodegen, TargetMachineFactoryFn,
};
use rustc_codegen_ssa::base::codegen_crate;
use rustc_codegen_ssa::target_features::cfg_target_feature;
pub use rustc_codegen_ssa::traits::CodegenBackend;
use rustc_codegen_ssa::traits::{
    ExtraBackendMethods, ModuleBufferMethods, ThinBufferMethods, WriteBackendMethods,
};
use rustc_codegen_ssa::{CodegenResults, CompiledModule, ModuleCodegen, TargetConfig};
use rustc_data_structures::fx::FxIndexMap;
use rustc_errors::DiagCtxtHandle;
use rustc_middle::dep_graph::{WorkProduct, WorkProductId};
use rustc_middle::ty::TyCtxt;
use rustc_middle::util::Providers;
use rustc_session::config::{OptLevel, OutputFilenames};
use rustc_session::Session;
use rustc_target::spec::PanicStrategy;

mod allocator;
mod archive;
mod base;
mod builder;
mod config;
mod context;
mod include_plan;
mod write;

rustc_fluent_macro::fluent_messages! { "../messages.ftl" }

#[derive(Clone)]
pub struct CCodegen {}

const PANIC_ABORT_REQUIRED_MSG: &str =
    "rustc_codegen_c currently supports only `panic=abort`.";

fn validate_panic_strategy(strategy: PanicStrategy) -> Result<(), &'static str> {
    if matches!(strategy, PanicStrategy::Abort) {
        Ok(())
    } else {
        Err(PANIC_ABORT_REQUIRED_MSG)
    }
}

fn enforce_panic_abort(tcx: TyCtxt<'_>) {
    if let Err(msg) = validate_panic_strategy(tcx.sess.panic_strategy()) {
        tcx.sess.dcx().fatal(msg);
    }
}

impl CodegenBackend for CCodegen {
    fn locale_resource(&self) -> &'static str {
        crate::DEFAULT_LOCALE_RESOURCE
    }

    fn provide(&self, providers: &mut Providers) {
        providers.global_backend_features = |tcx, ()| {
            let mut features: Vec<String> = tcx
                .sess
                .target
                .features
                .split(',')
                .filter(|feature| !feature.is_empty())
                .map(str::to_owned)
                .collect();

            features.extend(
                tcx.sess
                    .opts
                    .cg
                    .target_feature
                    .split(',')
                    .filter(|feature| !feature.is_empty())
                    .map(str::to_owned),
            );

            // `neon` is ABI-required on AArch64 even when target spec does not list it.
            if tcx.sess.target.arch == "aarch64" && !features.iter().any(|f| f == "+neon") {
                features.push("+neon".to_string());
            }

            features
        }
    }

    fn target_config(&self, sess: &Session) -> TargetConfig {
        let (unstable_target_features, target_features) = cfg_target_feature(sess, |feature| {
            if sess.target.arch == "x86_64" && (feature == "x87" || feature == "sse2") {
                return true;
            }
            if sess.target.arch == "aarch64" && feature == "neon" {
                return true;
            }

            sess.target.features.split(',').any(|target_feature| {
                target_feature
                    .strip_prefix('+')
                    .is_some_and(|target_feature| target_feature == feature)
            })
        });

        TargetConfig {
            target_features,
            unstable_target_features,
            has_reliable_f16: true,
            has_reliable_f16_math: true,
            has_reliable_f128: true,
            has_reliable_f128_math: true,
        }
    }

    fn codegen_crate(&self, tcx: TyCtxt<'_>) -> Box<dyn std::any::Any> {
        enforce_panic_abort(tcx);

        let target_cpu = match tcx.sess.opts.cg.target_cpu {
            Some(ref name) => name,
            None => tcx.sess.target.cpu.as_ref(),
        }
        .to_owned();

        let ongoing_codegen = codegen_crate(self.clone(), tcx, target_cpu);
        Box::new(ongoing_codegen)
    }

    fn join_codegen(
        &self,
        ongoing_codegen: Box<dyn std::any::Any>,
        sess: &Session,
        _outputs: &OutputFilenames,
    ) -> (CodegenResults, FxIndexMap<WorkProductId, WorkProduct>) {
        ongoing_codegen.downcast::<OngoingCodegen<Self>>().expect("expected CCodegen").join(sess)
    }
}

impl ExtraBackendMethods for CCodegen {
    fn supports_parallel(&self) -> bool {
        false
    }

    fn codegen_allocator(
        &self,
        tcx: TyCtxt<'_>,
        module_name: &str,
        kind: AllocatorKind,
        alloc_error_handler_kind: AllocatorKind,
    ) -> Self::Module {
        allocator::codegen(tcx, module_name, kind, alloc_error_handler_kind)
    }

    fn compile_codegen_unit(
        &self,
        tcx: TyCtxt<'_>,
        cgu_name: rustc_span::Symbol,
    ) -> (ModuleCodegen<Self::Module>, u64) {
        base::compile_codegen_unit(tcx, cgu_name)
    }

    fn target_machine_factory(
        &self,
        _sess: &Session,
        _opt_level: OptLevel,
        _target_features: &[String],
    ) -> TargetMachineFactoryFn<Self> {
        Arc::new(|_| Ok(()))
    }
}

pub struct ModuleBuffer;

impl ModuleBufferMethods for ModuleBuffer {
    fn data(&self) -> &[u8] {
        unimplemented!()
    }
}

pub struct ThinBuffer;

impl ThinBufferMethods for ThinBuffer {
    fn data(&self) -> &[u8] {
        unimplemented!()
    }
}

impl WriteBackendMethods for CCodegen {
    type Module = String;
    type TargetMachine = ();
    type TargetMachineError = ();
    type ModuleBuffer = ModuleBuffer;
    type ThinData = ();
    type ThinBuffer = ThinBuffer;

    fn run_and_optimize_fat_lto(
        _cgcx: &CodegenContext<Self>,
        _exported_symbols_for_lto: &[String],
        _each_linked_rlib_for_lto: &[PathBuf],
        _modules: Vec<FatLtoInput<Self>>,
    ) -> ModuleCodegen<Self::Module> {
        unimplemented!()
    }

    fn run_thin_lto(
        _cgcx: &CodegenContext<Self>,
        _exported_symbols_for_lto: &[String],
        _each_linked_rlib_for_lto: &[PathBuf],
        _modules: Vec<(String, Self::ThinBuffer)>,
        _cached_modules: Vec<(SerializedModule<Self::ModuleBuffer>, WorkProduct)>,
    ) -> (Vec<ThinModule<Self>>, Vec<WorkProduct>) {
        unimplemented!()
    }

    fn print_pass_timings(&self) {
        unimplemented!()
    }

    fn print_statistics(&self) {
        unimplemented!()
    }

    fn optimize(
        _cgcx: &CodegenContext<Self>,
        _dcx: DiagCtxtHandle<'_>,
        _module: &mut ModuleCodegen<Self::Module>,
        _config: &ModuleConfig,
    ) {
    }

    fn optimize_thin(
        _cgcx: &CodegenContext<Self>,
        _thin: ThinModule<Self>,
    ) -> ModuleCodegen<Self::Module> {
        unimplemented!()
    }

    fn codegen(
        cgcx: &CodegenContext<Self>,
        module: ModuleCodegen<Self::Module>,
        config: &ModuleConfig,
    ) -> CompiledModule {
        write::codegen(cgcx, module, config)
    }

    fn prepare_thin(_module: ModuleCodegen<Self::Module>) -> (String, Self::ThinBuffer) {
        unimplemented!()
    }

    fn serialize_module(
        _module: rustc_codegen_ssa::ModuleCodegen<Self::Module>,
    ) -> (String, Self::ModuleBuffer) {
        unimplemented!()
    }
}

/// This is the entrypoint for a hot plugged codegen backend.
#[no_mangle]
pub fn __rustc_codegen_backend() -> Box<dyn CodegenBackend> {
    Box::new(CCodegen {})
}

#[cfg(test)]
mod tests {
    use super::{PANIC_ABORT_REQUIRED_MSG, validate_panic_strategy};
    use rustc_target::spec::PanicStrategy;

    #[test]
    fn validate_panic_strategy_accepts_abort() {
        assert!(validate_panic_strategy(PanicStrategy::Abort).is_ok());
    }

    #[test]
    fn validate_panic_strategy_rejects_unwind() {
        assert_eq!(validate_panic_strategy(PanicStrategy::Unwind), Err(PANIC_ABORT_REQUIRED_MSG));
    }
}
