use std::fs;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

use rustc_codegen_ssa::back::write::{CodegenContext, ModuleConfig};
use rustc_codegen_ssa::{CompiledModule, ModuleCodegen};
use rustc_errors::DiagCtxtHandle;
use rustc_session::config::{OptLevel, OutputType};
use tracing::error;

use crate::config::BackendConfig;

fn c_opt_flag_from_rust_opt_level(level: OptLevel) -> &'static str {
    match level {
        OptLevel::No => "-O0",
        OptLevel::Less => "-O1",
        OptLevel::More => "-O2",
        OptLevel::Aggressive => "-O3",
        OptLevel::Size => "-Os",
        OptLevel::SizeMin => "-Oz",
    }
}

fn build_c_compiler_command(
    cgcx: &CodegenContext<crate::CCodegen>,
    backend_config: BackendConfig,
    c_out: &Path,
    obj_out: &Path,
) -> Command {
    let cc = std::env::var("CC")
        .ok()
        .filter(|v| !v.trim().is_empty())
        .unwrap_or_else(|| "clang".to_string());
    let mut cmd = Command::new(cc);
    cmd.arg(c_opt_flag_from_rust_opt_level(cgcx.opts.optimize))
        .arg(c_out)
        .arg("-o")
        .arg(obj_out)
        .arg("-c");
    if !backend_config.c_stack_protector {
        cmd.arg("-fno-stack-protector");
    }
    if backend_config.c_lto {
        cmd.arg("-flto");
    }
    cmd
}

pub(crate) fn codegen(
    cgcx: &CodegenContext<crate::CCodegen>,
    module: ModuleCodegen<String>,
    _config: &ModuleConfig,
) -> CompiledModule {
    let dcx = cgcx.create_dcx();
    let dcx = dcx.handle();
    let backend_config = BackendConfig::from_opts(&cgcx.opts.cg.llvm_args)
        .map_err(|err| dcx.fatal(format!("invalid rustc_codegen_c option: {err}")))
        .unwrap();
    let obj_out = cgcx.output_filenames.temp_path_for_cgu(
        OutputType::Object,
        &module.name,
        cgcx.invocation_temp.as_deref(),
    );
    let c_out = obj_out.with_extension("c");

    // output c source code
    // dcx.fatal() exits so unwrap is fine
    let c_out_file = fs::File::create(&c_out)
        .map_err(|err| dcx.fatal(format!("Failed to create C source file: {err}")))
        .unwrap();
    writeln!(&c_out_file, "// file: {}.c", module.name)
        .map_err(|err| dcx.fatal(format!("Failed to write to C source file: {err}")))
        .unwrap();
    write!(&c_out_file, "{}", module.module_llvm)
        .map_err(|err| dcx.fatal(format!("Failed to write to C source file: {err}")))
        .unwrap();

    // invoke cc to compile
    // FIXME: handle long command line (windows)
    // FIXME: flush_linked_file (windows)
    let mut cmd = build_c_compiler_command(cgcx, backend_config, &c_out, &obj_out);
    let output = match cmd
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .and_then(|child| child.wait_with_output())
    {
        Ok(output) => output,
        Err(e) => {
            error!("failed to spawn C compiler: {}", e);
            dcx.fatal(format!("failed to spawn C compiler: {e}"));
        }
    };

    if !output.status.success() {
        error!("compiler stderr:\n{}", String::from_utf8_lossy(&output.stderr));
        error!("compiler stdout:\n{}", String::from_utf8_lossy(&output.stdout));
        dcx.fatal("C compiler failed to compile module");
    }

    module.into_compiled_module(
        true,
        false,
        false,
        false,
        false,
        &cgcx.output_filenames,
        cgcx.invocation_temp.as_deref(),
    )
}

pub(crate) fn link(
    _cgcx: &CodegenContext<crate::CCodegen>,
    _dcx: DiagCtxtHandle<'_>,
    mut _modules: Vec<ModuleCodegen<String>>,
) -> ModuleCodegen<String> {
    unimplemented!();
}
