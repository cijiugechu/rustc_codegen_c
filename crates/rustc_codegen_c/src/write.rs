use std::fs;
use std::io::Write;
use std::process::{Command, Stdio};

use rustc_codegen_ssa::back::write::{CodegenContext, ModuleConfig};
use rustc_codegen_ssa::{CompiledModule, ModuleCodegen};
use rustc_errors::DiagCtxtHandle;
use rustc_session::config::OutputType;
use tracing::error;

pub(crate) fn codegen(
    cgcx: &CodegenContext<crate::CCodegen>,
    module: ModuleCodegen<String>,
    _config: &ModuleConfig,
) -> CompiledModule {
    let dcx = cgcx.create_dcx();
    let dcx = dcx.handle();
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
    // FIXME: configure cc
    // FIXME: handle long command line (windows)
    // FIXME: flush_linked_file (windows)
    let mut cmd = Command::new("clang");
    cmd.arg(&c_out).arg("-o").arg(&obj_out).arg("-c");
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
