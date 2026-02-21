use std::process::Command;

use clap::Args;

use crate::manifest::Manifest;
use crate::Run;

/// Invoke cargo with rustc_codegen_c backend injected via rustflags.
#[derive(Args, Debug)]
pub struct CargoCommand {
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    args: Vec<String>,

    #[arg(short, long)]
    pub verbose: bool,
}

impl Run for CargoCommand {
    const STEP_DISPLAY_NAME: &'static str = "CARGO";

    fn run(&self, manifest: &Manifest) {
        manifest.prepare();

        let display =
            if self.args.is_empty() { "(no args)".to_string() } else { self.args.join(" ") };
        self.log_action_start("running", display);

        let mut command = Command::new("cargo");
        command.args(&self.args);
        configure_cargo_command_env(&mut command, manifest);
        self.command_status("cargo", &mut command);
    }

    fn verbose(&self) -> bool {
        self.verbose
    }
}

pub(crate) fn configure_cargo_command_env(command: &mut Command, manifest: &Manifest) {
    configure_cargo_command_env_with_extra_rustflags(command, manifest, &[]);
}

pub(crate) fn configure_cargo_command_env_with_extra_rustflags(
    command: &mut Command,
    manifest: &Manifest,
    extra_rustflags: &[&str],
) {
    let encoded_rustflags = std::env::var("CARGO_ENCODED_RUSTFLAGS").ok();
    let rustflags = std::env::var("RUSTFLAGS").ok();
    let mut merged_rustflags = strip_codegen_backend_and_panic_flags(rustflags_from_vars(
        encoded_rustflags.as_deref(),
        rustflags.as_deref(),
    ));
    merged_rustflags.extend(manifest.backend_rustflags());
    merged_rustflags.extend(extra_rustflags.iter().map(|flag| (*flag).to_string()));

    command.env("CARGO_ENCODED_RUSTFLAGS", merged_rustflags.join("\x1f"));

    let cflags = std::env::var("CFLAGS").ok();
    command.env("CFLAGS", merge_cflags(cflags.as_deref(), &manifest.runtime_c_include_flag()));
}

fn rustflags_from_vars(encoded: Option<&str>, plain: Option<&str>) -> Vec<String> {
    if let Some(encoded) = encoded {
        if encoded.is_empty() {
            return Vec::new();
        }
        return encoded.split('\x1f').map(str::to_string).collect();
    }

    plain
        .unwrap_or_default()
        .split_whitespace()
        .filter(|flag| !flag.is_empty())
        .map(str::to_string)
        .collect()
}

fn strip_codegen_backend_and_panic_flags(flags: Vec<String>) -> Vec<String> {
    let mut out = Vec::with_capacity(flags.len());
    let mut i = 0;
    while i < flags.len() {
        let flag = &flags[i];

        if flag.starts_with("-Zcodegen-backend=") || flag.starts_with("-Cpanic=") {
            i += 1;
            continue;
        }

        if flag == "-Z" && flags.get(i + 1).is_some_and(|next| next.starts_with("codegen-backend="))
        {
            i += 2;
            continue;
        }

        if flag == "-C" && flags.get(i + 1).is_some_and(|next| next.starts_with("panic=")) {
            i += 2;
            continue;
        }

        out.push(flag.clone());
        i += 1;
    }

    out
}

fn merge_cflags(existing: Option<&str>, include_flag: &str) -> String {
    let mut merged: Vec<String> = existing
        .unwrap_or_default()
        .split_whitespace()
        .filter(|flag| !flag.is_empty())
        .map(str::to_string)
        .collect();

    if !merged.iter().any(|flag| flag == include_flag) {
        merged.push(include_flag.to_string());
    }

    merged.join(" ")
}

#[cfg(test)]
mod tests {
    use super::{merge_cflags, rustflags_from_vars, strip_codegen_backend_and_panic_flags};

    #[test]
    fn rustflags_prefers_encoded_over_plain() {
        let flags = rustflags_from_vars(Some("-Cdebuginfo=2\x1f-Zcodegen-backend=foo"), Some("-O"));
        assert_eq!(flags, vec!["-Cdebuginfo=2", "-Zcodegen-backend=foo"]);
    }

    #[test]
    fn strip_codegen_backend_and_panic_replaces_both_forms() {
        let flags = vec![
            "-Cdebuginfo=2".to_string(),
            "-Zcodegen-backend=old".to_string(),
            "-Z".to_string(),
            "codegen-backend=older".to_string(),
            "-Cpanic=abort".to_string(),
            "-C".to_string(),
            "panic=unwind".to_string(),
        ];

        let stripped = strip_codegen_backend_and_panic_flags(flags);
        assert_eq!(stripped, vec!["-Cdebuginfo=2"]);
    }

    #[test]
    fn strip_codegen_backend_and_panic_preserves_other_codegen_flags() {
        let flags = vec![
            "-C".to_string(),
            "debuginfo=2".to_string(),
            "-Copt-level=2".to_string(),
            "-Z".to_string(),
            "codegen-backend=old".to_string(),
            "-C".to_string(),
            "panic=abort".to_string(),
        ];

        let stripped = strip_codegen_backend_and_panic_flags(flags);
        assert_eq!(stripped, vec!["-C", "debuginfo=2", "-Copt-level=2"]);
    }

    #[test]
    fn merge_cflags_dedups_runtime_include() {
        let include = "-I/abs/repo/rust_runtime";
        assert_eq!(
            merge_cflags(Some("-Wall -I/abs/repo/rust_runtime"), include),
            "-Wall -I/abs/repo/rust_runtime"
        );
        assert_eq!(merge_cflags(Some("-Wall"), include), "-Wall -I/abs/repo/rust_runtime");
    }
}
