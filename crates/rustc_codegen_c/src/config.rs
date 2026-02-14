/// Configuration of rustc_codegen_c as passed in through `-Cllvm-args` and env vars.
#[derive(Debug, Clone, Copy, Default)]
pub struct BackendConfig {
    /// Enable C toolchain LTO (`-flto`) for C compilation.
    pub c_lto: bool,
    /// Enable C stack protector instrumentation.
    ///
    /// Defaults to `false` to match Rust's default behavior.
    pub c_stack_protector: bool,
}

impl BackendConfig {
    /// Parse backend options from `-Cllvm-args`.
    ///
    /// Supported options:
    /// - `c-lto`
    /// - `c-lto=yes|no|true|false|1|0|on|off`
    /// - `c-stack-protector`
    /// - `c-stack-protector=yes|no|true|false|1|0|on|off`
    ///
    /// `CG_C_LTO=1` and `CG_C_STACK_PROTECTOR=1` are accepted as env fallbacks.
    pub fn from_opts(opts: &[String]) -> Result<Self, String> {
        let mut config = BackendConfig {
            c_lto: std::env::var("CG_C_LTO")
                .ok()
                .is_some_and(|v| parse_bool_flag(&v).unwrap_or(false)),
            c_stack_protector: std::env::var("CG_C_STACK_PROTECTOR")
                .ok()
                .is_some_and(|v| parse_bool_flag(&v).unwrap_or(false)),
        };

        for opt in opts {
            if opt.starts_with("-import-instr-limit") {
                // rust's build system may set this; ignore it for non-LLVM backends.
                continue;
            }

            // Keep `cg-c-lto` as a compatibility alias while preferring `c-lto`.
            if opt == "c-lto" || opt == "cg-c-lto" {
                config.c_lto = true;
                continue;
            }

            if let Some(value) = opt.strip_prefix("c-lto=") {
                config.c_lto = parse_bool_flag(value).ok_or_else(|| {
                    format!("invalid value for `c-lto`: `{value}` (expected bool-like value)")
                })?;
                continue;
            }

            if let Some(value) = opt.strip_prefix("cg-c-lto=") {
                config.c_lto = parse_bool_flag(value).ok_or_else(|| {
                    format!("invalid value for `cg-c-lto`: `{value}` (expected bool-like value)")
                })?;
                continue;
            }

            // Keep `cg-c-stack-protector` as a compatibility alias while preferring
            // `c-stack-protector`.
            if opt == "c-stack-protector" || opt == "cg-c-stack-protector" {
                config.c_stack_protector = true;
                continue;
            }

            if let Some(value) = opt.strip_prefix("c-stack-protector=") {
                config.c_stack_protector = parse_bool_flag(value).ok_or_else(|| {
                    format!(
                        "invalid value for `c-stack-protector`: `{value}` (expected bool-like value)"
                    )
                })?;
                continue;
            }

            if let Some(value) = opt.strip_prefix("cg-c-stack-protector=") {
                config.c_stack_protector = parse_bool_flag(value).ok_or_else(|| {
                    format!(
                        "invalid value for `cg-c-stack-protector`: `{value}` (expected bool-like value)"
                    )
                })?;
                continue;
            }

            if opt.starts_with("c-") || opt.starts_with("cg-c-") {
                return Err(format!("unknown rustc_codegen_c option `{opt}`"));
            }
        }

        Ok(config)
    }
}

fn parse_bool_flag(value: &str) -> Option<bool> {
    match value {
        "1" | "true" | "TRUE" | "yes" | "on" => Some(true),
        "0" | "false" | "FALSE" | "no" | "off" => Some(false),
        _ => None,
    }
}
