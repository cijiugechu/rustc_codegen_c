/// Configuration of rustc_codegen_c as passed in through `-Cllvm-args` and env vars.
#[derive(Debug, Clone, Copy, Default)]
pub struct BackendConfig {
    /// Enable C toolchain LTO (`-flto`) for C compilation.
    pub c_lto: bool,
    /// Enable C stack protector instrumentation.
    ///
    /// Defaults to `false` to match Rust's default behavior.
    pub c_stack_protector: bool,
    /// C language standard used by the external C compiler.
    ///
    /// Defaults to C99 for broad compatibility.
    pub c_std: CStandard,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CStandard {
    C99,
    C11,
    C17,
    C23,
    Gnu99,
    Gnu11,
    Gnu17,
    Gnu23,
}

impl Default for CStandard {
    fn default() -> Self {
        Self::C99
    }
}

impl CStandard {
    pub fn parse(value: &str) -> Option<Self> {
        match value {
            "c99" => Some(Self::C99),
            "c11" => Some(Self::C11),
            "c17" => Some(Self::C17),
            "c23" => Some(Self::C23),
            "gnu99" => Some(Self::Gnu99),
            "gnu11" => Some(Self::Gnu11),
            "gnu17" => Some(Self::Gnu17),
            "gnu23" => Some(Self::Gnu23),
            _ => None,
        }
    }

    pub fn as_flag(self) -> &'static str {
        match self {
            Self::C99 => "c99",
            Self::C11 => "c11",
            Self::C17 => "c17",
            Self::C23 => "c23",
            Self::Gnu99 => "gnu99",
            Self::Gnu11 => "gnu11",
            Self::Gnu17 => "gnu17",
            Self::Gnu23 => "gnu23",
        }
    }

    pub fn level(self) -> u16 {
        match self {
            Self::C99 | Self::Gnu99 => 1999,
            Self::C11 | Self::Gnu11 => 2011,
            Self::C17 | Self::Gnu17 => 2017,
            Self::C23 | Self::Gnu23 => 2023,
        }
    }

    pub fn supported_values() -> &'static str {
        "c99|c11|c17|c23|gnu99|gnu11|gnu17|gnu23"
    }
}

impl BackendConfig {
    /// Parse backend options from `-Cllvm-args`.
    ///
    /// Supported options:
    /// - `c-lto`
    /// - `c-lto=yes|no|true|false|1|0|on|off`
    /// - `c-stack-protector`
    /// - `c-stack-protector=yes|no|true|false|1|0|on|off`
    /// - `c-std=c99|c11|c17|c23|gnu99|gnu11|gnu17|gnu23`
    ///
    /// `CG_C_LTO=1`, `CG_C_STACK_PROTECTOR=1`, and `CG_C_STD=<dialect>` are accepted as env
    /// fallbacks.
    pub fn from_opts(opts: &[String]) -> Result<Self, String> {
        let c_std = match std::env::var("CG_C_STD") {
            Ok(value) => CStandard::parse(value.trim()).ok_or_else(|| {
                format!(
                    "invalid value for `CG_C_STD`: `{value}` (supported: {})",
                    CStandard::supported_values()
                )
            })?,
            Err(_) => CStandard::default(),
        };

        let mut config = BackendConfig {
            c_lto: std::env::var("CG_C_LTO")
                .ok()
                .is_some_and(|v| parse_bool_flag(&v).unwrap_or(false)),
            c_stack_protector: std::env::var("CG_C_STACK_PROTECTOR")
                .ok()
                .is_some_and(|v| parse_bool_flag(&v).unwrap_or(false)),
            c_std,
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

            if let Some(value) = opt.strip_prefix("c-std=") {
                config.c_std = CStandard::parse(value).ok_or_else(|| {
                    format!(
                        "invalid value for `c-std`: `{value}` (supported: {})",
                        CStandard::supported_values()
                    )
                })?;
                continue;
            }

            if let Some(value) = opt.strip_prefix("cg-c-std=") {
                config.c_std = CStandard::parse(value).ok_or_else(|| {
                    format!(
                        "invalid value for `cg-c-std`: `{value}` (supported: {})",
                        CStandard::supported_values()
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

#[cfg(test)]
mod tests {
    use std::sync::{Mutex, OnceLock};

    use super::{BackendConfig, CStandard};

    fn env_lock() -> std::sync::MutexGuard<'static, ()> {
        static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        LOCK.get_or_init(|| Mutex::new(())).lock().unwrap()
    }

    fn swap_env(value: Option<&str>) -> Option<String> {
        let prev = std::env::var("CG_C_STD").ok();
        match value {
            Some(v) => {
                // SAFETY: test process-local env mutation.
                unsafe { std::env::set_var("CG_C_STD", v) }
            }
            None => {
                // SAFETY: test process-local env mutation.
                unsafe { std::env::remove_var("CG_C_STD") }
            }
        }
        prev
    }

    fn restore_env(prev: Option<String>) {
        match prev {
            Some(v) => {
                // SAFETY: test process-local env mutation.
                unsafe { std::env::set_var("CG_C_STD", v) }
            }
            None => {
                // SAFETY: test process-local env mutation.
                unsafe { std::env::remove_var("CG_C_STD") }
            }
        }
    }

    #[test]
    fn parse_c_std_from_option() {
        let _guard = env_lock();
        let prev = swap_env(None);
        let opts = vec!["c-std=gnu17".to_string()];
        let cfg = BackendConfig::from_opts(&opts).unwrap();
        assert_eq!(cfg.c_std, CStandard::Gnu17);
        restore_env(prev);
    }

    #[test]
    fn parse_c_std_alias_from_option() {
        let _guard = env_lock();
        let prev = swap_env(None);
        let opts = vec!["cg-c-std=c11".to_string()];
        let cfg = BackendConfig::from_opts(&opts).unwrap();
        assert_eq!(cfg.c_std, CStandard::C11);
        restore_env(prev);
    }

    #[test]
    fn parse_c_std_invalid_option_fails() {
        let _guard = env_lock();
        let prev = swap_env(None);
        let opts = vec!["c-std=bad".to_string()];
        let err = BackendConfig::from_opts(&opts).unwrap_err();
        assert!(err.contains("invalid value for `c-std`"));
        restore_env(prev);
    }

    #[test]
    fn parse_c_std_last_option_wins() {
        let _guard = env_lock();
        let prev = swap_env(None);
        let opts = vec!["c-std=c11".to_string(), "c-std=gnu23".to_string()];
        let cfg = BackendConfig::from_opts(&opts).unwrap();
        assert_eq!(cfg.c_std, CStandard::Gnu23);
        restore_env(prev);
    }

    #[test]
    fn parse_c_std_from_env_fallback() {
        let _guard = env_lock();
        let prev = swap_env(Some("c17"));
        let cfg = BackendConfig::from_opts(&[]).unwrap();
        assert_eq!(cfg.c_std, CStandard::C17);
        restore_env(prev);
    }

    #[test]
    fn parse_c_std_option_overrides_env() {
        let _guard = env_lock();
        let prev = swap_env(Some("c17"));
        let opts = vec!["c-std=gnu11".to_string()];
        let cfg = BackendConfig::from_opts(&opts).unwrap();
        assert_eq!(cfg.c_std, CStandard::Gnu11);
        restore_env(prev);
    }

    #[test]
    fn parse_c_std_invalid_env_fails() {
        let _guard = env_lock();
        let prev = swap_env(Some("bad"));
        let err = BackendConfig::from_opts(&[]).unwrap_err();
        assert!(err.contains("invalid value for `CG_C_STD`"));
        restore_env(prev);
    }
}
