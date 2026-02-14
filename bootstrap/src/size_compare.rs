use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use clap::{Args, ValueEnum};
use color_print::cprintln;

use crate::manifest::Manifest;
use crate::Run;

const HELLO_WORLD_SOURCE: &str = "fn main() {\n    println!(\"hello world\");\n}\n";
const EXPECTED_STDOUT: &[u8] = b"hello world\n";

#[derive(Clone, Copy, Debug, Eq, PartialEq, ValueEnum)]
pub enum SizeOptLevel {
    #[value(name = "0")]
    O0,
    #[value(name = "1")]
    O1,
    #[value(name = "2")]
    O2,
    #[value(name = "3")]
    O3,
    #[value(name = "s")]
    S,
    #[value(name = "z")]
    Z,
}

impl SizeOptLevel {
    fn rustc_value(self) -> &'static str {
        match self {
            SizeOptLevel::O0 => "0",
            SizeOptLevel::O1 => "1",
            SizeOptLevel::O2 => "2",
            SizeOptLevel::O3 => "3",
            SizeOptLevel::S => "s",
            SizeOptLevel::Z => "z",
        }
    }
}

/// Compare binary size between direct rustc and rustc_codegen_c for hello world.
#[derive(Args, Debug)]
pub struct SizeCompareCommand {
    /// Rust optimization level for both paths.
    #[arg(long, value_enum, default_value_t = SizeOptLevel::Z)]
    pub opt_level: SizeOptLevel,

    /// Number of Rust codegen units for both paths.
    #[arg(long, default_value_t = 1)]
    pub codegen_units: usize,

    /// Keep generated source and binaries under out_dir/size_compare.
    #[arg(long, default_value_t = true, action = clap::ArgAction::Set)]
    pub keep_artifacts: bool,

    /// Whether to show verbose output.
    #[arg(short, long)]
    pub verbose: bool,
}

struct BuildArtifacts {
    root: PathBuf,
    source: PathBuf,
    rust_raw: PathBuf,
    rust_stripped: PathBuf,
    codegen_c_raw: PathBuf,
    codegen_c_stripped: PathBuf,
    report_md: PathBuf,
    report_json: PathBuf,
}

impl BuildArtifacts {
    fn new(root: PathBuf) -> Self {
        Self {
            source: root.join("hello_world.rs"),
            rust_raw: root.join("hello_rust_raw"),
            rust_stripped: root.join("hello_rust_stripped"),
            codegen_c_raw: root.join("hello_codegen_c_raw"),
            codegen_c_stripped: root.join("hello_codegen_c_stripped"),
            report_md: root.join("report.md"),
            report_json: root.join("report.json"),
            root,
        }
    }
}

struct ComparisonRow {
    rustc_bytes: u64,
    codegen_c_bytes: u64,
    delta_bytes: i64,
    delta_percent: f64,
}

impl ComparisonRow {
    fn new(rustc_bytes: u64, codegen_c_bytes: u64) -> Self {
        let delta_bytes = codegen_c_bytes as i64 - rustc_bytes as i64;
        let delta_percent =
            if rustc_bytes == 0 { 0.0 } else { (delta_bytes as f64 / rustc_bytes as f64) * 100.0 };
        Self { rustc_bytes, codegen_c_bytes, delta_bytes, delta_percent }
    }
}

struct ComparisonReport {
    opt_level: &'static str,
    codegen_units: usize,
    raw: ComparisonRow,
    stripped: ComparisonRow,
}

impl Run for SizeCompareCommand {
    const STEP_DISPLAY_NAME: &'static str = "SIZE";

    fn run(&self, manifest: &Manifest) {
        manifest.prepare();

        let artifacts = BuildArtifacts::new(manifest.out_dir.join("size_compare"));
        fs::create_dir_all(&artifacts.root)
            .unwrap_or_else(|e| panic!("failed to create {}: {e}", artifacts.root.display()));

        self.log_action_start("writing source", artifacts.source.display());
        fs::write(&artifacts.source, HELLO_WORLD_SOURCE)
            .unwrap_or_else(|e| panic!("failed to write {}: {e}", artifacts.source.display()));

        self.compile_with_rustc(&artifacts.source, &artifacts.rust_raw, false);
        self.compile_with_rustc(&artifacts.source, &artifacts.rust_stripped, true);
        self.compile_with_codegen_c(manifest, &artifacts.source, &artifacts.codegen_c_raw, false);
        self.compile_with_codegen_c(
            manifest,
            &artifacts.source,
            &artifacts.codegen_c_stripped,
            true,
        );

        self.verify_binary_output("rust raw", &artifacts.rust_raw);
        self.verify_binary_output("rust stripped", &artifacts.rust_stripped);
        self.verify_binary_output("codegen_c raw", &artifacts.codegen_c_raw);
        self.verify_binary_output("codegen_c stripped", &artifacts.codegen_c_stripped);

        let report = ComparisonReport {
            opt_level: self.opt_level.rustc_value(),
            codegen_units: self.codegen_units,
            raw: ComparisonRow::new(
                file_size(&artifacts.rust_raw),
                file_size(&artifacts.codegen_c_raw),
            ),
            stripped: ComparisonRow::new(
                file_size(&artifacts.rust_stripped),
                file_size(&artifacts.codegen_c_stripped),
            ),
        };

        let markdown = self.render_markdown(&report);
        let json = self.render_json(&report);
        fs::write(&artifacts.report_md, markdown)
            .unwrap_or_else(|e| panic!("failed to write {}: {e}", artifacts.report_md.display()));
        fs::write(&artifacts.report_json, json)
            .unwrap_or_else(|e| panic!("failed to write {}: {e}", artifacts.report_json.display()));

        self.log_action_start("size summary", "");
        cprintln!(
            "       <b>raw</b>: rustc={} bytes, rustc_codegen_c={} bytes, delta={} ({:+.2}%)",
            report.raw.rustc_bytes,
            report.raw.codegen_c_bytes,
            report.raw.delta_bytes,
            report.raw.delta_percent
        );
        cprintln!(
            "       <b>stripped</b>: rustc={} bytes, rustc_codegen_c={} bytes, delta={} ({:+.2}%)",
            report.stripped.rustc_bytes,
            report.stripped.codegen_c_bytes,
            report.stripped.delta_bytes,
            report.stripped.delta_percent
        );
        self.log_action_context("report", artifacts.report_md.display());
        self.log_action_context("report json", artifacts.report_json.display());

        if !self.keep_artifacts {
            self.remove_non_report_artifacts(&artifacts);
        }
    }

    fn verbose(&self) -> bool {
        self.verbose
    }
}

impl SizeCompareCommand {
    fn compile_with_rustc(&self, source: &Path, output: &Path, strip: bool) {
        self.log_action_start(
            "compiling rustc",
            if strip { "stripped binary" } else { "raw binary" },
        );
        let mut command = Command::new("rustc");
        command
            .args(["--edition", "2021"])
            .arg(source)
            .args(["--crate-type", "bin"])
            .arg("-C")
            .arg(format!("opt-level={}", self.opt_level.rustc_value()))
            .arg("-C")
            .arg(format!("codegen-units={}", self.codegen_units))
            .args(["-C", "panic=abort"])
            .args(["-C", "overflow-checks=off"]);
        if strip {
            command.args(["-C", "strip=symbols"]);
        }
        command.arg("-o").arg(output);
        self.command_status("rustc", &mut command);
    }

    fn compile_with_codegen_c(
        &self,
        manifest: &Manifest,
        source: &Path,
        output: &Path,
        strip: bool,
    ) {
        self.log_action_start(
            "compiling rustc_codegen_c",
            if strip { "stripped binary" } else { "raw binary" },
        );
        let mut command = manifest.rustc();
        command
            .arg(source)
            .args(["--crate-type", "bin"])
            .arg("-C")
            .arg(format!("opt-level={}", self.opt_level.rustc_value()))
            .arg("-C")
            .arg(format!("codegen-units={}", self.codegen_units));
        if strip {
            command.args(["-C", "strip=symbols"]);
        }
        command.arg("-o").arg(output);
        self.command_status("rustc_codegen_c", &mut command);
    }

    fn verify_binary_output(&self, label: &str, binary: &Path) {
        self.log_action_start("running", format!("{label} ({})", binary.display()));
        let mut command = Command::new(binary);
        let output = self.command_output("run", &mut command);
        if output.stdout != EXPECTED_STDOUT {
            panic!(
                "{label} stdout mismatch: expected {:?}, got {:?}",
                String::from_utf8_lossy(EXPECTED_STDOUT),
                String::from_utf8_lossy(&output.stdout)
            );
        }
        if !output.stderr.is_empty() {
            panic!("{label} stderr is not empty: {:?}", String::from_utf8_lossy(&output.stderr));
        }
    }

    fn remove_non_report_artifacts(&self, artifacts: &BuildArtifacts) {
        self.log_action_start("cleaning artifacts", artifacts.root.display());
        for path in [
            &artifacts.source,
            &artifacts.rust_raw,
            &artifacts.rust_stripped,
            &artifacts.codegen_c_raw,
            &artifacts.codegen_c_stripped,
        ] {
            if path.exists() {
                fs::remove_file(path)
                    .unwrap_or_else(|e| panic!("failed to remove {}: {e}", path.display()));
            }
        }
    }

    fn render_markdown(&self, report: &ComparisonReport) -> String {
        format!(
            "# Hello World Size Comparison\n\
             \n\
             - `opt-level`: `{}`\n\
             - `codegen-units`: `{}`\n\
             \n\
             | variant | rustc_bytes | codegen_c_bytes | delta_bytes | delta_percent |\n\
             | --- | ---: | ---: | ---: | ---: |\n\
             | raw | {} | {} | {} | {:+.2}% |\n\
             | stripped | {} | {} | {} | {:+.2}% |\n",
            report.opt_level,
            report.codegen_units,
            report.raw.rustc_bytes,
            report.raw.codegen_c_bytes,
            report.raw.delta_bytes,
            report.raw.delta_percent,
            report.stripped.rustc_bytes,
            report.stripped.codegen_c_bytes,
            report.stripped.delta_bytes,
            report.stripped.delta_percent
        )
    }

    fn render_json(&self, report: &ComparisonReport) -> String {
        format!(
            "{{\n  \"opt_level\": \"{}\",\n  \"codegen_units\": {},\n  \"raw\": {{\n    \"rustc_bytes\": {},\n    \"codegen_c_bytes\": {},\n    \"delta_bytes\": {},\n    \"delta_percent\": {:.6}\n  }},\n  \"stripped\": {{\n    \"rustc_bytes\": {},\n    \"codegen_c_bytes\": {},\n    \"delta_bytes\": {},\n    \"delta_percent\": {:.6}\n  }}\n}}\n",
            report.opt_level,
            report.codegen_units,
            report.raw.rustc_bytes,
            report.raw.codegen_c_bytes,
            report.raw.delta_bytes,
            report.raw.delta_percent,
            report.stripped.rustc_bytes,
            report.stripped.codegen_c_bytes,
            report.stripped.delta_bytes,
            report.stripped.delta_percent
        )
    }
}

fn file_size(path: &Path) -> u64 {
    fs::metadata(path).unwrap_or_else(|e| panic!("failed to stat {}: {e}", path.display())).len()
}
