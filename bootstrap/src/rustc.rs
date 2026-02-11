use std::path::Path;
use std::path::PathBuf;

use clap::Args;
use regex::Regex;

use crate::manifest::Manifest;
use crate::Run;

/// Invoke rustc
#[derive(Args, Debug)]
pub struct RustcCommand {
    source: PathBuf,

    #[arg(last = true)]
    slop: Vec<String>,

    #[arg(short, long)]
    pub verbose: bool,
}

impl Run for RustcCommand {
    const STEP_DISPLAY_NAME: &'static str = "RUSTC";

    fn run(&self, manifest: &Manifest) {
        manifest.prepare();
        self.build_auxiliaries(manifest);

        let mut command = manifest.rustc();
        command
            .arg(&self.source)
            .args(["--crate-type", "bin"])
            .arg("--out-dir")
            .arg(&manifest.out_dir)
            .args(&self.slop);
        if self.verbose {
            command.env("RUST_BACKTRACE", "full");
        }

        self.command_status("rustc", &mut command);
    }

    fn verbose(&self) -> bool {
        self.verbose
    }
}

impl RustcCommand {
    fn build_auxiliaries(&self, manifest: &Manifest) {
        for source in self.collect_aux_builds() {
            self.log_action_start("building auxiliary", source.display());
            let mut command = manifest.rustc();
            command
                .args(["--crate-type", "lib"])
                .arg("-O")
                .arg(&source)
                .arg("--out-dir")
                .arg(&manifest.out_dir);
            self.command_status("rustc", &mut command);
        }
    }

    fn collect_aux_builds(&self) -> Vec<PathBuf> {
        let source = std::fs::read_to_string(&self.source)
            .unwrap_or_else(|e| panic!("failed to read {}: {}", self.source.display(), e));
        let aux_re = Regex::new(r"^//@\s*aux-build:\s*(.*)").unwrap();
        let mut auxiliaries = Vec::new();

        for line in source.lines() {
            if let Some(cap) = aux_re.captures(line) {
                let fname = cap[1].trim();
                let path = Path::new("tests/auxiliary").join(fname);
                if !auxiliaries.contains(&path) {
                    auxiliaries.push(path);
                }
            }
        }

        auxiliaries
    }
}
