# Development Guide

This document describes how to set up a local development environment for `rustc_codegen_c`, build the project, run examples, run tests, and run style/lint checks.

## 1. Prerequisites

### OS

- Primary supported environment is Linux (matching CI).
- macOS may work for local development, but Linux is the reference target.

### Required tools

- `rustup`
- `clang`
- LLVM `FileCheck` (for codegen tests)
- `ar` / `ranlib` (usually from system toolchain)

### Rust toolchain

This repository pins Rust to:

- `nightly-2024-07-01`
- components: `rust-src`, `rustc-dev`, `llvm-tools-preview`

Setup:

```bash
rustup toolchain install nightly-2024-07-01 --component rust-src rustc-dev llvm-tools-preview
rustup override set nightly-2024-07-01
```

### Ubuntu packages

```bash
sudo apt-get update
sudo apt-get install -y clang llvm-14-tools
```

`llvm-14-tools` provides `FileCheck-14`, which is accepted by this project.

## 2. Build System Entry Point

Use `./y` from repository root:

```bash
./y help
```

Main commands:

- `./y cargo <cargo-args...>`
- `./y rustc <source.rs>`
- `./y test`
- `./y fmt [--check]`
- `./y clean`
- `./y size-compare`

`./y rustc` is kept for compatibility; prefer `./y cargo`.

Global options:

- `--release`
- `--out-dir <dir>`
- `--verbose`

## 3. Build a Cargo Crate with rustc_codegen_c

Example:

```bash
./y cargo build --manifest-path tests/cargo/mod_smoke/Cargo.toml
echo $?
```

Expected result:

- command exits with code `0`
- crate contains `mod foo;` split across multiple files

Compatibility path (single source entrypoint) is still available:

```bash
./y rustc examples/basic_math.rs
./build/basic_math
```

## 4. Run Tests

Run all tests:

```bash
./y test
```

Run compile/build checks only (skip runtime execution):

```bash
./y test --stage compile
```

What this includes:

- `cargo test --manifest-path crates/Cargo.toml`
- cargo smoke build for `tests/cargo/mod_smoke/Cargo.toml`
- compile checks for `examples/*.rs`
- codegen checks for `tests/codegen/*.rs` via `FileCheck`
- blessed output checks for `tests/bless/*.rs`
- runtime checks from `//@ run-pass` directives (only when `--stage run`, the default)

Update blessed outputs:

```bash
./y test --bless
```

## 5. Formatting

Check formatting:

```bash
./y fmt --check
```

Apply formatting:

```bash
./y fmt
```

## 6. Lint and Docs (CI parity)

CI-equivalent lint:

```bash
cargo clippy --manifest-path crates/Cargo.toml --all-features --all-targets -- -D warnings
```

CI-equivalent docs:

```bash
RUSTFLAGS="-D warnings" RUSTDOCFLAGS="-D warnings" cargo doc --manifest-path crates/Cargo.toml
```

## 7. Clean Build Artifacts

```bash
./y clean
```

This removes:

- `crates/target`
- output directory (default: `build`)

## 8. Binary Size Comparison (Hello World)

Use this to compare binary size between:

- direct `rustc` build
- `rustc_codegen_c` build (`Rust -> C -> C compiler`)

Run:

```bash
./y size-compare
```

Default comparison profile:

- `-C opt-level=z`
- `-C codegen-units=1`
- LTO disabled by default on both paths (for toolchain compatibility and stable comparisons)
- both raw and stripped binaries

Optional tuning:

```bash
./y size-compare --opt-level z --codegen-units 1
```

Output files are written to:

- `build/size_compare/report.md`
- `build/size_compare/report.json`

The reports include:

- `rustc_bytes`
- `codegen_c_bytes`
- `delta_bytes` (`codegen_c - rustc`)
- `delta_percent`

Notes:

- results are machine/toolchain/linker dependent
- compare trends over time on the same environment for meaningful regression tracking
- backend env vars follow `CG_*` naming; for example `CG_C_LTO=1` enables C compile `-flto` when needed

## 9. Troubleshooting

### `FileCheck` not found

Install LLVM tools and ensure one of these exists in `PATH`:

- `FileCheck-18`
- `FileCheck-17`
- `FileCheck-16`
- `FileCheck-15`
- `FileCheck-14`
- `FileCheck`

### Wrong Rust version / missing rustc-private crates

Re-apply pinned toolchain and components:

```bash
rustup override set nightly-2024-07-01
rustup component add rust-src rustc-dev llvm-tools-preview --toolchain nightly-2024-07-01
```

### Custom C compiler

Set `CC` before running commands:

```bash
CC=clang ./y test
```
