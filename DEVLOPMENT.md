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

- `./y rustc <source.rs>`
- `./y test`
- `./y fmt [--check]`
- `./y clean`

Global options:

- `--release`
- `--out-dir <dir>`
- `--verbose`

## 3. Build and Run an Example

Example:

```bash
./y rustc examples/basic_math.rs
./build/basic_math
echo $?
```

Expected result:

- program exits with code `0`
- no stdout output for `basic_math` (this example does not print)

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

## 8. Troubleshooting

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
