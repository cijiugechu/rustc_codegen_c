//! Include modeling, normalization, and rendering.

use std::cmp::Ordering;
use std::collections::BTreeSet;

/// Include kind.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IncludeKind {
    /// `#include <...>`
    System,
    /// `#include "..."`
    Local,
}

/// Canonical include descriptor.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IncludeSpec {
    pub kind: IncludeKind,
    pub canonical_name: String,
}

impl IncludeSpec {
    pub fn system(name: &str) -> Self {
        canonicalize_include(name, IncludeKind::System)
    }

    pub fn local(name: &str) -> Self {
        canonicalize_include(name, IncludeKind::Local)
    }
}

impl PartialOrd for IncludeSpec {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for IncludeSpec {
    fn cmp(&self, other: &Self) -> Ordering {
        self.kind.cmp(&other.kind).then_with(|| self.canonical_name.cmp(&other.canonical_name))
    }
}

pub type IncludeSet = BTreeSet<IncludeSpec>;

/// Normalize include text:
/// - trims whitespace
/// - accepts `<...>` / `"..."` / bare names
/// - strips delimiters and records canonical `(kind, name)`.
pub fn canonicalize_include(raw: &str, default_kind: IncludeKind) -> IncludeSpec {
    let raw = raw.trim();
    assert!(!raw.is_empty(), "include name must not be empty");

    if raw.starts_with('<') && raw.ends_with('>') && raw.len() >= 2 {
        let inner = raw[1..raw.len() - 1].trim();
        assert!(!inner.is_empty(), "include name must not be empty");
        return IncludeSpec { kind: IncludeKind::System, canonical_name: inner.to_string() };
    }

    if raw.starts_with('"') && raw.ends_with('"') && raw.len() >= 2 {
        let inner = raw[1..raw.len() - 1].trim();
        assert!(!inner.is_empty(), "include name must not be empty");
        return IncludeSpec { kind: IncludeKind::Local, canonical_name: inner.to_string() };
    }

    IncludeSpec { kind: default_kind, canonical_name: raw.to_string() }
}

/// Render include block using stable grouped order:
/// - system headers first
/// - local headers second
/// - each group already lexicographically sorted by `BTreeSet`.
pub fn render_include_block(includes: &IncludeSet) -> String {
    let mut out = String::new();
    let mut wrote_system = false;
    let mut wrote_local = false;

    for include in includes {
        if include.kind == IncludeKind::System {
            out.push_str("#include <");
            out.push_str(&include.canonical_name);
            out.push_str(">\n");
            wrote_system = true;
        }
    }

    for include in includes {
        if include.kind == IncludeKind::Local {
            if wrote_system && !wrote_local {
                out.push('\n');
            }
            out.push_str("#include \"");
            out.push_str(&include.canonical_name);
            out.push_str("\"\n");
            wrote_local = true;
        }
    }

    out
}

#[cfg(test)]
mod tests {
    use super::{canonicalize_include, render_include_block, IncludeKind, IncludeSet, IncludeSpec};

    #[test]
    fn canonicalize_strips_delimiters_and_overrides_kind() {
        let system = canonicalize_include("<stdint.h>", IncludeKind::Local);
        assert_eq!(system.kind, IncludeKind::System);
        assert_eq!(system.canonical_name, "stdint.h");

        let local = canonicalize_include("\"foo/bar.h\"", IncludeKind::System);
        assert_eq!(local.kind, IncludeKind::Local);
        assert_eq!(local.canonical_name, "foo/bar.h");
    }

    #[test]
    fn canonicalize_bare_uses_default_kind() {
        let include = canonicalize_include("stddef.h", IncludeKind::System);
        assert_eq!(include.kind, IncludeKind::System);
        assert_eq!(include.canonical_name, "stddef.h");
    }

    #[test]
    fn render_include_block_groups_and_sorts_and_dedups() {
        let mut includes = IncludeSet::new();
        includes.insert(IncludeSpec::system("stddef.h"));
        includes.insert(IncludeSpec::system("<stdint.h>"));
        includes.insert(IncludeSpec::local("z.h"));
        includes.insert(IncludeSpec::local("\"a.h\""));
        includes.insert(IncludeSpec::system("stdint.h"));

        let rendered = render_include_block(&includes);
        assert_eq!(
            rendered,
            "#include <stddef.h>\n#include <stdint.h>\n\n#include \"a.h\"\n#include \"z.h\"\n"
        );
    }
}
