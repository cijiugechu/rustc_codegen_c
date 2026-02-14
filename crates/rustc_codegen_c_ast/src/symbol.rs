//! This module defines linkage/visibility metadata for global C symbols.

/// C linkage for global symbols.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CLinkage {
    /// Externally visible linkage.
    External,
    /// Translation-unit local linkage (`static`).
    Internal,
}

impl Default for CLinkage {
    fn default() -> Self {
        Self::External
    }
}

/// C visibility for externally linked symbols.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CVisibility {
    /// Default visibility.
    Default,
    /// Hidden visibility.
    Hidden,
}

impl Default for CVisibility {
    fn default() -> Self {
        Self::Default
    }
}
