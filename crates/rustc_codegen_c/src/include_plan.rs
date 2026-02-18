use std::collections::BTreeSet;

use rustc_codegen_c_ast::include::{IncludeSet, IncludeSpec};
use rustc_codegen_c_ast::module::Module;

use crate::config::CStandard;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum IncludeCapability {
    StdIntTypes,
    SizeTypes,
    AbortApi,
}

pub(crate) struct IncludePlanner {
    c_std: CStandard,
    capabilities: BTreeSet<IncludeCapability>,
}

impl IncludePlanner {
    pub(crate) fn new(c_std: CStandard) -> Self {
        Self { c_std, capabilities: BTreeSet::new() }
    }

    pub(crate) fn require(&mut self, capability: IncludeCapability) {
        self.capabilities.insert(capability);
    }

    pub(crate) fn build_set(&self) -> Result<IncludeSet, &'static str> {
        let mut includes = IncludeSet::new();
        for capability in &self.capabilities {
            self.ensure_capability_supported(*capability)?;
            includes.insert(IncludeSpec::system(capability_header(*capability)));
        }
        Ok(includes)
    }

    pub(crate) fn apply_to_module(&self, module: &Module<'_>) -> Result<(), &'static str> {
        let includes = self.build_set()?;
        for include in includes {
            match include.kind {
                rustc_codegen_c_ast::include::IncludeKind::System => {
                    module.require_system_include(&include.canonical_name);
                }
                rustc_codegen_c_ast::include::IncludeKind::Local => {
                    module.require_local_include(&include.canonical_name);
                }
            }
        }
        Ok(())
    }

    fn ensure_capability_supported(
        &self,
        capability: IncludeCapability,
    ) -> Result<(), &'static str> {
        if self.c_std.level() < minimum_required_level(capability) {
            return Err("requested include capability is not supported by selected C standard");
        }
        Ok(())
    }
}

fn capability_header(capability: IncludeCapability) -> &'static str {
    match capability {
        IncludeCapability::StdIntTypes => "stdint.h",
        IncludeCapability::SizeTypes => "stddef.h",
        IncludeCapability::AbortApi => "stdlib.h",
    }
}

fn minimum_required_level(capability: IncludeCapability) -> u16 {
    match capability {
        IncludeCapability::StdIntTypes
        | IncludeCapability::SizeTypes
        | IncludeCapability::AbortApi => 1999,
    }
}

#[cfg(test)]
mod tests {
    use super::{IncludeCapability, IncludePlanner};
    use crate::config::CStandard;

    #[test]
    fn build_set_dedups_repeated_capability() {
        let mut planner = IncludePlanner::new(CStandard::C99);
        planner.require(IncludeCapability::StdIntTypes);
        planner.require(IncludeCapability::StdIntTypes);

        let set = planner.build_set().unwrap();
        assert_eq!(set.len(), 1);
        let only = set.iter().next().unwrap();
        assert_eq!(only.canonical_name, "stdint.h");
    }

    #[test]
    fn build_set_empty_when_no_capabilities() {
        let planner = IncludePlanner::new(CStandard::C99);
        let set = planner.build_set().unwrap();
        assert!(set.is_empty());
    }

    #[test]
    fn build_set_validates_standard_compatibility() {
        let mut planner = IncludePlanner::new(CStandard::C99);
        planner.require(IncludeCapability::AbortApi);
        assert!(planner.build_set().is_ok());
    }
}
