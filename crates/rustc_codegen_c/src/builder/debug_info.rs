use rustc_codegen_ssa::traits::DebugInfoBuilderMethods;

use crate::builder::Builder;

impl DebugInfoBuilderMethods for Builder<'_, '_, '_> {
    fn dbg_var_addr(
        &mut self,
        _dbg_var: Self::DIVariable,
        _dbg_loc: Self::DILocation,
        _variable_alloca: Self::Value,
        _direct_offset: rustc_abi::Size,
        // NB: each offset implies a deref (i.e. they're steps in a pointer chain).
        _indirect_offsets: &[rustc_abi::Size],
        // Byte range in the `dbg_var` covered by this fragment,
        // if this is a fragment of a composite `DIVariable`.
        _fragment: Option<std::ops::Range<rustc_abi::Size>>,
    ) {
        // no-op for C backend
    }

    fn set_dbg_loc(&mut self, _dbg_loc: Self::DILocation) {
        // no-op for C backend
    }

    fn insert_reference_to_gdb_debug_scripts_section_global(&mut self) {
        // no-op for C backend
    }

    fn set_var_name(&mut self, _value: Self::Value, _name: &str) {
        // no-op for C backend
    }

    fn clear_dbg_loc(&mut self) {
        // no-op for C backend
    }
}
