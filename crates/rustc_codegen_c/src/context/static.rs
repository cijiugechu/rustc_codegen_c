use rustc_abi::Align;
use rustc_codegen_c_ast::expr::CValue;
use rustc_codegen_c_ast::ty::{CTy, CTyKind, CUintTy};
use rustc_codegen_ssa::traits::StaticCodegenMethods;
use rustc_data_structures::intern::Interned;
use rustc_hir::def_id::DefId;
use rustc_middle::mir::interpret::read_target_uint;

use crate::context::CodegenCx;

fn value_expr_text(value: CValue<'_>) -> String {
    match value {
        CValue::Scalar(v) | CValue::ScalarTyped(v, _) => v.to_string(),
        CValue::RealLiteral(v) => v.to_string(),
        CValue::Local(i) => format!("_{i}"),
        CValue::Func(name) => name.to_string(),
    }
}

fn bytes_initializer(bytes: &[u8]) -> String {
    use std::fmt::Write;

    let mut init = String::new();
    init.push('{');
    if !bytes.is_empty() {
        init.push(' ');
        for (idx, byte) in bytes.iter().enumerate() {
            if idx != 0 {
                init.push_str(", ");
            }
            let _ = write!(&mut init, "{byte}");
        }
        init.push(' ');
    }
    init.push('}');
    init
}

impl<'tcx, 'mx> StaticCodegenMethods for CodegenCx<'tcx, 'mx> {
    fn static_addr_of(&self, cv: Self::Value, align: Align, kind: Option<&str>) -> Self::Value {
        let _ = (align, kind);
        cv
    }

    fn codegen_static(&mut self, def_id: DefId) {
        self.mark_static_defined(def_id);
        let info = self.ensure_static_info(def_id);
        let symbol = info.decl_name;
        let linkage = info.linkage;
        let visibility = info.visibility;
        let link_name = info.link_name;
        let thread_local = info.thread_local;
        let alloc = self.tcx.eval_static_initializer(def_id).unwrap_or_else(|err| {
            panic!("failed to evaluate static initializer for {def_id:?}: {err:?}")
        });
        let alloc = alloc.inner();
        let bytes = alloc.inspect_with_uninit_and_ptr_outside_interpreter(0..alloc.len());
        let ptrs = alloc.provenance().ptrs();

        let (static_ty, init) = if ptrs.is_empty() {
            let array_len = bytes.len().max(1);
            let static_ty = CTy::Ref(Interned::new_unchecked(
                self.mcx.arena().alloc(CTyKind::Array(CTy::UInt(CUintTy::U8), array_len)),
            ));
            let init_bytes = if bytes.is_empty() { &[0u8][..] } else { bytes };
            (static_ty, self.mcx.alloc_str(&bytes_initializer(init_bytes)))
        } else {
            let ptr_size = self.tcx.data_layout.pointer_size().bytes() as usize;
            if ptr_size == 0
                || alloc.len() % ptr_size != 0
                || ptrs.iter().any(|(offset, _)| (offset.bytes() as usize) % ptr_size != 0)
            {
                panic!(
                    "unsupported static relocation layout for {def_id:?}: len={} ptr_size={} relocs={}",
                    alloc.len(),
                    ptr_size,
                    ptrs.len()
                );
            }

            let endianness = self.tcx.data_layout.endian;
            let words = (alloc.len() / ptr_size).max(1);
            let mut entries = Vec::with_capacity(words);
            for idx in 0..words {
                let byte_offset = idx * ptr_size;
                let word = read_target_uint(
                    endianness,
                    &bytes[byte_offset..byte_offset + ptr_size],
                )
                .unwrap_or_else(|_| {
                    panic!(
                        "failed to decode relocation word at offset {} for static {def_id:?}",
                        byte_offset
                    )
                });

                if let Some((_, prov)) =
                    ptrs.iter().find(|(offset, _)| (offset.bytes() as usize) == byte_offset)
                {
                    let base = self.global_alloc_base_value(self.tcx.global_alloc(prov.alloc_id()));
                    let base_expr = value_expr_text(base);
                    if word == 0 {
                        entries.push(format!("(size_t)({base_expr})"));
                    } else {
                        entries.push(format!("((size_t)({base_expr}) + {word})"));
                    }
                } else {
                    entries.push(word.to_string());
                }
            }

            let static_ty = CTy::Ref(Interned::new_unchecked(
                self.mcx.arena().alloc(CTyKind::Array(CTy::UInt(CUintTy::Usize), words)),
            ));
            let init = self.mcx.alloc_str(&format!("{{ {} }}", entries.join(", ")));
            (static_ty, init)
        };

        self.mcx.module().push_decl(self.mcx.var_with_symbol_attrs(
            CValue::Func(symbol),
            static_ty,
            Some(self.mcx.value(CValue::Func(init))),
            linkage,
            visibility,
            link_name,
            thread_local,
        ));
    }
}
