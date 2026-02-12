use rustc_abi::{Align, BackendRepr, Size};
use rustc_codegen_c_ast::cstruct::{CStructDef, CStructField};
use rustc_codegen_c_ast::expr::CValue;
use rustc_codegen_c_ast::ty::{CTy, CTyKind, CUintTy};
use rustc_codegen_ssa::traits::{
    ConstCodegenMethods, LayoutTypeCodegenMethods, StaticCodegenMethods,
};
use rustc_data_structures::intern::Interned;
use rustc_hir::def_id::DefId;
use rustc_middle::mir::interpret::{read_target_uint, GlobalAlloc};
use rustc_middle::ty::layout::LayoutOf;

use crate::context::CodegenCx;

fn value_expr_text(value: CValue<'_>) -> String {
    match value {
        CValue::Scalar(v) => v.to_string(),
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
        let symbol = self.static_symbol(def_id);
        let ty = self.tcx.type_of(def_id).instantiate_identity();
        let layout = self.layout_of(ty);
        let alloc = self.tcx.eval_static_initializer(def_id).unwrap_or_else(|err| {
            panic!("failed to evaluate static initializer for {def_id:?}: {err:?}")
        });
        let alloc = alloc.inner();

        match layout.backend_repr {
            BackendRepr::ScalarPair(a, b) => {
                let ptr_size = self.tcx.data_layout.pointer_size().bytes() as usize;
                let endianness = self.tcx.data_layout.endian;
                let bytes = alloc.inspect_with_uninit_and_ptr_outside_interpreter(0..alloc.len());

                let (_, ptr_prov) = alloc
                    .provenance()
                    .ptrs()
                    .iter()
                    .find(|(offset, _)| offset.bytes() == 0)
                    .copied()
                    .unwrap_or_else(|| {
                        panic!("expected pointer provenance at offset 0 for static {def_id:?}")
                    });

                let addend =
                    read_target_uint(endianness, &bytes[..ptr_size]).unwrap_or_else(|_| {
                        panic!("failed to decode pointer addend for static {def_id:?}")
                    });

                let base_ptr = match self.tcx.global_alloc(ptr_prov.alloc_id()) {
                    GlobalAlloc::Memory(target_alloc) => self.const_data_from_alloc(target_alloc),
                    GlobalAlloc::Function { instance, .. } => {
                        self.symbol_value(self.tcx.symbol_name(instance).name)
                    }
                    GlobalAlloc::Static(target_def_id) => {
                        let target_symbol = self.static_symbol(target_def_id);
                        let expr = self.mcx.alloc_str(&format!("((uint8_t *)&{target_symbol})"));
                        CValue::Func(expr)
                    }
                    GlobalAlloc::VTable(..) | GlobalAlloc::TypeId { .. } => CValue::Scalar(0),
                };
                let ptr = self.const_ptr_byte_offset(base_ptr, Size::from_bytes(addend));

                let second_offset = a.size(self).align_to(b.align(self).abi).bytes() as usize;
                let second_size = b.size(self).bytes() as usize;
                let second_end = second_offset + second_size;
                let len = read_target_uint(endianness, &bytes[second_offset..second_end])
                    .unwrap_or_else(|_| {
                        panic!("failed to decode scalar-pair metadata for {def_id:?}")
                    });

                let ptr_ty = self.scalar_pair_element_backend_type(layout, 0, true);
                let len_ty = self.scalar_pair_element_backend_type(layout, 1, true);
                let struct_name = self.mcx.alloc_str(&format!("__rcgenc_static_pair_{symbol}"));
                self.mcx.module().push_struct(CStructDef {
                    name: struct_name,
                    fields: vec![
                        CStructField { name: self.mcx.alloc_str("ptr"), ty: ptr_ty },
                        CStructField { name: self.mcx.alloc_str("meta"), ty: len_ty },
                    ],
                });

                let static_ty = CTy::Ref(Interned::new_unchecked(
                    self.mcx.arena().alloc(CTyKind::Struct(struct_name)),
                ));
                let init = self.mcx.alloc_str(&format!("{{ {}, {} }}", value_expr_text(ptr), len));
                self.mcx.module().push_decl(self.mcx.var(
                    CValue::Func(symbol),
                    static_ty,
                    Some(self.mcx.value(CValue::Func(init))),
                ));
            }
            _ => {
                if !alloc.provenance().ptrs().is_empty() {
                    panic!("unsupported static with relocations: {def_id:?}");
                }

                let bytes = alloc.inspect_with_uninit_and_ptr_outside_interpreter(0..alloc.len());
                let array_len = bytes.len().max(1);
                let array_ty = CTy::Ref(Interned::new_unchecked(
                    self.mcx.arena().alloc(CTyKind::Array(CTy::UInt(CUintTy::U8), array_len)),
                ));
                let init_bytes = if bytes.is_empty() { &[0u8][..] } else { bytes };
                let init = self.mcx.alloc_str(&bytes_initializer(init_bytes));
                self.mcx.module().push_decl(self.mcx.var(
                    CValue::Func(symbol),
                    array_ty,
                    Some(self.mcx.value(CValue::Func(init))),
                ));
            }
        }
    }
}
