use rustc_abi::{Float as AbiFloat, Primitive};
use rustc_codegen_c_ast::expr::CValue;
use rustc_codegen_c_ast::symbol::{CLinkage, CVisibility};
use rustc_codegen_c_ast::ty::{CFloatTy, CIntTy, CTy, CTyKind, CUintTy};
use rustc_codegen_ssa::traits::{ConstCodegenMethods, MiscCodegenMethods, StaticCodegenMethods};
use rustc_const_eval::interpret::{ConstAllocation, Scalar};
use rustc_data_structures::intern::Interned;
use rustc_middle::mir::interpret::{read_target_uint, GlobalAlloc};
use rustc_type_ir::{IntTy, UintTy};

use crate::context::{sanitize_symbol_name, CodegenCx};

fn c_string_literal_from_bytes(bytes: &[u8]) -> String {
    use std::fmt::Write;

    let mut out = String::with_capacity(bytes.len() + 2);
    out.push('"');
    for &byte in bytes {
        match byte {
            b'\\' => out.push_str("\\\\"),
            b'"' => out.push_str("\\\""),
            b'\n' => out.push_str("\\n"),
            b'\r' => out.push_str("\\r"),
            b'\t' => out.push_str("\\t"),
            0x20..=0x7e => out.push(byte as char),
            _ => {
                let _ = write!(&mut out, "\\{:03o}", byte);
            }
        }
    }
    out.push('"');
    out
}

fn value_expr_text(value: CValue<'_>) -> String {
    match value {
        CValue::Scalar(v) | CValue::ScalarTyped(v, _) => v.to_string(),
        CValue::RealLiteral(v) => v.to_string(),
        CValue::Local(i) => format!("_{i}"),
        CValue::Func(name) => name.to_string(),
    }
}

impl<'tcx, 'mx> CodegenCx<'tcx, 'mx> {
    fn next_scalar_id(&self) -> u64 {
        let next = self.scalar_ids.get();
        self.scalar_ids.set(next + 1);
        next
    }

    fn integer_scalar_shape(&self, ty: CTy<'mx>) -> Option<(u32, bool)> {
        match ty {
            CTy::Bool => Some((1, false)),
            CTy::Int(int) => {
                let bits = match int {
                    CIntTy::I8 => 8,
                    CIntTy::I16 => 16,
                    CIntTy::I32 => 32,
                    CIntTy::I64 => 64,
                    CIntTy::I128 => 128,
                    CIntTy::Isize => self.tcx.data_layout.pointer_size().bits() as u32,
                };
                Some((bits, true))
            }
            CTy::UInt(uint) => {
                let bits = match uint {
                    CUintTy::U8 => 8,
                    CUintTy::U16 => 16,
                    CUintTy::U32 => 32,
                    CUintTy::U64 => 64,
                    CUintTy::U128 => 128,
                    CUintTy::Usize => self.tcx.data_layout.pointer_size().bits() as u32,
                };
                Some((bits, false))
            }
            _ => None,
        }
    }

    fn normalize_integer_scalar_for_ty(&self, value: i128, ty: CTy<'mx>) -> i128 {
        let Some((bits, signed)) = self.integer_scalar_shape(ty) else {
            return value;
        };

        let bits = bits.clamp(1, 128);
        if bits == 128 {
            return value;
        }

        let mask = (1u128 << bits) - 1;
        let truncated = (value as u128) & mask;
        if signed {
            let sign_bit = 1u128 << (bits - 1);
            if truncated & sign_bit != 0 {
                (truncated | (!mask)) as i128
            } else {
                truncated as i128
            }
        } else {
            truncated as i128
        }
    }

    fn typed_scalar(&self, value: i128, ty: CTy<'mx>) -> CValue<'mx> {
        let value = self.normalize_integer_scalar_for_ty(value, ty);
        let scalar = CValue::ScalarTyped(value, self.next_scalar_id());
        if let Some(fkey) = self.current_fkey.get() {
            self.value_tys.borrow_mut().insert((fkey, scalar), ty);
        }
        scalar
    }

    fn record_u8_ptr_value(&self, value: CValue<'mx>) -> CValue<'mx> {
        if let Some(fkey) = self.current_fkey.get() {
            let ptr_ty = CTy::Ref(Interned::new_unchecked(
                self.mcx.arena().alloc(CTyKind::Pointer(CTy::UInt(CUintTy::U8))),
            ));
            self.value_tys.borrow_mut().insert((fkey, value), ptr_ty);
        }
        value
    }

    pub(crate) fn const_bytes_pointer(&self, bytes: &[u8]) -> CValue<'mx> {
        let literal = c_string_literal_from_bytes(bytes);
        let expr = self.mcx.alloc_str(&format!("((uint8_t *){literal})"));
        self.record_u8_ptr_value(CValue::Func(expr))
    }

    pub(crate) fn symbol_value(&self, symbol_name: &str) -> CValue<'mx> {
        let symbol_name = sanitize_symbol_name(symbol_name);
        CValue::Func(self.mcx.alloc_str(&symbol_name))
    }

    fn global_alloc_base_value(&self, alloc: GlobalAlloc<'tcx>) -> CValue<'mx> {
        match alloc {
            GlobalAlloc::Memory(alloc) => self.const_data_from_alloc(alloc),
            GlobalAlloc::Function { instance, .. } => CValue::Func(self.get_fn(instance).0.name),
            GlobalAlloc::Static(def_id) => self.record_u8_ptr_value(self.static_addr_expr(def_id)),
            GlobalAlloc::VTable(ty, dyn_ty) => self.get_vtable_value(
                ty,
                dyn_ty
                    .principal()
                    .map(|principal| self.tcx.instantiate_bound_regions_with_erased(principal)),
            ),
            GlobalAlloc::TypeId { .. } => {
                self.typed_scalar(0, self.mcx.get_uint_type(UintTy::Usize))
            }
        }
    }

    pub(crate) fn get_vtable_value(
        &self,
        ty: rustc_middle::ty::Ty<'tcx>,
        trait_ref: Option<rustc_middle::ty::ExistentialTraitRef<'tcx>>,
    ) -> CValue<'mx> {
        if let Some(&value) = self.vtables.borrow().get(&(ty, trait_ref)) {
            return value;
        }

        let alloc_id = self.tcx.vtable_allocation((ty, trait_ref));
        let alloc = self.tcx.global_alloc(alloc_id).unwrap_memory();
        let vtable_const = self.const_data_from_alloc(alloc);
        let align = self.tcx.data_layout.pointer_align().abi;
        let vtable = self.static_addr_of(vtable_const, align, Some("vtable"));
        self.vtables.borrow_mut().insert((ty, trait_ref), vtable);
        vtable
    }
}

impl<'tcx, 'mx> ConstCodegenMethods for CodegenCx<'tcx, 'mx> {
    fn const_null(&self, t: Self::Type) -> Self::Value {
        self.typed_scalar(0, t)
    }

    fn const_undef(&self, t: Self::Type) -> Self::Value {
        self.typed_scalar(0, t)
    }

    fn const_poison(&self, t: Self::Type) -> Self::Value {
        self.typed_scalar(0, t)
    }

    fn const_int(&self, t: Self::Type, i: i64) -> Self::Value {
        self.typed_scalar(i as i128, t)
    }

    fn const_uint(&self, t: Self::Type, i: u64) -> Self::Value {
        self.typed_scalar(i as i128, t)
    }

    fn const_uint_big(&self, t: Self::Type, u: u128) -> Self::Value {
        self.typed_scalar(u as i128, t)
    }

    fn const_bool(&self, val: bool) -> Self::Value {
        self.typed_scalar(if val { 1 } else { 0 }, CTy::Bool)
    }

    fn const_i16(&self, i: i16) -> Self::Value {
        self.typed_scalar(i as i128, self.mcx.get_int_type(IntTy::I16))
    }

    fn const_i32(&self, i: i32) -> Self::Value {
        self.typed_scalar(i as i128, self.mcx.get_int_type(IntTy::I32))
    }

    fn const_i8(&self, i: i8) -> Self::Value {
        self.typed_scalar(i as i128, self.mcx.get_int_type(IntTy::I8))
    }

    fn const_u32(&self, i: u32) -> Self::Value {
        self.typed_scalar(i as i128, self.mcx.get_uint_type(UintTy::U32))
    }

    fn const_u64(&self, i: u64) -> Self::Value {
        self.typed_scalar(i as i128, self.mcx.get_uint_type(UintTy::U64))
    }

    fn const_u128(&self, i: u128) -> Self::Value {
        self.typed_scalar(i as i128, self.mcx.get_uint_type(UintTy::U128))
    }

    fn const_usize(&self, i: u64) -> Self::Value {
        self.typed_scalar(i as i128, self.mcx.get_uint_type(UintTy::Usize))
    }

    fn const_u8(&self, i: u8) -> Self::Value {
        self.typed_scalar(i as i128, self.mcx.get_uint_type(UintTy::U8))
    }

    fn const_real(&self, t: Self::Type, val: f64) -> Self::Value {
        let literal = match t {
            CTy::Float(CFloatTy::F32) => {
                if val.is_nan() {
                    "__builtin_nanf(\"\")".to_string()
                } else if val.is_infinite() {
                    if val.is_sign_negative() {
                        "-__builtin_inff()".to_string()
                    } else {
                        "__builtin_inff()".to_string()
                    }
                } else {
                    let v = val as f32;
                    format!("{:.9e}f", v)
                }
            }
            CTy::Float(CFloatTy::F64) => {
                if val.is_nan() {
                    "__builtin_nan(\"\")".to_string()
                } else if val.is_infinite() {
                    if val.is_sign_negative() {
                        "-__builtin_inf()".to_string()
                    } else {
                        "__builtin_inf()".to_string()
                    }
                } else {
                    format!("{:.17e}", val)
                }
            }
            _ => panic!("const_real expects f32/f64 destination type, got {t:?}"),
        };

        let literal = self.mcx.alloc_str(&literal);
        let value = CValue::RealLiteral(literal);
        if let Some(fkey) = self.current_fkey.get() {
            self.value_tys.borrow_mut().insert((fkey, value), t);
        }
        value
    }

    fn const_str(&self, s: &str) -> (Self::Value, Self::Value) {
        (
            self.const_bytes_pointer(s.as_bytes()),
            self.typed_scalar(s.len() as i128, self.mcx.get_uint_type(UintTy::Usize)),
        )
    }

    fn const_struct(&self, elts: &[Self::Value], packed: bool) -> Self::Value {
        todo!()
    }

    fn const_to_opt_uint(&self, v: Self::Value) -> Option<u64> {
        match v {
            CValue::Scalar(v) | CValue::ScalarTyped(v, _) if v >= 0 => Some(v as u64),
            _ => None,
        }
    }

    fn const_to_opt_u128(&self, v: Self::Value, sign_ext: bool) -> Option<u128> {
        match v {
            CValue::Scalar(v) | CValue::ScalarTyped(v, _) if v >= 0 => Some(v as u128),
            _ => None,
        }
    }

    fn const_data_from_alloc(&self, alloc: ConstAllocation<'_>) -> Self::Value {
        let alloc = alloc.inner();
        let bytes = alloc.inspect_with_uninit_and_ptr_outside_interpreter(0..alloc.len());
        let ptrs = alloc.provenance().ptrs();
        if ptrs.is_empty() {
            return self.const_bytes_pointer(bytes);
        }

        let ptr_size = self.tcx.data_layout.pointer_size().bytes() as usize;
        if ptr_size == 0
            || alloc.len() % ptr_size != 0
            || ptrs.iter().any(|(offset, _)| (offset.bytes() as usize) % ptr_size != 0)
        {
            panic!(
                "unsupported const allocation with relocations: len={} ptr_size={} relocs={}",
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
            let word = read_target_uint(endianness, &bytes[byte_offset..byte_offset + ptr_size])
                .unwrap_or_else(|_| {
                    panic!(
                        "failed to decode relocation word at offset {} (alloc len {})",
                        byte_offset,
                        alloc.len()
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

        let symbol = self.mcx.alloc_str(&format!("__rcgenc_const_alloc_{}", self.next_scalar_id()));
        let static_ty = CTy::Ref(Interned::new_unchecked(
            self.mcx.arena().alloc(CTyKind::Array(CTy::UInt(CUintTy::Usize), words)),
        ));
        let init = self.mcx.alloc_str(&format!("{{ {} }}", entries.join(", ")));
        self.mcx.module().push_decl(self.mcx.var_with_attrs(
            CValue::Func(symbol),
            static_ty,
            Some(self.mcx.value(CValue::Func(init))),
            CLinkage::Internal,
            CVisibility::Default,
        ));

        let ptr_expr = self.mcx.alloc_str(&format!("((uint8_t *)&{symbol})"));
        self.record_u8_ptr_value(CValue::Func(ptr_expr))
    }

    fn scalar_to_backend(
        &self,
        cv: Scalar,
        layout: rustc_abi::Scalar,
        llty: Self::Type,
    ) -> Self::Value {
        match cv {
            Scalar::Int(scalar) => match layout.primitive() {
                Primitive::Float(AbiFloat::F32) => {
                    self.const_real(llty, f32::from_bits(scalar.to_u32()) as f64)
                }
                Primitive::Float(AbiFloat::F64) => {
                    self.const_real(llty, f64::from_bits(scalar.to_u64()))
                }
                Primitive::Float(float) => {
                    panic!("unsupported float scalar constant in scalar_to_backend: {float:?}")
                }
                _ => self.typed_scalar(scalar.to_int(scalar.size()), llty),
            },
            Scalar::Ptr(ptr, _) => {
                let (prov, offset) = ptr.prov_and_relative_offset();
                let base = self.global_alloc_base_value(self.tcx.global_alloc(prov.alloc_id()));
                if matches!(llty, CTy::Int(_) | CTy::UInt(_)) {
                    let base_expr = value_expr_text(base);
                    let expr = if offset.bytes() == 0 {
                        self.mcx.alloc_str(&format!("((size_t)({base_expr}))"))
                    } else {
                        self.mcx.alloc_str(&format!("((size_t)({base_expr}) + {})", offset.bytes()))
                    };
                    CValue::Func(expr)
                } else {
                    self.const_ptr_byte_offset(base, offset)
                }
            }
        }
    }

    fn const_ptr_byte_offset(&self, val: Self::Value, offset: rustc_abi::Size) -> Self::Value {
        if offset.bytes() == 0 {
            return val;
        }

        match val {
            CValue::Scalar(v) => CValue::Scalar(v + offset.bytes() as i128),
            CValue::ScalarTyped(v, _) => {
                let ty = self
                    .current_fkey
                    .get()
                    .and_then(|fkey| self.value_tys.borrow().get(&(fkey, val)).copied());
                match ty {
                    Some(ty) => self.typed_scalar(v + offset.bytes() as i128, ty),
                    None => CValue::Scalar(v + offset.bytes() as i128),
                }
            }
            other => {
                let base = value_expr_text(other);
                let expr =
                    self.mcx.alloc_str(&format!("((uint8_t *)({base}) + {})", offset.bytes()));
                CValue::Func(expr)
            }
        }
    }
    fn const_vector(&self, _: &[Self::Value]) -> Self::Value {
        todo!()
    }
}
