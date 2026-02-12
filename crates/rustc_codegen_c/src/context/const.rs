use rustc_codegen_c_ast::expr::CValue;
use rustc_codegen_ssa::traits::ConstCodegenMethods;
use rustc_const_eval::interpret::{ConstAllocation, Scalar};
use rustc_middle::mir::interpret::GlobalAlloc;

use crate::context::CodegenCx;

fn is_valid_c_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };

    if !(first == '_' || first.is_ascii_alphabetic()) {
        return false;
    }

    chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}

fn sanitize_symbol_name(symbol_name: &str) -> String {
    if is_valid_c_identifier(symbol_name) {
        return symbol_name.to_string();
    }

    let mut out = String::from("__rcgenc_");
    for byte in symbol_name.bytes() {
        if byte.is_ascii_alphanumeric() {
            out.push(byte as char);
        } else {
            use std::fmt::Write;
            let _ = write!(&mut out, "_{byte:02X}");
        }
    }
    out
}

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
        CValue::Scalar(v) => v.to_string(),
        CValue::Local(i) => format!("_{i}"),
        CValue::Func(name) => name.to_string(),
    }
}

impl<'tcx, 'mx> CodegenCx<'tcx, 'mx> {
    pub(crate) fn const_bytes_pointer(&self, bytes: &[u8]) -> CValue<'mx> {
        let literal = c_string_literal_from_bytes(bytes);
        let expr = self.mcx.alloc_str(&format!("((uint8_t *){literal})"));
        CValue::Func(expr)
    }

    pub(crate) fn symbol_value(&self, symbol_name: &str) -> CValue<'mx> {
        let symbol_name = sanitize_symbol_name(symbol_name);
        CValue::Func(self.mcx.alloc_str(&symbol_name))
    }
}

impl<'tcx, 'mx> ConstCodegenMethods for CodegenCx<'tcx, 'mx> {
    fn const_null(&self, t: Self::Type) -> Self::Value {
        CValue::Scalar(0)
    }

    fn const_undef(&self, t: Self::Type) -> Self::Value {
        CValue::Scalar(0)
    }

    fn const_poison(&self, t: Self::Type) -> Self::Value {
        CValue::Scalar(0)
    }

    fn const_int(&self, t: Self::Type, i: i64) -> Self::Value {
        CValue::Scalar(i as i128)
    }

    fn const_uint(&self, t: Self::Type, i: u64) -> Self::Value {
        CValue::Scalar(i as i128)
    }

    fn const_uint_big(&self, t: Self::Type, u: u128) -> Self::Value {
        CValue::Scalar(u as i128)
    }

    fn const_bool(&self, val: bool) -> Self::Value {
        CValue::Scalar(if val { 1 } else { 0 })
    }

    fn const_i16(&self, i: i16) -> Self::Value {
        CValue::Scalar(i as i128)
    }

    fn const_i32(&self, i: i32) -> Self::Value {
        CValue::Scalar(i as i128)
    }

    fn const_i8(&self, i: i8) -> Self::Value {
        CValue::Scalar(i as i128)
    }

    fn const_u32(&self, i: u32) -> Self::Value {
        CValue::Scalar(i as i128)
    }

    fn const_u64(&self, i: u64) -> Self::Value {
        CValue::Scalar(i as i128)
    }

    fn const_u128(&self, i: u128) -> Self::Value {
        CValue::Scalar(i as i128)
    }

    fn const_usize(&self, i: u64) -> Self::Value {
        CValue::Scalar(i as i128)
    }

    fn const_u8(&self, i: u8) -> Self::Value {
        CValue::Scalar(i as i128)
    }

    fn const_real(&self, t: Self::Type, val: f64) -> Self::Value {
        todo!()
    }

    fn const_str(&self, s: &str) -> (Self::Value, Self::Value) {
        (self.const_bytes_pointer(s.as_bytes()), CValue::Scalar(s.len() as i128))
    }

    fn const_struct(&self, elts: &[Self::Value], packed: bool) -> Self::Value {
        todo!()
    }

    fn const_to_opt_uint(&self, v: Self::Value) -> Option<u64> {
        match v {
            CValue::Scalar(v) if v >= 0 => Some(v as u64),
            _ => None,
        }
    }

    fn const_to_opt_u128(&self, v: Self::Value, sign_ext: bool) -> Option<u128> {
        match v {
            CValue::Scalar(v) if v >= 0 => Some(v as u128),
            _ => None,
        }
    }

    fn const_data_from_alloc(&self, alloc: ConstAllocation<'_>) -> Self::Value {
        let alloc = alloc.inner();
        let bytes = alloc.inspect_with_uninit_and_ptr_outside_interpreter(0..alloc.len());
        self.const_bytes_pointer(bytes)
    }

    fn scalar_to_backend(
        &self,
        cv: Scalar,
        layout: rustc_abi::Scalar,
        llty: Self::Type,
    ) -> Self::Value {
        match cv {
            Scalar::Int(scalar) => CValue::Scalar(scalar.to_int(scalar.size())),
            Scalar::Ptr(ptr, _) => {
                let (prov, offset) = ptr.prov_and_relative_offset();
                let base = match self.tcx.global_alloc(prov.alloc_id()) {
                    GlobalAlloc::Memory(alloc) => self.const_data_from_alloc(alloc),
                    GlobalAlloc::Function { instance, .. } => {
                        self.symbol_value(self.tcx.symbol_name(instance).name)
                    }
                    GlobalAlloc::Static(def_id) => {
                        let symbol = self.static_symbol(def_id);
                        let expr = self.mcx.alloc_str(&format!("((uint8_t *)&{symbol})"));
                        CValue::Func(expr)
                    }
                    GlobalAlloc::VTable(..) | GlobalAlloc::TypeId { .. } => CValue::Scalar(0),
                };
                self.const_ptr_byte_offset(base, offset)
            }
        }
    }

    fn const_ptr_byte_offset(&self, val: Self::Value, offset: rustc_abi::Size) -> Self::Value {
        if offset.bytes() == 0 {
            return val;
        }

        match val {
            CValue::Scalar(v) => CValue::Scalar(v + offset.bytes() as i128),
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
