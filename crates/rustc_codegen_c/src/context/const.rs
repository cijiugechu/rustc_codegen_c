use rustc_codegen_c_ast::expr::CValue;
use rustc_codegen_ssa::traits::ConstCodegenMethods;
use rustc_const_eval::interpret::{ConstAllocation, Scalar};

use crate::context::CodegenCx;

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
        todo!()
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
        todo!()
    }

    fn scalar_to_backend(
        &self,
        cv: Scalar,
        layout: rustc_abi::Scalar,
        llty: Self::Type,
    ) -> Self::Value {
        match cv {
            Scalar::Int(scalar) => CValue::Scalar(scalar.to_int(scalar.size())),
            // This backend does not materialize real constant addresses yet.
            // For panic location and similar metadata paths, a null pointer is sufficient.
            Scalar::Ptr(_, _) => CValue::Scalar(0),
        }
    }

    fn const_ptr_byte_offset(&self, val: Self::Value, offset: rustc_abi::Size) -> Self::Value {
        match val {
            CValue::Scalar(v) => CValue::Scalar(v + offset.bytes() as i128),
            _ => val,
        }
    }
    fn const_vector(&self, _: &[Self::Value]) -> Self::Value {
        todo!()
    }
}
