use rustc_codegen_ssa::traits::BaseTypeCodegenMethods;
use rustc_data_structures::intern::Interned;
use rustc_type_ir::{IntTy, UintTy};

use rustc_codegen_c_ast::ty::{CTy, CTyKind};

use crate::context::CodegenCx;

impl<'tcx, 'mx> BaseTypeCodegenMethods for CodegenCx<'tcx, 'mx> {
    fn type_i8(&self) -> Self::Type {
        self.mcx.get_int_type(IntTy::I8)
    }

    fn type_i16(&self) -> Self::Type {
        self.mcx.get_int_type(IntTy::I16)
    }

    fn type_i32(&self) -> Self::Type {
        self.mcx.get_int_type(IntTy::I32)
    }

    fn type_i64(&self) -> Self::Type {
        self.mcx.get_int_type(IntTy::I64)
    }

    fn type_i128(&self) -> Self::Type {
        todo!()
    }

    fn type_isize(&self) -> Self::Type {
        self.mcx.get_int_type(IntTy::Isize)
    }

    fn type_f16(&self) -> Self::Type {
        todo!()
    }

    fn type_f32(&self) -> Self::Type {
        todo!()
    }

    fn type_f64(&self) -> Self::Type {
        todo!()
    }

    fn type_f128(&self) -> Self::Type {
        todo!()
    }

    fn type_array(&self, ty: Self::Type, len: u64) -> Self::Type {
        CTy::Ref(Interned::new_unchecked(self.mcx.arena().alloc(CTyKind::Array(ty, len as usize))))
    }

    fn type_func(&self, args: &[Self::Type], ret: Self::Type) -> Self::Type {
        CTy::Ref(Interned::new_unchecked(
            self.mcx.arena().alloc(CTyKind::Function { ret, params: args.to_vec() }),
        ))
    }

    fn type_kind(&self, ty: Self::Type) -> rustc_codegen_ssa::common::TypeKind {
        match ty {
            CTy::Void => rustc_codegen_ssa::common::TypeKind::Void,
            CTy::Bool | CTy::Char | CTy::Int(_) | CTy::UInt(_) => {
                rustc_codegen_ssa::common::TypeKind::Integer
            }
            CTy::Ref(kind) => match kind.0 {
                CTyKind::Pointer(_) => rustc_codegen_ssa::common::TypeKind::Pointer,
                CTyKind::Array(_, _) => rustc_codegen_ssa::common::TypeKind::Array,
                CTyKind::Function { .. } => rustc_codegen_ssa::common::TypeKind::Function,
                CTyKind::Struct(_) => rustc_codegen_ssa::common::TypeKind::Struct,
            },
        }
    }

    fn type_ptr(&self) -> Self::Type {
        CTy::Ref(Interned::new_unchecked(
            self.mcx.arena().alloc(CTyKind::Pointer(self.mcx.get_uint_type(UintTy::U8))),
        ))
    }

    fn type_ptr_ext(&self, address_space: rustc_abi::AddressSpace) -> Self::Type {
        self.type_ptr()
    }

    fn element_type(&self, ty: Self::Type) -> Self::Type {
        match ty {
            CTy::Ref(kind) => match kind.0 {
                CTyKind::Pointer(inner) | CTyKind::Array(inner, _) => *inner,
                CTyKind::Function { .. } => panic!("function has no scalar element type"),
                CTyKind::Struct(_) => panic!("struct has no scalar element type"),
            },
            _ => panic!("not an aggregate type: {ty:?}"),
        }
    }

    fn vector_length(&self, ty: Self::Type) -> usize {
        todo!()
    }

    fn float_width(&self, ty: Self::Type) -> usize {
        todo!()
    }

    fn int_width(&self, ty: Self::Type) -> u64 {
        match ty {
            CTy::Int(int) => match int {
                rustc_codegen_c_ast::ty::CIntTy::Isize => {
                    self.tcx.data_layout.pointer_size().bits()
                }
                rustc_codegen_c_ast::ty::CIntTy::I8 => 8,
                rustc_codegen_c_ast::ty::CIntTy::I16 => 16,
                rustc_codegen_c_ast::ty::CIntTy::I32 => 32,
                rustc_codegen_c_ast::ty::CIntTy::I64 => 64,
            },
            CTy::UInt(int) => match int {
                rustc_codegen_c_ast::ty::CUintTy::Usize => {
                    self.tcx.data_layout.pointer_size().bits()
                }
                rustc_codegen_c_ast::ty::CUintTy::U8 => 8,
                rustc_codegen_c_ast::ty::CUintTy::U16 => 16,
                rustc_codegen_c_ast::ty::CUintTy::U32 => 32,
                rustc_codegen_c_ast::ty::CUintTy::U64 => 64,
            },
            _ => panic!("not an integer type: {ty:?}"),
        }
    }

    fn val_ty(&self, v: Self::Value) -> Self::Type {
        if let Some(fkey) = self.current_fkey.get() {
            if let Some(ty) = self.value_tys.borrow().get(&(fkey, v)).copied() {
                return ty;
            }
        }

        if let rustc_codegen_c_ast::expr::CValue::Func(_) = v {
            return self.type_ptr();
        }

        panic!("val_ty is not known for value {v:?}")
    }
}
