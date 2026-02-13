use rustc_abi::{BackendRepr, Integer, Primitive, RegKind};
use rustc_codegen_c_ast::cstruct::{CStructDef, CStructField};
use rustc_codegen_c_ast::ty::{CIntTy, CTy, CTyKind, CUintTy};
use rustc_codegen_ssa::traits::LayoutTypeCodegenMethods;
use rustc_data_structures::intern::Interned;
use rustc_middle::ty::layout::{LayoutOf, TyAndLayout};
use rustc_middle::ty::Ty;
use rustc_target::callconv::FnAbi;
use rustc_type_ir::TyKind;

use crate::context::{AdtFieldLayout, AdtLayoutInfo, CodegenCx};

impl<'tcx, 'mx> CodegenCx<'tcx, 'mx> {
    fn is_fieldless_enum(&self, adt_def: rustc_middle::ty::AdtDef<'tcx>) -> bool {
        adt_def.is_enum() && adt_def.variants().iter().all(|variant| variant.fields.is_empty())
    }

    fn scalar_backend_type(&self, scalar: rustc_abi::Scalar) -> CTy<'mx> {
        match scalar.primitive() {
            Primitive::Int(int, signed) => match (int, signed) {
                (Integer::I8, true) => CTy::Int(CIntTy::I8),
                (Integer::I16, true) => CTy::Int(CIntTy::I16),
                (Integer::I32, true) => CTy::Int(CIntTy::I32),
                (Integer::I64, true) => CTy::Int(CIntTy::I64),
                (Integer::I8, false) => CTy::UInt(CUintTy::U8),
                (Integer::I16, false) => CTy::UInt(CUintTy::U16),
                (Integer::I32, false) => CTy::UInt(CUintTy::U32),
                (Integer::I64, false) => CTy::UInt(CUintTy::U64),
                (Integer::I128, _) => todo!("i128 scalar backend type is not supported yet"),
            },
            Primitive::Pointer(_) => CTy::Ref(Interned::new_unchecked(
                self.mcx.arena().alloc(CTyKind::Pointer(CTy::UInt(CUintTy::U8))),
            )),
            Primitive::Float(_) => todo!("float scalar backend type is not supported yet"),
        }
    }

    fn ptr_backend_type_for_ty(&self, ty: Ty<'tcx>) -> CTy<'mx> {
        let pointee = if ty.builtin_deref(true).is_some_and(|mt| {
            if let TyKind::Adt(adt_def, _) = mt.kind() {
                self.tcx.item_name(adt_def.did()).as_str() == "c_void"
            } else {
                false
            }
        }) {
            CTy::Void
        } else {
            CTy::UInt(CUintTy::U8)
        };

        CTy::Ref(Interned::new_unchecked(self.mcx.arena().alloc(CTyKind::Pointer(pointee))))
    }

    pub(crate) fn define_simple_struct_layout(
        &self,
        layout: TyAndLayout<'tcx>,
        adt_def: rustc_middle::ty::AdtDef<'tcx>,
        args: rustc_middle::ty::GenericArgsRef<'tcx>,
    ) -> CTy<'mx> {
        if let Some(ty) = self.adt_types.borrow().get(&layout.ty).copied() {
            return ty;
        }

        if !adt_def.is_struct() {
            panic!("only struct ADTs are supported, got {}", adt_def.descr());
        }

        let name = if adt_def.repr().c() && args.is_empty() {
            self.mcx.alloc_str(self.tcx.item_name(adt_def.did()).as_str())
        } else {
            self.mcx.alloc_str(&format!("__rcgenc_struct_{}", self.struct_layouts.borrow().len()))
        };
        let cty = CTy::Ref(Interned::new_unchecked(self.mcx.arena().alloc(CTyKind::Struct(name))));

        let variant = adt_def.non_enum_variant();
        let mut fields = Vec::with_capacity(variant.fields.len());
        for (i, field) in variant.fields.iter().enumerate() {
            let field_name = self.mcx.alloc_str(field.name.as_str());
            let field_layout = layout.field(&*self, i);
            let field_cty = if field_layout.size.bytes() == 0 {
                CTy::UInt(CUintTy::U8)
            } else {
                self.backend_type(field_layout)
            };
            fields.push(AdtFieldLayout {
                index: i,
                name: field_name,
                ty: field_cty,
                offset: layout.fields.offset(i).bytes_usize(),
                size: field_layout.size.bytes_usize(),
            });
        }

        let layout_info = AdtLayoutInfo {
            size: layout.size.bytes_usize(),
            align: layout.align.abi.bytes() as usize,
            repr_c: adt_def.repr().c(),
            fields: fields.clone(),
        };

        self.adt_types.borrow_mut().insert(layout.ty, cty);
        self.adt_layouts.borrow_mut().insert(layout.ty, layout_info.clone());
        self.struct_layouts.borrow_mut().insert(cty, layout_info.clone());

        if layout_info.repr_c {
            self.struct_types.borrow_mut().insert(adt_def.did(), cty);
            self.mcx.module().push_struct(CStructDef {
                name,
                fields: fields.iter().map(|f| CStructField { name: f.name, ty: f.ty }).collect(),
            });
        }

        cty
    }

    pub(crate) fn define_tuple_layout(&self, layout: TyAndLayout<'tcx>) -> CTy<'mx> {
        if let Some(ty) = self.adt_types.borrow().get(&layout.ty).copied() {
            return ty;
        }

        let TyKind::Tuple(fields_tys) = layout.ty.kind() else {
            panic!("expected tuple type, got {:?}", layout.ty.kind());
        };

        let name =
            self.mcx.alloc_str(&format!("__rcgenc_tuple_{}", self.struct_layouts.borrow().len()));
        let cty = CTy::Ref(Interned::new_unchecked(self.mcx.arena().alloc(CTyKind::Struct(name))));

        let mut fields = Vec::with_capacity(fields_tys.len());
        let mut struct_fields = Vec::with_capacity(fields_tys.len());
        for i in 0..fields_tys.len() {
            let field_name = self.mcx.alloc_str(&format!("f{i}"));
            let field_layout = layout.field(&*self, i);
            let field_cty = self.backend_type(field_layout);
            fields.push(AdtFieldLayout {
                index: i,
                name: field_name,
                ty: field_cty,
                offset: layout.fields.offset(i).bytes_usize(),
                size: field_layout.size.bytes_usize(),
            });
            struct_fields.push(CStructField { name: field_name, ty: field_cty });
        }

        let layout_info = AdtLayoutInfo {
            size: layout.size.bytes_usize(),
            align: layout.align.abi.bytes() as usize,
            repr_c: false,
            fields: fields.clone(),
        };

        self.adt_types.borrow_mut().insert(layout.ty, cty);
        self.adt_layouts.borrow_mut().insert(layout.ty, layout_info.clone());
        self.struct_layouts.borrow_mut().insert(cty, layout_info);
        self.mcx.module().push_struct(CStructDef { name, fields: struct_fields });
        cty
    }

    pub(crate) fn define_data_enum_layout(
        &self,
        layout: TyAndLayout<'tcx>,
        adt_def: rustc_middle::ty::AdtDef<'tcx>,
    ) -> CTy<'mx> {
        if let Some(ty) = self.adt_types.borrow().get(&layout.ty).copied() {
            return ty;
        }

        assert!(adt_def.is_enum(), "expected enum ADT, got {}", adt_def.descr());
        let size = layout.size.bytes_usize();
        assert!(size > 0, "data enum with zero-sized layout is not supported: {:?}", layout.ty);

        let name =
            self.mcx.alloc_str(&format!("__rcgenc_enum_{}", self.struct_layouts.borrow().len()));
        let cty = CTy::Ref(Interned::new_unchecked(self.mcx.arena().alloc(CTyKind::Struct(name))));
        let mut fields = Vec::with_capacity(size);
        let mut struct_fields = Vec::with_capacity(size);
        for i in 0..size {
            let field_name = self.mcx.alloc_str(&format!("b{i}"));
            fields.push(AdtFieldLayout {
                index: i,
                name: field_name,
                ty: CTy::UInt(CUintTy::U8),
                offset: i,
                size: 1,
            });
            struct_fields.push(CStructField { name: field_name, ty: CTy::UInt(CUintTy::U8) });
        }
        let layout_info = AdtLayoutInfo {
            size,
            align: layout.align.abi.bytes() as usize,
            repr_c: false,
            fields: fields.clone(),
        };

        self.adt_types.borrow_mut().insert(layout.ty, cty);
        self.adt_layouts.borrow_mut().insert(layout.ty, layout_info.clone());
        self.struct_layouts.borrow_mut().insert(cty, layout_info);
        self.mcx.module().push_struct(CStructDef { name, fields: struct_fields });
        cty
    }
}

impl<'tcx, 'mx> LayoutTypeCodegenMethods<'tcx> for CodegenCx<'tcx, 'mx> {
    fn backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        match layout.ty.kind() {
            TyKind::Bool => CTy::Bool,
            TyKind::Int(_) | TyKind::Uint(_) => self.immediate_backend_type(layout),
            TyKind::Array(elem_ty, _) => {
                let elem_layout = self.layout_of(*elem_ty);
                let elem = self.immediate_backend_type(elem_layout);
                let array = self.mcx.arena().alloc(CTyKind::Array(elem, layout.fields.count()));
                CTy::Ref(Interned::new_unchecked(array))
            }
            TyKind::Adt(adt_def, args) => {
                if self.is_fieldless_enum(*adt_def) {
                    self.immediate_backend_type(layout)
                } else if adt_def.is_enum() {
                    self.define_data_enum_layout(layout, *adt_def)
                } else {
                    self.define_simple_struct_layout(layout, *adt_def, args)
                }
            }
            TyKind::Tuple(..) => self.define_tuple_layout(layout),
            TyKind::Ref(..) | TyKind::RawPtr(..) => self.immediate_backend_type(layout),
            _ => todo!("unsupported backend_type: {:?}", layout.ty.kind()),
        }
    }

    fn cast_backend_type(&self, ty: &rustc_target::callconv::CastTarget) -> Self::Type {
        let fields =
            self.cast_target_to_c_abi_pieces(ty).into_iter().map(|(_, ty)| ty).collect::<Vec<_>>();
        match fields.as_slice() {
            [] => CTy::Void,
            [single] => *single,
            _ => self.abi_tuple_ty(&fields),
        }
    }

    fn fn_decl_backend_type(&self, fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        CTy::Void
    }

    fn fn_ptr_backend_type(&self, fn_abi: &FnAbi<'tcx, Ty<'tcx>>) -> Self::Type {
        todo!()
    }

    fn reg_backend_type(&self, ty: &rustc_abi::Reg) -> Self::Type {
        match (ty.kind, ty.size.bytes()) {
            (RegKind::Integer, 1) => CTy::UInt(CUintTy::U8),
            (RegKind::Integer, 2) => CTy::UInt(CUintTy::U16),
            (RegKind::Integer, 3..=4) => CTy::UInt(CUintTy::U32),
            (RegKind::Integer, 5..=8) => CTy::UInt(CUintTy::U64),
            _ => panic!("unsupported ABI register for C backend: {ty:?}"),
        }
    }

    fn immediate_backend_type(&self, layout: TyAndLayout<'tcx>) -> Self::Type {
        if let BackendRepr::Scalar(scalar) = layout.backend_repr {
            if matches!(layout.ty.kind(), TyKind::Bool) {
                return CTy::Bool;
            }
            return self.scalar_backend_type(scalar);
        }

        if let BackendRepr::ScalarPair(_, _) = layout.backend_repr {
            return self.abi_tuple_ty(&[
                self.scalar_pair_element_backend_type(layout, 0, true),
                self.scalar_pair_element_backend_type(layout, 1, true),
            ]);
        }

        match layout.ty.kind() {
            TyKind::Bool => CTy::Bool,
            TyKind::Int(int) => self.mcx.get_int_type(*int),
            TyKind::Uint(uint) => self.mcx.get_uint_type(*uint),
            TyKind::Never => CTy::Void,
            TyKind::Ref(..) | TyKind::RawPtr(..) => self.ptr_backend_type_for_ty(layout.ty),
            _ => todo!("unsupported immediate_backend_type: {:?}", layout.ty.kind()),
        }
    }

    fn is_backend_immediate(&self, layout: TyAndLayout<'tcx>) -> bool {
        match layout.backend_repr {
            BackendRepr::Scalar(_) | BackendRepr::SimdVector { .. } => true,
            BackendRepr::ScalarPair(..) | BackendRepr::Memory { .. } => false,
        }
    }

    fn is_backend_scalar_pair(&self, layout: TyAndLayout<'tcx>) -> bool {
        matches!(layout.backend_repr, BackendRepr::ScalarPair(..))
    }

    fn scalar_pair_element_backend_type(
        &self,
        layout: TyAndLayout<'tcx>,
        index: usize,
        immediate: bool,
    ) -> Self::Type {
        let (a, b) = match layout.backend_repr {
            BackendRepr::ScalarPair(a, b) => (a, b),
            _ => panic!("layout is not scalar pair: {layout:?}"),
        };

        let scalar = match index {
            0 => a,
            1 => b,
            _ => panic!("invalid scalar pair index: {index}"),
        };

        match scalar.primitive() {
            Primitive::Int(int, signed) => match (int, signed) {
                (Integer::I8, true) => CTy::Int(CIntTy::I8),
                (Integer::I16, true) => CTy::Int(CIntTy::I16),
                (Integer::I32, true) => CTy::Int(CIntTy::I32),
                (Integer::I64, true) => CTy::Int(CIntTy::I64),
                (Integer::I8, false) => CTy::UInt(CUintTy::U8),
                (Integer::I16, false) => CTy::UInt(CUintTy::U16),
                (Integer::I32, false) => CTy::UInt(CUintTy::U32),
                (Integer::I64, false) => CTy::UInt(CUintTy::U64),
                (Integer::I128, _) => todo!(),
            },
            Primitive::Pointer(_) => CTy::Ref(Interned::new_unchecked(
                self.mcx.arena().alloc(CTyKind::Pointer(CTy::UInt(CUintTy::U8))),
            )),
            Primitive::Float(_) => todo!(),
        }
    }
}
