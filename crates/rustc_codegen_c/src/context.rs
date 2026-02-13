#![allow(unused_variables)] // TODO

use rustc_middle::ty;
use rustc_middle::ty::layout::HasTypingEnv;
use std::cell::{Cell, RefCell};
use std::hash::{Hash, Hasher};

use rustc_abi::{HasDataLayout, Reg, RegKind, Size, TargetDataLayout};
use rustc_codegen_c_ast::cstruct::{CStructDef, CStructField};
use rustc_codegen_c_ast::expr::{CExpr, CValue};
use rustc_codegen_c_ast::func::CFunc;
use rustc_codegen_c_ast::ty::{CIntTy, CTy, CTyKind, CUintTy};
use rustc_codegen_c_ast::ModuleCtx;
use rustc_codegen_ssa::traits::{BackendTypes, LayoutTypeCodegenMethods};
use rustc_hash::FxHashMap;
use rustc_hir::def::DefKind;
use rustc_hir::def_id::DefId;
use rustc_middle::ty::layout::{
    FnAbiError, FnAbiOfHelpers, FnAbiRequest, HasTyCtxt, LayoutError, LayoutOf, LayoutOfHelpers,
    TyAndLayout,
};
use rustc_middle::ty::{Instance, Ty, TyCtxt};
use rustc_target::callconv::FnAbi;
use rustc_target::spec::{HasTargetSpec, Target};
use rustc_type_ir::TyKind;

mod asm;
mod base_type;
mod r#const;
mod debug_info;
mod layout_type;
mod misc;
mod pre_define;
mod r#static;
mod type_membership;

#[derive(Debug, Clone, Copy)]
pub struct CBasicBlock<'mx> {
    pub func: CFunc<'mx>,
    pub label: &'mx str,
}

#[derive(Debug, Clone, Copy)]
pub struct PendingAlloca {
    pub bytes: usize,
    pub declared: bool,
}

#[derive(Debug, Clone)]
pub struct AdtFieldLayout<'mx> {
    pub index: usize,
    pub name: &'mx str,
    pub ty: CTy<'mx>,
    pub offset: usize,
    pub size: usize,
}

#[derive(Debug, Clone)]
pub struct AdtLayoutInfo<'mx> {
    pub size: usize,
    pub align: usize,
    pub repr_c: bool,
    pub fields: Vec<AdtFieldLayout<'mx>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum AbiTupleFieldKey<'mx> {
    Void,
    Bool,
    Char,
    Int(CIntTy),
    UInt(CUintTy),
    Pointer(Box<AbiTupleFieldKey<'mx>>),
    Array(Box<AbiTupleFieldKey<'mx>>, usize),
    Struct(&'mx str),
}

impl PartialEq for CBasicBlock<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.func.0, other.func.0) && self.label == other.label
    }
}

impl Eq for CBasicBlock<'_> {}

impl Hash for CBasicBlock<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.func.0 as *const _ as usize).hash(state);
        self.label.hash(state);
    }
}

/// Codegen context.
///
/// It provides the global context for code generation.
pub struct CodegenCx<'tcx, 'mx> {
    /// The type context. See [`TyCtxt`].
    pub tcx: TyCtxt<'tcx>,
    /// The output and context for the outputed module.
    pub mcx: ModuleCtx<'mx>,
    /// Mapping from Rust function instances to their corresponding C functions.
    pub function_instances: RefCell<FxHashMap<Instance<'tcx>, CFunc<'mx>>>,
    /// Cached exception personality function symbol.
    pub eh_personality_fn: RefCell<Option<CFunc<'mx>>>,
    /// Mapping from Rust static definitions to their generated C symbols.
    pub static_symbols: RefCell<FxHashMap<DefId, &'mx str>>,
    /// Monotonic counter for assigning unique identities to typed scalar constants.
    pub scalar_ids: Cell<u64>,
    /// Per-function value type cache shared across basic blocks.
    pub value_tys: RefCell<FxHashMap<(usize, CValue<'mx>), CTy<'mx>>>,
    /// The currently active function key for value type queries through `CodegenCx`.
    pub current_fkey: Cell<Option<usize>>,
    /// Per-function virtual packed scalar pairs used by extract/insert helpers.
    pub packed_scalar_pairs: RefCell<FxHashMap<(usize, CValue<'mx>), (CValue<'mx>, CValue<'mx>)>>,
    /// Per-function pointer pointee metadata.
    pub ptr_pointees: RefCell<FxHashMap<(usize, CValue<'mx>), CTy<'mx>>>,
    /// Per-function pointer lvalue expressions from GEP-like projections.
    pub ptr_lvalues: RefCell<FxHashMap<(usize, CValue<'mx>), CExpr<'mx>>>,
    /// Per-function pending stack allocas to be materialized as declarations.
    pub pending_allocas: RefCell<FxHashMap<(usize, CValue<'mx>), PendingAlloca>>,
    /// Mapping from Rust ADT definitions to C struct marker types.
    pub struct_types: RefCell<FxHashMap<DefId, CTy<'mx>>>,
    /// Mapping from monomorphized ADT type to C struct marker types.
    pub adt_types: RefCell<FxHashMap<Ty<'tcx>, CTy<'mx>>>,
    /// Mapping from monomorphized ADT type to computed layout metadata.
    pub adt_layouts: RefCell<FxHashMap<Ty<'tcx>, AdtLayoutInfo<'mx>>>,
    /// Mapping from C struct marker type to computed layout metadata.
    pub struct_layouts: RefCell<FxHashMap<CTy<'mx>, AdtLayoutInfo<'mx>>>,
    /// Cache of synthetic tuple-like ABI struct types used for multi-register aggregates.
    pub abi_tuple_structs: RefCell<FxHashMap<Vec<AbiTupleFieldKey<'mx>>, CTy<'mx>>>,
    /// Field names for each synthetic ABI tuple-like struct type.
    pub abi_tuple_fields: RefCell<FxHashMap<CTy<'mx>, Vec<&'mx str>>>,
    /// Field types for each synthetic ABI tuple-like struct type.
    pub abi_tuple_field_tys: RefCell<FxHashMap<CTy<'mx>, Vec<CTy<'mx>>>>,
    /// Monotonic counter for synthetic C struct/type names.
    pub synthetic_type_ids: Cell<usize>,
}

impl<'tcx, 'mx> CodegenCx<'tcx, 'mx> {
    pub fn new(tcx: TyCtxt<'tcx>, mcx: ModuleCtx<'mx>) -> Self {
        mcx.module().push_include("stdint.h");
        mcx.module().push_include("stddef.h");
        mcx.module().push_include("stdlib.h");
        mcx.module().push_include("stdio.h");
        let cx = Self {
            tcx,
            mcx,
            function_instances: RefCell::new(FxHashMap::default()),
            eh_personality_fn: RefCell::new(None),
            static_symbols: RefCell::new(FxHashMap::default()),
            scalar_ids: Cell::new(0),
            value_tys: RefCell::new(FxHashMap::default()),
            current_fkey: Cell::new(None),
            packed_scalar_pairs: RefCell::new(FxHashMap::default()),
            ptr_pointees: RefCell::new(FxHashMap::default()),
            ptr_lvalues: RefCell::new(FxHashMap::default()),
            pending_allocas: RefCell::new(FxHashMap::default()),
            struct_types: RefCell::new(FxHashMap::default()),
            adt_types: RefCell::new(FxHashMap::default()),
            adt_layouts: RefCell::new(FxHashMap::default()),
            struct_layouts: RefCell::new(FxHashMap::default()),
            abi_tuple_structs: RefCell::new(FxHashMap::default()),
            abi_tuple_fields: RefCell::new(FxHashMap::default()),
            abi_tuple_field_tys: RefCell::new(FxHashMap::default()),
            synthetic_type_ids: Cell::new(0),
        };
        cx.predeclare_repr_c_structs();
        cx
    }

    pub(crate) fn next_synthetic_type_id(&self) -> usize {
        let id = self.synthetic_type_ids.get();
        self.synthetic_type_ids.set(id + 1);
        id
    }

    pub(crate) fn register_static_symbol(&self, def_id: DefId, symbol_name: &str) -> &'mx str {
        if let Some(symbol) = self.static_symbols.borrow().get(&def_id).copied() {
            return symbol;
        }

        let symbol_name = sanitize_symbol_name(symbol_name);
        let symbol_name = self.mcx.alloc_str(&symbol_name);
        self.static_symbols.borrow_mut().insert(def_id, symbol_name);
        symbol_name
    }

    pub(crate) fn static_symbol(&self, def_id: DefId) -> &'mx str {
        if let Some(symbol) = self.static_symbols.borrow().get(&def_id).copied() {
            return symbol;
        }

        let instance = Instance::mono(self.tcx, def_id);
        self.register_static_symbol(def_id, self.tcx.symbol_name(instance).name)
    }

    fn predeclare_repr_c_structs(&self) {
        for def_id in self.tcx.hir_crate_items(()).definitions() {
            if self.tcx.def_kind(def_id) != DefKind::Struct {
                continue;
            }

            let ty = self.tcx.type_of(def_id).instantiate_identity();
            let rustc_type_ir::TyKind::Adt(adt_def, args) = ty.kind() else {
                continue;
            };

            if !adt_def.repr().c() {
                continue;
            }

            let variant = adt_def.non_enum_variant();
            let all_primitive_ints = variant.fields.iter().all(|field| {
                let field_ty = field.ty(self.tcx, args);
                matches!(
                    field_ty.kind(),
                    rustc_type_ir::TyKind::Bool
                        | rustc_type_ir::TyKind::Int(_)
                        | rustc_type_ir::TyKind::Uint(_)
                )
            });

            if all_primitive_ints {
                let layout = self.layout_of(ty);
                self.define_simple_struct_layout(layout, *adt_def, args);
            }
        }
    }

    pub(crate) fn reg_to_c_ty(&self, reg: Reg) -> CTy<'mx> {
        match (reg.kind, reg.size.bytes()) {
            (RegKind::Integer, 1) => CTy::UInt(CUintTy::U8),
            (RegKind::Integer, 2) => CTy::UInt(CUintTy::U16),
            (RegKind::Integer, 3..=4) => CTy::UInt(CUintTy::U32),
            (RegKind::Integer, 5..=8) => CTy::UInt(CUintTy::U64),
            _ => panic!("unsupported ABI register for C backend: {reg:?}"),
        }
    }

    pub(crate) fn cast_target_to_c_abi_pieces(
        &self,
        cast: &rustc_target::callconv::CastTarget,
    ) -> Vec<(usize, CTy<'mx>)> {
        let mut out = Vec::new();

        if let Some(offset_from_start) = cast.rest_offset {
            let first = cast.prefix[0].expect("invalid cast target without first prefix register");
            out.push((0, self.reg_to_c_ty(first)));
            out.push((offset_from_start.bytes_usize(), self.reg_to_c_ty(cast.rest.unit)));
            return out;
        }

        let mut offset = 0usize;
        for reg in cast.prefix.iter().flatten() {
            out.push((offset, self.reg_to_c_ty(*reg)));
            offset += reg.size.bytes_usize();
        }

        let rest_unit_bytes = cast.rest.unit.size.bytes_usize();
        if rest_unit_bytes != 0 {
            let rest_total = cast.rest.total.bytes_usize();
            let rest_count = rest_total / rest_unit_bytes;
            let rem_bytes = rest_total % rest_unit_bytes;

            for _ in 0..rest_count {
                out.push((offset, self.reg_to_c_ty(cast.rest.unit)));
                offset += rest_unit_bytes;
            }

            if rem_bytes != 0 {
                out.push((
                    offset,
                    self.reg_to_c_ty(Reg {
                        kind: RegKind::Integer,
                        size: Size::from_bytes(rem_bytes as u64),
                    }),
                ));
            }
        }

        out
    }

    pub(crate) fn abi_tuple_ty(&self, fields: &[CTy<'mx>]) -> CTy<'mx> {
        let key = fields
            .iter()
            .copied()
            .map(|field_ty| self.abi_tuple_field_key(field_ty))
            .collect::<Vec<_>>();
        if let Some(ty) = self.abi_tuple_structs.borrow().get(&key).copied() {
            return ty;
        }

        let idx = self.abi_tuple_structs.borrow().len();
        let name = self.mcx.alloc_str(&format!("__rcgenc_abi_tuple_{idx}"));
        let ty = CTy::Ref(rustc_data_structures::intern::Interned::new_unchecked(
            self.mcx.arena().alloc(CTyKind::Struct(name)),
        ));

        let mut field_names = Vec::with_capacity(fields.len());
        let mut struct_fields = Vec::with_capacity(fields.len());
        let mut layout_fields = Vec::with_capacity(fields.len());
        let mut offset = 0usize;
        let mut struct_align = 1usize;
        for (i, field_ty) in fields.iter().copied().enumerate() {
            let field_name = self.mcx.alloc_str(&format!("f{i}"));
            field_names.push(field_name);
            struct_fields.push(CStructField { name: field_name, ty: field_ty });

            let (field_size, field_align) = self.c_ty_size_align(field_ty).unwrap_or_else(|| {
                panic!("missing size/alignment for ABI tuple field type {field_ty:?}")
            });
            if field_align > 1 {
                offset = (offset + (field_align - 1)) & !(field_align - 1);
            }
            layout_fields.push(AdtFieldLayout {
                index: i,
                name: field_name,
                ty: field_ty,
                offset,
                size: field_size,
            });
            offset += field_size;
            struct_align = struct_align.max(field_align);
        }

        let struct_size = if struct_align > 1 {
            (offset + (struct_align - 1)) & !(struct_align - 1)
        } else {
            offset
        };

        self.mcx.module().push_struct(CStructDef { name, fields: struct_fields });
        self.struct_layouts.borrow_mut().insert(
            ty,
            AdtLayoutInfo {
                size: struct_size,
                align: struct_align,
                repr_c: true,
                fields: layout_fields,
            },
        );
        self.abi_tuple_structs.borrow_mut().insert(key, ty);
        self.abi_tuple_fields.borrow_mut().insert(ty, field_names);
        self.abi_tuple_field_tys.borrow_mut().insert(ty, fields.to_vec());
        ty
    }

    fn abi_tuple_field_key(&self, ty: CTy<'mx>) -> AbiTupleFieldKey<'mx> {
        match ty {
            CTy::Void => AbiTupleFieldKey::Void,
            CTy::Bool => AbiTupleFieldKey::Bool,
            CTy::Char => AbiTupleFieldKey::Char,
            CTy::Int(int) => AbiTupleFieldKey::Int(int),
            CTy::UInt(int) => AbiTupleFieldKey::UInt(int),
            CTy::Ref(kind) => match kind.0 {
                CTyKind::Pointer(elem) => {
                    AbiTupleFieldKey::Pointer(Box::new(self.abi_tuple_field_key(*elem)))
                }
                CTyKind::Array(elem, len) => {
                    AbiTupleFieldKey::Array(Box::new(self.abi_tuple_field_key(*elem)), *len)
                }
                CTyKind::Struct(name) => AbiTupleFieldKey::Struct(name),
            },
        }
    }

    fn c_ty_size_align(&self, ty: CTy<'mx>) -> Option<(usize, usize)> {
        match ty {
            CTy::Void => Some((0, 1)),
            CTy::Bool => Some((1, 1)),
            CTy::Char => Some((1, 1)),
            CTy::Int(int) => {
                let bytes = match int {
                    rustc_codegen_c_ast::ty::CIntTy::I8 => 1,
                    rustc_codegen_c_ast::ty::CIntTy::I16 => 2,
                    rustc_codegen_c_ast::ty::CIntTy::I32 => 4,
                    rustc_codegen_c_ast::ty::CIntTy::I64 => 8,
                    rustc_codegen_c_ast::ty::CIntTy::Isize => {
                        self.tcx.data_layout.pointer_size().bytes() as usize
                    }
                };
                Some((bytes, bytes))
            }
            CTy::UInt(int) => {
                let bytes = match int {
                    CUintTy::U8 => 1,
                    CUintTy::U16 => 2,
                    CUintTy::U32 => 4,
                    CUintTy::U64 => 8,
                    CUintTy::Usize => self.tcx.data_layout.pointer_size().bytes() as usize,
                };
                Some((bytes, bytes))
            }
            CTy::Ref(kind) => match kind.0 {
                CTyKind::Pointer(_) => {
                    let bytes = self.tcx.data_layout.pointer_size().bytes() as usize;
                    Some((bytes, bytes))
                }
                CTyKind::Array(elem, count) => {
                    let (elem_size, elem_align) = self.c_ty_size_align(*elem)?;
                    Some((elem_size.saturating_mul(*count), elem_align))
                }
                CTyKind::Struct(_) => {
                    let info = self.struct_layouts.borrow().get(&ty)?.clone();
                    Some((info.size, info.align))
                }
            },
        }
    }

    pub(crate) fn abi_tuple_field_name(&self, tuple_ty: CTy<'mx>, idx: usize) -> &'mx str {
        *self
            .abi_tuple_fields
            .borrow()
            .get(&tuple_ty)
            .and_then(|fields| fields.get(idx))
            .unwrap_or_else(|| panic!("missing ABI tuple field {idx} for {tuple_ty:?}"))
    }

    pub(crate) fn abi_tuple_field_ty(&self, tuple_ty: CTy<'mx>, idx: usize) -> CTy<'mx> {
        *self
            .abi_tuple_field_tys
            .borrow()
            .get(&tuple_ty)
            .and_then(|fields| fields.get(idx))
            .unwrap_or_else(|| panic!("missing ABI tuple field type {idx} for {tuple_ty:?}"))
    }

    pub(crate) fn indirect_ptr_ty_for_layout(&self, layout: TyAndLayout<'tcx>) -> CTy<'mx> {
        let pointee = match layout.ty.kind() {
            TyKind::Adt(..) | TyKind::Array(..) | TyKind::Tuple(..) => self.backend_type(layout),
            _ => CTy::UInt(CUintTy::U8),
        };
        CTy::Ref(rustc_data_structures::intern::Interned::new_unchecked(
            self.mcx.arena().alloc(CTyKind::Pointer(pointee)),
        ))
    }
}

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

impl<'tcx, 'mx> BackendTypes for CodegenCx<'tcx, 'mx> {
    type Value = CValue<'mx>;
    type Metadata = CValue<'mx>;
    type Function = CFunc<'mx>;
    type BasicBlock = CBasicBlock<'mx>;
    type Type = CTy<'mx>;
    type Funclet = ();
    type DIScope = ();
    type DILocation = ();
    type DIVariable = ();
}

impl<'tcx, 'mx> HasTargetSpec for CodegenCx<'tcx, 'mx> {
    fn target_spec(&self) -> &Target {
        todo!()
    }
}

impl<'tcx, 'mx> HasTypingEnv<'tcx> for CodegenCx<'tcx, 'mx> {
    fn typing_env(&self) -> ty::TypingEnv<'tcx> {
        ty::TypingEnv::fully_monomorphized()
    }
}

impl<'tcx, 'mx> HasTyCtxt<'tcx> for CodegenCx<'tcx, 'mx> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.tcx
    }
}

impl<'tcx, 'mx> HasDataLayout for CodegenCx<'tcx, 'mx> {
    fn data_layout(&self) -> &TargetDataLayout {
        &self.tcx.data_layout
    }
}

impl<'tcx, 'mx> LayoutOfHelpers<'tcx> for CodegenCx<'tcx, 'mx> {
    type LayoutOfResult = TyAndLayout<'tcx>;

    fn handle_layout_err(&self, err: LayoutError<'tcx>, span: rustc_span::Span, ty: Ty<'tcx>) -> ! {
        todo!()
    }
}

impl<'tcx, 'mx> FnAbiOfHelpers<'tcx> for CodegenCx<'tcx, 'mx> {
    type FnAbiOfResult = &'tcx FnAbi<'tcx, Ty<'tcx>>;

    fn handle_fn_abi_err(
        &self,
        err: FnAbiError<'tcx>,
        span: rustc_span::Span,
        fn_abi_request: FnAbiRequest<'tcx>,
    ) -> ! {
        todo!()
    }
}
