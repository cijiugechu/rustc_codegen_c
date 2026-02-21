#![allow(unused_variables)] // TODO

use rustc_middle::ty;
use rustc_middle::ty::layout::HasTypingEnv;
use std::cell::{Cell, RefCell};
use std::hash::{Hash, Hasher};

use rustc_abi::{HasDataLayout, Reg, RegKind, Size, TargetDataLayout};
use rustc_codegen_c_ast::cstruct::{CStructDef, CStructField};
use rustc_codegen_c_ast::expr::{CExpr, CValue};
use rustc_codegen_c_ast::func::CFunc;
use rustc_codegen_c_ast::symbol::{CLinkage, CVisibility};
use rustc_codegen_c_ast::ty::{CFloatTy, CIntTy, CTy, CTyKind, CUintTy};
use rustc_codegen_c_ast::ModuleCtx;
use rustc_codegen_ssa::traits::{BackendTypes, LayoutTypeCodegenMethods};
use rustc_hash::{FxHashMap, FxHashSet};
use rustc_hir::def::DefKind;
use rustc_hir::def_id::DefId;
use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
use rustc_middle::ty::layout::{
    FnAbiError, FnAbiOfHelpers, FnAbiRequest, HasTyCtxt, LayoutError, LayoutOf, LayoutOfHelpers,
    TyAndLayout,
};
use rustc_middle::ty::{ExistentialTraitRef, Instance, Ty, TyCtxt};
use rustc_target::callconv::{ArgAbi, FnAbi, PassMode};
use rustc_target::spec::{HasTargetSpec, Target};
use rustc_type_ir::TyKind;

use crate::config::{BackendConfig, CStandard};
use crate::include_plan::{IncludeCapability, IncludePlanner};

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
    pub prefer_bytes: bool,
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

#[derive(Debug, Clone, Copy)]
pub(crate) struct StaticSymbolInfo<'mx> {
    pub decl_name: &'mx str,
    pub link_name: Option<&'mx str>,
    pub linkage: CLinkage,
    pub visibility: CVisibility,
    pub thread_local: bool,
    pub is_local_definition_candidate: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum StaticExternDeclAction {
    EmitExtern,
    Skip,
    ConflictLocalAlreadyDeclared,
}

const STATIC_SYMBOL_CONFLICT_MSG: &str = "inconsistent static symbol materialization state";
const STATIC_SYMBOL_DEFINED_TWICE_MSG: &str = "static symbol defined more than once in one CGU";
const STATIC_SYMBOL_EXTERN_DEFINED_MSG: &str =
    "static symbol was emitted as extern declaration and then as definition in one CGU";
const STATIC_SYMBOL_EXTERNAL_DEFINITION_MSG: &str =
    "extern-only static symbol unexpectedly reached local definition path";
const INCOMPLETE_ARRAY_DECL_ONLY_MSG: &str = "incomplete array is declaration-only";

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum AbiTupleFieldKey<'mx> {
    Void,
    Bool,
    Char,
    Int(CIntTy),
    UInt(CUintTy),
    Float(CFloatTy),
    Pointer(Box<AbiTupleFieldKey<'mx>>),
    Array(Box<AbiTupleFieldKey<'mx>>, usize),
    IncompleteArray(Box<AbiTupleFieldKey<'mx>>),
    Function { ret: Box<AbiTupleFieldKey<'mx>>, params: Vec<AbiTupleFieldKey<'mx>> },
    Struct(&'mx str),
    Union(&'mx str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CAbiParamRole {
    StructReturn,
    Regular,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct CAbiParam<'mx> {
    pub role: CAbiParamRole,
    pub ty: CTy<'mx>,
}

#[derive(Debug, Clone)]
pub(crate) struct CAbiSignature<'mx> {
    pub ret: CTy<'mx>,
    pub params: Vec<CAbiParam<'mx>>,
}

impl<'mx> CAbiSignature<'mx> {
    pub(crate) fn has_struct_return(&self) -> bool {
        self.params.first().is_some_and(|param| param.role == CAbiParamRole::StructReturn)
    }

    pub(crate) fn param_tys(&self) -> Vec<CTy<'mx>> {
        self.params.iter().map(|param| param.ty).collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CAbiSignatureFlavor {
    LoweredRust,
    NativeC,
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
    /// Active backend configuration C language standard.
    pub c_std: CStandard,
    /// The output and context for the outputed module.
    pub mcx: ModuleCtx<'mx>,
    /// Mapping from Rust function instances to their corresponding C functions.
    pub function_instances: RefCell<FxHashMap<Instance<'tcx>, CFunc<'mx>>>,
    /// Cache of generated dynamic vtable pointers.
    pub vtables: RefCell<FxHashMap<(Ty<'tcx>, Option<ExistentialTraitRef<'tcx>>), CValue<'mx>>>,
    /// Cached exception personality function symbol.
    pub eh_personality_fn: RefCell<Option<CFunc<'mx>>>,
    /// Mapping from Rust static definitions to their generated C symbol metadata.
    pub static_symbol_infos: RefCell<FxHashMap<DefId, StaticSymbolInfo<'mx>>>,
    /// Set of statics that already emitted an extern declaration in this module.
    pub declared_statics: RefCell<FxHashSet<DefId>>,
    /// Set of statics that already emitted a local definition in this module.
    pub defined_statics: RefCell<FxHashSet<DefId>>,
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
        let backend_config = BackendConfig::from_opts(&tcx.sess.opts.cg.llvm_args)
            .map_err(|err| tcx.sess.dcx().fatal(format!("invalid rustc_codegen_c option: {err}")))
            .unwrap();

        let mut include_planner = IncludePlanner::new(backend_config.c_std);
        include_planner.require(IncludeCapability::StdIntTypes);
        include_planner.require(IncludeCapability::SizeTypes);
        include_planner.require(IncludeCapability::AbortApi);
        include_planner
            .apply_to_module(mcx.module())
            .map_err(|err| tcx.sess.dcx().fatal(err))
            .unwrap();

        let cx = Self {
            tcx,
            c_std: backend_config.c_std,
            mcx,
            function_instances: RefCell::new(FxHashMap::default()),
            vtables: RefCell::new(FxHashMap::default()),
            eh_personality_fn: RefCell::new(None),
            static_symbol_infos: RefCell::new(FxHashMap::default()),
            declared_statics: RefCell::new(FxHashSet::default()),
            defined_statics: RefCell::new(FxHashSet::default()),
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

    pub(crate) fn require_include_capability(&self, capability: IncludeCapability) {
        let mut include_planner = IncludePlanner::new(self.c_std);
        include_planner.require(capability);
        include_planner
            .apply_to_module(self.mcx.module())
            .map_err(|err| self.tcx.sess.dcx().fatal(err))
            .unwrap();
    }

    pub(crate) fn next_synthetic_type_id(&self) -> usize {
        let id = self.synthetic_type_ids.get();
        self.synthetic_type_ids.set(id + 1);
        id
    }

    fn default_static_symbol_info(&self, def_id: DefId) -> StaticSymbolInfo<'mx> {
        let instance = Instance::mono(self.tcx, def_id);
        let symbol_name = self.tcx.symbol_name(instance).name;
        let (decl_name, link_name) = self.declaration_symbol_names(symbol_name);
        let thread_local =
            self.tcx.codegen_fn_attrs(def_id).flags.contains(CodegenFnAttrFlags::THREAD_LOCAL);
        StaticSymbolInfo {
            decl_name: self.mcx.alloc_str(&decl_name),
            link_name: link_name.as_ref().map(|name| self.mcx.alloc_str(name)),
            linkage: CLinkage::External,
            visibility: CVisibility::Default,
            thread_local,
            is_local_definition_candidate: false,
        }
    }

    pub(crate) fn register_static_symbol_info(
        &self,
        def_id: DefId,
        symbol_name: &str,
        linkage: CLinkage,
        visibility: CVisibility,
        is_local_definition_candidate: bool,
    ) -> StaticSymbolInfo<'mx> {
        let (decl_name, link_name) = self.declaration_symbol_names(symbol_name);
        let decl_name = self.mcx.alloc_str(&decl_name);
        let link_name = link_name.as_ref().map(|name| self.mcx.alloc_str(name));
        let thread_local =
            self.tcx.codegen_fn_attrs(def_id).flags.contains(CodegenFnAttrFlags::THREAD_LOCAL);

        let mut infos = self.static_symbol_infos.borrow_mut();
        if let Some(info) = infos.get_mut(&def_id) {
            if info.decl_name != decl_name || info.link_name != link_name {
                self.tcx.sess.dcx().fatal(format!(
                    "{STATIC_SYMBOL_CONFLICT_MSG}: static {def_id:?} maps to both `{}` and `{}`",
                    info.decl_name, decl_name
                ));
            }
            info.linkage = linkage;
            info.visibility = visibility;
            info.thread_local |= thread_local;
            info.is_local_definition_candidate |= is_local_definition_candidate;
            return *info;
        }

        let info = StaticSymbolInfo {
            decl_name,
            link_name,
            linkage,
            visibility,
            thread_local,
            is_local_definition_candidate,
        };
        infos.insert(def_id, info);
        info
    }

    pub(crate) fn ensure_static_info(&self, def_id: DefId) -> StaticSymbolInfo<'mx> {
        if let Some(info) = self.static_symbol_infos.borrow().get(&def_id).copied() {
            return info;
        }

        let info = self.default_static_symbol_info(def_id);
        self.static_symbol_infos.borrow_mut().insert(def_id, info);
        info
    }

    pub(crate) fn static_symbol(&self, def_id: DefId) -> &'mx str {
        self.ensure_static_info(def_id).decl_name
    }

    pub(crate) fn ensure_static_declared(&self, def_id: DefId) {
        let info = self.ensure_static_info(def_id);
        let already_declared = self.declared_statics.borrow().contains(&def_id);
        let already_defined = self.defined_statics.borrow().contains(&def_id);
        match static_extern_decl_action(
            info.is_local_definition_candidate,
            already_declared,
            already_defined,
        ) {
            StaticExternDeclAction::Skip => {}
            StaticExternDeclAction::ConflictLocalAlreadyDeclared => {
                self.tcx.sess.dcx().fatal(format!(
                    "{STATIC_SYMBOL_CONFLICT_MSG}: static `{}` ({def_id:?})",
                    info.decl_name
                ));
            }
            StaticExternDeclAction::EmitExtern => {
                self.declared_statics.borrow_mut().insert(def_id);
                let decl_ty = CTy::Ref(rustc_data_structures::intern::Interned::new_unchecked(
                    self.mcx.arena().alloc(CTyKind::IncompleteArray(CTy::UInt(CUintTy::U8))),
                ));
                self.mcx.module().push_decl(self.mcx.extern_var_with_attrs(
                    CValue::Func(info.decl_name),
                    decl_ty,
                    info.visibility,
                    info.link_name,
                    info.thread_local,
                ));
            }
        }
    }

    pub(crate) fn mark_static_defined(&self, def_id: DefId) {
        let info = self.ensure_static_info(def_id);
        let already_declared = self.declared_statics.borrow().contains(&def_id);
        let already_defined = self.defined_statics.borrow().contains(&def_id);
        if let Err(msg) = validate_static_definition_transition(
            info.is_local_definition_candidate,
            already_declared,
            already_defined,
        ) {
            self.tcx.sess.dcx().fatal(format!("{msg}: static `{}` ({def_id:?})", info.decl_name));
        }
        self.defined_statics.borrow_mut().insert(def_id);
    }

    pub(crate) fn static_addr_expr(&self, def_id: DefId) -> CValue<'mx> {
        self.ensure_static_declared(def_id);
        let symbol = self.static_symbol(def_id);
        let expr = self.mcx.alloc_str(&format!("((uint8_t *)&{symbol})"));
        CValue::Func(expr)
    }

    /// Returns the C declaration name and optional extern link name for a Rust symbol.
    ///
    /// If the symbol is not a valid C identifier, a sanitized declaration name is generated and
    /// a link name is emitted to preserve ABI symbol identity.
    pub(crate) fn declaration_symbol_names(&self, symbol_name: &str) -> (String, Option<String>) {
        let sanitized = sanitize_symbol_name(symbol_name);
        let link_name = if sanitized == symbol_name {
            None
        } else {
            Some(self.symbol_object_link_name(symbol_name))
        };
        (sanitized, link_name)
    }

    pub(crate) fn symbol_object_link_name(&self, symbol_name: &str) -> String {
        if self.tcx.sess.target.options.is_like_darwin {
            // Mach-O symbols have a leading underscore in the object symbol table.
            format!("_{symbol_name}")
        } else {
            symbol_name.to_string()
        }
    }

    pub(crate) fn void_ptr_ty(&self) -> CTy<'mx> {
        CTy::Ref(rustc_data_structures::intern::Interned::new_unchecked(
            self.mcx.arena().alloc(CTyKind::Pointer(CTy::Void)),
        ))
    }

    pub(crate) fn apply_known_symbol_signature_overrides(
        &self,
        symbol_name: &str,
        signature: &mut CAbiSignature<'mx>,
    ) -> bool {
        match symbol_name {
            "malloc" => {
                signature.ret = self.void_ptr_ty();
                signature.params =
                    vec![CAbiParam { role: CAbiParamRole::Regular, ty: CTy::UInt(CUintTy::Usize) }];
                false
            }
            "realloc" => {
                signature.ret = self.void_ptr_ty();
                signature.params = vec![
                    CAbiParam { role: CAbiParamRole::Regular, ty: self.void_ptr_ty() },
                    CAbiParam { role: CAbiParamRole::Regular, ty: CTy::UInt(CUintTy::Usize) },
                ];
                false
            }
            "free" => {
                signature.ret = CTy::Void;
                signature.params =
                    vec![CAbiParam { role: CAbiParamRole::Regular, ty: self.void_ptr_ty() }];
                false
            }
            "printf" => {
                self.mcx.module().require_system_include("stdio.h");
                signature.ret = CTy::Int(CIntTy::I32);
                signature.params.clear();
                true
            }
            _ => false,
        }
    }

    fn predeclare_repr_c_structs(&self) {
        for def_id in self.tcx.hir_crate_items(()).definitions() {
            let def_kind = self.tcx.def_kind(def_id);
            if !matches!(def_kind, DefKind::Struct | DefKind::Union) {
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
                if adt_def.is_union() {
                    self.define_union_layout(layout, *adt_def, args);
                } else {
                    self.define_simple_struct_layout(layout, *adt_def, args);
                }
            }
        }
    }

    pub(crate) fn reg_to_c_ty(&self, reg: Reg) -> CTy<'mx> {
        match (reg.kind, reg.size.bytes()) {
            (RegKind::Integer, 1) => CTy::UInt(CUintTy::U8),
            (RegKind::Integer, 2) => CTy::UInt(CUintTy::U16),
            (RegKind::Integer, 3..=4) => CTy::UInt(CUintTy::U32),
            (RegKind::Integer, 5..=8) => CTy::UInt(CUintTy::U64),
            (RegKind::Integer, 9..=16) => CTy::UInt(CUintTy::U128),
            (RegKind::Float, 4) => CTy::Float(CFloatTy::F32),
            (RegKind::Float, 8) => CTy::Float(CFloatTy::F64),
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

    fn thin_ptr_u8_ty(&self) -> CTy<'mx> {
        CTy::Ref(rustc_data_structures::intern::Interned::new_unchecked(
            self.mcx.arena().alloc(CTyKind::Pointer(CTy::UInt(CUintTy::U8))),
        ))
    }

    fn arg_abi_to_c_params(&self, arg: &ArgAbi<'tcx, Ty<'tcx>>, out: &mut Vec<CAbiParam<'mx>>) {
        match arg.mode {
            PassMode::Ignore => {}
            PassMode::Direct(_) => out.push(CAbiParam {
                role: CAbiParamRole::Regular,
                ty: self.immediate_backend_type(arg.layout),
            }),
            PassMode::Pair(_, _) => {
                out.push(CAbiParam {
                    role: CAbiParamRole::Regular,
                    ty: self.scalar_pair_element_backend_type(arg.layout, 0, true),
                });
                out.push(CAbiParam {
                    role: CAbiParamRole::Regular,
                    ty: self.scalar_pair_element_backend_type(arg.layout, 1, true),
                });
            }
            PassMode::Cast { ref cast, pad_i32 } => {
                if pad_i32 {
                    out.push(CAbiParam {
                        role: CAbiParamRole::Regular,
                        ty: CTy::UInt(CUintTy::U32),
                    });
                }
                out.extend(
                    self.cast_target_to_c_abi_pieces(cast)
                        .into_iter()
                        .map(|(_, ty)| CAbiParam { role: CAbiParamRole::Regular, ty }),
                );
            }
            PassMode::Indirect { meta_attrs: None, .. } => out.push(CAbiParam {
                role: CAbiParamRole::Regular,
                ty: self.indirect_ptr_ty_for_layout(arg.layout),
            }),
            PassMode::Indirect { meta_attrs: Some(_), .. } => {
                out.push(CAbiParam {
                    role: CAbiParamRole::Regular,
                    ty: self.indirect_ptr_ty_for_layout(arg.layout),
                });
                out.push(CAbiParam { role: CAbiParamRole::Regular, ty: self.thin_ptr_u8_ty() });
            }
        }
    }

    fn forced_indirect_return_ty(&self, layout: TyAndLayout<'tcx>) -> CTy<'mx> {
        let storage = self.backend_type(layout);
        // Force C compilers to lower this call with an explicit return slot (sret-like path).
        let pad = self.abi_tuple_ty(&[
            CTy::UInt(CUintTy::U64),
            CTy::UInt(CUintTy::U64),
            CTy::UInt(CUintTy::U64),
            CTy::UInt(CUintTy::U64),
        ]);
        self.abi_tuple_ty(&[storage, pad])
    }

    pub(crate) fn fn_abi_to_c_signature_with_flavor(
        &self,
        fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
        flavor: CAbiSignatureFlavor,
    ) -> CAbiSignature<'mx> {
        let mut params = Vec::new();
        if matches!(flavor, CAbiSignatureFlavor::LoweredRust)
            && matches!(fn_abi.ret.mode, PassMode::Indirect { .. })
        {
            params.push(CAbiParam {
                role: CAbiParamRole::StructReturn,
                ty: self.indirect_ptr_ty_for_layout(fn_abi.ret.layout),
            });
        }
        for arg in fn_abi.args.iter() {
            self.arg_abi_to_c_params(arg, &mut params);
        }

        let ret = match (flavor, &fn_abi.ret.mode) {
            (CAbiSignatureFlavor::LoweredRust, PassMode::Ignore | PassMode::Indirect { .. }) => {
                CTy::Void
            }
            (CAbiSignatureFlavor::NativeC, PassMode::Ignore) => CTy::Void,
            (CAbiSignatureFlavor::NativeC, PassMode::Indirect { .. }) => {
                self.forced_indirect_return_ty(fn_abi.ret.layout)
            }
            (_, PassMode::Direct(_)) => self.immediate_backend_type(fn_abi.ret.layout),
            (_, PassMode::Pair(_, _)) => self.abi_tuple_ty(&[
                self.scalar_pair_element_backend_type(fn_abi.ret.layout, 0, true),
                self.scalar_pair_element_backend_type(fn_abi.ret.layout, 1, true),
            ]),
            (_, PassMode::Cast { cast, pad_i32: _ }) => self.cast_backend_type(cast),
        };

        CAbiSignature { ret, params }
    }

    pub(crate) fn fn_abi_to_c_signature(
        &self,
        fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
    ) -> CAbiSignature<'mx> {
        self.fn_abi_to_c_signature_with_flavor(fn_abi, CAbiSignatureFlavor::LoweredRust)
    }

    pub(crate) fn fn_abi_to_native_c_signature(
        &self,
        fn_abi: &FnAbi<'tcx, Ty<'tcx>>,
    ) -> CAbiSignature<'mx> {
        self.fn_abi_to_c_signature_with_flavor(fn_abi, CAbiSignatureFlavor::NativeC)
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
            CTy::Float(float) => AbiTupleFieldKey::Float(float),
            CTy::Ref(kind) => match kind.0 {
                CTyKind::Pointer(elem) => {
                    AbiTupleFieldKey::Pointer(Box::new(self.abi_tuple_field_key(*elem)))
                }
                CTyKind::Array(elem, len) => {
                    AbiTupleFieldKey::Array(Box::new(self.abi_tuple_field_key(*elem)), *len)
                }
                CTyKind::IncompleteArray(elem) => {
                    AbiTupleFieldKey::IncompleteArray(Box::new(self.abi_tuple_field_key(*elem)))
                }
                CTyKind::Function { ret, params } => AbiTupleFieldKey::Function {
                    ret: Box::new(self.abi_tuple_field_key(*ret)),
                    params: params
                        .iter()
                        .copied()
                        .map(|param| self.abi_tuple_field_key(param))
                        .collect(),
                },
                CTyKind::Struct(name) => AbiTupleFieldKey::Struct(name),
                CTyKind::Union(name) => AbiTupleFieldKey::Union(name),
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
                    rustc_codegen_c_ast::ty::CIntTy::I128 => 16,
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
                    CUintTy::U128 => 16,
                    CUintTy::Usize => self.tcx.data_layout.pointer_size().bytes() as usize,
                };
                Some((bytes, bytes))
            }
            CTy::Float(float) => {
                let bytes = match float {
                    CFloatTy::F32 => 4,
                    CFloatTy::F64 => 8,
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
                CTyKind::IncompleteArray(_) => {
                    let ty_name = format!("{ty:?}");
                    let msg = validate_incomplete_array_usage(false).unwrap_err();
                    self.tcx.sess.dcx().fatal(format!(
                        "{msg}: encountered in size/alignment query for `{ty_name}`"
                    ));
                }
                CTyKind::Function { .. } => None,
                CTyKind::Struct(_) | CTyKind::Union(_) => {
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
            TyKind::Adt(..) | TyKind::Array(..) | TyKind::Tuple(..) | TyKind::Closure(..) => {
                self.backend_type(layout)
            }
            _ => CTy::UInt(CUintTy::U8),
        };
        CTy::Ref(rustc_data_structures::intern::Interned::new_unchecked(
            self.mcx.arena().alloc(CTyKind::Pointer(pointee)),
        ))
    }
}

pub(crate) fn static_extern_decl_action(
    is_local_definition_candidate: bool,
    already_declared: bool,
    already_defined: bool,
) -> StaticExternDeclAction {
    if already_defined {
        return StaticExternDeclAction::Skip;
    }
    if is_local_definition_candidate && already_declared {
        return StaticExternDeclAction::ConflictLocalAlreadyDeclared;
    }
    if is_local_definition_candidate || already_declared {
        return StaticExternDeclAction::Skip;
    }
    StaticExternDeclAction::EmitExtern
}

pub(crate) fn validate_static_definition_transition(
    is_local_definition_candidate: bool,
    already_declared: bool,
    already_defined: bool,
) -> Result<(), &'static str> {
    if already_defined {
        return Err(STATIC_SYMBOL_DEFINED_TWICE_MSG);
    }
    if already_declared {
        return Err(STATIC_SYMBOL_EXTERN_DEFINED_MSG);
    }
    if !is_local_definition_candidate {
        return Err(STATIC_SYMBOL_EXTERNAL_DEFINITION_MSG);
    }
    Ok(())
}

pub(crate) fn validate_incomplete_array_usage(
    is_extern_declaration: bool,
) -> Result<(), &'static str> {
    if is_extern_declaration {
        Ok(())
    } else {
        Err(INCOMPLETE_ARRAY_DECL_ONLY_MSG)
    }
}

#[cfg(test)]
mod tests {
    use super::{
        static_extern_decl_action, validate_incomplete_array_usage,
        validate_static_definition_transition, StaticExternDeclAction,
    };

    #[test]
    fn extern_static_first_reference_emits_declaration_then_dedups() {
        assert_eq!(
            static_extern_decl_action(false, false, false),
            StaticExternDeclAction::EmitExtern
        );
        assert_eq!(static_extern_decl_action(false, true, false), StaticExternDeclAction::Skip);
    }

    #[test]
    fn local_definition_candidate_does_not_emit_extern() {
        assert_eq!(static_extern_decl_action(true, false, false), StaticExternDeclAction::Skip);
    }

    #[test]
    fn local_definition_conflicts_with_prior_extern() {
        assert_eq!(
            static_extern_decl_action(true, true, false),
            StaticExternDeclAction::ConflictLocalAlreadyDeclared
        );
    }

    #[test]
    fn static_definition_transition_validation() {
        assert!(validate_static_definition_transition(true, false, false).is_ok());
        assert!(validate_static_definition_transition(true, true, false).is_err());
        assert!(validate_static_definition_transition(false, false, false).is_err());
        assert!(validate_static_definition_transition(true, false, true).is_err());
    }

    #[test]
    fn incomplete_array_usage_validation() {
        assert!(validate_incomplete_array_usage(true).is_ok());
        assert!(validate_incomplete_array_usage(false).is_err());
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

pub(crate) fn sanitize_symbol_name(symbol_name: &str) -> String {
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
