#![allow(unused_variables)] // TODO

use rustc_middle::ty;
use rustc_middle::ty::layout::HasTypingEnv;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};

use rustc_abi::{HasDataLayout, TargetDataLayout};
use rustc_codegen_c_ast::expr::{CExpr, CValue};
use rustc_codegen_c_ast::func::CFunc;
use rustc_codegen_c_ast::ty::CTy;
use rustc_codegen_c_ast::ModuleCtx;
use rustc_codegen_ssa::traits::BackendTypes;
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
    /// Per-function value type cache shared across basic blocks.
    pub value_tys: RefCell<FxHashMap<(usize, CValue<'mx>), CTy<'mx>>>,
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
}

impl<'tcx, 'mx> CodegenCx<'tcx, 'mx> {
    pub fn new(tcx: TyCtxt<'tcx>, mcx: ModuleCtx<'mx>) -> Self {
        mcx.module().push_include("stdint.h");
        mcx.module().push_include("stddef.h");
        mcx.module().push_include("stdlib.h");
        let cx = Self {
            tcx,
            mcx,
            function_instances: RefCell::new(FxHashMap::default()),
            value_tys: RefCell::new(FxHashMap::default()),
            ptr_pointees: RefCell::new(FxHashMap::default()),
            ptr_lvalues: RefCell::new(FxHashMap::default()),
            pending_allocas: RefCell::new(FxHashMap::default()),
            struct_types: RefCell::new(FxHashMap::default()),
            adt_types: RefCell::new(FxHashMap::default()),
            adt_layouts: RefCell::new(FxHashMap::default()),
            struct_layouts: RefCell::new(FxHashMap::default()),
        };
        cx.predeclare_repr_c_structs();
        cx
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
