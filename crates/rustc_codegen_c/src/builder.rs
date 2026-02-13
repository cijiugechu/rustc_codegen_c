#![allow(unused_variables)] // TODO

use std::ops::Deref;

use rustc_abi::{BackendRepr, HasDataLayout, TargetDataLayout};
use rustc_codegen_c_ast::expr::CValue;
use rustc_codegen_c_ast::ty::{CIntTy, CTy, CTyKind, CUintTy};
use rustc_codegen_ssa::traits::{
    BackendTypes, BuilderMethods, ConstCodegenMethods, LayoutTypeCodegenMethods,
};
use rustc_data_structures::intern::Interned;
use rustc_hash::FxHashMap;
use rustc_hir::LangItem;
use rustc_middle::ty;
use rustc_middle::ty::layout::{
    FnAbiError, FnAbiOfHelpers, FnAbiRequest, HasTyCtxt, HasTypingEnv, LayoutError,
    LayoutOfHelpers, TyAndLayout,
};
use rustc_middle::ty::{Ty, TyCtxt};
use rustc_target::callconv::{FnAbi, PassMode};
use rustc_target::spec::{HasTargetSpec, Target};

use crate::context::{CBasicBlock, CodegenCx, PendingAlloca};

mod abi;
mod asm;
mod coverage_info;
mod debug_info;
mod intrinsic_call;
mod r#static;

/// Codegen builder.
///
/// It is created for each function and provides the local context for code generation.
pub struct Builder<'a, 'tcx, 'mx> {
    /// The associated codegen context.
    pub cx: &'a CodegenCx<'tcx, 'mx>,
    bb: CBasicBlock<'mx>,
    value_tys: FxHashMap<CValue<'mx>, CTy<'mx>>,
}

impl<'a, 'tcx, 'mx> Deref for Builder<'a, 'tcx, 'mx> {
    type Target = CodegenCx<'tcx, 'mx>;

    fn deref<'b>(&'b self) -> &'a Self::Target {
        self.cx
    }
}

impl<'tcx, 'mx> HasDataLayout for Builder<'_, 'tcx, 'mx> {
    fn data_layout(&self) -> &TargetDataLayout {
        &self.cx.tcx.data_layout
    }
}

impl<'tcx, 'mx> HasTyCtxt<'tcx> for Builder<'_, 'tcx, 'mx> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.cx.tcx()
    }
}

impl<'tcx, 'mx> HasTypingEnv<'tcx> for Builder<'_, 'tcx, 'mx> {
    fn typing_env(&self) -> ty::TypingEnv<'tcx> {
        self.cx.typing_env()
    }
}

impl<'tcx, 'mx> BackendTypes for Builder<'_, 'tcx, 'mx> {
    type Value = <CodegenCx<'tcx, 'mx> as BackendTypes>::Value;
    type Metadata = <CodegenCx<'tcx, 'mx> as BackendTypes>::Metadata;
    type Function = <CodegenCx<'tcx, 'mx> as BackendTypes>::Function;
    type BasicBlock = <CodegenCx<'tcx, 'mx> as BackendTypes>::BasicBlock;
    type Type = <CodegenCx<'tcx, 'mx> as BackendTypes>::Type;
    type Funclet = <CodegenCx<'tcx, 'mx> as BackendTypes>::Funclet;

    type DIScope = <CodegenCx<'tcx, 'mx> as BackendTypes>::DIScope;
    type DILocation = <CodegenCx<'tcx, 'mx> as BackendTypes>::DILocation;
    type DIVariable = <CodegenCx<'tcx, 'mx> as BackendTypes>::DIVariable;
}

impl<'tcx, 'mx> HasTargetSpec for Builder<'_, 'tcx, 'mx> {
    fn target_spec(&self) -> &Target {
        &self.cx.tcx.sess.target
    }
}

impl<'tcx, 'mx> LayoutOfHelpers<'tcx> for Builder<'_, 'tcx, 'mx> {
    type LayoutOfResult = TyAndLayout<'tcx>;

    fn handle_layout_err(&self, err: LayoutError<'tcx>, span: rustc_span::Span, ty: Ty<'tcx>) -> ! {
        todo!()
    }
}

impl<'tcx, 'mx> FnAbiOfHelpers<'tcx> for Builder<'_, 'tcx, 'mx> {
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

impl<'a, 'tcx, 'mx> Builder<'a, 'tcx, 'mx> {
    fn fkey(&self) -> usize {
        self.bb.func.0 as *const _ as usize
    }

    fn record_value_ty(&mut self, value: CValue<'mx>, ty: CTy<'mx>) {
        if !matches!(value, CValue::Scalar(_) | CValue::ScalarTyped(_, _)) {
            self.value_tys.insert(value, ty);
            self.cx.value_tys.borrow_mut().insert((self.fkey(), value), ty);
        }
    }

    fn value_ty(&self, value: CValue<'mx>) -> Option<CTy<'mx>> {
        self.value_tys
            .get(&value)
            .copied()
            .or_else(|| self.cx.value_tys.borrow().get(&(self.fkey(), value)).copied())
    }

    fn packed_pair_values(&self, value: CValue<'mx>) -> Option<(CValue<'mx>, CValue<'mx>)> {
        self.cx.packed_scalar_pairs.borrow().get(&(self.fkey(), value)).copied()
    }

    fn set_packed_pair_values(&mut self, pair: CValue<'mx>, a: CValue<'mx>, b: CValue<'mx>) {
        self.cx.packed_scalar_pairs.borrow_mut().insert((self.fkey(), pair), (a, b));
    }

    fn integer_shape(&self, ty: CTy<'mx>) -> Option<(bool, u64, bool)> {
        match ty {
            CTy::Int(int) => {
                let bits = match int {
                    CIntTy::Isize => self.tcx.data_layout.pointer_size().bits(),
                    CIntTy::I8 => 8,
                    CIntTy::I16 => 16,
                    CIntTy::I32 => 32,
                    CIntTy::I64 => 64,
                };
                Some((true, bits, matches!(int, CIntTy::Isize)))
            }
            CTy::UInt(uint) => {
                let bits = match uint {
                    CUintTy::Usize => self.tcx.data_layout.pointer_size().bits(),
                    CUintTy::U8 => 8,
                    CUintTy::U16 => 16,
                    CUintTy::U32 => 32,
                    CUintTy::U64 => 64,
                };
                Some((false, bits, matches!(uint, CUintTy::Usize)))
            }
            _ => None,
        }
    }

    fn compatible_integer_result_ty(&self, lhs: CTy<'mx>, rhs: CTy<'mx>) -> Option<CTy<'mx>> {
        if lhs == rhs {
            return Some(lhs);
        }

        let (lhs_signed, lhs_bits, lhs_is_size) = self.integer_shape(lhs)?;
        let (rhs_signed, rhs_bits, rhs_is_size) = self.integer_shape(rhs)?;
        if lhs_signed != rhs_signed || lhs_bits != rhs_bits {
            return None;
        }

        if lhs_is_size && !rhs_is_size {
            Some(lhs)
        } else if rhs_is_size && !lhs_is_size {
            Some(rhs)
        } else {
            Some(lhs)
        }
    }

    fn compatible_bitwise_result_ty(&self, lhs: CTy<'mx>, rhs: CTy<'mx>) -> Option<CTy<'mx>> {
        if lhs == rhs {
            return Some(lhs);
        }

        match (lhs, rhs) {
            (CTy::Bool, CTy::Bool) => Some(CTy::Bool),
            (CTy::Int(_) | CTy::UInt(_), CTy::Int(_) | CTy::UInt(_)) => {
                let (_, lhs_bits, lhs_is_size) = self.integer_shape(lhs)?;
                let (_, rhs_bits, rhs_is_size) = self.integer_shape(rhs)?;
                if lhs_bits != rhs_bits {
                    return None;
                }

                if lhs_is_size && !rhs_is_size {
                    return Some(lhs);
                }
                if rhs_is_size && !lhs_is_size {
                    return Some(rhs);
                }

                match (lhs, rhs) {
                    (CTy::UInt(_), CTy::Int(_)) => Some(lhs),
                    (CTy::Int(_), CTy::UInt(_)) => Some(rhs),
                    _ => Some(lhs),
                }
            }
            _ => None,
        }
    }

    fn infer_integer_binop_ty(&self, lhs: CValue<'mx>, rhs: CValue<'mx>, op: &str) -> CTy<'mx> {
        let ty = match (self.value_ty(lhs), self.value_ty(rhs)) {
            (Some(lhs), Some(rhs)) if lhs == rhs => lhs,
            (Some(lhs), Some(rhs)) => self
                .compatible_integer_result_ty(lhs, rhs)
                .unwrap_or_else(|| panic!("type mismatch for {op}: {lhs:?} vs {rhs:?}")),
            (Some(lhs), None) => lhs,
            (None, Some(rhs)) => rhs,
            (None, None) => panic!("cannot infer operand type for {op}"),
        };

        match ty {
            CTy::Bool | CTy::Int(_) | CTy::UInt(_) => ty,
            _ => panic!("unsupported type for {op}: {ty:?}"),
        }
    }

    fn infer_unchecked_integer_binop_ty(
        &self,
        lhs: CValue<'mx>,
        rhs: CValue<'mx>,
        op: &str,
    ) -> CTy<'mx> {
        let ty = match (self.value_ty(lhs), self.value_ty(rhs)) {
            (Some(lhs), Some(rhs)) if lhs == rhs => lhs,
            (Some(lhs), Some(rhs)) => self
                .compatible_integer_result_ty(lhs, rhs)
                .or_else(|| self.compatible_bitwise_result_ty(lhs, rhs))
                .unwrap_or_else(|| panic!("type mismatch for {op}: {lhs:?} vs {rhs:?}")),
            (Some(lhs), None) => lhs,
            (None, Some(rhs)) => rhs,
            (None, None) => panic!("cannot infer operand type for {op}"),
        };

        match ty {
            CTy::Int(_) | CTy::UInt(_) => ty,
            _ => panic!("unsupported type for {op}: {ty:?}"),
        }
    }

    fn codegen_unchecked_int_binop(
        &mut self,
        lhs: CValue<'mx>,
        rhs: CValue<'mx>,
        op_name: &str,
        c_op: &'static str,
    ) -> CValue<'mx> {
        let ty = self.infer_unchecked_integer_binop_ty(lhs, rhs, op_name);
        let ret = self.bb.func.0.next_local_var();
        let lhs = self.coerce_int_operand_expr(lhs, ty);
        let rhs = self.coerce_int_operand_expr(rhs, ty);
        let expr = self.mcx.binary(lhs, rhs, c_op);
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn coerce_int_operand_expr(
        &self,
        value: CValue<'mx>,
        target_ty: CTy<'mx>,
    ) -> rustc_codegen_c_ast::expr::CExpr<'mx> {
        match self.value_ty(value) {
            Some(ty) if ty != target_ty => self.mcx.cast(target_ty, self.mcx.value(value)),
            _ => self.mcx.value(value),
        }
    }

    fn infer_bitwise_binop_ty(&self, lhs: CValue<'mx>, rhs: CValue<'mx>, op: &str) -> CTy<'mx> {
        let ty = match (self.value_ty(lhs), self.value_ty(rhs)) {
            (Some(lhs), Some(rhs)) if lhs == rhs => lhs,
            (Some(lhs), Some(rhs)) => self
                .compatible_bitwise_result_ty(lhs, rhs)
                .unwrap_or_else(|| panic!("type mismatch for {op}: {lhs:?} vs {rhs:?}")),
            (Some(lhs), None) => lhs,
            (None, Some(rhs)) => rhs,
            (None, None) => panic!("cannot infer operand type for {op}"),
        };

        match ty {
            CTy::Bool | CTy::Int(_) | CTy::UInt(_) => ty,
            _ => panic!("unsupported type for {op}: {ty:?}"),
        }
    }

    fn pointer_to(&self, pointee: CTy<'mx>) -> CTy<'mx> {
        CTy::Ref(Interned::new_unchecked(self.mcx.arena().alloc(CTyKind::Pointer(pointee))))
    }

    fn pointer_pointee_ty(&self, ptr: CValue<'mx>) -> Option<CTy<'mx>> {
        let fkey = self.bb.func.0 as *const _ as usize;
        if let Some(pointee) = self.cx.ptr_pointees.borrow().get(&(fkey, ptr)).copied() {
            return Some(pointee);
        }

        self.value_ty(ptr).and_then(|ty| match ty {
            CTy::Ref(kind) => match kind.0 {
                CTyKind::Pointer(elem) | CTyKind::Array(elem, _) => Some(*elem),
                CTyKind::Function { .. } => None,
                CTyKind::Struct(_) => None,
            },
            _ => None,
        })
    }

    fn update_ptr_pointee_ty(&mut self, ptr: CValue<'mx>, pointee: CTy<'mx>) {
        let fkey = self.bb.func.0 as *const _ as usize;
        self.cx.ptr_pointees.borrow_mut().insert((fkey, ptr), pointee);
    }

    fn pointer_lvalue(&self, ptr: CValue<'mx>) -> Option<rustc_codegen_c_ast::expr::CExpr<'mx>> {
        self.cx.ptr_lvalues.borrow().get(&(self.fkey(), ptr)).copied()
    }

    fn update_ptr_lvalue(
        &mut self,
        ptr: CValue<'mx>,
        lvalue: rustc_codegen_c_ast::expr::CExpr<'mx>,
    ) {
        self.cx.ptr_lvalues.borrow_mut().insert((self.fkey(), ptr), lvalue);
    }

    fn struct_layout_info(&self, ty: CTy<'mx>) -> Option<crate::context::AdtLayoutInfo<'mx>> {
        self.cx.struct_layouts.borrow().get(&ty).cloned()
    }

    fn ensure_alloca_decl(&mut self, ptr: CValue<'mx>, suggested_ty: Option<CTy<'mx>>) {
        let fkey = self.bb.func.0 as *const _ as usize;
        let Some(slot) = self.cx.pending_allocas.borrow().get(&(fkey, ptr)).copied() else {
            return;
        };
        if slot.declared {
            return;
        }

        let decl_ty = match suggested_ty {
            Some(CTy::Ref(kind)) if matches!(kind.0, CTyKind::Array(_, _)) => CTy::Ref(kind),
            Some(CTy::Ref(kind)) if matches!(kind.0, CTyKind::Struct(_)) => {
                let struct_ty = CTy::Ref(kind);
                let info = self
                    .struct_layout_info(struct_ty)
                    .unwrap_or_else(|| panic!("missing struct layout metadata for {struct_ty:?}"));
                if slot.bytes % info.size == 0 {
                    CTy::Ref(Interned::new_unchecked(
                        self.mcx
                            .arena()
                            .alloc(CTyKind::Array(struct_ty, (slot.bytes / info.size).max(1))),
                    ))
                } else {
                    CTy::Ref(Interned::new_unchecked(
                        self.mcx
                            .arena()
                            .alloc(CTyKind::Array(CTy::UInt(CUintTy::U8), slot.bytes.max(1))),
                    ))
                }
            }
            Some(other) => self.array_decl_ty_from_bytes(slot.bytes, other).unwrap_or_else(|| {
                // When the suggested element type doesn't tile the stack slot, fall back to a
                // byte array and rely on typed loads/stores through explicit casts.
                CTy::Ref(Interned::new_unchecked(
                    self.mcx
                        .arena()
                        .alloc(CTyKind::Array(CTy::UInt(CUintTy::U8), slot.bytes.max(1))),
                ))
            }),
            None => panic!("cannot infer alloca declaration type for {ptr:?}"),
        };

        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ptr, decl_ty, None)));
        self.record_value_ty(ptr, decl_ty);
        if let CTy::Ref(kind) = decl_ty {
            match kind.0 {
                CTyKind::Array(elem, _) | CTyKind::Pointer(elem) => {
                    self.update_ptr_pointee_ty(ptr, *elem)
                }
                CTyKind::Function { .. } => {}
                CTyKind::Struct(_) => {}
            }
        }

        self.cx
            .pending_allocas
            .borrow_mut()
            .insert((fkey, ptr), PendingAlloca { declared: true, ..slot });
    }

    fn pending_alloca_bytes(&self, ptr: CValue<'mx>) -> Option<usize> {
        let fkey = self.bb.func.0 as *const _ as usize;
        self.cx.pending_allocas.borrow().get(&(fkey, ptr)).map(|slot| slot.bytes)
    }

    fn c_ty_size_bytes(&self, ty: CTy<'mx>) -> Option<usize> {
        match ty {
            CTy::Bool => Some(1),
            CTy::Int(int) => Some(match int {
                rustc_codegen_c_ast::ty::CIntTy::I8 => 1,
                rustc_codegen_c_ast::ty::CIntTy::I16 => 2,
                rustc_codegen_c_ast::ty::CIntTy::I32 => 4,
                rustc_codegen_c_ast::ty::CIntTy::I64 => 8,
                rustc_codegen_c_ast::ty::CIntTy::Isize => {
                    self.tcx.data_layout.pointer_size().bytes() as usize
                }
            }),
            CTy::UInt(int) => Some(match int {
                CUintTy::U8 => 1,
                CUintTy::U16 => 2,
                CUintTy::U32 => 4,
                CUintTy::U64 => 8,
                CUintTy::Usize => self.tcx.data_layout.pointer_size().bytes() as usize,
            }),
            CTy::Ref(kind) => match kind.0 {
                CTyKind::Pointer(_) => Some(self.tcx.data_layout.pointer_size().bytes() as usize),
                CTyKind::Array(elem, count) => self.c_ty_size_bytes(*elem).map(|size| size * count),
                CTyKind::Function { .. } => None,
                CTyKind::Struct(_) => self.struct_layout_info(ty).map(|info| info.size),
            },
            _ => None,
        }
    }

    fn infer_store_ty_from_pending_alloca(
        &self,
        ptr: CValue<'mx>,
        align: rustc_abi::Align,
    ) -> Option<CTy<'mx>> {
        let fkey = self.fkey();
        let slot = self.cx.pending_allocas.borrow().get(&(fkey, ptr)).copied()?;
        let size = slot.bytes;
        if size != 1 && size != 2 && size != 4 && size != 8 {
            return None;
        }
        if align.bytes() as usize != size {
            return None;
        }

        Some(match size {
            1 => CTy::UInt(CUintTy::U8),
            2 => CTy::UInt(CUintTy::U16),
            4 => CTy::UInt(CUintTy::U32),
            8 => CTy::UInt(CUintTy::U64),
            _ => return None,
        })
    }

    fn fallback_store_ty_from_align(&self, align: rustc_abi::Align) -> CTy<'mx> {
        match align.bytes() {
            1 => CTy::UInt(CUintTy::U8),
            2 => CTy::UInt(CUintTy::U16),
            4 => CTy::UInt(CUintTy::U32),
            8 => CTy::UInt(CUintTy::U64),
            _ => CTy::UInt(CUintTy::U8),
        }
    }

    fn array_decl_ty_from_bytes(&self, bytes: usize, elem_ty: CTy<'mx>) -> Option<CTy<'mx>> {
        let elem_bytes = self.c_ty_size_bytes(elem_ty)?;
        if elem_bytes == 0 || bytes % elem_bytes != 0 {
            return None;
        }
        Some(CTy::Ref(Interned::new_unchecked(
            self.mcx.arena().alloc(CTyKind::Array(elem_ty, bytes / elem_bytes)),
        )))
    }

    fn ensure_alloca_byte_array_decl(&mut self, ptr: CValue<'mx>) {
        let Some(bytes) = self.pending_alloca_bytes(ptr) else {
            return;
        };
        let decl_ty = CTy::Ref(Interned::new_unchecked(
            self.mcx.arena().alloc(CTyKind::Array(CTy::UInt(CUintTy::U8), bytes.max(1))),
        ));
        self.ensure_alloca_decl(ptr, Some(decl_ty));
    }

    fn abi_tuple_ty_for_pair_layout(&self, layout: TyAndLayout<'tcx>) -> CTy<'mx> {
        let a = self.cx.scalar_pair_element_backend_type(layout, 0, true);
        let b = self.cx.scalar_pair_element_backend_type(layout, 1, true);
        self.cx.abi_tuple_ty(&[a, b])
    }

    fn build_abi_tuple_from_values(
        &mut self,
        tuple_ty: CTy<'mx>,
        values: &[CValue<'mx>],
    ) -> CValue<'mx> {
        let tuple = self.bb.func.0.next_local_var();
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(tuple, tuple_ty, None)));
        self.record_value_ty(tuple, tuple_ty);

        for (i, value) in values.iter().copied().enumerate() {
            let field = self.cx.abi_tuple_field_name(tuple_ty, i);
            let field_ty = self.cx.abi_tuple_field_ty(tuple_ty, i);
            let lhs = self.mcx.member(self.mcx.value(tuple), field);
            let rhs = match self.value_ty(value) {
                Some(ty) if ty != field_ty => self.mcx.cast(field_ty, self.mcx.value(value)),
                _ => self.mcx.value(value),
            };
            self.bb.func.0.push_stmt(self.mcx.expr_stmt(self.mcx.binary(lhs, rhs, "=")));
        }

        tuple
    }

    fn load_abi_tuple_field(
        &mut self,
        tuple: CValue<'mx>,
        tuple_ty: CTy<'mx>,
        idx: usize,
    ) -> CValue<'mx> {
        let field = self.cx.abi_tuple_field_name(tuple_ty, idx);
        let field_ty = self.cx.abi_tuple_field_ty(tuple_ty, idx);
        let value = self.bb.func.0.next_local_var();
        let field_expr = self.mcx.member(self.mcx.value(tuple), field);
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(
            value,
            field_ty,
            Some(field_expr),
        )));
        self.record_value_ty(value, field_ty);
        value
    }
}

impl<'a, 'tcx, 'mx> BuilderMethods<'a, 'tcx> for Builder<'a, 'tcx, 'mx> {
    type CodegenCx = CodegenCx<'tcx, 'mx>;
    fn build(cx: &'a Self::CodegenCx, llbb: Self::BasicBlock) -> Self {
        let fkey = llbb.func.0 as *const _ as usize;
        cx.current_fkey.set(Some(fkey));
        let mut value_tys = FxHashMap::default();
        for (ty, value) in llbb.func.0.params.iter() {
            value_tys.insert(*value, *ty);
            cx.value_tys.borrow_mut().insert((fkey, *value), *ty);
        }
        llbb.func.0.emit_label_once(cx.mcx, llbb.label);
        Self { cx, bb: llbb, value_tys }
    }

    fn cx(&self) -> &Self::CodegenCx {
        self.cx
    }

    fn llbb(&self) -> Self::BasicBlock {
        self.bb
    }

    fn set_span(&mut self, _span: rustc_span::Span) {}

    fn append_block(cx: &'a Self::CodegenCx, llfn: Self::Function, name: &str) -> Self::BasicBlock {
        let id = llfn.0.next_block_id();
        let label = cx.mcx.alloc_str(&format!("bb{}_{}", id, name));
        CBasicBlock { func: llfn, label }
    }

    fn append_sibling_block(&mut self, name: &str) -> Self::BasicBlock {
        let id = self.bb.func.0.next_block_id();
        let label = self.mcx.alloc_str(&format!("bb{}_{}", id, name));
        CBasicBlock { func: self.bb.func, label }
    }

    fn switch_to_block(&mut self, llbb: Self::BasicBlock) {
        self.bb = llbb;
        self.bb.func.0.emit_label_once(self.mcx, llbb.label);
    }

    fn ret_void(&mut self) {
        self.bb.func.0.push_stmt(self.cx.mcx.ret(None));
    }

    fn ret(&mut self, v: Self::Value) {
        let ret_ty = self.bb.func.0.ty;
        if self.cx.abi_tuple_field_tys.borrow().contains_key(&ret_ty) {
            if let Some((a, b)) = self.packed_pair_values(v) {
                let tuple = self.build_abi_tuple_from_values(ret_ty, &[a, b]);
                self.bb.func.0.push_stmt(self.cx.mcx.ret(Some(self.cx.mcx.value(tuple))));
                return;
            }
        }

        self.bb.func.0.push_stmt(self.cx.mcx.ret(Some(self.cx.mcx.value(v))))
    }

    fn br(&mut self, dest: Self::BasicBlock) {
        self.bb.func.0.push_stmt(self.mcx.goto_stmt(dest.label));
    }

    fn cond_br(
        &mut self,
        cond: Self::Value,
        then_llbb: Self::BasicBlock,
        else_llbb: Self::BasicBlock,
    ) {
        let cond = self.mcx.value(cond);
        self.bb.func.0.push_stmt(self.mcx.if_goto_stmt(cond, then_llbb.label, else_llbb.label));
    }

    fn switch(
        &mut self,
        v: Self::Value,
        else_llbb: Self::BasicBlock,
        cases: impl ExactSizeIterator<Item = (u128, Self::BasicBlock)>,
    ) {
        let cases = cases.map(|(val, bb)| (val, bb.label)).collect();
        self.bb.func.0.push_stmt(self.mcx.switch_stmt(self.mcx.value(v), cases, else_llbb.label));
    }

    fn invoke(
        &mut self,
        llty: Self::Type,
        fn_attrs: Option<&rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrs>,
        fn_abi: Option<&rustc_target::callconv::FnAbi<'tcx, rustc_middle::ty::Ty<'tcx>>>,
        llfn: Self::Value,
        args: &[Self::Value],
        then: Self::BasicBlock,
        catch: Self::BasicBlock,
        funclet: Option<&Self::Funclet>,
        instance: Option<rustc_middle::ty::Instance<'tcx>>,
    ) -> Self::Value {
        todo!()
    }

    fn unreachable(&mut self) {
        let abort = self.mcx.call(self.mcx.value(CValue::Func("abort")), vec![]);
        self.bb.func.0.push_stmt(self.mcx.expr_stmt(abort));
    }

    fn add(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        let ty = self.infer_unchecked_integer_binop_ty(lhs, rhs, "add");
        let ret = self.bb.func.0.next_local_var();
        let lhs = self.coerce_int_operand_expr(lhs, ty);
        let rhs = self.coerce_int_operand_expr(rhs, ty);
        let expr = self.mcx.binary(lhs, rhs, "+");
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn fadd(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fadd_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fadd_algebraic(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn sub(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        let ty = self.infer_unchecked_integer_binop_ty(lhs, rhs, "sub");
        let ret = self.bb.func.0.next_local_var();
        let lhs = self.coerce_int_operand_expr(lhs, ty);
        let rhs = self.coerce_int_operand_expr(rhs, ty);
        let expr = self.mcx.binary(lhs, rhs, "-");
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn fsub(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fsub_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fsub_algebraic(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn mul(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        let ty = self.infer_unchecked_integer_binop_ty(lhs, rhs, "mul");
        let ret = self.bb.func.0.next_local_var();
        let lhs = self.coerce_int_operand_expr(lhs, ty);
        let rhs = self.coerce_int_operand_expr(rhs, ty);
        let expr = self.mcx.binary(lhs, rhs, "*");
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn fmul(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fmul_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fmul_algebraic(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn udiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        let ty = self.infer_unchecked_integer_binop_ty(lhs, rhs, "udiv");
        if !matches!(ty, CTy::UInt(_)) {
            panic!("unsupported type for udiv: {ty:?}");
        }
        let ret = self.bb.func.0.next_local_var();
        let lhs = self.coerce_int_operand_expr(lhs, ty);
        let rhs = self.coerce_int_operand_expr(rhs, ty);
        let expr = self.mcx.binary(lhs, rhs, "/");
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn exactudiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.udiv(lhs, rhs)
    }

    fn sdiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        let ty = self.infer_integer_binop_ty(lhs, rhs, "sdiv");
        if !matches!(ty, CTy::Int(_)) {
            panic!("unsupported type for sdiv: {ty:?}");
        }
        let ret = self.bb.func.0.next_local_var();
        let lhs = self.coerce_int_operand_expr(lhs, ty);
        let rhs = self.coerce_int_operand_expr(rhs, ty);
        let expr = self.mcx.binary(lhs, rhs, "/");
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn exactsdiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.sdiv(lhs, rhs)
    }

    fn fdiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fdiv_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn fdiv_algebraic(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn urem(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn srem(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn frem(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn frem_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn frem_algebraic(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn shl(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        let ty = self.infer_bitwise_binop_ty(lhs, rhs, "shl");
        if !matches!(ty, CTy::Int(_) | CTy::UInt(_)) {
            panic!("unsupported type for shl: {ty:?}");
        }

        let ret = self.bb.func.0.next_local_var();
        let lhs = self.coerce_int_operand_expr(lhs, ty);
        let rhs = self.coerce_int_operand_expr(rhs, ty);
        let expr = self.mcx.binary(lhs, rhs, "<<");
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn lshr(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        let ty = self.infer_bitwise_binop_ty(lhs, rhs, "lshr");
        if !matches!(ty, CTy::Int(_) | CTy::UInt(_)) {
            panic!("unsupported type for lshr: {ty:?}");
        }

        let ret = self.bb.func.0.next_local_var();
        let rhs = self.coerce_int_operand_expr(rhs, ty);
        let expr = match ty {
            CTy::UInt(_) => {
                let lhs = self.coerce_int_operand_expr(lhs, ty);
                self.mcx.binary(lhs, rhs, ">>")
            }
            CTy::Int(_) => {
                let unsigned_ty = ty.to_unsigned();
                let lhs = self.coerce_int_operand_expr(lhs, unsigned_ty);
                let shifted = self.mcx.binary(lhs, rhs, ">>");
                self.mcx.cast(ty, shifted)
            }
            _ => unreachable!(),
        };

        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn ashr(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        let ty = self.infer_bitwise_binop_ty(lhs, rhs, "ashr");
        if !matches!(ty, CTy::Int(_) | CTy::UInt(_)) {
            panic!("unsupported type for ashr: {ty:?}");
        }

        let ret = self.bb.func.0.next_local_var();
        let rhs = self.coerce_int_operand_expr(rhs, ty);
        let expr = match ty {
            CTy::Int(_) => {
                let lhs = self.coerce_int_operand_expr(lhs, ty);
                self.mcx.binary(lhs, rhs, ">>")
            }
            CTy::UInt(uint_ty) => {
                let signed_ty = match uint_ty {
                    CUintTy::Usize => CTy::Int(CIntTy::Isize),
                    CUintTy::U8 => CTy::Int(CIntTy::I8),
                    CUintTy::U16 => CTy::Int(CIntTy::I16),
                    CUintTy::U32 => CTy::Int(CIntTy::I32),
                    CUintTy::U64 => CTy::Int(CIntTy::I64),
                };
                let lhs = self.coerce_int_operand_expr(lhs, signed_ty);
                let shifted = self.mcx.binary(lhs, rhs, ">>");
                self.mcx.cast(ty, shifted)
            }
            _ => unreachable!(),
        };

        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn unchecked_sadd(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.codegen_unchecked_int_binop(lhs, rhs, "unchecked_sadd", "+")
    }

    fn unchecked_uadd(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.codegen_unchecked_int_binop(lhs, rhs, "unchecked_uadd", "+")
    }

    fn unchecked_ssub(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.codegen_unchecked_int_binop(lhs, rhs, "unchecked_ssub", "-")
    }

    fn unchecked_usub(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.codegen_unchecked_int_binop(lhs, rhs, "unchecked_usub", "-")
    }

    fn unchecked_smul(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.codegen_unchecked_int_binop(lhs, rhs, "unchecked_smul", "*")
    }

    fn unchecked_umul(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.codegen_unchecked_int_binop(lhs, rhs, "unchecked_umul", "*")
    }

    fn and(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        let ty = self.infer_bitwise_binop_ty(lhs, rhs, "and");
        let ret = self.bb.func.0.next_local_var();
        let lhs = self.coerce_int_operand_expr(lhs, ty);
        let rhs = self.coerce_int_operand_expr(rhs, ty);
        let expr = self.mcx.binary(lhs, rhs, "&");
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn or(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        let ty = self.infer_bitwise_binop_ty(lhs, rhs, "or");
        let ret = self.bb.func.0.next_local_var();
        let lhs = self.coerce_int_operand_expr(lhs, ty);
        let rhs = self.coerce_int_operand_expr(rhs, ty);
        let expr = self.mcx.binary(lhs, rhs, "|");
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn xor(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        let ty = self.infer_bitwise_binop_ty(lhs, rhs, "xor");
        let ret = self.bb.func.0.next_local_var();
        let lhs = self.coerce_int_operand_expr(lhs, ty);
        let rhs = self.coerce_int_operand_expr(rhs, ty);
        let expr = self.mcx.binary(lhs, rhs, "^");
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn neg(&mut self, v: Self::Value) -> Self::Value {
        todo!()
    }

    fn fneg(&mut self, v: Self::Value) -> Self::Value {
        todo!()
    }

    fn not(&mut self, v: Self::Value) -> Self::Value {
        let ty = match self.value_ty(v) {
            Some(ty @ (CTy::Bool | CTy::Int(_) | CTy::UInt(_))) => ty,
            Some(other) => panic!("unsupported type for not: {other:?}"),
            None => panic!("cannot infer operand type for not"),
        };
        let ret = self.bb.func.0.next_local_var();
        let operand = self.coerce_int_operand_expr(v, ty);
        let expr = if matches!(ty, CTy::Bool) {
            self.mcx.unary("!", operand)
        } else {
            self.mcx.unary("~", operand)
        };
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn checked_binop(
        &mut self,
        oop: rustc_codegen_ssa::traits::OverflowOp,
        ty: rustc_middle::ty::Ty<'_>,
        lhs: Self::Value,
        rhs: Self::Value,
    ) -> (Self::Value, Self::Value) {
        let ty = match ty.kind() {
            rustc_type_ir::TyKind::Int(int) => self.mcx.get_int_type(*int),
            rustc_type_ir::TyKind::Uint(uint) => self.mcx.get_uint_type(*uint),
            _ => panic!("checked_binop expects integer type, got {ty:?}"),
        };

        let lhs = self.coerce_int_operand_expr(lhs, ty);
        let rhs = self.coerce_int_operand_expr(rhs, ty);

        let result = self.bb.func.0.next_local_var();
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(result, ty, None)));
        self.record_value_ty(result, ty);

        let overflow = self.bb.func.0.next_local_var();
        let overflow_builtin = match oop {
            rustc_codegen_ssa::traits::OverflowOp::Add => "__builtin_add_overflow",
            rustc_codegen_ssa::traits::OverflowOp::Sub => "__builtin_sub_overflow",
            rustc_codegen_ssa::traits::OverflowOp::Mul => "__builtin_mul_overflow",
        };
        let result_ptr =
            self.mcx.cast(self.pointer_to(ty), self.mcx.unary("&", self.mcx.value(result)));
        let overflow_expr =
            self.mcx.call(self.mcx.raw(overflow_builtin), vec![lhs, rhs, result_ptr]);
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(
            overflow,
            CTy::Bool,
            Some(overflow_expr),
        )));
        self.record_value_ty(overflow, CTy::Bool);

        (result, overflow)
    }

    fn from_immediate(&mut self, val: Self::Value) -> Self::Value {
        if matches!(self.value_ty(val), Some(CTy::Bool)) {
            let backend_bool = CTy::UInt(CUintTy::U8);
            let ret = self.bb.func.0.next_local_var();
            let cast = self.mcx.cast(backend_bool, self.mcx.value(val));
            self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(
                ret,
                backend_bool,
                Some(cast),
            )));
            self.record_value_ty(ret, backend_bool);
            return ret;
        }

        val
    }

    fn to_immediate_scalar(&mut self, val: Self::Value, scalar: rustc_abi::Scalar) -> Self::Value {
        if scalar.is_bool() && !matches!(self.value_ty(val), Some(CTy::Bool)) {
            let ret = self.bb.func.0.next_local_var();
            let zero = self.mcx.value(CValue::Scalar(0));
            let expr = self.mcx.binary(self.mcx.value(val), zero, "!=");
            self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, CTy::Bool, Some(expr))));
            self.record_value_ty(ret, CTy::Bool);
            return ret;
        }

        val
    }

    fn alloca(&mut self, size: rustc_abi::Size, align: rustc_abi::Align) -> Self::Value {
        let ret = self.bb.func.0.next_local_var();
        let bytes = size.bytes_usize();
        let fkey = self.bb.func.0 as *const _ as usize;
        self.cx
            .pending_allocas
            .borrow_mut()
            .insert((fkey, ret), PendingAlloca { bytes, declared: false });
        ret
    }

    fn load(&mut self, ty: Self::Type, ptr: Self::Value, align: rustc_abi::Align) -> Self::Value {
        self.ensure_alloca_decl(ptr, Some(ty));
        let expr = if let Some(lvalue) = self.pointer_lvalue(ptr) {
            match self.pointer_pointee_ty(ptr) {
                Some(pointee) if pointee == ty => lvalue,
                _ => self.mcx.cast(ty, lvalue),
            }
        } else {
            let cast_ptr = self.mcx.cast(self.pointer_to(ty), self.mcx.value(ptr));
            self.mcx.unary("*", cast_ptr)
        };

        let ret = self.bb.func.0.next_local_var();
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn volatile_load(&mut self, ty: Self::Type, ptr: Self::Value) -> Self::Value {
        todo!()
    }

    fn atomic_load(
        &mut self,
        ty: Self::Type,
        ptr: Self::Value,
        order: rustc_middle::ty::AtomicOrdering,
        size: rustc_abi::Size,
    ) -> Self::Value {
        todo!()
    }

    fn load_operand(
        &mut self,
        place: rustc_codegen_ssa::mir::place::PlaceRef<'tcx, Self::Value>,
    ) -> rustc_codegen_ssa::mir::operand::OperandRef<'tcx, Self::Value> {
        use crate::rustc_codegen_ssa::traits::LayoutTypeCodegenMethods;
        use rustc_codegen_ssa::mir::operand::{OperandRef, OperandValue};

        let layout = place.layout;
        if layout.is_zst() {
            return OperandRef::zero_sized(layout);
        }

        if self.cx.is_backend_immediate(layout) {
            let llty = self.cx.immediate_backend_type(layout);
            let val = self.load(llty, place.val.llval, place.val.align);
            return OperandRef { val: OperandValue::Immediate(val), layout };
        }

        if self.cx.is_backend_scalar_pair(layout) {
            let (a_scalar, b_scalar) = match layout.backend_repr {
                BackendRepr::ScalarPair(a, b) => (a, b),
                _ => unreachable!("expected scalar pair layout"),
            };
            let b_offset = a_scalar.size(self).align_to(b_scalar.align(self).abi);

            let a_ty = self.cx.scalar_pair_element_backend_type(layout, 0, true);
            let b_ty = self.cx.scalar_pair_element_backend_type(layout, 1, true);
            let a = self.load(a_ty, place.val.llval, place.val.align);
            let b_ptr = self.inbounds_ptradd(place.val.llval, self.const_usize(b_offset.bytes()));
            let b_align = place.val.align.restrict_for_offset(b_offset);
            let b = self.load(b_ty, b_ptr, b_align);

            return OperandRef { val: OperandValue::Pair(a, b), layout };
        }

        OperandRef { val: OperandValue::Ref(place.val), layout }
    }

    fn write_operand_repeatedly(
        &mut self,
        elem: rustc_codegen_ssa::mir::operand::OperandRef<'tcx, Self::Value>,
        count: u64,
        dest: rustc_codegen_ssa::mir::place::PlaceRef<'tcx, Self::Value>,
    ) {
        todo!()
    }

    fn range_metadata(&mut self, load: Self::Value, range: rustc_abi::WrappingRange) {
        todo!()
    }

    fn nonnull_metadata(&mut self, load: Self::Value) {
        todo!()
    }

    fn store(
        &mut self,
        val: Self::Value,
        ptr: Self::Value,
        align: rustc_abi::Align,
    ) -> Self::Value {
        let val_ty = self.value_ty(val);
        let normalized_val_ty = val_ty.and_then(|ty| match ty {
            CTy::Ref(kind) => match kind.0 {
                CTyKind::Array(elem, 1) => Some(*elem),
                _ => Some(ty),
            },
            _ => Some(ty),
        });
        let store_ty = normalized_val_ty
            .or_else(|| self.pointer_pointee_ty(ptr))
            .or_else(|| self.infer_store_ty_from_pending_alloca(ptr, align))
            .unwrap_or_else(|| self.fallback_store_ty_from_align(align));
        self.ensure_alloca_decl(ptr, Some(store_ty));
        let lhs = if let Some(lvalue) = self.pointer_lvalue(ptr) {
            match self.pointer_pointee_ty(ptr) {
                Some(pointee) if pointee == store_ty => lvalue,
                _ => {
                    let cast_ptr = self.mcx.cast(self.pointer_to(store_ty), self.mcx.value(ptr));
                    self.mcx.unary("*", cast_ptr)
                }
            }
        } else {
            let cast_ptr = self.mcx.cast(self.pointer_to(store_ty), self.mcx.value(ptr));
            self.mcx.unary("*", cast_ptr)
        };
        let rhs = match val_ty {
            Some(val_ty) if val_ty == store_ty => self.mcx.value(val),
            Some(CTy::Ref(kind)) if matches!(kind.0, CTyKind::Array(elem, 1) if *elem == store_ty) => {
                self.mcx.index(self.mcx.value(val), self.mcx.value(CValue::Scalar(0)))
            }
            Some(_) => self.mcx.cast(store_ty, self.mcx.value(val)),
            None => self.mcx.value(val),
        };
        let assign = self.mcx.binary(lhs, rhs, "=");
        self.bb.func.0.push_stmt(self.mcx.expr_stmt(assign));
        val
    }

    fn store_with_flags(
        &mut self,
        val: Self::Value,
        ptr: Self::Value,
        align: rustc_abi::Align,
        flags: rustc_codegen_ssa::MemFlags,
    ) -> Self::Value {
        self.store(val, ptr, align)
    }

    fn atomic_store(
        &mut self,
        val: Self::Value,
        ptr: Self::Value,
        order: rustc_middle::ty::AtomicOrdering,
        size: rustc_abi::Size,
    ) {
        todo!()
    }

    fn gep(&mut self, ty: Self::Type, ptr: Self::Value, indices: &[Self::Value]) -> Self::Value {
        let mut expr = self.mcx.value(ptr);
        if let Some(CTy::Ref(kind)) = self.value_ty(ptr) {
            if matches!(kind.0, CTyKind::Struct(_)) && ty != CTy::Ref(kind) {
                let cast_ptr_ty = self.pointer_to(ty);
                let cast_ptr = self.bb.func.0.next_local_var();
                let addr = self.mcx.unary("&", self.mcx.value(ptr));
                let cast = self.mcx.cast(cast_ptr_ty, addr);
                self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(
                    cast_ptr,
                    cast_ptr_ty,
                    Some(cast),
                )));
                self.record_value_ty(cast_ptr, cast_ptr_ty);
                expr = self.mcx.value(cast_ptr);
            }
        }
        if let Some(pointee) = self.pointer_pointee_ty(ptr) {
            if pointee != ty {
                let cast_ptr_ty = self.pointer_to(ty);
                let cast_ptr = self.bb.func.0.next_local_var();
                let cast = self.mcx.cast(cast_ptr_ty, expr);
                self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(
                    cast_ptr,
                    cast_ptr_ty,
                    Some(cast),
                )));
                self.record_value_ty(cast_ptr, cast_ptr_ty);
                expr = self.mcx.value(cast_ptr);
            }
        }
        let mut projected_ty = ty;
        let mut projected = false;

        for (i, index) in indices.iter().enumerate() {
            let const_index = self.cx.const_to_opt_uint(*index).map(|v| v as usize);

            if i == 0 && const_index == Some(0) && indices.len() > 1 {
                if let CTy::Ref(kind) = projected_ty {
                    match kind.0 {
                        CTyKind::Array(elem, _) | CTyKind::Pointer(elem) => {
                            projected_ty = *elem;
                        }
                        CTyKind::Function { .. } => {
                            panic!("gep cannot project into function type")
                        }
                        CTyKind::Struct(_) => {}
                    }
                }
                continue;
            }
            projected = true;

            projected_ty = match projected_ty {
                CTy::Ref(kind) => match kind.0 {
                    CTyKind::Array(elem, _) => {
                        let idx_expr = if let Some(index) = const_index {
                            self.mcx.value(CValue::Scalar(index as i128))
                        } else {
                            self.mcx.value(*index)
                        };
                        expr = self.mcx.index(expr, idx_expr);
                        *elem
                    }
                    CTyKind::Pointer(elem) => {
                        let idx_expr = if let Some(index) = const_index {
                            self.mcx.value(CValue::Scalar(index as i128))
                        } else {
                            self.mcx.value(*index)
                        };
                        expr = self.mcx.index(expr, idx_expr);
                        *elem
                    }
                    CTyKind::Function { .. } => {
                        panic!("gep cannot index function type")
                    }
                    CTyKind::Struct(_) => {
                        let index = const_index.unwrap_or_else(|| {
                            panic!("gep on struct fields requires constant index")
                        });
                        let info = self.struct_layout_info(projected_ty).unwrap_or_else(|| {
                            panic!("missing struct layout metadata for {projected_ty:?}")
                        });
                        let field = info
                            .fields
                            .get(index)
                            .cloned()
                            .unwrap_or_else(|| panic!("invalid struct field index {index}"));
                        let byte_ptr = self.pointer_to(CTy::UInt(CUintTy::U8));
                        let base = self.mcx.cast(byte_ptr, self.mcx.unary("&", expr));
                        let addr = if field.offset == 0 {
                            base
                        } else {
                            self.mcx
                                .index(base, self.mcx.value(CValue::Scalar(field.offset as i128)))
                        };
                        let field_ptr = self.pointer_to(field.ty);
                        let typed_ptr = self.mcx.cast(field_ptr, addr);
                        expr = self.mcx.unary("*", typed_ptr);
                        field.ty
                    }
                },
                _ => {
                    let idx_expr = if let Some(index) = const_index {
                        self.mcx.value(CValue::Scalar(index as i128))
                    } else {
                        self.mcx.value(*index)
                    };
                    expr = self.mcx.index(expr, idx_expr);
                    projected_ty
                }
            };
        }

        if !projected {
            self.ensure_alloca_decl(ptr, Some(ty));
            return ptr;
        }

        let inferred_decl_ty = self.pending_alloca_bytes(ptr).and_then(|bytes| {
            self.c_ty_size_bytes(projected_ty).and_then(|elem_bytes| {
                if elem_bytes == 0 || bytes % elem_bytes != 0 {
                    return None;
                }
                let len = bytes / elem_bytes;
                Some(CTy::Ref(Interned::new_unchecked(
                    self.mcx.arena().alloc(CTyKind::Array(projected_ty, len)),
                )))
            })
        });
        let suggested_decl_ty = match ty {
            CTy::Ref(kind) if matches!(kind.0, CTyKind::Struct(_)) => Some(ty),
            _ => inferred_decl_ty.or(Some(ty)),
        };
        self.ensure_alloca_decl(ptr, suggested_decl_ty);

        let ret = self.bb.func.0.next_local_var();
        let ret_ty = self.pointer_to(projected_ty);
        let addr = self.mcx.unary("&", expr);
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ret_ty, Some(addr))));
        self.record_value_ty(ret, ret_ty);
        self.update_ptr_pointee_ty(ret, projected_ty);
        self.update_ptr_lvalue(ret, expr);
        ret
    }

    fn inbounds_gep(
        &mut self,
        ty: Self::Type,
        ptr: Self::Value,
        indices: &[Self::Value],
    ) -> Self::Value {
        self.gep(ty, ptr, indices)
    }

    fn trunc(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.intcast(val, dest_ty, false)
    }

    fn sext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.intcast(val, dest_ty, true)
    }

    fn fptoui_sat(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fptosi_sat(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fptoui(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fptosi(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn uitofp(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn sitofp(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fptrunc(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn fpext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn ptrtoint(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        let ret = self.bb.func.0.next_local_var();
        let cast = self.mcx.cast(dest_ty, self.mcx.value(val));
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, dest_ty, Some(cast))));
        self.record_value_ty(ret, dest_ty);
        ret
    }

    fn inttoptr(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        let ret = self.bb.func.0.next_local_var();
        let cast = self.mcx.cast(dest_ty, self.mcx.value(val));
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, dest_ty, Some(cast))));
        self.record_value_ty(ret, dest_ty);
        ret
    }

    fn bitcast(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        let src_ty = self.value_ty(val).unwrap_or(dest_ty);
        let src_bytes = self
            .c_ty_size_bytes(src_ty)
            .unwrap_or_else(|| panic!("bitcast source size is not known: {src_ty:?}"));
        let dest_bytes = self
            .c_ty_size_bytes(dest_ty)
            .unwrap_or_else(|| panic!("bitcast destination size is not known: {dest_ty:?}"));
        assert_eq!(src_bytes, dest_bytes, "bitcast size mismatch: {src_ty:?} -> {dest_ty:?}");

        let src = self.bb.func.0.next_local_var();
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(
            src,
            src_ty,
            Some(self.mcx.value(val)),
        )));
        self.record_value_ty(src, src_ty);

        let ret = self.bb.func.0.next_local_var();
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, dest_ty, None)));
        self.record_value_ty(ret, dest_ty);

        let void_ptr = self.pointer_to(CTy::Void);
        let dst_ptr = self.mcx.cast(void_ptr, self.mcx.unary("&", self.mcx.value(ret)));
        let src_ptr = self.mcx.cast(void_ptr, self.mcx.unary("&", self.mcx.value(src)));
        let size_arg = self
            .mcx
            .cast(CTy::UInt(CUintTy::Usize), self.mcx.value(CValue::Scalar(dest_bytes as i128)));
        let copy =
            self.mcx.call(self.mcx.raw("__builtin_memcpy"), vec![dst_ptr, src_ptr, size_arg]);
        self.bb.func.0.push_stmt(self.mcx.expr_stmt(copy));

        ret
    }

    /// Performs cast between integers, x as ty in Rust.
    ///
    /// If the bit width is different, a truncation or extension is required.
    /// The type of extension—sign-extension or zero-extension—depends on the
    /// signedness of the source type.
    ///
    /// According to the C17 standard, section "6.3.1.3 Signed and unsigned
    /// integers", casting to an unsigned integer behaves the same as in Rust.
    /// However, casting to a signed integer is implementation-defined.
    ///
    /// Therefore, a two-step cast is necessary. First, cast to an unsigned
    /// integer via explicit conversion. Then, use a helper function to cast the
    /// result to a signed integer.
    fn intcast(&mut self, val: Self::Value, dest_ty: Self::Type, is_signed: bool) -> Self::Value {
        let mcx = self.cx.mcx;
        let ret = self.bb.func.0.next_local_var();

        let mut cast = mcx.cast(dest_ty, mcx.value(val));
        if dest_ty.is_signed() {
            cast = mcx.call(
                mcx.raw("__rust_utos"),
                vec![
                    mcx.raw(dest_ty.to_unsigned().to_str()),
                    mcx.raw(dest_ty.to_str()),
                    cast,
                    mcx.raw(dest_ty.max_value()),
                ],
            );
        }
        self.bb.func.0.push_stmt(mcx.decl_stmt(mcx.var(ret, dest_ty, Some(cast))));
        self.record_value_ty(ret, dest_ty);
        ret
    }

    fn pointercast(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        let ret = self.bb.func.0.next_local_var();
        let cast = self.mcx.cast(dest_ty, self.mcx.value(val));
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, dest_ty, Some(cast))));
        self.record_value_ty(ret, dest_ty);

        if let Some(pointee) = self.pointer_pointee_ty(val) {
            self.update_ptr_pointee_ty(ret, pointee);
        }
        if let Some(lvalue) = self.pointer_lvalue(val) {
            self.update_ptr_lvalue(ret, lvalue);
        }

        ret
    }

    fn icmp(
        &mut self,
        op: rustc_codegen_ssa::common::IntPredicate,
        lhs: Self::Value,
        rhs: Self::Value,
    ) -> Self::Value {
        use rustc_codegen_ssa::common::IntPredicate;

        let op = match op {
            IntPredicate::IntEQ => "==",
            IntPredicate::IntNE => "!=",
            IntPredicate::IntUGT | IntPredicate::IntSGT => ">",
            IntPredicate::IntUGE | IntPredicate::IntSGE => ">=",
            IntPredicate::IntULT | IntPredicate::IntSLT => "<",
            IntPredicate::IntULE | IntPredicate::IntSLE => "<=",
        };

        let ret = self.bb.func.0.next_local_var();
        let expr = self.mcx.binary(self.mcx.value(lhs), self.mcx.value(rhs), op);
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, CTy::Bool, Some(expr))));
        self.record_value_ty(ret, CTy::Bool);
        ret
    }

    fn fcmp(
        &mut self,
        op: rustc_codegen_ssa::common::RealPredicate,
        lhs: Self::Value,
        rhs: Self::Value,
    ) -> Self::Value {
        todo!()
    }

    fn memcpy(
        &mut self,
        dst: Self::Value,
        dst_align: rustc_abi::Align,
        src: Self::Value,
        src_align: rustc_abi::Align,
        size: Self::Value,
        flags: rustc_codegen_ssa::MemFlags,
    ) {
        self.ensure_alloca_byte_array_decl(dst);
        self.ensure_alloca_byte_array_decl(src);

        let void_ptr = self.pointer_to(CTy::Void);
        let dst_ptr = self.mcx.cast(void_ptr, self.mcx.value(dst));
        let src_ptr = self.mcx.cast(void_ptr, self.mcx.value(src));
        let size_arg = match self.value_ty(size) {
            Some(CTy::UInt(CUintTy::Usize)) => self.mcx.value(size),
            _ => self.mcx.cast(CTy::UInt(CUintTy::Usize), self.mcx.value(size)),
        };
        let call =
            self.mcx.call(self.mcx.raw("__builtin_memcpy"), vec![dst_ptr, src_ptr, size_arg]);
        self.bb.func.0.push_stmt(self.mcx.expr_stmt(call));
    }

    fn memmove(
        &mut self,
        dst: Self::Value,
        dst_align: rustc_abi::Align,
        src: Self::Value,
        src_align: rustc_abi::Align,
        size: Self::Value,
        flags: rustc_codegen_ssa::MemFlags,
    ) {
        self.ensure_alloca_byte_array_decl(dst);
        self.ensure_alloca_byte_array_decl(src);

        let void_ptr = self.pointer_to(CTy::Void);
        let dst_ptr = self.mcx.cast(void_ptr, self.mcx.value(dst));
        let src_ptr = self.mcx.cast(void_ptr, self.mcx.value(src));
        let size_arg = match self.value_ty(size) {
            Some(CTy::UInt(CUintTy::Usize)) => self.mcx.value(size),
            _ => self.mcx.cast(CTy::UInt(CUintTy::Usize), self.mcx.value(size)),
        };
        let call =
            self.mcx.call(self.mcx.raw("__builtin_memmove"), vec![dst_ptr, src_ptr, size_arg]);
        self.bb.func.0.push_stmt(self.mcx.expr_stmt(call));
    }

    fn memset(
        &mut self,
        ptr: Self::Value,
        fill_byte: Self::Value,
        size: Self::Value,
        align: rustc_abi::Align,
        flags: rustc_codegen_ssa::MemFlags,
    ) {
        self.ensure_alloca_byte_array_decl(ptr);

        let void_ptr = self.pointer_to(CTy::Void);
        let dst_ptr = self.mcx.cast(void_ptr, self.mcx.value(ptr));
        let fill_arg = match self.value_ty(fill_byte) {
            Some(CTy::Int(CIntTy::I32)) => self.mcx.value(fill_byte),
            _ => self.mcx.cast(CTy::Int(CIntTy::I32), self.mcx.value(fill_byte)),
        };
        let size_arg = match self.value_ty(size) {
            Some(CTy::UInt(CUintTy::Usize)) => self.mcx.value(size),
            _ => self.mcx.cast(CTy::UInt(CUintTy::Usize), self.mcx.value(size)),
        };
        let call =
            self.mcx.call(self.mcx.raw("__builtin_memset"), vec![dst_ptr, fill_arg, size_arg]);
        self.bb.func.0.push_stmt(self.mcx.expr_stmt(call));
    }

    fn select(
        &mut self,
        cond: Self::Value,
        then_val: Self::Value,
        else_val: Self::Value,
    ) -> Self::Value {
        let result_ty = match (self.value_ty(then_val), self.value_ty(else_val)) {
            (Some(a), Some(b)) if a == b => a,
            (Some(a), None) => a,
            (None, Some(b)) => b,
            (Some(a), Some(b)) => panic!("select value type mismatch: {a:?} vs {b:?}"),
            (None, None) => panic!("cannot infer select result type"),
        };

        let cond_expr = self.mcx.value(cond);
        let then_expr = match self.value_ty(then_val) {
            Some(ty) if ty != result_ty => self.mcx.cast(result_ty, self.mcx.value(then_val)),
            _ => self.mcx.value(then_val),
        };
        let else_expr = match self.value_ty(else_val) {
            Some(ty) if ty != result_ty => self.mcx.cast(result_ty, self.mcx.value(else_val)),
            _ => self.mcx.value(else_val),
        };
        let expr = self.mcx.ternary(cond_expr, then_expr, else_expr);
        let ret = self.bb.func.0.next_local_var();
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, result_ty, Some(expr))));
        self.record_value_ty(ret, result_ty);
        ret
    }

    fn va_arg(&mut self, list: Self::Value, ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn extract_element(&mut self, vec: Self::Value, idx: Self::Value) -> Self::Value {
        todo!()
    }

    fn vector_splat(&mut self, num_elts: usize, elt: Self::Value) -> Self::Value {
        todo!()
    }

    fn extract_value(&mut self, agg_val: Self::Value, idx: u64) -> Self::Value {
        let (a, b) = self
            .packed_pair_values(agg_val)
            .unwrap_or_else(|| panic!("extract_value on non-pair aggregate: {agg_val:?}"));
        match idx {
            0 => a,
            1 => b,
            _ => panic!("invalid scalar-pair extract index {idx}"),
        }
    }

    fn insert_value(&mut self, agg_val: Self::Value, elt: Self::Value, idx: u64) -> Self::Value {
        let (mut a, mut b) = self.packed_pair_values(agg_val).unwrap_or((agg_val, agg_val));
        match idx {
            0 => a = elt,
            1 => b = elt,
            _ => panic!("invalid scalar-pair insert index {idx}"),
        }

        let pair_val = if self.packed_pair_values(agg_val).is_some() {
            agg_val
        } else {
            self.bb.func.0.next_local_var()
        };
        self.set_packed_pair_values(pair_val, a, b);
        pair_val
    }

    fn set_personality_fn(&mut self, personality: Self::Function) {
        let _ = personality;
    }

    fn cleanup_landing_pad(&mut self, pers_fn: Self::Function) -> (Self::Value, Self::Value) {
        todo!()
    }

    fn filter_landing_pad(&mut self, pers_fn: Self::Function) {
        todo!()
    }

    fn resume(&mut self, exn0: Self::Value, exn1: Self::Value) {
        let _ = (exn0, exn1);
        self.unreachable();
    }

    fn cleanup_pad(&mut self, parent: Option<Self::Value>, args: &[Self::Value]) -> Self::Funclet {
        todo!()
    }

    fn cleanup_ret(&mut self, funclet: &Self::Funclet, unwind: Option<Self::BasicBlock>) {
        todo!()
    }

    fn catch_pad(&mut self, parent: Self::Value, args: &[Self::Value]) -> Self::Funclet {
        todo!()
    }

    fn catch_switch(
        &mut self,
        parent: Option<Self::Value>,
        unwind: Option<Self::BasicBlock>,
        handlers: &[Self::BasicBlock],
    ) -> Self::Value {
        todo!()
    }

    fn atomic_cmpxchg(
        &mut self,
        dst: Self::Value,
        cmp: Self::Value,
        src: Self::Value,
        order: rustc_middle::ty::AtomicOrdering,
        failure_order: rustc_middle::ty::AtomicOrdering,
        weak: bool,
    ) -> (Self::Value, Self::Value) {
        todo!()
    }

    fn atomic_rmw(
        &mut self,
        op: rustc_codegen_ssa::common::AtomicRmwBinOp,
        dst: Self::Value,
        src: Self::Value,
        order: rustc_middle::ty::AtomicOrdering,
        ret_ptr: bool,
    ) -> Self::Value {
        todo!()
    }

    fn atomic_fence(
        &mut self,
        order: rustc_middle::ty::AtomicOrdering,
        scope: rustc_codegen_ssa::common::SynchronizationScope,
    ) {
        todo!()
    }

    fn set_invariant_load(&mut self, load: Self::Value) {
        todo!()
    }

    fn lifetime_start(&mut self, ptr: Self::Value, size: rustc_abi::Size) {
        // no-op for C backend
    }

    fn lifetime_end(&mut self, ptr: Self::Value, size: rustc_abi::Size) {
        // no-op for C backend
    }

    fn call(
        &mut self,
        llty: Self::Type,
        fn_attrs: Option<&rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrs>,
        fn_abi: Option<&rustc_target::callconv::FnAbi<'tcx, rustc_middle::ty::Ty<'tcx>>>,
        llfn: Self::Value,
        args: &[Self::Value],
        funclet: Option<&Self::Funclet>,
        instance: Option<rustc_middle::ty::Instance<'tcx>>,
    ) -> Self::Value {
        let fn_abi = fn_abi.unwrap();

        if matches!(fn_abi.ret.mode, PassMode::Indirect { .. }) {
            if let Some(ret_ptr) = args.first().copied() {
                self.ensure_alloca_decl(ret_ptr, Some(self.cx.backend_type(fn_abi.ret.layout)));
            }
        }
        let arg_start = if matches!(fn_abi.ret.mode, PassMode::Indirect { .. }) { 1 } else { 0 };
        for (abi_arg, value) in fn_abi.args.iter().zip(args.iter().skip(arg_start)) {
            if matches!(abi_arg.mode, PassMode::Indirect { meta_attrs: None, .. }) {
                self.ensure_alloca_decl(*value, Some(self.cx.backend_type(abi_arg.layout)));
            }
        }
        for value in args.iter().copied() {
            self.ensure_alloca_byte_array_decl(value);
        }

        let mut args = args.iter().map(|v| self.mcx.value(*v)).collect::<Vec<_>>();
        let mut callee = llfn;
        if instance
            .and_then(|inst| self.tcx.lang_items().from_def_id(inst.def_id()))
            .is_some_and(|item| item == LangItem::PanicBoundsCheck)
        {
            callee = CValue::Func("__rust_panic_bounds_check");
            if args.len() > 2 {
                args.truncate(2);
            }
        }

        let expected_callee_ty = self.cx.fn_ptr_backend_type(fn_abi);
        let callee_expr = match callee {
            CValue::Func(_) => self.mcx.value(callee),
            _ => self.mcx.cast(expected_callee_ty, self.mcx.value(callee)),
        };

        let call = self.mcx.call(callee_expr, args);
        match fn_abi.ret.mode {
            PassMode::Ignore | PassMode::Indirect { .. } => {
                self.bb.func.0.push_stmt(self.mcx.expr_stmt(call));
                CValue::Scalar(0)
            }
            PassMode::Direct(_) => {
                let ret_ty = self.cx.immediate_backend_type(fn_abi.ret.layout);
                if ret_ty == CTy::Void {
                    self.bb.func.0.push_stmt(self.mcx.expr_stmt(call));
                    CValue::Scalar(0)
                } else {
                    let ret = self.bb.func.0.next_local_var();
                    self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(
                        ret,
                        ret_ty,
                        Some(call),
                    )));
                    self.record_value_ty(ret, ret_ty);
                    ret
                }
            }
            PassMode::Pair(_, _) => {
                let tuple_ty = self.abi_tuple_ty_for_pair_layout(fn_abi.ret.layout);
                let tuple = self.bb.func.0.next_local_var();
                self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(
                    tuple,
                    tuple_ty,
                    Some(call),
                )));
                self.record_value_ty(tuple, tuple_ty);

                let a = self.load_abi_tuple_field(tuple, tuple_ty, 0);
                let b = self.load_abi_tuple_field(tuple, tuple_ty, 1);
                let ret = self.bb.func.0.next_local_var();
                self.set_packed_pair_values(ret, a, b);
                ret
            }
            PassMode::Cast { ref cast, pad_i32: _ } => {
                let field_tys = self
                    .cx
                    .cast_target_to_c_abi_pieces(cast)
                    .into_iter()
                    .map(|(_, ty)| ty)
                    .collect::<Vec<_>>();
                let tuple_ty = self.cx.abi_tuple_ty(&field_tys);
                let ret = self.bb.func.0.next_local_var();
                self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(
                    ret,
                    tuple_ty,
                    Some(call),
                )));
                self.record_value_ty(ret, tuple_ty);
                ret
            }
        }
    }

    fn tail_call(
        &mut self,
        llty: Self::Type,
        fn_attrs: Option<&rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrs>,
        fn_abi: &rustc_target::callconv::FnAbi<'tcx, rustc_middle::ty::Ty<'tcx>>,
        llfn: Self::Value,
        args: &[Self::Value],
        funclet: Option<&Self::Funclet>,
        instance: Option<rustc_middle::ty::Instance<'tcx>>,
    ) {
        todo!()
    }

    fn zext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.intcast(val, dest_ty, false)
    }

    fn apply_attrs_to_cleanup_callsite(&mut self, llret: Self::Value) {
        todo!()
    }
}
