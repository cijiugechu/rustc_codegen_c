#![allow(unused_variables)] // TODO

use std::ops::Deref;

use rustc_abi::{BackendRepr, HasDataLayout, TargetDataLayout};
use rustc_codegen_c_ast::expr::CValue;
use rustc_codegen_c_ast::ty::{CFloatTy, CIntTy, CTy, CTyKind, CUintTy};
use rustc_codegen_ssa::common::{AtomicRmwBinOp, SynchronizationScope};
use rustc_codegen_ssa::traits::{
    BackendTypes, BaseTypeCodegenMethods, BuilderMethods, ConstCodegenMethods,
    LayoutTypeCodegenMethods,
};
use rustc_data_structures::intern::Interned;
use rustc_hash::FxHashMap;
use rustc_hir::LangItem;
use rustc_middle::ty;
use rustc_middle::ty::layout::{
    FnAbiError, FnAbiOfHelpers, FnAbiRequest, HasTyCtxt, HasTypingEnv, LayoutError,
    LayoutOfHelpers, TyAndLayout,
};
use rustc_middle::ty::{AtomicOrdering, Ty, TyCtxt};
use rustc_target::callconv::{FnAbi, PassMode};
use rustc_target::spec::{HasTargetSpec, Target};

use crate::config::CStandard;
use crate::context::{CBasicBlock, CodegenCx, PendingAlloca};
use crate::include_plan::IncludeCapability;

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AtomicBackendMode {
    C11,
    Builtin,
}

const ATOMIC_CMPXCHG_INVALID_ORDER_MSG: &str =
    "invalid atomic cmpxchg ordering: failure ordering must be no stronger than success and must not be Release/AcqRel";

fn select_atomic_mode(c_std: CStandard) -> AtomicBackendMode {
    match c_std {
        CStandard::C99 | CStandard::Gnu99 => AtomicBackendMode::Builtin,
        CStandard::C11
        | CStandard::C17
        | CStandard::C23
        | CStandard::Gnu11
        | CStandard::Gnu17
        | CStandard::Gnu23 => AtomicBackendMode::C11,
    }
}

fn map_order_c11(order: AtomicOrdering) -> &'static str {
    match order {
        AtomicOrdering::Relaxed => "memory_order_relaxed",
        AtomicOrdering::Release => "memory_order_release",
        AtomicOrdering::Acquire => "memory_order_acquire",
        AtomicOrdering::AcqRel => "memory_order_acq_rel",
        AtomicOrdering::SeqCst => "memory_order_seq_cst",
    }
}

fn map_order_builtin(order: AtomicOrdering) -> &'static str {
    match order {
        AtomicOrdering::Relaxed => "__ATOMIC_RELAXED",
        AtomicOrdering::Release => "__ATOMIC_RELEASE",
        AtomicOrdering::Acquire => "__ATOMIC_ACQUIRE",
        AtomicOrdering::AcqRel => "__ATOMIC_ACQ_REL",
        AtomicOrdering::SeqCst => "__ATOMIC_SEQ_CST",
    }
}

fn validate_cmpxchg_order(
    success: AtomicOrdering,
    failure: AtomicOrdering,
) -> Result<(), &'static str> {
    let _ = success;
    let valid = matches!(
        failure,
        AtomicOrdering::Relaxed | AtomicOrdering::Acquire | AtomicOrdering::SeqCst
    );

    if valid {
        Ok(())
    } else {
        Err(ATOMIC_CMPXCHG_INVALID_ORDER_MSG)
    }
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
            .or_else(|| match value {
                CValue::Func(_) => Some(self.cx.val_ty(value)),
                _ => None,
            })
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
                    CIntTy::I128 => 128,
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
                    CUintTy::U128 => 128,
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

    fn infer_float_binop_ty(&self, lhs: CValue<'mx>, rhs: CValue<'mx>, op: &str) -> CTy<'mx> {
        let ty = match (self.value_ty(lhs), self.value_ty(rhs)) {
            (Some(lhs), Some(rhs)) if lhs == rhs => lhs,
            (Some(lhs), Some(rhs)) => {
                panic!("type mismatch for {op}: {lhs:?} vs {rhs:?}")
            }
            (Some(lhs), None) => lhs,
            (None, Some(rhs)) => rhs,
            (None, None) => panic!("cannot infer operand type for {op}"),
        };

        match ty {
            CTy::Float(CFloatTy::F32) | CTy::Float(CFloatTy::F64) => ty,
            _ => panic!("unsupported type for {op}: {ty:?}"),
        }
    }

    fn coerce_float_operand_expr(
        &self,
        value: CValue<'mx>,
        target_ty: CTy<'mx>,
    ) -> rustc_codegen_c_ast::expr::CExpr<'mx> {
        match self.value_ty(value) {
            Some(ty) if ty != target_ty => self.mcx.cast(target_ty, self.mcx.value(value)),
            None if matches!(value, CValue::Scalar(_) | CValue::ScalarTyped(_, _)) => {
                self.mcx.cast(target_ty, self.mcx.value(value))
            }
            _ => self.mcx.value(value),
        }
    }

    fn codegen_float_binop(
        &mut self,
        lhs: CValue<'mx>,
        rhs: CValue<'mx>,
        op_name: &str,
        c_op: &'static str,
    ) -> CValue<'mx> {
        let ty = self.infer_float_binop_ty(lhs, rhs, op_name);
        let ret = self.bb.func.0.next_local_var();
        let lhs = self.coerce_float_operand_expr(lhs, ty);
        let rhs = self.coerce_float_operand_expr(rhs, ty);
        let expr = self.mcx.binary(lhs, rhs, c_op);
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
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

    fn atomic_mode(&self) -> AtomicBackendMode {
        select_atomic_mode(self.cx.c_std)
    }

    fn atomic_fatal(&self, msg: impl Into<String>) -> ! {
        self.cx.tcx.sess.dcx().fatal(msg.into())
    }

    fn atomic_pointer_int_ty(&self) -> CTy<'mx> {
        match self.tcx.data_layout.pointer_size().bytes() {
            4 => CTy::UInt(CUintTy::U32),
            8 => CTy::UInt(CUintTy::U64),
            bytes => self.atomic_fatal(format!(
                "unsupported pointer width for atomic lowering: {bytes} bytes"
            )),
        }
    }

    fn atomic_storage_ty(&self, mem_ty: CTy<'mx>, intrinsic: &str) -> (CTy<'mx>, bool) {
        match mem_ty {
            CTy::Int(CIntTy::I128) | CTy::UInt(CUintTy::U128) => self.atomic_fatal(format!(
                "{intrinsic} does not support 128-bit atomics yet: {mem_ty:?}"
            )),
            CTy::Bool | CTy::Int(_) | CTy::UInt(_) => (mem_ty, false),
            CTy::Ref(kind) if matches!(kind.0, CTyKind::Pointer(_)) => {
                (self.atomic_pointer_int_ty(), true)
            }
            _ => self.atomic_fatal(format!(
                "{intrinsic} supports only integer/bool/pointer atomics, got {mem_ty:?}"
            )),
        }
    }

    fn atomic_validate_size(&self, mem_ty: CTy<'mx>, size: rustc_abi::Size, intrinsic: &str) {
        let Some(expected) = self.c_ty_size_bytes(mem_ty) else {
            self.atomic_fatal(format!(
                "{intrinsic} could not determine C size for atomic type {mem_ty:?}"
            ));
        };
        let actual = size.bytes() as usize;
        if expected != actual {
            self.atomic_fatal(format!(
                "{intrinsic} size mismatch: type size is {expected} bytes but intrinsic requested {actual} bytes"
            ));
        }
    }

    fn atomic_infer_mem_ty(
        &self,
        ptr: CValue<'mx>,
        fallback: Option<CTy<'mx>>,
        intrinsic: &str,
    ) -> CTy<'mx> {
        let ptr_ty = self.pointer_pointee_ty(ptr);
        match (ptr_ty, fallback) {
            (Some(ptr_ty), Some(fallback_ty)) => {
                let ptr_is_byte = matches!(ptr_ty, CTy::UInt(CUintTy::U8) | CTy::Int(CIntTy::I8));
                let fallback_is_pointer = matches!(
                    fallback_ty,
                    CTy::Ref(kind) if matches!(kind.0, CTyKind::Pointer(_))
                );
                let fallback_wider = self.c_ty_size_bytes(fallback_ty).unwrap_or(0)
                    > self.c_ty_size_bytes(ptr_ty).unwrap_or(0);
                if fallback_is_pointer || (ptr_is_byte && fallback_wider) {
                    fallback_ty
                } else {
                    ptr_ty
                }
            }
            (Some(ptr_ty), None) => ptr_ty,
            (None, Some(fallback_ty)) => fallback_ty,
            (None, None) => self.atomic_fatal(format!(
                "{intrinsic} could not infer atomic pointee type for pointer value {ptr:?}"
            )),
        }
    }

    fn atomic_c11_ptr_macro(&self, op_ty: CTy<'mx>, is_const: bool) -> &'static str {
        match (op_ty, is_const) {
            (CTy::Bool, false) => "__rust_atomic_bool_ptr",
            (CTy::Bool, true) => "__rust_atomic_bool_const_ptr",
            (CTy::Int(CIntTy::I8), false) => "__rust_atomic_i8_ptr",
            (CTy::Int(CIntTy::I8), true) => "__rust_atomic_i8_const_ptr",
            (CTy::Int(CIntTy::I16), false) => "__rust_atomic_i16_ptr",
            (CTy::Int(CIntTy::I16), true) => "__rust_atomic_i16_const_ptr",
            (CTy::Int(CIntTy::I32), false) => "__rust_atomic_i32_ptr",
            (CTy::Int(CIntTy::I32), true) => "__rust_atomic_i32_const_ptr",
            (CTy::Int(CIntTy::I64), false) => "__rust_atomic_i64_ptr",
            (CTy::Int(CIntTy::I64), true) => "__rust_atomic_i64_const_ptr",
            (CTy::Int(CIntTy::Isize), false) => "__rust_atomic_size_ptr",
            (CTy::Int(CIntTy::Isize), true) => "__rust_atomic_size_const_ptr",
            (CTy::UInt(CUintTy::U8), false) => "__rust_atomic_u8_ptr",
            (CTy::UInt(CUintTy::U8), true) => "__rust_atomic_u8_const_ptr",
            (CTy::UInt(CUintTy::U16), false) => "__rust_atomic_u16_ptr",
            (CTy::UInt(CUintTy::U16), true) => "__rust_atomic_u16_const_ptr",
            (CTy::UInt(CUintTy::U32), false) => "__rust_atomic_u32_ptr",
            (CTy::UInt(CUintTy::U32), true) => "__rust_atomic_u32_const_ptr",
            (CTy::UInt(CUintTy::U64), false) => "__rust_atomic_u64_ptr",
            (CTy::UInt(CUintTy::U64), true) => "__rust_atomic_u64_const_ptr",
            (CTy::UInt(CUintTy::Usize), false) => "__rust_atomic_size_ptr",
            (CTy::UInt(CUintTy::Usize), true) => "__rust_atomic_size_const_ptr",
            _ => self.atomic_fatal(format!(
                "C11 atomic lowering has no pointer cast macro for type {op_ty:?}"
            )),
        }
    }

    fn atomic_addr_expr(
        &self,
        ptr: CValue<'mx>,
        op_ty: CTy<'mx>,
        mode: AtomicBackendMode,
        is_const: bool,
    ) -> rustc_codegen_c_ast::expr::CExpr<'mx> {
        match mode {
            AtomicBackendMode::C11 => {
                let macro_name = self.atomic_c11_ptr_macro(op_ty, is_const);
                self.mcx.call(self.mcx.value(CValue::Func(macro_name)), vec![self.mcx.value(ptr)])
            }
            AtomicBackendMode::Builtin => {
                self.mcx.cast(self.pointer_to(op_ty), self.mcx.value(ptr))
            }
        }
    }

    fn atomic_order_expr(
        &self,
        order: AtomicOrdering,
        mode: AtomicBackendMode,
    ) -> rustc_codegen_c_ast::expr::CExpr<'mx> {
        let name = match mode {
            AtomicBackendMode::C11 => map_order_c11(order),
            AtomicBackendMode::Builtin => map_order_builtin(order),
        };
        self.mcx.value(CValue::Func(name))
    }

    fn atomic_value_expr(
        &self,
        value: CValue<'mx>,
        target_ty: CTy<'mx>,
    ) -> rustc_codegen_c_ast::expr::CExpr<'mx> {
        match self.value_ty(value) {
            Some(value_ty) if value_ty != target_ty => {
                self.mcx.cast(target_ty, self.mcx.value(value))
            }
            Some(_) => self.mcx.value(value),
            None => self.mcx.cast(target_ty, self.mcx.value(value)),
        }
    }

    fn ensure_c11_atomic_include(&self, mode: AtomicBackendMode) {
        if mode == AtomicBackendMode::C11 {
            self.cx.require_include_capability(IncludeCapability::AtomicApi);
        }
    }

    fn call_signature_from_ty(&self, llty: CTy<'mx>) -> (CTy<'mx>, CTy<'mx>) {
        match llty {
            CTy::Ref(kind) => match kind.0 {
                CTyKind::Function { ret, .. } => (*ret, self.pointer_to(llty)),
                CTyKind::Pointer(pointee) => match *pointee {
                    CTy::Ref(inner) => match inner.0 {
                        CTyKind::Function { ret, .. } => (*ret, llty),
                        _ => panic!("call expects function type, got pointer to {inner:?}"),
                    },
                    _ => panic!("call expects pointer-to-function type, got {llty:?}"),
                },
                _ => panic!("call expects function type, got {llty:?}"),
            },
            _ => panic!("call expects function type, got {llty:?}"),
        }
    }

    fn pointer_pointee_ty(&self, ptr: CValue<'mx>) -> Option<CTy<'mx>> {
        let fkey = self.bb.func.0 as *const _ as usize;
        if let Some(pointee) = self.cx.ptr_pointees.borrow().get(&(fkey, ptr)).copied() {
            return Some(pointee);
        }

        self.value_ty(ptr).and_then(|ty| match ty {
            CTy::Ref(kind) => match kind.0 {
                CTyKind::Pointer(elem)
                | CTyKind::Array(elem, _)
                | CTyKind::IncompleteArray(elem) => Some(*elem),
                CTyKind::Function { .. } => None,
                CTyKind::Struct(_) | CTyKind::Union(_) => None,
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

    fn alloca_byte_array_ty(&self, bytes: usize) -> CTy<'mx> {
        CTy::Ref(Interned::new_unchecked(
            self.mcx.arena().alloc(CTyKind::Array(CTy::UInt(CUintTy::U8), bytes.max(1))),
        ))
    }

    fn mark_alloca_prefer_bytes(&mut self, ptr: CValue<'mx>) {
        let fkey = self.fkey();
        let Some(slot) = self.cx.pending_allocas.borrow().get(&(fkey, ptr)).copied() else {
            return;
        };
        if slot.declared || slot.prefer_bytes {
            return;
        }
        self.cx
            .pending_allocas
            .borrow_mut()
            .insert((fkey, ptr), PendingAlloca { prefer_bytes: true, ..slot });
    }

    fn ensure_alloca_decl(&mut self, ptr: CValue<'mx>, suggested_ty: Option<CTy<'mx>>) {
        let fkey = self.bb.func.0 as *const _ as usize;
        let Some(slot) = self.cx.pending_allocas.borrow().get(&(fkey, ptr)).copied() else {
            return;
        };
        if slot.declared {
            return;
        }

        let decl_ty = if slot.prefer_bytes {
            self.alloca_byte_array_ty(slot.bytes)
        } else {
            match suggested_ty {
                Some(CTy::Ref(kind)) if matches!(kind.0, CTyKind::Array(_, _)) => CTy::Ref(kind),
                // Keep stack slots conservative for struct-like projections: represent storage as
                // bytes and rely on typed casts at use sites.
                Some(CTy::Ref(kind)) if matches!(kind.0, CTyKind::Struct(_)) => {
                    self.alloca_byte_array_ty(slot.bytes)
                }
                Some(other) => {
                    self.array_decl_ty_from_bytes(slot.bytes, other).unwrap_or_else(|| {
                        // When the suggested element type doesn't tile the stack slot, fall back to a
                        // byte array and rely on typed loads/stores through explicit casts.
                        self.alloca_byte_array_ty(slot.bytes)
                    })
                }
                None => panic!("cannot infer alloca declaration type for {ptr:?}"),
            }
        };

        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ptr, decl_ty, None)));
        self.record_value_ty(ptr, decl_ty);
        if let CTy::Ref(kind) = decl_ty {
            match kind.0 {
                CTyKind::Array(elem, _)
                | CTyKind::IncompleteArray(elem)
                | CTyKind::Pointer(elem) => self.update_ptr_pointee_ty(ptr, *elem),
                CTyKind::Function { .. } => {}
                CTyKind::Struct(_) | CTyKind::Union(_) => {}
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
                rustc_codegen_c_ast::ty::CIntTy::I128 => 16,
                rustc_codegen_c_ast::ty::CIntTy::Isize => {
                    self.tcx.data_layout.pointer_size().bytes() as usize
                }
            }),
            CTy::UInt(int) => Some(match int {
                CUintTy::U8 => 1,
                CUintTy::U16 => 2,
                CUintTy::U32 => 4,
                CUintTy::U64 => 8,
                CUintTy::U128 => 16,
                CUintTy::Usize => self.tcx.data_layout.pointer_size().bytes() as usize,
            }),
            CTy::Ref(kind) => match kind.0 {
                CTyKind::Pointer(_) => Some(self.tcx.data_layout.pointer_size().bytes() as usize),
                CTyKind::Array(elem, count) => self.c_ty_size_bytes(*elem).map(|size| size * count),
                CTyKind::IncompleteArray(_) => {
                    let msg = crate::context::validate_incomplete_array_usage(false).unwrap_err();
                    self.cx.tcx.sess.dcx().fatal(format!("{msg} and has no size"));
                }
                CTyKind::Function { .. } => None,
                CTyKind::Struct(_) | CTyKind::Union(_) => {
                    self.struct_layout_info(ty).map(|info| info.size)
                }
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
        if size != 1 && size != 2 && size != 4 && size != 8 && size != 16 {
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
            16 => CTy::UInt(CUintTy::U128),
            _ => return None,
        })
    }

    fn fallback_store_ty_from_align(&self, align: rustc_abi::Align) -> CTy<'mx> {
        match align.bytes() {
            1 => CTy::UInt(CUintTy::U8),
            2 => CTy::UInt(CUintTy::U16),
            4 => CTy::UInt(CUintTy::U32),
            8 => CTy::UInt(CUintTy::U64),
            16 => CTy::UInt(CUintTy::U128),
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
        self.mark_alloca_prefer_bytes(ptr);
        let decl_ty = self.alloca_byte_array_ty(bytes);
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
        // In abort-only mode, unwinding edges are unreachable.
        let _ = catch;
        let ret = self.call(llty, fn_attrs, fn_abi, llfn, args, funclet, instance);
        self.br(then);
        ret
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
        self.codegen_float_binop(lhs, rhs, "fadd", "+")
    }

    fn fadd_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.fadd(lhs, rhs)
    }

    fn fadd_algebraic(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.fadd(lhs, rhs)
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
        self.codegen_float_binop(lhs, rhs, "fsub", "-")
    }

    fn fsub_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.fsub(lhs, rhs)
    }

    fn fsub_algebraic(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.fsub(lhs, rhs)
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
        self.codegen_float_binop(lhs, rhs, "fmul", "*")
    }

    fn fmul_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.fmul(lhs, rhs)
    }

    fn fmul_algebraic(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.fmul(lhs, rhs)
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
        self.codegen_float_binop(lhs, rhs, "fdiv", "/")
    }

    fn fdiv_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.fdiv(lhs, rhs)
    }

    fn fdiv_algebraic(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.fdiv(lhs, rhs)
    }

    fn urem(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        let ty = self.infer_unchecked_integer_binop_ty(lhs, rhs, "urem");
        if !matches!(ty, CTy::UInt(_)) {
            panic!("unsupported type for urem: {ty:?}");
        }
        let ret = self.bb.func.0.next_local_var();
        let lhs = self.coerce_int_operand_expr(lhs, ty);
        let rhs = self.coerce_int_operand_expr(rhs, ty);
        let expr = self.mcx.binary(lhs, rhs, "%");
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn srem(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        let ty = self.infer_integer_binop_ty(lhs, rhs, "srem");
        if !matches!(ty, CTy::Int(_)) {
            panic!("unsupported type for srem: {ty:?}");
        }
        let ret = self.bb.func.0.next_local_var();
        let lhs = self.coerce_int_operand_expr(lhs, ty);
        let rhs = self.coerce_int_operand_expr(rhs, ty);
        let expr = self.mcx.binary(lhs, rhs, "%");
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn frem(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        let ty = self.infer_float_binop_ty(lhs, rhs, "frem");
        let ret = self.bb.func.0.next_local_var();
        let lhs = self.coerce_float_operand_expr(lhs, ty);
        let rhs = self.coerce_float_operand_expr(rhs, ty);
        let builtin = match ty {
            CTy::Float(CFloatTy::F32) => "__builtin_fmodf",
            CTy::Float(CFloatTy::F64) => "__builtin_fmod",
            _ => unreachable!(),
        };
        let expr = self.mcx.call(self.mcx.raw(builtin), vec![lhs, rhs]);
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn frem_fast(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.frem(lhs, rhs)
    }

    fn frem_algebraic(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        self.frem(lhs, rhs)
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
                    CUintTy::U128 => CTy::Int(CIntTy::I128),
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
        let ty = match self.value_ty(v) {
            Some(ty @ (CTy::Float(CFloatTy::F32) | CTy::Float(CFloatTy::F64))) => ty,
            Some(other) => panic!("unsupported type for fneg: {other:?}"),
            None => panic!("cannot infer operand type for fneg"),
        };
        let ret = self.bb.func.0.next_local_var();
        let operand = self.coerce_float_operand_expr(v, ty);
        let expr = self.mcx.unary("-", operand);
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
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
            .insert((fkey, ret), PendingAlloca { bytes, declared: false, prefer_bytes: false });
        ret
    }

    fn load(&mut self, ty: Self::Type, ptr: Self::Value, align: rustc_abi::Align) -> Self::Value {
        self.ensure_alloca_decl(ptr, Some(ty));
        let expr = if let Some(lvalue) = self.pointer_lvalue(ptr) {
            match self.pointer_pointee_ty(ptr) {
                Some(pointee) if pointee == ty => lvalue,
                _ => {
                    // `ptr` may be tracked as a byte lvalue (e.g. after ptradd). When loading a
                    // wider/different type, cast the pointer itself and dereference, rather than
                    // casting the lvalue value.
                    let cast_ptr = self.mcx.cast(self.pointer_to(ty), self.mcx.value(ptr));
                    self.mcx.unary("*", cast_ptr)
                }
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
        let (op_ty, is_ptr) = self.atomic_storage_ty(ty, "atomic_load");
        self.atomic_validate_size(ty, size, "atomic_load");
        let mode = self.atomic_mode();
        self.ensure_c11_atomic_include(mode);

        let addr = self.atomic_addr_expr(ptr, op_ty, mode, true);
        let order = self.atomic_order_expr(order, mode);
        let callee = match mode {
            AtomicBackendMode::C11 => self.mcx.value(CValue::Func("atomic_load_explicit")),
            AtomicBackendMode::Builtin => self.mcx.raw("__atomic_load_n"),
        };
        let loaded = self.bb.func.0.next_local_var();
        let call = self.mcx.call(callee, vec![addr, order]);
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(loaded, op_ty, Some(call))));
        self.record_value_ty(loaded, op_ty);

        if !is_ptr && op_ty == ty {
            return loaded;
        }

        let ret = self.bb.func.0.next_local_var();
        let cast = self.mcx.cast(ty, self.mcx.value(loaded));
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(cast))));
        self.record_value_ty(ret, ty);
        ret
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
        let _ = (load, range);
    }

    fn nonnull_metadata(&mut self, load: Self::Value) {
        let _ = load;
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
        let store_is_pointer =
            matches!(store_ty, CTy::Ref(kind) if matches!(kind.0, CTyKind::Pointer(_)));
        let scalar_literal = matches!(val, CValue::Scalar(_) | CValue::ScalarTyped(_, _));
        let rhs = match val_ty {
            Some(val_ty) if val_ty == store_ty && store_is_pointer && scalar_literal => {
                self.mcx.cast(store_ty, self.mcx.value(val))
            }
            Some(val_ty) if val_ty == store_ty => self.mcx.value(val),
            Some(CTy::Ref(kind)) if matches!(kind.0, CTyKind::Array(elem, 1) if *elem == store_ty) => {
                self.mcx.index(self.mcx.value(val), self.mcx.value(CValue::Scalar(0)))
            }
            Some(_) => self.mcx.cast(store_ty, self.mcx.value(val)),
            None if store_is_pointer && scalar_literal => {
                self.mcx.cast(store_ty, self.mcx.value(val))
            }
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
        let mem_ty = self
            .value_ty(val)
            .unwrap_or_else(|| self.atomic_infer_mem_ty(ptr, None, "atomic_store"));
        let (op_ty, _) = self.atomic_storage_ty(mem_ty, "atomic_store");
        self.atomic_validate_size(mem_ty, size, "atomic_store");
        let mode = self.atomic_mode();
        self.ensure_c11_atomic_include(mode);

        let addr = self.atomic_addr_expr(ptr, op_ty, mode, false);
        let value = self.atomic_value_expr(val, op_ty);
        let order = self.atomic_order_expr(order, mode);
        let callee = match mode {
            AtomicBackendMode::C11 => self.mcx.value(CValue::Func("atomic_store_explicit")),
            AtomicBackendMode::Builtin => self.mcx.raw("__atomic_store_n"),
        };
        let call = self.mcx.call(callee, vec![addr, value, order]);
        self.bb.func.0.push_stmt(self.mcx.expr_stmt(call));
    }

    fn gep(&mut self, ty: Self::Type, ptr: Self::Value, indices: &[Self::Value]) -> Self::Value {
        let mut expr = self.mcx.value(ptr);
        if let Some(CTy::Ref(kind)) = self.value_ty(ptr) {
            if matches!(kind.0, CTyKind::Struct(_) | CTyKind::Union(_)) && ty != CTy::Ref(kind) {
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
                        CTyKind::Array(elem, _)
                        | CTyKind::IncompleteArray(elem)
                        | CTyKind::Pointer(elem) => {
                            projected_ty = *elem;
                        }
                        CTyKind::Function { .. } => {
                            panic!("gep cannot project into function type")
                        }
                        CTyKind::Struct(_) | CTyKind::Union(_) => {}
                    }
                }
                continue;
            }

            // The first index on a pointer-to-aggregate is element stepping (array semantics),
            // not a field projection. Keep the pointee type as the same aggregate.
            if i == 0
                && matches!(
                    projected_ty,
                    CTy::Ref(kind) if matches!(kind.0, CTyKind::Struct(_) | CTyKind::Union(_))
                )
            {
                let idx_expr = if let Some(index) = const_index {
                    self.mcx.value(CValue::Scalar(index as i128))
                } else {
                    self.mcx.value(*index)
                };
                expr = self.mcx.index(expr, idx_expr);
                projected = true;
                continue;
            }
            projected = true;

            projected_ty = match projected_ty {
                CTy::Ref(kind) => match kind.0 {
                    CTyKind::Array(elem, _) | CTyKind::IncompleteArray(elem) => {
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
                    CTyKind::Struct(_) | CTyKind::Union(_) => {
                        let index = const_index.unwrap_or_else(|| {
                            panic!("gep on aggregate fields requires constant index")
                        });
                        let info = self.struct_layout_info(projected_ty).unwrap_or_else(|| {
                            panic!("missing aggregate layout metadata for {projected_ty:?}")
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
        let suggested_decl_ty = inferred_decl_ty.or(Some(ty));
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
        panic!("float->int cast is not supported yet in rustc_codegen_c")
    }

    fn fptosi_sat(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        panic!("float->int cast is not supported yet in rustc_codegen_c")
    }

    fn fptoui(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        panic!("float->int cast is not supported yet in rustc_codegen_c")
    }

    fn fptosi(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        panic!("float->int cast is not supported yet in rustc_codegen_c")
    }

    fn uitofp(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if !matches!(dest_ty, CTy::Float(CFloatTy::F32) | CTy::Float(CFloatTy::F64)) {
            panic!("uitofp destination must be float, got {dest_ty:?}");
        }
        let ret = self.bb.func.0.next_local_var();
        let cast = self.mcx.cast(dest_ty, self.mcx.value(val));
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, dest_ty, Some(cast))));
        self.record_value_ty(ret, dest_ty);
        ret
    }

    fn sitofp(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        if !matches!(dest_ty, CTy::Float(CFloatTy::F32) | CTy::Float(CFloatTy::F64)) {
            panic!("sitofp destination must be float, got {dest_ty:?}");
        }
        let ret = self.bb.func.0.next_local_var();
        let cast = self.mcx.cast(dest_ty, self.mcx.value(val));
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, dest_ty, Some(cast))));
        self.record_value_ty(ret, dest_ty);
        ret
    }

    fn fptrunc(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        let src_ty = self
            .value_ty(val)
            .unwrap_or_else(|| panic!("cannot infer source type for fptrunc: {val:?}"));
        if !matches!(src_ty, CTy::Float(CFloatTy::F32) | CTy::Float(CFloatTy::F64))
            || !matches!(dest_ty, CTy::Float(CFloatTy::F32) | CTy::Float(CFloatTy::F64))
        {
            panic!("fptrunc expects float->float cast, got {src_ty:?} -> {dest_ty:?}");
        }
        let ret = self.bb.func.0.next_local_var();
        let cast = self.mcx.cast(dest_ty, self.mcx.value(val));
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, dest_ty, Some(cast))));
        self.record_value_ty(ret, dest_ty);
        ret
    }

    fn fpext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        let src_ty = self
            .value_ty(val)
            .unwrap_or_else(|| panic!("cannot infer source type for fpext: {val:?}"));
        if !matches!(src_ty, CTy::Float(CFloatTy::F32) | CTy::Float(CFloatTy::F64))
            || !matches!(dest_ty, CTy::Float(CFloatTy::F32) | CTy::Float(CFloatTy::F64))
        {
            panic!("fpext expects float->float cast, got {src_ty:?} -> {dest_ty:?}");
        }
        let ret = self.bb.func.0.next_local_var();
        let cast = self.mcx.cast(dest_ty, self.mcx.value(val));
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, dest_ty, Some(cast))));
        self.record_value_ty(ret, dest_ty);
        ret
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
        use rustc_codegen_ssa::common::RealPredicate;

        let ty = self.infer_float_binop_ty(lhs, rhs, "fcmp");
        let lhs_expr = self.coerce_float_operand_expr(lhs, ty);
        let rhs_expr = self.coerce_float_operand_expr(rhs, ty);

        let lhs_is_nan = self.mcx.call(self.mcx.raw("__builtin_isnan"), vec![lhs_expr]);
        let rhs_is_nan = self.mcx.call(self.mcx.raw("__builtin_isnan"), vec![rhs_expr]);
        let unordered = self.mcx.binary(lhs_is_nan, rhs_is_nan, "||");
        let ordered = self.mcx.unary("!", unordered);
        let cmp = |op| self.mcx.binary(lhs_expr, rhs_expr, op);

        let expr = match op {
            RealPredicate::RealOEQ => self.mcx.binary(ordered, cmp("=="), "&&"),
            RealPredicate::RealOGT => self.mcx.binary(ordered, cmp(">"), "&&"),
            RealPredicate::RealOGE => self.mcx.binary(ordered, cmp(">="), "&&"),
            RealPredicate::RealOLT => self.mcx.binary(ordered, cmp("<"), "&&"),
            RealPredicate::RealOLE => self.mcx.binary(ordered, cmp("<="), "&&"),
            RealPredicate::RealONE => self.mcx.binary(ordered, cmp("!="), "&&"),
            RealPredicate::RealORD => ordered,
            RealPredicate::RealUNO => unordered,
            RealPredicate::RealUEQ => self.mcx.binary(unordered, cmp("=="), "||"),
            RealPredicate::RealUGT => self.mcx.binary(unordered, cmp(">"), "||"),
            RealPredicate::RealUGE => self.mcx.binary(unordered, cmp(">="), "||"),
            RealPredicate::RealULT => self.mcx.binary(unordered, cmp("<"), "||"),
            RealPredicate::RealULE => self.mcx.binary(unordered, cmp("<="), "||"),
            RealPredicate::RealUNE => self.mcx.binary(unordered, cmp("!="), "||"),
            RealPredicate::RealPredicateTrue => self.mcx.value(self.const_bool(true)),
            RealPredicate::RealPredicateFalse => self.mcx.value(self.const_bool(false)),
        };

        let ret = self.bb.func.0.next_local_var();
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, CTy::Bool, Some(expr))));
        self.record_value_ty(ret, CTy::Bool);
        ret
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
        let _ = pers_fn;
        self.unreachable();
        (CValue::Scalar(0), CValue::Scalar(0))
    }

    fn filter_landing_pad(&mut self, pers_fn: Self::Function) {
        let _ = pers_fn;
        self.unreachable();
    }

    fn resume(&mut self, exn0: Self::Value, exn1: Self::Value) {
        let _ = (exn0, exn1);
        self.unreachable();
    }

    fn cleanup_pad(&mut self, parent: Option<Self::Value>, args: &[Self::Value]) -> Self::Funclet {
        let _ = (parent, args);
        self.unreachable();
    }

    fn cleanup_ret(&mut self, funclet: &Self::Funclet, unwind: Option<Self::BasicBlock>) {
        let _ = (funclet, unwind);
        self.unreachable();
    }

    fn catch_pad(&mut self, parent: Self::Value, args: &[Self::Value]) -> Self::Funclet {
        let _ = (parent, args);
        self.unreachable();
    }

    fn catch_switch(
        &mut self,
        parent: Option<Self::Value>,
        unwind: Option<Self::BasicBlock>,
        handlers: &[Self::BasicBlock],
    ) -> Self::Value {
        let _ = (parent, unwind, handlers);
        self.unreachable();
        CValue::Scalar(0)
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
        if let Err(msg) = validate_cmpxchg_order(order, failure_order) {
            self.atomic_fatal(msg);
        }

        let mem_ty = self.atomic_infer_mem_ty(
            dst,
            self.value_ty(cmp).or(self.value_ty(src)),
            "atomic_cmpxchg",
        );
        let (op_ty, is_ptr) = self.atomic_storage_ty(mem_ty, "atomic_cmpxchg");
        let mode = self.atomic_mode();
        self.ensure_c11_atomic_include(mode);

        let addr = self.atomic_addr_expr(dst, op_ty, mode, false);
        let expected = self.bb.func.0.next_local_var();
        let expected_init = self.atomic_value_expr(cmp, op_ty);
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(
            expected,
            op_ty,
            Some(expected_init),
        )));
        self.record_value_ty(expected, op_ty);

        let expected_ptr =
            self.mcx.cast(self.pointer_to(op_ty), self.mcx.unary("&", self.mcx.value(expected)));
        let desired = self.atomic_value_expr(src, op_ty);
        let success_order = self.atomic_order_expr(order, mode);
        let fail_order = self.atomic_order_expr(failure_order, mode);
        let success = self.bb.func.0.next_local_var();
        let call = match mode {
            AtomicBackendMode::C11 => {
                let callee = if weak {
                    self.mcx.value(CValue::Func("atomic_compare_exchange_weak_explicit"))
                } else {
                    self.mcx.value(CValue::Func("atomic_compare_exchange_strong_explicit"))
                };
                self.mcx.call(callee, vec![addr, expected_ptr, desired, success_order, fail_order])
            }
            AtomicBackendMode::Builtin => self.mcx.call(
                self.mcx.raw("__atomic_compare_exchange_n"),
                vec![
                    addr,
                    expected_ptr,
                    desired,
                    self.mcx.value(self.const_bool(weak)),
                    success_order,
                    fail_order,
                ],
            ),
        };
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(success, CTy::Bool, Some(call))));
        self.record_value_ty(success, CTy::Bool);

        let old = self.bb.func.0.next_local_var();
        let old_init = if is_ptr {
            self.mcx.cast(mem_ty, self.mcx.value(expected))
        } else if mem_ty == op_ty {
            self.mcx.value(expected)
        } else {
            self.mcx.cast(mem_ty, self.mcx.value(expected))
        };
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(old, mem_ty, Some(old_init))));
        self.record_value_ty(old, mem_ty);

        (old, success)
    }

    fn atomic_rmw(
        &mut self,
        op: rustc_codegen_ssa::common::AtomicRmwBinOp,
        dst: Self::Value,
        src: Self::Value,
        order: rustc_middle::ty::AtomicOrdering,
        ret_ptr: bool,
    ) -> Self::Value {
        let mem_ty = self.atomic_infer_mem_ty(dst, self.value_ty(src), "atomic_rmw");
        let (op_ty, is_ptr) = self.atomic_storage_ty(mem_ty, "atomic_rmw");
        let mode = self.atomic_mode();

        let use_c11 = mode == AtomicBackendMode::C11
            && !is_ptr
            && matches!(
                op,
                AtomicRmwBinOp::AtomicXchg
                    | AtomicRmwBinOp::AtomicAdd
                    | AtomicRmwBinOp::AtomicSub
                    | AtomicRmwBinOp::AtomicAnd
                    | AtomicRmwBinOp::AtomicOr
                    | AtomicRmwBinOp::AtomicXor
            );
        let op_mode = if use_c11 { AtomicBackendMode::C11 } else { AtomicBackendMode::Builtin };
        self.ensure_c11_atomic_include(op_mode);

        if is_ptr
            && matches!(
                op,
                AtomicRmwBinOp::AtomicMax
                    | AtomicRmwBinOp::AtomicMin
                    | AtomicRmwBinOp::AtomicUMax
                    | AtomicRmwBinOp::AtomicUMin
            )
        {
            self.atomic_fatal(
                "atomic_rmw max/min operations are not supported for pointer atomics",
            );
        }

        let addr = self.atomic_addr_expr(dst, op_ty, op_mode, false);
        let rhs = self.atomic_value_expr(src, op_ty);
        let order = self.atomic_order_expr(order, op_mode);
        let old_storage = self.bb.func.0.next_local_var();
        let call = match (op_mode, op) {
            (AtomicBackendMode::C11, AtomicRmwBinOp::AtomicXchg) => self.mcx.call(
                self.mcx.value(CValue::Func("atomic_exchange_explicit")),
                vec![addr, rhs, order],
            ),
            (AtomicBackendMode::C11, AtomicRmwBinOp::AtomicAdd) => self.mcx.call(
                self.mcx.value(CValue::Func("atomic_fetch_add_explicit")),
                vec![addr, rhs, order],
            ),
            (AtomicBackendMode::C11, AtomicRmwBinOp::AtomicSub) => self.mcx.call(
                self.mcx.value(CValue::Func("atomic_fetch_sub_explicit")),
                vec![addr, rhs, order],
            ),
            (AtomicBackendMode::C11, AtomicRmwBinOp::AtomicAnd) => self.mcx.call(
                self.mcx.value(CValue::Func("atomic_fetch_and_explicit")),
                vec![addr, rhs, order],
            ),
            (AtomicBackendMode::C11, AtomicRmwBinOp::AtomicOr) => self.mcx.call(
                self.mcx.value(CValue::Func("atomic_fetch_or_explicit")),
                vec![addr, rhs, order],
            ),
            (AtomicBackendMode::C11, AtomicRmwBinOp::AtomicXor) => self.mcx.call(
                self.mcx.value(CValue::Func("atomic_fetch_xor_explicit")),
                vec![addr, rhs, order],
            ),
            (AtomicBackendMode::Builtin, AtomicRmwBinOp::AtomicXchg) => {
                self.mcx.call(self.mcx.raw("__atomic_exchange_n"), vec![addr, rhs, order])
            }
            (AtomicBackendMode::Builtin, AtomicRmwBinOp::AtomicAdd) => {
                self.mcx.call(self.mcx.raw("__atomic_fetch_add"), vec![addr, rhs, order])
            }
            (AtomicBackendMode::Builtin, AtomicRmwBinOp::AtomicSub) => {
                self.mcx.call(self.mcx.raw("__atomic_fetch_sub"), vec![addr, rhs, order])
            }
            (AtomicBackendMode::Builtin, AtomicRmwBinOp::AtomicAnd) => {
                self.mcx.call(self.mcx.raw("__atomic_fetch_and"), vec![addr, rhs, order])
            }
            (AtomicBackendMode::Builtin, AtomicRmwBinOp::AtomicNand) => {
                self.mcx.call(self.mcx.raw("__atomic_fetch_nand"), vec![addr, rhs, order])
            }
            (AtomicBackendMode::Builtin, AtomicRmwBinOp::AtomicOr) => {
                self.mcx.call(self.mcx.raw("__atomic_fetch_or"), vec![addr, rhs, order])
            }
            (AtomicBackendMode::Builtin, AtomicRmwBinOp::AtomicXor) => {
                self.mcx.call(self.mcx.raw("__atomic_fetch_xor"), vec![addr, rhs, order])
            }
            (
                AtomicBackendMode::Builtin,
                AtomicRmwBinOp::AtomicMax | AtomicRmwBinOp::AtomicUMax,
            ) => self.mcx.call(self.mcx.raw("__atomic_fetch_max"), vec![addr, rhs, order]),
            (
                AtomicBackendMode::Builtin,
                AtomicRmwBinOp::AtomicMin | AtomicRmwBinOp::AtomicUMin,
            ) => self.mcx.call(self.mcx.raw("__atomic_fetch_min"), vec![addr, rhs, order]),
            _ => self.atomic_fatal("unsupported C11 atomic_rmw operation"),
        };
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(old_storage, op_ty, Some(call))));
        self.record_value_ty(old_storage, op_ty);

        if is_ptr && ret_ptr {
            let ret = self.bb.func.0.next_local_var();
            let cast = self.mcx.cast(mem_ty, self.mcx.value(old_storage));
            self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, mem_ty, Some(cast))));
            self.record_value_ty(ret, mem_ty);
            return ret;
        }

        if !is_ptr && mem_ty != op_ty {
            let ret = self.bb.func.0.next_local_var();
            let cast = self.mcx.cast(mem_ty, self.mcx.value(old_storage));
            self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, mem_ty, Some(cast))));
            self.record_value_ty(ret, mem_ty);
            return ret;
        }

        old_storage
    }

    fn atomic_fence(
        &mut self,
        order: rustc_middle::ty::AtomicOrdering,
        scope: rustc_codegen_ssa::common::SynchronizationScope,
    ) {
        let mode = self.atomic_mode();
        self.ensure_c11_atomic_include(mode);
        let order = self.atomic_order_expr(order, mode);
        let callee = match (mode, scope) {
            (AtomicBackendMode::C11, SynchronizationScope::SingleThread) => {
                self.mcx.value(CValue::Func("atomic_signal_fence"))
            }
            (AtomicBackendMode::C11, SynchronizationScope::CrossThread) => {
                self.mcx.value(CValue::Func("atomic_thread_fence"))
            }
            (AtomicBackendMode::Builtin, SynchronizationScope::SingleThread) => {
                self.mcx.raw("__atomic_signal_fence")
            }
            (AtomicBackendMode::Builtin, SynchronizationScope::CrossThread) => {
                self.mcx.raw("__atomic_thread_fence")
            }
        };
        let call = self.mcx.call(callee, vec![order]);
        self.bb.func.0.push_stmt(self.mcx.expr_stmt(call));
    }

    fn set_invariant_load(&mut self, load: Self::Value) {
        let _ = load;
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
        if let Some(fn_abi) = fn_abi {
            for value in args.iter().copied() {
                self.mark_alloca_prefer_bytes(value);
            }
            if matches!(fn_abi.ret.mode, PassMode::Indirect { .. }) {
                if let Some(ret_ptr) = args.first().copied() {
                    self.ensure_alloca_decl(ret_ptr, Some(self.cx.backend_type(fn_abi.ret.layout)));
                }
            }
            let arg_start =
                if matches!(fn_abi.ret.mode, PassMode::Indirect { .. }) { 1 } else { 0 };
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
            return match fn_abi.ret.mode {
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
            };
        }

        let (ret_ty, expected_callee_ty) = self.call_signature_from_ty(llty);
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

        let callee_expr = match callee {
            CValue::Func(_) => self.mcx.value(callee),
            _ => self.mcx.cast(expected_callee_ty, self.mcx.value(callee)),
        };
        let call = self.mcx.call(callee_expr, args);
        if ret_ty == CTy::Void {
            self.bb.func.0.push_stmt(self.mcx.expr_stmt(call));
            CValue::Scalar(0)
        } else {
            let ret = self.bb.func.0.next_local_var();
            self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ret_ty, Some(call))));
            self.record_value_ty(ret, ret_ty);
            ret
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
        let call = self.call(llty, fn_attrs, Some(fn_abi), llfn, args, funclet, instance);
        match fn_abi.ret.mode {
            PassMode::Ignore | PassMode::Indirect { .. } => self.ret_void(),
            PassMode::Direct(_) | PassMode::Pair(_, _) | PassMode::Cast { .. } => self.ret(call),
        }
    }

    fn zext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        self.intcast(val, dest_ty, false)
    }

    fn apply_attrs_to_cleanup_callsite(&mut self, llret: Self::Value) {
        // C backend does not model callsite attributes; cleanup-callsite attrs are
        // optimization hints and can be ignored.
        let _ = llret;
    }
}

#[cfg(test)]
mod tests {
    use super::{
        map_order_builtin, map_order_c11, select_atomic_mode, validate_cmpxchg_order,
        AtomicBackendMode, ATOMIC_CMPXCHG_INVALID_ORDER_MSG,
    };
    use crate::config::CStandard;
    use rustc_middle::ty::AtomicOrdering;

    #[test]
    fn select_atomic_mode_prefers_builtin_for_c99_variants() {
        assert_eq!(select_atomic_mode(CStandard::C99), AtomicBackendMode::Builtin);
        assert_eq!(select_atomic_mode(CStandard::Gnu99), AtomicBackendMode::Builtin);
    }

    #[test]
    fn select_atomic_mode_prefers_c11_for_c11_and_newer() {
        assert_eq!(select_atomic_mode(CStandard::C11), AtomicBackendMode::C11);
        assert_eq!(select_atomic_mode(CStandard::Gnu11), AtomicBackendMode::C11);
        assert_eq!(select_atomic_mode(CStandard::C23), AtomicBackendMode::C11);
        assert_eq!(select_atomic_mode(CStandard::Gnu23), AtomicBackendMode::C11);
    }

    #[test]
    fn validate_cmpxchg_order_accepts_acquire_relaxed() {
        assert!(validate_cmpxchg_order(AtomicOrdering::Acquire, AtomicOrdering::Relaxed).is_ok());
    }

    #[test]
    fn validate_cmpxchg_order_rejects_failure_release() {
        assert_eq!(
            validate_cmpxchg_order(AtomicOrdering::SeqCst, AtomicOrdering::Release).unwrap_err(),
            ATOMIC_CMPXCHG_INVALID_ORDER_MSG
        );
    }

    #[test]
    fn validate_cmpxchg_order_accepts_release_acquire_pair() {
        assert!(validate_cmpxchg_order(AtomicOrdering::Release, AtomicOrdering::Acquire).is_ok());
    }

    #[test]
    fn ordering_mapping_matches_expected_symbols() {
        assert_eq!(map_order_c11(AtomicOrdering::Relaxed), "memory_order_relaxed");
        assert_eq!(map_order_c11(AtomicOrdering::SeqCst), "memory_order_seq_cst");
        assert_eq!(map_order_builtin(AtomicOrdering::Acquire), "__ATOMIC_ACQUIRE");
        assert_eq!(map_order_builtin(AtomicOrdering::AcqRel), "__ATOMIC_ACQ_REL");
    }
}
