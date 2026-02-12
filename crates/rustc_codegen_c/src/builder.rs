#![allow(unused_variables)] // TODO

use std::ops::Deref;

use rustc_abi::{HasDataLayout, TargetDataLayout};
use rustc_codegen_c_ast::expr::CValue;
use rustc_codegen_c_ast::ty::{CTy, CTyKind, CUintTy};
use rustc_codegen_ssa::traits::{BackendTypes, BuilderMethods, ConstCodegenMethods};
use rustc_data_structures::intern::Interned;
use rustc_hash::FxHashMap;
use rustc_middle::ty;
use rustc_middle::ty::layout::{
    FnAbiError, FnAbiOfHelpers, FnAbiRequest, HasTyCtxt, HasTypingEnv, LayoutError,
    LayoutOfHelpers, TyAndLayout,
};
use rustc_middle::ty::{Ty, TyCtxt};
use rustc_target::callconv::FnAbi;
use rustc_target::spec::{HasTargetSpec, Target};

use crate::context::{CBasicBlock, CodegenCx};

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
    fn record_value_ty(&mut self, value: CValue<'mx>, ty: CTy<'mx>) {
        if !matches!(value, CValue::Scalar(_)) {
            self.value_tys.insert(value, ty);
        }
    }

    fn value_ty(&self, value: CValue<'mx>) -> Option<CTy<'mx>> {
        match value {
            CValue::Scalar(_) => None,
            _ => self.value_tys.get(&value).copied(),
        }
    }

    fn infer_integer_binop_ty(&self, lhs: CValue<'mx>, rhs: CValue<'mx>, op: &str) -> CTy<'mx> {
        let ty = match (self.value_ty(lhs), self.value_ty(rhs)) {
            (Some(lhs), Some(rhs)) if lhs == rhs => lhs,
            (Some(lhs), Some(rhs)) => panic!("type mismatch for {op}: {lhs:?} vs {rhs:?}"),
            (Some(lhs), None) => lhs,
            (None, Some(rhs)) => rhs,
            (None, None) => CTy::UInt(CUintTy::Usize),
        };

        match ty {
            CTy::Int(_) | CTy::UInt(_) => ty,
            _ => panic!("unsupported type for {op}: {ty:?}"),
        }
    }

    fn require_const_usize(&self, value: CValue<'mx>, op: &str) -> usize {
        match self.cx.const_to_opt_uint(value) {
            Some(i) => i as usize,
            None => panic!("{op} only supports constant unsigned indices"),
        }
    }

    fn pointer_to(&self, pointee: CTy<'mx>) -> CTy<'mx> {
        CTy::Ref(Interned::new_unchecked(self.mcx.arena().alloc(CTyKind::Pointer(pointee))))
    }

    fn store_ty_from_align(&self, align: rustc_abi::Align) -> CTy<'mx> {
        match align.bytes() {
            1 => CTy::UInt(CUintTy::U8),
            2 => CTy::UInt(CUintTy::U16),
            4 => CTy::UInt(CUintTy::U32),
            8 => CTy::UInt(CUintTy::U64),
            _ => CTy::UInt(CUintTy::U8),
        }
    }
}

impl<'a, 'tcx, 'mx> BuilderMethods<'a, 'tcx> for Builder<'a, 'tcx, 'mx> {
    type CodegenCx = CodegenCx<'tcx, 'mx>;
    fn build(cx: &'a Self::CodegenCx, llbb: Self::BasicBlock) -> Self {
        let mut value_tys = FxHashMap::default();
        for (ty, value) in llbb.func.0.params.iter() {
            value_tys.insert(*value, *ty);
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
        let ty = self.infer_integer_binop_ty(lhs, rhs, "add");
        let ret = self.bb.func.0.next_local_var();
        let expr = self.mcx.binary(self.mcx.value(lhs), self.mcx.value(rhs), "+");
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
        let ty = self.infer_integer_binop_ty(lhs, rhs, "sub");
        let ret = self.bb.func.0.next_local_var();
        let expr = self.mcx.binary(self.mcx.value(lhs), self.mcx.value(rhs), "-");
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
        let ty = self.infer_integer_binop_ty(lhs, rhs, "mul");
        let ret = self.bb.func.0.next_local_var();
        let expr = self.mcx.binary(self.mcx.value(lhs), self.mcx.value(rhs), "*");
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
        let ty = self.infer_integer_binop_ty(lhs, rhs, "udiv");
        if !matches!(ty, CTy::UInt(_)) {
            panic!("unsupported type for udiv: {ty:?}");
        }
        let ret = self.bb.func.0.next_local_var();
        let expr = self.mcx.binary(self.mcx.value(lhs), self.mcx.value(rhs), "/");
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn exactudiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn sdiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        let ty = self.infer_integer_binop_ty(lhs, rhs, "sdiv");
        if !matches!(ty, CTy::Int(_)) {
            panic!("unsupported type for sdiv: {ty:?}");
        }
        let ret = self.bb.func.0.next_local_var();
        let expr = self.mcx.binary(self.mcx.value(lhs), self.mcx.value(rhs), "/");
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, Some(expr))));
        self.record_value_ty(ret, ty);
        ret
    }

    fn exactsdiv(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
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
        todo!()
    }

    fn lshr(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn ashr(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_sadd(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_uadd(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_ssub(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_usub(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_smul(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn unchecked_umul(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn and(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn or(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn xor(&mut self, lhs: Self::Value, rhs: Self::Value) -> Self::Value {
        todo!()
    }

    fn neg(&mut self, v: Self::Value) -> Self::Value {
        todo!()
    }

    fn fneg(&mut self, v: Self::Value) -> Self::Value {
        todo!()
    }

    fn not(&mut self, v: Self::Value) -> Self::Value {
        todo!()
    }

    fn checked_binop(
        &mut self,
        oop: rustc_codegen_ssa::traits::OverflowOp,
        ty: rustc_middle::ty::Ty<'_>,
        lhs: Self::Value,
        rhs: Self::Value,
    ) -> (Self::Value, Self::Value) {
        todo!()
    }

    fn from_immediate(&mut self, val: Self::Value) -> Self::Value {
        val
    }

    fn to_immediate_scalar(&mut self, val: Self::Value, scalar: rustc_abi::Scalar) -> Self::Value {
        val
    }

    fn alloca(&mut self, size: rustc_abi::Size, align: rustc_abi::Align) -> Self::Value {
        let ret = self.bb.func.0.next_local_var();
        let bytes = size.bytes_usize();
        let ty = CTy::Ref(Interned::new_unchecked(
            self.mcx.arena().alloc(CTyKind::Array(CTy::UInt(CUintTy::U8), bytes)),
        ));
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ty, None)));
        self.record_value_ty(ret, ty);
        ret
    }

    fn load(&mut self, ty: Self::Type, ptr: Self::Value, align: rustc_abi::Align) -> Self::Value {
        let cast_ptr = self.mcx.cast(self.pointer_to(ty), self.mcx.value(ptr));
        let expr = self.mcx.unary("*", cast_ptr);

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
        let store_ty = self
            .value_ty(val)
            .or_else(|| match self.value_ty(ptr) {
                Some(CTy::Ref(kind)) => match kind.0 {
                    CTyKind::Pointer(elem) => Some(*elem),
                    _ => None,
                },
                _ => None,
            })
            .unwrap_or_else(|| self.store_ty_from_align(align));
        let cast_ptr = self.mcx.cast(self.pointer_to(store_ty), self.mcx.value(ptr));
        let lhs = self.mcx.unary("*", cast_ptr);
        let assign = self.mcx.binary(lhs, self.mcx.value(val), "=");
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
        let mut projected_ty = ty;
        let mut projected = false;

        for (i, index) in indices.iter().enumerate() {
            let index = self.require_const_usize(*index, "gep");

            if i == 0 && index == 0 {
                continue;
            }
            projected = true;

            projected_ty = match projected_ty {
                CTy::Ref(kind) => match kind.0 {
                    CTyKind::Array(elem, _) => {
                        expr = self.mcx.index(expr, self.mcx.value(CValue::Scalar(index as i128)));
                        *elem
                    }
                    CTyKind::Pointer(elem) => {
                        expr = self.mcx.index(expr, self.mcx.value(CValue::Scalar(index as i128)));
                        *elem
                    }
                    CTyKind::Struct(_) => {
                        let fields = self
                            .cx
                            .struct_fields
                            .borrow()
                            .get(&projected_ty)
                            .cloned()
                            .unwrap_or_else(|| {
                                panic!("missing struct field metadata for {projected_ty:?}")
                            });
                        let (field, field_ty) = fields
                            .get(index)
                            .copied()
                            .unwrap_or_else(|| panic!("invalid struct field index {index}"));
                        expr = if i == 1 {
                            self.mcx.member_arrow(expr, field)
                        } else {
                            self.mcx.member(expr, field)
                        };
                        field_ty
                    }
                },
                _ => {
                    expr = self.mcx.index(expr, self.mcx.value(CValue::Scalar(index as i128)));
                    projected_ty
                }
            };
        }

        if !projected {
            return ptr;
        }

        let ret = self.bb.func.0.next_local_var();
        let ret_ty = self.pointer_to(projected_ty);
        let addr = self.mcx.unary("&", expr);
        self.bb.func.0.push_stmt(self.mcx.decl_stmt(self.mcx.var(ret, ret_ty, Some(addr))));
        self.record_value_ty(ret, ret_ty);
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
        todo!()
    }

    fn sext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
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
        todo!()
    }

    fn inttoptr(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn bitcast(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
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
        todo!()
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
        todo!()
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
        todo!()
    }

    fn memset(
        &mut self,
        ptr: Self::Value,
        fill_byte: Self::Value,
        size: Self::Value,
        align: rustc_abi::Align,
        flags: rustc_codegen_ssa::MemFlags,
    ) {
        todo!()
    }

    fn select(
        &mut self,
        cond: Self::Value,
        then_val: Self::Value,
        else_val: Self::Value,
    ) -> Self::Value {
        todo!()
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
        todo!()
    }

    fn insert_value(&mut self, agg_val: Self::Value, elt: Self::Value, idx: u64) -> Self::Value {
        todo!()
    }

    fn set_personality_fn(&mut self, personality: Self::Function) {
        todo!()
    }

    fn cleanup_landing_pad(&mut self, pers_fn: Self::Function) -> (Self::Value, Self::Value) {
        todo!()
    }

    fn filter_landing_pad(&mut self, pers_fn: Self::Function) {
        todo!()
    }

    fn resume(&mut self, exn0: Self::Value, exn1: Self::Value) {
        todo!()
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
        use crate::rustc_codegen_ssa::traits::LayoutTypeCodegenMethods;

        let fn_abi = fn_abi.unwrap();
        let ret_ty = self.cx.immediate_backend_type(fn_abi.ret.layout);

        let mut args = args.iter().map(|v| self.mcx.value(*v)).collect::<Vec<_>>();
        let mut callee = llfn;
        if let CValue::Func(name) = llfn {
            if name.contains("panic_bounds_check") {
                callee = CValue::Func("__rust_panic_bounds_check");
                if args.len() > 2 {
                    args.truncate(2);
                }
            }
        }

        let call = self.mcx.call(self.mcx.value(callee), args);
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
        todo!()
    }

    fn zext(&mut self, val: Self::Value, dest_ty: Self::Type) -> Self::Value {
        todo!()
    }

    fn apply_attrs_to_cleanup_callsite(&mut self, llret: Self::Value) {
        todo!()
    }
}
