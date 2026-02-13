use std::fmt::Write;

use rustc_ast::expand::allocator::{
    alloc_error_handler_name, default_fn_name, global_fn_name, AllocatorKind, AllocatorMethod,
    AllocatorTy, ALLOCATOR_METHODS, NO_ALLOC_SHIM_IS_UNSTABLE,
};
use rustc_middle::ty::TyCtxt;
use rustc_session::config::OomStrategy;
use rustc_symbol_mangling::mangle_internal_symbol;

pub(crate) fn codegen(
    tcx: TyCtxt<'_>,
    _module_name: &str,
    kind: AllocatorKind,
    alloc_error_handler_kind: AllocatorKind,
) -> String {
    let mut code = String::new();
    code.push_str("#include <stdint.h>\n");
    code.push_str("\n");

    if kind == AllocatorKind::Default {
        for method in ALLOCATOR_METHODS {
            let from_name = mangle_internal_symbol(tcx, &global_fn_name(method.name));
            let to_name = mangle_internal_symbol(tcx, &default_fn_name(method.name));
            emit_allocator_wrapper(&mut code, method, &from_name, &to_name);
        }
    }

    let alloc_error_handler_name =
        mangle_internal_symbol(tcx, alloc_error_handler_name(alloc_error_handler_kind));
    emit_simple_wrapper(
        &mut code,
        "void",
        &mangle_internal_symbol(tcx, "__rust_alloc_error_handler"),
        Some(&alloc_error_handler_name),
        &[("uintptr_t", "size"), ("uintptr_t", "align")],
    );

    emit_const_value_fn(
        &mut code,
        "uint8_t",
        &mangle_internal_symbol(tcx, OomStrategy::SYMBOL),
        if tcx.sess.opts.unstable_opts.oom.should_panic() != 0 { "1" } else { "0" },
    );

    emit_simple_wrapper(
        &mut code,
        "void",
        &mangle_internal_symbol(tcx, NO_ALLOC_SHIM_IS_UNSTABLE),
        None,
        &[],
    );

    code
}

fn emit_allocator_wrapper(
    out: &mut String,
    method: &AllocatorMethod,
    from_name: &str,
    to_name: &str,
) {
    let mut args: Vec<(String, String)> = Vec::new();
    let mut call_args = Vec::new();
    let mut layout_count = 0usize;

    for input in method.inputs {
        match input.ty {
            AllocatorTy::Layout => {
                let suffix =
                    if layout_count == 0 { String::new() } else { (layout_count + 1).to_string() };
                let size_name = format!("size{suffix}");
                let align_name = format!("align{suffix}");
                args.push(("uintptr_t".to_string(), size_name.clone()));
                args.push(("uintptr_t".to_string(), align_name.clone()));
                call_args.push(size_name);
                call_args.push(align_name);
                layout_count += 1;
            }
            AllocatorTy::Ptr => {
                args.push(("uint8_t *".to_string(), input.name.to_string()));
                call_args.push(input.name.to_string());
            }
            AllocatorTy::Usize => {
                args.push(("uintptr_t".to_string(), input.name.to_string()));
                call_args.push(input.name.to_string());
            }
            AllocatorTy::ResultPtr | AllocatorTy::Unit => panic!("invalid allocator arg"),
        }
    }

    let ret_ty = match method.output {
        AllocatorTy::ResultPtr => "uint8_t *",
        AllocatorTy::Unit => "void",
        AllocatorTy::Layout | AllocatorTy::Usize | AllocatorTy::Ptr => {
            panic!("invalid allocator output")
        }
    };

    emit_wrapper_with_call_args(out, ret_ty, from_name, Some(to_name), &args, &call_args);
}

fn emit_simple_wrapper(
    out: &mut String,
    ret_ty: &str,
    from_name: &str,
    to_name: Option<&str>,
    args: &[(&str, &str)],
) {
    let args =
        args.iter().map(|(ty, name)| ((*ty).to_string(), (*name).to_string())).collect::<Vec<_>>();
    let call_args = args.iter().map(|(_, name)| name.clone()).collect::<Vec<_>>();
    emit_wrapper_with_call_args(out, ret_ty, from_name, to_name, &args, &call_args);
}

fn emit_wrapper_with_call_args(
    out: &mut String,
    ret_ty: &str,
    from_name: &str,
    to_name: Option<&str>,
    args: &[(String, String)],
    call_args: &[String],
) {
    let arg_decl =
        args.iter().map(|(ty, name)| format!("{ty} {name}")).collect::<Vec<_>>().join(", ");
    let arg_decl = if arg_decl.is_empty() { "void" } else { &arg_decl };
    let call_args = call_args.iter().map(String::as_str).collect::<Vec<_>>().join(", ");

    if let Some(to_name) = to_name {
        let _ = writeln!(out, "extern {ret_ty} {to_name}({arg_decl});");
    }
    let _ = writeln!(out, "{ret_ty} {from_name}({arg_decl}) {{");
    if let Some(to_name) = to_name {
        if ret_ty == "void" {
            let _ = writeln!(out, "    {to_name}({call_args});");
        } else {
            let _ = writeln!(out, "    return {to_name}({call_args});");
        }
    }
    let _ = writeln!(out, "}}\n");
}

fn emit_const_value_fn(out: &mut String, ret_ty: &str, fn_name: &str, value_expr: &str) {
    let _ = writeln!(out, "{ret_ty} {fn_name}(void) {{");
    let _ = writeln!(out, "    return {value_expr};");
    let _ = writeln!(out, "}}\n");
}
