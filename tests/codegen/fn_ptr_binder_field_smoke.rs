//! Regression test for struct fields containing HRTB function pointers.

//@ run-pass
//@ exit-code: 0

// CHECK: fn_ptr_binder_field_smoke

type Pred = for<'a, 'b> fn(&'a &'b u8) -> bool;

#[repr(C)]
struct Holder {
    pred: Pred,
}

#[inline(never)]
fn eval(holder: Holder, x: &u8) -> bool {
    let nested = &x;
    (holder.pred)(nested)
}

fn is_nonzero(v: &&u8) -> bool {
    **v != 0
}

fn main() {
    let one = 1u8;
    let zero = 0u8;
    if !eval(Holder { pred: is_nonzero }, &one) {
        std::process::exit(1);
    }
    if eval(Holder { pred: is_nonzero }, &zero) {
        std::process::exit(2);
    }
}
