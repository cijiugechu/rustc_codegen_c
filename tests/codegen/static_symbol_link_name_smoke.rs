//! Ensure static symbols with non-C identifiers keep object symbol names via __asm__.

//@ run-pass
//@ exit-code: 0

#[export_name = "rcgenc$static$link$name"]
pub static EXPORTED: u8 = 7;

// CHECK: __rcgenc_rcgenc_24static_24link_24name
// CHECK: __asm__("{{_?rcgenc[$]static[$]link[$]name}}")
fn main() {
    let value = EXPORTED;
    if value != 7 {
        std::process::exit(1);
    }
}
