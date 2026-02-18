//! Ensure default include set is minimal, stable, and deduplicated.

//@ run-pass
//@ exit-code: 0

// CHECK: #include <stddef.h>
// CHECK-NEXT: #include <stdint.h>
// CHECK-NEXT: #include <stdlib.h>
// CHECK-NOT: #include <stdio.h>
// CHECK-NOT: #include <stddef.h>
// CHECK-NOT: #include <stdint.h>
// CHECK-NOT: #include <stdlib.h>
fn main() {}
