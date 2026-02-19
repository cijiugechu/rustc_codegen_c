//@ run-pass
//@ exit-code: 0

use std::hint::black_box;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

// CHECK-LABEL: main
fn main() {
    let now = SystemTime::now();
    let _since_epoch = now
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::from_secs(0));

    let start = Instant::now();
    let mut acc = 0u64;
    for i in 0..black_box(1000u64) {
        acc = acc.wrapping_add(i);
    }
    let elapsed = start.elapsed();
    assert!(elapsed >= Duration::from_nanos(0));

    let d = Duration::from_millis(black_box(1500u64));
    assert_eq!(d.subsec_millis(), 500);
    assert!(acc > 0);
}
