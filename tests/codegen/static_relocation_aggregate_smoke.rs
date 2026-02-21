//! Regression test for aggregate statics that contain relocations.

//@ run-pass
//@ exit-code: 0

enum Input {
    Keyword(&'static str),
    Otherwise,
}

enum Action {
    SetState(&'static [(Input, Action)]),
    Finish,
}

static INIT: [(Input, Action); 2] = [
    (Input::Keyword("break"), Action::SetState(&BREAK_LABEL)),
    (Input::Otherwise, Action::Finish),
];

static BREAK_LABEL: [(Input, Action); 1] = [(Input::Otherwise, Action::Finish)];

#[inline(never)]
fn break_keyword() -> &'static str {
    match INIT[0].0 {
        Input::Keyword(s) => s,
        Input::Otherwise => "",
    }
}

#[inline(never)]
fn break_state_len() -> usize {
    match INIT[0].1 {
        Action::SetState(state) => state.len(),
        Action::Finish => 0,
    }
}

// CHECK-LABEL: main
fn main() {
    assert_eq!(break_keyword(), "break");
    assert_eq!(break_state_len(), 1);
}
