#![cfg_attr(feature = "libfuzzer", no_main)]

#[cfg(not(any(feature = "libfuzzer", feature = "use_afl")))]
compile_error!("Please enable the libfuzzer of use_afl feature");

#[macro_use]
#[cfg(feature = "use_afl")]
extern crate afl;

#[macro_use]
#[cfg(feature = "libfuzzer")]
extern crate libfuzzer_sys;

extern crate cranelift_reader;
extern crate cranelift_codegen;
#[macro_use]
extern crate target_lexicon;

use cranelift_codegen::{isa, settings};
use cranelift_wasm::{translate_module, DummyEnvironment, ReturnMode};

use std::str;
use std::str::FromStr;

#[cfg(feature = "libfuzzer")]
fuzz_target!(|data: &[u8]| {
    fuzz(data);
});

#[cfg(feature = "use_afl")]
fn main() {
    fuzz!(|data: &[u8]| {
        fuzz(data);
    });
}

fn fuzz(data: &[u8]) {
    if let Ok(s) = str::from_utf8(data) {
        let options = cranelift_reader::ParseOptions::default();
        let test = match cranelift_reader::parse_test(s, options) {
            Ok(test) => test,
            _ => return,
        };

        let flags = settings::Flags::new(settings::builder());
        let triple = triple!("x86_64");
        let isa = isa::lookup(triple).unwrap().finish(flags);

        let mut context = cranelift_codegen::Context::new();
        for (func, _) in test.functions {
            context.clear();
            context.func = func;
            let _ = context.compile(&*isa).unwrap();
        }
    }
}
