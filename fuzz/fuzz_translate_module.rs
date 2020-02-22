#![cfg_attr(feature = "libfuzzer", no_main)]

#[cfg(not(any(feature = "libfuzzer", feature = "use_afl")))]
compile_error!("Please enable the libfuzzer of use_afl feature");

#[macro_use]
#[cfg(feature = "use_afl")]
extern crate afl;

#[macro_use]
#[cfg(feature = "libfuzzer")]
extern crate libfuzzer_sys;

extern crate binaryen;
extern crate cranelift_codegen;
extern crate cranelift_wasm;
#[macro_use]
extern crate target_lexicon;

use cranelift_codegen::{isa, settings};
use cranelift_wasm::{translate_module, DummyEnvironment, ReturnMode};
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
    let binaryen_module = binaryen::tools::translate_to_fuzz_mvp(data);

    let wasm = binaryen_module.write();

    let flags = settings::Flags::new(settings::builder());
    let triple = triple!("x86_64");
    let isa = isa::lookup(triple).unwrap().finish(flags);
    let mut dummy_environ = DummyEnvironment::new(isa.frontend_config(), ReturnMode::NormalReturns, false);
    translate_module(&wasm, &mut dummy_environ).unwrap();

    let mut context = cranelift_codegen::Context::new();
    for func in dummy_environ.info.function_bodies.values() {
        context.clear();
        context.func = func.clone();
        context.compile(&*isa).unwrap();
    }
}
