//! Utility routines for pretty-printing error messages.

use ir;
use ir::entities::Inst;
use ir::function::Function;
use isa::TargetIsa;
use result::CodegenError;
use std::fmt;
use std::fmt::Write;
use std::string::{String, ToString};
use verifier::VerifierError;
use write::{decorate_function, FuncWriter, PlainWriter};

/// Pretty-print a verifier error.
pub fn pretty_verifier_error<'a>(
    func: &ir::Function,
    isa: Option<&TargetIsa>,
    func_w: Option<Box<FuncWriter + 'a>>,
    err: &VerifierError,
) -> String {
    let mut w = String::new();
    decorate_function(
        &mut PrettyVerifierError(func_w.unwrap_or(Box::new(PlainWriter)), err),
        &mut w,
        func,
        isa,
    ).unwrap();
    w
}

struct PrettyVerifierError<'a>(Box<FuncWriter + 'a>, &'a VerifierError);

impl<'a> FuncWriter for PrettyVerifierError<'a> {
    fn write_instruction(
        &mut self,
        w: &mut Write,
        func: &Function,
        isa: Option<&TargetIsa>,
        inst: Inst,
        indent: usize,
    ) -> fmt::Result {
        pretty_function_error(w, func, isa, inst, indent, &mut *self.0, self.1)
    }
}

/// Pretty-print a function verifier error.
fn pretty_function_error(
    w: &mut Write,
    func: &Function,
    isa: Option<&TargetIsa>,
    cur_inst: Inst,
    indent: usize,
    func_w: &mut FuncWriter,
    err: &VerifierError,
) -> fmt::Result {
    match err.location {
        ir::entities::AnyEntity::Inst(inst) => {
            func_w.write_instruction(w, func, isa, cur_inst, indent)?;
            if inst == cur_inst {
                write!(w, "{1:0$}{2}", indent, "", "^")?;
                for _c in cur_inst.to_string().chars() {
                    write!(w, "~")?;
                }
                write!(w, "\n\nverifier {}\n\n", err.to_string())
            } else {
                Ok(())
            }
        }
        _ => write!(w, "{}", "\n"),
    }
}

/// Pretty-print a Cranelift error.
pub fn pretty_error(func: &ir::Function, isa: Option<&TargetIsa>, err: CodegenError) -> String {
    if let CodegenError::Verifier(e) = err {
        pretty_verifier_error(func, isa, None, &e)
    } else {
        err.to_string()
    }
}
