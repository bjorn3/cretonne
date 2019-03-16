use std::collections::HashMap;

use crate::cdsl::regs::RegClassIndex;
use crate::cdsl::isa::{EncRecipe, OperandConstraint};
use crate::cdsl::formats::{FormatRegistry, InstructionFormat};
use crate::cdsl::settings::PredicateNode;
use super::RegIndexes;

const OPCODE_PREFIX: &[(&[u8], (&'static str, u16))] = &[
        // Prefix bytes       Name     mmpp
        (&[],                 ("Op1", 0b0000)),
        (&[0x66],             ("Mp1", 0b0001)),
        (&[0xf3],             ("Mp1", 0b0010)),
        (&[0xf2],             ("Mp1", 0b0011)),
        (&[0x0f],             ("Op2", 0b0100)),
        (&[0x66, 0x0f],       ("Mp2", 0b0101)),
        (&[0xf3, 0x0f],       ("Mp2", 0b0110)),
        (&[0xf2, 0x0f],       ("Mp2", 0b0111)),
        (&[0x0f, 0x38],       ("Op3", 0b1000)),
        (&[0x66, 0x0f, 0x38], ("Mp3", 0b1001)),
        (&[0xf3, 0x0f, 0x38], ("Mp3", 0b1010)),
        (&[0xf2, 0x0f, 0x38], ("Mp3", 0b1011)),
        (&[0x0f, 0x3a],       ("Op3", 0b1100)),
        (&[0x66, 0x0f, 0x3a], ("Mp3", 0b1101)),
        (&[0xf3, 0x0f, 0x3a], ("Mp3", 0b1110)),
        (&[0xf2, 0x0f, 0x3a], ("Mp3", 0b1111)),
];

/// Given a sequence of opcode bytes, compute the recipe name prefix and
/// encoding bits.
fn decode_ops(ops: &[u8], rrr: u8, w: u8) -> (&'static str, u16) {
    assert!(rrr <= 0b111);
    assert!(w <= 1);
    let (_prefix, (name, mmpp)) = OPCODE_PREFIX.iter().find(|(prefix, _)| prefix == &&ops[0 .. ops.len() - 1]).unwrap();
    let &op = ops.last().unwrap();
    (
        name,
        op as u16 | (mmpp << 8) | ((rrr as u16) << 12) | ((w as u16) << 15)
    )
}

/// Given a snippet of Rust code (or None), replace the `PUT_OP` macro with the
/// corresponding `put_*` function from the `binemit.rs` module.
fn replace_put_op(emit: Option<&str>, prefix: &str) -> Option<String> {
    emit.map(|emit| {
        emit.replace("PUT_OP", &("put_".to_owned() + &prefix.to_lowercase()))
    })
}

fn map_regs_norex(reg_indexes: RegIndexes, regs: &[OperandConstraint]) -> Vec<OperandConstraint> {
    regs.iter().map(|rc| {
        match *rc {
            OperandConstraint::RegClass(rc) => {
                if rc == reg_indexes.gpr {
                    OperandConstraint::RegClass(reg_indexes.gpr8)
                } else if rc == reg_indexes.fpr {
                    OperandConstraint::RegClass(reg_indexes.fpr8)
                } else {
                    panic!("map_regs_norex({:?})", rc);
                }
            }
            _ => *rc,
        }
    }).collect()
}

/// Generate encoding recipes on demand.
///
/// x86 encodings are somewhat orthogonal with the opcode representation on
/// one side and the ModR/M, SIB and immediate fields on the other side.
///
/// A `TailRecipe` represents the part of an encoding that follow the opcode.
/// It is used to generate full encoding recipes on demand when combined with
/// an opcode.
///
/// The arguments are the same as for an `EncRecipe`, except for `size` which
/// does not include the size of the opcode.
///
/// The `when_prefixed` parameter specifies a recipe that should be substituted
/// for this one when a REX (or VEX) prefix is present. This is relevant for
/// recipes that can only access the ABCD registers without a REX prefix, but
/// are able to access all registers with a prefix.
///
/// The `requires_prefix` parameter indicates that the recipe can't be used
/// without a REX prefix.
///
/// The `emit` parameter contains Rust code to actually emit an encoding, like
/// `EncRecipe` does it. Additionally, the text `PUT_OP` is substituted with
/// the proper `put_*` function from the `x86/binemit.rs` module.
struct TailRecipe {
    name: &'static str,
    format: InstructionFormat,
    base_size: u64,
    ins: Vec<OperandConstraint>,
    outs: Vec<OperandConstraint>,
    branch_range: Option<u64>,
    clobbers_flags: bool,
    instp: PredicateNode,
    isap: PredicateNode,
    when_prefixed: Option<Box<TailRecipe>>,
    requires_prefix: bool,
    emit: Option<&'static str>,
    compute_size: Option<&'static str>,

    recipes: HashMap<String, EncRecipe>,
}

fn recipes(formats: &FormatRegistry, reg_indexes: RegIndexes) -> Vec<EncRecipe> {
    use crate::cdsl::isa::OperandConstraint::{RegClass, Register};

    let null = EncRecipe::new("null", formats, "Unary", 0, vec![RegClass(reg_indexes.gpr)], vec![]);
    let debugtrap = EncRecipe::new("debugtrap", formats, "NullAry", 1, vec![], vec![])
        .emit("sink.put1(0xcc);");

    let trapif = EncRecipe::new("trapif", formats, "IntCondTrap", 4, vec![Register(reg_indexes.rflags)], vec![])
        .emit("
        // Jump over a 2-byte ud2.
        sink.put1(0x70 | (icc2opc(cond.inverse()) as u8));
        sink.put1(2);
        // ud2.
        sink.trap(code, func.srclocs[inst]);
        sink.put1(0x0f);
        sink.put1(0x0b);
        ");

    // ...



    vec![]
}
