use crate::cdsl::cpu_modes::CpuMode;
use crate::cdsl::formats::{FormatRegistry, InstructionFormatIndex};
use crate::cdsl::inst::InstructionGroup;
use crate::cdsl::regs::{IsaRegs, RegClassIndex, Register, Stack};
use crate::cdsl::settings::{SettingGroup, PredicateNode};
use crate::cdsl::xform::{XFormGroupIndex, XFormGroups};

use std::collections::HashSet;
use std::iter::FromIterator;

pub struct TargetIsa {
    pub name: &'static str,
    pub instructions: InstructionGroup,
    pub settings: SettingGroup,
    pub regs: IsaRegs,
    pub cpu_modes: Vec<CpuMode>,

    pub all_recipes: Vec<EncRecipe>,
}

impl TargetIsa {
    pub fn new(
        name: &'static str,
        instructions: InstructionGroup,
        settings: SettingGroup,
        regs: IsaRegs,
        cpu_modes: Vec<CpuMode>,
    ) -> Self {
        Self {
            name,
            instructions,
            settings,
            regs,
            cpu_modes,

            all_recipes: vec![],
        }
    }

    /// Returns a deterministically ordered, deduplicated list of XFormGroupIndex for the
    /// transitive set of XFormGroup this TargetIsa uses.
    pub fn transitive_xform_groups(&self, all_groups: &XFormGroups) -> Vec<XFormGroupIndex> {
        let mut set = HashSet::new();
        for cpu_mode in &self.cpu_modes {
            set.extend(cpu_mode.transitive_xform_groups(all_groups));
        }
        let mut vec = Vec::from_iter(set);
        vec.sort();
        vec
    }

    /// Returns a deterministically ordered, deduplicated list of XFormGroupIndex for the directly
    /// reachable set of XFormGroup this TargetIsa uses.
    pub fn direct_xform_groups(&self) -> Vec<XFormGroupIndex> {
        let mut set = HashSet::new();
        for cpu_mode in &self.cpu_modes {
            set.extend(cpu_mode.direct_xform_groups());
        }

        let mut vec = Vec::from_iter(set);
        vec.sort();
        vec
    }
}

#[derive(Clone)]
pub struct BranchRange(Vec<u64>);

#[derive(Copy, Clone)]
pub enum OperandConstraint {
    RegClass(RegClassIndex),
    Register(Register),
    InputReference(usize), // FIXME what does it mean?
    Stack(Stack),
}

/// A recipe for encoding instructions with a given format.
///
/// Many different instructions can be encoded by the same recipe, but they
/// must all have the same instruction format.
#[derive(Clone)]
pub struct EncRecipe {
    pub name: String,
    pub format: InstructionFormatIndex,
    pub base_size: u64,
    pub ins: Vec<OperandConstraint>,
    pub outs: Vec<OperandConstraint>,

    // These have defaults
    pub compute_size: String,
    pub branch_range: Option<BranchRange>,
    pub clobbers_flags: bool,
    pub instp: Option<PredicateNode>,
    pub isap: Option<PredicateNode>,
    pub emit: Option<String>,
    pub number: Option<u64>,
}

impl EncRecipe {
    /// The `ins` and `outs` arguments are tuples specifying the register
    /// allocation constraints for the value operands and results respectively. The
    /// possible constraints for an operand are:
    ///
    /// - A `RegClass` specifying the set of allowed registers.
    /// - A `Register` specifying a fixed-register operand.
    /// - An integer indicating that this result is tied to a value operand, so
    ///   they must use the same register.
    /// - A `Stack` specifying a value in a stack slot.
    ///
    /// The `branch_range` argument must be provided for recipes that can encode
    /// branch instructions. It is an `(origin, bits)` tuple describing the exact
    /// range that can be encoded in a branch instruction.
    ///
    /// For ISAs that use CPU flags in `iflags` and `fflags` value types, the
    /// `clobbers_flags` is used to indicate instruction encodings that clobbers
    /// the CPU flags, so they can't be used where a flag value is live.
    ///
    /// :param name: Short mnemonic name for this recipe.
    /// :param format: All encoded instructions must have this
    ///         :py:class:`InstructionFormat`.
    /// :param base_size: Base number of bytes in the binary encoded instruction.
    /// :param compute_size: Function name to use when computing actual size.
    /// :param ins: Tuple of register constraints for value operands.
    /// :param outs: Tuple of register constraints for results.
    /// :param branch_range: `(origin, bits)` range for branches.
    /// :param clobbers_flags: This instruction clobbers `iflags` and `fflags`.
    /// :param instp: Instruction predicate.
    /// :param isap: ISA predicate.
    /// :param emit: Rust code for binary emission.
    pub fn new(
        name: &str,
        formats: &FormatRegistry,
        format_name: &str,
        base_size: u64,
        ins: Vec<OperandConstraint>,
        outs: Vec<OperandConstraint>,
    ) -> Self {
        let format = formats.lookup_by_name(format_name);
        if !formats.get(format).has_value_list {
            assert!(ins.len() == formats.get(format).num_value_operands);
        }

        Self::verify_constraints(&ins, &ins);
        Self::verify_constraints(&ins, &outs);

        Self {
            name: name.to_string(),
            format,
            base_size,
            ins,
            outs,

            compute_size: "base_size".to_string(),
            branch_range: None,
            clobbers_flags: false,
            instp: None,
            isap: None,
            emit: None,
            number: None,
        }
    }

    fn verify_constraints(in_constrains: &[OperandConstraint], constraints: &[OperandConstraint]) {
        for c in constraints {
            match *c {
                OperandConstraint::InputReference(n) => assert!(n < in_constrains.len()),
                _ => {}
            }
        }
    }

    pub fn compute_size(self, compute_size: &str) -> Self {
        Self {
            compute_size: compute_size.to_string(),
            ..self
        }
    }

    pub fn branch_range(self, branch_range: BranchRange) -> Self {
        Self {
            branch_range: Some(branch_range),
            ..self
        }
    }

    pub fn clobbers_flags(self) -> Self {
        Self {
            clobbers_flags: true,
            ..self
        }
    }

    pub fn instp(self, instp: PredicateNode) -> Self {
        Self {
            instp: Some(instp),
            ..self
        }
    }

    pub fn isap(self, isap: PredicateNode) -> Self {
        Self {
            isap: Some(isap),
            ..self
        }
    }

    pub fn emit(self, emit: &str) -> Self {
        Self {
            emit: Some(emit.to_string()),
            ..self
        }
    }
}
