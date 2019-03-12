use crate::cdsl::cpu_modes::CpuMode;
use crate::cdsl::inst::InstructionGroup;
use crate::cdsl::regs::IsaRegs;
use crate::cdsl::settings::SettingGroup;
use crate::cdsl::xform::{XFormGroupIndex, XFormGroups};

use std::collections::HashSet;
use std::iter::FromIterator;

pub struct TargetIsa {
    pub name: &'static str,
    pub instructions: InstructionGroup,
    pub settings: SettingGroup,
    pub regs: IsaRegs,
    pub cpu_modes: Vec<CpuMode>,
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
