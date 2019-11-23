use alloc::collections::BTreeMap;
use alloc::vec::Vec;
use core::cmp::Ordering;

use cranelift_entity::SparseMapValue;

use crate::cursor::{Cursor, EncCursor};
use crate::ir::{Function, ProgramOrder, ProgramPoint, Value};
use crate::isa::{RegUnit, TargetIsa};
use crate::regalloc::{Context, Liveness, LiveRange};

fn live_range_end(func: &Function, live_range: &LiveRange) -> ProgramPoint {
    let mut live_range_end = live_range.def_local_end();

    for (_livein_ebb, livein_inst) in live_range.liveins() {
        let livein = ProgramPoint::from(livein_inst);
        if func.layout.cmp(live_range_end, livein) == Ordering::Less {
            live_range_end = livein;
        }
    }

    live_range_end
}

fn insert_active<'a>(func: &Function, active: &mut Vec<&'a LiveRange>, i: &'a LiveRange) {
    let i_end = live_range_end(func, i);
    let insert_index = active
        .binary_search_by(|&probe| {
            func.layout.cmp(live_range_end(func, probe), i_end)
        })
        .unwrap_or_else(|insert_index| insert_index);
    active.insert(insert_index, i);
}

pub fn run(context: &mut Context, func: &mut Function, isa: &dyn TargetIsa) {
    let mut unused_regs: Vec<RegUnit> = Vec::new();
    for reg in isa.allocatable_registers(func).iter(isa.register_info().classes[0]) {
        unused_regs.push(reg);
    }
    let mut register_assignments: BTreeMap<Value, RegUnit> = BTreeMap::new();

    let mut live_ranges: Vec<&LiveRange> = context.liveness().ranges().values().collect();
    live_ranges.sort_by(|a, b| func.layout.cmp(a.def(), b.def()));

    let mut active: Vec<&LiveRange> = Vec::new();
    for live_range in live_ranges {
        expire_old_intervals(/*context,*/ func, &mut active, &mut unused_regs, &register_assignments, live_range);
        if let Some(reg) = unused_regs.pop() {
            register_assignments.insert(live_range.key(), reg);
            insert_active(func, &mut active, live_range);
        } else {
            spill_at_interval(/*context,*/ func, &mut active, &mut register_assignments, live_range);
        }
    }
}

fn expire_old_intervals(
    func: &mut Function,
    active: &mut Vec<&LiveRange>,
    unused_regs: &mut Vec<RegUnit>,
    register_assignments: &BTreeMap<Value, RegUnit>,
    i: &LiveRange,
) {
    let mut idx = 0;
    while idx < active.len() {
        let j = active[idx];
        // if >=
        if func.layout.cmp(live_range_end(func, j), i.def()) != Ordering::Less {
            idx += 1;
            continue;
        }
        active.remove(idx);
        unused_regs.push(register_assignments[&j.key()]);
    }
}

fn spill_at_interval<'a>(
    func: &mut Function,
    active: &mut Vec<&'a LiveRange>,
    register_assignments: &mut BTreeMap<Value, RegUnit>,
    i: &'a LiveRange,
) {
    let spill = *active.last().expect("Spilling => Register pressure => active.last().is_some()");
    if func.layout.cmp(live_range_end(func, spill), live_range_end(func, i)) == Ordering::Greater {
        assert!(register_assignments.insert(i.key(), register_assignments[&spill.key()]).is_none());
        // location[spill] <- new stack location
        active.pop().unwrap(); // remove spill from active
        insert_active(func, active, i);
    } else {
        // location[i] <- new stack location
    }
}
