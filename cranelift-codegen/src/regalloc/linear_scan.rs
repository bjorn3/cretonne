use alloc::collections::BTreeMap;
use alloc::vec::Vec;
use core::cmp::Ordering;

use cranelift_entity::{SparseMapValue, SecondaryMap};

use crate::cursor::{Cursor, EncCursor};
use crate::ir::{Function, ProgramOrder, ProgramPoint, StackSlotData, StackSlotKind, Value, ValueLoc};
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
    let mut value_locations: SecondaryMap<Value, ValueLoc> = SecondaryMap::new();

    let mut live_ranges: Vec<&LiveRange> = context.liveness().ranges().values().collect();
    live_ranges.sort_by(|a, b| func.layout.cmp(a.def(), b.def()));

    let mut active: Vec<&LiveRange> = Vec::new(); // active <- {}
    for i in live_ranges { // foreach live interval i, in order of increasing start point
        expire_old_intervals(/*context,*/ func, &mut active, &mut unused_regs, &value_locations, i);
        if let Some(reg) = unused_regs.pop() { // if length(active) != R
            assert!(value_locations[i.key()] == ValueLoc::Unassigned);
            value_locations[i.key()] = ValueLoc::Reg(reg); // register[i] <- a register removed from pool of free registers
            insert_active(func, &mut active, i); // add i to active, sorted by increasing end point
        } else {
            spill_at_interval(/*context,*/ func, &mut active, &mut value_locations, i);
        }
    }
}

fn expire_old_intervals(
    func: &mut Function,
    active: &mut Vec<&LiveRange>,
    unused_regs: &mut Vec<RegUnit>,
    value_locations: &SecondaryMap<Value, ValueLoc>,
    i: &LiveRange,
) {
    while !active.is_empty() { // foreach interval j in active, in order of increasing end point
        let j = active[0];
        if func.layout.cmp(live_range_end(func, j), i.def()) != Ordering::Less { // if endpoint[j]>=startpoint[i]
            return;
        }
        active.remove(0); // remove j from active
        unused_regs.push(value_locations[j.key()].unwrap_reg()); // add register[j] to pool of free registers
    }
}

fn spill_at_interval<'a>(
    func: &mut Function,
    active: &mut Vec<&'a LiveRange>,
    value_locations: &mut SecondaryMap<Value, ValueLoc>,
    i: &'a LiveRange,
) {
    let spill = *active.last().expect("Spilling => Register pressure => active.last().is_some()"); // last interval in active
    if func.layout.cmp(live_range_end(func, spill), live_range_end(func, i)) == Ordering::Greater { // if endpoint[spill] > endpoint[i]
        assert!(value_locations[i.key()] == ValueLoc::Unassigned);
        value_locations[i.key()] = ValueLoc::Reg(value_locations[spill.key()].unwrap_reg()); // register[i] <- register[spill]
        /*let slot = func.create_stack_slot(StackSlotData {
            kind: StackSlotKind::SpillSlot,
            size: func.dfg.value_type(spill.key()).bytes(),
            offset: None,
        });
        value_locations[spill.key()] = ValueLoc::Stack(slot); // location[spill] <- new stack location*/
        active.pop().unwrap(); // remove spill from active
        insert_active(func, active, i); // add i to active, sorted by increasing end point
    } else {
        /*let slot = func.create_stack_slot(StackSlotData {
            kind: StackSlotKind::SpillSlot,
            size: func.dfg.value_type(i.key()).bytes(),
            offset: None,
        });
        value_locations[i.key()] = ValueLoc::Stack(slot); // location[i] <- new stack location*/
    }
}
