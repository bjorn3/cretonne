//! Register allocation.
//!
//! This module contains data structures and algorithms used for register allocation.

mod affinity;
mod branch_splitting;
mod coalescing;
mod coloring;
mod context;
mod diversion;
mod linear_scan;
mod live_value_tracker;
mod liveness;
mod liverange;
mod pressure;
mod register_set;
mod reload;
mod safepoint;
mod solver;
mod spilling;
mod virtregs;

pub use self::context::Context;
pub use self::diversion::{EntryRegDiversions, RegDiversions};
pub use self::liveness::Liveness;
pub use self::register_set::RegisterSet;
pub use self::safepoint::emit_stackmaps;

// Only used by the verifier.
pub(crate) use self::liverange::LiveRange;
pub(crate) use self::virtregs::VirtRegs;
