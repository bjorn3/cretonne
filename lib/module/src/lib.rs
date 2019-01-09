//! Top-level lib.rs for `cranelift_module`.

#![deny(missing_docs, trivial_numeric_casts, unused_extern_crates)]
#![warn(unused_import_braces)]
#![cfg_attr(feature = "std", deny(unstable_features))]
#![cfg_attr(feature = "clippy", plugin(clippy(conf_file = "../../clippy.toml")))]
#![cfg_attr(feature = "cargo-clippy", allow(clippy::new_without_default))]
#![cfg_attr(
    feature = "cargo-clippy",
    warn(
        clippy::float_arithmetic,
        clippy::mut_mut,
        clippy::nonminimal_bool,
        clippy::option_map_unwrap_or,
        clippy::option_map_unwrap_or_else,
        clippy::print_stdout,
        clippy::unicode_not_nfc,
        clippy::use_self
    )
)]
// Turns on no_std and alloc features if std is not available.
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(not(feature = "std"), feature(alloc))]

#[cfg(not(feature = "std"))]
#[cfg_attr(test, macro_use)]
extern crate alloc;

mod backend;
mod data_context;
mod debug_context;
mod module;

pub use crate::backend::Backend;
pub use crate::data_context::{DataContext, DataDescription, Init};
pub use crate::debug_context::{DebugContext, DebugReloc};
pub use crate::module::{
    DataId, DebugId, DebugRelocation, FuncId, FuncOrDataId, Linkage, Module, ModuleError,
    ModuleNamespace, ModuleResult,
};

/// This replaces `std` in builds with `core`.
#[cfg(not(feature = "std"))]
mod std {
    pub use alloc::{borrow, boxed, string, vec};
    pub use core::*;
    pub mod collections {
        #[allow(unused_extern_crates)]
        extern crate hashmap_core;

        pub use self::hashmap_core::map as hash_map;
        pub use self::hashmap_core::{HashMap, HashSet};
    }
}

/// Version number of this crate.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
