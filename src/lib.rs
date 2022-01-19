//! This crate contains the LaunchDarkly Rust SDK feature flag evaluation engine.
//!
//! Normal use of the Rust SDK does not require referencing this crate directly. It is used internally
//! by the SDK, but is published and versioned separately so it can be used in other LaunchDarkly
//! components without making the SDK versioning dependent on these internal APIs.

#![deny(rustdoc::missing_crate_level_docs)]
#![deny(missing_docs)]

mod eval;
mod flag;
mod flag_value;
mod rule;
mod segment;
mod store;
mod test_common;
mod user;
mod util;
mod variation;

pub use eval::*;
pub use flag::*;
pub use flag_value::*;
pub use rule::*;
pub use segment::*;
pub use store::*;
pub use user::*;
pub use variation::*;
