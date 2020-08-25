mod eval;
mod flag;
mod flag_value;
mod rule;
mod segment;
mod store;
mod user;
mod variation;

pub use eval::*;
pub use flag::*;
pub use flag_value::*;
pub use rule::*;
pub use segment::*;
pub use store::*;
pub use user::*;
pub use variation::*;

#[cfg(test)]
mod tests {
    #[test]
    /// TODO remove me
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
