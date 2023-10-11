//! IR (intermediate representation) module.
//! Constructed module tree is guaranteed to be correct.

pub mod tree;
mod parser;

pub use parser::convert;
