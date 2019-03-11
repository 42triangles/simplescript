#[macro_use]  // TODO: Remove this as soon as `nom` gets its macros in order.
extern crate nom;

pub mod ast;
pub mod parse;

#[cfg(test)]  // TODO: Actual test coverage
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
