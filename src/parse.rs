use std::str::FromStr;

use nom;
use nom::types::CompleteStr;
use nom_locate::LocatedSpan;

use super::ast::Ast as GenericAst;

pub type Span<'a> = LocatedSpan<CompleteStr<'a>>;
pub type Ast<'a> = GenericAst<&'a str>;

#[derive(Debug)]
pub enum ErrorKind {
    CompletelyMalformed,
    UnexpectedEof,
    NotAnInteger,
    ErrorCode(u32),
}

// TODO: Impl `Error` trait
#[derive(Debug)]
pub struct Error<'a> {
    pub parent: Option<Box<Error<'a>>>,
    pub kind: ErrorKind,
    pub span: Span<'a>,
}

impl<'a> From<Error<'a>> for nom::Err<Span<'a>, Error<'a>> {
    fn from(this: Error<'a>) -> Self {
        nom::Err::Error(nom::Context::Code(this.span, nom::ErrorKind::Custom(this)))
    }
}

impl<'a> From<u32> for Error<'a> {
    fn from(code: u32) -> Self {
        Error {
            parent: None,
            kind: ErrorKind::ErrorCode(code),
            span: Span::new(CompleteStr("")),
        }
    }
}

/// Adds an error to the error chain, or spawns one in the first place, if the given parser failed.
/// In that case, the error parser is used to determine the span the error should display as being
/// faulty, this parser should never fail. If it does, 
macro_rules! with_error {
    (__impl, inherit_error $err:expr) => {
        match $err {
            nom::Err::Error(nom::Context::Code(_, nom::ErrorKind::Custom(error))) => {
                Some(Box::new(error))
            },
            _ => None,
        }
    };
    (__impl, discard_error $err:expr) => {
        // force the use of `$err`, so that the compiler doesn't emit a warning
        match $err {
            _ => None,
        }
    };
    ($inp:expr, $kind:ident @ $err_span:ident!($($err_arg:tt)*), $inherit:ident $inner:ident!($($inner_arg:tt)*)) => {
        match $inner!($inp, $($inner_arg)*) {
            Ok((left, out)) => Ok((left, out)),
            Err(err) => {
                let error = recognize!($inp, $err_span!($($err_arg)*));
                Err(Error {
                    parent: with_error!(__impl, $inherit err),
                    kind: match error {
                        Ok(_) => ErrorKind::$kind,
                        Err(_) => ErrorKind::CompletelyMalformed,
                    },
                    span: match error {
                        Ok((_, span)) => span,
                        Err(_) => $inp,
                    }
                }.into())
            }
        }
    };
    ($inp:expr, $kind:ident @ $err_span:ident!($($err_arg:tt)*), $inherit:ident $inner:expr) => {
        with_error!($inp, $kind @ $err_span!($($err_arg)*), $inherit call!($inner))
    };
    ($inp:expr, $kind:ident$(($($arg:tt)*))* @ $err_span:expr, $inherit:ident $($inner:tt)*) => {
        with_error!($inp, $kind @ call!($err_span), $inherit:ident $($inner)*)
    };
}

/// Returns `Err(Error::UnexpectedEof)` if the given input is empty
macro_rules! not_eof {
    ($inp:expr, $inner:ident!($($arg:tt)*)) => {
        match $inp.fragment.0 {
            "" => Err(Error {
                parent: None,
                kind: ErrorKind::UnexpectedEof,
                span: $inp,
            }.into()),
            _ => $inner!($inp, $($arg)*),
        }
    };
    ($inp:expr, $inner:expr) => {
        not_eof!($inp, call!($inner))
    };
}

macro_rules! parse_str {
    ($inp:expr, $($inner:tt)*) => {
        map!($inp, $($inner)*, |x| x.fragment.0)
    };
}

macro_rules! is_digit_impls {
    ($($name:ident $radix:literal,)*) => {
        $(
            #[allow(dead_code)]  // TODO: Remove as soon as the compile is smart enough to figure out usage in macros
            fn $name(c: char) -> bool {
                c.is_digit($radix)
            }
        )*
    };
}

is_digit_impls!(
    is_hex_digit 16,
    is_bin_digit 2,
    is_oct_digit 8,
    is_digit 10,
);

named!(
    integer_literal<Span, i64, Error>,
    not_eof!(
        with_error!(
            NotAnInteger @ alt!(
                preceded!(tag!("0x"), take_while1!(is_hex_digit)) => {|_| ()} |
                preceded!(tag!("0b"), take_while1!(is_bin_digit)) => {|_| ()} |
                preceded!(tag!("0o"), take_while!(is_oct_digit)) => {|_| ()} |
                take_while1!(is_digit) => {|_| ()} |
                take!(0) => {|_| ()}
            ),
            discard_error fix_error!(  // TODO: other integer literal kinds
                Error,
                map_res!(
                    alt!(
                        parse_str!(preceded!(tag!("0x"), take_while!(is_hex_digit))) => {
                            |x| i64::from_str_radix(x, 16)
                        } |
                        parse_str!(preceded!(tag!("0b"), take_while!(is_hex_digit))) => {
                            |x| i64::from_str_radix(x, 2)
                        } |
                        parse_str!(preceded!(tag!("0o"), take_while!(is_hex_digit))) => {
                            |x| i64::from_str_radix(x, 8)
                        } |
                        parse_str!(take_while!(is_digit)) => {|x| i64::from_str(x)}
                    ),
                    |x| x
                )
            )
        )
    )
);

// TODO: Test properly
#[test]
fn test_integer_literal() {
    assert_eq!(
        integer_literal(Span::new(CompleteStr("109"))).map(|(_, out)| out).ok(),
        Some(109)
    );
    assert_eq!(
        integer_literal(Span::new(CompleteStr("0b101"))).map(|(_, out)| out).ok(),
        Some(0b101)
    );
    assert_eq!(
        integer_literal(Span::new(CompleteStr("0o107"))).map(|(_, out)| out).ok(),
        Some(0o107)
    );
    assert_eq!(
        integer_literal(Span::new(CompleteStr("0x10f"))).map(|(_, out)| out).ok(),
        Some(0x10f)
    );
    assert!(
        integer_literal(Span::new(CompleteStr("a"))).is_err()
    );
}
