use std::iter::Peekable;
use std::str::Chars;

use crate::oref::{ORef, Fixnum};

#[derive(Clone, Copy)]
struct Pos {
    index: usize,
    line: usize,
    col: usize
}

impl Default for Pos {
    fn default() -> Self {
        Pos {
            index: 0,
            line: 1,
            col: 1
        }
    }
}

impl Pos {
    fn advance(&self, c: char) -> Self {
        if c != '\n' {
            Pos {
                index: self.index + 1,
                line: self.line,
                col: self.col + 1
            }
        } else {
            Pos {
                index: self.index + 1,
                line: self.line + 1,
                col: 1
            }
        }
    }
}

struct Span {
    start: Pos,
    end: Pos
}

struct Spanning<T> {
    v: T,
    span: Span
}

struct Positioned<T> {
    v: T,
    pos: Pos
}

struct Input<'a> {
    chars: Peekable<Chars<'a>>,
    pos: Pos
}

impl<'a> Input<'a> {
    fn new(chars: &'a str) -> Self {
        Input {
            chars: chars.chars().peekable(),
            pos: Pos::default()
        }
    }

    fn peek(&mut self) -> Option<Positioned<char>> {
        self.chars.peek().map(|&c| {
            Positioned {
                v: c,
                pos: self.pos
            }
        })
    }
}

impl<'a> Iterator for Input<'a> {
    type Item = Positioned<char>;

    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next().map(|c| {
            let res = Positioned {
                v: c,
                pos: self.pos
            };

            self.pos = self.pos.advance(c);

            res
        })
    }
}

// TODO: Spanning<T>, proper error type:
type ReadResult<T> = Result<T, ()>;

struct Reader<'a> {
    input: Input<'a>
}

impl<'a> Reader<'a> {
    fn new(chars: &'a str) -> Self { Reader {input: Input::new(chars)} }

    fn read_fixnum(&mut self, radix: u32, first_pc: Positioned<char>)
        -> ReadResult<Fixnum>
    {
        let mut n = first_pc.v.to_digit(radix).unwrap() as isize;

        while let Some(pc) = self.input.peek() {
            if pc.v.is_digit(radix) {
                self.input.next();
                n = (radix as isize) * n
                    + pc.v.to_digit(radix).unwrap() as isize;
            } else {
                break;
            }
        }

        Fixnum::try_from(n)
            .or(Err(())) // FIXME
    }
}

impl<'a> Iterator for Reader<'a> {
    type Item = ReadResult<ORef>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(pc) = self.input.peek() {
            if pc.v.is_whitespace() {
                self.input.next();
            } else {
                break;
            }
        }

        self.input.peek().map(|pc| {
            let radix = 10;

            if pc.v.is_digit(radix) {
                self.input.next();
                self.read_fixnum(radix, pc)
                    .map(ORef::from)
            } else {
                Err(())
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_fixnums() {
        assert_eq!(
            Reader::new("  1 1 2  3   5     ")
                .map(Result::unwrap)
                .collect::<Vec<ORef>>(),
            [1isize, 1, 2, 3, 5].into_iter()
                .map(|n| ORef::from(Fixnum::try_from(n).unwrap()))
                .collect::<Vec<ORef>>()
        );
    }
}
