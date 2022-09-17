use crate::oref::{ORef, Fixnum, Gc};
use crate::handle::Handle;
use crate::pos::{Pos, Positioned, Span, Spanning};
use crate::mutator::Mutator;
use crate::symbol::Symbol;

struct Input<'a> {
    chars: &'a str,
    pos: Pos
}

impl<'a> Input<'a> {
    fn new(chars: &'a str) -> Self {
        Input {
            chars: chars,
            pos: Pos::default()
        }
    }

    fn peek(&mut self) -> Option<Positioned<char>> {
        self.chars.get(self.pos.index..)
            .and_then(|cs| cs.chars().next())
            .map(|c| {
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
        self.chars.get(self.pos.index..)
            .and_then(|cs| {
                let mut cis = cs.char_indices();

                cis.next().map(|(_, c)| {
                    let res = Positioned {
                        v: c,
                        pos: self.pos
                    };

                    self.pos = self.pos.advance(
                        cis.next()
                            .map(|(i, _)| i)
                            .unwrap_or(1),
                        c
                    );

                    res
                })
            })
    }
}

// TODO: Proper error type:
type ReadResult<T> = Result<Spanning<T>, ()>;

pub struct Reader<'i> {
    input: Input<'i>
}

impl<'i> Reader<'i> {
    pub fn new(chars: &'i str) -> Self { Reader {input: Input::new(chars) } }

    fn read_fixnum(&mut self, radix: u32, first_pc: Positioned<char>)
        -> ReadResult<Fixnum>
    {
        let mut n = first_pc.v.to_digit(radix).unwrap() as isize;
        let start = first_pc.pos;
        let mut end = start;

        while let Some(pc) = self.input.peek() {
            if pc.v.is_digit(radix) {
                self.input.next();
                n = (radix as isize).checked_mul(n).unwrap()
                    .checked_add(pc.v.to_digit(radix).unwrap() as isize)
                    .unwrap();
                end = pc.pos;
            } else {
                break;
            }
        }

        match Fixnum::try_from(n) {
            Ok(n) => Ok(Spanning {
                v: n,
                span: Span {start, end}
            }),
            Err(()) => Err(()) // FIXME
        }
    }

    fn read_symbol(&mut self, mt: &mut Mutator, first_pc: Positioned<char>)
        -> Spanning<Gc<Symbol>>
    {
        let start = first_pc.pos;

        while let Some(pc) = self.input.peek() {
            if pc.v.is_alphanumeric() {
                self.input.next();
            } else {
                break;
            }
        }

        let end = self.input.pos;
        Spanning {
            v: Symbol::new(mt, &self.input.chars[start.index..end.index]),
            span: Span {start, end}
        }
    }

    pub fn next(&mut self, mt: &mut Mutator) -> Option<ReadResult<Handle>> {
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
                self.read_fixnum(radix, pc).map(|sv| sv.map(ORef::from))
            } else if pc.v.is_alphabetic() {
                self.input.next();
                Ok(self.read_symbol(mt, pc).map(ORef::from))
            } else {
                Err(())
            }
            .map(|res| res.map(|n| unsafe { mt.root(ORef::from(n)) }))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_fixnums() {
        let mut mt = Mutator::new(1 << 20 /* 1 MiB */).unwrap();

        let mut reader = Reader::new("  5  23  ");
        let mut vs = Vec::new();
        while let Some(res) = reader.next(&mut mt) {
            vs.push(res.unwrap().map(|v| *v));
        }

        assert_eq!(
            vs,
            [5isize, 23].into_iter()
                .enumerate()
                .map(|(i, n)| {
                    let index = (3*i + 2) as usize;
                    Spanning {
                        v: ORef::from(Fixnum::try_from(n).unwrap()),
                        span: Span {
                            start: Pos {
                                index,
                                line: 1,
                                col: index + 1
                            },
                            end: Pos {
                                index: index + i,
                                line: 1,
                                col: index + i + 1
                            }
                        }
                    }
                })
                .collect::<Vec<Spanning<ORef>>>()
        );
    }
}
