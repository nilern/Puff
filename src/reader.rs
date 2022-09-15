use crate::oref::{ORef, Fixnum};
use crate::pos::{Pos, Positioned, Span, Spanning};

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
                            .unwrap_or_else(|| self.chars.len()),
                        c
                    );

                    res
                })
            })
    }
}

// TODO: Proper error type:
type ReadResult<T> = Result<Spanning<T>, ()>;

pub struct Reader<'a> {
    input: Input<'a>
}

impl<'a> Reader<'a> {
    pub fn new(chars: &'a str) -> Self { Reader {input: Input::new(chars)} }

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
                    .map(|res| res.map(ORef::from))
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
            Reader::new("  5  23  ")
                .map(Result::unwrap)
                .collect::<Vec<Spanning<ORef>>>(),
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
