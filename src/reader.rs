use crate::oref::{ORef, Fixnum, Gc};
use crate::handle::{Handle, HandleT};
use crate::pos::{Pos, Positioned, Span, Spanning};
use crate::mutator::Mutator;
use crate::symbol::Symbol;
use crate::list::{EmptyList, Pair};
use crate::heap_obj::Singleton;
use crate::string::String;

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

fn is_initial(c: char) -> bool { c.is_alphabetic() || is_special_initial(c) }

fn is_special_initial(c: char) -> bool {
    c == '!' || c == '$' || c == '%' || c == '&' || c == '*' || c == '/' || c == ':'
    || c == '<' || c == '=' || c == '>' || c == '?' || c == '^' || c == '_' || c == '~'
}

fn is_subsequent(c: char) -> bool { is_initial(c) || c.is_digit(10) || is_special_subsequent(c) }

fn is_special_subsequent(c: char) -> bool { c == '+' || c == '-' || c == '.' || c == '@' }

impl<'i> Reader<'i> {
    pub fn new(chars: &'i str) -> Self { Reader {input: Input::new(chars) } }

    fn intertoken_space(&mut self) {
        while let Some(pc) = self.input.peek() {
            if pc.v.is_whitespace() {
                self.input.next();
            } else {
                break;
            }
        }
    }

    fn read_quoted(&mut self, mt: &mut Mutator, quote: Positioned<char>) -> ReadResult<Handle> {
        let start = quote.pos;

        if let Some(res) = self.next(mt) {
            let ls = EmptyList::instance(mt).into();
            let ls = mt.root(ls);
            let ls = Pair::new(mt, res?.v, ls).into();
            let ls = mt.root(ls);
            let quote = Symbol::new(mt, "quote").into();
            let quote = mt.root(quote);
            let ls = Pair::new(mt, quote, ls).into();
            let ls = mt.root(ls);

            Ok(Spanning {
                v: ls,
                span: Span {start, end: self.input.pos}
            })
        } else {
            Err(()) // FIXME
        }
    }

    fn read_fixnum(&mut self, radix: u32, first_pc: Positioned<char>) -> ReadResult<Fixnum> {
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

    fn read_symbol(&mut self, mt: &mut Mutator, first_pc: Positioned<char>) -> Spanning<Gc<Symbol>> {
        let start = first_pc.pos;

        while let Some(pc) = self.input.peek() {
            if is_subsequent(pc.v) {
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

    fn read_string(&mut self, mt: &mut Mutator, double_quote: Positioned<char>) -> ReadResult<Gc<String>> {
        let start = double_quote.pos;

        loop {
            if let Some(pc) = self.input.peek() {
                match pc.v {
                    '"' => {
                        self.input.next();
                        break;
                    },

                    '\\' => todo!(),

                    _ => { self.input.next(); }
                }
            } else {
                return Err(());
            }
        }

        let end = self.input.pos;
        Ok(Spanning {
            v: String::new(mt, &self.input.chars[start.index + 1..end.index - 1]),
            span: Span {start, end}
        })
    }

    fn read_list(&mut self, mt: &mut Mutator, lparen: Positioned<char>) -> ReadResult<Handle> {
        let start = lparen.pos;

        let empty = mt.root(EmptyList::instance(mt).into());

        self.intertoken_space();

        // ()
        if let Some(pc) = self.input.peek() {
            if pc.v == ')' {
                self.input.next();
                return Ok(Spanning {
                    v: empty,
                    span: Span {start, end: self.input.pos}
                })
            }
        }

        let car: Handle = match self.next(mt) {
            Some(res) => res?.v,
            None => return Err(())
        };

        let ls: HandleT<Pair> = {
            let ls = Pair::new(mt, car, empty.clone());
            mt.root_t(ls)
        };

        let mut last_pair = ls.clone();
        loop {
            self.intertoken_space();

            match self.input.peek() {
                Some(pc) if pc.v == ')' => { // (<datum>+)
                    self.input.next();
                    break;
                },

                Some(pc) if pc.v == '.' => { // (<datum>+ . <datum>)
                    self.input.next();

                    match self.next(mt) {
                        Some(res) => unsafe { last_pair.as_mut().cdr = *res?.v; },
                        None => return Err(())
                    }

                    self.intertoken_space();

                    match self.input.peek() {
                        Some(pc) =>
                            match pc.v {
                                ')' => {
                                    self.input.next();
                                    break;
                                },
                                _ => return Err(())
                            }

                        None => return Err(())
                    }
                },

                Some(_) => // (<datum>+ ...
                    match self.next(mt) {
                        Some(res) => {
                            let pair = Pair::new(mt, res?.v, empty.clone());
                            unsafe { last_pair.as_mut().cdr = pair.into(); }
                            last_pair = mt.root_t(pair);
                        },

                        None => return Err(())
                    },

                None => return Err(())
            }
        }

        Ok(Spanning {
            v: ls.into(),
            span: Span {start, end: self.input.pos}
        })
    }

    pub fn next(&mut self, mt: &mut Mutator) -> Option<ReadResult<Handle>> {
        self.intertoken_space();

        self.input.peek().map(|pc| {
            let radix = 10;

            match pc.v {
                '(' => {
                    self.input.next();
                    self.read_list(mt, pc)
                },

                '\'' => {
                    self.input.next();
                    self.read_quoted(mt, pc)
                },

                '"' => {
                    self.input.next();
                    self.read_string(mt, pc)
                        .map(|ps| ps.map(|s| mt.root(s.into())))
                },

                c if c.is_digit(radix) => {
                    self.input.next();
                    self.read_fixnum(radix, pc).map(|sv| sv.map(ORef::from))
                        .map(|pn| pn.map(|n| mt.root(ORef::from(n))))
                },

                c if is_initial(c) => {
                    self.input.next();
                    Ok(self.read_symbol(mt, pc).map(ORef::from))
                        .map(|ps| ps.map(|n| mt.root(ORef::from(n))))
                },

                _ => Err(())
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_fixnums() {
        let mut mt = Mutator::new(1 << 20 /* 1 MiB */, false).unwrap();

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

    #[test]
    fn read_symbols() {
        let mut mt = Mutator::new(1 << 20 /* 1 MiB */, false).unwrap();

        let mut reader = Reader::new("  foo  bar  ");
        let mut vs = Vec::new();
        while let Some(res) = reader.next(&mut mt) {
            vs.push(res.unwrap().map(|v| *v));
        }

        assert_eq!(
            vs,
            ["foo", "bar"].into_iter()
                .enumerate()
                .map(|(i, name)| {
                    let index = (5*i + 2) as usize;
                    Spanning {
                        v: ORef::from(Symbol::new(&mut mt, name)),
                        span: Span {
                            start: Pos {
                                index,
                                line: 1,
                                col: index + 1
                            },
                            end: Pos {
                                index: index + 3,
                                line: 1,
                                col: index + 3 + 1
                            }
                        }
                    }
                })
                .collect::<Vec<Spanning<ORef>>>()
        );
    }
}
