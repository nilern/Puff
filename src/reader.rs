use crate::oref::{ORef, Fixnum, Gc};
use crate::handle::{Handle, HandleT};
use crate::mutator::Mutator;
use crate::symbol::Symbol;
use crate::list::{EmptyList, Pair};
use crate::heap_obj::Singleton;
use crate::string::String;
use crate::bool::Bool;
use crate::vector::Vector;
use crate::syntax::{self, Syntax};

#[derive(Clone)]
struct Pos {
    filename: Option<HandleT<String>>,
    index: usize,
    line: usize,
    col: usize
}

impl Pos {
    fn default(filename: Option<HandleT<String>>) -> Self {
        Pos {
            filename,
            index: 0,
            line: 1,
            col: 1
        }
    }

    fn advance(&mut self, n: usize, c: char) {
        self.index += n;

        if c != '\n' {
            self.col += 1;
        } else {
            self.line += 1;
            self.col = 1;
        }
    }

    fn as_obj(&self, mt: &mut Mutator) -> Gc<syntax::Pos> {
        syntax::Pos::new(mt, self.filename.clone(),
            Fixnum::try_from(self.line).unwrap(),
            Fixnum::try_from(self.col).unwrap())
    }
}

struct Positioned<T> {
    v: T,
    pos: Pos
}

struct Input<'a> {
    chars: &'a str,
    pos: Pos
}

impl<'a> Input<'a> {
    fn new(chars: &'a str, filename: Option<HandleT<String>>) -> Self {
        Input {
            chars: chars,
            pos: Pos::default(filename)
        }
    }

    fn peek(&mut self) -> Option<Positioned<char>> {
        self.chars.get(self.pos.index..)
            .and_then(|cs| cs.chars().next())
            .map(|c| {
                Positioned {
                    v: c,
                    pos: self.pos.clone()
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
                        pos: self.pos.clone()
                    };

                    self.pos.advance(
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
type ReadResult = Result<HandleT<Syntax>, ()>;

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

fn is_explicit_sign(c: char) -> bool { c == '+' || c == '-' }

fn is_sign_subsequent(c: char) -> bool { is_initial(c) || is_explicit_sign(c) || c == '@' }

impl<'i> Reader<'i> {
    pub fn new(chars: &'i str, filename: Option<HandleT<String>>) -> Self {
        Reader { input: Input::new(chars, filename) }
    }

    fn intertoken_space(&mut self) {
        while let Some(pc) = self.input.peek() {
            if pc.v.is_whitespace() {
                self.input.next();
            } else {
                break;
            }
        }
    }

    fn read_bool(&mut self, mt: &mut Mutator, start: Pos) -> ReadResult {
        let b = match self.input.peek().unwrap().v {
            't' => {
                self.input.next();
                true
            },

            'f' => {
                self.input.next();
                false
            },

            _ => unreachable!()
        };

        let b = Bool::instance(mt, b);
        let b = mt.root(b.into());
        let start = start.as_obj(mt);
        let start = mt.root_t(start);
        let stx = Syntax::new(mt, b, Some(start));
        Ok(mt.root_t(stx))
    }

    fn read_quoted(&mut self, mt: &mut Mutator, quote: Positioned<char>) -> ReadResult {
        let start = quote.pos.as_obj(mt);
        let start = mt.root_t(start);

        if let Some(res) = self.next(mt) {
            let ls = EmptyList::instance(mt).into();
            let ls = mt.root(ls);
            let ls = Pair::new(mt, res?.into(), ls).into();
            let ls = mt.root(ls);
            let quote = Symbol::new(mt, "quote").into();
            let quote = mt.root(quote);
            let quote = Syntax::new(mt, quote, Some(start.clone()));
            let quote = mt.root(quote.into());
            let ls = Pair::new(mt, quote, ls).into();
            let ls = mt.root(ls);
            let ls = Syntax::new(mt, ls, Some(start));

            Ok(mt.root_t(ls))
        } else {
            Err(()) // FIXME
        }
    }

    fn read_fixnum(&mut self, mt: &mut Mutator, radix: u32, first_pc: Positioned<char>) -> ReadResult {
        let mut n = first_pc.v.to_digit(radix).unwrap() as isize;

        while let Some(pc) = self.input.peek() {
            if pc.v.is_digit(radix) {
                self.input.next();
                n = (radix as isize).checked_mul(n).unwrap()
                    .checked_add(pc.v.to_digit(radix).unwrap() as isize)
                    .unwrap();
            } else {
                break;
            }
        }

        match Fixnum::try_from(n) {
            Ok(n) => {
                let n = mt.root(n.into());
                let start = first_pc.pos.as_obj(mt);
                let start = mt.root_t(start);
                let stx = Syntax::new(mt, n, Some(start));
                Ok(mt.root_t(stx))
            },

            Err(()) => Err(()) // FIXME
        }
    }

    fn read_symbol(&mut self, mt: &mut Mutator, first_pc: Positioned<char>) -> HandleT<Syntax> {
        while let Some(pc) = self.input.peek() {
            if is_subsequent(pc.v) {
                self.input.next();
            } else {
                break;
            }
        }

        let sym = Symbol::new(mt, &self.input.chars[first_pc.pos.index..self.input.pos.index]);
        let sym = mt.root(sym.into());
        let start = first_pc.pos.as_obj(mt);
        let start = mt.root_t(start);
        let stx = Syntax::new(mt, sym, Some(start));
        mt.root_t(stx)
    }

    fn read_peculiar_identifier(&mut self, mt: &mut Mutator, sign: Positioned<char>) -> HandleT<Syntax> {
        while let Some(pc) = self.input.peek() {
            if is_sign_subsequent(pc.v) {
                self.input.next();
            } else {
                break;
            }
        }

        let sym = Symbol::new(mt, &self.input.chars[sign.pos.index..self.input.pos.index]);
        let sym = mt.root(sym.into());
        let start = sign.pos.as_obj(mt);
        let start = mt.root_t(start);
        let stx = Syntax::new(mt, sym, Some(start));
        mt.root_t(stx)
    }

    fn read_string(&mut self, mt: &mut Mutator, double_quote: Positioned<char>) -> ReadResult {
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

        let start = double_quote.pos;
        let str = String::new(mt, &self.input.chars[start.index + 1..self.input.pos.index - 1]);
        let str = mt.root(str.into());
        let start = start.as_obj(mt);
        let start = mt.root_t(start);
        let stx = Syntax::new(mt, str, Some(start));
        Ok(mt.root_t(stx))
    }

    fn read_list(&mut self, mt: &mut Mutator, lparen: Positioned<char>) -> ReadResult {
        let start = lparen.pos;
        let start = start.as_obj(mt);
        let start = mt.root_t(start);

        let empty = mt.root(EmptyList::instance(mt).into());

        self.intertoken_space();

        // ()
        if let Some(pc) = self.input.peek() {
            if pc.v == ')' {
                self.input.next();
                let stx = Syntax::new(mt, empty, Some(start));
                return Ok(mt.root_t(stx));
            }
        }

        let car: Handle = match self.next(mt) {
            Some(res) => res?.into(),
            None => return Err(())
        };

        let ls: HandleT<Pair> = {
            let ls = Pair::new(mt, car, empty.clone());
            mt.root_t(ls)
        };

        // (<datum> ...
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
                        Some(res) => unsafe { last_pair.as_mut().cdr = (*res?).into(); },
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
                            let pair = Pair::new(mt, res?.into(), empty.clone());
                            unsafe { last_pair.as_mut().cdr = pair.into(); }
                            last_pair = mt.root_t(pair);
                        },

                        None => return Err(())
                    },

                None => return Err(())
            }
        }

        let stx = Syntax::new(mt, ls.into(), Some(start));
        Ok(mt.root_t(stx))
    }

    fn read_vector(&mut self, mt: &mut Mutator, start: Pos) -> ReadResult {
        let mut vs: Vec<Handle> = Vec::new();

        loop {
            self.intertoken_space();

            match self.input.peek() {
                Some(pc) if pc.v == ')' => {
                    self.input.next();
                    break;
                },

                Some(_) =>
                    match self.next(mt) {
                        Some(res) => vs.push(res?.into()),
                        None => return Err(())
                    },

                None => return Err(())
            }
        }

        let vector = Vector::<ORef>::from_handles(mt, &vs);
        let vector = mt.root(vector.into());
        let start = start.as_obj(mt);
        let start = mt.root_t(start);
        let stx = Syntax::new(mt, vector, Some(start));
        Ok(mt.root_t(stx))
    }

    pub fn next(&mut self, mt: &mut Mutator) -> Option<ReadResult> {
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
                },

                '#' => {
                    let start = pc.pos;
                    self.input.next();

                    if let Some(pc) = self.input.peek() {
                        match pc.v {
                            't' | 'f' => self.read_bool(mt, pc.pos),

                            '(' => {
                                self.input.next();
                                self.read_vector(mt, start)
                            },

                            _ => todo!()
                        }
                    } else {
                        Err(())
                    }
                }

                c if c.is_digit(radix) => {
                    self.input.next();
                    self.read_fixnum(mt, radix, pc)
                },

                c if is_initial(c) => {
                    self.input.next();
                    Ok(self.read_symbol(mt, pc))
                },

                c if is_explicit_sign(c) => {
                    self.input.next();

                    match self.input.peek() {
                        Some(pc) if pc.v.is_digit(radix) => todo!(),

                        Some(_) | None => Ok(self.read_peculiar_identifier(mt, pc))
                    }
                }

                _ => Err(())
            }
        })
    }
}
