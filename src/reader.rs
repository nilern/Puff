use crate::oref::{ORef, Gc};
use crate::handle::{HandleAny, Handle, Root, root};
use crate::mutator::Mutator;
use crate::symbol::Symbol;
use crate::list::{EmptyList, Pair};
use crate::heap_obj::Singleton;
use crate::string::String;
use crate::bool::Bool;
use crate::vector::Vector;
use crate::syntax::{self, Syntax};
use crate::fixnum::Fixnum;

#[derive(Clone)]
struct Pos {
    filename: Option<Handle<String>>,
    index: usize,
    line: usize,
    col: usize
}

impl Pos {
    fn default(filename: Option<Handle<String>>) -> Self {
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
        syntax::Pos::new(mt, self.filename.as_ref().map(Handle::<String>::borrow),
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
    fn new(chars: &'a str, filename: Option<Handle<String>>) -> Self {
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
type ReadResult = Result<Handle<Syntax>, ()>;

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
    pub fn new(chars: &'i str, filename: Option<Handle<String>>) -> Self {
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

        let b = root!(mt, Bool::instance(mt, b));
        let start = root!(mt, start.as_obj(mt));
        Ok(root!(mt, Syntax::new(mt, b.borrow().into(), Some(start.borrow().into()))))
    }

    fn read_quoted(&mut self, mt: &mut Mutator, quote: Positioned<char>) -> ReadResult {
        let start = root!(mt, quote.pos.as_obj(mt));

        if let Some(res) = self.next(mt) {
            let ls = root!(mt, EmptyList::instance(mt));
            let ls = root!(mt, Gc::<Pair>::new(mt, res?.borrow().into(), ls.borrow().into()));
            let quote = root!(mt, Symbol::new(mt, "quote"));
            let quote = root!(mt, Syntax::new(mt, quote.borrow().into(), Some(start.borrow())));
            let ls = root!(mt, Gc::<Pair>::new(mt, quote.borrow().into(), ls.borrow().into()));

            Ok(root!(mt, Syntax::new(mt, ls.borrow().into(), Some(start.borrow()))))
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
                let n = root!(mt, ORef::from(n));
                let start = root!(mt, first_pc.pos.as_obj(mt));
                Ok(root!(mt, Syntax::new(mt, n.borrow(), Some(start.borrow().into()))))
            },

            Err(()) => Err(()) // FIXME
        }
    }

    fn read_symbol(&mut self, mt: &mut Mutator, first_pc: Positioned<char>) -> Handle<Syntax> {
        while let Some(pc) = self.input.peek() {
            if is_subsequent(pc.v) {
                self.input.next();
            } else {
                break;
            }
        }

        let sym = root!(mt, Symbol::new(mt, &self.input.chars[first_pc.pos.index..self.input.pos.index]));
        let start = root!(mt, first_pc.pos.as_obj(mt));
        root!(mt, Syntax::new(mt, sym.borrow().into(), Some(start.borrow())))
    }

    fn read_peculiar_identifier(&mut self, mt: &mut Mutator, sign: Positioned<char>) -> Handle<Syntax> {
        while let Some(pc) = self.input.peek() {
            if is_sign_subsequent(pc.v) {
                self.input.next();
            } else {
                break;
            }
        }

        let sym = root!(mt, Symbol::new(mt, &self.input.chars[sign.pos.index..self.input.pos.index]));
        let start = root!(mt, sign.pos.as_obj(mt));
        root!(mt, Syntax::new(mt, sym.borrow().into(), Some(start.borrow())))
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
        let str = root!(mt, String::new(mt, &self.input.chars[start.index + 1..self.input.pos.index - 1]));
        let start = root!(mt, start.as_obj(mt));
        Ok(root!(mt, Syntax::new(mt, str.borrow().into(), Some(start.borrow()))))
    }

    fn read_list(&mut self, mt: &mut Mutator, lparen: Positioned<char>) -> ReadResult {
        let start = lparen.pos;
        let start = root!(mt, start.as_obj(mt));

        let empty = root!(mt, ORef::from(EmptyList::instance(mt)));

        self.intertoken_space();

        // ()
        if let Some(pc) = self.input.peek() {
            if pc.v == ')' {
                self.input.next();
                return Ok(root!(mt, Syntax::new(mt, empty.borrow(), Some(start.borrow()))));
            }
        }

        let car: HandleAny = match self.next(mt) {
            Some(res) => res?.into(),
            None => return Err(())
        };

        let ls: Handle<Pair> = root!(mt, Gc::<Pair>::new(mt, car.borrow(), empty.borrow()));

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
                        Some(res) => last_pair.set_cdr(res?.oref().into()),
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
                            let pair = Gc::<Pair>::new(mt, res?.borrow().into(), empty.borrow());
                            last_pair.set_cdr(pair.into());
                            last_pair = root!(mt, pair);
                        },

                        None => return Err(())
                    },

                None => return Err(())
            }
        }

        Ok(root!(mt, Syntax::new(mt, ls.borrow().into(), Some(start.borrow()))))
    }

    fn read_vector(&mut self, mt: &mut Mutator, start: Pos) -> ReadResult {
        let mut vs: Vec<HandleAny> = Vec::new();

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

        let vector = root!(mt, Vector::<ORef>::from_handles(mt, &vs));
        let start = root!(mt, start.as_obj(mt));
        Ok(root!(mt, Syntax::new(mt, vector.borrow().into(), Some(start.borrow()))))
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
