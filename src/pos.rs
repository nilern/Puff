#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Pos {
    pub index: usize,
    pub line: usize,
    pub col: usize
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
    pub fn advance(&self, n: usize, c: char) -> Self {
        if c != '\n' {
            Pos {
                index: self.index + n,
                line: self.line,
                col: self.col + 1
            }
        } else {
            Pos {
                index: self.index + n,
                line: self.line + 1,
                col: 1
            }
        }
    }
}

pub struct Positioned<T> {
    pub v: T,
    pub pos: Pos
}

#[derive(Debug, PartialEq)]
pub struct Span {
    pub start: Pos,
    pub end: Pos
}

#[derive(Debug, PartialEq)]
pub struct Spanning<T> {
    pub v: T,
    pub span: Span
}

impl<T> Spanning<T> {
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanning<U> {
        Spanning {
            v: f(self.v),
            span: self.span
        }
    }
}
