#[derive(Clone, Copy)]
pub struct Pos {
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
    pub fn advance(&self, c: char) -> Self {
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

pub struct Positioned<T> {
    pub v: T,
    pub pos: Pos
}

struct Span {
    start: Pos,
    end: Pos
}

struct Spanning<T> {
    v: T,
    span: Span
}
