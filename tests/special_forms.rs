use molysite::reader::Reader;
use molysite::mutator::Mutator;
use molysite::compiler::compile;
use molysite::closure::Closure;
use molysite::verifier::verify;
use molysite::oref::{ORef, Fixnum};
use molysite::string::String;
use molysite::symbol::Symbol;

fn eval_string(mt: &mut Mutator, s: &str) -> ORef {
    let mut reader = Reader::new(s);

    let res = reader.next(mt).unwrap();
    let sv = res.unwrap();
    let code = compile(mt, *sv.v, mt.cfg().debug);

    unsafe { verify(&mt, code.as_ref()) }.unwrap();

    mt.push(code.into());
    let f = Closure::new(mt, 0);
    mt.pop();
    mt.push(f.into());
    let v = mt.invoke();

    assert!(reader.next(mt).is_none());

    v
}

#[test]
fn quote() {
    let mut mt = Mutator::new(1 << 20, false).unwrap();

    assert_eq!(eval_string(&mut mt, "(quote a)"), Symbol::new(&mut mt, "a").into());
    assert_eq!(
        eval_string(&mut mt, "(quote #(a b c))"),
        *Reader::new("#(a b c)").next(&mut mt).unwrap().unwrap().v);
    assert_eq!(
        eval_string(&mut mt, "(quote (+ 1 2))"),
        *Reader::new("(+ 1 2)").next(&mut mt).unwrap().unwrap().v);
}
