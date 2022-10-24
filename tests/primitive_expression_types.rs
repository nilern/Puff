use molysite::reader::Reader;
use molysite::mutator::Mutator;
use molysite::compiler::compile;
use molysite::closure::Closure;
use molysite::verifier::verify;
use molysite::oref::{ORef, Fixnum};

fn eval_string(mt: &mut Mutator, s: &str) -> ORef {
    let mut reader = Reader::new(s);

    let res = reader.next(mt).unwrap();
    let sv = res.unwrap();
    let code = {
        let code = compile(mt, *sv.v, false);
        mt.root_t(code)
    };

    unsafe { verify(&mt, code.as_ref()) }.unwrap();

    mt.push((*code).into());
    let f = Closure::new(mt, 0);
    mt.pop();
    mt.push(f.into());
    let v = mt.invoke();

    assert!(reader.next(mt).is_none());

    v
}

#[test]
fn fixnum_literal() {
    let mut mt = Mutator::new(1 << 20).unwrap();

    let n = eval_string(&mut mt, "5");

    assert_eq!(n, Fixnum::try_from(5isize).unwrap().into());
}
