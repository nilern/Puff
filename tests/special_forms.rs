use molysite::reader::Reader;
use molysite::mutator::Mutator;
use molysite::compiler::compile;
use molysite::closure::Closure;
use molysite::verifier::verify;
use molysite::oref::{ORef, Fixnum};
use molysite::string::String;
use molysite::symbol::Symbol;
use molysite::vector::Vector;
use molysite::heap_obj::Indexed;

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

fn assert_vector_equal(mt: &Mutator, v1: ORef, v2: ORef) {
    let v1 = v1.try_cast::<Vector<ORef>>(mt).unwrap();
    let v2 = v2.try_cast::<Vector<ORef>>(mt).unwrap();

    unsafe {
        assert_eq!(v1.as_ref().indexed_field().len(), v2.as_ref().indexed_field().len());

        for (v, u) in v1.as_ref().indexed_field().iter().zip(v2.as_ref().indexed_field().iter()) {
            assert_eq!(v, u);
        }
    }
}

#[test]
fn test_quote() {
    let mut mt = Mutator::new(1 << 20, false).unwrap();

    assert_eq!(eval_string(&mut mt, "(quote a)"), Symbol::new(&mut mt, "a").into());

    let v1 = eval_string(&mut mt, "(quote #(a b c))");
    let v2 = *Reader::new("#(a b c)").next(&mut mt).unwrap().unwrap().v;
    assert_vector_equal(&mt, v1, v2);

    assert_eq!(
        eval_string(&mut mt, "(quote (+ 1 2))"),
        *Reader::new("(+ 1 2)").next(&mut mt).unwrap().unwrap().v);
}
