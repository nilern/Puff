use molysite::reader::Reader;
use molysite::mutator::Mutator;
use molysite::compiler::compile;
use molysite::closure::Closure;
use molysite::verifier::verify;
use molysite::oref::{ORef, Fixnum};
use molysite::string::String;
use molysite::symbol::Symbol;
use molysite::vector::Vector;
use molysite::heap_obj::{Singleton, Indexed};
use molysite::list::{Pair, EmptyList};

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

fn assert_list_equal(mt: &Mutator, mut ls1: ORef, mut ls2: ORef) {
    loop {
        if let Some(pair1) = ls1.try_cast::<Pair>(mt) {
            if let Some(pair2) = ls2.try_cast::<Pair>(mt) {
                unsafe {
                    assert_eq!(pair1.as_ref().car, pair2.as_ref().car);

                    ls1 = pair1.as_ref().cdr;
                    ls2 = pair2.as_ref().cdr;
                }
            } else {
                assert!(false);
            }
        } else {
            assert_eq!(ls1, ls2);
            break;
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

    let ls1 = eval_string(&mut mt, "(quote (+ 1 2))");
    let ls2 = *Reader::new("(+ 1 2)").next(&mut mt).unwrap().unwrap().v;
    assert_list_equal(&mt, ls1, ls2);


    assert_eq!(eval_string(&mut mt, "'a"), Symbol::new(&mut mt, "a").into());

    let v1 = eval_string(&mut mt, "'#(a b c)");
    let v2 = *Reader::new("#(a b c)").next(&mut mt).unwrap().unwrap().v;
    assert_vector_equal(&mt, v1, v2);

    assert_eq!(eval_string(&mut mt, "'()"), EmptyList::instance(&mt).into());

    let ls1 = eval_string(&mut mt, "'(+ 1 2)");
    let ls2 = *Reader::new("(+ 1 2)").next(&mut mt).unwrap().unwrap().v;
    assert_list_equal(&mt, ls1, ls2);

    let ls1 = eval_string(&mut mt, "'(quote a)");
    let ls2 = *Reader::new("(quote a)").next(&mut mt).unwrap().unwrap().v;
    assert_list_equal(&mt, ls1, ls2);

    let ls1 = eval_string(&mut mt, "''a");
    let ls2 = *Reader::new("(quote a)").next(&mut mt).unwrap().unwrap().v;
    assert_list_equal(&mt, ls1, ls2);
}

#[test]
fn test_lambda() {
    let mut mt = Mutator::new(1 << 20, false).unwrap();

    let f = eval_string(&mut mt, "(lambda () 42)");
    assert!(f.instance_of::<Closure>(&mt));

    assert_eq!(eval_string(&mut mt, "((lambda () 42))"), Fixnum::try_from(42isize).unwrap().into());

    assert_eq!(eval_string(&mut mt, "((lambda (x) x) 42)"), Fixnum::try_from(42isize).unwrap().into());

    let ls1 = eval_string(&mut mt, "((lambda ls ls) 1 2 3)");
    let ls2 = *Reader::new("(1 2 3)").next(&mut mt).unwrap().unwrap().v;
    assert_list_equal(&mt, ls1, ls2);

    let ls1 = eval_string(&mut mt, "((lambda (x . ls) ls) 1 2 3)");
    let ls2 = *Reader::new("(2 3)").next(&mut mt).unwrap().unwrap().v;
    assert_list_equal(&mt, ls1, ls2);

    assert_eq!(eval_string(&mut mt, "(letrec ((f (lambda () 42))) (f))"), Fixnum::try_from(42isize).unwrap().into());
}

#[test]
fn test_if() {
    let mut mt = Mutator::new(1 << 20, false).unwrap();

    assert_eq!(eval_string(&mut mt, "(if #t 2 3)"), Fixnum::try_from(2isize).unwrap().into());
    assert_eq!(eval_string(&mut mt, "(if #f 2 3)"), Fixnum::try_from(3isize).unwrap().into());
}

#[test]
fn test_set() {
    let mut mt = Mutator::new(1 << 20, false).unwrap();

    assert_eq!(eval_string(&mut mt, "(begin (define x 0) (set! x 1) x)"),
        Fixnum::try_from(1isize).unwrap().into());

    assert_eq!(eval_string(&mut mt, "((lambda (x) (begin (set! x 1) x)) 0)"),
        Fixnum::try_from(1isize).unwrap().into());

}

#[test]
fn test_begin() {
    let mut mt = Mutator::new(1 << 20, false).unwrap();

    assert_eq!(eval_string(&mut mt, "(begin 42)"), Fixnum::try_from(42isize).unwrap().into());

    assert_eq!(eval_string(&mut mt, "((lambda (x) (begin (set! x 1) x)) 0)"),
        Fixnum::try_from(1isize).unwrap().into());
}
