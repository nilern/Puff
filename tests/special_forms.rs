use molysite::reader::Reader;
use molysite::mutator::Mutator;
use molysite::compiler::compile;
use molysite::closure::Closure;
use molysite::verifier::verify;
use molysite::oref::{ORef, Gc, Fixnum};
use molysite::symbol::Symbol;
use molysite::vector::Vector;
use molysite::heap_obj::{Singleton, Indexed};
use molysite::list::{Pair, EmptyList};

fn eval_string(mt: &mut Mutator, s: &str) -> ORef {
    let mut reader = Reader::new(s, None);

    let res = reader.next(mt).unwrap();
    let sv = res.unwrap();
    let code = compile(mt, (*sv).into(), mt.cfg().debug);

    unsafe { verify(&mt, code.as_ref()) }.unwrap();

    mt.push(code.into());
    let f = Closure::new(mt, 0);
    mt.pop();
    mt.push(f.into());
    let v = *mt.invoke().get(0).unwrap();

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
                    assert_eq!(pair1.as_ref().car(), pair2.as_ref().car());

                    ls1 = pair1.as_ref().cdr();
                    ls2 = pair2.as_ref().cdr();
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
    let quote = Symbol::new(&mut mt, "quote");
    let quote = mt.root(quote.into());
    let plus = Symbol::new(&mut mt, "+");
    let plus = mt.root(plus.into());
    let a = Symbol::new(&mut mt, "a");
    let a = mt.root(a.into());
    let one = mt.root(Fixnum::from(1u8).into());
    let two = mt.root(Fixnum::from(2u8).into());
    let empty_list = EmptyList::instance(&mt);
    let empty_list = mt.root(empty_list.into());

    assert_eq!(eval_string(&mut mt, "(quote a)"), (*a).into());

    let v1 = eval_string(&mut mt, "(quote #(a b c))");
    let v2 = eval_string(&mut mt, "#(a b c)");
    assert_vector_equal(&mt, v1, v2);

    let ls1 = eval_string(&mut mt, "(quote (+ 1 2))");
    let ls2 = Gc::<Pair>::new(&mut mt, two.clone(), empty_list.clone());
    let ls2 = mt.root(ls2.into());
    let ls2 = Gc::<Pair>::new(&mut mt, one.clone(), ls2);
    let ls2 = mt.root(ls2.into());
    let ls2 = Gc::<Pair>::new(&mut mt, plus.clone(), ls2);
    assert_list_equal(&mt, ls1, ls2.into());

    assert_eq!(eval_string(&mut mt, "'a"), (*a).into());

    let v1 = eval_string(&mut mt, "'#(a b c)");
    let v2 = eval_string(&mut mt, "#(a b c)");
    assert_vector_equal(&mt, v1, v2);

    assert_eq!(eval_string(&mut mt, "'()"), EmptyList::instance(&mt).into());

    let ls1 = eval_string(&mut mt, "'(+ 1 2)");
    let ls2 = Gc::<Pair>::new(&mut mt, two, empty_list.clone());
    let ls2 = mt.root(ls2.into());
    let ls2 = Gc::<Pair>::new(&mut mt, one, ls2);
    let ls2 = mt.root(ls2.into());
    let ls2 = Gc::<Pair>::new(&mut mt, plus, ls2);
    assert_list_equal(&mt, ls1, ls2.into());

    let ls1 = eval_string(&mut mt, "'(quote a)");
    let ls2 = Gc::<Pair>::new(&mut mt, a.clone(), empty_list.clone());
    let ls2 = mt.root(ls2.into());
    let ls2 = Gc::<Pair>::new(&mut mt, quote.clone(), ls2);
    assert_list_equal(&mt, ls1, ls2.into());

    let ls1 = eval_string(&mut mt, "''a");
    let ls2 = Gc::<Pair>::new(&mut mt, a, empty_list);
    let ls2 = mt.root(ls2.into());
    let ls2 = Gc::<Pair>::new(&mut mt, quote, ls2);
    assert_list_equal(&mt, ls1, ls2.into());
}

#[test]
fn test_lambda() {
    let mut mt = Mutator::new(1 << 20, false).unwrap();

    let f = eval_string(&mut mt, "(lambda () 42)");
    assert!(f.instance_of::<Closure>(&mt));

    assert_eq!(eval_string(&mut mt, "((lambda () 42))"), Fixnum::try_from(42isize).unwrap().into());

    assert_eq!(eval_string(&mut mt, "((lambda (x) x) 42)"), Fixnum::try_from(42isize).unwrap().into());

    let ls1 = eval_string(&mut mt, "((lambda ls ls) 1 2 3)");
    let ls2 = eval_string(&mut mt, "'(1 2 3)");
    assert_list_equal(&mt, ls1, ls2);

    let ls1 = eval_string(&mut mt, "((lambda (x . ls) ls) 1 2 3)");
    let ls2 = eval_string(&mut mt, "'(2 3)");
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

    assert_eq!(eval_string(&mut mt, "(letrec ((x 0)) (begin (set! x 1) x))"),
        Fixnum::try_from(1isize).unwrap().into());
}

#[test]
fn test_begin() {
    let mut mt = Mutator::new(1 << 20, false).unwrap();

    assert_eq!(eval_string(&mut mt, "(begin 42)"), Fixnum::try_from(42isize).unwrap().into());

    assert_eq!(eval_string(&mut mt, "(letrec ((x 0)) (begin (set! x 1) x))"),
        Fixnum::try_from(1isize).unwrap().into());
}

#[test]
fn test_letrec() {
    let mut mt = Mutator::new(1 << 20, false).unwrap();

    assert_eq!(eval_string(&mut mt, "(letrec ((ans 42)) ans)"), Fixnum::try_from(42isize).unwrap().into());

    assert!(eval_string(&mut mt, "(letrec ((forever (lambda () (forever)))) forever)").instance_of::<Closure>(&mt));

    assert_eq!(eval_string(&mut mt,
        "(letrec ((fact (lambda (n) (if (eq? n 0) 1 (fx* n (fact (fx- n 1))))))) (fact 5))"),
        Fixnum::try_from(120isize).unwrap().into());
}
