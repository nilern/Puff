(define number?
  (case-lambda
    (((: _ <fixnum>)) #t)
    (((: _ <flonum>)) #t)
    ((_) #f)))

(define complex? (lambda (obj) (number? obj)))

(define real?
  (case-lambda
    (((: _ <fixnum>)) #t)
    (((: _ <flonum>)) #t)
    ((_) #f)))

(define rational?
  (case-lambda
    (((: _ <fixnum>)) #t)
    (((: obj <flonum>)) (flfinite? obj))
    ((_) #f)))

(define integer?
  (case-lambda
    (((: _ <fixnum>)) #t)
    (((: obj <flonum>)) (flinteger? obj))
    ((_) #f)))

(define exact?
  (case-lambda
    (((: _ <fixnum>)) #t)
    (((: _ <flonum>)) #f)))

(define inexact?
  (case-lambda
    (((: _ <fixnum>)) #f)
    (((: _ <flonum>)) #t)))

(define comparer
  (lambda (proc)
    (case-lambda
      ((a b) (proc a b))
      ((a b . cs)
       (if (proc a b)
         (letrec ((compare (lambda (obj ls)
                             (if (pair? ls)
                               (letrec ((obj* (car ls)))
                                 (if (proc obj obj*)
                                   (compare obj* (cdr ls))
                                   #f))
                               #t))))
           (compare b cs))
         #f)))))

(define =
  (comparer
    (case-lambda
      (((: a <fixnum>) (: b <fixnum>)) (eq? a b))
      (((: a <flonum>) (: b <flonum>)) (eq? a b))
      (((: a <fixnum>) (: b <flonum>)) (eq? (fixnum->flonum a) b))
      (((: a <flonum>) (: b <fixnum>)) (eq? a (fixnum->flonum b))))))

(define >
  (comparer
    (case-lambda
      (((: a <fixnum>) (: b <fixnum>)) (fx>? a b))
      (((: a <flonum>) (: b <flonum>)) (fl>? a b))
      (((: a <fixnum>) (: b <flonum>)) (fl>? (fixnum->flonum a) b))
      (((: a <flonum>) (: b <fixnum>)) (fl>? a (fixnum->flonum b))))))

(define >=
  (comparer
    (case-lambda
      (((: a <fixnum>) (: b <fixnum>)) (fx>=? a b))
      (((: a <flonum>) (: b <flonum>)) (fl>=? a b))
      (((: a <fixnum>) (: b <flonum>)) (fl>=? (fixnum->flonum a) b))
      (((: a <flonum>) (: b <fixnum>)) (fl>=? a (fixnum->flonum b))))))

(define <
  (comparer
    (case-lambda
      (((: a <fixnum>) (: b <fixnum>)) (fx<? a b))
      (((: a <flonum>) (: b <flonum>)) (fl<? a b))
      (((: a <fixnum>) (: b <flonum>)) (fl<? (fixnum->flonum a) b))
      (((: a <flonum>) (: b <fixnum>)) (fl<? a (fixnum->flonum b))))))

(define <=
  (comparer
    (case-lambda
      (((: a <fixnum>) (: b <fixnum>)) (fx<=? a b))
      (((: a <flonum>) (: b <flonum>)) (fl<=? a b))
      (((: a <fixnum>) (: b <flonum>)) (fl<=? (fixnum->flonum a) b))
      (((: a <flonum>) (: b <fixnum>)) (fl<=? a (fixnum->flonum b))))))

(define + fx+)
(define - fx-)
(define * fx*)

(define zero? (lambda (n) (= n 0)))
(define positive? (lambda (n) (> n 0)))
(define negative? (lambda (n) (< n 0)))
