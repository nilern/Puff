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

(define + fx+)
(define - fx-)
(define * fx*)

(define > fx>)

(define zero? (lambda (n) (eq? n 0)))
