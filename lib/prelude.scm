(define call-with-values (lambda (producer consumer) (call-with-values* producer consumer)))

(define + fx+)
(define - fx-)

(define zero? (lambda (n) (eq? n 0)))

(define not (lambda (obj) (if obj #f #t)))

(define list (lambda ls ls))

(define fold
  (lambda (proc acc list1)
    (letrec ((fold (lambda (acc ls)
                     (if (pair? ls)
                       (fold (proc (car ls) acc) (cdr ls))
                       (if (null? ls)
                         acc
                         (error "fold: improper list" list1))))))
      (fold acc list1))))

(define fold-right
  (lambda (proc acc list1)
    (letrec ((fold-right (lambda (ls)
                           (if (pair? ls)
                             (proc (car ls) (fold-right (cdr ls)))
                             (if (null? ls)
                               acc
                               (error "fold-right: improper list" list1))))))
      (fold-right list1))))

(define map
  (lambda (proc list1)
    (if (pair? list1)
      (letrec ((ls* (cons (proc (car list1)) '()))
               (map-tail! (lambda (ls last-pair)
                            (if (pair? ls)
                              (letrec ((pair (cons (proc (car ls)) '())))
                                (begin
                                  (set-cdr! last-pair pair)
                                  (map-tail! (cdr ls) pair)))
                              (if (null? ls)
                                ls*
                                (error "map: improper list" list1))))))
        (map-tail! (cdr list1) ls*))
      (if (null? list1)
        list1
        (error "map: improper list" list1)))))

(define for-each
  (lambda (proc list1)
    (letrec ((for-each (lambda (ls)
                         (if (pair? ls)
                           (begin
                             (proc (car ls))
                             (for-each (cdr ls)))
                           (if (null? ls)
                             ls
                             (error "for-each: improper list" list1))))))
      (for-each list1))))

(define length (lambda (list) (fold (lambda (_ len) (+ len 1)) 0 list)))

(define append (lambda (list1 list2) (fold-right cons list2 list1)))

(define reverse (lambda (list1) (fold cons '() list1)))

(define list-tail
  (lambda (list k)
    (letrec ((list-tail (lambda (ls i)
                          (if (zero? i)
                            ls
                            (if (pair? ls)
                              (list-tail (cdr ls) (- i 1))
                              (if (null? ls)
                                (error "list-tail: out of bounds" list k)
                                (error "list-tail: improper list" list)))))))
      (list-tail list k))))

(define list-ref
  (lambda (list k)
    (letrec ((tail (list-tail list k)))
      (if (pair? tail)
        (car tail)
        (error "list-ref: out of bounds" list k)))))

(define list-set!
  (lambda (list k obj)
    (letrec ((tail (list-tail list k)))
      (if (pair? tail)
        (set-car! tail obj)
        (error "list-set!: out of bounds" list k)))))
