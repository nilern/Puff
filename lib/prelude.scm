(define call-with-values (lambda (producer consumer) (call-with-values* producer consumer)))

(define + fx+)
(define - fx-)

(define zero? (lambda (n) (eq? n 0)))

(define not (lambda (obj) (if obj #f #t)))

(define pair? (lambda (obj) (instance? <pair> obj)))

(define null? (lambda (obj) (eq? obj '())))

(define list?
  (lambda (obj)
    (if (pair? obj)
      (list? (cdr obj))
      (null? obj))))

(define cons (lambda (car cdr) (make <pair> car cdr)))

(define list (lambda ls ls))

(define car
  (lambda (pair)
    (if (pair? pair)
      (field-ref pair 0)
      (error "car: non-pair" pair))))

(define cdr
  (lambda (pair)
    (if (pair? pair)
      (field-ref pair 1)
      (error "cdr: non-pair" pair))))

(define caar (lambda (obj) (car (car obj))))
(define cadr (lambda (obj) (car (cdr obj))))
(define cdar (lambda (obj) (cdr (car obj))))
(define cddr (lambda (obj) (cdr (cdr obj))))

(define caaar (lambda (obj) (car (car (car obj)))))
(define caadr (lambda (obj) (car (car (cdr obj)))))
(define cadar (lambda (obj) (car (cdr (car obj)))))
(define caddr (lambda (obj) (car (cdr (cdr obj)))))
(define cdaar (lambda (obj) (cdr (car (car obj)))))
(define cdadr (lambda (obj) (cdr (car (cdr obj)))))
(define cddar (lambda (obj) (cdr (cdr (car obj)))))
(define cdddr (lambda (obj) (cdr (cdr (cdr obj)))))

(define caaaar (lambda (obj) (car (car (car (car obj))))))
(define caaadr (lambda (obj) (car (car (car (cdr obj))))))
(define caadar (lambda (obj) (car (car (cdr (car obj))))))
(define caaddr (lambda (obj) (car (car (cdr (cdr obj))))))
(define cadaar (lambda (obj) (car (cdr (car (car obj))))))
(define cadadr (lambda (obj) (car (cdr (car (cdr obj))))))
(define caddar (lambda (obj) (car (cdr (cdr (car obj))))))
(define cadddr (lambda (obj) (car (cdr (cdr (cdr obj))))))
(define cdaaar (lambda (obj) (cdr (car (car (car obj))))))
(define cdaadr (lambda (obj) (cdr (car (car (cdr obj))))))
(define cdadar (lambda (obj) (cdr (car (cdr (car obj))))))
(define cdaddr (lambda (obj) (cdr (car (cdr (cdr obj))))))
(define cddaar (lambda (obj) (cdr (cdr (car (car obj))))))
(define cddadr (lambda (obj) (cdr (cdr (car (cdr obj))))))
(define cdddar (lambda (obj) (cdr (cdr (cdr (car obj))))))
(define cddddr (lambda (obj) (cdr (cdr (cdr (cdr obj))))))

(define set-car!
  (lambda (pair car*)
    (if (pair? pair)
      (field-set! pair 0 car*)
      (error "set-car!: non-pair" pair))))

(define set-cdr!
  (lambda (pair cdr*)
    (if (pair? pair)
      (field-set! pair 1 cdr*)
      (error "set-cdr!: non-pair" pair))))

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

(define member
  (lambda (obj list compare)
    (letrec ((memq (lambda (ls)
                     (if (pair? ls)
                       (if (compare (car ls) obj)
                         ls
                         (memq (cdr ls)))
                       (if (null? ls)
                         #f
                         (error "member: improper list" list))))))
      (memq list))))

(define memq (lambda (obj list) (member obj list eq?)))

(define assoc
  (lambda (obj alist compare)
    (letrec ((assoc (lambda (als)
                      (if (pair? als)
                        (letrec ((entry (car als)))
                          (if (pair? entry)
                            (if (compare (car entry) obj)
                              entry
                              (assoc (cdr als)))
                            (error "assoc: non-pair list element" entry alist)))
                        (if (null? als)
                          #f
                          (error "assoc: improper list" alist))))))
      (assoc alist))))

(define assq (lambda (obj alist) (assoc obj alist eq?)))

(define list-copy
  (lambda (list)
    (if (pair? list)
      (letrec ((ls* (cons (car list) '()))
               (copy-tail! (lambda (ls last-pair)
                            (if (pair? ls)
                              (letrec ((pair (cons (car ls) '())))
                                (begin
                                  (set-cdr! last-pair pair)
                                  (copy-tail! (cdr ls) pair)))
                              (begin
                                (set-cdr! last-pair ls)
                                ls*)))))
        (copy-tail! (cdr list) ls*))
      (if (null? list)
        list
        (error "list-copy: not a list" list)))))

(define vector? (lambda (obj) (if (instance? <vector> obj) #t (instance? <vector-mut> obj))))

(define make-vector
  (lambda (k)
    (make-indexed-zeroed <vector-mut> k)))

(define vector-length
  (lambda (vector)
    (if (vector? vector)
      (indexed-length vector)
      (error "vector-length: non-vector" vector))))

(define vector-ref
  (lambda (vector k)
    (if (vector? vector)
      (indexed-ref vector k)
      (error "vector-ref: non-vector" vector))))

(define vector-set!
  (lambda (vector k obj)
    (if (instance? <vector-mut> vector)
      (indexed-set! vector k obj)
      (if (instance? <vector> vector)
        (error "vector-set!: immutable vector" vector)
        (error "vector-set!: non-vector" vector)))))

(define vector (lambda vs (apply make <vector-mut> vs)))

(define vector-unfold
  (lambda (f length seed)
    (letrec ((vector (make-vector length))
             (unfold (lambda (i seed)
                       (if (not (eq? i length))
                         (call-with-values (lambda () (f i seed))
                                           (lambda (v seed)
                                             (begin
                                               (vector-set! vector i v)
                                               (unfold (+ i 1) seed))))
                         vector))))
      (unfold 0 seed))))

(define vector-fold
  (lambda (proc acc vector)
    (letrec ((len (vector-length vector))
             (vector-fold (lambda (i acc)
                            (if (not (eq? i len))
                              (vector-fold (+ i 1) (proc i acc (vector-ref vector i)))
                              acc))))
      (vector-fold 0 acc))))

(define vector-fold-right
  (lambda (proc acc vector)
    (letrec ((vector-fold-right (lambda (i acc)
                                  (if (not (eq? i 0))
                                    (letrec ((i* (- i 1)))
                                      (vector-fold-right i* (proc i* acc (vector-ref vector i*))))
                                    acc))))
      (vector-fold-right (vector-length vector) acc))))

(define vector->list
  (lambda (vector)
    (vector-fold-right (lambda (_ list v) (cons v list)) '() vector)))

(define list->vector
  (lambda (list)
    (vector-unfold (lambda (_ ls) (values (car ls) (cdr ls)))
                   (length list)
                   list)))
