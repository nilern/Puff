(define pair? (lambda (obj) (instance? <pair> obj)))

(define null? (lambda (obj) (eq? obj '())))

(define list?
  (case-lambda
    (((: pair <pair>)) (list? (cdr pair)))
    (((: _ <empty-list>)) #t)
    ((_) #f)))

(define cons (lambda (car cdr) (make <pair> car cdr)))

(define list (lambda ls ls))

(define car (lambda ((: pair <pair>)) (field-ref pair 0)))

(define cdr (lambda ((: pair <pair>)) (field-ref pair 1)))

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

(define set-car! (lambda ((: pair <pair>) car*) (field-set! pair 0 car*)))

(define set-cdr! (lambda ((: pair <pair>) cdr*) (field-set! pair 1 cdr*)))

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

(define append
  (case-lambda
    (() '())

    ((list) list)

    ((list1 list2)
     (if (pair? list1)
       (letrec ((ls* (cons (car list1) '()))
                (append! (lambda (ls last-pair)
                             (if (pair? ls)
                               (letrec ((pair (cons (car ls) '())))
                                 (begin
                                   (set-cdr! last-pair pair)
                                   (append! (cdr ls) pair)))
                               (if (null? ls)
                                 (begin
                                   (set-cdr! last-pair list2)
                                   ls*)
                                 (error "append: improper list" list1))))))
         (append! (cdr list1) ls*))
       (if (null? list1)
         list2
         (error "append: not a list" list1))))

    (lists
     (letrec ((rev-lists (reverse lists)))
       (fold append (car rev-lists) (cdr rev-lists))))))

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

(define member
  (case-lambda
    ((obj list) (member obj list equal?))

    ((obj list compare)
     (letrec ((memq (lambda (ls)
                      (if (pair? ls)
                        (if (compare (car ls) obj)
                          ls
                          (memq (cdr ls)))
                        (if (null? ls)
                          #f
                          (error "member: improper list" list))))))
       (memq list)))))

(define memq (lambda (obj list) (member obj list eq?)))

(define assoc
  (case-lambda
    ((obj alist) (assoc obj alist equal?))

    ((obj alist compare)
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
       (assoc alist)))))

(define assq (lambda (obj alist) (assoc obj alist eq?)))
