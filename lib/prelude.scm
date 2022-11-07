(define call-with-values (lambda (producer consumer) (call-with-values* producer consumer)))

(define + fx+)
(define - fx-)

(define zero? (lambda (n) (eq? n 0)))

(define not (lambda (obj) (if obj #f #t)))

(define extends?
  (lambda (super sub)
    (if (eq? sub super)
      #t
      (letrec ((sub* (supertype sub)))
        (if sub*
          (extends? super sub*)
          #f)))))

(define instance? (lambda (type obj) (extends? type (type-of obj))))

(define null? (lambda (obj) (eq? obj '())))

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
