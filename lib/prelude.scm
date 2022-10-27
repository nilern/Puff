(define not (lambda (obj) (eq? obj #f)))

(define list (lambda ls ls))

(define map
  (lambda (proc list1)
    (if (pair? list1)
      (letrec ((map-tail! (lambda (ls last-pair)
                            (if (pair? ls)
                              (letrec ((pair (cons (proc (car ls)) '())))
                                (begin
                                  (set-cdr! last-pair (proc (car ls)))
                                  (map-tail! (cdr ls) pair)))
                              (if (null? ls)
                                ls
                                (error "map: improper list" list1)))))
               (ls* (cons (proc (car list1)) '())))
        (begin
          (map-tail! (cdr list1) ls*)
          ls*))
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
