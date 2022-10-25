(define list (lambda ls ls))

(define map
  (lambda (proc list1)
    (letrec ((map (lambda (f ls)
                    (if (pair? ls)
                      (cons (f (car ls))
                            (map f (cdr ls)))
                      (if (null? ls)
                        ls
                        (error "map: improper list" list1))))))
      (map proc list1))))
