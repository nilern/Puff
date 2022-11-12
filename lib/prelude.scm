(define call-with-values (lambda (producer consumer) (call-with-values* producer consumer)))

(define + fx+)
(define - fx-)
(define * fx*)

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

(define append
  (lambda (list1 list2)
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
        (error "append: not a list" list1)))))

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

(define vector? (lambda (obj) (if (instance? <vector> obj) #t (instance? <vector-mut> obj))))

(define make-vector (lambda (k) (make-indexed-zeroed <vector-mut> k)))

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

(define vector->list (lambda (vector) (vector-fold-right (lambda (_ list v) (cons v list)) '() vector)))

(define list->vector
  (lambda (list)
    (vector-unfold (lambda (_ ls) (values (car ls) (cdr ls)))
                   (length list)
                   list)))

(define vector-fill!
  (lambda (vector fill)
    (if (instance? <vector-mut> vector)
      (indexed-fill! vector fill)
      (if (instance? <vector> vector)
        (error "vector-fill!: immutable vector" vector)
        (error "vector-fill!: non-vector" vector)))))

(define bytevector? (lambda (obj) (instance? <bytevector-mut> obj)))

(define make-bytevector (lambda (k) (make-indexed-zeroed <bytevector-mut> k)))

(define bytevector-length
  (lambda (bytevector)
    (if (bytevector? bytevector)
      (indexed-length bytevector)
      (error "bytevector-length: non-bytevector" bytevector))))

(define string? (lambda (obj) (if (instance? <string> obj) #t (instance? <string-mut> obj))))

(define make-string (lambda (k) (make <string-mut> k k (make-bytevector k))))

(define string-length
  (lambda (string)
    (if (instance? <string> string)
      (field-ref string 0)
      (if (instance? <string-mut> string)
        (field-ref string 0)
        (error "string-length: non-string" string)))))

(define string-byte-length
  (lambda (string)
    (if (instance? <string> string)
      (indexed-length string)
      (if (instance? <string-mut> string)
        (field-ref string 1)
        (error "string-byte-length: non-string" string)))))

(define string-mut-bytes
  (lambda (string)
    (if (instance? <string-mut> string)
      (field-ref string 2)
      (if (instance? <string> string)
        (error "string-byte-length: immutable string" string)
        (error "string-byte-length: non-string" string)))))

(define string-ref
  (letrec ((string-immut-ref string-ref))
    (lambda (string k)
      (if (instance? <string> string)
        (string-immut-ref string k)
        (if (instance? <string-mut> string)
          (string-mut-ref string k)
          (error "string-ref: non-string" string))))))

(define string-fold-right
  (lambda (proc acc vector)
    (letrec ((string-fold-right (lambda (i acc)
                                  (if (not (eq? i 0))
                                    (letrec ((i* (- i 1)))
                                      (string-fold-right i* (proc i* acc (string-ref vector i*))))
                                    acc))))
      (string-fold-right (string-length vector) acc))))

(define string->list (lambda (string) (string-fold-right (lambda (_ list c) (cons c list)) '() string)))

(define list->string
  (letrec ((chars-lengths
            (lambda (chars)
              (letrec ((chars-lengths (lambda (chars char-len byte-len)
                                        (if (pair? chars)
                                          (letrec ((string (car chars)))
                                            (chars-lengths (cdr chars)
                                                           (+ char-len 1)
                                                           (+ byte-len (char-length-utf8 string))))
                                          (values char-len byte-len)))))
                (chars-lengths chars 0 0)))))
    (lambda (list)
      (call-with-values (lambda () (chars-lengths list))
                        (lambda (char-len byte-len)
                          (letrec ((bytes (make-bytevector byte-len)))
                            (begin
                              (fold (lambda (char at)
                                      (begin
                                        (indexed-char-utf8-set! bytes at char)
                                        (+ at (char-length-utf8 char))))
                                    0 list)
                              (make <string-mut> char-len byte-len bytes))))))))

(define substring
  (lambda (string start end)
    (letrec ((byte-len (string-byte-length string)))
      (letrec ((bytes (make-bytevector byte-len)))
        (begin
          (if (instance? <string> string)
            (indexed-copy! bytes 0 string start end)
            (if (instance? <string-mut> string)
              (indexed-copy! bytes 0 (string-mut-bytes string) start end)
              (error "substring: non-string" string)))
          (make <string-mut> (- end start) byte-len bytes))))))

(define string-bytevector-copy!
  (lambda (bytes at string)
    (if (instance? <string> string)
      (indexed-copy! bytes at string 0 (string-byte-length string))
      (if (instance? <string-mut> string)
        (letrec ((string-bytes (string-mut-bytes string)))
          (indexed-copy! bytes at string-bytes 0 (bytevector-length string-bytes)))
        (error "string-append: non-string" string)))))

(define string-append
  (letrec ((strings-lengths
            (lambda (strings)
              (letrec ((strings-lengths (lambda (strings char-len byte-len)
                                          (if (pair? strings)
                                            (letrec ((string (car strings)))
                                              (strings-lengths (cdr strings)
                                                               (+ char-len (string-length string))
                                                               (+ byte-len (string-byte-length string))))
                                            (values char-len byte-len)))))
                (strings-lengths strings 0 0)))))
    (lambda strings
      (call-with-values (lambda () (strings-lengths strings))
                        (lambda (char-len byte-len)
                          (letrec ((bytes (make-bytevector byte-len)))
                            (begin
                              (fold (lambda (string at)
                                      (begin
                                        (string-bytevector-copy! bytes at string)
                                        (+ at (string-byte-length string))))
                                    0 strings)
                              (make <string-mut> char-len byte-len bytes))))))))

(define string-copy
  (lambda (string)
    (letrec ((byte-len (string-byte-length string)))
      (letrec ((bytes (make-bytevector byte-len)))
        (begin
          (string-bytevector-copy! bytes 0 string)
          (make <string-mut> (string-length string) byte-len bytes))))))
