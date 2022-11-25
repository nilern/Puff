(load "lib/list.scm")

(load "lib/arithmetic.scm")

(define call-with-values (lambda (producer consumer) (call-with-values* producer consumer)))

(define winders '())

(define call-with-current-continuation
  (letrec ((call-cc call-with-current-continuation)

           (common-tail
            (lambda (list1 list2)
              (letrec ((len1 (length list1))
                       (len2 (length list2))
                       (common-tail (lambda (ls1 ls2)
                                      (if (not (eq? ls1 ls2))
                                        (common-tail (cdr ls1) (cdr ls2))
                                        ls1))))
                (common-tail (if (> len1 len2) (list-tail list1 (- len1 len2)) list1)
                             (if (> len2 len1) (list-tail list2 (- len2 len1)) list2)))))

           (do-wind
            (lambda (winders*)
              (letrec ((common (common-tail winders* winders)))
                (begin
                  (letrec ((wind-out (lambda (ls)
                                       (if (not (eq? ls common))
                                         (begin
                                           (set! winders (cdr ls))
                                           ((cdar ls))
                                           (wind-out (cdr ls)))
                                         '()))))
                    (wind-out winders))
                  (letrec ((wind-in (lambda (ls)
                                      (if (not (eq? ls common))
                                        (begin
                                          (wind-in (cdr ls))
                                          ((caar ls))
                                          (set! winders ls))
                                        '()))))
                    (wind-in winders*)))))))
    (lambda (proc)
      (call-cc (lambda (k)
                 (letrec ((saved-winders winders))
                   (proc (lambda vs
                           (begin
                             (if (eq? winders saved-winders)
                               '()
                               (do-wind saved-winders))
                             (apply continue k vs))))))))))

(define dynamic-wind
  (lambda (in body out)
    (begin
      (in)
      (set! winders (cons (cons in out) winders))
      (call-with-values body
                        (lambda vs
                          (begin
                            (set! winders (cdr winders))
                            (out)
                            (apply values vs)))))))

(define not (lambda (obj) (if obj #f #t)))


(define vector? (lambda (obj) (if (instance? <vector> obj) #t (instance? <vector-mut> obj))))

(define make-vector (lambda (k) (make-indexed-zeroed <vector-mut> k)))

(define vector-length
  (case-lambda
    (((: vector <vector>)) (indexed-length vector))
    (((: vector <vector-mut>)) (indexed-length vector))))

(define vector-ref
  (case-lambda
    (((: vector <vector>) k) (indexed-ref vector k))
    (((: vector <vector-mut>) k) (indexed-ref vector k))))

(define vector-set! (lambda ((: vector <vector-mut>) k obj) (indexed-set! vector k obj)))

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

(define vector-fill! (lambda ((: vector <vector-mut>) fill) (indexed-fill! vector fill)))

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
  (case-lambda
    (((: string <string>)) (field-ref string 0))
    (((: string <string-mut>)) (field-ref string 0))))

(define string-byte-length
  (case-lambda
    (((: string <string>)) (indexed-length string))
    (((: string <string-mut>)) (field-ref string 1))))

(define string-mut-bytes (lambda ((: string <string-mut>)) (field-ref string 2)))

(define string-ref
  (letrec ((string-immut-ref string-ref))
    (case-lambda
      (((: string <string>) k) (string-immut-ref string k))
      (((: string <string-mut>) k) (string-mut-ref string k)))))

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
  (case-lambda
    ((bytes at (: string <string>)) (indexed-copy! bytes at string 0 (string-byte-length string)))
    ((bytes at (: string <string-mut>))
     (letrec ((string-bytes (string-mut-bytes string)))
       (indexed-copy! bytes at string-bytes 0 (bytevector-length string-bytes))))))

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

(define +o-rdonly+ 0)
(define +o-wronly+ 1)

(define open-input-file (lambda (filename) (open-file filename +o-rdonly+)))
(define open-output-file (lambda (filename) (open-file filename +o-wronly+)))

(define close-input-port (lambda (port) (close-port port)))
(define close-output-port (lambda (port) (close-port port)))
