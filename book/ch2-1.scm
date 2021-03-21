(use compat.sdff)

;;;
;;; 2.1 Combinators
;;;

(define (compose f g)
  (define (the-composition . args)
    (f (apply g args)))
  the-composition)

(example
 ((compose (lambda (x) (list 'foo x))
           (lambda (x) (list 'bar x)))
  'z)
 => (foo (bar z))
 )


(define ((iterate n) f)
  (if (= n 0)
    identity
    (compose f ((iterate (- n 1)) f))))

(define (identity x) x)

(example
 (((iterate 3) square) 5)
 => 390625
 )

(define (parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  the-combination)

(example
 ((parallel-combine list
                    (lambda (x y z) (list 'foo x y z))
                    (lambda (u v w) (list 'bar u v w)))
  'a 'b 'c)
 => ((foo a b c) (bar a b c))
 )


(define (spread-combine h f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (h (apply f (list-head args n))
           (apply g (list-tail args n))))
      (restrict-arity the-combination t))))

(define (restrict-arity proc nargs)
  (hash-table-set! arity-table proc nargs)
  proc)

(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (let ((a (procedure-arity proc)))
        (assert (eqv? (procedure-arity-min a)
                      (procedure-arity-max a)))
        (procedure-arity-min a))))

(define arity-table (make-key-weak-eqv-hash-table))

(example
 ((spread-combine list
                  (lambda (x y) (list 'foo x y))
                  (lambda (u v w) (list 'bar u v w)))
  'a 'b 'c 'd 'e)
 => ((foo a b) (bar c d e))
 )


(define (spread-apply f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (values (apply f (list-head args n))
                (apply g (list-tail args n))))
      (restrict-arity the-combination t))))

(define (compose f g)                   ; redefinition
  (define (the-composition . args)
    (call-with-values (lambda () (apply g args))
      f))
  (restrict-arity the-composition (get-arity g)))

(define (spread-combine h f g)          ; redefinition
  (compose h (spread-apply f g)))

(example
 ((spread-combine list
                  (lambda (x y) (list 'foo x y))
                  (lambda (u v w) (list 'bar u v w)))
  'a 'b 'c 'd 'e)
 => ((foo a b) (bar c d e))
 )


(define (spread-apply f g)              ; redefinition
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (let-values ((fv (apply f (list-head args n)))
                     (gv (apply g (list-tail args n))))
          (apply values (append fv gv))))
      (restrict-arity the-combination t))))

(example
 ((spread-combine list
                  (lambda (x y) (values x y))
                  (lambda (u v w) (values w v u)))
  'a 'b 'c 'd 'e)
 => (a b e d c)
 )


(define (discard-argument i)
  (assert (exact-nonnegative-integer? i))
  (lambda (f)
    (let ((m (+ (get-arity f) 1)))
      (define (the-combination . args)
        (assert (= (length args) m))
        (apply f (list-remove args i)))
      (assert (< i m))
      (restrict-arity the-combination m))))

(define (list-remove lst index)
  (let lp ((lst lst) (index index))
    (if (= index 0)
      (cdr lst)
      (cons (car lst) (lp (cdr lst) (- index 1))))))

(example
 (((discard-argument 2)
   (lambda (x y z) (list 'foo x y z)))
  'a 'b 'c 'd)
 => (foo a b d)
 )


(define ((curry-argument i) . args)
  (lambda (f)
    (assert (= (length args) (- (get-arity f) 1)))
    (lambda (x)
      (apply f (list-insert args i x)))))

(define (list-insert lst index value)
  (let lp ((lst lst) (index index))
    (if (= index 0)
      (cons value lst)
      (cons (car lst) (lp (cdr lst) (- index 1))))))

(example
 ((((curry-argument 2) 'a 'b 'c)
   (lambda (x y z w) (list 'foo x y z w)))
  'd)
 => (foo a b d c)
 )


(define (permute-arguments . permspec)
  (let ((permute (make-permutation permspec)))
    (lambda (f)
      (define (the-combination . args)
        (apply f (permute args)))
      (let ((n (get-arity f)))
        (assert (= n (length permspec)))
        (restrict-arity the-combination n)))))

(define (make-permutation permspec)
  (define (the-permuter lst)
    (map (lambda (p) (list-ref lst p))
         permspec))
  the-permuter)

(example
 (((permute-arguments 1 2 0 3)
   (lambda (x y z w) (list 'foo x y z w)))
  'a 'b 'c 'd)
 => (foo b c a d)
 )
