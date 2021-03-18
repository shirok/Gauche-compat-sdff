(define-module compat.sdff
  (use util.match)
  (use scheme.hash-table :only (hash-table-ref/default))
  (use scheme.list)

  (export list-head list-tail
          procedure-arity procedure-arity-min procedure-arity-max
          make-key-weak-eqv-hash-table assert
          exact-nonnegative-integer?
          hash-table-ref/default        ;scheme.hash-table
          lset=                         ;scheme.list
          make-unit-conversion unit:invert

          example))
(select-module compat.sdff)

;; Ch2

(define (list-head lis n) (take lis n))
(define (list-tail lis n) (drop lis n))

(define (procedure-arity proc) (arity proc))

(define (procedure-arity-min a)
  (cond [(arity-at-least? a) (arity-at-least-value a)]
        [(integer? a) a]
        [(list? a) (apply min (map procedure-arity-min a))]))

(define (procedure-arity-max a)
  (define (rec a)
    (cond [(arity-at-least? a) +inf.0]
          [(integer? a) a]
          [(list? a) (apply max (map rec a))]))
  (let1 r (rec a)
    (if (infinite? r) #f r)))

;; not a weak table but enough to run examples
(define (make-key-weak-eqv-hash-table)
  (make-hash-table eqv-comparator))

(define-syntax assert
  (syntax-rules ()
    [(_ expr) (unless expr (error "Assertion failed" 'expr))]))

(define (exact-nonnegative-integer? n)
  (and (exact-integer? n) (>= n 0)))

(define *inverse-mark* (gensym "inverse"))

(define (make-unit-conversion forward inverse)
  (define (the-converter msg)
    (if (eq? msg *inverse-mark*)
      inverse
      (forward msg)))
  the-converter)

(define (unit:invert converter)
  (converter *inverse-mark*))

;; Syntax to mark examples.

(define-syntax example
  (syntax-rules (=>)
    ((_ example => expect ...)
     (begin
       (print ";; Example =======================================")
       (pprint 'example)
       (flush)
       (print ";; =>")
       (let1 rs (values->list example)
         (for-each (^[r] (write r) (newline)) rs)
         (unless (every example-compare rs '(expect ...))
           (error "Result differ from expectation" '(expect ...))))
       (print)))))

(define (example-compare a b)
  (if (or (inexact? a) (inexact? b))
    (approx=? a b 1e-5)
    (equal? a b)))
