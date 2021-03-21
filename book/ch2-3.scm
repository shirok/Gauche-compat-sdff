(use compat.sdff)

;; Exercise 2.5 c
(define (compose . args)                ; redefinitoin
  (define (compose2 f g)
    (lambda args
      (call-with-values (lambda () (apply g args))
        f)))
  (cond ((null? args) values)
        ((null? (cdr args)) (car args))
        (else (let lp ((f (car args)) (g (cadr args)) (rest (cddr args)))
                (if (null? rest)
                  (let ((r (compose2 f g)))
                    (restrict-arity r g)
                    r)
                  (lp (compose2 f g) (car rest) (cdr rest)))))))

;;;
;;; 2.3 Wrappers
;;;

(define (gas-law-volume pressure temperature amount)
  (/ (* amount gas-constant temperature) pressure))

(define gas-constant 8.3144621)         ;J/(K*mol)

(define (sphere-radius volume)
  (expt (/ volume (* 4/3 pi)) 1/3))

(define pi (* 4 (atan 1 1)))

(define fahrenheit-to-celsius
  (make-unit-conversion (lambda (f) (* 5/9 (- f 32)))
                        (lambda (c) (+ (* c 9/5) 32))))

(define celsius-to-kelvin
  (let ((zero-celsius 273.15))
    (make-unit-conversion (lambda (c) (+ c zero-celsius))
                          (lambda (k) (- k zero-celsius)))))

(example
 (fahrenheit-to-celsius -40)
 => -40)

(example
 (fahrenheit-to-celsius 32)
 => 0)

(example
 ((unit:invert fahrenheit-to-celsius) 20)
 => 68)

(example
 ((compose celsius-to-kelvin fahrenheit-to-celsius) 80)
 => 299.81666666666666)

(define pound-to-newton                 ; not in the book
  (make-unit-conversion (lambda (p) (* p 4.4482216))
                        (lambda (n) (/ n 4.4482216))))

(define inch-to-meter                   ; not in the book
  (make-unit-conversion (lambda (i) (* i 0.0254))
                        (lambda (m) (/ m 0.0254))))

(define psi-to-nsm
  (compose pound-to-newton
           (unit:invert inch-to-meter)
           (unit:invert inch-to-meter)))

(example
 ((unit:invert inch-to-meter)
  (sphere-radius
   (gas-law-volume
    (psi-to-nsm 14.7)
    ((compose celsius-to-kelvin fahrenheit-to-celsius) 68)
    1)))
 => 7.049624635839811)


(define (unit-specializer procedure implicit-output-unit
                          . implicit-input-units)
  (define (specializer specific-output-unit
                       . specific-input-units)
    (let ((output-converter
           (make-converter implicit-output-unit
                           specific-output-unit))
          (input-converters
           (map make-converter
                specific-input-units
                implicit-input-units)))
      (define (specialized-procedure . arguments)
        (output-converter
         (apply procedure
                (map (lambda (converter argument)
                       (converter argument))
                     input-converters
                     arguments))))
      specialized-procedure))
  specializer)

(define unit-conversion-table
  (make-hash-table equal-comparator))

(define (register-unit-conversion from to procedure)
  (hash-table-set! unit-conversion-table (cons from to) procedure))

(define (make-converter from-unit to-unit)
  (or (and (equal? from-unit to-unit) values)
      (hash-table-ref/default unit-conversion-table
                              (cons from-unit to-unit)
                              #f)
      (and-let* ((rev (hash-table-ref/default unit-conversion-table
                                              (cons to-unit from-unit)
                                              #f)))
        (unit:invert rev))
      (errorf "I don't know how to conver from ~s to ~s" from-unit to-unit)))

(define (unit:* u1 u2)
  (make-unit-conversion (compose u2 u1)
                        (compose (unit:invert u1)
                                 (unit:invert u2))))

(define (unit:/ u1 u2)                  ; not in the book
  (make-unit-conversion (compose (unit:invert u2) u1)
                        (compose (unit:invert u1) u2)))

(define (unit:expt u n)                 ; not in the book
  (define (compose-n f n)
    (case n
      ((0) values)
      ((1) f)
      ((2) (compose f f))
      (else (compose f (compose-n f (- n 1))))))
  (make-unit-conversion (compose-n u n)
                        (compose-n (unit:invert u) n)))

(register-unit-conversion 'fahrenheit 'celsius
                          fahrenheit-to-celsius)
(register-unit-conversion 'celsius 'kelvin
                          celsius-to-kelvin)
(register-unit-conversion 'fahrenheit 'kelvin
                          (unit:* fahrenheit-to-celsius
                                  celsius-to-kelvin))
(register-unit-conversion '(/ pound (expt inch 2))
                          '(/ newton (expt meter 2))
                          (unit:/ pound-to-newton
                                  (unit:expt inch-to-meter 2)))
(register-unit-conversion '(expt inch 3) '(expt meter 3)
                          (unit:expt inch-to-meter 3))

(define make-specialized-gas-law-volume
  (unit-specializer
   gas-law-volume
   '(expt meter 3)
   '(/ newton (expt meter 2))
   'kelvin
   'mole))

(define conventional-gas-law-volume
  (make-specialized-gas-law-volume
   '(expt inch 3)
   '(/ pound (expt inch 2))
   'fahrenheit
   'mole))

(example
 (sphere-radius (conventional-gas-law-volume 14.7 68 1))
 => 7.04962463583981)
