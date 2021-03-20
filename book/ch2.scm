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
;;; 2.2 Regular expressions
;;;

(define (r:dot) ".")
(define (r:bol) "^")
(define (r:eol) "$")

(define (r:seq . exprs)
  (string-append "\\(" (apply string-append exprs) "\\)"))

(define (r:quote string)
  (r:seq
   (list->string
    (append-map (lambda (char)
                  (if (memv char chars-needing-quoting)
                    (list #\\ char)
                    (list char)))
                (string->list string)))))

(define chars-needing-quoting
  '(#\. #\[ #\\ #\^ #\$ #\*))

(define (r:alt . exprs)
  (if (pair? exprs)
    (apply r:seq
           (cons (car exprs)
                 (append-map (lambda (expr)
                               (list "\\|" expr))
                             (cdr exprs))))
    (r:seq)))

(define (r:repeat min max expr)
  (apply r:seq
         (append (make-list min expr)
                 (cond ((not max) (li stexpr "*"))
                       ((= max min) '())
                       (else
                        (make-list (- max min)
                                   (r:alt expr "")))))))

(define (r:char-from string)
  (case (string-length string)
    ((0) (r:seq))
    ((1) (r:quote string))
    (else
     (bracket string
              (lambda (members)
                (if (lset= eqv? '(#\- #\^) members)
                  '(#\- #\^)
                  (quote-bracketed-contents members)))))))

(define (r:char-not-from string)
  (bracket string
           (lambda (members)
             (cons #\^ (quote-bracketed-contents members)))))

(define (bracket string procedure)
  (list->string
   (append '(#\[)
           (procedure (string->list string))
           '(#\]))))

(define (quote-bracketed-contents members)
  (define (optional char)
    (if (memv char members) (list char) '()))
  (append (optional #\])
          (remove
           (lambda (c)
             (memv c chars-needing-quoting-in-brackets))
           members)
          (optional #\^)
          (optional #\-)))

(define chars-needing-quoting-in-brackets
  '(#\] #\^ #\-))

(define (write-bourne-shell-grep-command expr filename)
  (display (bourne-shell-grep-command-string expr filename)))

(define (bourne-shell-grep-command-string expr filename)
  (string-append "grep -e "
                 (bourne-shell-quote-string expr)
                 " "
                 filename))

(define (bourne-shell-quote-string string)
  (list->string
   (append (list #\')
           (append-map (lambda (char)
                         (if (char=? char #\')
                           (list #\' #\\ char #\')
                           (list char)))
                       (string->list string))
           (list #\'))))

;; This isn't in the book, but we want to run the regexp!
(use gauche.process)
(use file.util)
(define (run-regexp expr input)
  (call-with-temporary-file
   (lambda (oport filename)
     (display input oport)
     (close-output-port oport)
     (let* ((cmd (with-output-to-string
                   (lambda ()
                     (write-bourne-shell-grep-command expr filename))))
            (r (process-output->string `(/bin/sh -c ,cmd)
                                       :on-abnormal-exit :ignore)))
       (if (equal? r "") #f r)))))

(example
 (run-regexp (r:seq (r:bol)
                    (r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))
                    (r:eol))
             "catdogcat")
 => "catdogcat"
 )

(example
 (run-regexp (r:seq (r:bol)
                    (r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))
                    (r:eol))
             "dogdogcatdogdog")
 => "dogdogcatdogdog"
 )

(example
 (run-regexp (r:seq (r:bol)
                    (r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))
                    (r:eol))
             "catcatcatdogdogdog")
 => #f
 )

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

;;;
;;; 2.4 Abstracting a domain
;;;

(define (try-step piece board direction path)
  (let ((new-coords
         (coords+ (piece-coords piece) direction)))
    (and (is-position-on-board? new-coords board)
         (case (position-info new-coords board)
           ((unoccupied)
            (and (not (path-contains-jump? path))
                 (cons (make-simple-move new-coords piece board)
                       path)))
           ((occupied-by-opponent)
            (let ((landing (coords+ new-coords direction)))
              (and (is-position-on-board? landing board)
                   (is-position-unoccupied? landing board)
                   (cons (make-jump landing new-coords piece board)
                         path))))
           ((occupied-by-self #f))
           (else (error "Unknown position info"))))))

(define (compute-next-steps piece board path)
  (filter-map (lambda (direction)
                (try-step piece board direction path))
              (possible-directions piece)))

(define (evolve-paths piece board)
  (let ((paths (compute-next-steps piece board '())))
    (let ((jumps (filter path-contains-jump? paths)))
      (if (null? jumps)
        paths
        (evolve-jumps jumps)))))

(define (evolve-jumps paths)
  (append-map (lambda (path)
                (let ((paths
                       (let ((step (car path)))
                         (compute-next-steps (step-to step)
                                             (step-board step)
                                             path))))
                  (if (null? paths)
                    (list path)
                    (evolve-jumps paths))))
              paths))

(define (generate-moves board)
  (crown-kings
   (mandate-jumps
    (append-map (lambda (piece)
                  (evolve-paths piece board))
                (current-pieces board)))))

(define (mandate-jumps paths)
  (let ((jumps (filter path-contains-jumps? paths)))
    (if (null? jumps)
      paths
      jumps)))

(define (crown-kings paths)
  (map (lambda (path)
         (let ((piece (step-to (car path))))
           (if (should-be-crowned? piece)
             (cons (replace-piece (crown-piece piece)
                                  piece
                                  (step-board (car path)))
                   path)
             path)))
       paths))
