(use compat.sdff)

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
