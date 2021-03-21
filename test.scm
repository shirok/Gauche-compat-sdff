;;;
;;; Test compat.sdff
;;;

(use gauche.test)

(test-start "compat.sdff")
(use compat.sdff)
(test-module 'compat.sdff)

(use compat.sdff.checker)
(test-module 'compat.sdff.checker)

(load "book/ch2-1.scm")
(load "book/ch2-2.scm")
(load "book/ch2-3.scm")
(load "book/ch2-4.scm")

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
