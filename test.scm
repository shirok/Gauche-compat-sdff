;;;
;;; Test compat.sdff
;;;

(use gauche.test)

(test-start "compat.sdff")
(use compat.sdff)
(test-module 'compat.sdff)

;; The following is a dummy test code.
;; Replace it for your tests.
(test* "test-gauche_compat_sdff" "gauche_compat_sdff is working"
       (test-gauche_compat_sdff))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)




