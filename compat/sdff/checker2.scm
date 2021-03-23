;; -*- coding: utf-8 -*-

(define-module compat.sdff.checker2
  (extend compat.sdff.checker)
  (export piece-type)
  )
(select-module compat.sdff.checker2)

;; Redefinition
(define-class <piece> ()
  ((owner :init-keyword :owner)          ;dark or light
   (coords :init-keyword :coords)        ;(row . column)
   (type   :init-keyword :type)))

(define (piece-type piece) (~ piece'type))

(define (piece-new-type piece type)
  (make <piece>
    :owner (~ piece'owner)
    :coords (~ piece'coords)
    :type (~ piece'type)))

;; <step> is superseded by <change>
(define-class <change> ()
  ((flags :init-keyword :flags)
   (piece :init-keyword :piece)
   (board :init-keyword :board)))

(define (make-change board piece flags)
  (make <change> :flags flags :piece piece :board board))

(define (get-board change) (~ change'board))
(define (get-piece change) (~ change'piece))
(define (get-flags change) (~ change'flags))

;; path becomes <pmove>
(define-class <pmove> ()
  ((initial-board :init-keyword :initial-board)
   (initial-piece :init-keyword :initial-piece)
   (changes :init-keyword :changes)))   ;newest change first

(define (initial-pmove board piece)
  (make <pmove> :initial-board board :initial-piece piece :changes '()))

(define (is-pmove-empty? pmove) (null? (~ pmove'changes)))

(define (is-pmove-finished? pmove)
  (and (pair? (~ pmove'changes))
       (memq 'finished (get-flags (car (~ pmove'changes))))))

(define (current-board pmove)
  (if (null? (~ pmove'changes))
    (~ pmove'initial-board)
    (get-board (car (~ pmove'changes)))))

(define (current-piece pmove)
  (if (null? (~ pmove'changes))
    (~ pmove'initial-piece)
    (get-piece (car (~ pmove'changes)))))
