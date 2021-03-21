;; -*- coding: utf-8 -*-
(define-module compat.sdff.checker
  (use util.match)
  (export current-pieces is-position-on-board? board-get position-info
          is-position-unoccupied? is-position-occupied-by-self?
          is-position-occupied-by-opponent?
          piece-coords should-be-crowned? crown-piece possible-directions
          step-to step-board make-simple-move make-jump
          replace-piece path-contains-jumps? coords+

          make-board initial-board draw-board
          ))
(select-module compat.sdff.checker)

;; Checker infrastrucutre
;; The book abstracts the actual implementation, so we take our liberty
;; to use Gauche features freely.

;; NB: All objects are immutable.

;; Checker Board
;;  - Row and column numbers are 0-based, (0,0) being lower-left.
;;  - 'Dark' side takes lower, 'light' side takes upper.
;;  - Placeable squares are where (column + row) is even.

(define checker-rows 8)
(define checker-cols 8)

;; coords are just a cons of row and column.
(define (coords+ coords-a coords-b)
  (cons (+ (car coords-a) (car coords-b))
        (+ (cdr coords-a) (cdr coords-b))))

(define-class <board> ()
  ((pieces :init-form '() :init-keyword :pieces) ;list of pieces
   (current-turn :init-form 'dark)))    ;dark or light
(define-method write-object ((b <board>) port)
  (format port "#<board turn=~a\n" (~ b'current-turn))
  (draw-board b port)
  (format port ">"))

(define (current-pieces board) (~ board'pieces))

(define (is-position-on-board? coords board)
  (match-let1 (row . column) coords
    (and (<= 0 row (- checker-rows 1))
         (<= 0 column (- checker-cols 1))
         (even? (+ row column)))))

(define (board-get coords board)
  (find (lambda (p) (equal? (~ p'coords) coords)) (~ board'pieces)))

(define (position-info coords board)
  (let ([piece (board-get coords board)]
        [turn (~ board'current-turn)])
    (if piece
      (if (eq? (~ piece'owner) turn)
        'occupied-by-self
        'occupied-by-opponent)
      'unoccupied)))

(define (is-position-unoccupied? coords board)
  (eq? (position-info coords board) 'unoccupied))

(define (is-position-occupied-by-self? coords board)
  (eq? (position-info coords board) 'occupied-by-self))

(define (is-position-occupied-by-opponent? coords board)
  (eq? (position-info coords board) 'occupied-by-opponent))

(define-class <piece> ()
  ((owner :init-keyword :owner)          ;dark or light
   (coords :init-keyword :coords)        ;(row . column)
   (crowned? :init-value #f :init-keyword :crowned?)))

(define-method write-object ((obj <piece>) port)
  (format port "#<piece ~a@~a~a>" (~ obj'owner) (~ obj'coords)
          (if (~ obj'crowned?) " (crowned)" "")))

(define (piece-coords piece) (~ piece'coords))

(define (should-be-crowned? piece)
  (and (not (~ piece'crowned?))
       (case (~ piece'owner)
         [(dark) (= (car (~ piece'coords)) (- checker-rows 1))]
         [(light) (= (car (~ piece'coords)) 0)])))

(define (crown-piece piece)
  (make <piece> :owner (~ piece'owner) :coords (~ piece'coords) :crowned? #t))

(define (possible-directions piece)
  (if (~ piece'crowned?)
    '((1 . 1) (1 . -1) (-1 . 1) (-1 . -1))
    (case (~ piece'owner)
      [(dark) '((1 . 1) (1 . -1))]
      [(light) '((-1 . 1) (-1 . -1))])))

(define-class <step> ()
  ((kind :init-keyword :kind)           ;'move, 'jump or 'replace
   (prev-piece :init-keyword :prev-piece)
   (prev-board :init-keyword :prev-board)
   (next-piece :init-keyword :next-piece)
   (next-board :init-keyword :next-board)))

(define (step-to step) (~ step'next-piece))

(define (step-board step) (~ step'next-board))

(define (make-simple-move new-coords piece board)
  (let* ([new-piece (make <piece>
                      :owner (~ piece'owner)
                      :coords new-coords
                      :crowned? (~ piece'crowned?))]
         [new-board (make <board>
                      :pieces (cons new-piece
                                    (delete piece (~ board'pieces))))])
    (make <step>
      :kind 'move
      :prev-piece piece
      :prev-board board
      :next-piece new-piece
      :next-board new-board)))

(define (make-jump new-coords jumped-coords piece board)
  (let* ([piece-to-remove (board-get jumped-coords board)]
         [new-piece (make <piece>
                      :owner (~ piece'owner)
                      :coords new-coords
                      :crowned? (~ piece'crowned?))]
         [new-board (make <board>
                      :pieces (cons new-piece
                                    (delete piece
                                            (delete piece-to-remove
                                                    (~ board'pieces)))))])
    (make <step>
      :kind 'jump
      :prev-piece piece
      :prev-board board
      :next-piece new-piece
      :next-board new-board)))

(define (replace-piece new-piece old-piece board)
  (let ([new-board (make <board>
                     :pieces (cons new-piece
                                   (delete old-piece (~ board'pieces))))])
    (make <step>
      :kind 'replace
      :prev-piece old-piece
      :prev-board board
      :next-piece new-piece
      :next-board new-board)))

(define (path-contains-jumps? path)     ;path is a list of steps
  (find (lambda (step) (eq? (~ step'kind) 'jump)) path))


;; Following stuff are not in the book, but useful when you actually try
;; the game.
;; Note: We use Unicode Box-Drawing character.  Make sure your terminal's
;; encoding setting.

(define (sort-pieces pieces)
  (sort pieces (lambda (pa pb)
                 (let ([a (~ pa'coords)]
                       [b (~ pb'coords)])
                   (cond [(> (car a) (car b))] ;greater row first
                         [(= (car a) (car b))  ;same row
                          (< (cdr a) (cdr b))] ;smaller column first
                         [else #f])))))

(define (draw-board board :optional (port (current-output-port)))
  (define (draw-separator border) ; border: top, middle, bottom
    (receive (left mid right)
        (case border
          [(top)    (values #\u250c #\u252c #\u2510)] ;┌ ┬ ┐
          [(middle) (values #\u251c #\u253c #\u2524)] ;├ ┼ ┤
          [(bottom) (values #\u2514 #\u2534 #\u2518)] ;└ ┴ ┘
          )
      (display #\space)
      (display left)
      (dotimes [n (- checker-cols 1)]
        (display #\u2500)                 ;─
        (display mid))
      (display #\u2500)                   ;─
      (display right)
      (newline)))
  (define (draw-piece piece)
    (case (~ piece'owner)
      [(dark)  (if (~ piece'crowned?)
                 (display #\u272a)      ;✪
                 (display #\u25cf))]    ;●
      [(light) (if (~ piece'crowned?)
                 (display #\u272b)      ;✫
                 (display #\u25cb))]))  ;○
  (define (draw-empty)
    (display #\space))
  (define (draw-row row pieces)         ;returns undrawn pieces
    (format #t "~a\u2502" row)          ;│
    (let loop ([col 0] [pieces pieces])
      (if (= col checker-cols)
        (begin (newline)
               pieces)
        (or (and-let* ([ (pair? pieces) ]
                       [coords (piece-coords (car pieces))]
                       [ (and (= (car coords) row)
                              (= (cdr coords) col)) ])
              (draw-piece (car pieces))
              (display #\u2502)         ;│
              (loop (+ col 1) (cdr pieces)))
            (begin
              (draw-empty)
              (display #\u2502)         ;│
              (loop (+ col 1) pieces))))))

  (with-output-to-port port
    (^[]
      (display " ")
      (dotimes [c checker-cols] (format #t " ~a" c))
      (newline)
      (let loop ([row (- checker-rows 1)]
                 [pieces (sort-pieces (~ board'pieces))])
        (when (>= row 0)
          (draw-separator (if (= row (- checker-rows 1)) 'top 'middle))
          (loop (- row 1) (draw-row row pieces))))
      (draw-separator 'bottom)
      (values))))

;; Each 'positions' are ((row col) ...)
(define (make-board dark-positions light-positions)
  (define (make-pieces type positions)
    (map (^[rc] (make <piece> :owner type :coords (cons (car rc) (cadr rc))))
         positions))
  (make <board>
    :pieces (append (make-pieces 'dark dark-positions)
                    (make-pieces 'light light-positions))))

(define (initial-board)
  (make-board '((0 0) (0 2) (0 4) (0 6)
                (1 1) (1 3) (1 5) (1 7)
                (2 0) (2 2) (2 4) (2 6))
              '((5 1) (5 3) (5 5) (5 7)
                (6 0) (6 2) (6 4) (6 6)
                (7 1) (7 3) (7 5) (7 7))))
