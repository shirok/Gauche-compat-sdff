(use compat.sdff.checker)

;;;
;;; 2.4 Abstracting a domain
;;;

(define (try-step piece board direction path)
  (let ((new-coords (coords+ (piece-coords piece) direction)))
    (and (is-position-on-board? new-coords board)
         (case (position-info new-coords board)
           ((unoccupied)
            (and (not (path-contains-jumps? path))
                 (cons (make-simple-move new-coords piece board)
                       path)))
           ((occupied-by-opponent)
            (let ((landing (coords+ new-coords direction)))
              (and (is-position-on-board? landing board)
                   (is-position-unoccupied? landing board)
                   (cons (make-jump landing new-coords piece board)
                         path))))
           ((occupied-by-self) #f)
           (else (error "Unknown position info"))))))

(define (compute-next-steps piece board path)
  (filter-map (lambda (direction)
                (try-step piece board direction path))
              (possible-directions piece)))

(define (evolve-paths piece board)
  (let ((paths (compute-next-steps piece board '())))
    (let ((jumps (filter path-contains-jumps? paths)))
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

;;
;; Not in book, but useful
;;

;; Evaluatate (game-repl (initial-board)) to start the game

(define (game-repl board)
  (print board)
  (let ((moves (generate-moves board)))
    (display "Possible moves:\n")
    (for-each (lambda (i path)
                (format #t " ~2d: ~s\n" i (reverse path)))
              (iota (length moves)) moves)
    (format #t "What now? [0-~a]: " (- (length moves) 1))
    (flush)
    (let1 ans (read-line)
      (unless (eof-object? ans)
        (let1 num (guard (e ((<read-error> e) #f))
                    (read-from-string ans))
          (if (and (integer? num)
                   (<= 0 num (- (length moves) 1)))
            (let1 move (list-ref moves num)
              (game-repl (flip-turn (step-board (car move)))))
            (begin
              (display "Hun?\n")
              (game-repl board))))))))
