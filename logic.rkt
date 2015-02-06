#lang racket

(require "board.rkt")
(provide (all-defined-out))

;;; RULES (to follow in order, ignoring rules if they dont apply,
;;; disregarding any subsequent rules after the first applied rule
;;;
;;;
;;; 1. A single stone in do, can't be captured.
;;;
;;; 2. if a group is in atari, it can be captured
;;;
;;; 3. you can't place a stone where the group it will become part of
;;;    results in a zero liberties group
;;;
;;; DEFINITIONS:
;;; Liberty: an empty orthoginal space adjacent to a group
;;; Ko: repetitive capture
;;; Atari: threat to capture at next move
;;;
;;; if ko then
;;;     invalid
;;;     stop
;;; else if capture
;;;     then valid
;;;     stop
;;; else if suicide
;;;     then invalid
;;; else valid

(define (on-board board point)
  (let ([x (car point)]
        [y (cdr point)])
    (and (< x 19) (< y 19)
         (>= x 0) (>= y 0))))


(define (adjacent-points board point)
  "finds all orthoginally adjacent points on the
   given board to the given piece, ignoring borders"
  (let* ([player (board-ref board point)]
         [x (car point)]
         [y (cdr point)]
         [above `(,x . ,(+ 1 y))]
         [left `(,(- x 1) . ,y)]
         [right `(,(+ 1 x) . ,y)]
         [below `(,x . ,(- y 1))])
    (filter (lambda (p) (on-board board p)) (list above left right below))))

(define (flatten1 list)
             (let loop ([l list] [acc null])
               (cond [(null? l) acc]
                     [(list? l) (loop (car l) (loop (cdr l) acc))]
                     [else (cons l acc)])))


(define (squash complex-list)
  "takes a arbitrarily nested list of pairs and flattens them
   down to a flat list, removing duplicates"
  (remove-duplicates (flatten1 complex-list)))


;;; thread-through :: a -> (a -> a) -> Int -> a
(define (thread-through val fun args)
  (if (eq? (length args) 0)
      val
      (thread-through (fun val (car args)) fun (cdr args))))

(define (capture board point)
  "removes the piece and all connected pieces (if any)
   from the board, returning the new board without those pieces"
  (let ([pieces (get-connected board point)])
    (thread-through board remove-piece pieces)))

;;; find the liberties for the current piece, and all adjacent pieces
;;; of the current pieces color, then recursively call get-liberties of
;;; those adjacent pieces, adding the liberties found therin to the list

(define (get-liberties board point)
  (let ([pieces (get-connected board point)])
    (filter (lambda (p) (equal? (board-ref board p) 'empty))
            (squash (map (lambda (p) (adjacent-points board p)) pieces)))))


(define (get-connected board point [found-pieces '()])
  "recursivly finds all connected stones, finding the whole group"
  (let* ([player (board-ref board point)]
         [adjacent (adjacent-points board point)]
         [pieces (filter (lambda (p) (equal? player (board-ref board p))) adjacent)])
    (if (member point found-pieces)
        found-pieces
        (squash (map (lambda (p) (get-connected board p (cons point found-pieces))) pieces)))))


;; TODO: Doesnt actually work
(define (suicide? board point player)
  "returns true if placing your stone at the specified point
   is suicidal"
  ;; place piece, get length of get-liberties, unplace piece
  (eq? (length (get-liberties board point) 0)))

(define (ko? board point piece)
  #f)
