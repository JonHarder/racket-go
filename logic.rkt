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


;;; this works correctly now (probably)
(define (adjacent-points board point)
  (let* ([player (board-ref board point)]
         [x (car point)]
         [y (cdr point)]
         [above `(,x . ,(+ 1 y))]
         [left `(,(- x 1) . ,y)]
         [right `(,(+ 1 x) . ,y)]
         [below `(,x . ,(- y 1))])
    (filter (λ (p) (on-board board p)) (list above left right below))))


;;; this still hangs
(define (get-liberties board point [found-pieces '()] [found-liberties '()])
  "returns list of empty spaces orthoginally adjacent to
   the group (possibly just that one point) given"
  (if (or (member point found-pieces)
          (equal? (board-ref board point) 'empty))
      found-liberties
      (let* ([player (board-ref board point)]
             [adjacent (adjacent-points board point)]
             [liberties (filter (λ (p) (equal? (board-ref board p) 'empty))
                               adjacent)]
             [pieces (filter (λ (p) (equal? player (board-ref board p))) adjacent)])
        (append found-liberties (get-liberties board point pieces liberties)))))



;; get liberties
;;; for current piece:
;;;   if piece in found pieces
;;;      stop
;;;      return list of liberties
;;;   else
;;;      add piece to list of found pieces
;;;      add add liberties to list of liberties
;;;      get liberties of adjacent pieces

(define (get-connected board point)
  '())

(define (suicide? board move player)
  "returns true if placing your stone at the specified point
   is suicidal"
  #f)
