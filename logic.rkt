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



;;; find the liberties for the current piece, and all adjacent pieces
;;; of the current pieces color, then recursively call get-liberties of
;;; those adjacent pieces, adding the liberties found therin to the list

(define (get-liberties board point [found-pieces '()] [found-liberties '()])
  "returns list of empty spaces orthoginally adjacent to
   the group (possibly just that one point) given"
  (if (member point found-pieces) ;; piece has already been searched. stop here
      found-liberties
      (let* ([player (board-ref board point)]
             [adjacent (adjacent-points board point)]
             [liberties (filter (lambda (p) (equal? (board-ref board p) 'empty)) adjacent)]
             [pieces (filter (lambda (p) (equal? player (board-ref board p))) adjacent)])
        (squash (append
                 found-liberties (map (lambda (p)
                                        (get-liberties board p
                                                       (cons point found-pieces)
                                                       (append liberties found-liberties)))
                                      pieces))))))


(define (get-connected board point)
  '())

(define (suicide? board move player)
  "returns true if placing your stone at the specified point
   is suicidal"
  #f)
