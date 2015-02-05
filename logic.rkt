#lang racket

(require "board.rkt")

(provide (all-defined-out))

(define (empty? board point)
  (equal? 'empty (board-ref board point)))

(define (adjacent-pieces board point player)
  "takes a board, a point and a player, returns the set of orthoginal
   points on the board relative to the point which are the same type
   of piece as the player"
  (when (pair? point)
    (let* ([x (car point)]
           [y (cdr point)]
           [above `(,x . ,(+ 1 y))]
           [left `(,(- x 1) . ,y)]
           [right `(,(+ 1 x) . ,y)]
           [below `(,x . ,(- y 1))])
      (apply set (filter (lambda (p)
                           (equal? player (board-ref board p)))
                         (list above left right below))))))


(define (get-connected board point)
  (let ([connected (adjacent-pieces board point
                                    (board-ref board point))])
    ; currently an infinite loop, need to check if pieces is in set already
    (set-map connected (lambda (p) (get-connected board p)))))


(define (suicide? board move player)
  "returns true if placing your stone at the specified point
   is suicidal"
  #f)
