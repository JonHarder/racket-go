#lang racket

(require "board.rkt")

(provide (all-defined-out))

(define (empty? board point)
  (equal? 'empty (board-ref board point)))

(define (adjacent-pieces board point player)
  "takes a board, a point and a player, returns the set of orthoginal
   points on the board relative to the point which are the same type
   of piece as the player"
  (let* ([x (car point)]
         [y (cdr point)]
         [above `(,x . ,(+ 1 y))]
         [left `(,(- x 1) . ,y)]
         [right `(,(+ 1 x) . ,y)]
         [below `(,x . ,(- y 1))])
    (apply set (filter (lambda (p) (equal? player p))
                       (map (lambda (p) (board-ref board p))
                            (list above left right below))))))

(define (get-connected board point)
  (let ([connected (adjacent-pieces board point
                                    (board-ref board point))])
    (set-map connected get-connected)))


(define (suicide? board move player)
  "returns true if placing your stone at the specified point
   is suicidal"
  #f)


(define (liberties-of board point)
  "functionality is incorrect. must check all adjacent, like colored
   pieces and find total liberties"
  (let* ([x (car point)]
         [y (cdr point)]
         [above `(,x . ,(+ 1 y))]
         [left `(,(- x 1) . ,y)]
         [right `(,(+ 1 x) . ,y)]
         [below `(,x . ,(- y 1))])
    (filter (lambda (point)
              (equal? 'empty (board-ref board point)))
            (list above left right below))))
