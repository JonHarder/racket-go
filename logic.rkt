#lang racket

(require "board.rkt")

(provide (all-defined-out))

(define (liberties-of board point)
  (let* ([x (car point)]
         [y (cdr point)]
         [above `(,x . ,(+ 1 y))]
         [left `(,(- x 1) . ,y)]
         [right `(,(+ 1 x) . ,y)]
         [below `(,x . ,(- y 1))])
    (filter (lambda (point)
              (equal? 'empty (board-ref board point)))
            (list above left right below))))

(define (get-connected board point)
  "returns a list of all points where pieces of the same
   color as at the point given which are connected to said
   piece

           O P Q R S
           . . X X . 19
           . O X . . 18
   board = . X X XO. 17
           . O . OO. 16
           . . . . . 15

   (get-connected board '(3 . 3)) 

   looks at point (3,3) sees that it's black, then for each
   connected stone, recursively finds all other black 
   stones connected to it

   ((2 . 3) (3 . 4) (3 . 5) (4 . 5) (4 . 3)) "
  'foo)
