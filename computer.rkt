#lang racket

(require "board.rkt")

(provide (all-defined-out))

(define (get-computer-move board player)
  (let* ([letters (string->list "ABCDEFGHIJKLMNOPQRS")]
         [x (string (list-ref letters (random 19)))]
         [y (random 19)])
  `(,x . ,y)))
