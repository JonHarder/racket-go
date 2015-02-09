#lang racket

(require "board.rkt")

(provide (all-defined-out))

(define (get-computer-move board player)
  (let* ([letters "ABCDEFGHIJKLMNOPQRS"]
         [x (string (list-ref (string->list letters) (random 19)))])
  '("B" . 2)))
