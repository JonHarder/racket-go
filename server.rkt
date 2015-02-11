#lang racket

(require "move.rkt")

(provide (all-defined-out))

(define server-listener (tcp-listen 8886))
(define-values (in out) (tcp-accept server-listener))

(define (server-loop)
  (displayln "Waiting for move from black")
  (flush-output)
  (let ([move (read in)])
    (printf "Blacks move: ~a\n" move))
  (let ([move (get-move 'white)])
    (write move out)
    (flush-output out))
  (server-loop))

(server-loop)

(tcp-close server-listener)
