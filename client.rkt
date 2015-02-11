#lang racket

(require "move.rkt")

(provide (all-defined-out))

(define-values (in out) (tcp-connect "localhost" 8886))

(define (client-loop)
  (let ([move (get-move 'black)])
    (write move out)
    (flush-output out)
    (printf "waiting for move from white...\n")
    (flush-output)
    (printf "White's move: ~a\n" (read in))
    (flush-output)
    (client-loop)))

(client-loop)

(close-input-port in)
(close-output-port out)
