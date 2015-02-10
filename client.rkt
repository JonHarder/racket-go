#lang racket

(require "move.rkt")
(require "board.rkt")

(provide (all-defined-out))

(struct client-connection (in out))

(define (make-client-connection [ip "localhost"] [port 8886])
  (let-values ([(in out) (tcp-connect ip port)])
    (client-connection in out)))
