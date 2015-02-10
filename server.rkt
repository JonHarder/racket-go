#lang racket

(require "move.rkt")

(provide (all-defined-out))

(struct connection (listener in out))

(define (make-server-connection [port 8886])
  (let ([listener (tcp-listen port)])
    (let-values ([(in out) (tcp-accept listener)])
      (connection listener in out))))
