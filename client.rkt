#lang racket

(provide (all-defined-out))

(define-values (in out) (tcp-connect "localhost" 8886))

(write "B12" out)
(flush-output out)

(close-input-port in)
(close-output-port out)
