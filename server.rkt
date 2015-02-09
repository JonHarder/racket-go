#lang racket

(provide (all-defined-out))

(define listener (tcp-listen 8886))
(define-values (in out) (tcp-accept listener))
(displayln (read in))
(tcp-close listener)
