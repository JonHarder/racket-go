#lang racket/base

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define-tokens token-values (PROPERTY VALUE))
(define-empty-tokens properties (C  ; comment
                                 B  ; blacks move
                                 W  ; white move
                                 PW ; whites name
                                 PB ; blacks name
                                 PL ; player (whos turn it is)
                                 MN ; move number
                                 GM ; game (1 for go)
                                 KO ; komi
                                 AB ; add black (places black stone)
                                 AW ; add white (places white stone)
                                 AE ; add empty (clears point)
                                 FF ; sgf format
                                 SZ ; board size
                                 ))

(define-lex-abbrevs
  [letter (:or (:/ "a" "z") (:/ #\A #\Z))]
  [point (:= 2 letter)]
  [digits (:+ (:/ #\0 #\9))]
  [property-label (:: ";" (:+ letter))]
  [value (:: "[" (:or point digits any-string) "]")]
  [property (:: property-label value)])


(define lex
  (lexer-src-pos
   [(:+ property) lexeme]))

(let ([in (open-input-file "foo.bar")])
  (printf "~a\n" (position-token-token (lex in))))
