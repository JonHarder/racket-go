#lang racket/base

(provide parse-game
         attribute
         attribute-name
         attribute-value)

(require racket/port
         parsack)

(struct attribute (name value))


(define (make-attribute name-list val-list)
  (let ([name (string->symbol (list->string name-list))])
    (attribute name (if (member name '(SZ MN FF GM))
                        (string->number (list->string val-list))
                        (if (> (length val-list) 2)
                            (list->string val-list)
                            val-list)))))

(define $attribute-name (many1 $letter))

(define $value (parser-compose (char #\[)
                               (val <- (many1 (noneOf "]")))
                               (char #\])
                               (return val)))

(define $attribute (parser-compose (name <- $attribute-name)
                                   (val  <- $value)
                                   (<or> $newline $spaces)
                                   (return (make-attribute name val))))

; $node : parser [struct attribute]
(define $node (many1 $attribute))

; $nodes : parser [ [struct attribute] ]
(define $nodes (sepBy $node (char #\;)))

(define $game (parser-compose (string "(;")
                              (game <- $nodes)
                              (string ")")
                              $eol
                              (return game)))

(define (print-attribute n)
  (printf "~a ~a\n" (attribute-name n) (attribute-value n)))

(define (parse-game file)
  (parse-result $game (port->string (open-input-file file))))
