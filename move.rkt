#lang racket

(provide get-move)


(define (pop-letter str)
  (let ([str-list (string->list str)])
    `(,(car str-list) . ,(list->string (cdr str-list)))))


(define (parse-move move)
  "parse letter out, then number out. Letter should
   be able to be parsed from either upper or lower case"
  (let* ([p-move (pop-letter (string-upcase (symbol->string move)))]
         [letter (car p-move)]
         [num    (string->number (cdr p-move))])
    `(,(string letter) . ,num)))


(define (legal? move)
  (not (or (string<? (car move) "A")
           (string>? (car move) "S")
           (< (cdr move) 1)
           (> (cdr move) 19))))


(define (display-player player)
  (cond
    [(equal? player 'white) "White"]
    [(equal? player 'black) "Black"]))


(define (get-move player)
  (printf "Enter move ~a: " (display-player player))
  (let ([raw-response (read)])
    (if (or (equal? raw-response 'exit)
            (equal? raw-response 'pass))
        raw-response
        (let ([move (parse-move raw-response)])
          (if (not (legal? move))
              (begin (printf "Invalid move. Try again\n")
                     (get-move player))
              move)))))
