#lang racket

(provide get-move)
(provide move-count)

(define move-count (make-parameter 1))

(define (pop-letter str)
  (let ([str-list (string->list str)])
    `(,(car str-list) . ,(list->string (cdr str-list)))))


(define (parse-move move)
  "parse letter out, then number out. Letter should
   be able to be parsed from either upper or lower case"
  (with-handlers ([exn:fail? (lambda (x) #f)])
    (let* ([p-move (pop-letter (string-upcase (symbol->string move)))]
           [letter (car p-move)]
           [num    (string->number (cdr p-move))])
      `(,(string letter) . ,num))))


(define (legal? move)
  (cond
    [(and (pair? move)
          (string? (car move))
          (number? (cdr move))
          (not (or (string<? (car move) "A")
                   (string>? (car move) "S")
                   (< (cdr move) 1)
                   (> (cdr move) 19)))) #t]
    [else #f]))


(define (display-player player)
  (cond
    [(equal? player 'white) "white"]
    [(equal? player 'black) "black"]))

(define (get-move player)
  (printf "~a(~a): " (display-player player) (move-count))
  (let ([raw-response (read)])
    (if (member raw-response '(exit pass save))
        raw-response
          (let ([move (parse-move raw-response)])
            (if (not (legal? move))
                (begin
                  (printf "Invalid move\nMoves should be of the form [number][letter]\nExample: D12 or f3\n")
                  (get-move player))
                  move)))))
