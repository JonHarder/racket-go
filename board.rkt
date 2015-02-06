#lang racket

;; board.rkt
(provide (all-defined-out))

(define initial-board (make-list 19 (make-list 19 'empty)))


(define (display-piece piece [index #f])
  (let ([repr (hash-ref (make-hash '((white . "O") (black . "X") (empty . "."))) piece)])
    (if (and (equal? piece 'empty)
             (member index '(3 9 15)))
        "+"
        repr)))


(define (print-row n row)
  (let ([row-str-list (if (member n '(3 9 15))
                          ;; map over display-piece, but pass extra reference-point
                          ;; when applicable
                          (map (lambda (num) (display-piece (list-ref row num) num)) (range 19))
                          (map display-piece row))]
        [number-buf (if (< n 9)
                        (string-append " " (number->string (+ 1 n)) " ")
                        (string-append (number->string (+ 1 n)) " "))])
    (printf "~a\n" (string-join row-str-list " "
                                #:before-first number-buf
                                #:after-last (string-append " " number-buf)))))


(define (board-ref board point)
  (let ([x (car point)]
        [y (cdr point)])
    (when (and (< x 19) (< y 19)
               (>= x 0) (>= y 0))
        (list-ref (list-ref board y) x))))


(define (assoc dat at val)
  "takes a list, the location in the list to replace
   and a value to replace it with.  returns copy of list
   with the new value in its place"
  (append (take dat at) (list val) (drop dat (+ 1 at))))

(define (board-set board point piece)
  (let ([x (car point)]
        [y (cdr point)])
    (assoc board y (assoc (list-ref board y) x piece))))


(define (print-board board)
  (printf "   A B C D E F G H I J K L M N O P Q R S\n")
  (for-each (lambda (n) (print-row n (list-ref board n)))
            (range 18 -1 -1))
  (printf "   A B C D E F G H I J K L M N O P Q R S\n"))
