#lang racket

(require "move.rkt")
(require "board.rkt")
(require "logic.rkt")

(define (letter->number letter)
  (let* ([letter-list (map string (string->list "ABCDEFGHIJKLMNOPQRS"))]
         [make-pair (lambda (num) `(,(list-ref letter-list num) . ,num))]
         [letter-map  (make-hash (map make-pair (range 19)))])
    (hash-ref letter-map letter)))


(define (place-piece board point piece)
  (let* ([actual-move `(,(letter->number (car point)) . ,(- (cdr point) 1))]
         [cur-piece (board-ref board actual-move)])
    (if (equal? 'empty cur-piece)
        ; return new board with piece at point
        (board-set board actual-move piece)
        board)))


(define (save-game board turn)
  (let ([home (find-system-path 'home-dir)])
    (display-to-file board "/home/jharder/go.save")))


(define (load-game [path "/home/jharder/go.save"])
  (file->value path))


(define player-seq '(black white))

(define (next-turn turn)
  (remainder (+ 1 turn) 2))

(define (play board turn)
  (newline)
  (print-board board)
  (newline)
  (let* ([player (list-ref player-seq turn)]
         [move (get-move player)])
    (cond
      [(equal? move 'save) (begin
                             (printf "Saving game...\n")
                             (save-game board turn)
                             (printf "Game saved, Bye!\n"))]
      [(equal? move 'exit) (printf "Bye!\n")]
      [(equal? move 'pass) (play board (next-turn turn))]
      [else (let ([board (place-piece board move player)])
              (play board (next-turn turn)))])))

(define (start-game)
  (play initial-board 0))

; (start-game)
(print-board (load-game))
