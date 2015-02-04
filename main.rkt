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


(define (save-game board player [location #f])
  (if location
      (display-to-file board location #:exists 'replace)
      (let* ([home (find-system-path 'home-dir)]
             [path (string-append (path->string home) "go.save")])
        (display-to-file board path #:exists 'replace))))


(define (load-game [path #f])
  (if (not path)
      (let* ([homepath (find-system-path 'home-dir)]
             [filepath (string-append (path->string homepath) "go.save")])
        (file->value filepath))
      (file->value path)))


(define (next-turn player)
  (cond
    [(equal? player 'black) 'white]
    [(equal? player 'white) 'black]))


(define (play board player)
  (newline)
  (print-board board)
  (newline)
  (let ([move (get-move player)])
    (cond
      [(equal? move 'save) (begin
                             (printf "Saving game...\n")
                             (save-game board player)
                             (printf "Game saved, Bye!\n"))]
      [(equal? move 'exit) (printf "Bye!\n")]
      [(equal? move 'pass) (play board (next-turn player))]
      [else (let ([board (place-piece board move player)])
              (play board (next-turn player)))])))

(define (start-game)
  (play initial-board 'black))

(start-game)
