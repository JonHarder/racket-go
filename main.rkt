#lang racket

(require "move.rkt")
(require "board.rkt")

(define (letter->number letter)
  (let* ([letter-list (map string (string->list "ABCDEFGHIJKLMNOPQRS"))]
         [make-pair (lambda (num) `(,(list-ref letter-list num) . ,num))]
         [letter-map  (make-hash (map make-pair (range 19)))])
    (hash-ref letter-map letter)))

(define (normalize-move point)
  `(,(letter->number (car point)) . ,(- (cdr point) 1)))


(define (place-piece board point piece)
  (let* ([actual-move (normalize-move point)]
         [cur-piece (board-ref board actual-move)])
    (if (equal? 'empty cur-piece)
        (board-set board actual-move piece)
        #f)))


(define (next-turn player)
  (cond
    [(equal? player 'black) 'white]
    [(equal? player 'white) 'black]))


(define (score-game board)
  (printf "Both players passed. Game is done.\n"))


(define (play board player)
  (newline)
  (print-board board)
  (newline)
  (let ([move (get-move player)])
    (cond
      [(equal? move 'save) (save-game board player)]
      [(equal? move 'exit) (printf "Bye!\n")]
      [(equal? move 'pass) (begin (num-passes (+ 1 (num-passes)))
                                  (if (>= (num-passes) 2)
                                      (score-game board)
                                      (play board (next-turn player))))]
      [else (let ([new-board (place-piece board move player)])
              (if new-board
                  (begin (num-passes 0)
                         (play new-board (next-turn player)))
                  (begin (printf "You can't place a ~a stone at ~a\n" (symbol->string player) move)
                         (play board player))))])))

(define (start-game)
  (if (load-game-file?)
      (let ([contents (load-game (load-game-file?))])
        (captured-black-stones (cdr (car contents)))
        (captured-white-stones (car (car contents)))
        (play (third contents)
              (second contents)))
      (play initial-board 'black)))


(define load-game-file? (make-parameter #f))

(define num-passes (make-parameter 0))


(define game-to-load
  (command-line
  #:once-each
   [("-l" "--load")
    file
    "Load save file to continue previous game"
    (load-game-file? file)]))

;;;;;;;;;;;;; run game ;;;;;;;;;;;;;;;;;;
(start-game)
