#lang racket

(require "move.rkt")
(require "board.rkt")
(require "computer.rkt")
(require "server.rkt")
(require "client.rkt")

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
  ; (print-board board)
  (new-print-board board)
  (newline)
  (let ([move (if (and (computer-on?) (equal? player 'white))
                  (get-computer-move board player)
                  (get-move player))])
    (cond
      [(equal? move 'save) (save-game board player)]
      [(equal? move 'exit) (printf "Bye!\n")]
      [(equal? move 'pass) (begin (num-passes (+ 1 (num-passes)))
                                  (if (>= (num-passes) 2)
                                      (score-game board)
                                      (play board (next-turn player))))]
      [else (let ([new-board (place-piece board move player)])
              (cond
                [(equal? new-board 'ko) (begin (printf "Playing at ~a is illegal due to ko rule.\n" move)
                                               (play board player))]
                [(equal? new-board 'suicide) (begin (printf "Playing at ~a is suicidal\n" move) (play board player))]
                [else
                 (if new-board
                     (begin (num-passes 0)
                            (play new-board (next-turn player)))
                     (begin (printf "You can't place a ~a stone at ~a\n" (symbol->string player) move)
                            (play board player)))]))])))

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

(define computer-on? (make-parameter #f))

(define server-conn (make-parameter #f))
(define client-conn (make-parameter #f))
(define run-as-server (make-parameter #f))
(define run-as-client (make-parameter #f))


(define game-to-load
  (command-line
  #:once-each
  [("-l" "--load")
   file
   "Load save file to continue previous game, default save location is ~/go.save"
   (load-game-file? file)]
  [("-c" "--computer")
   "Play against a computer opponent (Just picks randomly for now)"
   (computer-on? #t)]
  #:once-any
  [("-s" "--server")
   "[NOT IMPLEMENTED] Run as a server for someone to connect to"
   (run-as-server #t)]
  [("-r" "--remote")
   ip
   "[NOT IMPLEMENTED] Connect to ip of computer running racket-go as a server"
   (run-as-client ip)]))

;;;;;;;;;;;;; run game ;;;;;;;;;;;;;;;;;;
(when (run-as-server)
  (server-conn (make-server-connection)))

(when (run-as-client)
  (client-conn (make-client-connection)))

(start-game)
