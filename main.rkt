#lang racket

(require "move.rkt")
(require "board.rkt")
(require "logic.rkt")

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
        ; return new board with piece at point
        (board-set board actual-move piece)
        #f)))


(define (save-game board player [location #f])
  (printf "Saving game...\n")
  (if location
      (display-to-file (list player board) location #:exists 'replace)
      (let* ([home (find-system-path 'home-dir)]
             [path (build-path home "go.save")])
        (display-to-file (list player board) path #:exists 'replace)))
  (printf "Done saving game.\n"))


(define (load-game [path #f])
  "takes (optional) file path and returns (list player board)"
  (if (not path)
      (let* ([home (find-system-path 'home-dir)]
             [filepath (build-path home "go.save")])
        (printf "Loading save from \"~a\"\n" filepath)
        (file->value filepath))
      (begin
        (printf "Loading save from \"~a\"\n" path)
        (file->value path))))


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
      [(equal? move 'save) (save-game board player)]
      [(equal? move 'exit) (printf "Bye!\n")]
      [(equal? move 'pass) (play board (next-turn player))]
      [else (let ([new-board (place-piece board move player)])
              (if new-board
                  (play new-board (next-turn player))
                  (begin (printf "You can't place a ~a stone at ~a\n" (symbol->string player) move)
                         (play board player))))])))

(define (start-game)
  (if (load-game-file?)
      (let ([contents (load-game (load-game-file?))])
        (play (cadr contents) (car contents)))
      (play initial-board 'black)))

(define load-game-file? (make-parameter #f))

(define game-to-load
  (command-line
  #:once-each
   [("-l" "--load")
    file
    "Load save file to continue previous game"
    (load-game-file? file)]))

;;;;;;;;;;;;; run game ;;;;;;;;;;;;;;;;;;
; (start-game)
