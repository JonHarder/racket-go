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

(define (remove-piece board point)
  (board-set board point 'empty))

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
  (let* ([x (car point)]
         [y (cdr point)]
         [new-board (assoc board y (assoc (list-ref board y) x piece))])
    (apply-logic new-board point piece)))



(define (print-board board)
  (printf "   A B C D E F G H I J K L M N O P Q R S\n")
  (for-each (lambda (n) (print-row n (list-ref board n)))
            (range 18 -1 -1))
  (printf "   A B C D E F G H I J K L M N O P Q R S\n"))



(define (on-board board point)
  (let ([x (car point)]
        [y (cdr point)])
    (and (< x 19) (< y 19)
         (>= x 0) (>= y 0))))


(define (adjacent-points board point)
  "finds all orthoginally adjacent points on the
   given board to the given piece, ignoring borders"
  (let* ([player (board-ref board point)]
         [x (car point)]
         [y (cdr point)]
         [above `(,x . ,(+ 1 y))]
         [left `(,(- x 1) . ,y)]
         [right `(,(+ 1 x) . ,y)]
         [below `(,x . ,(- y 1))])
    (filter (lambda (p) (on-board board p)) (list above left right below))))


(define (flatten1 list)
             (let loop ([l list] [acc null])
               (cond [(null? l) acc]
                     [(list? l) (loop (car l) (loop (cdr l) acc))]
                     [else (cons l acc)])))


(define (squash complex-list)
  "takes a arbitrarily nested list of pairs and flattens them
   down to a flat list, removing duplicates"
  (remove-duplicates (flatten1 complex-list)))


;;; should be able to accomplish this with a fold
;;; thread-through :: a -> (a -> b -> a) -> [a] -> a
(define (thread-through val fun args)
  (if (eq? (length args) 0)
      val
      (thread-through (fun val (car args)) fun (cdr args))))

(define (capture board point)
  "removes the piece and all connected pieces (if any)
   from the board, returning the new board without those pieces"
  (let ([pieces (get-connected board point)])
    (thread-through board remove-piece pieces)))


;;; find the liberties for the current piece, and all adjacent pieces
;;; of the current pieces color, then recursively call get-liberties of
;;; those adjacent pieces, adding the liberties found therin to the list
(define (get-liberties board point)
  (let ([pieces (get-connected board point)])
    (filter (lambda (p) (equal? (board-ref board p) 'empty))
            (squash (map (lambda (p) (adjacent-points board p)) pieces)))))



(define (get-connected board point [found-pieces '()])
  "recursivly finds all connected stones, finding the whole group"
      (let* ([player (board-ref board point)]
             [adjacent (adjacent-points board point)]
             [pieces (filter (lambda (p) (equal? player (board-ref board p))) adjacent)])
        (if (member point found-pieces)
            found-pieces
            (squash (cons point (map (lambda (p) (get-connected board p (cons point found-pieces))) pieces))))))


(define (suicide? board point player)
  "returns true if placing your stone at the specified point
   is suicidal"
  (let ([new-board (board-set board point player)])
    (eq? 0 (length (get-liberties new-board point)))))

;; might need to keep state of board around for a few iterations
;; otherwise cant access previous board state
(define (ko? board point piece)
  #f)


;;; RULES (to follow in order, ignoring rules if they dont apply,
;;; disregarding any subsequent rules after the first applied rule
;;;
;;;
;;; 1. A single stone in do, can't be captured.
;;;
;;; 2. if a group is in atari, it can be captured
;;;
;;; 3. you can't place a stone where the group it will become part of
;;;    results in a zero liberties group
;;;
;;; DEFINITIONS:
;;; Liberty: an empty orthoginal space adjacent to a group
;;; Ko: repetitive capture
;;; Atari: threat to capture at next move
;;;
;;; if ko then
;;;     invalid
;;;     stop
;;; else if capture
;;;     then valid
;;;     stop
;;; else if suicide
;;;     then invalid
;;; else valid
(define (apply-logic board point piece)
  board)
