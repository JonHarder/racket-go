#lang racket

;; board.rkt

(provide (all-defined-out))

(define initial-board (make-list 19 (make-list 19 'empty)))

(define last-played-stone (make-parameter '()))


(define (display-piece piece [index #f])
  (let ([repr (hash-ref (make-hash '((white . "O") (black . "X") (empty . "."))) piece)])
    (if (and (equal? piece 'empty)
             (member index '(3 9 15)))
        "+"
        repr)))


(define (print-row n row)
  (let ([row-str-list (if (member n '(3 9 15))
                          (map (lambda (num) (display-piece (list-ref row num) num)) (range 19))
                          (map display-piece row))]
        [number-buf (if (< n 9)
                        (string-append " " (number->string (+ 1 n)) " ")
                        (string-append (number->string (+ 1 n)) " "))])
    (printf "~a\n" (string-join row-str-list " "
                                #:before-first number-buf
                                #:after-last (string-append " " number-buf)))))


(define (print-board board)
  (printf "Captured white stones: ~a\n" (captured-white-stones))
  (printf "Captured black stones: ~a\n" (captured-black-stones))
  (newline)
  (printf "   A B C D E F G H I J K L M N O P Q R S\n")
  (for-each (lambda (n) (print-row n (list-ref board n)))
            (range 18 -1 -1))
  (printf "   A B C D E F G H I J K L M N O P Q R S\n"))

(define (zip l1 l2)
  (cond
    [(null? l1) l2]
    [(null? l2) l1]
    [else
     (cons `(,(car l1) . ,(car l2)) (zip (cdr l1) (cdr l2)))]))

(define (new-display-piece piece-point-pair [is-left-of #f])
  "first check if last played is on this row, if not ignore, if yes,
   find left and right stone"
  (let* ([piece (car piece-point-pair)]
         [point (cdr piece-point-pair)]
         [last-played (last-played-stone)]
         [x (car point)]
         [y (cdr point)]
         [piece-hash (make-hash '((white . "O") (black . "X") (empty . ".")))]
         [repr (if (and (member x '(3 9 15))
                        (member y '(3 9 15))
                        (equal? piece 'empty))
                   "+"
                   (hash-ref piece-hash piece))])
    (cond
      [(equal? point last-played) (string-join (list repr) #:before-first "("
                                               #:after-last ")")]
      [is-left-of repr]
      [else (string-append repr " ")])))


(define (new-print-row row)
  "find left of last played, right of last played, only buffer whitespace"
  (let* ([row-num (cdr (cdr (car row)))]
         [left-of (if (null? (last-played-stone)) '() `(,(- (car (last-played-stone)) 1) . ,(cdr (last-played-stone))))]
         [row-str (apply string-append (map (lambda (piece-point)
                                              (let* ([piece (car piece-point)]
                                                     [point (cdr piece-point)]
                                                     [reverse-point (cons (car point) (- (cdr point) 18))])
                                                (if (equal? point left-of)
                                                    (new-display-piece (cons piece reverse-point) #t)
                                                    (new-display-piece (cons piece reverse-point))))) row))])
    (string-append (if (< (+ 1 row-num) 10) " " "")
                   (number->string (+ 1 row-num))
                   " "
                   row-str
                   (if (< (+ 1 row-num) 10) " " "")
                   (number->string  (+ 1 row-num)) "\n")))


(define (new-print-board board)
  (let* ([points (for*/list ([i (range 18 -1 -1)] [j (range 19)])
                   (cons j i))]
         [board-point-pairs (zip (flatten board) points)])
    (printf "Captured white stones: ~a\n" (captured-white-stones))
    (printf "Captured black stones: ~a\n" (captured-black-stones))
    (newline)
    (printf "   A B C D E F G H I J K L M N O P Q R S\n")
    (printf (apply string-append (map new-print-row (split-into-chunks 19 board-point-pairs))))
    (printf "   A B C D E F G H I J K L M N O P Q R S\n")))


(define (remove-piece board point)
  (place-stone board point 'empty))


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
         [result (apply-logic board point piece)])
    ;; if you can successfully place the piece, return the new board
    ;; as a result of doing so, if placing the piece was unsuccessful
    ;; due to ko or suicide etc. dont place the piece and return the board
    ;; unchanged
    (cond
      [(member result '(ko suicide)) result]
      [else
       (if (cdr result)
           (begin (last-played-stone point)
                  (previous-board-states (cons (car result) (previous-board-states)))
                  (car result))
           #f)])))


(define (place-stone board point piece)
  (let* ([x (car point)]
         [y (cdr point)])
    (assoc board y (assoc (list-ref board y) x piece))))



(define (on-board board point)
  (let ([x (car point)]
        [y (cdr point)])
    (and (< x 19) (< y 19)
         (>= x 0) (>= y 0))))


(define (adjacent-points board point)
  "finds all orthoginally adjacent points on the
   given board to the given piece, ignoring borders"
  (if (equal? (board-ref board point) 'empty)
      '()
      (let* ([player (board-ref board point)]
             [x (car point)]
             [y (cdr point)]
             [above `(,x . ,(+ 1 y))]
             [left `(,(- x 1) . ,y)]
             [right `(,(+ 1 x) . ,y)]
             [below `(,x . ,(- y 1))])
        (filter (lambda (p) (on-board board p)) (list above left right below)))))


(define (flatten1 list)
             (let loop ([l list] [acc null])
               (cond [(null? l) acc]
                     [(list? l) (loop (car l) (loop (cdr l) acc))]
                     [else (cons l acc)])))


(define (squash complex-list)
  "takes a arbitrarily nested list of pairs and flattens them
   down to a flat list, removing duplicates"
  (remove-duplicates (flatten1 complex-list)))


(define (split-into-chunks n xs)
  (if (null? xs)
      '()
      (let ((first-chunk (take xs n))
            (rest (drop xs n)))
        (cons first-chunk (split-into-chunks n rest)))))

;;; should be able to accomplish this with a fold
;;; thread-through :: a -> (a -> b -> a) -> [a] -> a
(define (thread-through val fun args)
  (if (eq? (length args) 0)
      val
      (thread-through (fun val (car args)) fun (cdr args))))


(define captured-white-stones (make-parameter 0))
(define captured-black-stones (make-parameter 0))

;;; TODO wire in parameter to keep track of number and color
;;; of captured stones
(define (capture board point)
  "removes the piece and all connected pieces (if any)
   from the board, returning the new board without those pieces"
  (let* ([pieces (get-connected board point)]
        [num-pieces (length pieces)]
        [player (board-ref board point)])
    (cond
     [(equal? player 'white) (captured-white-stones (+ num-pieces (captured-white-stones)))]
     [(equal? player 'black) (captured-black-stones (+ num-pieces (captured-black-stones)))])
    (thread-through board remove-piece pieces)))


(define (get-liberties board point)
  (let ([pieces (get-connected board point)])
    (filter (lambda (p) (equal? (board-ref board p) 'empty))
            (squash (map (lambda (p) (adjacent-points board p)) pieces)))))


(define (get-connected board point [found-pieces '()])
  "recursivly finds all connected stones, finding the whole group"
  (if (equal? 'empty (board-ref board point))
      '()
      (let* ([player (board-ref board point)]
             [adjacent (adjacent-points board point)]
             [pieces (filter (lambda (p) (equal? player (board-ref board p))) adjacent)])
        (if (member point found-pieces)
            found-pieces
            (squash (cons point (map (lambda (p) (get-connected board p (cons point found-pieces))) pieces)))))))


(define (suicide? board point player)
  "returns true if placing your stone at the specified point
   is suicidal"
  (let ([new-board (place-stone board point player)])
    (eq? 0 (length (get-liberties new-board point)))))


(define (captured? board point)
  (if (equal? (board-ref board point) 'empty)
      #f
      (let ([liberties (get-liberties board point)])
        (= 0 (length liberties)))))


(define (capturable? board point piece)
  "returns false if no capturable groups
   are made from placing piece at point,
   otherwise returns a list of a single point
   for each of the groups captured"
  (let* ([new-board (place-stone board point piece)]
         [adjacent (adjacent-points new-board point)]
         [captured-groups (filter (lambda (x) (captured? new-board x)) adjacent)])
    (if (empty? captured-groups)
        #f
        captured-groups)))


(define previous-board-states (make-parameter '()))

(define (ko? board)
  (member board (previous-board-states)))


(define (apply-logic board point piece)
  "called by board-set; applies all applicable game logic then returns a
   either the new board as a result of placing the piece and optionally
   capturing opposing pieces, or false, stating the move was invalid"
  (let ([new-board (simulate board point piece)])
    (if (equal? new-board 'suicide)
      'suicide
      (if (ko? (car new-board))
          'ko
          (begin (previous-board-states (cons new-board (previous-board-states)))
                 new-board)))))


(define (simulate board point piece)
  (let* ([new-board (place-stone board point piece)]
         [capture-groups (capturable? new-board point piece)])
    (if capture-groups
        (cons (thread-through new-board capture capture-groups) #t)
        (if (suicide? new-board point piece)
            'suicide
            (cons new-board #t)))))


(define (save-game board player [location #f])
  (printf "Saving game...\n")
  (let ([black (captured-white-stones)]
        [white (captured-black-stones)])
    (if location
        (display-to-file (list `(,black . ,white) player board) location #:exists 'replace)
        (let* ([home (find-system-path 'home-dir)]
               [path (build-path home "go.save")])
          (display-to-file (list `(,black . ,white) player board) path #:exists 'replace)))
    (printf "Done saving game.\n")))


(define (load-game [path #f])
  "takes (optional) file path and returns (list player board)"
  (if (not path)
      (let* ([home (find-system-path 'home-dir)]
             [filepath (build-path home "go.save")])
        (printf "Loading save from \"~a\"\n" filepath)
        (let ([loaded-data (file->value filepath)])
          loaded-data))
      (let ([loaded-data (file->value path)])
        (printf "Loading save from \"~a\"\n" path)
        loaded-data)))
