#lang racket

(require "array.rkt" "cell.rkt")

(provide (all-defined-out)
         (all-from-out "array.rkt"))

;; a Grid is a math/array Mutable-Array of cell%
;; (mutability is required for dungeon generation)

;; parses a list of strings into a grid, based on the printed representation
;; of each cell
(define (parse-grid los)
  (for*/array #:shape (vector (length los)
                              (apply max (map string-length los)))
              ([s (in-list los)]
               [c (in-string s)])
     (new (char->cell% c))))

(define (show-grid g)
  (with-output-to-string
    (lambda ()
      (for ([r (in-array-axis g)])
        (for ([c (in-array r)])
          (display (send c show)))
        (newline)))))

(define (grid-height g)
  (match-define (vector rows cols) (array-shape g))
  rows)
(define (grid-width g)
  (match-define (vector rows cols) (array-shape g))
  cols)
(define (within-grid? g pos)
  (and (<= 0 (vector-ref pos 0) (sub1 (grid-height g)))
       (<= 0 (vector-ref pos 1) (sub1 (grid-width  g)))))
(define (grid-ref g pos)
  (and (within-grid? g pos)
       (array-ref g pos)))


(module+ test
  (require rackunit)

  (define (parse-and-show los) (show-grid (parse-grid los)))
  (define (render-grid g) (string-join g "\n" #:after-last "\n"))

  (define g1
    '(" "))
  (check-equal? (parse-and-show g1) " \n")

  (define g2
    '(".........."
      ".        ."
      ".        ."
      ".        ."
      ".........."))
  (check-equal? (parse-and-show g2) (render-grid g2))

  (define g2* (parse-grid g2))
  (check-true (within-grid? g2* '#(0 0)))
  (check-true (within-grid? g2* '#(0 1)))
  (check-true (within-grid? g2* '#(1 0)))
  (check-true (within-grid? g2* '#(4 4)))
  (check-false (within-grid? g2* '#(0 10)))
  (check-false (within-grid? g2* '#(5 0)))
  (check-false (within-grid? g2* '#(5 10)))
  )


(define (left pos [n 1])
  (vector (vector-ref pos 0)
          (- (vector-ref pos 1) n)))
(define (right pos [n 1])
  (vector (vector-ref pos 0)
          (+ (vector-ref pos 1) n)))
(define (up pos [n 1])
  (vector (- (vector-ref pos 0) n)
          (vector-ref pos 1)))
(define (down pos [n 1])
  (vector (+ (vector-ref pos 0) n)
          (vector-ref pos 1)))

(define (horizontal? dir)
  (or (equal? dir left) (equal? dir right)))
(define (vertical? dir)
  (or (equal? dir up) (equal? dir down)))

(define (opposite dir)
  (cond [(equal? dir up)    down]
        [(equal? dir down)  up]
        [(equal? dir left)  right]
        [(equal? dir right) left]))

(define (opposite-directions? d1 d2)
  (or (and (eq? d1 up)    (eq? d2 down))
      (and (eq? d1 down)  (eq? d2 up))
      (and (eq? d1 left)  (eq? d2 right))
      (and (eq? d1 right) (eq? d2 left))))

(define (adjacent? pos1 pos2)
  (match-define (vector x1 y1) pos1)
  (match-define (vector x2 y2) pos2)
    (or (and (= 1 (abs (- x1 x2))) (= y1 y2))
        (and (= x1 x2) (= 1 (abs (- y1 y2))))))

(define (manhattan-distance p1 p2)
  (match-define (vector x1 y1) p1)
  (match-define (vector x2 y2) p2)
  (+ (abs (- x1 x2)) (abs (- y1 y2))))


;; simple pathfinding using A*
(define (find-path g a b #:extra-heuristic [extra-heuristic (lambda (g pos) 0)])
  (define height  (grid-height g))
  (define width   (grid-width g))
  ;; grid of pairs (cost . previous-pos)
  (define costs
    (for*/array #:shape (vector height width)
                ([x (in-range height)]
                 [y (in-range width)])
       (cons (if (send (grid-ref g (vector x y)) free?)
                 +inf.0 ; arbitrarily far
                 #f) ; we can't even get there
             #f))) ; no previous
  (array-set! costs a (cons 0 #f)) ; initialize origin point
  (let loop ([queue (list a)]) ; list of positions
    (cond [(null? queue)
           ;; found a path (or failed), trace it back (or return #f)
           (define path
             (let loop ([pos b] [acc '()])
               (define parent (cdr (grid-ref costs pos)))
               (if parent (loop parent (cons pos acc)) acc)))
           (and (not (empty? path))
                path)]
          [else
           ;; least expensive neighbor
           (define next (argmin (lambda (x) (car (grid-ref costs x))) queue))
           (define neighbors
             (for*/list ([dir       (in-list  (list up down left right))]
                         [pos       (in-value (dir next))]
                         [cost+prev (in-value (grid-ref costs pos))]
                         #:when cost+prev ; within bounds
                         [cost      (in-value (car cost+prev))]
                         [prev      (in-value (cdr cost+prev))]
                         #:when cost ; not a wall or other obstacle
                         [new-cost  (in-value
                                     (+ (car (grid-ref costs next))
                                        ;; heuristic cost
                                        (manhattan-distance pos b)
                                        (extra-heuristic g pos)))]
                         #:when (< new-cost cost))
               (array-set! costs pos (cons new-cost next))
               pos))
           (loop (append neighbors (remove next queue)))])))

(module+ test
  (define g3
    (parse-grid '("XXXXXXXXXX"
                  "X      X X"
                  "X X  XXX X"
                  "X X    X X"
                  "XXXXXXXXXX")))
  (check-equal?
   (find-path g3 #(1 1) #(1 6))
   '(#(1 2) #(1 3) #(1 4) #(1 5) #(1 6)))
  (check-equal?
   (find-path g3 #(1 1) #(3 1))
   '(#(2 1) #(3 1)))
  (check-equal?
   (find-path g3 #(3 1) #(1 6))
   '(#(2 1) #(1 1) #(1 2) #(1 3) #(1 4) #(1 5) #(1 6)))
  (check-equal?
   (find-path g3 #(3 1) #(3 6))
   '(#(2 1) #(1 1) #(1 2) #(1 3) #(2 3) #(3 3) #(3 4) #(3 5) #(3 6)))
  (check-equal?
   (find-path g3 #(1 1) #(1 8))
   #f)
  )
