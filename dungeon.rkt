#lang racket

(require math/array
         "cell.rkt" "grid.rkt" "utils.rkt")

;; dungeon generation

(struct room
  (height
   width
   poss->cells ; maps positions to cell constructors
   ;;            (so that we can construct the room later when we commit to it)
   free-cells  ; where monsters or treasure could go
   extension-points)) ; where a corridor could sprout

(define (try-add-rectangle grid pos height width direction)
  ;; height and width include a wall of one cell wide on each side
  (match-define (vector x y) pos)
  (define min-x (case direction
                  [(south) x]
                  ;; expanding north, we have to move the top of the room
                  ;; up so the bottom reaches the starting point
                  [(north) (+ (- x height) 1)]
                  ;; expanding east or west, position ourselves so the
                  ;; middle of the wall of the new room starts here
                  ;; TODO try not having it in the middle always
                  ;;   could even try shifting along that axis if we don't fit
                  ;;   (or maybe better to stick with random)
                  [else    (- x (quotient height 2))]))
  (define min-y (case direction
                  ;; same idea as for x
                  [(east) y]
                  [(west) (+ (- y width) 1)]
                  [else   (- y (quotient width 2))]))
  (define max-x (+ min-x height))
  (define max-y (+ min-y width))
  (define-values (success? poss->cells free-cells extension-points)
    (for*/fold ([success?         #t]
                [poss->cells      '()]
                [free-cells       '()]
                [extension-points '()])
        ([x (in-range min-x max-x)]
         [y (in-range min-y max-y)])
      #:break (not success?)
      (define p (vector x y))
      (define c (grid-ref grid p))
      (cond [(and c ; not out of bounds
                  (or (is-a? c void-cell%) ; unused yet
                      (is-a? c wall%)))    ; neighboring room, can abut
             ;; tentatively add stuff
             (define x-wall? (or (= x min-x) (= x (sub1 max-x))))
             (define y-wall? (or (= y min-y) (= y (sub1 max-y))))
             (if (or x-wall? y-wall?)
                 ;; add a wall
                 (values #t ; still succeeding
                         (dict-set poss->cells p wall%)
                         free-cells
                         (if (and x-wall? y-wall?)
                             ;; don't extend from corners
                             extension-points
                             (cons p extension-points)))
                 (values #t
                         (dict-set poss->cells p empty-cell%)
                         (cons p free-cells)
                         extension-points))]
            [else ; hit something, give up
             (values #f #f #f #f)])))
  (and success?
       (room height width poss->cells free-cells extension-points)))

;; mutate `grid` to add `room`
(define (commit-room grid room)
  (for ([(pos cell) (in-dict (room-poss->cells room))])
    (array-set! grid pos (new cell))))

(module+ test
  (require rackunit)
  (define (render-grid g) (string-join g "\n" #:after-last "\n"))
  (define (empty-grid)
    (array->mutable-array
     (build-array #(5 5) (lambda _ (new void-cell%)))))
  (define g1 (empty-grid))
  (check-equal? (show-grid g1)
                (render-grid '("....."
                               "....."
                               "....."
                               "....."
                               ".....")))
  (check-false (try-add-rectangle g1 #(10 10) 3 3 'east)) ; out of bounds
  (commit-room g1 (try-add-rectangle g1 #(2 1) 3 3 'east))
  (check-equal? (show-grid g1)
                (render-grid '("....."
                               ".XXX."
                               ".X X."
                               ".XXX."
                               ".....")))
  (check-false (try-add-rectangle g1 #(2 2) 2 2 'north))
  (commit-room g1 (try-add-rectangle g1 #(3 3) 2 2 'south))
  (check-equal? (show-grid g1)
                (render-grid '("....."
                               ".XXX."
                               ".X X."
                               ".XXX."
                               "..XX.")))
  (define g2 (empty-grid))
  (commit-room g2 (try-add-rectangle g2 #(1 1) 2 4 'east))
  (check-equal? (show-grid g2)
                (render-grid '(".XXXX"
                               ".XXXX"
                               "....."
                               "....."
                               ".....")))
  )
