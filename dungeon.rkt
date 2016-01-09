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


(define (new-room grid pos dir)
  (define w (random-between 5 9)) ; TODO tweak
  (define h (random-between 5 9))
  (try-add-rectangle grid pos w h dir))
(define (new-corridor grid pos dir)
  (define horizontal? (or (eq? dir 'east) (eq? dir 'west)))
  (define len (random-between 4 10))
  (define h (if horizontal? 3   len)) ; TODO tweak. and have wider too
  (define w (if horizontal? len 3))
  (try-add-rectangle grid pos h w dir))
;; TODO have bent corridors too

(define (random-direction) (random-from '(east west north south)))

(define animate-generation? #f) ; to see intermediate steps

(define dungeon-height 18) ; to be easy to display in 80x24, with other stuff
(define dungeon-width  60)
(define (generate-dungeon encounters)
  ;; a room for each encounter, and a few empty ones
  (define n-rooms (+ (length encounters) (random 4)))
  (define grid
    (array->mutable-array
     (build-array (vector dungeon-height dungeon-width)
                  (lambda _ (new void-cell%)))))
  (define first-room
    (let loop ()
      (define starting-point
        (vector (random dungeon-height)
                (random dungeon-width)))
      (define first-room
        (new-room grid starting-point (random-direction)))
      (or first-room (loop)))) ; if it doesn't fit, try again
  (commit-room grid first-room)
  (when animate-generation? (display (show-grid grid)))

  ;; for the rest of the rooms, try sprouting a corridor, with a room at the end
  ;; try until it works
  (define-values (_1 all-rooms _2)
    (for/fold ([n-rooms-to-go    (sub1 n-rooms)]
               [rooms            (list first-room)]
               [extension-points (room-extension-points first-room)])
        ([_ (in-naturals)])
      #:break (= n-rooms-to-go 0)
      ;; pick an extension point at random
      (define ext (random-from extension-points))
      ;; first, try branching a corridor at random
      (define corridor-dir (random-direction))
      (cond [(new-corridor grid ext corridor-dir) =>
             (lambda (corridor)
               ;; now try adding a room at the end
               ;; Note: we don't commit the corridor until we know the room fits
               ;;   This means that `try-add-rectangle` can't check whether the
               ;;   two collide. It so happens that, since we're putting the
               ;;   room at the far end of the corridor (and extending from it),
               ;;   then that can't happen. We rely on that invariant.
               (match-define (vector ext-x ext-y) ext)
               (define corridor-height (room-height corridor))
               (define corridor-width  (room-width  corridor))
               (define new-ext
                 (case corridor-dir
                   ;; add1 and sub1 for make corridor and room abut
                   [(north) (vector (add1 (- ext-x corridor-height)) ext-y)]
                   [(south) (vector (sub1 (+ ext-x corridor-height)) ext-y)]
                   [(east)  (vector ext-x (sub1 (+ ext-y corridor-width)))]
                   [(west)  (vector ext-x (add1 (- ext-y corridor-width)))]))
               (cond [(new-room grid new-ext corridor-dir) =>
                      (lambda (room) ; worked, commit both an keep going
                        (commit-room grid corridor)
                        (commit-room grid room)
                        (when animate-generation? (display (show-grid grid)))
                        (values (sub1 n-rooms-to-go)
                                (cons room rooms) ; corridors don't count
                                (append (room-extension-points corridor)
                                        (room-extension-points room)
                                        extension-points)))]
                     [else ; didn't fit, try again
                      (values n-rooms-to-go rooms extension-points)]))]
            [else ; didn't fit, try again
             (values n-rooms-to-go rooms extension-points)])))
  grid)

(module+ main
  (display (show-grid (generate-dungeon (range 3)))))
