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
  (define min-x (match direction
                  [(== down) x]
                  ;; expanding north, we have to move the top of the room
                  ;; up so the bottom reaches the starting point
                  [(== up) (+ (- x height) 1)]
                  ;; expanding east or west, position ourselves so the
                  ;; middle of the wall of the new room starts here
                  ;; TODO try not having it in the middle always
                  ;;   could even try shifting along that axis if we don't fit
                  ;;   (or maybe better to stick with random)
                  [else    (- x (quotient height 2))]))
  (define min-y (match direction
                  ;; same idea as for x
                  [(== right) y]
                  [(== left)  (+ (- y width) 1)]
                  [else       (- y (quotient width 2))]))
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
  (check-false (try-add-rectangle g1 #(10 10) 3 3 right)) ; out of bounds
  (commit-room g1 (try-add-rectangle g1 #(2 1) 3 3 right))
  (check-equal? (show-grid g1)
                (render-grid '("....."
                               ".XXX."
                               ".X X."
                               ".XXX."
                               ".....")))
  (check-false (try-add-rectangle g1 #(2 2) 2 2 up))
  (commit-room g1 (try-add-rectangle g1 #(3 3) 2 2 down))
  (check-equal? (show-grid g1)
                (render-grid '("....."
                               ".XXX."
                               ".X X."
                               ".XXX."
                               "..XX.")))
  (define g2 (empty-grid))
  (commit-room g2 (try-add-rectangle g2 #(1 1) 2 4 right))
  (check-equal? (show-grid g2)
                (render-grid '(".XXXX"
                               ".XXXX"
                               "....."
                               "....."
                               ".....")))
  )


(define (random-direction) (random-from (list left right up down)))
(define (horizontal? dir)  (or (eq? dir right)  (eq? dir left)))
(define (vertical? dir)    (or (eq? dir up) (eq? dir down)))

(define (new-room grid pos dir)
  (define w (random-between 6 10)) ; higher than that is hard to fit
  (define h (random-between 6 10))
  (try-add-rectangle grid pos w h dir))
(define (new-corridor grid pos dir)
  (define h? (horizontal? dir))
  (define len
    ;; given map proportions (terminal window), horizontal corridors are
    ;; easier to fit
    (if h?
        (random-between 6 10)
        (random-between 5 8)))
  (define h (if h? 3   len))
  (define w (if h? len 3))
  (try-add-rectangle grid pos h w dir))
;; TODO have bent corridors too

(define animate-generation? #f) ; to see intermediate steps

(define dungeon-height 18) ; to be easy to display in 80x24, with other stuff
(define dungeon-width  60)
(define (generate-dungeon encounters)
  ;; a room for each encounter, and a few empty ones
  (define n-rooms (max (length encounters) (random-between 5 8)))
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
  (let loop ()
    (define-values (n all-rooms _2)
      (for/fold ([n-rooms-to-go    (sub1 n-rooms)]
                 [rooms            (list first-room)]
                 [extension-points (room-extension-points first-room)])
          ([i (in-range 1000)])
        #:break (= n-rooms-to-go 0)
        ;; pick an extension point at random
        (define ext (random-from extension-points))
        ;; first, try branching a corridor at random
        (define dir (random-direction))
        (cond [(new-corridor grid ext dir) =>
               (lambda (corridor)
                 ;; now try adding a room at the end
                 ;; Note: we don't commit the corridor until we know the room
                 ;;   fits. This means that `try-add-rectangle` can't check
                 ;;   whether the two collide. It so happens that, since we're
                 ;;   putting the room at the far end of the corridor (and
                 ;;   extending from it), then that can't happen. We rely on
                 ;;   that invariant.
                 (define new-ext
                   (dir ext (if (horizontal? dir)
                                (sub1 (room-width corridor)) ; sub1 to make abut
                                (sub1 (room-height corridor)))))
                 (cond [(new-room grid new-ext dir) =>
                        (lambda (room) ; worked, commit both an keep going
                          (commit-room grid corridor)
                          (commit-room grid room)
                          ;; add doors
                          (define door-kind
                            (if (horizontal? dir)
                                vertical-door%
                                horizontal-door%))
                          (array-set! grid ext     (new door-kind))
                          (array-set! grid new-ext (new door-kind))
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
    (if (= n 0) ; we did it
        grid
        (begin (log-error "generate-dungeon: had to restart")
               ;; may have gotten too ambitious with n of rooms, back off
               (set! n-rooms (max (length encounters) (sub1 n-rooms)))
               (loop))))) ; we got stuck, try again


;; wall smoothing, for aesthetic reasons
(define (smooth-walls grid)
  (for* ([x (in-range (grid-height grid))]
         [y (in-range (grid-width  grid))])
    (smooth-single-wall grid (vector x y)))
  grid)
(define (smooth-single-wall grid pos)
  (define (wall? pos) (is-a? (grid-ref grid pos) wall%))
  (define (wall-or-door? pos)
    (or (wall? pos)
        (is-a? (grid-ref grid pos) door%)))
  (define (counts-as-free? pos) ; i.e., player could be there
    (define c (grid-ref grid pos))
    (or (is-a? c empty-cell%)
        (is-a? c door%)))
  (when (wall? pos)
    (define u   (wall-or-door? (up    pos)))
    (define d   (wall-or-door? (down  pos)))
    (define l   (wall-or-door? (left  pos)))
    (define r   (wall-or-door? (right pos)))
    (define ul  (wall-or-door? (up    (left  pos))))
    (define ur  (wall-or-door? (up    (right pos))))
    (define dl  (wall-or-door? (down  (left  pos))))
    (define dr  (wall-or-door? (down  (right pos))))
    (define fu  (counts-as-free? (up    pos)))
    (define fd  (counts-as-free? (down  pos)))
    (define fl  (counts-as-free? (left  pos)))
    (define fr  (counts-as-free? (right pos)))
    (define ful (counts-as-free? (up    (left  pos))))
    (define fur (counts-as-free? (up    (right pos))))
    (define fdl (counts-as-free? (down  (left  pos))))
    (define fdr (counts-as-free? (down  (right pos))))
    (define (2-of-3? a b c) (or (and a b) (and a c) (and b c)))
    (array-set!
     grid pos
     (new
      (match* (u d l r)
        [(#F #F #F #F) pillar%]
        [(#F #F #F #T) horizontal-wall%]
        [(#F #F #T #F) horizontal-wall%]
        [(#F #F #T #T) horizontal-wall%]
        [(#F #T #F #F) vertical-wall%]
        [(#F #T #F #T) north-west-wall%]
        [(#F #T #T #F) north-east-wall%]
        ;; only have tees if enough corners are "inside"
        [(#F #T #T #T) (cond [(2-of-3? fu fdl fdr) north-tee-wall%]
                             [fu                   horizontal-wall%]
                             [fdl                  north-east-wall%]
                             [fdr                  north-west-wall%])]
        [(#T #F #F #F) vertical-wall%]
        [(#T #F #F #T) south-west-wall%]
        [(#T #F #T #F) south-east-wall%]
        [(#T #F #T #T) (cond [(2-of-3? fd ful fur) south-tee-wall%]
                             [fd                   horizontal-wall%]
                             [ful                  south-east-wall%]
                             [fur                  south-west-wall%])]
        [(#T #T #F #F) vertical-wall%]
        [(#T #T #F #T) (cond [(2-of-3? fl fur fdr) west-tee-wall%]
                             [fl                   vertical-wall%]
                             [fur                  south-west-wall%]
                             [fdr                  north-west-wall%])]
        [(#T #T #T #F) (cond [(2-of-3? fr ful fdl) east-tee-wall%]
                             [fr                   vertical-wall%]
                             [ful                  south-east-wall%]
                             [fdl                  north-east-wall%])]
        [(#T #T #T #T) (cond ; similar to the tee cases
                        [(or (and ful fdr) (and fur fdl))
                         ;; if diagonals are free, need a four-corner wall
                         four-corner-wall%]
                        [(and ful fur) south-tee-wall%]
                        [(and fdl fdr) north-tee-wall%]
                        [(and ful fdl) east-tee-wall%]
                        [(and fur fdr) west-tee-wall%]
                        [ful           south-east-wall%]
                        [fur           south-west-wall%]
                        [fdl           north-east-wall%]
                        [fdr           south-east-wall%])])))))


(module+ main
  (display (show-grid (smooth-walls (generate-dungeon (range 6))))))
