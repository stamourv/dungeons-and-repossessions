#lang racket

(require racket/set
         "cell.rkt" "grid.rkt" "utils.rkt" "wall-smoothing.rkt")

(provide generate-dungeon)

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
                  ;; have the entrance be at a random position on the
                  ;; entrance-side wall
                  [else    (sub1 (- x (random (- height 2))))]))
  (define min-y (match direction
                  ;; same idea as for x
                  [(== right) y]
                  [(== left)  (+ (- y width) 1)]
                  [else       (sub1 (- y (random (- width 2))))]))
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
  (define w (random-between 7 11)) ; higher than that is hard to fit
  (define h (random-between 7 11))
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


(define (empty-cell? grid pos)
  (is-a? (grid-ref grid pos) empty-cell%))

(define animate-generation? #f) ; to see intermediate steps

(define dungeon-height 18) ; to be easy to display in 80x24, with other stuff
(define dungeon-width  60)
(define (generate-dungeon encounters)
  ;; a room for each encounter, and a few empty ones
  (define n-rooms (max (length encounters) (random-between 6 9)))
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
  (define connections '()) ; keep track of pairs of connected rooms
  (define (extension-points/room room)
    (for/list ([e (in-list (room-extension-points room))])
      (cons e room)))

  ;; for the rest of the rooms, try sprouting a corridor, with a room at the end
  ;; try until it works
  (let loop ()
    (define-values (n all-rooms _2)
      (for/fold ([n-rooms-to-go    (sub1 n-rooms)]
                 [rooms            (list first-room)]
                 [extension-points (extension-points/room first-room)])
          ([i (in-range 1000)])
        #:break (and (= n-rooms-to-go 0)
                     (log-error (format "generate-dungeon: success after ~a" i))
                     #t)
        (define (add-room origin-room room ext [corridor #f] [new-ext #f])
          (when corridor
            (commit-room grid corridor))
          (commit-room grid room)
          ;; add doors
          (define door-kind
            (if (horizontal? dir) vertical-door% horizontal-door%))
          (array-set! grid ext     (new door-kind))
          (when new-ext
            (array-set! grid new-ext (new door-kind)))
          (set! connections (cons (cons origin-room room) connections))
          (when animate-generation? (display (show-grid grid)))
          (values (sub1 n-rooms-to-go)
                  (cons room rooms) ; corridors don't count
                  (append (if corridor
                              (extension-points/room corridor)
                              '())
                          (extension-points/room room)
                          extension-points)))
        ;; pick an extension point at random
        (match-define `(,ext . ,origin-room) (random-from extension-points))
        ;; first, try branching a corridor at random
        (define dir (random-direction))
        (cond [(and (zero? (random 4)) ; maybe add a room directly, no corridor
                    (new-room grid ext dir)) =>
               (lambda (room) (add-room origin-room room ext))]
              [(new-corridor grid ext dir) =>
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
                        (lambda (room) ; worked, commit both and keep going
                          (add-room origin-room room ext corridor new-ext))]
                       [else ; didn't fit, try again
                        (values n-rooms-to-go rooms extension-points)]))]
              [else ; didn't fit, try again
               (values n-rooms-to-go rooms extension-points)])))

    (cond [(not (= n 0)) ; we got stuck, try again
           (log-error "generate-dungeon: had to restart")
           ;; may have gotten too ambitious with n of rooms, back off
           (set! n-rooms (max (length encounters) (sub1 n-rooms)))
           (loop)]
          [else ; we did it
           ;; try adding more doors
           (define potential-connections
             (for*/fold ([potential-connections '()])
                 ([r1 (in-list all-rooms)]
                  [r2 (in-list all-rooms)]
                  #:unless (or (eq? r1 r2)
                               (member (cons r1 r2) connections)
                               (member (cons r2 r1) connections)
                               (member (cons r2 r1) potential-connections)))
               (cons (cons r1 r2) potential-connections)))
           ;; if the two in a pair share a wall, put a door through it
           (for ([(r1 r2) (in-dict potential-connections)])
             (define common
               (set-intersect (room-extension-points r1)
                              (room-extension-points r2)))
             (define possible-doors
               (filter values
                       (for/list ([pos (in-list common)])
                         (cond [(and (empty-cell? grid (up   pos))
                                     (empty-cell? grid (down pos)))
                                (cons pos horizontal-door%)]
                               [(and (empty-cell? grid (left  pos))
                                     (empty-cell? grid (right pos)))
                                (cons pos vertical-door%)]
                               [else #f]))))
             (when (not (empty? possible-doors))
               (match-define (cons pos door-kind) (random-from possible-doors))
               (array-set! grid pos (new door-kind))))
           grid])))


(module+ main
  (display (show-grid (smooth-walls (generate-dungeon (range 6))))))
