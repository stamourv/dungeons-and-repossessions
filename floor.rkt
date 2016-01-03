#lang racket

(require "grid.rkt"
         "character.rkt"
         "state.rkt")

(provide new-floor
         show-floor
         remove-dead-monsters!)

(struct floor
  ;; TODO unclear I even need that structure anymore
  ;;   well, assuming we can't go back to previous floors
  ;;     (which, given current design, makes sense)
  ;;   may come in handy when we add generation, to keep list of
  ;;     rooms, empty cells, etc. so we can place thing
  ;;     but that's a structure useful for generation, not during the game
  (grid
   [monsters #:mutable]))
;; TODO entry and exit, chests (or are those just cells?), etc.

(define (show-floor f)
  (show-grid (floor-grid f)))

(define (new-floor los p #:player-pos player-pos
                   #:other-characters [other-characters '()]); dictof character%
  (define g (parse-grid los))
  (define f (floor g (dict-keys other-characters)))
  ;; TODO eventually, those next two lines should go in a separate function
  ;;   so that we can create floors without putting the player there
  ;;   (and the player can move between (possibly existing) floors)
  (set-field! grid p g)
  (send p move player-pos)
  (for ([(char pos) (in-dict other-characters)])
    (set-field! grid char g)
    (send char move pos))
  f)

(define (remove-dead-monsters! f)
  (define new-monsters ; remove dead monsters
    (for/list ([m (in-list (floor-monsters f))]
               #:when (if (positive? (get-field current-hp m))
                          #t ; alive, keep
                          (begin (send m die) #f))) ; dead, remove
      m))
  (set-floor-monsters! f new-monsters))


(module+ test
  (require rackunit)

  (define (render-grid g) (string-join g "\n" #:after-last "\n"))

  (define p1 (new player%))
  (define g1
    '("****"
      "*  *"
      "****"))
  (define f1
    (new-floor g1 p1 #:player-pos #(1 1)))
  (check-equal? (show-floor f1)
                (render-grid '("****"
                               "*@ *"
                               "****")))
  (void (send p1 move-right))
  (check-equal? (show-floor f1)
                (render-grid '("****"
                               "* @*"
                               "****")))
  (void (send p1 move-up)) ; can't move into a wall
  (check-equal? (show-floor f1)
                (render-grid '("****"
                               "* @*"
                               "****")))
  )