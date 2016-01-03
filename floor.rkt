#lang racket

(require "grid.rkt"
         "character.rkt")

(provide new-floor
         show-floor
         floor-player)

(struct floor
  (grid
   player))
;; TODO encounters, entry and exit, chests (or are those just cells?), etc.

(define (show-floor f)
  (show-grid (floor-grid f)))

(define (new-floor los p #:player-pos player-pos
                   #:other-characters [other-characters '()]); dictof character%
  (define g (parse-grid los))
  (define f (floor g p))
  ;; TODO eventually, those next two lines should go in a separate function
  ;;   so that we can create floors without putting the player there
  ;;   (and the player can move between (possibly existing) floors)
  (set-field! grid p g)
  (send p move player-pos)
  (for ([(char pos) (in-dict other-characters)])
    (set-field! grid char g)
    (send char move pos))
  f)


(module+ test
  (require rackunit)

  (define (render-grid g) (string-join g "\n" #:after-last "\n"))

  (define g1
    '("****"
      "*  *"
      "****"))
  (define f1
    (new-floor g1 #:player-pos #(1 1)))
  (check-equal? (show-floor f1)
                (render-grid '("****"
                               "*@ *"
                               "****")))
  (define p1 (floor-player f1))
  (send p1 move-right)
  (check-equal? (show-floor f1)
                (render-grid '("****"
                               "* @*"
                               "****")))
  (send p1 move-up) ; can't move into a wall
  (check-equal? (show-floor f1)
                (render-grid '("****"
                               "* @*"
                               "****")))
  )