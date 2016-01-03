#lang racket

(require "grid.rkt"
         "player.rkt")

(struct floor
  (grid
   player))
;; TODO encounters, entry and exit, chests (or are those just cells?), etc.

(define (show-floor f)
  (show-grid (floor-grid f)))

(define (new-floor los #:player-pos player-pos)
  (define g (parse-grid los))
  (define p (new player% [grid g]))
  (define f (floor g p))
  (send p move player-pos)
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