#lang racket

(require "encounters.rkt"
         "dungeon.rkt"
         "state.rkt"
         "utils.rkt")

(provide generate)

(define (generate player)
  (define lvl        (get-field level player))
  (define encounters (generate-encounters lvl))
  (define-values (grid rooms) (generate-dungeon (length encounters)))
  (define start-room (random-from rooms))
  (define encounter-rooms
    (random-sample rooms (length encounters) #:replacement? #f))
  (define (random-room-poss room n)
    (random-sample (room-free-cells room) n #:replacement? #f))
  (define monster-poss
    (for/list ([e    (in-list encounters)]
               [r    (in-list encounter-rooms)]
               #:when #t ; nest iteration
               [poss (in-value (random-room-poss r (length e)))]
               #:when #t ; nest iteration
               [m    (in-list e)]
               [pos  (in-list poss)])
      (cons (new m) pos)))
  (define player-pos
    (let loop ()
      (define pos (first (random-room-poss start-room 1)))
      (if (for/or ([(m p) (in-dict monster-poss)])
            (equal? p pos))
          (loop) ; already a monster there, try again
          pos)))
  (new-state player grid
             #:player-pos       player-pos
             #:other-characters monster-poss))


(module+ main
  (require "grid.rkt" "character.rkt")
  (display (show-grid (state-grid (generate (new player%))))))
