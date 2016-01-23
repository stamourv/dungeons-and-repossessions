#lang racket

(require racket/random
         "encounters.rkt"
         "dungeon.rkt"
         "state.rkt"
         "utils.rkt")

(provide generate)

(define (generate player)
  (define lvl        (get-field level player))
  (define encounters (generate-encounters lvl))
  ;; need enough rooms for all the encounters, plus a starting room
  ;; (we don't want monsters in the starting room)
  (define n-rooms (add1 (length encounters)))
  (match-define (dungeon grid rooms connections) (generate-dungeon n-rooms))
  (match-define (cons player-room encounter-rooms)
    (random-sample rooms n-rooms #:replacement? #f))
  (define (random-room-poss room n)
    (random-sample (room-free-cells room) n #:replacement? #f))
  (define monster-poss
    (for/list ([e    (in-list encounters)]
               [r    (in-list encounter-rooms)]
               #:when #t ; nest iteration
               [poss (in-value (random-room-poss r (length e)))]
               #:when #t ; nest iteration
               [m    (in-list (instantiate-encounter e))]
               [pos  (in-list poss)])
      (set-room-free-cells! r (remove pos (room-free-cells r)))
      (cons m pos)))
  (define player-pos (first (random-room-poss player-room 1)))
  (new-state player grid
             #:characters (cons (cons player player-pos) monster-poss)))


(module+ main
  (require "grid.rkt" "player.rkt")
  (display (show-grid (state-grid (generate (new player%))))))
