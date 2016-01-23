#lang racket

(require racket/random graph
         "encounters.rkt"
         "dungeon.rkt"
         "state.rkt"
         "grid.rkt"
         "cell.rkt"
         "items.rkt"
         "utils.rkt")

(provide generate)

(define (room-centroid r)
  (match-define (room (vector x y) height width _1 _2) r)
  (vector (+ x (/ height 2.0)) (+ y (/ width 2.0))))

(define (manhattan-distance p1 p2)
  (match-define (vector x1 y1) p1)
  (match-define (vector x2 y2) p2)
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(define (random-room-poss room n)
  (random-sample (room-free-cells room) n #:replacement? #f))

(define (claim-room-cell! room pos)
  (set-room-free-cells! room (remove pos (room-free-cells room))))


(define (generate player)
  (define lvl        (get-field level player))
  (define encounters (generate-encounters lvl))
  ;; need enough rooms for all the encounters, plus a starting room
  ;; (we don't want monsters in the starting room)
  (define n-encounters (length encounters))
  (match-define (dungeon grid rooms connections)
    (generate-dungeon (add1 n-encounters)))

  ;; build room graph
  (define graph
    (weighted-graph/undirected
     (for*/list ([(r1 r2) (in-dict connections)]
                 ;; `connections` includes corridors, which we don't care
                 ;; about (and who don't keep track of pos, height and width
                 ;; so we can't use them for the graph anyway)
                 ;; Note: we may get nicer results if we could, though
                 #:when (and (room? r1) (room? r2)))
       (define weight
         (manhattan-distance (room-centroid r1) (room-centroid r2)))
       (list weight r1 r2))))

  ;; place player and goal in furthest rooms
  ;; see: roguebasin.com/index.php?title=Creating_Measurably_%22Fun%22_Maps
  ;;  (idea based roughly on that. not following specific technique)
  (define all-pairs-shortest-path (floyd-warshall graph))
  (define-values (player-room goal-room _)
    (for/fold ([player-room  #f]
               [goal-room    #f]
               [max-distance 0])
        ([(r1+r2 distance) (in-hash all-pairs-shortest-path)])
      (match-define (list r1 r2) r1+r2)
      (if (> distance max-distance) ; found a more distant pair
          (values r1 r2 distance)
          (values player-room goal-room max-distance))))
  (define player-pos (first (random-room-poss player-room 1)))
  (claim-room-cell! player-room player-pos)
  (array-set! grid player-pos (new entrance%))
  (define goal-pos (first (random-room-poss goal-room 1)))
  (claim-room-cell! goal-room goal-pos)
  (array-set! grid goal-pos (new chest% [items (list (new macguffin%))]))

  ;; place encounters
  (define encounter-rooms ; excludes player's room. don't start with monsters
    (random-sample (remove player-room rooms) n-encounters #:replacement? #f))
  (define monster-poss
    (for/list ([e    (in-list encounters)]
               [r    (in-list encounter-rooms)]
               #:when #t ; nest iteration
               [poss (in-value (random-room-poss r (length e)))]
               #:when #t ; nest iteration
               [m    (in-list (instantiate-encounter e))]
               [pos  (in-list poss)])
      (claim-room-cell! r pos)
      (cons m pos)))
  (new-state player grid
             #:characters (cons (cons player player-pos) monster-poss)))


(module+ main
  (require "grid.rkt" "player.rkt")
  (display (show-grid (state-grid (generate (new player%))))))
