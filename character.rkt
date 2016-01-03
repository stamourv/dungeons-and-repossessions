#lang racket

(require math/array
         "grid.rkt"
         "state.rkt")

(provide player%
         training-dummy%)

;; should not be instantiated directly (hence not exported)
;; interfaces can't have method definitions (AFAICT), so this "abstract class"
;; will have to do
(define character%
  (class object%
    (field [grid #f]  ; grid where the character is active
           [pos  #f]  ; 2-vector of integer (what math/array uses as indices)
           [speed 6]) ; default to human speed (30 ft = 6 squares)

    (define/public (move new-pos)
      (cond
       [(within-grid? grid new-pos) ; don't go off the map
        (define new-cell (array-ref grid new-pos))
        (cond
         [(send new-cell free?) ; can move there
          (when pos ; when initially placed, won't have a position
            (set-field! occupant (array-ref grid pos) #f))
          (set! pos new-pos)
          (set-field! occupant new-cell this)
          'move] ; return the action we took
         [(get-field occupant new-cell) =>
          (lambda (occ)
            (attack this occ))] ; we attack whoever is there
         [else
          'invalid])]
       [else
        'invalid]))
    (define/public (move-left)
      (move (left pos)))
    (define/public (move-right)
      (move (right pos)))
    (define/public (move-up)
      (move (up pos)))
    (define/public (move-down)
      (move (down pos)))

    (super-new)))

(define (attack attacker defender)
  (enqueue-message! "Attack!")
  'attack)

(define player%
  (class character%
    (define/public (show)
      #\@) ;; TODO add parsing for player position
    (super-new)))

(define training-dummy%
  (class character%
    (define/public (show)
      #\D)
    (super-new)))
