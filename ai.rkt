#lang racket

(require "state.rkt" "message-queue.rkt" "grid.rkt")

(provide (all-defined-out))

;; just sit there doing nothing
(define (wait-ai this state)
  ;; to avoid printing both when moving and attacking
  (when (equal? (state-mode state) 'attack)
    (enqueue-message! (format "~a waits."
                              (send this describe
                                    #:capitalize? #t #:specific? #t))))
  'wait)

;; moves at random, which attacks if it runs into something
(define (random-move-ai this state)
  (define mode (state-mode state))
  (case (random 5)
    [(0) (send this move-left  mode)]
    [(1) (send this move-right mode)]
    [(2) (send this move-up    mode)]
    [(3) (send this move-down  mode)]
    [(4) 'wait]))

;; goes towards the player as directly as possible and attacks
;; TODO once we add visibility, have it not "see" the player through walls
(define (rush-ai this state)
  (define grid   (state-grid   state))
  (define player (state-player state))
  (match-define (vector player-x player-y) (get-field pos player))
  (match-define (vector pos-x    pos-y)    (get-field pos this))
  (define (go-if-ok pos)
    ;; can go there is empty or player is there (attack)
    ;; otherwise, it's a monster, and we'd be doing friendly fire
    (define c   (grid-ref grid pos))
    (define occ (get-field occupant c))
    (and (or (send c free?) (equal? occ player))
         pos))
  (define new-pos
    (or (and (> player-x pos-x) (go-if-ok (vector (add1 pos-x) pos-y)))
        (and (> player-y pos-y) (go-if-ok (vector pos-x (add1 pos-y))))
        (and (< player-x pos-x) (go-if-ok (vector (sub1 pos-x) pos-y)))
        (and (< player-y pos-y) (go-if-ok (vector pos-x (sub1 pos-y))))))
  (define action-type
    (if new-pos
        (send this move new-pos (state-mode state))
        'wait)) ; nowhere to really go
  (if (equal? action-type 'invalid)
      ;; tried running into a wall, or attacking after we've started dashing
      ;; need to stop what we're doing, or we'll keep trying to do it forever
      'wait
      action-type))

;; TODO want an AI that runs away, and only attacks is adjacent
;;   for bat, commoner
;; TODO want an AI that moves at random, but if player is adjacent (or near),
;;   then it attacks
;;   for rat, spider, zombie?
;; TODO want an AI that rushes, but once it gets injured, cowers
;;   for kobolds, goblins?, acolytes?

;; TODO move sure AIs avoid friendly fire
