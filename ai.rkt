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

;; TODO once we add visibility, have it not "see" the player through walls
(define (player-pos state)
  (get-field pos (state-player state)))

(define (pos-if-ok x y state)
  ;; can go there is empty or player is there (attack)
  ;; otherwise, it's a monster, and we'd be doing friendly fire
  (define pos (vector x y))
  (define c   (grid-ref (state-grid state) pos))
  (define occ (get-field occupant c))
  (and (or (send c free?) (equal? occ (state-player state)))
       pos))

(define (go-or-wait this pos state)
  (define action-type
    (if pos
        (send this move pos (state-mode state))
        'wait)) ; not going anywhere
  (if (equal? action-type 'invalid)
      ;; tried running into a wall, or attacking after we've started dashing
      ;; need to stop what we're doing, or we'll keep trying to do it forever
      'wait
      action-type))

;; goes towards the player as directly as possible and attacks
(define (rush-ai this state)
  (match-define (vector player-x player-y) (player-pos state))
  (match-define (vector pos-x    pos-y)    (get-field pos this))
  (define new-pos
    (or (and (> player-x pos-x) (pos-if-ok (add1 pos-x) pos-y state))
        (and (> player-y pos-y) (pos-if-ok pos-x (add1 pos-y) state))
        (and (< player-x pos-x) (pos-if-ok (sub1 pos-x) pos-y state))
        (and (< player-y pos-y) (pos-if-ok pos-x (sub1 pos-y) state))))
  (go-or-wait this new-pos state))

;; runs away from the player, but attacks if player is adjacent
(define (cower-ai this state)
  (match-define (vector player-x player-y) (player-pos state))
  (match-define (vector pos-x    pos-y)    (get-field pos this))
  (define player-adjacent?
    (or (and (= 1 (abs (- player-x pos-x))) (= player-y pos-y))
        (and (= player-x pos-x) (= 1 (abs (- player-y pos-y))))))
  (cond
   [player-adjacent? ; attack
    (go-or-wait this (vector player-x player-y) state)]
   [else ; cower. essentially the reverse of rush
    (define new-pos
      (or (and (> player-x pos-x) (pos-if-ok (sub1 pos-x) pos-y state))
          (and (> player-y pos-y) (pos-if-ok pos-x (sub1 pos-y) state))
          (and (< player-x pos-x) (pos-if-ok (add1 pos-x) pos-y state))
          (and (< player-y pos-y) (pos-if-ok pos-x (add1 pos-y) state))))
    (go-or-wait this new-pos state)]))

;; TODO want an AI that moves at random, but if player is adjacent (or near),
;;   then it attacks
;;   for rat, spider, zombie?
;; TODO want an AI that rushes, but once it gets injured, cowers
;;   for kobolds, goblins?, acolytes?

;; TODO move sure AIs avoid friendly fire
