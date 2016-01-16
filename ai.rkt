#lang racket

(require "state.rkt" "message-queue.rkt" "grid.rkt" "utils.rkt")

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
(define (get-player-pos state)
  (get-field pos (state-player state)))

(define (pos-if-ok pos state)
  ;; can go there is empty or player is there (attack)
  ;; otherwise, it's a monster, and we'd be doing friendly fire
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
  (define pos (get-field pos this))
  (match-define (vector player-x player-y) (get-player-pos state))
  (match-define (vector pos-x    pos-y)    pos)
  (define new-pos
    (or (and (> player-x pos-x) (pos-if-ok (down pos)  state))
        (and (> player-y pos-y) (pos-if-ok (right pos) state))
        (and (< player-x pos-x) (pos-if-ok (up pos)    state))
        (and (< player-y pos-y) (pos-if-ok (left pos)  state))))
  (go-or-wait this new-pos state))

;; runs away from the player, but attacks if player is adjacent
(define (cower-ai this state)
  (define pos        (get-field pos this))
  (define player-pos (get-player-pos state))
  (match-define (vector pos-x    pos-y)    pos)
  (match-define (vector player-x player-y) player-pos)
  (cond
   [(adjacent? pos player-pos) ; attack
    (go-or-wait this player-pos state)]
   [else ; cower. essentially the reverse of rush
    (define new-pos
      (or (and (> player-x pos-x) (pos-if-ok (up pos)    state))
          (and (> player-y pos-y) (pos-if-ok (left pos)  state))
          (and (< player-x pos-x) (pos-if-ok (down pos)  state))
          (and (< player-y pos-y) (pos-if-ok (right pos) state))))
    (go-or-wait this new-pos state)]))

;; moves at random, until the player gets close enough (adjacent, currently),
;; in which case it attacks
(define (wander-ai this state)
  (define pos        (get-field pos this))
  (define player-pos (get-player-pos state))
  (cond [(adjacent? pos player-pos)
         (go-or-wait this player-pos state)]
        [else ; wander randomly
         (go-or-wait this
                     (pos-if-ok ((random-from (list up down left right)) pos)
                                state)
                     state)]))

;; TODO want an AI that rushes, but once it gets injured, cowers
;;   for kobolds, goblins?, acolytes?

;; TODO move sure AIs avoid friendly fire
