#lang racket

(require racket/random
         "state.rkt" "message-queue.rkt" "grid.rkt" "vision.rkt" "utils.rkt")

(provide (all-defined-out))

;; just sit there doing nothing
(define (wait-ai this)
  (lambda (state)
    ;; to avoid printing both when moving and attacking
    (when (equal? (state-mode state) 'attack)
      (enqueue-message! (format "~a waits."
                                (send this describe
                                      #:capitalize? #t #:specific? #t))))
    'wait))

;; moves at random, which attacks if it runs into something
(define (random-move-ai this)
  (lambda (state)
    (define mode (state-mode state))
    (case (random 5)
      [(0) (send this move-left  mode)]
      [(1) (send this move-right mode)]
      [(2) (send this move-up    mode)]
      [(3) (send this move-down  mode)]
      [(4) 'wait])))

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

(define (rush pos state)
  (match-define (vector player-x player-y) (get-player-pos state))
  (match-define (vector pos-x    pos-y)    pos)
  (or (and (> player-x pos-x) (pos-if-ok (down pos)  state))
      (and (> player-y pos-y) (pos-if-ok (right pos) state))
      (and (< player-x pos-x) (pos-if-ok (up pos)    state))
      (and (< player-y pos-y) (pos-if-ok (left pos)  state))))

(define-syntax-rule (with-fov seen-player? state [seen ...] [not-seen ...])
  (let ()
    (define pos (get-field pos (first (state-initiative-order state))))
    (define grid (state-grid state))
    (define player-pos (get-player-pos state))
    (cond [(or seen-player?
               (and (set-member? (compute-fov grid pos 7) ; arbitrary range
                                 player-pos)
                    (set! seen-player? #t)))
           seen ...]
          [else not-seen ...])))

;; goes towards the player as directly as possible and attacks
(define (rush-ai this)
  ;; until we see the player, just wait. once we do, though, pursue
  (define seen-player? #f)
  (define (act state)
    (define pos        (get-field  pos this))
    (with-fov seen-player? state
              [(define new-pos (rush pos state))
               (go-or-wait this new-pos state)]
              ['wait]))
  act)

(define (cower pos state)
  (define player-pos (get-player-pos state))
  (match-define (vector pos-x    pos-y)    pos)
  (match-define (vector player-x player-y) player-pos)
  (cond
   [(adjacent? pos player-pos) ; attack
    player-pos]
   [else ; cower. essentially the reverse of rush
    (or (and (> player-x pos-x) (pos-if-ok (up pos)    state))
        (and (> player-y pos-y) (pos-if-ok (left pos)  state))
        (and (< player-x pos-x) (pos-if-ok (down pos)  state))
        (and (< player-y pos-y) (pos-if-ok (right pos) state)))]))

;; runs away from the player, but attacks if player is adjacent
(define (cower-ai this)
  (lambda (state)
    (define new-pos (cower (get-field pos this) state))
    (go-or-wait this new-pos state)))

;; moves at random, until the player gets close enough (adjacent, currently),
;; in which case it attacks
(define (wander-ai this)
  (lambda (state)
    (define pos        (get-field pos this))
    (define player-pos (get-player-pos state))
    (cond [(adjacent? pos player-pos)
           (go-or-wait this player-pos state)]
          [else ; wander randomly
           (go-or-wait this
                       (pos-if-ok ((random-ref (list up down left right)) pos)
                                  state)
                       state)])))

;; rushes, but once injured, cowers
;; TODO have it reset to rush after some time (like the fallen in diablo)
(define (injury-shy-ai this)
  (define seen-player? #f) ; don't rush until we've actually seen the player
  (lambda (state)
    (define pos (get-field pos this))
    (with-fov seen-player? state
              [(define new-pos
                 (if (= (get-field current-hp this) (get-field max-hp this))
                     (rush  pos state)
                     (cower pos state)))
               (go-or-wait this new-pos state)]
              ['wait])))
