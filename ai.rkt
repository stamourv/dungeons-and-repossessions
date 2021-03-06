#lang racket

(require racket/random
         "state.rkt" "message-queue.rkt" "grid.rkt" "vision.rkt" "utils.rkt")

(provide (all-defined-out))

(define ai%
  (class object%
    (init-field monster)
    (field [seen-player? #f])

    (define/public (act state)
      (error "this ai% can't act"))

    (define/public (update-fov state)
      ;; if we already saw the player, then just use its position direcly
      ;; we're not operating by sight anymore
      (unless seen-player?
        (define pos        (get-field pos monster))
        (define player-pos (get-player-pos state))
        (define grid       (get-field grid monster))
        (when (set-member? (compute-fov grid pos 7) ; arbitrary range
                           player-pos)
          (wake-up))))

    (define/public (wake-up) ; someone saw the player
      (unless seen-player? ; already woke up. to avoid infinite loops
        ;; Note: `seen-player?` must not be set directly elsewhere, otherwise
        ;;   we're not going to wake up the rest of the encounter
        (set! seen-player? #t)
        (for ([m (in-list (get-field encounter monster))]) ; wake up others
          (send m wake-up))))

    (super-new)))


;; just sit there doing nothing
(define wait-ai%
  (class ai%
    (inherit-field monster)
    (define/override (act state)
      (enqueue-message! (format "~a waits."
                                (send monster describe
                                      #:capitalize? #t #:specific? #t)))
      'wait)
    (super-new)))

;; moves at random, which attacks if it runs into something
(define random-move-ai%
  (class ai%
    (inherit-field monster)
    (define/override (act state)
      (define mode (state-mode state))
      (case (random 5)
        [(0) (send monster move-left  mode)]
        [(1) (send monster move-right mode)]
        [(2) (send monster move-up    mode)]
        [(3) (send monster move-down  mode)]
        [(4) 'wait]))
    (super-new)))

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
  (define player-pos (get-player-pos state))
  (define grid       (state-grid     state))
  (define path
    (find-path grid pos player-pos
               ;; penalize occupied tiles to avoid pileups and go around
               #:extra-heuristic (lambda (g pos)
                                   (if (send (grid-ref g pos) free?) 0 10))))
  (and path (pos-if-ok (first path) state)))

;; goes towards the player as directly as possible and attacks
(define rush-ai%
  (class ai%
    (inherit update-fov)
    (inherit-field monster seen-player?)
    (define/override (act state)
      ;; until we see the player, just wait. once we do, though, pursue
      (define pos (get-field pos monster))
      (update-fov state)
      (if seen-player?
          (go-or-wait monster (rush pos state) state)
          'wait))
    (super-new)))

(define (cower pos state)
  (define player-pos (get-player-pos state))
  (match-define (vector pos-x    pos-y)    pos)
  (match-define (vector player-x player-y) player-pos)
  (cond
   [(adjacent? pos player-pos) ; attack
    player-pos]
   [else ; get away from the player
    (or (and (> player-x pos-x) (pos-if-ok (up pos)    state))
        (and (> player-y pos-y) (pos-if-ok (left pos)  state))
        (and (< player-x pos-x) (pos-if-ok (down pos)  state))
        (and (< player-y pos-y) (pos-if-ok (right pos) state)))]))

;; runs away from the player, but attacks if player is adjacent
(define cower-ai%
  (class ai%
    (inherit-field monster)
    (define/override (act state)
      (define new-pos (cower (get-field pos monster) state))
      (go-or-wait monster new-pos state))
    (super-new)))

;; moves at random, until the player gets close enough (adjacent, currently),
;; in which case it attacks
(define wander-ai%
  (class ai%
    (inherit update-fov)
    (inherit-field monster seen-player?)
    (define/override (act state)
      (define pos        (get-field pos monster))
      (define player-pos (get-player-pos state))
      (update-fov state)
      (cond [(and seen-player? ; within range. rush
                  (< (manhattan-distance pos player-pos) 5))
             (go-or-wait monster (rush pos state) state)]
            [else ; wander randomly
             (go-or-wait monster
                         (pos-if-ok ((random-ref (list up down left right)) pos)
                                    state)
                         state)]))
    (super-new)))

;; rushes, but once injured, cowers
;; TODO have it reset to rush after some time (like the fallen in diablo)
(define injury-shy-ai%
  (class ai%
    (inherit update-fov)
    (inherit-field monster seen-player?)
    (define/override (act state) ; don't rush until we've seen the player
      (define pos (get-field pos monster))
      (update-fov state)
      (cond [seen-player?
             (define new-pos
               (if (= (get-field current-hp monster)
                      (get-field max-hp monster))
                   (rush  pos state)
                   (cower pos state)))
             (go-or-wait monster new-pos state)]
            [else
             'wait]))
    (super-new)))
