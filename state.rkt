#lang racket

(provide (all-defined-out))

;; message-queue is a list of strings (messages) which were produced
;; during the previous round, and need to be displayed now
(struct state
  (floor
   [message-queue #:mutable]
   active-character ; is-a? character%
   mode)) ; a mode is either `(move ,n-moves-left) or 'attack

(define (enqueue-message! s m)
  (set-state-message-queue! s (cons m (state-message-queue s))))

(define current-state (make-parameter #f))

;; TODO allow taking moves and attacks to be in a different order, or broken up
;;   (for now, always move, then attack, then end of turn)
(define (next-state s action-taken)
  (match-define (state floor q active mode) s)
  (define (new-turn)
    ;; TODO change active character, once we have more than 1 character
    ;;   probably need a character queue (order of initiative) for that
    ;;   (could have global initiative for starters, then per-encounter later)
    (new-move-state (get-field speed active)))
  (define (new-move-state n)
    (state floor q active `(move ,n)))
  (define (new-attack-state)
    (state floor q active 'attack))
  (match mode
    [`(move ,n-moves-left)
     (case action-taken
       [(wait)
        (new-attack-state)]
       [(move)
        (define new-n (sub1 n-moves-left))
        (if (zero? new-n) ; no more moves
            (new-attack-state)
            (new-move-state new-n))]
       [(attack) ; end move prematurely to attack
        (new-turn)]
       [(invalid) ; doesn't count against the number of moves
        s]
       [else
        (raise-argument-error
         'next-state "valid action (move state)" action-taken)])]
    ['attack
     (case action-taken
       [(wait)
        (new-turn)]
       [(move) ; forego attack for double move
        (new-move-state (sub1 (get-field speed active)))] ; already moved 1
       [(attack)
        (new-turn)]
       [(invalid) ; try again
        s]
       [else
        (raise-argument-error
         'next-state "valid action (attack state)" action-taken)])]
    [else
     (raise-argument-error 'next-state "valid mode" mode)]))
