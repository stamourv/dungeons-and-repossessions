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

(define (next-state s action-taken)
  (match-define (state floor q active mode) s)
  (define (new-move-state n)
    (state floor q active `(move ,n)))
  (define (new-attack-state)
    (state floor q active 'attack))
  (match mode
    [`(move ,n-moves-left)
     (define new-n (sub1 n-moves-left))
     ;; TODO take action-taken into account
     (if (zero? new-n)
         (new-attack-state)
         (new-move-state new-n))]
    ['attack
     ;; TODO take action-taken into account
     ;; TODO change active character, once we have more than 1 character
     ;;   probably need a character queue (order of initiative) for that
     ;;   (could have global initiative for starters, then per-encounter later)
     (new-move-state (get-field speed active))]
    [else
     (raise-argument-error 'next-state "valid mode" mode)]))
