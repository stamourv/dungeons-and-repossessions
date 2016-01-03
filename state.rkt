#lang racket

(provide (all-defined-out))

;; message-queue is a list of strings (messages) which were produced
;; during the previous round, and need to be displayed now
(struct state
  (player
   floor
   [message-queue #:mutable]
   initiative-order ; listof character%
   mode)) ; a mode is either `(move ,n-moves-left) or 'attack

(define (enqueue-message! m [s (current-state)])
  (set-state-message-queue! s (cons m (state-message-queue s))))

(define current-state (make-parameter #f))

;; TODO allow taking moves and attacks to be in a different order, or broken up
;;   (for now, always move, then attack, then end of turn)
(define (next-state s action-taken)
  (match-define (state player the-floor q initiative-order mode) s)
  (define (new-turn) ; switch active character and go back to moving
    (define new-initiative-order
      (append (rest initiative-order) (list (first initiative-order))))
    (state player
           the-floor
           q
           new-initiative-order
           `(move ,(get-field speed (first new-initiative-order)))))
  (define (new-move-state n)
    (state player the-floor q initiative-order `(move ,n)))
  (define (new-attack-state)
    (state player the-floor q initiative-order 'attack))
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
        ;; TODO bug: since we go back to the move mode, it's never anyone else's turn
        (new-move-state
         (sub1 (get-field speed (first initiative-order))))] ; already moved 1
       [(attack)
        (new-turn)]
       [(invalid) ; try again
        s]
       [else
        (raise-argument-error
         'next-state "valid action (attack state)" action-taken)])]
    [else
     (raise-argument-error 'next-state "valid mode" mode)]))
