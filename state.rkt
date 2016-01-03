#lang racket/base

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
  s) ;; TODO
