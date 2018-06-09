#lang racket/base

(provide (all-defined-out))

(define-syntax-rule (define-queue enqueue! drain!)
  (begin (define queue '())
         (define (enqueue! m)
           (set! queue (cons m queue)))
         (define (drain!)
           (begin0 (reverse queue)
             (set! queue '())))))

;; list of strings (messages) which were produced since the previous
;; display, and need to be displayed now
(define-queue enqueue-message! drain-messages!)

;; similar, but for mission briefing and ending
(define-queue enqueue-briefing! drain-briefing!)
(define-queue enqueue-ending!   drain-ending!)
