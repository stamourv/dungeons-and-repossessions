#lang racket/base

(provide (all-defined-out))

;; list of strings (messages) which were produced since the previous
;; previous display, and need to be displayed now
(define message-queue '())
(define (enqueue-message! m)
  (set! message-queue (cons m message-queue)))
(define (reset-message-queue!)
  (set! message-queue '()))

;; similar, but for mission briefings
(define briefing-queue '())
(define (enqueue-briefing! m)
  (set! briefing-queue (cons m briefing-queue)))
(define (reset-briefing-queue!)
  (set! briefing-queue '()))
