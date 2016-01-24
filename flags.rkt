#lang racket/base

(provide (all-defined-out))

(define debug:reveal-map #f)
(define (set-debug:reveal-map!) (set! debug:reveal-map #t))

(define debug:god-mode #f)
(define (set-debug:god-mode!) (set! debug:god-mode #t))
