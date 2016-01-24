#lang racket/base

(require (for-syntax racket/base racket/syntax))

(provide (all-defined-out))

(define-syntax (define-flag stx)
  (syntax-case stx ()
    [(_ name)
     #`(begin (define name #f)
              (define (#,(format-id #'name "set-~a!" #'name))
                (set! name #t)))]))

(define-flag debug:reveal-map)
(define-flag debug:god-mode)
