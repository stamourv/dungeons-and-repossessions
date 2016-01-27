#lang racket

(provide (all-defined-out))

(define (article capitalize? specific?
                 #:an? [an? #f])
  (if specific?
      (if capitalize? "The" "the")
      (if an?
          (if capitalize? "An" "an")
          (if capitalize? "A"  "a"))))

(define (d4)  (random 1 5))
(define (d6)  (random 1 7))
(define (d8)  (random 1 9))
(define (d10) (random 1 11))
(define (d12) (random 1 13))
(define (d20) (random 1 21))

(define (dice . args)
  (define (loop args)
    (match args
      ['()
       0]
      [`(,(? integer? n))
       n]
      [`(,(? integer? n) ,d . ,rest)
       (+ (for/sum ([i (in-range n)]) (d))
          (loop rest))]
      [`(,d . ,rest)
       (+ (d) (loop rest))]))
  (lambda _ (max 1 (loop args))))
