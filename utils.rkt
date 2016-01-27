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
