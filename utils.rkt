#lang racket

(provide (all-defined-out))

(define (article capitalize? specific?
                 #:an? [an? #f])
  (if specific?
      (if capitalize? "The" "the")
      (if an?
          (if capitalize? "An" "an")
          (if capitalize? "A"  "a"))))

(define (random-between min max) ;; TODO replace with 6.4's `random`
  (+ min (random (- max min))))
(define (d4)  (random-between 1 5))
(define (d6)  (random-between 1 7))
(define (d20) (random-between 1 21))

(define (random-from l) ;; TODO replace with 6.4's `random-ref`
  (first (shuffle l)))

;; TODO taken straight from 6.4. remove once I upgrade
(define (random-sample seq n [prng (current-pseudo-random-generator)]
                       #:replacement? [replacement? #t])
  ;; doing reservoir sampling, to do a single pass over the sequence
  ;; (some sequences may not like multiple passes, e.g., ports)
  (cond
   [(zero? n) '()]
   [(not replacement?)
    ;; Based on: http://rosettacode.org/wiki/Knuth's_algorithm_S#Racket
    (define not-there (gensym))
    (define samples (make-vector n not-there))
    (for ([elt seq]
          [i (in-naturals)])
      (cond [(< i n) ; we're not full, sample for sure
             (vector-set! samples i elt)]
            [(< (random (add1 i) prng) n) ; we've already seen n items; replace one?
             (vector-set! samples (random n prng) elt)]))
    ;; did we get enough?
    (unless (for/and ([s (in-vector samples)])
              (not (eq? s not-there)))
      (raise-argument-error 'random-sample
                            "integer less than or equal to sequence length"
                            n))
    (vector->list samples)]
   [else
    ;; similar to above, except each sample is independent
    (define samples #f)
    (for ([elt seq]
          [i (in-naturals)])
      (cond [(= i 0) ; initialize samples
             (set! samples (make-vector n elt))]
            [else ; independently, maybe replace
             (for ([j (in-range n)])
               (when (zero? (random (add1 i) prng))
                 (vector-set! samples j elt)))]))
    (unless samples
      (raise-argument-error 'random-sample
                            "non-empty sequence for n>0"
                            seq))
    (vector->list samples)]))
