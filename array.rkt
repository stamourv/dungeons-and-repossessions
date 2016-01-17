#lang racket

;; "reimplementation" of part of math/array
;; the typed-untyed boundary is way too expensive, and unsafe-provide
;; doesn't eliminate the entire cost
;; out interface is designed to be compatible, in case we want to switch
;; back to using math/array

(provide (all-defined-out))

(struct array (n-rows n-cols cells) #:transparent)

(define (array-shape a) (vector (array-n-rows a) (array-n-cols a)))

(define (build-array shape fun)
  (match-define (vector h w) shape)
  (array h w (for*/vector #:length (* h w)
                          ([x (in-range h)]
                           [y (in-range w)])
               (fun (vector x y)))))

(define array->mutable-array values) ; already mutable

(define-syntax-rule (for*/array #:shape shape (clause ...) body ...)
  (let* ([s shape]
         [h (vector-ref shape 0)]
         [w (vector-ref shape 1)])
    (array h w (for*/vector #:length (* h w) (clause ...) body ...))))

(define (pos->idx pos a)
  (match-define (vector x y) pos)
  (+ (* x (array-n-cols a)) y))
(define (array-ref a pos)
  (vector-ref (array-cells a) (pos->idx pos a)))
(define (array-set! a pos v)
  (vector-set! (array-cells a) (pos->idx pos a) v))

;; from math/array, and adapted
;; does not yield arrays (which for us are always 2d, never 1d)
;; instead yields vectors, so we need to always follow `in-array-axis`
;; with `in-array`
(define (in-array-axis arr)
  (define dk (array-n-rows arr))
  (make-do-sequence
   (λ ()
     (values (λ (jk) (array-axis-ref arr jk))
             add1
             0
             (λ (jk) (jk . < . dk))
             #f
             #f))))
(define (array-axis-ref arr jk)
  (define n-cols (array-n-cols arr))
  (define cells  (array-cells  arr))
  (build-vector (array-n-cols arr)
                (lambda (y) (vector-ref cells (+ (* jk n-cols) y)))))

;; not true in general, but true because we only ever used `in-array`
;; on the result of `in-array-axis`
(define-syntax-rule (in-array x ...) (in-vector x ...))

(define (mutable-array-copy a)
  (match-define (array n-rows n-cols cells) a)
  (array n-rows n-cols (vector-copy cells)))
