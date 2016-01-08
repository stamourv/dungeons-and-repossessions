#lang racket

(require math/array math/matrix
         "cell.rkt")

(provide (all-defined-out))

;; a Grid is a math/matrix Matrix of cell%

;; parses a list of strings into a grid, based on the printed representation
;; of each cell
(define (parse-grid los)
  (for*/matrix (length los)
               (apply max (map string-length los))
               #:fill (new cell%)
               ([s (in-list los)]
                [c (in-string s)])
     (new (char->cell% c))))

(define (show-grid g)
  (with-output-to-string
    (lambda ()
      (for ([r (in-list (matrix-rows g))])
        (for ([c (in-array r)])
          (display (send c show)))
        (newline)))))

(define (within-grid? g pos)
  (and (<= 0 (vector-ref pos 0) (sub1 (matrix-num-rows g)))
       (<= 0 (vector-ref pos 1) (sub1 (matrix-num-cols g)))))
(define (grid-ref g pos)
  (and (within-grid? g pos)
       (array-ref g pos)))

(define (left pos)
  (vector (vector-ref pos 0)
          (sub1 (vector-ref pos 1))))
(define (right pos)
  (vector (vector-ref pos 0)
          (add1 (vector-ref pos 1))))
(define (up pos)
  (vector (sub1 (vector-ref pos 0))
          (vector-ref pos 1)))
(define (down pos)
  (vector (add1 (vector-ref pos 0))
          (vector-ref pos 1)))


(module+ test
  (require rackunit)

  (define (parse-and-show los) (show-grid (parse-grid los)))
  (define (render-grid g) (string-join g "\n" #:after-last "\n"))

  (define g1
    '(" "))
  (check-equal? (parse-and-show g1) " \n")

  (define g2
    '("**********"
      "*        *"
      "*        *"
      "*        *"
      "**********"))
  (check-equal? (parse-and-show g2) (render-grid g2))

  (define g3 ; padding should work
    '("**********"
      "*        *"
      "*        *"
      "*        *"
      "*****"))
  (check-equal? (parse-and-show g3) (render-grid g2))

  (define g2* (parse-grid g2))
  (check-true (within-grid? g2* '#(0 0)))
  (check-true (within-grid? g2* '#(0 1)))
  (check-true (within-grid? g2* '#(1 0)))
  (check-true (within-grid? g2* '#(4 4)))
  (check-false (within-grid? g2* '#(0 10)))
  (check-false (within-grid? g2* '#(5 0)))
  (check-false (within-grid? g2* '#(5 10)))
  )
