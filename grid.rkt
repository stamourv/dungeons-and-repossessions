#lang racket

(require racket/format
         math/array math/matrix
         "cell.rkt")

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
  )
