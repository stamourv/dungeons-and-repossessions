#lang racket

(require racket/format
         math/array math/matrix
         "cell.rkt")

;; a Map is a math/matrix Matrix of cell%

;; parses a list of strings into a map, based on the printed representation
;; of each cell
(define (parse-map los)
  (for*/matrix (length los)
               (apply max (map string-length los))
               #:fill (new cell%)
               ([s (in-list los)]
                [c (in-string s)])
     (new (char->cell% c))))

(define (show-map m)
  (with-output-to-string
    (lambda ()
      (for ([r (in-list (matrix-rows m))])
        (for ([c (in-array r)])
          (display (send c show)))
        (newline)))))


(module+ test
  (require rackunit)

  (define (parse-and-show los) (show-map (parse-map los)))
  (define (render-map m) (string-join m "\n" #:after-last "\n"))

  (define m1
    '(" "))
  (check-equal? (parse-and-show m1) " \n")

  (define m2
    '("**********"
      "*        *"
      "*        *"
      "*        *"
      "**********"))
  (check-equal? (parse-and-show m2) (render-map m2))

  (define m3 ; padding should work
    '("**********"
      "*        *"
      "*        *"
      "*        *"
      "*****"))
  (check-equal? (parse-and-show m3) (render-map m2))
  )
