#lang racket

(provide
 char->cell%
 cell%
 empty-cell%)

;; maps printed representations to cell classes
;; for map parsing
(define chars->cell%s (make-hash))
(define (register-cell-type! c% char)
  (dict-set! chars->cell%s char c%))
(define (char->cell% char)
  (dict-ref chars->cell%s char))

(define cell%
  (class object%
    (define/public (free?)
      #f)
    (define/public (show)
      #\*) ; for debugging
    (super-new)))
(register-cell-type! cell% #\*)

(define empty-cell%
  (class cell%
    (init-field [items    '()]
                [occupant #f]) ; player, monster, etc.
    (define/override (free?)
      (not occupant))
    (define/override (show)
      #\space)
    (super-new)))
(register-cell-type! empty-cell% #\space)

;; TODO walls, etc.
