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

(define cell% ; some kind of obstacle by default
  (class object%
    (init-field [items    '()]
                [occupant #f]) ; player, monster, etc.
    (define/public (free?)
      #f)
    (define/public (show)
      #\*) ; for debugging
    (super-new)))
(register-cell-type! cell% #\*)

(define empty-cell%
  (class cell%
    (inherit-field occupant)
    (define/override (free?)
      (not occupant))
    (define/override (show)
      (if occupant
          (send occupant show)
          #\space))
    (super-new)))
(register-cell-type! empty-cell% #\space)

(define double-bar? #t)
(define-syntax-rule (define-wall name single-bar double-bar)
  (begin (define name
           (class cell%
             (define/override (show) (if double-bar? double-bar single-bar))
             (super-new)))
         ;; parse either kind
         (register-cell-type! name single-bar)
         (register-cell-type! name double-bar)
         (provide name)))
(define-wall pillar%           #\+     #\+)
(define-wall vertical-wall%    #\u2502 #\u2551)
(define-wall horizontal-wall%  #\u2500 #\u2550)
(define-wall four-corner-wall% #\u253c #\u256c)
(define-wall north-east-wall%  #\u2510 #\u2557)
(define-wall north-west-wall%  #\u250c #\u2554)
(define-wall south-east-wall%  #\u2518 #\u255d)
(define-wall south-west-wall%  #\u2514 #\u255a)
(define-wall north-tee-wall%   #\u252c #\u2566)
(define-wall south-tee-wall%   #\u2534 #\u2569)
(define-wall east-tee-wall%    #\u2524 #\u2563)
(define-wall west-tee-wall%    #\u251c #\u2560)

;; TODO chests, doors, entry/exit
