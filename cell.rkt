#lang racket

(require "message-queue.rkt")

(provide (all-defined-out))

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
    (define/public (opaque?)
      #t)
    (define/public (show [show-occupant? #t])
      #\*) ; for debugging
    (define/public (open)
      (enqueue-message! "Can't open that."))
    (define/public (close)
      (enqueue-message! "Can't close that."))
    (super-new)))
(register-cell-type! cell% #\*)

(define (free-cell-show this char [show-occupant? #t])
  (define occupant (get-field occupant this))
  (if (and show-occupant? occupant)
      (send occupant show)
      char))

(define empty-cell%
  (class cell%
    (inherit-field occupant)
    (define/override (free?)
      (not occupant))
    (define/override (opaque?)
      #f)
    (define/override (show [show-occupant? #t])
      (free-cell-show this #\space show-occupant?))
    (super-new)))
(register-cell-type! empty-cell% #\space)

(define void-cell%
  (class cell%
    (define/override (show [show-occupant? #t]) #\.) ; for testing only
    (super-new)))
(register-cell-type! void-cell% #\.)

(define wall%
  (class cell%
    (define/override (show [show-occupant? #t]) #\X) ; for testing only
    (super-new)))
(register-cell-type! wall% #\X)

;; some walls, e.g., tee walls, can leak information about what's on the
;; other side, which we don't want if the other side is not explored yet
;; those are handled specially by FOV display
(define info-leak-wall% (class wall% (super-new)))

(define double-bar? #t)
(define-syntax-rule (define-wall name single-bar double-bar super-class)
  (begin (define name
           (class super-class
             (define/override (show [show-occupant? #t])
               (if double-bar? double-bar single-bar))
             (super-new)))
         ;; parse either kind
         (register-cell-type! name single-bar)
         (register-cell-type! name double-bar)))
(define-wall vertical-wall%    #\u2502 #\u2551 wall%)
(define-wall horizontal-wall%  #\u2500 #\u2550 wall%)
(define-wall north-east-wall%  #\u2510 #\u2557 wall%)
(define-wall north-west-wall%  #\u250c #\u2554 wall%)
(define-wall south-east-wall%  #\u2518 #\u255d wall%)
(define-wall south-west-wall%  #\u2514 #\u255a wall%)
(define-wall north-tee-wall%   #\u252c #\u2566 info-leak-wall%)
(define-wall south-tee-wall%   #\u2534 #\u2569 info-leak-wall%)
(define-wall east-tee-wall%    #\u2524 #\u2563 info-leak-wall%)
(define-wall west-tee-wall%    #\u251c #\u2560 info-leak-wall%)
(define-wall four-corner-wall% #\u253c #\u256c info-leak-wall%)

(define pillar%
  (class cell%
    (define/override (show [show-occupant? #t])
      (if double-bar? #\# #\+))
    (super-new)))
(register-cell-type! pillar% #\#)
(register-cell-type! pillar% #\+)


(define door%
  (class cell%
    (init-field [open? #f])
    (inherit-field occupant)
    (define/override (free?)
      (and open? (not occupant)))
    (define/override (opaque?)
      (not open?))
    (define/override (open)
      (if open?
          (enqueue-message! "The door is already open.")
          (set! open? #t)))
    (define/override (close)
      (if open?
          (set! open? #f)
          (enqueue-message! "The door is already closed.")))
    (super-new)))
(define vertical-door%
  (class door%
    (inherit-field open? occupant)
    (define/override (show [show-occupant? #t])
      (if open?
          (free-cell-show this #\_ show-occupant?)
          #\|))
    (super-new)))
(register-cell-type! vertical-door% #\|)
(register-cell-type! (class vertical-door% (super-new [open? #t])) #\_)
(define horizontal-door%
  (class door%
    (inherit-field open? occupant)
    (define/override (show [show-occupant? #t])
      (if open?
          (free-cell-show this #\' show-occupant?)
          #\-))
    (super-new)))
(register-cell-type! horizontal-door% #\-)
(register-cell-type! (class horizontal-door% (super-new [open? #t])) #\')

;; TODO chests, entry/exit
