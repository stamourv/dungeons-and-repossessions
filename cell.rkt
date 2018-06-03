#lang racket

(require "sprite.rkt" "message-queue.rkt")

(provide (all-defined-out))

;; maps printed representations to cell classes
;; for map parsing
(define chars->cell%s (make-hash))
(define (register-cell-type! c% char)
  (dict-set! chars->cell%s char c%))
(define (char->cell% char)
  (dict-ref chars->cell%s char))

(define cell% ; some kind of obstacle by default
  (class sprite%
    (init-field [items    '()]
                [occupant #f]) ; player, monster, etc.
    (define/public (free? #:occupant-ok? [_ #f]) #f)
    (define/public (opaque?)  #t)
    (define/public (show/fog) (send this show))
    (define/public (open)     (enqueue-message! "Can't open that."))
    (define/public (close)    (enqueue-message! "Can't close that."))
    (define/public (enter char) ; called when a character enters the cell
      (void))
    (super-new)))

(define (free-cell-show this char show-occupant?)
  (define occupant (get-field occupant this))
  (define items    (get-field items    this))
  (cond [(and show-occupant? occupant) (send occupant show)]
        [(not (empty? items))          (send (first items) show)]
        [else                          char]))

(define (add-item! cell item)
  (set-field! items cell (append (get-field items cell) (list item))))

(define free-cell%
  (class cell%
    (inherit-field occupant char)
    (define/override (free? #:occupant-ok? [ok? #f]) (or ok? (not occupant)))
    (define/override (opaque?)  #f)
    (define/override (show)     (free-cell-show this char #t))
    (define/override (show/fog) (free-cell-show this char #f))
    (super-new)))

(define empty-cell%
  (class free-cell%
    (super-new [char #\space] [name "empty cell"] [article "an"])))
(register-cell-type! empty-cell% #\space)

(define void-cell% (class cell% (super-new [char #\.] [name "void cell"])))
(register-cell-type! void-cell% #\.)

(define wall% (class cell% (super-new [name "wall"])))

;; during gameplay, the nicer walls below are used instead
(define raw-wall% (class wall% (super-new [char #\X])))
(register-cell-type! raw-wall% #\X)

;; some walls, e.g., tee walls, can leak information about what's on the
;; other side, which we don't want if the other side is not explored yet
;; those are handled specially by FOV display
(define info-leak-wall% (class wall% (super-new)))

(define double-bar? #t)
(define-syntax-rule (define-wall name single-bar double-bar super-class)
  (begin (define name
           (class super-class
             (super-new [char (if double-bar? double-bar single-bar)])))
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
    (super-new [char (if double-bar? #\# #\+)]
               [name "pillar"])))
(register-cell-type! pillar% #\#)
(register-cell-type! pillar% #\+)


(define door%
  (class cell%
    (init-field [open? #f])
    (inherit-field occupant char)
    (define/override (free? #:occupant-ok? [ok? #f])
      (and open? (or ok? (not occupant))))
    (define/override (opaque?)
      (not open?))
    (define (-show show-occupant?)
      (define chars (get-field char this))
      (if open?
          (free-cell-show this (first char) show-occupant?)
          (second char)))
    (define/override (show)     (-show #t))
    (define/override (show/fog) (-show #f))
    (define/override (open)
      (if open?
          (enqueue-message!
           (format "~a is already open."
                   (send this describe #:capitalize? #t #:specific? #t)))
          (set! open? #t)))
    (define/override (close)
      (if open?
          (set! open? #f)
          (enqueue-message!
           (format "~a is already closed."
                   (send this describe #:capitalize? #t #:specific? #t)))))
    (super-new)))
(define vertical-door%
  (class door% (super-new [char '(#\_ #\|)] [name "door"])))
(register-cell-type! vertical-door% #\|)
(register-cell-type! (class vertical-door% (super-new [open? #t])) #\_)
(define horizontal-door%
  (class door% (super-new [char '(#\' #\-)] [name "door"])))
(register-cell-type! horizontal-door% #\-)
(register-cell-type! (class horizontal-door% (super-new [open? #t])) #\')

(define chest%
  (class door% ; behaves almost the same
    (inherit-field open?)
    (define/override (opaque?) #f) ; unlike doors
    (super-new [char '(#\= #\≘)] [name "chest"])))
(register-cell-type! chest% #\≘)
(register-cell-type! chest% #\=)

(define entrance%
  (class free-cell%
    (super-new [char    #\⋂] ; doorway
               [name    "entrance"]
               [article "an"])))
(register-cell-type! entrance% #\⋂)
;; other candidates: ≣∬⪋⬆∆ (i.e., stairs, arrows)
