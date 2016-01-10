#lang racket

(require "character.rkt")

(provide (all-defined-out))

(define monster%
  (class npc%
    [init-field theme xp-value]
    (super-new)))

(define monsters-by-theme (make-hash)) ; hash from theme to set of monsters
(define (add-monster! m t)
  (hash-update! monsters-by-theme t (lambda (ms) (cons m ms)) '()))
(define all-themes '())
(define (add-theme! t)
  (unless (member t all-themes)
    (set! all-themes (cons t all-themes))))
(define monsters->xp (make-hash))
(define (monster->xp m) (hash-ref monsters->xp m))

(define-syntax-rule (define-simple-monster def-name char n t hp xp)
  (begin
    (define def-name
      (class monster%
        (define/override (show)
          char)
        ;; TODO act method
        (super-new [name n] [theme t] [max-hp hp] [xp-value xp])))
    (add-monster! def-name t)
    (add-theme! t)
    (hash-set! monsters->xp def-name xp)))
;; TODO add ac, damage die, stats, speed, etc.

(define-simple-monster bat% #\b "bat" 'vermin 1 10) ; TODO 1 -> (max 1d4-1 1)
(define-simple-monster rat% #\r "rat" 'vermin 1 10) ; TODO 1 -> (max 1d4-1 1)
(define-simple-monster spider% #\s "spider" 'vermin 1 10) ; TODO 1 -> 1d-1
;; TODO should ignore difficult terrain

(define-simple-monster giant-rat% #\R "giant rat" 'vermin 7 25) ; TODO 7 -> 2d6
;; TODO pack tactics, once I implement advantage
(define-simple-monster kobold% #\k "kobold" 'vermin 5 25) ; TODO 5 -> 2d6 - 2
;; TODO + slinger variant
;; TODO pack tactics, once I implement advantage

(define-simple-monster goblin% #\g "goblin" 'vermin 7 50) ; TODO 7 -> 2d6
;; TODO + archer variant
(define-simple-monster wolf% #\w "wolf" 'vermin 11 50) ; TODO 11 -> 2d8+2
;; TODO pack tactics, once I implement advantage


(define-simple-monster commoner% #\c "commoner" 'cult 4 10) ; TODO 4 -> 1d8
;; TODO AI that cowers, and only attacks when you're next to them (like bats!)

(define-simple-monster guard% #\u "guard" 'cult 11 25) ; TODO 11 -> 2d8+2
(define-simple-monster cultist% #\l "cultist" 'cult 9 25) ; TODO 9 -> 2d8

(define-simple-monster acolyte% #\a "acolyte" 'cult 9 50) ; TODO 9 -> 2d8
;; TODO has healing spells, plus misc other spells (o/w not worth 50 xp)
(define-simple-monster skeleton% #\t "skeleton" 'cult 13 50) ; TODO 13 -> 2d8+4
;; TODO + archer variant
(define-simple-monster zombie% #\z "zombie" 'cult 22 50) ; TODO 22 -> 3d8+9


;; TODO bandits (25 xp) could be a good monster. what theme, though?
;;   doesn't fit with either vermin or cult


;; TODO already a lot of collisions for display characters
;;   maybe it's ok to have collisions across themes?
;;   start using color?

;; TODO other monsters. see page 61 of DM Basic Rules for monster per CR
