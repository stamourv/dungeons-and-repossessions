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

(define-syntax-rule (define-simple-monster
                      def-name char n
                      #:theme t #:max-hp hp #:xp-value xp #:ai ai)
  (begin
    (define def-name
      (class monster%
        (define/override (show)
          char)
        (define/override (act state)
          (ai state))
        (super-new [name n] [theme t] [max-hp hp] [xp-value xp])))
    (add-monster! def-name t)
    (add-theme! t)
    (hash-set! monsters->xp def-name xp)))
;; TODO add ac, damage die, stats, speed, etc.

(define-simple-monster bat% #\b "bat"
  #:theme 'vermin #:max-hp 1 #:xp-value 10 #:ai #f) ; TODO 1 -> (max 1d4-1 1)
(define-simple-monster rat% #\r "rat"
  #:theme 'vermin #:max-hp 1 #:xp-value 10 #:ai #f) ; TODO 1 -> (max 1d4-1 1)
(define-simple-monster spider% #\s "spider"
  #:theme 'vermin #:max-hp 1 #:xp-value 10 #:ai #f) ; TODO 1 -> 1d-1
;; TODO should ignore difficult terrain

(define-simple-monster giant-rat% #\R "giant rat"
  #:theme 'vermin #:max-hp 7 #:xp-value 25 #:ai #f) ; TODO 7 -> 2d6
;; TODO pack tactics, once I implement advantage
(define-simple-monster kobold% #\k "kobold"
  #:theme 'vermin #:max-hp 5 #:xp-value 25 #:ai #f) ; TODO 5 -> 2d6 - 2
;; TODO + slinger variant
;; TODO pack tactics, once I implement advantage

(define-simple-monster goblin% #\g "goblin"
  #:theme 'vermin #:max-hp 7 #:xp-value 50 #:ai #f) ; TODO 7 -> 2d6
;; TODO + archer variant
(define-simple-monster wolf% #\w "wolf"
  #:theme 'vermin #:max-hp 11 #:xp-value 50 #:ai #f) ; TODO 11 -> 2d8+2
;; TODO pack tactics, once I implement advantage


(define-simple-monster commoner% #\c "commoner"
  #:theme 'cult #:max-hp 4 #:xp-value 10 #:ai #f) ; TODO 4 -> 1d8
;; TODO AI that cowers, and only attacks when you're next to them (like bats!)

(define-simple-monster guard% #\u "guard"
  #:theme 'cult #:max-hp 11 #:xp-value 25 #:ai #f) ; TODO 11 -> 2d8+2
(define-simple-monster cultist% #\l "cultist"
  #:theme 'cult #:max-hp 9 #:xp-value 25 #:ai #f) ; TODO 9 -> 2d8

(define-simple-monster acolyte% #\a "acolyte"
  #:theme 'cult #:max-hp 9 #:xp-value 50 #:ai #f) ; TODO 9 -> 2d8
;; TODO has healing spells, plus misc other spells (o/w not worth 50 xp)
(define-simple-monster skeleton% #\t "skeleton"
  #:theme 'cult #:max-hp 13 #:xp-value 50 #:ai #f) ; TODO 13 -> 2d8+4
;; TODO + archer variant
(define-simple-monster zombie% #\z "zombie"
  #:theme 'cult #:max-hp 22 #:xp-value 50 #:ai #f) ; TODO 22 -> 3d8+9


;; TODO bandits (25 xp) could be a good monster. what theme, though?
;;   doesn't fit with either vermin or cult


;; TODO already a lot of collisions for display characters
;;   maybe it's ok to have collisions across themes?
;;   start using color?

;; TODO other monsters. see page 61 of DM Basic Rules for monster per CR
