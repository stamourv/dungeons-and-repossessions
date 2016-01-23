#lang racket

(require "character.rkt" "ai.rkt" "utils.rkt")

(provide (all-defined-out))

(define monster%
  (class character%
    (init-field theme xp-value)
    (field [encounter '()])
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
                      def-name char n #:theme t
                      #:max-hp hp #:speed sp
                      #:attack-bonus ab #:ac ac #:damage-die dmg
                      #:xp-value xp #:ai ai)
  (begin
    (define def-name
      (class monster%
        (define/override (show)             char)
        (define my-ai (new ai [monster this]))
        (define/override (act state)        (send my-ai act state))
        (define/public   (wake-up)          (send my-ai wake-up))
        (define/override (get-attack-bonus) ab)
        (define/override (get-ac)           ac)
        (define/override (get-damage-die)   dmg)
        (super-new [name n] [theme t] [max-hp (hp)] [speed sp] [xp-value xp])))
    (add-monster! def-name t)
    (add-theme! t)
    (hash-set! monsters->xp def-name xp)))

(define-simple-monster bat% #\b "bat"
  #:theme 'vermin #:max-hp (lambda _ (max (- (d4) 1) 1)) #:speed 6
  #:attack-bonus 0 #:ac 12 #:damage-die (lambda _ 1)
  #:xp-value 10 #:ai cower-ai%)
(define-simple-monster rat% #\r "rat"
  #:theme 'vermin #:max-hp (lambda _ (max (- (d4) 1) 1)) #:speed 4
  #:attack-bonus 0 #:ac 10 #:damage-die (lambda _ 1)
  #:xp-value 10 #:ai wander-ai%)
(define-simple-monster spider% #\s "spider"
  #:theme 'vermin #:max-hp (lambda _ (max (- (d4) 1) 1)) #:speed 4
  #:attack-bonus 4 #:ac 12 #:damage-die (lambda _ 1)
  #:xp-value 10 #:ai wander-ai%)
;; TODO should ignore difficult terrain. and add poison to attack

(define-simple-monster giant-rat% #\R "giant rat"
  #:theme 'vermin #:max-hp (lambda _ (+ (d6) (d6))) #:speed 6
  #:attack-bonus 4 #:ac 12 #:damage-die (lambda _ (+ (d4) 2))
  #:xp-value 25 #:ai rush-ai%)
;; TODO pack tactics, once I implement advantage
(define-simple-monster kobold% #\k "kobold"
  #:theme 'vermin #:max-hp (lambda _ (max (+ (d6) (d6) -2) 1)) #:speed 6
  #:attack-bonus 4 #:ac 12 #:damage-die (lambda _ (+ (d4) 2))
  #:xp-value 25 #:ai injury-shy-ai%)
;; TODO + slinger variant
;; TODO pack tactics, once I implement advantage

(define-simple-monster goblin% #\g "goblin"
  #:theme 'vermin #:max-hp (lambda _ (+ (d6) (d6))) #:speed 6
  #:attack-bonus 4 #:ac 15 #:damage-die (lambda _ (+ (d6) 2))
  #:xp-value 50 #:ai injury-shy-ai%)
;; TODO + archer variant
(define-simple-monster wolf% #\w "wolf"
  #:theme 'vermin #:max-hp (lambda _ (+ (d8) (d8) 2)) #:speed 8
  #:attack-bonus 4 #:ac 13 #:damage-die (lambda _ (+ (d4) (d4) 2))
  #:xp-value 50 #:ai rush-ai%)
;; TODO pack tactics, once I implement advantage. + knock prone from attack


(define-simple-monster commoner% #\c "commoner"
  #:theme 'cult #:max-hp d8 #:speed 6
  #:attack-bonus 2 #:ac 10 #:damage-die d4
  #:xp-value 10 #:ai cower-ai%)

(define-simple-monster guard% #\u "guard"
  #:theme 'cult #:max-hp (lambda _ (+ (d8) (d8) 2)) #:speed 6
  #:attack-bonus 3 #:ac 16 #:damage-die (lambda _ (+ (d6) 1))
  #:xp-value 25 #:ai rush-ai%)
(define-simple-monster cultist% #\l "cultist"
  #:theme 'cult #:max-hp (lambda _ (+ (d8) (d8))) #:speed 6
  #:attack-bonus 4 #:ac 12 #:damage-die (lambda _ (+ (d6) 1))
  #:xp-value 25 #:ai rush-ai%)

(define-simple-monster acolyte% #\a "acolyte"
  #:theme 'cult #:max-hp (lambda _ (+ (d8) (d8))) #:speed 6
  #:attack-bonus 2 #:ac 10 #:damage-die d4
  #:xp-value 50 #:ai injury-shy-ai%)
;; TODO has healing spells, plus misc other spells (o/w not worth 50 xp)
(define-simple-monster skeleton% #\t "skeleton"
  #:theme 'cult #:max-hp (lambda _ (+ (d8) (d8) 4)) #:speed 6
  #:attack-bonus 4 #:ac 13 #:damage-die (lambda _ (+ (d6) 2))
  #:xp-value 50 #:ai rush-ai%)
;; TODO + archer variant
(define-simple-monster zombie% #\z "zombie"
  #:theme 'cult #:max-hp (lambda _ (+ (d8) (d8) (d8) 9)) #:speed 4
  #:attack-bonus 3 #:ac 8 #:damage-die (lambda _ (+ (d6) 1))
  #:xp-value 50 #:ai wander-ai%)


;; TODO bandits (25 xp) could be a good monster. what theme, though?
;;   doesn't fit with either vermin or cult


;; TODO already a lot of collisions for display characters
;;   maybe it's ok to have collisions across themes?
;;   start using color?

;; TODO other monsters. see page 61 of DM Basic Rules for monster per CR
