#lang racket

(require "character.rkt" "ai.rkt" "utils.rkt")

(provide (all-defined-out))

(define monster%
  (class character%
    (field [encounter '()])
    (super-new)))

(define all-themes '())
(define (add-theme! t)
  (unless (member t all-themes)
    (set! all-themes (cons t all-themes))))
(define theme+cr-table (make-hash))
(define (theme+cr->monsters theme cr)
  (hash-ref theme+cr-table (cons theme cr) '()))


(define-syntax-rule (define-simple-monster
                      def-name ch n #:themes ts
                      #:max-hp hp #:speed sp
                      #:attack-bonus ab #:ac ac #:damage-die dmg
                      #:cr cr #:ai ai)
  (begin
    (define def-name
      (class monster%
        (define my-ai (new ai [monster this]))
        (define/override (act state)        (send my-ai act state))
        (define/public   (wake-up)          (send my-ai wake-up))
        (define/override (get-attack-bonus) ab)
        (define/override (get-ac)           ac)
        (define/override (get-damage-die)   dmg)
        (super-new [name n] [char ch] [max-hp (hp)] [speed sp])))
    (for ([t (in-list ts)])
      (add-theme! t)
      (hash-update! theme+cr-table (cons t cr)
                    (lambda (l) (cons def-name l)) '()))))

(define-simple-monster rat% #\r "rat" #:themes '(vermin tomb)
  #:max-hp (dice d4 -1) #:speed 4
  #:attack-bonus 0 #:ac 10 #:damage-die (dice 1)
  #:cr 0 #:ai wander-ai%)

(define-simple-monster giant-rat% #\R "giant rat" #:themes '(vermin)
  #:max-hp (dice 2 d6) #:speed 6
  #:attack-bonus 4 #:ac 12 #:damage-die (dice d4 2)
  #:cr 1/8 #:ai rush-ai%)
;; TODO pack tactics, once I implement advantage
(define-simple-monster kobold% #\k "kobold" #:themes '(vermin)
  #:max-hp (dice 2 d6 -2) #:speed 6
  #:attack-bonus 4 #:ac 12 #:damage-die (dice d4 2)
  #:cr 1/8 #:ai injury-shy-ai%)
;; TODO + slinger variant
;; TODO pack tactics, once I implement advantage

(define-simple-monster goblin% #\g "goblin" #:themes '(vermin)
  #:max-hp (dice 2 d6) #:speed 6
  #:attack-bonus 4 #:ac 15 #:damage-die (dice d6 2)
  #:cr 1/4 #:ai injury-shy-ai%)
;; TODO + archer variant
(define-simple-monster wolf% #\w "wolf" #:themes '(vermin)
  #:max-hp (dice 2 d8 2) #:speed 8
  #:attack-bonus 4 #:ac 13 #:damage-die (dice 2 d4 2)
  #:cr 1/4 #:ai rush-ai%)
;; TODO pack tactics, once I implement advantage. + knock prone from attack

(define-simple-monster orc% #\o "orc" #:themes '(vermin)
  #:max-hp (dice 2 d8 6) #:speed 6
  #:attack-bonus 5 #:ac 13 #:damage-die (dice d12 3)
  #:cr 1/2 #:ai rush-ai%)
;; TODO javelin variant. aggressive trait
;; (define-simple-monster gnoll% #\n "gnoll" #:themes '(vermin)
;;   #:max-hp (dice 5 d8) #:speed 6
;;   #:attack-bonus 4 #:ac 15 #:damage-die (dice d6 2)
;;   #:cr 1/2 #:ai injury-shy-ai%)
;; TODO + bite attack, and archer variant
(define-simple-monster hobgoblin% #\h "hobgoblin" #:themes '(vermin)
  #:max-hp (dice 2 d8 2) #:speed 6
  #:attack-bonus 3 #:ac 18 #:damage-die (dice d8 1)
  #:cr 1/2 #:ai rush-ai%)
;; TODO martial advantage. archer variant
;; (define-simple-monster worg% #\W "worg" #:themes '(vermin)
;;   #:max-hp (dice 4 d10 4) #:speed 10
;;   #:attack-bonus 5 #:ac 13 #:damage-die (dice 2 d6 3)
;;   #:cr 1/2 #:ai rush-ai%) ; TODO want #\W for dire wolf instead

(define-simple-monster bugbear% #\b "bugbear" #:themes '(vermin)
  #:max-hp (dice 5 d8 5) #:speed 6
  #:attack-bonus 4 #:ac 16 #:damage-die (dice 2 d8 2)
  #:cr 1 #:ai rush-ai%)
;; TODO surprise attack. javelin variant
(define-simple-monster dire-wolf% #\W "dire wolf" #:themes '(vermin)
  #:max-hp (dice 5 d10 10) #:speed 10
  #:attack-bonus 5 #:ac 14 #:damage-die (dice 2 d6 3)
  #:cr 1 #:ai rush-ai%)
;; TODO pack tactics, once I implement advantage. + knock prone from attack


;; TODO removing cult for now. not very interesting, esp. with acolyte
;;  that has no spells

;; (define-simple-monster cultist% #\c "cultist" #:themes '(cult)
;;   #:max-hp (dice 2 d8) #:speed 6
;;   #:attack-bonus 4 #:ac 12 #:damage-die (dice d6 1)
;;   #:cr 1/8 #:ai rush-ai%)

;; (define-simple-monster acolyte% #\a "acolyte" #:themes '(cult)
;;   #:max-hp (dice 2 d8) #:speed 6
;;   #:attack-bonus 2 #:ac 10 #:damage-die d4
;;   #:cr 1/4 #:ai injury-shy-ai%)
;; ;; TODO has healing spells, plus misc other spells (o/w not worth 50 xp)


(define-simple-monster bat% #\b "bat" #:themes '(tomb)
  #:max-hp (dice d4 -1) #:speed 6
  #:attack-bonus 0 #:ac 12 #:damage-die (dice 1)
  #:cr 0 #:ai wander-ai%)

(define-simple-monster skeleton% #\s "skeleton" #:themes '(tomb)
  #:max-hp (dice 2 d8 4) #:speed 6
  #:attack-bonus 4 #:ac 13 #:damage-die (dice d6 2)
  #:cr 1/4 #:ai rush-ai%)
;; TODO + archer variant
(define-simple-monster zombie% #\z "zombie" #:themes '(tomb)
  #:max-hp (dice 3 d8 9) #:speed 4
  #:attack-bonus 3 #:ac 8 #:damage-die (dice d6 1)
  #:cr 1/4 #:ai wander-ai%)


(define-simple-monster commoner% #\c "commoner" #:themes '(castle)
  #:max-hp d8 #:speed 6
  #:attack-bonus 2 #:ac 10 #:damage-die d4
  #:cr 0 #:ai injury-shy-ai%)

(define-simple-monster bandit% #\b "bandit" #:themes '(castle)
  #:max-hp (dice 2 d8 2) #:speed 6
  #:attack-bonus 4 #:ac 12 #:damage-die (dice d6 1)
  #:cr 1/8 #:ai injury-shy-ai%)
;; TODO crossbow variant

(define-simple-monster guard% #\g "guard" #:themes '(#|cult|# castle)
  #:max-hp (dice 2 d8 2) #:speed 6
  #:attack-bonus 3 #:ac 16 #:damage-die (dice d6 1)
  #:cr 1/4 #:ai rush-ai%) ; rules say CR 1/8, but stronger than a goblin...

(define-simple-monster thug% #\t "thug" #:themes '(castle)
  #:max-hp (dice 5 d8 10) #:speed 6
  #:attack-bonus 4 #:ac 11 #:damage-die (dice d6 2)
  #:cr 1/2 #:ai rush-ai%)
;; TODO pack tactics. multiattack. crossbow variant
;; TODO warhorse for CR 1/2? or is that just silly?


(define-simple-monster spider% #\s "spider" #:themes '(jungle)
  #:max-hp (dice d4 -1) #:speed 4
  #:attack-bonus 4 #:ac 12 #:damage-die (dice 1)
  #:cr 0 #:ai wander-ai%)
;; TODO should ignore difficult terrain. and add poison to attack


;; TODO other monsters. see page 61 of DM Basic Rules for monster per CR
