#lang racket

(require "character.rkt"
         "encounters.rkt")

(provide (all-defined-out))

(define monster%
  (class npc%
    [init-field xp-value]
    (super-new)))

(define all-monsters '())

(define-syntax-rule (define-simple-monster def-name char n hp xp)
  (begin
    (define def-name
      (class monster%
        (define/override (show)
          char)
        ;; TODO act method
        (super-new [name n] [max-hp hp] [xp-value xp])))
    (set! all-monsters (cons def-name all-monsters))))
;; TODO add ac, damage die, stats, speed, etc.

(define-simple-monster bat% #\b "bat" 1 10) ; TODO 1 -> (max 1d4-1 1)
(make-encounter 1 'easy 'vermin bat%)
(make-encounter 1 'hard 'vermin bat% bat%)
(make-encounter 2 'medium 'vermin bat% bat% bat%)

(define-simple-monster rat% #\r "rat" 1 10) ; TODO 1 -> (max 1d4-1 1)
(make-encounter 1 'easy 'vermin rat%)
(make-encounter 1 'hard 'vermin rat% rat%)
(make-encounter 1 'deadly 'vermin rat% rat% rat%)
(make-encounter 2 'medium 'vermin rat% rat% rat%)

(define-simple-monster spider% #\s "spider" 1 10) ; TODO 1 -> 1d-1
;; TODO should ignore difficult terrain

(define-simple-monster giant-rat% #\R "giant rat" 7 25) ; TODO 7 -> 2d6
;; TODO pack tactics, once I implement advantage
(make-encounter 1 'medium 'vermin giant-rat%)
(make-encounter 2 'easy 'vermin giant-rat%)
(make-encounter 1 'deadly 'vermin giant-rat% rat%)
(make-encounter 2 'medium 'vermin giant-rat% rat%)
;; (make-encounter 2 'hard 'vermin giant-rat% giant-rat%) ; too many 2 hard
(make-encounter 2 'hard 'vermin giant-rat% rat% rat%)
(make-encounter 2 'deadly 'vermin giant-rat% rat% rat% rat%)

(define-simple-monster kobold% #\k "kobold" 5 25) ; TODO 5 -> 2d6 - 2
;; TODO + slinger variant
;; TODO pack tactics, once I implement advantage
(make-encounter 1 'medium 'vermin kobold%)
(make-encounter 2 'easy 'vermin kobold%)
(make-encounter 1 'deadly 'vermin kobold% spider%)
(make-encounter 2 'medium 'vermin kobold% spider%)
(make-encounter 2 'hard 'vermin kobold% kobold%)
(make-encounter 2 'deadly 'vermin kobold% spider% spider%)
(make-encounter 2 'deadly 'vermin kobold% kobold% spider%)

(define-simple-monster goblin% #\g "goblin" 7 50) ; TODO 7 -> 2d6
;; TODO + archer variant
(make-encounter 2 'hard 'vermin goblin%)
(make-encounter 2 'deadly 'vermin goblin% kobold%)

(define-simple-monster wolf% #\w "wolf" 11 50) ; TODO 11 -> 2d8+2
;; TODO pack tactics, once I implement advantage
(make-encounter 2 'hard 'vermin wolf%)
(make-encounter 2 'deadly 'vermin wolf% rat%)


(define-simple-monster commoner% #\c "commoner" 4 10) ; TODO 4 -> 1d8
;; TODO AI that cowers, and only attacks when you're next to them (like bats!)

(define-simple-monster guard% #\u "guard" 11 25) ; TODO 11 -> 2d8+2
(make-encounter 2 'easy 'cult guard%)
(make-encounter 2 'medium 'cult guard% commoner%)
(make-encounter 2 'hard 'cult guard% commoner% commoner%)
(make-encounter 2 'hard 'cult guard% guard%)

(define-simple-monster cultist% #\l "cultist" 9 25) ; TODO 9 -> 2d8
(make-encounter 2 'easy 'cult cultist%)
(make-encounter 2 'medium 'cult cultist% commoner%)
(make-encounter 2 'hard 'cult cultist% guard%)
(make-encounter 2 'hard 'cult cultist% commoner% commoner%)
(make-encounter 2 'hard 'cult cultist% cultist%)

(define-simple-monster acolyte% #\a "acolyte" 9 50) ; TODO 9 -> 2d8
;; TODO has healing spells, plus misc other spells (o/w not worth 50 xp)
(make-encounter 2 'hard 'cult acolyte%)
(make-encounter 2 'deadly 'cult acolyte% guard%)
(make-encounter 2 'deadly 'cult acolyte% cultist%)

(define-simple-monster skeleton% #\t "skeleton" 13 50) ; TODO 13 -> 2d8+4
;; TODO + archer variant
(make-encounter 2 'hard 'cult skeleton%)
(make-encounter 2 'deadly 'cult skeleton% cultist%)

(define-simple-monster zombie% #\z "zombie" 22 50) ; TODO 22 -> 3d8+9
(make-encounter 2 'hard 'cult zombie%)
(make-encounter 2 'deadly 'cult zombie% cultist%)


;; TODO bandits (25 xp) could be a good monster. what theme, though?
;;   doesn't fit with either vermin or cult


;; TODO already a lot of collisions for display characters
;;   maybe it's ok to have collisions across themes?
;;   start using color?

;; TODO other monsters. see page 61 of DM Basic Rules for monster per CR

(module+ main
  ;; show how many of each kind of encounter we have, split by theme
  (define all-themes
    (remove-duplicates
     (apply append (for/list ([encs (in-dict-values all-encounters)])
                     (map first encs)))))
  (for ([t all-themes])
    (displayln t)
    (display (~a "" #:min-width 5))
    (for ([d all-difficulties])
      (display (~a d #:min-width 10)))
    (newline)
    (for ([l (in-range 1 21)])
      (define counts
        (for/list ([d all-difficulties])
          (define encs (dict-ref all-encounters (cons l d) '()))
          (length (for/list ([e encs]
                             #:when (equal? (car e) t))
                    e))))
      (unless (andmap zero? counts)
        (display (~a l #:min-width 5))
        (for ([c counts])
          (display (~a c #:min-width 10)))
        (newline)))
    (newline))
  )
