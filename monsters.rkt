#lang racket

(require "character.rkt"
         "encounters.rkt")

(provide (all-defined-out))

(define monster%
  (class npc%
    [init-field xp-value]
    (super-new)))

(define-syntax-rule (define-simple-monster def-name char n hp xp)
  (define def-name
    (class monster%
      (define/override (show)
        char)
      ;; TODO act method
      (super-new [name n] [max-hp hp] [xp-value xp]))))
;; TODO add ac, damage die, stats, speed, etc.

(define-simple-monster bat% #\b "bat" 1 10) ; TODO 1 -> (max 1d4-1 1)
(make-encounter 1 'easy bat%)
(make-encounter 1 'hard bat% bat%)
(make-encounter 2 'medium bat% bat% bat%)

(define-simple-monster rat% #\r "rat" 1 10) ; TODO 1 -> (max 1d4-1 1)
(make-encounter 1 'easy rat%)
(make-encounter 1 'hard rat% rat%)
(make-encounter 1 'deadly rat% rat% rat%)
(make-encounter 2 'medium rat% rat% rat%)

(define-simple-monster spider% #\s "spider" 1 10) ; TODO 1 -> 1d-1
;; TODO should ignore difficult terrain

(define-simple-monster giant-rat% #\R "giant rat" 7 25) ; TODO 7 -> 2d6
;; TODO pack tactics, once I implement advantage
(make-encounter 1 'medium giant-rat%)
(make-encounter 2 'easy giant-rat%)
(make-encounter 1 'deadly giant-rat% rat%)
(make-encounter 2 'medium giant-rat% rat%)
;; (make-encounter 2 'hard giant-rat% giant-rat%) ; too many 2 hard
(make-encounter 2 'hard giant-rat% rat% rat%)
(make-encounter 2 'deadly giant-rat% rat% rat% rat%)

(define-simple-monster kobold% #\k "kobold" 5 25) ; TODO 5 -> 2d6 - 2
;; TODO pack tactics, once I implement advantage
(make-encounter 1 'medium kobold%)
(make-encounter 2 'easy kobold%)
(make-encounter 1 'deadly kobold% spider%)
(make-encounter 2 'medium kobold% spider%)
(make-encounter 2 'hard kobold% kobold%)
(make-encounter 2 'deadly kobold% spider% spider%)
(make-encounter 2 'deadly kobold% kobold% spider%)

(define-simple-monster goblin% #\g "goblin" 7 50) ; TODO 7 -> 2d6
(make-encounter 2 'hard goblin%)
(make-encounter 2 'deadly goblin% kobold%)

(define-simple-monster wolf% #\w "wolf" 11 50) ; TODO 11 -> 2d8+2
;; TODO pack tactics, once I implement advantage
(make-encounter 2 'hard wolf%)
(make-encounter 2 'deadly wolf% rat%)


;; TODO other monsters. see page 61 of DM Basic Rules for monster per CR

(module+ main
  ;; show how many of each kind of encounter we have
  (display (~a "" #:min-width 5))
  (for ([d all-difficulties])
    (display (~a d #:min-width 10)))
  (newline)
  (for ([l (in-range 1 21)])
    (define counts
      (for/list ([d all-difficulties])
        (length (dict-ref all-encounters (cons l d) '()))))
    (unless (andmap zero? counts)
      (display (~a l #:min-width 5))
      (for ([c counts])
        (display (~a c #:min-width 10)))
      (newline)))
  )
