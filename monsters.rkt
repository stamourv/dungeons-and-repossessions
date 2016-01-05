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

(define-simple-monster rat% #\r "rat" 1 10) ; TODO 1 -> (max 1d4-1 1)
(make-encounter 1 'easy rat%)
(make-encounter 1 'hard rat% rat%)
(make-encounter 1 'deadly rat% rat% rat%)

(define-simple-monster spider% #\s "spider" 1 10) ; TODO 1 -> 1d-1
;; TODO should ignore difficult terrain

(define-simple-monster giant-rat% #\R "giant rat" 7 25) ; TODO 7 -> 2d6
(make-encounter 1 'medium giant-rat%)
(make-encounter 1 'deadly giant-rat% rat%)

(define-simple-monster kobold% #\k "kobold" 5 25) ; TODO 5 -> 2d6 - 2
(make-encounter 1 'medium kobold%)
(make-encounter 1 'deadly kobold% spider%)


;; TODO other monsters. see page 61 of DM Basic Rules for monster per CR
