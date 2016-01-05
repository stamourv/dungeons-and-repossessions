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

(define-simple-monster bat% #\b "bat" 1 10) ; TODO 1 -> (max 1d4-1 1)
(make-encounter 1 'easy (new bat%))

(define-simple-monster rat% #\r "rat" 1 10) ; TODO 1 -> (max 1d4-1 1)
(make-encounter 1 'easy (new rat%))


;; TODO other monsters. see page 61 of DM Basic Rules for monster per CR
