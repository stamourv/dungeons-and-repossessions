#lang racket

(require "character.rkt"
         "encounters.rkt")

(provide (all-defined-out))

(define monster%
  (class npc%
    [init-field xp-value]
    (super-new)))

(define bat%
  (class monster%
    (define/override (show)
      #\b)
    ;; TODO act method
    (super-new [name     "bat"]
               [max-hp   1] ;; TODO (max 1d4-1 1)
               [xp-value 10])))
(make-encounter 1 'easy (new bat%))

(define rat%
  (class monster%
    (define/override (show)
      #\r)
    ;; TODO act method
    (super-new [name     "rat"]
               [max-hp   1] ;; TODO (max 1d4-1 1)
               [xp-value 10])))
(make-encounter 1 'easy (new rat%))


;; TODO other monsters. see page 61 of DM Basic Rules for monster per CR
