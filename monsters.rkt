#lang racket

(require "character.rkt")

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

(define rat%
  (class monster%
    (define/override (show)
      #\r)
    ;; TODO act method
    (super-new [name     "rat"]
               [max-hp   1] ;; TODO (max 1d4-1 1)
               [xp-value 10])))


;; TODO other monsters. see page 61 of DM Basic Rules for monster per CR
