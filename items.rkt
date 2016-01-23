#lang racket

(require "sprite.rkt")

(provide (all-defined-out))

(define item%
  (class sprite%
    (init-field char)
    (define/override (show) char)
    (super-new)))

(define macguffin%
  (class item%
    (super-new [char #\$]
               [name "MacGuffin"]))); TODO have it change for each dungeon
