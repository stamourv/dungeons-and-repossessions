#lang racket

(require "sprite.rkt")

(provide (all-defined-out))

(define item%
  (class sprite%
    (super-new)))

(define macguffin%
  (class item%
    (super-new [char #\$])))
