#lang racket

(require "sprite.rkt"
         "utils.rkt")

(provide (all-defined-out))

(define item%
  (class sprite%
    (super-new)))

(define macguffin%
  (class item%
    (super-new [char #\$])))
(define decoy%
  (class item%
    (super-new [char #\$])))

(define equippable%
  (class item%
    (super-new)))

(define armor%
  (class equippable%
    (super-new)))
(define shield%
  (class armor%
    [init-field [ac 2]] ; standard shield
    (super-new [name "shield"] [char #\0])))
(define body-armor%
  (class armor%
    (init-field [ac 0] [max-dex 10])
    (super-new [char #\&])))
(define scale-armor%
  (class body-armor%
    (super-new [name "scale armor"] [ac 4] [max-dex 2])))

(define weapon%
  (class equippable%
    (init-field [damage-die d1])
    (super-new [char #\!])))
(define hand-axe%
  (class weapon%
    (super-new [name "handaxe"] [damage-die d6])))
