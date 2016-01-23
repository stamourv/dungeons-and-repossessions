#lang racket

(require "utils.rkt")

(provide (all-defined-out))

;; currently, items and characters are sprite%s
;; eventually, will want to include cell%s too, probably
;; should probably also include the (a? all?) display character, if only
;; for the (eventual) help system
(define sprite%
  (class object%
    (init-field name)
    (define/public (show)
      (error "can't show this sprite%"))
    (define/public (describe #:capitalize? [capitalize? #f]
                             #:specific?   [specific?   #f])
      (string-append (article capitalize? specific?) " " name))
    (super-new)))
