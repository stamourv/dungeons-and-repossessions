#lang racket

(require "utils.rkt")

(provide (all-defined-out))

(define sprite%
  (class object%
    (init-field name char)
    (define/public (show)
      char)
    (define/public (describe #:capitalize? [capitalize? #f]
                             #:specific?   [specific?   #f])
      (string-append (article capitalize? specific?) " " name))
    (super-new)))
