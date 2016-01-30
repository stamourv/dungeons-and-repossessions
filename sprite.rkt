#lang racket

(require "utils.rkt")

(provide (all-defined-out))

(define sprite%
  (class object%
    (init-field name
                char
                [an? #f]) ; which article to use?
    (define/public (show)
      char)
    (define/public (describe #:capitalize? [capitalize? #f]
                             #:specific?   [specific?   #f])
      (string-append (article capitalize? specific? #:an? an?)
                     " " name))
    (super-new)))
