#lang racket

(require "utils.rkt")

(provide sprite%)

(define (add-article capitalize? specific?
                     #:article [article #f])
  ((if capitalize? string-titlecase values)
   (if specific? ; specific trumps all
       "the"
       (or article "a"))))

(define sprite%
  (class object%
    (init-field name
                char
                [article #f]) ; which article to use? ("a" vs "an" vs etc.)
    (define/public (show)
      char)
    (define/public (describe #:capitalize? [capitalize? #f]
                             #:specific?   [specific?   #f])
      (string-append (add-article capitalize? specific? #:article article)
                     " " name))
    (super-new)))
