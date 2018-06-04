#lang racket

(require "utils.rkt")

(provide sprite%)

(define sprite%
  (class object%
    (init-field name
                char
                [article #f]) ; which article to use? ("a" vs "an" vs etc.)
    (define/public (show)
      char)
    (define/public (describe #:capitalize? [capitalize? #f]
                             #:specific?   [specific?   #f])
      (define no-article (eq? article 'none))
      (define a (cond [no-article ""]
                      [specific?  "the"] ; specific trumps all
                      [article    article]
                      [else       "a"]))
      (string-append (if capitalize? (string-titlecase a) a)
                     (if no-article "" " ")
                     name))
    (super-new)))
