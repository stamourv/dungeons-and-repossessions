#lang racket

(provide (all-defined-out))

(struct -dice (roll description)
        #:property prop:procedure
        (struct-field-index roll)
        #:methods gen:custom-write
        [(define (write-proc d port mode)
           (write-string (-dice-description d) port))])

(define-syntax-rule (fixed-dice [name max] ...)
  (begin (define name
           (-dice (lambda _ (random 1 (add1 max))) (symbol->string 'name)))
         ...))
(fixed-dice [d1 1] [d4 4] [d6 6] [d8 8] [d10 10] [d12 12] [d20 20])

(define (dice . args)
  (define (roll args)
    (match args
      ['()
       0]
      [`(,(? integer? n))
       n]
      [`(,(? integer? n) ,d . ,rest)
       (+ (for/sum ([i (in-range n)]) (d))
          (roll rest))]
      [`(,d . ,rest)
       (+ (d) (roll rest))]))
  (define (describe args)
    (define (format-prefix prefix) ; mix of dice and multiplied dice
      (define (format-dice dice)
        (match dice
          ['()
           '()]
          [`(,(? integer? n) ,d . ,rest)
           (cons (format "~a~a" n d) (format-dice rest))]
          [`(,d . ,rest)
           (define s  (format "~a" d))
           ;; if it starts with a number, leave it alone, otherwise prefix "1"
           ;; (ugly, but can't think of a much better way with "nested" dice...)
           (define s* (if (regexp-match "^[1-9]" s) s (format "1~a" s)))
           (cons s* (format-dice rest))]))
      (string-join (format-dice prefix) "+"))
    (define end (last args))
    (cond [(integer? end)
           (define prefix (format-prefix (drop-right args 1)))
           (if (positive? end)
               (format "~a+~a" prefix end)
               (format "~a~a"  prefix end))] ; the `-` is already in the number
          [else
           (format-prefix args)]))
  (-dice (lambda _ (max 1 (roll args)))
         (describe args)))

(define (random-bool [probability 0.5])
  (< (random) probability))

;; Takes a string, and breaks it into lines.
(define (break-lines s [columns (pretty-print-columns)])
  (define res (open-output-string))
  (for/fold ([len 0])
      ([word (in-list (regexp-split #px"[[:blank:]]+" s))])
    (let ([new-len (+ len (string-length word) 1)])
      (cond [(< new-len columns)
             (display (format "~a~a" (if (= len 0) "" " ") word) res)
             new-len]
            [else ; break the line
             (display (format "\n~a" word) res)
             (string-length word)])))
  (get-output-string res))
