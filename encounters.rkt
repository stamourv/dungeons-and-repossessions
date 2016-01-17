#lang racket

(require racket/random math/distributions
         "encounter-tables.rkt" "monsters.rkt" "utils.rkt")

(provide generate-encounters)

;; An Encounter is a (Listof Monster)

(define (encounter-cost monsters)
  (define total-xp
    (for/sum ([m (in-list monsters)]) (monster->xp m)))
  (define adjusted-xp
    (* total-xp
       (encounter-multiplier (length monsters))
       ;; additional fudge factor
       ;; we have a single character, so things are even harder
       ;; and makes it possible to fit certain budgets that would
       ;; be impossible to fit (i.e., level 1 easy)
       ;; computed to be what makes a single CR 0 monster an easy
       ;; level 1 encounter
       ;; subject to tweaking (or even removal, and going back to
       ;; allowing skipping the budget check, as we had before)
       (exact->inexact 5/3))) ; float for printing
  adjusted-xp)

;; maps (level difficulty theme) triples to sets of encounters
(define all-encounters (make-hash))

(define all-difficulties '(easy medium hard deadly))

(define (close-enough? cost budget) ; within 25%
  (<= (* 0.75 budget) cost (* 1.25 budget)))

;; pulled out of thin air, subject to tweaking
(define encounter-difficulty-probabilities
  '((easy   . 0.3)
    (medium . 0.4)
    (hard   . 0.2)
    (deadly . 0.1)))

;; subject to tweaking, based on how crowded we want dungeons to feel
;; Note: also useful to avoid a "long tail" of easy encounters to fill
;;   a budget, when nothing else will fit
(define max-n-encounters 6)

;; given the player's level, allocate a day's xp budget between a number
;; of encounters of varying difficulty
;; returns a list of difficulties
(define (generate-encounter-template character-level)
  (define budget (day-experience-budget character-level))
  (define costs
    (for/list ([d (in-list all-difficulties)])
      (cons d (encounter-experience-budget character-level d))))
  (define (try)
    (let loop ([encs-so-far '()] [remaining-budget budget])
      (cond
       ;; Note: budget is currently checked based on the expected encounter
       ;;   cost (determined by level and difficulty), not on the actual cost
       ;;   of the concrete encounters
       ;;   in the end (I think) the total cost per day remains bounded by
       ;;   the `close-enough?` ratio anyway
       [(close-enough? budget
                       (for/sum ([e (in-list encs-so-far)])
                         (dict-ref costs e)))
        encs-so-far] ; we're done
       [(>= (length encs-so-far) max-n-encounters)
        ;; too many encounters, and not at budget yet. try again
        (try)]
       [else
        (define possible-difficulties
          (filter (lambda (diff+prob) (<= (dict-ref costs (car diff+prob))
                                          remaining-budget))
                  encounter-difficulty-probabilities))
        (when (empty? possible-difficulties)
          ;; we're not close enough, and nothing can fit
          ;; that probably shouldn't happen. internal error
          (raise-arguments-error 'generate-encounter-template "nothing can fit"
                                 "encounters" encs-so-far
                                 "remaining budget" remaining-budget
                                 "level" character-level))
        (define dist
          (discrete-dist (map car possible-difficulties)
                         (map cdr possible-difficulties)))
        (define new (sample dist))
        ;; TODO try preventing 2+ deadlies in the same dungeon?
        (loop (cons new encs-so-far)
              (- budget (dict-ref costs new)))])))
  (try))

;; (module+ main ; to test it out
;;   (for ([i 10])
;;     (displayln (generate-encounter-template 1))))

;; pick concrete encounters given a template and the character level
(define (fill-encounter-template template level)
  (define possible-themes ; stick to one theme for the dungeon
    (for/list ([t (in-list all-themes)]
               ;; can we populate the template with this theme?
               #:when (for/and ([d (in-list (remove-duplicates template))])
                        (not (empty?
                              (dict-ref all-encounters
                                        (list level d t)
                                        (lambda ()
                                          (enumerate-encounters level d t)))))))
      t))
  (define theme (random-ref possible-themes))
  (for/list ([diff (in-list template)])
    (random-ref (dict-ref all-encounters (list level diff theme)))))

;; generates a list of encounters for a given player level
(define (generate-encounters level)
  (fill-encounter-template (generate-encounter-template level) level))

;; (module+ main ; to test it out
;;   (define level 2)
;;   (for ([i 10])
;;     (define t (generate-encounter-template level))
;;     (for ([d t]
;;           [e (fill-encounter-template t level)])
;;       (printf "~a ~a\n" (~a d #:min-width 6) e))
;;     (newline)))


;; Limit number of monsters per encounter.
;; More for efficiency than for balance. Encounter multiplier takes care of
;; balance already, but since this number is the exponent for encounter
;; enumeration, want to keep it low. n^6 is already pretty bad. n is the
;; number of monsters that need to be considered at once (e.g. same theme).
;; FWIW, with the monsters available as of this writing, a max of 6 has
;; enumeration of all levels, difficulties and themes take ~30s. A max of 8
;; for to like level 6 in 10 minutes. Not reasonable.
(define max-monsters-per-encounter 6)
(define (enumerate-encounters level difficulty theme)
  (define budget (encounter-experience-budget level difficulty))
  (define ms (dict-ref monsters-by-theme theme))
  ;; enumerate up to max encounter size
  (define es
    (set->list
     (for*/set ([n (in-range 1 (add1 max-monsters-per-encounter))]
                ;; enumerate all encounters of n monsters (within budget)
                [e (in-list (apply cartesian-product (make-list n ms)))]
                #:when (close-enough? (encounter-cost e) budget))
       ;; order doesn't matter, encounters are bags of monsters
       ;; canonicalize representation to avoid repeats
       (sort e < #:key eq-hash-code))))
  ;; cache the result
  (hash-set! all-encounters (list level difficulty theme) es)
  es)


(module+ main
  ;; generate all encounters
  (for* ([level (in-range 1 6 #|21|#)]
         [diff  (in-list all-difficulties)]
         [theme (in-list all-themes)])
    (printf "generating: ~a ~a ~a\n" level diff theme)
    (hash-set! all-encounters
               (list level diff theme)
               (enumerate-encounters level diff theme)))
  ;; show how many of each kind of encounter we have, split by theme
  (for ([t all-themes])
    (displayln t)
    (display (~a "" #:min-width 5))
    (for ([d all-difficulties])
      (display (~a d #:min-width 10)))
    (newline)
    (for ([l (in-range 1 6 #|21|#)])
      (define counts
        (for/list ([d all-difficulties])
          (length (dict-ref all-encounters (list l d t) '()))))
      (unless (andmap zero? counts)
        (display (~a l #:min-width 5))
        (for ([c counts])
          (display (~a c #:min-width 10)))
        (newline)))
    (newline))
  )
