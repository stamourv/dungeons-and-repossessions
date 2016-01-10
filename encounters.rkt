#lang racket

(require math/distributions
         "encounter-tables.rkt" "monsters.rkt")

(provide all-encounters)

;; An Encounter is a (Pair Theme (Listof Monster))
;; where a Theme is a symbol (chosen from a small set) that determines
;; which encounters can go together (i.e., have the same theme)

(define (encounter-cost encounter)
  (define monsters (cdr encounter))
  (define total-xp
    (for/sum ([m (in-list monsters)]) (get-field xp-value (new m))))
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

;; maps (level . difficulty) pairs to sets of encounters
(define all-encounters (make-hash))

(define all-difficulties '(easy medium hard deadly))

(define (close-enough? x y) ; within 25%
  (<= (* 0.75 y) x (* 1.25 y)))

(define (make-encounter level difficulty theme . monsters)
  (unless (integer? level)
    (raise-argument-error 'make-encounter "integer?" level))
  (unless (member difficulty all-difficulties)
    (raise-argument-error 'make-encounter "difficulty?" difficulty))
  (unless (symbol? theme)
    (raise-argument-error 'make-encounter "symbol?" theme))
  (define encounter (cons theme monsters))
  (define adjusted-xp (encounter-cost encounter))
  (define budget (encounter-experience-budget level difficulty))
  (unless (close-enough? adjusted-xp budget)
    (raise-arguments-error 'make-encounter "not within budget"
                           "encounter" monsters
                           "budget"    budget
                           "cost"      adjusted-xp))
  (hash-update! all-encounters
                (cons level difficulty)
                (lambda (xs) (cons encounter xs))
                '()))


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
    ;; Note: we assume that if one theme exists for a level at one
    ;;   difficulty, it does for all of them (o/w, bug in encounter list)
    (remove-duplicates
     (map first (dict-ref all-encounters
                          (cons level (first all-difficulties))))))
  (define theme (sample (discrete-dist possible-themes)))
  (for/list ([diff (in-list template)])
    (define possible-encounters (dict-ref all-encounters (cons level diff)))
    (define in-theme-encounters (for/list ([e (in-list possible-encounters)]
                                           #:when (equal? (car e) theme))
                                  e))
    (sample (discrete-dist in-theme-encounters))))

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

(make-encounter 1 'easy 'vermin bat%)
(make-encounter 1 'hard 'vermin bat% bat%)
(make-encounter 2 'medium 'vermin bat% bat% bat%)

(make-encounter 1 'easy 'vermin rat%)
(make-encounter 1 'hard 'vermin rat% rat%)
(make-encounter 1 'deadly 'vermin rat% rat% rat%)
(make-encounter 2 'medium 'vermin rat% rat% rat%)

(make-encounter 1 'medium 'vermin giant-rat%)
(make-encounter 2 'easy 'vermin giant-rat%)
(make-encounter 1 'deadly 'vermin giant-rat% rat%)
(make-encounter 2 'medium 'vermin giant-rat% rat%)
;; (make-encounter 2 'hard 'vermin giant-rat% giant-rat%) ; too many 2 hard
(make-encounter 2 'hard 'vermin giant-rat% rat% rat%)
(make-encounter 2 'deadly 'vermin giant-rat% rat% rat% rat%)

(make-encounter 1 'medium 'vermin kobold%)
(make-encounter 2 'easy 'vermin kobold%)
(make-encounter 1 'deadly 'vermin kobold% spider%)
(make-encounter 2 'medium 'vermin kobold% spider%)
(make-encounter 2 'hard 'vermin kobold% kobold%)
(make-encounter 2 'deadly 'vermin kobold% spider% spider%)
(make-encounter 2 'deadly 'vermin kobold% kobold% spider%)

(make-encounter 2 'hard 'vermin goblin%)
(make-encounter 2 'deadly 'vermin goblin% kobold%)

(make-encounter 2 'hard 'vermin wolf%)
(make-encounter 2 'deadly 'vermin wolf% rat%)


(make-encounter 2 'easy 'cult guard%)
(make-encounter 2 'medium 'cult guard% commoner%)
(make-encounter 2 'hard 'cult guard% commoner% commoner%)
(make-encounter 2 'hard 'cult guard% guard%)

(make-encounter 2 'easy 'cult cultist%)
(make-encounter 2 'medium 'cult cultist% commoner%)
(make-encounter 2 'hard 'cult cultist% guard%)
(make-encounter 2 'hard 'cult cultist% commoner% commoner%)
(make-encounter 2 'hard 'cult cultist% cultist%)

(make-encounter 2 'hard 'cult acolyte%)
(make-encounter 2 'deadly 'cult acolyte% guard%)
(make-encounter 2 'deadly 'cult acolyte% cultist%)

(make-encounter 2 'hard 'cult skeleton%)
(make-encounter 2 'deadly 'cult skeleton% cultist%)

(make-encounter 2 'hard 'cult zombie%)
(make-encounter 2 'deadly 'cult zombie% cultist%)


(module+ main
  ;; show how many of each kind of encounter we have, split by theme
  (for ([t all-themes])
    (displayln t)
    (display (~a "" #:min-width 5))
    (for ([d all-difficulties])
      (display (~a d #:min-width 10)))
    (newline)
    (for ([l (in-range 1 21)])
      (define counts
        (for/list ([d all-difficulties])
          (define encs (dict-ref all-encounters (cons l d) '()))
          (length (for/list ([e encs]
                             #:when (equal? (car e) t))
                    e))))
      (unless (andmap zero? counts)
        (display (~a l #:min-width 5))
        (for ([c counts])
          (display (~a c #:min-width 10)))
        (newline)))
    (newline))
  )
