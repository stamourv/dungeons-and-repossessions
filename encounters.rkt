#lang 2d racket

(require 2d/match
         math/distributions)

(provide make-encounter
         all-difficulties
         all-encounters)

;; An Encounter is a (Pair Theme (Listof Monster))
;; where a Theme is a symbol (chosen from a small set) that determines
;; which encounters can go together (i.e., have the same theme)

(define (encounter-cost encounter)
  (define monsters (cdr encounter))
  (for/sum ([m (in-list monsters)]) (get-field xp-value (new m))))

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
  (define total-xp (encounter-cost encounter))
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

;; subject to tweaking, based on how crowded we want floors to feel
;; Note: also useful to avoid a "long tail" of easy encounters to fill
;;   a budget, when nothing else will fit
(define max-n-encounters 6)

;; given the player's level, allocate a floor's xp budget between a number
;; of encounters of varying difficulty
;; returns a list of difficulties
(define (generate-encounter-template character-level)
  (define budget (floor-experience-budget character-level))
  (define costs
    (for/list ([d (in-list all-difficulties)])
      (cons d (encounter-experience-budget character-level d))))
  (define (try)
    (let loop ([encs-so-far '()] [remaining-budget budget])
      (cond
       ;; Note: budget is currently checked based on the expected encounter
       ;;   cost (determined by level and difficulty), not on the actual cost
       ;;   of the concrete encounters
       ;;   in the end (I think) the total cost per floor remains bounded by
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
          (raise-arguments-error 'generate-floor-encounters "nothing can fit"
                                 "encounters" encs-so-far
                                 "remaining budget" remaining-budget
                                 "level" character-level))
        (define dist
          (discrete-dist (map car possible-difficulties)
                         (map cdr possible-difficulties)))
        (define new (sample dist))
        ;; TODO try preventing 2+ deadlies on the same floor?
        (loop (cons new encs-so-far)
              (- budget (dict-ref costs new)))])))
  (try))

;; (module+ main ; to test it out
;;   (for ([i 10])
;;     (displayln (generate-encounter-template 1))))

;; pick concrete encounters given a template and the character level
(define (fill-encounter-template template level)
  (define possible-themes ; stick to one theme for the floor
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
;;   (dynamic-require "monsters.rkt" #f)
;;   (define level 2)
;;   (for ([i 10])
;;     (define t (generate-encounter-template level))
;;     (for ([d t]
;;           [e (fill-encounter-template t level)])
;;       (printf "~a ~a\n" (~a d #:min-width 6) e))
;;     (newline)))


;; from DM Basic Rules, page 57: Adventuring Day XP
;; these values are after adjusting with the encounter-multiplier
(define (floor-experience-budget level)
  (case level
    [( 1)   300] [( 2)   600] [( 3)  1200] [( 4)  1700] [ (5)  3500]
    [( 6)  4000] [( 7)  5000] [( 8)  6000] [( 9)  7500] [(10)  9000]
    [(11) 10500] [(12) 11500] [(13) 13500] [(14) 15000] [(15) 18000]
    [(16) 20000] [(17) 25000] [(18) 27000] [(19) 30000] [(20) 40000]))

;; from DM Basic Rules, page 56: XP Thresholds by Character Level
;; these values are after adjusting with the encounter-multiplier
(define (encounter-experience-budget level difficulty)
  #2dmatch
  ╔══════════════╦═══════╦══════════╦═══════╦═════════╗
  ║   difficulty ║ 'easy ║ 'medium  ║ 'hard ║ 'deadly ║
  ║ level        ║       ║          ║       ║         ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║   1          ║    25 ║     50   ║    75 ║    100  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║   2          ║    50 ║    100   ║   150 ║    200  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║   3          ║    75 ║    150   ║   225 ║    400  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║   4          ║   125 ║    250   ║   375 ║    500  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║   5          ║   250 ║    500   ║   750 ║   1100  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║   6          ║   300 ║    600   ║   900 ║   1400  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║   7          ║   350 ║    750   ║  1100 ║   1700  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║   8          ║   450 ║    900   ║  1400 ║   2100  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║   9          ║   550 ║   1100   ║  1600 ║   2400  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║  10          ║   600 ║   1200   ║  1900 ║   2800  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║  11          ║   800 ║   1600   ║  2400 ║   3600  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║  12          ║  1000 ║   2000   ║  3000 ║   4500  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║  13          ║  1100 ║   2200   ║  3400 ║   5100  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║  14          ║  1250 ║   2500   ║  3800 ║   5700  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║  15          ║  1400 ║   2800   ║  4300 ║   6400  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║  16          ║  1600 ║   3200   ║  4800 ║   7200  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║  17          ║  2000 ║   3900   ║  5900 ║   8800  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║  18          ║  2100 ║   4200   ║  6300 ║   9500  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║  19          ║  2400 ║   4900   ║  7300 ║  10900  ║
  ╠══════════════╬═══════╬══════════╬═══════╬═════════╣
  ║  20          ║  2800 ║   5700   ║  8500 ║  12700  ║
  ╚══════════════╩═══════╩══════════╩═══════╩═════════╝)

;; from DM Basic Rules, page 56: Encounter Multipliers
;; using next multiplier up, as suggested for parties of less than 3
;; characters (player is alone in this game)
(define (encounter-multiplier n-monsters)
  (case n-monsters
    [( 1)          1.5]
    [( 2)          2]
    [( 3  4  5  6) 2.5]
    [( 7  8  9 10) 3]
    [(11 12 13 14) 4]
    [else          5]))
