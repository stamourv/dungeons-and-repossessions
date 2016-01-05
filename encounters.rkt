#lang 2d racket

(require 2d/match)

(provide make-encounter)

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


;; An Encounter is a Listof Monster

;; maps (level . difficulty) pairs to sets of encounters
(define possible-encounters (make-hash))

(define (close-enough? x y) ; within 25%
  (<= (* 0.75 y) x (* 1.25 y)))
(define (make-encounter level difficulty . monsters)
  (define total-xp
    (for/sum ([m (in-list monsters)]) (get-field xp-value m)))
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
  (hash-update! possible-encounters
                (cons level difficulty)
                (lambda (xs) (cons monsters xs))
                '()))
