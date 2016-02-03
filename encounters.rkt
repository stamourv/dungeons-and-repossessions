#lang 2d racket

(require racket/random math/distributions 2d/match
         "monsters.rkt" "utils.rkt")

(provide generate-encounters instantiate-encounter)

;; An Encounter is a (Listof Monster)
;; An Encounter Template is a (Listof CR)
;; A Dungeon Template is a (Listof Difficulty)

(define (encounter-template-cost crs)
  (define total-xp
    (for/sum ([cr (in-list crs)]) (cr->xp cr)))
  (define adjusted-xp
    (* total-xp
       (encounter-multiplier (length crs))
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
(define (generate-dungeon-template character-level)
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
          (raise-arguments-error 'generate-dungeon-template "nothing can fit"
                                 "encounters" encs-so-far
                                 "remaining budget" remaining-budget
                                 "level" character-level))
        (define dist
          (discrete-dist (map car possible-difficulties)
                         (map cdr possible-difficulties)))
        (define new (sample dist))
        (loop (cons new encs-so-far)
              (- budget (dict-ref costs new)))])))
  (try))

;; (module+ main ; to test it out
;;   (for ([i 10])
;;     (displayln (generate-dungeon-template 1))))

(define (theme->valid-templates theme templates)
  (for/list ([t (in-list templates)]
             #:when (for/and ([cr (in-list t)])
                      (not (empty? (theme+cr->monsters theme cr)))))
    t))

;; pick concrete encounters given a template and the character level
(define (fill-dungeon-template template level)
  (define required-difficulties (remove-duplicates template))
  (define potential-encounter-templates
    (for/list ([d (in-list required-difficulties)])
      (cons d (enumerate-encounter-templates level d))))
  (define (get-valid-templates theme)
    (for/list ([(d ts) (in-dict potential-encounter-templates)])
      (cons d (theme->valid-templates theme ts))))
  (define possible-themes+
    (for*/list ([theme  (in-list all-themes)]
                [ds+tss (in-value (get-valid-templates theme))]
                #:when (for/and ([(d ts) (in-dict ds+tss)]) (not (empty? ts))))
      (cons theme ds+tss)))
  (define theme+ (random-ref possible-themes+)) ; stick to one theme per dungeon
  (match-define (cons theme ds+tss) theme+)
  (values
   theme
   (for/list ([diff (in-list template)])
     (define encounter-template (random-ref (dict-ref ds+tss diff)))
     (for/list ([cr (in-list encounter-template)])
       (random-ref (theme+cr->monsters theme cr))))))

;; generates a list of encounters for a given player level
(define (generate-encounters level)
  (fill-dungeon-template (generate-dungeon-template level) level))

(define (instantiate-encounter e)
  (define inst (for/list ([m-c (in-list e)]) (new m-c)))
  (for ([m (in-list inst)])
    (set-field! encounter m inst)) ; connect monsters to the others in the room
  inst)

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
;; number of CRs that need to be considered at once.
(define max-monsters-per-encounter 6)
(define (enumerate-encounter-templates level difficulty)
  (define budget (encounter-experience-budget level difficulty))
  (define possible-crs
    (drop (for/list ([cr (in-list all-crs)]
                     #:when (< cr level))
            cr)
          (max 0 (- level 3)))) ; at level 4, no more rats ; TODO tweak
  ;; enumerate up to max encounter size
  (set->list
   (for*/set ([n (in-range 1 (add1 max-monsters-per-encounter))]
              ;; enumerate all encounters of n monsters (within budget)
              [e (in-list (apply cartesian-product (make-list n possible-crs)))]
              #:when (close-enough? (encounter-template-cost e) budget))
     ;; order doesn't matter, templates are bags of CRs
     ;; canonicalize representation to avoid repeats
     (sort e <))))


(module+ main

  ;; show number of encounter templates for each level+difficulty
  ;; then, for each theme, show how many of those are possible

  (define all-encounter-templates (make-hash))

  (for ([d all-difficulties])
    (display (~a d #:min-width 10)))
  (newline)
  (for ([l (in-range 1 8 #|21|#)])
    (define counts
      (for/list ([d all-difficulties])
        (define templates (enumerate-encounter-templates l d))
        (hash-set! all-encounter-templates (cons l d) templates)
        (length templates)))
    (unless (andmap zero? counts)
      (display (~a l #:min-width 5))
      (for ([c counts])
        (display (~a c #:min-width 10)))
      (newline)))
  (newline)

  (for ([t all-themes])
    (displayln t)
    (display (~a "" #:min-width 5))
    (for ([d all-difficulties])
      (display (~a d #:min-width 10)))
    (newline)
    (for ([l (in-range 1 8 #|21|#)])
      (define counts
        (for/list ([d all-difficulties])
          (define templates (hash-ref all-encounter-templates (cons l d)))
          (length (theme->valid-templates t templates))))
      (unless (andmap zero? counts)
        (display (~a l #:min-width 5))
        (for ([c counts])
          (display (~a c #:min-width 10)))
        (newline)))
    (newline))
  )


;; from DM Basic Rules, page 57: Adventuring Day XP
;; these values are after adjusting with the encounter-multiplier
(define (day-experience-budget level)
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

;; from DM Basic Rules, page 61 and on
(define all-crs (append '(0 1/8 1/4 1/2) (range 1 11)))
(define (cr->xp cr)
  (case cr
    [(  0)   10]
    [(1/8)   25]
    [(1/4)   50]
    [(1/2)  100]
    [(  1)  200]
    [(  2)  450]
    [(  3)  700]
    [(  4) 1100]
    [(  5) 1800]
    [(  6) 2300]
    [(  7) 2900]
    [(  8) 3900]
    [(  9) 5000]
    [( 10) 5900]))
