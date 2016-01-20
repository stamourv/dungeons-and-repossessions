#lang racket

(require "character.rkt"
         "state.rkt"
         "message-queue.rkt"
         "vision.rkt"
         "ui.rkt"
         "grid.rkt"
         "utils.rkt")

(provide player%)

(define player%
  (class character%
    (field [level             1]
           [proficiency-bonus 2]
           [strength     3] ; as a "standard" fighter (based on pre-gens)
           [dexterity    2] ; Note: monsters don't need stats, as all their
           [constitution 2] ;   info (e.g., AC, attack bonus) is pre-computed
           [intelligence 0] ; Note: stored as bonus only, for simplicity
           [wisdom       -1]
           [charisma     1]
           [fov  (set)]
           [seen (set)])
    (inherit-field pos grid)

    (define/override (show) #\@)
    (define/override (describe #:capitalize? [capitalize? #f]
                               #:specific?   [specific? 'n/a]) ; always specific
      (string-append (article capitalize? #t) " player")) ; TODO have a name

    (define/override (act state)
      (update-fov)
      (display-state state)
      (handle-input state))

    (define debug:reveal-map #f)
    (define sight-range 7)
    (define (update-fov)
      (set! fov  (compute-fov grid pos sight-range))
      (when debug:reveal-map ; set the FOV to be the whole map
        (set! fov (for*/set ([x (in-range (grid-height grid))]
                             [y (in-range (grid-width  grid))])
                     (vector x y))))
      (set! seen (set-union seen fov)))

    (define/override (get-attack-bonus)
      ;; Note: assumes we're proficient with whatever weapon we're using
      (+ proficiency-bonus strength))
    (define/override (get-ac)
      (+ 14 (min dexterity 2) ; scale armor ; TODO have logic in item defn
         2)) ; shield
    (define/override (get-damage-die)
      (lambda _ (+ (d6) strength))) ; hand axe ; TODO have logic in item defn

    (super-new [max-hp (+ 10 constitution)]))) ; as a fighter


(module+ test
  (require rackunit
           "grid.rkt" "ai.rkt")

  (define (render-grid g) (string-join g "\n" #:after-last "\n"))
  (define p1 (new player%))
  (define g1
    '("****"
      "*  *"
      "****"))
  (define s1
    (new-state p1 (parse-grid g1)
               #:characters (list (cons p1 #(1 1)))))
  (check-equal? (show-grid (state-grid s1))
                (render-grid '("****"
                               "*@ *"
                               "****")))
  (void (send p1 move-right '(move 1)))
  (check-equal? (show-grid (state-grid s1))
                (render-grid '("****"
                               "* @*"
                               "****")))
  (void (send p1 move-up '(move 1))) ; can't move into a wall
  (check-equal? (show-grid (state-grid s1))
                (render-grid '("****"
                               "* @*"
                               "****")))

  (define training-dummy%
    (class character%
      (define/override (show)
        #\D)
      (define ai (wait-ai this))
      (define/override (act state) ; the dummy doesn't do anything
        (ai state))
      (define/override (get-ac) 10)
      (define/override (describe #:capitalize? [capitalize? #f]
                                 #:specific?   [specific?   #f])
        (string-append (article capitalize? specific?) " training dummy"))
      (super-new [max-hp 10])))

  (define (get-log thunk)
    (thunk)
    (begin0 (string-join message-queue "\n")
      (reset-message-queue!)))

  (random-seed 10)
  (check-equal?
   (get-log (lambda ()
              (define p (new player%))
              (define d (new training-dummy%))
              (for ([i 5]) (send p attack d))))
   (string-join '("The player attacks the training dummy and deals 7 damage!"
                  "The player attacks the training dummy and deals 9 damage!"
                  "The player attacks the training dummy and deals 6 damage!"
                  "The player attacks the training dummy and deals 4 damage!"
                  "The player attacks the training dummy and misses.")
                "\n")))
