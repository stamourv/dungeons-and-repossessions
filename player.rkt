#lang racket

(require "character.rkt"
         "state.rkt"
         "message-queue.rkt"
         "vision.rkt"
         "items.rkt"
         "ui.rkt"
         "grid.rkt"
         "utils.rkt")

(provide player%)

(define player%
  (class character%
    (field [level             0] ; we call `level-up` on construction
           [proficiency-bonus #f]
           [strength     3] ; as a "standard" fighter (based on pre-gens)
           [dexterity    2] ; Note: monsters don't need stats, as all their
           [constitution 2] ;   info (e.g., AC, attack bonus) is pre-computed
           [intelligence 0] ; Note: stored as bonus only, for simplicity
           [wisdom       -1]
           [charisma     1]
           [fov          (set)]
           [seen         (set)]
           [inventory    '()]
           [has-won?     #f])
    (inherit-field pos grid name max-hp current-hp)

    (define/override (describe #:capitalize? [capitalize? #f]
                               #:specific?   [specific? 'n/a]) ; always specific
      (super describe #:capitalize? capitalize? #:specific? #t))

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

    (define/public (level-up)
      ;; TODO increase stats at the right levels, etc.
      ;;   also other level benefits, like fighting styles, etc.
      (set! level             (add1 level))
      (set! max-hp            (* level (+ 10 constitution))) ; as a fighter
      (set! current-hp        max-hp)
      (set! proficiency-bonus (+ (quotient (sub1 level) 4) 2)))

    (define/public (pick-up) ; TODO bring up a dialog to ask what to pick up
      (define cell  (grid-ref grid pos))
      (define items (get-field items cell))
      (cond [(empty? items)
             (enqueue-message! "There is nothing to pick up.")
             'invalid]
            [else
             (set! inventory (append inventory items))
             (set-field! items cell '())
             (enqueue-message!
              (format "Picked up ~a."
                      (string-join (for/list ([i (in-list items)])
                                     (send i describe))
                                   ", ")))
             'wait])) ; ends turn
    (define/public (show-inventory) ; TODO have it be a separate dialog
      (cond [(empty? inventory)
             (enqueue-message! "You have nothing in your inventory.")]
            [else
             (enqueue-message! "Inventory:")
             (for ([item (in-list inventory)])
               (enqueue-message! (format "  ~a" (send item describe))))])
      'invalid) ; doesn't consume an action

    (define/override (check-win-condition)
      (for/first ([item (in-list inventory)]
                  #:when (is-a? item macguffin%))
        (set! has-won? item)
        ;; and remove it from inventory, to not carry it across dungeons
        (set! inventory (remove item inventory))))

    (super-new [char #\@] [name "player"]) ; TODO have a name
    (level-up)))


(module+ test
  (require rackunit
           "grid.rkt" "ai.rkt")

  (define (render-grid g) (string-join g "\n" #:after-last "\n"))
  (define p1 (new player%))
  (define g1
    '("XXXX"
      "X  X"
      "XXXX"))
  (define s1
    (new-state p1 (parse-grid g1)
               #:characters (list (cons p1 #(1 1)))))
  (check-equal? (show-grid (state-grid s1))
                (render-grid '("XXXX"
                               "X@ X"
                               "XXXX")))
  (void (send p1 move-right '(move 1)))
  (check-equal? (show-grid (state-grid s1))
                (render-grid '("XXXX"
                               "X @X"
                               "XXXX")))
  (void (send p1 move-up '(move 1))) ; can't move into a wall
  (check-equal? (show-grid (state-grid s1))
                (render-grid '("XXXX"
                               "X @X"
                               "XXXX")))

  (define training-dummy%
    (class character%
      (define ai (new wait-ai% [monster this]))
      (define/override (act state) ; the dummy doesn't do anything
        (send ai act state))
      (define/override (get-ac) 10)
      (super-new [name "training dummy"] [char #\D] [max-hp 10])))

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
