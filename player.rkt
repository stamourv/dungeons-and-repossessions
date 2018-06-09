#lang racket

(require "character.rkt"
         "state.rkt"
         "message-queue.rkt"
         "vision.rkt"
         "items.rkt"
         "ui.rkt"
         "cell.rkt"
         "grid.rkt"
         "utils.rkt"
         "flags.rkt")

(provide fighter% brute%)

(define player%
  (class character%
    ;; Note: all of those are filled in by subclasses.
    ;; Note: monsters don't need those; all their info (e.g., AC, attack
    ;; bonus, etc.) is built-in
    (init-field strength dexterity constitution intelligence wisdom charisma
                hit-die body-armor shield weapon)
    (field [level             #f] ; we call `level-up` on construction
           [proficiency-bonus #f] ; ditto
           [fov               #f] ; initialized by `enter-dungeon`
           [seen              #f] ; ditto
           [inventory         '()])
    (inherit-field pos grid name max-hp current-hp)

    (define/override (describe #:capitalize? [capitalize? #f]
                               #:specific?   [specific? 'n/a]) ; always specific
      (super describe #:capitalize? capitalize? #:specific? #t))

    (define/override (act state)
      (update-fov)
      (display-state state)
      (handle-input state))

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
      (+ 10
         (if body-armor (get-field ac body-armor) 0)
         (min dexterity
              (if body-armor (get-field max-dex body-armor) dexterity))
         (if shield (get-field ac shield) 0)))
    (define/override (get-damage-die)
      (dice (if weapon (get-field damage-die weapon) d1)
            strength))

    (define/public (level-up [new-level (add1 level)])
      ;; TODO increase stats at the right levels, etc.
      ;;   also other level benefits, like fighting styles, etc.
      (set! level             new-level)
      (set! max-hp            (* level (+ hit-die constitution)))
      (set! current-hp        max-hp)
      (set! proficiency-bonus (+ (quotient (sub1 level) 4) 2)))

    (define/public (enter-dungeon)
      (set! fov  (set))
      (set! seen (set)))

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
    (define/public (show-inventory)
      (cond [(empty? inventory)
             (enqueue-message! "You have nothing in your inventory.")]
            [else
             (enqueue-message! "Inventory:")
             (for ([item (in-list inventory)])
               (enqueue-message! (format "  ~a" (send item describe))))])
      'invalid) ; doesn't consume an action

    (define/override (check-win-condition)
      (and (is-a? (grid-ref grid pos) entrance%)
           (for/first ([item (in-list inventory)]
                       #:when (is-a? item macguffin%))
             ;; and remove it from inventory, to not carry it across dungeons
             (set! inventory (remove item inventory))
             item)))

    (super-new [char #\@] [article 'none])
    (level-up 1)
    (enter-dungeon)))

;; balanced melee fighter
(define fighter%
  (class player%
    (super-new [name "Bland Alan"]
               [hit-die      10] ; fighter class
               [strength     2] ; Note: stored as bonus only, for simplicity
               [dexterity    2]
               [constitution 2]
               [intelligence 0]
               [wisdom       0]
               [charisma     0]
               [body-armor (new scale-armor%)]
               [shield     (new shield%)]
               [weapon     (new handaxe%)])))

;; high offense, low defense
(define brute%
  (class player%
    (super-new [name "Reckless Rick"]
               [hit-die      8]
               [strength     4]
               [dexterity    2]
               [constitution 0]
               [intelligence 0]
               [wisdom       0]
               [charisma     0]
               [body-armor (new leather-armor%)]
               [shield     #f]
               [weapon     (new greatsword%)])))


(module+ test
  (require rackunit
           "grid.rkt" "ai.rkt")

  (define (render-grid g) (string-join g "\n" #:after-last "\n"))
  (define p1 (new fighter%))
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
    (string-join (drain-messages!) "\n"))

  (random-seed 10)
  (check-equal?
   (get-log (lambda ()
              (define p (new fighter%))
              (define d (new training-dummy%))
              (for ([i 5]) (send p attack d))))
   (string-join '("The player attacks the training dummy and misses."
                  "The player attacks the training dummy and deals 4 damage!"
                  "The player attacks the training dummy and deals 6 damage!"
                  "The player attacks the training dummy and deals 9 damage!"
                  "The player attacks the training dummy and deals 7 damage!")
                "\n")))
