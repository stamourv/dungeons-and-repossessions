#lang racket

(require "grid.rkt"
         "message-queue.rkt"
         "state.rkt"
         "ai.rkt"
         "vision.rkt"
         "ui.rkt"
         "utils.rkt")

(provide player%
         npc%
         training-dummy%
         brownian-dummy%)

(module+ test (require rackunit))

;; should not be instantiated directly (hence not exported)
;; interfaces can't have method definitions (AFAICT), so this "abstract class"
;; will have to do
(define character%
  (class object%
    (init-field [max-hp 1]
                [speed  6]) ; default to human speed (30 ft = 6 squares)
    (field [grid #f] ; grid where the character is active
           [pos  #f] ; 2-vector of integer (what math/array uses as indices)
           [current-hp max-hp])

    (define/public (show)
      (error "can't show a character%"))
    (define/public (describe)
      (error "can't describe a character%"))
    (define/public (act state) ; returns a kind of action
      (error "can't ask a character% to act"))

    (define/public (move new-pos mode)
      (cond
       [(grid-ref grid new-pos) => ; don't go off the map
        (lambda (new-cell)
          (cond
           [(send new-cell free?) ; can move there
            (when pos ; when initially placed, won't have a position
              (set-field! occupant (array-ref grid pos) #f))
            (set! pos new-pos)
            (set-field! occupant new-cell this)
            'move] ; return the action we took
           [(get-field occupant new-cell) => ; occupied, try to attack
            (lambda (occ)
              (match mode
                [`(dash ,_) ; already gave up on attacking this turn
                 'invalid]
                [_
                 (attack this occ)]))] ; we attack whoever is there
           [else
            'invalid]))]
       [else
        'invalid]))
    (define/public (move-left mode)
      (move (left pos) mode))
    (define/public (move-right mode)
      (move (right pos) mode))
    (define/public (move-up mode)
      (move (up pos) mode))
    (define/public (move-down mode)
      (move (down pos) mode))

    (define/public (get-attack-bonus)
      (error "no attack bonus specified" this))
    (define/public (get-ac)
      (error "no AC specified" this))
    (define/public (get-damage-die)
      (error "no damage die specified" this))

    (define/public (die)
      (enqueue-message!
       (format "~a dies." (send this describe #:capitalize? #t #:specific? #t)))
      (set-field! occupant (array-ref grid pos) #f))

    (super-new)))


(module+ test
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
                               "****"))))


(define (attack-hits? attack-roll ac)
  (and (not (= attack-roll 1)) ; automatic miss
       (or (>= attack-roll ac)
           (=  attack-roll 20)))) ; automatic hit

(define (attack attacker defender)
  (define base-message
    (format "~a attacks ~a"
            (send attacker describe #:capitalize? #t #:specific? #t)
            (send defender describe #:specific? #t)))
  ;; TODO add advantage / disadvantage, criticals, etc.
  (define attack-roll (+ (d20) (send attacker get-attack-bonus)))
  (cond [(attack-hits? attack-roll (send defender get-ac))
         (define damage-roll ((send attacker get-damage-die)))
         (set-field! current-hp defender
                     (- (get-field current-hp defender) damage-roll))
         (enqueue-message!
          (format "~a and deals ~a damage!" base-message damage-roll))]
        [else
         (enqueue-message!
          (string-append base-message " and misses."))])
  'attack)

(module+ test
  (define (get-log thunk)
    (thunk)
    (begin0 (string-join message-queue "\n")
      (reset-message-queue!)))

  (random-seed 10)
  (check-equal?
   (get-log (lambda ()
              (define p (new player%))
              (define d (new training-dummy%))
              (for ([i 5]) (attack p d))))
   (string-join '("The player attacks the training dummy and deals 7 damage!"
                  "The player attacks the training dummy and deals 9 damage!"
                  "The player attacks the training dummy and deals 6 damage!"
                  "The player attacks the training dummy and deals 4 damage!"
                  "The player attacks the training dummy and misses.")
                "\n")))


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

    (define sight-range 7)
    (define (update-fov)
      (set! fov  (compute-fov grid pos sight-range))
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


(define npc%
  (class character%
    [init-field name]
    (define/override (describe #:capitalize? [capitalize? #f]
                               #:specific?   [specific?   #f])
      (string-append (article capitalize? specific?) " " name))
    (super-new)))

(define training-dummy%
  (class npc%
    (define/override (show)
      #\D)
    (define/override (act state) ; the dummy doesn't do anything
      (wait-ai this state))
    (define/override (get-ac) 10)
    (super-new [name "training dummy"] [max-hp 10])))

(define brownian-dummy%
  (class npc%
    (define/override (show)
      #\B)
    ;; moves at random, which attacks if it runs into something
    (define/override (act state)
      (random-move-ai this state))
    (define/override (get-ac)           10)
    (define/override (get-attack-bonus) 0)
    (define/override (get-damage-die)   d4)
    (super-new [name "brownian dummy"] [max-hp 10])))
