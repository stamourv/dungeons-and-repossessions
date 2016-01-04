#lang racket

(require math/array
         "grid.rkt"
         "state.rkt"
         "utils.rkt")

(provide player%
         training-dummy%
         brownian-dummy%)

(module+ test (require rackunit))

;; should not be instantiated directly (hence not exported)
;; interfaces can't have method definitions (AFAICT), so this "abstract class"
;; will have to do
(define character%
  (class object%
    (init-field [max-hp 1])
    (field [grid #f] ; grid where the character is active
           [pos  #f] ; 2-vector of integer (what math/array uses as indices)
           [speed 6] ; default to human speed (30 ft = 6 squares)
           [proficiency-bonus 0]
           [current-hp max-hp])

    (define/public (show)
      (error "can't show a character%"))
    (define/public (describe)
      (error "can't describe a character%"))

    (define/public (move new-pos mode)
      (cond
       [(within-grid? grid new-pos) ; don't go off the map
        (define new-cell (array-ref grid new-pos))
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
          'invalid])]
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
      ;; Note: assumes we're proficient with whatever weapon we're using
      proficiency-bonus) ; TODO add strength
    (define/public (get-ac)
      10) ; TODO add dex, armor, etc.
    (define/public (get-damage-die)
      d6) ; TODO have it based on equipped weapon

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
    (new-state p1 g1 #:player-pos #(1 1)))
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
   (string-join '("The player attacks the training dummy and deals 6 damage!"
                  "The player attacks the training dummy and deals 3 damage!"
                  "The player attacks the training dummy and misses."
                  "The player attacks the training dummy and misses."
                  "The player attacks the training dummy and misses.")
                "\n")))


(define player%
  (class character%
    (define/override (show)
      #\@) ;; TODO add parsing for player position
    (define/override (describe #:capitalize? [capitalize? #f]
                               #:specific?   [specific? 'n/a]) ; always specific
      (string-append (article capitalize? #t) " player")) ; TODO have a name
    (super-new [max-hp 10]))) ; as a fighter ;; TODO add constitution


(define npc%
  (class character%
    [init-field name]
    (define/override (describe #:capitalize? [capitalize? #f]
                               #:specific?   [specific?   #f])
      (string-append (article capitalize? specific?) " " name))
    (define/public (act) ; returns a kind of action
      (error "npc%s can't act"))
    (super-new)))

(define training-dummy%
  (class npc%
    (define/override (show)
      #\D)
    (define/override (act mode) ; the dummy doesn't do anything
      ;; to avoid printing both when moving and attackin
      (when (equal? mode 'attack)
        (enqueue-message! (format "~a waits."
                                  (send this describe
                                        #:capitalize? #t #:specific? #t))))
      'wait)
    (super-new [name "training dummy"] [max-hp 10])))

(define brownian-dummy%
  (class npc%
    (define/override (show)
      #\B)
    ;; moves at random, which attacks if it runs into something
    (define/override (act mode)
      (case (random 5)
        [(0) (send this move-left  mode)]
        [(1) (send this move-right mode)]
        [(2) (send this move-up    mode)]
        [(3) (send this move-down  mode)]
        [(4) 'wait]))
    (super-new [name "brownian dummy"] [max-hp 10])))
