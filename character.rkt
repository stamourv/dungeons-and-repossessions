#lang racket

(require math/array
         "grid.rkt"
         "state.rkt")

(provide player%
         training-dummy%)

(module+ test (require rackunit))

;; should not be instantiated directly (hence not exported)
;; interfaces can't have method definitions (AFAICT), so this "abstract class"
;; will have to do
(define character%
  (class object%
    (field [grid #f] ; grid where the character is active
           [pos  #f] ; 2-vector of integer (what math/array uses as indices)
           [speed 6] ; default to human speed (30 ft = 6 squares)
           [proficiency-bonus 0]
           )

    (define/public (move new-pos)
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
         [(get-field occupant new-cell) =>
          (lambda (occ)
            (attack this occ))] ; we attack whoever is there
         [else
          'invalid])]
       [else
        'invalid]))
    (define/public (move-left)
      (move (left pos)))
    (define/public (move-right)
      (move (right pos)))
    (define/public (move-up)
      (move (up pos)))
    (define/public (move-down)
      (move (down pos)))

    (define/public (get-attack-bonus)
      ;; Note: assumes we're proficient with whatever weapon we're using
      proficiency-bonus) ; TODO add strength
    (define/public (get-ac)
      10) ; TODO add dex, armor, etc.
    (define/public (get-damage-die)
      d6) ; TODO have it based on equipped weapon

    (super-new)))

(define (random-between min max) ;; TODO replace with 6.4's `random`
  (+ min (random (- max min))))
(define (d6)
  (random-between 1 7))
(define (d20)
  (random-between 1 21))

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
         ;; TODO actually deal damage, death, player death, etc.
         (enqueue-message!
          (format "~a and deals ~a damage!" base-message damage-roll))]
        [else
         (enqueue-message!
          (string-append base-message " and misses."))])
  'attack)

(module+ test
  (define (get-log thunk)
    (define s (state #f '() #f #f)) ; "mock" state
    (parameterize ([current-state s])
      (thunk))
    (string-join (state-message-queue s) "\n"))

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


;; TODO have in some misc utils file
(define (article capitalize? specific?
                 #:an? [an? #f])
  (if specific?
      (if capitalize? "The" "the")
      (if an?
          (if capitalize? "An" "an")
          (if capitalize? "A"  "a"))))

(define player%
  (class character%
    (define/public (show)
      #\@) ;; TODO add parsing for player position
    (define/public (describe #:capitalize? [capitalize? #f]
                             #:specific?   [specific?   'n/a]) ; always specific
      (string-append (article capitalize? #t) " player")) ; TODO have a name
    (super-new)))

(define training-dummy%
  (class character%
    (define/public (show)
      #\D)
    (define/public (describe #:capitalize? [capitalize? #f]
                             #:specific?   [specific?   #f])
      (string-append (article capitalize? specific?) " training dummy"))
    (super-new)))
