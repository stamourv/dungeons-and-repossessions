#lang racket

(require "grid.rkt"
         "message-queue.rkt"
         "sprite.rkt"
         "ai.rkt"
         "utils.rkt")

(provide character%)

;; should not be instantiated directly (hence not exported)
;; interfaces can't have method definitions (AFAICT), so this "abstract class"
;; will have to do
(define character%
  (class sprite%
    (init-field [max-hp 1]
                [speed  6]) ; default to human speed (30 ft = 6 squares)
    (field [grid #f] ; grid where the character is active
           [pos  #f] ; 2-vector of integer (what math/array uses as indices)
           [current-hp max-hp])

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
                 (attack occ)]))] ; we attack whoever is there
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

    (define/public (attack defender)
      (define base-message
        (format "~a attacks ~a"
                (send this describe #:capitalize? #t #:specific? #t)
                (send defender describe #:specific? #t)))
      ;; TODO add advantage / disadvantage, criticals, etc.
      (define attack-roll (+ (d20) (send this get-attack-bonus)))
      (cond [(attack-hits? attack-roll (send defender get-ac))
             (define damage-roll ((send this get-damage-die)))
             (set-field! current-hp defender
                         (- (get-field current-hp defender) damage-roll))
             (enqueue-message!
              (format "~a and deals ~a damage!" base-message damage-roll))]
            [else
             (enqueue-message!
              (string-append base-message " and misses."))])
      'attack)

    (define/public (die)
      (enqueue-message!
       (format "~a dies." (send this describe #:capitalize? #t #:specific? #t)))
      (set-field! occupant (array-ref grid pos) #f))

    (super-new)))


(define (attack-hits? attack-roll ac)
  (and (not (= attack-roll 1)) ; automatic miss
       (or (>= attack-roll ac)
           (=  attack-roll 20)))) ; automatic hit
