#lang racket

(require "grid.rkt")

(provide (all-defined-out))

;; list of strings (messages) which were produced since the previous
;; previous display, and need to be displayed now
(define message-queue '())
(define (enqueue-message! m)
  (set! message-queue (cons m message-queue)))
(define (reset-message-queue!)
  (set! message-queue '()))

(struct state
  (player
   grid
   initiative-order ; listof character%
   mode)) ; a mode is either `(move ,n-moves-left) or 'attack


(define (new-state p map-los
                   #:player-pos player-pos
                   #:other-characters [other-characters '()]); dictof character%
  (define g (parse-grid map-los))
  (define initiative-order
    (cons p (dict-keys other-characters))) ;; TODO roll initiative instead
  ;; place characters
  (for ([(char pos) (in-dict (dict-set other-characters p player-pos))])
    (set-field! grid char g)
    (send char move pos '(move 1))) ; dummy mode, to ensure move validity
  (state p
         g
         initiative-order
         `(move ,(get-field speed (first initiative-order)))))


;; TODO allow taking moves and attacks to be in a different order, or broken up
;;   (for now, always move, then attack, then end of turn)
(define (next-state s action-taken)
  (match-define (state player grid initiative-order mode) s)
  (define (new-turn) ; switch active character and go back to moving
    (define new-initiative-order
      (append (rest initiative-order) (list (first initiative-order))))
    (state player
           grid
           new-initiative-order
           `(move ,(get-field speed (first new-initiative-order)))))
  (define (new-move-state n #:dash? [dash? #f])
    (state player grid initiative-order `(,(if dash? 'dash 'move) ,n)))
  (define (new-attack-state)
    (state player grid initiative-order 'attack))
  (match mode
    [`(,(and head (or 'move 'dash)) ,n-moves-left)
     (case action-taken
       [(wait)
        (new-attack-state)]
       [(move)
        (define new-n (sub1 n-moves-left))
        (define dash? (equal? head 'dash))
        (if (zero? new-n) ; no more moves
            (if dash?
                (new-turn)
                (new-attack-state))
            (new-move-state new-n #:dash? dash?))]
       [(attack) ; end move prematurely to attack
        ;; (invalid when dashing. checking in character% move method)
        (new-turn)]
       [(invalid) ; doesn't count against the number of moves
        s]
       [else
        (raise-argument-error
         'next-state "valid action (move state)" action-taken)])]
    ['attack
     (case action-taken
       [(wait)
        (new-turn)]
       [(move) ; forego attack for double move
        (new-move-state
         #:dash? #t
         (sub1 (get-field speed (first initiative-order))))] ; already moved 1
       [(attack)
        (new-turn)]
       [(invalid) ; try again
        s]
       [else
        (raise-argument-error
         'next-state "valid action (attack state)" action-taken)])]
    [else
     (raise-argument-error 'next-state "valid mode" mode)]))


;; end of turn cleanup
(define (state-cleanup s)
  (match-define (state player grid initiative-order mode) s)
  ;; remove dead monsters
  (define new-initiative-order
    (for/list ([m (in-list initiative-order)]
               #:when (if (or (positive? (get-field current-hp m))
                              (equal? m player)) ; dead player handled elsewhere
                          #t ; alive, keep
                          (begin (send m die) #f))) ; dead, remove
      m))
  (state player grid new-initiative-order mode))
