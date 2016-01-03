#lang racket

(require "character.rkt"
         "floor.rkt"
         "state.rkt"
         "ui.rkt")

;; A game state is a Floor

(define (init-game)
  (define grid
    '("**********"
      "* *      *"
      "* *   ****"
      "* *      *"
      "* *      *"
      "*        *"
      "**********"))
  (define p     (new player%))
  (define dummy (new training-dummy%))
  (state p
         (new-floor grid p #:player-pos #(1 1)
                    #:other-characters `((,dummy . #(1 8))))
         '("Welcome!")
         (list p dummy) ;; TODO roll initiative instead
         `(move 1))) ;; TODO base on active character. encapsulate in state.rkt

(define (game-loop s)
  (display-state s)
  (define active-character (first (state-initiative-order s)))
  (define action-taken
    (parameterize ([current-state s])
      (if (equal? active-character
                  (state-player s))
          (handle-input s) ;; TODO should this be a player's `act` method?
          (send active-character act))))
  (unless (equal? action-taken 'quit)
    (define new-s (next-state s action-taken))
    ;; end of turn cleanup
    (parameterize ([current-state new-s])
      (remove-dead-monsters! (state-floor s)))
    (cond [(positive? (get-field current-hp (state-player new-s)))
           (game-loop new-s)] ; alive, keep going
          [else
           (printf "~a has died.\nGame over.\n"
                   (send (state-player new-s) describe
                         #:capitalize? #t))])))

(module+ main
  (set-up-ui)
  (define exn #f)
  ;; whatever we do, always tear down UI
  (with-handlers ([values (lambda (e) (set! exn e))])
    (game-loop (init-game)))
  (void (tear-down-ui))
  (when exn (raise exn)))
