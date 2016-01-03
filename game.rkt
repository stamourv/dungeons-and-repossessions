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
  (define p (new player%))
  (state (new-floor grid p #:player-pos #(1 1)
                    #:other-characters `((,(new training-dummy%) . #(1 8))))
         '("Welcome!")
         p ;; TODO roll initiative instead
         `(move 1))) ;; TODO base on active character. encapsulate in state.rkt

(define (game-loop s)
  (display-state s)
  (define action-taken (handle-input s))
  (unless (equal? action-taken 'quit)
    ;; TODO once we have other agents, return to main loop only if player is
    ;;   the active character, otherwise have an inner loop to have monsters
    ;;   act until it's the player's turn
    ;;   (will need other entry points besides `handle-input`, that return
    ;;   action-taken)
    (game-loop (next-state s action-taken))))

(module+ main
  (set-up-ui)
  (define exn #f)
  ;; whatever we do, always tear down UI
  (with-handlers ([values (lambda (e) (set! exn e))])
    (game-loop (init-game)))
  (void (tear-down-ui))
  (when exn (raise exn)))
