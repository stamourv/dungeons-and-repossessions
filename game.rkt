#lang racket

(require "floor.rkt"
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
  (state (new-floor grid #:player-pos #(1 1))
         '("Welcome!")))

(define (game-loop f)
  (display-state f)
  (unless (equal? (handle-input f) 'quit)
    (game-loop f)))

(module+ main
  (set-up-ui)
  (with-handlers ([values values]) ; whatever we do, always tear down UI
    (game-loop (init-game)))
  (void (tear-down-ui)))
