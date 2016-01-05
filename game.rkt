#lang racket

(require "character.rkt"
         "message-queue.rkt"
         "state.rkt"
         "ui.rkt")

;; A game state is a Floor

(define (init-game)
  (define grid
    '("╔═╦══════╗"
      "║ ║   |  ║"
      "║ ║   ═══╣"
      "║ ║      ║"
      "║-║  #   ║"
      "║        ║"
      "╚════════╝"))
  (enqueue-message! "Welcome!")
  (new-state (new player%) grid
             #:player-pos #(1 1)
             #:other-characters `((,(new brownian-dummy%) . #(1 8))
                                  (,(new training-dummy%) . #(3 8)))))

(define (game-loop s)
  (define active-character (first (state-initiative-order s)))
  (define action-taken
    (cond [(equal? active-character (state-player s))
           (display-state s)
           (handle-input s)]
          [else ; npc
           (send active-character act s)]))
  (unless (equal? action-taken 'quit)
    (define new-s (state-cleanup (next-state s action-taken)))
    (cond [(positive? (get-field current-hp (state-player new-s)))
           (game-loop new-s)] ; alive, keep going
          [else
           (enqueue-message!
            (format "~a has died.\nGame over.\n"
                    (send (state-player new-s) describe
                          #:capitalize? #t)))
           (display-state new-s)])))

(module+ main
  (set-up-ui)
  (define exn #f)
  ;; whatever we do, always tear down UI
  (with-handlers ([values (lambda (e) (set! exn e))])
    (game-loop (init-game)))
  (void (tear-down-ui))
  (when exn (raise exn)))
