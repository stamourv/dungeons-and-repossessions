#lang racket

(require racket/cmdline
         "player.rkt"
         "generation.rkt"
         "message-queue.rkt"
         "state.rkt"
         "ui.rkt"
         "flags.rkt")

(define (init-player)
  (new fighter%))

(define (init-dungeon player)
  (begin0 (generate player)
    (full-screen-message (drain-briefing!))))

(define (game-loop s)
  (define player (state-player s))
  (define active-character (first (state-initiative-order s)))
  (define action-taken     (send active-character act s))
  (unless (equal? action-taken 'quit)
    (define new-s (state-cleanup (next-state s action-taken)))
    (cond [(send player check-win-condition)
           (full-screen-message (drain-ending!))]
          [(or (positive? (get-field current-hp player))
               debug:god-mode)
           (game-loop new-s)] ; alive, keep going
          [else
           (full-screen-message (drain-game-over!))])))

(module+ main
  (void (set-up-ui))
  (define exn #f)
  ;; whatever we do, always tear down UI
  (with-handlers ([values (lambda (e) (set! exn e))])
    (display-title)
    (game-loop (init-dungeon (init-player))))
  (void (tear-down-ui))
  (when exn (raise exn)))
