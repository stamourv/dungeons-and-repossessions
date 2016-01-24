#lang racket

(require "player.rkt"
         "generation.rkt"
         "message-queue.rkt"
         "state.rkt"
         "ui.rkt")

;; A game state is a Floor

(define (init-game [player (new player%)])
  (generate player))

(define (game-loop s)
  (define player (state-player s))
  (define active-character (first (state-initiative-order s)))
  (define action-taken     (send active-character act s))
  (unless (equal? action-taken 'quit)
    (define new-s (state-cleanup (next-state s action-taken)))
    (cond [(get-field has-won? player)
           => (lambda (item)
                (enqueue-message!
                 (format "~a has retrieved the ~a."
                         (send player describe #:capitalize? #t)
                         (send item   describe #:specific?   #t)))
                (enqueue-message! "Press any key for the next dungeon")
                (display-state new-s)
                (await-any-key)
                (send player next-dungeon)
                (game-loop (init-game player)))]
          [(positive? (get-field current-hp player))
           (game-loop new-s)] ; alive, keep going
          [else
           (enqueue-message!
            (format "~a has died.\nGame over.\n"
                    (send player describe #:capitalize? #t)))
           (display-state new-s)])))

(module+ main
  (void (set-up-ui))
  (define exn #f)
  ;; whatever we do, always tear down UI
  (with-handlers ([values (lambda (e) (set! exn e))])
    (game-loop (init-game)))
  (void (tear-down-ui))
  (when exn (raise exn)))
