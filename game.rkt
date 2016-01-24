#lang racket

(require racket/cmdline
         "player.rkt"
         "generation.rkt"
         "message-queue.rkt"
         "state.rkt"
         "ui.rkt"
         "flags.rkt")

(define init-level
  (command-line
   #:once-each
   [("--reveal-map") "Show entire map (for debugging)."
    (set-debug:reveal-map!)]
   [("--god-mode")   "Make player invincible (for debugging)."
    (set-debug:god-mode!)]
   #:args ([init-level "1"])
   (string->number init-level)))

(define (init-player)
  (define p (new player%))
  (send p level-up init-level)
  p)

(define (init-dungeon player)
  (generate player))

(define (game-loop s)
  (define player (state-player s))
  (define active-character (first (state-initiative-order s)))
  (define action-taken     (send active-character act s))
  (unless (equal? action-taken 'quit)
    (define new-s (state-cleanup (next-state s action-taken)))
    (cond [(send player check-win-condition)
           => (lambda (item)
                (enqueue-message!
                 (format "~a has retrieved the ~a."
                         (send player describe #:capitalize? #t)
                         (send item   describe #:specific?   #t)))
                (enqueue-message! "Press any key for the next dungeon")
                (display-state new-s)
                (await-any-key)
                (send player next-dungeon)
                (game-loop (init-dungeon player)))]
          [(or (positive? (get-field current-hp player))
               debug:god-mode)
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
    (game-loop (init-dungeon (init-player))))
  (void (tear-down-ui))
  (when exn (raise exn)))
