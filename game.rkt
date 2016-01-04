#lang racket

(require "character.rkt"
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
  (enqueue-message! "Welcome!")
  (new-state (new player%) grid
             #:player-pos #(1 1)
             #:other-characters `((,(new brownian-dummy%) . #(1 8))
                                  (,(new training-dummy%) . #(3 8)))))

(define animate? #f)

(define (game-loop s)
  (define active-character (first (state-initiative-order s)))
  (define action-taken
    (cond [(equal? active-character
                   (state-player s))
           ;; TODO should this be a player's `act` method?
           (reset-message-queue!)
           (begin0 (handle-input s)
             (unless animate? ; only display during player turn
               (display-state s)))]
          [else
           (define action-taken
             (send active-character act (state-mode s)))
           (when (and animate?
                      ;; don't animate invalid actions
                      (not (equal? action-taken 'invalid)))
             (sleep 0.5))
           action-taken]))
  (unless (equal? action-taken 'quit)
    (define new-s (state-cleanup (next-state s action-taken)))
    (when animate?
      (display-state new-s))
    (cond [(positive? (get-field current-hp (state-player new-s)))
           (game-loop new-s)] ; alive, keep going
          [else
           ;; TODO handle this in a player `die` method?
           (printf "~a has died.\nGame over.\n"
                   (send (state-player new-s) describe
                         #:capitalize? #t))])))

(module+ main
  (set-up-ui)
  (define exn #f)
  ;; whatever we do, always tear down UI
  (with-handlers ([values (lambda (e) (set! exn e))])
    (define s (init-game))
    (display-state s)
    (game-loop s))
  (void (tear-down-ui))
  (when exn (raise exn)))
