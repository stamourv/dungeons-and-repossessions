#lang racket

(require "floor.rkt"
         "character.rkt"
         "state.rkt"
         "terminal.rkt")

(provide set-up-ui
         tear-down-ui
         state
         display-state
         handle-input)

(define (set-up-ui)
  (cursor-off)
  (echo-off))
(define (tear-down-ui)
  (cursor-on)
  (echo-on))

;; TODO flickers, not sure why. old roguelike does not, see what's different
(define (display-state s)
  (clear-all)
  (displayln (state-mode s))
  (printf "~a HP\n" (get-field current-hp (state-player s)))
  (display (show-floor (state-floor s)))
  (for-each displayln (reverse (state-message-queue s)))
  (set-state-message-queue! s '()))

(define (invalid-command)
  (enqueue-message! "Invalid command."))

(define (which-direction?)
  (define char (read-char))
  (when (not (or (eq? char #\[)
                 (eq? char #\O))) ; emacs's ansi-term uses this
    (invalid-command))
  (case (read-char)
    ((#\A) 'up)
    ((#\B) 'down)
    ((#\C) 'right)
    ((#\D) 'left)
    (else  (invalid-command))))

;; game-state -> kind-of-action-performed
(define (handle-input s)
  (parameterize ([current-state s])
    (intercept-tty)
    (define player (state-player s))
    (define in (read-char))
    (begin0
        (match in
          [(app char->integer 27) ; escape, we're trying to move
           (case (which-direction?)
             [(up)    (send player move-up)]
             [(down)  (send player move-down)]
             [(right) (send player move-right)]
             [(left)  (send player move-left)]
             [else 'invalid])]
          [#\q
           'quit]
          [#\space
           'wait]
          [_
           (invalid-command)
           'invalid])
      (restore-tty))))
