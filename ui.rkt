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

(define (display-state s)
  (clear-all)
  (displayln (state-mode s))
  (display (show-floor (state-floor s)))
  (for-each displayln (reverse (state-message-queue s)))
  (set-state-message-queue! s '()))

(define (invalid-command)
  (enqueue-message! (current-state) "Invalid command."))

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
    (define player (floor-player (state-floor s)))
    (define in (read-char))
    (begin0
        (match in
          [(app char->integer 27) ; escape, we're trying to move
           (case (which-direction?)
             [(up)    (send player move-up)    'move]
             [(down)  (send player move-down)  'move]
             [(right) (send player move-right) 'move]
             [(left)  (send player move-left)  'move]
             [else 'invalid])]
          [#\q
           'quit]
          [#\space
           'wait]
          [_
           (invalid-command)
           'invalid])
      (restore-tty))))
