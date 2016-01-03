#lang racket

(require "floor.rkt"
         "player.rkt"
         "terminal.rkt")

(provide set-up-ui
         tear-down-ui
         display-game-state
         handle-input)

(define (set-up-ui)
  (cursor-off)
  (echo-off))
(define (tear-down-ui)
  (cursor-on)
  (echo-on))

;; A game state is a Floor

(define (display-game-state f)
  (clear-all)
  (display (show-floor f)))

(define (invalid-command) (display "Invalid command.\n"))

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
(define (handle-input f)
  (intercept-tty)
  (define player (floor-player f))
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
        [_
         (invalid-command)
         'invalid])
    (restore-tty)))
