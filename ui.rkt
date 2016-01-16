#lang racket

(require "character.rkt"
         "grid.rkt"
         "message-queue.rkt"
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

(define sidebar-col 63)

(define (display-state s)
  (clear-all)
  ;; sidebar
  (set-cursor-position! 2 sidebar-col)
  (displayln (show-mode s))
  (set-cursor-position! 3 sidebar-col)
  (printf "~a / ~a HP\n"
          (get-field current-hp (state-player s))
          (get-field max-hp (state-player s)))
  ;; main display
  (cursor-home)
  (newline) ; top "margin"
  (display (show-grid (state-grid s)))
  (for-each displayln (reverse message-queue))
  (reset-message-queue!))

(define (invalid-command)
  (enqueue-message! "Invalid command.")
  'invalid)

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

(define (choose-direction)
  (if (= (char->integer (read-char)) 27) ; escape
      (case (which-direction?)
        ((up)    up)
        ((down)  down)
        ((left)  left)
        ((right) right))
      #f))

(define (direction-command s name f)
  (printf "~a in which direction?\n" name)
  (cond [(choose-direction) =>
         (lambda (dir)
           (cond [(grid-ref (state-grid s)
                            (dir (get-field pos (state-player s))))
                  => f]
                 [else (invalid-command)]))]
        [else
         (invalid-command)]))

;; game-state -> kind-of-action-performed
(define (handle-input s)
  (intercept-tty)
  (define player (state-player s))
  (define mode   (state-mode   s)) ; to check move validity
  (define in (read-char))
  (begin0
      (match in
        [(app char->integer 27) ; escape, we're trying to move
         (case (which-direction?)
           [(up)    (send player move-up    mode)]
           [(down)  (send player move-down  mode)]
           [(right) (send player move-right mode)]
           [(left)  (send player move-left  mode)]
           [else 'invalid])]
        [#\o ; open
         (direction-command s "Open" (lambda (x) (send x open) 'move))]
        [#\c ; close
         (direction-command s "Close" (lambda (x) (send x close) 'move))]
        [#\s ; suicide
         (set-field! current-hp player 0)
         'invalid]
        [#\q
         'quit]
        [#\space
         'wait]
        [_
         (invalid-command)])
    (restore-tty)))
