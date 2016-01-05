#lang racket

(require "character.rkt"
         "grid.rkt"
         "message-queue.rkt"
         "state.rkt"
         "terminal.rkt"
         math/array) ;; TODO for testing

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
  (display (show-grid (state-grid s)))
  (for-each displayln (reverse message-queue))
  (reset-message-queue!))

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
         (for ([c (in-array (state-grid s))]) ;; TODO for testing
           (send c open))
         'move]
        [#\c ; close
         (for ([c (in-array (state-grid s))]) ;; TODO for testing
           (send c close))
         'move]
        [#\s ; suicide
         (set-field! current-hp player 0)
         'invalid]
        [#\q
         'quit]
        [#\space
         'wait]
        [_
         (invalid-command)
         'invalid])
    (restore-tty)))
