#lang racket

(require "grid.rkt"
         "cell.rkt"
         "message-queue.rkt"
         "state.rkt"
         "wall-smoothing.rkt"
         "terminal.rkt"
         "utils.rkt")

(provide set-up-ui
         tear-down-ui
         display-state
         display-title
         display-briefing
         read-key
         handle-input)

(define (set-up-ui)
  (cursor-off)
  (echo-off))
(define (tear-down-ui)
  (echo-on)
  (cursor-on))

(define sidebar-col 63)

(define (display-state s)
  (define player (state-player s))
  (clear-all)
  ;; sidebar
  (set-cursor-position! 2 sidebar-col)
  (displayln (show-mode s))
  (set-cursor-position! 4 sidebar-col)
  (printf "level ~a\n" (get-field level player))
  (set-cursor-position! 5 sidebar-col)
  (printf "~a / ~a HP\n"
          (get-field current-hp player)
          (get-field max-hp player))
  ;; main display
  (cursor-home)
  (newline) ; top "margin"
  (display-grid/fov (state-grid s)
                    (get-field fov player)
                    (get-field seen player))
  (for-each displayln (reverse message-queue))
  (reset-message-queue!))

(define (display-grid/fov grid fov seen)
  ;; to avoid leaking info about what's on the other side of a wall,
  ;; only connect walls (into tees or four-corners) based on what we've
  ;; seen so far
  (define smoothing-grid
    (build-array (array-shape grid) (lambda _ (new void-cell%))))
  (for ([pos (in-set seen)])
    (array-set! smoothing-grid pos (array-ref grid pos)))
  (smooth-walls smoothing-grid #:only-info-leaks? #t)
  ;; do the actual printing
  (for ([x (in-range (grid-height grid))])
    (for ([y (in-range (grid-width grid))])
      (define pos   (vector x y))
      (define cell (grid-ref smoothing-grid pos))
      (cond [(and (set-member? fov pos)
                  (or (is-a? cell pillar%) ; light pillars up
                      (not (send cell opaque?)))) ; but not other walls
             (terminal-print (send cell show) #:fg 'black #:bg 'white)]
            [(set-member? seen pos)
             (display (send cell show/fog))] ; don't show occupant
            [else
             (display " ")]))
    (newline)))

(define (press-any-key)
  (printf "\n\n    Press any key to continue")
  (read-key))

;; see network-science.de/ascii/ font: big
(define (display-title)
  (clear-all)
  (newline) (newline)
  (displayln #<<END
     _____                                                             _
    |  __ \                                                           | |
    | |  | |_   _ _ __   __ _  ___  ___  _ __  ___      __ _ _ __   __| |
    | |  | | | | | '_ \ / _` |/ _ \/ _ \| '_ \/ __|    / _` | '_ \ / _` |
    | |__| | |_| | | | | (_| |  __/ (_) | | | \__ \   | (_| | | | | (_| |
    |_____/ \__,_|_| |_|\__, |\___|\___/|_| |_|___/    \__,_|_| |_|\__,_|
                         __/ |
                        |___/
     _____                                        _
    |  __ \                                      (_)
    | |__) |___ _ __   ___  ___ ___  ___  ___ ___ _  ___  _ __  ___
    |  _  // _ \ '_ \ / _ \/ __/ __|/ _ \/ __/ __| |/ _ \| '_ \/ __|
    | | \ \  __/ |_) | (_) \__ \__ \  __/\__ \__ \ | (_) | | | \__ \
    |_|  \_\___| .__/ \___/|___/___/\___||___/___/_|\___/|_| |_|___/
               | |
               |_|
END
)
    (newline)
    (press-any-key))




;; show mission briefing before entering a dungeon
(define (display-briefing)
  (clear-all)
  (for* ([m  (in-list (reverse briefing-queue))]
         [ls (in-value (string-split (break-lines m 72) "\n"))]
         [l  (in-list (if (empty? ls)
                          '("") ; if we enqueued a newline, still show a newline
                          ls))])
    (printf "    ~a\n" l))
  (reset-briefing-queue!)
  (press-any-key))


(define (read-key)
  (intercept-tty)
  (begin0 (-read-key)
    (restore-tty)))

(define (-read-key) ; reads a "whole" key (incl. escape sequences)
  (define char (read-char))
  (if (= (char->integer char) 27) ; escape
      (which-direction?)
      char))

(define (invalid-command)
  (enqueue-message! "Invalid command.")
  'invalid)

(define (which-direction?)
  (define char (read-char))
  (when (not (or (eq? char #\[)
                 (eq? char #\O))) ; emacs's ansi-term uses this
    (invalid-command))
  (case (read-char)
    [(#\A) 'up]
    [(#\B) 'down]
    [(#\C) 'right]
    [(#\D) 'left]
    [else  #f]))

(define (choose-direction)
  (case (-read-key)
    [(up)    up]
    [(down)  down]
    [(left)  left]
    [(right) right]
    [else    #f]))

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
           [else    (invalid-command)])]
        [#\o (direction-command s "Open" (lambda (x) (send x open) 'move))]
        [#\c (direction-command s "Close" (lambda (x) (send x close) 'move))]
        [#\p (send player pick-up)]
        [#\i (send player show-inventory)]
        [#\s ; suicide
         (set-field! current-hp player 0)
         'invalid]
        [#\q ; quit
         (printf "Do you really want to quit? (y/n)\n")
         (define key (-read-key))
         (cond [(equal? key #\y)
                'quit]
               [else
                (enqueue-message! "Alright then.")
                'invalid])]
        [#\space
         'wait]
        [_
         (invalid-command)])
    (restore-tty)))
