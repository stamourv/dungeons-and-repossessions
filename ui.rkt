#lang racket

(require "grid.rkt"
         "cell.rkt"
         "character.rkt"
         "flags.rkt"
         "message-queue.rkt"
         "state.rkt"
         "wall-smoothing.rkt"
         "terminal.rkt"
         "utils.rkt")

(provide set-up-ui
         tear-down-ui
         display-state
         display-title
         full-screen-message
         choose-hero
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
  (printf "HP:  ~a / ~a\n"
          (get-field current-hp player)
          (get-field max-hp player))
  (set-cursor-position! 4 sidebar-col)
  (printf "AC:      ~a\n"
          (send player get-ac))
  (set-cursor-position! 5 sidebar-col)
  (printf "Attack:  +~a\n"
          (send player get-attack-bonus))
  (set-cursor-position! 6 sidebar-col)
  (printf "Damage:  ~a\n"
          (send player get-damage-die))
  (set-cursor-position! 8 sidebar-col)
  (displayln (show-mode s))
  ;; main display
  (cursor-home)
  (newline) ; top "margin"
  (display-grid/fov (state-grid s)
                    (get-field fov player)
                    (get-field seen player))
  (for-each displayln (drain-messages!)))

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
  (printf "\n\n    Press any key")
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

(define (full-screen-message ms)
  (clear-all)
  (for* ([m  (in-list ms)]
         [ls (in-value (string-split (break-lines m 72) "\n"))]
         [l  (in-list (if (empty? ls)
                          '("") ; if we enqueued a newline, still show a newline
                          ls))])
    (printf "    ~a\n" l))
  (press-any-key)
  (clear-all))

(define (choose-hero candidate-classes)
  (clear-all)
  (display "\n\nChoose your hero:\n\n")
  (for ([i  (in-naturals 1)]
        [c% (in-list candidate-classes)])
    (define hero (new c%))
    (printf "  ~a: ~a" i (send hero describe))
    (display "\n\n"))
  (define n (choose-number (length candidate-classes)))
  (if n
      (list-ref candidate-classes n)
      ;; did not get a number, try again
      (choose-hero candidate-classes)))

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

(define (choose-number max) ; between 1 and max, returns choice -1
  (define k (read-key))
  (cond [(and (char? k) (string->number (string k)))
         => sub1]
        [else #f]))

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
        ;; debugging commands
        [#\G (set-debug:god-mode!)   'invalid]
        [#\R (set-debug:reveal-map!) 'invalid]
        [_
         (invalid-command)])
    (restore-tty)))
