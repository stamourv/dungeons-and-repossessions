#lang racket

(provide (all-defined-out))

(define (terminal-command command)
  (printf "~a~a" (integer->char #x1b) command))

(define (terminal-reset) (terminal-command "[0m"))

(define (terminal-colors bg fg [bold? #f] [underline? #f])
  (terminal-command
   (format "[~a;~a~a~am"
           (case bg
             ((black) "40") ((red)     "41")
             ((green) "42") ((yellow)  "43")
             ((blue)  "44") ((magenta) "45")
             ((cyan)  "46") ((white)   "47"))
           (case fg
             ((black) "30") ((red)     "31")
             ((green) "32") ((yellow)  "33")
             ((blue)  "34") ((magenta) "35")
             ((cyan)  "36") ((white)   "37"))
           (if bold?      ";1" "")
           (if underline? ";4" ""))))

;; bright is bold, dim is regular weight, blink, reverse and hidden don't work
(define (terminal-print text #:bg (bg 'black) #:fg (fg 'white)
                        #:bold? (bold? #f) #:underline? (underline? #f))
  (terminal-colors bg fg bold? underline?)
  (display text)
  (terminal-reset))

(define (clear-line)      (terminal-command "[K"))
(define (clear-to-bottom) (terminal-command "[J"))
(define (cursor-home)     (terminal-command "[H"))
(define (clear-all)       (cursor-home) (clear-to-bottom))

(define (set-cursor-position! x (y #f))
  (terminal-command (format "[~a~aH" x (if y (format ";~a" y) ""))))

(define (cursor-on)  (system "tput cnorm"))
(define (cursor-off) (system "tput civis"))

(define tty-intercepted? #f)
(define (intercept-tty)
  (unless tty-intercepted?
    (system "stty raw -echo opost")
    (set! tty-intercepted? #t)))
(define (restore-tty)
  (when tty-intercepted?
    (system "stty cooked echo")
    (set! tty-intercepted? #f)))

(define (echo-on)  (system "stty echo"))
(define (echo-off) (system "stty -echo"))
