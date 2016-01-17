#lang racket

(require "grid.rkt" "cell.rkt")

(provide compute-fov
         show-grid/fov)

(define debug-fov #f)

;; ray casting FOV
;; based on: https://blog.jverkamp.com/2013/05/17/racket-roguelike-7-into-darkness/
;; improved to keep track of octants (otherwise, we can end up seeing behind
;; walls in some cases)
(define (compute-fov grid pos [range 5])
  (define fov (mutable-set))
  (define (spread-light pos dir dir2 ttl) ; dir2 determines octant
    (set-add! fov pos)
    (when debug-fov
      (printf "spreading to ~a ~a ~a\n" pos dir ttl)
      (define cp (mutable-array-copy grid))
      (apply-fov! cp fov)
      (displayln (show-grid cp)))
    (unless (or (<= ttl 0) ; out of range
                (send (grid-ref grid pos) opaque?)) ; light can't spread past
      (spread-light (dir pos)        dir dir2 (- ttl 1))
      (spread-light (dir2 (dir pos)) dir dir2 (- ttl (sqrt 2)))))
  (spread-light pos up    left  range)
  (spread-light pos up    right range)
  (spread-light pos down  left  range)
  (spread-light pos down  right range)
  (spread-light pos left  up    range)
  (spread-light pos left  down  range)
  (spread-light pos right up    range)
  (spread-light pos right down  range)
  fov)


(define (apply-fov! grid fov) ; for testing
  (for ([pos (in-set fov)])
    (array-set! grid pos (new pillar%)))) ; arbitrary marker


(module+ test

  (require rackunit
           "cell.rkt")

  (define debug-fov-tests #t) ;; TODO

  (define (render-grid g) (string-join g "\n" #:after-last "\n"))
  (define (check-fov m pos range res)
    (define g (parse-grid m))
    (apply-fov! g (compute-fov g pos range))
    (array-set! g pos (new empty-cell%))
    (when debug-fov-tests
      (displayln (show-grid g)))
    (check-equal? (show-grid g) (render-grid res)))

  (define m1 '("            "
               " XXXXXXXXXX "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " XXXXXXXXXX "
               "            "))

  (define r1 '("            "
               " #######XXX "
               " # #####  X "
               " #######  X "
               " #######  X "
               " #######  X "
               " ######   X "
               " #####    X "
               " X        X "
               " X        X "
               " XXXXXXXXXX "
               "            "))
  (check-fov m1 #(2 2) 5 r1)

  (define r2 '("            "
               " XXXXXXXXXX "
               " X        X "
               " X        X "
               " X  ##### X "
               " X  ##### X "
               " X  ## ## X "
               " X  ##### X "
               " X  ##### X "
               " X        X "
               " XXXXXXXXXX "
               "            "))
  (check-fov m1 #(6 6) 2 r2)

  (define r3 '("            "
               " XXXX###### "
               " X   ### ## "
               " X   ###### "
               " X   ###### "
               " X   ###### "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " XXXXXXXXXX "
               "            "))
  (check-fov m1 #(2 8) 3 r3)

  (define m2 '("            "
               " XXXXXXXXXX "
               " X X      X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " XXXXXXXXXX "
               "            "))
  (define r4 '("            " ;; TODO too much corner peeking
               " ###XXXXXXX "
               " # #      X "
               " ######   X "
               " ######   X "
               " ######   X "
               " #####    X "
               " X        X "
               " X        X "
               " X        X "
               " XXXXXXXXXX "
               "            "))
  (check-fov m2 #(2 2) 4 r4)

  (define m3 '("            "
               " XXXXXXXXXX "
               " X X      X "
               " X X      X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " XXXXXXXXXX "
               "            "))
  (define r5 '("            "
               " ###XXXXXXX "
               " # #      X "
               " ###      X "
               " ###      X "
               " ####     X "
               " #####    X "
               " X        X "
               " X        X "
               " X        X "
               " XXXXXXXXXX "
               "            "))
  (check-fov m3 #(2 2) 4 r5)

  (define m4 '("            "
               " XXXXXXXXXX "
               " X        X "
               " X X      X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " XXXXXXXXXX "
               "            "))
  (define r6 '("            " ;; TODO not great. would hope to obscure more
               " ######XXXX "
               " # ####   X "
               " ######   X "
               " ### ##   X "
               " #### #   X "
               " #####    X "
               " X        X "
               " X        X "
               " X        X "
               " XXXXXXXXXX "
               "            "))
  (check-fov m4 #(2 2) 4 r6)

  (define r7 '("            " ;; TODO ditto
               " ######XXXX "
               " ######   X "
               " # #      X "
               " ######   X "
               " ######   X "
               " ######   X "
               " #####    X "
               " X        X "
               " X        X "
               " XXXXXXXXXX "
               "            "))
  (check-fov m4 #(3 2) 4 r7)

  (define r8 '("            " ;; TODO ditto
               " ####X#XXXX "
               " ### ##   X "
               " ######   X "
               " # ####   X "
               " ######   X "
               " ######   X "
               " ######   X "
               " #####    X "
               " X        X "
               " XXXXXXXXXX "
               "            "))
  (check-fov m4 #(4 2) 4 r8)

  (define m5 '("            "
               " XXXXXXXXXX "
               " X X      X "
               " X X      X "
               " X X      X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " XXXXXXXXXX "
               "            "))
  (define r9 '("            "
               " ###XXXXXXX "
               " # #      X "
               " ###      X "
               " ###      X "
               " ###      X "
               " ####     X "
               " X        X "
               " X        X "
               " X        X "
               " XXXXXXXXXX "
               "            "))
  (check-fov m5 #(2 2) 4 r9)

  ;; pillar further away
  (define m6 '("            "
               " XXXXXXXXXX "
               " X        X "
               " X  X     X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " X        X "
               " XXXXXXXXXX "
               "            "))
  (define r10 '("            " ; TODO worthless
                " ######XXXX "
                " # ####   X "
                " ######   X "
                " ######   X "
                " ######   X "
                " #####    X "
                " X        X "
                " X        X "
                " X        X "
                " XXXXXXXXXX "
                "            "))
  (check-fov m6 #(2 2) 4 r10)

  (define r11 '("            " ; TODO not great
                " ######XXXX "
                " ######   X "
                " # ##     X "
                " ######   X "
                " ######   X "
                " ######   X "
                " #####    X "
                " X        X "
                " X        X "
                " XXXXXXXXXX "
                "            "))
  (check-fov m6 #(3 2) 4 r11)

  (define r12 '("            " ; TODO worthless
                " ######XXXX "
                " ######   X "
                " ######   X "
                " # ####   X "
                " ######   X "
                " ######   X "
                " ######   X "
                " #####    X "
                " X        X "
                " XXXXXXXXXX "
                "            "))
  (check-fov m6 #(4 2) 4 m6)
  )

(define (show-grid/fov grid fov seen)
  (with-output-to-string
    (lambda ()
      (for ([x (in-range (grid-height grid))])
        (for ([y (in-range (grid-width grid))])
          ;; TODO light up FOV more brightly
          (define pos (vector x y))
          (display (if (set-member? seen pos)
                       (send (grid-ref grid pos) show)
                       "?"))) ;; TODO for testing
        (newline)))))
