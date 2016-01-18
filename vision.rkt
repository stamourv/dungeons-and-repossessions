#lang racket

(require "grid.rkt" "cell.rkt")

(provide compute-fov)

(define debug-fov #f)

;; shadow casting
;; based on: http://www.roguebasin.com/index.php?title=Improved_Shadowcasting_in_Java
;; algorithm: http://www.roguebasin.com/index.php?title=FOV_using_recursive_shadowcasting
(define (compute-fov grid pos radius)
  (match-define (vector start-x start-y) pos)
  (define fov (mutable-set pos))
  ;; xx, xy and co specify the octant
  (define (cast-light row-no start-slope end-slope xx xy yx yy)
    (define new-start-slope 0.0)
    (define blocked? #f)
    (unless (< start-slope end-slope) ; ...in which case we're done
      (for ([row-no (in-range row-no (add1 radius))]
            #:break blocked?)
        (define dy (- row-no))
        (let/ec break
          (for ([dx (in-range (- row-no) 1)])
            (define pos (vector (+ start-x (* dx xx) (* dy xy))
                                (+ start-y (* dx yx) (* dy yy))))
            (define left-slope  (/ (- dx 0.5) (+ dy 0.5)))
            (define right-slope (/ (+ dx 0.5) (- dy 0.5)))
            (unless (or (not (within-grid? grid pos)) ; out of bounds
                        (< start-slope right-slope)) ; not yet in octant, skip
              (when (> end-slope left-slope) ; out of the octant, do next row
                (break))
              ;; if within radius, light up
              (when (< (sqrt (+ (sqr dx) (sqr dy))) (add1 radius))
                (set-add! fov pos))
              (cond [blocked? ; previous cell was a blocking one
                     (cond [(send (grid-ref grid pos) opaque?) ; still on a wall
                            (set! new-start-slope right-slope)] ; keep skipping
                           [else
                            (set! blocked? #f) ; on clear ground again
                            (set! start-slope new-start-slope)])]
                    [else
                     (when (and (send (grid-ref grid pos) opaque?)
                                (< row-no radius))
                       ;; hit a wall within range, recur on a smaller slice
                       (set! blocked? #t)
                       (cast-light (add1 row-no)
                                   start-slope left-slope
                                   xx xy yx yy)
                       ;; resume after the obstacle
                       (set! new-start-slope right-slope))])))))))
  ;; for each octant, do shadow casting
  (for ([d (in-list (cartesian-product '(1 -1) '(1 -1)))])
    (match-define (list dx dy) d)
    (cast-light 1 1.0 0.0 0  dx dy 0)
    (cast-light 1 1.0 0.0 dx 0  0  dy))
  fov)


(define (apply-fov! grid fov) ; for testing
  (for ([pos (in-set fov)])
    (array-set! grid pos (new pillar%)))) ; arbitrary marker


(module+ test

  (require rackunit
           "cell.rkt")

  (define debug-fov-tests #f)

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
               " X    ##### "
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
  (define r4 '("            "
               " ###XXXXXXX "
               " # #      X "
               " ####     X "
               " #####    X "
               " #####    X "
               " ####     X "
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
               " ###      X "
               " ####     X "
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
  (define r6 '("            "
               " ######XXXX "
               " # ####   X "
               " ######   X "
               " ###  #   X "
               " ###      X "
               " ####     X "
               " X        X "
               " X        X "
               " X        X "
               " XXXXXXXXXX "
               "            "))
  (check-fov m4 #(2 2) 4 r6)

  (define r7 '("            "
               " #####XXXXX "
               " ####     X "
               " # #      X "
               " ####     X "
               " #####    X "
               " #####    X "
               " ####     X "
               " X        X "
               " X        X "
               " XXXXXXXXXX "
               "            "))
  (check-fov m4 #(3 2) 4 r7)

  (define r8 '("            "
               " ###XXXXXXX "
               " ###  #   X "
               " ######   X "
               " # ####   X "
               " ######   X "
               " ######   X "
               " #####    X "
               " ####     X "
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
               " ###      X "
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
  (define r10 '("            " ; not ideal
                " ######XXXX "
                " # ####   X "
                " ######   X "
                " #####    X "
                " #####    X "
                " ####     X "
                " X        X "
                " X        X "
                " X        X "
                " XXXXXXXXXX "
                "            "))
  (check-fov m6 #(2 2) 4 r10)

  (define r11 '("            "
                " ######XXXX "
                " ######   X "
                " # ##     X "
                " ######   X "
                " ######   X "
                " #####    X "
                " ####     X "
                " X        X "
                " X        X "
                " XXXXXXXXXX "
                "            "))
  (check-fov m6 #(3 2) 4 r11)

  (define r12 '("            " ; not ideal
                " #####XXXXX "
                " #####    X "
                " ######   X "
                " # ####   X "
                " ######   X "
                " ######   X "
                " #####    X "
                " ####     X "
                " X        X "
                " XXXXXXXXXX "
                "            "))
  (check-fov m6 #(4 2) 4 r12)
  )
