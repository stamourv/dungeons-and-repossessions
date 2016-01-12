#lang racket

(require math/array
         "cell.rkt" "grid.rkt" "utils.rkt"
         "dungeon.rkt") ;; TODO eventually merge in with that

(define dungeon-height 18) ; to be easy to display in 80x24, with other stuff
(define dungeon-width  60)

(define (empty-grid)
  (array->mutable-array
   (build-array (vector dungeon-height dungeon-width)
                (lambda _ (new void-cell%)))))


(struct bsp
  (height
   width
   start-pos
   children)) ; (or/c bsp? #f)
;; Note: children have one row/column of overlap, to have abutting walls

(define (show-bsp bsp)
  (define h (bsp-height bsp))
  (define w (bsp-width bsp))
  (define grid
    (array->mutable-array
     (for*/array #:shape (vector h w)
                 ([x (in-range h)]
                  [y (in-range w)])
        #f)))
  (define id 0)
  (define ids (in-string "0123456789abcdefghijklmnopqrstuvwxyz"))
  (let loop ([bsp bsp])
    (define children (bsp-children bsp))
    (cond [children
           (for-each loop children)]
          [else ; leaf, draw
           (for* ([dx (in-range (bsp-height bsp))]
                  [dy (in-range (bsp-width  bsp))])
             (define pos (down (right (bsp-start-pos bsp) dy) dx))
             (if (array-ref grid pos) ; already something there, so overlap zone
                 (array-set! grid pos " ")
                 (array-set! grid pos (sequence-ref ids id))))
           (set! id (add1 id))]))
  ;; actual display
  (with-output-to-string
    (lambda ()
      (for ([row (in-array-axis grid)])
        (for ([cell (in-array row)])
          (display cell))
        (newline)))))

(module+ test
  (require rackunit)
  (define (render-grid g) (string-join g "\n" #:after-last "\n"))

  (check-equal? (show-bsp (bsp 5 5 #(0 0) #f))
                (render-grid '("00000"
                               "00000"
                               "00000"
                               "00000"
                               "00000")))
  (check-equal? (show-bsp (bsp 5 5 #(0 0)
                               (list (bsp 5 3 #(0 0) #f)
                                     (bsp 5 3 #(0 2) #f))))
                (render-grid '("00 11"
                               "00 11"
                               "00 11"
                               "00 11"
                               "00 11")))
  (check-equal? (show-bsp (bsp 5 5 #(0 0)
                               (list (bsp 5 3 #(0 0)
                                          (list (bsp 2 3 #(0 0) #f)
                                                (bsp 4 3 #(1 0) #f)))
                                     (bsp 5 3 #(0 2) #f))))
                (render-grid '("00 22"
                               "   22"
                               "11 22"
                               "11 22"
                               "11 22")))
  (check-equal? (show-bsp (bsp 5 5 #(0 0)
                               (list (bsp 5 3 #(0 0)
                                          (list (bsp 2 3 #(0 0) #f)
                                                (bsp 4 3 #(1 0) #f)))
                                     (bsp 5 3 #(0 2)
                                          (list (bsp 3 3 #(0 2) #f)
                                                (bsp 3 3 #(2 2) #f))))))
                (render-grid '("00 22"
                               "   22"
                               "11   "
                               "11 33"
                               "11 33")))
  )


;; Notes:
;; - with 5, can have 3 rows of rooms, without which things look bad
;;   but that's pretty small rooms. maybe only allow 5 for height?
;; - problem with small values here: we end up with way too many subdivisions
;;   which would make for too many rooms, and no room for corridors
;; - with the size constraints we have, not that much randomness with bsp
;;   we get 10 or 12 rooms, in predictable-ish configuration, with some jitter
;;   not super exciting
(define min-room-dimension 7) ; TODO tweak
(define max-room-dimension 11)

;; TODO explain the one-cell overlap
(define (make-bsp [height    dungeon-height]
                  [width     dungeon-width]
                  [start-pos #(0 0)])
  (printf "make-bsp ~a ~a ~a\n" height width start-pos) ;; TODO
  ;; we can split in a given dimension if there's enough space for at two
  ;; rooms of the minimum size, minus the one cell of overlap between regions,
  ;; plus some slack
  (define min-can-split           (+ (sub1 (* 2 min-room-dimension)) 5))
  (define can-split-horizontally? (>= width  min-can-split))
  (define can-split-vertically?   (>= height min-can-split))
  ;; TODO should we always split in the largest dimension?
  (define split-horizontally? (and can-split-horizontally?
                                   (or (not can-split-vertically?)
                                       ;; (zero? (random 2))
                                       ;; TODO trying always splitting largest
                                       (>= width height)
                                       )))
  (match-define (vector start-x start-y) start-pos)
  (bsp height
       width
       start-pos
       (cond
        [split-horizontally?
         (displayln "  splitting horizontally") ;; TODO
         (define split-dy (random-between (sub1 min-room-dimension)
                                          (add1 (- width min-room-dimension))))
         ;; TODO I just *must* have made some off by 1 errors here
         (list (make-bsp height (add1 split-dy) start-pos)
               (make-bsp height (- width split-dy) (right start-pos split-dy)))]
        [can-split-vertically?
         (displayln "  splitting vertically") ;; TODO
         (define split-dx (random-between (sub1 min-room-dimension)
                                          (add1 (- height min-room-dimension))))
         (list (make-bsp (add1 split-dx) width start-pos)
               (make-bsp (- height split-dx) width (down start-pos split-dx)))]
        [else ; leaf, no children
         (displayln "  not splitting") ;; TODO
         #f])))

(display (show-bsp (make-bsp)))

;; TODO then, for each leaf in the BSP tree, cerate a room of random dimensions (until it fits, or maybe just have the max of random range be the min of the space available and max-room-dimension)
