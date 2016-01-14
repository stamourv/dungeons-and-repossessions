#lang typed/racket

;; unsafe wrapper over math/array
;; the typed-untyped boundary is just too expensive
;; Note: doesn't remove boundaries entirely. there are still contracts
;;   between the expansion of, e.g., `for/array` and the `math/array`
;;   internals. I think that's the cause, at least.
;;   Still an improvement, though.

(require typed/racket/unsafe math/array)
(unsafe-provide (all-from-out math/array))
