#lang racket

(require "../../fmt/fmt.rkt")


(define (P m)
 (let loop ((n 1) (p 2))
  (if (= m n) p
   (loop (add1 n) (let ((p2 (sqr p))) (+ p2 (* p p2)))))))

(for ((n (in-range 1 15)))
  ((fmt 'current "i2xe24.7/") n (P n)))

(for ((n (in-range 1 15)))
  ((fmt 'current "i2xf9.7/") n (/ (log (P (add1 n))) (log (P n)))))

(for ((n (in-range 1 15)))
  ((fmt 'current "i2xf9.20/") n (/ (log (P (add1 n))) (expt 3 n))))