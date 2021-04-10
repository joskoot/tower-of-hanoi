#lang racket

(require "hanoi-tools.rkt" "shortest-paths.rkt")

;---------------------------------------------------------------------------------------------------
; By Jacob J. A. Koot, April 2016
;
; The tower of Hanoi
;
; Shortest paths between two arbitrary distributions of disks.
; Brute force method using a general algorithm to find shortest paths
; in a graph with edges of length 1.

(define (find-neighbours distr)
 (define h (vector-length distr))
 (define node-a (vector-copy distr))
 (define node-b (vector-copy distr))
 (define node-c (vector-copy distr))
 (define peg0 (vector-ref distr 0))
 (vector-set! node-a 0 (modulo (+ peg0 1) 3))
 (vector-set! node-b 0 (modulo (+ peg0 2) 3))
 (let loop ((d 1))
   (if (>= d h) (list node-a node-b)
    (cond
     ((= (vector-ref node-c d) peg0)
      (loop (add1 d)))
     (else
      (define pegd (vector-ref distr d))
      (vector-set! node-c d (- 3 peg0 pegd))
      (list node-a node-b node-c))))))

(define (shortest-hanoi-paths from-distr to-distr)
 (shortest-paths from-distr to-distr find-neighbours))

;---------------------------------------------------------------------------------------------------

(define (example from-distr to-distr)
 (unless (= (vector-length from-distr) (vector-length to-distr))
  (error 'example-h "incompatible length ~s ~s" from-distr to-distr))
 (printf "Minimal length paths from ~a to ~a~n" (vector->list from-distr) (vector->list to-distr))
 (define paths (shortest-hanoi-paths from-distr to-distr))
 (define n (length paths))
 (printf "Nr of paths: ~a~n" n)
 (printf "Length = ~a~n" (sub1 (length (car paths))))
 (for ((n (in-range 1 (add1 n))) (path (in-list paths)))
  (check-path path)
  (printf "Path ~a~n" n)
  (printf "move distr~n")
  (printf "     ~a~n" (vector->list from-distr))
  (for ((distr (in-list (cdr path))) (m (in-naturals 1)))
   (printf "~a ~a~n" (pad m 4) (vector->list distr)))
  (printf "end of path~n~n")))

(define (check-path path)
 (define p (map vector->list path))
 (or (null? p)
  (for ((a (in-list p)) (b (in-list (cdr p)))) (check-move a b))))

(define (check-move new-distr distr)
 (define (move-error) (error 'hanoi-h "illegal move from ~a to ~a" distr new-distr))
 (let loop ((new-distr new-distr) (distr distr) (not-allowed (set)))
  (cond
   ((null? distr)
    (move-error))
   ((= (car new-distr) (car distr))
    (loop (cdr new-distr) (cdr distr) (set-add not-allowed (car distr))))
   ((and
     (not (set-member? not-allowed (car new-distr)))
     (not (set-member? not-allowed (car distr)))
     (equal? (cdr new-distr) (cdr distr))))
   (else (move-error)))))

(print-example-header 'hanoi-i
 "Shortest paths between two arbitrary distributions of disks.")

(example #(0) #(0))
(example #(0) #(1))
(example #(0 0 0 0) #(1 1 1 1))
(example #(0 0 1 2) #(0 0 2 1))
(example #(0 0 0 0) #(1 1 1 0))
(example #(0 1 2 0 1 2) #(2 1 0 2 1 0))
