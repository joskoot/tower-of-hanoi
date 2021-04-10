#lang racket

(require "hanoi-tools.rkt" "shortest-paths.rkt")

;---------------------------------------------------------------------------------------------------
; By Jacob J. A. Koot, April 2016
;
; The tower of Hanoi
;
; Shortest path with 3 or more pegs from one distr to another one.
; Brute force method using a general algorithm to find shortest paths
; in a graph with edges of length 1.

(define (make-G nr-of-pegs)
 (lambda (distr)
  (define smallest-disk-on-peg (make-vector nr-of-pegs #f))
  (for ((peg (in-vector distr)) (d (in-naturals)))
   (unless (vector-ref smallest-disk-on-peg peg)
    (vector-set! smallest-disk-on-peg peg d)))
  (filter identity
   (for*/list ((from-peg (in-range nr-of-pegs)) (onto-peg (in-range nr-of-pegs)))
    (define from (vector-ref smallest-disk-on-peg from-peg))
    (define onto (vector-ref smallest-disk-on-peg onto-peg))
    (cond
     ((not from) #f)
     ((or (not onto) (< from onto))
      (define new-distr (vector-copy distr))
      (vector-set! new-distr from onto-peg)
      new-distr)
     (else #f))))))

;---------------------------------------------------------------------------------------------------

(define (example start dest nr-of-pegs (print? #t))
 (define G (make-G nr-of-pegs))
 (printf "Shortest paths from ~a to ~a.~n~
          Nr of pegs: ~s~n" (vector->list start) (vector->list dest) nr-of-pegs)
 (define paths (shortest-paths start dest G))
 (define nr-of-paths (length paths))
 (for ((path (in-list paths))) (check-path path))
 (printf "Nr of paths: ~s~n" nr-of-paths)
 (when print?
  (unless (zero? nr-of-paths) (printf "Length of each path: ~s~n" (sub1 (length (car paths)))))
  (for ((path (in-list paths)) (k (in-naturals 1)))
   (printf "~nPath ~s~n" k)
   (for ((distr (in-list path)) (k (in-naturals 0)))
    (printf "~a: ~a~n" (pad k 3) (vector->list distr)))))
 (newline))

(define (check-path path)
 (define p (map vector->list path))
 (unless (null? p)
  (for ((distr (in-list p)) (new-distr (in-list (cdr p))))
   (check-move distr new-distr))))

(define (check-move distr new-distr)
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

(print-example-header 'hanoi-j
 "Shortest path between arbitrary distributions of disks~n~
  with 3 or more pegs.~n~
  Brute force using a simple procedure finding all paths.")

(example #(0) #(1) 2)
(example #(0 0) #(1 1) 2)
(example #(0 0) #(1 1) 3)
(example #(1 1 0) #(1 0 1) 3)
(example #(1 1 0) #(1 0 1) 4)
(example #(0 0 0) #(1 1 1) 3)
(example #(0 0 0) #(1 1 1) 4)
(example #(0 1 2) #(2 1 0) 3)
(example #(0 1 2) #(2 1 0) 4)
(example #(0 1 2) #(2 1 0) 5)
(example #(0 1 2 0 1 2) #(2 1 0 2 1 0) 3)
(example #(0 1 2 0 1 2) #(2 1 0 2 1 0) 4)
(example #(0 1 2 0 1 2) #(2 1 0 2 1 0) 5)
(example #(1 3 2 1 0) #(0 1 2 3 1) 4)
(example #(4 3 2 1 0) #(0 1 2 3 4) 5)
(example #(4 3 2 1 4) #(0 1 2 3 4) 5)
(example #(0 0 0 0 0) #(1 1 1 1 1) 6)
(example #(0 0 0 0 0 0 0) #(1 1 1 1 1 1 1) 4 #f)
