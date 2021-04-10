#lang racket

(require "hanoi-tools.rkt")

;---------------------------------------------------------------------------------------------------
; By Jacob J. A. Koot, April 2016
;
; The tower of Hanoi
;
; Simple recursive procedure for the shortest path
; of moving a all disks from an arbitrary distribution of disks to one peg.
;
; h     : nr of disks.
; distr : starting distribution of disks.
; t     : destination peg.
;
; A distr is a vector whose d-th element shows the position of disk d.
; The disks are identified in order of increasing size
; by the natural numbers 0 up to but not including h.
; The pegs are identified by 0, 1 and 2.

(define (path h distr t)
 (unless (zero? h)
  (define h-1 (sub1 h))
  (define f (vector-ref distr h-1))
  (cond
   ((= f t) (path h-1 distr t))
   (else
    (define r (- 3 f t))
    (path h-1 distr r)
    (move-disk     h-1 f t)
    (shortest-path h-1 r t)))))

(define (shortest-path h f t)
 (unless (zero? h)
  (define h-1 (sub1 h))
  (define r (- 3 f t))
  (shortest-path h-1 f r)
  (move-disk     h-1 f t)
  (shortest-path h-1 r t)))
        
;---------------------------------------------------------------------------------------------------
; We use procedures move-disk and print-path of module hanoi-tools.rkt.
; Procedure move-disk keeps a record of all moves. It also checks each move to be legal.
; Procedure print-path uses this record to print results.
; Procedure initialize-path initializes the bookkeeping for procedure move-disk.

(define (example distr t)
 (printf "Shortest path from distr ~a to peg ~a.~n" (vector->list distr) t)
 (initialize-path distr)
 (path (vector-length distr) distr t)
 (print-path t))

; Procedure (list-distrs h) produces a list of all distibutions of h disks. 

(define (list-distrs h)
 (map list->vector
  (let loop ((h h))
   (if (zero? h) '(())
    (for*/list ((distr (in-list (loop (sub1 h)))) (p (in-range 0 3))) 
     (cons p distr))))))

(print-example-header 'hanoi-d
 "Shortest path to move all disks to one peg~n~
  starting from arbitrary distr.")

(for* ((n (in-range 1 5)) (distr (in-list (list-distrs n))))
 (example distr 0))
