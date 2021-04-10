#lang racket

(require "hanoi-tools.rkt") ; odd h: disk 0 f t r; even h: disk 0 f r t.

;---------------------------------------------------------------------------------------------------
; By Jacob J. A. Koot, April 2016
;
; The tower of Hanoi
;
; Shortest path of moving a tower from one peg to another one,
; using the fact that for every odd move number the smallest disk has to be moved.
;
; h : nr of disks.
; f : from peg.
; t : onto peg.

; The disks are identified in order of increasing size
; by the natural numbers 0 up to but not including h.
; The pegs are identified by 0, 1 and 2.
;
; Let r be the third peg.
; If h is odd, the smallest disk moves from f to t to r etc.
; If h is even, the smallest disks moves from f to r to t etc.
; If the move-number is even, there is only one choise for one of the larger disks.
; In fact disk d moves from f to t to r etc if (- h d) is odd and
; from f to r to t etc if (- h d) is even.
; In the next algorithm this fact is used for disk 0 only.
; For every other disk there is always one legal move.

(define (move-tower h f t)
 (define disk-0-increment (if (odd? h) (- t f) (- f t)))
 (define final-distr (make-vector h t))
 (define distr (make-vector h f))
 (define (move m)
  (cond
   ((equal? distr final-distr) (void))
   ((odd? m)
    (define f (vector-ref distr 0))
    (define t (modulo (+ f disk-0-increment) 3))
    (move-disk 0 f t)
    (vector-set! distr 0 t)
    (move (add1 m)))
   (else
    (define peg-0 (vector-ref distr 0))
    ; Look for the smallest disk not on the same peg as disk 0.
    (define (find-peg d)
     (define f (vector-ref distr d))
     (cond
      ((= f peg-0) (find-peg (add1 d)))
      (else
       (define t (- 3 peg-0 f))
       (vector-set! distr d t)
       (move-disk d f t)
       (move (add1 m)))))
    (find-peg 1))))
 (move 1))

;---------------------------------------------------------------------------------------------------
; We use procedures move-disk and print-path of module hanoi-tools.rkt.
; Procedure move-disk keeps a record of all moves. It also checks each move to be legal.
; Procedure print-path uses this record to print results.
; Procedure initialize-path initializes the bookkeeping for procedure move-disk.

(define (example h f t)
 (printf "Shortest path of tower of ~a disks from peg ~a to peg ~a.~n" h f t)
 (initialize-path h f)
 (move-tower h f t)
 (print-path t))

(print-example-header 'hanoi-e
 "Shortest path to move a tower from one peg to another one~n~
  using the fact that every odd move is made with disk 0.")

(for ((h (in-range 1 7))) (example h 0 1) (example h 0 2))
