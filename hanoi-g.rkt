#lang racket

(require "hanoi-tools.rkt")

;---------------------------------------------------------------------------------------------------
; By Jacob J. A. Koot, April 2016
;
; The tower of Hanoi
;
; Computation of move m and the resulting distribution of disks
; for the longest non self-crossing path of moving a tower of h disks from peg f to peg t.
;
; h is the heigth of the tower.
; f is the starting peg.
; t is the destination peg.
; m is the move-counter, the first move having m=1.
; d is a disk (in-range 0 h).

; The pegs are identified by 0, 1 and 2.

; (mcnt m   d    ) is the number of times disk d has been moved after m moves.
; (disk m        ) is the disk being moved during move m. Nr of times m can be divided by 3.
; (from m h   f t) is the peg a disk is taken from during move m.
; (onto m h   f t) is the peg a disk is put onto during move m.
; (thrd m     f t) is the peg not involved during move m.
; (posi m h d f t) is the position of disk d after m moves.

(define (exp3 n      ) (expt   3 n))
(define (mod3 n      ) (modulo n 3))
(define (mod4 n      ) (modulo n 4))
(define (thrd m   f t) (if (odd? m) t f))
(define (onto m h f t) (posi m (disk m) f t))
(define (from m h f t) (- 3 (onto m h f t) (thrd m f t)))
(define (disk m) (if (zero? (mod3 m)) (add1 (disk (quotient m 3))) 0))

(define (posi m d f t)
 (case (mod4 (mcnt m d))
  ((0) f)
  ((1 3) (- 3 f t))
  ((2) t)))

(define (mcnt m d)
 (+
  (* 2 (quotient m (exp3 (add1 d))))
  (mod3 (quotient m (exp3 d)))))

;---------------------------------------------------------------------------------------------------

(define (example h f t)
 (printf "Longest non self-crossing path. h=~a f=~a t=~a.~n" h f t)
 (print-header (make-list h f))
 (for ((m (in-range 1 (expt 3 h))))
  (print-move m (disk m) (from m h f t) (onto m h f t)
   (for/list ((d (in-range 0 h))) (posi m d f t))))
 (printf "End-of-path~n~n"))

(print-example-header 'hanoi-g
 "Longest non self-crossing path from one peg to another one~n~
  computed move by move without recursion.")

(for ((h (in-range 1 5))) (example h 0 1) (example h 0 2) (example h 1 2))

(define (format-distr distr)
 (list->string (map (λ (p) (vector-ref #(#\0 #\1 #\2) p)) distr)))

(let ((N-avagadro #e6.022140857e23) (h 80) (f 0) (t 1))
 (printf "~nMove ~a from ~a to ~a with ~a disks~n" N-avagadro f t h)
  (let-values
   (((d f t r distr)
     (time
      (values
       (disk N-avagadro)
       (from N-avagadro h 0 1)
       (onto N-avagadro h f t)
       (thrd N-avagadro f t)
       (for/list ((d (in-range 0 h))) (posi N-avagadro d f t))))))
   (printf "disk from onto thrd~n")
   (printf "~a ~a ~a ~a~n~a~n" (pad d 4) (pad f 4) (pad t 4) (pad r 4) (format-distr distr))
   (newline)))
