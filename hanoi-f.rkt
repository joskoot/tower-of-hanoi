#lang racket

(require "hanoi-tools.rkt")

;---------------------------------------------------------------------------------------------------
; By Jacob J. A. Koot, April 2016
;
; The tower of Hanoi
;
; Computation of move m and the resulting distribution of disks
; for the path of moving a tower of h disks from peg f to peg t.
;
; h is the heigth of the tower.
; f is the starting peg.
; t is the destination peg.
; m is the move-counter, the first move having m=1.
; d is a disk (in-range 0 h).
;
; The pegs are identified by 0, 1 and 2.
;
; (mcnt m   d    ) is the number of times disk d has been moved after a total of m moves.
; (disk m        ) is the disk being moved during move m.
; (from m h   f t) is the peg a disk is taken from during move m.
; (onto m h   f t) is the peg a disk is put onto during move m.
; (thrd m h   f t) is the peg not involved in move m.
; (posi m h d f t) is the position of disk d after move m.

(define (exp2 n        ) (expt   2 n))
(define (mod2 n        ) (modulo n 2))
(define (mod3 n        ) (modulo n 3))
(define (pari n        ) (add1 (mod2 (add1 n))))
(define (rotd h   d f t) (mod3 (* (- t f) (pari (- h d)))))
(define (rotr h     f t) (rotd h 0 t f))
(define (mcnt m   d    ) (quotient (+ m (exp2 d)) (exp2 (add1 d))))
(define (thrd m h   f t) (mod3 (+ f (* m (rotr h f t)))))
(define (onto m h   f t) (mod3 (- (thrd m h f t) (rotd h (disk m) f t))))
(define (from m h   f t) (mod3 (+ (thrd m h f t) (rotd h (disk m) f t))))
(define (posi m h d f t) (mod3 (+ f (* (rotd h d f t) (mcnt m d)))))
(define (disk m)         (sub1 (integer-length (bitwise-xor m (sub1 m)))))

; (disk m) is the number of times m can be divided by 2.

;---------------------------------------------------------------------------------------------------

(define (example h f t)
 (printf "Shortest path of tower of ~a disks from peg ~a to peg ~a.~n" h f t)
 (print-header (make-list h f))
 (define distr (make-list h f))
 (for ((m (in-range 1 (expt 2 h))))
  (let-values
   (((d f t new-distr)
     (values (disk m) (from m h f t) (onto m h f t)
      (for/list ((d (in-range 0 h))) (posi m h d f t)))))
   (print-move m d f t new-distr)
   (check-move new-distr distr)
   (set! distr new-distr)))
 (printf "End-of-path~n~n"))

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

(define (format-distr distr)
 (list->string (map (λ (p) (vector-ref #(#\0 #\1 #\2) p)) distr)))

(print-example-header 'hanoi-f
 "Shortest path from peg to another peg~n~
  computed move by move without recursion.")

(for ((h (in-range 1 8))) (example h 0 1))

; Some moves for a tower of 100 disks
; before and after moving the largest disk.

(let* ((h 100) (m (expt 2 (sub1 h))) (f 0) (t 1) (n 50)) 
 (printf
  "~a moves starting from move ~a~n~
   for the fastest way of moving a tower of ~a disks~n~
   from peg ~a onto peg ~a~n"
  (* 2 n) (- m n) h f t)
 (printf "Notice that ~a = (expt 2 ~a)~n" m (sub1 h))
 (printf "For this move the largest disk ~s is moved.~n" (sub1 h))
 (printf "Also notice how fast the computation is made.~n")
 (printf "This is shown by the following timing.~n")
 (define path
  (time
   (for/list ((m (in-range (- m n) (+ m n))))
    (list m (disk m) (from m h f t) (onto m h f t)
     (for/list ((d (in-range 0 h))) (posi m h d f t))))))
 (define distrs (map car (map cddddr path)))
 (for ((a (in-list distrs)) (b (in-list (cdr distrs)))) (check-move a b))
 (newline)
 (define pad-move (add1 (order-of-magnitude (+ m 100))))
 (for ((move (in-list path)))
  (printf "move ~a: disk ~a from ~a to ~a~n~a~n"
   (pad (car move) pad-move)
   (pad (cadr move) 2)
   (caddr move)
   (cadddr move)
   (format-distr (car (cddddr move)))))
 (printf "end of list~n~n"))

(let ((N-avagadro #e6.022140857e23) (h 80) (f 0) (t 1))
 (printf "~nMove ~a from ~a to ~a with ~a disks~n" N-avagadro f t h)
  (let-values
   (((d f t r distr)
     (time
      (values
       (disk N-avagadro)
       (from N-avagadro h 0 1)
       (onto N-avagadro h f t)
       (thrd N-avagadro h f t)
       (for/list ((d (in-range 0 h))) (posi N-avagadro h d f t))))))
   (printf "disk from onto thrd~n")
   (printf "~a ~a ~a ~a~n~a~n" (pad d 4) (pad f 4) (pad t 4) (pad r 4) (format-distr distr))
   (newline)))

(let ((N (expt 2 79)) (h 80) (f 0) (t 1))
 (printf "~nMove ~a from ~a to ~a with ~a disks~n" N f t h)
  (let-values
   (((d f t r distr)
     (time
      (values
       (disk N)
       (from N h 0 1)
       (onto N h f t)
       (thrd N h f t)
       (for/list ((d (in-range 0 h))) (posi N h d f t))))))
   (printf "disk from onto thrd~n")
   (printf "~a ~a ~a ~a~n~a~n" (pad d 4) (pad f 4) (pad t 4) (pad r 4) (format-distr distr))
   (newline)))

