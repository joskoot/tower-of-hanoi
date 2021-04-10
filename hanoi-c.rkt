#lang racket

(require "hanoi-tools.rkt")

;---------------------------------------------------------------------------------------------------
; By Jacob J. A. Koot, April 2016
;
; The tower of Hanoi
;
; Simple procedure for the longest circular non self-crossing path
; of moving a tower from one peg f via peg t and peg (- 3 f t) back to peg f.
; h   : nr of disks.
; f t : two distinct pegs.
; The disks are identified in order of increasing size
; by the natural numbers 0 up to but not including h.
; The pegs are identified by 0, 1 and 2.

(define (longest-circular-path h f t)
 (unless (zero? h)
  (define h-1 (sub1 h))
  (define r (- 3 f t))
  (start-path                h-1 f r)
  (move-disk                 h-1 f t)
  (longest-non-circular-path h-1 r f)
  (move-disk                 h-1 t r)
  (longest-non-circular-path h-1 f t)
  (move-disk                 h-1 r f)
  (finish-path               h-1 t f)))
 
(define (longest-non-circular-path h f t)
 (unless (zero? h)
  (define h-1 (sub1 h))
  (define r (- 3 f t))
  (longest-non-circular-path h-1 f t)
  (move-disk                 h-1 f r)
  (longest-non-circular-path h-1 t f)
  (move-disk                 h-1 r t)
  (longest-non-circular-path h-1 f t)))

(define (start-path h f t)
 (unless (zero? h)
  (define h-1 (sub1 h))
  (define r (- 3 f t))
  (start-path                h-1 f r)
  (move-disk                 h-1 f t)
  (longest-non-circular-path h-1 r t)))

(define (finish-path h f t)
 (unless (zero? h)
  (define h-1 (sub1 h))
  (define r (- 3 f t))
  (longest-non-circular-path h-1 f r)
  (move-disk                 h-1 f t)
  (finish-path               h-1 r t)))

;---------------------------------------------------------------------------------------------------
; We use procedures move-disk and print-path of module hanoi-tools.rkt.
; Procedure move-disk keeps a record of all moves. It also checks each move to be legal.
; Procedure print-path uses this record to print results.
; Procedure initialize-path initializes the bookkeeping for procedure move-disk.

(define (example h f t)
 (printf "Circular Hamilton path for ~a disks~n~
          from peg ~a via peg ~a and peg ~a back to peg ~a.~n"
  h f t (- 3 f t) f)
 (initialize-path h f)
 (longest-circular-path h f t)
 (print-path f))

(print-example-header 'hanoi-c "Simple recursive procedure for circular Hamilton path.")
(for ((h (in-range 1 5))) (example h 0 1))
