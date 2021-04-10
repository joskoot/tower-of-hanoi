#lang racket

(require "hanoi-tools.rkt")

;---------------------------------------------------------------------------------------------------
; By Jacob J. A. Koot, April 2016
;
; The tower of Hanoi
;
; Simple recursive procedure for the longest non self-crossing path
; of moving a tower from one peg to another one.
;
; h : nr of disks.
; f : from peg.
; t : onto peg, t≠f.
;
; The disks are identified in order of increasing size
; by the natural numbers 0 up to but not including h.
; The pegs are identified by 0, 1 and 2.

(define (longest-non-circular-path h f t)
 (unless (zero? h)
  (define h-1 (sub1 h))
  (define r (- 3 f t))
  (longest-non-circular-path h-1 f t)
  (move-disk                 h-1 f r)
  (longest-non-circular-path h-1 t f)
  (move-disk                 h-1 r t)
  (longest-non-circular-path h-1 f t)))

;---------------------------------------------------------------------------------------------------
; We use procedures move-disk and print-path of module hanoi-tools.rkt.
; Procedure move-disk keeps a record of all moves. It also checks each move to be legal.
; Procedure print-path uses this record to print results.
; Procedure initialize-path initializes the bookkeeping for procedure move-disk.

(define (example h f t)
 (printf "Hamilton path for ~a disks from peg ~a via peg ~a to peg ~a.~n" h f (- 3 f t) t)
 (initialize-path h f)
 (longest-non-circular-path h f t)
 (print-path t))

(print-example-header 'hanoi-b
 "Simple recursive procedure for non circular Hamilton path~n~
  moving a tower from one peg to another one.")

(for ((h (in-range 1 5))) (example h 0 1))

;---------------------------------------------------------------------------------------------------
