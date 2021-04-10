#lang racket

;---------------------------------------------------------------------------------------------------
; By Jacob J. A. Koot, April 2016
;
; The tower of Hanoi
;
; Simple recursive procedure for the shortest path of moving a tower from one peg to another one.
;
; h : nr of disks.
; f : starting peg.
; t : destination peg, t≠f.
;
; The disks are identified in order of increasing size
; by the natural numbers 0 up to but not including h.
; The pegs are identified by 0, 1 and 2.
; r is the third peg.

(require "hanoi-tools.rkt")

(define (move-tower h f t)
 (unless (zero? h)
  (define h-1 (sub1 h))
  (define r (- 3 f t))
  (move-tower h-1 f r)
  (move-disk  h-1 f t)
  (move-tower h-1 r t)))

;---------------------------------------------------------------------------------------------------
; Procedures initialize-path, move-disk and print-path of module hanoi-tools.rkt are used.
; Procedure move-disk keeps a record of all moves. It also checks each move to be legal.
; Procedure print-path uses this record to print results.
; Procedure initialize-path initializes the bookkeeping for procedure move-disk.

(define (example h f t)
 (printf "Shortest path of ~a disks from peg ~a to peg ~a.~n" h f t)
 (initialize-path h f) ; Start with all disks on peg f.
 (move-tower h f t)    ; Move the tower.
 (print-path t))       ; Print the moves and check that all disks are on peg t.

(print-example-header 'hanoi-a
 "Simple recursive procedure for shortest-path~n~
  to move a tower from one peg to another one.")

(for ((h (in-range 1 7))) (example h 0 1))

;---------------------------------------------------------------------------------------------------
