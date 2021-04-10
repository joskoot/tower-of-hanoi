#lang racket

(require "hanoi-tools.rkt")

;---------------------------------------------------------------------------------------------------
; By Jacob J. A. Koot, April 2016
;
; The tower of Hanoi
;
; Shortest paths between two arbitrary distributions of disks.
; Ignore largest disks already on the right peg.
; Below the largest disk is the largest one that has to be moved.
; Usually there is 1 shortest path, moving the largest disk once or twice.
; In some cases moving the largest disk once gives the shortest path.
; In other cases moving the largest disk twice gives the shortest path.
; In some particular cases there are 2 minimal paths,
; one moving the largest disk once and one moving the largest disk twice.
; There always is at least one minimal length path.

(define (shortest-paths from-distr onto-distr)
 
 (define path-1a '())
 (define path-1b '())
 (define path-2a '())
 (define path-2b '())
 (define (move-disk-1a d f t) (set! path-1a (cons (list d f t) path-1a)))
 (define (move-disk-1b d f t) (set! path-1b (cons (list d t f) path-1b)))
 (define (move-disk-2a d f t) (set! path-2a (cons (list d f t) path-2a)))
 (define (move-disk-2b d f t) (set! path-2b (cons (list d t f) path-2b)))
 
 (define (shortest-paths reversed-from-distr reversed-onto-distr)
  (cond
   ((null? reversed-from-distr))
   (else
    (define h (length reversed-from-distr))
    (define h-1 (sub1 h))
    (define f (car reversed-from-distr))
    (define t (car reversed-onto-distr))
    (cond
     ((= f t)
      ; Ignore largest disk if already in destination position.
      (shortest-paths (cdr reversed-from-distr) (cdr reversed-onto-distr)))
     (else
      (define r (- 3 f t))
      ; 2 alternatives must be investigated, the first one moving the largest disk once.
      (shortest-path h-1 (cdr reversed-from-distr) r move-disk-1a)
      (move-disk-1a h-1 f t)
      (shortest-path h-1 (cdr reversed-onto-distr) r move-disk-1b)
      ; The second alternative moves the largest disk twice.
      (shortest-path h-1 (cdr reversed-from-distr) t move-disk-2a)
      (move-disk-2a h-1 f r)
      (move-tower h-1 t f move-disk-2a)
      (move-disk-2a h-1 r t)
      (shortest-path h-1 (cdr reversed-onto-distr) f move-disk-2b))))))
 
 (shortest-paths (reverse from-distr) (reverse onto-distr))
 
 (define path1 (append (reverse path-1a) path-1b))
 (define path2 (append (reverse path-2a) path-2b))
 (define len1 (length path1))
 (define len2 (length path2))
 (printf "Length of paths: ~s and ~s~n" len1 len2)
 
 (cond
  ((and (zero? len1) (zero? len2))
   ; The from-distr equals the to-distr.
   '(()))
  ((= len1 len2)
   ; two paths found of same length.
   (list path1 path2))
  ((< len1 len2)
   ; two paths found, but path1 is shorter.
   (list path1))
  (else
   ; two paths found, but path2 is shorter.
   (list path2))))

(define (shortest-path h distr t local-move-disk)
 (unless (zero? h)
  (define h-1 (sub1 h))
  (define f (car distr))
  (cond
   ((= f t) (shortest-path h-1 (cdr distr) t local-move-disk))
   (else
    (define r (- 3 f t))
    (shortest-path   h-1 (cdr distr) r local-move-disk)
    (local-move-disk h-1 f t)
    (move-tower      h-1 r t local-move-disk)))))

(define (move-tower h f t local-move-disk)
 (define (move-tower h f t)
  (unless (zero? h)
   (define r (- 3 f t))
   (define h-1 (sub1 h))
   (move-tower      h-1 f r)
   (local-move-disk h-1 f t)
   (move-tower      h-1 r t)))
 (move-tower h f t))

;---------------------------------------------------------------------------------------------------

(define (example from-distr to-distr)
 (unless (= (length from-distr) (length to-distr))
  (error 'example-h "incompatible length ~s ~s" from-distr to-distr))
 (printf "Minimal length paths from ~a to ~a~n" from-distr to-distr)
 (define paths (shortest-paths from-distr to-distr))
 (define n (length paths))
 (printf "Nr of paths: ~a~n" n)
 (printf "Length = ~a~n" (length (car paths)))
 (for ((n (in-range 1 (add1 n))) (path (in-list paths)))
  (initialize-path (list->vector from-distr))
  (printf "Path ~a~n" n)
  (for ((move (in-list path))) (apply move-disk move))
  (print-path to-distr)))

(print-example-header 'hanoi-h
 "Shortest paths between two arbitrary distributions of disks.")

(example '(0) '(0))
(example '(0) '(1))
(example '(0 0 0 0) '(1 1 1 1))
(example '(0 0 1 2) '(0 0 2 1))
(example '(0 0 0 0) '(1 1 1 0))
(example '(0 1 2 0 1 2 0) '(2 1 0 2 1 0 2))
