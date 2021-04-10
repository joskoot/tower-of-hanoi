#lang racket

;---------------------------------------------------------------------------------------------------
; By Jacob J. A. Koot, April 2016

(provide
 initialize-path
 move-disk
 print-path
 print-move
 print-example-header
 print-header
 print-bar
 pad)

;---------------------------------------------------------------------------------------------------
; Tools for programs that compute paths for the Tower of Hanoi.

; The disks are identified in order of increasing size by subsequent natural numbers from 0.
; The pegs are identified by 0, 1 and 2.
; A distr is a vector or list whose d-th element shows the peg disk d is located at.
; A move is a list (d f t), d the disk being moved from peg f to peg t.

; First call initialize-path.
; Then call move-disk zero, one or more times.
; Then call print-path.

; The moves are recorded in variable path-info.
; The variable is initialized by procedure initialize-path.
; It contains the following fields:
; initial-distr : starting distr of disks, initialized by procedure initialize-path.
; move-list     : list of recorded moves, initially empty, filled by procedure move-disk.
; passed        : set of distrs of disks already passed along the path. Used by move-disk.
; current-distr : current distr of the disks along the path. Used by both move-disk and print-path.

; (initialize-path distr) -> void
; (initialize-path h t) -> void
; distr : vector or list of pegs. Starting distr.
; h     : number of disks.
; f     : starting peg.
; When called with h and f, the starting distr is a tower of h disks with all disks on peg f.

; Procedure (move-disk d f t) -> void
; d : disk being moved,
; f : the peg it is taken from and
; t : the peg it is put onto. 
; Procedure move-disk records a move of disk d from peg f to peg t.
; The move is checked to be consistent with the current distr and to be legal.
; The move is checked not to produce a self-crossing path.
; The current distr is updated.
; The passed distrs are gathered in field passed of variable path-info.
; The path may be circular.
; Therefore the current distr is checked not already to be on the path
; right before it is updated for a new move.

; Procedure (print-path)
; Procedure (print-path t)
; Procedure (print-path distr)
; t     : peg   : the path is checked to end with all disks on peg t.
; distr : distr : the path is checked to end with this distr.
; When called without argument, the path is not checked to end at a particular distr.
; Prints the path. Each printed line contains m, d, f, t and the resulting distr,
; where m is the move number.

; Procedure (print-move m d f t distr)
; m     : move number starting from 1.
; d     : disk being moved.
; f     : peg disk d is taken from.
; t     : peg disk d is put onto.
; distr : distr of disks after making the move.
;
; Procedure print-move does not use variable 'path-info'.
; It accepts pegs and distrs in arbitrary representation.

;---------------------------------------------------------------------------------------------------

(define (initialize-path . args)
 (define distr
  (cond
   ((and (= (length args) 1) (vector? (car args))) (car args))
   ((= (length args) 1) (list->vector (car args)))
   (else (apply make-vector args))))
 (define initial-distr (vector->immutable-vector distr))
 (set! path-info (make-path-info initial-distr '() (mutable-set) distr)))

;---------------------------------------------------------------------------------------------------

(define (move-disk d f t)
 (unless path-info (error 'move-disk "the move-list has not been initialized"))
 (check-move d f t (current-distr))
 (check-not-passed (current-distr))
 (vector-set! (current-distr) d t)
 (add-move! (list d f t)))

;---------------------------------------------------------------------------------------------------

(define (print-path (t #f))
 (unless path-info (error 'print-path "the move-list has not been initialized"))
 (define current-distr (vector-copy (initial-distr)))
 ; copy, because distr will be updated for each move.
 (print-header (->list current-distr))
 (define moves (get-move-list))
 (define last (length moves))
 ; Print the list of moves.
 (for ((move (in-list moves)) (m (in-naturals 1)))
  (define-values (d f t) (apply values move))
  (vector-set! current-distr d t)
  (print-move m d f t (->list current-distr)))
 ; Check the final distr.
 (cond
  ((not t) (void))
  ((member t '(0 1 2))
   (define final-distr (make-vector (vector-length (initial-distr)) t))
   (unless (equal? current-distr final-distr)
    (error 'print-path "path did not end in the desired distr.")))
  ((vector? t)
   (unless (equal? t current-distr)
    (error 'print-path "path did not end in the desired distr.")))
  ((list? t)
   (define final-distr (list->vector t))
   (unless (equal? final-distr current-distr)
    (error 'print-path "path did not end in the desired distr.")))
  (else (error 'print-path "incorrect destination argument: ~s" t)))
 ; Clean up.
 (clear-path-info)
 (printf "End of path~n~n"))

;---------------------------------------------------------------------------------------------------

(define (check-move d f t distr)
 (when
  (or
   (= f t)
   (for/or ((d (in-range 0 d))) (define r (vector-ref distr d)) (or (= r t) (= r f))))
  (error 'move-disk "illegal move d:~a f:~a t:~a distr:~a" d f t distr)))

(define (check-not-passed distr)
 (when (set-member? (passed) distr) (error 'move-disk "self-crossing path at distr ~s" distr))
 (add-passed! distr))

;---------------------------------------------------------------------------------------------------
; Some additional utilities.

(define (->list x) (if (vector? x) (vector->list x) x))

(define (print-header distr)
 (printf "move disk from onto thrd    distr~n")
 (printf "                            ~a~n" (->list distr)))

(define (print-move m d f t distr)
 (printf "~a ~a ~a ~a ~a    ~a~n"
  (pad m 4) (pad d 4) (pad f 4) (pad t 4) (pad (- 3 f t) 4) distr))

(define (pad x n) ; Allign the displayed form of x right justified in a field of n characters.
 (let* ((x (format "~a" x)) (n (- n (string-length x))))
  (string-append (make-string (max 0 n) #\space) x)))

(define (print-example-header name descr)
 (print-bar)
 (printf "Results of ~a.~n~a~n~n" name (format descr)))

(define (print-bar) (printf "---------------------------------------------------------~n"))

;---------------------------------------------------------------------------------------------------
; Internal state for procedures move-disk and print-path.
; To be initialized by calling procedure initialize-path.

(struct path-info (initial-distr (move-list #:mutable) passed current-distr)
 #:constructor-name make-path-info #:omit-define-syntaxes)

(define path-info #f) ; Initialized by procedure initialize-move-disk.
(define (initial-distr) (path-info-initial-distr path-info))
(define (move-list) (path-info-move-list path-info))
(define (passed) (path-info-passed path-info))
(define (current-distr) (path-info-current-distr path-info))
(define (add-move! move) (set-path-info-move-list! path-info (cons move (move-list))))
(define (get-move-list) (reverse (move-list)))
(define (clear-path-info) (set! path-info #f))
(define (add-passed! distr) (set-add! (passed) (if (vector? distr) (vector-copy distr) distr)))

;---------------------------------------------------------------------------------------------------
