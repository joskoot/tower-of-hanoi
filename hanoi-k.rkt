#lang racket

(require "hanoi-tools.rkt")

;---------------------------------------------------------------------------------------------------
; By Jacob J. A. Koot, April 2016
;
; The tower of Hanoi
;
; Using a Gray-code for the longest non self-crossing path from peg 0 to peg 2.
;
; A (b,h) Gray-code is a bijection G(m) of the natural numbers 0 up to and including b^h-1
; such that, when written in base b positional notation,
; G(m) and G(m+1) differ in exactly one digit.
; A Gray-code is circular if the same holds for G(0) and G(b^h-1).
; 
; Gray-codes are not uniquely defined.
; Let G be a circular Gray-code of for given b and h.
; Then G((m+n) mod b^h) is a Gray-code too for every n.
; Let G be a Gray-code, not necessarily circular.
; Write G(0) up to and including G(b^h-1) in subsequent rows and in positional base b notation.
; Add heading zeros such as to write every number with h digits.
; Then a permutation of the columns yields a Gray-code too.
;
; Procedure Gray-code is such that:
; (Gray-code 0 b h) = 0
; (Gray-code (sub1 (expt b h)) b h) = b^h-1 if b is odd.
; (Gray-code (sub1 (expt b h)) b h) = b-1 if b is even.
; For m running from 0 up to but not including b^h each digit of (Gray-code m b h)
; each less sigificant digit changes more frequently than each more significant digit.

; If b is even, Gray-code is circular.
; If b is odd, Gray-code is not circular.
; If b=3 and writing the Gray-codes in base 3 with exactly h digits,
; the list (map reverse (Gray-code m 3 h)) for m running from 0 up to but not including b^h,
; shows the longest non self-crossing path from peg 0 to peg 2
; as a list of distributions of disks among the pegs.

; (Gray-code m b h) -> Gray code of m with base b and h digits represented by a list of digits.

(define (Gray-code m b h)
 (define 2b (* 2 b))
 (define (Gray-code m h gc)
  (cond
   ((zero? h) gc)
   (else
    (define q (quotient m b))
    (define d (modulo m 2b))
    (Gray-code q (sub1 h) (cons (if (>= d b) (- 2b d 1) d) gc)))))
 (Gray-code m h '()))

; Inverse of Gray-code:
; (Gray-code-inverse (Gray-code m b h) b h) -> m
; (Gray-code (Gray-code-inverse gc b h) b h) -> gc

(define (Gray-code-inverse gc b h)
 (define (Gray-code-inverse gc b^h)
  (cond
   ((null? gc) 0)
   (else
    (define p (car gc))
    (define m (Gray-code-inverse (cdr gc) (quotient b^h b)))
    (+ (* p b^h) (if (odd? p) (- b^h m 1) m)))))
 (Gray-code-inverse gc (expt b (sub1 (length gc)))))

; number->list procuces a list of h digits in base b for number m.
; list->number is its inverse.

(define (number->list m b h)
 (define (number->list m h gc-list)
  (if (zero? m) (append (make-list h 0) gc-list)
   (let-values (((q r) (quotient/remainder m b)))
    (number->list q (sub1 h) (cons r gc-list)))))
 (number->list m h '()))

(define (list->number gc-list b)
 (define (list->number gc-list)
  (if (null? gc-list) 0
   (let ((d (car gc-list)) (gc (cdr gc-list)))
    (+ d (* b (list->number gc))))))
 (list->number (reverse gc-list)))

; Check that (Gray-code m 3 h) -> m-th distribution of longest path.
; Procedure long-move-list is used to check the paths computed with (3,h) Gray code.

(define (long-move-list h)
 (define distr (make-vector h 2))
 (define move-list (list (vector->list distr)))
 (define (move-disk d f t)
  (vector-set! distr d t)
  (set! move-list (cons (vector->list distr) move-list)))
 (define (move-tower h f t)
  (unless (zero? h)
   (define h-1 (sub1 h))
   (define r (- 3 f t))
   (move-tower h-1 f t)
   (move-disk  h-1 f r)
   (move-tower h-1 t f)
   (move-disk  h-1 r t)
   (move-tower h-1 f t)))
 (move-tower h 2 0)
 move-list)

(define (Gray-code-list b h)
 (for/list ((m (in-range (expt b h)))) (Gray-code m b h)))

; Now do the test.

(print-example-header 'hanoi-k
 "Base 3 Gray-code used for the~n~
  non self-crossing path from peg 0 to peg 2.")

(let ((b 3))
 (if
  (for/and ((h (in-range 1 10)))
   (printf "Test (Gray-code m 3 ~a)~n" h)
   (equal?
    (map reverse (Gray-code-list b h))
    (long-move-list h)))
  (printf "~nFirst sequense of tests passed.~n~n")
  (fprintf (current-error-port) "Test failed.~n")))

; More checks on procedures Gray-code and Gray-code-inverse:

(define (check a b)
 (and (not (or (null? a) (null? b)))
  (if (= (car a) (car b)) (check (cdr a) (cdr b))
   (equal? (cdr a) (cdr b)))))

(if
 (for*/and ((b (in-range 2 7)) (h (in-range 1 7)))
  (printf "Test (Gray-code m ~a ~a) and its inverse.~n" b h)
  (define n (expt b h))
  (define gc-list (Gray-code-list b h))
  (and
   (equal? (sort (map (λ (gc) (list->number gc b)) gc-list) <) (build-list n identity))
   (for/and ((m (in-range (expt b h))) (gc (in-list gc-list)))
    (= (Gray-code-inverse gc b h) m))
   (for/and ((m (in-range (expt b h))))
    (= (list->number (Gray-code (Gray-code-inverse (number->list m b h) b h) b h) b) m))
   (for/and ((a (in-list gc-list)) (b (in-list (cdr gc-list))))
    (check a b))))
 (printf "~nSecond sequence of tests passed.~n~n")
 (fprintf (current-error-port) "Test failed.~n"))
