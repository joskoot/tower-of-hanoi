#lang racket

(require redex racket/draw mrlib/graph)

(define (tower-of-hanoi p h)

 ; Uses procedure traces to form the complete graph of the tower of hanoi with h disks and p pegs.
 ; The pegs are identified by the natural numbers from 0 up to but not including p.
 ; Every node shows a list of h pegs, element d showing on which peg disk d is located,
 ; identifying disks in order of increasing size from 0 up to but not including h.

 ; p = nr-of-pegs,  exact-nonnegative-integer, p>2
 ; h = nr-of-disks, exact-nonnegative-integer
 ; (< (expt p h) 3200) in order to avoid too big problems for procedure traces.  
 
 (unless (and (exact-nonnegative-integer? p) (> p 2))
  (raise-user-error 'tower-of-hanoi "expected exact-integer greater than 2, given p=~a" p))
 
 (unless (exact-nonnegative-integer? h)
  (raise-user-error 'tower-of-hanoi "expected exact-nonnegative-integer, given h=~a" h))
 
 (define p^h (expt p h))
 
 (unless (< p^h 3200)
  (raise-user-error 'tower-of-hanoi "p=~a, h=~a, p^h=~a: problem too big" p h p^h))
 
 (define-language lang
  (distr (peg ...))
  (peg (side-condition natural_0 (< (term natural_0) p)))
  (label string))
 
 (define (make-rule q)
  (define q+1 (add1 q))
  (reduction-relation lang
   (-->
    (peg_0 ... peg_1 peg_2 ...)
    (peg_0 ... peg_3 peg_2 ...)
    (where peg_3 ,(modulo (+ (term peg_1) q+1) p))
    (side-condition 
     (let ((from-peg (term peg_1)) (to-peg (term peg_3)))
      (define (check peg) (not (or (= peg from-peg) (= peg to-peg))))
      (andmap check (term (peg_0 ...))))))))
 
 (define rules (apply union-reduction-relations (build-list (sub1 p) make-rule)))
 (define (pp x) (if (string? x) x (get-string x)))
 (define green (send the-color-database find-color "GreenYellow"))
 (define (pred x) (or (and (string? x) green) (not (or (<= h 1) (apply = x)))))

 (define (get-string x)
  (apply string-append (map number->string x)))
 
 (parameterize ((reduction-steps-cutoff (+ p^h 5)))
  (traces rules
   (list
    (make-list h 0)
    (format "p=~a~nh=~a~nn=~a" p h p^h))
   #:multiple? #t
   #:pp pp
   #:pred pred
   #:x-spacing 25
   #:y-spacing 25)))

(for* ((h (in-range 1 5)) (p (in-range 3 6))) (tower-of-hanoi p h))