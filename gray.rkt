#lang racket

(define (gray b h)
 (if (= h 1) (map list (build-list b identity))
  (apply append
   (for/list ((d (in-range 0 b)))
    (for/list ((x (in-list (if (even? d) (gray b (sub1 h)) (reverse (gray b (sub1 h)))))))
     (cons d x))))))

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

(define (check b h)
 (define code (gray b h))
 (unless
  (and
   (= (length code) (expt b h))
   (= (length code) (length (remove-duplicates code))))
  (error 'check "~s ~s" b h))
 (for/and ((aa (in-list code)) (bb (in-list (cdr code))))
  (let loop ((x aa) (y bb) (done '()))
   (unless
    (and (not (null? x))
     (if (= (car x) (car y)) (loop (cdr x) (cdr y) (cons (car x) done))
      (equal? (cdr x) (cdr y))))
    (error 'check "~s ~s ~s ~s" (reverse aa) (reverse bb) b h)))))

(for*/and ((b (in-range 3 7)) (h (in-range 1 7))) (check b h))