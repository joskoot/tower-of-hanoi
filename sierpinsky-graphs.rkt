#lang racket

; By Jacob J. A. Koot, April 2016

(require graphics/graphics racket/gui/base)

(define (draw-graph h mode)
 
 (define (draw-whole-graph a b c h)
  (cond
   ((= h 1)
    (draw-line* a b)
    (draw-line* b c)
    (draw-line* c a))
   (else
    (define h-1 (sub1 h))
    (define factor (/ (expt2-1 h-1) (expt2-1 h)))
    (draw-whole-graph a (+ a (* (- b a) factor)) (+ a (* (- c a) factor)) h-1)
    (draw-whole-graph b (+ b (* (- c b) factor)) (+ b (* (- a b) factor)) h-1)
    (draw-whole-graph c (+ c (* (- a c) factor)) (+ c (* (- b c) factor)) h-1)
    (draw-line* (+ a (* (- b a) factor)) (+ b (* (- a b) factor)))
    (draw-line* (+ b (* (- c b) factor)) (+ c (* (- b c) factor)))
    (draw-line* (+ c (* (- a c) factor)) (+ a (* (- c a) factor))))))
 
 (define (long-graph a b c h)
  (cond
   ((= h 1)
    (draw-thick-line* a c)
    (draw-thick-line* c b)
    (draw-line* b a))
   (else
    (define h-1 (sub1 h))
    (define factor (/ (expt2-1 h-1) (expt2-1 h)))
    (long-graph a (+ a (* (- c a) factor)) (+ a (* (- b a) factor)) h-1)
    (long-graph b (+ b (* (- c b) factor)) (+ b (* (- a b) factor)) h-1)
    (long-graph   (+ c (* (- b c) factor)) (+ c (* (- a c) factor)) c h-1)
    (draw-line*        (+ a (* (- b a) factor)) (+ b (* (- a b) factor)))
    (draw-thick-line*  (+ b (* (- c b) factor)) (+ c (* (- b c) factor)))
    (draw-thick-line*  (+ c (* (- a c) factor)) (+ a (* (- c a) factor))))))
 
 (define (circular-graph a b c h)
  (cond
   ((= h 1)
    (draw-thick-line* a c)
    (draw-thick-line* c b)
    (draw-thick-line* b a))
   (else
    (define h-1 (sub1 h))
    (define factor (/ (expt2-1 h-1) (expt2-1 h)))
    (long-graph  (+ a (* (- c a) factor)) (+ a (* (- b a) factor)) a h-1)
    (long-graph  (+ b (* (- c b) factor)) (+ b (* (- a b) factor)) b h-1)
    (long-graph  (+ c (* (- b c) factor)) (+ c (* (- a c) factor)) c h-1)
    (draw-thick-line* (+ a (* (- b a) factor)) (+ b (* (- a b) factor)))
    (draw-thick-line* (+ b (* (- c b) factor)) (+ c (* (- b c) factor)))
    (draw-thick-line* (+ c (* (- a c) factor)) (+ a (* (- c a) factor))))))
 
 (define mode-string
  (case mode
   ((whole) "The whole graph.")
   ((long) "Hamilton path from one peg to another one.")
   ((circular) "Circular Hamilton path.")))
 
 (define margin 10)
 (define string-height 15)
 (define lateral 600)
 (define y-factor (sqrt 3/4))
 (define port-size-x (+ (* 2 margin) lateral))
 (define port-size-y (ceiling (inexact->exact (+ 25 (* y-factor port-size-x)))))
 (define origin (make-rectangular margin (- port-size-y margin)))
 (define A origin)
 (define B (+ A lateral))
 (define C (+ A (make-rectangular (/ lateral 2) (- (* y-factor lateral)))))
 
 (define (expt2 n) (arithmetic-shift 1 n))
 (define (expt2-1 n) (sub1 (expt2 n)))
 
 (define (draw-string* linecount string)
  ((draw-string viewport) (make-posn margin (* string-height linecount)) string))
 
 (define (draw-line* f t) ((draw-line viewport) (complex->posn f) (complex->posn t)))
 
 (define (draw-thick-line* f t)
  (define half-width 2)
  (define phi (angle (- t f)))
  (define d1 (make-polar half-width (+ phi (/ pi 2))))
  (define d2 (make-polar half-width (- phi (/ pi 2))))
  (define a (complex->posn (+ f d1)))
  (define b (complex->posn (+ t d1)))
  (define c (complex->posn (+ t d2)))
  (define d (complex->posn (+ f d2)))
  ((draw-solid-polygon viewport) (list a b c d) (make-posn 0 0)))
 
 (define (posn->complex p) (make-rectangular (posn-x p)(posn-y p)))
 (define (complex->posn c) (make-posn (real-part c) (imag-part c)))
 
 (define choices (get-choices h mode))
 (define viewport
  (begin (open-graphics)
   ((if (member 3 choices) open-viewport open-pixmap) "Hanoi" port-size-x port-size-y)))
 
 (draw-string* 1 (format "Number of disks: ~a." h))
 (draw-string* 2 mode-string)
 
 (case mode
  ((whole) (draw-whole-graph A B C h))
  ((long) (long-graph A B C h))
  ((circular) (circular-graph A B C h)))

 (when choices
  (when (member 1 choices) (display (viewport->snip viewport)) (newline) (newline))
  (when (member 2 choices) ((save-pixmap viewport) (format "hanoi-~a-~a.bmp" mode h) 'bmp))
  (unless (or (member 0 choices) (not (member 3 choices))) (close-viewport viewport))))

(define choices '())

(define (get-choices h mode)
 (unless (member 4 choices)
  (set! choices
   (or
    (get-choices-from-user
     (format "Hanoi, ~a disks, ~a-path" h mode)  
      "Choose one or more options"
      (list
       "don't close the graph"
       "display graph in interactions window"
       "write graph onto a file"
       "show the graph"
       "remember these options")
     #f
     '()
     '(multiple))
    '())))
 choices)

(for* ((h (in-range 1 7)) (mode (in-list '(whole long circular)))) (draw-graph h mode))
