#lang racket
(require "../../fmt/fmt.rkt")
(define r (/ (sub1 (expt 3 64)) (sub1 (expt 2 64))))
((fmt "e.30") (/ (sub1 (expt 3 64)) (sub1 (expt 2 64))))
