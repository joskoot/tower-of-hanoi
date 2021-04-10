#lang racket
(require "../../../fmt/fmt.rkt")
((fmt "f34.30x'teracenturies'") (/ (sub1 (expt 2 64)) #e365.25                100 #e1e12))
((fmt "f34.30x'years'")         (/ (sub1 (expt 2 64)) #e365.25 24 3600 #e20e9))
((fmt "f34.30x'gigacenturies'") (/ (sub1 (expt 3 64)) #e365.25 24 3600 #e20e9 100 #e1e9))
((fmt "e38.30") (/ (sub1 (expt 3 64)) (sub1 (expt 2 64))))

