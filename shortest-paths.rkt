#lang racket

(provide shortest-paths)

; By Jacob J. A. Koot, April 2016
;
; Finding all shortest path between two vertices of a graph of the following type:
;
; All edges have length 1.
; Both unidirectional and bidirectional edges are allowed, even intermixed.
; The graph is not required to be connected.
; Duplicate edges between two vertices are treated as one single edge.
;
; The graph is represented by a function that given a vertex N
; returns a list of all vertices with an inbound edge from N.

; Procedure (shortest-paths S T G) -> F
;
; For any type N:
;
; S : N	           starting vertex
; T : N	           destination vertex
; G : N -> (N ...) procedure representing the graph
; P : (N ...)	   list of vertices showing a path from S to T
; F : (P ...)      list of all paths from S to T

; Procedure G is given a vertex N.
; It is expected to return a list of all vertices having an incoming edge from N.

; Below reversed-P is a reversed partial path ending in S.
; reversed-F is a list of reversed-Ps.
; Set H is used to record all vertices already in one of the paths of reversed-F.
; New reversed-Ps starting with a vertex already recorded in H are discarded.

; Method
; A list reversed-F of partial paths is maintained.
; Initialy reversed-F is ((S)).
; As long as the shortest paths have not been found and reversed-F is not empty,
; a new reversed-F is formed. For each reversed-P of reversed-F new partial paths are formed
; by consing each distinct vertex of (G (car reversed-P)) to reversed-P, yielding as many
; partial paths as G returns distinct vertices. The new reversed-P contains all new partial paths
; that do not start with a vertex already found in previous partial paths,
; for this means that there is a shorter path from S to that vertex.
; If at least one of the new partial paths starts with vertex T, all shortest paths have been found.
; The paths are constructed in reverse order, starting from S and consing other vertices.
; The result is obtained by keeping the reversed paths that start with T and reversing them.
; The new reversed-F being empty means that there is no path from S to T.

(define (shortest-paths S T G)
 
 (define (next-reversed-F reversed-F)
  (define new-reversed-F (filter found-T? reversed-F))
  (cond
   ((null? reversed-F)
    ; No path found.
    '())
   ((pair? new-reversed-F)
    ; The paths have been found.
    (map reverse new-reversed-F)) ; (*)
   (else
    ; Vertex T has not yet been found.
    (for-each put-car! reversed-F)
    (next-reversed-F (apply append (map extend-paths reversed-F))))))
 
 (define (extend-paths reversed-F)
  (define new-Ns (remove-duplicates (filtered-G (car reversed-F))))
  (define (cons-N N) (cons N reversed-F))
  (map cons-N new-Ns))
 
 (define H (mutable-set))
 
 (define (put-car! reversed-P) (set-add! H (car reversed-P)))
 
 (define (new-vertex? N) (not (set-member? H N)))
 
 (define (filtered-G N) (filter new-vertex? (G N)))
 
 (define (found-T? reversed-P) (equal? (car reversed-P) T))
 
 (next-reversed-F (list (list S))))

; (*) If all edges would be bidirectional, it would be possible to start at the destination node
;     and work towards the starting vertex such as to avoid the reversal of all paths.
;     Another way to avoid the reversals is to replace G by a function such that (G N)
;     returns a list of all vertices that have an outbound edge to N, but this seems less natural.
