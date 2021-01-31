;;; Topological sort
;;;
;;; Function `topological-sort` returns a topologically sorted list of vertices for
;;; a given directed acyclic graph. The algortihm is inspired by "Introduction to Algorithms",
;;; chapter 22.4, by Cormen, Leiserson, Rivest, and Stein (2009).
;;; 
;;; A graph is represented by a list of sublists. The first element of each sublist is a
;;; vertex. The remaining elements of the sublist define all vertices that are adjacent
;;; to that vertex; i.e. for these vertices, there exists an edge to the first vertex.
;;; `topological-sort` sorts an acyclic directed graph such that for every edge from
;;; vertex `u` to `v`, `u` will become before `v` in the resulting list of vertices.
;;; The algorithm achieves this in O(|V| + |E|), where |V| is the number of vertices and
;;; |E| the number of edges.
;;; 
;;; This is the example from Cormen, Leiserson, Rivest, and Stein:
;;;   Prof. Bumstead topologically sorts his clothing when getting dressed. Clothes are
;;;   vertices. Each directed edge (`u`,`v`) means that garment `u` must be put on before
;;;   garment `v`. All edges from `u` are described as a single sublist of the form
;;;   (`u` `v1` `v2` ...). In the following example, Prof. Bumstead needs to put on his
;;;   shirt before both his tie and his belt:
;;;   
;;;     ((shirt tie belt)
;;;      (tie jacket)
;;;      (belt jacket)
;;;      (watch)
;;;      (pants shoes belt)
;;;      (undershorts pants shoes)
;;;      (socks shoes))
;;;   
;;;   Function `topological-sort` can now compute the order in which Prof. Bumstead
;;;   has to get dressed:
;;;   
;;;     (topological-sort '((shirt tie belt)
;;;                         (tie jacket)
;;;                         (belt jacket)
;;;                         (watch)
;;;                         (pants shoes belt)
;;;                         (undershorts pants shoes)
;;;                         (socks shoes))
;;;     ==> (socks undershorts pants shoes watch shirt belt tie jacket)
;;;  
;;;  The original code from the algorithm below was written by Mikael Djurfeldt.
;;;    "tsort.scm" Topological sort
;;;    Copyright (C) 1995 Mikael Djurfeldt
;;;    This code is in the public domain.

(import (lispkit base))

;; Returns a topologically sorted list of vertices for a given directed acyclic graph.
;; A graph is represented by a list of sublists. The first element of each sublist is a
;; vertex. The remaining elements of the sublist define all vertices for which there
;; exists an edge to the first vertex of the sublist.
;;   `dag`      is a directed acyclic graph
;;   `eql?`     is an equivalence relation
;;   `eql-hash` is a hash function compatible to `eql?`. `eql-hash` can be omitted if
;;              `eql?` is either `eq?`, `eqv?`, or `equal?`.
(define topological-sort
  (case-lambda
    ((dag)
      (tsort dag eqv-hash eqv?))
    ((dag eql?)
      (cond ((eq? eql? eq?)    (tsort dag eq-hash eq?))
            ((eq? eql? eqv?)   (tsort dag eqv-hash eqv?))
            ((eq? eql? equal?) (tsort dag equal-hash equal?))
            (else              (error "topological-sort: missing hash function"))))
    ((dag eql-hash eql?)
      (tsort dag eql-hash eql?))))

(define (tsort dag eql-hash eql?)
  (if (null? dag)
      '()
      (letrec* ((adj-table (make-hashtable eql-hash eql?))
                (sorted    '())
                (visit     (lambda (u adj-list)
                             ;; Color vertex u
                             (hashtable-set! adj-table u #t)
                             ;; Visit uncolored vertices which u connects to
                             (for-each (lambda (v)
                                         (let ((val (hashtable-ref adj-table v #f)))
                                           (if (not (eq? val #t))
                                               (visit v (or val '())))))
                                       adj-list)
                             ;; Since all vertices downstream u are visited by now, we can
                             ;; safely put u on the output list
                             (set! sorted (cons u sorted)))))
        ;; Hash adjacency lists
        (for-each (lambda (def) (hashtable-set! adj-table (car def) (cdr def)))
                  (cdr dag))
        ;; Visit vertices
        (visit (caar dag) (cdar dag))
        (for-each (lambda (def)
                    (let ((val (hashtable-ref adj-table (car def) #f)))
                      (if (not (eq? val #t))
                          (visit (car def) (cdr def)))))
                  (cdr dag))
        sorted)))
