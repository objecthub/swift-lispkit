;;; LISPKIT GRAPH
;;;
;;; Simple library for representing directed graphs.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2019 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed under the
;;; License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;;; either express or implied. See the License for the specific language governing permissions
;;; and limitations under the License.

(define-library (lispkit graph)

  (export make-graph
          make-eq-graph
          make-eqv-hashtable
          node-equivalence-function
          node-hash-function
          graph-copy
          graph?
          graph-empty?
          graph-cyclic?
          graph-add-node!
          graph-remove-node!
          graph-has-node?
          graph-nodes
          graph-add-edge!
          graph-remove-edge!
          graph-has-edge?
          graph-fold-nodes
          graph-fold-edges
          neighbors
          neighbors+labels
          reachable?
          reachable-nodes
          shortest-path
          edge-label
          in-degree
          out-degree
          make-node-hashtable)

  (import (lispkit base))

  (begin

    (define-values (new-graph graph? graph-ref make-graph-subtype) (make-type 'graph))

    (define (make-graph hash equiv)
      (new-graph (make-hashtable hash equiv)))

    (define (make-eq-graph)
      (new-graph (make-eq-hashtable)))

    (define (make-eqv-graph)
      (new-graph (make-eqv-hashtable)))

    (define (graph-copy graph . args)
      (cond ((pair? args)
              (let* ((ht (graph-ref graph))
                     (res (make-same-hashtable ht)))
                (for-each (lambda (node)
                            (let ((edges (hashtable-get ht node)))
                              (if edges
                                  (hashtable-set! res node (cdr edges))
                                  (error "$0 not a node of graph $1" node graph)))) (car args))
                (hashtable-for-each
                  (lambda (k v)
                    (hashtable-set! res k
                      (filter (lambda (edge) (hashtable-get res (car edge))) v)))
                  res)
                (new-graph res)))
            (else
              (new-graph (hashtable-copy (graph-ref graph) #t)))))

    (define (node-equivalence-function graph)
      (hashtable-equivalence-function (graph-ref graph)))

    (define (node-hash-function graph)
      (hashtable-hash-function (graph-ref graph)))

    (define (graph-empty? graph)
      (zero? (hashtable-size (graph-ref graph))))

    (define (graph-cyclic? graph)
      (let* ((ht (graph-ref graph))
             (onpath (make-same-hashtable ht)))
        (define (visit node)
          (let ((visited (hashtable-get onpath node)))
            (if visited
                (cdr visited)
                (begin (hashtable-set! onpath node #t)
                       (do ((next (cdr (hashtable-get ht node)) (cdr next)))
                           ((or (null? next) (visit (caar next)))
                            (begin (hashtable-set! onpath node #f)
                                   (pair? next))))))))
         (do ((nodes (hashtable-key-list ht) (cdr nodes)))
             ((or (null? nodes) (visit (car nodes))) (pair? nodes)))))

    (define (graph-add-node! graph node)
      (let ((ht (graph-ref graph)))
        (if (not (hashtable-get ht node))
            (hashtable-add! ht node '()))))

    (define (graph-remove-node! graph node)
      (let* ((ht (graph-ref graph))
             (delete-edges (delete-edges-proc ht)))
        (if (hashtable-get ht node)
            (begin (hashtable-delete! ht node)
                   (hashtable-map! (lambda (k v) (delete-edges node v)) ht)))))

    (define (graph-has-node? graph node)
      (pair? (hashtable-get (graph-ref graph) node)))

    (define (graph-nodes graph)
      (hashtable-key-list (graph-ref graph)))

    (define (graph-add-edge! graph from to . args)
      (let-optionals args ((label #f))
        (let* ((ht (graph-ref graph))
               (edges (hashtable-get ht from)))
          (if edges
              (if (hashtable-get ht to)
                  (hashtable-set! ht from (cons (cons to label) (delete-edges ht to (cdr edges))))
                  (error "unknown node $1 of graph $0" graph to))
              (error "unknown node $1 of graph $0" graph from)))))

    (define (graph-remove-edge! graph from to)
      (let* ((ht (graph-ref graph))
             (edges (hashtable-get ht from)))
        (if edges
            (if (hashtable-get ht to)
                (hashtable-set! ht from (delete-edges ht to (cdr edges)))
                (error "unknown node $1 of graph $0" graph to))
            (error "unknown node $1 of graph $0" graph from))))

    (define (graph-has-edge? graph from to)
      (let* ((ht (graph-ref graph))
             (edges (hashtable-get ht from)))
        (if edges
            (if (assoc-edges ht to (cdr edges)) #t #f)
            #f)))

    (define (graph-fold-nodes f z graph)
      (do ((nodes (graph-nodes graph) (cdr nodes))
           (acc z (f (car nodes) acc)))
          ((null? nodes) acc)))

    (define (graph-fold-edges f z graph)
      (let ((ht (graph-ref graph)))
        (do ((ns (hashtable-key-list ht) (cdr ns))
             (acc z (do ((es (cdr (hashtable-get ht (car ns))) (cdr es))
                         (iacc acc (f (car ns) (caar es) (cdar es) iacc)))
                        ((null? es) iacc))))
            ((null? ns) acc))))

    (define (neighbors graph from)
      (let* ((ht (graph-ref graph))
             (edges (hashtable-get ht from)))
        (if edges (map car (cdr edges)) #f)))

    (define (neighbors+labels graph from)
      (let* ((ht (graph-ref graph))
             (edges (hashtable-get ht from)))
        (if edges (cdr edges) #f)))

    (define (reachable? graph from to)
      (let* ((ht       (graph-ref graph))
             (shortcut (let ((compare (hashtable-equivalence-function ht)))
                         (lambda (n) (compare n to))))
             (tags     (make-same-hashtable ht)))
        (hashtable-set! tags from #t)
        (do ((nodes (explore ht (list from) tags shortcut) (explore ht nodes tags shortcut)))
            ((or (not nodes) (null? nodes)) (not nodes)))))

    (define (reachable-nodes graph from . args)
      (let-optionals args ((limit (greatest-fixnum)))
        (cond ((fxnegative? limit)
                (error "limit of procedure reachable-nodes is negative" limit))
              ((fxzero? limit)
                (list from))
              (else
                (let* ((ht       (graph-ref graph))
                     (shortcut (lambda (n) #f))
                     (tags     (make-same-hashtable ht)))
                (hashtable-set! tags from #t)
                (do ((nodes (explore ht (list from) tags shortcut) (explore ht nodes tags shortcut))
                     (reachable (list from) (append nodes reachable))
                     (iteration limit (fx1- iteration)))
                    ((or (null? nodes) (fxzero? iteration)) reachable)))))))

    (define (explore ht nodelist tags shortcut)
      (do ((nodes nodelist (cdr nodes))
           (newnodes '()))
          ((or (null? nodes) (shortcut (car nodes))) (if (pair? nodes) #f newnodes))
        (if (cdr (hashtable-get tags (car nodes)))
            (begin (hashtable-set! tags (car nodes) #f)
                   (do ((next (cdr (hashtable-get ht (car nodes))) (cdr next)))
                       ((null? next))
                     (if (not (hashtable-get tags (caar next)))
                         (begin (hashtable-set! tags (caar next) #t)
                                (set! newnodes (cons (caar next) newnodes)))))))))

    (define (shortest-path graph from to)
      (let* ((ht      (graph-ref graph))
             (compare (hashtable-equivalence-function ht))
             (tags    (make-same-hashtable ht)))
        (define (track nodelist)
          (do ((nodes nodelist (cdr nodes))
               (res '()))
              ((or (null? nodes) (compare (caar nodes) to))
               (if (pair? nodes) (cons #t (car nodes)) (cons #f res)))
            (if (cdr (hashtable-get tags (caar nodes)))
                (begin (hashtable-set! tags (caar nodes) #f)
                       (do ((next (cdr (hashtable-get ht (caar nodes))) (cdr next)))
                           ((null? next))
                         (if (not (hashtable-get tags (caar next)))
                             (begin (hashtable-set! tags (caar next) #t)
                                    (set! res (cons (cons (caar next) (car nodes)) res)))))))))
        (hashtable-set! tags from #t)
        (do ((nodes (track (list (list from))) (track (cdr nodes))))
            ((or (car nodes) (null? (cdr nodes))) (if (car nodes) (reverse (cdr nodes)) #f)))))

    (define (edge-label graph from to)
      (let ((ht (graph-ref graph)))
        (cdr (assoc-edges ht to (cdr (hashtable-get ht from))))))

    (define (out-degree graph node)
      (length (cdr (hashtable-get (graph-ref graph) node))))

    (define (in-degree graph node)
      (let ((compare (hashtable-equivalence-function (graph-ref graph))))
        (graph-fold-edges (lambda (from to label acc)
                            (if (compare to node) (fx1+ acc) acc)) 0 graph)))

    (define (make-node-hashtable graph)
      (make-same-hashtable (graph-ref graph)))

    (define (make-same-hashtable ht)
      (cond ((eq-hashtable? ht)  (make-eq-hashtable))
            ((eqv-hashtable? ht) (make-eqv-hashtable))
            (else                (make-hashtable (hashtable-hash-function ht)
                                                 (hashtable-equivalence-function ht)))))

    (define (assoc-edges ht obj alist)
      (cond ((eq-hashtable? ht)
              (assq obj alist))
            ((eqv-hashtable? ht)
              (assv obj alist))
            (else
              (assoc obj alist (hashtable-equivalence-function ht)))))

    (define (delete-edges ht obj alist)
      (cond ((eq-hashtable? ht)  (alist-delq obj alist))
            ((eqv-hashtable? ht) (alist-delv obj alist))
            (else                (alist-delete obj alist (hashtable-equivalence-function ht)))))

    (define (delete-edges-proc ht)
      (cond ((eq-hashtable? ht)  alist-delq)
            ((eqv-hashtable? ht) alist-delv)
            (else                (let ((compare (hashtable-equivalence-function ht)))
                                   (lambda (obj alist) (alist-delete obj alist compare))))))
  )
)
