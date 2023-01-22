;;; LISPKIT GRAPH
;;;
;;; Simple library for representing and manipulating directed graphs. The library
;;; also provides implementations for the most common graph algorithms.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2019-2023 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
;;; file except in compliance with the License. You may obtain a copy of the License at
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
          make-equal-hashtable
          graph
          eq-graph
          eqv-graph
          equal-graph
          node-equivalence-function
          node-hash-function
          graph-copy
          graph-transpose
          graph-complement
          graph?
          graph-type-tag
          graph-empty?
          graph-cyclic?
          graph-add-node!
          graph-remove-node!
          graph-has-node?
          graph-nodes
          graph-add-edge!
          graph-remove-edge!
          graph-has-edge?
          graph-edges
          graph-edge-label
          graph-in-degree
          graph-out-degree
          graph-fold-nodes
          graph-fold-edges
          graph-neighbors
          graph-neighbors+labels
          graph-reachable?
          graph-reachable-nodes
          graph-topological-sort
          graph-weakly-connected-components
          graph-strongly-connected-components
          graph-shortest-path
          graph-shortest-paths)

  (import (lispkit base)
          (lispkit iterate))

  (begin

    (define-values (graph-type-tag new-graph graph? graph-ref make-graph-subtype)
      (make-type 'graph))

    (define (make-graph hash equiv)
      (new-graph (make-hashtable hash equiv)))

    (define (make-eq-graph)
      (new-graph (make-eq-hashtable)))

    (define (make-eqv-graph)
      (new-graph (make-eqv-hashtable)))

    (define (make-equal-graph)
      (new-graph (make-equal-hashtable)))

    (define (graph hash equiv nodes edges)
      (graph/hashtable (make-hashtable hash equiv) nodes edges))

    (define (eq-graph nodes edges)
      (graph/hashtable (make-eq-hashtable) nodes edges))

    (define (eqv-graph nodes edges)
      (graph/hashtable (make-eqv-hashtable) nodes edges))

    (define (equal-graph nodes edges)
      (graph/hashtable (make-equal-hashtable) nodes edges))

    (define (graph/hashtable ht nodes edges)
      (for-each (lambda (node) (hashtable-add! ht node '())) nodes)
      (for-each (lambda (edge)
                  (graph/hashtable-add-edge! ht (car edge) (cadr edge)
                     (if (null? (cddr edge)) #f (cddr edge))))
                edges)
      (new-graph ht))

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

    (define (graph-transpose graph)
      (graph/hashtable (make-same-hashtable (graph-ref graph))
                       (graph-nodes graph)
                       (map (lambda (edge) (cons (cadr edge) (cons (car edge) (cddr edge))))
                            (graph-edges graph))))

    (define (graph-complement graph)
      (let* ((nodes (graph-nodes graph))
             (res (graph/hashtable (make-node-hashtable graph) nodes '())))
        (for-each (lambda (from)
                    (for-each (lambda (to)
                                (if (not (graph-has-edge? graph from to))
                                    (graph-add-edge! res from to))) nodes)) nodes)
        res))

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

    (define graph-add-edge!
      (case-lambda
        ((graph edge)
          (if (null? (cddr edge))
              (graph-add-edge! graph (car edge) (cadr edge))
              (graph-add-edge! graph (car edge) (cadr edge) (cddr edge))))
        ((graph from to)
          (graph/hashtable-add-edge! (graph-ref graph) from to #f))
        ((graph from to label)
          (graph/hashtable-add-edge! (graph-ref graph) from to label))))

    (define (graph/hashtable-add-edge! ht from to label)
      (let ((edges (hashtable-get ht from)))
        (if edges
            (if (hashtable-get ht to)
                (hashtable-set! ht from (cons (cons to label) (delete-edges ht to (cdr edges))))
                (error "unknown node $1 of graph $0" graph to))
            (error "unknown node $1 of graph $0" graph from))))

    (define graph-remove-edge!
      (case-lambda
        ((graph edge)
          (graph-remove-edge! graph (car edge) (cadr edge)))
        ((graph from to)
          (let* ((ht (graph-ref graph))
                 (edges (hashtable-get ht from)))
            (if edges
                (if (hashtable-get ht to)
                    (hashtable-set! ht from (delete-edges ht to (cdr edges)))
                    (error "unknown node $1 of graph $0" graph to))
                (error "unknown node $1 of graph $0" graph from))))))

    (define graph-has-edge?
      (case-lambda
        ((graph edge)
          (graph-has-edge? graph (car edge) (cadr edge)))
        ((graph from to)
          (let* ((ht (graph-ref graph))
                 (edges (hashtable-get ht from)))
            (if edges
                (if (assoc-edges ht to (cdr edges)) #t #f)
                #f)))))

    (define (graph-edges graph)
      (let ((ht (graph-ref graph))
            (edges '()))
        (for-each
          (lambda (from)
            (for-each
              (lambda (toedge)
                (set! edges (cons (cons from
                                        (cons (car toedge)
                                              (if (cdr toedge) (cdr toedge) '())))
                                  edges)))
              (cdr (hashtable-get ht from))))
          (hashtable-key-list ht))
        edges))

    (define (graph-edge-label graph from to)
      (let* ((ht    (graph-ref graph))
             (edges (hashtable-get ht from))
             (res   (and (pair? edges) (assoc-edges ht to (cdr edges)))))
        (if res
            (cdr res)
            (error "cannot return label; graph does not have an edge from $0 to $1" from to))))

    (define (graph-out-degree graph node)
      (length (cdr (hashtable-get (graph-ref graph) node))))

    (define (graph-in-degree graph node)
      (let ((compare (hashtable-equivalence-function (graph-ref graph))))
        (graph-fold-edges (lambda (from to label acc)
                            (if (compare to node) (fx1+ acc) acc)) 0 graph)))
    
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

    (define (graph-neighbors graph from)
      (let* ((ht (graph-ref graph))
             (edges (hashtable-get ht from)))
        (if edges (map car (cdr edges)) #f)))

    (define (graph-neighbors+labels graph from)
      (let* ((ht (graph-ref graph))
             (edges (hashtable-get ht from)))
        (if edges (cdr edges) #f)))

    (define (graph-reachable? graph from to)
      (let* ((ht       (graph-ref graph))
             (shortcut (let ((compare (hashtable-equivalence-function ht)))
                         (lambda (n) (compare n to))))
             (tags     (make-same-hashtable ht)))
        (hashtable-set! tags from #t)
        (do ((nodes (explore ht (list from) tags shortcut) (explore ht nodes tags shortcut)))
            ((or (not nodes) (null? nodes)) (not nodes)))))

    (define (graph-reachable-nodes graph from . args)
      (let-optionals args ((limit fx-greatest))
        (cond ((fxnegative? limit)
                (error "limit of procedure reachable-nodes is negative" limit))
              ((fxzero? limit)
                (list from))
              (else
                (let* ((ht       (graph-ref graph))
                       (shortcut (lambda (n) #f))
                       (tags     (make-same-hashtable ht)))
                  (hashtable-set! tags from #t)
                  (do ((nodes (explore ht (list from) tags shortcut)
                              (explore ht nodes tags shortcut))
                       (reachable (list from) (append nodes reachable))
                       (iteration limit (fx1- iteration)))
                      ((or (null? nodes) (fxzero? iteration)) reachable)))))))
    
    (define (graph-weakly-connected-components graph)
      (let* ((ht        (graph-ref graph))
             (component (make-same-hashtable ht)))
        (define (merge-into comp nodes)
          (set-box! comp (append nodes (unbox comp)))
          (do ((next nodes (cdr next)))
              ((null? next))
            (hashtable-set! component (car next) comp)))
        (do ((next (hashtable-key-list ht) (cdr next)))
            ((null? next))
          (let ((curr (hashtable-ref component (car next) #f)))
            (if (not curr)
                (begin
                  (set! curr (box (list (car next))))
                  (hashtable-set! component (car next) curr)))
            (do ((follow (hashtable-ref ht (car next) '()) (cdr follow)))
                ((null? follow))
              (let ((comp (hashtable-ref component (caar follow) #f)))
                (if comp
                    (if (not (eq? comp curr)) (merge-into curr (unbox comp)))
                    (begin
                      (set-box! curr (cons (caar follow) (unbox curr)))
                      (hashtable-set! component (caar follow) curr)))))))
        (do ((comp (hashtable-value-list component) (cdr comp))
             (res '()))
            ((null? comp) res)
          (if (pair? (unbox (car comp)))
              (begin
                (set! res (cons (unbox (car comp)) res))
                (set-box! (car comp) '()))))))
    
    (define-record-type node-props
      (make-node-props ix ll ns)
      node-props?
      (ix node-index node-index-set!)
      (ll node-lowlevel node-lowlevel-set!)
      (ns node-onstack? node-onstack-set!))

    (define (graph-strongly-connected-components graph)
      (let* ((ht      (graph-ref graph))
             (compare (hashtable-equivalence-function ht))
             (meta    (make-same-hashtable ht))
             (s       '())
             (index   0)
             (res     '()))
        (define (connect node)
          (let ((nodep (make-node-props index index #t)))
            (hashtable-set! meta node nodep)
            (set! index (+ index 1))
            (set! s (cons node s))
            (do ((next (hashtable-ref ht node '()) (cdr next)))
                ((null? next))
              (let ((nextp (hashtable-ref meta (caar next) #f)))
                (if nextp
                    (if (node-onstack? nextp)
                        (node-lowlevel-set! nodep
                                            (min (node-lowlevel nodep) (node-index nextp))))
                    (begin
                      (connect (caar next))
                      (node-lowlevel-set!
                        nodep
                        (min (node-lowlevel nodep)
                             (node-lowlevel (hashtable-ref meta (caar next) #f))))))))
            (if (= (node-index nodep) (node-lowlevel nodep))
                (do ((nodes s (cdr nodes))
                     (nextp (hashtable-ref meta (car s) #f) (hashtable-ref meta (cadr nodes) #f))
                     (comp '() (cons (car nodes) comp)))
                    ((compare (car nodes) node)
                     (begin
                       (node-onstack-set! nextp #f)
                       (set! res (cons (cons (car nodes) comp) res))
                       (set! s (cdr nodes))))
                  (node-onstack-set! nextp #f)))))
        (do ((next (hashtable-key-list ht) (cdr next)))
            ((null? next) res)
          (if (not (hashtable-ref meta (car next) #f))
              (connect (car next))))))

    (define (graph-topological-sort graph)
      (let* ((ht    (graph-ref graph))
             (marks (make-same-hashtable ht))
             (res   '()))
        (define (visit node)  ; returns #f if there are cycles
          (let ((mark (hashtable-ref marks node #f)))
            (if mark
                (null? mark)
                (begin
                  (hashtable-set! marks node #t)
                  (if (do ((next (hashtable-ref ht node '()) (cdr next)))
                          ((or (null? next) (not (visit (caar next)))) (null? next)))
                      (begin
                        (hashtable-set! marks node '())
                        (set! res (cons node res))
                        #t)
                      #f)))))
        (do ((next (hashtable-key-list ht) (cdr next)))
            ((or (null? next) (not (visit (car next))))
             (if (null? next) res #f)))))
    
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
    
    (define (shortest-paths graph from to distance)
      (let* ((ht      (graph-ref graph))
             (compare (hashtable-equivalence-function ht))
             (prev    (make-same-hashtable ht))
             (q       (make-same-hashtable ht)))
        ; Compute the next node with minimal distance in q and return pair (distance . node)
        (define (min-dist-node)
          (let ((res (cons fl-greatest #f)))
            (hashtable-for-each
              (lambda (node val)
                (let ((d (cadr (hashtable-get prev node))))
                  (if (< d (car res))
                      (set! res (cons d node)))))
              q)
            (if (cdr res) (hashtable-remove! q (cdr res)))
            res))
        ; Reconstruct path for node following the prev pointers
        (define (path node res)
          (if (compare node from)
              (cons from res)
              (path (cddr (hashtable-get prev node)) (cons node res))))
        ; Initialize the prev table containing, for each node a pair (distance . node)
        ; Mark all nodes as unreached
        (hashtable-for-each
          (lambda (node adj)
            (hashtable-set! prev node (cons fl-greatest #f))
            (hashtable-set! q node #t))
          ht)
        ; Set the starting node as reached
        (hashtable-set! prev from (cons 0.0 #f))
        ; Iterate through all nodes in q
        (do ((next (min-dist-node) (min-dist-node)))
            ((or (not (cdr next)) (and to (compare to (cdr next))))
             (cond ((cdr next)
                     (list (cons (car next) (path to '()))))
                   (to
                     '())
                   (else
                     (let ((res '()))
                       (hashtable-for-each
                         (lambda (node prv)
                           (if (cdr prv)
                               (set! res (cons (cons (car prv) (path (cdr prv) (list node)))
                                               res))))
                         prev)
                       res))))
          (do ((neighbors (cdr (hashtable-get ht (cdr next))) (cdr neighbors)))
              ((null? neighbors))
            (if (hashtable-get q (caar neighbors))
                (let ((dist (+ (car next) (distance (cdr next) (caar neighbors)))))
                  (if (< dist (cadr (hashtable-get prev (caar neighbors))))
                      (hashtable-set! prev (caar neighbors) (cons dist (cdr next))))))))))
    
    (define (graph-shortest-paths graph from . args)
      (let-optionals args ((distance (lambda (x y) 1.0)))
        (shortest-paths graph from #f distance)))
    
    (define (graph-shortest-path graph from to . args)
      (let-optionals args ((distance (lambda (x y) 1.0)))
        (let ((res (shortest-paths graph from to distance)))
          (if (pair? res)
              (values (cdar res) (caar res))
              (values #f #f)))))
    
    (define (make-node-hashtable graph)
      (make-same-hashtable (graph-ref graph)))

    (define (make-same-hashtable ht)
      (cond ((eq-hashtable? ht)    (make-eq-hashtable))
            ((eqv-hashtable? ht)   (make-eqv-hashtable))
            ((equal-hashtable? ht) (make-equal-hashtable))
            (else                  (make-hashtable (hashtable-hash-function ht)
                                                   (hashtable-equivalence-function ht)))))

    (define (assoc-edges ht obj alist)
      (cond ((eq-hashtable? ht)    (assq obj alist))
            ((eqv-hashtable? ht)   (assv obj alist))
            ((equal-hashtable? ht) (assoc obj alist equal))
            (else                  (assoc obj alist (hashtable-equivalence-function ht)))))

    (define (delete-edges ht obj alist)
      (cond ((eq-hashtable? ht)    (alist-delq obj alist))
            ((eqv-hashtable? ht)   (alist-delv obj alist))
            ((equal-hashtable? ht) (alist-delete obj alist equal))
            (else                  (alist-delete obj alist (hashtable-equivalence-function ht)))))

    (define (delete-edges-proc ht)
      (cond ((eq-hashtable? ht)    alist-delq)
            ((eqv-hashtable? ht)   alist-delv)
            ((equal-hashtable? ht) (lambda (obj alist) (alist-delete obj alist equal)))
            (else                  (let ((compare (hashtable-equivalence-function ht)))
                                     (lambda (obj alist) (alist-delete obj alist compare))))))
  )
)
