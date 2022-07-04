;;; LISPKIT WT-TREE
;;;
;;; This is the enhanced (and bug corrected) implementation of weight-balanced binary trees
;;; by Yoichi Hirai and Kazuhiko Yamamoto from https://github.com/kazu-yamamoto/wttree
;;; adapted for R7RS and the use in LispKit.
;;; 
;;; Copyright © 2010 Kazu Yamamoto
;;; 
;;; Reference:
;;;   Yoichi Hirai and Kazuhiko Yamamoto,
;;;   "Balancing weight-balanced trees",
;;;   Journal of Functional Programming, 21(3):287-307, May 2011
;;; 
;;; Copyright © 1993-1994 Stephen Adams
;;; 
;;; References:
;;;   Stephen Adams, Implemeting Sets Efficiently in a Functional
;;;   Language, CSTR 92-10, Department of Electronics and Computer
;;;   Science, University of Southampton, 1992
;;; 
;;; Copyright © 1993-94 Massachusetts Institute of Technology
;;;
;;; This material was developed by the Scheme project at the Massachusetts Institute of
;;; Technology, Department of Electrical Engineering and Computer Science. Permission to
;;; copy and modify this software, to redistribute either the original software or a
;;; modified version, and to use this software for any purpose is granted, subject to the
;;; following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright notice in full.
;;;
;;; 2. Users of this software agree to make their best efforts (a) to return to the
;;;    MIT Scheme project any improvements or extensions that they make, so that these
;;;    may be included in future releases; and (b) to inform MIT of noteworthy uses of
;;;    this software.
;;;
;;; 3. All materials developed as a consequence of the use of this software shall duly
;;;    acknowledge such use, in accordance with the usual standards of acknowledging
;;;    credit in academic research.
;;;
;;; 4. MIT has made no warranty or representation that the operation of this software
;;;    will be error-free, and MIT is under no obligation to provide any services, by
;;;    way of maintenance, update, or otherwise.
;;;
;;; 5. In conjunction with products arising from the use of this material, there shall
;;;    be no use of the name of the Massachusetts Institute of Technology nor of any
;;;    adaptation thereof in any advertising, promotional, or sales literature without
;;;    prior written consent from MIT in each case.
;;; 
;;; Adaptation to LispKit
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(define-library (lispkit wt-tree)

  (export make-wt-tree-type
          make-comparator-wt-tree-type
          wt-tree-type?
          number-wt-type
          string-wt-type
          string-ci-wt-type
          char-wt-type
          char-ci-wt-type)
  
  (export make-wt-tree
          singleton-wt-tree
          alist->wt-tree
          wt-tree?
          wt-tree-type-tag
          wt-tree/copy
          wt-tree/empty?
          wt-tree/size
          wt-tree/add
          wt-tree/delete
          wt-tree/add!
          wt-tree/delete!
          wt-tree/member?
          wt-tree/lookup
          wt-tree/split<
          wt-tree/split>
          wt-tree/union
          wt-tree/union-merge
          wt-tree/intersection
          wt-tree/difference
          wt-tree/subset?
          wt-tree/set-equal?
          wt-tree/fold
          wt-tree/for-each
          wt-tree/index
          wt-tree/index-datum
          wt-tree/index-pair
          wt-tree/rank
          wt-tree/min
          wt-tree/min-datum
          wt-tree/min-pair
          wt-tree/delete-min
          wt-tree/delete-min!
          wt-tree/valid?)

  (import (lispkit base)
          (lispkit comparator))

  (begin

    ;; A tree-type is a collection of those procedures that depend on the ordering relation.
    ;; (written out by hand, using vectors)
    
    (define-values (wt-tree-type-tag new-tree-type wt-tree-type? tree-type-ref make-tree-type-subtype)
                   (make-type 'wt-tree-type))
    
    (define (%make-tree-type key<?
                             alist->tree
                             add
                             insert!
                             delete
                             delete!
                             member?
                             lookup
                             split-lt
                             split-gt
                             union
                             union-merge
                             intersection
                             difference
                             subset?
                             rank)
      (new-tree-type
        (vector key<?
                alist->tree
                add
                insert!
                delete
                delete!
                member?
                lookup
                split-lt
                split-gt
                union
                union-merge
                intersection
                difference
                subset?
                rank)))
    
    (define (tree-type/key<? tt)
      (vector-ref (tree-type-ref tt) 0))
    
    (define (tree-type/alist->tree tt)
      (vector-ref (tree-type-ref tt) 1))
    
    (define (tree-type/add tt)
      (vector-ref (tree-type-ref tt) 2))
    
    (define (tree-type/insert! tt)
      (vector-ref (tree-type-ref tt) 3))
    
    (define (tree-type/delete tt)
      (vector-ref (tree-type-ref tt) 4))
    
    (define (tree-type/delete! tt)
      (vector-ref (tree-type-ref tt) 5))
    
    (define (tree-type/member? tt)
      (vector-ref (tree-type-ref tt) 6))
    
    (define (tree-type/lookup tt)
      (vector-ref (tree-type-ref tt) 7))
    
    (define (tree-type/split-lt tt)
      (vector-ref (tree-type-ref tt) 8))
    
    (define (tree-type/split-gt tt)
      (vector-ref (tree-type-ref tt) 9))
    
    (define (tree-type/union tt)
      (vector-ref (tree-type-ref tt) 10))
    
    (define (tree-type/union-merge  tt)
      (vector-ref (tree-type-ref tt) 11))
    
    (define (tree-type/intersection tt)
      (vector-ref (tree-type-ref tt) 12))
    
    (define (tree-type/difference tt)
      (vector-ref (tree-type-ref tt) 13))
    
    (define (tree-type/subset? tt)
      (vector-ref (tree-type-ref tt) 14))
    
    (define (tree-type/rank tt)
      (vector-ref (tree-type-ref tt) 15))
  )
  
  (begin
    
    ;;  wt-tree is a wrapper for trees of nodes of a certain wt-tree-type.

    (define-values (wt-tree-type new-wt-tree wt-tree? wt-tree-ref make-wt-tree-subtype)
      (make-type 'wt-tree))
    
    (define (%make-wt-tree type root)
      (new-wt-tree (mcons type root)))

    (define (tree/type t)
      (mcar (wt-tree-ref t)))
    
    (define (tree/root t)
      (mcdr (wt-tree-ref t)))
    
    (define (set-tree/root! t v)
      (set-mcdr! (wt-tree-ref t) v))
    
    ;; Nodes are the thing from which the real trees are built.  There are
    ;; lots of these and the uninquisitive user will never see them, so
    ;; they are represented as untagged to save the slot that would be
    ;; used for tagging structures.

    (define (make-node k v l r w)
      (cons (cons l r) (cons k (cons v w))))
    
    (define node/k cadr)
    (define node/v caddr)
    (define node/l caar)
    (define node/r cdar)

    (define empty (string->uninterned-symbol "empty"))
    
    (define (empty? x)
      (eq? x empty))
    
    (define (node/size node)
      (if (empty? node) 0 (cdddr node)))

    (define (node/singleton k v)
      (make-node k v empty empty 1))
    
    (define (with-n-node node receiver)
      (receiver (cadr node) (caddr node) (caar node) (cdar node)))
    
    ;; Constructors for building node trees of various complexity

    (define (n-join k v l r)
      (make-node k v l r (fx1+ (fx+ (node/size l) (node/size r)))))

    (define (single-l a_k a_v x r)
      (with-n-node r
        (lambda (b_k b_v y z) (n-join b_k b_v (n-join a_k a_v x y) z))))

    (define (double-l a_k a_v x r)
      (with-n-node r
        (lambda (c_k c_v r_l z)
          (with-n-node r_l
            (lambda (b_k b_v y1 y2)
              (n-join b_k b_v
                      (n-join a_k a_v x y1)
                      (n-join c_k c_v y2 z)))))))
    
    (define (single-r b_k b_v l z)
      (with-n-node l
        (lambda (a_k a_v x y) (n-join a_k a_v x (n-join b_k b_v y z)))))

    (define (double-r c_k c_v l z)
      (with-n-node l
        (lambda (a_k a_v x l_r)
          (with-n-node l_r
            (lambda (b_k b_v y1 y2)
              (n-join b_k b_v
                      (n-join a_k a_v x y1)
                      (n-join c_k c_v y2 z)))))))
    
    (define wt-tree-delta 3)
    (define wt-tree-gamma 2)
    
    (define (t-join k v l r)
      (let ((l_n (fx1+ (node/size l)))
            (r_n (fx1+ (node/size r))))
        (cond ((fx> r_n (fx* wt-tree-delta l_n))
               ;; right is too big
               (let ((r_l_n (fx1+ (node/size (node/l r))))
                     (r_r_n (fx1+ (node/size (node/r r)))))
                 (if (fx< r_l_n (fx* wt-tree-gamma r_r_n))
                     (single-l k v l r)
                     (double-l k v l r))))
              ((fx> l_n (fx* wt-tree-delta r_n))
               ;; left is too big
               (let ((l_l_n (fx1+ (node/size (node/l l))))
                     (l_r_n (fx1+ (node/size (node/r l)))))
                 (if (fx< l_r_n (fx* wt-tree-gamma l_l_n))
                     (single-r k v l r)
                     (double-r k v l r))))
              (else
                (n-join k v l r)))))

    ;; Error handling
    
    (define (error:empty owner)
      (error "operation requires non-empty tree: $0" owner))
    
    ;; Node tree procedures that are independent of key<?

    (define (node/min node)
      (cond ((empty? node)          (error:empty 'min))
            ((empty? (node/l node)) node)
            (else                   (node/min (node/l node)))))

    (define (node/delmin node)
      (cond ((empty? node)          (error:empty 'delmin))
            ((empty? (node/l node)) (node/r node))
            (else                   (t-join (node/k node)
                                            (node/v node)
                                            (node/delmin (node/l node))
                                            (node/r node)))))

    (define (node/concat2 node1 node2)
      (cond ((empty? node1) node2)
            ((empty? node2) node1)
            (else           (let ((min-node (node/min node2)))
                              (t-join (node/k min-node)
                                      (node/v min-node)
                                      node1
                                      (node/delmin node2))))))

    (define (node/inorder-fold procedure base node)
      (define (fold base node)
        (if (empty? node)
            base
            (with-n-node node
              (lambda (k v l r)
                (fold (procedure k v (fold base r)) l)))))
      (fold base node))

    (define (node/for-each procedure node)
      (if (not (empty? node))
          (with-n-node node
            (lambda (k v l r)
              (node/for-each procedure l)
              (procedure k v)
              (node/for-each procedure r)))))

    (define (node/height node)
      (if (empty? node)
          0
          (fx1+ (fxmax (node/height (node/l node)) (node/height (node/r node))))))

    (define (node/index node index)
      (define (loop node index)
        (let ((size_l (node/size (node/l node))))
          (cond ((fx< index size_l) (loop (node/l node) index))
                ((fx> index size_l) (loop (node/r node) (fx- index (fx1+ size_l))))
                (else               node))))
      (let ((bound (node/size node)))
        (if (or (not (fixnum? index))
                (fx< index 0)
                (fx>= index bound))
          (error "$1: index $0 outside of supported range" index 'node/index)
          (loop node index))))

    (define (local:make-wt-tree-type key<?)
      (define my-type #f)
      
      (define (key>? x y)
        (key<? y x))
      
      (define node/find
        (case-lambda
          ((k node)
            (node/find k node #f))
          ((k node best)
            (cond ((empty? node)
                    (if (or (not best) (key<? (node/k best) k))
                        #f
                        best))
                  ((key<? k (node/k node))
                    (node/find k (node/l node) best))
                  (else
                    (node/find k (node/r node) node))))))
      
      (define (node/find k node)
        ;; Returns either the node or #f.
        ;; Loop takes D comparisons where D is the depth of the tree
        ;; rather than the traditional compare-low, compare-high which
        ;; takes on average 1.5(D-1) comparisons
        (define (loop this best)
          (cond ((empty? this)           best)
                ((key<? k (node/k this)) (loop (node/l this) best))
                (else                    (loop (node/r this) this))))
        (let ((best (loop node #f)))
          (cond ((not best)              #f)
                ((key<? (node/k best) k) #f)
                (else                    best))))
      
      (define (node/rank k node rank)
        (cond ((empty? node)           #f)
              ((key<? k (node/k node)) (node/rank k (node/l node) rank))
              ((key>? k (node/k node)) (node/rank k
                                                  (node/r node)
                                                  (fx1+ (fx+ rank (node/size (node/l node))))))
              (else                    (fx+ rank (node/size (node/l node))))))
      
      (define (node/add node k v)
        (if (empty? node)
            (node/singleton k v)
            (with-n-node node
              (lambda (key val l r)
                (cond ((key<? k key) (t-join key val (node/add l k v) r))
                      ((key<? key k) (t-join key val l (node/add r k v)))
                      (else          (n-join key v l r)))))))
      
      (define (node/delete x node)
        (if (empty? node)
            empty
            (with-n-node node
              (lambda (key val l r)
                (cond ((key<? x key) (t-join key val (node/delete x l) r))
                      ((key<? key x) (t-join key val l (node/delete x r)))
                      (else          (node/concat2 l r)))))))

      (define (node/concat tree1 tree2)
        (cond ((empty? tree1) tree2)
              ((empty? tree2) tree1)
              (else           (let ((min-node (node/min tree2)))
                                (node/concat3 (node/k min-node)
                                              (node/v min-node)
                                              tree1
                                              (node/delmin tree2))))))

      (define (node/concat3 k v l r)
        (cond ((empty? l) (node/add r k v))
              ((empty? r) (node/add l k v))
              (else       (let ((n1 (fx+ (node/size l) 1))
                                (n2 (fx+ (node/size r) 1)))
                            (cond ((fx< (fx* wt-tree-delta n1) n2)
                                    (with-n-node r
                                      (lambda (k2 v2 l2 r2)
                                        (t-join k2 v2 (node/concat3 k v l l2) r2))))
                                  ((fx< (fx* wt-tree-delta n2) n1)
                                    (with-n-node l
                                      (lambda (k1 v1 l1 r1)
                                        (t-join k1 v1 l1 (node/concat3 k v r1 r)))))
                                  (else
                                    (n-join k v l r)))))))

      (define (node/split-lt node x)
        (cond ((empty? node)
                empty)
              ((key<? x (node/k node))
                (node/split-lt (node/l node) x))
              ((key<? (node/k node) x)
                (node/concat3 (node/k node)
                              (node/v node)
                              (node/l node)
                              (node/split-lt (node/r node) x)))
              (else
                (node/l node))))

      (define (node/split-gt node x)
        (cond ((empty? node)
                empty)
              ((key<? (node/k node) x)
                (node/split-gt (node/r node) x))
              ((key<? x (node/k node))
                (node/concat3 (node/k node)
                              (node/v node)
                              (node/split-gt (node/l node) x) (node/r node)))
              (else
                (node/r node))))

      (define (node/union tree1 tree2)
        (cond ((empty? tree1) tree2)
              ((empty? tree2) tree1)
              (else           (with-n-node tree2
                                (lambda (ak av l r)
                                  (let ((l1  (node/split-lt tree1 ak))
                                        (r1  (node/split-gt tree1 ak)))
                                    (node/concat3 ak av (node/union l1 l) (node/union r1 r))))))))

      (define (node/union-merge tree1 tree2 merge)
        (cond ((empty? tree1) tree2)
              ((empty? tree2) tree1)
              (else           (with-n-node tree2
                                (lambda (ak av l r)
                                  (let* ((node1 (node/find ak tree1))
                                         (l1    (node/split-lt tree1 ak))
                                         (r1    (node/split-gt tree1 ak))
                                         (value (if node1 (merge ak av (node/v node1)) av)))
                                    (node/concat3 ak
                                                  value
                                                  (node/union-merge l1 l merge)
                                                  (node/union-merge r1 r merge))))))))

      (define (node/difference tree1 tree2)
        (cond ((empty? tree1) empty)
              ((empty? tree2) tree1)
              (else           (with-n-node tree2
                                (lambda (ak av l r)
                                  (let ((l1  (node/split-lt tree1 ak))
                                        (r1  (node/split-gt tree1 ak)))
                                    (node/concat (node/difference l1 l)
                                                 (node/difference r1 r))))))))

      (define (node/intersection tree1 tree2)
        (cond ((empty? tree1) empty)
              ((empty? tree2) empty)
              (else           (with-n-node tree2
                                (lambda (ak av l r)
                                  (let ((l1  (node/split-lt tree1 ak))
                                        (r1  (node/split-gt tree1 ak)))
                                    (if (node/find ak tree1)
                                        (node/concat3 ak
                                                      av
                                                      (node/intersection l1 l)
                                                      (node/intersection r1 r))
                                        (node/concat (node/intersection l1 l)
                                                     (node/intersection r1 r)))))))))
   
      (define (node/subset? tree1 tree2)
        (or (empty? tree1)
            (and (fx<= (node/size tree1) (node/size tree2))
                 (with-n-node tree1
                   (lambda (k v l r)
                     (cond ((key<? k (node/k tree2))
                             (and (node/subset? l (node/l tree2))
                                  (node/find k tree2)
                                  (node/subset? r tree2)))
                           ((key>? k (node/k tree2))
                             (and (node/subset? r (node/r tree2))
                                  (node/find k tree2)
                                  (node/subset? l tree2)))
                           (else
                             (and (node/subset? l (node/l tree2))
                                  (node/subset? r (node/r tree2))))))))))
  
      ;; Tree interface: stripping off or injecting the tree types

      (define (tree/map-add tree k v)
        (%make-wt-tree (tree/type tree) (node/add (tree/root tree) k v)))

      (define (tree/insert! tree k v)
        (set-tree/root! tree (node/add (tree/root tree) k v)))

      (define (tree/delete tree k)
        (%make-wt-tree (tree/type tree) (node/delete k (tree/root tree))))

      (define (tree/delete! tree k)
        (set-tree/root! tree (node/delete k (tree/root tree))))

      (define (tree/split-lt tree key)
        (%make-wt-tree (tree/type tree) (node/split-lt (tree/root tree) key)))

      (define (tree/split-gt tree key)
        (%make-wt-tree (tree/type tree) (node/split-gt (tree/root tree) key)))

      (define (tree/union tree1 tree2)
        (%make-wt-tree (tree/type tree1) (node/union (tree/root tree1) (tree/root tree2))))

      (define (tree/union-merge tree1 tree2 merge)
        (%make-wt-tree (tree/type tree1)
                       (node/union-merge (tree/root tree1) (tree/root tree2) merge)))

      (define (tree/intersection tree1 tree2)
        (%make-wt-tree (tree/type tree1)
                       (node/intersection (tree/root tree1) (tree/root tree2))))

      (define (tree/difference tree1 tree2)
        (%make-wt-tree (tree/type tree1)
                       (node/difference (tree/root tree1) (tree/root tree2))))

      (define (tree/subset? tree1 tree2)
        (node/subset? (tree/root tree1) (tree/root tree2)))

      (define (alist->tree-loop alist node)
        (cond ((null? alist)
                node)
              ((pair? alist)
                (alist->tree-loop (cdr alist) (node/add node (caar alist) (cdar alist))))
              (else
                (error "$1: $0 is not a valid alist node" alist 'alist->tree))))

      (define (alist->tree alist)
        (%make-wt-tree my-type (alist->tree-loop alist empty)))

      (define (tree/get tree key default)
        (let ((node (node/find key (tree/root tree))))
          (if node (node/v node) default)))

      (define (tree/rank tree key)
        (node/rank key (tree/root tree) 0))

      (define (tree/member? key tree)
        (and (node/find key (tree/root tree)) #t))

      (set! my-type
        (%make-tree-type key<?                        ;  key<?
                         alist->tree                  ;  alist->tree
                         tree/map-add                 ;  add
                         tree/insert!                 ;  insert!
                         tree/delete                  ;  delete
                         tree/delete!                 ;  delete!
                         tree/member?                 ;  member?
                         tree/get                     ;  lookup
                         tree/split-lt                ;  split-lt
                         tree/split-gt                ;  split-gt
                         tree/union                   ;  union
                         tree/union-merge             ;  union-merge
                         tree/intersection            ;  intersection
                         tree/difference              ;  difference
                         tree/subset?                 ;  subset?
                         tree/rank))                  ;  rank
      my-type)

    (define (guarantee-tree tree procedure)
      (if (not (wt-tree? tree))
          (error "$1: $0 is not a valid wt-tree" tree procedure)))

    (define (guarantee-tree-type type procedure)
      (if (not (wt-tree-type? type))
          (error "$1: $0 is not a valid wt-tree-type" type procedure)))

    (define (guarantee-compatible-trees tree1 tree2 procedure)
      (guarantee-tree tree1 procedure)
      (guarantee-tree tree2 procedure)
      (if (not (eq? (tree/type tree1) (tree/type tree2)))
        (error "the trees $0 and $1 have incompatible types: $2 vs $3"
               tree1 tree2 (tree/type tree1) (tree/type tree2))))

    (define (valid? tree)
      (let ((root (tree/root tree)))
        (and (balanced? root) (ordered? root))))

    (define (is-balanced a b)
      (let ((x (fx1+ (node/size a)))
            (y (fx1+ (node/size b))))
        (fx<= y (fx* wt-tree-delta x))))

    (define (balanced? n)
      (or (empty? n)
          (let ((l (node/l n))
                (r (node/r n)))
            (and (is-balanced l r) (is-balanced r l) (balanced? l) (balanced? r)))))

    (define (true x) #t)

    (define (is-ordered lo hi m)
      (or (empty? m)
          (let ((k (node/k m))
                (l (node/l m))
                (r (node/r m)))
            (and (lo k) (hi k)
                 (is-ordered lo (lambda (x) (< x k)) l)
                 (is-ordered (lambda (x) (< k x)) hi r)))))

    (define (ordered? n)
      (is-ordered true true n))
  )
  
  ;; Exported procedures
  
  (begin
    
    (define make-wt-tree-type local:make-wt-tree-type)

    (define (make-comparator-wt-tree-type comp)
      (local:make-wt-tree-type (comparator-ordering-predicate comp)))

    (define number-wt-type
      (local:make-wt-tree-type  <))
    
    (define string-wt-type
      (local:make-wt-tree-type  string<?))
    
    (define string-ci-wt-type
      (local:make-wt-tree-type  string-ci<?))
    
    (define char-wt-type
      (local:make-wt-tree-type  char<?))
    
    (define char-ci-wt-type
      (local:make-wt-tree-type  char-ci<?))
    
    (define make-wt-tree
      (case-lambda (()
                     (%make-wt-tree number-wt-type empty))
                   ((type)
                     (guarantee-tree-type type 'make-wt-tree)
                     (%make-wt-tree type empty))))
        
    (define singleton-wt-tree
      (case-lambda ((key value)
                     (%make-wt-tree number-wt-type (node/singleton key value)))
                   ((type key value)
                     (guarantee-tree-type type 'singleton-wt-tree)
                     (%make-wt-tree type (node/singleton key value)))))

    (define (alist->wt-tree type alist)
      (guarantee-tree-type type 'alist->wt-tree)
      ((tree-type/alist->tree type) alist))
 
    (define (wt-tree/copy tree)
      (guarantee-tree tree 'wt-tree/copy)
      (%make-wt-tree (tree/type tree) (tree/root tree)))

    (define (wt-tree/empty? tree)
      (guarantee-tree tree 'wt-tree/empty?)
      (empty? (tree/root tree)))

    (define (wt-tree/size tree)
      (guarantee-tree tree 'wt-tree/size)
      (node/size (tree/root tree)))

    (define (wt-tree/add tree key datum)
      (guarantee-tree tree 'wt-tree/add)
      ((tree-type/add (tree/type tree)) tree key datum))

    (define (wt-tree/delete tree key)
      (guarantee-tree tree 'wt-tree/delete)
      ((tree-type/delete (tree/type tree)) tree key))

    (define (wt-tree/add! tree key datum)
      (guarantee-tree tree 'wt-tree/add!)
      ((tree-type/insert! (tree/type tree)) tree key datum))

    (define (wt-tree/delete! tree key)
      (guarantee-tree tree 'wt-tree/delete!)
      ((tree-type/delete! (tree/type tree)) tree key))

    (define (wt-tree/member? key tree)
      (guarantee-tree tree 'wt-tree/member?)
      ((tree-type/member? (tree/type tree)) key tree))

    (define (wt-tree/lookup tree key default)
      (guarantee-tree tree 'wt-tree/lookup)
      ((tree-type/lookup (tree/type tree)) tree key default))

    (define (wt-tree/split< tree key)
      (guarantee-tree tree 'wt-tree/split<)
      ((tree-type/split-lt (tree/type tree)) tree key))

    (define (wt-tree/split> tree key)
      (guarantee-tree tree 'wt-tree/split>)
      ((tree-type/split-gt (tree/type tree)) tree key))

    (define (wt-tree/union tree1 tree2)
      (guarantee-compatible-trees tree1 tree2 'wt-tree/union)
      ((tree-type/union (tree/type tree1)) tree1 tree2))

    (define (wt-tree/union-merge tree1 tree2 merge)
      (guarantee-compatible-trees tree1 tree2 'wt-tree/union-merge)
      ((tree-type/union-merge (tree/type tree1)) tree1 tree2 merge))

    (define (wt-tree/intersection tree1 tree2)
      (guarantee-compatible-trees tree1 tree2 'wt-tree/intersection)
      ((tree-type/intersection (tree/type tree1)) tree1 tree2))

    (define (wt-tree/difference tree1 tree2)
      (guarantee-compatible-trees tree1 tree2 'wt-tree/difference)
      ((tree-type/difference (tree/type tree1)) tree1 tree2))

    (define (wt-tree/subset? tree1 tree2)
      (guarantee-compatible-trees tree1 tree2 'wt-tree/subset?)
      ((tree-type/subset? (tree/type tree1)) tree1 tree2))

    (define (wt-tree/set-equal? tree1 tree2)
      (and (wt-tree/subset? tree1 tree2)
           (wt-tree/subset? tree2 tree1)))

    (define (wt-tree/fold combiner-key-datum-result init tree)
      (guarantee-tree tree 'wt-tree/fold)
      (node/inorder-fold combiner-key-datum-result init (tree/root tree)))

    (define (wt-tree/for-each action-key-datum tree)
      (guarantee-tree tree 'wt-tree/for-each)
      (node/for-each action-key-datum (tree/root tree)))

    (define (wt-tree/index tree index)
      (guarantee-tree tree 'wt-tree/index)
      (let ((node  (node/index (tree/root tree) index)))
        (and node (node/k node))))

    (define (wt-tree/index-datum tree index)
      (guarantee-tree tree 'wt-tree/index-datum)
      (let ((node  (node/index (tree/root tree) index)))
        (and node (node/v node))))

    (define (wt-tree/index-pair tree index)
      (guarantee-tree tree 'wt-tree/index-pair)
      (let ((node  (node/index (tree/root tree) index)))
        (and node (cons (node/k node) (node/v node)))))

    (define (wt-tree/rank tree key)
      (guarantee-tree tree 'wt-tree/rank)
      ((tree-type/rank (tree/type tree)) tree key))

    (define (wt-tree/min tree)
      (guarantee-tree tree 'wt-tree/min)
      (node/k (node/min (tree/root tree))))

    (define (wt-tree/min-datum tree)
      (guarantee-tree tree 'wt-tree/min-datum)
      (node/v (node/min (tree/root tree))))

    (define (wt-tree/min-pair tree)
      (guarantee-tree tree 'wt-tree/min-pair)
      (let ((node  (node/min (tree/root tree))))
        (cons (node/k node) (node/v node))))

    (define (wt-tree/delete-min tree)
      (guarantee-tree tree 'wt-tree/delete-min)
      (%make-wt-tree (tree/type tree) (node/delmin (tree/root tree))))

    (define (wt-tree/delete-min! tree)
      (guarantee-tree tree 'wt-tree/delete-min!)
      (set-tree/root! tree (node/delmin (tree/root tree))))
    
    (define (wt-tree/valid? tree)
      (guarantee-tree tree 'wt-tree/valid?)
      (valid? tree))
  )
)
