;;; Sample AVL Tree Library
;;;
;;; This is an example showcasing the usage of libraries to implement abstract data types.
;;; As opposed to the R7RS standard, LispKit allows for the definition of libraries in programs
;;; directly. Such "local libraries" can be defined and imported just like globally installed
;;; libraries in .sld files. They are useful to hide internal functions and to document the
;;; official interface of a datatype.
;;;
;;; The AVL tree library example below implements purely functional AVL trees: inserting or
;;; deleting an element from an AVL tree results in a new AVL tree. Internally, AVL trees are
;;; implemented using algebraic datatypes. Algebraic datatypes usually don't exist in Scheme
;;; implementations. LispKit provides a lightweight algebraic datatype implementation via
;;; library `(lispkit datatype)`.
;;;
;;; The end of the file shows how to use the AVL tree library and access functionality defined
;;; by the library in the same file.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2017 Matthias Zenger. All rights reserved.
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


(define-library (avl-tree)
 
  (export make-avl-tree
          avl-tree?
          avl-empty?
          avl-size
          avl-contains?
          avl-insert
          avl-delete
          avl-tree->list)
 
  (import (lispkit base)
          (lispkit datatype))
 
  (begin
 
    ;; Define data type. AVL trees are implemented as an algebraic datatype with two
    ;; constructors: `empty` and `node`, for representing an empty AVL tree and AVL
    ;; trees of at least one element.
    (define-datatype avl-tree avl-tree?
      (empty)
      (node elem left right height))
 
    ;;; Make a new AVL tree containing the given elements.
    (define (make-avl-tree . elems)
      (if (null? elems)
          empty-tree
          (avl-insert (car elems) (apply make-avl-tree (cdr elems)))))
 
    ;; Constant defining the empty AVL tree.
    (define empty-tree (empty))
 
    ;; Constructor creating an AVL tree with one element.
    (define (make-leaf elem)
      (node elem empty-tree empty-tree 1))
 
    ;; Constructor creating an AVL tree with a left and right subtree.
    (define (make-tree elem left right)
      (node elem left right (fx1+ (max (height left) (height right)))))
 
    ;; Returns the height of the given tree.
    (define (height tree)
      (match tree
        ((empty)         0)
        ((node _ _ _ h)  h)))
 
    ;; Computes the height difference between the left and right subtrees of the given tree.
    (define (height-diff tree)
      (match tree
        ((empty)         0)
        ((node _ l r _)  (fx- (height r) (height l)))))
 
    ;; Returns #t if the given tree is an empty tree.
    (define (avl-empty? tree)
      (match tree
        ((empty)  #t)
        (else     #f)))
 
    ;; Returns the number of elements in the given AVL tree.
    (define (avl-size tree)
      (match tree
        ((empty)        0)
        ((node _ l r _) (fx1+ (fx+ (avl-size l) (avl-size r))))))
 
    ;; Returns #t if the AVL tree contains the given element.
    (define (avl-contains? x tree)
      (match tree
        ((empty)        #f)
        ((node e l r _) (cond ((= x e) #t)
                              ((< x e) (avl-contains? x l))
                              ((> x e) (avl-contains? x r))))))
 
    ;; Returns a balanced version of the given AVL tree.
    (define (avl-balance tree)
      (match tree
        ((empty)         tree)
        ((node _ l r _)  (case (fx- (height r) (height l))
                           ((2)   (if (negative? (height-diff r))
                                      (rotate-right-left tree)
                                      (rotate-right tree)))
                           ((-2)  (if (positive? (height-diff l))
                                      (rotate-left-right tree)
                                      (rotate-left tree)))
                           (else  tree)))))
 
    (define (rotate-left tree)
      (match tree
        ((empty)                         tree)
        ((node e (node le ll lr _) r _)  (make-tree le ll (make-tree e lr r)))))
 
    (define (rotate-right tree)
      (match tree
        ((empty)                         tree)
        ((node e l (node re rl rr _) _)  (make-tree re (make-tree e l rl) rr))))
 
    (define (rotate-left-right tree)
      (match tree
        ((node e (node le ll (node lre lrl lrr _) _) r _)
          (make-tree lre (make-tree le ll lrl) (make-tree e lrr r)))))
 
    (define (rotate-right-left tree)
      (match tree
        ((node e l (node re (node rle rll rlr _) rr _) _)
          (make-tree rle (make-tree e l rll) (make-tree re rlr rr)))))
 
    ;; Returns a new AVL tree with the given element inserted.
    (define (avl-insert x tree)
      (if (avl-contains? x tree) tree (avl-insert-internal x tree)))
 
    (define (avl-insert-internal x tree)
      (match tree
        ((empty)         (make-leaf x))
        ((node e l r h)  (cond ((< x e)  (avl-balance (make-tree e (avl-insert-internal x l) r)))
                               ((> x e)  (avl-balance (make-tree e l (avl-insert-internal x r))))
                               (else     tree)))))
 
    ;; Returns a new AVL tree with the given element removed.
    (define (avl-delete x tree)
      (if (avl-contains? x tree) (avl-delete-internal x tree) tree))
 
    (define (avl-delete-internal x tree)
      (match tree
        ((node e l r _)
          (cond ((< x e) (avl-balance (make-tree e (avl-delete-internal x l) r)))
                ((> x e) (avl-balance (make-tree e l (avl-delete-internal x r))))
                ((= x e) (cond ((avl-empty? r) l)
                               ((avl-empty? l) r)
                               (else           (match (avl-delete-leftmost r)
                                                 ((node lme _ lmr _)  (make-tree lme l lmr))))))))))
 
    (define (avl-delete-leftmost tree)
      (match tree
        ((node _ (empty) _ _) tree)
        ((node e l r _)       (match (avl-delete-leftmost l)
                                ((node lme _ lmr _)
                                  (make-tree lme empty-tree (avl-balance (make-tree e lmr r))))))))
 
    ;; Returns the elements of the given AVL tree as a list in sorted order.
    (define (avl-tree->list tree)
      (match tree
        ((empty)         '())
        ((node e l r _)  (append (avl-tree->list l) (list e) (avl-tree->list r)))))
  )
)

(import (avl-tree))

(define tree1 (make-avl-tree 39 21 99 4 1 19 78 41 21))
(define tree2 (avl-insert 50 tree1))

(display "tree1 is a subset of tree2: ")
(display (every? (lambda (x) (avl-contains? x tree2)) (avl-tree->list tree1)))
(newline)

(define tree3 (avl-delete 21 tree2))

(display "tree1 is a subset of tree3: ")
(display (every? (lambda (x) (avl-contains? x tree3)) (avl-tree->list tree1)))
(newline)

(display "elements in tree1: ")
(display (avl-size tree1))
(newline)

(display "elements in tree2: ")
(display (avl-size tree2))
(newline)

(display "elements in tree3: ")
(display (avl-size tree3))
(newline)
