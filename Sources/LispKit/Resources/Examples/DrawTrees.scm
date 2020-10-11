;;; Draw binary trees
;;;
;;; This program implements a layout procedure for binary trees. It is based on the article
;;; "Tidier Drawing of Trees" by Edward M. Reingold and John S. Tilford in IEEE Transactions
;;; on Software Engineering, Vol 7, #2, March 1981.
;;; 
;;; The layout algorithm, called TR, guarantees a number of aesthetical properties:
;;;   1. Nodes at the same depth of a tree appear at the same vertical distance from
;;;      the top of the tree,
;;;   2. Parent nodes appear centered on top of their children, which are placed from
;;;      left to right,
;;;   3. Isomorphic subtrees are drawn the same way, no matter where they appear in the
;;;      tree, and
;;;   4. Trees and their mirrors produce symmetrical tree layouts (where mirrored tree
;;;      structures mirror each other in their representation).
;;; 
;;; Within those constraints, the algorithm attempts to make a tree layout as narrow as
;;; possible, even to the point that one subtree of a given node might cross under another
;;; sub-tree. The algorithm doesn't always find the optimal solution (the problem is NP
;;; hard) but it operates in linear time and typically results in aesthetically pleasing
;;; tree layouts.
;;; 
;;; Binary trees are represented as s-expressions. An inner node of a binary tree is
;;; represented by a list with tree elements: `(<label> <left tree> <right tree>)`. A leaf
;;; node is just a label. For example: `(1 (2 3 (4 5 #f)) (6 #f 7))` represents this binary tree:
;;; 
;;;              1
;;;            /   \
;;;           2     6
;;;          / \     \
;;;         3   4     7
;;;            /
;;;           5
;;; 
;;; The following call of procedure `save-tree-drawing` will store a drawing of this tree
;;; in a PDF file at file path `~/Desktop/DemoTree.pdf`.
;;;   (save-tree-drawing "~/Desktop/TestTree.pdf" '(1 (2 3 (4 5 #f)) (6 #f 7)))
;;; 
;;; Here are a few more layouts of example trees:
;;;   (save-tree-drawing "~/Desktop/TestTree1.pdf" test-tree-1)
;;;   (save-tree-drawing "~/Desktop/TestTree2.pdf" test-tree-2)
;;;   (save-tree-drawing "~/Desktop/TestTree3.pdf" test-tree-3)
;;;   (save-tree-drawing "~/Desktop/TestTree4.pdf" test-tree-4)
;;;   (save-tree-drawing "~/Desktop/TestTree5.pdf" test-tree-5)
;;; 
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2020 Matthias Zenger. All rights reserved.
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

(import (lispkit base)
        (lispkit draw))

;; Node representation

(define-record-type <node>
  (node info left right x y offset thread)
  node?
  (info node-info)
  (left node-left set-node-left!)
  (right node-right set-node-right!)
  (x node-x set-node-x!)
  (y node-y set-node-y!)
  (offset node-offset set-node-offset!)
  (thread node-thread? set-node-thread!))

(define (make-node info left right)
  (node info left right 0 0 0 #f))

;; Representation of "extreme" nodes to trace contours

(define-record-type <extreme>
  (extreme node off lev)
  extreme?
  (node extreme-node set-extreme-node!)
  (off extreme-off set-extreme-off!)
  (lev extreme-lev set-extreme-lev!))

(define (copy-extreme! from to)
  (set-extreme-node! to (extreme-node from))
  (set-extreme-off! to (extreme-off from))
  (set-extreme-lev! to (extreme-lev from)))

(define (make-extreme)
  (extreme #f 0 0))

;; Placement algorithm based on procedure "Setup" from "Tidier Drawing of Trees"
;; by Edward M. Reingold and John S. Tilford.
;; This is a 1:1 re-implementation of the Pascal procedure in Scheme.

(define (place-nodes node level rmost lmost minsep)
  (cond
    (node                             ; this is a valid node
       (set-node-y! node level)
       (let ((l (node-left node))     ; follows contour of left subtree
             (r (node-right node))    ; follows contour of right subtree
             (lr (make-extreme))      ; right-most node on lowest level of left subtree
             (ll (make-extreme))      ; left-most node on lowest level of left subtree
             (rr (make-extreme))      ; right-most node on lowest level of right subtree
             (rl (make-extreme)))     ; left-most node on lowest level of left subtree
         (place-nodes l (fx1+ level) lr ll minsep)
         (place-nodes r (fx1+ level) rr rl minsep)
         (cond
           ((and (not r) (not l))     ; node is a leaf
             (set-extreme-node! rmost node)
             (set-extreme-node! lmost node)
             (set-extreme-lev! rmost level)
             (set-extreme-lev! lmost level)
             (set-extreme-off! rmost 0)
             (set-extreme-off! lmost 0)
             (set-node-offset! node 0))
           (else                      ; node is an inner node
             (let ((cursep minsep)    ; set up for subtree pushing; place roots of subtrees
                   (rootsep minsep)   ; at minimum distance apart
                   (loffsum 0)
                   (roffsum 0))
               (do ()
                   ((or (not l) (not r)))
                 (when (fx< cursep minsep)  ; push?
                   (set! rootsep (fx+ rootsep (fx- minsep cursep)))
                   (set! cursep minsep))
                 (cond
                   ((node-right l)
                     (set! loffsum (fx+ loffsum (node-offset l)))
                     (set! cursep (fx- cursep (node-offset l)))
                     (set! l (node-right l)))
                   (else
                     (set! loffsum (fx- loffsum (node-offset l)))
                     (set! cursep (fx+ cursep (node-offset l)))
                     (set! l (node-left l))))
                 (cond
                   ((node-left r)
                     (set! roffsum (fx- roffsum (node-offset r)))
                     (set! cursep (fx- cursep (node-offset r)))
                     (set! r (node-left r)))
                   (else
                     (set! roffsum (fx+ roffsum (node-offset r)))
                     (set! cursep (fx+ cursep (node-offset r)))
                     (set! r (node-right r)))))
               ; set the offset in node and include it in accumulated offsets for l and r
               (set-node-offset! node (fx/ (fx1+ rootsep) 2))
               (set! loffsum (fx- loffsum (node-offset node)))
               (set! roffsum (fx+ roffsum (node-offset node)))
               ; update extreme descendants information
               (cond
                 ((or (fx> (extreme-lev rl) (extreme-lev ll)) (not (node-left node)))
                   (copy-extreme! rl lmost)
                   (set-extreme-off! lmost (fx+ (extreme-off lmost) (node-offset node))))
                 (else
                   (copy-extreme! ll lmost)
                   (set-extreme-off! lmost (fx- (extreme-off lmost) (node-offset node)))))
               (cond
                 ((or (fx> (extreme-lev rl) (extreme-lev rr)) (not (node-right node)))
                   (copy-extreme! lr rmost)
                   (set-extreme-off! rmost (fx- (extreme-off rmost) (node-offset node))))
                 (else
                   (copy-extreme! rr rmost)
                   (set-extreme-off! rmost (fx+ (extreme-off rmost) (node-offset node)))))
               ; if subtrees of node were of uneven heights, check to see if threading is
               ; necessary; at most one thread needs to be inserted
               (cond
                 ((and l (not (eq? l (node-left node))))
                   (set-node-thread! (extreme-node rr) #t)
                   (set-node-offset! (extreme-node rr)
                     (fxabs (fx- (fx+ (extreme-off rr) (node-offset node)) loffsum)))
                   (if (fx<= (fx- loffsum (node-offset node)) (extreme-off rr))
                       (set-node-left! (extreme-node rr) l)
                       (set-node-right! (extreme-node rr) l)))
                 ((and r (not (eq? r (node-right node))))
                   (set-node-thread! (extreme-node ll) #t)
                   (set-node-offset! (extreme-node ll)
                     (fxabs (fx- (fx- (extreme-off ll) (node-offset node)) roffsum)))
                   (if (fx>= (fx+ roffsum (node-offset node)) (extreme-off ll))
                       (set-node-right! (extreme-node ll) r)
                       (set-node-left! (extreme-node ll) r)))))))))
    (else                             ; this is not a valid node
      (set-extreme-lev! lmost -1)
      (set-extreme-lev! rmost -1)))
    node)

;; `finalize-placement` corresponds to Pascal procedure `Petrify`. It converts the
;; relative positionings determined by `place-nodes` to absolute coordinates.

(define (finalize-placement node dx)
  (when node
    (set-node-x! node dx)
    (when (node-thread? node)
      (set-node-thread! node #f)
      (set-node-right! node #f)
      (set-node-left! node #f))
    (finalize-placement (node-left node) (fx- dx (node-offset node)))
    (finalize-placement (node-right node) (fx+ dx (node-offset node)))
    node))

;; `layout-tree` takes a binary tree `xs` in list form and a `minsep` parameter which defines
;; the minimum number of x-coordinates to separate subtrees and returns a `<node>` representation
;; of the graph with absolute coordinates set. The root of `xs` is always rendered at x
;; coordinate 0. The left subtree has negative x coordinates, the right subtree has positive
;; coordinates.

(define (layout-tree xs minsep)
  (finalize-placement (place-nodes (list->node xs) 0 (make-extreme) (make-extreme) minsep) 0))

;; `tree-dimensions` returns three values: the minimum x coordinate, the maximum x coordinate,
;; and the maximum y coordinate.

(define (tree-dimensions node)
  (let* ((xmin (node-x node))
         (xmax xmin)
         (ymax (node-y node)))
    (when (node-left node)
      (let-values (((lxmin lxmax lymax) (tree-dimensions (node-left node))))
        (if (< lxmin xmin) (set! xmin lxmin))
        (if (> lxmax xmax) (set! xmax lxmax))
        (if (> lymax ymax) (set! ymax lymax))))
    (when (node-right node)
      (let-values (((rxmin rxmax rymax) (tree-dimensions (node-right node))))
        (if (< rxmin xmin) (set! xmin rxmin))
        (if (> rxmax xmax) (set! xmax rxmax))
        (if (> rymax ymax) (set! ymax rymax))))
    (values xmin xmax ymax)))

;; Convert binary trees in list form into `<node>` objects. A binary tree in list form is an
;; s-expression in which a list with three elements `(<label> <left tree> <right tree>)`
;; represents a node with two subtrees `<left tree>` and `<right tree>`. Atoms represent labels
;; of leaves.
;; 
;; For example: `(1 (2 3 4) 5)` represents this binary tree:
;; 
;;              1
;;            /   \
;;           2     5
;;          / \
;;         3   4

(define (list->node xs)
  (cond ((pair? xs) (make-node (car xs) (list->node (cadr xs)) (list->node (caddr xs))))
        (xs         (make-node xs #f #f))
        (else       #f)))

;; `node->list` returns a list representation for the given `<node>` object

(define (node->list node)
  (if node
      (if (or (node-left node) (node-right node))
          (list (node-info node) (node->list (node-left node)) (node->list (node-right node)))
          (node-info node))
      #f))

;; The remaining program deals with drawing binary trees using library `(lispkit draw)`.

; The font used to render the label
(define node-label-font (make-parameter (font "Helvetica" 11)))

; The radius of the circle representing a node
(define node-radius (make-parameter 12))

(define (display-to-string x)
  (call-with-output-string (lambda (p) (display x p))))

(define (coord x y fx fy dx dy)
  (point (fx+ (fx* x fx) dx) (fx+ (fx* y fy) dy)))

(define (draw-node-line from to fx fy dx dy)
  (draw-line (coord (node-x from) (node-y from) fx fy dx dy)
             (coord (node-x to) (node-y to) fx fy dx dy)))

(define (draw-tree-node node fx fy dx dy)
  (when (node-left node)
    (draw-node-line node (node-left node) fx fy dx dy)
    (draw-tree-node (node-left node) fx fy dx dy))
  (when (node-right node)
    (draw-node-line node (node-right node) fx fy dx dy)
    (draw-tree-node (node-right node) fx fy dx dy))
  (let* ((radius (node-radius))
         (label  (display-to-string (node-info node)))
         (lsize  (text-size label (node-label-font)))
         (center (coord (node-x node) (node-y node) fx fy dx dy))
         (bound  (rect (move-point center (- radius) (- radius))
                       (size (* radius 2) (* radius 2)))))
    (fill-ellipse bound)
    (draw-ellipse bound)
    (draw-text label
               (move-point center (* (size-width lsize) -0.5) (* (size-height lsize) -0.5))
               (node-label-font)
               blue)
    (set-fill-color white)
    (set-color black)))

;; Draws the `<node>` object `node` scaling up x coordinates via `fx` and y coordinates via
;; `fy` and shifting x coordinates by `dx` and y coordinates by `dy`.

(define (draw-tree node fx fy dx dy)
  (drawing
    (set-fill-color white)
    (set-color black)
    (draw-tree-node node fx fy dx dy)))

(define (save-tree-drawing path xs . args)
  (let-optionals args ((fx 34)
                       (fy 45)
                       (pad 30))
    (let ((node (layout-tree xs 2)))
      (let-values (((xmin xmax ymax) (tree-dimensions node)))
        (save-drawing path
                      (draw-tree node fx fy (- pad (* xmin fx)) pad)
                      (size (+ (* (- xmax xmin) fx) pad pad) (+ (* ymax fy) pad pad)))))))

;; Test trees

(define test-tree-1
  '(1 (2 (3 (4 (5 6 (7 8 (9 10 (11 12 13)))) 14) 15) 16)
      (17 18 (19 20 (21 22 (23 (24 (25 (26 27 28) 29) 30) 31))))))

(define test-tree-2
  '(1 (2 (3 (4 (5 6 (7 8 (9 10 (11 12 13)))) 14) 15) (16 32 (33 34 (35 36 37))))
      (17 18 (19 20 (21 22 (23 (24 (25 (26 27 28) 29) 30) 31))))))

(define test-tree-3
  '(1 (2 (3 (4 (5 6 (7 8 (9 10 (11 12 13)))) 14) 15) (16 32 (33 34 (35 36 (37 38 39)))))
      (17 (18 40 (41 42 43)) (19 20 (21 22 (23 (24 (25 (26 27 28) 29) 30) 31))))))

(define test-tree-4
  '(1 (2 #f (3 #f (4 #f 5))) (6 #f (7 (8 (9 (10 (11 12 #f) #f) #f) #f) #f))))

(define test-tree-5
  '(1 (2 3 (4 #f (5 #f (6 #f 7)))) (5 (6 7 8) 7)))
