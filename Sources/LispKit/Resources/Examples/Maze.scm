;;; Maze generation
;;;
;;; This is a slightly bigger example which implements a maze generation framework and
;;; two sample algorithms which generate random mazes.
;;;
;;; The code is organized in two parts: the framework for representing and generating
;;; mazes is implemented in terms of a library `(lispkit example maze)`. It defines a
;;; new datatype `maze` which implements a maze of given dimensions as a vector of fixnum
;;; values. Conceptually, a maze is a board of cells. Each cell has up to four walls in
;;; the directions towards `north`, `south`, `east`, and `west`. In addition, each cell
;;; provides a 60-bit fixnum value for representing custom flags.
;;;
;;; The second part of the code imports the library and implements two algorithms for
;;; generating random mazes: one is based on a randomized backtracking approach doing
;;; a depth first search, the second one implements a very simple approach by generating
;;; a random binary tree.
;;;
;;; Mazes can be either printed onto a terminal via function `display-maze` or there is
;;; a simple function `save-maze` which draws a maze into a LispKit drawing which is then
;;; saved into a PDF file.
;;;
;;; Example usage:
;;;   (display-maze (make-maze/randomized-dfs 15 15))
;;;   (save-maze "Maze-DFS.pdf" (make-maze/randomized-dfs 40 40) 10 10)
;;;   (save-maze "Maze-BinTree.pdf" (make-maze/bintree 50 50) 8 8)
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

;; Library providing functionality for creating, representing and manipulating data structures
;; for mazes, maze cells, and directions.

(define-library (lispkit example maze)

  (export west
          east
          north
          south
          all-directions
          direction-west?
          direction-east?
          direction-north?
          direction-south?
          invert-direction
          mirror-direction)

  (export cell
          cell-x
          cell-y)

  (export maze?
          maze-type-tag
          make-maze
          maze-width
          maze-height
          maze-bounds
          maze-cell-in-bounds?
          maze-cell-id
          maze-cell-id->cell
          maze-cell-walls
          maze-has-wall?
          maze-has-no-wall?
          set-maze-cell-walls!
          maze-add-wall!
          maze-remove-wall!
          maze-add-border-walls!
          maze-cell-flags
          set-maze-cell-flags!
          maze-cell-unvisited?
          maze-for-each-neighbor-cell
          display-maze
          maze->shape)

  (import (lispkit base)
          (lispkit draw))

  (begin
    (define west 1)
    (define east 2)
    (define north 4)
    (define south 8)
    (define all-directions 15)

    (define (direction-west? dir)
      (not (fxzero? (fxand dir west))))

    (define (direction-east? dir)
      (not (fxzero? (fxand dir east))))

    (define (direction-north? dir)
      (not (fxzero? (fxand dir north))))

    (define (direction-south? dir)
      (not (fxzero? (fxand dir south))))

    (define (invert-direction dir)
      (fxxor dir all-directions))

    (define (mirror-direction dir)
      (fxior
        (fxior (if (direction-west? dir) east 0)
               (if (direction-east? dir) west 0))
        (fxior (if (direction-north? dir) south 0)
               (if (direction-south? dir) north 0))))
  )

  (begin
    (define cell cons)
    (define cell-x car)
    (define cell-y cdr)
  )

  (begin
    (define-values (maze-type-tag new-maze maze? maze-ref make-maze-subtype) (make-type 'maze))

    (define (make-maze width height . args)
      (let-optionals args ((walls 0))
        (new-maze (cons (cons width height) (make-vector (fx* width height) walls)))))

    (define (maze-width maze)
      (caar (maze-ref maze)))

    (define (maze-height maze)
      (cdar (maze-ref maze)))

    (define (maze-bounds maze)
      (let ((m (maze-ref maze)))
        (values (caar m) (cdar m))))

    (define (maze-cell-in-bounds? maze cll)
      (let ((m (maze-ref maze)))
        (and (fx>= (cell-x cll) 0)
             (fx>= (cell-y cll) 0)
             (fx< (cell-x cll) (caar m))
             (fx< (cell-y cll) (cdar m)))))

    (define (maze-cell-id maze cll)
      (let ((m (maze-ref maze)))
        (fx+ (fx* (caar m) (cell-y cll)) (cell-x cll))))

    (define (maze-cell-id->cell maze id)
      (let ((m (maze-ref maze)))
        (cell (fxremainder id (caar m)) (fx/ id (caar m)))))

    (define (maze-cell-walls maze cll)
      (let ((m (maze-ref maze)))
        (fxand (vector-ref (cdr m) (fx+ (fx* (caar m) (cell-y cll)) (cell-x cll))) 15)))

    (define (maze-has-wall? maze cll dir)
      (not (fxzero? (fxand (maze-cell-walls maze cll) dir))))

    (define (maze-has-no-wall? maze cll dir)
      (fxzero? (fxand (maze-cell-walls maze cll) dir)))

    (define (set-maze-cell-walls! maze cll walls)
      (let* ((m (maze-ref maze))
             (i (fx+ (fx* (caar m) (cell-y cll)) (cell-x cll)))
             (v (vector-ref (cdr m) i)))
        (vector-set! (cdr m) i (fxior (fxand v -16) walls))))

    (define (maze-add-wall! maze cll dir)
      (set-maze-cell-walls! maze cll (fxior (maze-cell-walls maze cll) dir))
      (maze-for-each-neighbor-cell maze cll dir
        (lambda (ncll ndir rdir)
          (set-maze-cell-walls! maze ncll (fxior (maze-cell-walls maze ncll) rdir)))))

    (define (maze-remove-wall! maze cll dir)
      (let ((walls (maze-cell-walls maze cll)))
        (set-maze-cell-walls! maze cll (fxxor walls (fxand walls dir))))
      (maze-for-each-neighbor-cell maze cll dir
        (lambda (ncll ndir rdir)
          (let ((nwalls (maze-cell-walls maze ncll)))
            (set-maze-cell-walls! maze ncll (fxxor nwalls (fxand nwalls rdir)))))))

    (define (maze-add-border-walls! maze)
      (let-values (((width height) (maze-bounds maze)))
        (do ((x 0 (fx+ x 1)))
            ((fx>= x width))
          (maze-add-wall! maze (cell x 0) north)
          (maze-add-wall! maze (cell x (fx1- height)) south))
        (do ((y 0 (fx+ y 1)))
            ((fx>= y height))
          (maze-add-wall! maze (cell 0 y) west)
          (maze-add-wall! maze (cell (fx1- width) y) east))))

    (define (maze-cell-flags maze cll)
      (let ((m (maze-ref maze)))
        (fxrshift (vector-ref (cdr m) (fx+ (fx* (caar m) (cell-y cll)) (cell-x cll))) 4)))

    (define (set-maze-cell-flags! maze cll flags)
      (let* ((m (maze-ref maze))
             (i (fx+ (fx* (caar m) (cell-y cll)) (cell-x cll)))
             (v (vector-ref (cdr m) i)))
        (vector-set! (cdr m) i (fxior (fxlshift flags 4) (fxand v 15)))))

    (define (maze-cell-unvisited? maze cll)
      (fx= (maze-cell-walls maze cll) all-directions))

    (define (maze-for-each-neighbor-cell maze cll dir f)
      (let-values (((width height) (maze-bounds maze)))
        (if (and (direction-west? dir) (fx> (cell-x cll) 0))
            (f (cell (fx1- (cell-x cll)) (cell-y cll)) west east))
        (if (and (direction-east? dir) (fx< (cell-x cll) (fx1- width)))
            (f (cell (fx1+ (cell-x cll)) (cell-y cll)) east west))
        (if (and (direction-north? dir) (fx> (cell-y cll) 0))
            (f (cell (cell-x cll) (fx1- (cell-y cll))) north south))
        (if (and (direction-south? dir) (fx< (cell-y cll) (fx1- height)))
            (f (cell (cell-x cll) (fx1+ (cell-y cll))) south north))))

    (define (display-maze maze . args)
      (let-optionals args ((port (current-output-port)))
        (let-values (((width height) (maze-bounds maze)))
          (display
            (do ((line (string #\+))
                 (x 0 (fx+ x 1)))
                ((fx>= x width) line)
              (string-append! line (if (maze-has-wall? maze (cell x 0) north) "---+" "   +")))
            port)
          (newline port)
          (do ((y 0 (fx+ y 1)))
              ((fx>= y height))
            (display
              (do ((line (string (if (maze-has-wall? maze (cell 0 y) west) #\| #\space)))
                   (x 0 (fx+ x 1)))
                  ((fx>= x width) line)
                (string-append! line (if (maze-has-wall? maze (cell x y) east) "   |" "    ")))
              port)
            (newline port)
            (display
              (do ((line (string #\+))
                   (x 0 (fx+ x 1)))
                  ((fx>= x width) line)
                (string-append! line (if (maze-has-wall? maze (cell x y) south) "---+" "   +")))
              port)
            (newline port)))))

    (define (maze->shape maze dx dy)
      (let-values (((width height) (maze-bounds maze)))
        (shape
          (do ((y 0 (fx1+ y)))
              ((fx>= y height))
            (do ((x 0 (fx1+ x))
                 (cll (cell 0 y) (cell (fx1+ x) y)))
                ((fx>= x width))
              (if (maze-has-wall? maze cll north)
                  (begin
                    (move-to (point (* x dx) (* y dy)))
                    (relative-line-to (point dx 0))))
              (if (maze-has-wall? maze cll west)
                  (begin
                    (move-to (point (* x dx) (* y dy)))
                    (relative-line-to (point 0 dy))))))
          (do ((x 0 (fx1+ x))
               (y (fx1- height)))
              ((fx>= x width))
            (if (maze-has-wall? maze (cell x y) south)
                (begin
                  (move-to (point (* x dx) (* height dy)))
                  (relative-line-to (point dx 0)))))
          (do ((x (fx1- width))
               (y 0 (fx1+ y)))
              ((fx>= y height))
            (if (maze-has-wall? maze (cell x y) east)
                (begin
                  (move-to (point (* width dx) (* y dy)))
                  (relative-line-to (point 0 dy))))))))
  )
)

(import (lispkit example maze)
        (lispkit draw))

;; Returns a random maze generated by a randomized depth first search algorithm. Backtracking
;; is implemented by memorizing the previous cell using cell flags, as provided by library
;; `(lispkit example maze)`. `height` and `width` define the dimensions of the maze in terms
;; of number of vertical and horizontal cells.
(define (make-maze/randomized-dfs width height . args)
  (let-optionals args ((startx 0)
                       (starty 0))
    (do ((maze      (make-maze width height all-directions))
         (current   (cell startx starty))
         (unvisited (fx1- (fx* width height))
                    (if (fxzero? count) unvisited (fx1- unvisited)))
         (next      (make-vector 4))
         (count     0 0))
        ((fxzero? unvisited) maze)
      (maze-for-each-neighbor-cell maze current all-directions
        (lambda (ncll ndir rdir)
          (if (maze-cell-unvisited? maze ncll)
              (begin (vector-set! next count (cons ncll ndir))
                     (set! count (fx1+ count))))))
      (if (fxzero? count)
          (set! current (maze-cell-id->cell maze (maze-cell-flags maze current)))
          (let ((target (vector-ref next (fxrandom count))))
            (maze-remove-wall! maze current (cdr target))
            (set-maze-cell-flags! maze (car target) (maze-cell-id maze current))
            (set! current (car target)))))))

;; Returns a random maze generated by a randomized binary tree algorithm. `height` and
;; `width` define the dimensions of the maze in terms of number of vertical and horizontal
;; cells.
(define (make-maze/bintree width height . args)
  (let ((maze (make-maze width height 0))
        (max-x (fx1- width)))
    (maze-add-border-walls! maze)
    (do ((x 1 (if (fx>= x max-x) 1 (fx1+ x)))
         (y 1 (if (fx>= x max-x) (fx1+ y) y)))
        ((fx>= y height) maze)
      (maze-add-wall! maze (cell x y) (if (fxzero? (fxrandom 2)) north west)))))

;; Saves a maze into a PDF file at `path`. `dx` and `dy` refer to the dimensions of a single
;; cell in the drawing that is used for generating the PDF file. The size of the PDF canvas
;; gets automatically computed from the dimensions of the `maze` as well as `dx` and `dy`.
(define (save-maze path maze dx dy)
  (let* ((shape    (maze->shape maze dx dy))
         (mdrawing (drawing (draw (transform-shape shape (translate 20 20)))))
         (dsize    (size (fx+ (fx* (maze-width maze) dx) 40)
                         (fx+ (fx* (maze-height maze) dy) 40))))
    (save-drawing path mdrawing dsize)))
