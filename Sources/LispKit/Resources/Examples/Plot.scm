;;; Plot graphs into a PDF file
;;;
;;; This is a demo of library (lispkit draw). Function `plot` draws a graph for a
;;; function for a given range using a number of interpolation points into a given
;;; rectangle. `plot` is used a number of times in function `plot-demo-page` which
;;; explains how to compose drawings and save them in a PDF file.
;;;
;;; Usage: (plot-demo-page "graph-demo.pdf")
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2018 Matthias Zenger. All rights reserved.
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


(import (lispkit draw))

;; Plots a function `f` over range `[xmin; xmax]` using `n` interpolation points
;; within rectangle `rect`. Prints `label` at the bottom of the graph. Returns the result
;; as a drawing object.
(define (plot f xmin xmax n rect label)
  (let* ((dx (/ (- xmax xmin) n))
         (xs (tabulate (fx1+ n) (lambda (i) (+ xmin (* i dx)))))
         (ys (map f xs))
         (ymin (apply min ys))
         (ymax (apply max ys))
         (xfac (/ (rect-width rect) (- xmax xmin)))
         (yfac (/ (rect-height rect) (- ymax ymin)))
         (ps (map (lambda (x y) (point (* xfac (- x xmin)) (* yfac (- y ymin)))) xs ys))
         (graph (flip-shape (interpolate ps))))
    (drawing
      ; Draw a bounding box
      (draw (rectangle (rect-point rect) (rect-size rect)) 0.5)
      ; Move rest of drawing into the bounding box
      (transform (translate (rect-x rect) (rect-y rect))
        ; Draw the coordinate axis
        (set-color (color 0.3 0.3 0.3))
        (if (and (<= xmin 0.0) (>= xmax 0.0))
          (draw (line (point (* xfac (- xmin)) 0)
                      (point (* xfac (- xmin)) (rect-height rect))) 0.3))
        (if (and (<= ymin 0.0) (>= ymax 0.0))
          (draw (line (point 0 (+ (rect-height rect) (* yfac ymin)))
                      (point (rect-width rect) (+ (rect-height rect) (* yfac ymin)))) 0.3))
        ; Draw flipped interpolation shape
        (set-color blue)
        (draw graph)
        ; Draw interpolation points
        (set-fill-color white)
        (for-each (lambda (p)
                    (let ((s (flip-shape (circle p 1) (shape-bounds graph))))
                      (fill s) (draw s 0.3))) ps)
        ; Draw the label
        (draw-text label
                   (point 30 (- (rect-height rect) 12))
                   (font "Times-Italic" 7)
                   (color 0.3 0.3 0.3))))))

;; Creates a demo page consisting of a header and four graphs
(define (plot-demo-page path)
  ; Create a new drawing
  (define page
    (drawing
      ; Draw a header in font "Helvetica-Bold" of size 8
      (draw-text "Demo of library (lispkit draw)" (point 160 8) (font "Helvetica-Bold" 8) black)
      ; Plot four graphs
      (draw-drawing (plot sin -1 6.3 50 (rect 10 30 200 100) "sin(x)"))
      (draw-drawing (plot cos -1 6.3 50 (rect 220 30 200 100) "cos(x)"))
      (draw-drawing (plot (lambda (x) (* (sin (* x 2)) (cos (/ x 4))))
        -1 6.3 50 (rect 10 140 200 100) "sin(x*2)*cos(x/4)"))
      (draw-drawing (plot (lambda (x) (/ (* x x) 40)) -1 6.3 50 (rect 220 140 200 100) "x*x/40"))))
  ; Save drawing in a PDF file
  (save-drawing page path (size 430 250)))
