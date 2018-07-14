;;; Plot graphs into a PDF file
;;;
;;; This is a demo of library (lispkit draw). Function `plot` draws a function over
;;; a given range using a number of interpolation points into a given rectangle.
;;; It is used a number of times in function `plot-demo-page` which explains how to
;;; compose drawings and save them in a PDF file.
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
;; within rectangle `rect`. Prints `label` at the bottom of the graph.
(define (plot f xmin xmax n rect label)
  (let* ((dx (/ (- xmax xmin) n))
         (xs (tabulate (fx1+ n) (lambda (i) (+ xmin (* i dx)))))
         (ys (map f xs))
         (ymin (apply min ys))
         (ymax (apply max ys))
         (xfac (/ (car (rect-size rect)) (- xmax xmin)))
         (yfac (/ (cdr (rect-size rect)) (- ymax ymin)))
         (ps (map (lambda (x y) (point (* xfac (- x xmin)) (* yfac (- y ymin)))) xs ys))
         (shift (translate (car (rect-point rect)) (cdr (rect-point rect))))
         (d (make-drawing)))
    ; Interpolate the points and flip the shape
    (define s (flip-shape (interpolate ps)))
    ; Draw a bounding box
    (define box (shape-bounds s))
    (draw-shape d (make-rect (rect-point rect) (rect-size rect)) 0.5)
    ; Draw the graph and coordinate axis
    (enable-transformation d shift)
    (if (and (<= xmin 0.0) (>= xmax 0.0))
        (draw-shape d (make-polygon (point (* xfac (- xmin)) 0)
                                     (point (* xfac (- xmin)) (cdr (rect-size rect))))
                     0.3))
    (if (and (<= ymin 0.0) (>= ymax 0.0))
        (draw-shape d (make-polygon (point 0
                                            (+ (cdr (rect-size rect)) (* yfac ymin)))
                                     (point (car (rect-size rect))
                                            (+ (cdr (rect-size rect)) (* yfac ymin))))
                     0.3))
    (set-color d (make-color 0.0 0.0 1.0 1.0))
    (draw-shape d s)
    ; Draw interpolation points
    (set-color d (make-color 0.0 0.0 0.0))
    (set-fill-color d (make-color 0.0 0.0 0.0))
    (for-each (lambda (p) (fill-shape d (flip-shape (make-arc p 1 0) box))) ps)
    ; Draw the label
    (draw-text d label (point 30 (- (cdr (rect-size rect)) 12))
      (make-color 0.3 0.3 0.3) (font "Times-Italic" 7))
    (disable-transformation d shift)
    d))

;; Creates a demo page consisting of a header and four graphs
(define (plot-demo-page path)
  ; Create a new drawing
  (define page (make-drawing))
  ; Draw a header in font "Helvetica" of size 8
  (draw-text page "Demo of library (lispkit draw)"
    (point 160 8) (make-color 0.0 0.0 0.0) (font "Helvetica" 8))
  ; Plot four graphs
  (draw-drawing page (plot sin -1 6.3 50 (rect 10 30 200 100) "sin(x)"))
  (draw-drawing page (plot cos -1 6.3 50 (rect 220 30 200 100) "cos(x)"))
  (draw-drawing page (plot (lambda (x) (* (sin (* x 2)) (cos (/ x 4))))
    -1 6.3 50 (rect 10 140 200 100) "sin(x*2)*cos(x/4)"))
  (draw-drawing page (plot (lambda (x) (/ (* x x) 40)) -1 6.3 50 (rect 220 140 200 100) "x*x/40"))
  ; Save drawing in a PDF file
  (save-drawing page path (size 430 250)))

