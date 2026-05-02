;;; Draw function charts into a PDF file
;;;
;;; This is a demo of library `(lispkit draw chart function)`. This library
;;; provides a procedure `draw-function-chart` which draws a function chart
;;; into the given/current drawing. The code below defines three different
;;; functions charts (some showing multiple function graphs) and draws them
;;; all into a PDF file, one chart per A4 page. In the end, the PDF file
;;; `function-charts.pdf` is saved in the documents folder and gets
;;; opened and shown to the user.
;;; 
;;; This demo produces a multi-page PDF file with three charts:
;;;   1. sin(x) and cos(x) on [-2π, 2π]
;;;   2. A polynomial x³ − 3x on [-3, 3]
;;;   3. Multiple functions demonstrating legend support
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may
;;; not use this file except in compliance with the License. You may obtain
;;; a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(import (lispkit base)
        (lispkit draw)
        (lispkit draw chart)
        (lispkit draw chart function))

;; Size of the generated PDF (we use A4 here)
(define a4-size (size 595 842))

;; Utility: centered title text
(define (draw-title str box drw)
  (let* ((fnt  (font "Times-Bold" 16))
         (tsz  (text-size str fnt (rect-size box))))
    (draw-text str
               (point (+ (rect-x box)
                         (/ (- (rect-width box) (size-width tsz)) 2))
                      (+ (rect-y box)
                         (/ (- (rect-height box) (size-height tsz)) 2)))
               fnt black drw)))

;; ----- Chart 1: sin and cos -----

(define chart1-config
  (make-function-chart-config
    'size:             (size 495 280)
    'box-color:        #f
    'samples:          300
    'line-width:       2.0
    'axis-font:        (font "Helvetica" 8)
    'label-font:       (font "Helvetica" 9)
    'descr-font:       (font "Helvetica" 9)
    'stroke-width:     0.7
    'top-pad:          12
    'left-pad:         12
    'right-pad:        12
    'bottom-pad:       12
    'axis-label-width: 35
    'axis-label-height: 20))

(define chart1-legend
  (make-function-legend-config
    'font:              (font "Helvetica" 8)
    'color:             (color 0.2 0.2 0.2)
    'bg-color:          (color 0.9 0.9 0.9)
    'stroke-width:      0.4
    'horizontal-offset: -0.001
    'vertical-offset:   150
    'sample-length:     10
    'entry-pad:         3
    'line-pad:          3))

(define page1
  (let ((d (make-drawing)))
    (draw-title "Trigonometric Functions"
                (rect 50 50 495 24) d)
    (draw-function-chart
      (list (function-graph sin "sin(x)" blue)
            (function-graph cos "cos(x)" red))
      (- (* 0 pi))    ; xmin = 0
      (* 3 pi)        ; xmax = 4π
      -1.05           ; ymin
      1.05            ; ymax
      1.5708          ; xstep ≈ π/2
      0.5             ; ystep
      "x"             ; x-axis description
      "y"             ; y-axis description
      (point 20 90)   ; location
      chart1-config   ; chart configuration
      chart1-legend   ; legend configuration
      d)              ; target drawing
    d))

;; ----- Chart 2: Polynomial x³ − 3x -----

(define chart2-config
  (make-function-chart-config
    'size:             (size 495 280)
    'samples:          250
    'line-width:       2.0
    'axis-font:        (font "Helvetica" 8)
    'label-font:       (font "Helvetica" 9)
    'descr-font:       (font "Helvetica" 9)
    'stroke-width:     0.7
    'top-pad:          15
    'left-pad:         15
    'right-pad:        10
    'axis-label-width: 35
    'axis-label-height: 22))

(define (cubic x)
  (- (* x x x) (* 3 x)))

(define page2
  (let ((d (make-drawing)))
    (draw-title "Polynomial: f(x) = x³ − 3x"
                (rect 50 50 495 24) d)
    (draw-function-chart
      (function-graph cubic "x³ − 3x" (color "4A90D9"))
      -3.0               ; xmin
      3.0                ; xmax
      -10.0              ; ymin
      10.0               ; ymax
      1.0                ; xstep
      2.0                ; ystep
      "x"                ; x-axis description
      "f(x)"             ; y-axis description
      (point 50 90)      ; location
      chart2-config      ; chart configuration
      #f                 ; no legend (single function)
      d)                 ; target drawing
    d))

;; ----- Chart 3: Multiple functions with legend -----

(define chart3-config
  (make-function-chart-config
    'size:              (size 500 350)
    'samples:           300
    'line-width:        1.8
    'bg-color:          (color 0.97 0.97 0.97)
    'axis-font:         (font "Helvetica" 8)
    'label-font:        (font "Helvetica" 9)
    'descr-font:        (font "Helvetica" 9)
    'stroke-width:      0.7
    'top-pad:           15
    'left-pad:          15
    'right-pad:         10
    'axis-label-width:  40
    'axis-label-height: 22
    'grid-line-lengths: '(1 2)))

(define chart3-legend
  (make-function-legend-config
    'font:              (font "Helvetica" 8)
    'stroke-width:      0.4
    'corner-radius:     4
    'horizontal-offset: -15
    'vertical-offset:   20
    'sample-length:     10
    'entry-pad:         5
    'line-pad:          3))

;; A few interesting functions
(define (gaussian x)
  (exp (- (* x x 0.5))))

(define (sinc x)
  (if (zero? x) 1.0 (/ (sin x) x)))

(define (damped-sine x)
  (* (exp (* -0.15 (abs x))) (sin x)))

(define page3
  (let ((d (make-drawing)))
    (draw-title "Comparison of Functions"
                (rect 50 50 495 24) d)
    (draw-function-chart
      (list (function-graph gaussian "Gaussian" (color "2196F3") #f '(8 4))
            (function-graph sinc "sinc(x)" (color "F44336"))
            (function-graph damped-sine "Damped sine" (color "4CAF50") 1.5)
            (function-graph (lambda (x) 0.8) #f gray 0.5 '(6 3))
            (function-graph (lambda (x) -0.8) #f gray 0.5 '(6 3))
            (vertical-line-graph 4.4 #f blue 0.5 '(6 3 2 3))
            (inline-label-graph (point 4.8 -0.5) "← Local minima" red))
      -10.0              ; xmin
      10.0               ; xmax
      -0.9               ; ymin
      1.1                ; ymax
      2.0                ; xstep
      0.25               ; ystep
      "x"                ; x-axis description
      "y"                ; y-axis description
      (point 50 90)      ; location
      chart3-config      ; chart configuration
      chart3-legend      ; legend configuration
      d)                 ; target drawing
    d))

;; ----- Save all charts to a multi-page PDF -----

(define pdf-file-path
  (path (car (system-directory 'documents)) "function-charts.pdf"))

(save-drawings pdf-file-path
  (list
    (list page1 a4-size)
    (list page2 a4-size)
    (list page3 a4-size))
  "Function Charts"
  "Demo Author")

;; Open the generated PDF
(open-file pdf-file-path)

;; Return the path
pdf-file-path
