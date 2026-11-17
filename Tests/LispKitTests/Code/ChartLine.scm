;;; ChartLine.scm
;;; Regression test data for library `(lispkit draw chart line)`
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;      http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(
  "Series"
  (("Berlin" (1 2 3) #f #f #f #f)
   (#f (1.5 #f -2) #t 3 (4 2) square)
   (#t #f))
  (import (lispkit draw) (lispkit draw chart line))
  (let ((s1 (line-series "Berlin" '(1 2 3)))
        (s2 (line-series #f '(1.5 #f -2) red 3 '(4 2) 'square)))
    (list (list (line-series-label s1) (line-series-values s1) (line-series-color s1)
                (line-series-width s1) (line-series-lengths s1) (line-series-marker s1))
          (list (line-series-label s2) (line-series-values s2) (color? (line-series-color s2))
                (line-series-width s2) (line-series-lengths s2) (line-series-marker s2))
          (list (line-series? s1) (line-series? '("a" 1 2)))))
)

(
  "Series without values"
  (())
  (import (lispkit draw) (lispkit draw chart line))
  (list (line-series-values (line-series "Empty" '())))
)

(
  "Series with invalid label"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (line-series 'a '(1 2))
)

(
  "Series with invalid values"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (line-series "a" '(1 "2" 3))
)

(
  "Series with values that are not a list"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (line-series "a" #(1 2 3))
)

(
  "Series with infinite value"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (line-series "a" (list 1 +inf.0))
)

(
  "Series with invalid color"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (line-series "a" '(1 2) 'red)
)

(
  "Series with negative line width"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (line-series "a" '(1 2) red -1)
)

(
  "Series with invalid dash lengths"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (line-series "a" '(1 2) red 1 '(2 0))
)

(
  "Series with invalid marker"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (line-series "a" '(1 2) red 1 #f 'star)
)

(
  "Line chart configuration defaults"
  (#t 500.0 300.0 #f 10 #f #f none 2 #f #f #f #f #f 20 40 (1 3) #f 4 #f #f #f #f #f #f)
  (import (lispkit draw) (lispkit draw chart line))
  (let ((c (make-line-chart-config)))
    (list (line-chart-config? c)
          (size-width (line-chart-size c))
          (size-height (line-chart-size c))
          (line-chart-bg-color c)
          (length (line-chart-palette c))
          (line-chart-point-font c)
          (line-chart-value-format c)
          (line-chart-marker c)
          (line-chart-line-width c)
          (line-chart-smooth? c)
          (line-chart-area? c)
          (line-chart-stacked? c)
          (line-chart-area-opacity c)
          (line-chart-vertical-grid? c)
          (line-chart-top-pad c)
          (line-chart-vlabel-width c)
          (line-chart-grid-line-lengths c)
          (line-chart-edge-pad c)
          (line-chart-value-pad c)
          (line-chart-x-step c)
          (line-chart-x-min c)
          (line-chart-x-max c)
          (line-chart-y-min c)
          (line-chart-y-max c)
          (line-chart-config? 'c)))
)

(
  "Line chart configuration keywords and setters"
  (circle #t #t #f 0.5 5 diamond #f #t 0 100 #f)
  (import (lispkit draw) (lispkit draw chart line))
  (let ((c (make-line-chart-config 'marker: 'circle
                                   'stacked?: #t
                                   'area?: #t
                                   'area-opacity: 0.5
                                   'top-pad: 5)))
    (let ((initial (list (line-chart-marker c)
                         (line-chart-stacked? c)
                         (line-chart-area? c)
                         (line-chart-smooth? c)
                         (line-chart-area-opacity c)
                         (line-chart-top-pad c))))
      (line-chart-stacked-set! c #f)
      (line-chart-marker-set! c 'diamond)
      (line-chart-area-set! c #f)
      (line-chart-smooth-set! c #t)
      (line-chart-y-min-set! c 0)
      (line-chart-y-max-set! c 100)
      (line-chart-point-font-set! c #f)
      (append initial
              (list (line-chart-marker c)
                    (line-chart-stacked? c)
                    (line-chart-smooth? c)
                    (line-chart-y-min c)
                    (line-chart-y-max c)
                    (line-chart-point-font c)))))
)

(
  "Line legend configuration"
  (#t -10 10 6 20 #f)
  (import (lispkit draw) (lispkit draw chart) (lispkit draw chart line))
  (let ((l1 (make-line-legend-config))
        (l2 (make-line-legend-config 'horizontal-offset: 20)))
    (list (legend-config? l1)
          (legend-horizontal-offset l1)
          (legend-vertical-offset l1)
          (legend-entry-pad l1)
          (legend-horizontal-offset l2)
          (legend-config? (make-line-chart-config))))
)

(
  "Draw line charts, area charts and their stacked variants"
  (#t #t #t #t #t #t #t #t)
  (import (lispkit draw) (lispkit draw chart) (lispkit draw chart line))
  (let ((series (list (line-series "Berlin" '(0 1 5 9 14 17))
                      (line-series "Oslo" '(-4 -4 0 5 11 15) blue)
                      (line-series "Rome" '(8 9 12 15 19 23) #f 1 '(4 2))
                      (line-series #f '(2 2 2 2 2 2))))
        (xs '("Jan" "Feb" "Mar" "Apr" "May" "Jun"))
        (legend (make-line-legend-config))
        (d (make-drawing)))
    (append
      (map (lambda (args)
             (draw-line-chart series xs 5 "Month" "Temperature" (point 10 10)
                              (apply make-line-chart-config args) legend d)
             (drawing? d))
           (list '()
                 '(area?: #t)
                 '(stacked?: #t)
                 '(area?: #t stacked?: #t)
                 '(area?: #t smooth?: #t)
                 '(stacked?: #t area?: #t smooth?: #t marker: circle)))
      (list (begin (draw-line-chart series xs #f #f #f (point 0 0) (make-line-chart-config) #f d)
                   (drawing? d))
            (drawing? (drawing (draw-line-chart series xs 5 #f #f (point 0 0)
                                                (make-line-chart-config 'stacked?: #t 'area?: #t)
                                                legend))))))
)

(
  "Draw line charts with an automatic y axis step"
  (#t #t #t #t #t)
  (import (lispkit draw) (lispkit draw chart line))
  (let ((d (make-drawing)))
    (map (lambda (vals)
           (draw-line-chart (list (line-series "a" vals)) '("x" "y" "z") #f #f #f (point 0 0)
                            (make-line-chart-config) #f d)
           (drawing? d))
         '((1 2 3)
           (100 105 103)
           (0.001 0.002 0.0015)
           (-5000000 0 5000000)
           (3 3 3))))
)

(
  "Draw line charts with numeric x values"
  (#t #t #t #t #t)
  (import (lispkit draw) (lispkit draw chart line))
  (let ((series (list (line-series "A" '(1 4 2 8 5))
                      (line-series "B" '(2 3 #f 6 7) green 2 #f 'triangle)))
        (xs '(0 1 2.5 4 10))
        (d (make-drawing)))
    (map (lambda (args)
           (draw-line-chart series xs 2 "x" "y" (point 0 0) (apply make-line-chart-config args) #f d)
           (drawing? d))
         '((area?: #t)
           (x-step: 2 vertical-grid?: #t)
           (x-min: -2 x-max: 12 smooth?: #t)
           (stacked?: #t area?: #t)
           (marker: square edge-pad: 30))))
)

(
  "Draw line charts with missing values"
  (#t #t #t #t #t)
  (import (lispkit draw) (lispkit draw chart line))
  (let ((d (make-drawing)))
    (map (lambda (vals args)
           (draw-line-chart (list (line-series "A" vals) (line-series "B" '(1 2 3 4 5 6)))
                            '(1 2 3 4 5 6)
                            #f #f #f
                            (point 0 0)
                            (apply make-line-chart-config args)
                            #f
                            d)
           (drawing? d))
         '((#f 1 2 #f 3 #f) (1 #f #f 2 #f 3) (#f #f #f #f #f #f) (1 2 3 4 5 #f) (#f 2 3 4 5 6))
         '((area?: #t smooth?: #t marker: circle)
           (area?: #t marker: circle)
           (area?: #t marker: circle point-font: #f)
           (stacked?: #t area?: #t smooth?: #t)
           (stacked?: #t marker: circle))))
)

(
  "Draw line charts with markers, values and custom formats"
  (#t #t #t #t #t #t)
  (import (lispkit draw) (lispkit draw chart line))
  (let ((series (list (line-series "A" '(3 -2 4 5 -1) #f #f #f 'circle)
                      (line-series "B" '(2 3 -3 1 4) #f #f #f 'square)
                      (line-series "C" '(1 1 2 #f 2) #f #f #f 'diamond)
                      (line-series "D" '(1 1 2 3 2) #f #f #f 'triangle)
                      (line-series "E" '(4 3 2 3 4) #f #f #f 'none)))
        (xs '("Q1" "Q2" "Q3" "Q4" #f))
        (d (make-drawing)))
    (map (lambda (args)
           (draw-line-chart series xs #f #f #f (rect 10 10 400 300)
                            (apply make-line-chart-config args) #f d)
           (drawing? d))
         (list (list 'point-font: (font "Helvetica" 9))
               (list 'point-font: (font "Helvetica" 9) 'stacked?: #t)
               (list 'point-font: (font "Helvetica" 9) 'stacked?: #t 'area?: #t)
               (list 'point-font: (font "Helvetica" 9)
                     'value-format: (lambda (v) (string-append (number->string v) "%")))
               (list 'point-font: (font "Helvetica" 9) 'marker: 'circle 'marker-size: 12)
               (list 'value-font: #f 'label-font: #f 'marker: 'triangle))))
)

(
  "Draw line charts with y axis overrides and grids"
  (#t #t #t #t #t #t)
  (import (lispkit draw) (lispkit draw chart line))
  (let ((series (list (line-series "A" '(1 4 9 16 25 36 49))))
        (xs '("a" "b" "c" "d" "e" "f" "g"))
        (d (make-drawing)))
    (map (lambda (args)
           (draw-line-chart series xs 10 "x" "y" (point 0 0) (apply make-line-chart-config args) #f d)
           (drawing? d))
         (list '(y-min: 0 y-max: 30)
               '(y-min: 10)
               '(y-max: 100 area?: #t)
               '(y-min: 20 area?: #t)
               '(grid-line-lengths: #f)
               (list 'grid-line-lengths: '(2 2)
                     'vertical-grid?: #t
                     'bg-color: (color 0.9 0.9 0.9)))))
)

(
  "Draw line charts with single x position or constant values"
  (#t #t #t #t)
  (import (lispkit draw) (lispkit draw chart line))
  (let ((d (make-drawing)))
    (draw-line-chart (list (line-series "A" '(5)) (line-series "B" '(3)))
                     '("only") #f #f #f (point 0 0)
                     (make-line-chart-config 'area?: #t 'marker: 'circle)
                     (make-line-legend-config)
                     d)
    (draw-line-chart (list (line-series "A" '(5))) '(10) #f #f #f (point 0 0)
                     (make-line-chart-config 'stacked?: #t 'smooth?: #t) #f d)
    (draw-line-chart (list (line-series "A" '(0 0 0)))
                     '(1 2 3) #f #f #f (point 0 0)
                     (make-line-chart-config 'area?: #t) #f d)
    (draw-line-chart (list (line-series "A" '(#f #f)))
                     '("a" "b") #f #f #f (point 0 0)
                     (make-line-chart-config) #f d)
    (list (drawing? d) (drawing? d) (drawing? d) (drawing? d)))
)

(
  "Draw line chart with many points"
  (#t)
  (import (lispkit draw) (lispkit draw chart line))
  (let ((xs (map (lambda (i) (/ i 10.0)) (iota 300)))
        (d (make-drawing)))
    (draw-line-chart (list (line-series "sin" (map sin xs)) (line-series "cos" (map cos xs)))
                     xs 0.5 "x" #f (point 0 0)
                     (make-line-chart-config 'smooth?: #t 'area?: #t)
                     (make-line-legend-config)
                     d)
    (list (drawing? d)))
)

(
  "Draw line chart without series"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (draw-line-chart '() '("a" "b") #f #f #f (point 0 0) (make-line-chart-config) #f (make-drawing))
)

(
  "Draw line chart with invalid series"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (draw-line-chart (list (line-series "a" '(1 2)) 'b)
                   '("a" "b") #f #f #f (point 0 0) (make-line-chart-config) #f (make-drawing))
)

(
  "Draw line chart with invalid configuration"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (draw-line-chart (list (line-series "a" '(1 2))) '("a" "b") #f #f #f (point 0 0) 'config #f
                   (make-drawing))
)

(
  "Draw line chart without x values"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (draw-line-chart (list (line-series "a" '())) '() #f #f #f (point 0 0)
                   (make-line-chart-config) #f (make-drawing))
)

(
  "Draw line chart with mismatching number of values"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (draw-line-chart (list (line-series "a" '(1 2 3))) '("a" "b") #f #f #f (point 0 0)
                   (make-line-chart-config) #f (make-drawing))
)

(
  "Draw line chart with mixed x values"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (draw-line-chart (list (line-series "a" '(1 2 3))) '(1 "b" 3) #f #f #f (point 0 0)
                   (make-line-chart-config) #f (make-drawing))
)

(
  "Draw line chart with x values that are not increasing"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (draw-line-chart (list (line-series "a" '(1 2 3))) '(1 3 2) #f #f #f (point 0 0)
                   (make-line-chart-config) #f (make-drawing))
)

(
  "Draw line chart with duplicate x values"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (draw-line-chart (list (line-series "a" '(1 2 3))) '(1 2 2) #f #f #f (point 0 0)
                   (make-line-chart-config) #f (make-drawing))
)

(
  "Draw line chart with invalid y axis step"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (draw-line-chart (list (line-series "a" '(1 2))) '("a" "b") 0 #f #f (point 0 0)
                   (make-line-chart-config) #f (make-drawing))
)

(
  "Draw line chart with too small y axis step"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (draw-line-chart (list (line-series "a" '(1 2000))) '("a" "b") 0.001 #f #f (point 0 0)
                   (make-line-chart-config) #f (make-drawing))
)

(
  "Draw line chart with invalid axis description"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (draw-line-chart (list (line-series "a" '(1 2))) '("a" "b") #f 'x #f (point 0 0)
                   (make-line-chart-config) #f (make-drawing))
)

(
  "Draw line chart with invalid marker in configuration"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (draw-line-chart (list (line-series "a" '(1 2))) '("a" "b") #f #f #f (point 0 0)
                   (make-line-chart-config 'marker: 'star) #f (make-drawing))
)

(
  "Draw line chart with invalid x axis step"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (draw-line-chart (list (line-series "a" '(1 2))) '(1 2) #f #f #f (point 0 0)
                   (make-line-chart-config 'x-step: -1) #f (make-drawing))
)

(
  "Draw line chart with invalid area opacity"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (draw-line-chart (list (line-series "a" '(1 2))) '("a" "b") #f #f #f (point 0 0)
                   (make-line-chart-config 'area-opacity: 2) #f (make-drawing))
)

(
  "Draw line chart with invalid y axis range"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (draw-line-chart (list (line-series "a" '(1 2))) '("a" "b") #f #f #f (point 0 0)
                   (make-line-chart-config 'y-min: 10 'y-max: 5) #f (make-drawing))
)

(
  "Draw line chart which is too small"
  <error>
  (import (lispkit draw) (lispkit draw chart line))
  (draw-line-chart (list (line-series "a" '(1 2))) '("a" "b") #f #f #f (point 0 0)
                   (make-line-chart-config 'size: (size 60 60)) #f (make-drawing))
)
