;;; ChartPie.scm
;;; Regression test data for library `(lispkit draw chart pie)` and for the legend
;;; procedure of library `(lispkit draw chart)`
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
  "Slices"
  (("Apples" 40 #f 0)
   (#f 1.5 #t 12)
   (#t #f))
  (import (lispkit draw) (lispkit draw chart pie))
  (let ((s1 (slice "Apples" 40))
        (s2 (slice #f 1.5 red 12)))
    (list (list (slice-label s1) (slice-value s1) (slice-color s1) (slice-offset s1))
          (list (slice-label s2) (slice-value s2) (color? (slice-color s2)) (slice-offset s2))
          (list (slice? s1) (slice? 42))))
)

(
  "Slice with negative value"
  <error>
  (import (lispkit draw) (lispkit draw chart pie))
  (slice "a" -1)
)

(
  "Slice with invalid label"
  <error>
  (import (lispkit draw) (lispkit draw chart pie))
  (slice 'a 1)
)

(
  "Slice with invalid color"
  <error>
  (import (lispkit draw) (lispkit draw chart pie))
  (slice "a" 1 'red)
)

(
  "Slice with negative offset"
  <error>
  (import (lispkit draw) (lispkit draw chart pie))
  (slice "a" 1 red -3)
)

(
  "Pie chart configuration defaults"
  (#t 500.0 300.0 0 0 #t percent #f 10 10 3 10 #t #f)
  (import (lispkit draw) (lispkit draw chart pie))
  (let ((c (make-pie-chart-config)))
    (list (pie-chart-config? c)
          (size-width (pie-chart-size c))
          (size-height (pie-chart-size c))
          (pie-chart-hole c)
          (pie-chart-start-angle c)
          (pie-chart-clockwise? c)
          (pie-chart-value-format c)
          (pie-chart-bg-color c)
          (pie-chart-top-pad c)
          (length (pie-chart-palette c))
          (pie-chart-label-pad c)
          (pie-chart-leader-length c)
          (color? (pie-chart-border-color c))
          (pie-chart-config? 'c)))
)

(
  "Pie chart configuration keywords and setters"
  (0.4 90 #f value 5 0.6 #t #f)
  (import (lispkit draw) (lispkit draw chart pie))
  (let ((c (make-pie-chart-config 'hole: 0.4
                                  'start-angle: 90
                                  'clockwise?: #f
                                  'value-format: 'value
                                  'top-pad: 5)))
    (let ((initial (list (pie-chart-hole c)
                         (pie-chart-start-angle c)
                         (pie-chart-clockwise? c)
                         (pie-chart-value-format c)
                         (pie-chart-top-pad c))))
      (pie-chart-hole-set! c 0.6)
      (pie-chart-clockwise-set! c #t)
      (pie-chart-border-color-set! c #f)
      (append initial
              (list (pie-chart-hole c)
                    (pie-chart-clockwise? c)
                    (pie-chart-border-color c)))))
)

(
  "Pie legend configuration"
  (#t -10 10 6 20 #f)
  (import (lispkit draw) (lispkit draw chart) (lispkit draw chart pie))
  (let ((l1 (make-pie-legend-config))
        (l2 (make-pie-legend-config 'horizontal-offset: 20)))
    (list (legend-config? l1)
          (legend-horizontal-offset l1)
          (legend-vertical-offset l1)
          (legend-entry-pad l1)
          (legend-horizontal-offset l2)
          (legend-config? (make-pie-chart-config))))
)

(
  "Draw pie charts"
  (#t #t #t #t #t #t #t #t)
  (import (lispkit draw) (lispkit draw chart) (lispkit draw chart pie))
  (let ((slices (list (slice "Apples" 40)
                      (slice "Pears" 25 blue 10)
                      (slice "Plums" 0)
                      (slice #f 15)
                      (slice "Cherries" 20 red)))
        (legend (make-pie-legend-config))
        (d (make-drawing)))
    (list
      ; Defaults, with legend
      (begin (draw-pie-chart slices (point 10 10) (make-pie-chart-config) legend d)
             (drawing? d))
      ; Without legend
      (begin (draw-pie-chart slices (point 10 10) (make-pie-chart-config) #f d)
             (drawing? d))
      ; Location as rectangle
      (begin (draw-pie-chart slices (rect 10 10 300 200) (make-pie-chart-config) legend d)
             (drawing? d))
      ; Donut chart, counter-clockwise, custom start angle
      (begin (draw-pie-chart slices
                             (point 0 0)
                             (make-pie-chart-config 'hole: 0.5 'start-angle: 45 'clockwise?: #f)
                             legend
                             d)
             (drawing? d))
      ; Only legend, no labels and values, no borders, background
      (begin (draw-pie-chart slices
                             (point 0 0)
                             (make-pie-chart-config 'label-font: #f
                                                    'value-font: #f
                                                    'border-color: #f
                                                    'bg-color: (color 0.9 0.9 0.9))
                             legend
                             d)
             (drawing? d))
      ; Custom value formats
      (begin (draw-pie-chart slices
                             (point 0 0)
                             (make-pie-chart-config
                               'value-format: (lambda (v f) (number->string v)))
                             #f
                             d)
             (draw-pie-chart slices
                             (point 0 0)
                             (make-pie-chart-config 'value-format: 'value 'leader-length: 0)
                             #f
                             d)
             (drawing? d))
      ; A single slice, as pie and as donut
      (begin (draw-pie-chart (list (slice "All" 3)) (point 0 0) (make-pie-chart-config) #f d)
             (draw-pie-chart (list (slice "All" 3))
                             (point 0 0)
                             (make-pie-chart-config 'hole: 0.6)
                             #f
                             d)
             (drawing? d))
      ; Drawing into the current drawing
      (drawing? (drawing (draw-pie-chart slices (point 0 0) (make-pie-chart-config) legend)))))
)

(
  "Draw pie charts with crowded labels"
  (#t #t #t #t #t)
  (import (lispkit draw) (lispkit draw chart pie))
  (let ((many (map (lambda (i) (slice (string-append "Category " (number->string i))
                                      (+ 1 (modulo (* i 7) 5))))
                   (iota 14)))
        (tiny-top (list (slice "Alpha" 50) (slice "Tiny one" 1) (slice "Tiny two" 1.2)
                        (slice "Tiny three" 0.8) (slice "Omega" 30)))
        (tiny-bottom (list (slice "Alpha" 50) (slice "Omega" 30) (slice "Tiny one" 1)
                           (slice "Tiny two" 1.2) (slice "Tiny three" 0.8)))
        (quarters (list (slice "North east" 1) (slice "South east" 1)
                        (slice "South west" 1) (slice "North west" 1)))
        (d (make-drawing)))
    (map (lambda (args)
           (draw-pie-chart (car args) (point 0 0) (apply make-pie-chart-config (cdr args)) #f d)
           (drawing? d))
         (list (list many 'size: (size 500 400))
               (list many 'size: (size 300 300) 'clockwise?: #f 'start-angle: 30)
               (list tiny-top)
               (list tiny-bottom 'leader-length: 0)
               (list quarters 'start-angle: 45 'size: (size 400 300)))))
)

(
  "Draw pie charts with thin and pulled out slices"
  (#t #t #t #t #t #t)
  (import (lispkit draw) (lispkit draw chart pie))
  (let ((d (make-drawing)))
    (append
      (map (lambda (offset)
             (draw-pie-chart (list (slice "Americas" 1840)
                                   (slice "EMEA" 1290 #f offset)
                                   (slice "Asia Pacific" 960)
                                   (slice "Other" 215))
                             (point 0 0)
                             (make-pie-chart-config 'size: (size 495 300)
                                                    'label-font: #f
                                                    'hole: 0.5
                                                    'start-angle: 20
                                                    'left-pad: 130)
                             #f
                             d)
             (drawing? d))
           '(0 14 30 60))
      ; Pulled out slices covering the full circle, or without extent
      (list (begin (draw-pie-chart (list (slice "All" 3 #f 10))
                                   (point 0 0)
                                   (make-pie-chart-config)
                                   #f
                                   d)
                   (drawing? d))
            (begin (draw-pie-chart (list (slice "A" 3 #f 10) (slice "B" 0 #f 20))
                                   (point 0 0)
                                   (make-pie-chart-config 'hole: 0.5)
                                   #f
                                   d)
                   (drawing? d)))))
)

(
  "Draw pie chart without slices"
  <error>
  (import (lispkit draw) (lispkit draw chart pie))
  (draw-pie-chart '() (point 0 0) (make-pie-chart-config) #f (make-drawing))
)

(
  "Draw pie chart with zero total"
  <error>
  (import (lispkit draw) (lispkit draw chart pie))
  (draw-pie-chart (list (slice "a" 0) (slice "b" 0))
                  (point 0 0)
                  (make-pie-chart-config)
                  #f
                  (make-drawing))
)

(
  "Draw pie chart with invalid slices"
  <error>
  (import (lispkit draw) (lispkit draw chart pie))
  (draw-pie-chart (list (slice "a" 1) 'b)
                  (point 0 0)
                  (make-pie-chart-config)
                  #f
                  (make-drawing))
)

(
  "Draw pie chart with invalid value format"
  <error>
  (import (lispkit draw) (lispkit draw chart pie))
  (draw-pie-chart (list (slice "a" 1))
                  (point 0 0)
                  (make-pie-chart-config 'value-format: 'bogus)
                  #f
                  (make-drawing))
)

(
  "Draw pie chart which is too small"
  <error>
  (import (lispkit draw) (lispkit draw chart pie))
  (draw-pie-chart (list (slice "a" 1) (slice "b" 2))
                  (point 0 0)
                  (make-pie-chart-config 'size: (size 40 40))
                  #f
                  (make-drawing))
)

(
  "Draw legends"
  (#t #t #t #t)
  (import (lispkit draw) (lispkit draw chart))
  (let ((d (make-drawing))
        (entries (list (list "Low" blue) (list "High" red black))))
    (map (lambda (offsets)
           (draw-legend entries
                        (rect 10 20 400 250)
                        (make-legend-config 'horizontal-offset: (car offsets)
                                            'vertical-offset: (cdr offsets))
                        black
                        white
                        d)
           (drawing? d))
         '((10 . 10) (-10 . 10) (10 . -10) (-10 . -10))))
)
