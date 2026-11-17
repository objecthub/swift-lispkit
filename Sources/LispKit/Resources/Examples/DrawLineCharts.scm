;;; Draw line charts and area charts into a PDF file
;;;
;;; This is a demo of library `(lispkit draw chart line)`. This library
;;; provides a procedure `draw-line-chart` which draws line charts and
;;; area charts, including stacked variants, into the given/current drawing.
;;; The code below defines four different charts and draws them all into a
;;; PDF file, one chart per A4 page. In the end, the PDF file
;;; `line-charts.pdf` is saved in the documents folder and gets opened and
;;; shown to the user.
;;;
;;; This demo produces a multi-page PDF file with four charts:
;;;   1. A line chart with markers, dashed lines, and a legend
;;;   2. A smooth area chart on a numeric x axis
;;;   3. A stacked area chart with a legend next to the plot area
;;;   4. A stacked line chart with values, markers, and missing values
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
;;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;;; License for the specific language governing permissions and limitations
;;; under the License.

(import (lispkit base)
        (lispkit draw)
        (lispkit draw chart)
        (lispkit draw chart line))

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

;; ----- Chart 1: A line chart with markers and a legend -----

;; Average monthly temperatures in degrees Celsius. The x axis is a category axis; the
;; labels are provided by a list of strings. Series without a color get a color from the
;; palette of the chart configuration.
(define months '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define temperatures
  (list (line-series "Berlin" '(0 1 5 9 14 17 19 19 15 10 5 1))
        (line-series "Oslo" '(-4 -4 0 5 11 15 18 17 12 7 2 -2))
        (line-series "Rome" '(8 9 12 15 19 23 26 26 22 18 13 9)
                     #f 1.5 '(5 3))))        ; custom line width and dashed line

(define chart1-config
  (make-line-chart-config
    'size:          (size 495 340)
    'marker:        'circle
    'marker-size:   7
    'line-width:    2
    'descr-font:    (font "Helvetica-Oblique" 10)))

(define chart1-legend
  (make-line-legend-config
    'font:              (font "Helvetica" 10)
    'stroke-width:      0.4
    'horizontal-offset: 70
    'vertical-offset:   12))

(define page1
  (let ((d (make-drawing)))
    (draw-title "Average Monthly Temperatures" (rect 50 50 495 24) d)
    (draw-line-chart temperatures
                     months                  ; x values
                     5                       ; step of the y axis
                     "Month"                 ; x axis description
                     "Temperature [C°]"      ; y axis description
                     (point 50 90)           ; location
                     chart1-config           ; chart configuration
                     chart1-legend           ; legend configuration
                     d)                      ; target drawing
    d))

;; ----- Chart 2: A smooth area chart on a numeric x axis -----

;; Visitors of a web site by hour of the day. The measurements are not evenly spaced, which
;; is possible since the x values are numbers. In this case, they are placed proportionally on
;; the x axis. Missing values (`#f`) create gaps.
(define hours '(0 2 4 6 7 8 10 12 14 16 18 19 20 22 24))

(define visitors
  (list (line-series "Desktop" '(12 6 4 10 28 52 70 66 64 60 46 40 30 20 14)
                     (color "4E79A7"))
        (line-series "Mobile" '(30 18 10 14 40 66 58 62 60 66 80 86 72 52 38)
                     (color "F28E2B"))))

(define chart2-config
  (make-line-chart-config
    'size:          (size 495 300)
    'area?:         #t                      ; fill the areas below the lines
    'smooth?:       #t                      ; draw smooth curves
    'area-opacity:  0.35
    'x-step:        4                       ; ticks of the x axis
    'vertical-grid?: #t
    'bg-color:      (color 0.97 0.97 0.97)))

(define chart2-legend
  (make-line-legend-config
    'stroke-width:      0.4
    'horizontal-offset: 70
    'vertical-offset:   12))

(define page2
  (let ((d (make-drawing)))
    (draw-title "Web Site Visitors by Hour" (rect 50 50 495 24) d)
    (draw-line-chart visitors hours #f "Hour of the day" "Visitors" (point 50 90)
                     chart2-config chart2-legend d)
    d))

;; ----- Chart 3: A stacked area chart -----

;; Electricity generation in TWh by source. In stacked charts, the series are drawn on top of
;; each other and the y axis shows the sum. The right padding reserves space for the legend,
;; which lists the series in the order of the stack.
(define years '(2015 2016 2017 2018 2019 2020 2021 2022 2023 2024))

(define generation
  (list (line-series "Coal"    '(180 172 165 158 140 120 118 112 100 92)  (color "575757"))
        (line-series "Gas"     '(120 126 130 136 142 146 150 148 152 150) (color "9C755F"))
        (line-series "Nuclear" '( 90  90  88  86  84  80  78  76  74  74) (color "B07AA1"))
        (line-series "Hydro"   '( 50  52  50  54  52  56  54  58  56  60) (color "4E79A7"))
        (line-series "Wind"    '( 30  38  46  55  66  78  90 104 118 132) (color "59A14F"))
        (line-series "Solar"   '(  5   8  12  18  26  36  50  66  86 110) (color "EDC948"))))

(define chart3-config
  (make-line-chart-config
    'size:          (size 495 340)
    'area?:         #t
    'stacked?:      #t
    'x-step:        1
    'right-pad:     90
    'line-width:    1
    'stroke-width:  1))

(define chart3-legend
  (make-line-legend-config
    'font:              (font "Helvetica" 10)
    'stroke-width:      0.4
    'corner-radius:     4
    'horizontal-offset: -5
    'vertical-offset:   12
    'entry-pad:         7))

(define page3
  (let ((d (make-drawing)))
    (draw-title "Electricity Generation by Source" (rect 50 50 495 24) d)
    (draw-line-chart generation years 100 #f "TWh" (point 50 90) chart3-config chart3-legend d)
    d))

;; ----- Chart 4: A stacked line chart with values and missing values -----

;; Quarterly sales by region in millions. A stacked line chart shows the accumulated values;
;; the values of the series are drawn next to their points. A missing value is counted as 0
;; in stacked charts. Series can have their own marker shapes.
(define quarters '("Q1 2025" "Q2 2025" "Q3 2025" "Q4 2025" "Q1 2026" "Q2 2026"))

(define sales
  (list (line-series "Americas" '(12 14 13 17 18 21) (color "4E79A7") 2 #f 'circle)
        (line-series "EMEA"     '( 8  9 #f 11 12 14) (color "F28E2B") 2 #f 'square)
        (line-series "APAC"     '( 4  6  7  9 11 13) (color "59A14F") 2 #f 'diamond)))

(define chart4-config
  (make-line-chart-config
    'size:          (size 495 340)
    'stacked?:      #t
    'marker-size:   8
    'point-font:    (font "Helvetica-Bold" 9)
    'value-format:  (lambda (v) (string-append (number->string v) "M"))
    'y-min:         0
    'y-max:         60
    'vertical-grid?: #t))

(define chart4-legend
  (make-line-legend-config
    'stroke-width:      0.4
    'horizontal-offset: 70
    'vertical-offset:   12))

(define page4
  (let ((d (make-drawing)))
    (draw-title "Quarterly Sales by Region" (rect 50 50 495 24) d)
    (draw-line-chart sales quarters 10 #f "Sales" (point 50 90) chart4-config chart4-legend d)
    d))

;; ----- Save all charts to a multi-page PDF -----

(define pdf-file-path
  (path (car (system-directory 'documents)) "line-charts.pdf"))

(save-drawings pdf-file-path
  (list
    (list page1 a4-size)
    (list page2 a4-size)
    (list page3 a4-size)
    (list page4 a4-size))
  "Line Charts"
  "Demo Author")

;; Open the generated PDF
(open-file pdf-file-path)

;; Return the path
pdf-file-path
