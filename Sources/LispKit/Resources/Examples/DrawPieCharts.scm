;;; Draw pie charts into a PDF file
;;;
;;; This is a demo of library `(lispkit draw chart pie)`. This library
;;; provides a procedure `draw-pie-chart` which draws a pie chart into the
;;; given/current drawing. The code below defines three different pie charts
;;; and draws them all into a PDF file, one chart per A4 page. In the end,
;;; the PDF file `pie-charts.pdf` is saved in the documents folder and gets
;;; opened and shown to the user.
;;;
;;; This demo produces a multi-page PDF file with three charts:
;;;   1. A classic pie chart with labels and percentages
;;;   2. A donut chart with an exploded slice, custom values, and a legend
;;;   3. A counter-clockwise pie chart with custom colors and a legend
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
        (lispkit draw chart pie))

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

;; ----- Chart 1: A classic pie chart -----

;; Slices without a color get a color from the palette of the chart configuration
(define budget
  (list (slice "Salaries" 52)
        (slice "Infrastructure" 18)
        (slice "Marketing" 12)
        (slice "Research" 10)
        (slice "Travel" 5)
        (slice "Other" 3)))

(define chart1-config
  (make-pie-chart-config
    'size:          (size 495 340)
    'label-font:    (font "Helvetica" 10)
    'value-font:    (font "Helvetica-Bold" 9)
    'stroke-width:  0.7
    'leader-length: 12))

(define page1
  (let ((d (make-drawing)))
    (draw-title "Where the Budget Goes" (rect 50 50 495 24) d)
    (draw-pie-chart budget
                    (point 50 90)       ; location
                    chart1-config       ; chart configuration
                    #f                  ; no legend, slices are labeled
                    d)                  ; target drawing
    d))

;; ----- Chart 2: A donut chart with an exploded slice -----

;; Quarterly revenue in thousands of dollars; slice "EMEA" is pulled out
(define revenue
  (list (slice "Americas" 1840)
        (slice "EMEA" 1290 #f 14)
        (slice "Asia Pacific" 960)
        (slice "Other" 215)))

(define chart2-config
  (make-pie-chart-config
    'size:          (size 495 300)
    'label-font:    #f                  ; the legend identifies the slices
    'value-font:    (font "Helvetica-Bold" 10)
    'value-format:  (lambda (value fraction)
                      (string-append "$" (number->string (/ (round (/ value 100.0)) 10.0)) "M"))
    'hole:          0.5
    'start-angle:   20
    'border-color:  (color 0.97 0.97 0.97)
    'bg-color:      (color 0.97 0.97 0.97)
    'stroke-width:  1.5
    'left-pad:      130))

(define chart2-legend
  (make-pie-legend-config
    'font:              (font "Helvetica" 10)
    'stroke-width:      0.4
    'corner-radius:     4
    'horizontal-offset: 12
    'vertical-offset:   -12
    'entry-pad:         7))

(define page2
  (let ((d (make-drawing)))
    (draw-title "Quarterly Revenue by Region" (rect 50 50 495 24) d)
    (draw-pie-chart revenue (point 50 90) chart2-config chart2-legend d)
    d))

;; ----- Chart 3: Counter-clockwise pie chart with custom colors -----

(define hours
  (list (slice "Sleep" 8 (color "283593"))
        (slice "Work" 8 (color "1976D2"))
        (slice "Commute" 1.5 (color "64B5F6"))
        (slice "Meals" 2 (color "00897B"))
        (slice "Exercise" 1 (color "7CB342"))
        (slice "Family & friends" 2.5 (color "F9A825"))
        (slice "Reading" 1 (color "EF6C00"))))

(define chart3-config
  (make-pie-chart-config
    'size:          (size 495 360)
    'label-font:    (font "Helvetica" 10)
    'value-format:  'value
    'value-font:    (font "Helvetica-Bold" 10)
    'start-angle:   0
    'clockwise?:    #f
    'border-color:  white
    'stroke-width:  1.5))

(define chart3-legend
  (make-pie-legend-config
    'font:              (font "Helvetica" 9)
    'stroke-width:      0.4
    'horizontal-offset: -5
    'vertical-offset:   -5
    'entry-pad:         5
    'line-pad:          2))

(define page3
  (let ((d (make-drawing)))
    (draw-title "A Day in Hours" (rect 50 50 495 24) d)
    (draw-pie-chart hours (point 50 90) chart3-config chart3-legend d)
    d))

;; ----- Save all charts to a multi-page PDF -----

(define pdf-file-path
  (path (car (system-directory 'documents)) "pie-charts.pdf"))

(save-drawings pdf-file-path
  (list
    (list page1 a4-size)
    (list page2 a4-size)
    (list page3 a4-size))
  "Pie Charts"
  "Demo Author")

;; Open the generated PDF
(open-file pdf-file-path)

;; Return the path
pdf-file-path
