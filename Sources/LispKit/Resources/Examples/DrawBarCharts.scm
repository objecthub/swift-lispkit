;;; Draw bar charts into a PDF file
;;;
;;; This is a demo of library (lispkit draw chart bar). This library provides a
;;; procedure `draw-bar-chart` which draws a bart chart into the given/current
;;; drawing. The code below defines three different bar charts and draws them
;;; all into a PDF file, one chart per A4 page. In the end, the PDF file gets
;;; opened and shown to the user.
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2022 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not
;;; use this file except in compliance with the License. You may obtain a copy
;;; of the License at
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
        (lispkit draw chart bar))

;; Size of the generated PDF (we use A4 here)
(define a4-size (size 595 842))

;; Center text in the given rectangle
(define (draw-text-centered str box font col)
  (let ((tsize (text-size str font (rect-size box))))
    (draw-text str
               (point (+ (rect-x box) (/ (- (rect-width box) (size-width tsize)) 2))
                      (+ (rect-y box) (/ (- (rect-height box) (size-height tsize)) 2)))
               font col)))

;; Monthly average temperature in Zurich (as list of bars)
(define avg-temp
  (list
    (bar "Jan" 0)
    (bar "Feb" 2)
    (bar "Mar" 6)
    (bar "Apr" 9)
    (bar "May" 14)
    (bar "Jun" 16)
    (bar "Jul" 19)
    (bar "Aug" 18)
    (bar "Sep" 15)
    (bar "Oct" 11)
    (bar "Nov" 5)
    (bar "Dec" 2)))

;; Define a bar chart configuration using keyword syntax
(define avg-temp-config
  (make-bar-chart-config
    'size: (size 495 200)
    'color: black
    'value-font: (font "Helvetica" 8.5)
    'bar-font: (font "Helvetica" 8)
    'label-font: (font "Helvetica" 9)
    'descr-font: (font "Helvetica-LightOblique" 8)
    'stroke-width: 0.7
    'top-pad: 5
    'left-pad: 10
    'right-pad: 5
    'vindicator-width: 8
    'bar-gap: 10
    'vlabel-width: 34
    'glabel-height: 5
    'blabel-height: 20))

;; Create a bar chart drawing
(define avg-temp-diag
  (drawing
    (draw-text-centered
      "Monthly Temperature Average in Zurich, Switzerland"
      (rect 50 70 495 20)
      (font "Times-Bold" 14)
      black)
    (draw-bar-chart avg-temp gray 5 "Temperature [C°]" "Month" (point 50 105) avg-temp-config #f)))

;; Monthly temperature range in Zurich (represented as monthly bar groups)
(define temp-range
  (list
    (bar-group "Jan" (bar #f -2 0) (bar #f 0 2))
    (bar-group "Feb" (bar #f -2 0) (bar #f 0 6))
    (bar-group "Mar" (bar #f 1 0) (bar #f 0 11))
    (bar-group "Apr" (bar #f 3 0) (bar #f 0 15))
    (bar-group "May" (bar #f 7 0) (bar #f 0 20))
    (bar-group "Jun" (bar #f 11 0) (bar #f 0 21))
    (bar-group "Jul" (bar #f 13 0) (bar #f 0 24))
    (bar-group "Aug" (bar #f 13 0) (bar #f 0 23))
    (bar-group "Sep" (bar #f 10 0) (bar #f 0 20))
    (bar-group "Oct" (bar #f 7 0) (bar #f 0 14))
    (bar-group "Nov" (bar #f 2 0) (bar #f 0 7))
    (bar-group "Dec" (bar #f -1 0) (bar #f 0 4))))

;; Define a bar chart configuration using record syntax
(define temp-range-config (make-bar-chart-config))
(bar-chart-size-set! temp-range-config (size 495 200))
(bar-chart-value-font-set! temp-range-config (font "Helvetica" 8))
(bar-chart-bar-font-set! temp-range-config (font "Helvetica" 7.5))
(bar-chart-group-font-set! temp-range-config (font "Helvetica-Bold" 8))
(bar-chart-descr-font-set! temp-range-config (font "Helvetica-LightOblique" 8))
(bar-chart-stroke-width-set! temp-range-config 0.7)
(bar-chart-top-pad-set! temp-range-config 5)
(bar-chart-left-pad-set! temp-range-config 10)
(bar-chart-right-pad-set! temp-range-config 5)
(bar-chart-bottom-pad-set! temp-range-config 0)
(bar-chart-vindicator-width-set! temp-range-config 8)
(bar-chart-bar-gap-set! temp-range-config 9)
(bar-chart-group-gap-set! temp-range-config 2)
(bar-chart-vlabel-width-set! temp-range-config 34)
(bar-chart-glabel-height-set! temp-range-config 20)
(bar-chart-blabel-height-set! temp-range-config -5)
(bar-chart-yaxis-overhead-set! temp-range-config 20)

;; Define a legend configuration using record syntax
(define temp-range-legend (make-legend-config))
(legend-font-set! temp-range-legend (font "Helvetica" 7))
(legend-stroke-width-set! temp-range-legend 0.5)
(legend-line-pad-set! temp-range-legend 3)
(legend-entry-pad-set! temp-range-legend 5)
(legend-sample-area-width-set! temp-range-legend 16)
(legend-sample-length-set! temp-range-legend 8)
(legend-horizontal-offset-set! temp-range-legend -13)
(legend-vertical-offset-set! temp-range-legend 13)

;; Create a bar chart drawing for the list of temperature ranges
(define temp-range-diag
  (drawing
    (draw-text-centered
      "Monthly Temperature Range in Zurich, Switzerland"
      (rect 50 70 495 20)
      (font "Times-Bold" 14)
      black)
    (draw-bar-chart
      temp-range
      (list (bar-segment "Low" blue)
            (bar-segment "High" red))
      5
      "Temperature [C°]"
      #f
      (point 50 105)
      temp-range-config
      temp-range-legend)))

;; Combine bar groups and segmentation
(define smedia
  (list
    (bar-group "Q3 2021"
      (bar "Male" 2.1 3.3 1.3)
      (bar "Female" 3.2 2.9 2.2))
    (bar-group "Q1 2021"
      (bar "Male" 3.3 2.2 2.6)
      (bar "Female" 4.0 3.0 2.0))
    (bar-group "Q2 2021"
      (bar "Male" 1.0 3.5 1.8)
      (bar "Female" 2.0 4.1 4.0))
    (bar-group "Q4 2021"
      (bar "Male" 2.7 4.1 3.0)
      (bar "Female" 2.0 4.6 4.5))))

;; Create a chart configuration
(define smedia-config (make-bar-chart-config 'color: black 'bg-color: (color 0.95 0.95 0.95)))
(bar-chart-size-set! smedia-config (size 495 200))
(bar-chart-value-font-set! smedia-config (font "Helvetica" 8))
(bar-chart-bar-font-set! smedia-config (font "Helvetica" 7.5))
(bar-chart-label-font-set! smedia-config (font "Helvetica" 7.5))
(bar-chart-group-font-set! smedia-config (font "Helvetica-Bold" 8.5))
(bar-chart-descr-font-set! smedia-config (font "Helvetica-LightOblique" 7.5))
(bar-chart-stroke-width-set! smedia-config 0.7)
(bar-chart-top-pad-set! smedia-config 5)
(bar-chart-left-pad-set! smedia-config 10)
(bar-chart-right-pad-set! smedia-config 5)
(bar-chart-bottom-pad-set! smedia-config 2)
(bar-chart-vindicator-width-set! smedia-config 8)
(bar-chart-bar-gap-set! smedia-config 18)
(bar-chart-group-gap-set! smedia-config 5)
(bar-chart-vlabel-width-set! smedia-config 39)
(bar-chart-glabel-height-set! smedia-config 20)
(bar-chart-blabel-height-set! smedia-config 5)
(bar-chart-yaxis-overhead-set! smedia-config 25)

;; Create a legend configuration
(define smedia-legend
  (make-legend-config
    'font: (font "Helvetica" 7)
    'stroke-width: 0.4
    'line-pad: 3
    'entry-pad: 5
    'sample-area-width: 16
    'sample-length: 8
    'horizontal-offset: 50
    'vertical-offset: 8))

;; Draw the chart
(define smedia-diag
  (drawing
    (draw-text-centered
      "Quarterly Engagements per Channel"
      (rect 50 70 495 20)
      (font "Times-Bold" 14)
      black)
    (draw-bar-chart smedia
                 (list (bar-segment "Facebook" red black)
                       (bar-segment "Instagram" green black)
                       (bar-segment "Twitter" blue white))
                 2.0
                 "Engagements [Thousands]"
                 #f
                 (point 50 105)
                 smedia-config
                 smedia-legend)))

;; Path to the generated PDF file
(define pdf-file-path
  (path (car (system-directory 'temporary)) "bar-charts.pdf"))

;; Save all drawings in a PDF (one A4 page per drawing)
(save-drawings pdf-file-path
  (list
    (list avg-temp-diag a4-size)
    (list temp-range-diag a4-size)
    (list smedia-diag a4-size))
  "Sample charts"
  "Matthias Zenger")

;; Open the generated PDF file
(open-file pdf-file-path)

;; Return the path to the generated PDF
pdf-file-path

