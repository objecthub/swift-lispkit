;;; LISPKIT DRAW CHART
;;;
;;; This library provides access to abstractions that are shared between
;;; the various chart libraries. For now, this library defines a record
;;; encapsulating graph legend metadata.
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

(define-library (lispkit draw chart)
  
  (export
    ; Legends
    make-legend-config
    legend-config
    legend-config?
    legend-font
    legend-font-set!
    legend-color
    legend-color-set!
    legend-bg-color
    legend-bg-color-set!
    legend-stroke-width
    legend-stroke-width-set!
    legend-corner-radius
    legend-corner-radius-set!
    legend-horizontal-offset
    legend-horizontal-offset-set!
    legend-vertical-offset
    legend-vertical-offset-set!
    legend-sample-area-width
    legend-sample-area-width-set!
    legend-sample-length
    legend-sample-length-set!
    legend-line-pad
    legend-line-pad-set!
    legend-entry-pad
    legend-entry-pad-set!)
  
  (import (lispkit base)
          (lispkit draw))
  
  ;; Legends
  
  (begin
    (define-record-type <legend-config>
      (legend-config
       legend-font               ; font used for legend labels
       legend-color              ; color used for legend labels
       legend-bg-color           ; background color of legend
       legend-stroke-width       ; width of a stroke for drawing the bounding box
       legend-corner-radius      ; radius for rounded bounding box
       legend-horizontal-offset  ; horizontal offset from chart bounds (neg = right offset)
       legend-vertical-offset    ; vertical offset from chart bounds (neg = bottom offset)
       legend-sample-area-width  ; width of the sample area
       legend-sample-length      ; heigh and width of the color sample box
       legend-line-pad           ; padding between lines
       legend-entry-pad)         ; top/bottom and left/right padding around legend
      legend-config?
      (legend-font legend-font legend-font-set!)
      (legend-color legend-color legend-color-set!)
      (legend-bg-color legend-bg-color legend-bg-color-set!)
      (legend-stroke-width legend-stroke-width legend-stroke-width-set!)
      (legend-corner-radius legend-corner-radius legend-corner-radius-set!)
      (legend-horizontal-offset legend-horizontal-offset legend-horizontal-offset-set!)
      (legend-vertical-offset legend-vertical-offset legend-vertical-offset-set!)
      (legend-sample-area-width legend-sample-area-width legend-sample-area-width-set!)
      (legend-sample-length legend-sample-length legend-sample-length-set!)
      (legend-line-pad legend-line-pad legend-line-pad-set!)
      (legend-entry-pad legend-entry-pad legend-entry-pad-set!))
    
    (define (make-legend-config . args)
      (let-keywords args
        ((font              (font "Helvetica" 10)) ; font used in legends
         (color             black)  ; width of a stroke for drawing the bounding box
         (bg-color          white)  ; width of a stroke for drawing the bounding box
         (stroke-width      1)      ; width of a stroke for drawing the bounding box
         (corner-radius     0)      ; radius for rounded bounding box
         (horizontal-offset 70)     ; horizontal offset from chart bounds (neg = right offset)
         (vertical-offset   10)     ; vertical offset from chart bounds (neg = bottom offset)
         (sample-area-width 17)     ; width of the sample area
         (sample-length     10)     ; heigh and width of the color sample box
         (line-pad          3)      ; padding between lines
         (entry-pad         6))     ; top/bottom and left/right padding around legend
        (legend-config
          font
          color
          bg-color
          stroke-width
          corner-radius
          horizontal-offset
          vertical-offset
          sample-area-width
          sample-length
          line-pad
          entry-pad)))
  )
)
