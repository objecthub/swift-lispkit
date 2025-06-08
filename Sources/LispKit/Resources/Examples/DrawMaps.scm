;;; Draw a map including two locations and a line illustrating the distance
;;;
;;; This is a demo showing how to create images of maps and how to draw on top
;;; of such images. The demo illustrates the direct distance between two
;;; locations. It uses the `(lispkit draw map)` library to create an image of
;;; a map, and library `(lispkit location)` to determine geocodes of two given
;;; addresses and their distance.
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2025 Matthias Zenger. All rights reserved.
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
        (lispkit draw map)
        (lispkit location)
        (lispkit math stats)
        (lispkit thread future))

;; Load the font used for showing the distance label
(define label-font (font "Helvetica" 13))

;; Determine the center between two given locations
(define (center-location loc1 loc2)
  (location (mean (list (location-latitude loc1) (location-latitude loc2)))
            (mean (list (location-longitude loc1) (location-longitude loc2)))))

;; Compute the latitude/longitude span between two locations (and expand the
;; span slightly so that the locations are not at the edge)
(define (map-region loc1 loc2 f)
  (lat-long-span (* (abs (- (location-latitude loc1) (location-latitude loc2))) f)
                 (* (abs (- (location-longitude loc1) (location-longitude loc2))) f)))

;; Create a rect that is centered at point `pt` with the given size.
(define (centered-rect pt size)
  (rect (point (- (point-x pt) (/ (size-width size) 2))
               (- (point-y pt) (/ (size-height size) 2)))
        size))

;; Draw a location pin into the current drawing
(define (draw-location-pin pt)
  (draw-ellipse (centered-rect pt (size 14 14)))
  (fill-ellipse (centered-rect pt (size 7 7))))

;; Create a drawing of a map that includes locations `loc1` and `loc2` and illustrate
;; the direct distance between the two locations with a line and a distance label.
(define (distance-map-drawing loc1 loc2 msize)
  (let* (; Determine the center of the map
         (center   (center-location loc1 loc2))
         ; Create a map snapshot
         (snapshot (future-get (make-map-snapshot center (map-region loc1 loc2 1.15) msize)))
         ; Determine the points on the map image for the two locations and the center
         (pt1      (map-snapshot-point snapshot loc1))
         (pt2      (map-snapshot-point snapshot loc2))
         (pt3      (map-snapshot-point snapshot center))
         ; Compute the distance between the two locations
         (dist     (/ (location-distance loc1 loc2) 1000))
         ; Define the distance label text
         (label    (string-append (number->string dist 10 0 2 #t) " km distance"))
         ; Define the rect for the distance label
         (lrect    (centered-rect pt3 (increase-size (text-size label label-font) 12 4))))
    ; Create the drawing
    (drawing
      ; Draw the map image-size; if we wouldn't draw the image at the origin, we
      ; would need to ajust the coordinates for the points below accordingly.
      (draw-image (map-snapshot-image snapshot) (point 0 0))
      ; Draw the location pins
      (set-color red)
      (set-fill-color red)
      (set-line-width 1.5)
      (draw-location-pin pt1)
      (draw-location-pin pt2)
      ; Draw the distance line and label
      (draw-line pt1 pt2)
      (set-fill-color white)
      (fill (rectangle (rect-point lrect) (rect-size lrect) 8 8))
      (draw (rectangle (rect-point lrect) (rect-size lrect) 8 8))
      (draw-text label (centered-rect pt3 (text-size label label-font)) label-font red))))

;; Show a map drawing in a window which illustrates the distance between two
;; given addresses.
(define (location-distance-map addr1 addr2 msize)
  (distance-map-drawing (car (future-get (geocode addr1)))
                        (car (future-get (geocode addr2)))
                        msize))

;; Return a map drawing illustrating the distance between Zürich and Copenhagen.
(define map-drawing
  (location-distance-map "Zürich, Switzerland"
                         "Copenhagen, Denmark"
                         (size 600 600)))

;; Path to the generated PDF file
(define pdf-file-path
  (path (car (system-directory 'documents)) "map-zrh-cph.pdf"))

;; Save the map drawing in a PDF
(save-drawing pdf-file-path map-drawing (size 600 600))

;; Open the generated PDF file
(open-file pdf-file-path)

;; Return the path to the generated PDF and the map drawing
(values pdf-file-path map-drawing)
