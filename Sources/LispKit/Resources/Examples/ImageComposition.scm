;;; Transform, process and compose two photos
;;; 
;;; This is a demo of libraries (lispkit image), (lispkit image process),
;;; and (lispkit vision). It loads two images from the LispKit asset
;;; directory and projects one processed image onto a part of another image.
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2025 Matthias Zenger. All rights reserved.
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
        (lispkit vision)
        (lispkit image)
        (lispkit image process)
        (lispkit thread future))

;; Returns the biggest rect of the given aspect ratio `ratio` that is contained
;; in rect `rct`
(define (fit-rect rct ratio)
  (let ((width (* ratio (rect-height rct))))
    (if (<= width (rect-width rct))
        (rect (+ (rect-x rct) (/ (- (rect-width rct) width) 2.0))
              (rect-y rct)
              width
              (rect-height rct))
        (rect (rect-x rct)
              (+ (rect-y rct)
                 (/ (- (rect-height rct) (/ (rect-width rct) ratio)) 2.0))
              (rect-width rct)
              (/ (rect-width rct) ratio)))))

;; Load the billboard image
(define billboard-image
  (load-image (asset-file-path "Billboard" "jpg" "Images")))
(display "loaded billboard image")
(newline)

;; Maps the given billboard image point onto the corresponding
;; abstract image
(define (billboard-point pnt)
  (map-image-point pnt billboard-image))

;; Detect the billboard surface
(define detection-result
  (detect-rectangles billboard-image #f #f '(0.5 . 1.0) #f 0.3 0.5))

;; In the meantime, load an image of Inga
(define cat-image (load-image (asset-file-path "Inga" "jpg" "Images")))
(display "loaded cat image")
(newline)

;; Wait for the billboard surface to be determined
(define polygon (car (map caddr (future-get detection-result))))
(display "detected billboard polygon: ")
(write polygon)
(newline)

;; Determine the boundary of the perspective corrected polygon
;; representing the billboard 
(define rect
  (abstract-image-bounds
    (process-abstract-image
        (image->abstract-image billboard-image)
        (perspective-correction (billboard-point (list-ref polygon 0))
                                (billboard-point (list-ref polygon 1))
                                (billboard-point (list-ref polygon 2))
                                (billboard-point (list-ref polygon 3))
                                #t))))

;; Create an abstract image from the loaded cat image
(define cat-input (image->abstract-image cat-image))

;; Process the cat image and overlay it on top of the billboard image
(define output
  ; Put the perspective corrected cat image on top of a billboard image
  (process-abstract-image
    ; Change the perspective of the cat image
    cat-input
    ; Crop the cat image to a rect that has the needed aspect ratio
    (crop (fit-rect (abstract-image-bounds cat-input) (size-ratio (rect-size rect))))
    ; Turn the image into a bit more monochrome image
    (color-monochrome (color 0.82 0.74 0.59) 0.5)
    ; Transform the image into the given perspective moving it into the right spot
    (perspective-transform (move-point (billboard-point (list-ref polygon 0)) 0 5)
                           (move-point (billboard-point (list-ref polygon 1)) 0 5)
                           (billboard-point (list-ref polygon 2))
                           (billboard-point (list-ref polygon 3)))
    ; Put the billboard image underneath
    (source-atop-compositing (image->abstract-image billboard-image))))

(display "processed images and created output with bounds ")
(write (abstract-image-bounds output))
(newline)

;; Path to the generated JPG file
(define jpg-file-path
  (path (car (system-directory 'documents)) "cat-on-billboard.jpg"))

;; Save all drawings in a JPG file
(save-bitmap jpg-file-path (abstract-image->image output) 'jpg)

;; Open the generated JPG file
(open-file jpg-file-path)

;; Return the path to the generated JPG file
jpg-file-path

