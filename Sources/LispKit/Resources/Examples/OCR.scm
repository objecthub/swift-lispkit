;;; OCR of document on photo
;;; 
;;; This is a larger demo that combines the usage of the libraries (lispkit draw),
;;; (lispkit image), (lispkit vision), and (lispkit pdf). This program first
;;; loads the photo of a printed document, it then detects the document in that
;;; photo and applies a perspective correction to obtain a rectangular image
;;; representation of the document. This image is written to a file. Next,
;;; optical character recognition (OCR) is applied to the image and the recognized
;;; text printed into the console. An illustrated PDF file showing the document
;;; together with annotations referring to the recognized text gets then created
;;; and saved. Finally, a similar PDF file is created using library (lispkit pdf)
;;; but instead of drawing the annotations on top of the document image, the
;;; annotations are included as editable PDF annotations.
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
        (lispkit iterate)
        (lispkit draw)
        (lispkit image)
        (lispkit vision)
        (lispkit pdf)
        (lispkit thread future))

;; Returns the outermost polygon for a given list of polygons `points`. `current` is
;; the initial outermost polygon and must be set to `#f` for the initial call.
(define (outer-polygon current points)
  (if (pair? points)
      (if (not current)
          (outer-polygon (cons (closed-polygon (car points)) (car points)) (cdr points))
          (if (shape-contains? (car current) (car points))
              (outer-polygon current (cdr points))
              (let ((poly (closed-polygon (car points))))
                (if (shape-contains? poly (cdr current))
                    (outer-polygons (cons poly (car points)) (cdr points))
                    #f))))
      current))

;; Shrinks the given rectangle `rct` by `pct` percent.
(define (shrink rct pct)
  (inset-rect rct (/ (* (rect-width rct) pct) 200.0) (/ (* (rect-height rct) pct) 200.0)))

;; Returns a perspective and color corrected image representing a
;; scanned piece of paper (or any rectangular object).
(define (perspective-color-corrected img candidates)
  (if-let*
    (((pair? candidates))
      ; Determine the outermost polygon and fail if that does not exist
      (poly (outer-polygon #f (map caddr candidates)))
      ; Correct the perspective of the photo based on `poly`
      (pers (make-image-filter 'perspective-correction img
              `((input-top-left . ,(map-image-point (list-ref (cdr poly) 0) img))
               (input-top-right . ,(map-image-point (list-ref (cdr poly) 1) img))
               (input-bottom-right . ,(map-image-point (list-ref (cdr poly) 2) img))
               (input-bottom-left . ,(map-image-point (list-ref (cdr poly) 3) img)))))
      ; Crop the photo 5% at each document border
      (crop (make-image-filter 'crop pers
              `((input-rectangle .
                 ,(shrink (abstract-image-bounds (image-filter-output pers)) 10)))))
      ; Sharpen the image
      (sharp (make-image-filter 'sharpen-luminance crop
              `((input-sharpness . 1.0)
                (input-radius . 5.0))))
      ; Dial up exposure a little
      (exp (make-image-filter 'exposure-adjust sharp
              `((input-e-v . 0.7))))
      ; Increase contrast
      (col (make-image-filter 'color-controls exp
              `((input-saturation . 0)
                (input-brightness . 0)
                (input-contrast . 2.5))))
      (out (image-filter-output col)))
    (abstract-image->image
      ; Perform 90 degree rotation, if needed, to make sure the image is in a
      ; portrait aspect ratio
      (if (> (rect-width (abstract-image-bounds out)) (rect-height (abstract-image-bounds out)))
          (apply-image-filter out `(affine-transform (input-transform . ,(rotate 1.5708))))
          out))
    #f))

;; Load Helvetica font
(define helvetica (font "Helvetica" 34))

;; Draw number `i` in a circle in `box` in "Helvetica" font size 34.
(define (draw-number i box)
  (let* ((mid (/ (rect-height box) 2))
         (bbox (rect (move-point (rect-point box) -70 (- mid 24)) (size 48 48)))
         (str (number->string i))
         (dim (text-size str helvetica)))
    (fill-ellipse bbox)
    (fill-rect (rect (rect-point box) (size 10 10)))
    (draw-text
      str
      (move-point (rect-point bbox) (/ (- 48 (size-width dim)) 2) 3)
      helvetica
      white)))

;; Path to the generated files
(define target-path (car (system-directory 'documents)))

;; Load photo from asset library
(define photo (load-image (asset-file-path "Page" "jpg" "Images")))

;; Detect rectangles in the photo (with default parameters).
;; The result of this operation is a future for detection results.
(define rectangles (detect-rectangles photo))

;; Correct perspective, improve colors, and crop document
(define document (perspective-color-corrected photo (future-get rectangles)))

;; Save the document in a JPG file
(save-bitmap (path target-path "ExtractedDocument.jpg") document 'jpg)
(display "Saved extracted document image to ")
(display (path target-path "ExtractedDocument.jpg"))
(newline)

;; Recognize text in the document image (using default parameters). This
;; returns a future eventually referring to the recognized text candidates.
(define recognized-text (recognize-text document))

;; Display the recognized text
(display "------------------------")
(newline)
(for obs in (future-get recognized-text)
  (display (recognized-text-string (cadr obs)))
  (newline))
(display "------------------------")
(newline)

;; Create a drawing that illustrates the recognized text in the document
;; with rectangles showing where text was detected and a counter for each
;; detected line of text.
(define illustrated-document
  (drawing
    ; First draw the color and perspective corrected page
    (draw-image document (point 0 0))
    ; Switch to color red
    (set-color red)
    (set-fill-color red)
    ; For every detected line of text, draw a bounding box
    (for (i obs) in (future-get recognized-text)
      (draw (closed-polygon (recognized-text-corners (cadr obs))) 2)
      (draw-number i (recognized-text-bounds (cadr obs))))))

;; Save the illustrated document in a PDF file
(save-drawing (path target-path "IllustratedDocument.pdf")
              illustrated-document
              (image-size document)
              "Detected text")
(display "Saved illustrated document to ")
(display (path target-path "IllustratedDocument.pdf"))
(newline)

;; The following code recreates the same PDF using the library (lispkit pdf)

;; Create a new empty PDF document and set a few document-level attributes
(define doc (make-pdf))
(pdf-attribute-set! doc 'Title "Annotated Document")
(pdf-attribute-set! doc 'Author "Matthias Zenger")
(pdf-attribute-set! doc 'Subject "Annotated PDF document created with (lispkit pdf)")

;; Create a new PDF page on the basis of the extracted document image
;; and attach it to the empty PDF document
(define page (make-pdf-page document 0.0 #f #f #f))
(pdf-insert-page! doc 0 page)

;; Iterate through the recognized text and drop two types of annotations onto
;; the PDF page: 1) red rectangles indicate recognized text, 2) text popups
;; displaying the recognized text.
(for (i obs) in (future-get recognized-text)
  (let* ((box (recognized-text-bounds (cadr obs)))
         (mid (/ (rect-height box) 2))
         (tbox (rect (move-point (rect-point box) -60 (- mid 20)) (size 40 40)))
         (pbox (rect (move-point (rect-point box) 60 (- mid 60)) (size 200 130))))
    ; Place a red box annotation over the line
    (let ((annot (make-pdf-annotation (map-image-rect box document) 'square)))
      (pdf-annotation-border-set! annot '(solid 2.0))
      (pdf-annotation-color-set! annot red)
      (pdf-page-annotation-add! page annot))
    ; Make a text annotation which pops up when the yellow note icon gets clicked
    (let ((text (make-pdf-annotation (map-image-rect tbox document) 'text))
          (popup (make-pdf-annotation (map-image-rect pbox document) 'popup)))
      (pdf-annotation-contents-set! text (recognized-text-string (cadr obs)))
      (pdf-annotation-alignment-set! text 'left)
      (pdf-annotation-icon-set! text 'note)
      (pdf-annotation-color-set! text yellow)
      (pdf-annotation-popup-set! text popup)
      (pdf-page-annotation-add! page text))))

;; Save the annotated document in a PDF file; annotations are regular
;; (editable) PDF annotations in this PDF file.
(save-pdf (path target-path "AnnotatedDocument.pdf")
          doc
          '((burn-in-annotations . #f)
            (save-text-from-ocr . #t)))
(display "Saved annotated document to ")
(display (path target-path "AnnotatedDocument.pdf"))
(newline)
