;;; LISPKIT PDF
;;;
;;; This is a simple library for creating PDF documents. The implementation of the library is
;;; based on cl-pdf, a Common Lisp library for generating PDF files, written by Marc Battyani.
;;; The library was ported to Scheme by Bruce Butterfield and packed up as a R7RS library by
;;; Peter Lane. Matthias Zenger ported it to LispKit, replacing dependecies on slib and other
;;; libraries with libraries directly provided by LispKit.
;;;
;;; Copyright information:
;;;
;;; cl-pdf is a Common Lisp library for generating PDF files.
;;;
;;;   It is distributed under a FreeBSD style license
;;;   (if you want another license contact me) marc.battyani@fractalconcept.com
;;;
;;;   Copyright (c) 2002 Marc Battyani. All rights reserved.
;;;
;;;   Redistribution and use in source and binary forms, with or without modification, are
;;;   permitted provided that the following conditions are met:
;;;
;;;   Redistributions of source code must retain the above copyright notice, this list of
;;;   conditions and the following disclaimer.
;;;
;;;   Redistributions in binary form must reproduce the above copyright notice, this list of
;;;   conditions and the following disclaimer in the documentation and/or other materials
;;;   provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE MARC BATTYANI ``AS IS'' AND ANY EXPRESS OR IMPLIED
;;;   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
;;;   AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL MARC BATTYANI OR
;;;   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;   GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;;;   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
;;;   EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;   The latest version is at http://www.fractalconcept.com/asp/html/cl-pdf.html
;;;   You can contact me at marc.battyani@fractalconcept.com or marc@battyani.net
;;;
;;; pdf.ss is a Scheme port of cl-pdf
;;;
;;;   Author: Bruce Butterfield <bab@entricom.com>
;;;
;;;   The port from Common Lisp was done as "Scheme-ishly" as possible; most of the changes
;;;   from the original code involved mapping CLOS objects to structures and associated
;;;   functions. I would have used the PLT class library but I wanted to be able to use this
;;;   code in other Scheme implementations; structures/records are a bit more universal.
;;;
;;; R7RS port
;;;   Packaged for R7RS-compliant Schemes by Peter Lane in 2017.
;;;   TODO: port-position not available, so removed xref offsets
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2017 Matthias Zenger. All rights reserved.

(define-library (lispkit pdf)
  
  (export *page-stream*
          set-page-stream
          in-text-mode
          set-font
          move-to-next-line
          draw-text
          move-text
          draw-text-on-next-line
          set-text-rendering-mode
          set-char-spacing
          set-text-x-scale
          set-text-leading
          set-text-rise
          set-text-matrix
          draw-and-adjust-string
          escape
          with-saved-state
          rotate
          translate
          scale
          set-line-width
          set-line-cap
          set-line-join
          set-dash-pattern
          set-mitter-limit
          move-to
          line-to
          polyline
          bezier-to
          bezier2-to
          bezier3-to
          close-path
          basic-rect
          stroke
          close-and-stroke
          fill-path
          close-and-fill
          even-odd-fill
          fill-and-stroke
          even-odd-fill-and-stroke
          close-fill-and-stroke
          close-even-odd-fill-and-stroke
          end-path-no-op
          clip-path
          even-odd-clip-path
          set-gray-stroke
          set-gray-fill
          set-rgb-stroke
          set-rgb-fill
          set-cymk-stroke
          set-cymk-fill
          arc
          pie
          circle
          ellipse
          rectangle
          regular-polygon
          star
          with-document
          with-document-to-file
          with-page
          build-font
          write-document
          font-name
          unit-size
          page-width
          page-height)

  (import (scheme base)
          (srfi 48))
  
  (begin

    (define (sign x)
      (cond ((negative? x) -1)
            ((positive? x) 1)
            ((zero? x)     0)
            (else          (error "cannot determine sign" x))))
    
    ;; output port
    
    (define *page-stream* (make-parameter #f))

    (define (set-page-stream port)
      (*page-stream* port))

    ;; text functions

    (define-syntax in-text-mode
      (syntax-rules ()
                    ((_ arg ...)
                     (begin
                       (write-string "BT\n" (*page-stream*))
                       arg ...
                       (write-string "ET\n" (*page-stream*))))))

    (define (set-font font size)                  
      (format (*page-stream*) "~a ~1,2F Tf~%" font size)) 

    (define-syntax define-pdf-op
      (syntax-rules ()
                    ((_ name tmpl)
                     (define name
                       (lambda ()
                         (format (*page-stream*) tmpl)
                         (format (*page-stream*) "~%"))))
                    ((_ name (arg ...) tmpl)
                     (define name
                       (lambda (arg ...)
                         (format (*page-stream*) tmpl arg ...))))))

    (define-pdf-op move-to-next-line " T*~%")
    (define-pdf-op draw-text (str) "(~a) Tj~%")
    (define-pdf-op move-text (dx dy) "~1,3F ~1,3F Td~%")
    (define-pdf-op draw-text-on-next-line (string) "(~a) '~%")
    (define-pdf-op set-text-rendering-mode (mode) "~d Tr~%")
    (define-pdf-op set-char-spacing (space) "~1,3F Tc~%")
    (define-pdf-op set-text-x-scale (scale) "~1,3F Tz~%")
    (define-pdf-op set-text-leading (space) "~1,3F TL~%")
    (define-pdf-op set-text-rise (rise) "~1,3F Ts~%")
    (define-pdf-op set-text-matrix (a b c d e f) "~1,3F ~1,3F ~1,3F ~1,3F ~1,3F ~1,3F Tm~%")

    (define (draw-and-adjust-string strings)
      (format (*page-stream*) "[ ")
      (for-each
        (lambda (str)
          (if (number? str)
            (format (*page-stream*) "~1,3F " str)
            (format (*page-stream*) "(~a) " str)))
        strings)
      (format (*page-stream*) "] TJ"))
    
    ;; escape special characters in strings
    (define (escape str)
      (let ((escaped (string-copy str)))
        (string-replace! escaped "\\\\" "\\\\\\")
        (string-replace! escaped "\\(" "\\\\(")
        (string-replace! escaped "\\)" "\\\\)")
        escaped))
   
    ;; graphic functions

    (define-syntax with-saved-state
      (syntax-rules ()
                    ((_ arg ...)
                     (begin
                       (format (*page-stream*) "q~%")
                       arg ...
                       (format (*page-stream*) "Q~%")))))

    (define (rotate deg)
      (let* ((angle (/ (* pi deg) 180.0))
             (s (sin angle))
             (c (cos angle)))
        (format (*page-stream*) "~1,3F ~1,3F ~1,3F ~1,3F 0.0 0.0 cm~%" c s (- s) c)))

    (define-pdf-op translate (dx dy) "1.0 0.0 0.0 1.0 ~1,3F ~1,3F cm~%")
    (define-pdf-op scale (sx sy) " ~1,3F 0.0 0.0 ~1,3F 0.0 0.0 cm~%")
    (define-pdf-op set-line-width (width) "~1,3F w~%")
    (define-pdf-op set-line-cap (mode) "~d J~%")
    (define-pdf-op set-line-join (mode) "~d j~%")
    (define-pdf-op set-dash-pattern (dash-array phase) "[~{~d~^ ~}] ~d~%") ;;; TODO - Fix pattern
    (define-pdf-op set-mitter-limit (limit) "~1,3F M~%")
    (define-pdf-op move-to (x y) "~1,3F ~1,3F m~%")
    (define-pdf-op line-to (x y) "~1,3F ~1,3F l~%")
    (define-pdf-op bezier-to (x1 y1 x2 y2 x3 y3) "~1,3F ~1,3F ~1,3F ~1,3F ~1,3F ~1,3F c~%")
    (define-pdf-op bezier2-to (x2 y2 x3 y3) "~1,3F ~1,3F ~1,3F ~1,3F v~%")
    (define-pdf-op bezier3-to (x1 y1 x3 y3) "~1,3F ~1,3F ~1,3F ~1,3F y~%")
    (define-pdf-op close-path " h")
    (define-pdf-op basic-rect (x y dx dy) "~1,3F ~1,3F ~1,3F ~1,3F re~%")
    (define-pdf-op stroke " S")
    (define-pdf-op close-and-stroke " s")
    (define-pdf-op fill-path " f")
    (define-pdf-op close-and-fill " h f")
    (define-pdf-op even-odd-fill " f*")
    (define-pdf-op fill-and-stroke " B")
    (define-pdf-op even-odd-fill-and-stroke " B*")
    (define-pdf-op close-fill-and-stroke " b")
    (define-pdf-op close-even-odd-fill-and-stroke " b*")
    (define-pdf-op end-path-no-op " n")
    (define-pdf-op clip-path " W")
    (define-pdf-op even-odd-clip-path " W*")
    (define-pdf-op set-gray-stroke (gray) "~1,3F G~%")
    (define-pdf-op set-gray-fill (gray) "~1,3F g~%")
    (define-pdf-op set-rgb-stroke (r g b) "~1,3F ~1,3F ~1,3F RG~%")
    (define-pdf-op set-rgb-fill (r g b) "~1,3F ~1,3F ~1,3F rg~%")
    (define-pdf-op set-cymk-stroke (c y m k) "~1,3F ~1,3F ~1,3F ~1,3F K~%")
    (define-pdf-op set-cymk-fill (c y m k) "~1,3F ~1,3F ~1,3F ~1,3F k~%")

    ;; geometry

    (define *2pi* (* 2 pi))
    (define *pi/2* (/ pi 2))

    (define (arc center-x center-y radius start extent)
      (move-to (+ center-x (* radius (cos start)))
               (+ center-y (* radius (sin start))))
      (arc-to center-x center-y radius start extent)
      (line-to center-x center-y))

    (define (pie center-x center-y radius start extent)
      (move-to center-x center-y)
      (line-to (+ center-x (* radius (cos start)))
               (+ center-y (* radius (sin start))))
      (arc-to center-x center-y radius start extent)
      (line-to center-x center-y))

    (define (circle center-x center-y radius)
      (move-to (+ center-x radius) center-y)
      (arc-to center-x center-y radius 0 *2pi*))

    (define (ellipse center-x center-y radius-a radius-b)
      (move-to (+ center-x radius-a) center-y)
      (let ((kappa (* 4 (/ (- (sqrt 2) 1) 3.0))))
        (bezier-to (+ center-x radius-a) (+ center-y (* kappa radius-b))
                   (+ center-x (* kappa radius-a)) (+ center-y radius-b)
                   center-x (+ center-y radius-b))
        (bezier-to (- center-x (* kappa radius-a)) (+ center-y radius-b)
                   (- center-x radius-a) (+ center-y (* kappa radius-b))
                   (- center-x radius-a) center-y)
        (bezier-to (- center-x radius-a) (- center-y (* kappa radius-b))
                   (- center-x (* kappa radius-a)) (- center-y radius-b)
                   center-x (- center-y radius-b))
        (bezier-to (+ center-x (* kappa radius-a)) (- center-y radius-b)
                   (+ center-x radius-a) (- center-y (* kappa radius-b))
                   (+ center-x radius-a) center-y)))

    (define (rectangle x y dx dy radius)
      (if (zero? radius)
        (basic-rect x y dx dy)
        (begin
          (move-to (+ x dx) (- (+ y dy) radius))
          (polyline (list (list x y)
                          (list (+ x dx) y)
                          (list (+ x dx) (+ y dy))
                          (list x (+ y dy)))
                    radius
                    #t))))

    (define-syntax dotimes
      (syntax-rules ()
                    ((_ (index maxval) body ...)
                     (do ((index 0 (+ index 1)))
                       ((= index maxval))
                       body ...))))

    (define (last-pair items)
      (list (car (reverse items))))

    (define (x-coord pt)
      (car (car pt)))

    (define (y-coord pt)
      (cadr (car pt)))

    (define (polyline points radius closed)
      (if (zero? radius)
        (let ((x1 (x-coord points))
              (y1 (y-coord points)))
          (move-to x1 y1)
          (let loop ((point (cdr points)))
            (if (not (null? point))
              (begin
                (line-to (x-coord point) (y-coord point))
                (loop (cdr point)))))
          (if closed
            (line-to x1 y1))))
      (begin
        (if closed
          (let ((break-point (midpoint (car points) (car (last-pair points)) 0.5)))
            (set! points `(,break-point ,@points ,break-point))))
        (move-to (x-coord points) (y-coord points))
        (dotimes (i (- (length points) 2))
          (let ((p1 (list-ref points i))
                (p2 (list-ref points (fx1+ i)))
                (p3 (list-ref points (fx1+ (fx1+ i)))))
            (fillet p2 p1 p3 radius)))
        (line-to (x-coord (last-pair points))
                 (y-coord (last-pair points)))))

    (define regular-polygon
      (case-lambda
        ((center-x center-y radius sides fillet-radius)
         (polyline
           (let ((step-angle (/ *2pi* sides)))
             (do ((current-angle *2pi* (+ current-angle step-angle))
                  (side 0 (+ side 1))
                  (lst '()))
               ((> side sides) lst)
               (set! lst (cons (list (+ center-x (* radius (cos current-angle)))
                                     (+ center-y (* radius (sin current-angle))))
                               lst))))
           fillet-radius #t))
        ((center-x center-y radius sides)
         (regular-polygon center-x center-y radius sides 0))))

    (define star
      (case-lambda
        ((center-x center-y ext-radius int-radius sides fillet-radius)
         (let* ((current-angle *pi/2*)
                (step-angle (/ *2pi* sides))
                (half-step (/ step-angle 2.0))
                (points '()))
           (dotimes (i sides)
             (set! points 
               (cons (list (+ center-x (* ext-radius (cos current-angle)))
                           (+ center-y (* ext-radius (sin current-angle))))
                     points))
             (set! points
               (cons (list (+ center-x (* int-radius (cos (+ current-angle half-step))))
                           (+ center-y (* int-radius (sin (+ current-angle half-step)))))
                     points))
             (set! current-angle (+ current-angle step-angle)))
           (polyline points fillet-radius #t)))
        ((center-x center-y ext-radius int-radius sides)
         (star center-x center-y ext-radius int-radius sides 0))))


    ;;; Non exported functions

    (define (arc-to center-x center-y radius start extent)
      ;; An arc of extent zero will generate an error at bezarc (divide by zero).
      ;; This case may be given by two aligned points in a polyline.
      ;; Better do nothing.
      (unless (zero? extent)
        (if (<= (abs extent) (/ pi 2.0))
          (let-values (((x1 y1 x2 y2 x3 y3)
                        (bezarc center-x center-y radius start extent)))
            (bezier-to x1 y1 x2 y2 x3 y3))
          (let ((half-extent (/ extent 2.0)))
            (arc-to center-x center-y radius start half-extent)
            (arc-to center-x center-y radius (+ start half-extent) half-extent)))))

    (define (bezarc center-x center-y radius start extent)
      ;; start and extent should be in radians.
      ;; Returns first-control-point-x first-control-point-y
      ;;         second-control-point-x second-control-point-y
      ;;         end-point-x end-point-y
      (let* ((end (+ start extent))
             (s-start (sin start)) (c-start (cos start))
             (s-end (sin end)) (c-end (cos end))
             (ang/2 (/ extent 2.0))
             (kappa (* (/ 4.0 3.0)
                       (/ (- 1 (cos ang/2))
                          (sin ang/2))))
             (x1 (- c-start (* kappa s-start)))
             (y1 (+ s-start (* kappa c-start)))
             (x2 (+ c-end   (* kappa s-end)))
             (y2 (- s-end   (* kappa c-end))))
        (values (+ (* x1 radius) center-x) (+ (* y1 radius) center-y)
                (+ (* x2 radius) center-x) (+ (* y2 radius) center-y)
                (+ (* c-end radius) center-x) (+ (* s-end radius) center-y))))


    (define (distance p1 p2)
      (sqrt (+ (expt (- (car p2)  (car p1)) 2) (expt (- (cadr p2) (cadr p1)) 2))))

    (define (angle2 p1 p2)
      (if (zero? (distance p1 p2))
        0.0
        (atan (- (cadr p2) (cadr p1)) (- (car p2) (car p1)))))

    ;;;============================================================================;
    ;;;
    ;;; (angle-3points <point> <point> <point>)
    ;;;
    ;;; Devuelve el angulo en radianes entre tres puntos.  Se considera el punto
    ;;; 'pt1' como vertice del angulo.  El rango del angulo de salida es [+Pi -Pi)
    ;;;

    (define (angle-3points pt1 pt2 pt3)
      (let* ((ang (- (angle2 pt1 pt3) (angle2 pt1 pt2))))
        (if (or (> ang pi) (<= ang (- pi)))
          (- ang (* (sign ang) *2pi*))
          ang)))


    ;;;============================================================================;
    ;;;
    ;;; (midpoint <point> <point> <real>)
    ;;;
    ;;; Returns a point between the two points given as an argument. Factor `ratio` indicates
    ;;; the ratio of the distances between the points `pt1` and `pt2`.
    ;;;

    (define (midpoint pt1 pt2 ratio)
      (let ((x1 (car pt1))
            (y1 (cadr pt1))
            (x2 (car pt2))
            (y2 (cadr pt2)))
        (list (+ x1 (* ratio (- x2 x1)))
              (+ y1 (* ratio (- y2 y1))))))

    ;; This function is the support to create rounded polylines
    ;;
    ;; p1 = corner
    ;; p2 = start
    ;; p3 = end
    ;; -> no useful return value
    (define (fillet p1 p2 p3 radius)
      (let* ((gamma      (/ (abs (angle-3points p1 p2 p3)) 2))
             (dist-p1-t  (/ radius (tan gamma)))
             (dist-p1-s  (/ (sqrt (+ (expt radius 2) (expt dist-p1-t 2))) (cos gamma)))
             (dist-p1-p2 (distance p1 p2))
             (dist-p1-p3 (distance p1 p3)))
        (if (or (< dist-p1-p2 dist-p1-t) (< dist-p1-p3 dist-p1-t))
            ;; Radius is too large, so we aren't going to draw the arc.
            (line-to (car p1) (cadr p1))
            ;; Else, draw the arc.
            (let ((t2     (midpoint p1 p2 (/ dist-p1-t dist-p1-p2)))
                  (t3     (midpoint p1 p3 (/ dist-p1-t dist-p1-p3)))
                  (center (midpoint (midpoint p1 p2 (/ dist-p1-s dist-p1-p2))
                                    (midpoint p1 p3 (/ dist-p1-s dist-p1-p3))
                                    0.5)))
              (line-to (car t2) (cadr t2))
              (arc-to (car center) (cadr center) radius
                      (angle2 center t2) (angle-3points center t2 t3))))))
    
    ;; structure definitions

    (define-record-type <doc>
                        (make-doc catalog root-page pages xref objects fonts)
                        doc?
                        (catalog doc-catalog)
                        (root-page doc-root-page)
                        (pages doc-pages doc-pages-set!)
                        (xref doc-xref doc-xref-set!)
                        (objects doc-objects doc-objects-set!)
                        (fonts doc-fonts doc-fonts-set!))

    (define-record-type <indirect-obj>
                        (make-indirect-obj obj-number gen-number content name)
                        indirect-obj?
                        (obj-number indirect-obj-obj-number)
                        (gen-number indirect-obj-gen-number)
                        (content indirect-obj-content)
                        (name indirect-obj-name indirect-obj-name-set!))

    (define-record-type <dictionary>
                        (make-dictionary values)
                        dictionary?
                        (values dictionary-values dictionary-values-set!))
    
    (define-record-type <pdf-stream>
                        (make-pdf-stream content)
                        pdf-stream?
                        (content pdf-stream-content))

    ;; constants
    (define *unit-size*         72)  ; default 72 points per inch
    (define *default-width*    612)  ; in units, 8.5x11
    (define *default-height*   792)  ; in units, 8.5x11

    ;; handy parameters
    (define *output*           (make-parameter #f))
    (define *document*         (make-parameter #f))  
    (define *page*             (make-parameter #f))
    (define *next-obj-number*  (make-parameter 0))
    (define *next-var-number*  (make-parameter 100))
    (define *page-width*       (make-parameter *default-width*))
    (define *page-height*      (make-parameter *default-height*))

    (define (reset-parameters)
      (*output*          #f)
      (*document*        #f)
      (*page*            #f)
      (*next-obj-number* 0)
      (*next-var-number* 100)
      (*page-height*     *default-height*)
      (*page-width*      *default-width*))

    ;; structure builder funcs

    (define-syntax enforce-/
      (syntax-rules ()
                    ((_ arg)
                     (unless (char=? (string-ref arg 0) #\/)
                       (set! arg (string-append "/" arg))))
                    ((_ arg1 arg2 ...)
                     (begin
                       (enforce-/ arg1)
                       (enforce-/ arg2 ...)))))

    (define (build-indirect-obj content)
      (let ((obj (make-indirect-obj (get-next-obj-number) 0 content "indirect-obj")))
        (if (*document*)
          (doc-objects-set! (*document*) (cons obj (doc-objects (*document*)))))
        obj))

    (define (build-dictionary values)
      (let ((obj (make-dictionary values)))
        obj))

    (define (build-pdf-stream content)
      (let ((obj (make-pdf-stream content)))
        obj))

    (define (build-font base-font)
      (enforce-/ base-font)
      (let ((obj (build-indirect-obj
                   (build-dictionary `(("/Type" . "/Font")
                                       ("/Subtype" . "/Type1")
                                       ("/BaseFont" . ,base-font)
                                       ("/Encoding" . "/WinAnsiEncoding"))))))
        (indirect-obj-name-set! obj (gen-name "/CLF"))
        (doc-fonts-set! (*document*) (cons obj (doc-fonts (*document*))))
        obj))

    (define (build-page width height content)
      (let* ((root-page (doc-root-page (*document*)))
             (res-obj (build-dictionary `(("/Xobject" . ,(build-dictionary '()))
                                          ("/Font" . ,(lambda () (get-document-font-refs))))))
             (obj (build-indirect-obj
                    (build-dictionary `(("/Type" . "/Page")
                                        ("/Parent" . ,(lambda () (get-obj-ref root-page)))
                                        ("/MediaBox" . #(0 0 ,width ,height))
                                        ("/Resources" . ,res-obj)
                                        ("/Contents" . ,(lambda () (get-obj-ref content))))))))
        obj))

    (define (build-doc)
      (let* ((root-page (build-indirect-obj 
                          (build-dictionary `(("/Type" . "/Pages")
                                              ("/Count" . ,(lambda () (page-count)))
                                              ("/Kids" . ,(lambda () (page-refs)))))))
             (catalog (build-indirect-obj
                        (build-dictionary `(("/Type" . "/Catalog")
                                            ("/Pages" . ,(lambda () (get-obj-ref root-page))))))))
        (let ((obj (make-doc catalog root-page '() '((0 65535 f)) '() '())))
          obj)))

    ;; writers

    (define (write-obj obj)
      (cond ((indirect-obj? obj)
             (write-indirect-obj obj))
            ((dictionary? obj)
             (write-dictionary obj))
            ((pdf-stream? obj)
             (write-pdf-stream obj))
            ((procedure? obj)
             (write-obj (obj)))
            ((vector? obj)
             (format (*output*) "[ ")
             (for-each
               (lambda (x)
                 (write-obj x))
               (vector->list obj))
             (format (*output*) "] "))
            (else
              (format (*output*) "~a " obj))))

    (define (write-dictionary obj)
      (format (*output*) "<< ")
      (for-each
        (lambda (x)
          (write-obj (car x))
          (write-obj (cdr x))
          (format (*output*) "~%"))
        (dictionary-values obj))
      (format (*output*) ">> "))

    (define (write-indirect-obj obj)
      (let ((offset 0))
        (doc-xref-set! (*document*) (cons (list offset 0 'n) (doc-xref (*document*))))
        (format (*output*) "~d ~d obj~%" (indirect-obj-obj-number obj) (indirect-obj-gen-number obj))
        (write-obj (indirect-obj-content obj))
        (format (*output*) "~%endobj~%")))

    (define (write-pdf-stream obj)
      (let ((content (pdf-stream-content obj)))
        (format (*output*) "<< /Length ~d~%>>~%stream~%~a~%endstream~%"
                (string-length content)
                content)))

    (define write-document
      (lambda (file)
        (*output* (open-output-file file))
        (format (*output*) "%PDF-1.3~%")
        (write-obj (doc-root-page (*document*)))
        (write-obj (doc-catalog (*document*)))
        (for-each
          (lambda (x)
            (write-obj x))
          (reverse (doc-objects (*document*))))
        (let ((xref-offset 0))
          (format (*output*) "xref~%~d ~d~%" 0 (length (doc-xref (*document*))))
          (for-each
            (lambda (x)
              (format (*output*) "~10,0F ~5,0F ~a ~%" (car x) (cadr x) (caddr x))) 
            (reverse (doc-xref (*document*))))
          (let ((docobjs (fx1+ (length (doc-objects (*document*))))))
            (format (*output*)
                    "trailer ~%<< /Size ~d /Root ~a~%>>~%"
                    docobjs
                    (get-obj-ref (doc-catalog (*document*)))))
          (format (*output*) "startxref~%~d~%%%EOF~%" xref-offset))
        (close-output-port (*output*))))


    ;; utilities

    (define (add-page page)
      (doc-pages-set! (*document*) (cons page (doc-pages (*document*)))))

    (define (page-count)
      (if (*document*)
        (length (doc-pages (*document*)))
        0))

    (define (page-refs)
      (if (*document*)
        (list->vector (map get-obj-ref (reverse (doc-pages (*document*)))))
        (list->vector '())))

    (define (add-dictionary-item dict name value)
      (dictionary-values-set! dict (cons (cons name value) (dictionary-values dict))))

; not used
;    (define (get-dictionary-value dict name)
;      (cdr (assoc name (dictionary-values dict))))
;
;    (define (set-dictionary-value dict name value)
;      (set-cdr! (assoc name (dictionary-values dict)) value))

    (define (get-obj-ref obj)
      (cond ((indirect-obj? obj)
             (format #f "~d ~d R" 
                     (indirect-obj-obj-number obj)
                     (indirect-obj-gen-number obj)))
            ((procedure? obj)
             (get-obj-ref (obj)))
            (else
              (error "get-obj-ref: not an indirect-obj" obj))))

    (define (get-font-ref obj)
      (if (indirect-obj? obj)
        (build-dictionary `((,(indirect-obj-name obj) . ,(get-obj-ref obj))))
        (error "get-font-ref: not an indirect-obj" obj)))

    (define (gen-name prefix)
      (*next-var-number* (+ 1 (*next-var-number*)))
      (format #f "~a~d" prefix (*next-var-number*)))

    (define (get-document-font-refs)
      (let ((fonts (doc-fonts (*document*)))
            (dict (build-dictionary '())))
        (for-each
          (lambda (x)
            (let ((font-ref (get-font-ref x)))
              (add-dictionary-item dict 
                                   (caar (dictionary-values font-ref)) 
                                   (cdar (dictionary-values font-ref)))))
          fonts)
        dict))

    (define (font-name font)
      (indirect-obj-name font))

    (define (get-next-obj-number)
      (*next-obj-number* (+ 1 (*next-obj-number*)))
      (*next-obj-number*))

    (define (page-height)
      (*page-height*))

    (define (page-width)
      (*page-width*))

    (define (unit-size)
      *unit-size*)

    ;; helpful document structure macros

    (define-syntax with-output-to-string
      (syntax-rules ()
                    ((_ body ...)
                     (let ((s-port (open-output-string)))
                       (set-page-stream s-port)
                       body ...
                       (get-output-string s-port)))))

    (define-syntax with-document
      (syntax-rules ()
                    ((_ body ...)
                     (begin
                       (reset-parameters)
                       (*document* (build-doc))
                       body ...))))

    (define-syntax with-document-to-file
      (syntax-rules ()
                    ((_ filename body ...)
                     (begin
                       (reset-parameters)
                       (*document* (build-doc))
                       body ...
                       (write-document filename)))))


    (define-syntax with-page
      (syntax-rules ()
                    ((_ (width height) body ...)
                     (begin
                       (*page-width* width)
                       (*page-height* height)
                       (let* ((pdf-stream
                                (build-pdf-stream
                                  (with-output-to-string body ...)))
                              (content (build-indirect-obj pdf-stream))
                              (page (build-page width height content)))
                         (*page* page)
                         (add-page (*page*)))))
                    ((_ body ...) ; default media box size
                     (with-page (*default-width* *default-height*) body ...))))

  )
)
