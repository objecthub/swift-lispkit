;;; Generation of PDF files
;;; 
;;; This code is an adapted version of the PDF generation examples written by
;;; Marc Battyani for the Common Lisp library "cl-pdf". It is using a port of this
;;; library which is available for LispKit as library (lispkit pdf).
;;; 
;;; An example PDF can be generated, for example, via: (pdfex1 "example1.pdf").
;;; There are four examples provided: pdfex1, pdfex2, pdfex3, pdfex4.
;;; The absolut path of a generated PDF file can be determined using `file-path`; for
;;; instance: (file-path "example1.pdf").
;;; 
;;; License of the original cl-pdf library:
;;; 
;;;   cl-pdf is a Common Lisp library for generating PDF files.
;;;   It is distributed under a FreeBSD style license
;;;   (if you want another license contact me) marc.battyani@fractalconcept.com
;;;
;;;   Copyright © 2002 Marc Battyani. All rights reserved.
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
;;; Original port to Scheme as pdf.ss:
;;;   Author: Bruce Butterfield <bab@entricom.com>
;;;
;;;   The port from Common Lisp was done as "Scheme-ishly" as possible; most of the
;;;   changes from the original code involved mapping CLOS objects to structures and
;;;   associated functions. I would have used the PLT class library but I wanted to
;;;   be able to use this code in other Scheme implementations; structures/records
;;;   are a bit more universal.
;;;
;;; R7RS port:
;;;   Packaged for R7RS Scheme by Peter Lane in 2017
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2017 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (prefix (lispkit pdf) pdf:)
        (srfi 27))

(define-syntax dotimes
  (syntax-rules ()
    ((_ (index maxval) body ...)
      (do ((index 0 (+ index 1)))
          ((= index maxval))
        body ...))))

(define (pdfex1 filename)
  (pdf:with-document-to-file filename
    (let ((helvetica (pdf:build-font "Helvetica"))
          (courier (pdf:build-font "Courier")))
      (pdf:with-page
        (pdf:in-text-mode
          (pdf:set-font (pdf:font-name helvetica) 36)
          (pdf:move-text 100 750)
          (pdf:draw-text "scm-pdf: Example 1"))
        (pdf:in-text-mode
          (dotimes (i 25)
            (pdf:set-font (pdf:font-name helvetica) (* i 1.5))
            (pdf:move-text (+ i 5) (+ i 10))
            (pdf:draw-text "Helvetica")))
        (pdf:in-text-mode
          (dotimes (i 25)
            (pdf:set-font (pdf:font-name courier) (* i 1.5))
            (pdf:move-text (+ i 5) (- 50 (+ i 10)))
            (pdf:draw-text "Courier")))))))

(define (pdfex2 filename)
  (pdf:with-document-to-file filename
    (let ((helvetica (pdf:build-font "Helvetica")))
      (pdf:with-page
        (pdf:in-text-mode
          (pdf:set-font (pdf:font-name helvetica) 36)
          (pdf:move-text 100 750)
          (pdf:draw-text "scm-pdf: Example 2"))
        (pdf:translate 230 500)
        (do ((j 0 (+ j 1))
             (i 0.67 (* i 1.045)))
            ((= j 101))
          (pdf:in-text-mode
            (pdf:set-font (pdf:font-name helvetica) i)
            (pdf:move-text (* i 3) 0)
            (pdf:draw-text "rotation"))
          (pdf:rotate 18)))
      (pdf:with-page
        (pdf:in-text-mode
          (pdf:set-font (pdf:font-name helvetica) 40)
          (pdf:move-text 230 500)
          (pdf:draw-text "That's All, Folks!"))))))

(define (pdfex3 filename)
  (pdf:with-document-to-file filename
    (let ((helvetica (pdf:build-font "Helvetica")))
      (pdf:with-page (800 1000)
        (pdf:in-text-mode
          (pdf:set-font (pdf:font-name helvetica) 36.0)
          (pdf:move-text 100 900)
          (pdf:draw-text "scm-pdf: Example 3"))
        (pdf:move-to (+ 10 (random-integer 500))(+ 10 (random-integer 400)))
        (pdf:set-gray-fill 0.5)
        (dotimes (i 50)
                 (pdf:line-to (+ 50 (random-integer 500)) (+ 50 (random-integer 400))))
        (pdf:close-even-odd-fill-and-stroke)
        (pdf:move-to (+ 50 (random-integer 500))(+ 400 (random-integer 400)))
        (pdf:set-rgb-fill 0.5 0.5 0.8)
        (pdf:set-rgb-stroke 0.9 0.5 0.1)
        (dotimes (i 50)
              (pdf:bezier2-to (+ 50 (random-integer 500)) (+ 400 (random-integer 400))
                              (+ 50 (random-integer 500)) (+ 400 (random-integer 400))))
        (pdf:close-even-odd-fill-and-stroke)))))

(define (pdfex4 filename)
  (pdf:with-document-to-file filename
    (let ((helvetica (pdf:build-font "Helvetica")))
      (pdf:with-page
        (pdf:in-text-mode
          (pdf:set-font (pdf:font-name helvetica) 36.0)
          (pdf:move-text 100 750)
          (pdf:draw-text "scm-pdf: Example 4"))
        (pdf:set-rgb-stroke 0.1 0.1 0.1)
        (pdf:set-rgb-fill 0.8 0.8 0.8)
        (let ((x 50) (y 600))
          (dotimes (i 2)
                   (pdf:rectangle x y 500 140 10)
                   (pdf:close-fill-and-stroke)
                   (set! y (- y 180))))
        (pdf:translate 50 670)
        (let ((x 50) (y 0))
          (do ((i 0 (+ i 1))
               (j 8 (* j 1.05)))
              ((= i 4))
            (pdf:set-rgb-fill (* 0.1 j) (* 0.01 j) (* 0.02 j))
            (pdf:circle x y (* 4 j))
            (pdf:close-fill-and-stroke)
            (pdf:ellipse (+ x 250) y (* 5 j) (* 4 j))
            (pdf:close-fill-and-stroke)
            (set! x (+ x 50))))
       
        (pdf:translate 0 -180)
        (pdf:regular-polygon 150 0 50 7 8)
        (pdf:close-fill-and-stroke)
        (pdf:star 350 0 50 30 6 5)
        (pdf:close-fill-and-stroke)
       
        (pdf:set-rgb-fill 0.8 0.6 0.2)
        (pdf:regular-polygon 150 0 30 5 4)
        (pdf:close-fill-and-stroke)
        (pdf:star 350 0 40 20 4 6)
        (pdf:close-fill-and-stroke)
       
        (pdf:set-rgb-fill 0.4 0.8 0.7)
        (pdf:regular-polygon 150 0 15 3 3)
        (pdf:close-fill-and-stroke)
        (pdf:star 350 0 35 10 12 1)
        (pdf:close-fill-and-stroke)
       
        (pdf:set-line-width 0.5)
        (do ((r 2 (+ r 2)))
            ((= r 100))
          (pdf:set-rgb-stroke (* 0.01 (random-integer 100))
                              (* 0.01 (random-integer 100))
                              (* 0.01 (random-integer 100)))
          (pdf:arc 250 -230 r (* pi 0.001 (random-integer 2000))
                              (* pi 0.001 (random-integer 2000)))
          (pdf:stroke))))))
