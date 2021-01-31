;;; Turtle graphics
;;;
;;; This is a demo of library (lispkit draw turtle). Three differerent fractal curves
;;; are drawn and saved as individual pages in a PDF document. These examples are based
;;; on the sample code for LILA, a historic Lisp interpreter for Mac OS 8.
;;; (see http://zenger.org/lila/).
;;;
;;; Usage: (save-graphics "turtle-demo.pdf")
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2018 Matthias Zenger. All rights reserved.
;;; Original copyright © 1994-03-06, Matthias Zenger.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed under the
;;; License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;;; either express or implied. See the License for the specific language governing permissions
;;; and limitations under the License.

(import (lispkit base)
        (lispkit draw)
        (lispkit draw turtle))

(define (fern size min)
   (if (>= size min)
       (begin
         (forward (* 0.18 size))
         (turn 4)
         (fern (* 0.82 size) min)
         (turn 58)
         (fern (* 0.40 size) min)
         (turn -122)
         (fern (* 0.40 size) min)
         (turn 60)
         (forward (* -0.18 size)))))

(define (fern-page)
  (display "Drawing fern page")
  (newline)
  (parameterize ((current-turtle (make-turtle 200 200 2.0)))
    (pen-color (color 0.0 0.5 0.0))
    (pen-size 0.3)
    (move -60 100)
    (heading -50)
    (fern 300.0 2.0)
    (list (turtle-drawing (current-turtle)) (size 610 460))))

(define (ccurve len angle min)
  (cond ((< len min)
          (heading angle)
          (forward len))
        (else
          (ccurve (* len 0.7071) (+ angle 45) min)
          (ccurve (* len 0.7071) (- angle 45) min))))

(define (ccurve-page)
  (display "Drawing c-curve page")
  (newline)
  (parameterize ((current-turtle (make-turtle 200 200 2.0)))
    (pen-color (color 0.0 0.0 0.5))
    (pen-size 0.3)
    (move -5 -25)
    (ccurve 120.0 0 1.0)
    (list (turtle-drawing (current-turtle)) (size 610 460))))

(define (dragon len angle min)
  (define ang2 (- 90 angle))
  (define (sdragon size positive)
    (cond ((< size min)
            (forward size))
          (positive
            (turn angle)
            (sdragon (* size 0.7071) #t)
            (turn -90)
            (sdragon (* size 0.7071) #f)
            (turn ang2))
          (else
            (turn (- ang2))
            (sdragon (* size 0.7071) #t)
            (turn 90)
            (sdragon (* size 0.7071) #f)
            (turn (- angle)))))
  (heading angle)
  (sdragon len #t))

(define (dragon-page)
  (display "Drawing dragon page")
  (newline)
  (parameterize ((current-turtle (make-turtle 200 200 2.0)))
    (pen-color (color 0.7 0.0 0.0))
    (pen-size 0.3)
    (move 10 -47)
    (dragon 180.0 90 1.0)
    (list (turtle-drawing (current-turtle)) (size 610 460))))

(define (save-graphics filename)
  (let ((pages (list (fern-page) (ccurve-page) (dragon-page))))
    (display "Saving drawings in PDF file ")
    (display filename)
    (newline)
    (save-drawings filename pages "Demo of library (lispkit draw turtle)")))
