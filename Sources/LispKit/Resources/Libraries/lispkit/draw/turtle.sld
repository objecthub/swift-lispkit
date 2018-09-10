;;; LISPKIT DRAW TURTLE
;;;
;;; This is a simple library implementing turtle graphics. A new turtle plane gets created
;;; by invoking `make-turtle`. `make-turtle` sets the origin of the plane as well as a scaling
;;; factor. A range of functions for modifying the state of a turtle plane is provided:
;;; 
;;;   - `(pen-up turtle)`: Lifts the turtle from the plane
;;;   - `(pen-down turtle)`: Drops the turtle onto the plane
;;;   - `(pen-color color turtle)`: Sets the current color of the turtle
;;;   - `(pen-size size turtle)`: Sets the size of the turtle pen
;;;   - `(home)`: Moves the turtle back to the origin
;;;   - `(move x y turtle)`: Moves the turtle to position `(x, y)`
;;;   - `(heading angle turtle)`: Sets the angle of the turtle (in radians)
;;;   - `(turn angle turtle)`: Turns the turtle by the given angle (in radians)
;;;   - `(left angle turtle)`: Turn left by the given angle (in radians)
;;;   - `(right angle turtle)`: Turn right by the given angle (in radians)
;;;   - `(forward length turtle)`: Moves forward by `length` units drawing a line if the
;;;     pen is down
;;;   - `(backward length turtle)`: Moves backward by `length` units drawing a line if the
;;;     pen is down
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2018 Matthias Zenger. All rights reserved.
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

(define-library (lispkit draw turtle)

  (export turtle?
          make-turtle
          turtle-drawing
          pen-up
          pen-down
          pen-color
          pen-size
          home
          move
          heading
          turn
          left
          right
          forward
          backward)

  (import (lispkit base)
          (lispkit draw))

  (begin

    (define-record-type turtle
                        (new-turtle drawing x y angle down)
                        turtle?
                        (drawing turtle-drawing)
                        (x turtle-x set-turtle-x!)
                        (y turtle-y set-turtle-y!)
                        (angle turtle-angle set-turtle-angle!)
                        (down turtle-pen-down? set-turtle-pen-down!))

    (define (make-turtle x y sc)
      (let ((drawing (make-drawing)))
        (enable-transformation (scale sc sc (translate x y)) drawing)
        (new-turtle drawing 0.0 0.0 0.0 #t)))

    (define (pen-up plane)
      (set-turtle-pen-down! plane #f))

    (define (pen-down plane)
      (set-turtle-pen-down! plane #t))

    (define (pen-color color plane)
      (set-color color (turtle-drawing plane)))

    (define (pen-size size plane)
      (set-line-width size (turtle-drawing plane)))

    (define (home plane)
      (move 0.0 0.0 plane))

    (define (move x y plane)
      (set-turtle-x! plane x)
      (set-turtle-y! plane y))

    (define (heading angle plane)
      (set-turtle-angle! plane (radian angle)))

    (define (turn angle plane)
      (set-turtle-angle! plane (+ (radian angle) (turtle-angle plane))))

    (define (left angle plane)
      (turn (- angle) plane))

    (define (right angle plane)
      (turn angle plane))

    (define (forward len plane)
      (let* ((angle (turtle-angle plane))
             (ox (turtle-x plane))
             (oy (turtle-y plane))
             (x (+ ox (* (cos angle) len)))
             (y (+ oy (* (sin angle) len))))
        (if (turtle-pen-down? plane)
          (draw-line (point ox oy) (point x y) (turtle-drawing plane)))
        (move x y plane)))

    (define (backward len plane)
      (forward (- len) plane))

    (define (radian angle)
      (inexact (/ (* angle pi) 180.0)))
  )
)

