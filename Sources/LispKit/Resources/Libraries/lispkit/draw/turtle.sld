;;; LISPKIT DRAW TURTLE
;;;
;;; This is a simple library implementing turtle graphics. A new turtle plane gets
;;; created by invoking `make-turtle`. `make-turtle` sets the origin of the plane
;;; as well as a scaling factor. A range of functions for modifying the state of a
;;; turtle plane is provided:
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
;;;   - `(forward length turtle)`: Moves forward by `length` units drawing a
;;;     line if the pen is down
;;;   - `(backward length turtle)`: Moves backward by `length` units drawing a
;;;     line if the pen is down
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2018-2023 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use
;;; this file except in compliance with the License. You may obtain a copy of the
;;; License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed
;;; under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
;;; CONDITIONS OF ANY KIND, either express or implied. See the License for the
;;; specific language governing permissions and limitations under the License.

(define-library (lispkit draw turtle)

  (export turtle?
          make-turtle
          current-turtle
          turtle-drawing
          turtle-x
          turtle-y
          turtle-angle
          turtle-pen-down?
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

    (define current-turtle (make-parameter #f))

    (define (pen-up . args)
      (let-optionals args ((turtle (current-turtle)))
        (set-turtle-pen-down! turtle #f)))

    (define (pen-down . args)
      (let-optionals args ((turtle (current-turtle)))
        (set-turtle-pen-down! turtle #t)))

    (define (pen-color color . args)
      (let-optionals args ((turtle (current-turtle)))
        (set-color color (turtle-drawing turtle))))

    (define (pen-size size . args)
      (let-optionals args ((turtle (current-turtle)))
        (set-line-width size (turtle-drawing turtle))))

    (define (home . args)
      (let-optionals args ((turtle (current-turtle)))
        (move 0.0 0.0 turtle)))

    (define (move x y . args)
      (let-optionals args ((turtle (current-turtle)))
        (set-turtle-x! turtle x)
        (set-turtle-y! turtle y)))

    (define (heading angle . args)
      (let-optionals args ((turtle (current-turtle)))
        (set-turtle-angle! turtle (radian angle))))

    (define (turn angle . args)
      (let-optionals args ((turtle (current-turtle)))
        (set-turtle-angle! turtle (+ (radian angle) (turtle-angle turtle)))))

    (define (left angle . args)
      (let-optionals args ((turtle (current-turtle)))
        (turn (- angle) turtle)))

    (define (right angle . args)
      (let-optionals args ((turtle (current-turtle)))
        (turn angle turtle)))

    (define (forward len . args)
      (let-optionals args ((turtle (current-turtle)))
        (let* ((angle (turtle-angle turtle))
               (ox (turtle-x turtle))
               (oy (turtle-y turtle))
               (x (+ ox (* (cos angle) len)))
               (y (+ oy (* (sin angle) len))))
          (if (turtle-pen-down? turtle)
            (draw-line (point ox oy) (point x y) (turtle-drawing turtle)))
          (move x y turtle))))

    (define (backward len . args)
      (let-optionals args ((turtle (current-turtle)))
        (forward (- len) turtle)))

    (define (radian angle)
      (inexact (/ (* angle pi) 180.0)))
  )
)
