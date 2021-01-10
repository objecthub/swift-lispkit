;;; Visualize points, lines and planes
;;; 
;;; This program visualizes points, lines and planes in a 3-dimensional cartesian coordinate
;;; system using an oblique projection as described in http://zenger.org/papers/fa.pdf (in
;;; German).
;;; 
;;; The oblique projection used to map 3-dimensional vectors into 2-dimensional screen
;;; coordinates is described in terms of the following parameters:
;;; 
;;;   - _bounds_ : a vector of ranges describing the space that is visualized (as a cube)
;;;   - _root_   : screen coordinates specifying the center of the visualized space
;;;   - _angles_ : a vector of angles (radian) describing the drawing angle of the axis
;;;                for all three dimensions
;;;   - _factors_: a vector of factors specifying the size of the visualized space on
;;;                the screen
;;; 
;;; The record `projection` implements such oblique projections. Function `make-projection`
;;; is used to create a new projection object consisting of the parameters listed above.
;;; 
;;; 3-dimensional point sets such as points, lines, and planes, are created using the
;;; following constructor functions:
;;; 
;;;   - `(point x)` creates a point; `x` is a vector of length 3 of floating-point
;;;     numbers describing the location of the point.
;;;   - `(line x u)` creates a line from a location vector `x` and a direction vector `u`.
;;;   - `(plane x u v)` creates a plane from a location vector `x` and two direction
;;;     vectors `u` and `v`.
;;; 
;;; A number of drawing functions can be used to create drawings containing visualizations
;;; of axis, the bounds of the displayed space (in form of a cube), as well as point sets:
;;; 
;;;   - `(draw-projection-axis proj d)` draws the three axis using projection `proj`
;;;     into the drawing `d`.
;;;   - `(draw-projection-cube proj d)` draws a cube depicting the boundaries of
;;;     projection `proj` into drawing `d`.
;;;   - `(draw-point-set proj ps lcol fcol d)` draws the point set `ps` into drawing
;;;     `d` using projection `proj`. `lcol` specifies the line color, `fcol` specifies
;;;     the surface/fill color used for planes.
;;;   - `(draw-projection proj pss d)` draws axis, the boundary cube, as well as the
;;;     point sets in list `pss` into drawing `d`. Elements of `pss` are either
;;;     point sets or lists of the form `(ps lcol fcol)` where `ps` is a point set, and
;;;     `lcol` and `fcol` are colors.
;;; 
;;; Invoking the following procedure will save two demo projections into a PDF file at the
;;; given path:
;;;   (save-demo-drawings "~/Desktop/ProjectionDemo.pdf")
;;; 
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2021 Matthias Zenger. All rights reserved.
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
        (rename (lispkit draw) (point coord)
                               (point-x coord-x)
                               (point-y coord-y)
                               (move-point move-coord)
                               (point? coord?)))

;; Utilities

(define (sqr x)
  (* x x))

(define (deg->rad w)
  (/ (* w pi) 180.0))

(define (rad->deg w)
  (/ (* w 180.0) pi))

;; Ranges

(define-values (new-range range? range-ref make-range-subtype) (make-type 'range))

(define (range min max)
  (assert (real? min) (real? max))
  (new-range (list (real->flonum min) (real->flonum max))))

(define (range-min rng)
  (car (range-ref rng)))

(define (range-max rng)
  (cadr (range-ref rng)))

(define (range-size rng)
  (- (range-max rng) (range-min rng)))

(define (fraction-in-range x rng)
  (/ (- x (range-min rng)) (range-size rng)))

(define (range-contains? rng x)
  (and (>= x (range-min rng)) (<= x (range-max rng))))

;; Coordinates

(define (add-coord p . ps)
  (if (null? ps)
      p
      (apply add-coord
             (coord (+ (coord-x p) (coord-x (car ps))) (+ (coord-y p) (coord-y (car ps))))
             (cdr ps))))

(define (magnitude+angle->coord mag angle)
  (coord (* mag (cos angle)) (- (* mag (sin angle)))))

;; Vectors with three elements

(define (vector-predicate f)
  (lambda (v)
    (and (vector? v)
         (= (vector-length v) 3)
         (f (vector-ref v 0))
         (f (vector-ref v 1))
         (f (vector-ref v 2)))))

(define real-vector? (vector-predicate real?))
(define range-vector? (vector-predicate range?))

(define (zero-vector? v)
  (and (zero? (vector-ref v 0))
       (zero? (vector-ref v 1))
       (zero? (vector-ref v 2))))

(define (round-number n)
  (/ (truncate (* (real->flonum n) 10000.0)) 10000.0))

(define (round-vector v)
  (vector-set! v 0 (round-number (vector-ref v 0)))
  (vector-set! v 1 (round-number (vector-ref v 1)))
  (vector-set! v 2 (round-number (vector-ref v 2)))
  v)

;; Oblique projection

(define-record-type projection
  (new-projection bounds root angles factors)
  projection?
  (bounds projection-bounds)
  (root projection-root set-projection-root!)
  (angles projection-angles)
  (factors projection-factors))

(define (make-projection bounds root angles factors)
  (assert (range-vector? bounds)
          (coord? root)
          (real-vector? angles)
          (real-vector? factors))
  (new-projection bounds root angles factors))

(define (copy-projection proj . args)
  (let-optionals args ((bounds #f)
                       (root #f)
                       (angles #f)
                       (factors #f))
    (make-projection
      (if bounds bounds (projection-bounds proj))
      (if root root (projection-root proj))
      (if angles angles (projection-angles proj))
      (if factors factors (projection-factors proj)))))

(define current-projection
  (make-parameter
    (make-projection (vector (range -10 10) (range -10 10) (range -10 10))
                     (coord 200 200)
                     (vector (deg->rad 0) (deg->rad 90) (deg->rad 225))
                     (vector 200.0 200.0 100.0))))

(define (projection-includes? proj v)
  (let ((bounds (projection-bounds proj)))
    (and (range-contains? (vector-ref bounds 0) (vector-ref v 0))
         (range-contains? (vector-ref bounds 1) (vector-ref v 1))
         (range-contains? (vector-ref bounds 2) (vector-ref v 2)))))

(define (projection-includes-zero? proj)
  (projection-includes? proj #(0 0 0)))

(define (projection-corners proj)
  (let* ((bounds (projection-bounds proj))
         (xbounds (vector-ref bounds 0))
         (ybounds (vector-ref bounds 1))
         (zbounds (vector-ref bounds 2)))
    (vector (vector (range-min xbounds) (range-min ybounds) (range-min zbounds))
            (vector (range-max xbounds) (range-min ybounds) (range-min zbounds))
            (vector (range-min xbounds) (range-max ybounds) (range-min zbounds))
            (vector (range-max xbounds) (range-max ybounds) (range-min zbounds))
            (vector (range-min xbounds) (range-min ybounds) (range-max zbounds))
            (vector (range-max xbounds) (range-min ybounds) (range-max zbounds))
            (vector (range-min xbounds) (range-max ybounds) (range-max zbounds))
            (vector (range-max xbounds) (range-max ybounds) (range-max zbounds)))))

;; Transform vectors into points

(define (projected-coord proj v i)
  (magnitude+angle->coord (* (- (fraction-in-range
                                  (vector-ref v i)
                                  (vector-ref (projection-bounds proj) i)) 0.5)
                             (vector-ref (projection-factors proj) i))
                          (vector-ref (projection-angles proj) i)))

(define (vector->coord proj v)
  (add-coord (projection-root proj)
             (projected-coord proj v 0)
             (projected-coord proj v 1)
             (projected-coord proj v 2)))

;; Point sets in a 3-dimensional space

(define-values (new-point-set point-set? point-set-ref make-point-set-subtype)
               (make-type 'point-set))

(define (point a)
  (assert (real-vector? a))
  (new-point-set (list a #(0 0 0) #(0 0 0))))

(define (line a u)
  (assert (real-vector? a) (real-vector? u))
  (new-point-set (list a u #(0 0 0))))

(define (plane a u v)
  (assert (real-vector? a) (real-vector? u) (real-vector? v))
  (new-point-set (list a u v)))

(define (point-set-location ps)
  (car (point-set-ref ps)))

(define (point-set-vectors ps)
  (apply values (point-set-ref ps)))

(define (point-set-point? ps)
  (let-values (((a u v) (point-set-vectors ps)))
    (and (zero-vector? u) (zero-vector? v))))

(define (point-set-line? ps)
  (let-values (((a u v) (point-set-vectors ps)))
    (not (boolean=? (zero-vector? u) (zero-vector? v)))))

(define (point-set-plane? ps)
  (let-values (((a u v) (point-set-vectors ps)))
    (and (not (zero-vector? u)) (not (zero-vector? v)))))

;; Polygons (lists of vectors)

(define (polygon? vs)
  (every? real-vector? vs))

(define (polygon-points-for-bound proj a u v i bound)
  (if (not (zero? (vector-ref u i)))
      (let ((j  (remainder (+ i 1) 3))
            (k  (remainder (+ i 2) 3))
            (p  (/ (- bound (vector-ref a i)) (vector-ref u i)))
            (q  (/ (vector-ref v i) (vector-ref u i)))
            (sa (vector 0.0 0.0 0.0))
            (su (vector 0.0 0.0 0.0)))
        (vector-set! sa i bound)
        (vector-set! sa j (+ (vector-ref a j) (* (vector-ref u j) p)))
        (vector-set! sa k (+ (vector-ref a k) (* (vector-ref u k) p)))
        (vector-set! su j (- (vector-ref v j) (* (vector-ref u j) q)))
        (vector-set! su k (- (vector-ref v k) (* (vector-ref u k) q)))
        (point-set->polygon proj (line sa su)))
      '()))

(define (polygon-points-for-dimension proj a u v i)
  (let ((bounds (vector-ref (projection-bounds proj) i)))
    (append (polygon-points-for-bound proj a u v i (range-min bounds))
            (polygon-points-for-bound proj a u v i (range-max bounds)))))

(define (polygon-points proj ps i)
  (let-values (((a u v) (point-set-vectors ps)))
    (if (zero? (vector-ref u i))
        (polygon-points-for-dimension proj a v u i))
        (polygon-points-for-dimension proj a u v i)))

(define (point-set->polygon proj ps)
  (if (point-set-point? ps)
      (let ((v (round-vector (point-set-location ps))))
        (if (projection-includes? proj v) (list v) '()))
      (append (polygon-points proj ps 0)
              (polygon-points proj ps 1)
              (polygon-points proj ps 2))))

(define (dimension-match n1 n2 rng)
  (and (= n1 n2) (or (= n1 (range-min rng)) (= n1 (range-max rng)))))

(define (adjacent-vectors? proj v1 v2)
  (let ((bounds (projection-bounds proj)))
    (or (dimension-match (vector-ref v1 0) (vector-ref v2 0) (vector-ref bounds 0))
        (dimension-match (vector-ref v1 1) (vector-ref v2 1) (vector-ref bounds 1))
        (dimension-match (vector-ref v1 2) (vector-ref v2 2) (vector-ref bounds 2)))))

(define (sort-polygon proj v vs nvs)
  (if (pair? vs)
      (if (equal? v (car vs))
          (sort-polygon proj v (cdr vs) nvs)
          (if (adjacent-vectors? proj v (car vs))
              (cons (car vs) (sort-polygon proj (car vs) (append (delete v (cdr vs)) nvs) '()))
              (sort-polygon proj v (cdr vs) (cons (car vs) nvs))))
      '()))

(define (normalize-polygon proj vs)
  (if (pair? vs)
      (cons (car vs) (sort-polygon proj (car vs) (cdr vs) '()))
      '()))

;;; Drawing procedures

(define (draw-solid-line proj v1 v2 d)
  (draw-line (vector->coord proj v1) (vector->coord proj v2) d))

(define (draw-line-with-angle c mag angle)
  (line-to (add-coord c (magnitude+angle->coord mag angle))))

(define (draw-outlined-text s c col d)
  (let ((fnt (font "Times" 14))
        (boldfnt (font "Times" 14 extrablack)))
    (for-each (lambda (dx)
      (for-each (lambda (dy)
        (draw-text s (move-coord c dx dy) boldfnt white d)) '(-2.0 -1.0 0.0 1.0 2.0)))
      '(-1.0 -0.5 0.0 0.5 1.0))
    (draw-text s c fnt col d)))

(define (draw-vector proj v1 v2 s col d)
  (let* ((c1 (vector->coord proj v1))
         (c2 (vector->coord proj v2))
         (dx (- (coord-x c2) (coord-x c1)))
         (dy (- (coord-y c1) (coord-y c2)))
         (adj (coord -6 -13))
         (w  (if (= dx 0)
                 (if (>= dy 0) 90.0 180.0)
                 (+ (rad->deg (atan (/ dy dx))) (if (>= dx 0.0) 0.0 180.0)))))
    (set-color (color 0.0 0.0 0.5) d)
    (draw-dashed
      (shape
        (move-to c1)
        (line-to c2)
        (draw-line-with-angle c2 10 (deg->rad (+ w 150)))
        (move-to c2)
        (draw-line-with-angle c2 10 (deg->rad (- w 150))))
      '(2.0 1.0)
      0 1.0 d)
    (draw-outlined-text s (add-coord c2 (magnitude+angle->coord 11 (deg->rad w)) adj) col d)))

(define (draw-axis proj i s d)
  (let ((from (vector 0.0 0.0 0.0))
        (to   (vector 0.0 0.0 0.0))
        (rng  (vector-ref (projection-bounds proj) i))
        (col  (color 0.0 0.0 0.7)))
    (vector-set! from i (range-min rng))
    (vector-set! to i (+ (* 1.3 (range-size rng)) (range-min rng)))
    (draw-vector proj from to s col d)))

(define (draw-projection-axis proj . args)
  (if (projection-includes-zero? proj)
      (let-optionals args ((d (current-drawing)))
        (draw-axis proj 0 "x₁" d)
        (draw-axis proj 1 "x₂" d)
        (draw-axis proj 2 "x₃" d))))

(define (draw-projection-cube proj . args)
  (let-optionals args ((d (current-drawing)))
    (do ((cns (projection-corners proj))
         (x 0 (+ x 1)))
        ((> x 3))
      (set-line-width 2.5 d)
      (draw-solid-line proj (vector-ref cns (* 2 x)) (vector-ref cns (+ (* 2 x) 1)) d)
      (draw-solid-line proj (vector-ref cns (* 2 x)) (vector-ref cns (+ (* 2 x) 1)) d)
      (draw-solid-line proj (vector-ref cns (+ x (if (> x 1) 2 0)))
                            (vector-ref cns (+ x (if (> x 1) 4 2))) d)
      (draw-solid-line proj (vector-ref cns x) (vector-ref cns (+ x 4)) d)
      (set-line-width 1 d))))

(define (point-set-shape proj ps)
  (let* ((vs  (point-set->polygon proj ps))
         (nvs (normalize-polygon proj vs))
         (s   (make-shape)))
    (case (length nvs)
      ((0 1) s)
      ((2)
        (move-to (vector->coord proj (car nvs)) s)
        (line-to (vector->coord proj (cadr nvs)) s)
        s)
      (else
        (move-to (vector->coord proj (car nvs)) s)
        (for-each (lambda (v) (line-to (vector->coord proj v) s)) (cdr nvs))
        (line-to (vector->coord proj (car nvs)) s)
        s))))

(define (draw-point-set proj ps . args)
  (let-optionals args ((lcol #f)
                       (fcol #f)
                       (d (current-drawing)))
    (let ((sh (point-set-shape proj ps)))
      (if lcol (set-color lcol d))
      (if fcol (set-fill-color fcol d))
      (fill sh d)
      (draw sh 1.0 d))))

(define (draw-projection proj pss . args)
  (let-optionals args ((d (current-drawing)))
    (draw-projection-cube proj d)
    (draw-projection-axis proj d)
    (for-each (lambda (ps)
                (if (point-set? ps)
                    (draw-point-set proj ps black (color 1 1 1 0) d)
                    (let-optionals ps ((qs #f)
                                       (lcol black)
                                       (fcol (color 1 1 1 0)))
                      (draw-point-set proj qs lcol fcol d)))) pss)))

;; Procedures for saving drawings

(define (page-drawing spec)
  (assert (pair? spec) (projection? (car spec)))  
  (let* ((d (make-drawing))
         (proj (car spec))
         (root (projection-root proj)))
    (draw-projection proj (cdr spec) d)
    (list d (size (* 2.0 (coord-x root)) (* 2.0 (coord-y root))))))

(define (save-projection-drawings path . args)
  (save-drawings path (map page-drawing args)))

(define (save-demo-drawings path)
  (let ((gray (color 0.5 0.5 0.5 0.15)))
    (save-projection-drawings
      path
      (list (current-projection)
            (list (plane #(0 0 0) #(1.0 0.1 -0.5) #(0.0 0.8 0.8))
                  (color 0.6 0.6 0.6) (color 0 0 1 0.15))
            (list (plane #(0 -7.5 0) #(1.0 0.0 0.0) #(0.0 0.0 1.0))
                  (color 0.6 0.6 0.6) (color 1 0 0 0.15)))
      (list (make-projection
              (vector (range -10 10) (range -10 10) (range -10 10))
              (coord 200 200)
              (vector (deg->rad 350) (deg->rad 90) (deg->rad 215))
              (vector 180.0 200.0 130.0))
            (list (plane #(0 0 0) #(0.0 0.0 1.0) #(1.0 0.0 0.0)) black gray)
            (list (plane #(0 0 0) #(0.0 0.0 1.0) #(1.7 1.0 0.0)) black gray)
            (list (plane #(0 0 0) #(0.0 0.0 1.0) #(1.0 1.7 0.0)) black gray)
            (list (plane #(0 0 0) #(0.0 0.0 1.0) #(0.0 1.0 0.0)) black gray)
            (list (plane #(0 0 0) #(0.0 0.0 1.0) #(-1.0 1.7 0.0)) black gray)
            (list (plane #(0 0 0) #(0.0 0.0 1.0) #(1.7 -1.0 0.0)) black gray)))))
