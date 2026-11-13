;;; LISPKIT DRAW CHART PIE
;;;
;;; This is a drawing library implementing pie charts and donut charts. A pie chart
;;; consists of a sequence of slices. Each slice has a value, an optional label, an
;;; optional color, and an optional offset by which the slice is pulled out of the pie.
;;; The angle of a slice is proportional to its value. A slice is identified by its
;;; color. A mapping from colors to labels can be shown in a legend. Labels are drawn
;;; next to the slices connected via leader lines, and values can be drawn inside of
;;; the slices, either as percentages or via a custom format.
;;;
;;; The library follows the same design patterns as `(lispkit draw chart bar)`
;;; and `(lispkit draw chart function)`:
;;;
;;;   - A mutable configuration record (`pie-chart-config`) controlling all visual
;;;     aspects of a chart: size, fonts, colors, padding, hole size, etc.
;;;   - Immutable descriptors for the chart data (`slice`).
;;;   - Legends are configured via `legend-config` objects defined by library
;;;     `(lispkit draw chart)`, which also draws them.
;;;   - The main entry point `draw-pie-chart` renders a chart into the current drawing
;;;     or an explicitly provided one.
;;;
;;; Example usage:
;;;    ```
;;;    (import (lispkit base)
;;;            (lispkit draw)
;;;            (lispkit draw chart pie))
;;;
;;;    (define my-drawing
;;;      (drawing
;;;        (draw-pie-chart
;;;          (list (slice "Apples" 40)
;;;                (slice "Pears" 25)
;;;                (slice "Cherries" 20 red)
;;;                (slice "Plums" 15))
;;;          (point 20 20)                 ; location
;;;          (make-pie-chart-config)       ; chart configuration
;;;          (make-pie-legend-config))))   ; legend (or #f)
;;;
;;;    (save-drawing "pie.pdf" my-drawing (size 540 340))
;;;    ```
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
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

(define-library (lispkit draw chart pie)

  (export
    ; Slices
    slice
    slice?
    slice-label
    slice-value
    slice-color
    slice-offset

    ; Legend configuration
    make-pie-legend-config

    ; Pie chart configuration
    make-pie-chart-config
    pie-chart-config?
    pie-chart-size
    pie-chart-size-set!
    pie-chart-color
    pie-chart-color-set!
    pie-chart-bg-color
    pie-chart-bg-color-set!
    pie-chart-border-color
    pie-chart-border-color-set!
    pie-chart-stroke-width
    pie-chart-stroke-width-set!
    pie-chart-label-font
    pie-chart-label-font-set!
    pie-chart-value-font
    pie-chart-value-font-set!
    pie-chart-value-format
    pie-chart-value-format-set!
    pie-chart-value-color
    pie-chart-value-color-set!
    pie-chart-palette
    pie-chart-palette-set!
    pie-chart-hole
    pie-chart-hole-set!
    pie-chart-start-angle
    pie-chart-start-angle-set!
    pie-chart-clockwise?
    pie-chart-clockwise-set!
    pie-chart-top-pad
    pie-chart-top-pad-set!
    pie-chart-bottom-pad
    pie-chart-bottom-pad-set!
    pie-chart-left-pad
    pie-chart-left-pad-set!
    pie-chart-right-pad
    pie-chart-right-pad-set!
    pie-chart-leader-length
    pie-chart-leader-length-set!
    pie-chart-label-pad
    pie-chart-label-pad-set!

    ; Draw pie charts
    draw-pie-chart)

  (import (lispkit base)
          (lispkit draw)
          (lispkit draw chart)
          (lispkit math util))

  (begin

    ;; ---------------------------------------------------------------
    ;; Slices
    ;; ---------------------------------------------------------------

    ;; A slice bundles a value with an optional label, an optional color and an offset.
    (define-record-type <slice>
      (make-slice label value color offset)
      slice?
      (label slice-label)
      (value slice-value)
      (color slice-color)
      (offset slice-offset))

    ;; Constructor with convenient argument order.
    ;;     (slice label value)
    ;;   | (slice label value color)
    ;;   | (slice label value color offset)
    (define (slice label value . args)
      (let-optionals args ((col #f)
                           (offset 0))
        (cond ((not (opt string? label))
                (error "invalid label for slice: $0" label))
              ((not (and (real? value) (>= value 0)))
                (error "invalid value for slice: $0" value))
              ((not (opt color? col))
                (error "invalid color for slice: $0" col))
              ((not (and (real? offset) (>= offset 0)))
                (error "invalid offset for slice: $0" offset)))
        (make-slice label value col offset)))

    ;; ---------------------------------------------------------------
    ;; Legend configuration
    ;; ---------------------------------------------------------------

    ;; Creates a legend configuration with defaults that fit pie charts: the legend is
    ;; placed in the top right corner of the chart.
    (define (make-pie-legend-config . args)
      (let-keywords args
        ((font              (font "Helvetica" 10)) ; font used in legends
         (color             black)  ; color used for labels in legends
         (bg-color          white)  ; background color of legend
         (stroke-width      0.5)    ; width of a stroke for drawing the bounding box
         (corner-radius     0)      ; radius for rounded bounding box
         (horizontal-offset -10)    ; horizontal offset from chart bounds (neg = right offset)
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

    ;; ---------------------------------------------------------------
    ;; Pie chart configuration
    ;; ---------------------------------------------------------------

    (define-record-type <pie-chart-config>
      (pie-chart-config
        pie-chart-size           ; the rectangle in which the chart is drawn
        pie-chart-color          ; color of text and leader lines
        pie-chart-bg-color       ; background color of the chart (#f = transparent)
        pie-chart-border-color   ; color of the border of slices (#f = no border)
        pie-chart-stroke-width   ; width of strokes (slice borders and leader lines)
        pie-chart-label-font     ; font for slice labels drawn outside (#f = no labels)
        pie-chart-value-font     ; font for values drawn inside slices (#f = no values)
        pie-chart-value-format   ; #f, `percent`, `value`, or a procedure
        pie-chart-value-color    ; color for values inside slices (#f = automatic)
        pie-chart-palette        ; colors for slices without a color
        pie-chart-hole           ; size of the hole relative to the radius (0 = pie)
        pie-chart-start-angle    ; angle of the first slice in degrees (0 = 12 o'clock)
        pie-chart-clockwise?     ; are slices arranged clockwise?
        pie-chart-top-pad        ; top padding
        pie-chart-bottom-pad     ; bottom padding
        pie-chart-left-pad       ; left padding
        pie-chart-right-pad      ; right padding
        pie-chart-leader-length  ; length of leader lines between slices and labels
        pie-chart-label-pad)     ; padding between leader lines and label text
      pie-chart-config?
      (pie-chart-size pie-chart-size pie-chart-size-set!)
      (pie-chart-color pie-chart-color pie-chart-color-set!)
      (pie-chart-bg-color pie-chart-bg-color pie-chart-bg-color-set!)
      (pie-chart-border-color pie-chart-border-color pie-chart-border-color-set!)
      (pie-chart-stroke-width pie-chart-stroke-width pie-chart-stroke-width-set!)
      (pie-chart-label-font pie-chart-label-font pie-chart-label-font-set!)
      (pie-chart-value-font pie-chart-value-font pie-chart-value-font-set!)
      (pie-chart-value-format pie-chart-value-format pie-chart-value-format-set!)
      (pie-chart-value-color pie-chart-value-color pie-chart-value-color-set!)
      (pie-chart-palette pie-chart-palette pie-chart-palette-set!)
      (pie-chart-hole pie-chart-hole pie-chart-hole-set!)
      (pie-chart-start-angle pie-chart-start-angle pie-chart-start-angle-set!)
      (pie-chart-clockwise? pie-chart-clockwise? pie-chart-clockwise-set!)
      (pie-chart-top-pad pie-chart-top-pad pie-chart-top-pad-set!)
      (pie-chart-bottom-pad pie-chart-bottom-pad pie-chart-bottom-pad-set!)
      (pie-chart-left-pad pie-chart-left-pad pie-chart-left-pad-set!)
      (pie-chart-right-pad pie-chart-right-pad pie-chart-right-pad-set!)
      (pie-chart-leader-length pie-chart-leader-length pie-chart-leader-length-set!)
      (pie-chart-label-pad pie-chart-label-pad pie-chart-label-pad-set!))

    ;; The default palette (the "Tableau 10" colors)
    (define default-palette
      (map color
           '("4E79A7" "F28E2B" "E15759" "76B7B2" "59A14F"
             "EDC948" "B07AA1" "FF9DA7" "9C755F" "BAB0AC")))

    (define (make-pie-chart-config . args)
      (let-keywords args
        ((size           (size 500 300))        ; the rectangle in which the chart is drawn
         (color          black)                 ; color of text and leader lines
         (bg-color       #f)                    ; background color (#f = transparent)
         (border-color   white)                 ; color of slice borders (#f = no border)
         (stroke-width   1)                     ; width of a stroke
         (label-font     (font "Helvetica" 11)) ; font for slice labels (#f = no labels)
         (value-font     (font "Helvetica-Bold" 10)) ; font for values (#f = no values)
         (value-format   'percent)              ; #f, percent, value, or a procedure
         (value-color    #f)                    ; color of values (#f = automatic)
         (palette        default-palette)       ; colors for slices without a color
         (hole           0)                     ; relative size of hole (0 = pie chart)
         (start-angle    0)                     ; start angle in degrees (0 = 12 o'clock)
         (clockwise?     #t)                    ; draw slices clockwise?
         (top-pad        10)                    ; top padding
         (bottom-pad     10)                    ; bottom padding
         (left-pad       10)                    ; left padding
         (right-pad      10)                    ; right padding
         (leader-length  10)                    ; length of leader lines
         (label-pad      3))                    ; padding between leader line and label
        (pie-chart-config
          size
          color
          bg-color
          border-color
          stroke-width
          label-font
          value-font
          value-format
          value-color
          palette
          hole
          start-angle
          clockwise?
          top-pad
          bottom-pad
          left-pad
          right-pad
          leader-length
          label-pad)))

    ;; ---------------------------------------------------------------
    ;; Geometry
    ;; ---------------------------------------------------------------

    (define two-pi (* 2.0 pi))

    ;; Maximum angle covered by one line segment when approximating an arc (2 degrees)
    (define arc-step (/ pi 90.0))

    ;; Returns the point at distance `radius` and angle `angle` (in radians, measured
    ;; clockwise from the positive x-axis, i.e. 0 is at 3 o'clock and `pi/2` is at
    ;; 6 o'clock) from the point `(cx, cy)`.
    (define (polar cx cy radius angle)
      (point (+ cx (* radius (cos angle)))
             (+ cy (* radius (sin angle)))))

    ;; Returns a list of points approximating the arc around `(cx, cy)` with the given
    ;; radius, from angle `a0` to angle `a1` (inclusive). `a1` may be smaller than `a0`.
    (define (arc-points cx cy radius a0 a1)
      (let* ((n (max 1 (exact (ceiling (/ (abs (- a1 a0)) arc-step)))))
             (da (/ (- a1 a0) n)))
        (let loop ((i n) (pts '()))
          (if (< i 0)
              pts
              (loop (- i 1) (cons (polar cx cy radius (+ a0 (* i da))) pts))))))

    ;; Returns a (closed) shape for a ring sector around `(cx, cy)` between radius `r0` (inner
    ;; radius, 0 for a pie slice) and radius `r1` (outer radius) covering angles `a0` to `a1`.
    (define (sector-shape cx cy r0 r1 a0 a1)
      (cond ((< (abs (- a1 a0)) (- two-pi 1e-6))
              ; Slice covering less than the full circle
              (apply closed-polygon
                     (if (> r0 0)
                         (append (arc-points cx cy r1 a0 a1)
                                 (reverse (arc-points cx cy r0 a0 a1)))
                         (cons (point cx cy) (arc-points cx cy r1 a0 a1)))))
            ((> r0 0)
              ; Full ring: two circles with opposite orientation
              (let ((s (make-shape)))
                (add-shape (apply closed-polygon (arc-points cx cy r1 0.0 two-pi)) s)
                (add-shape (apply closed-polygon (arc-points cx cy r0 two-pi 0.0)) s)
                s))
            (else
              ; Full disc
              (circle (point cx cy) r1))))

    ;; Returns a color which is easily readable on a background of color `c`.
    (define (contrast-color c)
      (if (> (+ (* 0.299 (color-red c)) (* 0.587 (color-green c)) (* 0.114 (color-blue c))) 0.6)
          black
          white))

    ;; ---------------------------------------------------------------
    ;; Segments: slices with their color and position within the pie
    ;; ---------------------------------------------------------------

    (define-record-type <segment>
      (make-segment slice color start end fraction)
      segment?
      (slice segment-slice)
      (color segment-color)
      (start segment-start)       ; start angle in radians
      (end segment-end)           ; end angle in radians
      (fraction segment-fraction)) ; share of the slice in the total

    (define (segment-mid seg)
      (/ (+ (segment-start seg) (segment-end seg)) 2.0))

    (define (segment-offset seg)
      (slice-offset (segment-slice seg)))

    ;; Returns the segments for the given list of slices, positioning them within the pie.
    (define (make-segments slices palette start-angle clockwise?)
      (let ((total (inexact (sum (map slice-value slices))))
            (theta0 (* (- start-angle 90.0) (/ pi 180.0)))
            (dir (if clockwise? 1.0 -1.0))
            (ncolors (length palette)))
        (if (<= total 0)
            (error "total value of the slices of a pie chart is not positive"))
        (let loop ((ss slices) (i 0) (acc 0.0) (res '()))
          (if (null? ss)
              (reverse res)
              (let* ((s (car ss))
                     (f (/ (inexact (slice-value s)) total)))
                (loop (cdr ss)
                      (+ i 1)
                      (+ acc f)
                      (cons (make-segment s
                                          (or (slice-color s)
                                              (if (pair? palette)
                                                  (list-ref palette (modulo i ncolors))
                                                  gray))
                                          (+ theta0 (* dir two-pi acc))
                                          (+ theta0 (* dir two-pi (+ acc f)))
                                          f)
                            res)))))))

    ;; Returns the text shown inside of a slice with value `v` and share `f` of the total
    ;; or `#f` if there is no such text.
    (define (value-text fmt v f)
      (cond ((not fmt)
              #f)
            ((procedure? fmt)
              (fmt v f))
            ((eq? fmt 'value)
              (number->string v))
            ((eq? fmt 'percent)
              (let ((p (* 100 f)))
                (if (< p 0.5)
                    "<1%"
                    (string-append (number->string (exact (round p))) "%"))))
            (else
              (error "invalid value format for pie chart: $0" fmt))))

    ;; Returns the maximum of `cos(a - theta)` for all angles `a` between `a0` and `a1`.
    (define (max-cos a0 a1 theta)
      (let ((lo (min a0 a1))
            (hi (max a0 a1)))
        (if (or (>= (- hi lo) (- two-pi 1e-9))
                ; Is `theta` (plus a multiple of a full circle) contained in the interval?
                (<= (+ theta (* (ceiling (/ (- lo theta) two-pi)) two-pi)) (+ hi 1e-9)))
            1.0
            (max (cos (- a0 theta)) (cos (- a1 theta))))))

    ;; Returns the largest radius of the pie for which the segment `seg`, which may be pulled
    ;; out of the pie, stays within the plot area. The plot area has width `pw` and height `ph`
    ;; and is centered around the center of the pie. Every sector reaches as far as the
    ;; furthest point of its outer arc in a direction (or of its inner arc, if the sector
    ;; does not face that direction), shifted by the offset of the segment.
    (define (slice-limit seg hole pw ph)
      (let ((offset (segment-offset seg))
            (mid (segment-mid seg)))
        (minimum
          (map (lambda (dir)
                 (let* ((theta (car dir))
                        (m (max-cos (segment-start seg) (segment-end seg) theta))
                        (reach (if (>= m 0) m (* hole m))))
                   (if (> reach 1e-6)
                       (/ (- (cdr dir) (* offset (cos (- mid theta)))) reach)
                       +inf.0)))
               (list (cons 0.0 (/ pw 2))
                     (cons (/ pi 2) (/ ph 2))
                     (cons pi (/ pw 2))
                     (cons (- (/ pi 2)) (/ ph 2)))))))

    ;; Returns the distance from the center of the pie at which a text box of size `tw` x `th`
    ;; can be centered such that the box lies completely within a ring sector, or `#f` if the
    ;; text does not fit. The ring sector is defined by its `inner` and `outer` radius, the
    ;; angle `mid` (in radians) of the middle of the sector, and the angle `half` (in radians)
    ;; by which the sector extends to both sides of `mid`. The box is kept at least `margin`
    ;; points away from the borders of the sector. Starting at the preferred distance `pref`,
    ;; the box is moved outwards until it fits, since sectors are wider there.
    (define (value-radius tw th inner outer mid half pref margin)
      (let* ((ux (cos mid))
             (uy (sin mid))
             (hw (/ tw 2.0))
             (hh (/ th 2.0))
             (wedge? (< half (- (/ pi 2) 1e-6)))
             (slope (if wedge? (tan half) 0.0))
             (fits? (lambda (rc)
                      (let ((px (* rc ux))
                            (py (* rc uy)))
                        (and
                          ; All corners are inside of the sector
                          (every? (lambda (c)
                                    (let* ((x (+ px (* (car c) hw)))
                                           (y (+ py (* (cdr c) hh)))
                                           (s (+ (* x ux) (* y uy)))
                                           (t (- (* y ux) (* x uy))))
                                      (and (<= (sqrt (+ (* x x) (* y y))) (- outer margin))
                                           (or (not wedge?)
                                               (and (> s 0)
                                                    (<= (+ (abs t) margin) (* s slope)))))))
                                  '((-1 . -1) (1 . -1) (-1 . 1) (1 . 1)))
                          ; The box does not reach into the hole
                          (or (<= inner 0)
                              (let ((nx (max (- px hw) (min 0.0 (+ px hw))))
                                    (ny (max (- py hh) (min 0.0 (+ py hh)))))
                                (>= (sqrt (+ (* nx nx) (* ny ny))) (+ inner margin)))))))))
        (let loop ((rc pref))
          (cond ((> rc outer) #f)
                ((fits? rc) rc)
                (else (loop (+ rc 0.5)))))))

    ;; ---------------------------------------------------------------
    ;; Labels outside of the pie
    ;; ---------------------------------------------------------------

    ;; A tag is a label drawn outside of the pie connected to its slice via a leader line.
    ;; The label is a box of the size of the text. Its center is `(x, y)`. The leader line
    ;; goes from `(ex, ey)` on the edge of the slice to the anchor `(ax, ay)`. The anchor
    ;; is located on the ray from the center of the pie through the middle of the slice.
    (define-record-type <tag>
      (make-tag text size ex ey ax ay x y)
      tag?
      (text tag-text)
      (size tag-size)
      (ex tag-ex)
      (ey tag-ey)
      (ax tag-ax)
      (ay tag-ay)
      (x tag-x tag-x-set!)
      (y tag-y tag-y-set!))

    (define (tag-width t) (size-width (tag-size t)))
    (define (tag-height t) (size-height (tag-size t)))

    ;; Returns the offset `(dx . dy)` of the center of a label box of size `w` x `h`
    ;; relative to its anchor. `(ux, uy)` is the unit vector pointing from the pie towards
    ;; the anchor. The label is placed such that it is located in the direction of this
    ;; vector, i.e. labels at the top are put above and labels at the bottom below their
    ;; anchor, labels at the sides are put beside it, and labels in between are placed at
    ;; the corresponding corner. The box starts out with its center at
    ;; `(ux * w/2, uy * h/2)`. It is then pushed along `(ux, uy)` until it is at least
    ;; `pad` away from the anchor in either horizontal or vertical direction.
    (define (label-offset ux uy w h pad)
      (let* ((bx (* ux (/ w 2.0)))
             (by (* uy (/ h 2.0)))
             (steps (append
                      (cond ((> ux 1e-9) (list (/ (- pad (- bx (/ w 2.0))) ux)))
                            ((< ux -1e-9) (list (/ (+ (+ bx (/ w 2.0)) pad) (- ux))))
                            (else '()))
                      (cond ((> uy 1e-9) (list (/ (- pad (- by (/ h 2.0))) uy)))
                            ((< uy -1e-9) (list (/ (+ (+ by (/ h 2.0)) pad) (- uy))))
                            (else '()))))
             (s (max 0.0 (minimum steps))))
        (cons (+ bx (* s ux)) (+ by (* s uy)))))

;; Do the boxes of two tags overlap horizontally?
    (define (tags-overlap-x? t u)
      (< (abs (- (tag-x t) (tag-x u))) (/ (+ (tag-width t) (tag-width u)) 2.0)))

    ;; Gap in points between two labels that were moved apart
    (define label-gap 1.0)

    ;; Moves tags down (in the order of their vertical positions) such that boxes which
    ;; overlap horizontally do not overlap vertically, and such that tags do not move
    ;; above `lo`.
    (define (push-down! tags lo)
      (let loop ((ts (sort (lambda (t u) (< (tag-y t) (tag-y u))) tags)) (placed '()))
        (when (pair? ts)
          (let ((t (car ts)))
            (tag-y-set! t (fold-left (lambda (y u)
                                       (if (tags-overlap-x? t u)
                                           (max y (+ (tag-y u)
                                                     (/ (+ (tag-height t) (tag-height u)) 2.0)
                                                     label-gap))
                                           y))
                                     (max (tag-y t) (+ lo (/ (tag-height t) 2.0)))
                                     placed))
            (loop (cdr ts) (cons t placed))))))

    ;; Moves tags up such that boxes which overlap horizontally do not overlap vertically,
    ;; and such that tags do not move below `hi`.
    (define (push-up! tags hi)
      (let loop ((ts (sort (lambda (t u) (> (tag-y t) (tag-y u))) tags)) (placed '()))
        (when (pair? ts)
          (let ((t (car ts)))
            (tag-y-set! t (fold-left (lambda (y u)
                                       (if (tags-overlap-x? t u)
                                           (min y (- (tag-y u)
                                                     (/ (+ (tag-height t) (tag-height u)) 2.0)
                                                     label-gap))
                                           y))
                                     (min (tag-y t) (- hi (/ (tag-height t) 2.0)))
                                     placed))
            (loop (cdr ts) (cons t placed))))))

    ;; Moves tags vertically such that their boxes do not overlap and such that they stay
    ;; between `lo` and `hi` (as far as this is possible). Labels in the upper half are
    ;; moved upwards and labels in the lower half downwards, i.e. labels move away from
    ;; the pie, which sits between the halves, when making room for each other.
    (define (spread-tags! tags lo hi)
      (let* ((mid (/ (+ lo hi) 2.0))
             (upper (filter (lambda (t) (< (tag-y t) mid)) tags))
             (lower (filter (lambda (t) (>= (tag-y t) mid)) tags)))
        (push-up! upper hi)
        (push-down! upper lo)
        (push-down! lower lo)
        (push-up! lower hi)
        ; Resolve remaining overlaps between labels of both halves
        (push-down! tags lo)
        (push-up! tags hi)))

    ;; Returns the distance by which the box of tag `t` needs to be moved horizontally
    ;; (to the right if `dir` is positive, otherwise to the left) so that it does not
    ;; cover the leader line of tag `u`.
    (define (leader-clearance t u dir pad)
      (let ((left (- (tag-x t) (/ (tag-width t) 2.0)))
            (right (+ (tag-x t) (/ (tag-width t) 2.0)))
            (top (- (tag-y t) (/ (tag-height t) 2.0)))
            (bottom (+ (tag-y t) (/ (tag-height t) 2.0))))
        (let loop ((i 0) (shift 0.0))
          (if (> i 8)
              shift
              (let* ((f (/ i 8.0))
                     (x (+ (tag-ex u) (* f (- (tag-ax u) (tag-ex u)))))
                     (y (+ (tag-ey u) (* f (- (tag-ay u) (tag-ey u))))))
                (loop (+ i 1)
                      (if (and (> y (- top label-gap))
                               (< y (+ bottom label-gap))
                               (> x (- left pad))
                               (< x (+ right pad)))
                          (max shift (if (> dir 0)
                                         (- (+ x pad) left)
                                         (- right (- x pad))))
                          shift)))))))

    ;; Moves tags horizontally away from the pie, whose center is at `cx`, if they cover the
    ;; leader line of another tag. Tags stay between `lo` and `hi` (as far as possible).
    (define (clear-leaders! tags cx lo hi pad)
      (do ((round 0 (+ round 1)))
          ((= round 2))
        (for-each
          (lambda (t)
            (let* ((dir (if (>= (tag-ax t) cx) 1.0 -1.0))
                   (shift (maximum
                            (cons 0.0
                                  (map (lambda (u)
                                         (if (eq? t u) 0.0 (leader-clearance t u dir pad)))
                                       tags))))
                   (half (/ (tag-width t) 2.0)))
              (when (> shift 0.0)
                (tag-x-set! t (max (+ lo half)
                                   (min (- hi half) (+ (tag-x t) (* dir shift))))))))
          tags)))

    ;; ---------------------------------------------------------------
    ;; Main drawing procedure
    ;; ---------------------------------------------------------------

    ;; draw-pie-chart draws a pie chart into a drawing.
    ;;
    ;; slices  - a non-empty list of slices
    ;; loc     - a point at which the chart is placed (the size of the chart is defined by
    ;;           the configuration) or a rectangle defining the bounds of the chart
    ;; config  - a pie chart configuration
    ;; legend  - a legend configuration or #f
    ;; drawing - (optional) target drawing; defaults to current-drawing
    (define (draw-pie-chart slices loc config legend . args)
      (let-optionals args ((drw (current-drawing)))
        (if (not (and (list? slices) (pair? slices) (every? slice? slices)))
            (error "invalid list of slices for pie chart: $0" slices))
        (if (not (pie-chart-config? config))
            (error "invalid pie chart configuration: $0" config))
        (let* ((bounds (if (point? loc) (rect loc (pie-chart-size config)) loc))
               (color (pie-chart-color config))
               (sw (pie-chart-stroke-width config))
               (lfont (pie-chart-label-font config))
               (vfont (pie-chart-value-font config))
               (leader (pie-chart-leader-length config))
               (lpad (pie-chart-label-pad config))
               (hole (max 0 (min 0.95 (pie-chart-hole config))))
               ; The area for drawing the pie and its labels, and its center
               (px (+ (rect-x bounds) (pie-chart-left-pad config)))
               (py (+ (rect-y bounds) (pie-chart-top-pad config)))
               (pw (- (rect-width bounds)
                      (pie-chart-left-pad config)
                      (pie-chart-right-pad config)))
               (ph (- (rect-height bounds)
                      (pie-chart-top-pad config)
                      (pie-chart-bottom-pad config)))
               (cx (+ px (/ pw 2)))
               (cy (+ py (/ ph 2)))
               ; The slices with their positions in the pie
               (segs (make-segments slices
                                    (pie-chart-palette config)
                                    (pie-chart-start-angle config)
                                    (pie-chart-clockwise? config)))
               ; Segments with a label shown outside
               (lsegs (if lfont
                          (filter (lambda (seg)
                                    (and (> (segment-fraction seg) 0)
                                         (slice-label (segment-slice seg))))
                                  segs)
                          '()))
               (lsizes (map (lambda (seg) (text-size (slice-label (segment-slice seg)) lfont))
                            lsegs))
               ; The position of each label relative to its anchor
               (loffs (map (lambda (seg tsz)
                             (label-offset (cos (segment-mid seg))
                                           (sin (segment-mid seg))
                                           (size-width tsz)
                                           (size-height tsz)
                                           lpad))
                           lsegs
                           lsizes))
               ; The radius of the pie is as large as possible such that all slices, including
               ; the ones which are pulled out, and all labels fit
               (radius (minimum
                         (append (filter-map (lambda (seg) (and (> (segment-fraction seg) 0)
                                                                (slice-limit seg hole pw ph)))
                                             segs)
                               (map (lambda (seg tsz off)
                                      (let ((ux (abs (cos (segment-mid seg))))
                                            (uy (abs (sin (segment-mid seg)))))
                                        (min (if (> ux 1e-6)
                                                 (- (/ (- (/ pw 2)
                                                          (/ (size-width tsz) 2)
                                                          (abs (car off)))
                                                       ux)
                                                    (segment-offset seg)
                                                    leader)
                                                 +inf.0)
                                             (if (> uy 1e-6)
                                                 (- (/ (- (/ ph 2)
                                                          (/ (size-height tsz) 2)
                                                          (abs (cdr off)))
                                                       uy)
                                                    (segment-offset seg)
                                                    leader)
                                                 +inf.0))))
                                    lsegs
                                    lsizes
                                    loffs))))
               (inner (* radius hole))
               ; Returns the center of the pie for a given segment
               (seg-center (lambda (seg)
                             (polar cx cy (segment-offset seg) (segment-mid seg))))
               ; The shapes of the segments (or #f if a segment has no extent)
               (shapes (map (lambda (seg)
                              (and (> (segment-fraction seg) 0)
                                   (let ((c (seg-center seg)))
                                     (sector-shape (point-x c) (point-y c) inner radius
                                                   (segment-start seg) (segment-end seg)))))
                            segs)))
          (if (<= radius 1)
              (error "pie chart is too small for its labels: $0" (rect-size bounds)))
          (with-drawing drw
            ; Background
            (when (pie-chart-bg-color config)
              (set-fill-color (pie-chart-bg-color config))
              (fill-rect bounds))
            ; Slices
            (for-each (lambda (seg shp)
                        (when shp
                          (set-fill-color (segment-color seg))
                          (fill shp)))
                      segs
                      shapes)
            ; Slice borders
            (when (and (pie-chart-border-color config) (> sw 0))
              (set-color (pie-chart-border-color config))
              (for-each (lambda (shp) (if shp (draw shp sw))) shapes))
            ; Values inside of the slices
            (when vfont
              (for-each
                (lambda (seg)
                  (let ((txt (and (> (segment-fraction seg) 0)
                                  (value-text (pie-chart-value-format config)
                                              (slice-value (segment-slice seg))
                                              (segment-fraction seg)))))
                    (when txt
                      (let* ((tsz (text-size txt vfont))
                             ; The value is centered in the ring if there is enough room;
                             ; otherwise it is moved towards the outer edge of the slice
                             (rm (value-radius (size-width tsz)
                                               (size-height tsz)
                                               inner
                                               radius
                                               (segment-mid seg)
                                               (/ (abs (- (segment-end seg) (segment-start seg))) 2)
                                               (* radius (+ hole (* (- 1.0 hole)
                                                                    (if (zero? hole) 0.62 0.5))))
                                               2.0)))
                        ; Only show the value if there is enough room
                        (when rm
                          (let ((c (polar cx cy (+ rm (segment-offset seg)) (segment-mid seg))))
                            (draw-text txt
                                       (point (- (point-x c) (/ (size-width tsz) 2))
                                              (- (point-y c) (/ (size-height tsz) 2)))
                                       vfont
                                       (or (pie-chart-value-color config)
                                           (contrast-color (segment-color seg))))))))))
                segs))
            ; Labels outside of the slices connected via leader lines
            (when (pair? lsegs)
              (let ((tags (map (lambda (seg tsz off)
                                 (let* ((a (segment-mid seg))
                                        (c (seg-center seg))
                                        (e (polar (point-x c) (point-y c) radius a))
                                        (l (polar (point-x c) (point-y c) (+ radius leader) a)))
                                   (make-tag (slice-label (segment-slice seg))
                                             tsz
                                             (point-x e) (point-y e)
                                             (point-x l) (point-y l)
                                             (+ (point-x l) (car off))
                                             (+ (point-y l) (cdr off)))))
                               lsegs
                               lsizes
                               loffs)))
                ; Resolve overlapping labels
                (spread-tags! tags py (+ py ph))
                (clear-leaders! tags cx px (+ px pw) lpad)
                (set-color color)
                (for-each
                  (lambda (t)
                    (let* ((w (tag-width t))
                           (h (tag-height t))
                           (ax (tag-ax t))
                           (ay (tag-ay t))
                           ; The point of the label box which is closest to the anchor
                           (qx (max (- (tag-x t) (/ w 2)) (min ax (+ (tag-x t) (/ w 2)))))
                           (qy (max (- (tag-y t) (/ h 2)) (min ay (+ (tag-y t) (/ h 2)))))
                           (dist (sqrt (+ (square (- qx ax)) (square (- qy ay))))))
                      ; Leader line, extended to the label if the label has been moved away
                      (when (> leader 0)
                        (draw (line (point (tag-ex t) (tag-ey t)) (point ax ay)) sw)
                        (when (> dist (+ lpad 0.5))
                          (let ((f (/ (- dist lpad) dist)))
                            (draw (line (point ax ay)
                                        (point (+ ax (* f (- qx ax))) (+ ay (* f (- qy ay)))))
                                  sw))))
                      ; Label
                      (draw-text (tag-text t)
                                 (point (- (tag-x t) (/ w 2)) (- (tag-y t) (/ h 2)))
                                 lfont
                                 color)))
                  tags)))
            ; Legend
            (when (legend-config? legend)
              (let ((entries (filter-map (lambda (seg)
                                           (let ((label (slice-label (segment-slice seg))))
                                             (and label (list label (segment-color seg)))))
                                         segs)))
                (when (pair? entries)
                  (draw-legend entries
                               bounds
                               legend
                               color
                               (or (pie-chart-bg-color config) white)
                               drw))))))))
  )
)
