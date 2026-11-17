;;; LISPKIT DRAW CHART LINE
;;;
;;; This is a drawing library implementing line charts and area charts, including
;;; stacked line charts and stacked area charts. A chart consists of one or more
;;; series. Each series has a label, a color, and one value for each position on
;;; the x axis. The x axis is either a category axis, with evenly spaced positions
;;; that are identified by labels, or a numeric axis, on which positions are placed
;;; proportionally to their value. Values can be missing, which creates gaps in lines
;;; and areas. Series can be drawn with markers, as smooth curves, and with their
;;; values shown next to each data point. A mapping from colors to labels can be shown
;;; in a legend.
;;;
;;; The library follows the same design patterns as `(lispkit draw chart bar)`,
;;; `(lispkit draw chart pie)`, and `(lispkit draw chart function)`:
;;;
;;;   - A mutable configuration record (`line-chart-config`) controlling all visual
;;;     aspects of a chart: size, fonts, colors, padding, markers, stacking, etc.
;;;   - Immutable descriptors for the chart data (`line-series`).
;;;   - Legends are configured via `legend-config` objects defined by library
;;;     `(lispkit draw chart)`, which also draws them.
;;;   - The main entry point `draw-line-chart` renders a chart into the current
;;;     drawing or an explicitly provided one.
;;;
;;; Example usage:
;;;    ```
;;;    (import (lispkit base)
;;;            (lispkit draw)
;;;            (lispkit draw chart line))
;;;
;;;    (define my-drawing
;;;      (drawing
;;;        (draw-line-chart
;;;          (list (line-series "Berlin" '(0 1 5 9 14 17 19 19 15 10 5 1))
;;;                (line-series "Oslo" '(-4 -4 0 5 11 15 18 17 12 7 2 -2)))
;;;          '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
;;;            "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")   ; x values
;;;          5                                        ; y axis step
;;;          "Month"                                  ; x axis description
;;;          "Temperature [C°]"                       ; y axis description
;;;          (point 20 20)                            ; location
;;;          (make-line-chart-config 'marker: 'circle) ; chart configuration
;;;          (make-line-legend-config))))             ; legend (or #f)
;;;
;;;    (save-drawing "temperature.pdf" my-drawing (size 540 340))
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

(define-library (lispkit draw chart line)

  (export
    ; Series
    line-series
    line-series?
    line-series-label
    line-series-values
    line-series-color
    line-series-width
    line-series-lengths
    line-series-marker

    ; Legend configuration
    make-line-legend-config

    ; Line chart configuration
    make-line-chart-config
    line-chart-config?
    line-chart-size
    line-chart-size-set!
    line-chart-color
    line-chart-color-set!
    line-chart-bg-color
    line-chart-bg-color-set!
    line-chart-palette
    line-chart-palette-set!
    line-chart-value-font
    line-chart-value-font-set!
    line-chart-label-font
    line-chart-label-font-set!
    line-chart-descr-font
    line-chart-descr-font-set!
    line-chart-point-font
    line-chart-point-font-set!
    line-chart-value-format
    line-chart-value-format-set!
    line-chart-stroke-width
    line-chart-stroke-width-set!
    line-chart-line-width
    line-chart-line-width-set!
    line-chart-marker
    line-chart-marker-set!
    line-chart-marker-size
    line-chart-marker-size-set!
    line-chart-smooth?
    line-chart-smooth-set!
    line-chart-area?
    line-chart-area-set!
    line-chart-stacked?
    line-chart-stacked-set!
    line-chart-area-opacity
    line-chart-area-opacity-set!
    line-chart-grid-line-lengths
    line-chart-grid-line-lengths-set!
    line-chart-vertical-grid?
    line-chart-vertical-grid-set!
    line-chart-top-pad
    line-chart-top-pad-set!
    line-chart-bottom-pad
    line-chart-bottom-pad-set!
    line-chart-left-pad
    line-chart-left-pad-set!
    line-chart-right-pad
    line-chart-right-pad-set!
    line-chart-vlabel-width
    line-chart-vlabel-width-set!
    line-chart-blabel-height
    line-chart-blabel-height-set!
    line-chart-tick-length
    line-chart-tick-length-set!
    line-chart-xaxis-overhead
    line-chart-xaxis-overhead-set!
    line-chart-yaxis-overhead
    line-chart-yaxis-overhead-set!
    line-chart-edge-pad
    line-chart-edge-pad-set!
    line-chart-value-pad
    line-chart-value-pad-set!
    line-chart-x-step
    line-chart-x-step-set!
    line-chart-x-min
    line-chart-x-min-set!
    line-chart-x-max
    line-chart-x-max-set!
    line-chart-y-min
    line-chart-y-min-set!
    line-chart-y-max
    line-chart-y-max-set!

    ; Draw line charts
    draw-line-chart)

  (import (lispkit base)
          (lispkit draw)
          (lispkit draw chart)
          (lispkit math util))

  (begin

    ;; ---------------------------------------------------------------
    ;; Series
    ;; ---------------------------------------------------------------

    ;; The supported marker shapes
    (define marker-kinds '(none circle square diamond triangle))

    ;; Is `x` a real number that is neither infinite nor `nan`?
    (define (finite-real? x)
      (and (real? x) (< (abs x) +inf.0)))

    ;; A series bundles a list of values with an optional label, an optional color, and
    ;; optional properties of the line drawn for the series.
    (define-record-type <line-series>
      (make-line-series label values color width lengths marker)
      line-series?
      (label line-series-label)
      (values line-series-values)
      (color line-series-color)
      (width line-series-width)
      (lengths line-series-lengths)
      (marker line-series-marker))

    ;; Constructor with convenient argument order.
    ;;     (line-series label values)
    ;;   | (line-series label values color)
    ;;   | (line-series label values color width)
    ;;   | (line-series label values color width lengths)
    ;;   | (line-series label values color width lengths marker)
    ;;
    ;; label    - a string or `#f`; unlabeled series do not show up in the legend
    ;; values   - a list of real numbers, one for each x value; `#f` is a missing value
    ;; color    - a color or `#f` (the color is taken from the palette)
    ;; width    - the width of the line in points, or `#f` (the default is taken from
    ;;            the configuration)
    ;; lengths  - an alternating list of dash/space lengths for dashed lines, or `#f`
    ;; marker   - one of `none`, `circle`, `square`, `diamond`, `triangle`, or `#f` (the
    ;;            default is taken from the configuration)
    (define (line-series label vals . args)
      (let-optionals args ((col #f)
                           (width #f)
                           (lengths #f)
                           (marker #f))
        (cond ((not (opt string? label))
                (error "invalid label for line series: $0" label))
              ((not (and (list? vals) (every? (lambda (v) (or (not v) (finite-real? v))) vals)))
                (error "invalid values for line series: $0" vals))
              ((not (opt color? col))
                (error "invalid color for line series: $0" col))
              ((not (opt (lambda (w) (and (finite-real? w) (>= w 0))) width))
                (error "invalid line width for line series: $0" width))
              ((not (opt (lambda (ls) (and (list? ls)
                                           (pair? ls)
                                           (every? (lambda (l) (and (finite-real? l) (> l 0))) ls)))
                         lengths))
                (error "invalid line lengths for line series: $0" lengths))
              ((not (opt (lambda (m) (memq m marker-kinds)) marker))
                (error "invalid marker for line series: $0" marker)))
        (make-line-series label vals col width lengths marker)))

    ;; ---------------------------------------------------------------
    ;; Legend configuration
    ;; ---------------------------------------------------------------

    ;; Creates a legend configuration with defaults that fit line charts: the legend is
    ;; placed in the top right corner of the chart.
    (define (make-line-legend-config . args)
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
    ;; Line chart configuration
    ;; ---------------------------------------------------------------

    (define-record-type <line-chart-config>
      (line-chart-config
        line-chart-size              ; the rectangle in which the chart is drawn
        line-chart-color             ; color of axes, text, and grid labels
        line-chart-bg-color          ; background color of the plot area (#f = transparent)
        line-chart-palette           ; colors for series without a color
        line-chart-value-font        ; font for labels of the y axis (#f = no labels)
        line-chart-label-font        ; font for labels of the x axis (#f = no labels)
        line-chart-descr-font        ; font for describing the axes
        line-chart-point-font        ; font for values shown at data points (#f = no values)
        line-chart-value-format      ; #f or a procedure mapping numbers to strings
        line-chart-stroke-width      ; width of a stroke for axes and grid lines
        line-chart-line-width        ; default width of lines of series
        line-chart-marker            ; default marker: none, circle, square, diamond, triangle
        line-chart-marker-size       ; size of markers
        line-chart-smooth?           ; are lines smoothed?
        line-chart-area?             ; are areas below lines filled?
        line-chart-stacked?          ; are series stacked?
        line-chart-area-opacity      ; opacity of areas (#f = automatic)
        line-chart-grid-line-lengths ; alternating list of dash/space lengths for grid lines
        line-chart-vertical-grid?    ; are vertical grid lines drawn?
        line-chart-top-pad           ; top padding
        line-chart-bottom-pad        ; bottom padding
        line-chart-left-pad          ; left padding
        line-chart-right-pad         ; right padding
        line-chart-vlabel-width      ; width reserved for the labels of the y axis
        line-chart-blabel-height     ; height reserved for the labels of the x axis
        line-chart-tick-length       ; length of tick marks
        line-chart-xaxis-overhead    ; overhead on x axis
        line-chart-yaxis-overhead    ; overhead on y axis
        line-chart-edge-pad          ; space between axes and first/last data point
        line-chart-value-pad         ; padding between data points and their values
        line-chart-x-step            ; step of ticks of a numeric x axis (#f = automatic)
        line-chart-x-min             ; minimum of a numeric x axis (#f = automatic)
        line-chart-x-max             ; maximum of a numeric x axis (#f = automatic)
        line-chart-y-min             ; minimum of the y axis (#f = automatic)
        line-chart-y-max)            ; maximum of the y axis (#f = automatic)
      line-chart-config?
      (line-chart-size line-chart-size line-chart-size-set!)
      (line-chart-color line-chart-color line-chart-color-set!)
      (line-chart-bg-color line-chart-bg-color line-chart-bg-color-set!)
      (line-chart-palette line-chart-palette line-chart-palette-set!)
      (line-chart-value-font line-chart-value-font line-chart-value-font-set!)
      (line-chart-label-font line-chart-label-font line-chart-label-font-set!)
      (line-chart-descr-font line-chart-descr-font line-chart-descr-font-set!)
      (line-chart-point-font line-chart-point-font line-chart-point-font-set!)
      (line-chart-value-format line-chart-value-format line-chart-value-format-set!)
      (line-chart-stroke-width line-chart-stroke-width line-chart-stroke-width-set!)
      (line-chart-line-width line-chart-line-width line-chart-line-width-set!)
      (line-chart-marker line-chart-marker line-chart-marker-set!)
      (line-chart-marker-size line-chart-marker-size line-chart-marker-size-set!)
      (line-chart-smooth? line-chart-smooth? line-chart-smooth-set!)
      (line-chart-area? line-chart-area? line-chart-area-set!)
      (line-chart-stacked? line-chart-stacked? line-chart-stacked-set!)
      (line-chart-area-opacity line-chart-area-opacity line-chart-area-opacity-set!)
      (line-chart-grid-line-lengths line-chart-grid-line-lengths
                                    line-chart-grid-line-lengths-set!)
      (line-chart-vertical-grid? line-chart-vertical-grid? line-chart-vertical-grid-set!)
      (line-chart-top-pad line-chart-top-pad line-chart-top-pad-set!)
      (line-chart-bottom-pad line-chart-bottom-pad line-chart-bottom-pad-set!)
      (line-chart-left-pad line-chart-left-pad line-chart-left-pad-set!)
      (line-chart-right-pad line-chart-right-pad line-chart-right-pad-set!)
      (line-chart-vlabel-width line-chart-vlabel-width line-chart-vlabel-width-set!)
      (line-chart-blabel-height line-chart-blabel-height line-chart-blabel-height-set!)
      (line-chart-tick-length line-chart-tick-length line-chart-tick-length-set!)
      (line-chart-xaxis-overhead line-chart-xaxis-overhead line-chart-xaxis-overhead-set!)
      (line-chart-yaxis-overhead line-chart-yaxis-overhead line-chart-yaxis-overhead-set!)
      (line-chart-edge-pad line-chart-edge-pad line-chart-edge-pad-set!)
      (line-chart-value-pad line-chart-value-pad line-chart-value-pad-set!)
      (line-chart-x-step line-chart-x-step line-chart-x-step-set!)
      (line-chart-x-min line-chart-x-min line-chart-x-min-set!)
      (line-chart-x-max line-chart-x-max line-chart-x-max-set!)
      (line-chart-y-min line-chart-y-min line-chart-y-min-set!)
      (line-chart-y-max line-chart-y-max line-chart-y-max-set!))

    ;; The default palette (the "Tableau 10" colors)
    (define default-palette
      (map color
           '("4E79A7" "F28E2B" "E15759" "76B7B2" "59A14F"
             "EDC948" "B07AA1" "FF9DA7" "9C755F" "BAB0AC")))

    (define (make-line-chart-config . args)
      (let-keywords args
        ((size              (size 500 300))        ; the rectangle in which the chart is drawn
         (color             black)                 ; color of axes and text
         (bg-color          #f)                    ; background of plot area (#f = transparent)
         (palette           default-palette)       ; colors for series without a color
         (value-font        (font "Helvetica" 10)) ; font for y axis labels (#f = no labels)
         (label-font        (font "Helvetica" 10)) ; font for x axis labels (#f = no labels)
         (descr-font        (font "Helvetica-LightOblique" 10)) ; font for axis descriptions
         (point-font        #f)                    ; font for values at points (#f = no values)
         (value-format      #f)                    ; #f or procedure mapping numbers to strings
         (stroke-width      1)                     ; width of a stroke for axes
         (line-width        2)                     ; default width of lines of series
         (marker            'none)                 ; default marker of series
         (marker-size       7)                     ; size of markers
         (smooth?           #f)                    ; smooth lines?
         (area?             #f)                    ; fill areas below lines?
         (stacked?          #f)                    ; stack series?
         (area-opacity      #f)                    ; opacity of areas (#f = automatic)
         (grid-line-lengths '(1 3))                ; alternating dash/space lengths (#f = no grid)
         (vertical-grid?    #f)                    ; draw vertical grid lines?
         (top-pad           20)                    ; top padding
         (bottom-pad        5)                     ; bottom padding
         (left-pad          10)                    ; left padding
         (right-pad         15)                    ; right padding
         (vlabel-width      40)                    ; width of y axis labels
         (blabel-height     18)                    ; height of x axis labels
         (tick-length       4)                     ; length of tick marks
         (xaxis-overhead    15)                    ; overhead on x axis
         (yaxis-overhead    15)                    ; overhead on y axis
         (edge-pad          #f)                    ; space between axis and first/last point
         (value-pad         4)                     ; padding between points and their values
         (x-step            #f)                    ; step for ticks on a numeric x axis
         (x-min             #f)                    ; minimum of a numeric x axis
         (x-max             #f)                    ; maximum of a numeric x axis
         (y-min             #f)                    ; minimum of the y axis
         (y-max             #f))                   ; maximum of the y axis
        (line-chart-config
          size
          color
          bg-color
          palette
          value-font
          label-font
          descr-font
          point-font
          value-format
          stroke-width
          line-width
          marker
          marker-size
          smooth?
          area?
          stacked?
          area-opacity
          grid-line-lengths
          vertical-grid?
          top-pad
          bottom-pad
          left-pad
          right-pad
          vlabel-width
          blabel-height
          tick-length
          xaxis-overhead
          yaxis-overhead
          edge-pad
          value-pad
          x-step
          x-min
          x-max
          y-min
          y-max)))

    ;; ---------------------------------------------------------------
    ;; Numbers, ticks and text
    ;; ---------------------------------------------------------------

    ;; Color of grid lines
    (define grid-color (color 0.8 0.8 0.8))

    ;; Returns a list of the integers 0, ..., n-1
    (define (index-list n)
      (let loop ((i (- n 1)) (acc '()))
        (if (< i 0)
            acc
            (loop (- i 1) (cons i acc)))))

    ;; Returns a "nice" step (1, 2, 5 times a power of 10) such that a range of size `span`
    ;; contains about `target` steps.
    (define (nice-step span target)
      (let* ((raw (/ (inexact span) (max 1 target)))
             (mag (expt 10.0 (floor (/ (log raw) (log 10.0)))))
             (m (/ raw mag)))
        (* mag (cond ((<= m 1.5) 1.0)
                     ((<= m 3.5) 2.0)
                     ((<= m 7.5) 5.0)
                     (else 10.0)))))

    ;; Returns all multiples of `step` in the interval from `lo` to `hi`. Multiples that
    ;; are whole numbers are returned as exact integers.
    (define (tick-values lo hi step)
      (let ((k0 (exact (ceiling (- (/ lo step) 1e-9))))
            (k1 (exact (floor (+ (/ hi step) 1e-9)))))
        (if (> (- k1 k0) 500)
            (error "step $0 results in too many tick marks" step))
        (let loop ((k k1) (acc '()))
          (if (< k k0)
              acc
              (let ((v (/ (round (* (* k step) 1e10)) 1e10)))
                (loop (- k 1) (cons (if (= v (round v)) (exact (round v)) v) acc)))))))

    ;; Formats number `v` with at most four decimal places, without trailing zeros.
    (define (format-value v)
      (let ((r (/ (round (* (inexact v) 10000.0)) 10000.0)))
        (if (= r (round r))
            (number->string (exact (round r)))
            (number->string r))))

    ;; Returns the text for number `v` given an optional format procedure `fmt`.
    (define (value-text v fmt)
      (if fmt (fmt v) (format-value v)))

    ;; ---------------------------------------------------------------
    ;; Colors
    ;; ---------------------------------------------------------------

    ;; Returns the color `c` with its opacity multiplied by `a`.
    (define (with-opacity c a)
      (color (color-red c) (color-green c) (color-blue c) (* a (color-alpha c))))

    ;; Returns a color which is easily readable on a background of color `c`.
    (define (contrast-color c)
      (if (> (+ (* 0.299 (color-red c)) (* 0.587 (color-green c)) (* 0.114 (color-blue c))) 0.6)
          black
          white))

    ;; Returns the color of the `i`-th series of the palette
    (define (palette-color palette i)
      (if (pair? palette)
          (list-ref palette (modulo i (length palette)))
          gray))

    ;; ---------------------------------------------------------------
    ;; Shapes
    ;; ---------------------------------------------------------------

    ;; Returns an arrow shape starting at point `start` with the given length `len`
    (define (arrow start len angle d)
      (transform-shape
        (shape
          (move-to zero-point)
          (line-to (point len 0))
          (line-to (point (- len d) (- d)))
          (move-to (point len 0))
          (line-to (point (- len d) d)))
        (rotate angle (translate (point-x start) (point-y start)))))

    ;; Returns the shape of a marker of the given kind centered at `(cx, cy)`.
    (define (marker-shape kind cx cy sz)
      (let ((r (/ sz 2.0)))
        (case kind
          ((circle)
            (circle (point cx cy) r))
          ((square)
            (rectangle (point (- cx r) (- cy r)) (size sz sz)))
          ((diamond)
            (let ((d (* r 1.3)))
              (closed-polygon (point cx (- cy d))
                              (point (+ cx d) cy)
                              (point cx (+ cy d))
                              (point (- cx d) cy))))
          ((triangle)
            (closed-polygon (point cx (- cy (* r 1.4)))
                            (point (+ cx (* r 1.2)) (+ cy (* r 0.7)))
                            (point (- cx (* r 1.2)) (+ cy (* r 0.7)))))
          (else #f))))

    ;; Returns a vector of points for the `a`-th to the `b`-th (inclusive) element of the
    ;; vectors `xs` and `ys` of coordinates.
    (define (subpoints xs ys a b)
      (let ((res (make-vector (+ (- b a) 1) #f)))
        (do ((i a (+ i 1)))
            ((> i b) res)
          (vector-set! res (- i a) (point (vector-ref xs i) (vector-ref ys i))))))

    ;; Returns a list of pairs `(a . b)` for all maximal ranges of consecutive elements of
    ;; vector `v` that are not `#f`.
    (define (runs-of v)
      (let ((n (vector-length v)))
        (let loop ((i 0) (start #f) (res '()))
          (cond ((= i n)
                  (reverse (if start (cons (cons start (- n 1)) res) res)))
                ((vector-ref v i)
                  (loop (+ i 1) (or start i) res))
                (else
                  (loop (+ i 1) #f (if start (cons (cons start (- i 1)) res) res)))))))

    ;; Returns a vector with one pair `(c1 . c2)` of control points for each segment of the
    ;; path through the points of vector `pts`. The Bézier curves defined by the control
    ;; points form a smooth monotone cubic interpolation of the points (Fritsch-Carlson/
    ;; Fritsch-Butland): curves never overshoot, i.e. between two points they stay within
    ;; the range of the two points. The x coordinates of the points have to be increasing.
    (define (curve-controls pts)
      (let ((n (vector-length pts)))
        (if (< n 2)
            (vector)
            (let* ((m (- n 1))
                   (xs (vector-map point-x pts))
                   (ys (vector-map point-y pts))
                   (h (make-vector m 0.0))  ; widths of segments
                   (d (make-vector m 0.0))  ; slopes of segments
                   (t (make-vector n 0.0))  ; tangents at points
                   (res (make-vector m #f)))
              (do ((i 0 (+ i 1)))
                  ((= i m))
                (let ((w (- (vector-ref xs (+ i 1)) (vector-ref xs i))))
                  (vector-set! h i w)
                  (vector-set! d i (if (> w 1e-12)
                                       (/ (- (vector-ref ys (+ i 1)) (vector-ref ys i)) w)
                                       0.0))))
              (vector-set! t 0 (vector-ref d 0))
              (vector-set! t m (vector-ref d (- m 1)))
              (do ((i 1 (+ i 1)))
                  ((= i m))
                (let ((d0 (vector-ref d (- i 1)))
                      (d1 (vector-ref d i))
                      (h0 (vector-ref h (- i 1)))
                      (h1 (vector-ref h i)))
                  (vector-set! t i
                    (if (<= (* d0 d1) 0)
                        0.0
                        (let ((w1 (+ (* 2 h1) h0))
                              (w2 (+ h1 (* 2 h0))))
                          (/ (+ w1 w2) (+ (/ w1 d0) (/ w2 d1))))))))
              (do ((i 0 (+ i 1)))
                  ((= i m) res)
                (let ((h3 (/ (vector-ref h i) 3.0)))
                  (vector-set! res i
                    (cons (point (+ (vector-ref xs i) h3)
                                 (+ (vector-ref ys i) (* (vector-ref t i) h3)))
                          (point (- (vector-ref xs (+ i 1)) h3)
                                 (- (vector-ref ys (+ i 1)) (* (vector-ref t (+ i 1)) h3)))))))))))

    ;; Extends shape `s`, whose current point is the first point of the vector `pts`, by
    ;; a path through the remaining points of `pts`. If `ctrl` is a vector of control
    ;; points (see `curve-controls`), the path consists of curves, otherwise of lines.
    (define (trace-forward! s pts ctrl)
      (do ((i 1 (+ i 1)))
          ((>= i (vector-length pts)))
        (if ctrl
            (let ((cp (vector-ref ctrl (- i 1))))
              (curve-to (vector-ref pts i) (car cp) (cdr cp) s))
            (line-to (vector-ref pts i) s))))

    ;; Extends shape `s`, whose current point is the last point of the vector `pts`, by
    ;; a path back to the first point of `pts`.
    (define (trace-backward! s pts ctrl)
      (do ((i (- (vector-length pts) 2) (- i 1)))
          ((< i 0))
        (if ctrl
            (let ((cp (vector-ref ctrl i)))
              (curve-to (vector-ref pts i) (cdr cp) (car cp) s))
            (line-to (vector-ref pts i) s))))

    ;; ---------------------------------------------------------------
    ;; Layers: series with their positions within the chart
    ;; ---------------------------------------------------------------

    (define-record-type <layer>
      (make-layer ser col vals tsy lsy lw marker)
      layer?
      (ser layer-ser)       ; the series
      (col layer-col)       ; color of the series
      (vals layer-vals)     ; vector of values of the series (`#f` for missing values)
      (tsy layer-tsy)       ; vector of y coordinates of the line (`#f` for gaps)
      (lsy layer-lsy)       ; vector of y coordinates of the lower border of the area
      (lw layer-lw)         ; width of the line
      (marker layer-marker)) ; marker shape

    ;; Returns a pair of two lists of vectors, the cumulative values and the values below,
    ;; for stacking the list `vals` of vectors with `n` values each. Missing values are
    ;; counted as 0.
    (define (stack-values vals n)
      (let loop ((vs vals) (prev (make-vector n 0)) (tops '()) (lows '()))
        (if (null? vs)
            (cons (reverse tops) (reverse lows))
            (let ((cum (make-vector n 0)))
              (do ((i 0 (+ i 1)))
                  ((= i n))
                (vector-set! cum i (+ (vector-ref prev i) (or (vector-ref (car vs) i) 0))))
              (loop (cdr vs) cum (cons cum tops) (cons prev lows))))))

    ;; Are the numbers of the list `xs` strictly increasing?
    (define (increasing? xs)
      (or (null? xs)
          (let loop ((x (car xs)) (rest (cdr xs)))
            (or (null? rest)
                (and (< x (car rest))
                     (loop (car rest) (cdr rest)))))))

    ;; Checks the arguments of `draw-line-chart`.
    (define (check-arguments series xs ystep xdescr ydescr config)
      (cond ((not (and (list? series) (pair? series) (every? line-series? series)))
              (error "invalid list of series for line chart: $0" series))
            ((not (line-chart-config? config))
              (error "invalid line chart configuration: $0" config))
            ((not (and (list? xs) (pair? xs)))
              (error "invalid list of x values for line chart: $0" xs))
            ((not (or (every? finite-real? xs) (every? (lambda (x) (opt string? x)) xs)))
              (error "x values of line chart are neither all numbers nor all labels: $0" xs))
            ((and (finite-real? (car xs)) (not (increasing? xs)))
              (error "x values of line chart are not strictly increasing: $0" xs))
            ((not (opt (lambda (s) (and (finite-real? s) (> s 0))) ystep))
              (error "invalid step for y axis of line chart: $0" ystep))
            ((not (opt string? xdescr))
              (error "invalid description for x axis of line chart: $0" xdescr))
            ((not (opt string? ydescr))
              (error "invalid description for y axis of line chart: $0" ydescr))
            ((not (every? (lambda (s) (= (length (line-series-values s)) (length xs))) series))
              (error "line series do not have exactly one value for each of the $0 x values"
                     (length xs)))
            ((not (memq (line-chart-marker config) marker-kinds))
              (error "invalid marker in line chart configuration: $0" (line-chart-marker config)))
            ((not (opt (lambda (s) (and (finite-real? s) (> s 0))) (line-chart-x-step config)))
              (error "invalid step for x axis of line chart: $0" (line-chart-x-step config)))
            ((and (line-chart-area-opacity config)
                  (not (and (real? (line-chart-area-opacity config))
                            (<= 0 (line-chart-area-opacity config) 1))))
              (error "invalid area opacity in line chart configuration: $0"
                     (line-chart-area-opacity config)))))

    ;; ---------------------------------------------------------------
    ;; Main drawing procedure
    ;; ---------------------------------------------------------------

    ;; draw-line-chart draws a line chart or an area chart into a drawing.
    ;;
    ;; series  - a non-empty list of series
    ;; xs      - a non-empty list of x values, one for each data point of a series. Either
    ;;           all x values are labels (strings or `#f`) of evenly spaced positions on a
    ;;           category axis, or all x values are strictly increasing numbers, which are
    ;;           placed proportionally on a numeric axis.
    ;; ystep   - the step between ticks of the y axis, or `#f` for choosing a step
    ;;           automatically
    ;; xdescr  - description of the x axis (string or #f)
    ;; ydescr  - description of the y axis (string or #f)
    ;; loc     - a point at which the chart is placed (the size of the chart is defined by
    ;;           the configuration) or a rectangle defining the bounds of the chart
    ;; config  - a line chart configuration
    ;; legend  - a legend configuration or #f
    ;; drawing - (optional) target drawing; defaults to current-drawing
    (define (draw-line-chart series xs ystep xdescr ydescr loc config legend . args)
      (let-optionals args ((drw (current-drawing)))
        (check-arguments series xs ystep xdescr ydescr config)
        (let* ((n (length xs))
               (nser (length series))
               (numeric? (finite-real? (car xs)))
               (bounds (if (point? loc) (rect loc (line-chart-size config)) loc))
               (bx (rect-x bounds))
               (by (rect-y bounds))
               (bw (rect-width bounds))
               (bh (rect-height bounds))
               (col (line-chart-color config))
               (bgc (line-chart-bg-color config))
               (area? (line-chart-area? config))
               (stacked? (line-chart-stacked? config))
               (smooth? (line-chart-smooth? config))
               (vfont (line-chart-value-font config))
               (lfont (line-chart-label-font config))
               (dfont (line-chart-descr-font config))
               (pfont (line-chart-point-font config))
               (vfmt (line-chart-value-format config))
               (sw (line-chart-stroke-width config))
               (tick (line-chart-tick-length config))
               (msz (line-chart-marker-size config))
               (xo (line-chart-xaxis-overhead config))
               (yo (line-chart-yaxis-overhead config))
               (gridlens (line-chart-grid-line-lengths config))
               ; The plot area
               (ydescr-w (if ydescr (+ (size-height (text-size ydescr dfont)) 4) 0))
               (xdescr-h (if xdescr (+ (size-height (text-size xdescr dfont)) 2) 0))
               (px0 (+ bx (line-chart-left-pad config) ydescr-w (line-chart-vlabel-width config)))
               (px1 (- (+ bx bw) (line-chart-right-pad config) xo))
               (py0 (+ by (line-chart-top-pad config) yo))
               (py1 (- (+ by bh)
                       (line-chart-bottom-pad config)
                       xdescr-h
                       (line-chart-blabel-height config)))
               (pw (- px1 px0))
               (ph (- py1 py0))
               ; The values of the series, and, for stacked charts, the values below
               (vals (map (lambda (s) (list->vector (line-series-values s))) series))
               (stack (and stacked? (stack-values vals n)))
               (tops (if stacked? (car stack) vals))
               (lows (if stacked? (cdr stack) (map (lambda (s) #f) series)))
               ; The range of the y axis
               (ys (append (if (or area? stacked?) (list 0) '())
                           (append-map (lambda (v) (filter number? (vector->list v))) tops)))
               (data-lo (if (null? ys) 0 (minimum ys)))
               (data-hi (if (null? ys) 1 (maximum ys)))
               (lo (if (= data-lo data-hi) (- data-lo 1) data-lo))
               (hi (if (= data-lo data-hi) (+ data-hi 1) data-hi))
               (lo* (or (line-chart-y-min config) lo))
               (hi* (or (line-chart-y-max config) hi))
               (ystp (or ystep
                         (nice-step (if (> hi* lo*) (- hi* lo*) 1)
                                    (max 2 (exact (floor (/ ph 38)))))))
               (ymin (or (line-chart-y-min config)
                         (* ystp (floor (+ (/ lo ystp) 1e-9)))))
               (ymax (or (line-chart-y-max config)
                         (* ystp (ceiling (- (/ hi ystp) 1e-9)))))
               ; Maps values to y coordinates; the zero line is the baseline for areas
               (y->sy (lambda (v) (- py1 (* (/ (- v ymin) (- ymax ymin)) ph))))
               (zero-sy (y->sy (max ymin (min 0 ymax))))
               ; The x axis
               (xv (if numeric? (list->vector xs) (list->vector (index-list n))))
               (xlo (if numeric? (or (line-chart-x-min config) (vector-ref xv 0)) 0))
               (xhi (if numeric? (or (line-chart-x-max config) (vector-ref xv (- n 1))) (- n 1)))
               (ep (min (/ pw 4)
                        (or (line-chart-edge-pad config)
                            (if area?
                                0
                                (max (+ (/ msz 2) 2) (min 20 (/ pw (max 1 (- n 1)) 2)))))))
               (x->sx (lambda (v)
                        (if (> xhi xlo)
                            (+ px0 ep (* (/ (- v xlo) (- xhi xlo)) (- pw (* 2 ep))))
                            (+ px0 (/ pw 2)))))
               (sxs (vector-map x->sx xv))
               ; Ticks of the x axis: a list of pairs `(x . label)`
               (xticks (if numeric?
                           (map (lambda (v) (cons (x->sx v) (value-text v vfmt)))
                                (if (> xhi xlo)
                                    (tick-values xlo
                                                 xhi
                                                 (or (line-chart-x-step config)
                                                     (nice-step (- xhi xlo)
                                                                (max 2 (exact (floor (/ pw 70)))))))
                                    (list xlo)))
                           (map (lambda (i label) (cons (vector-ref sxs i) label))
                                (index-list n)
                                xs)))
               (yticks (tick-values ymin ymax ystp))
               ; The layers
               (palette (line-chart-palette config))
               (opacity (or (line-chart-area-opacity config) (if stacked? 1.0 0.35)))
               (layers (map (lambda (s v top low i)
                              (make-layer s
                                          (or (line-series-color s) (palette-color palette i))
                                          v
                                          (vector-map (lambda (t) (and t (y->sy t))) top)
                                          (if low
                                              (vector-map y->sy low)
                                              (make-vector n zero-sy))
                                          (or (line-series-width s) (line-chart-line-width config))
                                          (or (line-series-marker s) (line-chart-marker config))))
                            series
                            vals
                            tops
                            lows
                            (index-list nser)))
               ; Series are clipped at the plot area. If the range of an axis is automatic, a
               ; margin makes sure that markers and lines at the border are not cut off.
               (clip-pad (+ (/ msz 2) (line-chart-line-width config) 1))
               (clip-left (if (and numeric? (line-chart-x-min config)) 0 clip-pad))
               (clip-right (if (and numeric? (line-chart-x-max config)) 0 clip-pad))
               (clip-top (if (line-chart-y-max config) 0 clip-pad))
               (clip-bottom (if (line-chart-y-min config) 0 clip-pad))
               (clip-shape (rectangle (point (- px0 clip-left) (- py0 clip-top))
                                      (size (+ pw clip-left clip-right)
                                            (+ ph clip-top clip-bottom))))
               ; Draws a stroke with the width of axes
               (stroke (lambda (x0 y0 x1 y1)
                         (draw (line (point x0 y0) (point x1 y1)) sw))))
          (if (or (< pw 10) (< ph 10))
              (error "line chart is too small: $0" (rect-size bounds)))
          (if (<= ymax ymin)
              (error "invalid range of y axis of line chart: $0 to $1" ymin ymax))
          (with-drawing drw
            ; Background
            (when bgc
              (set-fill-color bgc)
              (fill-rect (rect px0 py0 pw ph)))
            ; Grid lines
            (when gridlens
              (set-color grid-color)
              (for-each (lambda (v)
                          (let ((sy (y->sy v)))
                            (draw-dashed (line (point px0 sy) (point px1 sy))
                                         gridlens
                                         0
                                         (/ sw 2))))
                        yticks)
              (when (line-chart-vertical-grid? config)
                (for-each (lambda (t)
                            (draw-dashed (line (point (car t) py0) (point (car t) py1))
                                         gridlens
                                         0
                                         (/ sw 2)))
                          xticks)))
            ; Areas, lines, and markers
            (clip-drawing
              (drawing
                ; Areas, bounded by the lines of the series and its lower border
                (when area?
                  (for-each
                    (lambda (ly)
                      (set-fill-color (with-opacity (layer-col ly) opacity))
                      (for-each
                        (lambda (run)
                          (when (> (cdr run) (car run))
                            (let* ((top (subpoints sxs (layer-tsy ly) (car run) (cdr run)))
                                   (low (subpoints sxs (layer-lsy ly) (car run) (cdr run)))
                                   (tcs (and smooth? (curve-controls top)))
                                   (lcs (and smooth? (curve-controls low)))
                                   (shp (make-shape)))
                              (move-to (vector-ref top 0) shp)
                              (trace-forward! shp top tcs)
                              (line-to (vector-ref low (- (vector-length low) 1)) shp)
                              (trace-backward! shp low lcs)
                              (line-to (vector-ref top 0) shp)
                              (fill shp))))
                        (runs-of (layer-tsy ly))))
                    layers))
                ; Lines
                (for-each
                  (lambda (ly)
                    (when (> (layer-lw ly) 0)
                      (set-color (layer-col ly))
                      (for-each
                        (lambda (run)
                          (when (> (cdr run) (car run))
                            (let* ((pts (subpoints sxs (layer-tsy ly) (car run) (cdr run)))
                                   (shp (make-shape))
                                   (lengths (line-series-lengths (layer-ser ly))))
                              (move-to (vector-ref pts 0) shp)
                              (trace-forward! shp pts (and smooth? (curve-controls pts)))
                              (if lengths
                                  (draw-dashed shp lengths 0 (layer-lw ly))
                                  (draw shp (layer-lw ly))))))
                        (runs-of (layer-tsy ly)))))
                  layers)
                ; Markers
                (for-each
                  (lambda (ly)
                    (unless (eq? (layer-marker ly) 'none)
                      (do ((i 0 (+ i 1)))
                          ((= i n))
                        (when (and (vector-ref (layer-vals ly) i) (vector-ref (layer-tsy ly) i))
                          (let ((shp (marker-shape (layer-marker ly)
                                                   (vector-ref sxs i)
                                                   (vector-ref (layer-tsy ly) i)
                                                   msz)))
                            (set-fill-color (layer-col ly))
                            (fill shp)
                            (set-color (or bgc white))
                            (draw shp 1))))))
                  layers))
              clip-shape)
            ; Axes
            (set-color col)
            (draw (arrow (point px0 zero-sy) (+ pw xo) 0 5) sw)
            (draw (arrow (point px0 py1) (+ ph yo) (- (/ pi 2)) 5) sw)
            ; Ticks of the x axis
            (for-each (lambda (t) (stroke (car t) zero-sy (car t) (+ zero-sy tick))) xticks)
            ; Labels of the x axis. Labels that would overlap are omitted.
            (when lfont
              (let loop ((ts xticks) (last-right -inf.0))
                (when (pair? ts)
                  (if (cdar ts)
                      (let* ((tsz (text-size (cdar ts) lfont))
                             (tw (size-width tsz))
                             (left (max bx (min (- (+ bx bw) tw) (- (caar ts) (/ tw 2))))))
                        (cond ((>= left (+ last-right 4))
                                (draw-text (cdar ts) (point left (+ py1 tick 2)) lfont col)
                                (loop (cdr ts) (+ left tw)))
                              (else
                                (loop (cdr ts) last-right))))
                      (loop (cdr ts) last-right)))))
            ; Ticks and labels of the y axis
            (for-each
              (lambda (v)
                (let ((sy (y->sy v)))
                  (stroke (- px0 tick) sy px0 sy)
                  (when vfont
                    (let ((tsz (text-size (value-text v vfmt) vfont)))
                      (draw-text (value-text v vfmt)
                                 (point (- px0 tick 3 (size-width tsz))
                                        (- sy (/ (size-height tsz) 2)))
                                 vfont
                                 col)))))
              yticks)
            ; Descriptions of the axes
            (when xdescr
              (let ((tsz (text-size xdescr dfont)))
                (draw-text xdescr
                           (point (- (/ (+ px0 px1) 2) (/ (size-width tsz) 2))
                                  (- (+ by bh) (line-chart-bottom-pad config) (size-height tsz)))
                           dfont
                           col)))
            (when ydescr
              (let ((tsz (text-size ydescr dfont)))
                (transform
                  (rotate (/ pi -2)
                          (translate (+ bx (line-chart-left-pad config))
                                     (+ (/ (+ py0 py1) 2) (/ (size-width tsz) 2))))
                  (draw-text ydescr zero-point dfont col))))
            ; Values next to data points. In stacked area charts, values are shown within
            ; the areas, but only if there is enough room.
            (when pfont
              (for-each
                (lambda (ly)
                  (do ((i 0 (+ i 1)))
                      ((= i n))
                    (let ((v (vector-ref (layer-vals ly) i))
                          (sy (vector-ref (layer-tsy ly) i))
                          (sx (vector-ref sxs i)))
                      (when (and v sy (>= sy (- py0 0.5)) (<= sy (+ py1 0.5))
                                      (>= sx (- px0 0.5)) (<= sx (+ px1 0.5)))
                        (let* ((txt (value-text v vfmt))
                               (tsz (text-size txt pfont))
                               (tw (size-width tsz))
                               (th (size-height tsz))
                               (tx (max px0 (min (- (if (and stacked? area?) px1 (+ bx bw)) tw)
                                                 (- sx (/ tw 2))))))
                          (if (and stacked? area?)
                              (let ((a (max py0 (min py1 sy)))
                                    (b (max py0 (min py1 (vector-ref (layer-lsy ly) i)))))
                                (when (>= (abs (- a b)) (+ th 2))
                                  (draw-text txt
                                             (point tx (- (/ (+ a b) 2) (/ th 2)))
                                             pfont
                                             (contrast-color (layer-col ly)))))
                              (let* ((r (if (eq? (layer-marker ly) 'none)
                                            (/ (layer-lw ly) 2)
                                            (/ msz 2)))
                                     (pad (line-chart-value-pad config))
                                     (above (- sy r pad th))
                                     (below (+ sy r pad)))
                                ; Values of negative numbers are shown below points, all others
                                ; above. Values are moved to the other side if they would not
                                ; fit between the top of the chart and the labels of the x axis.
                                (draw-text txt
                                           (point tx (cond ((< v 0) (if (> (+ below th) py1)
                                                                        above
                                                                        below))
                                                           (else (if (< above by) below above))))
                                           pfont
                                           col))))))))
                layers))
            ; Legend. The order of entries of stacked charts corresponds to the stack.
            (when (legend-config? legend)
              (let* ((es (filter-map (lambda (ly)
                                       (let ((label (line-series-label (layer-ser ly))))
                                         (and label (list label (layer-col ly)))))
                                     layers))
                     (entries (if stacked? (reverse es) es)))
                (when (pair? entries)
                  (draw-legend entries bounds legend col (or bgc white) drw))))))))
  )
)
