;;; LISPKIT DRAW CHART FUNCTION
;;;
;;; This library provides a configurable function plotter built on top of
;;; `(lispkit draw)`. It follows the same design patterns as
;;; `(lispkit draw chart bar)`, offering:
;;;
;;;   - A mutable configuration record (`function-chart-config`) controlling
;;;     all visual aspects: size, fonts, colors, padding, grid lines, etc.
;;;   - Support for plotting one or more functions simultaneously, each with
;;;     its own color and optional label.
;;;   - A legend configuration (reusing design from bar charts).
;;;   - Automatic axis tick computation given a step size.
;;;   - The main entry point `draw-function-chart` renders into the
;;;     current drawing or an explicitly provided one.
;;; 
;;; Example usage:
;;;    ```
;;;    (import (lispkit base)
;;;            (lispkit draw)
;;;            (lispkit draw chart function))
;;;    
;;;    (define config
;;;      (make-function-chart-config
;;;        'size: (size 500 300)
;;;        'samples: 200))
;;;    
;;;    (define my-drawing
;;;      (drawing
;;;        (draw-function-chart
;;;          (list (function-graph sin "sin(x)" blue)
;;;                (function-graph cos "cos(x)" red))
;;;          -6.3 6.3         ; x-range
;;;          -1.5 1.5         ; y-range
;;;          2.0              ; x-step
;;;          0.5              ; y-step
;;;          "x"              ; x-axis description
;;;          "y"              ; y-axis description
;;;          (point 20 20)    ; location
;;;          config           ; chart configuration
;;;          (make-function-legend-config)))) ; legend (or #f)
;;;    
;;;    (define pdf-file-path
;;;      (path (car (system-directory 'documents)) "functionplot.pdf"))
;;;    (save-drawing pdf-file-path my-drawing (size 540 340))
;;;    (open-file pdf-file-path)
;;;    ```
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may
;;; not use this file except in compliance with the License. You may obtain
;;; a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;;; License for the specific language governing permissions and limitations
;;; under the License.


(define-library (lispkit draw chart function)

  (export
    ; Function graph descriptors
    function-graph
    function-graph?
    function-graph-procedure
    function-graph-label
    function-graph-color
    function-graph-line-width
    function-graph-line-lengths
    vertical-line-graph
    inline-label-graph
    
    ; Legend configuration
    make-function-legend-config

    ; Function chart configuration
    make-function-chart-config
    function-chart-config?
    function-chart-size
    function-chart-size-set!
    function-chart-color
    function-chart-color-set!
    function-chart-bg-color
    function-chart-bg-color-set!
    function-chart-axis-font
    function-chart-axis-font-set!
    function-chart-label-font
    function-chart-label-font-set!
    function-chart-descr-font
    function-chart-descr-font-set!
    function-chart-stroke-width
    function-chart-stroke-width-set!
    function-chart-line-width
    function-chart-line-width-set!
    function-chart-top-pad
    function-chart-top-pad-set!
    function-chart-bottom-pad
    function-chart-bottom-pad-set!
    function-chart-left-pad
    function-chart-left-pad-set!
    function-chart-right-pad
    function-chart-right-pad-set!
    function-chart-axis-label-width
    function-chart-axis-label-width-set!
    function-chart-axis-label-height
    function-chart-axis-label-height-set!
    function-chart-tick-length
    function-chart-tick-length-set!
    function-chart-grid-line-lengths
    function-chart-grid-line-lengths-set!
    function-chart-samples
    function-chart-samples-set!
    function-chart-axis-overhead
    function-chart-axis-overhead-set!
    
    ; Draw function charts
    draw-function-chart)

  (import (lispkit base)
          (lispkit draw)
          (lispkit draw chart)
          (lispkit format))

  (begin

    ;; ---------------------------------------------------------------
    ;; Function descriptors
    ;; ---------------------------------------------------------------

    ;; A function graph descriptor bundles a Scheme procedure, an optional label,
    ;; and a color for plotting.
    (define-record-type <function-graph>
      (make-function-graph proc label col line-width line-lengths)
      function-graph?
      (proc  function-graph-procedure)
      (label function-graph-label)
      (col function-graph-color)
      (line-width function-graph-line-width)
      (line-lengths function-graph-line-lengths))

    ;; Constructor with convenient argument order.
    ;; (func proc) | (func proc label) | (func proc label color)
    (define (function-graph proc . args)
      (let-optionals args ((label #f)
                           (col blue)
                           (width #f)
                           (lengths #f))
        (make-function-graph proc label col width lengths)))
    
    ;; Should this function graph show up in the legend?
    (define (function-graph-in-legend? f)
      (and (function-graph-label f) (not (pair? (function-graph-procedure f)))))
    
    (define (vertical-line-graph x . args)
      (let-optionals args ((label #f)
                           (col blue)
                           (width #f)
                           (lengths #f))
        (make-function-graph x label col width lengths)))
    
    ;; Return an inline label for location `loc` with the given color `col`
    ;; and font `font`.
    (define (inline-label-graph loc label . args)
      (let-optionals args ((col blue)
                           (font (font "Helvetica" 8)))
        (make-function-graph (list loc font) label col #f #f)))

    ;; ---------------------------------------------------------------
    ;; Legend configuration
    ;; ---------------------------------------------------------------
    
    ;; Create a default function legend config with keyword arguments.
    (define (make-function-legend-config . args)
      (let-keywords args
        ((font (font "Helvetica" 9)) ; font used in legends
         (color             black)   ; color used for labels in legends
         (bg-color          white)   ; background color of legend
         (stroke-width      0.5)     ; width of a stroke for drawing the bounding box
         (corner-radius     0)       ; radius for rounded bounding box
         (horizontal-offset 0)       ; horizontal offset from chart bounds (neg = right offset)
         (vertical-offset   0)       ; vertical offset from chart bounds (neg = bottom offset)
         (sample-area-width #f)      ; width of the sample area
         (sample-length     12)      ; heigh and width of the color sample box
         (line-pad          3)       ; padding between lines
         (entry-pad         5))      ; top/bottom and left/right padding around legend
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
    ;; Chart configuration
    ;; ---------------------------------------------------------------

    (define-record-type <function-chart-config>
      (create-function-chart-config
        fcc-size              ; Overall size of the chart area in points.
        fcc-color             ; Color for axes, ticks, and labels.
        fcc-bg-color          ; Background color of the plot area. `#f` disables
                              ; background filling.
        fcc-box-color         ; Color for box around plot area. `#f` disables box.
        fcc-axis-font         ; Font used for numeric tick labels on both axes.     
        fcc-label-font        ; Font used for axis name labels.
        fcc-descr-font        ; Font used for axis description text.
        fcc-stroke-width      ; Stroke width in points for axes, ticks, and the plot border.
        fcc-line-width        ; Stroke width in points for the function graphs.
        fcc-top-pad           ; Padding above the plot area in points.
        fcc-bottom-pad        ; Padding below the x-axis label area in points.
        fcc-left-pad          ; Padding to the left of the y-axis label area in points.
        fcc-right-pad         ; Padding to the right of the plot area in points.
        fcc-axis-label-width  ; Width reserved for y-axis tick labels in points.
        fcc-axis-label-height ; Height reserved for x-axis tick labels in points.
        fcc-tick-length       ; Length of axis tick marks in points.
        fcc-grid-line-lengths ; List of alternating dash/space lengths for grid lines
                              ; (e.g. `(1 3)`). `#f` disables grid lines.
        fcc-samples           ; Number of evenly spaced sample points used to render each
                              ; function graph.
        fcc-axis-overhead)    ; Extra length in points by which axes extend beyond the plot
                              ; area boundary.
      function-chart-config?
      (fcc-size              function-chart-size
                             function-chart-size-set!)
      (fcc-color             function-chart-color
                             function-chart-color-set!)
      (fcc-bg-color          function-chart-bg-color
                             function-chart-bg-color-set!)
      (fcc-box-color         function-chart-box-color
                             function-chart-box-color-set!)
      (fcc-axis-font         function-chart-axis-font
                             function-chart-axis-font-set!)
      (fcc-label-font        function-chart-label-font
                             function-chart-label-font-set!)
      (fcc-descr-font        function-chart-descr-font
                             function-chart-descr-font-set!)
      (fcc-stroke-width      function-chart-stroke-width
                             function-chart-stroke-width-set!)
      (fcc-line-width        function-chart-line-width
                             function-chart-line-width-set!)
      (fcc-top-pad           function-chart-top-pad
                             function-chart-top-pad-set!)
      (fcc-bottom-pad        function-chart-bottom-pad
                             function-chart-bottom-pad-set!)
      (fcc-left-pad          function-chart-left-pad
                             function-chart-left-pad-set!)
      (fcc-right-pad         function-chart-right-pad
                             function-chart-right-pad-set!)
      (fcc-axis-label-width  function-chart-axis-label-width
                             function-chart-axis-label-width-set!)
      (fcc-axis-label-height function-chart-axis-label-height
                             function-chart-axis-label-height-set!)
      (fcc-tick-length       function-chart-tick-length
                             function-chart-tick-length-set!)
      (fcc-grid-line-lengths function-chart-grid-line-lengths
                             function-chart-grid-line-lengths-set!)
      (fcc-samples           function-chart-samples
                             function-chart-samples-set!)
      (fcc-axis-overhead     function-chart-axis-overhead
                             function-chart-axis-overhead-set!))
  
    (define (make-function-chart-config . args)
      (let-keywords args
        ((size              (size 500 300))        ; size
         (color             black)                 ; color (axes, text)
         (bg-color          white)                 ; bg-color
         (box-color         (color 0.7 0.7 0.7))   ; box-color
         (axis-font         (font "Helvetica" 9))  ; axis-font (tick labels)
         (label-font        (font "Helvetica" 10)) ; label-font (axis names)
         (descr-font        (font "Helvetica" 9))  ; descr-font
         (stroke-width      1.0)                   ; stroke-width (axes)
         (line-width        1.5)                   ; line-width (function graphs)
         (top-pad           12)                    ; top-pad
         (bottom-pad        12)                    ; bottom-pad
         (left-pad          12)                    ; left-pad
         (right-pad         12)                    ; right-pad
         (axis-label-width  40)                    ; axis-label-width (y-axis labels)
         (axis-label-height 20)                    ; axis-label-height (x-axis labels)
         (tick-length       4)                     ; tick-length
         (grid-line-lengths '(1 3))                ; grid-line-lengths (#f to disable)
         (samples           200)                   ; samples
         (axis-overhead     12))                   ; axis-overhead
        (create-function-chart-config
          size
          color
          bg-color
          box-color
          axis-font
          label-font
          descr-font
          stroke-width
          line-width
          top-pad
          bottom-pad
          left-pad
          right-pad
          axis-label-width
          axis-label-height
          tick-length
          grid-line-lengths
          samples
          axis-overhead)))
    
    ;; ---------------------------------------------------------------
    ;; Internal helpers
    ;; ---------------------------------------------------------------

    ;; Format a numeric value for tick labels.
    ;; Uses up to 4 decimal places but strips trailing zeros.
    (define (format-tick-value v)
      (if (= v (round v))
        (format "~D" (exact (round v)))
        (let ((s (format "~,4F" v)))
          ; Strip trailing zeros after decimal point
          (let loop ((i (- (string-length s) 1)))
            (cond
              ((< i 0) s)
              ((char=? (string-ref s i) #\0)
               (loop (- i 1)))
              ((char=? (string-ref s i) #\.)
               (string-copy s 0 i))
              (else
               (string-copy s 0 (+ i 1))))))))

    ;; Generate tick values: all multiples of step in [lo, hi]
    (define (tick-values lo hi step)
      (let ((start (* (ceiling (/ lo step)) step)))
        (let loop ((v start) (acc '()))
          (if (> v (+ hi (* step 0.001)))
            (reverse acc)
            (loop (+ v step) (cons v acc))))))

    ;; Map a value from [domain-lo, domain-hi] to [range-lo, range-hi]
    (define (map-value val domain-lo domain-hi range-lo range-hi)
      (let ((t (/ (- val domain-lo) (- domain-hi domain-lo))))
        (+ range-lo (* t (- range-hi range-lo)))))

    ;; Clamp a value between lo and hi
    (define (clamp val lo hi)
      (min hi (max lo val)))
    
    ;; Return a line shape ending with an arrowhead.
    ;; start - starting point of the line
    ;; len   - length of the line
    ;; angle - rotation angle in radians (0 = rightward)
    ;; d     - size of the arrowhead fins
    ;; dbl   - (unused here, reserved for double arrowheads)
    (define (arrow start len angle d dbl)
      (transform-shape
        (shape
          (move-to zero-point)
          (line-to (point len 0))
          (line-to (point (- len d) (- d)))
          (move-to (point len 0))
          (line-to (point (- len d) d)))
        (rotate angle (translate (point-x start) (point-y start)))))
        
    ;; ---------------------------------------------------------------
    ;; Drawing the legend
    ;; ---------------------------------------------------------------
    
    ;; Draw the legend within the plot area.
    ;; ox, oy, plot-w, plot-h define the plot area rectangle.
    (define (draw-legend funcs ox oy plot-w plot-h lconf drw)
      (let* ((lfont       (legend-font lconf))
             (lcol        (legend-color lconf))
             (lsw         (legend-stroke-width lconf))
             (horiz-off   (legend-horizontal-offset lconf))
             (vert-off    (legend-vertical-offset lconf))
             (sample-len  (legend-sample-length lconf))
             (lpad        (legend-line-pad lconf))
             (epad        (legend-entry-pad lconf))
             ; Filter to only those funcs with labels
             (labeled     (filter (lambda (f) (function-graph-label f)) funcs))
             ; Compute legend dimensions
             (line-height (+ (size-height (text-size "Mg" lfont)) lpad))
             (max-label-w (apply max 0
                            (map (lambda (f)
                                   (size-width (text-size (function-graph-label f) lfont)))
                                 labeled)))
             (numlabeled  (length (filter function-graph-in-legend? labeled)))
             (legend-w    (+ epad sample-len epad max-label-w epad))
             (legend-h    (+ epad (* numlabeled line-height) epad))
             ; Horizontal position: positive = from left, negative = from right
             (lx (if (>= horiz-off 0)
                   (+ ox horiz-off)
                   (+ ox plot-w horiz-off (- legend-w))))
             ; Vertical position: positive = from top, negative = from bottom
             (ly (if (>= vert-off 0)
                   (+ oy vert-off)
                   (+ oy plot-h vert-off (- legend-h)))))
        (when (> numlabeled 0)
          (with-drawing drw
            ; Draw legend background and edge
            (let ((surface (if (> (legend-corner-radius lconf) 0)
                               (rectangle (point lx ly)
                                          (size legend-w legend-h)
                                          (legend-corner-radius lconf))
                               (rectangle (point lx ly) (size legend-w legend-h)))))
              (set-fill-color (legend-bg-color lconf))
              (fill surface)
              (set-color lcol)
              (draw surface lsw))
            ; Draw each entry
            (let loop ((fs labeled) (y (+ ly epad)))
              (cond
                ((null? fs))
                ((function-graph-in-legend? (car fs))
                  (let* ((f (car fs))
                         (sample-y (+ y (/ line-height 2))))
                    ; Draw colored line sample
                    (set-color (function-graph-color f))
                    (set-line-width 1.5)
                    (draw-line (point (+ lx epad) sample-y)
                               (point (+ lx epad sample-len) sample-y))
                    ; Draw label text
                    (draw-text (function-graph-label f)
                               (point (+ lx epad sample-len epad)
                                      (- y (* lpad -0.2)))
                               lfont
                               lcol))
                  (loop (cdr fs) (+ y line-height)))
                (else
                  (loop (cdr fs) y))))))))

   ;; ---------------------------------------------------------------
    ;; Main drawing procedure
    ;; ---------------------------------------------------------------

    ;; draw-function-chart draws one or more functions into a drawing.
    ;;
    ;; funcs    - a single func or list of func descriptors
    ;; xmin     - minimum x value of domain
    ;; xmax     - maximum x value of domain
    ;; ymin     - minimum y value of range
    ;; ymax     - maximum y value of range
    ;; xstep    - increment for x-axis ticks
    ;; ystep    - increment for y-axis ticks
    ;; xdescr   - label for x-axis (string or #f)
    ;; ydescr   - label for y-axis (string or #f)
    ;; loc      - point at which the chart is placed
    ;; config   - function-chart-config object
    ;; legend   - legend-config or #f
    ;; drawing  - (optional) target drawing; defaults to current-drawing
    (define (draw-function-chart funcs xmin xmax ymin ymax
                                 xstep ystep xdescr ydescr
                                 loc config legend . rest)
      (let* ((drw (if (pair? rest) (car rest) (current-drawing)))
             ; Normalize funcs to a list
             (funcs (if (function-graph? funcs) (list funcs) funcs))
             ; Extract config values
             (chart-size   (function-chart-size config))
             (total-w      (size-width chart-size))
             (total-h      (size-height chart-size))
             (col          (function-chart-color config))
             (bg-col       (function-chart-bg-color config))
             (axis-font    (function-chart-axis-font config))
             (label-font   (function-chart-label-font config))
             (descr-font   (function-chart-descr-font config))
             (sw           (function-chart-stroke-width config))
             (lw           (function-chart-line-width config))
             (top-pad      (function-chart-top-pad config))
             (bot-pad      (function-chart-bottom-pad config))
             (left-pad     (function-chart-left-pad config))
             (right-pad    (function-chart-right-pad config))
             (ylabel-w     (function-chart-axis-label-width config))
             (xlabel-h     (function-chart-axis-label-height config))
             (tick-len     (function-chart-tick-length config))
             (grid-lens    (function-chart-grid-line-lengths config))
             (n-samples    (function-chart-samples config))
             (overhead     (function-chart-axis-overhead config))
             ; Adaptive y-axis label margins
             ; - Compute the fraction of the plot that lies to the left of x=0
             (y-axis-frac  (cond ((<= xmax 0) 1.0)
                                 ((>= xmin 0) 0.0)
                                 (else (/ (- 0 xmin) (- xmax xmin)))))
             ; - Raw plot width before label-margin adjustment
             (raw-plot-w   (- total-w left-pad right-pad))
             ; - Interior space available to the left/right of the y-axis
             (interior-left  (* y-axis-frac raw-plot-w))
             (interior-right (* (- 1.0 y-axis-frac) raw-plot-w))
             ; - Extra margin needed: deficit between required label width
             ;   and the available interior space (zero if enough room)
             (ylabel-left-w  (max 0 (- ylabel-w interior-left)))
             (ylabel-right-w (max 0 (- ylabel-w interior-right)))
             ; Adaptive x-axis label margins
             ; - Compute the fraction of the plot that lies below y=0
             ;   (in data coords; screen y is flipped later)
             (x-axis-frac  (cond ((<= ymax 0) 0.0)
                                 ((>= ymin 0) 1.0)
                                 (else (/ (- 0 ymin) (- ymax ymin)))))
             ; - Raw plot height before label-margin adjustment
             (raw-plot-h   (- total-h top-pad bot-pad))
             ; - Interior space available below/above the x-axis
             (interior-below (* x-axis-frac raw-plot-h))
             (interior-above (* (- 1.0 x-axis-frac) raw-plot-h))
             ; - Extra margin needed for x-axis labels
             (xlabel-bot-h   (max 0 (- xlabel-h interior-below)))
             (xlabel-top-h   (max 0 (- xlabel-h interior-above)))
             ; Final plot area rectangle (in output coords)
             (ox        (+ (point-x loc) left-pad ylabel-left-w))
             (oy        (+ (point-y loc) top-pad xlabel-top-h))
             (plot-w    (- total-w left-pad ylabel-left-w ylabel-right-w right-pad))
             (plot-h    (- total-h top-pad xlabel-top-h xlabel-bot-h bot-pad))
             ; Mapping helpers: data coords -> screen coords
             ; - Screen y is flipped (0 at top, increasing downward)
             (x->sx     (lambda (x) (map-value x xmin xmax ox (+ ox plot-w))))
             (y->sy     (lambda (y) (map-value y ymin ymax (+ oy plot-h) oy)))
             ; Axis origin in screen coords (clamped to plot area)
             (origin-sx (clamp (x->sx 0) ox (+ ox plot-w)))
             (origin-sy (clamp (y->sy 0) oy (+ oy plot-h))))
        (with-drawing drw
          ; Background
          (when bg-col
            (set-fill-color bg-col)
            (fill-rect (rect ox oy plot-w plot-h)))
          ; Grid lines
          (when grid-lens
            ; Horizontal grid lines at y ticks
            (let ((yticks (tick-values ymin ymax ystep)))
              (for-each
                (lambda (yv)
                  (let ((sy (y->sy yv)))
                    (when (and (>= sy oy) (<= sy (+ oy plot-h)))
                      (set-color (color 0.8 0.8 0.8))
                      (draw-dashed (line (point ox sy)
                                         (point (+ ox plot-w) sy))
                                   grid-lens 0 sw))))
                yticks))
            ; Vertical grid lines at x ticks
            (let ((xticks (tick-values xmin xmax xstep)))
              (for-each
                (lambda (xv)
                  (let ((sx (x->sx xv)))
                    (when (and (>= sx ox) (<= sx (+ ox plot-w)))
                      (set-color (color 0.8 0.8 0.8))
                      (draw-dashed (line (point sx oy)
                                         (point sx (+ oy plot-h)))
                                   grid-lens 0 sw))))
                xticks)))
          ; Axes (drawn as arrows)
          (set-color col)
          (set-line-width sw drw)
          (let ((arrow-d 5))  ; arrowhead fin size in points
            ; X-axis: arrow pointing right, from left overhead to right overhead
            (let* ((x-left  (- ox overhead))
                   (x-right (+ ox plot-w overhead))
                   (x-len   (- x-right x-left 1)))
              (draw (arrow (point x-left origin-sy) x-len 0 arrow-d #f) sw drw))
            ; Y-axis: arrow pointing upward, from bottom overhead to top overhead
            ; In screen coords, "up" is negative y, so the arrow points from
            ; bottom (+ oy plot-h overhead) upward to (- oy overhead).
            ; We draw from the top going downward (angle π/2) for the shaft,
            ; but the arrowhead should be at the top — so we draw from bottom
            ; to top using angle -π/2.
            (let* ((y-top    (- oy overhead))
                   (y-bottom (+ oy plot-h overhead))
                   (y-len    (- y-bottom y-top 1)))
              (draw (arrow (point origin-sx y-bottom)
                           y-len
                           (* -0.5 pi)  ; -π/2 = upward
                           arrow-d
                           #f)
                     sw drw)))
          ; X-axis ticks and labels
          (let ((xticks (tick-values xmin xmax xstep)))
            (for-each
              (lambda (xv)
                (let* ((sx    (x->sx xv))
                       (label (format-tick-value xv))
                       (tsz   (text-size label axis-font)))
                  (when (and (>= sx ox) (<= sx (+ ox plot-w)))
                    ; Tick mark
                    (set-color col)
                    (draw-line (point sx origin-sy)
                               (point sx (+ origin-sy tick-len))
                               drw)
                    ; Label below tick
                    (draw-text label
                               (point (- sx (/ (size-width tsz) 2))
                                      (+ origin-sy tick-len 2))
                               axis-font col drw))))
              xticks))
          ; Y-axis ticks and labels
          (let ((yticks (tick-values ymin ymax ystep)))
            (for-each
              (lambda (yv)
                (let* ((sy    (y->sy yv))
                       (label (format-tick-value yv))
                       (tsz   (text-size label axis-font)))
                  (when (and (>= sy oy) (<= sy (+ oy plot-h)))
                    ;; Tick mark
                    (set-color col)
                    (draw-line (point origin-sx sy)
                               (point (- origin-sx tick-len) sy)
                               drw)
                    ;; Label to the left of tick
                    (draw-text label
                               (point (- origin-sx tick-len 2
                                         (size-width tsz))
                                      (- sy (/ (size-height tsz) 2)))
                               axis-font col drw))))
              yticks))
          ; Axis description labels
          ; - X-axis description: past the right overhead, centred on the axis
          (when xdescr
            (let ((tsz (text-size xdescr descr-font)))
              (draw-text xdescr
                         (point (+ ox plot-w overhead 2)
                                (- origin-sy (/ (size-height tsz) 2)))
                         descr-font col drw)))
          ; - Y-axis description: above the top overhead, centred on the axis
          (when ydescr
            (let ((tsz (text-size ydescr descr-font)))
              (draw-text ydescr
                         (point (- origin-sx (/ (size-width tsz) 2))
                                (- oy overhead (size-height tsz) 2))
                         descr-font col drw)))
          ; Plot each function
          (for-each
            (lambda (f)
              (let* ((proc (function-graph-procedure f))
                     (fcol (function-graph-color f))
                     (dx   (/ (- xmax xmin) n-samples))
                     (s    (make-shape)))
                (set-color fcol)
                (cond
                  ((procedure? proc)
                    (let loop ((i 0) (first? #t))
                      (when (<= i n-samples)
                        (let* ((x  (+ xmin (* i dx)))
                               (sx (x->sx x))
                               (y (proc x)))
                          (if (and y (real? y))
                              (let ((sy (clamp (y->sy y) (- oy 1000) (+ oy plot-h 1000))))
                                (if first?
                                    (move-to (point sx sy) s)
                                    (line-to (point sx sy) s))
                                 (loop (+ i 1) #f))
                              (loop (+ i 1) #t))))))
                  ((real? proc)
                    (move-to (point (x->sx proc) (y->sy ymax)) s)
                    (line-to (point (x->sx proc) (y->sy ymin)) s))
                  ((list? proc)
                    (let-optionals proc ((loc (point (/ (+ xmin xmax) 2)
                                                     (/ (+ ymin ymax) 2)))
                                         (font (font "Helvetica" 8)))
                      (draw-text
                        (function-graph-label f)
                        (point (x->sx (point-x loc))
                               (- (y->sy (point-y loc))
                                  (/ (size-height (text-size (function-graph-label f) font)) 2)))
                        font
                        fcol))))
                ; Clip the curve to the plot area and draw it
                (let ((clip-shape (rectangle (point ox oy)
                                             (size plot-w plot-h)))
                      (curve-drawing
                        (drawing
                          (set-color fcol)
                          (if-let* ((lengths (function-graph-line-lengths f)))
                            (draw-dashed s lengths 0 (or (function-graph-line-width f) lw))
                            (draw s (or (function-graph-line-width f) lw))))))
                  (clip-drawing curve-drawing clip-shape drw))))
            funcs)
          ; Draw border around plot area
          (when-let* ((box-col (function-chart-box-color config)))
            (set-color box-col)
            (draw (rectangle (point ox oy) (size plot-w plot-h)) sw drw))
        ; Legend
        (if (legend-config? legend)
            (draw-legend funcs (point-x loc) (point-y loc) total-w total-h legend drw)))))
            ; (draw-legend funcs ox oy plot-w plot-h legend drw)))))
  )
)
