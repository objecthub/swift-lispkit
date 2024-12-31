;;; LISPKIT DRAW CHART BAR
;;;
;;; This is a drawing library implementing bar charts. A bar chart consists of a
;;; sequence of bars and bar groups. A bar group is a cluster of related bars that are
;;; shown next to each other. Bars and bar groups have labels that are shown underneath
;;; the corresponding bar or bar group. Each bar might be segmented, consisting of
;;; stacked bar segments. Each bar segment is identified via a distinct color. A mapping
;;; from colors to labels can be shown in a legend. The value of each bar can be drawn
;;; on top of the bar, or can be read from the y-axis showing a sequence of bar values in
;;; with a given delta.
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2022 Matthias Zenger. All rights reserved.
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

(define-library (lispkit draw chart bar)
  
  (export
    ; Legends
    make-legend-config
    legend-config?
    legend-font
    legend-font-set!
    legend-stroke-width
    legend-stroke-width-set!
    legend-horizontal-offset
    legend-horizontal-offset-set!
    legend-vertical-offset
    legend-vertical-offset-set!
    legend-sample-area-width
    legend-sample-area-width-set!
    legend-sample-length
    legend-sample-length-set!
    legend-line-pad
    legend-line-pad-set!
    legend-entry-pad
    legend-entry-pad-set!
    ; Bar chart configs
    make-bar-chart-config
    bar-chart-config?
    bar-chart-size
    bar-chart-size-set!
    bar-chart-value-font
    bar-chart-value-font-set!
    bar-chart-bar-font
    bar-chart-bar-font-set!
    bar-chart-label-font
    bar-chart-label-font-set!
    bar-chart-group-font
    bar-chart-group-font-set!
    bar-chart-descr-font
    bar-chart-descr-font-set!
    bar-chart-stroke-width
    bar-chart-stroke-width-set!
    bar-chart-top-pad
    bar-chart-top-pad-set!
    bar-chart-bottom-pad
    bar-chart-bottom-pad-set!
    bar-chart-right-pad
    bar-chart-right-pad-set!
    bar-chart-left-pad
    bar-chart-left-pad-set!
    bar-chart-bar-gap
    bar-chart-bar-gap-set!
    bar-chart-group-gap
    bar-chart-group-gap-set!
    bar-chart-vlabel-width
    bar-chart-vlabel-width-set!
    bar-chart-vindicator-width
    bar-chart-vindicator-width-set!
    bar-chart-vline-lengths
    bar-chart-vline-lengths-set!
    bar-chart-value-pad
    bar-chart-value-pad-set!
    bar-chart-blabel-height
    bar-chart-blabel-height-set!
    bar-chart-glabel-height
    bar-chart-glabel-height-set!
    bar-chart-xaxis-overhead
    bar-chart-xaxis-overhead-set!
    bar-chart-yaxis-overhead
    bar-chart-yaxis-overhead-set!
    ; Bar specs
    bar-spec?
    bar?
    bar
    bar-label
    bar-color
    bar-values
    bar-group?
    bar-group
    bar-group-label
    bar-group-bars
    bar-segment
    ; Draw bar charts
    draw-bar-chart)
  
  (import (lispkit base)
          (lispkit math util)
          (lispkit draw))
  
  ;; Legends
  
  (begin
    (define-record-type <legend-config>
      (legend-config
        legend-font                  ; font used in legends
        legend-stroke-width          ; width of a stroke for drawing the bounding box
        legend-horizontal-offset     ; horizontal offset from chart bounds (neg = right offset)
        legend-vertical-offset       ; vertical offset from chart bounds (neg = bottom offset)
        legend-sample-area-width     ; width of the sample area
        legend-sample-length         ; heigh and width of the color sample box
        legend-line-pad              ; padding between lines
        legend-entry-pad)            ; top/bottom and left/right padding around legend
      legend-config?
      (legend-font legend-font legend-font-set!)
      (legend-stroke-width legend-stroke-width legend-stroke-width-set!)
      (legend-horizontal-offset legend-horizontal-offset legend-horizontal-offset-set!)
      (legend-vertical-offset legend-vertical-offset legend-vertical-offset-set!)
      (legend-sample-area-width legend-sample-area-width legend-sample-area-width-set!)
      (legend-sample-length legend-sample-length legend-sample-length-set!)
      (legend-line-pad legend-line-pad legend-line-pad-set!)
      (legend-entry-pad legend-entry-pad legend-entry-pad-set!))
    
    (define (make-legend-config . args)
      (let-keywords args
        ((font              (font "Helvetica" 10)) ; font used in legends
         (stroke-width      1)       ; width of a stroke for drawing the bounding box
         (horizontal-offset 70)      ; horizontal offset from chart bounds (neg = right offset)
         (vertical-offset   10)      ; vertical offset from chart bounds (neg = bottom offset)
         (sample-area-width 17)      ; width of the sample area
         (sample-length     10)      ; heigh and width of the color sample box
         (line-pad          3)       ; padding between lines
         (entry-pad         6))      ; top/bottom and left/right padding around legend
        (legend-config
          font
          stroke-width
          horizontal-offset
          vertical-offset
          sample-area-width
          sample-length
          line-pad
          entry-pad)))
  )
  
  ;; Bar charts
  
  (begin
    
    ;; Bar chart configs
    
    (define-record-type <bar-chart-config>
      (bar-chart-config
        bar-chart-size             ; the rectangle in which the chart is drawn
        bar-chart-color            ; color of text, axis, etc.
        bar-chart-bg-color         ; background color (of legend)
        bar-chart-value-font       ; font for displaying values
        bar-chart-bar-font         ; font for displaying values on top of bars
        bar-chart-label-font       ; font for displaying bar labels
        bar-chart-group-font       ; font for displaying bar group labels
        bar-chart-descr-font       ; font for describing the axis
        bar-chart-stroke-width     ; width of a stroke (e.g. for drawing the coordinate system)
        bar-chart-top-pad          ; top padding
        bar-chart-bottom-pad       ; padding below the x axis
        bar-chart-right-pad        ; right-side padding
        bar-chart-left-pad         ; padding left to y axis
        bar-chart-bar-gap          ; space between two bars
        bar-chart-group-gap        ; space between two bars within a group
        bar-chart-vlabel-width     ; width of y-labels
        bar-chart-vindicator-width ; width of y-label indicators
        bar-chart-vline-lengths    ; alternating list of dash/space lengths for value lines
        bar-chart-value-pad        ; padding above bars between bar and displayed value
        bar-chart-blabel-height    ; height of bar labels
        bar-chart-glabel-height    ; height of group labels
        bar-chart-xaxis-overhead   ; overhead on x axis
        bar-chart-yaxis-overhead)  ; overhead on y axis
      bar-chart-config?
      (bar-chart-size bar-chart-size bar-chart-size-set!)
      (bar-chart-color bar-chart-color bar-chart-color-set!)
      (bar-chart-bg-color bar-chart-bg-color bar-chart-bg-color-set!)
      (bar-chart-value-font bar-chart-value-font bar-chart-value-font-set!)
      (bar-chart-bar-font bar-chart-bar-font bar-chart-bar-font-set!)
      (bar-chart-label-font bar-chart-label-font bar-chart-label-font-set!)
      (bar-chart-group-font bar-chart-group-font bar-chart-group-font-set!)
      (bar-chart-descr-font bar-chart-descr-font bar-chart-descr-font-set!)
      (bar-chart-stroke-width bar-chart-stroke-width bar-chart-stroke-width-set!)
      (bar-chart-top-pad bar-chart-top-pad bar-chart-top-pad-set!)
      (bar-chart-bottom-pad bar-chart-bottom-pad bar-chart-bottom-pad-set!)
      (bar-chart-right-pad bar-chart-right-pad bar-chart-right-pad-set!)
      (bar-chart-left-pad bar-chart-left-pad bar-chart-left-pad-set!)
      (bar-chart-bar-gap bar-chart-bar-gap bar-chart-bar-gap-set!)
      (bar-chart-group-gap bar-chart-group-gap bar-chart-group-gap-set!)
      (bar-chart-vlabel-width bar-chart-vlabel-width bar-chart-vlabel-width-set!)
      (bar-chart-vindicator-width bar-chart-vindicator-width bar-chart-vindicator-width-set!)
      (bar-chart-vline-lengths bar-chart-vline-lengths bar-chart-vline-lengths-set!)
      (bar-chart-value-pad bar-chart-value-pad bar-chart-value-pad-set!)
      (bar-chart-blabel-height bar-chart-blabel-height bar-chart-blabel-height-set!)
      (bar-chart-glabel-height bar-chart-glabel-height bar-chart-glabel-height-set!)
      (bar-chart-xaxis-overhead bar-chart-xaxis-overhead bar-chart-xaxis-overhead-set!)
      (bar-chart-yaxis-overhead bar-chart-yaxis-overhead bar-chart-yaxis-overhead-set!))
    
    (define (make-bar-chart-config . args)
      (let-keywords args
        ((size             (size 800 400))        ; the rectangle in which the chart is drawn
         (color            black)                 ; color of text, axis, etc.
         (bg-color         white)                 ; color of background (for legend)
         (value-font       (font "Helvetica" 10)) ; font for displaying values
         (bar-font         (font "Helvetica" 11)) ; font for displaying values on top of bars
         (label-font       (font "Helvetica" 12)) ; font for displaying bar labels
         (group-font       (font "Helvetica-Bold" 12)) ; font for displaying bar group labels
         (descr-font       (font "Helvetica-LightOblique" 10)) ; font for describing the axis
         (stroke-width     1)                     ; width of a stroke
         (top-pad          20)                    ; top padding
         (bottom-pad       5)                     ; padding below the x axis
         (right-pad        15)                    ; right-side padding
         (left-pad         10)                    ; padding left to y axis
         (bar-gap          20)                    ; space between two bars
         (group-gap        5)                     ; space between two bars within a group
         (vlabel-width     50)                    ; width of y-labels
         (vindicator-width 10)                    ; width of y-label indicators
         (vline-lengths    '(1 2))                ; alternating list of dash/space lengths
         (value-pad        1)                     ; padding between bar and displayed value
         (blabel-height    14)                    ; height of bar labels
         (glabel-height    30)                    ; height of group labels
         (xaxis-overhead   20)                    ; overhead on x axis
         (yaxis-overhead   20))                   ; overhead on y axis
        (bar-chart-config
          size
          color
          bg-color
          value-font
          bar-font
          label-font
          group-font
          descr-font
          stroke-width
          top-pad
          bottom-pad
          right-pad
          left-pad
          bar-gap
          group-gap
          vlabel-width
          vindicator-width
          vline-lengths
          value-pad
          blabel-height
          glabel-height
          xaxis-overhead
          yaxis-overhead)))
  )
  
  (begin
    
    ;; Creating bar charts
    
    (define (same-sign? numbers sign)
      (if (pair? numbers)
          (if (or (zero? sign) (zero? (sgn (car numbers))) (= (sgn (car numbers)) sign))
              (same-sign? (cdr numbers) (if (zero? sign) (sgn (car numbers)) sign))
              #f)
          #t))
    
    (define (bar-spec? obj)
      (or (bar? obj) (bar-group? obj)))
    
    (define (bar? obj)
      (and (pair? obj)
           (opt string? (car obj))
           (pair? (cdr obj))
           (opt color? (cadr obj))
           (every? number? (cddr obj))))
    
    (define (bar label arg . args)
      (assert (opt string? label))
      (if (color? arg)
          (if (pair? args)
              (if (every? number? args)
                  (cons label (cons arg args))
                  (error "invalid values for bar: $0" args))
              (error "missing value for bar: $0" (cons 'bar (cons label (cons arg args)))))
          (if (every? number? (cons arg args))
              (if (same-sign? (cons arg args) 0.0)
                  (cons label (cons #f (cons arg args)))
                  (error "segment values have inconsistent signs: $0" (cons arg args)))
              (error "invalid values for bar: $0" (cons arg args)))))
    
    (define bar-label car)
    (define bar-color cadr)
    (define bar-values cddr)
    
    (define (bar-group? obj)
      (and (pair? obj)
           (opt string? (car obj))
           (every? bar? (cdr obj))))
    
    (define (bar-group label bar . bars)
      (assert (opt string? label)
              (every? bar? (cons bar bars)))
      (cons label (cons bar bars)))
    
    (define bar-group-label car)
    (define bar-group-bars cdr)
    
    (define (bar-spec-totals b)
      (if (bar? b)
          (list (sum (bar-values b)))
          (append-map bar-spec-totals (bar-group-bars b))))
    
    (define (count-bars bspecs)
      (sum (map (lambda (x) (if (bar? x) 1 (length (bar-group-bars x)))) bspecs)))
    
    (define bar-segment
      (case-lambda
        ((label color)
          (assert (string? label) (color? color))
          (list label color #f))
        ((label color textcolor)
          (assert (string? label) (color? color) (or (not textcolor) (color? textcolor)))
          (list label color textcolor))))
    
    ;; Drawing bar charts
    
    ; Returns an arrow shape starting at point `start` with the given length `len`
    (define (arrow start len angle d dbl)
      (transform-shape
        (shape
          (move-to zero-point)
          (line-to (point len 0))
          (line-to (point (- len d) (- d)))
          (move-to (point len 0))
          (line-to (point (- len d) d))
          (if dbl
              (begin
                (move-to zero-point)
                (line-to (point d (- d)))
                (move-to zero-point)
                (line-to (point d d)))))
        (rotate angle (translate (point-x start) (point-y start)))))
    
    ; Draws text centered in a rectangle
    (define (draw-text-centered str box font col)
      (let ((tsize (text-size str font (rect-size box))))
        (draw-text str
                   (point (+ (rect-x box) (/ (- (rect-width box) (size-width tsize)) 2))
                          (+ (rect-y box) (/ (- (rect-height box) (size-height tsize)) 2)))
                   font col)))
    
    ; Draws a value label
    (define (draw-vlabel y ylen yrange xlen origin config)
      (let* ((ycoord (- (point-y origin) (/ (* y ylen) yrange)))
             (label (number->string y))
             (lsize (text-size label
                               (bar-chart-value-font config)
                               (- (bar-chart-vlabel-width config)
                                  (bar-chart-left-pad config)))))
        (set-color (bar-chart-color config))
        ; Draw dashed line
        (if (pair? (bar-chart-vline-lengths config))
            (draw-dashed (line (point (point-x origin) ycoord)
                               (point (+ (point-x origin)
                                         xlen
                                         (bar-chart-xaxis-overhead config))
                                      ycoord))
                         (bar-chart-vline-lengths config)
                         0
                         (/ (bar-chart-stroke-width config) 2)))
        ; Draw value indicator
        (if (> (bar-chart-vindicator-width config) 0)
            (draw (line (point (- (point-x origin) (/ (bar-chart-vindicator-width config) 2))
                               ycoord)
                        (point (+ (point-x origin)
                                  (/ (bar-chart-vindicator-width config) 2))
                               ycoord))
                  (bar-chart-stroke-width config)))
        ; Draw value label
        (draw-text label
                   (rect (point (- (point-x origin)
                                   (bar-chart-left-pad config)
                                   (size-width lsize))
                                (- ycoord (/ (size-height lsize) 2) 0.5))
                         lsize)
                   (bar-chart-value-font config)
                   (bar-chart-color config))))
    
    ; Draws the legend
    (define (draw-legend bcolor bounds color bgcolor config)
      (let* ((lfont (legend-font config))
             (legend-size (fold-left (lambda (acc x)
                                       (let ((s (text-size (car x) lfont)))
                                         (size (max (size-width acc)
                                                    (+ (size-width s)
                                                       (legend-sample-area-width config)
                                                       (* 2 (legend-entry-pad config))))
                                               (+ (size-height acc)
                                                  (size-height s)
                                                  (legend-line-pad config)))))
                                     (size 0 (- (* 2 (legend-entry-pad config))
                                                (legend-line-pad config)))
                                     bcolor))
             (origin (cond ((and (>= (legend-horizontal-offset config) 0)
                                 (>= (legend-vertical-offset config) 0))
                             (point (legend-horizontal-offset config)
                                    (legend-vertical-offset config)))
                           ((>= (legend-horizontal-offset config) 0)
                             (point (legend-horizontal-offset config)
                                    (- (rect-height bounds)
                                       (size-height legend-size)
                                       (- (legend-vertical-offset config)))))
                           ((>= (legend-vertical-offset config) 0)
                             (point (- (rect-width bounds)
                                       (size-width legend-size)
                                       (- (legend-horizontal-offset config)))
                                    (legend-vertical-offset config)))
                           (else
                             (point (- (rect-width bounds)
                                       (size-width legend-size)
                                       (- (legend-horizontal-offset config)))
                                    (- (rect-height bounds)
                                       (size-height legend-size)
                                       (- (legend-vertical-offset config))))))))
        (set-color gray)
        (set-fill-color bgcolor)
        (set-line-width (legend-stroke-width config))
        (fill-rect (rect origin legend-size))
        (draw-rect (rect origin legend-size))
        (do ((bs bcolor (cdr bs))
             (dy (+ (point-y origin) (legend-entry-pad config))))
          ((not (pair? bs)))
          (let ((s (text-size (caar bs) lfont)))
            (set-fill-color (cadar bs))
            (fill-rect (rect (point (+ (point-x origin) (legend-entry-pad config))
                                    (+ dy
                                       (/ (size-height s) 2)
                                       (/ (legend-sample-length config) -2)))
                             (size (legend-sample-length config) (legend-sample-length config))))
            (draw-text (caar bs)
                       (point (+ (point-x origin)
                                 (legend-entry-pad config)
                                 (legend-sample-area-width config))
                              dy)
                       lfont
                       color)
            (set! dy (+ dy (size-height s) (legend-line-pad config)))))))
    
    ; Draw a bar chart
    (define (draw-bar-chart bars bcolor ystep ydescr xdescr loc config legend-config . args)
      (let-optionals args ((drawng (current-drawing)))
        (assert (every? bar-spec? bars))
        (let* ((bounds (if (point? loc) (rect loc (bar-chart-size config)) loc))
               (ys (append-map bar-spec-totals bars))
               (ymin (min 0 (minimum ys)))
               (ymax (max 0 (maximum ys)))
               (yrange (- ymax ymin))
               (n (length bars))
               (m (- (count-bars bars) n))
               (dybottom (+ (bar-chart-bottom-pad config)
                            (bar-chart-blabel-height config)
                            (bar-chart-glabel-height config)))
               (xlen (- (rect-width bounds)
                        (bar-chart-right-pad config)
                        (bar-chart-vlabel-width config)
                        (bar-chart-xaxis-overhead config)))
               (ylen (- (rect-height bounds)
                        (bar-chart-top-pad config)
                        dybottom
                        (bar-chart-yaxis-overhead config)
                        (if (not (or (zero? ymin) (zero? ymax)))
                            (bar-chart-yaxis-overhead config)
                            0)))
               (barwidth (/ (- xlen (* n (bar-chart-bar-gap config))
                               (* m (bar-chart-group-gap config)))
                            (+ n m)))
               (origin (point (bar-chart-vlabel-width config)
                              (- (rect-height bounds)
                                 dybottom
                                 (* ylen (/ (abs ymin) yrange))
                                 (if (not (or (zero? ymin) (zero? ymax)))
                                     (bar-chart-yaxis-overhead config)
                                     0)))))
          (with-drawing drawng
            (transform (translate (rect-x bounds) (rect-y bounds))
              ; Set drawing color
              (set-color (bar-chart-color config))
              ; Draw a bounding box
              ; (draw (rectangle zero-point (rect-size bounds)) 0.3)
              ; Draw value labels
              (do ((y 0 (+ y ystep)))
                  ((>= y ymax)
                   (if (>= (- (point-y origin) (/ (* y ylen) yrange))
                           (+ (bar-chart-top-pad config)
                              (/ (bar-chart-yaxis-overhead config) 2)))
                     (draw-vlabel y ylen yrange xlen origin config)))
                (draw-vlabel y ylen yrange xlen origin config))
              (do ((y (- ystep) (- y ystep)))
                  ((<= y ymin)
                   (if (<= (- (point-y origin) (/ (* y ylen) yrange))
                           (- (rect-height bounds)
                              dybottom
                              (/ (bar-chart-yaxis-overhead config) 2)))
                       (draw-vlabel y ylen yrange xlen origin config)))
                (draw-vlabel y ylen yrange xlen origin config))
              ; Draw value axis description
              (if ydescr
                  (let ((dsize (text-size
                                 ydescr
                                 (bar-chart-descr-font config)
                                 (size (+ ylen (bar-chart-yaxis-overhead config)) +inf.0))))
                    (transform
                      (rotate
                        (/ pi -2)
                        (translate 0
                          (+ (bar-chart-top-pad config)
                             (size-width dsize)
                             (/ (bar-chart-yaxis-overhead config) 3)
                             (/ (- (+ ylen (bar-chart-yaxis-overhead config))
                                   (size-width dsize))
                                2))))
                      (draw-text ydescr
                                 zero-point
                                 (bar-chart-descr-font config)
                                 (bar-chart-color config)))))
             ; Draw horizontal axis description
             (if xdescr
                 (let ((dsize (text-size
                                xdescr
                                (bar-chart-descr-font config)
                                (size (+ xlen (bar-chart-xaxis-overhead config)) +inf.0))))
                   (draw-text xdescr (point (+ (bar-chart-vlabel-width config)
                                               (/ (- (+ xlen (bar-chart-xaxis-overhead config))
                                                     (size-width dsize)) 2))
                                            (- (rect-height bounds)
                                               (size-height dsize)))
                              (bar-chart-descr-font config)
                              (bar-chart-color config))))
              ; Draw groups
              (do ((dx (+ (bar-chart-vlabel-width config)
                          (bar-chart-bar-gap config)
                          (- (bar-chart-group-gap config)))
                       (+ dx
                          (bar-chart-bar-gap config)
                          (- (bar-chart-group-gap config))))
                   (ls bars (cdr ls)))
                  ((null? ls))
                ; Draw group label
                (if (and (bar-group? (car ls)) (bar-group-label (car ls)))
                    (let* ((width (* (length (bar-group-bars (car ls)))
                                     (+ barwidth (bar-chart-group-gap config))))
                           (lsize (text-size (bar-group-label (car ls))
                                             (bar-chart-group-font config)
                                             (size width
                                                   (bar-chart-glabel-height config)))))
                      (draw-text (bar-group-label (car ls))
                                 (rect (point (+ dx
                                                 (bar-chart-group-gap config)
                                                 (/ (- width (bar-chart-group-gap config))
                                                    2)
                                                 (/ (size-width lsize) -2))
                                              (- (rect-height bounds)
                                                 (bar-chart-glabel-height config)
                                                 -5))
                                       (size width (bar-chart-glabel-height config)))
                                 (bar-chart-group-font config)
                                 (bar-chart-color config))))
                ; Draw bars
                (do ((bs (if (bar? (car ls))
                             (list (car ls))
                             (bar-group-bars (car ls))) (cdr bs)))
                    ((null? bs))
                  (set! dx (+ dx (bar-chart-group-gap config)))
                  ; Draw bar segments
                  (do ((bs (bar-values (car bs)) (cdr bs))
                       (cs (if (pair? bcolor) bcolor #f) (if (pair? cs) (cdr cs) #f))
                       (acc 0 (+ acc (car bs))))
                    ((null? bs)
                     ; Draw bar value
                     (if (bar-chart-bar-font config)
                         (let* ((vsize (text-size (number->string acc)
                                                  (bar-chart-bar-font config)
                                                  (size (+ (bar-chart-bar-gap config)
                                                           barwidth)
                                                        +inf.0)))
                                (voffset (if (< acc 0)
                                             (- (bar-chart-value-pad config))
                                             (+ (bar-chart-value-pad config)
                                                (size-height vsize)))))
                           (draw-text (number->string acc)
                                      (rect (point (+ dx
                                                      (/ barwidth 2)
                                                      (/ (size-width vsize) -2))
                                                   (- (point-y origin)
                                                      (/ (* acc ylen) yrange)
                                                      voffset))
                                            vsize)
                                      (bar-chart-bar-font config)
                                      (bar-chart-color config)))))
                    (let* ((oldcoord (- (point-y origin) (/ (* acc ylen) yrange)))
                           (ycoord (- (point-y origin) (/ (* (+ acc (car bs)) ylen) yrange)))
                           (box (rect (point dx ycoord) (size barwidth (- oldcoord ycoord)))))
                      (if (and (pair? cs) (pair? (car cs)) (color? (cadar cs)))
                          (set-fill-color (cadar cs))
                          (if cs
                              (error "incorrect segment color specification: $0" cs)
                              (set-fill-color bcolor)))
                      (fill-rect box)
                      (if (and cs (pair? (cddar cs)) (color? (caddar cs)))
                          (draw-text-centered (number->string (car bs))
                                              box
                                              (bar-chart-bar-font config)
                                              (caddar cs)))))
                  ; Draw bar label
                  (if (bar-label (car bs))
                      (let ((lsize (text-size (bar-label (car bs))
                                              (bar-chart-label-font config)
                                              (size (+ (bar-chart-bar-gap config) barwidth)
                                                    (bar-chart-blabel-height config)))))
                        (draw-text (bar-label (car bs))
                                   (rect (point (+ dx
                                                   (/ barwidth 2)
                                                   (/ (size-width lsize) -2))
                                                (- (rect-height bounds)
                                                   (bar-chart-blabel-height config)
                                                   (bar-chart-glabel-height config)))
                                         lsize)
                                   (bar-chart-label-font config)
                                   (bar-chart-color config))))
                  (set! dx (+ dx barwidth))))
              ; Draw x-axis
              (set-color (bar-chart-color config))
              (draw (arrow origin (+ xlen (bar-chart-xaxis-overhead config)) 0 5 #f)
                    (bar-chart-stroke-width config))
              ; Draw y-axis
              (cond ((zero? ymin)
                      (draw (arrow origin
                                   (+ ylen (bar-chart-yaxis-overhead config))
                                   (- (/ pi 2)) 5 #f)
                            (bar-chart-stroke-width config)))
                    ((zero? ymax)
                      (draw (arrow origin
                                   (+ ylen (bar-chart-yaxis-overhead config))
                                   (/ pi 2) 5 #f)
                            (bar-chart-stroke-width config)))
                    (else
                      (draw (arrow (point (bar-chart-vlabel-width config)
                                          (- (rect-height bounds)
                                             dybottom))
                                   (+ ylen (* 2 (bar-chart-yaxis-overhead config)))
                                   (- (/ pi 2))
                                   5
                                   #t)
                            (bar-chart-stroke-width config))))
              ; Draw legend
              (if (and (legend-config? legend-config) (pair? bcolor))
                  (draw-legend bcolor
                               bounds
                               (bar-chart-color config)
                               (bar-chart-bg-color config)
                               legend-config)))))))
  )
)
