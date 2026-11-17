# LispKit Draw Chart Line

Library `(lispkit draw chart line)` supports drawing line charts and area charts, including stacked line charts and stacked area charts, based on a data-driven API in which _line charts_ are being described declaratively. A drawing procedure is then able to render a chart into a given drawing as defined by library `(lispkit draw)`.


## Overview

A line chart is set up and drawn in a few steps: the data is described by _series_ objects together with a list of x values, the appearance of the chart by a line chart configuration, and an optional legend by a legend configuration. Procedure `draw-line-chart` then renders everything into a drawing, which can be saved, e.g. as a PDF file. The following code draws a line chart with two series and a legend:

```scheme
(import (lispkit base)
        (lispkit draw)
        (lispkit draw chart line))

(define d (make-drawing))
(draw-line-chart
  (list (line-series "Berlin" '(0 1 5 9 14 17 19 19 15 10 5 1))
        (line-series "Oslo" '(-4 -4 0 5 11 15 18 17 12 7 2 -2)))
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")     ; x values
  5                                          ; y axis step
  "Month"                                    ; x axis description
  "Temperature [C°]"                         ; y axis description
  (point 50 30)                              ; location
  (make-line-chart-config
    'size: (size 495 300)
    'marker: 'circle)                        ; chart configuration
  (make-line-legend-config
    'horizontal-offset: 70)                  ; legend configuration
  d)                                         ; target drawing
(save-drawing "temperature.pdf" d (size 600 380))
```

Setting `area?` to `#t` in the chart configuration fills the area below each line, `stacked?` stacks the series on top of each other, and `smooth?` draws smooth curves instead of straight lines. If the x values are numbers, they are placed proportionally on a numeric x axis. The following code draws a smooth stacked area chart over the years 2015 to 2020. The right padding of the chart makes room for a legend placed to the right of the plot area. The legend lists the series in the order of the stack, with the topmost series first:

```scheme
(define d (make-drawing))
(draw-line-chart
  (list (line-series "Coal" '(180 172 165 158 140 120))
        (line-series "Gas" '(120 126 130 136 142 146))
        (line-series "Wind" '(30 38 46 55 66 78)))
  '(2015 2016 2017 2018 2019 2020)           ; numeric x values
  100                                        ; y axis step
  #f                                         ; no x axis description
  "TWh"                                      ; y axis description
  (point 50 30)                              ; location
  (make-line-chart-config
    'area?: #t
    'stacked?: #t
    'smooth?: #t
    'x-step: 1
    'right-pad: 80)                          ; chart configuration
  (make-line-legend-config
    'horizontal-offset: -5)                  ; legend configuration
  d)                                         ; target drawing
(save-drawing "generation.pdf" d (size 600 380))
```


## Line Chart Model

The line chart model on which library `(lispkit draw chart line)` is based on consists of the following components:

- A line chart is defined by a list of _series_ and a list of _x values_.
- Each _series_ consists of a list of values, one for each x value. It has an optional label, an optional color, an optional line width, optional dash lengths and an optional marker. A value can be missing, in which case it is represented by `#f`.
- The _x values_ define the positions of the data points on the x axis. If the x values are strings (or `#f`), the x axis is a _category axis_: data points are evenly spaced, and the x values are used as labels. `#f` stands for an unlabeled position. If the x values are numbers, the x axis is a _numeric axis_: they have to be strictly increasing, and data points are placed proportionally to their value. Tick marks and labels are placed at multiples of a step size, which is chosen automatically unless it is specified by the chart configuration.
- Series without a color get a color from the _palette_ of the chart configuration (the n-th series gets the n-th color, restarting with the first color if there are more series than colors in the palette).
- The range of the y axis is determined from the data: it covers all values, including `0` for area charts and stacked charts, and it is extended to multiples of the _step_ between tick marks. The step is either provided explicitly, or it is chosen automatically. The chart configuration can override the minimum and maximum of the range.
- The data points of a series are connected by a _line_. Optionally, the lines are _smoothed_, in which case they are drawn as monotone cubic curves that never overshoot the data points. A _marker_ can highlight each data point.
- In an _area chart_, the area below each line is filled with the color of the series, down to the zero line (or to the bottom of the plot area if zero is not part of the range of the y axis). Areas are translucent by default if they can overlap.
- In a _stacked chart_, the values of the series are added up: the line of the first series shows its values, the line of the second series shows the sum of the first two series, and so on. In an area chart, each series fills the area between its line and the line of the series below. A negative value reduces the sum.
- A _missing value_ creates a gap in the line and the area of a series, and gets no marker. Isolated values between two gaps are only shown via a marker. In stacked charts, missing values are treated like `0`.
- The _values_ of a series can be shown next to their data points. In stacked area charts, values are shown within the areas, and are omitted if the area is too small.
- _Grid lines_ at the tick marks of the axes help with reading values.
- A _legend_ shows the labels of the series and the colors used to draw them. Series without a label are not part of the legend.
- A _legend configuration_ specifies how the legend is layed out.
- A _line chart configuration_ specifies how the chart overall is being drawn. This includes all fonts, colors, padding values, whether it is an area and/or stacked chart, the markers, etc.

The chart layout is structured as follows (from left to right): left padding, y axis description, y axis label area, plot area, overhead of the x axis, right padding. From top to bottom: top padding, overhead of the y axis, plot area, x axis label area, x axis description, bottom padding. Series are clipped at the plot area; if the range of an axis is determined automatically, there is a small margin so that markers and lines at the border of the plot area are fully visible. The x axis is drawn at the zero line of the y axis if it is part of the range of the y axis; otherwise it is drawn at the border of the plot area. Its labels are always shown below the plot area. Labels that would overlap are omitted. The legend is drawn on top of the chart at a position defined by the legend configuration relative to the bounds of the chart. It does not reduce the space available for the plot area. Padding values can be used to reserve space for it.


## Series

A _series_ bundles a list of values with an optional label, an optional color, and optional properties for drawing its line. Series are immutable objects created via procedure `line-series`.

**(line-series _label values_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(line-series _label values color_)**  
**(line-series _label values color width_)**  
**(line-series _label values color width lengths_)**  
**(line-series _label values color width lengths marker_)**  

Creates a new series. _label_ is a string identifying the series in the legend. If _label_ is `#f`, the series does not show up in the legend. _values_ is a list of real numbers, one for each x value of the chart in which the series is drawn. `#f` represents a missing value. _color_ is the color used to draw the series; if it is `#f` or omitted, the color is taken from the palette of the line chart configuration. _width_ is the width of the line in points; if it is `#f` or omitted, the line width of the line chart configuration is used. `0` hides the line. _lengths_ is an alternating list of positive dash/space lengths for drawing a dashed line; `#f` or omitting it results in a solid line. _marker_ is one of the symbols `none`, `circle`, `square`, `diamond`, and `triangle` and defines the shape of the markers shown at the data points of the series; if it is `#f` or omitted, the marker of the line chart configuration is used. It is an error if one of the arguments is invalid.

```scheme
(line-series "Berlin" '(0 1 5 9 14 17))
(line-series "Oslo" '(-4 -4 0 5 #f 15) (color "4E79A7"))
(line-series "Rome" '(8 9 12 15 19 23) red 1.5 '(4 2) 'square)
(line-series #f '(5 5 5 5 5 5) gray 1)
```

**(line-series? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a series, otherwise `#f` is returned.

**(line-series-label _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the label of the given series _s_, or `#f` if the series does not have a label.

**(line-series-values _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the list of values of the given series _s_.

**(line-series-color _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the color of the given series _s_, or `#f` if the color is taken from the palette of the line chart configuration.

**(line-series-width _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the line width of the given series _s_, or `#f` if the line width is taken from the line chart configuration.

**(line-series-lengths _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the list of alternating dash/space lengths of the given series _s_, or `#f` if the line is solid.

**(line-series-marker _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the marker of the given series _s_, or `#f` if the marker is taken from the line chart configuration.


## Legend Configurations

A _legend configuration_ is a record encapsulating all parameters needed for drawing a legend in a chart. Legend configurations are defined independently by library `(lispkit draw chart)`. But there is a specialized `make-line-legend-config` procedure which provides defaults specifically for usage with line charts.

**(make-line-legend-config _key val ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new legend configuration object from the provided keyword/value pairs. The following keyword arguments are supported. The default value is provided in parenthesis.

   - `font:` Font used for all text in a legend (Helvetica 10).
   - `color:` Color used for legend labels (black).
   - `bg-color:` Background color of the legend's bounding box (white).
   - `stroke-width:` Width of a stroke for drawing the bounding box of the legend (0.5).
   - `corner-radius:` Corner radius for a rounded bounding box; `0` results in a rectangular bounding box with square corners (0).
   - `horizontal-offset:` Horizontal offset from chart bounds. A non-negative value is interpreted as an offset from the left bound; a negative value is interpreted as an offset from the right bound (-10).
   - `vertical-offset:` Vertical offset from chart bounds. A non-negative value is interpreted as an offset from the top bound; a negative value is interpreted as an offset from the bottom bound (10).
   - `sample-area-width:` Width of the area reserved for the color sample box in front of each legend label (17).
   - `sample-length:` Height and width of the color sample box shown for each entry of the legend (10).
   - `line-pad:` Padding between two consecutive lines/entries of the legend (3).
   - `entry-pad:` Top/bottom and left/right padding between the legend entries and the bounding box of the legend (6).

With the default offsets, the legend is shown in the top right corner of the chart. Since the legend is drawn on top of the chart, it can cover parts of the plot area. Either choose offsets that place the legend where there is no data, or use the padding of the line chart configuration (e.g. `right-pad:`) to reserve space for the legend next to the plot area.

```scheme
(make-line-legend-config
  'font: (font "Helvetica" 9)
  'stroke-width: 0.4
  'corner-radius: 4
  'horizontal-offset: 70
  'vertical-offset: 12)
```

Accessor procedures for legend configuration objects are provided by library `(lispkit draw chart)`.


## Line Chart Configurations

A _line chart configuration_ is a record encapsulating all parameters needed for drawing a line chart or area chart (excluding the legend). Line chart configurations are mutable objects that are created via procedure `make-line-chart-config`. For every parameter of the configuration, there is an accessor and a setter procedure.

**(make-line-chart-config _key val ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new line chart configuration object from the provided keyword/value pairs. The following keyword arguments are supported. The default value is provided in parenthesis.

   - `size:` Size of the rectangle in which the chart is drawn (500 × 300).
   - `color:` Color of axes, tick marks, and text (black).
   - `bg-color:` Background color of the plot area or `#f` for a transparent background. The color is also the fallback for the background color of the legend and for the outline of markers; both are white if `bg-color` is `#f` (`#f`).
   - `palette:` List of colors for series that do not define a color (the ten colors `4E79A7`, `F28E2B`, `E15759`, `76B7B2`, `59A14F`, `EDC948`, `B07AA1`, `FF9DA7`, `9C755F`, `BAB0AC`).
   - `value-font:` Font for the labels of the y axis; `#f` disables the labels (Helvetica 10).
   - `label-font:` Font for the labels of the x axis; `#f` disables the labels (Helvetica 10).
   - `descr-font:` Font for the descriptions of the axes (Helvetica-LightOblique 10).
   - `point-font:` Font for the values drawn next to data points; `#f` disables values (`#f`).
   - `value-format:` Defines how numbers are shown, both on the tick labels of the axes and next to data points (`#f`). Supported are `#f`, in which case numbers are rounded to at most four decimal places and shown without trailing zeros, and procedures. A procedure is called with a number and returns the string to show.
   - `stroke-width:` Width of the strokes for axes and tick marks in points. Grid lines use half of this width (1).
   - `line-width:` Default width in points of the lines of series (2).
   - `marker:` Default marker shown at data points of series without a marker. Supported are `none`, `circle`, `square`, `diamond`, and `triangle` (`none`).
   - `marker-size:` Size of markers in points (7).
   - `smooth?:` If `#t`, lines are drawn as smooth monotone curves; if `#f`, data points are connected via straight lines (`#f`).
   - `area?:` If `#t`, the areas below the lines are filled (`#f`).
   - `stacked?:` If `#t`, the series are stacked (`#f`).
   - `area-opacity:` Opacity of filled areas, a number between `0` and `1`. If it is `#f`, areas of stacked charts are opaque, and areas of charts that are not stacked have an opacity of `0.35` so that overlapping areas remain visible (`#f`).
   - `grid-line-lengths:` Alternating list of dash/space lengths for drawing grid lines; `#f` disables grid lines ((1 3)).
   - `vertical-grid?:` If `#t`, vertical grid lines are drawn in addition to the horizontal ones (`#f`).
   - `top-pad:` Top padding in points (20).
   - `bottom-pad:` Bottom padding in points (5).
   - `left-pad:` Left padding in points (10).
   - `right-pad:` Right padding in points (15).
   - `vlabel-width:` Width in points reserved for the labels of the y axis (40).
   - `blabel-height:` Height in points reserved for the labels of the x axis (18).
   - `tick-length:` Length of tick marks in points (4).
   - `xaxis-overhead:` Length in points by which the x axis extends beyond the plot area (15).
   - `yaxis-overhead:` Length in points by which the y axis extends beyond the plot area (15).
   - `edge-pad:` Space in points between the y axis and the first data point as well as between the last data point and the right border of the plot area. If it is `#f`, the space is `0` for area charts, and half of the distance between two data points (at most 20 points) for line charts (`#f`).
   - `value-pad:` Padding in points between data points and the values drawn next to them (4).
   - `x-step:` Step between the tick marks of a numeric x axis; `#f` chooses a step automatically (`#f`).
   - `x-min:` Minimum of a numeric x axis; `#f` uses the first x value (`#f`).
   - `x-max:` Maximum of a numeric x axis; `#f` uses the last x value (`#f`).
   - `y-min:` Minimum of the y axis; `#f` determines it from the data (`#f`).
   - `y-max:` Maximum of the y axis; `#f` determines it from the data (`#f`).

```scheme
(make-line-chart-config
  'size: (size 495 300)
  'area?: #t
  'smooth?: #t
  'marker: 'circle
  'point-font: (font "Helvetica" 9)
  'value-format: (lambda (v) (string-append (number->string v) "%"))
  'y-min: 0
  'y-max: 100)
```

**(line-chart-config? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a line chart configuration, otherwise `#f` is returned.

**(line-chart-size _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the size of the rectangle in which the chart is drawn as defined by the given line chart configuration _cconf_.

**(line-chart-size-set! _cconf size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the size of the rectangle in which the chart is drawn for the given line chart configuration _cconf_ to _size_. _size_ is a size object.

**(line-chart-color _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the color of axes, ticks, and text as defined by the given line chart configuration _cconf_.

**(line-chart-color-set! _cconf color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the color of axes, ticks, and text for the given line chart configuration _cconf_ to _color_.

**(line-chart-bg-color _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the background color of the plot area, or `#f` for a transparent background as defined by the given line chart configuration _cconf_.

**(line-chart-bg-color-set! _cconf color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the background color of the plot area, or `#f` for a transparent background for the given line chart configuration _cconf_ to _color_. _color_ is a color or `#f`.

**(line-chart-palette _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the palette, i.e. the list of colors for series that do not define a color as defined by the given line chart configuration _cconf_.

**(line-chart-palette-set! _cconf colors_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the palette, i.e. the list of colors for series that do not define a color for the given line chart configuration _cconf_ to _colors_. _colors_ is a list of colors.

**(line-chart-value-font _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the font for the labels of the y axis, or `#f` if there are no labels as defined by the given line chart configuration _cconf_.

**(line-chart-value-font-set! _cconf font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the font for the labels of the y axis, or `#f` if there are no labels for the given line chart configuration _cconf_ to _font_.

**(line-chart-label-font _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the font for the labels of the x axis, or `#f` if there are no labels as defined by the given line chart configuration _cconf_.

**(line-chart-label-font-set! _cconf font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the font for the labels of the x axis, or `#f` if there are no labels for the given line chart configuration _cconf_ to _font_.

**(line-chart-descr-font _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the font for the descriptions of the axes as defined by the given line chart configuration _cconf_.

**(line-chart-descr-font-set! _cconf font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the font for the descriptions of the axes for the given line chart configuration _cconf_ to _font_.

**(line-chart-point-font _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the font for the values drawn next to data points, or `#f` if no values are shown as defined by the given line chart configuration _cconf_.

**(line-chart-point-font-set! _cconf font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the font for the values drawn next to data points, or `#f` if no values are shown for the given line chart configuration _cconf_ to _font_.

**(line-chart-value-format _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the format for numbers, i.e. `#f` or a procedure mapping numbers to strings as defined by the given line chart configuration _cconf_.

**(line-chart-value-format-set! _cconf format_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the format for numbers, i.e. `#f` or a procedure mapping numbers to strings for the given line chart configuration _cconf_ to _format_.

**(line-chart-stroke-width _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the width of strokes for axes and tick marks in points as defined by the given line chart configuration _cconf_.

**(line-chart-stroke-width-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the width of strokes for axes and tick marks in points for the given line chart configuration _cconf_ to _val_.

**(line-chart-line-width _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the default width in points of the lines of series as defined by the given line chart configuration _cconf_.

**(line-chart-line-width-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the default width in points of the lines of series for the given line chart configuration _cconf_ to _val_.

**(line-chart-marker _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the default marker shape of series, one of `none`, `circle`, `square`, `diamond`, and `triangle` as defined by the given line chart configuration _cconf_.

**(line-chart-marker-set! _cconf marker_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the default marker shape of series, one of `none`, `circle`, `square`, `diamond`, and `triangle` for the given line chart configuration _cconf_ to _marker_.

**(line-chart-marker-size _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the size of markers in points as defined by the given line chart configuration _cconf_.

**(line-chart-marker-size-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the size of markers in points for the given line chart configuration _cconf_ to _val_.

**(line-chart-smooth? _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the flag specifying whether lines are drawn as smooth curves as defined by the given line chart configuration _cconf_.

**(line-chart-smooth-set! _cconf flag_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the flag specifying whether lines are drawn as smooth curves for the given line chart configuration _cconf_ to _flag_.

**(line-chart-area? _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the flag specifying whether the areas below lines are filled as defined by the given line chart configuration _cconf_.

**(line-chart-area-set! _cconf flag_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the flag specifying whether the areas below lines are filled for the given line chart configuration _cconf_ to _flag_.

**(line-chart-stacked? _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the flag specifying whether series are stacked as defined by the given line chart configuration _cconf_.

**(line-chart-stacked-set! _cconf flag_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the flag specifying whether series are stacked for the given line chart configuration _cconf_ to _flag_.

**(line-chart-area-opacity _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the opacity of filled areas, or `#f` if the opacity is chosen automatically as defined by the given line chart configuration _cconf_.

**(line-chart-area-opacity-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the opacity of filled areas, or `#f` if the opacity is chosen automatically for the given line chart configuration _cconf_ to _val_. _val_ is a number between `0` and `1` or `#f`.

**(line-chart-grid-line-lengths _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the alternating list of dash/space lengths for drawing grid lines, or `#f` if no grid lines are drawn as defined by the given line chart configuration _cconf_.

**(line-chart-grid-line-lengths-set! _cconf lengths_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the alternating list of dash/space lengths for drawing grid lines, or `#f` if no grid lines are drawn for the given line chart configuration _cconf_ to _lengths_.

**(line-chart-vertical-grid? _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the flag specifying whether vertical grid lines are drawn in addition to horizontal ones as defined by the given line chart configuration _cconf_.

**(line-chart-vertical-grid-set! _cconf flag_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the flag specifying whether vertical grid lines are drawn in addition to horizontal ones for the given line chart configuration _cconf_ to _flag_.

**(line-chart-top-pad _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the top padding in points as defined by the given line chart configuration _cconf_.

**(line-chart-top-pad-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the top padding in points for the given line chart configuration _cconf_ to _val_.

**(line-chart-bottom-pad _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the bottom padding in points as defined by the given line chart configuration _cconf_.

**(line-chart-bottom-pad-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the bottom padding in points for the given line chart configuration _cconf_ to _val_.

**(line-chart-left-pad _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the left padding in points as defined by the given line chart configuration _cconf_.

**(line-chart-left-pad-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the left padding in points for the given line chart configuration _cconf_ to _val_.

**(line-chart-right-pad _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the right padding in points as defined by the given line chart configuration _cconf_.

**(line-chart-right-pad-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the right padding in points for the given line chart configuration _cconf_ to _val_.

**(line-chart-vlabel-width _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the width in points reserved for the labels of the y axis as defined by the given line chart configuration _cconf_.

**(line-chart-vlabel-width-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the width in points reserved for the labels of the y axis for the given line chart configuration _cconf_ to _val_.

**(line-chart-blabel-height _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the height in points reserved for the labels of the x axis as defined by the given line chart configuration _cconf_.

**(line-chart-blabel-height-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the height in points reserved for the labels of the x axis for the given line chart configuration _cconf_ to _val_.

**(line-chart-tick-length _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the length of tick marks in points as defined by the given line chart configuration _cconf_.

**(line-chart-tick-length-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the length of tick marks in points for the given line chart configuration _cconf_ to _val_.

**(line-chart-xaxis-overhead _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the length in points by which the x axis extends beyond the plot area as defined by the given line chart configuration _cconf_.

**(line-chart-xaxis-overhead-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the length in points by which the x axis extends beyond the plot area for the given line chart configuration _cconf_ to _val_.

**(line-chart-yaxis-overhead _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the length in points by which the y axis extends beyond the plot area as defined by the given line chart configuration _cconf_.

**(line-chart-yaxis-overhead-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the length in points by which the y axis extends beyond the plot area for the given line chart configuration _cconf_ to _val_.

**(line-chart-edge-pad _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the space in points between the axes and the first and last data point, or `#f` if the space is chosen automatically as defined by the given line chart configuration _cconf_.

**(line-chart-edge-pad-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the space in points between the axes and the first and last data point, or `#f` if the space is chosen automatically for the given line chart configuration _cconf_ to _val_.

**(line-chart-value-pad _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the padding in points between data points and the values drawn next to them as defined by the given line chart configuration _cconf_.

**(line-chart-value-pad-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the padding in points between data points and the values drawn next to them for the given line chart configuration _cconf_ to _val_.

**(line-chart-x-step _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the step between the tick marks of a numeric x axis, or `#f` if the step is chosen automatically as defined by the given line chart configuration _cconf_.

**(line-chart-x-step-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the step between the tick marks of a numeric x axis, or `#f` if the step is chosen automatically for the given line chart configuration _cconf_ to _val_.

**(line-chart-x-min _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the minimum of a numeric x axis, or `#f` if it is the first x value as defined by the given line chart configuration _cconf_.

**(line-chart-x-min-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the minimum of a numeric x axis, or `#f` if it is the first x value for the given line chart configuration _cconf_ to _val_.

**(line-chart-x-max _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the maximum of a numeric x axis, or `#f` if it is the last x value as defined by the given line chart configuration _cconf_.

**(line-chart-x-max-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the maximum of a numeric x axis, or `#f` if it is the last x value for the given line chart configuration _cconf_ to _val_.

**(line-chart-y-min _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the minimum of the y axis, or `#f` if it is determined from the data as defined by the given line chart configuration _cconf_.

**(line-chart-y-min-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the minimum of the y axis, or `#f` if it is determined from the data for the given line chart configuration _cconf_ to _val_.

**(line-chart-y-max _cconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the maximum of the y axis, or `#f` if it is determined from the data as defined by the given line chart configuration _cconf_.

**(line-chart-y-max-set! _cconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the maximum of the y axis, or `#f` if it is determined from the data for the given line chart configuration _cconf_ to _val_.


## Drawing Line Charts

**(draw-line-chart _series xs ystep xdescr ydescr loc config legend_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(draw-line-chart _series xs ystep xdescr ydescr loc config legend drawing_)**  

Draws a line chart or an area chart into the drawing _drawing_. If _drawing_ is not provided, the drawing provided by the `current-drawing` parameter object of library `(lispkit draw)` is used.

_series_ is a non-empty list of series as created by `line-series`. In an area chart, series are drawn in the order of the list, i.e. later series are drawn on top of earlier ones. In a stacked chart, the first series is drawn at the bottom of the stack. _xs_ is a non-empty list of x values. Either all x values are strings or `#f`, which makes the x axis a category axis with evenly spaced positions, or all x values are numbers in strictly increasing order, which makes the x axis a numeric axis. Every series needs to have exactly as many values as there are x values.

_ystep_ is the step between the tick marks of the y axis. If it is `#f`, a step is chosen automatically. _xdescr_ and _ydescr_ are strings with descriptions of the x and y axis, or `#f` if there is no description. The description of the y axis is drawn rotated at the left side of the chart. _loc_ is either a point specifying the top-left corner at which the chart is placed within the drawing, in which case the size of the chart is defined by the configuration, or a rectangle defining the bounds of the chart. _config_ is a line chart configuration object as created by `make-line-chart-config`. _legend_ is either a legend configuration (as created by `make-line-legend-config`) or `#f`. If `#f`, no legend is drawn. If a legend configuration is provided, a legend box is drawn showing the label and color for each series that has a label. For stacked charts, the legend lists the series in reverse order, matching the order in which they appear from top to bottom in the chart.

An error is signaled if the arguments are invalid, if a step would result in too many tick marks, if the range of the y axis is empty, or if the chart is too small for showing the plot area together with its labels, descriptions, and padding.

The following chart shows a stacked line chart with a value format, markers, and the values of the series next to their data points. The value of the second series for the third quarter is missing, which is treated as `0`:

```scheme
(define sales
  (list (line-series "Americas" '(12 14 13 17 18 21) #f #f #f 'circle)
        (line-series "EMEA" '(8 9 #f 11 12 14) #f #f #f 'square)
        (line-series "APAC" '(4 6 7 9 11 13) #f #f #f 'diamond)))
(define d (make-drawing))
(draw-line-chart
  sales
  '("Q1 2025" "Q2 2025" "Q3 2025" "Q4 2025" "Q1 2026" "Q2 2026")
  10 #f "Sales"
  (point 50 30)
  (make-line-chart-config
    'size: (size 495 300)
    'stacked?: #t
    'point-font: (font "Helvetica-Bold" 9)
    'value-format: (lambda (v) (string-append (number->string v) "M"))
    'y-min: 0)
  (make-line-legend-config 'horizontal-offset: 70)
  d)
(save-drawing "sales.pdf" d (size 600 380))
```

The `draw-line-chart` procedure can also be used within the `drawing` syntax form, in which case the _drawing_ argument can be omitted and the chart is drawn into the `current-drawing`:

```scheme
(define my-chart
  (drawing
    (draw-line-chart
      (list (line-series "Sensor A" '(2 5 4 6 7 9))
            (line-series "Sensor B" '(1 2 6 7 6 4)))
      '(0 1 2 3 4 5)
      #f "Time [s]" "Signal"
      (point 50 30)
      (make-line-chart-config 'area?: #t 'smooth?: #t)
      #f)))
(save-drawing "sensors.pdf" my-chart (size 600 380))
```
