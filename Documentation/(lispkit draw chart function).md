# LispKit Draw Chart Function

Library `(lispkit draw chart function)` supports drawing function plots based on a data-driven API in which _function charts_ are being described declaratively. A drawing procedure is then able to render one or more mathematical functions into a given drawing.


## Overview

A function chart is set up and drawn in a few steps: the functions are described by _function graphs_, the appearance of the chart by a function chart configuration, and an optional legend by a function legend configuration. Procedure `draw-function-chart` then renders everything into a drawing, which can be saved, e.g. as a PDF file. The following code draws two functions into a chart with a legend:

```scheme
(import (lispkit base)
        (lispkit draw)
        (lispkit draw chart function))
(define d (make-drawing))
(draw-function-chart
  (list (function-graph sin "sin(x)" blue)
        (function-graph cos "cos(x)" red))
  -6.3 6.3                                   ; x range
  -1.5 1.5                                   ; y range
   2.0 0.5                                   ; x and y-axis tick step
  "x" "y"                                    ; x and y-axis description
  (point 50 30)                              ; location
  (make-function-chart-config
    'size: (size 500 300)
    'samples: 200)                           ; chart configuration
  (make-function-legend-config)              ; legend configuration
  d)                                         ; target drawing
(save-drawing "functions.pdf" d (size 600 380))
```

A single function can also be plotted without a legend:

```scheme
(define d (make-drawing))
(draw-function-chart
  (function-graph (lambda (x) (* x x)) "x²" red)
  -5.0 5.0                                   ; x range
  -2.0 25.0                                  ; y range
  1.0                                        ; x-axis tick step
  5.0                                        ; y-axis tick step
  "x"                                        ; x-axis description
  "f(x)"                                     ; y-axis description
  (point 10 15)                              ; location
  (make-function-chart-config
    'box-color: #f
    'size: (size 400 250))                   ; chart configuration
  #f                                         ; no legend
  d)                                         ; target drawing
(save-drawing "parabola.pdf" d (size 420 270))
```


## Function Chart Model

The function chart model on which library `(lispkit draw chart function)` is based on consists of the following components:

- A function chart is defined by one or more _function graphs_.
- Each _function graph_ consists of a Scheme procedure (accepting and returning a single number), an optional label string, a color, a line width in points, as well as a list of floating-point numbers specifying an alternating list of dash/space lengths.
- The _domain_ of the chart is specified by a minimum and maximum x value. The _range_ is specified by a minimum and maximum y value.
- _Tick marks_ are placed along both axes at regular intervals defined by step sizes.
- Optional _grid lines_ (dashed) are drawn at each tick position to aid readability.
- The _axes_ are drawn at x = 0 and y = 0 when the origin falls within the visible area; otherwise they are drawn at the boundary of the plot area.
- A _legend_ shows the function labels and the colors used to draw the corresponding curves.
- A _legend configuration_ specifies how the legend is layed out.
- A _function chart configuration_ specifies how the chart overall is being drawn. This includes all fonts, colors, padding values, stroke widths, grid line styles, the number of samples, etc.

The chart layout is structured as follows (from left to right): left padding, y-axis label area, plot area, right padding. From top to bottom: top padding, plot area, x-axis label area, bottom padding. Function curves are clipped to the plot area so that extreme values do not overflow into the surrounding labels or padding.


## Function Graphs

A _function graph_ bundles a Scheme procedure, an optional label, a color, a line width in points, as well as a list of floating-point numbers specifying an alternating list of dash/space lengths. Function graphs are immutable objects created via procedure `function-graph`.

**(function-graph _proc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(function-graph _proc label_)**  
**(function-graph _proc label color_)**  
**(function-graph _proc label color width_)**  
**(function-graph _proc label color width lengths_)**  

Creates a new function graph. _proc_ is a procedure accepting a single numeric argument and returning a numeric result. _label_ is an optional string used to identify the function in the legend; if `#f` or omitted, the function will not appear in the legend. _color_ is an optional color used to draw the curve; it defaults to `blue` if omitted. _width_ is the line width in points (default is `#f`). _lengths_ is a list of floating-point numbers specifying an alternating list of dash/space lengths (default is `#f`).

```scheme
(function-graph sin)
(function-graph sin "sin(x)")
(function-graph sin "sin(x)" blue)
(function-graph (lambda (x) (* x x)) "x²" red)
```

**(vertical-line-graph _x_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(vertical-line-graph _x label_)**  
**(vertical-line-graph _x label color_)**  
**(vertical-line-graph _x label color width_)**  
**(vertical-line-graph _x label color width lengths_)**  

Returns a new function graph object representing a vertical line at coordinate _x_. _label_ is an optional string used to identify the vertical line; if `#f` or omitted, the vertical line will not appear in the legend. _color_ is an optional color used to draw the line; it defaults to `blue` if omitted. _width_ is the line width in points (default is `#f`). _lengths_ is a list of floating-point numbers specifying an alternating list of dash/space lengths (default is `#f`).

**(inline-label-graph _loc label_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(inline-label-graph _loc label color_)**  
**(inline-label-graph _loc label color font_)**  

Returns a new function graph object representing an inline label at point _loc_.  _label_ is an optional string used to identify the vertical line; if `#f` or omitted, the vertical line will not appear in the legend. _color_ is an optional color used to draw the inline label; it defaults to `blue` if omitted. _font_ is the font used to draw the inline label; "Helvetica" in size 8 is used as a default.

**(function-graph? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a function graph, otherwise `#f` is returned.

**(function-graph-procedure _f_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the procedure of the given function graph _f_.

**(function-graph-label _f_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the label string of the given function graph _f_, or `#f` if no label was provided.

**(function-graph-color _f_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the color of the given function graph _f_.

**(function-graph-line-width _f_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the line width of the given function graph _f_.

**(function-graph-line-lengths _f_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of floating-point numbers specifying an alternating list of dash/space lengths for the given function graph _f_.


## Legend Configurations

A _legend configuration_ is a record encapsulating all parameters needed for drawing a legend in a chart. Legend configurations are defined independently by library `(lispkit draw chart)`. But there is a specialized `make-function-legend-config` procedure which provides defaults specifically for usage with function charts.

**(make-function-legend-config _key val ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new legend configuration object from the provided keyword/value pairs. The following keyword arguments are supported. The default value is provided in parenthesis.

   - `font:` Font used for all text in a legend (Helvetica 9).
   - `stroke-width:` Width of a stroke for drawing the bounding box of the legend (0.5).
   - `horizontal-offset:` Horizontal offset from chart bounds. Positive values are offsets from the left bound; negative values are offsets from the right bound (-10).
   - `vertical-offset:` Vertical offset from chart bounds (10).
   - `sample-length:` Length of the colored line sample drawn next to each label (20).
   - `line-pad:` Vertical padding between legend entries (3).
   - `entry-pad:` Padding around legend entries on all sides (6).

```scheme
(make-function-legend-config
  'font: (font "Helvetica" 8)
  'stroke-width: 0.4
  'horizontal-offset: -15
  'vertical-offset: 15
  'sample-length: 18
  'entry-pad: 5
  'line-pad: 3)
```

Accessor procedures for legend configuration objects are provided by library `(lispkit draw chart)`.


## Function Chart Configurations

A _function chart configuration_ is a record encapsulating all parameters needed for drawing a function chart (excluding the legend). Function chart configurations are mutable objects that are created via procedure `make-function-chart-config`. For every parameter of the configuration, there is an accessor and a setter procedure.

**(make-function-chart-config _key val ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new function chart configuration object from the provided keyword/value pairs. The following keyword arguments are supported. The default value is provided in parenthesis.

   - `size:` Size of the rectangle in which the chart is drawn (500 × 300).
   - `color:` Color of text, axes, and ticks (black).
   - `bg-color:` Background color of the plot area (white).
   - `box-color:` Color of the plot border box or `#f` to disable the box (grey).
   - `axis-font:` Font for tick labels on both axes (Helvetica 9).
   - `label-font:` Font for axis name labels (Helvetica 10).
   - `descr-font:` Font for axis description text (Helvetica-LightOblique 9).
   - `stroke-width:` Width of the axis strokes and the plot border in points (1.0).
   - `line-width:` Width of the function curve strokes in points (1.5).
   - `top-pad:` Top padding in points (10).
   - `bottom-pad:` Bottom padding in points (5).
   - `left-pad:` Left padding in points (10).
   - `right-pad:` Right padding in points (10).
   - `axis-label-width:` Width reserved for y-axis tick labels in points (40).
   - `axis-label-height:` Height reserved for x-axis tick labels in points (20).
   - `tick-length:` Length of axis tick marks in points (5).
   - `grid-line-lengths:` List of alternating dash/space lengths for grid lines; can be set to `#f` to disable grid lines ((1 3)).
   - `samples:` Number of sample points used to render each function curve (200).
   - `axis-overhead:` Extra length in points by which axes extend beyond the plot area (15).

```scheme
(make-function-chart-config
  'size: (size 500 300)
  'samples: 300
  'line-width: 2.0
  'axis-font: (font "Helvetica" 8)
  'descr-font: (font "Helvetica-LightOblique" 9)
  'stroke-width: 0.7
  'top-pad: 15
  'left-pad: 15
  'right-pad: 10
  'axis-label-width: 35
  'axis-label-height: 22
  'grid-line-lengths: '(1 2))
```

**(function-chart-config? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a function chart configuration, otherwise `#f` is returned.

**(function-chart-size _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the size defined by the given function chart configuration _fconf_.

**(function-chart-size-set! _fconf size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the size for the given function chart configuration _fconf_ to _size_. _size_ is a size object.

**(function-chart-color _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the foreground color (used for axes, ticks, labels, and border) defined by the given function chart configuration _fconf_.

**(function-chart-color-set! _fconf color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the foreground color for the given function chart configuration _fconf_ to _color_.

**(function-chart-bg-color _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the background color of the plot area defined by the given function chart configuration _fconf_.

**(function-chart-bg-color-set! _fconf color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the background color of the plot area for the given function chart configuration _fconf_ to _color_. Set to `#f` to disable background filling.


**(function-chart-box-color _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the box color defining the border of the plot area of the given function chart configuration _fconf_. The box color is set to `#f` if no box is drawn.

**(function-chart-box-color-set! _fconf color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the box color defining the border of the plot area of the given function chart configuration _fconf_ to _color_. Set to `#f` to disable drawing the box.

**(function-chart-axis-font _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the axis tick label font defined by the given function chart configuration _fconf_.

**(function-chart-axis-font-set! _fconf font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the axis tick label font for the given function chart configuration _fconf_ to _font_.

**(function-chart-label-font _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the axis name label font defined by the given function chart configuration _fconf_.

**(function-chart-label-font-set! _fconf font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the axis name label font for the given function chart configuration _fconf_ to _font_.

**(function-chart-descr-font _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the axis description font defined by the given function chart configuration _fconf_.

**(function-chart-descr-font-set! _fconf font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the axis description font for the given function chart configuration _fconf_ to _font_.

**(function-chart-stroke-width _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the axis and border stroke width defined by the given function chart configuration _fconf_.

**(function-chart-stroke-width-set! _fconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the axis and border stroke width for the given function chart configuration _fconf_ to _val_.

**(function-chart-line-width _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the function curve stroke width defined by the given function chart configuration _fconf_.

**(function-chart-line-width-set! _fconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the function curve stroke width for the given function chart configuration _fconf_ to _val_.

**(function-chart-top-pad _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the top padding defined by the given function chart configuration _fconf_.

**(function-chart-top-pad-set! _fconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the top padding for the given function chart configuration _fconf_ to _val_.

**(function-chart-bottom-pad _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the bottom padding defined by the given function chart configuration _fconf_.

**(function-chart-bottom-pad-set! _fconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the bottom padding for the given function chart configuration _fconf_ to _val_.

**(function-chart-left-pad _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the left padding defined by the given function chart configuration _fconf_.

**(function-chart-left-pad-set! _fconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the left padding for the given function chart configuration _fconf_ to _val_.

**(function-chart-right-pad _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the right padding defined by the given function chart configuration _fconf_.

**(function-chart-right-pad-set! _fconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the right padding for the given function chart configuration _fconf_ to _val_.

**(function-chart-axis-label-width _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the width reserved for y-axis tick labels defined by the given function chart configuration _fconf_.

**(function-chart-axis-label-width-set! _fconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the width reserved for y-axis tick labels for the given function chart configuration _fconf_ to _val_.

**(function-chart-axis-label-height _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the height reserved for x-axis tick labels defined by the given function chart configuration _fconf_.

**(function-chart-axis-label-height-set! _fconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the height reserved for x-axis tick labels for the given function chart configuration _fconf_ to _val_.

**(function-chart-tick-length _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the tick mark length defined by the given function chart configuration _fconf_.

**(function-chart-tick-length-set! _fconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the tick mark length for the given function chart configuration _fconf_ to _val_.

**(function-chart-grid-line-lengths _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of alternating dash/space lengths for grid lines defined by the given function chart configuration _fconf_. If `#f` is returned, no grid lines are drawn.

**(function-chart-grid-line-lengths-set! _fconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the list of alternating dash/space lengths for grid lines for the given function chart configuration _fconf_ to _val_. _val_ may be set to `#f` to disable drawing grid lines.

**(function-chart-samples _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of sample points used for rendering function curves as defined by the given function chart configuration _fconf_.

**(function-chart-samples-set! _fconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the number of sample points for the given function chart configuration _fconf_ to _val_. Higher values produce smoother curves at the cost of more computation.

**(function-chart-axis-overhead _fconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the axis overhead defined by the given function chart configuration _fconf_. The axis overhead is the extra length by which axes extend beyond the plot area boundary.

**(function-chart-axis-overhead-set! _fconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the axis overhead for the given function chart configuration _fconf_ to _val_.


## Drawing Function Charts

**(draw-function-chart _funcs xmin xmax ymin ymax xstep ystep xdescr ydescr loc config legend_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(draw-function-chart _funcs xmin xmax ymin ymax xstep ystep xdescr ydescr loc config legend drawing_)**  

Draws a function chart into the drawing _drawing_. If _drawing_ is not provided, the drawing provided by the `current-drawing` parameter object of library `(lispkit draw)` is used.

_funcs_ is either a single function graph (as created by `function-graph`) or a list of function graphs. Each function is sampled across the domain and rendered as a curve in its associated color, clipped to the plot area. _xmin_ and _xmax_ define the domain (x-axis range) of the chart. _ymin_ and _ymax_ define the visible range (y-axis range). _xstep_ defines the increment between tick marks on the x-axis. _ystep_ defines the increment between tick marks on the y-axis. Tick labels are generated automatically for each tick position. _xdescr_ is a string describing the x-axis, or `#f` to omit the description. _ydescr_ is a string describing the y-axis, or `#f` to omit the description. _loc_ is a point specifying the top-left corner at which the chart is placed within the drawing. _config_ is a function chart configuration object as created by `make-function-chart-config`. _legend_ is either a function legend configuration (as created by `make-function-legend-config`) or `#f`. If `#f`, no legend is drawn. If a legend configuration is provided, a legend box is drawn showing the label and color for each function graph that has a non-`#f` label.

The `draw-function-chart` procedure can also be used within the `drawing` syntax form, in which case the _drawing_ argument can be omitted and the chart is drawn into the `current-drawing`:

```scheme
(define my-chart
  (drawing
    (draw-function-chart
      (list (function-graph sin "sin(x)" blue)
            (function-graph cos "cos(x)" green))
      -6.3 6.3 -1.5 1.5
      2.0 0.5 "x" "y"
      (point 50 30)
      (make-function-chart-config
        'color: (color 0.6 0.6 0.6)
        'box-color: #f
        'size: (size 500 300)
        'samples: 300)
      (make-function-legend-config
        'horizontal-offset: -15
        'vertical-offset: 15))))
(save-drawing "trig.pdf" my-chart (size 600 380))
```

