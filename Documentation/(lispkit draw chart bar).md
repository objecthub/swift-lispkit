# LispKit Draw Chart Bar

Library `(lispkit draw chart bar)` supports drawing bar charts based on a data-driven API in which _bar charts_ are being described declaratively. A drawing function is then able to draw a bar chart into a given drawing as defined by library `(lispkit draw)`.


## Bar Chart Model

The following diagram shows a generic bar chart model including the various elements that make up bar charts and parameters that can be configured:

![Bar Chart Model and Parameters](x-devonthink-item://B0D68944-538F-432F-BB26-40D578EDF35C)

The bar chart model on which library `(lispkit draw chart bar)` is based on consists of the following components:

   - A bar chart is defined by a list of _bar groups_.
   - Each _bar group_ consists of a list of _bars_ and an optional label.
   - Each _bar_ consists of a list of _values_ and optionally a label and a color.
   - Each _value_ of a bar matches a _bar segment_ (via the position in the list).
   - A list of _bar segments_ is provided when a bar chart is being drawn. Each _bar segment_ consists of a label, a bar color, and optionally, a text color (for the value shown in the bar).
   - A _legend_ shows the provided _bar segments_ and the color used to highlight them in the bar chart.
   - A _legend configuration_ specifies how the legend is layed out (see the various parameters in the diagram above)
   - A _bar chart configuration_ specifies how the bar chart overall is being drawn. This includes all fonts, offsets, padding values, etc. that are shown in the diagram above.


## Legend Configurations

A _legend configuration_ is a record encapsulating all parameters needed for drawing a legend in a bar chart. Legend configurations are mutable objects that are created via procedure `make-legend-config`. For every parameter, there is an accessor and a setter procedure.

**(make-legend-config _key val ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new legend configuration object from the provided keyword/value pairs. The following keyword arguments are supported. The default value is provided in parenthesis.

   - `font:` Font used for all text in a legend (Helvetica 10).
   - `stroke-width:` Width of a stroke for drawing the bounding box of the legend (1.0).
   - `horizontal-offset:` Horizontal offset from chart bounds (negative values are interpreted as offsets from the right bound) (70).
   - `vertical-offset:` Vertical offset from chart bounds (negative values are interpreted as offsets from the bottom bound) (10).
   - `sample-area-width:` Width of the sample area (17).
   - `sample-length:` Heigh and width of color sample boxes for the various segments (10).
   - `line-pad:` Padding between segment lines (3).
   - `entry-pad:` Top/bottom and left/right padding around segment descriptions including the sample area (6).

```scheme
(make-legend-config
  'font: (font "Helvetica" 7)
  'stroke-width: 0.4
  'entry-pad: 5
  'sample-area-width: 16
  'sample-length: 8
  'horizontal-offset: 50)
```

**(legend-config? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a legend configuration object, `#f` otherwise.

**(legend-font _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the font defined by the given legend configuration _lconf_.

**(legend-font-set! _lconf font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the font for the given legend configuration _lconf_ to _font_.

**(legend-stroke-width _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the stroke width defined by the given legend configuration _lconf_.

**(legend-stroke-width-set! _lconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the stroke width for the given legend configuration _lconf_ to _val_.

**(legend-horizontal-offset _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the horizontal offset defined by the given legend configuration _lconf_.

**(legend-horizontal-offset-set! _lconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the horizontal offset for the given legend configuration _lconf_ to _val_.

**(legend-vertical-offset _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the vertical offset defined by the given legend configuration _lconf_.

**(legend-vertical-offset-set! _lconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the horizontal offset for the given legend configuration _lconf_ to _val_.

**(legend-sample-area-width _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the sample area width defined by the given legend configuration _lconf_.

**(legend-sample-area-width-set! _lconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the sample area width for the given legend configuration _lconf_ to _val_.

**(legend-sample-length _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the sample length defined by the given legend configuration _lconf_.

**(legend-sample-length-set! _lconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the sample length for the given legend configuration _lconf_ to _val_.

**(legend-line-pad _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the line padding defined by the given legend configuration _lconf_.

**(legend-line-pad-set! _lconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the line padding for the given legend configuration _lconf_ to _val_.

**(legend-entry-pad _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the entry padding defined by the given legend configuration _lconf_.

**(legend-entry-pad-set! _lconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the entry padding for the given legend configuration _lconf_ to _val_.


## Bar Chart Configurations

A _bar chart configuration_ is a record encapsulating all parameters needed for drawing a bar chart (excluding the bar chart legend). Bar chart configurations are mutable objects that are created via procedure `make-bar-chart-config`. For every parameter of the configuration, there is an accessor and a setter procedure.

**(make-bar-chart-config _key val ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new bar chart configuration object from the provided keyword/value pairs. The following keyword arguments are supported. The default value is provided in parenthesis.

   - `size:` The rectangle in which the chart is drawn (495 x 200)
   - `color:` Color of text, axis, etc. (black)
   - `bg-color:` Color of legend background (white)
   - `value-font:` Font for displaying values (Helvetica 10)
   - `bar-font:` Font for displaying values on top of bars (Helvetica 11)
   - `label-font:` Font for displaying bar labels (Helvetica 12)
   - `group-font:` Font for displaying bar group labels (Helvetica-Bold 12)
   - `descr-font:` Font for describing the axis (Helvetica-LightOblique 10)
   - `stroke-width:` Width of a stroke in pixels (1.0)
   - `top-pad:` Top padding in pixels (20)
   - `bottom-pad:` Padding below the x axis (5)
   - `right-pad:` Right-side padding (15)
   - `left-pad:` Padding left to the y axis (10)
   - `bar-gap:` Space between two bar groups (20)
   - `group-gap:` Space between two bars within a group (5)
   - `vlabel-width:` Width of labels on y axis (50)
   - `vindicator-width:` Width of y label indicator lines (10)
   - `vline-lengths:` List of alternating dash/space lengths; can be set to `#f` to disable the line ((1 2))
   - `value-pad:` Padding between bar and displayed value (1)
   - `blabel-height:` Height of bar labels (14)
   - `glabel-height:` Height of group labels (30)
   - `xaxis-overhead:` Overhead on x axis (20)
   - `yaxis-overhead:` Overhead on y axis (20)

```scheme
(make-bar-chart-config
  'size: (size 495 200)
  'color: (color 0.9 0.9 0.9)
  'value-font: (font "Helvetica" 8.5)
  'bar-font: (font "Helvetica" 8)
  'label-font: (font "Helvetica" 9)
  'top-pad: 5
  'left-pad: 10
  'right-pad: 5
  'bar-gap: 10
  'vlabel-width: 34
  'glabel-height: 5
  'blabel-height: 20)
```

**(bar-chart-config? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a bar chart configuration, otherwise `#f` is returned.

**(bar-chart-size _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the size defined by the given bar chart configuration _bconf_.

**(bar-chart-size-set! _bconf size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the size for the given bar chart configuration _bconf_ to _size_. _size_ is a size object.

**(bar-chart-value-font _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the value font defined by the given bar chart configuration _bconf_.

**(bar-chart-value-font-set! _bconf font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the value font for the given bar chart configuration _bconf_ to _font_.

**(bar-chart-bar-font _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the bar font defined by the given bar chart configuration _bconf_.

**(bar-chart-bar-font-set! _bconf font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the bar font for the given bar chart configuration _bconf_ to _font_.

**(bar-chart-label-font _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the label font defined by the given bar chart configuration _bconf_.

**(bar-chart-label-font-set! _bconf font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the label font for the given bar chart configuration _bconf_ to _font_.

**(bar-chart-group-font _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the group font defined by the given bar chart configuration _bconf_.

**(bar-chart-group-font-set! _bconf font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the group font for the given bar chart configuration _bconf_ to _font_.

**(bar-chart-descr-font _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the description font defined by the given bar chart configuration _bconf_.

**(bar-chart-descr-font-set! _bconf font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the description font for the given bar chart configuration _bconf_ to _font_.

**(bar-chart-stroke-width _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the stroke width defined by the given bar chart configuration _bconf_.

**(bar-chart-stroke-width-set! _bconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the stroke width for the given bar chart configuration _bconf_ to _val_.

**(bar-chart-top-pad _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the top padding defined by the given bar chart configuration _bconf_.

**(bar-chart-top-pad-set! _bconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the top padding for the given bar chart configuration _bconf_ to _val_.

**(bar-chart-bottom-pad _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the bottom padding defined by the given bar chart configuration _bconf_.

**(bar-chart-bottom-pad-set! _bconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the bottom padding for the given bar chart configuration _bconf_ to _val_.

**(bar-chart-right-pad _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the right padding defined by the given bar chart configuration _bconf_.

**(bar-chart-right-pad-set! _bconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the right padding for the given bar chart configuration _bconf_ to _val_.

**(bar-chart-left-pad _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the left padding defined by the given bar chart configuration _bconf_.

**(bar-chart-left-pad-set! _bconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the left padding for the given bar chart configuration _bconf_ to _val_.

**(bar-chart-bar-gap _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the bar gap defined by the given bar chart configuration _bconf_.

**(bar-chart-bar-gap-set! _bconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the bar gap for the given bar chart configuration _bconf_ to _val_.

**(bar-chart-group-gap _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the group gap defined by the given bar chart configuration _bconf_.

**(bar-chart-group-gap-set! _bconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the group gap for the given bar chart configuration _bconf_ to _val_.

**(bar-chart-vlabel-width _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the vertical label width defined by the given bar chart configuration _bconf_.

**(bar-chart-vlabel-width-set! _bconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the vertical label width for the given bar chart configuration _bconf_ to _val_.

**(bar-chart-vindicator-width _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the vertical value indicator width defined by the given bar chart configuration _bconf_.

**(bar-chart-vindicator-width-set! _bconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the vertical value indicator width for the given bar chart configuration _bconf_ to _val_.

**(bar-chart-vline-lengths _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of alternating dash/space lengths defined by the given bar chart configuration _bconf_. If `#f` is returned, no horizontal value lines are drawn.

**(bar-chart-vline-lengths-set! _bconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the list of alternating dash/space lengths for the given bar chart configuration _bconf_ to _val_. _val_ may be set to `#f` to disable drawing horizontal value lines.

**(bar-chart-value-pad _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the value padding defined by the given bar chart configuration _bconf_.

**(bar-chart-value-pad-set! _bconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the value padding for the given bar chart configuration _bconf_ to _val_.

**(bar-chart-blabel-height _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the value padding, i.e. the space between bar and displayed value, defined by the given bar chart configuration _bconf_.

**(bar-chart-blabel-height-set! _bconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the value padding, i.e. the space between bar and displayed value, for the given bar chart configuration _bconf_ to _val_.

**(bar-chart-glabel-height _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the group label height defined by the given bar chart configuration _bconf_.

**(bar-chart-glabel-height-set! _bconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the group label height for the given bar chart configuration _bconf_ to _val_.

**(bar-chart-xaxis-overhead _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the overhead on the x axis of the coordinate system defined by the given bar chart configuration _bconf_.

**(bar-chart-xaxis-overhead-set! _bconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the overhead on the x axis of the coordinate system for the given bar chart configuration _bconf_ to _val_.

**(bar-chart-yaxis-overhead _bconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the overhead on the y axis of the coordinate system defined by the given bar chart configuration _bconf_.

**(bar-chart-yaxis-overhead-set! _bconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the overhead on the y axis of the coordinate system for the given bar chart configuration _bconf_ to _val_.


## Constructing Bar Charts

**(bar-spec? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a bar diagram specification, `#f` otherwise. A bar diagram specification is a list of bars and bar groups.

**(bar? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a bar object, otherwise `#f` is returned.

**(bar _label value ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(bar _label color value ..._)**  

Creates a new bar object. A bar without bar segments consists of a single value and an optional label string (`#f` disables the label) and color. A segmented bar has a value for all segments (i.e. all bars of a bar diagram should have the same number of segments). A segment is disabled by setting its value to 0.

**(bar-label _bar_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the label of the given bar object _bar_.

**(bar-color _bar_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the color of the given bar object _bar_.

**(bar-values _bar_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the values of the given bar object _bar_.

**(bar-group? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a bar group object, otherwise `#f` is returned.

**(bar-group _label bar ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new bar group from the bars _bar_ ... with string _label_ as label.

**(bar-group-label _group_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the label of the given bar group _group_.

**(bar-group-bars _group_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the bars of the given bar group _group_.

**(bar-segment _label col_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(bar-segment _label col textcol_)**  

Creates a bar segment represented by label string _label_ and segment color _col_. Text color _textcol_ is optional (and might be `#f`).


## Drawing Bar Charts

**(draw-bar-chart _bars col ystep ydescr xdescr loc config legend_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(draw-bar-chart _bars col ystep ydescr xdescr loc config legend drawing_)**  

Draws the bar diagram _bars_ with _col_ as the default bar color into the drawing _drawing_. If _drawing_ is not provided, the drawing provided by the `current-drawing` parameter object of library `(lispkit draw)` is used.

_ystep_ defines the increment between values on the y axis. _ydescr_ defines the label of the y axis. _xdescr_ defines the label of the x axis. _loc_ is a point at which the bar diagram is drawn with the bar diagram configuration _config_. If a legend should be drawn, a legend configuration needs to be provided as parameter _legend_ and _col_ needs to refer to a list of `bar-segment` objects describing a label string, a bar and a text color for all the segments used within a bar. The legend links labels to bar colors.

```scheme
(define d (make-drawing))
(draw-bar-chart 
  (list (bar "Jan" 0) (bar "Feb" 2) (bar "Mar" 6)
        (bar "Apr" 9) (bar "May" 14) (bar "Jun" 16)
        (bar "Jul" 19) (bar "Aug" 18) (bar "Sep" 15)
        (bar "Oct" 11) (bar "Nov" 5) (bar "Dec" 2))
  gray 5 "Temperature [C°]" "Month"
  (point 10 0)
  (make-bar-chart-config 'size: (size 500 200))
  #f d)
(save-drawing "test.pdf" d (size 520 210))
```
