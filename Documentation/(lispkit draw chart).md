# LispKit Draw Chart

Library `(lispkit draw chart)` provides abstractions that are shared between the various chart libraries of LispKit, such as `(lispkit draw chart bar)`, `(lispkit draw chart pie)`, `(lispkit draw chart line)`, and `(lispkit draw chart function)`. Currently, this library defines a single record type, `legend-config`, which encapsulates the parameters needed for drawing a legend within a chart, and a procedure `draw-legend` for drawing legends. A legend links labels (e.g. names of bar segments, slices of a pie chart, series of a line chart, or function graphs) to the colors used to visually distinguish them in a chart.

Chart-specific libraries typically provide their own constructor for legend configurations (e.g. `make-pie-legend-config` from `(lispkit draw chart pie)`, `make-line-legend-config` from `(lispkit draw chart line)`, or `make-function-legend-config` from `(lispkit draw chart function)`) that come with defaults tailored to the corresponding chart type. The accessor and mutator procedures for legend configuration objects, however, are defined once, centrally, by this library and are used by all chart libraries.


## Legends

A _legend configuration_ is a record encapsulating all parameters needed for drawing a legend. Legend configurations are mutable objects that are created via procedure `make-legend-config` (or via a chart-specific constructor such as `make-pie-legend-config`, `make-line-legend-config`, or `make-function-legend-config`). For every parameter, there is an accessor and a setter procedure.

**(make-legend-config _key val ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new legend configuration object from the provided keyword/value pairs. The following keyword arguments are supported. The default value is provided in parenthesis.

   - `font:` Font used for all text in a legend (Helvetica 10).
   - `color:` Color used for legend labels and the border of the legend's bounding box (black).
   - `bg-color:` Background color of the legend's bounding box (white).
   - `stroke-width:` Width of a stroke for drawing the bounding box of the legend (1).
   - `corner-radius:` Corner radius for a rounded bounding box; `0` results in a rectangular bounding box with square corners (0).
   - `horizontal-offset:` Horizontal offset from chart bounds. A non-negative value is interpreted as an offset from the left bound; a negative value is interpreted as an offset from the right bound (70).
   - `vertical-offset:` Vertical offset from chart bounds. A non-negative value is interpreted as an offset from the top bound; a negative value is interpreted as an offset from the bottom bound (10).
   - `sample-area-width:` Width of the area reserved for the color sample box in front of each legend label (17).
   - `sample-length:` Height and width of the color sample box shown for each entry of the legend (10).
   - `line-pad:` Padding between two consecutive lines/entries of the legend (3).
   - `entry-pad:` Top/bottom and left/right padding between the legend entries and the bounding box of the legend (6).

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

**(legend-color _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the color used for legend labels and the bounding box border defined by the given legend configuration _lconf_. Some chart libraries fall back to a chart-specific default color if _lconf_ does not define a color (i.e. `legend-color` returns `#f`).

**(legend-color-set! _lconf color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the color used for legend labels and the bounding box border for the given legend configuration _lconf_ to _color_.

**(legend-bg-color _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the background color of the legend's bounding box defined by the given legend configuration _lconf_. Some chart libraries fall back to a chart-specific default background color if _lconf_ does not define a background color (i.e. `legend-bg-color` returns `#f`).

**(legend-bg-color-set! _lconf color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the background color of the legend's bounding box for the given legend configuration _lconf_ to _color_.

**(legend-stroke-width _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the stroke width defined by the given legend configuration _lconf_, i.e. the width used for drawing the border of the legend's bounding box.

**(legend-stroke-width-set! _lconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the stroke width for the given legend configuration _lconf_ to _val_.

**(legend-corner-radius _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the corner radius defined by the given legend configuration _lconf_, used for drawing a rounded bounding box around the legend. A value of `0` results in a rectangular bounding box with square corners.

**(legend-corner-radius-set! _lconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the corner radius for the given legend configuration _lconf_ to _val_.

**(legend-horizontal-offset _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the horizontal offset defined by the given legend configuration _lconf_. A non-negative value is interpreted as an offset of the legend from the left bound of the chart; a negative value is interpreted as an offset from the right bound.

**(legend-horizontal-offset-set! _lconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the horizontal offset for the given legend configuration _lconf_ to _val_.

**(legend-vertical-offset _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the vertical offset defined by the given legend configuration _lconf_. A non-negative value is interpreted as an offset of the legend from the top bound of the chart; a negative value is interpreted as an offset from the bottom bound.

**(legend-vertical-offset-set! _lconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the vertical offset for the given legend configuration _lconf_ to _val_.

**(legend-sample-area-width _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the sample area width defined by the given legend configuration _lconf_, i.e. the width reserved in front of each label for the color sample box.

**(legend-sample-area-width-set! _lconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the sample area width for the given legend configuration _lconf_ to _val_.

**(legend-sample-length _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the sample length defined by the given legend configuration _lconf_, i.e. the height and width of the color sample box (for bar charts) or the length of the color sample line (for function charts) shown for each legend entry.

**(legend-sample-length-set! _lconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the sample length for the given legend configuration _lconf_ to _val_.

**(legend-line-pad _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the line padding defined by the given legend configuration _lconf_, i.e. the vertical padding between two consecutive legend entries.

**(legend-line-pad-set! _lconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the line padding for the given legend configuration _lconf_ to _val_.

**(legend-entry-pad _lconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the entry padding defined by the given legend configuration _lconf_, i.e. the top/bottom and left/right padding between the legend entries and the bounding box of the legend.

**(legend-entry-pad-set! _lconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the entry padding for the given legend configuration _lconf_ to _val_.


## Drawing Legends

**(draw-legend _entries bounds lconf color bgcolor_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(draw-legend _entries bounds lconf color bgcolor drawing_)**  

Draws a legend into the drawing _drawing_. If _drawing_ is not provided, the drawing provided by the `current-drawing` parameter object of library `(lispkit draw)` is used. The legend consists of a bounding box with one line for each entry. Each line shows a box in the color of the entry followed by the entry's label. This procedure is used by the bar chart, pie chart, and line chart libraries.

_entries_ is a list of legend entries. Each entry is a list whose first element is a label (a string) and whose second element is a color. Further elements are ignored. _bounds_ is a rectangle, in the coordinate system of the drawing, defining the bounds of the chart for which the legend is drawn. The position of the legend is defined by the horizontal and vertical offsets of the legend configuration _lconf_ relative to _bounds_: a non-negative offset is measured from the left/top edge of _bounds_, a negative offset is measured from the right/bottom edge. _lconf_ is a legend configuration object. _color_ is the color used for labels and _bgcolor_ is the color used for filling the bounding box. They are used if _lconf_ does not define a color or a background color. The border of the bounding box is drawn in gray.

```scheme
(define d (make-drawing))
(draw-legend (list (list "Low" blue) (list "High" red))
             (rect 0 0 400 250)
             (make-legend-config 'horizontal-offset: -10 'vertical-offset: 10)
             black
             white
             d)
```
