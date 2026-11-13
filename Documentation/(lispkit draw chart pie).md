# LispKit Draw Chart Pie

Library `(lispkit draw chart pie)` supports drawing pie charts and donut charts based on a data-driven API in which _pie charts_ are being described declaratively. A drawing procedure is then able to render a pie chart into a given drawing as defined by library `(lispkit draw)`.


## Pie Chart Model

The pie chart model on which library `(lispkit draw chart pie)` is based on consists of the following components:

- A pie chart is defined by a list of _slices_.
- Each _slice_ consists of a value, an optional label, an optional color, and an offset in points by which the slice is pulled out of the pie.
- The angle covered by a slice is proportional to its value: it is the share of the slice's value in the sum of the values of all slices. Slices with value `0` are not drawn.
- Slices without a color get a color from the _palette_ of the chart configuration (the n-th slice gets the n-th color, restarting with the first color if there are more slices than colors in the palette).
- The first slice starts at the _start angle_, which is measured in degrees clockwise from 12 o'clock. Subsequent slices follow clockwise or counter-clockwise.
- A _hole_ in the center turns the pie chart into a donut chart.
- The _label_ of a slice is drawn next to the slice and connected to it via a _leader line_. The label is placed beyond the end of the leader line, in the direction of the middle of the slice: labels at the top of the pie are placed above and labels at the bottom below the end of their leader line, labels at the sides are placed beside it, and labels in between are placed at the corresponding corner. Labels that would overlap are moved apart vertically, away from the horizontal center line of the pie, and are moved sideways if they would otherwise cover the leader line of another label. If a label ends up farther away from its leader line, a short line connects the two. Very small neighboring slices can still lead to crowded labels. In this case, labels can be switched off with `label-font: #f` and shown in a legend instead.
- The _value_ of a slice is drawn inside of the slice, either as a percentage, as the raw value, or in a format defined by a procedure. Values are centered in the slice if there is enough room; for narrow slices they are moved towards the outer edge of the slice, where the slice is wider. Values are omitted for slices that are too small to show them.
- A _legend_ shows the labels of the slices and the colors used to draw the corresponding slices.
- A _legend configuration_ specifies how the legend is layed out.
- A _pie chart configuration_ specifies how the chart overall is being drawn. This includes all fonts, colors, padding values, the size of the hole, the start angle, etc.

The pie is centered in the rectangle in which the chart is drawn, after subtracting the padding on all four sides. Its radius is as large as possible such that all labels, including their leader lines, still fit into the rectangle. If slices are pulled out of the pie, the radius is reduced just enough such that the pulled out slices stay within the rectangle. The legend is drawn on top of the chart at a position defined by the legend configuration relative to the bounds of the chart. It does not reduce the space available for the pie. Padding values can be used to reserve space for it.


## Slices

A _slice_ bundles a value with an optional label, an optional color, and an offset. Slices are immutable objects created via procedure `slice`.

**(slice _label value_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(slice _label value color_)**  
**(slice _label value color offset_)**  

Creates a new slice. _label_ is a string identifying the slice in the chart and in the legend. If _label_ is `#f`, the slice is not labeled and does not show up in the legend. _value_ is a non-negative real number. _color_ is the color used to draw the slice; if it is `#f` or omitted, the color is taken from the palette of the pie chart configuration. _offset_ is a non-negative number defining the distance in points by which the slice is pulled out of the pie; the default is `0`. It is an error if one of the arguments is invalid.

```scheme
(slice "Apples" 40)
(slice "Pears" 25 (color "4E79A7"))
(slice "Cherries" 20 red 12)
(slice #f 15)
```

**(slice? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a slice, otherwise `#f` is returned.

**(slice-label _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the label of the given slice _s_, or `#f` if the slice does not have a label.

**(slice-value _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the value of the given slice _s_.

**(slice-color _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the color of the given slice _s_, or `#f` if the color is taken from the palette of the pie chart configuration.

**(slice-offset _s_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the offset of the given slice _s_, i.e. the distance in points by which it is pulled out of the pie.


## Legend Configurations

A _legend configuration_ is a record encapsulating all parameters needed for drawing a legend in a chart. Legend configurations are defined independently by library `(lispkit draw chart)`. But there is a specialized `make-pie-legend-config` procedure which provides defaults specifically for usage with pie charts.

**(make-pie-legend-config _key val ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

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

With the default offsets, the legend is shown in the top right corner of the chart.

```scheme
(make-pie-legend-config
  'font: (font "Helvetica" 9)
  'stroke-width: 0.4
  'corner-radius: 4
  'horizontal-offset: 12
  'vertical-offset: -12
  'entry-pad: 7)
```

Accessor procedures for legend configuration objects are provided by library `(lispkit draw chart)`.


## Pie Chart Configurations

A _pie chart configuration_ is a record encapsulating all parameters needed for drawing a pie chart (excluding the legend). Pie chart configurations are mutable objects that are created via procedure `make-pie-chart-config`. For every parameter of the configuration, there is an accessor and a setter procedure.

**(make-pie-chart-config _key val ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new pie chart configuration object from the provided keyword/value pairs. The following keyword arguments are supported. The default value is provided in parenthesis.

   - `size:` Size of the rectangle in which the chart is drawn (500 × 300).
   - `color:` Color of text and leader lines (black).
   - `bg-color:` Background color of the chart or `#f` for a transparent background (`#f`).
   - `border-color:` Color of the borders between slices or `#f` to disable borders (white).
   - `stroke-width:` Width of the slice borders and the leader lines in points (1).
   - `label-font:` Font for the labels of slices, which are drawn outside of the pie; `#f` disables labels (Helvetica 11).
   - `value-font:` Font for the values drawn inside of the slices; `#f` disables values (Helvetica-Bold 10).
   - `value-format:` Defines how values are shown (`percent`). Supported are `#f` (no values), `percent` (share of the slice in percent, e.g. `"34%"`), `value` (the value of the slice), and procedures. A procedure is called with the value of a slice and its share in the total (a number between 0 and 1) and returns the string to show, or `#f` for showing nothing.
   - `value-color:` Color for the values drawn inside of the slices. If it is `#f`, black or white is chosen automatically, depending on the color of the slice (`#f`).
   - `palette:` List of colors for slices that do not define a color (the ten colors `4E79A7`, `F28E2B`, `E15759`, `76B7B2`, `59A14F`, `EDC948`, `B07AA1`, `FF9DA7`, `9C755F`, `BAB0AC`).
   - `hole:` Radius of the hole in the center of the chart relative to the radius of the chart. `0` results in a pie chart; a value between `0` and `0.95` results in a donut chart (0).
   - `start-angle:` Angle in degrees at which the first slice starts, measured clockwise from 12 o'clock (0).
   - `clockwise?:` If `#t`, the slices are arranged clockwise; if `#f`, they are arranged counter-clockwise (`#t`).
   - `top-pad:` Top padding in points (10).
   - `bottom-pad:` Bottom padding in points (10).
   - `left-pad:` Left padding in points (10).
   - `right-pad:` Right padding in points (10).
   - `leader-length:` Length in points of the leader lines between the pie and the labels. `0` disables leader lines (10).
   - `label-pad:` Minimal distance in points between the end of a leader line and the label (3).

```scheme
(make-pie-chart-config
  'size: (size 495 300)
  'hole: 0.5
  'start-angle: 90
  'label-font: (font "Helvetica" 10)
  'value-format: (lambda (value share) (string-append (number->string value) " kg"))
  'border-color: (color 0.95 0.95 0.95)
  'stroke-width: 1.5)
```

**(pie-chart-config? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a pie chart configuration, otherwise `#f` is returned.

**(pie-chart-size _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the size defined by the given pie chart configuration _pconf_.

**(pie-chart-size-set! _pconf size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the size for the given pie chart configuration _pconf_ to _size_. _size_ is a size object.

**(pie-chart-color _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the color for text and leader lines defined by the given pie chart configuration _pconf_.

**(pie-chart-color-set! _pconf color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the color for text and leader lines for the given pie chart configuration _pconf_ to _color_.

**(pie-chart-bg-color _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the background color defined by the given pie chart configuration _pconf_, or `#f` if the background is transparent.

**(pie-chart-bg-color-set! _pconf color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the background color for the given pie chart configuration _pconf_ to _color_. Set to `#f` to disable background filling.

**(pie-chart-border-color _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the color of the borders between slices defined by the given pie chart configuration _pconf_, or `#f` if no borders are drawn.

**(pie-chart-border-color-set! _pconf color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the color of the borders between slices for the given pie chart configuration _pconf_ to _color_. Set to `#f` to disable drawing borders.

**(pie-chart-stroke-width _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the stroke width for slice borders and leader lines defined by the given pie chart configuration _pconf_.

**(pie-chart-stroke-width-set! _pconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the stroke width for slice borders and leader lines for the given pie chart configuration _pconf_ to _val_.

**(pie-chart-label-font _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the font for slice labels defined by the given pie chart configuration _pconf_, or `#f` if labels are disabled.

**(pie-chart-label-font-set! _pconf font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the font for slice labels for the given pie chart configuration _pconf_ to _font_. Set to `#f` to disable labels next to the slices, e.g. if the labels are only shown in a legend.

**(pie-chart-value-font _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the font for the values drawn inside of the slices defined by the given pie chart configuration _pconf_, or `#f` if values are disabled.

**(pie-chart-value-font-set! _pconf font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the font for the values drawn inside of the slices for the given pie chart configuration _pconf_ to _font_. Set to `#f` to disable drawing values.

**(pie-chart-value-format _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the value format defined by the given pie chart configuration _pconf_. It is either `#f`, `percent`, `value`, or a procedure.

**(pie-chart-value-format-set! _pconf fmt_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the value format for the given pie chart configuration _pconf_ to _fmt_. _fmt_ is either `#f`, `percent`, `value`, or a procedure accepting the value of a slice and its share in the total and returning a string or `#f`.

**(pie-chart-value-color _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the color for the values drawn inside of the slices defined by the given pie chart configuration _pconf_, or `#f` if the color is chosen automatically.

**(pie-chart-value-color-set! _pconf color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the color for the values drawn inside of the slices for the given pie chart configuration _pconf_ to _color_. Set to `#f` to choose between black and white automatically, depending on the color of the slice.

**(pie-chart-palette _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the palette defined by the given pie chart configuration _pconf_, a list of colors for slices that do not define a color.

**(pie-chart-palette-set! _pconf colors_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the palette for the given pie chart configuration _pconf_ to the list _colors_.

**(pie-chart-hole _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the radius of the hole relative to the radius of the chart defined by the given pie chart configuration _pconf_.

**(pie-chart-hole-set! _pconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the relative radius of the hole for the given pie chart configuration _pconf_ to _val_. `0` results in a pie chart, a larger value (at most `0.95`) in a donut chart.

**(pie-chart-start-angle _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the start angle in degrees defined by the given pie chart configuration _pconf_. The angle is measured clockwise from 12 o'clock.

**(pie-chart-start-angle-set! _pconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the start angle in degrees for the given pie chart configuration _pconf_ to _val_.

**(pie-chart-clockwise? _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the given pie chart configuration _pconf_ arranges slices clockwise, `#f` if it arranges them counter-clockwise.

**(pie-chart-clockwise-set! _pconf flag_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the direction in which slices are arranged for the given pie chart configuration _pconf_. `#t` stands for clockwise, `#f` for counter-clockwise.

**(pie-chart-top-pad _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the top padding defined by the given pie chart configuration _pconf_.

**(pie-chart-top-pad-set! _pconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the top padding for the given pie chart configuration _pconf_ to _val_.

**(pie-chart-bottom-pad _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the bottom padding defined by the given pie chart configuration _pconf_.

**(pie-chart-bottom-pad-set! _pconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the bottom padding for the given pie chart configuration _pconf_ to _val_.

**(pie-chart-left-pad _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the left padding defined by the given pie chart configuration _pconf_.

**(pie-chart-left-pad-set! _pconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the left padding for the given pie chart configuration _pconf_ to _val_.

**(pie-chart-right-pad _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the right padding defined by the given pie chart configuration _pconf_.

**(pie-chart-right-pad-set! _pconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the right padding for the given pie chart configuration _pconf_ to _val_.

**(pie-chart-leader-length _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the length of the leader lines between pie and labels defined by the given pie chart configuration _pconf_.

**(pie-chart-leader-length-set! _pconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the length of the leader lines for the given pie chart configuration _pconf_ to _val_. Set to `0` to disable leader lines.

**(pie-chart-label-pad _pconf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the minimal distance between the end of a leader line and its label defined by the given pie chart configuration _pconf_.

**(pie-chart-label-pad-set! _pconf val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the minimal distance between the end of a leader line and its label for the given pie chart configuration _pconf_ to _val_.


## Drawing Pie Charts

**(draw-pie-chart _slices loc config legend_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(draw-pie-chart _slices loc config legend drawing_)**  

Draws a pie chart into the drawing _drawing_. If _drawing_ is not provided, the drawing provided by the `current-drawing` parameter object of library `(lispkit draw)` is used.

_slices_ is a non-empty list of slices as created by `slice`, in the order in which they are arranged. At least one slice needs to have a value greater than zero. _loc_ is either a point specifying the top-left corner at which the chart is placed within the drawing, in which case the size of the chart is defined by the configuration, or a rectangle defining the bounds of the chart. _config_ is a pie chart configuration object as created by `make-pie-chart-config`. _legend_ is either a legend configuration (as created by `make-pie-legend-config`) or `#f`. If `#f`, no legend is drawn. If a legend configuration is provided, a legend box is drawn showing the label and color for each slice that has a label, including slices with value `0`.

An error is signaled if the chart is too small for showing the pie together with its labels.

Here is an example showcasing the usage of `draw-pie-chart`:

```scheme
(define d (make-drawing))
(draw-pie-chart
  (list (slice "Apples" 40)
        (slice "Pears" 25)
        (slice "Cherries" 20)
        (slice "Plums" 15))
  (point 50 30)                              ; location
  (make-pie-chart-config
    'size: (size 500 300))                   ; chart configuration
  (make-pie-legend-config)                   ; legend configuration
  d)                                         ; target drawing
(save-drawing "fruits.pdf" d (size 600 380))
```

A donut chart without labels next to the slices, an exploded slice, and a custom value format:

```scheme
(define d (make-drawing))
(draw-pie-chart
  (list (slice "Americas" 1840)
        (slice "EMEA" 1290 #f 14)
        (slice "Asia Pacific" 960)
        (slice "Other" 215))
  (point 50 30)
  (make-pie-chart-config
    'hole: 0.5
    'label-font: #f
    'value-format: (lambda (value share)
                     (string-append "$" (number->string value) "k")))
  (make-pie-legend-config
    'horizontal-offset: 12
    'vertical-offset: -12)
  d)
(save-drawing "revenue.pdf" d (size 600 380))
```

The `draw-pie-chart` procedure can also be used within the `drawing` syntax form, in which case the _drawing_ argument can be omitted and the chart is drawn into the `current-drawing`:

```scheme
(define my-chart
  (drawing
    (draw-pie-chart
      (list (slice "Salaries" 52)
            (slice "Infrastructure" 18)
            (slice "Marketing" 12)
            (slice "Research" 10)
            (slice "Other" 8))
      (point 50 30)
      (make-pie-chart-config 'start-angle: 90 'clockwise?: #f)
      #f)))
(save-drawing "budget.pdf" my-chart (size 600 380))
```

