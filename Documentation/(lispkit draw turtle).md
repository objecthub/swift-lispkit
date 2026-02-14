# LispKit Draw Turtle

Library `(lispkit draw turtle)` defines a simple "turtle graphics" API. The API provides functionality for creating turtles and for moving turtles on a plane generating _drawings_ as a side-effect. A _drawing_ is a data structure defined by library `(lispkit draw)`.

A _turtle_ is defined in terms of the following components:
   - A position _(x, y)_ defining the coordinates where the turtle is currently located within a coordinate system defined by parameters used to create the turtle via `make-turtle`
   - A heading _angle_ which defines the direction in degrees into which the turtle is moving
   - A boolean flag _pen down_ which, if set to `#t`, will make the turtle draw lines on the graphics plane when moving.
   - A _line width_ defining the width of lines drawn by the turtle
   - A _color_ defining the color of lines drawn by the turtle
   - A _drawing_ which records the moves of the turtle while the pen is down.

Turtles are mutable objects created via `make-turtle`. The functions listed below change the state of a turtle. In particular, they generate a drawing as a side-effect which can be accessed via `turtle-drawing`. For most functions, the turtle is an optional argument. If it is not provided, the function applies to the turtle provided by the `current-turtle` parammeter object.


**current-turtle** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

Defines the _current turtle_, which is used as a default by all functions for which the turtle argument is optional. If there is no current turtle, this parameter is set to `#f`.

**(turtle? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a turtle. Otherwise, it returns `#f`.

**(make-turtle _x y scale_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new turtle object. _x_ and _y_ determine the "home point" of the turtle. This is equivalent to the zero point of the coordinate system in which the turtle navigates. _scale_ is a scaling factor.

**(turtle-drawing _turtle_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the drawing associated with the given _turtle_.

**(turtle-x _turtle_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the x coordinate of the position of the given _turtle_.

**(turtle-y _turtle_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the y coordinate of the position of the given _turtle_.

**(turtle-angle _turtle_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the current angle of the given _turtle_.

**(turtle-angle _turtle_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the current angle of the given _turtle_.

**(turtle-pen-down? _turtle_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the given _turtle_ has its pen down; otherwise `#f` is returned.

**(pen-up)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pen-up _turtle_)**  

Lifts _turtle_ from the plane. If _turtle_ is not provided, the turtle defined by `current-turtle` is used. Subsequent `forward` and `backward` operations don't lead to lines being drawn. Only the current coordinates are getting updated.

**(pen-down)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pen-down _turtle_)**  

Drops _turtle_ onto the plane. If _turtle_ is not provided, the turtle defined by `current-turtle` is used. Subsequent `forward` and `backward` operations will lead to lines being drawn.

**(pen-color _color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pen-color _color turtle_)**  

Sets the drawing color of _turtle_ to _color_. If _turtle_ is not provided, the turtle defined by `current-turtle` is used. _color_ is a color object as defined by library `(lispkit draw)`.

**(pen-size _size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pen-size _size turtle_)**  

Sets the pen size of _turtle_ to _size_. If _turtle_ is not provided, the turtle defined by `current-turtle` is used. The pen size corresponds to the width of lines drawn by `forward` and `backward`.

**(home)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(home _turtle_)**  

Moves _turtle_ to its home position. If _turtle_ is not provided, the turtle defined by `current-turtle` is used.

**(move _x y_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(move _x y turtle_)**  

Moves _turtle_ to the position described by the coordinates _x_ and _y_. If _turtle_ is not provided, the turtle defined by `current-turtle` is used.

**(heading _angle_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(heading _angle turtle_)**  

Sets the heading of _turtle_ to _angle_. If _turtle_ is not provided, the turtle defined by `current-turtle` is used. _angle_ is expressed in terms of degrees.

**(turn _angle_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(turn _angle turtle_)**  

Adjusts the heading of _turtle_ by _angle_ degrees. If _turtle_ is not provided, the turtle defined by `current-turtle` is used.

**(right _angle_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(right _angle turtle_)**  

Adjusts the heading of _turtle_ by _angle_ degrees. If _turtle_ is not provided, the turtle defined by `current-turtle` is used.

**(left _angle_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(left _angle turtle_)**  

Adjusts the heading of _turtle_ by _-angle_ degrees. If _turtle_ is not provided, the turtle defined by `current-turtle` is used.

**(forward _distance_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(forward _distance turtle_)**  

Moves _turtle_ forward by _distance_ units drawing a line if the pen is down. If _turtle_ is not provided, the turtle defined by `current-turtle` is used.

**(backward _distance_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(backward _distance turtle_)**  

Moves _turtle_ backward by _distance_ units drawing a line if the pen is down. If _turtle_ is not provided, the turtle defined by `current-turtle` is used.

**(arc _angle radius_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(arc _angle radius turtle_)**  

Turns the turtle by the given _angle_ (in radians) and draws an arc with _radius_ around the current turtle position if the pen is down. If _turtle_ is not provided, the turtle defined by `current-turtle` is used.
