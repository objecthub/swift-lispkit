# LispKit Draw

Library `(lispkit draw)` provides an API for creating *drawings*. A drawing is defined in terms of a sequence of instructions for drawing *shapes* and *images*. Drawings can be composed and saved as a PDF. It is also possible to draw a drawing into a *bitmap* and save it in formats like PNG, JPG, or TIFF. A *bitmap* is a special *image* that is not based on vector graphics.

Both drawings and shapes are based on a coordinate system whose zero point is in the upper left corner of a plane. The x and y axis extend to the right and down. Coordinates and dimensions are always expressed in terms of floating-point numbers.


## Drawings

Drawings are mutable objects created via `make-drawing`. The functions listed in this section change the state of a drawing object and they persist drawing instructions defining the drawing. For most functions, the drawing is an optional argument. If it is not provided, the function applies to the drawing provided by the `current-drawing` parammeter object.

**current-drawing** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

Defines the _current drawing_, which is used as a default by all functions for which the drawing argument is optional. If there is no current drawing, this parameter is set to `#f`.

**(drawing? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a drawing. Otherwise, it returns `#f`.

**(make-drawing)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new, empty drawing. A drawing consists of a sequence of drawing instructions and drawing state consisting of the following components:

   - Stroke color (set via `set-color`)
   - Fill color (set via `fill-color`)
   - Shadow (set via `set-shadow` and `remove-shadow`)
   - Transformation (add transformation via `enable-transformation` and remove via `disable-transformation`)

**(copy-drawing _drawing_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of the given _drawing_.

**(clear-drawing _drawing_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Clears the given _drawing_.

**(set-color _color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(set-color _color drawing_)**  

Sets the _stroke color_ for the given drawing, or `current-drawing` if the drawing argument is not provided.

**(set-fill-color _color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(set-fill-color _color drawing_)**  

Sets the _fill color_ for the given drawing, or `current-drawing` if the drawing argument is not provided.

**(set-line-width _width_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(set-line-width _width drawing_)**  

Sets the default _stroke width_ for the given drawing, or `current-drawing` if the drawing argument is not provided.

**(set-shadow _color size blur-radius_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(set-shadow _color size blur-radius drawing_)**  

Defines a shadow for the given drawing, or `current-drawing` if the drawing argument is not provided. _color_ is the color of the shade, _blur-radius_ defines the radius for bluring the shadow.

**(remove-shadow)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(remove-shadow _drawing_)**  

Removes shadow for the subsequent drawing instructions of the given drawing, or `current-drawing` if the drawing argument is missing.

**(enable-transformation _tf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(enable-transformation _tf drawing_)**  

Enables the transformation _tf_ for subsequent drawing instructions of the given drawing, or `current-drawing` if the drawing argument is missing. Each drawing maintains an active affine transformation for shifting, rotating, and scaling the coordinate systems of subsequent drawing instructions.

**(disable-transformation _tf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(disable-transformation _tf drawing_)**  

Disables the transformation _tf_ for subsequent drawing instructions of the given drawing, or `current-drawing` if the drawing argument is missing. 

**(draw _shape_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(draw _shape width_)**  
**(draw _shape width drawing_)**  

Draws _shape_ with a given stroke _width_ into the drawing specified via _drawing_ or parameter object `current-drawing` if _drawing_ is not provided. The default for _width_, in case it is not provided, is set via _set-line-width_. The stroke is drawn in the current stroke color of the drawing.

**(draw-dashed _shape lengths phase_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(draw-dashed _shape lengths phase width_)**  
**(draw-dashed _shape lengths phase width drawing_)**  

Draws _shape_ with a dashed stroke of width _width_ into the drawing specified via _drawing_ or parameter object `current-drawing` if _drawing_ is not provided. `1.0` is the default for _width_ in case it is not provided. _lengths_ specifies an alternating list of dash/space lengths. _phase_ determines the start of the dash/space pattern. The dashed stroke is drawn in the current stroke color of the drawing. 

**(fill _shape_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(fill _shape drawing_)**  

Fills _shape_ with the current _fill color_ in the drawing specified via _drawing_ or parameter object `current-drawing` if _drawing_ is not provided.

**(fill-gradient _shape colors_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(fill-gradient _shape colors spec_)**  
**(fill-gradient _shape colors spec drawing_)**  

Fills _shape_ with a gradient in the drawing specified via argument _drawing_ or parameter object `current-drawing` if _drawing_ is not provided. The gradient is specified in terms of a list of _colors_ and argument _spec_. _spec_ can either be a number or a point. If _spec_ is a number, this number determines an angle for a linear gradient. If _spec_ is a point, it is the center of a radial gradient.

**(draw-line _start end_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(draw-line _start end drawing_)**  

Draws a line between point _start_ and point _end_ in the drawing specified via argument _drawing_ or parameter object `current-drawing`, if _drawing_ is not provided. The line is drawn in the default _stroke width_ and the current _stroke color_.

**(draw-rect _rect_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(draw-rect _rect drawing_)**  

Draws a rectangular given by _rect_ in the drawing specified via argument _drawing_ or parameter object `current-drawing`, if _drawing_ is not provided. The rectangular is drawn in the default _stroke width_ and the current _stroke color_.

**(fill-rect _rect_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(fill-rect _rect drawing_)**  

Fills a rectangular given by _rect_ with the current _fill color_ in the drawing specified via argument _drawing_ or parameter object `current-drawing`, if _drawing_ is not provided.

**(draw-ellipse _rect_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(draw-ellipse _rect drawing_)**  

Draws an ellipse into the rectangular _rect_ in the drawing specified via argument _drawing_ or parameter object `current-drawing`, if _drawing_ is not provided. The ellipse is drawn in the default _stroke width_ and the current _stroke color_.

**(fill-ellipse _rect_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(fill-ellipse _rect drawing_)**  

Fills an ellipse given by rectangular _rect_ with the current _fill color_ in the drawing specified via argument _drawing_ or parameter object `current-drawing`, if _drawing_ is not provided.

**(draw-text _str location font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(draw-text _str location font color_)**  
**(draw-text _str location font color drawing_)**  

Draws string _str_ at _location_ in the given font and color in the drawing specified by argument _drawing_ or parameter object `current-drawing` if _drawing_ is not provided. _location_ is either the left, top-most point at which the string is drawn, or it is a rect specifying a bounding box. _color_ specifies the text color. If it is not provided, the text is drawn in black.

**(text-size _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(text-size _str font_)**  
**(text-size _str font dimensions_)**  

Returns a size object describing the width and height needed to draw string _str_ using _font_ in a space constrained by _dimensions_. _dimensions_ is either a size object specifying the maximum width and height, or it is a number constraining the width only, assuming infinite hight. If _dimensions_ is omitted, the maximum width and height is infinity.

**(draw-styled-text _txt location_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(draw-styled-text _txt location drawing_)**  

Draws styled text _txt_ at _location_ in the drawing specified by argument _drawing_ or parameter object `current-drawing` if _drawing_ is not provided. _location_ is either the left, top-most point at which the styled text is drawn, or it is a rect specifying a bounding box.

**(styled-text-size _txt_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(styled-text-size _txt dimensions_)**  

Returns a size object describing the width and height needed to draw styled text _txt_ in a space constrained by _dimensions_. _dimensions_ is either a size object specifying the maximum width and height, or it is a number constraining the width only, assuming infinite hight. If _dimensions_ is omitted, the maximum width and height is infinity.

**(draw-html _html location_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(draw-html _html location drawing_)**  

Draws a string _html_ containing HTML source code at _location_ in the drawing specified by argument _drawing_ or parameter object `current-drawing` if _drawing_ is not provided. _location_ is either the left, top-most point at which the HTML is drawn, or it is a rect specifying a bounding box.

**(html-size _html_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(html-size _html dimensions_)**  

Returns a size object describing the width and height needed to render the HTML in string _html_ in a space constrained by _dimensions_. _dimensions_ is either a size object specifying the maximum width and height, or it is a number constraining the width only, assuming infinite hight. If _dimensions_ is omitted, the maximum width and height is infinity.

**(draw-image _image location_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(draw-image _image location opacity_)**  
**(draw-image _image location opacity composition_)**  
**(draw-image _image location opacity composition drawing_)**  

Draws image _image_ at _location_ with the given _opacity_ and _composition_ method. The image is drawn in the drawing specified by argument _drawing_ or parameter object `current-drawing` if _drawing_ is not provided. _location_ is either the left, top-most point at which the image is drawn, or it is a rect specifying a bounding box for the image. _composition_ is a floating-point number between 0.0 (= transparent) and 1.0 (= completely not transparent) with 1.0 being the default. _composition_ refers to a symbol specifying a composition method. The following methods are supported (the source is the image, the destination is the drawing):

   - `clear`: Transparency everywhere.
   - `copy`: The source image (default).
   - `multiply`: The source color is multiplied by the destination color.
   - `overlay`: Source colors overlay the destination.
   - `source-over`: The source image wherever it is opaque, and the destination elsewhere.
   - `source-in`: The source image wherever both images are opaque, and transparent elsewhere.
   - `source-out`: The source image wherever it is opaque and the destination is transparent, and transparent elsewhere.
   - `source-atop`: The source image wherever both source and destination are opaque, the destination wherever it is opaque but the source image is transparent, and transparent elsewhere.
   - `destination-over`: The destination wherever it is opaque, and the source image elsewhere.
   - `destination-in`: The destination wherever both images are opaque, and transparent elsewhere.
   - `destination-out`: The destination wherever it is opaque and the source image is transparent, and transparent elsewhere.
   - `destination-atop`: The destination wherever both image and destination are opaque, the source image wherever it is opaque and the destination is transparent, and transparent elsehwere.

**(draw-drawing _other_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(draw-drawing _other drawing_)**  

Draws drawing _other_ into the drawing specified by argument _drawing_ or parameter object `current-drawing` if _drawing_ is not provided. This function can be used
to compose drawings.

**(clip-drawing _other clippingshape_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(clip-drawing _other clippingshape drawing_)**  

Draws drawing _other_ into the drawing specified by argument _drawing_ or parameter object `current-drawing` if _drawing_ is not provided. This function clips the drawing using shape _clippingshape_; i.e. only parts within _clippingshape_ are drawn.

**(inline-drawing _other_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(inline-drawing _other drawing_)**  

Draws drawing _other_ into the drawing specified by argument _drawing_ or parameter object `current-drawing` if _drawing_ is not provided. This function can be used to compose drawings in a way such that the drawing instructions from _other_ are inlined into _drawing_. 

**(save-drawing _path drawing size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(save-drawing _path drawing size title_)**  
**(save-drawing _path drawing size title author_)**  

Saves _drawing_ into a PDF file at the given filepath _path_. _size_ is a size specifying the width and height of the PDF page containing the drawing in points; i.e. the media box of the page is `(rect zero-point` _size_`)`. _title_ and _author_ are optional strings defining the title and author metadata for the generated PDF file.

**(save-drawings _path pages_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(save-drawings _path pages title_)**  
**(save-drawings _path pages title author_)**  

Saves a list of pages into a PDF file at the given filepath _path_. A page is defined in terms of a list of two elements (_drawing_ _size_), where _drawing_ is a drawing for that page and _size_ is a media box for the page.  _title_ and _author_ are optional strings defining the title and author metadata for the generated PDF file.

**(drawing _body ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Creates a new empty drawing, binds parameter object `current-drawing` to it and executes the body statements in the dynamic scope of this binding. This special form returns the new drawing.

**(with-drawing _drawing body ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Binds parameter object `current-drawing` to _drawing_ and executes the body statements in the dynamic scope of this binding. This special form returns the result of the last statement in the body.

**(transform _tf body ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

This form is used in the context of drawing into `current-drawing`. It enables the transformation _tf_, executes the statements in the body and disables the transformation again. 


## Shapes

Shapes are mutable objects created via a number of constructors, including `make-shape`, `copy-shape`, `line`, `polygon`, `rectangular`, `circle`, `oval`, `arc`, and `glyphs`. Besides the constructors, functions like `move-to`, `line-to` and `curve-to` are used to extend a shape. For those functions, the affected shape is an optional argument. If it is not provided, the function applies to the shape defined by the `current-shape` parammeter object.

**current-shape** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

Defines the _current shape_, which is used as a default by all functions for which the shape argument is optional. If there is no current shape, this parameter is set to `#f`.

**(shape? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a shape. Otherwise, it returns `#f`.

**(make-shape)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-shape _prototype_)**  

Returns a new shape object. If argument _prototype_ is provided, the new shape object will inherit from shape _prototype_; i.e. the new shape's definition will extend the definition of shape _prototype_.

**(copy-shape _shape_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of _shape_.

**(line _start end_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Retuns a new line shape. _start_ and _end_ are the start and end points of the line.

**(polygon _point ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new polygon shape. The polygon is defined in terms of a sequence of points.

**(rectangle _point size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(rectangle _point size radius_)**  
**(rectangle _point size xradius yradius_)**  

Returns a new rectangular shape. The rectangle is defined in terms of the left, topmost point and a _size_ defining both width and height of the rectangle. The optional _radius_, _xradius_ and _yradius_ arguments are used to create a rounded rectangular whose rounded edges are defined in terms of an x and y-radius. If only one radius is provided, it defines both x and y-radius.

**(circle _point radius_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new circle shape. The circle is defined in terms of a center point and a radius.

**(oval _point size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new oval shape. The oval is defined in terms of a rectangle whose left, topmost point is provided as argument _point_, and whose width and height are given via argument _size_.

**(arc _point radius start end_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(arc _point radius start end clockwise_)**  

Returns a new arc shape. The arc is defined via the arguments _point_, _radius_, _start_ , _end_ and optionally _clockwise_. _point_ is the starting point of the arc, _radius_ defines the radius of the arc, _start_ is a starting angle in radians, and _end_ is the end angle in radians. _clockwise_ is a boolean argument defining whether the arc is drawn clockwise or counter-clockwise. The default is `#t`.

**(glyphs _str point size font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new "glyphs" shape. This is a shape defined by a string _str_ rendered in the given size and font at a given point. _font_ is a font object, _size_ is the font size in points, and _point_ are the start coordinates where the glyphs are drawn.

**(transform-shape _shape tf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new shape derived from _shape_ by applying transformation _tf_.

**(flip-shape _shape_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(flip-shape _shape box_)**  
**(flip-shape _shape box orientation_)**  

Returns a new shape by flipping/mirroring _shape_ within _box_. _box_ is a rect. If it is not provided, the bounding box of _shape_ is used as a default. Argument _orientation_ is a symbol defining along which axis the shape is flipped. Supported are `horizontal`, `vertical`, and `mirror`. Default is `vertical`.

**(interpolate _points_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(interpolate _points closed_)**  
**(interpolate _points closed alpha_)**  
**(interpolate _points closed alpha method_)**  

Returns a shape interpolating a list of points. _closed_ is an optional boolean argument specifying whether the shape is closed. The default for _closed_ is `#f`. _alpha_ is an interpolation parameter in the range [0.0,1.0]; default is 0.33. _method_ specifies the interpolation method via a symbol. The following two methods are supported: `hermite` and `catmull-rom`; default is `hermite`.

**(move-to _point_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(move-to _point shape_)**  

Sets the "current point" to _point_ for the shape specified by argument _shape_ or parameter object `current-shape` if _shape_ is not provided.

**(line-to _point ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(line-to _point ... shape_)**  

Creates a line from the "current point" to _point_ for the shape specified by argument _shape_ or parameter object `current-shape` if _shape_ is not provided. _point_ becomes the new "current point".

**(curve-to _point cntrl1 cntrl2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(curve-to _point cntrl1 cntrl2 shape_)**  

Creates a curve from the "current point" to _point_ for the shape specified by argument _shape_ or parameter object `current-shape` if _shape_ is not provided. _cntrl1_ and _cntrl2_ are control points defining tangets shaping the curve at the start and end points.

**(relative-move-to _point_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(relative-move-to _point shape_)**  

This function is equivalent to `move-to` with the exception that _point_ is relative to the "current point".

**(relative-line-to _point ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(relative-line-to _point ... shape_)**  

This function is equivalent to `line-to` with the exception that _point_ is relative to the "current point".

**(relative-curve-to _point cntrl1 cntrl2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(relative-curve-to _point cntrl1 cntrl2 shape_)**  

This function is equivalent to `curve-to` with the exception that _point_, _cntrl1_ and _cntrl2_ are relative to the "current point".

**(add-shape _other_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(add-shape _other shape_)**  

Adds shape _other_ to the shape specified by argument _shape_ or parameter object `current-shape` if _shape_ is not provided. This function is typically used to compose shapes.

**(shape-bounds _shape_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the bounding box for the given shape as a rect.

**(shape _body ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Creates a new empty shape, binds parameter object `current-shape` to it and executes the body statements in the dynamic scope of this binding. This special form returns the new drawing.

**(with-shape _shape body ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Binds parameter object `current-shape` to _shape_ and executes the body statements in the dynamic scope of this binding. This special form returns the result of the last statement in the body.


## Images

Images are objects representing immutable pictures of mutable size and metadata. Images are either loaded from image files or they are created from drawings. Images are either vector-based or bitmap-based. The current image API only allows loading vector-based images from PDF files. Bitmap-based images, on the other hand, can be loaded from PNG, JPG, GIF, etc. image files or they are created by drawing a drawing object into an empty bitmap. Bitmap-based images optionally have mutable EXIF data.

**(image? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an image. Otherwise, it returns `#f`.

**(load-image _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Loads an image from the file at _path_ and returns the corresponding image object.

**(load-image-asset _path type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(load-image-asset _path type dir_)**   

Loads an image from the file at the given relative file _path_ and returns the corresponding image object. _type_ refers to the default suffix of the file to load (e.g. `"png"` for PNG images).  

`load-image-asset` constructs a relative file path in the following way (assuming _path_ does not have a suffix already):

&nbsp;&nbsp;&nbsp;_dir/path.type_

where _dir_ is `"Images"` if it is not provided as a parameter. It then searches the asset paths in their given order for a file matching this relative file path. Once the first matching file is found, the file is loaded as an image file and the image gets returned by `load-image-asset`. It is an error if no matching image was found.

**(bytevector-\>image _bvec_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(bytevector-\>image _bvec start_)**  
**(bytevector-\>image _bvec start end_)**  

Loads an image from the binary data provided by bytevector _bvec_ between positions _start_ and _end_ and returns the corresponding image object.

**(image-size _image_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the size of the given image object in points.

**(set-image-size! _image size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the size of _image_ to _size_, a size in points.

**(bitmap? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a bitmap-based image. Otherwise, it returns `#f`.

**(bitmap-size _bmap_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the size of the bitmap _bmap_ in points. If _bmap_ is not a bitmap object, `bitmap-size` returns `#f`.

**(bitmap-pixels _bmap_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of horizontal and vertical pixels of the bitmap _bmap_ as a size. If _bmap_ is not a bitmap object, `bitmap-size` returns `#f`.

**(bitmap-exif-data _bmap_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the EXIF metadata associated with bitmap _bmap_. EXIF metadata is represented as an association list in which symbols are used as keys.

```scheme
> (define photo (load-image (asset-file-path "Regensberg" "jpeg" "Images")))
> (bitmap-exif-data photo)
((ExposureBiasValue . 0)
 (CustomRendered . 6)
 (SensingMethod . 2)
 (SubsecTimeOriginal . "615")
 (SubsecTimeDigitized . "615")
 (Flash . 0)
 (ExposureTime . 0.00040306328093510683)
 (OffsetTime . "+01:00")
 (PixelXDimension . 8066)
 (ExifVersion 2 3 1)
 (OffsetTimeDigitized . "+01:00")
 (ISOSpeedRatings 25)
 (OffsetTimeOriginal . "+01:00")
 (DateTimeDigitized . "2019:10:27 14:21:39")
 (FlashPixVersion 1 0)
 (WhiteBalance . 0)
 (PixelYDimension . 3552)
 (LensSpecification 4.25 4.25 1.7999999523162842 1.7999999523162842)
 (ColorSpace . 65535)
 (LensModel . "iPhone XS back camera 4.25mm f/1.8")
 (SceneCaptureType . 0)
 (ApertureValue . 1.6959938128383605)
 (SceneType . 1)
 (ShutterSpeedValue . 11.276932534193945)
 (FocalLength . 4.25)
 (FNumber . 1.8)
 (LensMake . "Apple")
 (FocalLenIn35mmFilm . 26)
 (BrightnessValue . 10.652484683458134)
 (ComponentsConfiguration 1 2 3 0)
 (MeteringMode . 5)
 (DateTimeOriginal . "2019:10:27 14:21:39"))
```

**(set-bitmap-exif-data! _bmap_ _exif_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the EXIF metadata for the given bitmap _bmap_ to _exif_. _exif_ is an association list defining all the EXIF attributes with symbols being used as keys.

```scheme
> (define photo (load-image (asset-file-path "Regensberg" "jpeg" "Images")))
> (set-bitmap-exif-data! photo
    '((ExposureBiasValue . 0)
      (Flash . 0)
      (ExposureTime . 0.0005)
      (PixelXDimension . 8066)
      (PixelYDimension . 3552)
      (ExifVersion 2 3 1)
      (ISOSpeedRatings 25)
      (FlashPixVersion 1 0)
      (WhiteBalance . 0)
      (LensSpecification 4.25 4.25 1.7999999523162842 1.7999999523162842)
      (ColorSpace . 65535)
      (SceneCaptureType . 0)
      (ApertureValue . 1.6959938128383605)
      (SceneType . 1)
      (ShutterSpeedValue . 11.276932534193945)
      (FocalLength . 4.25)
      (FNumber . 1.8)
      (BrightnessValue . 10.652484683458134)
      (ComponentsConfiguration 1 2 3 0)
      (OffsetTime . "+01:00")
      (OffsetTimeOriginal . "+01:00")
      (DateTimeOriginal . "2019:10:27 14:21:39")
      (OffsetTimeDigitized . "+01:00")
      (DateTimeDigitized . "2019:10:27 14:21:39")))
```

**(make-bitmap _drawing size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-bitmap _drawing size ppi_)**  

Creates a new bitmap-based image by drawing the object _drawing_ into an empty bitmap of size _size_ in points. _ppi_ determines the number of pixels per inch. By default, _ppi_ is set to 72. In this case, the number of pixels of the bitmap corresponds to the number of points (since 1 pixel corresponds to 1/72 of an inch). For a _ppi_ value of 144, the horizontal and vertial number of pixels is doubled, etc.

**(bitmap-crop _bitmap rect_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Crops a rectangle from the given bitmap and returns the result in a new bitmap. _rect_ is a rectangle in pixels. Its intersection with the dimensions of _bitmap_ (in pixels) are used for cropping.

**(bitmap-blur _bitmap radius_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Blurs the given bitmap with the given blur radius and returns the result in a new bitmap of the same size.

**(save-bitmap _path bitmap format_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Saves a given bitmap-based image _bitmap_ in a file at filepath _path_. _format_ is a symbol specifying the image file format. Supported are: `png`, `jpg`, `gif`, `bmp`, and `tiff`.

**(bitmap-\>bytevector _bitmap format_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a bytevector with an encoding of _bitmap_ in the given format. _format_ is a symbol specifying the image format. Supported are: `png`, `jpg`, `gif`, `bmp`, and `tiff`.


## Transformations

A transformation is an immutable object defining an affine transformation. Transformations can be used to:

   * shift,
   * scale, and
   * rotate coordinate systems.

Transformations are typically used in drawings to transform drawing instructions. They can also be used to transform shapes.

**(transformation? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a transformation. Otherwise, it returns `#f`.

**(transformation _tf ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new transformation by composing the given transformations _tf_.

**(invert _tf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Inverts transformation _tf_ and returns a new transformation object for it.

**(translate _dx dy_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(translate _dx dy tf_)**  

Returns a transformation for shifting the coordinate system by _dx_ and _dy_. If transformation _tf_ is provided, the translation transformation extends _tf_.

**(scale _dx dy_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(scale _dx dy tf_)**  

Returns a transformation for scaling the coordinate system by _dx_ and _dy_. If transformation _tf_ is provided, the scaling transformation extends _tf_.

**(rotate _angle_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(rotate _angle tf_)**  

Returns a transformation for rotating the coordinate system by _angle_ (in radians). If transformation _tf_ is provided, the rotation transformation extends _tf_.


## Colors

Colors are immutable objects defining colors in terms of four components: red, green, blue and alpha. Library `(lispkit draw)` currently only supports RGB color spaces.

`(lispkit draw)` supports the concept of _color lists_ on _macOS_. A color list is provided as a `.plist` file and stored in the "ColorLists" asset directory of LispKit. It maps color names expressed as symbols to color values. Color lists need to be loaded explicitly via procedure `load-color-list`.

**(color? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a color. Otherwise, it returns `#f`.

**(color _spec_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(color _name clist_)**  
**(color _r g b_)**  
**(color _r g b alpha_)**  

This procedure returns new color objects. If _spec_ is provided, it either is a string containing a color description in hex format, or it is a symbol referring to the name of a color in the default color list `(White Yellow Red Purple Orange Magenta Green Cyan Brown Blue Black)`. If a different color list should be used, its name can be specified via string _clist_. Procedure `(available-color-lists)` returns a list of all available color lists. If the color is specified via a hex string, the following formats can be used: `"ccc"`, `"#ccc"`, `"rrggbb"`, and `"#rrggbb"`.

The color can also be specified using color components _r, g, b,_ and _alpha_. _alpha_ determines the transparency of the color (0.0 = fully transparent, 1.0 = no transparency). The default value for _alpha_ is 1.0.

**(color-red _color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the red color component of _color_.

**(color-green _color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the green color component of _color_.

**(color-blue _color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the blue color component of _color_.

**(color-alpha _color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the alpha color component of _color_.

**(color-\>hex _color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a representation of the given _color_ in hex form as a string.

```scheme
(color->hex (color 1.0 0.5 0.1))  ⇒  "#FF801A"
(color->hex (color "#6AF"))       ⇒  "#66AAFF"
```

**black** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  
**gray**  
**white**  
**red**  
**green**  
**blue**  
**yellow**  

Predefined color objects.

**(available-color-lists)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of available color lists. The LispKit installation guarantees that there is at least color list "HTML" containing all named colors from the HTML 5 specification.

```scheme
(available-color-lists)
⇒  ("HTML" "Web Safe Colors" "Crayons" "System" "Apple")
```

**(load-color-list _name path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Loads a new color list stored as a `.plist` file in the assets directory of LispKit at the given file _path_ (which can also refer to color lists outside of the assets directory via absolute file paths). _name_ is a string which specifies the name of the color list. It is added to the list of available colors if loading of the color list was successful. `load-color-list` returns `#t` if the color list could be successfully loaded, `#f` otherwise.

**(available-colors _clist_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of color identifiers supported by the given color list. _clist_ is a string specifying the name of the color list.

```scheme
(available-colors "HTML")
⇒  (YellowGreen Yellow WhiteSmoke White Wheat Violet Turquoise Tomato Thistle Teal Tan SteelBlue Snow SlateGrey SlateGray SlateBlue SkyBlue Silver Sienna SeaShell SeaGreen SandyBrown Salmon SaddleBrown RoyalBlue RosyBrown Red RebeccaPurple Purple PowderBlue Plum Pink Peru PeachPuff PapayaWhip PaleVioletRed PaleTurquoise PaleGreen PaleGoldenRod Orchid OrangeRed Orange OliveDrab Olive OldLace Navy NavajoWhite Moccasin MistyRose MintCream MidnightBlue MediumVioletRed MediumTurquoise MediumSpringGreen MediumSlateBlue MediumSeaGreen MediumPurple MediumOrchid MediumBlue MediumAquaMarine Maroon Magenta Linen LimeGreen Lime LightYellow LightSteelBlue LightSlateGrey LightSlateGray LightSkyBlue LightSeaGreen LightSalmon LightPink LightGrey LightGreen LightGray LightGoldenRodYellow LightCyan LightCoral LightBlue LemonChiffon LawnGreen LavenderBlush Lavender Khaki Ivory Indigo IndianRed HotPink HoneyDew Grey GreenYellow Green Gray GoldenRod Gold GhostWhite Gainsboro Fuchsia ForestGreen FloralWhite FireBrick DodgerBlue DimGrey DimGray DeepSkyBlue DeepPink DarkViolet DarkTurquoise DarkSlateGrey DarkSlateGray DarkSlateBlue DarkSeaGreen DarkSalmon DarkRed DarkOrchid DarkOrange DarkOliveGreen DarkMagenta DarkKhaki DarkGrey DarkGreen DarkGray DarkGoldenRod DarkCyan DarkBlue Cyan Crimson Cornsilk CornflowerBlue Coral Chocolate Chartreuse CadetBlue BurlyWood Brown BlueViolet Blue BlanchedAlmond Black Bisque Beige Azure Aquamarine Aqua AntiqueWhite AliceBlue)
```


## Fonts

Fonts are immutable objects defining fonts in terms of a font name and a font size (in points).

**(font? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a font. Otherwise, it returns `#f`.

**(font _fontname size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(font _familyname size weight trait ..._)**  

If only two arguments, _fontname_ and _size_, are provided, `font` will return a new font object for a font with the given font name and font size (in points). If more than two arguments are provided, `font` will return a new font object for a font with the given font family name, font size (in points), font weight, as well as a number of font traits.

The weight of a font is specified as an integer on a scale from 0 to 15. Library `(lispkit draw)` exports the following weight constants:

   - `ultralight` (1)
   - `thin` (2)
   - `light` (3)
   - `book` (4)
   - `normal` (5)
   - `medium` (6)
   - `demi` (7)
   - `semi` (8)
   - `bold` (9)
   - `extra` (10)
   - `heavy` (11)
   - `super` (12)
   - `ultra` (13)
   - `extrablack` (14)

Font traits are specified as integer masks. The following trait constants are exported from library `(lispkit draw)`:

   - `italic`
   - `boldface`
   - `unitalic`
   - `unboldface`
   - `narrow`
   - `expanded`
   - `condensed`
   - `small-caps`
   - `poster`
   - `compressed`
   - `monospace`

**(font-name _font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the font name of _font_.

**(font-family-name _font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the font family name of _font_.

**(font-size _font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the font size of _font_ in points.

**(font-weight _font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the font weight of _font_. See documentation of function `font` for details.

**(font-traits _font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the font traits of _font_ as an integer bitmask. See documentation of function `font` for details.

**(font-has-traits _font trait ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if font _font_ has all the given traits.

**(available-fonts)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(available-fonts _trait ..._)**  

Returns all the available fonts that have matching font traits.

**(available-font-families)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns all the available font families, i.e. all font families for which there is at least one font installed.


## Points

A _point_ describes a location on a two-dimensional plane consisting of a _x_ and _y_ coordinate. Points are represented as pairs of floating-point numbers where the car representes the x-coordinate and the cdr represents the y-coordinate. Even though an expression like `'(3.5 . -2.0)` does represent a point, it is recommended to always construct points via function `point`; e.g. `(point 3.5 -2.0)`.

**(point? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid point. Otherwise, it returns `#f`.

**(point _x y_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a point for coordinates _x_ and _y_.

**(move-point _point dx fy_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Moves _point_ by _dx_ and _dy_ and returns the result as a point.

**(point-x _point_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the x-coordinate for _point_.

**(point-y _point_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the y-coordinate for _point_.

**zero-point** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  

The _zero point_, i.e `(point 0.0 0.0)`.


## Size

A _size_ describes the dimensions of a rectangle consisting of _width_ and _height_ values. Sizes are represented as pairs of floating-point numbers where the car representes the width and the cdr represents the height. Even though an expression like `'(5.0 . 3.0)` does represent a size, it is recommended to always construct sizes via function `size`; e.g. `(size 5.0 3.0)`.

**(size? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid size. Otherwise, it returns `#f`. 

**(size _w h_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a size for the given width _w_ and height _h_.

**(size-width _size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the width for _size_.

**(size-height _size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the height for _size_.

**(increase-size _size dx dy_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new size object whose width is increased by _dx_ and whose height is increased by _dy_.

**(scale-size _size factor_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new size object whose width and height is multiplied by _factor_.


## Rects

A _rect_ describes a rectangle in terms of an upper left _point_ and a _size_. Rects are represented as pairs whose car is a point and whose cdr is a size. Even though an expression like `'((1.0 . 2.0) . (3.0 4.0))` does represent a rect, it is recommended to always construct rects via function `rect`; e.g. `(rect (point 1.0 2.0) (size 3.0 4.0))`.

**(rect? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid rect. Otherwise, it returns `#f`. 

**(rect _point size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(rect _x y width height_)**  

Returns a rect either from the given _point_ and _size_, or from x-coordinate _x_, y-coordinate _y_, width _w_, and height _h_.

**(rect-point _rect_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the upper left corner point of _rect_.

**(rect-size _rect_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the size of _rect_ as a size object, i.e. as a pair of floating-point numbers where the car representes the width and the cdr represents the height of _rect_.

**(rect-x _rect_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the x-coordinate of the upper left corner point of _rect_.

**(rect-y _rect_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the y-coordinate of the upper left corner point of _rect_.

**(rect-width _rect_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the width of _rect_.

**(rect-height _rect_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the height of _rect_.

**(rect-max-point _rect_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the lower right corner point of _rect_.

**(rect-max-x _rect_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the x-coordinate of the lower right corner point of _rect_.

**(rect-max-y _rect_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the y-coordinate of the lower right corner point of _rect_.

**(move-rect _rect d_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(move-rect _rect dx dy_)**  

Moves _rect_ by _dx_ and _dy_ and returns the result. If only one argument _d_ is provided, _rect_ is moved by _d_ both horizontally and vertically.

**(inset-rect _rect d_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(inset-rect _rect horizental vertical_)**  
**(inset-rect _rect left top right bottom_)**  

Insets _rect_ on all sides as specified by the arguments. If only _d_ is provided, it specifies the amount added to the rectangle's left and top and the amount subtracted from the rectangle's right and bottom. _horizontal_ specifies the amount added to the rectangle's left and subtracted from the rectangle's right. _vertical_ specifies the amount added to the rectangle's top and subtracted from the rectangle's bottom. If _left_, _top_, _right_, and _bottom_ are given individually, they specify the amounts to add/subtract from the individual sides.
