# LispKit Image

Library `(lispkit image)` provides a comprehensive interface to Apple's _Core Image_ framework for advanced image processing operations. The library supports creating image processing pipelines using abstract images, applying various filters, and performing coordinate transformations.

The image library is built around three main object types:

- **Abstract images**: Represent images in Core Image's processing pipeline, supporting lazy evaluation and efficient composition of operations.
- **Image filters**: Represent image operations for generating, transforming, or combining abstract images, e.g. to apply effects like blur, color adjustment, distortion, and composition.
- **Image coefficients**: Represent numeric vectors used as parameters for filters.

All image operations work with abstract images, which can be converted to and from concrete images as defined by library `(lispkit draw)` for display, output, or file I/O operations.


## Filter Categories and Implementations

**(available-image-filter-categories)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(available-image-filter-categories _raw?_)**  

Returns a list of available Core Image filter categories. If _raw?_ is `#t`, returns the raw string names; otherwise returns symbolic identifiers. Categories include:

- `blur`: Image blurring effects
- `sharpen`: Image sharpening effects
- `color-adjustment`, `color-effect`: Color manipulation filters
- `distortion-effect`: Geometric distortion filters
- `composite-operation`: Image blending and composition
- `generator`: Filters that create images from scratch
- `stylize`: Artistic and stylization effects

```scheme
(available-image-filter-categories)
⇒  (non-square-pixels composite-operation tile-effect interlaced color-adjustment reduction generator blur gradient transition sharpen builtin high-dynamic-range stylize filter-generator still-image halftone-effect video geometry-adjustment distortion-effect color-effect)
(available-image-filter-categories #t)
⇒  ("CICategoryFilterGenerator" "CICategoryHighDynamicRange" "CICategoryHalftoneEffect" "CICategoryNonSquarePixels" "CICategoryStylize" "CICategoryColorAdjustment" "CICategoryStillImage" "CICategoryGenerator" "CICategorySharpen" "CICategoryTransition" "CICategoryGeometryAdjustment" "CICategoryDistortionEffect" "CICategoryGradient" "CICategoryBuiltIn" "CICategoryInterlaced" "CICategoryVideo" "CICategoryCompositeOperation" "CICategoryReduction" "CICategoryTileEffect" "CICategoryColorEffect" "CICategoryBlur")
```

**(image-filter-category _category_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(image-filter-category _category raw?_)**  

_category_ is either a string or a symbol. `image-filter-category` returns a symbol matching _category_ if _raw?_ is either not provided or set to `#f`. `image-filter-category` returns a string matching _category_ if _raw?_ is set to `#f`. `image-filter-category` returns `#f` if _category_ is unknown or unsupported.

```scheme
(image-filter-category 'distortion-effect)  ⇒  distortion-effect
(image-filter-category 'distortion-effect #t)  ⇒  "CICategoryDistortionEffect"
(image-filter-category 'unknown)  ⇒  #f
(image-filter-category "CICategoryDistortionEffect")  ⇒  distortion-effect
```

**(available-image-filter-implementations)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(available-image-filter-implementations _categories_)**  
**(available-image-filter-implementations _categories raw?_)**  

Returns a list of available image filter implementations in the given filter _categories_. _categories_ is a list of Core Image filter category identifiers. A category identifier is either a symbol or a string. Use boolean argument _raw?_ to get internal Core Image filter implementation names instead of symbolic identifiers.

```scheme
➤(available-image-filter-implementations '(reduction))
⇒  (area-alpha-weighted-histogram area-average area-average-maximum-red area-bounds-red area-histogram area-logarithmic-histogram area-maximum area-maximum-alpha area-minimum area-minimum-alpha area-min-max area-min-max-red column-average histogram-display-filter kmeans row-average)
(available-image-filter-implementations '(reduction high-dynamic-range) #t)
⇒  ("CIAreaAverage" "CIAreaAverageMaximumRed" "CIAreaBoundsRed" "CIAreaLogarithmicHistogram" "CIAreaMaximum" "CIAreaMaximumAlpha" "CIAreaMinimum" "CIAreaMinimumAlpha" "CIAreaMinMax" "CIAreaMinMaxRed" "CIColumnAverage" "CIKMeans" "CIRowAverage")
```

## Abstract Images

**(abstract-image? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an abstract image, `#f` otherwise.

**(make-abstract-image)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-abstract-image _source_)**  
**(make-abstract-image _source flip?_)**  

Creates an abstract image from various sources:

- If _source_ is a string: loads image from the given file path
- If _source_ is a bytevector: decodes image data
- If _source_ is an image: converts to abstract image
- If _source_ is an abstract image: returns the abstract image
- If _source_ is `#f` or omitted: creates an empty abstract image

The boolean argument _flip?_ controls vertical orientation (defaults to `#f`).

```scheme
(make-abstract-image)               ⇒ <empty abstract image>
(make-abstract-image "dir/pt.jpg")  ⇒ <abstract image from file>
(make-abstract-image image #t)      ⇒ <vertically flipped abstract image>
```

**(image->abstract-image _image_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(image->abstract-image _image flip?_)**  

Converts an image to an abstract image. Use boolean argument _flip?_ to control vertical orientation.

**(color->abstract-image _color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates an infinite abstract image filled with the specified _color_. This generator is useful for making backgrounds or for color generation filters.

```scheme
(color->abstract-image (color 1.0 0.0 0.0))  ⇒ <infinite red image>
```

**(abstract-image->image _aimage_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(abstract-image->image _aimage ppi_)**  
**(abstract-image->image _aimage ppi flip?_)**  

Renders an abstract image _aimage_ into an image. Argument _ppi_ specifies pixels per inch (default 72, maximum 720). Returns `#f` if rendering fails.

```scheme
(abstract-image->image ai)        ⇒ <rendered native image>
(abstract-image->image ai 144 #t) ⇒ <high-DPI flipped image>
```

**(abstract-image-bounds _aimage_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the bounding rectangle of an abstract image as `((x . y) . (width . height))`. Returns `#f` for images with infinite bounds. In the Core Image framework, the bounding rectangle is also called the _extent_ of an abstract image.

```scheme
(abstract-image-bounds my-image)  ⇒  ((0.0 . 0.0) . (1024.0 . 768.0))
```

**(abstract-image-adjustment-filters _aimage_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(abstract-image-adjustment-filters _aimage options_)**  

Generates adjustment filters for enhancing the given abstract image _aimage_. Argument _options_ is an association list that can include:

- `(crop . #t)`: Enable automatic cropping
- `(enhance . #t)`: Enable contrast/exposure enhancement
- `(rotate . #t)`: Enable automatic rotation correction
- `(red-eye . #t)`: Enable red-eye reduction

Returns a list of image filters that can be applied to improve the image.

## Image Filters

**(image-filter? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an image filter, `#f` otherwise.

**(make-image-filter _name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-image-filter _name input_)**  
**(make-image-filter _name input args_)**  

Creates an image filter with the specified _name_. Optionally sets an abstract input image and provides arguments for the specified filter. _args_ is an association list of argument names and values.

```scheme
(make-image-filter 'gaussian-blur)
(make-image-filter 'gaussian-blur img '((input-radius . 5.0)))
(make-image-filter 'color-controls #f '((input-brightness . 0.1) (input-contrast . 1.2)))
```

**(image-filter-name _filter_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the localized display name of the image _filter_ as a string.

**(image-filter-implementation _filter-or-id_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(image-filter-implementation _filter-or-id raw?_)**  

Returns the filter implementation identifier of the image filter _filter-or-id_. If _raw?_ is `#t`, returns the Core Image internal name as a string; otherwise returns the symbolic identifier. _filter-or-id_ is either an image filter, a symbolic image filter identifier, or a name of a Core Image filter as a string.

**(image-filter-description _filter_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the localized description of the image _filter_ as a string, or `#f` if no description is available.

**(image-filter-categories _filter_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(image-filter-categories _filter raw?_)**  

Returns a list of categories that the image _filter_ belongs to. If _raw?_ is `#t`, returns the raw string names; otherwise returns symbolic identifiers.

**(image-filter-available _filter_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns availability information as two string values _mac-version_ and _ios-version_ indicating the minimum OS versions where the image _filter_ is available.

**(image-filter-inputs _filter_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(image-filter-inputs _filter raw?_)**  

Returns a list of input argument names for the given image _filter_.

**(image-filter-outputs _filter_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(image-filter-outputs _filter raw?_)**  

Returns a list of output argument names for the filter.

**(image-filter-output _filter_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the primary output image of the filter as an abstract image, or `#f` if no output is available.

**(image-filter-argument _filter key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>    

Returns metadata about the filter argument _key_ for the given image _filter_ as an association list. Keys include:

- `name`: Name of the argument.
- `display-name`: Human-readable argument name.
- `identity`: An identifier for the argument.
- `description`: Human-readable argument description.
- `reference-documentation`: Reference to documentation.
- `class`: Objective C/Swift class name for representing values of this attribute (e.g. `"NSNumber"`).
- `type`: Argument type. Supported are: `time`, `scalar`, `distance`, `angle`, `boolean`, `integer`, `count`, `color`, `opaque-color`, `gradient`, `point`, `offset`, `coordinate-3d`, `rect`, `abstract-image`, `transformation`, `date`, `styled-text`, `string`, `number`, `bytevector`, `image-coefficients`, `array`, and `color-space`.
- `default`: Default value
- `min`, `max`: Value range limits
- `slider-min`, `slider-max`: UI slider range

**(image-filter-argument-ref _filter key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(image-filter-argument-ref _filter key default_)**  

Returns the current value of the given image _filter_ argument _key_, or _default_ if the argument is not set.

**(image-filter-argument-set! _filter key value_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the value of the specified image _filter_ argument _key_ to _value_. This is a mutating operation.

```scheme
(define blur (make-image-filter 'gaussian-blur))
(image-filter-argument-set! blur 'input-radius 10.0)
(image-filter-argument-ref blur 'input-radius)  ⇒  10.0
```

## Image Coefficients

Image filters have attributes, which are key value pairs associated with image filter objects. Attributes have types, which can be determined by using the `image-filter-argument` procedure and extracting the `type` property. Here is an example:

```scheme
(define f (make-image-filter 'color-cross-polynomial))
(image-filter-inputs f)
⇒  (input-image input-red-coefficients input-green-coefficients input-blue-coefficients)
(cdr (assoc 'type (image-filter-argument f 'input-red-coefficients)))
⇒  image-coefficients
```

Filter attributes of type `image-coefficients` are represented by `image-coefficients` objects.

**(image-coefficients _arg ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates an image coefficients object from numeric values and sequences of numeric values _arg ..._. Arguments can be numbers, lists of numbers, or vectors of numbers, which are sequentially flattened into a single coefficient vector within a image coefficients object.

```scheme
(image-coefficients 1.0 2.0 3.0)       ⇒  #<image-coefficients 98c840750 3:  1.0,  2.0,  3.0>
(image-coefficients '(1.0 2.0) '(3 4)) ⇒  #<image-coefficients 98c8407b0 4:  1.0,  2.0,  3.0,  4.0>
(image-coefficients #(1 2) '(3) 4)     ⇒  #<image-coefficients 98c8407e0 4:  1.0,  2.0,  3.0,  4.0>
(image-coefficients (rect 1 2 3 4))    ⇒  #<image-coefficients 98c847ae0 4:  1.0,  2.0,  3.0,  4.0>
```

**(image-coefficients->vector _coeffs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Converts image coefficients _coeffs_ to a vector of flonums.

**(image-coefficients->point _coeffs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Interprets the first two coefficient values `x` and `y` as a point, returning `(x . y)`. Returns `#f` if fewer than two coefficients _coeffs_ are available.

**(image-coefficients->rect _coeffs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Interprets the first four coefficient values `x`, `y`, `width`, and `hight` as a rectangle, returning `((x . y) . (width . height))`. Returns `#f` if fewer than four coefficients are available.

```scheme
(define r (rect '(1 . 2) '(3 . 4)))
r  ⇒  ((1.0 . 2.0) 3.0 . 4.0)
(define c (image-coefficients r))
c  ⇒  #<image-coefficients 98c847f60 4:  1.0,  2.0,  3.0,  4.0>
(image-coefficients->rect c)
⇒  ((1.0 . 2.0) 3.0 . 4.0)
```

## Image Processing Pipeline

**(apply-image-filter _aimage filter ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies a sequence of filters to an abstract image _aimage_, creating an image processing pipeline and returning the resulting abstract image of `#f` if processing fails. Each argument _filter_ is either a filter specifier or a list of filter specifiers. The following two forms of filter specifiers are supported:

- _image filter object_: A configured image filter that will be applied as is.
- `(filter-identifier (arg1 . value) ...)`: A list whose first element identifies the image filter followed by pairs defining image filter attributes. The image filter identifier is either a symbol or a string.

```scheme
;; Apply gaussian blur with radius 5.0
(apply-image-filter img '(gaussian-blur (input-radius . 5.0)))
⇒  #<abstract-image 9eef6c280: 0×0>

;; Apply multiple filters in sequence
(apply-image-filter img
  '(gaussian-blur (input-radius . 2.0))
  '(color-controls (input-brightness . 0.1) (input-contrast . 1.2))
  (make-image-filter 'vignette))
⇒  #<abstract-image 782434020: 0×0>
```

## Coordinate Mapping

**(map-image-point _pnt image_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Maps the coordinates of a point _pnt_ on _image_ (using points as units) to pixel coordinates on a corresponding abstract image. The _point_ should be in the format `(x . y)`.

**(map-image-rect _rect image_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Maps the representation of a rectangle _rect_ on _image_ (using points as units) to pixel coordinates on a corresponding abstract image. The _rect_ should be in the format `((x . y) . (width . height))`.
