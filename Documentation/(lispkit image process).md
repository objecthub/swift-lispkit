# LispKit Image Process

Library `(lispkit image process)` defines a high-level API for all the image filters provided by the _Core Image_ framework of iOS and macOS. As opposed to the generic, imperative interface as implemented by library `(lispkit image)`, the API of `(lispkit image process)` is functional: The main building blocks are _image processors_ which transform _abstract images_. Since _image processors_ are simply functions, they can be composed easily with existing functional composition operators, e.g. as provided by library `(lispkit combinator)`.


## Image processors

**(make-filter-proc _filter_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-filter-proc _filters_)**  

Creates an image processor for a list of configured filters that are being applied in sequence. Creates an image processor for a given configured filter.

**(filter-pipeline _proc ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates an _image filter pipeline_ from the given image processors _proc ..._. An image filter pipeline is a composite image processor that executes the encapsulated image processors in sequence, piping the output of a processor into the input of the next processor. The result of the final processor is returned by an image filter pipeline.


## Image generator implementations

**(attributed-text-image-generator _text scale-factor padding_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `attributed-text-image-generator` (`CIAttributedTextImageGenerator`).
Generate an image attributed string.

- _text_ (`styled-text/NSAttributedString`): The attributed text to render.
- _scale-factor_ (`scalar/NSNumber`): The scale of the font to use for the generated text.
- _padding_ (`integer/NSNumber`): A value for an additional number of pixels to pad around the text’s bounding box.

Filter categories: `builtin`, `video`, `generator`, `still-image`

**(aztec-code-generator _message correction-level layers compact-style_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `aztec-code-generator` (`CIAztecCodeGenerator`).
Generate an Aztec barcode image for message data.

- _message_ (`bytevector/NSData`): The message to encode in the Aztec Barcode
- _correction-level_ (`integer/NSNumber`): Aztec error correction value between 5 and 95
- _layers_ (`integer/NSNumber`): Aztec layers value between 1 and 32. Set to nil for automatic.
- _compact-style_ (`boolean/NSNumber`): Force a compact style Aztec code to @YES or @NO. Set to nil for automatic.

Filter categories: `builtin`, `generator`, `still-image`

**(blurred-rectangle-generator _extent sigma color_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `blurred-rectangle-generator` (`CIBlurredRectangleGenerator`).
Generates a blurred rectangle image with the specified extent, blur sigma, and color.

- _extent_ (`rect/CIVector`): A rectangle that defines the extent of the effect.
- _sigma_ (`distance/NSNumber`): The sigma for a gaussian blur.
- _color_ (`color/CIColor`): A color.

Filter categories: `builtin`, `high-dynamic-range`, `generator`, `still-image`

**(checkerboard-generator _center color0 color1 width sharpness_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `checkerboard-generator` (`CICheckerboardGenerator`).
Generates a pattern of squares of alternating colors. You can specify the size, colors, and the sharpness of the pattern.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _color0_ (`color/CIColor`): A color to use for the first set of squares.
- _color1_ (`color/CIColor`): A color to use for the second set of squares.
- _width_ (`distance/NSNumber`): The width of the squares in the pattern.
- _sharpness_ (`scalar/NSNumber`): The sharpness of the edges in pattern. The smaller the value, the more blurry the pattern. Values range from 0.0 to 1.0.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `generator`, `still-image`

**(code128-barcode-generator _message quiet-space barcode-height_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `code128-barcode-generator` (`CICode128BarcodeGenerator`).
Generate a Code 128 barcode image for message data.

- _message_ (`bytevector/NSData`): The message to encode in the Code 128 Barcode
- _quiet-space_ (`integer/NSNumber`): The number of empty white pixels that should surround the barcode.
- _barcode-height_ (`integer/NSNumber`): The height of the generated barcode in pixels.

Filter categories: `builtin`, `generator`, `still-image`

**(constant-color-generator _color_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `constant-color-generator` (`CIConstantColorGenerator`).
Generates a solid color. You typically use the output of this filter as the input to another filter.

- _color_ (`color/CIColor`): The color to generate.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `generator`, `still-image`

**(lenticular-halo-generator _center color halo-radius halo-width halo-overlap striation-strength striation-contrast time_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `lenticular-halo-generator` (`CILenticularHaloGenerator`).
Simulates a halo that is generated by the diffraction associated with the spread of a lens. This filter is typically applied to another image to simulate lens flares and similar effects.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _color_ (`color/CIColor`): A color.
- _halo-radius_ (`distance/NSNumber`): The radius of the halo.
- _halo-width_ (`distance/NSNumber`): The width of the halo, from its inner radius to its outer radius.
- _halo-overlap_ (`scalar/NSNumber`)
- _striation-strength_ (`scalar/NSNumber`): The intensity of the halo colors. Larger values are more intense.
- _striation-contrast_ (`scalar/NSNumber`): The contrast of the halo colors. Larger values are higher contrast.
- _time_ (`scalar/NSNumber`): The duration of the effect.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `generator`, `still-image`

**(mesh-generator _width color mesh_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `mesh-generator` (`CIMeshGenerator`).
Generates a mesh from an array of line segments.

- _width_ (`distance/NSNumber`): The width in pixels of the effect.
- _color_ (`color/CIColor`): A color.
- _mesh_ (`array/NSArray`): An array of line segments stored as an array of CIVectors each containing a start point and end point.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `generator`, `still-image`

**(pdf417-barcode-generator _message min-width max-width min-height max-height data-columns rows preferred-aspect-ratio compaction-mode compact-style correction-level always-specify-compaction_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `pdf417-barcode-generator` (`CIPDF417BarcodeGenerator`).
Generate a PDF417 barcode image for message data.

- _message_ (`bytevector/NSData`): The message to encode in the PDF417 Barcode
- _min-width_ (`integer/NSNumber`): The minimum width of the generated barcode in pixels.
- _max-width_ (`integer/NSNumber`): The maximum width of the generated barcode in pixels.
- _min-height_ (`integer/NSNumber`): The minimum height of the generated barcode in pixels.
- _max-height_ (`integer/NSNumber`): The maximum height of the generated barcode in pixels.
- _data-columns_ (`integer/NSNumber`): The number of data columns in the generated barcode
- _rows_ (`integer/NSNumber`): The number of rows in the generated barcode
- _preferred-aspect-ratio_ (`number/NSNumber`): The preferred aspect ratio of the generated barcode
- _compaction-mode_ (`integer/NSNumber`): The compaction mode of the generated barcode.
- _compact-style_ (`boolean/NSNumber`): Force a compact style Aztec code to @YES or @NO. Set to nil for automatic.
- _correction-level_ (`integer/NSNumber`): The correction level ratio of the generated barcode
- _always-specify-compaction_ (`boolean/NSNumber`): Force compaction style to @YES or @NO. Set to nil for automatic.

Filter categories: `builtin`, `video`, `generator`, `still-image`

**(qrcode-generator _message correction-level_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `qrcode-generator` (`CIQRCodeGenerator`).
Generate a QR Code image for message data.

- _message_ (`bytevector/NSData`): The message to encode in the QR Code
- _correction-level_ (`string/NSString`): QR Code correction level L, M, Q, or H.

Filter categories: `builtin`, `generator`, `still-image`

**(random-generator)**

Returns an image generator for image filter `random-generator` (`CIRandomGenerator`).
Generates an image of infinite extent whose pixel values are made up of four independent, uniformly-distributed random numbers in the 0 to 1 range.

Filter categories: `builtin`, `video`, `generator`, `still-image`

**(rounded-rectangle-generator _extent radius color_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `rounded-rectangle-generator` (`CIRoundedRectangleGenerator`).
Generates a rounded rectangle image with the specified extent, corner radius, and color.

- _extent_ (`rect/CIVector`): A rectangle that defines the extent of the effect.
- _radius_ (`distance/NSNumber`): The distance from the center of the effect.
- _color_ (`color/CIColor`): A color.

Filter categories: `builtin`, `high-dynamic-range`, `generator`, `still-image`

**(rounded-rectangle-stroke-generator _extent radius color width_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `rounded-rectangle-stroke-generator` (`CIRoundedRectangleStrokeGenerator`).
Generates a rounded rectangle stroke image with the specified extent, corner radius, stroke width, and color.

- _extent_ (`rect/CIVector`): A rectangle that defines the extent of the effect.
- _radius_ (`distance/NSNumber`): The distance from the center of the effect.
- _color_ (`color/CIColor`): A color.
- _width_ (`distance/NSNumber`): The width in pixels of the effect.

Filter categories: `builtin`, `high-dynamic-range`, `generator`, `still-image`

**(star-shine-generator _center color radius cross-scale cross-angle cross-opacity cross-width epsilon_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `star-shine-generator` (`CIStarShineGenerator`).
Generates a starburst pattern. The output image is typically used as input to another filter.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _color_ (`color/CIColor`): The color to use for the outer shell of the circular star.
- _radius_ (`distance/NSNumber`): The radius of the star.
- _cross-scale_ (`scalar/NSNumber`): The size of the cross pattern.
- _cross-angle_ (`angle/NSNumber`): The angle in radians of the cross pattern.
- _cross-opacity_ (`scalar/NSNumber`): The opacity of the cross pattern.
- _cross-width_ (`distance/NSNumber`): The width of the cross pattern.
- _epsilon_ (`scalar/NSNumber`): The length of the cross spikes.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `generator`, `still-image`

**(stripes-generator _center color0 color1 width sharpness_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `stripes-generator` (`CIStripesGenerator`).
Generates a stripe pattern. You can control the color of the stripes, the spacing, and the contrast.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _color0_ (`color/CIColor`): A color to use for the odd stripes.
- _color1_ (`color/CIColor`): A color to use for the even stripes.
- _width_ (`distance/NSNumber`): The width of a stripe.
- _sharpness_ (`scalar/NSNumber`): The sharpness of the stripe pattern. The smaller the value, the more blurry the pattern. Values range from 0.0 to 1.0.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `generator`, `still-image`

**(sunbeams-generator _center color sun-radius max-striation-radius striation-strength striation-contrast time_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `sunbeams-generator` (`CISunbeamsGenerator`).
Generates a sun effect. You typically use the output of the sunbeams filter as input to a composite filter.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _color_ (`color/CIColor`): The color of the sun.
- _sun-radius_ (`distance/NSNumber`): The radius of the sun.
- _max-striation-radius_ (`scalar/NSNumber`): The radius of the sunbeams.
- _striation-strength_ (`scalar/NSNumber`): The intensity of the sunbeams. Higher values result in more intensity.
- _striation-contrast_ (`scalar/NSNumber`): The contrast of the sunbeams. Higher values result in more contrast.
- _time_ (`scalar/NSNumber`): The duration of the effect.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `generator`, `still-image`

**(text-image-generator _text font-name font-size scale-factor padding_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `text-image-generator` (`CITextImageGenerator`).
Generate an image from a string and font information.

- _text_ (`string/NSString`): The text to render.
- _font-name_ (`string/NSString`): The name of the font to use for the generated text.
- _font-size_ (`scalar/NSNumber`): The size of the font to use for the generated text.
- _scale-factor_ (`scalar/NSNumber`): The scale of the font to use for the generated text.
- _padding_ (`integer/NSNumber`): The number of additional pixels to pad around the text’s bounding box.

Filter categories: `builtin`, `video`, `generator`, `still-image`


## Image processor implementations

**(accordion-fold-transition _target-image bottom-height number-of-folds fold-shadow-amount time_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `accordion-fold-transition` (`CIAccordionFoldTransition`).
Transitions from one image to another of a differing dimensions by unfolding.

- _target-image_ (`abstract-image/CIImage`): The target image for a transition.
- _bottom-height_ (`distance/NSNumber`): The height in pixels from the bottom of the image to the bottom of the folded part of the transition.
- _number-of-folds_ (`scalar/NSNumber`): The number of folds used in the transition.
- _fold-shadow-amount_ (`scalar/NSNumber`): A value that specifies the intensity of the shadow in the transition.
- _time_ (`time/NSNumber`): The duration of the effect.

Filter categories: `builtin`, `video`, `transition`, `high-dynamic-range`, `still-image`

**(addition-compositing _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `addition-compositing` (`CIAdditionCompositing`).
Adds color components to achieve a brightening effect. This filter is typically used to add highlights and lens flare effects.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `high-dynamic-range`, `interlaced`, `still-image`, `non-square-pixels`

**(affine-clamp _transform_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `affine-clamp` (`CIAffineClamp`).
Performs an affine transformation on a source image and then clamps the pixels at the edge of the transformed image, extending them outwards. This filter performs similarly to the “Affine Transform” filter except that it produces an image with infinite extent. You can use this filter when you need to blur an image but you want to avoid a soft, black fringe along the edges.

- _transform_ (`transformation/NSAffineTransform`): The transform to apply to the image.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(affine-tile _transform_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `affine-tile` (`CIAffineTile`).
Applies an affine transformation to an image and then tiles the transformed image.

- _transform_ (`transformation/NSAffineTransform`): The transform to apply to the image.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(affine-transform _transform_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `affine-transform` (`CIAffineTransform`).
Applies an affine transformation to an image. You can scale, translate, or rotate the input image. You can also apply a combination of these operations.

- _transform_ (`transformation/NSAffineTransform`): A transform to apply to the image.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `still-image`, `geometry-adjustment`

**(area-alpha-weighted-histogram _extent scale count_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `area-alpha-weighted-histogram` (`CIAreaAlphaWeightedHistogram`).
Calculates alpha-weighted histograms of the unpremultiplied R, G, B channels for the specified area of an image. The output image is a one pixel tall image containing the histogram data for the RGB channels.

- _extent_ (`rect/CIVector`): A rectangle that defines the extent of the effect.
- _scale_ (`scalar/NSNumber`): The scale value to use for the histogram values. If the scale is 1.0 and the image is opaque, then the bins in the resulting image will add up to 1.0.
- _count_ (`scalar/NSNumber`): The number of bins for the histogram. This value will determine the width of the output image.

Filter categories: `builtin`, `video`, `reduction`, `still-image`

**(area-average _extent_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `area-average` (`CIAreaAverage`).
Calculates the average color for the specified area in an image, returning the result in a pixel.

- _extent_ (`rect/CIVector`): A rectangle that specifies the subregion of the image that you want to process.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `reduction`, `still-image`

**(area-bounds-red _extent_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `area-bounds-red` (`CIAreaBoundsRed`).
Calculates the approximate bounding box of pixels within the specified area of an image where the red component values are non-zero. The result is 1x1 pixel image where the RGBA values contain the normalized X,Y,W,H dimensions of the bounding box.

- _extent_ (`rect/CIVector`): A rectangle that specifies the subregion of the image that you want to process.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `reduction`, `still-image`

**(area-histogram _extent scale count_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `area-histogram` (`CIAreaHistogram`).
Calculates histograms of the R, G, B, and A channels of the specified area of an image. The output image is a one pixel tall image containing the histogram data for all four channels.

- _extent_ (`rect/CIVector`): A rectangle that, after intersection with the image extent, specifies the subregion of the image that you want to process.
- _scale_ (`scalar/NSNumber`): The scale value to use for the histogram values. If the scale is 1.0, then the bins in the resulting image will add up to 1.0.
- _count_ (`scalar/NSNumber`): The number of bins for the histogram. This value will determine the width of the output image.

Filter categories: `builtin`, `video`, `reduction`, `still-image`

**(area-logarithmic-histogram _extent scale count minimum-stop maximum-stop_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `area-logarithmic-histogram` (`CIAreaLogarithmicHistogram`).
Calculates histogram of the R, G, B, and A channels of the specified area of an image. Before binning, the R, G, and B channel values are transformed by the log base two function. The output image is a one pixel tall image containing the histogram data for all four channels.

- _extent_ (`rect/CIVector`): A rectangle that defines the extent of the effect.
- _scale_ (`scalar/NSNumber`): The amount of the effect.
- _count_ (`scalar/NSNumber`): The number of bins for the histogram. This value will determine the width of the output image.
- _minimum-stop_ (`scalar/NSNumber`): The minimum of the range of color channel values to be in the logarithmic histogram image.
- _maximum-stop_ (`scalar/NSNumber`): The maximum of the range of color channel values to be in the logarithmic histogram image.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `reduction`, `still-image`

**(area-maximum _extent_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `area-maximum` (`CIAreaMaximum`).
Calculates the maximum component values for the specified area in an image, returning the result in a pixel.

- _extent_ (`rect/CIVector`): A rectangle that specifies the subregion of the image that you want to process.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `reduction`, `still-image`

**(area-maximum-alpha _extent_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `area-maximum-alpha` (`CIAreaMaximumAlpha`).
Finds and returns the pixel with the maximum alpha value.

- _extent_ (`rect/CIVector`): A rectangle that specifies the subregion of the image that you want to process.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `reduction`, `still-image`

**(area-minimum _extent_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `area-minimum` (`CIAreaMinimum`).
Calculates the minimum component values for the specified area in an image, returning the result in a pixel.

- _extent_ (`rect/CIVector`): A rectangle that specifies the subregion of the image that you want to process.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `reduction`, `still-image`

**(area-minimum-alpha _extent_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `area-minimum-alpha` (`CIAreaMinimumAlpha`).
Finds and returns the pixel with the minimum alpha value.

- _extent_ (`rect/CIVector`): A rectangle that specifies the subregion of the image that you want to process.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `reduction`, `still-image`

**(area-min-max _extent_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `area-min-max` (`CIAreaMinMax`).
Calculates the per-component minimum and maximum value for the specified area in an image. The result is returned in a 2x1 image where the component minimum values are stored in the pixel on the left.

- _extent_ (`rect/CIVector`): A rectangle that specifies the subregion of the image that you want to process.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `reduction`, `still-image`

**(area-min-max-red _extent_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `area-min-max-red` (`CIAreaMinMaxRed`).
Calculates the minimum and maximum red component value for the specified area in an image. The result is returned in the red and green channels of a one pixel image.

- _extent_ (`rect/CIVector`): A rectangle that specifies the subregion of the image that you want to process.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `reduction`, `still-image`

**(bars-swipe-transition _target-image angle width bar-offset time_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `bars-swipe-transition` (`CIBarsSwipeTransition`).
Transitions from one image to another by swiping rectangular portions of the foreground image to disclose the target image.

- _target-image_ (`abstract-image/CIImage`): The target image for a transition.
- _angle_ (`angle/NSNumber`): The angle in radians of the bars.
- _width_ (`distance/NSNumber`): The width of each bar.
- _bar-offset_ (`scalar/NSNumber`): The offset of one bar with respect to another.
- _time_ (`time/NSNumber`): The parametric time of the transition. This value drives the transition from start (at time 0) to end (at time 1).

Filter categories: `builtin`, `video`, `transition`, `high-dynamic-range`, `still-image`

**(bicubic-scale-transform _scale aspect-ratio b c_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `bicubic-scale-transform` (`CIBicubicScaleTransform`).
Produces a high-quality, scaled version of a source image. The parameters of B and C for this filter determine the sharpness or softness of the resampling. The most commonly used B and C values are 0.0 and 0.75, respectively.

- _scale_ (`scalar/NSNumber`): The scaling factor to use on the image. Values less than 1.0 scale down the images. Values greater than 1.0 scale up the image.
- _aspect-ratio_ (`scalar/NSNumber`): The additional horizontal scaling factor to use on the image.
- _b_ (`scalar/NSNumber`): Specifies the value of B to use for the cubic resampling function.
- _c_ (`scalar/NSNumber`): Specifies the value of C to use for the cubic resampling function.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `still-image`, `non-square-pixels`, `geometry-adjustment`

**(blend-with-alpha-mask _background-image mask-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `blend-with-alpha-mask` (`CIBlendWithAlphaMask`).
Uses values from a mask image to interpolate between an image and the background. When a mask alpha value is 0.0, the result is the background. When the mask alpha value is 1.0, the result is the image.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.
- _mask-image_ (`abstract-image/CIImage`): A masking image.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(blend-with-blue-mask _background-image mask-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `blend-with-blue-mask` (`CIBlendWithBlueMask`).
Uses values from a mask image to interpolate between an image and the background. When a mask blue value is 0.0, the result is the background. When the mask blue value is 1.0, the result is the image.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.
- _mask-image_ (`abstract-image/CIImage`): A masking image.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(blend-with-mask _background-image mask-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `blend-with-mask` (`CIBlendWithMask`).
Uses values from a grayscale mask to interpolate between an image and the background. When a mask green value is 0.0, the result is the background. When the mask green value is 1.0, the result is the image.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.
- _mask-image_ (`abstract-image/CIImage`): A grayscale mask. When a mask value is 0.0, the result is the background. When the mask value is 1.0, the result is the image.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(blend-with-red-mask _background-image mask-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `blend-with-red-mask` (`CIBlendWithRedMask`).
Uses values from a mask image to interpolate between an image and the background. When a mask red value is 0.0, the result is the background. When the mask red value is 1.0, the result is the image.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.
- _mask-image_ (`abstract-image/CIImage`): A masking image.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(bloom _radius intensity_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `bloom` (`CIBloom`).
Softens edges and applies a pleasant glow to an image.

- _radius_ (`distance/NSNumber`): The radius determines how many pixels are used to create the effect. The larger the radius, the greater the effect.
- _intensity_ (`scalar/NSNumber`): The intensity of the effect. A value of 0.0 is no effect. A value of 1.0 is the maximum effect.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(bokeh-blur _radius ring-amount ring-size softness_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `bokeh-blur` (`CIBokehBlur`).
Smooths an image using a disc-shaped convolution kernel.

- _radius_ (`distance/NSNumber`): The radius determines how many pixels are used to create the blur. The larger the radius, the blurrier the result.
- _ring-amount_ (`scalar/NSNumber`): The amount of extra emphasis at the ring of the bokeh.
- _ring-size_ (`scalar/NSNumber`): The size of extra emphasis at the ring of the bokeh.
- _softness_ (`scalar/NSNumber`)

Filter categories: `builtin`, `blur`, `video`, `high-dynamic-range`, `still-image`

**(box-blur _radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `box-blur` (`CIBoxBlur`).
Smooths or sharpens an image using a box-shaped convolution kernel.

- _radius_ (`distance/NSNumber`): The radius determines how many pixels are used to create the blur. The larger the radius, the blurrier the result.

Filter categories: `builtin`, `blur`, `video`, `high-dynamic-range`, `still-image`

**(bump-distortion _center radius scale_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `bump-distortion` (`CIBumpDistortion`).
Creates a concave or convex bump that originates at a specified point in the image.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _radius_ (`distance/NSNumber`): The radius determines how many pixels are used to create the distortion. The larger the radius, the wider the extent of the distortion.
- _scale_ (`scalar/NSNumber`): The scale of the effect determines the curvature of the bump. A value of 0.0 has no effect. Positive values create an outward bump; negative values create an inward bump.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(bump-distortion-linear _center radius angle scale_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `bump-distortion-linear` (`CIBumpDistortionLinear`).
Creates a bump that originates from a linear portion of the image.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _radius_ (`distance/NSNumber`): The radius determines how many pixels are used to create the distortion. The larger the radius, the wider the extent of the distortion.
- _angle_ (`angle/NSNumber`): The angle in radians of the line around which the distortion occurs.
- _scale_ (`scalar/NSNumber`): The scale of the effect.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(canny-edge-detector _gaussian-sigma perceptual threshold-high threshold-low hysteresis-passes_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `canny-edge-detector` (`CICannyEdgeDetector`).
Applies the Canny Edge Detection algorithm to an image.

- _gaussian-sigma_ (`scalar/NSNumber`): The gaussian sigma of blur to apply to the image to reduce high-frequency noise.
- _perceptual_ (`boolean/NSNumber`): Specifies whether the edge thresholds should be computed in a perceptual color space.
- _threshold-high_ (`scalar/NSNumber`): The threshold that determines if gradient magnitude is a strong edge.
- _threshold-low_ (`scalar/NSNumber`): The threshold that determines if gradient magnitude is a weak edge.
- _hysteresis-passes_ (`integer/NSNumber`): The number of hysteresis passes to apply to promote weak edge pixels.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(circle-splash-distortion _center radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `circle-splash-distortion` (`CICircleSplashDistortion`).
Distorts the pixels starting at the circumference of a circle and emanating outward.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _radius_ (`distance/NSNumber`): The radius determines how many pixels are used to create the distortion. The larger the radius, the wider the extent of the distortion.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(circular-screen _center width sharpness_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `circular-screen` (`CICircularScreen`).
Simulates a circular-shaped halftone screen.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _width_ (`distance/NSNumber`): The distance between each circle in the pattern.
- _sharpness_ (`scalar/NSNumber`): The sharpness of the circles. The larger the value, the sharper the circles.

Filter categories: `builtin`, `video`, `halftone-effect`, `still-image`

**(circular-wrap _center radius angle_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `circular-wrap` (`CICircularWrap`).
Wraps an image around a transparent circle. The distortion of the image increases with the distance from the center of the circle.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _radius_ (`distance/NSNumber`): The radius determines how many pixels are used to create the distortion. The larger the radius, the wider the extent of the distortion.
- _angle_ (`angle/NSNumber`): The angle in radians of the effect.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(clamp _extent_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `clamp` (`CIClamp`).
Clamps an image so the pixels with the specified extent are left unchanged but those at the boundary of the extent are extended outwards. This filter produces an image with infinite extent. You can use this filter when you need to blur an image but you want to avoid a soft, black fringe along the edges.

- _extent_ (`rect/CIVector`): A rectangle that defines the extent of the effect.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(cmyk-halftone _center width angle sharpness g-c-r u-c-r_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `cmyk-halftone` (`CICMYKHalftone`).
Creates a color, halftoned rendition of the source image, using cyan, magenta, yellow, and black inks over a white page.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _width_ (`distance/NSNumber`): The distance between dots in the pattern.
- _angle_ (`angle/NSNumber`): The angle in radians of the pattern.
- _sharpness_ (`distance/NSNumber`): The sharpness of the pattern. The larger the value, the sharper the pattern.
- _g-c-r_ (`scalar/NSNumber`): The gray component replacement value. The value can vary from 0.0 (none) to 1.0.
- _u-c-r_ (`scalar/NSNumber`): The under color removal value. The value can vary from 0.0 to 1.0. 

Filter categories: `builtin`, `video`, `halftone-effect`, `still-image`

**(color-absolute-difference _image2_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-absolute-difference` (`CIColorAbsoluteDifference`).
Produces an image that is the absolute value of the color difference between two images. The alpha channel of the result will be the product of the two image alpha channels.

- _image2_ (`abstract-image/CIImage`): The second input image for differencing.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(color-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-blend-mode` (`CIColorBlendMode`).
Uses the luminance values of the background with the hue and saturation values of the source image. This mode preserves the gray levels in the image.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(color-burn-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-burn-blend-mode` (`CIColorBurnBlendMode`).
Darkens the background image samples to reflect the source image samples. Source image sample values that specify white do not produce a change.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(color-clamp _min-components max-components_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-clamp` (`CIColorClamp`).
Clamp color to a certain range.

- _min-components_ (`image-coefficients/CIVector`): Lower clamping values.
- _max-components_ (`image-coefficients/CIVector`): Higher clamping values.

Filter categories: `builtin`, `video`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(color-controls _saturation brightness contrast_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-controls` (`CIColorControls`).
Adjusts saturation, brightness, and contrast values.

- _saturation_ (`scalar/NSNumber`): The amount of saturation to apply. The larger the value, the more saturated the result.
- _brightness_ (`scalar/NSNumber`): The amount of brightness to apply. The larger the value, the brighter the result.
- _contrast_ (`scalar/NSNumber`): The amount of contrast to apply. The larger the value, the more contrast in the resulting image.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(color-cross-polynomial _red-coefficients green-coefficients blue-coefficients_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-cross-polynomial` (`CIColorCrossPolynomial`).
Adjusts the color of an image with polynomials.

- _red-coefficients_ (`image-coefficients/CIVector`): Polynomial coefficients for red channel.
- _green-coefficients_ (`image-coefficients/CIVector`): Polynomial coefficients for green channel.
- _blue-coefficients_ (`image-coefficients/CIVector`): Polynomial coefficients for blue channel.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(color-cube _cube-dimension cube-data extrapolate_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-cube` (`CIColorCube`).
Uses a three-dimensional color table to transform the source image pixels.

- _cube-dimension_ (`count/NSNumber`): The dimension of the color cube.
- _cube-data_ (`bytevector/NSData`): Data containing a 3-dimensional color table of floating-point premultiplied RGBA values. The cells are organized in a standard ordering. The columns and rows of the data are indexed by red and green, respectively. Each data plane is followed by the next higher plane in the data, with planes indexed by blue.
- _extrapolate_ (`boolean/NSNumber`): If true, then the color cube will be extrapolated if the input image contains RGB component values outside the range 0.0 to 1.0.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(color-cubes-mixed-with-mask _mask-image cube-dimension cube0-data cube1-data color-space extrapolate_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-cubes-mixed-with-mask` (`CIColorCubesMixedWithMask`).
Uses two three-dimensional color tables in a specified colorspace to transform the source image pixels. The mask image is used as an interpolant to mix the output of the two cubes.

- _mask-image_ (`abstract-image/CIImage`): A masking image.
- _cube-dimension_ (`count/NSNumber`): The dimension of the color cubes.
- _cube0-data_ (`bytevector/NSData`): Data containing a 3-dimensional color table of floating-point premultiplied RGBA values. The cells are organized in a standard ordering. The columns and rows of the data are indexed by red and green, respectively. Each data plane is followed by the next higher plane in the data, with planes indexed by blue.
- _cube1-data_ (`bytevector/NSData`): Data containing a 3-dimensional color table of floating-point premultiplied RGBA values. The cells are organized in a standard ordering. The columns and rows of the data are indexed by red and green, respectively. Each data plane is followed by the next higher plane in the data, with planes indexed by blue.
- _color-space_ (`color-space/NSObject`): The CGColorSpace that defines the RGB values in the color table.
- _extrapolate_ (`boolean/NSNumber`): If true, then the color cube will be extrapolated if the input image contains RGB component values outside the range 0 to 1.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(color-cube-with-color-space _cube-dimension cube-data extrapolate color-space_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-cube-with-color-space` (`CIColorCubeWithColorSpace`).
Uses a three-dimensional color table in a specified colorspace to transform the source image pixels.

- _cube-dimension_ (`count/NSNumber`): The dimension of the color cube.
- _cube-data_ (`bytevector/NSData`): Data containing a 3-dimensional color table of floating-point premultiplied RGBA values. The cells are organized in a standard ordering. The columns and rows of the data are indexed by red and green, respectively. Each data plane is followed by the next higher plane in the data, with planes indexed by blue.
- _extrapolate_ (`number/NSNumber`): If true, then the color cube will be extrapolated if the input image contains RGB component values outside the range 0.0 to 1.0.
- _color-space_ (`color-space/NSObject`): The CGColorSpace that defines the RGB values in the color table.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(color-curves _curves-data curves-domain color-space_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-curves` (`CIColorCurves`).
Uses a three-channel one-dimensional color table to transform the source image pixels.

- _curves-data_ (`bytevector/NSData`): Data containing a color table of floating-point RGB values.
- _curves-domain_ (`image-coefficients/CIVector`): A two-element vector that defines the minimum and maximum RGB component values that are used to look up result values from the color table.
- _color-space_ (`color-space/NSObject`): The CGColorSpace that defines the RGB values in the color table.

Filter categories: `builtin`, `video`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(color-dodge-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-dodge-blend-mode` (`CIColorDodgeBlendMode`).
Brightens the background image samples to reflect the source image samples. Source image sample values that specify black do not produce a change.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(color-invert)**

Returns an image processor for image filter `color-invert` (`CIColorInvert`).
Inverts the colors in an image.


Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(color-map _gradient-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-map` (`CIColorMap`).
Performs a nonlinear transformation of source color values using mapping values provided in a table.

- _gradient-image_ (`gradient/CIImage`): The image data from this image transforms the source image values.

Filter categories: `builtin`, `video`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(color-matrix _r-vector g-vector b-vector a-vector bias-vector_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-matrix` (`CIColorMatrix`).
Multiplies source color values and adds a bias factor to each color component.

- _r-vector_ (`image-coefficients/CIVector`): The amount of red to multiply the source color values by.
- _g-vector_ (`image-coefficients/CIVector`): The amount of green to multiply the source color values by.
- _b-vector_ (`image-coefficients/CIVector`): The amount of blue to multiply the source color values by.
- _a-vector_ (`image-coefficients/CIVector`): The amount of alpha to multiply the source color values by.
- _bias-vector_ (`image-coefficients/CIVector`): A vector that’s added to each color component.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(color-monochrome _color intensity_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-monochrome` (`CIColorMonochrome`).
Remaps colors so they fall within shades of a single color.

- _color_ (`opaque-color/CIColor`): The monochrome color to apply to the image.
- _intensity_ (`scalar/NSNumber`): The intensity of the monochrome effect. A value of 1.0 creates a monochrome image using the supplied color. A value of 0.0 has no effect on the image.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(color-polynomial _red-coefficients green-coefficients blue-coefficients alpha-coefficients_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-polynomial` (`CIColorPolynomial`).
Adjusts the color of an image with polynomials.

- _red-coefficients_ (`image-coefficients/CIVector`): Polynomial coefficients for red channel.
- _green-coefficients_ (`image-coefficients/CIVector`): Polynomial coefficients for green channel.
- _blue-coefficients_ (`image-coefficients/CIVector`): Polynomial coefficients for blue channel.
- _alpha-coefficients_ (`image-coefficients/CIVector`): Polynomial coefficients for alpha channel.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(color-posterize _levels_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-posterize` (`CIColorPosterize`).
Remaps red, green, and blue color components to the number of brightness values you specify for each color component. This filter flattens colors to achieve a look similar to that of a silk-screened poster.

- _levels_ (`scalar/NSNumber`): The number of brightness levels to use for each color component. Lower values result in a more extreme poster effect.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(color-threshold _threshold_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `color-threshold` (`CIColorThreshold`).
Produces a binarized image from an image and a threshold value. The red, green and blue channels of the resulting image will be one if its value is greater than the threshold and zero otherwise.

- _threshold_ (`scalar/NSNumber`): The threshold value that governs if the RGB channels of the resulting image will be zero or one.

Filter categories: `builtin`, `video`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(color-threshold-otsu)**

Returns an image processor for image filter `color-threshold-otsu` (`CIColorThresholdOtsu`).
Produces a binarized image from an image with finite extent. The threshold is calculated from the image histogram using Otsu’s method. The red, green and blue channels of the resulting image will be one if its value is greater than the threshold and zero otherwise.


Filter categories: `builtin`, `video`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(column-average _extent_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `column-average` (`CIColumnAverage`).
Calculates the average color for each column of the specified area in an image, returning the result in a 1D image.

- _extent_ (`rect/CIVector`): A rectangle that specifies the subregion of the image that you want to process.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `reduction`, `still-image`

**(comic-effect)**

Returns an image processor for image filter `comic-effect` (`CIComicEffect`).
Simulates a comic book drawing by outlining edges and applying a color halftone effect.


Filter categories: `builtin`, `video`, `stylize`, `still-image`

**(convert-lab-to-rgb _normalize_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `convert-lab-to-rgb` (`CIConvertLabToRGB`).
Converts an image from La*b* color space to the Core Image RGB working space.

- _normalize_ (`boolean/NSNumber`): If normalize is false then the L channel is in the range 0 to 100 and the a*b* channels are in the range -128 to 128. If normalize is true then the La*b* channels are in the range 0 to 1.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(convert-rgb-to-lab _normalize_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `convert-rgb-to-lab` (`CIConvertRGBtoLab`).
Converts an image from the Core Image RGB working space to La*b* color space.

- _normalize_ (`boolean/NSNumber`): If normalize is false then the L channel is in the range 0 to 100 and the a*b* channels are in the range -128 to 128. If normalize is true then the La*b* channels are in the range 0 to 1.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(convolution-3x3 _weights bias_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `convolution-3x3` (`CIConvolution3X3`).
Convolution with 3 by 3 matrix.

- _weights_ (`image-coefficients/CIVector`): A vector containing the 9 weights of the convolution kernel.
- _bias_ (`scalar/NSNumber`): A value that is added to the RGBA components of the output pixel.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(convolution-5x5 _weights bias_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `convolution-5x5` (`CIConvolution5X5`).
Convolution with 5 by 5 matrix.

- _weights_ (`image-coefficients/CIVector`): A vector containing the 25 weights of the convolution kernel.
- _bias_ (`scalar/NSNumber`): A value that is added to the RGBA components of the output pixel.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(convolution-7x7 _weights bias_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `convolution-7x7` (`CIConvolution7X7`).
Convolution with 7 by 7 matrix.

- _weights_ (`image-coefficients/CIVector`): A vector containing the 49 weights of the convolution kernel.
- _bias_ (`scalar/NSNumber`): A value that is added to the RGBA components of the output pixel.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(convolution9-horizontal _weights bias_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `convolution9-horizontal` (`CIConvolution9Horizontal`).
Horizontal Convolution with 9 values.

- _weights_ (`image-coefficients/CIVector`): A vector containing the 9 weights of the convolution kernel.
- _bias_ (`scalar/NSNumber`): A value that is added to the RGBA components of the output pixel.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(convolution9-vertical _weights bias_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `convolution9-vertical` (`CIConvolution9Vertical`).
Vertical Convolution with 9 values.

- _weights_ (`image-coefficients/CIVector`): A vector containing the 9 weights of the convolution kernel.
- _bias_ (`scalar/NSNumber`): A value that is added to the RGBA components of the output pixel.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(convolution-rgb-3x3 _weights bias_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `convolution-rgb-3x3` (`CIConvolutionRGB3X3`).
Convolution of RGB channels with 3 by 3 matrix.

- _weights_ (`image-coefficients/CIVector`): A vector containing the 9 weights of the convolution kernel.
- _bias_ (`scalar/NSNumber`): A value that is added to the RGB components of the output pixel.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(convolution-rgb-5x5 _weights bias_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `convolution-rgb-5x5` (`CIConvolutionRGB5X5`).
Convolution of RGB channels with 5 by 5 matrix.

- _weights_ (`image-coefficients/CIVector`): A vector containing the 25 weights of the convolution kernel.
- _bias_ (`scalar/NSNumber`): A value that is added to the RGB components of the output pixel.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(convolution-rgb-7x7 _weights bias_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `convolution-rgb-7x7` (`CIConvolutionRGB7X7`).
Convolution of RGB channels with 7 by 7 matrix.

- _weights_ (`image-coefficients/CIVector`): A vector containing the 49 weights of the convolution kernel.
- _bias_ (`scalar/NSNumber`): A value that is added to the RGB components of the output pixel.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(convolution-rgb9-horizontal _weights bias_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `convolution-rgb9-horizontal` (`CIConvolutionRGB9Horizontal`).
Horizontal Convolution of RGB channels with 9 values.

- _weights_ (`image-coefficients/CIVector`): A vector containing the 9 weights of the convolution kernel.
- _bias_ (`scalar/NSNumber`): A value that is added to the RGB components of the output pixel.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(convolution-rgb9-vertical _weights bias_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `convolution-rgb9-vertical` (`CIConvolutionRGB9Vertical`).
Vertical Convolution of RGB channels with 9 values.

- _weights_ (`image-coefficients/CIVector`): A vector containing the 9 weights of the convolution kernel.
- _bias_ (`scalar/NSNumber`): A value that is added to the RGB components of the output pixel.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(copy-machine-transition _target-image extent color time angle width opacity_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `copy-machine-transition` (`CICopyMachineTransition`).
Transitions from one image to another by simulating the effect of a copy machine.

- _target-image_ (`abstract-image/CIImage`): The target image for a transition.
- _extent_ (`rect/CIVector`): A rectangle that defines the extent of the effect.
- _color_ (`opaque-color/CIColor`): The color of the copier light.
- _time_ (`time/NSNumber`): The parametric time of the transition. This value drives the transition from start (at time 0) to end (at time 1).
- _angle_ (`angle/NSNumber`): The angle in radians of the copier light.
- _width_ (`distance/NSNumber`): The width of the copier light. 
- _opacity_ (`scalar/NSNumber`): The opacity of the copier light. A value of 0.0 is transparent. A value of 1.0 is opaque.

Filter categories: `builtin`, `video`, `transition`, `high-dynamic-range`, `still-image`

**(crop _rectangle_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `crop` (`CICrop`).
Applies a crop to an image. The size and shape of the cropped image depend on the rectangle you specify.

- _rectangle_ (`rect/CIVector`): The rectangle that specifies the crop to apply to the image.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `still-image`, `geometry-adjustment`

**(crystallize _radius center_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `crystallize` (`CICrystallize`).
Creates polygon-shaped color blocks by aggregating source pixel-color values.

- _radius_ (`distance/NSNumber`): The radius determines how many pixels are used to create the effect. The larger the radius, the larger the resulting crystals.
- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(darken-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `darken-blend-mode` (`CIDarkenBlendMode`).
Creates composite image samples by choosing the darker samples (from either the source image or the background). The result is that the background image samples are replaced by any source image samples that are darker. Otherwise, the background image samples are left unchanged.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(depth-of-field _point0 point1 saturation unsharp-mask-radius unsharp-mask-intensity radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `depth-of-field` (`CIDepthOfField`).
Simulates miniaturization effect created by Tilt & Shift lens by performing depth of field effects.

- _point0_ (`point/CIVector`)
- _point1_ (`point/CIVector`)
- _saturation_ (`scalar/NSNumber`): The amount to adjust the saturation.
- _unsharp-mask-radius_ (`scalar/NSNumber`)
- _unsharp-mask-intensity_ (`scalar/NSNumber`)
- _radius_ (`scalar/NSNumber`): The distance from the center of the effect.

Filter categories: `builtin`, `video`, `stylize`, `still-image`

**(depth-to-disparity)**

Returns an image processor for image filter `depth-to-disparity` (`CIDepthToDisparity`).
Convert a depth data image to disparity data.


Filter categories: `builtin`, `video`, `color-adjustment`, `still-image`

**(difference-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `difference-blend-mode` (`CIDifferenceBlendMode`).
Subtracts either the source image sample color from the background image sample color, or the reverse, depending on which sample has the greater brightness value. Source image sample values that are black produce no change; white inverts the background color values.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(disc-blur _radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `disc-blur` (`CIDiscBlur`).
Smooths an image using a disc-shaped convolution kernel.

- _radius_ (`distance/NSNumber`): The radius determines how many pixels are used to create the blur. The larger the radius, the blurrier the result.

Filter categories: `builtin`, `blur`, `video`, `high-dynamic-range`, `still-image`

**(disintegrate-with-mask-transition _target-image mask-image time shadow-radius shadow-density shadow-offset_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `disintegrate-with-mask-transition` (`CIDisintegrateWithMaskTransition`).
Transitions from one image to another using the shape defined by a mask.

- _target-image_ (`abstract-image/CIImage`): The target image for a transition.
- _mask-image_ (`abstract-image/CIImage`): An image that defines the shape to use when disintegrating from the source to the target image.
- _time_ (`time/NSNumber`): The parametric time of the transition. This value drives the transition from start (at time 0) to end (at time 1).
- _shadow-radius_ (`distance/NSNumber`): The radius of the shadow created by the mask.
- _shadow-density_ (`scalar/NSNumber`): The density of the shadow created by the mask.
- _shadow-offset_ (`offset/CIVector`): The offset of the shadow created by the mask.

Filter categories: `builtin`, `video`, `transition`, `high-dynamic-range`, `still-image`

**(disparity-to-depth)**

Returns an image processor for image filter `disparity-to-depth` (`CIDisparityToDepth`).
Convert a disparity data image to depth data.


Filter categories: `builtin`, `video`, `color-adjustment`, `still-image`

**(displacement-distortion _displacement-image scale_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `displacement-distortion` (`CIDisplacementDistortion`).
Applies the grayscale values of the second image to the first image. The output image has a texture defined by the grayscale values.

- _displacement-image_ (`abstract-image/CIImage`): An image whose grayscale values will be applied to the source image.
- _scale_ (`distance/NSNumber`): The amount of texturing of the resulting image. The larger the value, the greater the texturing.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(dissolve-transition _target-image time_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `dissolve-transition` (`CIDissolveTransition`).
Uses a dissolve to transition from one image to another.

- _target-image_ (`abstract-image/CIImage`): The target image for a transition.
- _time_ (`time/NSNumber`): The parametric time of the transition. This value drives the transition from start (at time 0) to end (at time 1).

Filter categories: `builtin`, `video`, `transition`, `high-dynamic-range`, `interlaced`, `still-image`, `non-square-pixels`

**(distance-gradient-from-red-mask _maximum-distance_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `distance-gradient-from-red-mask` (`CIDistanceGradientFromRedMask`).
Produces an infinite image where the red channel contains the distance in pixels from each pixel to the mask.

- _maximum-distance_ (`distance/NSNumber`): Determines the maximum distance to the mask that can be measured. Distances between zero and the maximum will be normalized to zero and one.

Filter categories: `builtin`, `video`, `gradient`, `still-image`

**(dither _intensity_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `dither` (`CIDither`).
Apply dithering to an image. This operation is usually performed in a perceptual color space.

- _intensity_ (`scalar/NSNumber`): The intensity of the effect.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `color-effect`, `still-image`

**(divide-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `divide-blend-mode` (`CIDivideBlendMode`).
Divides the background image sample color from the source image sample color.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(document-enhancer _amount_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `document-enhancer` (`CIDocumentEnhancer`).
Enhance a document image by removing unwanted shadows, whitening the background, and enhancing contrast.

- _amount_ (`scalar/NSNumber`): The amount of enhancement.

Filter categories: `builtin`, `color-effect`, `still-image`, `non-square-pixels`

**(dot-screen _center angle width sharpness_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `dot-screen` (`CIDotScreen`).
Simulates the dot patterns of a halftone screen.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _angle_ (`angle/NSNumber`): The angle in radians of the pattern.
- _width_ (`distance/NSNumber`): The distance between dots in the pattern.
- _sharpness_ (`scalar/NSNumber`): The sharpness of the pattern. The larger the value, the sharper the pattern.

Filter categories: `builtin`, `video`, `halftone-effect`, `still-image`

**(droste _inset-point0 inset-point1 strands periodicity rotation zoom_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `droste` (`CIDroste`).
Performs M.C. Escher Droste style deformation.

- _inset-point0_ (`point/CIVector`)
- _inset-point1_ (`point/CIVector`)
- _strands_ (`scalar/NSNumber`)
- _periodicity_ (`scalar/NSNumber`)
- _rotation_ (`angle/NSNumber`)
- _zoom_ (`scalar/NSNumber`)

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(edge-preserve-upsample-filter _small-image spatial-sigma luma-sigma_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `edge-preserve-upsample-filter` (`CIEdgePreserveUpsampleFilter`).
Upsamples a small image to the size of the input image using the luminance of the input image as a guide to preserve detail.

- _small-image_ (`abstract-image/CIImage`)
- _spatial-sigma_ (`scalar/NSNumber`)
- _luma-sigma_ (`scalar/NSNumber`)

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `still-image`, `non-square-pixels`, `geometry-adjustment`

**(edges _intensity_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `edges` (`CIEdges`).
Finds all edges in an image and displays them in color.

- _intensity_ (`scalar/NSNumber`): The intensity of the edges. The larger the value, the higher the intensity.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(edge-work _radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `edge-work` (`CIEdgeWork`).
Produces a stylized black-and-white rendition of an image that looks similar to a woodblock cutout.

- _radius_ (`distance/NSNumber`): The thickness of the edges. The larger the value, the thicker the edges.

Filter categories: `builtin`, `video`, `stylize`, `still-image`

**(eightfold-reflected-tile _center angle width_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `eightfold-reflected-tile` (`CIEightfoldReflectedTile`).
Produces a tiled image from a source image by applying an 8-way reflected symmetry.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _angle_ (`angle/NSNumber`): The angle in radians of the tiled pattern.
- _width_ (`distance/NSNumber`): The width of a tile.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(exclusion-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `exclusion-blend-mode` (`CIExclusionBlendMode`).
Produces an effect similar to that produced by the “Difference Blend Mode” filter but with lower contrast. Source image sample values that are black do not produce a change; white inverts the background color values.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(exposure-adjust _e-v_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `exposure-adjust` (`CIExposureAdjust`).
Adjusts the exposure setting for an image similar to the way you control exposure for a camera when you change the F-stop.

- _e-v_ (`scalar/NSNumber`): The amount to adjust the exposure of the image by. The larger the value, the brighter the exposure.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(false-color _color0 color1_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `false-color` (`CIFalseColor`).
Maps luminance to a color ramp of two colors. False color is often used to process astronomical and other scientific data, such as ultraviolet and X-ray images.

- _color0_ (`color/CIColor`): The first color to use for the color ramp.
- _color1_ (`color/CIColor`): The second color to use for the color ramp.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(flash-transition _target-image center extent color time max-striation-radius striation-strength striation-contrast fade-threshold_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `flash-transition` (`CIFlashTransition`).
Transitions from one image to another by creating a flash. The flash originates from a point you specify. Small at first, it rapidly expands until the image frame is completely filled with the flash color. As the color fades, the target image begins to appear.

- _target-image_ (`abstract-image/CIImage`): The target image for a transition.
- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _extent_ (`rect/CIVector`): The extent of the flash.
- _color_ (`color/CIColor`): The color of the light rays emanating from the flash.
- _time_ (`time/NSNumber`): The parametric time of the transition. This value drives the transition from start (at time 0) to end (at time 1).
- _max-striation-radius_ (`scalar/NSNumber`): The radius of the light rays emanating from the flash.
- _striation-strength_ (`scalar/NSNumber`): The strength of the light rays emanating from the flash.
- _striation-contrast_ (`scalar/NSNumber`): The contrast of the light rays emanating from the flash.
- _fade-threshold_ (`scalar/NSNumber`): The amount of fade between the flash and the target image. The higher the value, the more flash time and the less fade time.

Filter categories: `builtin`, `video`, `transition`, `high-dynamic-range`, `still-image`

**(fourfold-reflected-tile _center angle width acute-angle_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `fourfold-reflected-tile` (`CIFourfoldReflectedTile`).
Produces a tiled image from a source image by applying a 4-way reflected symmetry.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _angle_ (`angle/NSNumber`): The angle in radians of the tiled pattern.
- _width_ (`distance/NSNumber`): The width of a tile.
- _acute-angle_ (`angle/NSNumber`): The primary angle for the repeating reflected tile. Small values create thin diamond tiles, and higher values create fatter reflected tiles.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(fourfold-rotated-tile _center angle width_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `fourfold-rotated-tile` (`CIFourfoldRotatedTile`).
Produces a tiled image from a source image by rotating the source at increments of 90 degrees.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _angle_ (`angle/NSNumber`): The angle in radians of the tiled pattern.
- _width_ (`distance/NSNumber`): The width of a tile.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(fourfold-translated-tile _center angle width acute-angle_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `fourfold-translated-tile` (`CIFourfoldTranslatedTile`).
Produces a tiled image from a source image by applying 4 translation operations.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _angle_ (`angle/NSNumber`): The angle in radians of the tiled pattern.
- _width_ (`distance/NSNumber`): The width of a tile.
- _acute-angle_ (`angle/NSNumber`): The primary angle for the repeating translated tile. Small values create thin diamond tiles, and higher values create fatter translated tiles.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(gabor-gradients)**

Returns an image processor for image filter `gabor-gradients` (`CIGaborGradients`).
Applies multichannel 5 by 5 Gabor gradient filter to an image. The resulting image has maximum horizontal gradient in the red channel and the maximum vertical gradient in the green channel. The gradient values can be positive or negative.


Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(gamma-adjust _power_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `gamma-adjust` (`CIGammaAdjust`).
Adjusts midtone brightness. This filter is typically used to compensate for nonlinear effects of displays. Adjusting the gamma effectively changes the slope of the transition between black and white.

- _power_ (`scalar/NSNumber`): A gamma value to use to correct image brightness. The larger the value, the darker the result.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(gaussian-blur _radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `gaussian-blur` (`CIGaussianBlur`).
Spreads source pixels by an amount specified by a Gaussian distribution.

- _radius_ (`scalar/NSNumber`): The radius determines how many pixels are used to create the blur. The larger the radius, the blurrier the result.

Filter categories: `builtin`, `blur`, `video`, `high-dynamic-range`, `still-image`

**(gaussian-gradient _center color0 color1 radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `gaussian-gradient` (`CIGaussianGradient`).
Generates a gradient that varies from one color to another using a Gaussian distribution.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _color0_ (`color/CIColor`): The first color to use in the gradient.
- _color1_ (`color/CIColor`): The second color to use in the gradient.
- _radius_ (`distance/NSNumber`): The radius of the Gaussian distribution.

Filter categories: `builtin`, `video`, `gradient`, `high-dynamic-range`, `still-image`

**(glass-distortion _texture center scale_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `glass-distortion` (`CIGlassDistortion`).
Distorts an image by applying a glass-like texture. The raised portions of the output image are the result of applying a texture map.

- _texture_ (`abstract-image/CIImage`): A texture to apply to the source image.
- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _scale_ (`distance/NSNumber`): The amount of texturing of the resulting image. The larger the value, the greater the texturing.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(glass-lozenge _point0 point1 radius refraction_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `glass-lozenge` (`CIGlassLozenge`).
Creates a lozenge-shaped lens and distorts the portion of the image over which the lens is placed.

- _point0_ (`point/CIVector`): The x and y position that defines the center of the circle at one end of the lozenge.
- _point1_ (`point/CIVector`): The x and y position that defines the center of the circle at the other end of the lozenge.
- _radius_ (`distance/NSNumber`): The radius of the lozenge. The larger the radius, the wider the extent of the distortion.
- _refraction_ (`scalar/NSNumber`): The refraction of the glass.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(glide-reflected-tile _center angle width_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `glide-reflected-tile` (`CIGlideReflectedTile`).
Produces a tiled image from a source image by translating and smearing the image.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _angle_ (`angle/NSNumber`): The angle in radians of the tiled pattern.
- _width_ (`distance/NSNumber`): The width of a tile.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(gloom _radius intensity_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `gloom` (`CIGloom`).
Dulls the highlights of an image.

- _radius_ (`distance/NSNumber`): The radius determines how many pixels are used to create the effect. The larger the radius, the greater the effect.
- _intensity_ (`scalar/NSNumber`): The intensity of the effect. A value of 0.0 is no effect. A value of 1.0 is the maximum effect.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(guided-filter _guide-image radius epsilon_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `guided-filter` (`CIGuidedFilter`).
Upsamples a small image to the size of the guide image using the content of the guide to preserve detail.

- _guide-image_ (`abstract-image/CIImage`): A larger image to use as a guide.
- _radius_ (`scalar/NSNumber`): The distance from the center of the effect.
- _epsilon_ (`scalar/NSNumber`)

Filter categories: `builtin`, `video`, `high-dynamic-range`, `still-image`, `geometry-adjustment`

**(hard-light-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `hard-light-blend-mode` (`CIHardLightBlendMode`).
Either multiplies or screens colors, depending on the source image sample color. If the source image sample color is lighter than 50% gray, the background is lightened, similar to screening. If the source image sample color is darker than 50% gray, the background is darkened, similar to multiplying. If the source image sample color is equal to 50% gray, the source image is not changed. Image samples that are equal to pure black or pure white result in pure black or white. The overall effect is similar to what you would achieve by shining a harsh spotlight on the source image.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(hatched-screen _center angle width sharpness_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `hatched-screen` (`CIHatchedScreen`).
Simulates the hatched pattern of a halftone screen.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _angle_ (`angle/NSNumber`): The angle in radians of the pattern.
- _width_ (`distance/NSNumber`): The distance between lines in the pattern.
- _sharpness_ (`scalar/NSNumber`): The amount of sharpening to apply.

Filter categories: `builtin`, `video`, `halftone-effect`, `still-image`

**(height-field-from-mask _radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `height-field-from-mask` (`CIHeightFieldFromMask`).
Produces a continuous three-dimensional, loft-shaped height field from a grayscale mask. The white values of the mask define those pixels that are inside the height field while the black values define those pixels that are outside. The field varies smoothly and continuously inside the mask, reaching the value 0 at the edge of the mask. You can use this filter with the Shaded Material filter to produce extremely realistic shaded objects.

- _radius_ (`distance/NSNumber`): The distance from the edge of the mask for the smooth transition is proportional to the input radius. Larger values make the transition smoother and more pronounced. Smaller values make the transition approximate a fillet radius.

Filter categories: `builtin`, `video`, `stylize`, `still-image`

**(hexagonal-pixellate _center scale_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `hexagonal-pixellate` (`CIHexagonalPixellate`).
Displays an image as colored hexagons whose color is an average of the pixels they replace.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _scale_ (`distance/NSNumber`): The scale determines the size of the hexagons. Larger values result in larger hexagons.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(highlight-shadow-adjust _radius shadow-amount highlight-amount_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `highlight-shadow-adjust` (`CIHighlightShadowAdjust`).
Adjust the tonal mapping of an image while preserving spatial detail.

- _radius_ (`scalar/NSNumber`): Shadow Highlight Radius.
- _shadow-amount_ (`scalar/NSNumber`): The amount of adjustment to the shadows of the image.
- _highlight-amount_ (`scalar/NSNumber`): The amount of adjustment to the highlights of the image.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(histogram-display-filter _height high-limit low-limit_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `histogram-display-filter` (`CIHistogramDisplayFilter`).
Generates a displayable histogram image from the output of the “Area Histogram” filter.

- _height_ (`scalar/NSNumber`): The height of the displayable histogram image.
- _high-limit_ (`scalar/NSNumber`): The fraction of the right portion of the histogram image to make lighter.
- _low-limit_ (`scalar/NSNumber`): The fraction of the left portion of the histogram image to make darker.

Filter categories: `builtin`, `video`, `reduction`, `still-image`

**(hole-distortion _center radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `hole-distortion` (`CIHoleDistortion`).
Creates a circular area that pushes the image pixels outward, distorting those pixels closest to the circle the most.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _radius_ (`distance/NSNumber`): The radius determines how many pixels are used to create the distortion. The larger the radius, the wider the extent of the distortion.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(hue-adjust _angle_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `hue-adjust` (`CIHueAdjust`).
Changes the overall hue, or tint, of the source pixels.

- _angle_ (`angle/NSNumber`): An angle in radians to use to correct the hue of an image.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(hue-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `hue-blend-mode` (`CIHueBlendMode`).
Uses the luminance and saturation values of the background with the hue of the source image.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(hue-saturation-value-gradient _value radius softness dither color-space_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `hue-saturation-value-gradient` (`CIHueSaturationValueGradient`).
Generates a color wheel that shows hues and saturations for a specified value.

- _value_ (`scalar/NSNumber`): The color value used to generate the color wheel.
- _radius_ (`distance/NSNumber`): The distance from the center of the effect.
- _softness_ (`scalar/NSNumber`)
- _dither_ (`scalar/NSNumber`)
- _color-space_ (`color-space/NSObject`): The CGColorSpaceRef that the color wheel should be generated in.

Filter categories: `builtin`, `video`, `gradient`, `still-image`

**(kaleidoscope _count center angle_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `kaleidoscope` (`CIKaleidoscope`).
Produces a kaleidoscopic image from a source image by applying 12-way symmetry.

- _count_ (`scalar/NSNumber`): The number of reflections in the pattern.
- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _angle_ (`angle/NSNumber`): The angle in radians of reflection.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(keystone-correction-combined _focal-length top-left top-right bottom-right bottom-left_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `keystone-correction-combined` (`CIKeystoneCorrectionCombined`).
Apply keystone correction to an image with combined horizontal and vertical guides.

- _focal-length_ (`scalar/NSNumber`): 35mm equivalent focal length of the input image.
- _top-left_ (`point/CIVector`): The top left coordinate of the guide.
- _top-right_ (`point/CIVector`): The top right coordinate of the guide.
- _bottom-right_ (`point/CIVector`): The bottom right coordinate of the guide.
- _bottom-left_ (`point/CIVector`): The bottom left coordinate of the guide.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `still-image`, `geometry-adjustment`

**(keystone-correction-horizontal _focal-length top-left top-right bottom-right bottom-left_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `keystone-correction-horizontal` (`CIKeystoneCorrectionHorizontal`).
Apply horizontal keystone correction to an image with guides.

- _focal-length_ (`scalar/NSNumber`): 35mm equivalent focal length of the input image.
- _top-left_ (`point/CIVector`): The top left coordinate of the guide.
- _top-right_ (`point/CIVector`): The top right coordinate of the guide.
- _bottom-right_ (`point/CIVector`): The bottom right coordinate of the guide.
- _bottom-left_ (`point/CIVector`): The bottom left coordinate of the guide.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `still-image`, `geometry-adjustment`

**(keystone-correction-vertical _focal-length top-left top-right bottom-right bottom-left_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `keystone-correction-vertical` (`CIKeystoneCorrectionVertical`).
Apply vertical keystone correction to an image with guides.

- _focal-length_ (`scalar/NSNumber`): 35mm equivalent focal length of the input image.
- _top-left_ (`point/CIVector`): The top left coordinate of the guide.
- _top-right_ (`point/CIVector`): The top right coordinate of the guide.
- _bottom-right_ (`point/CIVector`): The bottom right coordinate of the guide.
- _bottom-left_ (`point/CIVector`): The bottom left coordinate of the guide.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `still-image`, `geometry-adjustment`

**(kmeans _extent means count passes perceptual_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `kmeans` (`CIKMeans`).
Create a palette of the most common colors found in the image.

- _extent_ (`rect/CIVector`): A rectangle that defines the extent of the effect.
- _means_ (`abstract-image/CIImage`): Specifies the color seeds to use for k-means clustering, either passed as an image or an array of colors.
- _count_ (`count/NSNumber`): Specifies how many k-means color clusters should be used.
- _passes_ (`count/NSNumber`): Specifies how many k-means passes should be performed.
- _perceptual_ (`boolean/NSNumber`): Specifies whether the k-means color palette should be computed in a perceptual color space.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `reduction`, `still-image`

**(lab-delta-e _image2_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `lab-delta-e` (`CILabDeltaE`).
Produces an image with the Lab ∆E difference values between two images. The result image will contain ∆E 1994 values between 0.0 and 100.0 where 2.0 is considered a just noticeable difference.

- _image2_ (`abstract-image/CIImage`): The second input image for comparison.

Filter categories: `builtin`, `video`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(lanczos-scale-transform _scale aspect-ratio_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `lanczos-scale-transform` (`CILanczosScaleTransform`).
Produces a high-quality, scaled version of a source image. You typically use this filter to scale down an image.

- _scale_ (`scalar/NSNumber`): The scaling factor to use on the image. Values less than 1.0 scale down the images. Values greater than 1.0 scale up the image.
- _aspect-ratio_ (`scalar/NSNumber`): The additional horizontal scaling factor to use on the image.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `still-image`, `geometry-adjustment`

**(lighten-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `lighten-blend-mode` (`CILightenBlendMode`).
Creates composite image samples by choosing the lighter samples (either from the source image or the background). The result is that the background image samples are replaced by any source image samples that are lighter. Otherwise, the background image samples are left unchanged.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(light-tunnel _center rotation radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `light-tunnel` (`CILightTunnel`).
Light tunnel distortion.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _rotation_ (`angle/NSNumber`): Rotation angle in radians of the light tunnel.
- _radius_ (`distance/NSNumber`): Center radius of the light tunnel.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(linear-burn-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `linear-burn-blend-mode` (`CILinearBurnBlendMode`).
Inverts the unpremultiplied source and background image sample color, inverts the sum, and then blends the result with the background according to the PDF basic compositing formula. Source image values that are white produce no change. Source image values that are black invert the background color values.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(linear-dodge-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `linear-dodge-blend-mode` (`CILinearDodgeBlendMode`).
Unpremultiplies the source and background image sample colors, adds them, and then blends the result with the background according to the PDF basic compositing formula. Source image values that are black produces output that is the same as the background. Source image values that are non-black brighten the background color values.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(linear-gradient _point0 point1 color0 color1_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `linear-gradient` (`CILinearGradient`).
Generates a gradient that varies along a linear axis between two defined endpoints.

- _point0_ (`point/CIVector`): The starting position of the gradient -- where the first color begins.
- _point1_ (`point/CIVector`): The ending position of the gradient -- where the second color begins.
- _color0_ (`color/CIColor`): The first color to use in the gradient.
- _color1_ (`color/CIColor`): The second color to use in the gradient.

Filter categories: `builtin`, `video`, `gradient`, `high-dynamic-range`, `still-image`

**(linear-light-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `linear-light-blend-mode` (`CILinearLightBlendMode`).
A blend mode that is a combination of linear burn and linear dodge blend modes.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(linear-to-srgbtone-curve)**

Returns an image processor for image filter `linear-to-srgbtone-curve` (`CILinearToSRGBToneCurve`).
Converts an image in linear space to sRGB space.


Filter categories: `builtin`, `video`, `high-dynamic-range`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(line-overlay _n-r-noise-level n-r-sharpness edge-intensity threshold contrast_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `line-overlay` (`CILineOverlay`).
Creates a sketch that outlines the edges of an image in black, leaving the non-outlined portions of the image transparent. The result has alpha and is rendered in black, so it won’t look like much until you render it over another image using source over compositing.

- _n-r-noise-level_ (`scalar/NSNumber`): The noise level of the image (used with camera data) that gets removed before tracing the edges of the image. Increasing the noise level helps to clean up the traced edges of the image.
- _n-r-sharpness_ (`scalar/NSNumber`): The amount of sharpening done when removing noise in the image before tracing the edges of the image. This improves the edge acquisition.
- _edge-intensity_ (`scalar/NSNumber`): The accentuation factor of the Sobel gradient information when tracing the edges of the image. Higher values find more edges, although typically a low value (such as 1.0) is used.
- _threshold_ (`scalar/NSNumber`): This value determines edge visibility. Larger values thin out the edges.
- _contrast_ (`scalar/NSNumber`): The amount of anti-aliasing to use on the edges produced by this filter. Higher values produce higher contrast edges (they are less anti-aliased).

Filter categories: `builtin`, `video`, `stylize`, `still-image`

**(line-screen _center angle width sharpness_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `line-screen` (`CILineScreen`).
Simulates the line pattern of a halftone screen.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _angle_ (`angle/NSNumber`): The angle in radians of the pattern.
- _width_ (`distance/NSNumber`): The distance between lines in the pattern.
- _sharpness_ (`scalar/NSNumber`): The sharpness of the pattern. The larger the value, the sharper the pattern.

Filter categories: `builtin`, `video`, `halftone-effect`, `still-image`

**(luminosity-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `luminosity-blend-mode` (`CILuminosityBlendMode`).
Uses the hue and saturation of the background with the luminance of the source image. This mode creates an effect that is inverse to the effect created by the “Color Blend Mode” filter.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(masked-variable-blur _mask radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `masked-variable-blur` (`CIMaskedVariableBlur`).
Blurs an image according to the brightness levels in a mask image.

- _mask_ (`abstract-image/CIImage`): The mask image that determines how much to blur the image. The mask’s green channel value from 0.0 to 1.0 determines if the image is not blurred or blurred by the full radius.
- _radius_ (`scalar/NSNumber`): A value that governs the maximum blur radius to apply.

Filter categories: `builtin`, `blur`, `video`, `high-dynamic-range`, `still-image`

**(mask-to-alpha)**

Returns an image processor for image filter `mask-to-alpha` (`CIMaskToAlpha`).
Converts a grayscale image to a white image that is masked by alpha. The white values from the source image produce the inside of the mask; the black values become completely transparent.


Filter categories: `builtin`, `video`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(maximum-component)**

Returns an image processor for image filter `maximum-component` (`CIMaximumComponent`).
Converts an image to grayscale using the maximum of the three color components.


Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(maximum-compositing _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `maximum-compositing` (`CIMaximumCompositing`).
Computes the maximum value, by color component, of two input images and creates an output image using the maximum values. This is similar to dodging.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `high-dynamic-range`, `interlaced`, `still-image`, `non-square-pixels`

**(maximum-scale-transform _scale aspect-ratio_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `maximum-scale-transform` (`CIMaximumScaleTransform`).
Produces a scaled version of a source image that uses the maximum of neighboring pixels instead of linear averaging.

- _scale_ (`scalar/NSNumber`): The scaling factor to use on the image. Values less than 1.0 scale down the images. Values greater than 1.0 scale up the image.
- _aspect-ratio_ (`scalar/NSNumber`): The additional horizontal scaling factor to use on the image.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `still-image`, `geometry-adjustment`

**(median-filter)**

Returns an image processor for image filter `median-filter` (`CIMedianFilter`).
Computes the median value for a group of neighboring pixels and replaces each pixel value with the median. The effect is to reduce noise.


Filter categories: `builtin`, `blur`, `video`, `high-dynamic-range`, `still-image`

**(minimum-component)**

Returns an image processor for image filter `minimum-component` (`CIMinimumComponent`).
Converts an image to grayscale using the minimum of the three color components.


Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(minimum-compositing _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `minimum-compositing` (`CIMinimumCompositing`).
Computes the minimum value, by color component, of two input images and creates an output image using the minimum values. This is similar to burning.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `high-dynamic-range`, `interlaced`, `still-image`, `non-square-pixels`

**(mix _background-image amount_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `mix` (`CIMix`).
Uses an amount parameter to interpolate between an image and a background image. When value is 0.0 or less, the result is the background image. When the value is 1.0 or more, the result is the image.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.
- _amount_ (`scalar/NSNumber`): The amount of the effect.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(mod-transition _target-image center time angle radius compression_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `mod-transition` (`CIModTransition`).
Transitions from one image to another by revealing the target image through irregularly shaped holes.

- _target-image_ (`abstract-image/CIImage`): The target image for a transition.
- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _time_ (`time/NSNumber`): The parametric time of the transition. This value drives the transition from start (at time 0) to end (at time 1).
- _angle_ (`angle/NSNumber`): The angle in radians of the mod hole pattern.
- _radius_ (`distance/NSNumber`): The radius of the undistorted holes in the pattern.
- _compression_ (`distance/NSNumber`): The amount of stretching applied to the mod hole pattern. Holes in the center are not distorted as much as those at the edge of the image.

Filter categories: `builtin`, `video`, `transition`, `high-dynamic-range`, `still-image`

**(morphology-gradient _radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `morphology-gradient` (`CIMorphologyGradient`).
Finds the edges of an image by returning the difference between the morphological minimum and maximum operations to the image.

- _radius_ (`distance/NSNumber`): The desired radius of the circular morphological operation to the image.

Filter categories: `builtin`, `blur`, `video`, `high-dynamic-range`, `still-image`

**(morphology-maximum _radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `morphology-maximum` (`CIMorphologyMaximum`).
Lightens areas of an image by applying a circular morphological maximum operation to the image.

- _radius_ (`distance/NSNumber`): The desired radius of the circular morphological operation to the image.

Filter categories: `builtin`, `blur`, `video`, `high-dynamic-range`, `still-image`

**(morphology-minimum _radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `morphology-minimum` (`CIMorphologyMinimum`).
Darkens areas of an image by applying a circular morphological maximum operation to the image.

- _radius_ (`distance/NSNumber`): The desired radius of the circular morphological operation to the image.

Filter categories: `builtin`, `blur`, `video`, `high-dynamic-range`, `still-image`

**(morphology-rectangle-maximum _width height_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `morphology-rectangle-maximum` (`CIMorphologyRectangleMaximum`).
Lightens areas of an image by applying a rectangular morphological maximum operation to the image.

- _width_ (`integer/NSNumber`): The width in pixels of the morphological operation. The value will be rounded to the nearest odd integer.
- _height_ (`integer/NSNumber`): The height in pixels of the morphological operation. The value will be rounded to the nearest odd integer.

Filter categories: `builtin`, `blur`, `video`, `high-dynamic-range`, `still-image`

**(morphology-rectangle-minimum _width height_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `morphology-rectangle-minimum` (`CIMorphologyRectangleMinimum`).
Darkens areas of an image by applying a rectangular morphological maximum operation to the image.

- _width_ (`integer/NSNumber`): The width in pixels of the morphological operation. The value will be rounded to the nearest odd integer.
- _height_ (`integer/NSNumber`): The height in pixels of the morphological operation. The value will be rounded to the nearest odd integer.

Filter categories: `builtin`, `blur`, `video`, `high-dynamic-range`, `still-image`

**(motion-blur _radius angle_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `motion-blur` (`CIMotionBlur`).
Blurs an image to simulate the effect of using a camera that moves a specified angle and distance while capturing the image.

- _radius_ (`distance/NSNumber`): The radius determines how many pixels are used to create the blur. The larger the radius, the blurrier the result.
- _angle_ (`angle/NSNumber`): The angle in radians of the motion determines which direction the blur smears.

Filter categories: `builtin`, `blur`, `video`, `high-dynamic-range`, `still-image`

**(multiply-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `multiply-blend-mode` (`CIMultiplyBlendMode`).
Multiplies the source image samples with the background image samples. This results in colors that are at least as dark as either of the two contributing sample colors.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(multiply-compositing _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `multiply-compositing` (`CIMultiplyCompositing`).
Multiplies the color component of two input images and creates an output image using the multiplied values. This filter is typically used to add a spotlight or similar lighting effect to an image.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `high-dynamic-range`, `interlaced`, `still-image`, `non-square-pixels`

**(nine-part-stretched _breakpoint0 breakpoint1 grow-amount_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `nine-part-stretched` (`CINinePartStretched`).
Distorts an image by stretching an image based on two input breakpoints.

- _breakpoint0_ (`point/CIVector`): Lower left corner of image to retain before stretching begins.
- _breakpoint1_ (`point/CIVector`): Upper right corner of image to retain after stretching ends.
- _grow-amount_ (`offset/CIVector`): Vector indicating how much image should grow in pixels in both dimensions.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(nine-part-tiled _breakpoint0 breakpoint1 grow-amount flip-y-tiles_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `nine-part-tiled` (`CINinePartTiled`).
Distorts an image by tiling an image based on two input breakpoints.

- _breakpoint0_ (`point/CIVector`): Lower left corner of image to retain before tiling begins.
- _breakpoint1_ (`point/CIVector`): Upper right corner of image to retain after tiling ends.
- _grow-amount_ (`offset/CIVector`): Vector indicating how much image should grow in pixels in both dimensions.
- _flip-y-tiles_ (`boolean/NSNumber`): Indicates that Y-Axis flip should occur.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(noise-reduction _noise-level sharpness_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `noise-reduction` (`CINoiseReduction`).
Reduces noise using a threshold value to define what is considered noise. Small changes in luminance below that value are considered noise and get a noise reduction treatment, which is a local blur. Changes above the threshold value are considered edges, so they are sharpened.

- _noise-level_ (`scalar/NSNumber`): The amount of noise reduction. The larger the value, the more noise reduction.
- _sharpness_ (`scalar/NSNumber`): The sharpness of the final image. The larger the value, the sharper the result.

Filter categories: `builtin`, `blur`, `video`, `high-dynamic-range`, `still-image`

**(op-tile _center scale angle width_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `op-tile` (`CIOpTile`).
Segments an image, applying any specified scaling and rotation, and then assembles the image again to give an op art appearance.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _scale_ (`scalar/NSNumber`): The scale determines the number of tiles in the effect.
- _angle_ (`angle/NSNumber`): The angle in radians of a tile.
- _width_ (`distance/NSNumber`): The width of a tile.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(overlay-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `overlay-blend-mode` (`CIOverlayBlendMode`).
Either multiplies or screens the source image samples with the background image samples, depending on the background color. The result is to overlay the existing image samples while preserving the highlights and shadows of the background. The background color mixes with the source image to reflect the lightness or darkness of the background.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(page-curl-transition _target-image backside-image shading-image extent time angle radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `page-curl-transition` (`CIPageCurlTransition`).
Transitions from one image to another by simulating a curling page, revealing the new image as the page curls.

- _target-image_ (`abstract-image/CIImage`): The target image for a transition.
- _backside-image_ (`abstract-image/CIImage`): The image that appears on the back of the source image, as the page curls to reveal the target image.
- _shading-image_ (`abstract-image/CIImage`): An image that looks like a shaded sphere enclosed in a square image.
- _extent_ (`rect/CIVector`): The extent of the effect.
- _time_ (`time/NSNumber`): The parametric time of the transition. This value drives the transition from start (at time 0) to end (at time 1).
- _angle_ (`angle/NSNumber`): The angle in radians of the curling page.
- _radius_ (`distance/NSNumber`): The radius of the curl.

Filter categories: `builtin`, `video`, `transition`, `high-dynamic-range`, `still-image`

**(page-curl-with-shadow-transition _target-image backside-image extent time angle radius shadow-size shadow-amount shadow-extent_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `page-curl-with-shadow-transition` (`CIPageCurlWithShadowTransition`).
Transitions from one image to another by simulating a curling page, revealing the new image as the page curls.

- _target-image_ (`abstract-image/CIImage`): The target image for a transition.
- _backside-image_ (`abstract-image/CIImage`): The image that appears on the back of the source image, as the page curls to reveal the target image.
- _extent_ (`rect/CIVector`): The extent of the effect.
- _time_ (`time/NSNumber`): The parametric time of the transition. This value drives the transition from start (at time 0) to end (at time 1).
- _angle_ (`angle/NSNumber`): The angle in radians of the curling page.
- _radius_ (`distance/NSNumber`): The radius of the curl.
- _shadow-size_ (`distance/NSNumber`): The maximum size in pixels of the shadow.
- _shadow-amount_ (`distance/NSNumber`): The strength of the shadow.
- _shadow-extent_ (`rect/CIVector`): The rectagular portion of input image that will cast a shadow.

Filter categories: `builtin`, `video`, `transition`, `high-dynamic-range`, `still-image`

**(palette-centroid _palette-image perceptual_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `palette-centroid` (`CIPaletteCentroid`).
Calculate the mean (x,y) image coordinates of a color palette.

- _palette-image_ (`abstract-image/CIImage`): The input color palette, obtained using “CIKMeans“ filter.
- _perceptual_ (`boolean/NSNumber`): Specifies whether the color palette should be applied in a perceptual color space.

Filter categories: `builtin`, `video`, `color-effect`, `still-image`

**(palettize _palette-image perceptual_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `palettize` (`CIPalettize`).
Paint an image from a color palette obtained using “CIKMeans“.

- _palette-image_ (`abstract-image/CIImage`): The input color palette, obtained using “CIKMeans“ filter.
- _perceptual_ (`boolean/NSNumber`): Specifies whether the color palette should be applied in a perceptual color space.

Filter categories: `builtin`, `video`, `color-effect`, `still-image`

**(parallelogram-tile _center angle acute-angle width_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `parallelogram-tile` (`CIParallelogramTile`).
Warps an image by reflecting it in a parallelogram, and then tiles the result.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _angle_ (`angle/NSNumber`): The angle in radians of the tiled pattern.
- _acute-angle_ (`angle/NSNumber`): The primary angle for the repeating parallelogram tile. Small values create thin diamond tiles, and higher values create fatter parallelogram tiles.
- _width_ (`distance/NSNumber`): The width of a tile.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(person-segmentation _quality-level_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `person-segmentation` (`CIPersonSegmentation`).
Returns a segmentation mask that is red in the portions of an image that are likely to be persons. The returned image may have a different size and aspect ratio from the input image.

- _quality-level_ (`integer/NSNumber`): Determines the size and quality of the resulting segmentation mask. The value can be a number where 0 is accurate, 1 is balanced, and 2 is fast.

Filter categories: `builtin`, `video`, `stylize`, `still-image`

**(perspective-correction _top-left top-right bottom-right bottom-left crop_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `perspective-correction` (`CIPerspectiveCorrection`).
Apply a perspective correction to an image.

- _top-left_ (`point/CIVector`): The top left coordinate to be perspective corrected.
- _top-right_ (`point/CIVector`): The top right coordinate to be perspective corrected.
- _bottom-right_ (`point/CIVector`): The bottom right coordinate to be perspective corrected.
- _bottom-left_ (`point/CIVector`): The bottom left coordinate to be perspective corrected.
- _crop_ (`boolean/NSNumber`)

Filter categories: `builtin`, `video`, `high-dynamic-range`, `still-image`, `geometry-adjustment`

**(perspective-rotate _focal-length pitch yaw roll_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `perspective-rotate` (`CIPerspectiveRotate`).
Apply a homogenous rotation transform to an image.

- _focal-length_ (`scalar/NSNumber`): 35mm equivalent focal length of the input image.
- _pitch_ (`angle/NSNumber`): Pitch angle in radians.
- _yaw_ (`angle/NSNumber`): Yaw angle in radians.
- _roll_ (`angle/NSNumber`): Roll angle in radians.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `still-image`, `geometry-adjustment`

**(perspective-tile _top-left top-right bottom-right bottom-left_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `perspective-tile` (`CIPerspectiveTile`).
Applies a perspective transform to an image and then tiles the result.

- _top-left_ (`point/CIVector`): The top left coordinate of a tile.
- _top-right_ (`point/CIVector`): The top right coordinate of a tile.
- _bottom-right_ (`point/CIVector`): The bottom right coordinate of a tile.
- _bottom-left_ (`point/CIVector`): The bottom left coordinate of a tile.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(perspective-transform _top-left top-right bottom-right bottom-left_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `perspective-transform` (`CIPerspectiveTransform`).
Alters the geometry of an image to simulate the observer changing viewing position. You can use the perspective filter to skew an image.

- _top-left_ (`point/CIVector`): The top left coordinate to map the image to.
- _top-right_ (`point/CIVector`): The top right coordinate to map the image to.
- _bottom-right_ (`point/CIVector`): The bottom right coordinate to map the image to.
- _bottom-left_ (`point/CIVector`): The bottom left coordinate to map the image to.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `still-image`, `geometry-adjustment`

**(perspective-transform-with-extent _extent top-left top-right bottom-right bottom-left_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `perspective-transform-with-extent` (`CIPerspectiveTransformWithExtent`).
Alters the geometry of an image to simulate the observer changing viewing position. You can use the perspective filter to skew an image.

- _extent_ (`rect/CIVector`): A rectangle that defines the extent of the effect.
- _top-left_ (`point/CIVector`): The top left coordinate to map the image to.
- _top-right_ (`point/CIVector`): The top right coordinate to map the image to.
- _bottom-right_ (`point/CIVector`): The bottom right coordinate to map the image to.
- _bottom-left_ (`point/CIVector`): The bottom left coordinate to map the image to.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `still-image`, `geometry-adjustment`

**(photo-effect-chrome _extrapolate_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `photo-effect-chrome` (`CIPhotoEffectChrome`).
Apply a “Chrome” style effect to an image.

- _extrapolate_ (`boolean/NSNumber`): If true, then the color effect will be extrapolated if the input image contains RGB component values outside the range 0.0 to 1.0.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(photo-effect-fade _extrapolate_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `photo-effect-fade` (`CIPhotoEffectFade`).
Apply a “Fade” style effect to an image.

- _extrapolate_ (`boolean/NSNumber`): If true, then the color effect will be extrapolated if the input image contains RGB component values outside the range 0.0 to 1.0.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(photo-effect-instant _extrapolate_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `photo-effect-instant` (`CIPhotoEffectInstant`).
Apply an “Instant” style effect to an image.

- _extrapolate_ (`boolean/NSNumber`): If true, then the color effect will be extrapolated if the input image contains RGB component values outside the range 0.0 to 1.0.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(photo-effect-mono _extrapolate_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `photo-effect-mono` (`CIPhotoEffectMono`).
Apply a “Mono” style effect to an image.

- _extrapolate_ (`boolean/NSNumber`): If true, then the color effect will be extrapolated if the input image contains RGB component values outside the range 0.0 to 1.0.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(photo-effect-noir _extrapolate_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `photo-effect-noir` (`CIPhotoEffectNoir`).
Apply a “Noir” style effect to an image.

- _extrapolate_ (`boolean/NSNumber`): If true, then the color effect will be extrapolated if the input image contains RGB component values outside the range 0.0 to 1.0.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(photo-effect-process _extrapolate_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `photo-effect-process` (`CIPhotoEffectProcess`).
Apply a “Process” style effect to an image.

- _extrapolate_ (`boolean/NSNumber`): If true, then the color effect will be extrapolated if the input image contains RGB component values outside the range 0.0 to 1.0.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(photo-effect-tonal _extrapolate_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `photo-effect-tonal` (`CIPhotoEffectTonal`).
Apply a “Tonal” style effect to an image.

- _extrapolate_ (`boolean/NSNumber`): If true, then the color effect will be extrapolated if the input image contains RGB component values outside the range 0.0 to 1.0.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(photo-effect-transfer _extrapolate_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `photo-effect-transfer` (`CIPhotoEffectTransfer`).
Apply a “Transfer” style effect to an image.

- _extrapolate_ (`boolean/NSNumber`): If true, then the color effect will be extrapolated if the input image contains RGB component values outside the range 0.0 to 1.0.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(pinch-distortion _center radius scale_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `pinch-distortion` (`CIPinchDistortion`).
Creates a rectangular-shaped area that pinches source pixels inward, distorting those pixels closest to the rectangle the most.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _radius_ (`distance/NSNumber`): The radius determines how many pixels are used to create the distortion. The larger the radius, the wider the extent of the distortion.
- _scale_ (`scalar/NSNumber`): The amount of pinching. A value of 0.0 has no effect. A value of 1.0 is the maximum pinch.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(pin-light-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `pin-light-blend-mode` (`CIPinLightBlendMode`).
Unpremultiplies the source and background image sample color, combines them according to the relative difference, and then blends the result with the background according to the PDF basic compositing formula. Source image values that are brighter than the destination will produce an output that is lighter than the destination. Source image values that are darker than the destination will produce an output that is darker than the destination.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(pixellate _center scale_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `pixellate` (`CIPixellate`).
Makes an image blocky.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _scale_ (`distance/NSNumber`): The scale determines the size of the squares. Larger values result in larger squares.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(pointillize _radius center_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `pointillize` (`CIPointillize`).
Renders the source image in a pointillistic style.

- _radius_ (`distance/NSNumber`): The radius of the circles in the resulting pattern.
- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(radial-gradient _center radius0 radius1 color0 color1_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `radial-gradient` (`CIRadialGradient`).
Generates a gradient that varies radially between two circles having the same center. It is valid for one of the two circles to have a radius of 0.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _radius0_ (`distance/NSNumber`): The radius of the starting circle to use in the gradient.
- _radius1_ (`distance/NSNumber`): The radius of the ending circle to use in the gradient.
- _color0_ (`color/CIColor`): The first color to use in the gradient.
- _color1_ (`color/CIColor`): The second color to use in the gradient.

Filter categories: `builtin`, `video`, `gradient`, `high-dynamic-range`, `still-image`

**(ripple-transition _target-image shading-image center extent time width scale_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `ripple-transition` (`CIRippleTransition`).
Transitions from one image to another by creating a circular wave that expands from the center point, revealing the new image in the wake of the wave.

- _target-image_ (`abstract-image/CIImage`): The target image for a transition.
- _shading-image_ (`abstract-image/CIImage`): An image that looks like a shaded sphere enclosed in a square image.
- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _extent_ (`rect/CIVector`): A rectangle that defines the extent of the effect.
- _time_ (`time/NSNumber`): The parametric time of the transition. This value drives the transition from start (at time 0) to end (at time 1).
- _width_ (`distance/NSNumber`): The width of the ripple.
- _scale_ (`scalar/NSNumber`): A value that determines whether the ripple starts as a bulge (higher value) or a dimple (lower value).

Filter categories: `builtin`, `video`, `transition`, `high-dynamic-range`, `still-image`

**(row-average _extent_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `row-average` (`CIRowAverage`).
Calculates the average color for each row of the specified area in an image, returning the result in a 1D image.

- _extent_ (`rect/CIVector`): A rectangle that specifies the subregion of the image that you want to process.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `reduction`, `still-image`

**(saliency-map-filter)**

Returns an image processor for image filter `saliency-map-filter` (`CISaliencyMapFilter`).
Generates output image as a saliency map of the input image.


Filter categories: `builtin`, `video`, `stylize`, `still-image`

**(sample-nearest)**

Returns an image processor for image filter `sample-nearest` (`CISampleNearest`).
Produces an image that forces the image sampling to “nearest” mode instead of the default “linear” mode. This filter can be used to alter the behavior of filters that alter the geometry of an image. The output of this filter should be passed as the input to the geometry filter. For example, passing the output of this filter to CIAffineTransform can be used to produce a pixelated upsampled image.


Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(saturation-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `saturation-blend-mode` (`CISaturationBlendMode`).
Uses the luminance and hue values of the background with the saturation of the source image. Areas of the background that have no saturation (that is, pure gray areas) do not produce a change.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(screen-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `screen-blend-mode` (`CIScreenBlendMode`).
Multiplies the inverse of the source image samples with the inverse of the background image samples. This results in colors that are at least as light as either of the two contributing sample colors.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(sepia-tone _intensity_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `sepia-tone` (`CISepiaTone`).
Maps the colors of an image to various shades of brown.

- _intensity_ (`scalar/NSNumber`): The intensity of the sepia effect. A value of 1.0 creates a monochrome sepia image. A value of 0.0 has no effect on the image.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(shaded-material _shading-image scale_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `shaded-material` (`CIShadedMaterial`).
Produces a shaded image from a height field. The height field is defined to have greater heights with lighter shades, and lesser heights (lower areas) with darker shades. You can combine this filter with the “Height Field From Mask” filter to produce quick shadings of masks, such as text.

- _shading-image_ (`abstract-image/CIImage`): The image to use as the height field. The resulting image has greater heights with lighter shades, and lesser heights (lower areas) with darker shades.
- _scale_ (`distance/NSNumber`): The scale of the effect. The higher the value, the more dramatic the effect.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(sharpen-luminance _sharpness radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `sharpen-luminance` (`CISharpenLuminance`).
Increases image detail by sharpening. It operates on the luminance of the image; the chrominance of the pixels remains unaffected.

- _sharpness_ (`scalar/NSNumber`): The amount of sharpening to apply. Larger values are sharper.
- _radius_ (`scalar/NSNumber`): The distance from the center of the effect.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `sharpen`, `still-image`

**(sixfold-reflected-tile _center angle width_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `sixfold-reflected-tile` (`CISixfoldReflectedTile`).
Produces a tiled image from a source image by applying a 6-way reflected symmetry.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _angle_ (`angle/NSNumber`): The angle in radians of the tiled pattern.
- _width_ (`distance/NSNumber`): The width of a tile.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(sixfold-rotated-tile _center angle width_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `sixfold-rotated-tile` (`CISixfoldRotatedTile`).
Produces a tiled image from a source image by rotating the source at increments of 60 degrees.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _angle_ (`angle/NSNumber`): The angle in radians of the tiled pattern.
- _width_ (`distance/NSNumber`): The width of a tile.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(smooth-linear-gradient _point0 point1 color0 color1_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image generator for image filter `smooth-linear-gradient` (`CISmoothLinearGradient`).
Generates a gradient that varies along a linear axis between two defined endpoints.

- _point0_ (`point/CIVector`): The starting position of the gradient -- where the first color begins.
- _point1_ (`point/CIVector`): The ending position of the gradient -- where the second color begins.
- _color0_ (`color/CIColor`): The first color to use in the gradient.
- _color1_ (`color/CIColor`): The second color to use in the gradient.

Filter categories: `builtin`, `video`, `gradient`, `high-dynamic-range`, `still-image`

**(sobel-gradients)**

Returns an image processor for image filter `sobel-gradients` (`CISobelGradients`).
Applies multichannel 3 by 3 Sobel gradient filter to an image. The resulting image has maximum horizontal gradient in the red channel and the maximum vertical gradient in the green channel. The gradient values can be positive or negative.


Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(soft-light-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `soft-light-blend-mode` (`CISoftLightBlendMode`).
Either darkens or lightens colors, depending on the source image sample color. If the source image sample color is lighter than 50% gray, the background is lightened, similar to dodging. If the source image sample color is darker than 50% gray, the background is darkened, similar to burning. If the source image sample color is equal to 50% gray, the background is not changed. Image samples that are equal to pure black or pure white produce darker or lighter areas, but do not result in pure black or white. The overall effect is similar to what you would achieve by shining a diffuse spotlight on the source image.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(source-atop-compositing _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `source-atop-compositing` (`CISourceAtopCompositing`).
Places the source image over the background image, then uses the luminance of the background image to determine what to show. The composite shows the background image and only those portions of the source image that are over visible parts of the background.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `high-dynamic-range`, `interlaced`, `still-image`, `non-square-pixels`

**(source-in-compositing _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `source-in-compositing` (`CISourceInCompositing`).
Uses the second image to define what to leave in the source image, effectively cropping the image.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `high-dynamic-range`, `interlaced`, `still-image`, `non-square-pixels`

**(source-out-compositing _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `source-out-compositing` (`CISourceOutCompositing`).
Uses the second image to define what to take out of the first image.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `high-dynamic-range`, `interlaced`, `still-image`, `non-square-pixels`

**(source-over-compositing _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `source-over-compositing` (`CISourceOverCompositing`).
Places the second image over the first.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `high-dynamic-range`, `interlaced`, `still-image`, `non-square-pixels`

**(spot-color _center-color1 replacement-color1 closeness1 contrast1 center-color2 replacement-color2 closeness2 contrast2 center-color3 replacement-color3 closeness3 contrast3_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `spot-color` (`CISpotColor`).
Replaces one or more color ranges with spot colors.

- _center-color1_ (`color/CIColor`): The center value of the first color range to replace.
- _replacement-color1_ (`color/CIColor`): A replacement color for the first color range.
- _closeness1_ (`scalar/NSNumber`): A value that indicates how close the first color must match before it is replaced.
- _contrast1_ (`scalar/NSNumber`): The contrast of the first replacement color.
- _center-color2_ (`color/CIColor`): The center value of the second color range to replace.
- _replacement-color2_ (`color/CIColor`): A replacement color for the second color range.
- _closeness2_ (`scalar/NSNumber`): A value that indicates how close the second color must match before it is replaced.
- _contrast2_ (`scalar/NSNumber`): The contrast of the second replacement color.
- _center-color3_ (`color/CIColor`): The center value of the third color range to replace.
- _replacement-color3_ (`color/CIColor`): A replacement color for the third color range.
- _closeness3_ (`scalar/NSNumber`): A value that indicates how close the third color must match before it is replaced.
- _contrast3_ (`scalar/NSNumber`): The contrast of the third replacement color.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(spot-light _light-position light-points-at brightness concentration color_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `spot-light` (`CISpotLight`).
Applies a directional spotlight effect to an image.

- _light-position_ (`coordinate-3d/CIVector`): The x and y position of the spotlight.
- _light-points-at_ (`coordinate-3d/CIVector`): The x and y position that the spotlight points at.
- _brightness_ (`distance/NSNumber`): The brightness of the spotlight.
- _concentration_ (`scalar/NSNumber`): The spotlight size. The smaller the value, the more tightly focused the light beam.
- _color_ (`opaque-color/CIColor`): The color of the spotlight.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `stylize`, `still-image`

**(srgbtone-curve-to-linear)**

Returns an image processor for image filter `srgbtone-curve-to-linear` (`CISRGBToneCurveToLinear`).
Converts an image in sRGB space to linear space.


Filter categories: `builtin`, `video`, `high-dynamic-range`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(straighten-filter _angle_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `straighten-filter` (`CIStraightenFilter`).
Rotates a source image by the specified angle in radians. The image is then scaled and cropped so that the rotated image fits the extent of the input image.

- _angle_ (`angle/NSNumber`): The angle in radians of the effect.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `still-image`, `geometry-adjustment`

**(stretch-crop _size crop-amount center-stretch-amount_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `stretch-crop` (`CIStretchCrop`).
Distorts an image by stretching and or cropping to fit a target size.

- _size_ (`point/CIVector`): The size in pixels of the output image.
- _crop-amount_ (`scalar/NSNumber`): Determines if and how much cropping should be used to achieve the target size. If value is 0 then only stretching is used. If 1 then only cropping is used.
- _center-stretch-amount_ (`scalar/NSNumber`): Determine how much the center of the image is stretched if stretching is used. If value is 0 then the center of the image maintains the original aspect ratio. If 1 then the image is stretched uniformly.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(subtract-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `subtract-blend-mode` (`CISubtractBlendMode`).
Unpremultiplies the source and background image sample colors, subtracts the source from the background, and then blends the result with the background according to the PDF basic compositing formula. Source image values that are black produces output that is the same as the background. Source image values that are non-black darken the background color values.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(swipe-transition _target-image extent color time angle width opacity_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `swipe-transition` (`CISwipeTransition`).
Transitions from one image to another by simulating a swiping action.

- _target-image_ (`abstract-image/CIImage`): The target image for a transition.
- _extent_ (`rect/CIVector`): The extent of the effect.
- _color_ (`opaque-color/CIColor`): The color of the swipe.
- _time_ (`time/NSNumber`): The parametric time of the transition. This value drives the transition from start (at time 0) to end (at time 1).
- _angle_ (`angle/NSNumber`): The angle in radians of the swipe.
- _width_ (`distance/NSNumber`): The width of the swipe.
- _opacity_ (`scalar/NSNumber`): The opacity of the swipe.

Filter categories: `builtin`, `video`, `transition`, `high-dynamic-range`, `still-image`

**(temperature-and-tint _neutral target-neutral_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `temperature-and-tint` (`CITemperatureAndTint`).
Adapt the reference white point for an image.

- _neutral_ (`offset/CIVector`): A vector containing the source white point defined by color temperature and tint or chromaticity (x,y).
- _target-neutral_ (`offset/CIVector`): A vector containing the desired white point defined by color temperature and tint or chromaticity (x,y).

Filter categories: `builtin`, `video`, `high-dynamic-range`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(thermal)**

Returns an image processor for image filter `thermal` (`CIThermal`).
Apply a “Thermal” style effect to an image.


Filter categories: `builtin`, `video`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(tone-curve _point0 point1 point2 point3 point4 extrapolate_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `tone-curve` (`CIToneCurve`).
Adjusts tone response of the R, G, and B channels of an image. The input points are five x,y values that are interpolated using a spline curve. The curve is applied in a perceptual (gamma 2) version of the working space.

- _point0_ (`offset/CIVector`)
- _point1_ (`offset/CIVector`)
- _point2_ (`offset/CIVector`)
- _point3_ (`offset/CIVector`)
- _point4_ (`offset/CIVector`)
- _extrapolate_ (`boolean/NSNumber`): If true, then the color effect will be extrapolated if the input image contains RGB component values outside the range 0.0 to 1.0.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(tone-map-headroom _source-headroom target-headroom_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `tone-map-headroom` (`CIToneMapHeadroom`).
Apply a global tone curve to an image that reduces colors from a source headroom value to a target headroom value.

- _source-headroom_ (`scalar/NSNumber`): Specifies the headroom of the input image.
- _target-headroom_ (`scalar/NSNumber`): Specifies the target headroom of the output image.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(torus-lens-distortion _center radius width refraction_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `torus-lens-distortion` (`CITorusLensDistortion`).
Creates a torus-shaped lens and distorts the portion of the image over which the lens is placed.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _radius_ (`distance/NSNumber`): The outer radius of the torus.
- _width_ (`distance/NSNumber`): The width of the ring.
- _refraction_ (`scalar/NSNumber`): The refraction of the glass.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(triangle-kaleidoscope _point size rotation decay_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `triangle-kaleidoscope` (`CITriangleKaleidoscope`).
Maps a triangular portion of image to a triangular area and then generates a kaleidoscope effect.

- _point_ (`point/CIVector`): The x and y position to use as the center of the triangular area in the input image.
- _size_ (`scalar/NSNumber`): The size in pixels of the triangle.
- _rotation_ (`angle/NSNumber`): Rotation angle in radians of the triangle.
- _decay_ (`scalar/NSNumber`): The decay determines how fast the color fades from the center triangle.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(triangle-tile _center angle width_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `triangle-tile` (`CITriangleTile`).
Maps a triangular portion of image to a triangular area and then tiles the result.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _angle_ (`angle/NSNumber`): The angle in radians of the tiled pattern.
- _width_ (`distance/NSNumber`): The width of a tile.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(twelvefold-reflected-tile _center angle width_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `twelvefold-reflected-tile` (`CITwelvefoldReflectedTile`).
Produces a tiled image from a source image by applying a 12-way reflected symmetry.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _angle_ (`angle/NSNumber`): The angle in radians of the tiled pattern.
- _width_ (`distance/NSNumber`): The width of a tile.

Filter categories: `tile-effect`, `builtin`, `video`, `high-dynamic-range`, `still-image`

**(twirl-distortion _center radius angle_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `twirl-distortion` (`CITwirlDistortion`).
Rotates pixels around a point to give a twirling effect. You can specify the number of rotations as well as the center and radius of the effect.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _radius_ (`distance/NSNumber`): The radius determines how many pixels are used to create the distortion. The larger the radius, the wider the extent of the distortion.
- _angle_ (`angle/NSNumber`): The angle in radians of the twirl. Values can be positive or negative.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(unsharp-mask _radius intensity_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `unsharp-mask` (`CIUnsharpMask`).
Increases the contrast of the edges between pixels of different colors in an image.

- _radius_ (`distance/NSNumber`): The radius around a given pixel to apply the unsharp mask. The larger the radius, the more of the image is affected.
- _intensity_ (`scalar/NSNumber`): The intensity of the effect. The larger the value, the more contrast in the affected area.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `sharpen`, `still-image`

**(vibrance _amount_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `vibrance` (`CIVibrance`).
Adjusts the saturation of an image while keeping pleasing skin tones.

- _amount_ (`scalar/NSNumber`): The amount to adjust the saturation.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(vignette _intensity radius_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `vignette` (`CIVignette`).
Applies a vignette shading to the corners of an image.

- _intensity_ (`scalar/NSNumber`): The intensity of the effect.
- _radius_ (`scalar/NSNumber`): The distance from the center of the effect.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`

**(vignette-effect _center radius intensity falloff_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `vignette-effect` (`CIVignetteEffect`).
Applies a vignette shading to the corners of an image.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _radius_ (`distance/NSNumber`): The distance from the center of the effect.
- _intensity_ (`scalar/NSNumber`): The intensity of the effect.
- _falloff_ (`scalar/NSNumber`): The falloff of the effect.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `interlaced`, `color-effect`, `still-image`

**(vivid-light-blend-mode _background-image_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `vivid-light-blend-mode` (`CIVividLightBlendMode`).
A blend mode that is a combination of color burn and color dodge blend modes.

- _background-image_ (`abstract-image/CIImage`): The image to use as a background image.

Filter categories: `composite-operation`, `builtin`, `video`, `interlaced`, `still-image`, `non-square-pixels`

**(vortex-distortion _center radius angle_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `vortex-distortion` (`CIVortexDistortion`).
Rotates pixels around a point to simulate a vortex. You can specify the number of rotations as well the center and radius of the effect. 

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _radius_ (`distance/NSNumber`): The radius determines how many pixels are used to create the distortion. The larger the radius, the wider the extent of the distortion.
- _angle_ (`angle/NSNumber`): The angle in radians of the effect.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `distortion-effect`, `still-image`

**(white-point-adjust _color_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `white-point-adjust` (`CIWhitePointAdjust`).
Adjusts the reference white point for an image and maps all colors in the source using the new reference.

- _color_ (`color/CIColor`): A color to use as the white point.

Filter categories: `builtin`, `video`, `high-dynamic-range`, `color-adjustment`, `interlaced`, `still-image`, `non-square-pixels`

**(xray)**

Returns an image processor for image filter `xray` (`CIXRay`).
Apply an “XRay” style effect to an image.


Filter categories: `builtin`, `video`, `interlaced`, `color-effect`, `still-image`, `non-square-pixels`

**(zoom-blur _center amount_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an image processor for image filter `zoom-blur` (`CIZoomBlur`).
Simulates the effect of zooming the camera while capturing the image.

- _center_ (`point/CIVector`): The center of the effect as x and y pixel coordinates.
- _amount_ (`distance/NSNumber`): The zoom-in amount. Larger values result in more zooming in.

Filter categories: `builtin`, `blur`, `video`, `high-dynamic-range`, `still-image`
