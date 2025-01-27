;;; LISPKIT IMAGE PROCESS
;;;
;;; TODO
;;; 238 image processors
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2025 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use
;;; this file except in compliance with the License. You may obtain a copy of the
;;; License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed
;;; under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
;;; CONDITIONS OF ANY KIND, either express or implied. See the License for the
;;; specific language governing permissions and limitations under the License.

(define-library (lispkit image process)

  (export filter-pipeline
          make-filter-proc
          accordion-fold-transition
          addition-compositing
          affine-clamp
          affine-tile
          affine-transform
          area-alpha-weighted-histogram
          area-average
          area-bounds-red
          area-histogram
          area-logarithmic-histogram
          area-maximum
          area-maximum-alpha
          area-minimum
          area-minimum-alpha
          area-min-max
          area-min-max-red
          attributed-text-image-generator
          aztec-code-generator
          bars-swipe-transition
          bicubic-scale-transform
          blend-with-alpha-mask
          blend-with-blue-mask
          blend-with-mask
          blend-with-red-mask
          bloom
          blurred-rectangle-generator
          bokeh-blur
          box-blur
          bump-distortion
          bump-distortion-linear
          canny-edge-detector
          checkerboard-generator
          circle-splash-distortion
          circular-screen
          circular-wrap
          clamp
          cmyk-halftone
          code128-barcode-generator
          color-absolute-difference
          color-blend-mode
          color-burn-blend-mode
          color-clamp
          color-controls
          color-cross-polynomial
          color-cube
          color-cubes-mixed-with-mask
          color-cube-with-color-space
          color-curves
          color-dodge-blend-mode
          color-invert
          color-map
          color-matrix
          color-monochrome
          color-polynomial
          color-posterize
          color-threshold
          color-threshold-otsu
          column-average
          comic-effect
          constant-color-generator
          convert-lab-to-rgb
          convert-rgb-to-lab
          convolution-3x3
          convolution-5x5
          convolution-7x7
          convolution9-horizontal
          convolution9-vertical
          convolution-rgb-3x3
          convolution-rgb-5x5
          convolution-rgb-7x7
          convolution-rgb9-horizontal
          convolution-rgb9-vertical
          copy-machine-transition
          crop
          crystallize
          darken-blend-mode
          depth-of-field
          depth-to-disparity
          difference-blend-mode
          disc-blur
          disintegrate-with-mask-transition
          disparity-to-depth
          displacement-distortion
          dissolve-transition
          distance-gradient-from-red-mask
          dither
          divide-blend-mode
          document-enhancer
          dot-screen
          droste
          edge-preserve-upsample-filter
          edges
          edge-work
          eightfold-reflected-tile
          exclusion-blend-mode
          exposure-adjust
          false-color
          flash-transition
          fourfold-reflected-tile
          fourfold-rotated-tile
          fourfold-translated-tile
          gabor-gradients
          gamma-adjust
          gaussian-blur
          gaussian-gradient
          glass-distortion
          glass-lozenge
          glide-reflected-tile
          gloom
          guided-filter
          hard-light-blend-mode
          hatched-screen
          height-field-from-mask
          hexagonal-pixellate
          highlight-shadow-adjust
          histogram-display-filter
          hole-distortion
          hue-adjust
          hue-blend-mode
          hue-saturation-value-gradient
          kaleidoscope
          keystone-correction-combined
          keystone-correction-horizontal
          keystone-correction-vertical
          kmeans
          lab-delta-e
          lanczos-scale-transform
          lenticular-halo-generator
          lighten-blend-mode
          light-tunnel
          linear-burn-blend-mode
          linear-dodge-blend-mode
          linear-gradient
          linear-light-blend-mode
          linear-to-srgbtone-curve
          line-overlay
          line-screen
          luminosity-blend-mode
          masked-variable-blur
          mask-to-alpha
          maximum-component
          maximum-compositing
          maximum-scale-transform
          median-filter
          mesh-generator
          minimum-component
          minimum-compositing
          mix
          mod-transition
          morphology-gradient
          morphology-maximum
          morphology-minimum
          morphology-rectangle-maximum
          morphology-rectangle-minimum
          motion-blur
          multiply-blend-mode
          multiply-compositing
          nine-part-stretched
          nine-part-tiled
          noise-reduction
          op-tile
          overlay-blend-mode
          page-curl-transition
          page-curl-with-shadow-transition
          palette-centroid
          palettize
          parallelogram-tile
          pdf417-barcode-generator
          person-segmentation
          perspective-correction
          perspective-rotate
          perspective-tile
          perspective-transform
          perspective-transform-with-extent
          photo-effect-chrome
          photo-effect-fade
          photo-effect-instant
          photo-effect-mono
          photo-effect-noir
          photo-effect-process
          photo-effect-tonal
          photo-effect-transfer
          pinch-distortion
          pin-light-blend-mode
          pixellate
          pointillize
          qrcode-generator
          radial-gradient
          random-generator
          ripple-transition
          rounded-rectangle-generator
          rounded-rectangle-stroke-generator
          row-average
          saliency-map-filter
          sample-nearest
          saturation-blend-mode
          screen-blend-mode
          sepia-tone
          shaded-material
          sharpen-luminance
          sixfold-reflected-tile
          sixfold-rotated-tile
          smooth-linear-gradient
          sobel-gradients
          soft-light-blend-mode
          source-atop-compositing
          source-in-compositing
          source-out-compositing
          source-over-compositing
          spot-color
          spot-light
          srgbtone-curve-to-linear
          star-shine-generator
          straighten-filter
          stretch-crop
          stripes-generator
          subtract-blend-mode
          sunbeams-generator
          swipe-transition
          temperature-and-tint
          text-image-generator
          thermal
          tone-curve
          tone-map-headroom
          torus-lens-distortion
          triangle-kaleidoscope
          triangle-tile
          twelvefold-reflected-tile
          twirl-distortion
          unsharp-mask
          vibrance
          vignette
          vignette-effect
          vivid-light-blend-mode
          vortex-distortion
          white-point-adjust
          xray
          zoom-blur)
  
  (import (lispkit base)
          (lispkit image))
  
  (begin
    
    ;; Creates an image filter pipeline from the given image processors. An image
    ;; filter pipeline is a composite image processor that executes the encapsulated
    ;; image processors in sequence, piping the output of a processor into the input
    ;; of the next processor. The result of the final processor is returned by an
    ;; image filter pipeline.
    (define (filter-pipeline . processors)
      (lambda (img)
        (do ((input img ((car procs) input))
             (procs processors (cdr procs)))
            ((null? procs) input))))
    
    ;; Creates an image processor for a given configured filter.
    (define (make-filter-proc1 filter)
      (lambda (img)
        (image-filter-argument-set! filter 'input-image img)
        (image-filter-output filter)))
    
    ;; Creates an image processor for a list of configured filters that are being applied
    ;; in sequence.
    (define (make-filter-proc filters)
      (cond ((null? filters) identity)
            ((pair? filters) (apply filter-pipeline (map make-filter-proc1 filters)))
            (else (make-filter-proc1 filters))))
    
    ;; Returns an image processor for image filter accordion-fold-transition
    ;; (CIAccordionFoldTransition).
    ;; Transitions from one image to another of a differing dimensions by unfolding.
    ;;   - target-image (abstract-image/CIImage): The target image for a transition.
    ;;   - bottom-height (distance/NSNumber): The height in pixels from the bottom of
    ;;     the image to the bottom of the folded part of the transition.
    ;;   - number-of-folds (scalar/NSNumber): The number of folds used in the transition.
    ;;   - fold-shadow-amount (scalar/NSNumber): A value that specifies the intensity of
    ;;     the shadow in the transition.
    ;;   - time (time/NSNumber): The duration of the effect.
    ;; Filter categories: still-image, builtin, transition, high-dynamic-range, video
    (define (accordion-fold-transition target-image
                                       bottom-height
                                       number-of-folds
                                       fold-shadow-amount
                                       time)
      (make-filter-proc
        (make-image-filter
          'accordion-fold-transition
          `((input-target-image . ,target-image)
            (input-bottom-height . ,bottom-height)
            (input-number-of-folds . ,number-of-folds)
            (input-fold-shadow-amount . ,fold-shadow-amount)
            (input-time . ,time)))))

    ;; Returns an image processor for image filter addition-compositing (CIAdditionCompositing).
    ;; Adds color components to achieve a brightening effect. This filter is typically used
    ;; to add highlights and lens flare effects.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, high-dynamic-range, video
    (define (addition-compositing background-image)
      (make-filter-proc
        (make-image-filter
          'addition-compositing
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter affine-clamp (CIAffineClamp).
    ;; Performs an affine transformation on a source image and then clamps the pixels at
    ;; the edge of the transformed image, extending them outwards. This filter performs
    ;; similarly to the “Affine Transform” filter except that it produces an image with
    ;; infinite extent. You can use this filter when you need to blur an image but you
    ;; want to avoid a soft, black fringe along the edges.
    ;;   - transform (transformation/NSAffineTransform): The transform to apply to the image.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (affine-clamp transform)
      (make-filter-proc
        (make-image-filter
          'affine-clamp
          `((input-transform . ,transform)))))

    ;; Returns an image processor for image filter affine-tile (CIAffineTile).
    ;; Applies an affine transformation to an image and then tiles the transformed image.
    ;;   - transform (transformation/NSAffineTransform): The transform to apply to the image.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (affine-tile transform)
      (make-filter-proc
        (make-image-filter
          'affine-tile
          `((input-transform . ,transform)))))

    ;; Returns an image processor for image filter affine-transform (CIAffineTransform).
    ;; Applies an affine transformation to an image. You can scale, translate, or rotate
    ;; the input image. You can also apply a combination of these operations.
    ;;   - transform (transformation/NSAffineTransform): A transform to apply to the image.
    ;; Filter categories: still-image, builtin, geometry-adjustment, high-dynamic-range, video
    (define (affine-transform transform)
      (make-filter-proc
        (make-image-filter
          'affine-transform
          `((input-transform . ,transform)))))

    ;; Returns an image processor for image filter area-alpha-weighted-histogram
    ;; (CIAreaAlphaWeightedHistogram).
    ;; Calculates alpha-weighted histograms of the unpremultiplied R, G, B channels for the
    ;; specified area of an image. The output image is a one pixel tall image containing
    ;; the histogram data for the RGB channels.
    ;;   - extent (rect/CIVector): A rectangle that defines the extent of the effect.
    ;;   - scale (scalar/NSNumber): The scale value to use for the histogram values. If
    ;;     the scale is 1.0 and the image is opaque, then the bins in the resulting image
    ;;     will add up to 1.0.
    ;;   - count (scalar/NSNumber): The number of bins for the histogram. This value will
    ;;     determine the width of the output image.
    ;; Filter categories: still-image, builtin, reduction, video
    (define (area-alpha-weighted-histogram extent scale count)
      (make-filter-proc
        (make-image-filter
          'area-alpha-weighted-histogram
          `((input-extent . ,extent)
            (input-scale . ,scale)
            (input-count . ,count)))))

    ;; Returns an image processor for image filter area-average (CIAreaAverage).
    ;; Calculates the average color for the specified area in an image, returning the
    ;; result in a pixel.
    ;;   - extent (rect/CIVector): A rectangle that specifies the subregion of the image
    ;;     that you want to process.
    ;; Filter categories: still-image, builtin, reduction, high-dynamic-range, video
    (define (area-average extent)
      (make-filter-proc
        (make-image-filter
          'area-average
          `((input-extent . ,extent)))))

    ;; Returns an image processor for image filter area-bounds-red (CIAreaBoundsRed).
    ;; Calculates the approximate bounding box of pixels within the specified area of
    ;; an image where the red component values are non-zero. The result is 1x1 pixel image
    ;; where the RGBA values contain the normalized X,Y,W,H dimensions of the bounding box.
    ;;   - extent (rect/CIVector): A rectangle that specifies the subregion of the image
    ;;     that you want to process.
    ;; Filter categories: still-image, builtin, reduction, high-dynamic-range, video
    (define (area-bounds-red extent)
      (make-filter-proc
        (make-image-filter
          'area-bounds-red
          `((input-extent . ,extent)))))

    ;; Returns an image processor for image filter area-histogram (CIAreaHistogram).
    ;; Calculates histograms of the R, G, B, and A channels of the specified area of an
    ;; image. The output image is a one pixel tall image containing the histogram data
    ;; for all four channels.
    ;;   - extent (rect/CIVector): A rectangle that, after intersection with the image
    ;;     extent, specifies the subregion of the image that you want to process.
    ;;   - scale (scalar/NSNumber): The scale value to use for the histogram values.
    ;;     If the scale is 1.0, then the bins in the resulting image will add up to 1.0.
    ;;   - count (scalar/NSNumber): The number of bins for the histogram. This value
    ;;     will determine the width of the output image.
    ;; Filter categories: still-image, builtin, reduction, video
    (define (area-histogram extent scale count)
      (make-filter-proc
        (make-image-filter
          'area-histogram
          `((input-extent . ,extent)
            (input-scale . ,scale)
            (input-count . ,count)))))

    ;; Returns an image processor for image filter area-logarithmic-histogram
    ;; (CIAreaLogarithmicHistogram).
    ;; Calculates histogram of the R, G, B, and A channels of the specified area of an
    ;; image. Before binning, the R, G, and B channel values are transformed by the log
    ;; base two function. The output image is a one pixel tall image containing the
    ;; histogram data for all four channels.
    ;;   - extent (rect/CIVector): A rectangle that defines the extent of the effect.
    ;;   - scale (scalar/NSNumber): The amount of the effect.
    ;;   - count (scalar/NSNumber): The number of bins for the histogram. This value will
    ;;     determine the width of the output image.
    ;;   - minimum-stop (scalar/NSNumber): The minimum of the range of color channel values
    ;;     to be in the logarithmic histogram image.
    ;;   - maximum-stop (scalar/NSNumber): The maximum of the range of color channel values
    ;;     to be in the logarithmic histogram image.
    ;; Filter categories: still-image, builtin, reduction, high-dynamic-range, video
    (define (area-logarithmic-histogram extent scale count minimum-stop maximum-stop)
      (make-filter-proc
        (make-image-filter
          'area-logarithmic-histogram
          `((input-extent . ,extent)
            (input-scale . ,scale)
            (input-count . ,count)
            (input-minimum-stop . ,minimum-stop)
            (input-maximum-stop . ,maximum-stop)))))

    ;; Returns an image processor for image filter area-maximum (CIAreaMaximum).
    ;; Calculates the maximum component values for the specified area in an image, returning
    ;; the result in a pixel.
    ;;   - extent (rect/CIVector): A rectangle that specifies the subregion of the image that
    ;;     you want to process.
    ;; Filter categories: still-image, builtin, reduction, high-dynamic-range, video
    (define (area-maximum extent)
      (make-filter-proc
        (make-image-filter
          'area-maximum
          `((input-extent . ,extent)))))

    ;; Returns an image processor for image filter area-maximum-alpha (CIAreaMaximumAlpha).
    ;; Finds and returns the pixel with the maximum alpha value.
    ;;   - extent (rect/CIVector): A rectangle that specifies the subregion of the image
    ;;     that you want to process.
    ;; Filter categories: still-image, builtin, reduction, high-dynamic-range, video
    (define (area-maximum-alpha extent)
      (make-filter-proc
        (make-image-filter
          'area-maximum-alpha
          `((input-extent . ,extent)))))

    ;; Returns an image processor for image filter area-minimum (CIAreaMinimum).
    ;; Calculates the minimum component values for the specified area in an image, returning
    ;; the result in a pixel.
    ;;   - extent (rect/CIVector): A rectangle that specifies the subregion of the image that
    ;;     you want to process.
    ;; Filter categories: still-image, builtin, reduction, high-dynamic-range, video
    (define (area-minimum extent)
      (make-filter-proc
        (make-image-filter
          'area-minimum
          `((input-extent . ,extent)))))

    ;; Returns an image processor for image filter area-minimum-alpha (CIAreaMinimumAlpha).
    ;; Finds and returns the pixel with the minimum alpha value.
    ;;   - extent (rect/CIVector): A rectangle that specifies the subregion of the image
    ;;     that you want to process.
    ;; Filter categories: still-image, builtin, reduction, high-dynamic-range, video
    (define (area-minimum-alpha extent)
      (make-filter-proc
        (make-image-filter
          'area-minimum-alpha
          `((input-extent . ,extent)))))

    ;; Returns an image processor for image filter area-min-max (CIAreaMinMax).
    ;; Calculates the per-component minimum and maximum value for the specified area in
    ;; an image. The result is returned in a 2x1 image where the component minimum values
    ;; are stored in the pixel on the left.
    ;;   - extent (rect/CIVector): A rectangle that specifies the subregion of the image
    ;;     that you want to process.
    ;; Filter categories: still-image, builtin, reduction, high-dynamic-range, video
    (define (area-min-max extent)
      (make-filter-proc
        (make-image-filter
          'area-min-max
          `((input-extent . ,extent)))))

    ;; Returns an image processor for image filter area-min-max-red (CIAreaMinMaxRed).
    ;; Calculates the minimum and maximum red component value for the specified area in
    ;; an image. The result is returned in the red and green channels of a one pixel image.
    ;;   - extent (rect/CIVector): A rectangle that specifies the subregion of the image
    ;;     that you want to process.
    ;; Filter categories: still-image, builtin, reduction, high-dynamic-range, video
    (define (area-min-max-red extent)
      (make-filter-proc
        (make-image-filter
          'area-min-max-red
          `((input-extent . ,extent)))))

    ;; Returns an image processor for image filter attributed-text-image-generator
    ;; (CIAttributedTextImageGenerator).
    ;; Generate an image attributed string.
    ;;   - text (styled-text/NSAttributedString): The attributed text to render.
    ;;   - scale-factor (scalar/NSNumber): The scale of the font to use for the generated text.
    ;;   - padding (integer/NSNumber): A value for an additional number of pixels to pad
    ;;     around the text’s bounding box.
    ;; Filter categories: still-image, builtin, generator, video
    (define (attributed-text-image-generator text scale-factor padding)
      (image-filter-output
        (make-image-filter
          'attributed-text-image-generator
          `((input-text . ,text)
            (input-scale-factor . ,scale-factor)
            (input-padding . ,padding)))))

    ;; Returns an image processor for image filter aztec-code-generator (CIAztecCodeGenerator).
    ;; Generate an Aztec barcode image for message data.
    ;;   - message (bytevector/NSData): The message to encode in the Aztec Barcode
    ;;   - correction-level (integer/NSNumber): Aztec error correction value between 5 and 95
    ;;   - layers (integer/NSNumber): Aztec layers value between 1 and 32. Set to nil
    ;;     for automatic.
    ;;   - compact-style (boolean/NSNumber): Force a compact style Aztec code to #t or #f.
    ;;     Set to '() for automatic.
    ;; Filter categories: still-image, builtin, generator
    (define (aztec-code-generator message correction-level layers compact-style)
      (image-filter-output
        (make-image-filter
          'aztec-code-generator
          `((input-message . ,message)
            (input-correction-level . ,correction-level)
            (input-layers . ,layers)
            (input-compact-style . ,compact-style)))))

    ;; Returns an image processor for image filter bars-swipe-transition (CIBarsSwipeTransition).
    ;; Transitions from one image to another by swiping rectangular portions of the foreground
    ;; image to disclose the target image.
    ;;   - target-image (abstract-image/CIImage): The target image for a transition.
    ;;   - angle (angle/NSNumber): The angle in radians of the bars.
    ;;   - width (distance/NSNumber): The width of each bar.
    ;;   - bar-offset (scalar/NSNumber): The offset of one bar with respect to another.
    ;;   - time (time/NSNumber): The parametric time of the transition. This value drives the
    ;;     transition from start (at time 0) to end (at time 1).
    ;; Filter categories: still-image, builtin, transition, high-dynamic-range, video
    (define (bars-swipe-transition target-image angle width bar-offset time)
      (make-filter-proc
        (make-image-filter
          'bars-swipe-transition
          `((input-target-image . ,target-image)
            (input-angle . ,angle)
            (input-width . ,width)
            (input-bar-offset . ,bar-offset)
            (input-time . ,time)))))

    ;; Returns an image processor for image filter bicubic-scale-transform
    ;; (CIBicubicScaleTransform).
    ;; Produces a high-quality, scaled version of a source image. The parameters of B and C
    ;; for this filter determine the sharpness or softness of the resampling. The most
    ;; commonly used B and C values are 0.0 and 0.75, respectively.
    ;;   - scale (scalar/NSNumber): The scaling factor to use on the image. Values less
    ;;     than 1.0 scale down the images. Values greater than 1.0 scale up the image.
    ;;   - aspect-ratio (scalar/NSNumber): The additional horizontal scaling factor to use
    ;;     on the image.
    ;;   - b (scalar/NSNumber): Specifies the value of B to use for the cubic resampling
    ;;     function.
    ;;   - c (scalar/NSNumber): Specifies the value of C to use for the cubic resampling
    ;;     function.
    ;; Filter categories: still-image, builtin, geometry-adjustment, non-square-pixels,
    ;;                    high-dynamic-range, video
    (define (bicubic-scale-transform scale aspect-ratio b c)
      (make-filter-proc
        (make-image-filter
          'bicubic-scale-transform
          `((input-scale . ,scale)
            (input-aspect-ratio . ,aspect-ratio)
            (input-b . ,b)
            (input-c . ,c)))))

    ;; Returns an image processor for image filter blend-with-alpha-mask (CIBlendWithAlphaMask).
    ;; Uses values from a mask image to interpolate between an image and the background. When
    ;; a mask alpha value is 0.0, the result is the background. When the mask alpha value is
    ;; 1.0, the result is the image.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;;   - mask-image (abstract-image/CIImage): A masking image.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (blend-with-alpha-mask background-image mask-image)
      (make-filter-proc
        (make-image-filter
          'blend-with-alpha-mask
          `((input-background-image . ,background-image)
            (input-mask-image . ,mask-image)))))

    ;; Returns an image processor for image filter blend-with-blue-mask (CIBlendWithBlueMask).
    ;; Uses values from a mask image to interpolate between an image and the background. When
    ;; a mask blue value is 0.0, the result is the background. When the mask blue value is 1.0,
    ;; the result is the image.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;;   - mask-image (abstract-image/CIImage): A masking image.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (blend-with-blue-mask background-image mask-image)
      (make-filter-proc
        (make-image-filter
          'blend-with-blue-mask
          `((input-background-image . ,background-image)
            (input-mask-image . ,mask-image)))))

    ;; Returns an image processor for image filter blend-with-mask (CIBlendWithMask).
    ;; Uses values from a grayscale mask to interpolate between an image and the background.
    ;; When a mask green value is 0.0, the result is the background. When the mask green value
    ;; is 1.0, the result is the image.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;;   - mask-image (abstract-image/CIImage): A grayscale mask. When a mask value is 0.0,
    ;;     the result is the background. When the mask value is 1.0, the result is the image.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (blend-with-mask background-image mask-image)
      (make-filter-proc
        (make-image-filter
          'blend-with-mask
          `((input-background-image . ,background-image)
            (input-mask-image . ,mask-image)))))

    ;; Returns an image processor for image filter blend-with-red-mask (CIBlendWithRedMask).
    ;; Uses values from a mask image to interpolate between an image and the background. When
    ;; a mask red value is 0.0, the result is the background. When the mask red value is 1.0,
    ;; the result is the image.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;;   - mask-image (abstract-image/CIImage): A masking image.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (blend-with-red-mask background-image mask-image)
      (make-filter-proc
        (make-image-filter
          'blend-with-red-mask
          `((input-background-image . ,background-image)
            (input-mask-image . ,mask-image)))))

    ;; Returns an image processor for image filter bloom (CIBloom).
    ;; Softens edges and applies a pleasant glow to an image.
    ;;   - radius (distance/NSNumber): The radius determines how many pixels are used to
    ;;     create the effect. The larger the radius, the greater the effect.
    ;;   - intensity (scalar/NSNumber): The intensity of the effect. A value of 0.0 is no
    ;;     effect. A value of 1.0 is the maximum effect.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (bloom radius intensity)
      (make-filter-proc
        (make-image-filter
          'bloom
          `((input-radius . ,radius)
            (input-intensity . ,intensity)))))

    ;; Returns an image processor for image filter blurred-rectangle-generator
    ;; (CIBlurredRectangleGenerator).
    ;; Generates a blurred rectangle image with the specified extent, blur sigma, and color.
    ;;   - extent (rect/CIVector): A rectangle that defines the extent of the effect.
    ;;   - sigma (distance/NSNumber): The sigma for a gaussian blur.
    ;;   - color (color/CIColor): A color.
    ;; Filter categories: still-image, builtin, generator, high-dynamic-range
    (define (blurred-rectangle-generator extent sigma color)
      (image-filter-output
        (make-image-filter
          'blurred-rectangle-generator
          `((input-extent . ,extent)
            (input-sigma . ,sigma)
            (input-color . ,color)))))

    ;; Returns an image processor for image filter bokeh-blur (CIBokehBlur).
    ;; Smooths an image using a disc-shaped convolution kernel.
    ;;   - radius (distance/NSNumber): The radius determines how many pixels are used to
    ;;     create the blur. The larger the radius, the blurrier the result.
    ;;   - ring-amount (scalar/NSNumber): The amount of extra emphasis at the ring of the bokeh.
    ;;   - ring-size (scalar/NSNumber): The size of extra emphasis at the ring of the bokeh.
    ;;   - softness (scalar/NSNumber)
    ;; Filter categories: still-image, builtin, blur, high-dynamic-range, video
    (define (bokeh-blur radius ring-amount ring-size softness)
      (make-filter-proc
        (make-image-filter
          'bokeh-blur
          `((input-radius . ,radius)
            (input-ring-amount . ,ring-amount)
            (input-ring-size . ,ring-size)
            (input-softness . ,softness)))))

    ;; Returns an image processor for image filter box-blur (CIBoxBlur).
    ;; Smooths or sharpens an image using a box-shaped convolution kernel.
    ;;   - radius (distance/NSNumber): The radius determines how many pixels are used to
    ;;     create the blur. The larger the radius, the blurrier the result.
    ;; Filter categories: still-image, builtin, blur, high-dynamic-range, video
    (define (box-blur radius)
      (make-filter-proc
        (make-image-filter
          'box-blur
          `((input-radius . ,radius)))))

    ;; Returns an image processor for image filter bump-distortion (CIBumpDistortion).
    ;; Creates a concave or convex bump that originates at a specified point in the image.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - radius (distance/NSNumber): The radius determines how many pixels are used to
    ;;     create the distortion. The larger the radius, the wider the extent of the distortion.
    ;;   - scale (scalar/NSNumber): The scale of the effect determines the curvature of
    ;;     the bump. A value of 0.0 has no effect. Positive values create an outward bump;
    ;;     negative values create an inward bump.
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (bump-distortion center radius scale)
      (make-filter-proc
        (make-image-filter
          'bump-distortion
          `((input-center . ,center)
            (input-radius . ,radius)
            (input-scale . ,scale)))))

    ;; Returns an image processor for image filter bump-distortion-linear (CIBumpDistortionLinear).
    ;; Creates a bump that originates from a linear portion of the image.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - radius (distance/NSNumber): The radius determines how many pixels are used to
    ;;     create the distortion. The larger the radius, the wider the extent of the distortion.
    ;;   - angle (angle/NSNumber): The angle in radians of the line around which the
    ;;     distortion occurs.
    ;;   - scale (scalar/NSNumber): The scale of the effect.
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (bump-distortion-linear center radius angle scale)
      (make-filter-proc
        (make-image-filter
          'bump-distortion-linear
          `((input-center . ,center)
            (input-radius . ,radius)
            (input-angle . ,angle)
            (input-scale . ,scale)))))

    ;; Returns an image processor for image filter canny-edge-detector (CICannyEdgeDetector).
    ;; Applies the Canny Edge Detection algorithm to an image.
    ;;   - gaussian-sigma (scalar/NSNumber): The gaussian sigma of blur to apply to the image
    ;;     to reduce high-frequency noise.
    ;;   - perceptual (boolean/NSNumber): Specifies whether the edge thresholds should be
    ;;     computed in a perceptual color space.
    ;;   - threshold-high (scalar/NSNumber): The threshold that determines if gradient
    ;;     magnitude is a strong edge.
    ;;   - threshold-low (scalar/NSNumber): The threshold that determines if gradient
    ;;     magnitude is a weak edge.
    ;;   - hysteresis-passes (integer/NSNumber): The number of hysteresis passes to apply
    ;;     to promote weak edge pixels.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (canny-edge-detector gaussian-sigma
                                 perceptual
                                 threshold-high
                                 threshold-low
                                 hysteresis-passes)
      (make-filter-proc
        (make-image-filter
          'canny-edge-detector
          `((input-gaussian-sigma . ,gaussian-sigma)
            (input-perceptual . ,perceptual)
            (input-threshold-high . ,threshold-high)
            (input-threshold-low . ,threshold-low)
            (input-hysteresis-passes . ,hysteresis-passes)))))

    ;; Returns an image processor for image filter checkerboard-generator (CICheckerboardGenerator).
    ;; Generates a pattern of squares of alternating colors. You can specify the size,
    ;; colors, and the sharpness of the pattern.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - color0 (color/CIColor): A color to use for the first set of squares.
    ;;   - color1 (color/CIColor): A color to use for the second set of squares.
    ;;   - width (distance/NSNumber): The width of the squares in the pattern.
    ;;   - sharpness (scalar/NSNumber): The sharpness of the edges in pattern. The smaller
    ;;     the value, the more blurry the pattern. Values range from 0.0 to 1.0.
    ;; Filter categories: still-image, builtin, generator, high-dynamic-range, video
    (define (checkerboard-generator center color0 color1 width sharpness)
      (image-filter-output
        (make-image-filter
          'checkerboard-generator
          `((input-center . ,center)
            (input-color0 . ,color0)
            (input-color1 . ,color1)
            (input-width . ,width)
            (input-sharpness . ,sharpness)))))

    ;; Returns an image processor for image filter circle-splash-distortion
    ;; (CICircleSplashDistortion).
    ;; Distorts the pixels starting at the circumference of a circle and emanating outward.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - radius (distance/NSNumber): The radius determines how many pixels are used to
    ;;     create the distortion. The larger the radius, the wider the extent of the distortion.
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (circle-splash-distortion center radius)
      (make-filter-proc
        (make-image-filter
          'circle-splash-distortion
          `((input-center . ,center)
            (input-radius . ,radius)))))

    ;; Returns an image processor for image filter circular-screen (CICircularScreen).
    ;; Simulates a circular-shaped halftone screen.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - width (distance/NSNumber): The distance between each circle in the pattern.
    ;;   - sharpness (scalar/NSNumber): The sharpness of the circles. The larger the value,
    ;;     the sharper the circles.
    ;; Filter categories: still-image, builtin, halftone-effect, video
    (define (circular-screen center width sharpness)
      (make-filter-proc
        (make-image-filter
          'circular-screen
          `((input-center . ,center)
            (input-width . ,width)
            (input-sharpness . ,sharpness)))))

    ;; Returns an image processor for image filter circular-wrap (CICircularWrap).
    ;; Wraps an image around a transparent circle. The distortion of the image increases
    ;; with the distance from the center of the circle.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - radius (distance/NSNumber): The radius determines how many pixels are used to
    ;;     create the distortion. The larger the radius, the wider the extent of the distortion.
    ;;   - angle (angle/NSNumber): The angle in radians of the effect.
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (circular-wrap center radius angle)
      (make-filter-proc
        (make-image-filter
          'circular-wrap
          `((input-center . ,center)
            (input-radius . ,radius)
            (input-angle . ,angle)))))

    ;; Returns an image processor for image filter clamp (CIClamp).
    ;; Clamps an image so the pixels with the specified extent are left unchanged but those
    ;; at the boundary of the extent are extended outwards. This filter produces an image
    ;; with infinite extent. You can use this filter when you need to blur an image but you
    ;; want to avoid a soft, black fringe along the edges.
    ;;   - extent (rect/CIVector): A rectangle that defines the extent of the effect.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (clamp extent)
      (make-filter-proc
        (make-image-filter
          'clamp
          `((input-extent . ,extent)))))

    ;; Returns an image processor for image filter cmyk-halftone (CICMYKHalftone).
    ;; Creates a color, halftoned rendition of the source image, using cyan, magenta, yellow,
    ;; and black inks over a white page.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - width (distance/NSNumber): The distance between dots in the pattern.
    ;;   - angle (angle/NSNumber): The angle in radians of the pattern.
    ;;   - sharpness (distance/NSNumber): The sharpness of the pattern. The larger the value,
    ;;     the sharper the pattern.
    ;;   - g-c-r (scalar/NSNumber): The gray component replacement value. The value can vary
    ;;     from 0.0 (none) to 1.0.
    ;;   - u-c-r (scalar/NSNumber): The under color removal value. The value can vary from
    ;;     0.0 to 1.0. 
    ;; Filter categories: still-image, builtin, halftone-effect, video
    (define (cmyk-halftone center width angle sharpness g-c-r u-c-r)
      (make-filter-proc
        (make-image-filter
          'cmyk-halftone
          `((input-center . ,center)
            (input-width . ,width)
            (input-angle . ,angle)
            (input-sharpness . ,sharpness)
            (input-g-c-r . ,g-c-r)
            (input-u-c-r . ,u-c-r)))))

    ;; Returns an image processor for image filter code128-barcode-generator
    ;; (CICode128BarcodeGenerator).
    ;; Generate a Code 128 barcode image for message data.
    ;;   - message (bytevector/NSData): The message to encode in the Code 128 Barcode
    ;;   - quiet-space (integer/NSNumber): The number of empty white pixels that should
    ;;     surround the barcode.
    ;;   - barcode-height (integer/NSNumber): The height of the generated barcode in pixels.
    ;; Filter categories: still-image, builtin, generator
    (define (code128-barcode-generator message quiet-space barcode-height)
      (image-filter-output
        (make-image-filter
          'code128-barcode-generator
          `((input-message . ,message)
            (input-quiet-space . ,quiet-space)
            (input-barcode-height . ,barcode-height)))))

    ;; Returns an image processor for image filter color-absolute-difference
    ;; (CIColorAbsoluteDifference).
    ;; Produces an image that is the absolute value of the color difference between two
    ;; images. The alpha channel of the result will be the product of the two image alpha channels.
    ;;   - image2 (abstract-image/CIImage): The second input image for differencing.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    high-dynamic-range, color-adjustment, video
    (define (color-absolute-difference image2)
      (make-filter-proc
        (make-image-filter
          'color-absolute-difference
          `((input-image2 . ,image2)))))

    ;; Returns an image processor for image filter color-blend-mode (CIColorBlendMode).
    ;; Uses the luminance values of the background with the hue and saturation values of
    ;; the source image. This mode preserves the gray levels in the image.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (color-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'color-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter color-burn-blend-mode (CIColorBurnBlendMode).
    ;; Darkens the background image samples to reflect the source image samples. Source image
    ;; sample values that specify white do not produce a change.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (color-burn-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'color-burn-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter color-clamp (CIColorClamp).
    ;; Clamp color to a certain range.
    ;;   - min-components (image-coefficients/CIVector): Lower clamping values.
    ;;   - max-components (image-coefficients/CIVector): Higher clamping values.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    color-adjustment, video
    (define (color-clamp min-components max-components)
      (make-filter-proc
        (make-image-filter
          'color-clamp
          `((input-min-components . ,min-components)
            (input-max-components . ,max-components)))))

    ;; Returns an image processor for image filter color-controls (CIColorControls).
    ;; Adjusts saturation, brightness, and contrast values.
    ;;   - saturation (scalar/NSNumber): The amount of saturation to apply. The larger the
    ;;     value, the more saturated the result.
    ;;   - brightness (scalar/NSNumber): The amount of brightness to apply. The larger the
    ;;     value, the brighter the result.
    ;;   - contrast (scalar/NSNumber): The amount of contrast to apply. The larger the value,
    ;;     the more contrast in the resulting image.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    high-dynamic-range, color-adjustment, video
    (define (color-controls saturation brightness contrast)
      (make-filter-proc
        (make-image-filter
          'color-controls
          `((input-saturation . ,saturation)
            (input-brightness . ,brightness)
            (input-contrast . ,contrast)))))

    ;; Returns an image processor for image filter color-cross-polynomial (CIColorCrossPolynomial).
    ;; Adjusts the color of an image with polynomials.
    ;;   - red-coefficients (image-coefficients/CIVector): Polynomial coefficients for
    ;;     red channel.
    ;;   - green-coefficients (image-coefficients/CIVector): Polynomial coefficients for
    ;;     green channel.
    ;;   - blue-coefficients (image-coefficients/CIVector): Polynomial coefficients for
    ;;     blue channel.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (color-cross-polynomial red-coefficients green-coefficients blue-coefficients)
      (make-filter-proc
        (make-image-filter
          'color-cross-polynomial
          `((input-red-coefficients . ,red-coefficients)
            (input-green-coefficients . ,green-coefficients)
            (input-blue-coefficients . ,blue-coefficients)))))

    ;; Returns an image processor for image filter color-cube (CIColorCube).
    ;; Uses a three-dimensional color table to transform the source image pixels.
    ;;   - cube-dimension (count/NSNumber): The dimension of the color cube.
    ;;   - cube-data (bytevector/NSData): Data containing a 3-dimensional color table of
    ;;     floating-point premultiplied RGBA values. The cells are organized in a standard
    ;;     ordering. The columns and rows of the data are indexed by red and green,
    ;;     respectively. Each data plane is followed by the next higher plane in the data,
    ;;     with planes indexed by blue.
    ;;   - extrapolate (boolean/NSNumber): If true, then the color cube will be extrapolated
    ;;     if the input image contains RGB component values outside the range 0.0 to 1.0.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (color-cube cube-dimension cube-data extrapolate)
      (make-filter-proc
        (make-image-filter
          'color-cube
          `((input-cube-dimension . ,cube-dimension)
            (input-cube-data . ,cube-data)
            (input-extrapolate . ,extrapolate)))))

    ;; Returns an image processor for image filter color-cubes-mixed-with-mask
    ;; (CIColorCubesMixedWithMask).
    ;; Uses two three-dimensional color tables in a specified colorspace to transform the
    ;; source image pixels. The mask image is used as an interpolant to mix the output of
    ;; the two cubes.
    ;;   - mask-image (abstract-image/CIImage): A masking image.
    ;;   - cube-dimension (count/NSNumber): The dimension of the color cubes.
    ;;   - cube0-data (bytevector/NSData): Data containing a 3-dimensional color table of
    ;;     floating-point premultiplied RGBA values. The cells are organized in a standard
    ;;     ordering. The columns and rows of the data are indexed by red and green, respectively.
    ;;     Each data plane is followed by the next higher plane in the data, with planes
    ;;     indexed by blue.
    ;;   - cube1-data (bytevector/NSData): Data containing a 3-dimensional color table of
    ;;     floating-point premultiplied RGBA values. The cells are organized in a standard
    ;;     ordering. The columns and rows of the data are indexed by red and green, respectively.
    ;;     Each data plane is followed by the next higher plane in the data, with planes
    ;;     indexed by blue.
    ;;   - color-space (color-space/NSObject): The CGColorSpace that defines the RGB values in
    ;;     the color table.
    ;;   - extrapolate (boolean/NSNumber): If true, then the color cube will be extrapolated
    ;;     if the input image contains RGB component values outside the range 0 to 1.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (color-cubes-mixed-with-mask mask-image
                                         cube-dimension
                                         cube0-data
                                         cube1-data
                                         color-space
                                         extrapolate)
      (make-filter-proc
        (make-image-filter
          'color-cubes-mixed-with-mask
          `((input-mask-image . ,mask-image)
            (input-cube-dimension . ,cube-dimension)
            (input-cube0-data . ,cube0-data)
            (input-cube1-data . ,cube1-data)
            (input-color-space . ,color-space)
            (input-extrapolate . ,extrapolate)))))

    ;; Returns an image processor for image filter color-cube-with-color-space
    ;; (CIColorCubeWithColorSpace).
    ;; Uses a three-dimensional color table in a specified colorspace to transform the source
    ;; image pixels.
    ;;   - cube-dimension (count/NSNumber): The dimension of the color cube.
    ;;   - cube-data (bytevector/NSData): Data containing a 3-dimensional color table of
    ;;     floating-point premultiplied RGBA values. The cells are organized in a standard
    ;;     ordering. The columns and rows of the data are indexed by red and green,
    ;;     respectively. Each data plane is followed by the next higher plane in the data,
    ;;     with planes indexed by blue.
    ;;   - extrapolate (number/NSNumber): If true, then the color cube will be extrapolated
    ;;     if the input image contains RGB component values outside the range 0.0 to 1.0.
    ;;   - color-space (color-space/NSObject): The CGColorSpace that defines the RGB values
    ;;     in the color table.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (color-cube-with-color-space cube-dimension cube-data extrapolate color-space)
      (make-filter-proc
        (make-image-filter
          'color-cube-with-color-space
          `((input-cube-dimension . ,cube-dimension)
            (input-cube-data . ,cube-data)
            (input-extrapolate . ,extrapolate)
            (input-color-space . ,color-space)))))

    ;; Returns an image processor for image filter color-curves (CIColorCurves).
    ;; Uses a three-channel one-dimensional color table to transform the source image pixels.
    ;;   - curves-data (bytevector/NSData): Data containing a color table of floating-point
    ;;     RGB values.
    ;;   - curves-domain (image-coefficients/CIVector): A two-element vector that defines
    ;;     the minimum and maximum RGB component values that are used to look up result values
    ;;     from the color table.
    ;;   - color-space (color-space/NSObject): The CGColorSpace that defines the RGB values in
    ;;     the color table.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    color-effect, video
    (define (color-curves curves-data curves-domain color-space)
      (make-filter-proc
        (make-image-filter
          'color-curves
          `((input-curves-data . ,curves-data)
            (input-curves-domain . ,curves-domain)
            (input-color-space . ,color-space)))))

    ;; Returns an image processor for image filter color-dodge-blend-mode (CIColorDodgeBlendMode).
    ;; Brightens the background image samples to reflect the source image samples. Source image
    ;; sample values that specify black do not produce a change.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (color-dodge-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'color-dodge-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter color-invert (CIColorInvert).
    ;; Inverts the colors in an image.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (color-invert)
      (make-filter-proc
        (make-image-filter
          'color-invert)))

    ;; Returns an image processor for image filter color-map (CIColorMap).
    ;; Performs a nonlinear transformation of source color values using mapping values
    ;; provided in a table.
    ;;   - gradient-image (gradient/CIImage): The image data from this image transforms
    ;;     the source image values.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    color-effect, video
    (define (color-map gradient-image)
      (make-filter-proc
        (make-image-filter
          'color-map
          `((input-gradient-image . ,gradient-image)))))

    ;; Returns an image processor for image filter color-matrix (CIColorMatrix).
    ;; Multiplies source color values and adds a bias factor to each color component.
    ;;   - r-vector (image-coefficients/CIVector): The amount of red to multiply the source
    ;;     color values by.
    ;;   - g-vector (image-coefficients/CIVector): The amount of green to multiply the source
    ;;     color values by.
    ;;   - b-vector (image-coefficients/CIVector): The amount of blue to multiply the source
    ;;     color values by.
    ;;   - a-vector (image-coefficients/CIVector): The amount of alpha to multiply the source
    ;;     color values by.
    ;;   - bias-vector (image-coefficients/CIVector): A vector that’s added to each color
    ;;     component.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    high-dynamic-range, color-adjustment, video
    (define (color-matrix r-vector g-vector b-vector a-vector bias-vector)
      (make-filter-proc
        (make-image-filter
          'color-matrix
          `((input-r-vector . ,r-vector)
            (input-g-vector . ,g-vector)
            (input-b-vector . ,b-vector)
            (input-a-vector . ,a-vector)
            (input-bias-vector . ,bias-vector)))))

    ;; Returns an image processor for image filter color-monochrome (CIColorMonochrome).
    ;; Remaps colors so they fall within shades of a single color.
    ;;   - color (opaque-color/CIColor): The monochrome color to apply to the image.
    ;;   - intensity (scalar/NSNumber): The intensity of the monochrome effect. A value of 1.0
    ;;     creates a monochrome image using the supplied color. A value of 0.0 has no effect
    ;;     on the image.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (color-monochrome color intensity)
      (make-filter-proc
        (make-image-filter
          'color-monochrome
          `((input-color . ,color)
            (input-intensity . ,intensity)))))

    ;; Returns an image processor for image filter color-polynomial (CIColorPolynomial).
    ;; Adjusts the color of an image with polynomials.
    ;;   - red-coefficients (image-coefficients/CIVector): Polynomial coefficients for
    ;;     red channel.
    ;;   - green-coefficients (image-coefficients/CIVector): Polynomial coefficients for
    ;;     green channel.
    ;;   - blue-coefficients (image-coefficients/CIVector): Polynomial coefficients for
    ;;     blue channel.
    ;;   - alpha-coefficients (image-coefficients/CIVector): Polynomial coefficients for
    ;;     alpha channel.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    high-dynamic-range, color-adjustment, video
    (define (color-polynomial red-coefficients
                              green-coefficients
                              blue-coefficients
                              alpha-coefficients)
      (make-filter-proc
        (make-image-filter
          'color-polynomial
          `((input-red-coefficients . ,red-coefficients)
            (input-green-coefficients . ,green-coefficients)
            (input-blue-coefficients . ,blue-coefficients)
            (input-alpha-coefficients . ,alpha-coefficients)))))

    ;; Returns an image processor for image filter color-posterize (CIColorPosterize).
    ;; Remaps red, green, and blue color components to the number of brightness values you
    ;; specify for each color component. This filter flattens colors to achieve a look similar
    ;; to that of a silk-screened poster.
    ;;   - levels (scalar/NSNumber): The number of brightness levels to use for each color
    ;;     component. Lower values result in a more extreme poster effect.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (color-posterize levels)
      (make-filter-proc
        (make-image-filter
          'color-posterize
          `((input-levels . ,levels)))))

    ;; Returns an image processor for image filter color-threshold (CIColorThreshold).
    ;; Produces a binarized image from an image and a threshold value. The red, green and
    ;; blue channels of the resulting image will be one if its value is greater than the
    ;; threshold and zero otherwise.
    ;;   - threshold (scalar/NSNumber): The threshold value that governs if the RGB channels
    ;;     of the resulting image will be zero or one.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    color-adjustment, video
    (define (color-threshold threshold)
      (make-filter-proc
        (make-image-filter
          'color-threshold
          `((input-threshold . ,threshold)))))

    ;; Returns an image processor for image filter color-threshold-otsu (CIColorThresholdOtsu).
    ;; Produces a binarized image from an image with finite extent. The threshold is calculated
    ;; from the image histogram using Otsu’s method. The red, green and blue channels of the
    ;; resulting image will be one if its value is greater than the threshold and zero otherwise.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    color-adjustment, video
    (define (color-threshold-otsu)
      (make-filter-proc
        (make-image-filter
          'color-threshold-otsu)))

    ;; Returns an image processor for image filter column-average (CIColumnAverage).
    ;; Calculates the average color for each column of the specified area in an image,
    ;; returning the result in a 1D image.
    ;;   - extent (rect/CIVector): A rectangle that specifies the subregion of the image
    ;;     that you want to process.
    ;; Filter categories: still-image, builtin, reduction, high-dynamic-range, video
    (define (column-average extent)
      (make-filter-proc
        (make-image-filter
          'column-average
          `((input-extent . ,extent)))))

    ;; Returns an image processor for image filter comic-effect (CIComicEffect).
    ;; Simulates a comic book drawing by outlining edges and applying a color
    ;; halftone effect.
    ;; Filter categories: still-image, builtin, stylize, video
    (define (comic-effect)
      (make-filter-proc
        (make-image-filter
          'comic-effect)))

    ;; Returns an image processor for image filter constant-color-generator
    ;; (CIConstantColorGenerator).
    ;; Generates a solid color. You typically use the output of this filter as the input
    ;; to another filter.
    ;;   - color (color/CIColor): The color to generate.
    ;; Filter categories: still-image, builtin, generator, high-dynamic-range, video
    (define (constant-color-generator color)
      (image-filter-output
        (make-image-filter
          'constant-color-generator
          `((input-color . ,color)))))

    ;; Returns an image processor for image filter convert-lab-to-rgb (CIConvertLabToRGB).
    ;; Converts an image from La*b* color space to the Core Image RGB working space.
    ;;   - normalize (boolean/NSNumber): If normalize is false then the L channel is in
    ;;     the range 0 to 100 and the a*b* channels are in the range -128 to 128. If
    ;;     normalize is true then the La*b* channels are in the range 0 to 1.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (convert-lab-to-rgb normalize)
      (make-filter-proc
        (make-image-filter
          'convert-lab-to-rgb
          `((input-normalize . ,normalize)))))

    ;; Returns an image processor for image filter convert-rgb-to-lab (CIConvertRGBtoLab).
    ;; Converts an image from the Core Image RGB working space to La*b* color space.
    ;;   - normalize (boolean/NSNumber): If normalize is false then the L channel is in the
    ;;     range 0 to 100 and the a*b* channels are in the range -128 to 128. If normalize
    ;;     is true then the La*b* channels are in the range 0 to 1.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (convert-rgb-to-lab normalize)
      (make-filter-proc
        (make-image-filter
          'convert-rgb-to-lab
          `((input-normalize . ,normalize)))))

    ;; Returns an image processor for image filter convolution-3x3 (CIConvolution3X3).
    ;; Convolution with 3 by 3 matrix.
    ;;   - weights (image-coefficients/CIVector): A vector containing the 9 weights of
    ;;     the convolution kernel.
    ;;   - bias (scalar/NSNumber): A value that is added to the RGBA components of the
    ;;     output pixel.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (convolution-3x3 weights bias)
      (make-filter-proc
        (make-image-filter
          'convolution-3x3
          `((input-weights . ,weights)
            (input-bias . ,bias)))))

    ;; Returns an image processor for image filter convolution-5x5 (CIConvolution5X5).
    ;; Convolution with 5 by 5 matrix.
    ;;   - weights (image-coefficients/CIVector): A vector containing the 25 weights of
    ;;     the convolution kernel.
    ;;   - bias (scalar/NSNumber): A value that is added to the RGBA components of the
    ;;     output pixel.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (convolution-5x5 weights bias)
      (make-filter-proc
        (make-image-filter
          'convolution-5x5
          `((input-weights . ,weights)
            (input-bias . ,bias)))))

    ;; Returns an image processor for image filter convolution-7x7 (CIConvolution7X7).
    ;; Convolution with 7 by 7 matrix.
    ;;   - weights (image-coefficients/CIVector): A vector containing the 49 weights of
    ;;     the convolution kernel.
    ;;   - bias (scalar/NSNumber): A value that is added to the RGBA components of the
    ;;     output pixel.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (convolution-7x7 weights bias)
      (make-filter-proc
        (make-image-filter
          'convolution-7x7
          `((input-weights . ,weights)
            (input-bias . ,bias)))))

    ;; Returns an image processor for image filter convolution9-horizontal
    ;; (CIConvolution9Horizontal).
    ;; Horizontal Convolution with 9 values.
    ;;   - weights (image-coefficients/CIVector): A vector containing the 9 weights of the
    ;;     convolution kernel.
    ;;   - bias (scalar/NSNumber): A value that is added to the RGBA components of the
    ;;     output pixel.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (convolution9-horizontal weights bias)
      (make-filter-proc
        (make-image-filter
          'convolution9-horizontal
          `((input-weights . ,weights)
            (input-bias . ,bias)))))

    ;; Returns an image processor for image filter convolution9-vertical (CIConvolution9Vertical).
    ;; Vertical Convolution with 9 values.
    ;;   - weights (image-coefficients/CIVector): A vector containing the 9 weights of the
    ;;     convolution kernel.
    ;;   - bias (scalar/NSNumber): A value that is added to the RGBA components of the
    ;;     output pixel.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (convolution9-vertical weights bias)
      (make-filter-proc
        (make-image-filter
          'convolution9-vertical
          `((input-weights . ,weights)
            (input-bias . ,bias)))))

    ;; Returns an image processor for image filter convolution-rgb-3x3 (CIConvolutionRGB3X3).
    ;; Convolution of RGB channels with 3 by 3 matrix.
    ;;   - weights (image-coefficients/CIVector): A vector containing the 9 weights of the
    ;;     convolution kernel.
    ;;   - bias (scalar/NSNumber): A value that is added to the RGB components of the
    ;;     output pixel.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (convolution-rgb-3x3 weights bias)
      (make-filter-proc
        (make-image-filter
          'convolution-rgb-3x3
          `((input-weights . ,weights)
            (input-bias . ,bias)))))

    ;; Returns an image processor for image filter convolution-rgb-5x5 (CIConvolutionRGB5X5).
    ;; Convolution of RGB channels with 5 by 5 matrix.
    ;;   - weights (image-coefficients/CIVector): A vector containing the 25 weights of the
    ;;     convolution kernel.
    ;;   - bias (scalar/NSNumber): A value that is added to the RGB components of the
    ;;     output pixel.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (convolution-rgb-5x5 weights bias)
      (make-filter-proc
        (make-image-filter
          'convolution-rgb-5x5
          `((input-weights . ,weights)
            (input-bias . ,bias)))))

    ;; Returns an image processor for image filter convolution-rgb-7x7 (CIConvolutionRGB7X7).
    ;; Convolution of RGB channels with 7 by 7 matrix.
    ;;   - weights (image-coefficients/CIVector): A vector containing the 49 weights of
    ;;     the convolution kernel.
    ;;   - bias (scalar/NSNumber): A value that is added to the RGB components of the
    ;;     output pixel.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (convolution-rgb-7x7 weights bias)
      (make-filter-proc
        (make-image-filter
          'convolution-rgb-7x7
          `((input-weights . ,weights)
            (input-bias . ,bias)))))

    ;; Returns an image processor for image filter convolution-rgb9-horizontal
    ;; (CIConvolutionRGB9Horizontal).
    ;; Horizontal Convolution of RGB channels with 9 values.
    ;;   - weights (image-coefficients/CIVector): A vector containing the 9 weights of
    ;;     the convolution kernel.
    ;;   - bias (scalar/NSNumber): A value that is added to the RGB components of the
    ;;     output pixel.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (convolution-rgb9-horizontal weights bias)
      (make-filter-proc
        (make-image-filter
          'convolution-rgb9-horizontal
          `((input-weights . ,weights)
            (input-bias . ,bias)))))

    ;; Returns an image processor for image filter convolution-rgb9-vertical
    ;; (CIConvolutionRGB9Vertical).
    ;; Vertical Convolution of RGB channels with 9 values.
    ;;   - weights (image-coefficients/CIVector): A vector containing the 9 weights of the
    ;;     convolution kernel.
    ;;   - bias (scalar/NSNumber): A value that is added to the RGB components of the
    ;;     output pixel.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (convolution-rgb9-vertical weights bias)
      (make-filter-proc
        (make-image-filter
          'convolution-rgb9-vertical
          `((input-weights . ,weights)
            (input-bias . ,bias)))))

    ;; Returns an image processor for image filter copy-machine-transition
    ;; (CICopyMachineTransition).
    ;; Transitions from one image to another by simulating the effect of a copy machine.
    ;;   - target-image (abstract-image/CIImage): The target image for a transition.
    ;;   - extent (rect/CIVector): A rectangle that defines the extent of the effect.
    ;;   - color (opaque-color/CIColor): The color of the copier light.
    ;;   - time (time/NSNumber): The parametric time of the transition. This value drives the
    ;;     transition from start (at time 0) to end (at time 1).
    ;;   - angle (angle/NSNumber): The angle in radians of the copier light.
    ;;   - width (distance/NSNumber): The width of the copier light. 
    ;;   - opacity (scalar/NSNumber): The opacity of the copier light. A value of 0.0 is
    ;;     transparent. A value of 1.0 is opaque.
    ;; Filter categories: still-image, builtin, transition, high-dynamic-range, video
    (define (copy-machine-transition target-image extent color time angle width opacity)
      (make-filter-proc
        (make-image-filter
          'copy-machine-transition
          `((input-target-image . ,target-image)
            (input-extent . ,extent)
            (input-color . ,color)
            (input-time . ,time)
            (input-angle . ,angle)
            (input-width . ,width)
            (input-opacity . ,opacity)))))

    ;; Returns an image processor for image filter crop (CICrop).
    ;; Applies a crop to an image. The size and shape of the cropped image depend on
    ;; the rectangle you specify.
    ;;   - rectangle (rect/CIVector): The rectangle that specifies the crop to apply
    ;;     to the image.
    ;; Filter categories: still-image, builtin, geometry-adjustment,
    ;;                    high-dynamic-range, video
    (define (crop rectangle)
      (make-filter-proc
        (make-image-filter
          'crop
          `((input-rectangle . ,rectangle)))))

    ;; Returns an image processor for image filter crystallize (CICrystallize).
    ;; Creates polygon-shaped color blocks by aggregating source pixel-color values.
    ;;   - radius (distance/NSNumber): The radius determines how many pixels are used to
    ;;     create the effect. The larger the radius, the larger the resulting crystals.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (crystallize radius center)
      (make-filter-proc
        (make-image-filter
          'crystallize
          `((input-radius . ,radius)
            (input-center . ,center)))))

    ;; Returns an image processor for image filter darken-blend-mode (CIDarkenBlendMode).
    ;; Creates composite image samples by choosing the darker samples (from either the source
    ;; image or the background). The result is that the background image samples are replaced
    ;; by any source image samples that are darker. Otherwise, the background image samples
    ;; are left unchanged.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (darken-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'darken-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter depth-of-field (CIDepthOfField).
    ;; Simulates miniaturization effect created by Tilt & Shift lens by performing depth
    ;; of field effects.
    ;;   - point0 (point/CIVector)
    ;;   - point1 (point/CIVector)
    ;;   - saturation (scalar/NSNumber): The amount to adjust the saturation.
    ;;   - unsharp-mask-radius (scalar/NSNumber)
    ;;   - unsharp-mask-intensity (scalar/NSNumber)
    ;;   - radius (scalar/NSNumber): The distance from the center of the effect.
    ;; Filter categories: still-image, builtin, stylize, video
    (define (depth-of-field point0
                            point1
                            saturation
                            unsharp-mask-radius
                            unsharp-mask-intensity
                            radius)
      (make-filter-proc
        (make-image-filter
          'depth-of-field
          `((input-point0 . ,point0)
            (input-point1 . ,point1)
            (input-saturation . ,saturation)
            (input-unsharp-mask-radius . ,unsharp-mask-radius)
            (input-unsharp-mask-intensity . ,unsharp-mask-intensity)
            (input-radius . ,radius)))))

    ;; Returns an image processor for image filter depth-to-disparity (CIDepthToDisparity).
    ;; Convert a depth data image to disparity data.
    ;; Filter categories: still-image, builtin, color-adjustment, video
    (define (depth-to-disparity)
      (make-filter-proc
        (make-image-filter
          'depth-to-disparity)))

    ;; Returns an image processor for image filter difference-blend-mode (CIDifferenceBlendMode).
    ;; Subtracts either the source image sample color from the background image sample
    ;; color, or the reverse, depending on which sample has the greater brightness value.
    ;; Source image sample values that are black produce no change; white inverts the
    ;; background color values.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (difference-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'difference-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter disc-blur (CIDiscBlur).
    ;; Smooths an image using a disc-shaped convolution kernel.
    ;;   - radius (distance/NSNumber): The radius determines how many pixels are used to
    ;;     create the blur. The larger the radius, the blurrier the result.
    ;; Filter categories: still-image, builtin, blur, high-dynamic-range, video
    (define (disc-blur radius)
      (make-filter-proc
        (make-image-filter
          'disc-blur
          `((input-radius . ,radius)))))

    ;; Returns an image processor for image filter disintegrate-with-mask-transition
    ;; (CIDisintegrateWithMaskTransition).
    ;; Transitions from one image to another using the shape defined by a mask.
    ;;   - target-image (abstract-image/CIImage): The target image for a transition.
    ;;   - mask-image (abstract-image/CIImage): An image that defines the shape to use
    ;;     when disintegrating from the source to the target image.
    ;;   - time (time/NSNumber): The parametric time of the transition. This value drives
    ;;     the transition from start (at time 0) to end (at time 1).
    ;;   - shadow-radius (distance/NSNumber): The radius of the shadow created by the mask.
    ;;   - shadow-density (scalar/NSNumber): The density of the shadow created by the mask.
    ;;   - shadow-offset (offset/CIVector): The offset of the shadow created by the mask.
    ;; Filter categories: still-image, builtin, transition, high-dynamic-range, video
    (define (disintegrate-with-mask-transition target-image
                                               mask-image
                                               time
                                               shadow-radius
                                               shadow-density
                                               shadow-offset)
      (make-filter-proc
        (make-image-filter
          'disintegrate-with-mask-transition
          `((input-target-image . ,target-image)
            (input-mask-image . ,mask-image)
            (input-time . ,time)
            (input-shadow-radius . ,shadow-radius)
            (input-shadow-density . ,shadow-density)
            (input-shadow-offset . ,shadow-offset)))))

    ;; Returns an image processor for image filter disparity-to-depth (CIDisparityToDepth).
    ;; Convert a disparity data image to depth data.
    ;; Filter categories: still-image, builtin, color-adjustment, video
    (define (disparity-to-depth)
      (make-filter-proc
        (make-image-filter
          'disparity-to-depth)))

    ;; Returns an image processor for image filter displacement-distortion
    ;; (CIDisplacementDistortion).
    ;; Applies the grayscale values of the second image to the first image. The output
    ;; image has a texture defined by the grayscale values.
    ;;   - displacement-image (abstract-image/CIImage): An image whose grayscale values will
    ;;     be applied to the source image.
    ;;   - scale (distance/NSNumber): The amount of texturing of the resulting image. The
    ;;     larger the value, the greater the texturing.
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (displacement-distortion displacement-image scale)
      (make-filter-proc
        (make-image-filter
          'displacement-distortion
          `((input-displacement-image . ,displacement-image)
            (input-scale . ,scale)))))

    ;; Returns an image processor for image filter dissolve-transition (CIDissolveTransition).
    ;; Uses a dissolve to transition from one image to another.
    ;;   - target-image (abstract-image/CIImage): The target image for a transition.
    ;;   - time (time/NSNumber): The parametric time of the transition. This value drives the
    ;;     transition from start (at time 0) to end (at time 1).
    ;; Filter categories: still-image, builtin, interlaced, transition, non-square-pixels,
    ;;                    high-dynamic-range, video
    (define (dissolve-transition target-image time)
      (make-filter-proc
        (make-image-filter
          'dissolve-transition
          `((input-target-image . ,target-image)
            (input-time . ,time)))))

    ;; Returns an image processor for image filter distance-gradient-from-red-mask
    ;; (CIDistanceGradientFromRedMask).
    ;; Produces an infinite image where the red channel contains the distance in pixels
    ;; from each pixel to the mask.
    ;;   - maximum-distance (distance/NSNumber): Determines the maximum distance to the mask
    ;;     that can be measured. Distances between zero and the maximum will be normalized
    ;;     to zero and one.
    ;; Filter categories: still-image, builtin, gradient, video
    (define (distance-gradient-from-red-mask maximum-distance)
      (make-filter-proc
        (make-image-filter
          'distance-gradient-from-red-mask
          `((input-maximum-distance . ,maximum-distance)))))

    ;; Returns an image processor for image filter dither (CIDither).
    ;; Apply dithering to an image. This operation is usually performed in a perceptual
    ;; color space.
    ;;   - intensity (scalar/NSNumber): The intensity of the effect.
    ;; Filter categories: still-image, builtin, color-effect, high-dynamic-range, video
    (define (dither intensity)
      (make-filter-proc
        (make-image-filter
          'dither
          `((input-intensity . ,intensity)))))

    ;; Returns an image processor for image filter divide-blend-mode (CIDivideBlendMode).
    ;; Divides the background image sample color from the source image sample color.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (divide-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'divide-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter document-enhancer (CIDocumentEnhancer).
    ;; Enhance a document image by removing unwanted shadows, whitening the background,
    ;; and enhancing contrast.
    ;;   - amount (scalar/NSNumber): The amount of enhancement.
    ;; Filter categories: still-image, builtin, non-square-pixels, color-effect
    (define (document-enhancer amount)
      (make-filter-proc
        (make-image-filter
          'document-enhancer
          `((input-amount . ,amount)))))

    ;; Returns an image processor for image filter dot-screen (CIDotScreen).
    ;; Simulates the dot patterns of a halftone screen.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - angle (angle/NSNumber): The angle in radians of the pattern.
    ;;   - width (distance/NSNumber): The distance between dots in the pattern.
    ;;   - sharpness (scalar/NSNumber): The sharpness of the pattern. The larger the value,
    ;;     the sharper the pattern.
    ;; Filter categories: still-image, builtin, halftone-effect, video
    (define (dot-screen center angle width sharpness)
      (make-filter-proc
        (make-image-filter
          'dot-screen
          `((input-center . ,center)
            (input-angle . ,angle)
            (input-width . ,width)
            (input-sharpness . ,sharpness)))))

    ;; Returns an image processor for image filter droste (CIDroste).
    ;; Performs M.C. Escher Droste style deformation.
    ;;   - inset-point0 (point/CIVector)
    ;;   - inset-point1 (point/CIVector)
    ;;   - strands (scalar/NSNumber)
    ;;   - periodicity (scalar/NSNumber)
    ;;   - rotation (angle/NSNumber)
    ;;   - zoom (scalar/NSNumber)
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (droste inset-point0 inset-point1 strands periodicity rotation zoom)
      (make-filter-proc
        (make-image-filter
          'droste
          `((input-inset-point0 . ,inset-point0)
            (input-inset-point1 . ,inset-point1)
            (input-strands . ,strands)
            (input-periodicity . ,periodicity)
            (input-rotation . ,rotation)
            (input-zoom . ,zoom)))))

    ;; Returns an image processor for image filter edge-preserve-upsample-filter
    ;; (CIEdgePreserveUpsampleFilter).
    ;; Upsamples a small image to the size of the input image using the luminance of the
    ;; input image as a guide to preserve detail.
    ;;   - small-image (abstract-image/CIImage)
    ;;   - spatial-sigma (scalar/NSNumber)
    ;;   - luma-sigma (scalar/NSNumber)
    ;; Filter categories: still-image, builtin, interlaced, geometry-adjustment,
    ;;                    non-square-pixels, high-dynamic-range, video
    (define (edge-preserve-upsample-filter small-image spatial-sigma luma-sigma)
      (make-filter-proc
        (make-image-filter
          'edge-preserve-upsample-filter
          `((input-small-image . ,small-image)
            (input-spatial-sigma . ,spatial-sigma)
            (input-luma-sigma . ,luma-sigma)))))

    ;; Returns an image processor for image filter edges (CIEdges).
    ;; Finds all edges in an image and displays them in color.
    ;;   - intensity (scalar/NSNumber): The intensity of the edges. The larger the value,
    ;;     the higher the intensity.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (edges intensity)
      (make-filter-proc
        (make-image-filter
          'edges
          `((input-intensity . ,intensity)))))

    ;; Returns an image processor for image filter edge-work (CIEdgeWork).
    ;; Produces a stylized black-and-white rendition of an image that looks similar to a
    ;; woodblock cutout.
    ;;   - radius (distance/NSNumber): The thickness of the edges. The larger the value,
    ;;     the thicker the edges.
    ;; Filter categories: still-image, builtin, stylize, video
    (define (edge-work radius)
      (make-filter-proc
        (make-image-filter
          'edge-work
          `((input-radius . ,radius)))))

    ;; Returns an image processor for image filter eightfold-reflected-tile
    ;; (CIEightfoldReflectedTile).
    ;; Produces a tiled image from a source image by applying an 8-way reflected symmetry.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - angle (angle/NSNumber): The angle in radians of the tiled pattern.
    ;;   - width (distance/NSNumber): The width of a tile.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (eightfold-reflected-tile center angle width)
      (make-filter-proc
        (make-image-filter
          'eightfold-reflected-tile
          `((input-center . ,center)
            (input-angle . ,angle)
            (input-width . ,width)))))

    ;; Returns an image processor for image filter exclusion-blend-mode (CIExclusionBlendMode).
    ;; Produces an effect similar to that produced by the “Difference Blend Mode” filter but
    ;; with lower contrast. Source image sample values that are black do not produce a change;
    ;; white inverts the background color values.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (exclusion-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'exclusion-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter exposure-adjust (CIExposureAdjust).
    ;; Adjusts the exposure setting for an image similar to the way you control exposure
    ;; for a camera when you change the F-stop.
    ;;   - e-v (scalar/NSNumber): The amount to adjust the exposure of the image by. The
    ;;     larger the value, the brighter the exposure.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    high-dynamic-range, color-adjustment, video
    (define (exposure-adjust e-v)
      (make-filter-proc
        (make-image-filter
          'exposure-adjust
          `((input-e-v . ,e-v)))))

    ;; Returns an image processor for image filter false-color (CIFalseColor).
    ;; Maps luminance to a color ramp of two colors. False color is often used to process
    ;; astronomical and other scientific data, such as ultraviolet and X-ray images.
    ;;   - color0 (color/CIColor): The first color to use for the color ramp.
    ;;   - color1 (color/CIColor): The second color to use for the color ramp.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (false-color color0 color1)
      (make-filter-proc
        (make-image-filter
          'false-color
          `((input-color0 . ,color0)
            (input-color1 . ,color1)))))

    ;; Returns an image processor for image filter flash-transition (CIFlashTransition).
    ;; Transitions from one image to another by creating a flash. The flash originates
    ;; from a point you specify. Small at first, it rapidly expands until the image frame
    ;; is completely filled with the flash color. As the color fades, the target image
    ;; begins to appear.
    ;;   - target-image (abstract-image/CIImage): The target image for a transition.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - extent (rect/CIVector): The extent of the flash.
    ;;   - color (color/CIColor): The color of the light rays emanating from the flash.
    ;;   - time (time/NSNumber): The parametric time of the transition. This value drives
    ;;     the transition from start (at time 0) to end (at time 1).
    ;;   - max-striation-radius (scalar/NSNumber): The radius of the light rays emanating
    ;;     from the flash.
    ;;   - striation-strength (scalar/NSNumber): The strength of the light rays emanating
    ;;     from the flash.
    ;;   - striation-contrast (scalar/NSNumber): The contrast of the light rays emanating
    ;;     from the flash.
    ;;   - fade-threshold (scalar/NSNumber): The amount of fade between the flash and the
    ;;     target image. The higher the value, the more flash time and the less fade time.
    ;; Filter categories: still-image, builtin, transition, high-dynamic-range, video
    (define (flash-transition target-image
                              center
                              extent
                              color
                              time
                              max-striation-radius
                              striation-strength
                              striation-contrast
                              fade-threshold)
      (make-filter-proc
        (make-image-filter
          'flash-transition
          `((input-target-image . ,target-image)
            (input-center . ,center)
            (input-extent . ,extent)
            (input-color . ,color)
            (input-time . ,time)
            (input-max-striation-radius . ,max-striation-radius)
            (input-striation-strength . ,striation-strength)
            (input-striation-contrast . ,striation-contrast)
            (input-fade-threshold . ,fade-threshold)))))

    ;; Returns an image processor for image filter fourfold-reflected-tile
    ;; (CIFourfoldReflectedTile).
    ;; Produces a tiled image from a source image by applying a 4-way reflected symmetry.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - angle (angle/NSNumber): The angle in radians of the tiled pattern.
    ;;   - width (distance/NSNumber): The width of a tile.
    ;;   - acute-angle (angle/NSNumber): The primary angle for the repeating reflected
    ;;     tile. Small values create thin diamond tiles, and higher values create fatter
    ;;     reflected tiles.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (fourfold-reflected-tile center angle width acute-angle)
      (make-filter-proc
        (make-image-filter
          'fourfold-reflected-tile
          `((input-center . ,center)
            (input-angle . ,angle)
            (input-width . ,width)
            (input-acute-angle . ,acute-angle)))))

    ;; Returns an image processor for image filter fourfold-rotated-tile (CIFourfoldRotatedTile).
    ;; Produces a tiled image from a source image by rotating the source at increments
    ;; of 90 degrees.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - angle (angle/NSNumber): The angle in radians of the tiled pattern.
    ;;   - width (distance/NSNumber): The width of a tile.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (fourfold-rotated-tile center angle width)
      (make-filter-proc
        (make-image-filter
          'fourfold-rotated-tile
          `((input-center . ,center)
            (input-angle . ,angle)
            (input-width . ,width)))))

    ;; Returns an image processor for image filter fourfold-translated-tile
    ;; (CIFourfoldTranslatedTile).
    ;; Produces a tiled image from a source image by applying 4 translation operations.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - angle (angle/NSNumber): The angle in radians of the tiled pattern.
    ;;   - width (distance/NSNumber): The width of a tile.
    ;;   - acute-angle (angle/NSNumber): The primary angle for the repeating translated
    ;;     tile. Small values create thin diamond tiles, and higher values create fatter
    ;;     translated tiles.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (fourfold-translated-tile center angle width acute-angle)
      (make-filter-proc
        (make-image-filter
          'fourfold-translated-tile
          `((input-center . ,center)
            (input-angle . ,angle)
            (input-width . ,width)
            (input-acute-angle . ,acute-angle)))))

    ;; Returns an image processor for image filter gabor-gradients (CIGaborGradients).
    ;; Applies multichannel 5 by 5 Gabor gradient filter to an image. The resulting image
    ;; has maximum horizontal gradient in the red channel and the maximum vertical gradient
    ;; in the green channel. The gradient values can be positive or negative.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (gabor-gradients)
      (make-filter-proc
        (make-image-filter
          'gabor-gradients)))

    ;; Returns an image processor for image filter gamma-adjust (CIGammaAdjust).
    ;; Adjusts midtone brightness. This filter is typically used to compensate for nonlinear
    ;; effects of displays. Adjusting the gamma effectively changes the slope of the
    ;; transition between black and white.
    ;;   - power (scalar/NSNumber): A gamma value to use to correct image brightness. The
    ;;     larger the value, the darker the result.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    high-dynamic-range, color-adjustment, video
    (define (gamma-adjust power)
      (make-filter-proc
        (make-image-filter
          'gamma-adjust
          `((input-power . ,power)))))

    ;; Returns an image processor for image filter gaussian-blur (CIGaussianBlur).
    ;; Spreads source pixels by an amount specified by a Gaussian distribution.
    ;;   - radius (scalar/NSNumber): The radius determines how many pixels are used to
    ;;     create the blur. The larger the radius, the blurrier the result.
    ;; Filter categories: still-image, builtin, blur, high-dynamic-range, video
    (define (gaussian-blur radius)
      (make-filter-proc
        (make-image-filter
          'gaussian-blur
          `((input-radius . ,radius)))))

    ;; Returns an image processor for image filter gaussian-gradient (CIGaussianGradient).
    ;; Generates a gradient that varies from one color to another using a Gaussian
    ;; distribution.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - color0 (color/CIColor): The first color to use in the gradient.
    ;;   - color1 (color/CIColor): The second color to use in the gradient.
    ;;   - radius (distance/NSNumber): The radius of the Gaussian distribution.
    ;; Filter categories: still-image, builtin, gradient, high-dynamic-range, video
    (define (gaussian-gradient center color0 color1 radius)
      (image-filter-output
        (make-image-filter
          'gaussian-gradient
          `((input-center . ,center)
            (input-color0 . ,color0)
            (input-color1 . ,color1)
            (input-radius . ,radius)))))

    ;; Returns an image processor for image filter glass-distortion (CIGlassDistortion).
    ;; Distorts an image by applying a glass-like texture. The raised portions of the
    ;; output image are the result of applying a texture map.
    ;;   - texture (abstract-image/CIImage): A texture to apply to the source image.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - scale (distance/NSNumber): The amount of texturing of the resulting image.
    ;;     The larger the value, the greater the texturing.
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (glass-distortion texture center scale)
      (make-filter-proc
        (make-image-filter
          'glass-distortion
          `((input-texture . ,texture)
            (input-center . ,center)
            (input-scale . ,scale)))))

    ;; Returns an image processor for image filter glass-lozenge (CIGlassLozenge).
    ;; Creates a lozenge-shaped lens and distorts the portion of the image over which
    ;; the lens is placed.
    ;;   - point0 (point/CIVector): The x and y position that defines the center of the
    ;;     circle at one end of the lozenge.
    ;;   - point1 (point/CIVector): The x and y position that defines the center of the
    ;;     circle at the other end of the lozenge.
    ;;   - radius (distance/NSNumber): The radius of the lozenge. The larger the radius,
    ;;     the wider the extent of the distortion.
    ;;   - refraction (scalar/NSNumber): The refraction of the glass.
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (glass-lozenge point0 point1 radius refraction)
      (make-filter-proc
        (make-image-filter
          'glass-lozenge
          `((input-point0 . ,point0)
            (input-point1 . ,point1)
            (input-radius . ,radius)
            (input-refraction . ,refraction)))))

    ;; Returns an image processor for image filter glide-reflected-tile (CIGlideReflectedTile).
    ;; Produces a tiled image from a source image by translating and smearing the image.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - angle (angle/NSNumber): The angle in radians of the tiled pattern.
    ;;   - width (distance/NSNumber): The width of a tile.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (glide-reflected-tile center angle width)
      (make-filter-proc
        (make-image-filter
          'glide-reflected-tile
          `((input-center . ,center)
            (input-angle . ,angle)
            (input-width . ,width)))))

    ;; Returns an image processor for image filter gloom (CIGloom).
    ;; Dulls the highlights of an image.
    ;;   - radius (distance/NSNumber): The radius determines how many pixels are used to
    ;;     create the effect. The larger the radius, the greater the effect.
    ;;   - intensity (scalar/NSNumber): The intensity of the effect. A value of 0.0 is
    ;;     no effect. A value of 1.0 is the maximum effect.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (gloom radius intensity)
      (make-filter-proc
        (make-image-filter
          'gloom
          `((input-radius . ,radius)
            (input-intensity . ,intensity)))))

    ;; Returns an image processor for image filter guided-filter (CIGuidedFilter).
    ;; Upsamples a small image to the size of the guide image using the content of the guide
    ;; to preserve detail.
    ;;   - guide-image (abstract-image/CIImage): A larger image to use as a guide.
    ;;   - radius (scalar/NSNumber): The distance from the center of the effect.
    ;;   - epsilon (scalar/NSNumber)
    ;; Filter categories: still-image, builtin, geometry-adjustment, high-dynamic-range, video
    (define (guided-filter guide-image radius epsilon)
      (make-filter-proc
        (make-image-filter
          'guided-filter
          `((input-guide-image . ,guide-image)
            (input-radius . ,radius)
            (input-epsilon . ,epsilon)))))

    ;; Returns an image processor for image filter hard-light-blend-mode (CIHardLightBlendMode).
    ;; Either multiplies or screens colors, depending on the source image sample color. If
    ;; the source image sample color is lighter than 50% gray, the background is lightened,
    ;; similar to screening. If the source image sample color is darker than 50% gray, the
    ;; background is darkened, similar to multiplying. If the source image sample color is
    ;; equal to 50% gray, the source image is not changed. Image samples that are equal to
    ;; pure black or pure white result in pure black or white. The overall effect is similar
    ;; to what you would achieve by shining a harsh spotlight on the source image.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (hard-light-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'hard-light-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter hatched-screen (CIHatchedScreen).
    ;; Simulates the hatched pattern of a halftone screen.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - angle (angle/NSNumber): The angle in radians of the pattern.
    ;;   - width (distance/NSNumber): The distance between lines in the pattern.
    ;;   - sharpness (scalar/NSNumber): The amount of sharpening to apply.
    ;; Filter categories: still-image, builtin, halftone-effect, video
    (define (hatched-screen center angle width sharpness)
      (make-filter-proc
        (make-image-filter
          'hatched-screen
          `((input-center . ,center)
            (input-angle . ,angle)
            (input-width . ,width)
            (input-sharpness . ,sharpness)))))

    ;; Returns an image processor for image filter height-field-from-mask (CIHeightFieldFromMask).
    ;; Produces a continuous three-dimensional, loft-shaped height field from a grayscale
    ;; mask. The white values of the mask define those pixels that are inside the height
    ;; field while the black values define those pixels that are outside. The field varies
    ;; smoothly and continuously inside the mask, reaching the value 0 at the edge of the
    ;; mask. You can use this filter with the Shaded Material filter to produce extremely
    ;; realistic shaded objects.
    ;;   - radius (distance/NSNumber): The distance from the edge of the mask for the smooth
    ;;     transition is proportional to the input radius. Larger values make the transition
    ;;     smoother and more pronounced. Smaller values make the transition approximate a
    ;;     fillet radius.
    ;; Filter categories: still-image, builtin, stylize, video
    (define (height-field-from-mask radius)
      (make-filter-proc
        (make-image-filter
          'height-field-from-mask
          `((input-radius . ,radius)))))

    ;; Returns an image processor for image filter hexagonal-pixellate (CIHexagonalPixellate).
    ;; Displays an image as colored hexagons whose color is an average of the pixels they replace.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - scale (distance/NSNumber): The scale determines the size of the hexagons. Larger
    ;;     values result in larger hexagons.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (hexagonal-pixellate center scale)
      (make-filter-proc
        (make-image-filter
          'hexagonal-pixellate
          `((input-center . ,center)
            (input-scale . ,scale)))))

    ;; Returns an image processor for image filter highlight-shadow-adjust
    ;; (CIHighlightShadowAdjust).
    ;; Adjust the tonal mapping of an image while preserving spatial detail.
    ;;   - radius (scalar/NSNumber): Shadow Highlight Radius.
    ;;   - shadow-amount (scalar/NSNumber): The amount of adjustment to the shadows of
    ;;     the image.
    ;;   - highlight-amount (scalar/NSNumber): The amount of adjustment to the highlights
    ;;     of the image.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (highlight-shadow-adjust radius shadow-amount highlight-amount)
      (make-filter-proc
        (make-image-filter
          'highlight-shadow-adjust
          `((input-radius . ,radius)
            (input-shadow-amount . ,shadow-amount)
            (input-highlight-amount . ,highlight-amount)))))

    ;; Returns an image processor for image filter histogram-display-filter
    ;; (CIHistogramDisplayFilter).
    ;; Generates a displayable histogram image from the output of the “Area Histogram” filter.
    ;;   - height (scalar/NSNumber): The height of the displayable histogram image.
    ;;   - high-limit (scalar/NSNumber): The fraction of the right portion of the
    ;;     histogram image to make lighter.
    ;;   - low-limit (scalar/NSNumber): The fraction of the left portion of the
    ;;     histogram image to make darker.
    ;; Filter categories: still-image, builtin, reduction, video
    (define (histogram-display-filter height high-limit low-limit)
      (make-filter-proc
        (make-image-filter
          'histogram-display-filter
          `((input-height . ,height)
            (input-high-limit . ,high-limit)
            (input-low-limit . ,low-limit)))))

    ;; Returns an image processor for image filter hole-distortion (CIHoleDistortion).
    ;; Creates a circular area that pushes the image pixels outward, distorting those pixels
    ;; closest to the circle the most.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - radius (distance/NSNumber): The radius determines how many pixels are used to
    ;;     create the distortion. The larger the radius, the wider the extent of the distortion.
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (hole-distortion center radius)
      (make-filter-proc
        (make-image-filter
          'hole-distortion
          `((input-center . ,center)
            (input-radius . ,radius)))))

    ;; Returns an image processor for image filter hue-adjust (CIHueAdjust).
    ;; Changes the overall hue, or tint, of the source pixels.
    ;;   - angle (angle/NSNumber): An angle in radians to use to correct the hue of an image.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    high-dynamic-range, color-adjustment, video
    (define (hue-adjust angle)
      (make-filter-proc
        (make-image-filter
          'hue-adjust
          `((input-angle . ,angle)))))

    ;; Returns an image processor for image filter hue-blend-mode (CIHueBlendMode).
    ;; Uses the luminance and saturation values of the background with the hue of the
    ;; source image.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (hue-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'hue-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter hue-saturation-value-gradient
    ;; (CIHueSaturationValueGradient).
    ;; Generates a color wheel that shows hues and saturations for a specified value.
    ;;   - value (scalar/NSNumber): The color value used to generate the color wheel.
    ;;   - radius (distance/NSNumber): The distance from the center of the effect.
    ;;   - softness (scalar/NSNumber)
    ;;   - dither (scalar/NSNumber)
    ;;   - color-space (color-space/NSObject): The CGColorSpaceRef that the color wheel
    ;;     should be generated in.
    ;; Filter categories: still-image, builtin, gradient, video
    (define (hue-saturation-value-gradient value radius softness dither color-space)
      (image-filter-output
        (make-image-filter
          'hue-saturation-value-gradient
          `((input-value . ,value)
            (input-radius . ,radius)
            (input-softness . ,softness)
            (input-dither . ,dither)
            (input-color-space . ,color-space)))))

    ;; Returns an image processor for image filter kaleidoscope (CIKaleidoscope).
    ;; Produces a kaleidoscopic image from a source image by applying 12-way symmetry.
    ;;   - count (scalar/NSNumber): The number of reflections in the pattern.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - angle (angle/NSNumber): The angle in radians of reflection.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (kaleidoscope count center angle)
      (make-filter-proc
        (make-image-filter
          'kaleidoscope
          `((input-count . ,count)
            (input-center . ,center)
            (input-angle . ,angle)))))

    ;; Returns an image processor for image filter keystone-correction-combined
    ;; (CIKeystoneCorrectionCombined).
    ;; Apply keystone correction to an image with combined horizontal and vertical guides.
    ;;   - focal-length (scalar/NSNumber): 35mm equivalent focal length of the input image.
    ;;   - top-left (point/CIVector): The top left coordinate of the guide.
    ;;   - top-right (point/CIVector): The top right coordinate of the guide.
    ;;   - bottom-right (point/CIVector): The bottom right coordinate of the guide.
    ;;   - bottom-left (point/CIVector): The bottom left coordinate of the guide.
    ;; Filter categories: still-image, builtin, geometry-adjustment, high-dynamic-range, video
    (define (keystone-correction-combined focal-length top-left top-right bottom-right bottom-left)
      (make-filter-proc
        (make-image-filter
          'keystone-correction-combined
          `((input-focal-length . ,focal-length)
            (input-top-left . ,top-left)
            (input-top-right . ,top-right)
            (input-bottom-right . ,bottom-right)
            (input-bottom-left . ,bottom-left)))))

    ;; Returns an image processor for image filter keystone-correction-horizontal
    ;; (CIKeystoneCorrectionHorizontal).
    ;; Apply horizontal keystone correction to an image with guides.
    ;;   - focal-length (scalar/NSNumber): 35mm equivalent focal length of the input image.
    ;;   - top-left (point/CIVector): The top left coordinate of the guide.
    ;;   - top-right (point/CIVector): The top right coordinate of the guide.
    ;;   - bottom-right (point/CIVector): The bottom right coordinate of the guide.
    ;;   - bottom-left (point/CIVector): The bottom left coordinate of the guide.
    ;; Filter categories: still-image, builtin, geometry-adjustment, high-dynamic-range, video
    (define (keystone-correction-horizontal focal-length
                                            top-left
                                            top-right
                                            bottom-right
                                            bottom-left)
      (make-filter-proc
        (make-image-filter
          'keystone-correction-horizontal
          `((input-focal-length . ,focal-length)
            (input-top-left . ,top-left)
            (input-top-right . ,top-right)
            (input-bottom-right . ,bottom-right)
            (input-bottom-left . ,bottom-left)))))

    ;; Returns an image processor for image filter keystone-correction-vertical
    ;; (CIKeystoneCorrectionVertical).
    ;; Apply vertical keystone correction to an image with guides.
    ;;   - focal-length (scalar/NSNumber): 35mm equivalent focal length of the input image.
    ;;   - top-left (point/CIVector): The top left coordinate of the guide.
    ;;   - top-right (point/CIVector): The top right coordinate of the guide.
    ;;   - bottom-right (point/CIVector): The bottom right coordinate of the guide.
    ;;   - bottom-left (point/CIVector): The bottom left coordinate of the guide.
    ;; Filter categories: still-image, builtin, geometry-adjustment, high-dynamic-range, video
    (define (keystone-correction-vertical focal-length top-left top-right bottom-right bottom-left)
      (make-filter-proc
        (make-image-filter
          'keystone-correction-vertical
          `((input-focal-length . ,focal-length)
            (input-top-left . ,top-left)
            (input-top-right . ,top-right)
            (input-bottom-right . ,bottom-right)
            (input-bottom-left . ,bottom-left)))))

    ;; Returns an image processor for image filter kmeans (CIKMeans).
    ;; Create a palette of the most common colors found in the image.
    ;;   - extent (rect/CIVector): A rectangle that defines the extent of the effect.
    ;;   - means (abstract-image/CIImage): Specifies the color seeds to use for k-means
    ;;     clustering, either passed as an image or an array of colors.
    ;;   - count (count/NSNumber): Specifies how many k-means color clusters should be used.
    ;;   - passes (count/NSNumber): Specifies how many k-means passes should be performed.
    ;;   - perceptual (boolean/NSNumber): Specifies whether the k-means color palette
    ;;     should be computed in a perceptual color space.
    ;; Filter categories: still-image, builtin, reduction, high-dynamic-range, video
    (define (kmeans extent means count passes perceptual)
      (make-filter-proc
        (make-image-filter
          'kmeans
          `((input-extent . ,extent)
            (input-means . ,means)
            (input-count . ,count)
            (input-passes . ,passes)
            (input-perceptual . ,perceptual)))))

    ;; Returns an image processor for image filter lab-delta-e (CILabDeltaE).
    ;; Produces an image with the Lab ∆E difference values between two images. The result
    ;; image will contain ∆E 1994 values between 0.0 and 100.0 where 2.0 is considered a
    ;; just noticeable difference.
    ;;   - image2 (abstract-image/CIImage): The second input image for comparison.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect, video
    (define (lab-delta-e image2)
      (make-filter-proc
        (make-image-filter
          'lab-delta-e
          `((input-image2 . ,image2)))))

    ;; Returns an image processor for image filter lanczos-scale-transform
    ;; (CILanczosScaleTransform).
    ;; Produces a high-quality, scaled version of a source image. You typically use this
    ;; filter to scale down an image.
    ;;   - scale (scalar/NSNumber): The scaling factor to use on the image. Values less
    ;;     than 1.0 scale down the images. Values greater than 1.0 scale up the image.
    ;;   - aspect-ratio (scalar/NSNumber): The additional horizontal scaling factor to
    ;;     use on the image.
    ;; Filter categories: still-image, builtin, geometry-adjustment, high-dynamic-range, video
    (define (lanczos-scale-transform scale aspect-ratio)
      (make-filter-proc
        (make-image-filter
          'lanczos-scale-transform
          `((input-scale . ,scale)
            (input-aspect-ratio . ,aspect-ratio)))))

    ;; Returns an image processor for image filter lenticular-halo-generator
    ;; (CILenticularHaloGenerator).
    ;; Simulates a halo that is generated by the diffraction associated with the spread
    ;; of a lens. This filter is typically applied to another image to simulate lens flares
    ;; and similar effects.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - color (color/CIColor): A color.
    ;;   - halo-radius (distance/NSNumber): The radius of the halo.
    ;;   - halo-width (distance/NSNumber): The width of the halo, from its inner radius to
    ;;     its outer radius.
    ;;   - halo-overlap (scalar/NSNumber)
    ;;   - striation-strength (scalar/NSNumber): The intensity of the halo colors. Larger
    ;;     values are more intense.
    ;;   - striation-contrast (scalar/NSNumber): The contrast of the halo colors. Larger
    ;;     values are higher contrast.
    ;;   - time (scalar/NSNumber): The duration of the effect.
    ;; Filter categories: still-image, builtin, generator, high-dynamic-range, video
    (define (lenticular-halo-generator center
                                       color
                                       halo-radius
                                       halo-width halo-overlap
                                       striation-strength
                                       striation-contrast
                                       time)
      (image-filter-output
        (make-image-filter
          'lenticular-halo-generator
          `((input-center . ,center)
            (input-color . ,color)
            (input-halo-radius . ,halo-radius)
            (input-halo-width . ,halo-width)
            (input-halo-overlap . ,halo-overlap)
            (input-striation-strength . ,striation-strength)
            (input-striation-contrast . ,striation-contrast)
            (input-time . ,time)))))

    ;; Returns an image processor for image filter lighten-blend-mode (CILightenBlendMode).
    ;; Creates composite image samples by choosing the lighter samples (either from the
    ;; source image or the background). The result is that the background image samples
    ;; are replaced by any source image samples that are lighter. Otherwise, the background
    ;; image samples are left unchanged.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (lighten-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'lighten-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter light-tunnel (CILightTunnel).
    ;; Light tunnel distortion.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - rotation (angle/NSNumber): Rotation angle in radians of the light tunnel.
    ;;   - radius (distance/NSNumber): Center radius of the light tunnel.
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (light-tunnel center rotation radius)
      (make-filter-proc
        (make-image-filter
          'light-tunnel
          `((input-center . ,center)
            (input-rotation . ,rotation)
            (input-radius . ,radius)))))

    ;; Returns an image processor for image filter linear-burn-blend-mode (CILinearBurnBlendMode).
    ;; Inverts the unpremultiplied source and background image sample color, inverts the
    ;; sum, and then blends the result with the background according to the PDF basic
    ;; compositing formula. Source image values that are white produce no change. Source
    ;; image values that are black invert the background color values.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (linear-burn-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'linear-burn-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter linear-dodge-blend-mode
    ;; (CILinearDodgeBlendMode).
    ;; Unpremultiplies the source and background image sample colors, adds them, and
    ;; then blends the result with the background according to the PDF basic compositing
    ;; formula. Source image values that are black produces output that is the same as
    ;; the background. Source image values that are non-black brighten the background
    ;; color values.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (linear-dodge-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'linear-dodge-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter linear-gradient (CILinearGradient).
    ;; Generates a gradient that varies along a linear axis between two defined endpoints.
    ;;   - point0 (point/CIVector): The starting position of the gradient -- where the
    ;;     first color begins.
    ;;   - point1 (point/CIVector): The ending position of the gradient -- where the
    ;;     second color begins.
    ;;   - color0 (color/CIColor): The first color to use in the gradient.
    ;;   - color1 (color/CIColor): The second color to use in the gradient.
    ;; Filter categories: still-image, builtin, gradient, high-dynamic-range, video
    (define (linear-gradient point0 point1 color0 color1)
      (image-filter-output
        (make-image-filter
          'linear-gradient
          `((input-point0 . ,point0)
            (input-point1 . ,point1)
            (input-color0 . ,color0)
            (input-color1 . ,color1)))))

    ;; Returns an image processor for image filter linear-light-blend-mode (CILinearLightBlendMode).
    ;; A blend mode that is a combination of linear burn and linear dodge blend modes.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (linear-light-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'linear-light-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter linear-to-srgbtone-curve
    ;; (CILinearToSRGBToneCurve).
    ;; Converts an image in linear space to sRGB space.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    high-dynamic-range, color-adjustment, video
    (define (linear-to-srgbtone-curve)
      (make-filter-proc
        (make-image-filter
          'linear-to-srgbtone-curve)))

    ;; Returns an image processor for image filter line-overlay (CILineOverlay).
    ;; Creates a sketch that outlines the edges of an image in black, leaving the non-outlined
    ;; portions of the image transparent. The result has alpha and is rendered in black, so
    ;; it won’t look like much until you render it over another image using source over
    ;; compositing.
    ;;   - n-r-noise-level (scalar/NSNumber): The noise level of the image (used with
    ;;     camera data) that gets removed before tracing the edges of the image.
    ;;     Increasing the noise level helps to clean up the traced edges of the image.
    ;;   - n-r-sharpness (scalar/NSNumber): The amount of sharpening done when removing
    ;;     noise in the image before tracing the edges of the image. This improves
    ;;     the edge acquisition.
    ;;   - edge-intensity (scalar/NSNumber): The accentuation factor of the Sobel
    ;;     gradient information when tracing the edges of the image. Higher values
    ;;     find more edges, although typically a low value (such as 1.0) is used.
    ;;   - threshold (scalar/NSNumber): This value determines edge visibility. Larger
    ;;     values thin out the edges.
    ;;   - contrast (scalar/NSNumber): The amount of anti-aliasing to use on the edges
    ;;     produced by this filter. Higher values produce higher contrast edges (they
    ;;     are less anti-aliased).
    ;; Filter categories: still-image, builtin, stylize, video
    (define (line-overlay n-r-noise-level n-r-sharpness edge-intensity threshold contrast)
      (make-filter-proc
        (make-image-filter
          'line-overlay
          `((input-n-r-noise-level . ,n-r-noise-level)
            (input-n-r-sharpness . ,n-r-sharpness)
            (input-edge-intensity . ,edge-intensity)
            (input-threshold . ,threshold)
            (input-contrast . ,contrast)))))

    ;; Returns an image processor for image filter line-screen (CILineScreen).
    ;; Simulates the line pattern of a halftone screen.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - angle (angle/NSNumber): The angle in radians of the pattern.
    ;;   - width (distance/NSNumber): The distance between lines in the pattern.
    ;;   - sharpness (scalar/NSNumber): The sharpness of the pattern. The larger the value,
    ;;     the sharper the pattern.
    ;; Filter categories: still-image, builtin, halftone-effect, video
    (define (line-screen center angle width sharpness)
      (make-filter-proc
        (make-image-filter
          'line-screen
          `((input-center . ,center)
            (input-angle . ,angle)
            (input-width . ,width)
            (input-sharpness . ,sharpness)))))

    ;; Returns an image processor for image filter luminosity-blend-mode (CILuminosityBlendMode).
    ;; Uses the hue and saturation of the background with the luminance of the source
    ;; image. This mode creates an effect that is inverse to the effect created by the
    ;; “Color Blend Mode” filter.
    ;;   - background-image (abstract-image/CIImage): The image to use as a
    ;;     background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (luminosity-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'luminosity-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter masked-variable-blur (CIMaskedVariableBlur).
    ;; Blurs an image according to the brightness levels in a mask image.
    ;;   - mask (abstract-image/CIImage): The mask image that determines how much to blur
    ;;     the image. The mask’s green channel value from 0.0 to 1.0 determines if the image
    ;;     is not blurred or blurred by the full radius.
    ;;   - radius (scalar/NSNumber): A value that governs the maximum blur radius to apply.
    ;; Filter categories: still-image, builtin, blur, high-dynamic-range, video
    (define (masked-variable-blur mask radius)
      (make-filter-proc
        (make-image-filter
          'masked-variable-blur
          `((input-mask . ,mask)
            (input-radius . ,radius)))))

    ;; Returns an image processor for image filter mask-to-alpha (CIMaskToAlpha).
    ;; Converts a grayscale image to a white image that is masked by alpha. The white
    ;; values from the source image produce the inside of the mask; the black values become
    ;; completely transparent.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect, video
    (define (mask-to-alpha)
      (make-filter-proc
        (make-image-filter
          'mask-to-alpha)))

    ;; Returns an image processor for image filter maximum-component (CIMaximumComponent).
    ;; Converts an image to grayscale using the maximum of the three color components.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (maximum-component)
      (make-filter-proc
        (make-image-filter
          'maximum-component)))

    ;; Returns an image processor for image filter maximum-compositing (CIMaximumCompositing).
    ;; Computes the maximum value, by color component, of two input images and creates an
    ;; output image using the maximum values. This is similar to dodging.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, high-dynamic-range, video
    (define (maximum-compositing background-image)
      (make-filter-proc
        (make-image-filter
          'maximum-compositing
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter maximum-scale-transform
    ;; (CIMaximumScaleTransform).
    ;; Produces a scaled version of a source image that uses the maximum of neighboring
    ;; pixels instead of linear averaging.
    ;;   - scale (scalar/NSNumber): The scaling factor to use on the image. Values less
    ;;     than 1.0 scale down the images. Values greater than 1.0 scale up the image.
    ;;   - aspect-ratio (scalar/NSNumber): The additional horizontal scaling factor to
    ;;     use on the image.
    ;; Filter categories: still-image, builtin, geometry-adjustment, high-dynamic-range, video
    (define (maximum-scale-transform scale aspect-ratio)
      (make-filter-proc
        (make-image-filter
          'maximum-scale-transform
          `((input-scale . ,scale)
            (input-aspect-ratio . ,aspect-ratio)))))

    ;; Returns an image processor for image filter median-filter (CIMedianFilter).
    ;; Computes the median value for a group of neighboring pixels and replaces each pixel
    ;; value with the median. The effect is to reduce noise.
    ;; Filter categories: still-image, builtin, blur, high-dynamic-range, video
    (define (median-filter)
      (make-filter-proc
        (make-image-filter
          'median-filter)))

    ;; Returns an image processor for image filter mesh-generator (CIMeshGenerator).
    ;; Generates a mesh from an array of line segments.
    ;;   - width (distance/NSNumber): The width in pixels of the effect.
    ;;   - color (color/CIColor): A color.
    ;;   - mesh (array/NSArray): An array of line segments stored as an array of CIVectors
    ;;     each containing a start point and end point.
    ;; Filter categories: still-image, builtin, generator, high-dynamic-range, video
    (define (mesh-generator width color mesh)
      (image-filter-output
        (make-image-filter
          'mesh-generator
          `((input-width . ,width)
            (input-color . ,color)
            (input-mesh . ,mesh)))))

    ;; Returns an image processor for image filter minimum-component (CIMinimumComponent).
    ;; Converts an image to grayscale using the minimum of the three color components.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (minimum-component)
      (make-filter-proc
        (make-image-filter
          'minimum-component)))

    ;; Returns an image processor for image filter minimum-compositing (CIMinimumCompositing).
    ;; Computes the minimum value, by color component, of two input images and creates
    ;; an output image using the minimum values. This is similar to burning.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, high-dynamic-range, video
    (define (minimum-compositing background-image)
      (make-filter-proc
        (make-image-filter
          'minimum-compositing
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter mix (CIMix).
    ;; Uses an amount parameter to interpolate between an image and a background image.
    ;; When value is 0.0 or less, the result is the background image. When the value is 1.0
    ;; or more, the result is the image.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;;   - amount (scalar/NSNumber): The amount of the effect.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (mix background-image amount)
      (make-filter-proc
        (make-image-filter
          'mix
          `((input-background-image . ,background-image)
            (input-amount . ,amount)))))

    ;; Returns an image processor for image filter mod-transition (CIModTransition).
    ;; Transitions from one image to another by revealing the target image through
    ;; irregularly shaped holes.
    ;;   - target-image (abstract-image/CIImage): The target image for a transition.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - time (time/NSNumber): The parametric time of the transition. This value drives
    ;;     the transition from start (at time 0) to end (at time 1).
    ;;   - angle (angle/NSNumber): The angle in radians of the mod hole pattern.
    ;;   - radius (distance/NSNumber): The radius of the undistorted holes in the pattern.
    ;;   - compression (distance/NSNumber): The amount of stretching applied to the mod
    ;;     hole pattern. Holes in the center are not distorted as much as those at the
    ;;     edge of the image.
    ;; Filter categories: still-image, builtin, transition, high-dynamic-range, video
    (define (mod-transition target-image center time angle radius compression)
      (make-filter-proc
        (make-image-filter
          'mod-transition
          `((input-target-image . ,target-image)
            (input-center . ,center)
            (input-time . ,time)
            (input-angle . ,angle)
            (input-radius . ,radius)
            (input-compression . ,compression)))))

    ;; Returns an image processor for image filter morphology-gradient (CIMorphologyGradient).
    ;; Finds the edges of an image by returning the difference between the morphological
    ;; minimum and maximum operations to the image.
    ;;   - radius (distance/NSNumber): The desired radius of the circular morphological
    ;;     operation to the image.
    ;; Filter categories: still-image, builtin, blur, high-dynamic-range, video
    (define (morphology-gradient radius)
      (make-filter-proc
        (make-image-filter
          'morphology-gradient
          `((input-radius . ,radius)))))

    ;; Returns an image processor for image filter morphology-maximum (CIMorphologyMaximum).
    ;; Lightens areas of an image by applying a circular morphological maximum operation
    ;; to the image.
    ;;   - radius (distance/NSNumber): The desired radius of the circular morphological
    ;;     operation to the image.
    ;; Filter categories: still-image, builtin, blur, high-dynamic-range, video
    (define (morphology-maximum radius)
      (make-filter-proc
        (make-image-filter
          'morphology-maximum
          `((input-radius . ,radius)))))

    ;; Returns an image processor for image filter morphology-minimum (CIMorphologyMinimum).
    ;; Darkens areas of an image by applying a circular morphological maximum operation to
    ;; the image.
    ;;   - radius (distance/NSNumber): The desired radius of the circular morphological
    ;;     operation to the image.
    ;; Filter categories: still-image, builtin, blur, high-dynamic-range, video
    (define (morphology-minimum radius)
      (make-filter-proc
        (make-image-filter
          'morphology-minimum
          `((input-radius . ,radius)))))

    ;; Returns an image processor for image filter morphology-rectangle-maximum
    ;; (CIMorphologyRectangleMaximum).
    ;; Lightens areas of an image by applying a rectangular morphological maximum operation
    ;; to the image.
    ;;   - width (integer/NSNumber): The width in pixels of the morphological operation. The
    ;;     value will be rounded to the nearest odd integer.
    ;;   - height (integer/NSNumber): The height in pixels of the morphological operation.
    ;;     The value will be rounded to the nearest odd integer.
    ;; Filter categories: still-image, builtin, blur, high-dynamic-range, video
    (define (morphology-rectangle-maximum width height)
      (make-filter-proc
        (make-image-filter
          'morphology-rectangle-maximum
          `((input-width . ,width)
            (input-height . ,height)))))

    ;; Returns an image processor for image filter morphology-rectangle-minimum
    ;; (CIMorphologyRectangleMinimum).
    ;; Darkens areas of an image by applying a rectangular morphological maximum operation
    ;; to the image.
    ;;   - width (integer/NSNumber): The width in pixels of the morphological operation.
    ;;     The value will be rounded to the nearest odd integer.
    ;;   - height (integer/NSNumber): The height in pixels of the morphological operation.
    ;;     The value will be rounded to the nearest odd integer.
    ;; Filter categories: still-image, builtin, blur, high-dynamic-range, video
    (define (morphology-rectangle-minimum width height)
      (make-filter-proc
        (make-image-filter
          'morphology-rectangle-minimum
          `((input-width . ,width)
            (input-height . ,height)))))

    ;; Returns an image processor for image filter motion-blur (CIMotionBlur).
    ;; Blurs an image to simulate the effect of using a camera that moves a specified angle
    ;; and distance while capturing the image.
    ;;   - radius (distance/NSNumber): The radius determines how many pixels are used to
    ;;     create the blur. The larger the radius, the blurrier the result.
    ;;   - angle (angle/NSNumber): The angle in radians of the motion determines which
    ;;     direction the blur smears.
    ;; Filter categories: still-image, builtin, blur, high-dynamic-range, video
    (define (motion-blur radius angle)
      (make-filter-proc
        (make-image-filter
          'motion-blur
          `((input-radius . ,radius)
            (input-angle . ,angle)))))

    ;; Returns an image processor for image filter multiply-blend-mode (CIMultiplyBlendMode).
    ;; Multiplies the source image samples with the background image samples. This results in
    ;; colors that are at least as dark as either of the two contributing sample colors.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (multiply-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'multiply-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter multiply-compositing (CIMultiplyCompositing).
    ;; Multiplies the color component of two input images and creates an output image using
    ;; the multiplied values. This filter is typically used to add a spotlight or similar
    ;; lighting effect to an image.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, high-dynamic-range, video
    (define (multiply-compositing background-image)
      (make-filter-proc
        (make-image-filter
          'multiply-compositing
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter nine-part-stretched (CINinePartStretched).
    ;; Distorts an image by stretching an image based on two input breakpoints.
    ;;   - breakpoint0 (point/CIVector): Lower left corner of image to retain before
    ;;     stretching begins.
    ;;   - breakpoint1 (point/CIVector): Upper right corner of image to retain after
    ;;     stretching ends.
    ;;   - grow-amount (offset/CIVector): Vector indicating how much image should grow
    ;;     in pixels in both dimensions.
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (nine-part-stretched breakpoint0 breakpoint1 grow-amount)
      (make-filter-proc
        (make-image-filter
          'nine-part-stretched
          `((input-breakpoint0 . ,breakpoint0)
            (input-breakpoint1 . ,breakpoint1)
            (input-grow-amount . ,grow-amount)))))

    ;; Returns an image processor for image filter nine-part-tiled (CINinePartTiled).
    ;; Distorts an image by tiling an image based on two input breakpoints.
    ;;   - breakpoint0 (point/CIVector): Lower left corner of image to retain before
    ;;     tiling begins.
    ;;   - breakpoint1 (point/CIVector): Upper right corner of image to retain after
    ;;     tiling ends.
    ;;   - grow-amount (offset/CIVector): Vector indicating how much image should grow
    ;;     in pixels in both dimensions.
    ;;   - flip-y-tiles (boolean/NSNumber): Indicates that Y-Axis flip should occur.
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (nine-part-tiled breakpoint0 breakpoint1 grow-amount flip-y-tiles)
      (make-filter-proc
        (make-image-filter
          'nine-part-tiled
          `((input-breakpoint0 . ,breakpoint0)
            (input-breakpoint1 . ,breakpoint1)
            (input-grow-amount . ,grow-amount)
            (input-flip-y-tiles . ,flip-y-tiles)))))

    ;; Returns an image processor for image filter noise-reduction (CINoiseReduction).
    ;; Reduces noise using a threshold value to define what is considered noise. Small
    ;; changes in luminance below that value are considered noise and get a noise reduction
    ;; treatment, which is a local blur. Changes above the threshold value are considered
    ;; edges, so they are sharpened.
    ;;   - noise-level (scalar/NSNumber): The amount of noise reduction. The larger the
    ;;     value, the more noise reduction.
    ;;   - sharpness (scalar/NSNumber): The sharpness of the final image. The larger the
    ;;     value, the sharper the result.
    ;; Filter categories: still-image, builtin, blur, high-dynamic-range, video
    (define (noise-reduction noise-level sharpness)
      (make-filter-proc
        (make-image-filter
          'noise-reduction
          `((input-noise-level . ,noise-level)
            (input-sharpness . ,sharpness)))))

    ;; Returns an image processor for image filter op-tile (CIOpTile).
    ;; Segments an image, applying any specified scaling and rotation, and then assembles
    ;; the image again to give an op art appearance.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - scale (scalar/NSNumber): The scale determines the number of tiles in the effect.
    ;;   - angle (angle/NSNumber): The angle in radians of a tile.
    ;;   - width (distance/NSNumber): The width of a tile.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (op-tile center scale angle width)
      (make-filter-proc
        (make-image-filter
          'op-tile
          `((input-center . ,center)
            (input-scale . ,scale)
            (input-angle . ,angle)
            (input-width . ,width)))))

    ;; Returns an image processor for image filter overlay-blend-mode (CIOverlayBlendMode).
    ;; Either multiplies or screens the source image samples with the background image samples,
    ;; depending on the background color. The result is to overlay the existing image samples
    ;; while preserving the highlights and shadows of the background. The background color
    ;; mixes with the source image to reflect the lightness or darkness of the background.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (overlay-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'overlay-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter page-curl-transition (CIPageCurlTransition).
    ;; Transitions from one image to another by simulating a curling page, revealing the new
    ;; image as the page curls.
    ;;   - target-image (abstract-image/CIImage): The target image for a transition.
    ;;   - backside-image (abstract-image/CIImage): The image that appears on the back of
    ;;     the source image, as the page curls to reveal the target image.
    ;;   - shading-image (abstract-image/CIImage): An image that looks like a shaded sphere
    ;;     enclosed in a square image.
    ;;   - extent (rect/CIVector): The extent of the effect.
    ;;   - time (time/NSNumber): The parametric time of the transition. This value drives
    ;;     the transition from start (at time 0) to end (at time 1).
    ;;   - angle (angle/NSNumber): The angle in radians of the curling page.
    ;;   - radius (distance/NSNumber): The radius of the curl.
    ;; Filter categories: still-image, builtin, transition, high-dynamic-range, video
    (define (page-curl-transition target-image
                                  backside-image
                                  shading-image
                                  extent
                                  time
                                  angle
                                  radius)
      (make-filter-proc
        (make-image-filter
          'page-curl-transition
          `((input-target-image . ,target-image)
            (input-backside-image . ,backside-image)
            (input-shading-image . ,shading-image)
            (input-extent . ,extent)
            (input-time . ,time)
            (input-angle . ,angle)
            (input-radius . ,radius)))))

    ;; Returns an image processor for image filter page-curl-with-shadow-transition
    ;; (CIPageCurlWithShadowTransition).
    ;; Transitions from one image to another by simulating a curling page, revealing the
    ;; new image as the page curls.
    ;;   - target-image (abstract-image/CIImage): The target image for a transition.
    ;;   - backside-image (abstract-image/CIImage): The image that appears on the back
    ;;     of the source image, as the page curls to reveal the target image.
    ;;   - extent (rect/CIVector): The extent of the effect.
    ;;   - time (time/NSNumber): The parametric time of the transition. This value drives
    ;; the transition from start (at time 0) to end (at time 1).
    ;;   - angle (angle/NSNumber): The angle in radians of the curling page.
    ;;   - radius (distance/NSNumber): The radius of the curl.
    ;;   - shadow-size (distance/NSNumber): The maximum size in pixels of the shadow.
    ;;   - shadow-amount (distance/NSNumber): The strength of the shadow.
    ;;   - shadow-extent (rect/CIVector): The rectagular portion of input image that will
    ;;     cast a shadow.
    ;; Filter categories: still-image, builtin, transition, high-dynamic-range, video
    (define (page-curl-with-shadow-transition target-image
                                              backside-image
                                              extent
                                              time
                                              angle
                                              radius
                                              shadow-size
                                              shadow-amount
                                              shadow-extent)
      (make-filter-proc
        (make-image-filter
          'page-curl-with-shadow-transition
          `((input-target-image . ,target-image)
            (input-backside-image . ,backside-image)
            (input-extent . ,extent)
            (input-time . ,time)
            (input-angle . ,angle)
            (input-radius . ,radius)
            (input-shadow-size . ,shadow-size)
            (input-shadow-amount . ,shadow-amount)
            (input-shadow-extent . ,shadow-extent)))))

    ;; Returns an image processor for image filter palette-centroid (CIPaletteCentroid).
    ;; Calculate the mean (x,y) image coordinates of a color palette.
    ;;   - palette-image (abstract-image/CIImage): The input color palette, obtained using
    ;;     “CIKMeans“ filter.
    ;;   - perceptual (boolean/NSNumber): Specifies whether the color palette should be
    ;;     applied in a perceptual color space.
    ;; Filter categories: still-image, builtin, color-effect, video
    (define (palette-centroid palette-image perceptual)
      (make-filter-proc
        (make-image-filter
          'palette-centroid
          `((input-palette-image . ,palette-image)
            (input-perceptual . ,perceptual)))))

    ;; Returns an image processor for image filter palettize (CIPalettize).
    ;; Paint an image from a color palette obtained using “CIKMeans“.
    ;;   - palette-image (abstract-image/CIImage): The input color palette, obtained using
    ;;     “CIKMeans“ filter.
    ;;   - perceptual (boolean/NSNumber): Specifies whether the color palette should be
    ;;     applied in a perceptual color space.
    ;; Filter categories: still-image, builtin, color-effect, video
    (define (palettize palette-image perceptual)
      (make-filter-proc
        (make-image-filter
          'palettize
          `((input-palette-image . ,palette-image)
            (input-perceptual . ,perceptual)))))

    ;; Returns an image processor for image filter parallelogram-tile (CIParallelogramTile).
    ;; Warps an image by reflecting it in a parallelogram, and then tiles the result.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - angle (angle/NSNumber): The angle in radians of the tiled pattern.
    ;;   - acute-angle (angle/NSNumber): The primary angle for the repeating parallelogram
    ;;     tile. Small values create thin diamond tiles, and higher values create fatter
    ;;     parallelogram tiles.
    ;;   - width (distance/NSNumber): The width of a tile.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (parallelogram-tile center angle acute-angle width)
      (make-filter-proc
        (make-image-filter
          'parallelogram-tile
          `((input-center . ,center)
            (input-angle . ,angle)
            (input-acute-angle . ,acute-angle)
            (input-width . ,width)))))

    ;; Returns an image processor for image filter pdf417-barcode-generator
    ;; (CIPDF417BarcodeGenerator).
    ;; Generate a PDF417 barcode image for message data.
    ;;   - message (bytevector/NSData): The message to encode in the PDF417 Barcode
    ;;   - min-width (integer/NSNumber): The minimum width of the generated barcode in pixels.
    ;;   - max-width (integer/NSNumber): The maximum width of the generated barcode in pixels.
    ;;   - min-height (integer/NSNumber): The minimum height of the generated barcode in pixels.
    ;;   - max-height (integer/NSNumber): The maximum height of the generated barcode in pixels.
    ;;   - data-columns (integer/NSNumber): The number of data columns in the generated barcode
    ;;   - rows (integer/NSNumber): The number of rows in the generated barcode
    ;;   - preferred-aspect-ratio (number/NSNumber): The preferred aspect ratio of the
    ;;     generated barcode
    ;;   - compaction-mode (integer/NSNumber): The compaction mode of the generated barcode.
    ;;   - compact-style (boolean/NSNumber): Force a compact style Aztec code to @YES or @NO.
    ;;     Set to nil for automatic.
    ;;   - correction-level (integer/NSNumber): The correction level ratio of the generated
    ;;     barcode
    ;;   - always-specify-compaction (boolean/NSNumber): Force compaction style to @YES or
    ;;     @NO. Set to nil for automatic.
    ;; Filter categories: still-image, builtin, generator, video
    (define (pdf417-barcode-generator message
                                      min-width
                                      max-width
                                      min-height
                                      max-height
                                      data-columns
                                      rows preferred-aspect-ratio
                                      compaction-mode compact-style
                                      correction-level
                                      always-specify-compaction)
      (image-filter-output
        (make-image-filter
          'pdf417-barcode-generator
          `((input-message . ,message)
            (input-min-width . ,min-width)
            (input-max-width . ,max-width)
            (input-min-height . ,min-height)
            (input-max-height . ,max-height)
            (input-data-columns . ,data-columns)
            (input-rows . ,rows)
            (input-preferred-aspect-ratio . ,preferred-aspect-ratio)
            (input-compaction-mode . ,compaction-mode)
            (input-compact-style . ,compact-style)
            (input-correction-level . ,correction-level)
            (input-always-specify-compaction . ,always-specify-compaction)))))

    ;; Returns an image processor for image filter person-segmentation (CIPersonSegmentation).
    ;; Returns a segmentation mask that is red in the portions of an image that are likely
    ;; to be persons. The returned image may have a different size and aspect ratio from
    ;; the input image.
    ;;   - quality-level (integer/NSNumber): Determines the size and quality of the resulting
    ;;     segmentation mask. The value can be a number where 0 is accurate, 1 is balanced,
    ;;     and 2 is fast.
    ;; Filter categories: still-image, builtin, stylize, video
    (define (person-segmentation quality-level)
      (make-filter-proc
        (make-image-filter
          'person-segmentation
          `((input-quality-level . ,quality-level)))))

    ;; Returns an image processor for image filter perspective-correction
    ;; (CIPerspectiveCorrection).
    ;; Apply a perspective correction to an image.
    ;;   - top-left (point/CIVector): The top left coordinate to be perspective corrected.
    ;;   - top-right (point/CIVector): The top right coordinate to be perspective corrected.
    ;;   - bottom-right (point/CIVector): The bottom right coordinate to be perspective corrected.
    ;;   - bottom-left (point/CIVector): The bottom left coordinate to be perspective corrected.
    ;;   - crop (boolean/NSNumber)
    ;; Filter categories: still-image, builtin, geometry-adjustment, high-dynamic-range, video
    (define (perspective-correction top-left top-right bottom-right bottom-left crop)
      (make-filter-proc
        (make-image-filter
          'perspective-correction
          `((input-top-left . ,top-left)
            (input-top-right . ,top-right)
            (input-bottom-right . ,bottom-right)
            (input-bottom-left . ,bottom-left)
            (input-crop . ,crop)))))

    ;; Returns an image processor for image filter perspective-rotate (CIPerspectiveRotate).
    ;; Apply a homogenous rotation transform to an image.
    ;;   - focal-length (scalar/NSNumber): 35mm equivalent focal length of the input image.
    ;;   - pitch (angle/NSNumber): Pitch angle in radians.
    ;;   - yaw (angle/NSNumber): Yaw angle in radians.
    ;;   - roll (angle/NSNumber): Roll angle in radians.
    ;; Filter categories: still-image, builtin, geometry-adjustment, high-dynamic-range, video
    (define (perspective-rotate focal-length pitch yaw roll)
      (make-filter-proc
        (make-image-filter
          'perspective-rotate
          `((input-focal-length . ,focal-length)
            (input-pitch . ,pitch)
            (input-yaw . ,yaw)
            (input-roll . ,roll)))))

    ;; Returns an image processor for image filter perspective-tile (CIPerspectiveTile).
    ;; Applies a perspective transform to an image and then tiles the result.
    ;;   - top-left (point/CIVector): The top left coordinate of a tile.
    ;;   - top-right (point/CIVector): The top right coordinate of a tile.
    ;;   - bottom-right (point/CIVector): The bottom right coordinate of a tile.
    ;;   - bottom-left (point/CIVector): The bottom left coordinate of a tile.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (perspective-tile top-left top-right bottom-right bottom-left)
      (make-filter-proc
        (make-image-filter
          'perspective-tile
          `((input-top-left . ,top-left)
            (input-top-right . ,top-right)
            (input-bottom-right . ,bottom-right)
            (input-bottom-left . ,bottom-left)))))

    ;; Returns an image processor for image filter perspective-transform (CIPerspectiveTransform).
    ;; Alters the geometry of an image to simulate the observer changing viewing position.
    ;; You can use the perspective filter to skew an image.
    ;;   - top-left (point/CIVector): The top left coordinate to map the image to.
    ;;   - top-right (point/CIVector): The top right coordinate to map the image to.
    ;;   - bottom-right (point/CIVector): The bottom right coordinate to map the image to.
    ;;   - bottom-left (point/CIVector): The bottom left coordinate to map the image to.
    ;; Filter categories: still-image, builtin, geometry-adjustment, high-dynamic-range, video
    (define (perspective-transform top-left top-right bottom-right bottom-left)
      (make-filter-proc
        (make-image-filter
          'perspective-transform
          `((input-top-left . ,top-left)
            (input-top-right . ,top-right)
            (input-bottom-right . ,bottom-right)
            (input-bottom-left . ,bottom-left)))))

    ;; Returns an image processor for image filter perspective-transform-with-extent
    ;; (CIPerspectiveTransformWithExtent).
    ;; Alters the geometry of an image to simulate the observer changing viewing position.
    ;; You can use the perspective filter to skew an image.
    ;;   - extent (rect/CIVector): A rectangle that defines the extent of the effect.
    ;;   - top-left (point/CIVector): The top left coordinate to map the image to.
    ;;   - top-right (point/CIVector): The top right coordinate to map the image to.
    ;;   - bottom-right (point/CIVector): The bottom right coordinate to map the image to.
    ;;   - bottom-left (point/CIVector): The bottom left coordinate to map the image to.
    ;; Filter categories: still-image, builtin, geometry-adjustment, high-dynamic-range, video
    (define (perspective-transform-with-extent extent top-left top-right bottom-right bottom-left)
      (make-filter-proc
        (make-image-filter
          'perspective-transform-with-extent
          `((input-extent . ,extent)
            (input-top-left . ,top-left)
            (input-top-right . ,top-right)
            (input-bottom-right . ,bottom-right)
            (input-bottom-left . ,bottom-left)))))

    ;; Returns an image processor for image filter photo-effect-chrome (CIPhotoEffectChrome).
    ;; Apply a “Chrome” style effect to an image.
    ;;   - extrapolate (boolean/NSNumber): If true, then the color effect will be extrapolated
    ;;     if the input image contains RGB component values outside the range 0.0 to 1.0.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (photo-effect-chrome extrapolate)
      (make-filter-proc
        (make-image-filter
          'photo-effect-chrome
          `((input-extrapolate . ,extrapolate)))))

    ;; Returns an image processor for image filter photo-effect-fade (CIPhotoEffectFade).
    ;; Apply a “Fade” style effect to an image.
    ;;   - extrapolate (boolean/NSNumber): If true, then the color effect will be extrapolated
    ;;     if the input image contains RGB component values outside the range 0.0 to 1.0.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (photo-effect-fade extrapolate)
      (make-filter-proc
        (make-image-filter
          'photo-effect-fade
          `((input-extrapolate . ,extrapolate)))))

    ;; Returns an image processor for image filter photo-effect-instant (CIPhotoEffectInstant).
    ;; Apply an “Instant” style effect to an image.
    ;;   - extrapolate (boolean/NSNumber): If true, then the color effect will be extrapolated
    ;;     if the input image contains RGB component values outside the range 0.0 to 1.0.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (photo-effect-instant extrapolate)
      (make-filter-proc
        (make-image-filter
          'photo-effect-instant
          `((input-extrapolate . ,extrapolate)))))

    ;; Returns an image processor for image filter photo-effect-mono (CIPhotoEffectMono).
    ;; Apply a “Mono” style effect to an image.
    ;;   - extrapolate (boolean/NSNumber): If true, then the color effect will be extrapolated
    ;;     if the input image contains RGB component values outside the range 0.0 to 1.0.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (photo-effect-mono extrapolate)
      (make-filter-proc
        (make-image-filter
          'photo-effect-mono
          `((input-extrapolate . ,extrapolate)))))

    ;; Returns an image processor for image filter photo-effect-noir (CIPhotoEffectNoir).
    ;; Apply a “Noir” style effect to an image.
    ;;   - extrapolate (boolean/NSNumber): If true, then the color effect will be
    ;;     extrapolated if the input image contains RGB component values outside the
    ;;     range 0.0 to 1.0.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (photo-effect-noir extrapolate)
      (make-filter-proc
        (make-image-filter
          'photo-effect-noir
          `((input-extrapolate . ,extrapolate)))))

    ;; Returns an image processor for image filter photo-effect-process (CIPhotoEffectProcess).
    ;; Apply a “Process” style effect to an image.
    ;;   - extrapolate (boolean/NSNumber): If true, then the color effect will be extrapolated
    ;;     if the input image contains RGB component values outside the range 0.0 to 1.0.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (photo-effect-process extrapolate)
      (make-filter-proc
        (make-image-filter
          'photo-effect-process
          `((input-extrapolate . ,extrapolate)))))

    ;; Returns an image processor for image filter photo-effect-tonal (CIPhotoEffectTonal).
    ;; Apply a “Tonal” style effect to an image.
    ;;   - extrapolate (boolean/NSNumber): If true, then the color effect will be
    ;;     extrapolated if the input image contains RGB component values outside the
    ;;     range 0.0 to 1.0.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (photo-effect-tonal extrapolate)
      (make-filter-proc
        (make-image-filter
          'photo-effect-tonal
          `((input-extrapolate . ,extrapolate)))))

    ;; Returns an image processor for image filter photo-effect-transfer (CIPhotoEffectTransfer).
    ;; Apply a “Transfer” style effect to an image.
    ;;   - extrapolate (boolean/NSNumber): If true, then the color effect will be extrapolated
    ;;     if the input image contains RGB component values outside the range 0.0 to 1.0.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (photo-effect-transfer extrapolate)
      (make-filter-proc
        (make-image-filter
          'photo-effect-transfer
          `((input-extrapolate . ,extrapolate)))))

    ;; Returns an image processor for image filter pinch-distortion (CIPinchDistortion).
    ;; Creates a rectangular-shaped area that pinches source pixels inward, distorting those
    ;; pixels closest to the rectangle the most.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - radius (distance/NSNumber): The radius determines how many pixels are used to
    ;;     create the distortion. The larger the radius, the wider the extent of the distortion.
    ;;   - scale (scalar/NSNumber): The amount of pinching. A value of 0.0 has no effect.
    ;;     A value of 1.0 is the maximum pinch.
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (pinch-distortion center radius scale)
      (make-filter-proc
        (make-image-filter
          'pinch-distortion
          `((input-center . ,center)
            (input-radius . ,radius)
            (input-scale . ,scale)))))

    ;; Returns an image processor for image filter pin-light-blend-mode (CIPinLightBlendMode).
    ;; Unpremultiplies the source and background image sample color, combines them according
    ;; to the relative difference, and then blends the result with the background according
    ;; to the PDF basic compositing formula. Source image values that are brighter than the
    ;; destination will produce an output that is lighter than the destination. Source image
    ;; values that are darker than the destination will produce an output that is darker
    ;; than the destination.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (pin-light-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'pin-light-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter pixellate (CIPixellate).
    ;; Makes an image blocky.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - scale (distance/NSNumber): The scale determines the size of the squares. Larger
    ;;     values result in larger squares.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (pixellate center scale)
      (make-filter-proc
        (make-image-filter
          'pixellate
          `((input-center . ,center)
            (input-scale . ,scale)))))

    ;; Returns an image processor for image filter pointillize (CIPointillize).
    ;; Renders the source image in a pointillistic style.
    ;;   - radius (distance/NSNumber): The radius of the circles in the resulting pattern.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (pointillize radius center)
      (make-filter-proc
        (make-image-filter
          'pointillize
          `((input-radius . ,radius)
            (input-center . ,center)))))

    ;; Returns an image processor for image filter qrcode-generator (CIQRCodeGenerator).
    ;; Generate a QR Code image for message data.
    ;;   - message (bytevector/NSData): The message to encode in the QR Code
    ;;   - correction-level (string/NSString): QR Code correction level L, M, Q, or H.
    ;; Filter categories: still-image, builtin, generator
    (define (qrcode-generator message correction-level)
      (image-filter-output
        (make-image-filter
          'qrcode-generator
          `((input-message . ,message)
            (input-correction-level . ,correction-level)))))

    ;; Returns an image processor for image filter radial-gradient (CIRadialGradient).
    ;; Generates a gradient that varies radially between two circles having the same center.
    ;; It is valid for one of the two circles to have a radius of 0.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - radius0 (distance/NSNumber): The radius of the starting circle to use in the gradient.
    ;;   - radius1 (distance/NSNumber): The radius of the ending circle to use in the gradient.
    ;;   - color0 (color/CIColor): The first color to use in the gradient.
    ;;   - color1 (color/CIColor): The second color to use in the gradient.
    ;; Filter categories: still-image, builtin, gradient, high-dynamic-range, video
    (define (radial-gradient center radius0 radius1 color0 color1)
      (image-filter-output
        (make-image-filter
          'radial-gradient
          `((input-center . ,center)
            (input-radius0 . ,radius0)
            (input-radius1 . ,radius1)
            (input-color0 . ,color0)
            (input-color1 . ,color1)))))

    ;; Returns an image processor for image filter random-generator (CIRandomGenerator).
    ;; Generates an image of infinite extent whose pixel values are made up of four
    ;; independent, uniformly-distributed random numbers in the 0 to 1 range.
    ;; Filter categories: still-image, builtin, generator, video
    (define (random-generator)
      (image-filter-output
        (make-image-filter
          'random-generator)))

    ;; Returns an image processor for image filter ripple-transition (CIRippleTransition).
    ;; Transitions from one image to another by creating a circular wave that expands from
    ;; the center point, revealing the new image in the wake of the wave.
    ;;   - target-image (abstract-image/CIImage): The target image for a transition.
    ;;   - shading-image (abstract-image/CIImage): An image that looks like a shaded sphere
    ;;     enclosed in a square image.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - extent (rect/CIVector): A rectangle that defines the extent of the effect.
    ;;   - time (time/NSNumber): The parametric time of the transition. This value drives
    ;;     the transition from start (at time 0) to end (at time 1).
    ;;   - width (distance/NSNumber): The width of the ripple.
    ;;   - scale (scalar/NSNumber): A value that determines whether the ripple starts as
    ;;     a bulge (higher value) or a dimple (lower value).
    ;; Filter categories: still-image, builtin, transition, high-dynamic-range, video
    (define (ripple-transition target-image shading-image center extent time width scale)
      (make-filter-proc
        (make-image-filter
          'ripple-transition
          `((input-target-image . ,target-image)
            (input-shading-image . ,shading-image)
            (input-center . ,center)
            (input-extent . ,extent)
            (input-time . ,time)
            (input-width . ,width)
            (input-scale . ,scale)))))

    ;; Returns an image processor for image filter rounded-rectangle-generator
    ;; (CIRoundedRectangleGenerator).
    ;; Generates a rounded rectangle image with the specified extent, corner radius, and color.
    ;;   - extent (rect/CIVector): A rectangle that defines the extent of the effect.
    ;;   - radius (distance/NSNumber): The distance from the center of the effect.
    ;;   - color (color/CIColor): A color.
    ;; Filter categories: still-image, builtin, generator, high-dynamic-range
    (define (rounded-rectangle-generator extent radius color)
      (image-filter-output
        (make-image-filter
          'rounded-rectangle-generator
          `((input-extent . ,extent)
            (input-radius . ,radius)
            (input-color . ,color)))))

    ;; Returns an image processor for image filter rounded-rectangle-stroke-generator
    ;; (CIRoundedRectangleStrokeGenerator).
    ;; Generates a rounded rectangle stroke image with the specified extent, corner radius,
    ;; stroke width, and color.
    ;;   - extent (rect/CIVector): A rectangle that defines the extent of the effect.
    ;;   - radius (distance/NSNumber): The distance from the center of the effect.
    ;;   - color (color/CIColor): A color.
    ;;   - width (distance/NSNumber): The width in pixels of the effect.
    ;; Filter categories: still-image, builtin, generator, high-dynamic-range
    (define (rounded-rectangle-stroke-generator extent radius color width)
      (image-filter-output
        (make-image-filter
          'rounded-rectangle-stroke-generator
          `((input-extent . ,extent)
            (input-radius . ,radius)
            (input-color . ,color)
            (input-width . ,width)))))

    ;; Returns an image processor for image filter row-average (CIRowAverage).
    ;; Calculates the average color for each row of the specified area in an image,
    ;; returning the result in a 1D image.
    ;;   - extent (rect/CIVector): A rectangle that specifies the subregion of the image
    ;;     that you want to process.
    ;; Filter categories: still-image, builtin, reduction, high-dynamic-range, video
    (define (row-average extent)
      (make-filter-proc
        (make-image-filter
          'row-average
          `((input-extent . ,extent)))))

    ;; Returns an image processor for image filter saliency-map-filter (CISaliencyMapFilter).
    ;; Generates output image as a saliency map of the input image.
    ;; Filter categories: still-image, builtin, stylize, video
    (define (saliency-map-filter)
      (make-filter-proc
        (make-image-filter
          'saliency-map-filter)))

    ;; Returns an image processor for image filter sample-nearest (CISampleNearest).
    ;; Produces an image that forces the image sampling to “nearest” mode instead of the
    ;; default “linear” mode. This filter can be used to alter the behavior of filters
    ;; that alter the geometry of an image. The output of this filter should be passed
    ;; as the input to the geometry filter. For example, passing the output of this
    ;; filter to CIAffineTransform can be used to produce a pixelated upsampled image.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (sample-nearest)
      (make-filter-proc
        (make-image-filter
          'sample-nearest)))

    ;; Returns an image processor for image filter saturation-blend-mode (CISaturationBlendMode).
    ;; Uses the luminance and hue values of the background with the saturation of the
    ;; source image. Areas of the background that have no saturation (that is, pure gray
    ;; areas) do not produce a change.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (saturation-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'saturation-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter screen-blend-mode (CIScreenBlendMode).
    ;; Multiplies the inverse of the source image samples with the inverse of the background
    ;; image samples. This results in colors that are at least as light as either of the two
    ;; contributing sample colors.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (screen-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'screen-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter sepia-tone (CISepiaTone).
    ;; Maps the colors of an image to various shades of brown.
    ;;   - intensity (scalar/NSNumber): The intensity of the sepia effect. A value of 1.0
    ;;     creates a monochrome sepia image. A value of 0.0 has no effect on the image.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect,
    ;;                    high-dynamic-range, video
    (define (sepia-tone intensity)
      (make-filter-proc
        (make-image-filter
          'sepia-tone
          `((input-intensity . ,intensity)))))

    ;; Returns an image processor for image filter shaded-material (CIShadedMaterial).
    ;; Produces a shaded image from a height field. The height field is defined to have
    ;; greater heights with lighter shades, and lesser heights (lower areas) with darker
    ;; shades. You can combine this filter with the “Height Field From Mask” filter to
    ;; produce quick shadings of masks, such as text.
    ;;   - shading-image (abstract-image/CIImage): The image to use as the height field.
    ;;     The resulting image has greater heights with lighter shades, and lesser heights
    ;;     (lower areas) with darker shades.
    ;;   - scale (distance/NSNumber): The scale of the effect. The higher the value, the
    ;;     more dramatic the effect.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (shaded-material shading-image scale)
      (make-filter-proc
        (make-image-filter
          'shaded-material
          `((input-shading-image . ,shading-image)
            (input-scale . ,scale)))))

    ;; Returns an image processor for image filter sharpen-luminance (CISharpenLuminance).
    ;; Increases image detail by sharpening. It operates on the luminance of the image; the
    ;; chrominance of the pixels remains unaffected.
    ;;   - sharpness (scalar/NSNumber): The amount of sharpening to apply. Larger values
    ;;     are sharper.
    ;;   - radius (scalar/NSNumber): The distance from the center of the effect.
    ;; Filter categories: still-image, builtin, sharpen, high-dynamic-range, video
    (define (sharpen-luminance sharpness radius)
      (make-filter-proc
        (make-image-filter
          'sharpen-luminance
          `((input-sharpness . ,sharpness)
            (input-radius . ,radius)))))

    ;; Returns an image processor for image filter sixfold-reflected-tile
    ;; (CISixfoldReflectedTile).
    ;; Produces a tiled image from a source image by applying a 6-way reflected symmetry.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - angle (angle/NSNumber): The angle in radians of the tiled pattern.
    ;;   - width (distance/NSNumber): The width of a tile.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (sixfold-reflected-tile center angle width)
      (make-filter-proc
        (make-image-filter
          'sixfold-reflected-tile
          `((input-center . ,center)
            (input-angle . ,angle)
            (input-width . ,width)))))

    ;; Returns an image processor for image filter sixfold-rotated-tile (CISixfoldRotatedTile).
    ;; Produces a tiled image from a source image by rotating the source at increments of
    ;; 60 degrees.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - angle (angle/NSNumber): The angle in radians of the tiled pattern.
    ;;   - width (distance/NSNumber): The width of a tile.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (sixfold-rotated-tile center angle width)
      (make-filter-proc
        (make-image-filter
          'sixfold-rotated-tile
          `((input-center . ,center)
            (input-angle . ,angle)
            (input-width . ,width)))))

    ;; Returns an image processor for image filter smooth-linear-gradient
    ;; (CISmoothLinearGradient).
    ;; Generates a gradient that varies along a linear axis between two defined endpoints.
    ;;   - point0 (point/CIVector): The starting position of the gradient -- where the first
    ;;     color begins.
    ;;   - point1 (point/CIVector): The ending position of the gradient -- where the second
    ;;     color begins.
    ;;   - color0 (color/CIColor): The first color to use in the gradient.
    ;;   - color1 (color/CIColor): The second color to use in the gradient.
    ;; Filter categories: still-image, builtin, gradient, high-dynamic-range, video
    (define (smooth-linear-gradient point0 point1 color0 color1)
      (image-filter-output
        (make-image-filter
          'smooth-linear-gradient
          `((input-point0 . ,point0)
            (input-point1 . ,point1)
            (input-color0 . ,color0)
            (input-color1 . ,color1)))))

    ;; Returns an image processor for image filter sobel-gradients (CISobelGradients).
    ;; Applies multichannel 3 by 3 Sobel gradient filter to an image. The resulting image
    ;; has maximum horizontal gradient in the red channel and the maximum vertical gradient
    ;; in the green channel. The gradient values can be positive or negative.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (sobel-gradients)
      (make-filter-proc
        (make-image-filter
          'sobel-gradients)))

    ;; Returns an image processor for image filter soft-light-blend-mode (CISoftLightBlendMode).
    ;; Either darkens or lightens colors, depending on the source image sample color. If
    ;; the source image sample color is lighter than 50% gray, the background is lightened,
    ;; similar to dodging. If the source image sample color is darker than 50% gray, the
    ;; background is darkened, similar to burning. If the source image sample color is
    ;; equal to 50% gray, the background is not changed. Image samples that are equal to
    ;; pure black or pure white produce darker or lighter areas, but do not result in pure
    ;; black or white. The overall effect is similar to what you would achieve by shining
    ;; a diffuse spotlight on the source image.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (soft-light-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'soft-light-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter source-atop-compositing
    ;; (CISourceAtopCompositing).
    ;; Places the source image over the background image, then uses the luminance of the
    ;; background image to determine what to show. The composite shows the background image
    ;; and only those portions of the source image that are over visible parts of the background.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, high-dynamic-range, video
    (define (source-atop-compositing background-image)
      (make-filter-proc
        (make-image-filter
          'source-atop-compositing
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter source-in-compositing (CISourceInCompositing).
    ;; Uses the second image to define what to leave in the source image, effectively
    ;; cropping the image.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, high-dynamic-range, video
    (define (source-in-compositing background-image)
      (make-filter-proc
        (make-image-filter
          'source-in-compositing
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter source-out-compositing (CISourceOutCompositing).
    ;; Uses the second image to define what to take out of the first image.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, high-dynamic-range, video
    (define (source-out-compositing background-image)
      (make-filter-proc
        (make-image-filter
          'source-out-compositing
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter source-over-compositing (CISourceOverCompositing).
    ;; Places the second image over the first.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, high-dynamic-range, video
    (define (source-over-compositing background-image)
      (make-filter-proc
        (make-image-filter
          'source-over-compositing
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter spot-color (CISpotColor).
    ;; Replaces one or more color ranges with spot colors.
    ;;   - center-color1 (color/CIColor): The center value of the first color range to replace.
    ;;   - replacement-color1 (color/CIColor): A replacement color for the first color range.
    ;;   - closeness1 (scalar/NSNumber): A value that indicates how close the first color
    ;;     must match before it is replaced.
    ;;   - contrast1 (scalar/NSNumber): The contrast of the first replacement color.
    ;;   - center-color2 (color/CIColor): The center value of the second color range to replace.
    ;;   - replacement-color2 (color/CIColor): A replacement color for the second color range.
    ;;   - closeness2 (scalar/NSNumber): A value that indicates how close the second color
    ;;     must match before it is replaced.
    ;;   - contrast2 (scalar/NSNumber): The contrast of the second replacement color.
    ;;   - center-color3 (color/CIColor): The center value of the third color range to replace.
    ;;   - replacement-color3 (color/CIColor): A replacement color for the third color range.
    ;;   - closeness3 (scalar/NSNumber): A value that indicates how close the third color
    ;;     must match before it is replaced.
    ;;   - contrast3 (scalar/NSNumber): The contrast of the third replacement color.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (spot-color center-color1
                        replacement-color1
                        closeness1
                        contrast1
                        center-color2
                        replacement-color2
                        closeness2 contrast2
                        center-color3
                        replacement-color3
                        closeness3
                        contrast3)
      (make-filter-proc
        (make-image-filter
          'spot-color
          `((input-center-color1 . ,center-color1)
            (input-replacement-color1 . ,replacement-color1)
            (input-closeness1 . ,closeness1)
            (input-contrast1 . ,contrast1)
            (input-center-color2 . ,center-color2)
            (input-replacement-color2 . ,replacement-color2)
            (input-closeness2 . ,closeness2)
            (input-contrast2 . ,contrast2)
            (input-center-color3 . ,center-color3)
            (input-replacement-color3 . ,replacement-color3)
            (input-closeness3 . ,closeness3)
            (input-contrast3 . ,contrast3)))))

    ;; Returns an image processor for image filter spot-light (CISpotLight).
    ;; Applies a directional spotlight effect to an image.
    ;;   - light-position (coordinate-3d/CIVector): The x and y position of the spotlight.
    ;;   - light-points-at (coordinate-3d/CIVector): The x and y position that the
    ;;     spotlight points at.
    ;;   - brightness (distance/NSNumber): The brightness of the spotlight.
    ;;   - concentration (scalar/NSNumber): The spotlight size. The smaller the value,
    ;;     the more tightly focused the light beam.
    ;;   - color (opaque-color/CIColor): The color of the spotlight.
    ;; Filter categories: still-image, builtin, stylize, high-dynamic-range, video
    (define (spot-light light-position light-points-at brightness concentration color)
      (make-filter-proc
        (make-image-filter
          'spot-light
          `((input-light-position . ,light-position)
            (input-light-points-at . ,light-points-at)
            (input-brightness . ,brightness)
            (input-concentration . ,concentration)
            (input-color . ,color)))))

    ;; Returns an image processor for image filter srgbtone-curve-to-linear
    ;; (CISRGBToneCurveToLinear).
    ;; Converts an image in sRGB space to linear space.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    high-dynamic-range, color-adjustment, video
    (define (srgbtone-curve-to-linear)
      (make-filter-proc
        (make-image-filter
          'srgbtone-curve-to-linear)))

    ;; Returns an image processor for image filter star-shine-generator (CIStarShineGenerator).
    ;; Generates a starburst pattern. The output image is typically used as input to
    ;; another filter.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - color (color/CIColor): The color to use for the outer shell of the circular star.
    ;;   - radius (distance/NSNumber): The radius of the star.
    ;;   - cross-scale (scalar/NSNumber): The size of the cross pattern.
    ;;   - cross-angle (angle/NSNumber): The angle in radians of the cross pattern.
    ;;   - cross-opacity (scalar/NSNumber): The opacity of the cross pattern.
    ;;   - cross-width (distance/NSNumber): The width of the cross pattern.
    ;;   - epsilon (scalar/NSNumber): The length of the cross spikes.
    ;; Filter categories: still-image, builtin, generator, high-dynamic-range, video
    (define (star-shine-generator center
                                  color
                                  radius
                                  cross-scale
                                  cross-angle
                                  cross-opacity
                                  cross-width
                                  epsilon)
      (image-filter-output
        (make-image-filter
          'star-shine-generator
          `((input-center . ,center)
            (input-color . ,color)
            (input-radius . ,radius)
            (input-cross-scale . ,cross-scale)
            (input-cross-angle . ,cross-angle)
            (input-cross-opacity . ,cross-opacity)
            (input-cross-width . ,cross-width)
            (input-epsilon . ,epsilon)))))

    ;; Returns an image processor for image filter straighten-filter (CIStraightenFilter).
    ;; Rotates a source image by the specified angle in radians. The image is then scaled
    ;; and cropped so that the rotated image fits the extent of the input image.
    ;;   - angle (angle/NSNumber): The angle in radians of the effect.
    ;; Filter categories: still-image, builtin, geometry-adjustment, high-dynamic-range, video
    (define (straighten-filter angle)
      (make-filter-proc
        (make-image-filter
          'straighten-filter
          `((input-angle . ,angle)))))

    ;; Returns an image processor for image filter stretch-crop (CIStretchCrop).
    ;; Distorts an image by stretching and or cropping to fit a target size.
    ;;   - size (point/CIVector): The size in pixels of the output image.
    ;;   - crop-amount (scalar/NSNumber): Determines if and how much cropping should be
    ;;     used to achieve the target size. If value is 0 then only stretching is used.
    ;;     If 1 then only cropping is used.
    ;;   - center-stretch-amount (scalar/NSNumber): Determine how much the center of the
    ;;     image is stretched if stretching is used. If value is 0 then the center of the
    ;;     image maintains the original aspect ratio. If 1 then the image is stretched uniformly.
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (stretch-crop size crop-amount center-stretch-amount)
      (make-filter-proc
        (make-image-filter
          'stretch-crop
          `((input-size . ,size)
            (input-crop-amount . ,crop-amount)
            (input-center-stretch-amount . ,center-stretch-amount)))))

    ;; Returns an image processor for image filter stripes-generator (CIStripesGenerator).
    ;; Generates a stripe pattern. You can control the color of the stripes, the spacing,
    ;; and the contrast.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - color0 (color/CIColor): A color to use for the odd stripes.
    ;;   - color1 (color/CIColor): A color to use for the even stripes.
    ;;   - width (distance/NSNumber): The width of a stripe.
    ;;   - sharpness (scalar/NSNumber): The sharpness of the stripe pattern. The smaller
    ;;     the value, the more blurry the pattern. Values range from 0.0 to 1.0.
    ;; Filter categories: still-image, builtin, generator, high-dynamic-range, video
    (define (stripes-generator center color0 color1 width sharpness)
      (image-filter-output
        (make-image-filter
          'stripes-generator
          `((input-center . ,center)
            (input-color0 . ,color0)
            (input-color1 . ,color1)
            (input-width . ,width)
            (input-sharpness . ,sharpness)))))

    ;; Returns an image processor for image filter subtract-blend-mode (CISubtractBlendMode).
    ;; Unpremultiplies the source and background image sample colors, subtracts the source
    ;; from the background, and then blends the result with the background according to
    ;; the PDF basic compositing formula. Source image values that are black produces
    ;; output that is the same as the background. Source image values that are non-black
    ;; darken the background color values.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (subtract-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'subtract-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter sunbeams-generator (CISunbeamsGenerator).
    ;; Generates a sun effect. You typically use the output of the sunbeams filter as input
    ;; to a composite filter.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - color (color/CIColor): The color of the sun.
    ;;   - sun-radius (distance/NSNumber): The radius of the sun.
    ;;   - max-striation-radius (scalar/NSNumber): The radius of the sunbeams.
    ;;   - striation-strength (scalar/NSNumber): The intensity of the sunbeams. Higher
    ;;     values result in more intensity.
    ;;   - striation-contrast (scalar/NSNumber): The contrast of the sunbeams. Higher
    ;;     values result in more contrast.
    ;;   - time (scalar/NSNumber): The duration of the effect.
    ;; Filter categories: still-image, builtin, generator, high-dynamic-range, video
    (define (sunbeams-generator center
                                color
                                sun-radius
                                max-striation-radius
                                striation-strength
                                striation-contrast
                                time)
      (image-filter-output
        (make-image-filter
          'sunbeams-generator
          `((input-center . ,center)
            (input-color . ,color)
            (input-sun-radius . ,sun-radius)
            (input-max-striation-radius . ,max-striation-radius)
            (input-striation-strength . ,striation-strength)
            (input-striation-contrast . ,striation-contrast)
            (input-time . ,time)))))

    ;; Returns an image processor for image filter swipe-transition (CISwipeTransition).
    ;; Transitions from one image to another by simulating a swiping action.
    ;;   - target-image (abstract-image/CIImage): The target image for a transition.
    ;;   - extent (rect/CIVector): The extent of the effect.
    ;;   - color (opaque-color/CIColor): The color of the swipe.
    ;;   - time (time/NSNumber): The parametric time of the transition. This value
    ;;     drives the transition from start (at time 0) to end (at time 1).
    ;;   - angle (angle/NSNumber): The angle in radians of the swipe.
    ;;   - width (distance/NSNumber): The width of the swipe.
    ;;   - opacity (scalar/NSNumber): The opacity of the swipe.
    ;; Filter categories: still-image, builtin, transition, high-dynamic-range, video
    (define (swipe-transition target-image extent color time angle width opacity)
      (make-filter-proc
        (make-image-filter
          'swipe-transition
          `((input-target-image . ,target-image)
            (input-extent . ,extent)
            (input-color . ,color)
            (input-time . ,time)
            (input-angle . ,angle)
            (input-width . ,width)
            (input-opacity . ,opacity)))))

    ;; Returns an image processor for image filter temperature-and-tint (CITemperatureAndTint).
    ;; Adapt the reference white point for an image.
    ;;   - neutral (offset/CIVector): A vector containing the source white point defined by
    ;;     color temperature and tint or chromaticity (x,y).
    ;;   - target-neutral (offset/CIVector): A vector containing the desired white point
    ;;     defined by color temperature and tint or chromaticity (x,y).
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    high-dynamic-range, color-adjustment, video
    (define (temperature-and-tint neutral target-neutral)
      (make-filter-proc
        (make-image-filter
          'temperature-and-tint
          `((input-neutral . ,neutral)
            (input-target-neutral . ,target-neutral)))))

    ;; Returns an image processor for image filter text-image-generator (CITextImageGenerator).
    ;; Generate an image from a string and font information.
    ;;   - text (string/NSString): The text to render.
    ;;   - font-name (string/NSString): The name of the font to use for the generated text.
    ;;   - font-size (scalar/NSNumber): The size of the font to use for the generated text.
    ;;   - scale-factor (scalar/NSNumber): The scale of the font to use for the generated text.
    ;;   - padding (integer/NSNumber): The number of additional pixels to pad around the
    ;;     text’s bounding box.
    ;; Filter categories: still-image, builtin, generator, video
    (define (text-image-generator text font-name font-size scale-factor padding)
      (image-filter-output
        (make-image-filter
          'text-image-generator
          `((input-text . ,text)
            (input-font-name . ,font-name)
            (input-font-size . ,font-size)
            (input-scale-factor . ,scale-factor)
            (input-padding . ,padding)))))

    ;; Returns an image processor for image filter thermal (CIThermal).
    ;; Apply a “Thermal” style effect to an image.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect, video
    (define (thermal)
      (make-filter-proc
        (make-image-filter
          'thermal)))

    ;; Returns an image processor for image filter tone-curve (CIToneCurve).
    ;; Adjusts tone response of the R, G, and B channels of an image. The input points are
    ;; five x,y values that are interpolated using a spline curve. The curve is applied in
    ;; a perceptual (gamma 2) version of the working space.
    ;;   - point0 (offset/CIVector)
    ;;   - point1 (offset/CIVector)
    ;;   - point2 (offset/CIVector)
    ;;   - point3 (offset/CIVector)
    ;;   - point4 (offset/CIVector)
    ;;   - extrapolate (boolean/NSNumber): If true, then the color effect will be extrapolated
    ;;     if the input image contains RGB component values outside the range 0.0 to 1.0.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    high-dynamic-range, color-adjustment, video
    (define (tone-curve point0 point1 point2 point3 point4 extrapolate)
      (make-filter-proc
        (make-image-filter
          'tone-curve
          `((input-point0 . ,point0)
            (input-point1 . ,point1)
            (input-point2 . ,point2)
            (input-point3 . ,point3)
            (input-point4 . ,point4)
            (input-extrapolate . ,extrapolate)))))

    ;; Returns an image processor for image filter tone-map-headroom (CIToneMapHeadroom).
    ;; Apply a global tone curve to an image that reduces colors from a source headroom
    ;; value to a target headroom value.
    ;;   - source-headroom (scalar/NSNumber): Specifies the headroom of the input image.
    ;;   - target-headroom (scalar/NSNumber): Specifies the target headroom of the output image.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    high-dynamic-range, color-adjustment, video
    (define (tone-map-headroom source-headroom target-headroom)
      (make-filter-proc
        (make-image-filter
          'tone-map-headroom
          `((input-source-headroom . ,source-headroom)
            (input-target-headroom . ,target-headroom)))))

    ;; Returns an image processor for image filter torus-lens-distortion (CITorusLensDistortion).
    ;; Creates a torus-shaped lens and distorts the portion of the image over which the
    ;; lens is placed.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - radius (distance/NSNumber): The outer radius of the torus.
    ;;   - width (distance/NSNumber): The width of the ring.
    ;;   - refraction (scalar/NSNumber): The refraction of the glass.
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (torus-lens-distortion center radius width refraction)
      (make-filter-proc
        (make-image-filter
          'torus-lens-distortion
          `((input-center . ,center)
            (input-radius . ,radius)
            (input-width . ,width)
            (input-refraction . ,refraction)))))

    ;; Returns an image processor for image filter triangle-kaleidoscope (CITriangleKaleidoscope).
    ;; Maps a triangular portion of image to a triangular area and then generates a
    ;; kaleidoscope effect.
    ;;   - point (point/CIVector): The x and y position to use as the center of the
    ;;     triangular area in the input image.
    ;;   - size (scalar/NSNumber): The size in pixels of the triangle.
    ;;   - rotation (angle/NSNumber): Rotation angle in radians of the triangle.
    ;;   - decay (scalar/NSNumber): The decay determines how fast the color fades from the
    ;;     center triangle.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (triangle-kaleidoscope point size rotation decay)
      (make-filter-proc
        (make-image-filter
          'triangle-kaleidoscope
          `((input-point . ,point)
            (input-size . ,size)
            (input-rotation . ,rotation)
            (input-decay . ,decay)))))

    ;; Returns an image processor for image filter triangle-tile (CITriangleTile).
    ;; Maps a triangular portion of image to a triangular area and then tiles the result.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - angle (angle/NSNumber): The angle in radians of the tiled pattern.
    ;;   - width (distance/NSNumber): The width of a tile.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (triangle-tile center angle width)
      (make-filter-proc
        (make-image-filter
          'triangle-tile
          `((input-center . ,center)
            (input-angle . ,angle)
            (input-width . ,width)))))

    ;; Returns an image processor for image filter twelvefold-reflected-tile
    ;; (CITwelvefoldReflectedTile).
    ;; Produces a tiled image from a source image by applying a 12-way reflected symmetry.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - angle (angle/NSNumber): The angle in radians of the tiled pattern.
    ;;   - width (distance/NSNumber): The width of a tile.
    ;; Filter categories: still-image, builtin, tile-effect, high-dynamic-range, video
    (define (twelvefold-reflected-tile center angle width)
      (make-filter-proc
        (make-image-filter
          'twelvefold-reflected-tile
          `((input-center . ,center)
            (input-angle . ,angle)
            (input-width . ,width)))))

    ;; Returns an image processor for image filter twirl-distortion (CITwirlDistortion).
    ;; Rotates pixels around a point to give a twirling effect. You can specify the number
    ;; of rotations as well as the center and radius of the effect.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - radius (distance/NSNumber): The radius determines how many pixels are used to
    ;;     create the distortion. The larger the radius, the wider the extent of the distortion.
    ;;   - angle (angle/NSNumber): The angle in radians of the twirl. Values can be
    ;;     positive or negative.
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (twirl-distortion center radius angle)
      (make-filter-proc
        (make-image-filter
          'twirl-distortion
          `((input-center . ,center)
            (input-radius . ,radius)
            (input-angle . ,angle)))))

    ;; Returns an image processor for image filter unsharp-mask (CIUnsharpMask).
    ;; Increases the contrast of the edges between pixels of different colors in an image.
    ;;   - radius (distance/NSNumber): The radius around a given pixel to apply the unsharp
    ;;     mask. The larger the radius, the more of the image is affected.
    ;;   - intensity (scalar/NSNumber): The intensity of the effect. The larger the value,
    ;;     the more contrast in the affected area.
    ;; Filter categories: still-image, builtin, sharpen, high-dynamic-range, video
    (define (unsharp-mask radius intensity)
      (make-filter-proc
        (make-image-filter
          'unsharp-mask
          `((input-radius . ,radius)
            (input-intensity . ,intensity)))))

    ;; Returns an image processor for image filter vibrance (CIVibrance).
    ;; Adjusts the saturation of an image while keeping pleasing skin tones.
    ;;   - amount (scalar/NSNumber): The amount to adjust the saturation.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    high-dynamic-range, color-adjustment, video
    (define (vibrance amount)
      (make-filter-proc
        (make-image-filter
          'vibrance
          `((input-amount . ,amount)))))

    ;; Returns an image processor for image filter vignette (CIVignette).
    ;; Applies a vignette shading to the corners of an image.
    ;;   - intensity (scalar/NSNumber): The intensity of the effect.
    ;;   - radius (scalar/NSNumber): The distance from the center of the effect.
    ;; Filter categories: still-image, builtin, interlaced, color-effect, high-dynamic-range, video
    (define (vignette intensity radius)
      (make-filter-proc
        (make-image-filter
          'vignette
          `((input-intensity . ,intensity)
            (input-radius . ,radius)))))

    ;; Returns an image processor for image filter vignette-effect (CIVignetteEffect).
    ;; Applies a vignette shading to the corners of an image.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - radius (distance/NSNumber): The distance from the center of the effect.
    ;;   - intensity (scalar/NSNumber): The intensity of the effect.
    ;;   - falloff (scalar/NSNumber): The falloff of the effect.
    ;; Filter categories: still-image, builtin, interlaced, color-effect, high-dynamic-range, video
    (define (vignette-effect center radius intensity falloff)
      (make-filter-proc
        (make-image-filter
          'vignette-effect
          `((input-center . ,center)
            (input-radius . ,radius)
            (input-intensity . ,intensity)
            (input-falloff . ,falloff)))))

    ;; Returns an image processor for image filter vivid-light-blend-mode (CIVividLightBlendMode).
    ;; A blend mode that is a combination of color burn and color dodge blend modes.
    ;;   - background-image (abstract-image/CIImage): The image to use as a background image.
    ;; Filter categories: still-image, builtin, interlaced, composite-operation,
    ;;                    non-square-pixels, video
    (define (vivid-light-blend-mode background-image)
      (make-filter-proc
        (make-image-filter
          'vivid-light-blend-mode
          `((input-background-image . ,background-image)))))

    ;; Returns an image processor for image filter vortex-distortion (CIVortexDistortion).
    ;; Rotates pixels around a point to simulate a vortex. You can specify the number of
    ;; rotations as well the center and radius of the effect. 
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - radius (distance/NSNumber): The radius determines how many pixels are used to
    ;;     create the distortion. The larger the radius, the wider the extent of the distortion.
    ;;   - angle (angle/NSNumber): The angle in radians of the effect.
    ;; Filter categories: still-image, builtin, distortion-effect, high-dynamic-range, video
    (define (vortex-distortion center radius angle)
      (make-filter-proc
        (make-image-filter
          'vortex-distortion
          `((input-center . ,center)
            (input-radius . ,radius)
            (input-angle . ,angle)))))

    ;; Returns an image processor for image filter white-point-adjust (CIWhitePointAdjust).
    ;; Adjusts the reference white point for an image and maps all colors in the source
    ;; using the new reference.
    ;;   - color (color/CIColor): A color to use as the white point.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels,
    ;;                    high-dynamic-range, color-adjustment, video
    (define (white-point-adjust color)
      (make-filter-proc
        (make-image-filter
          'white-point-adjust
          `((input-color . ,color)))))

    ;; Returns an image processor for image filter xray (CIXRay).
    ;; Apply an “XRay” style effect to an image.
    ;; Filter categories: still-image, builtin, interlaced, non-square-pixels, color-effect, video
    (define (xray)
      (make-filter-proc
        (make-image-filter
          'xray)))

    ;; Returns an image processor for image filter zoom-blur (CIZoomBlur).
    ;; Simulates the effect of zooming the camera while capturing the image.
    ;;   - center (point/CIVector): The center of the effect as x and y pixel coordinates.
    ;;   - amount (distance/NSNumber): The zoom-in amount. Larger values result in more zooming in.
    ;; Filter categories: still-image, builtin, blur, high-dynamic-range, video
    (define (zoom-blur center amount)
      (make-filter-proc
        (make-image-filter
          'zoom-blur
          `((input-center . ,center)
            (input-amount . ,amount)))))
  )
)
