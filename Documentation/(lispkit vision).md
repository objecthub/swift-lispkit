# LispKit Vision

Library `(lispkit vision)` provides computer vision capabilities through Apple's _Vision_ framework. The library supports optical character recognition (OCR), shape detection, barcode recognition, and image classification. All vision operations return Future objects that execute asynchronously, as defined by library `(lispkit thread future)`.


## Rectangle Detection

**(detect-rectangles _image_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(detect-rectangles _image area_)**  
**(detect-rectangles _image area num_)**  
**(detect-rectangles _image area num aratio_)**  
**(detect-rectangles _image area num aratio tolerance_)**  
**(detect-rectangles _image area num aratio tolerance msize_)**  
**(detect-rectangles _image area num aratio tolerance msize mconf_)**  

Detects rectangular shapes in bitmap _image_ and returns a future which eventually refers to a list of detected rectangles with the results having the form `(((x . y) . (width . height)) confidence (top-left top-right bottom-right bottom-left))` where `((x . y) . (width . height))` defines a bounding box, `confidence` a confidence level ranging from 0.0 to 1.0, where 0.0 represents no confidence and 1.0 represents full confidence, and `top-left`, `top-right`, `bottom-right` and `bottom-left` all are points in normalized coordinates relative to the image’s lower-left corner.

_area_ is a region of interest provided as a rectangular of form `((x . y) . (width . height))` in normalized coordinates relative to the image’s lower-left corner; default is `((0 . 0) . (1 . 1))` (= `#f`). _num_ is the maximum number of rectangular shapes to detect; default is `#f` which will not constrain the number of results.

_aratio_ is a ratio range expressed as a pair of two flonum values: `(min-ratio . max-ratio)` specifying the minimum and maximum aspect ratio of the rectangles,  defined as the shorter dimension over the longer dimension (i.e. both ratios are in the range of [0; 1]); default is `(0.3 . 1.0)`. _tolerance_ specifies the number of degrees a rectangle corner angle can deviate from 90°. It should range from 0 to 45, inclusive; default is 20. _msize_ defines the minimum size of a rectangle to detect, as a proportion of the smallest dimension from the image size; default is 0.2. _mconf_ is the minimum acceptable confidence level ranging from 0.0 to 1.0, where 0.0 represents no confidence and 1.0 represents full confidence; default is 0.0.


## Text Recognition

**(recognize-text _image_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(recognize-text _image area_)**  
**(recognize-text _image area num_)**  
**(recognize-text _image area num lang_)**  
**(recognize-text _image area num lang words_)**  
**(recognize-text _image area num lang words mheight_)**  

Performs optical character recognition on bitmap _image_ and returns a future containing a list of detected text observations. Each observation includes bounding box information and recognized text candidates. _area_ is a region of interest provided as a rectangular of form `((x . y) . (width . height))` in normalized coordinates relative to the image’s lower-left corner; default is `((0 . 0) . (1 . 1))` (= `#f`). _num_ is the maximum number of candidates determined by `recognize-text` for each text observation; default is 1, maximum is 10.

_lang_ is a list of ISO 639 language codes represented as strings which define the recognized languages. If _lang_ is set to `#t`, automatic language detection is enabled. If _lang_ is set to `#f`, automatic language detection is disabled. _words_ is a list of strings defining words supplementing the recognized languages at the word-recognition stage. _mheight_ is the minimum text height, relative to the image height, of the text to recognize. For example, to limit recognition to text that’s half of the image height, use 0.5. Increasing the size reduces memory consumption and expedites recognition with the tradeoff of ignoring text smaller than the minimum height; default is 1/32 (= 0.03125, = `#f`).

The result of `recognize-text` is a future eventually referring to a list of text observations. Each text observation consists of a bounding box and a list of recognized text candidates. Each text observation has the following format: `(((x . y) . (width . height)) text-candidate ...)` where `text-candidate` refers to objects for which `recognized-text?` returns `#t`. Each recognized text object has a confidence, a string, text corners, and a bounding box.

**(recognized-text? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a recognized text object; `#f` otherwise.

**(recognized-text-confidence _rtext_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the confidence score from the range 0.0-1.0 for a recognized text object _rtext_, where 1.0 indicates highest confidence.

**(recognized-text-string _rtext_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Extracts the actual text string from a recognized text object _rtext_.

**(recognized-text-corners _rtext_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(recognized-text-corners _rtext start_)**  
**(recognized-text-corners _rtext start end_)**  

Returns the four corner points of the text's bounding quadrilateral as: `((top-left-x . top-left-y) (top-right-x . top-right-y) (bottom-right-x . bottom-right-y) (bottom-left-x . bottom-left-y))`. Optional _start_ and _end_ parameters specify a character range for which the text corners are being returned.

**(recognized-text-bounds _rtext_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(recognized-text-bounds _rtext start_)**  
**(recognized-text-bounds _rtext start end_)**  

Returns the axis-aligned bounding box as `((x . y) . (width . height))`. Optional _start_ and _end_ parameters specify a character range for which the bounding box is being returned.


## Barcode Recognition

**(supported-barcode-symbologies)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of supported barcode symbology identifiers each refering to a type of barcode. Supported symbology identifiers include: `aztec`, `codabar`, `code39`, `code39-checksum`, `code39-full-ascii`, `code39-full-ascii-checksum`, `code93`, `code93i`, `code128`, `data-matrix`, `ean8`, `ean13`, `gs1-databar`, `gs1-databar-expanded`, `gs1-databar-limited`, `i2of5`, `i2of5-checksum`, `itf14`, `micro-pdf417`, `micro-qr`, `qr`, `pdf417`, `upce`, `msi-plessey`.

**(recognize-barcodes _image_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>
**(recognize-barcodes _image area_)**
**(recognize-barcodes _image area symbologies_)**

Recognizes barcodes in bitmap _image_ and returns a future eventually refering to a list of detected barcodes, each with corresponding metadata. Each result has the form `(((x . y) . (width . height)) confidence  (top-left top-right bottom-right bottom-left) symbology payload-string)` where `((x . y) . (width . height))` defines a bounding box, `confidence` defines a confidence level ranging from 0.0 to 1.0, where 0.0 represents no confidence and 1.0 represents full confidence, and `top-left`, `top-right`, `bottom-right` and `bottom-left` all are points in normalized coordinates relative to the image’s lower-left corner. `symbology` is a symbology identifier (a symbol) and `payload-string` a decoded payload string.

_area_ is a region of interest provided as a rectangular of form `((x . y) . (width . height))` in normalized coordinates relative to the image’s lower-left corner; default is `((0 . 0) . (1 . 1))` (= `#f`). _symbologies_ is a list of barcode symbology identifiers (symbols) specifying which types of barcodes to detect; default is `(aztec code128 data-matrix ean8 ean13 gs1-data-bar qr pdf417)`.


## Image Classification

**(supported-classification-identifiers)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>

Returns a list of all supported image classification identifiers. Each identifier is a string representing objects, scenes, and concepts that the vision system can recognize.

```scheme
(supported-classification-identifiers)
⇒  ("abacus" "accordion" "acorn" "acrobat" "adult" "adult_cat" "agriculture" "aircraft" "airplane" "airport" "airshow" "alley" "alligator_crocodile" "almond" "ambulance" "amusement_park" "anchovy" "angelfish" "animal" "ant" "antipasti" "anvil" "apartment" "apple" "appliance" "apricot" "apron" "aquarium" "arachnid" "arch" "archery" "arena" "armchair" "art" ...)
```

**(classify-image _image_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(classify-image _image area_)**  
**(classify-image _image area mconf_)**  

Classifies the content of bitmap _image_ and returns a future eventually referring to recognized objects and scenes each with confidence score. Results have the form `(confidence identifier)` where `confidence` defines a confidence level ranging from 0.0 to 1.0, where 0.0 represents no confidence and 1.0 represents full confidence, and `identifier` is a string representing a classification identifier. A list of all supported classifiers can be obtained by invoking `(supported-classification-identifiers)`.
Results are sorted by confidence in descending order.

_area_ is a region of interest provided as a rectangular of form `((x . y) . (width . height))` in normalized coordinates relative to the image’s lower-left corner; default is `((0 . 0) . (1 . 1))` (= `#f`). _mconf_ is the minimum acceptable confidence level ranging from 0.0 to 1.0, where 0.0 represents no confidence and 1.0 represents full confidence; default is 0.0.
