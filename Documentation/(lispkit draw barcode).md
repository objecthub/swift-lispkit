# LispKit Draw Barcode

Library `(lispkit draw barcode)` provides procedures for generating images of barcodes. Supported are _Code 128_, _QR Code_, _Aztec_, and _PDF417_ barcodes.

_Code 128_ is a high-density linear barcode defined in ISO 15417. It is used for alphanumeric or numeric-only barcodes and can encode any of the 128 ASCII characters. _GS1-128_ (formerly known as _UCC/EAN-128_) is a subset of _Code 128_ and is used extensively worldwide in retail, shipping and packaging industries as a product identification code.

![Code 128 Barcode](x-devonthink-item://DF7208E4-1ECE-432E-95FF-D927898889E2)

_QR codes_ (quick-response codes) are two-dimensional matrix barcodes, featureing black squares on a white background with fiducial markers, readable by imaging devices like cameras, and processed using Reed–Solomon error correction.

![QR Code](x-devonthink-item://29F1FB6D-52E1-4396-8C90-1896088A126B)

_Aztec codes_ are matrix codes defined by ISO 24778. Named after the resemblance of the central finder pattern to an Aztec pyramid, Aztec codes have the potential to use less space than other matrix barcodes because they do not require a surrounding blank/"quiet" zone.

![Aztec Code](x-devonthink-item://5E64B888-D93E-47DC-84EC-70159E6FFA82)

_PDF417_ is a stacked linear barcode format used in a variety of applications such as transport, identification cards, and inventory management. It is defined in ISO 15438.

![PDF417 Code](x-devonthink-item://51DB5D21-B430-483E-A948-00A3AC214C84)


**(code128-image _str height_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(code128-image _str height pad_)**  
**(code128-image _str height pad scale_)**  
**(code128-image _str height pad scale color_)**  
**(code128-image _str height pad scale color backgr_)**  
**(code128-image _str height pad scale color backgr ppi_)**  

Returns a _[Code 128](https://en.wikipedia.org/wiki/Code_128)_ barcode for the given string _str_ and _height_ in points as a bitmap image. _pad_ defines the padding around the barcode in points (default: 0.0). It is possible to scale the barcode with scaling factor _scale_ (default: 1.0). _color_ defines the color of the barcode (default: `#f`), _backgr_ defines the background color (default: `#f`). _ppi_ determines the number of pixels per inch. By default, _ppi_ is set to 72.

```scheme
(define bc (code128-image "010123456789012815051231" 50 10))
(save-bitmap "barcode.png" bc 'png)
```

**(qr-code-image _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(qr-code-image _str corr_)**  
**(qr-code-image _str corr scale_)**  
**(qr-code-image _str corr scale color_)**  
**(qr-code-image _str corr scale color backgr_)**  
**(qr-code-image _str corr scale color backgr ppi_)**  

Returns a _[QR Code](https://en.wikipedia.org/wiki/QR_code)_ matrix barcode for the given string _str_ with 1 pixel matrix cells as a bitmap image. _corr_ is the correction level, a symbol which can be one of: `low`, `medium`, `quartile`, and `high` (default: `medium`). It is possible to scale the QR code with scaling factor _scale_ (default: 2.0). _color_ defines the color of the barcode (default: `#f`), _backgr_ defines the background color (default: `#f`). _ppi_ determines the number of pixels per inch. By default, _ppi_ is set to 72.

```scheme
(define qc (qr-code-image "http://lisppad.app" 'quartile 2.5))
(save-bitmap "qrcode.png" qc 'png)
```

**(aztec-code-image _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(aztec-code-image _str corr_)**  
**(aztec-code-image _str corr compact?_)**  
**(aztec-code-image _str corr compact? scale_)**  
**(aztec-code-image _str corr compact? scale color_)**  
**(aztec-code-image _str corr compact? scale color backgr_)**  
**(aztec-code-image _str corr compact? scale color backgr ppi_)**  

Returns an [Aztec Code](https://en.wikipedia.org/wiki/Aztec_Code) matrix barcode for the given string _str_ with 1 pixel matrix cells as a bitmap image. _corr_ is the correction level (default: 23.0). If _compact?_ is provided, it can be used to select between a standard and a compact representation (default: `()`). It is possible to scale the Aztec code with scaling factor _scale_ (default: 1.0). _color_ defines the color of the barcode (default: `#f`), _backgr_ defines the background color (default: `#f`). _ppi_ determines the number of pixels per inch. By default, _ppi_ is set to 72.

```scheme
(define ac (aztec-code-image "help@lisppad.app" #f '() 3.0))
(save-bitmap "azteccode.png" ac 'png)
```

**(pdf417-code-image _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pdf417-code-image _str config_)**  
**(pdf417-code-image _str config scale_)**  
**(pdf417-code-image _str config scale color_)**  
**(pdf417-code-image _str config scale color backgr_)**  
**(pdf417-code-image _str config scale color backgr ppi_)**  

Returns a [PDF417](https://en.wikipedia.org/wiki/PDF417) barcode for the given string _str_ and barcode configuration _config_ as a bitmap image. _config_ is an association list supporting the following symbolic keys:

  - `min-width`: Minimum width in points
  - `max-width`: Maximum width in points
  - `min-height`: Minimum height in points
  - `max-height`: Maximum height in points
  - `columns`: The number of columns (between 1 and 31)
  - `rows`: The number of rows (between 1 and 91)
  - `preferred-aspect-ratio`: Aspect ratio of the barcode
  - `compaction-mode`: Compaction mode (between 0 and 4)
  - `correction-level`: Compaction level (between 0 and 9)
  - `always-specify-compaction`: Is compaction mode always required? (bool)

It is possible to scale the PDF417 barcode with scaling factor _scale_ (default: 1.0). _color_ defines the color of the barcode (default: `#f`), _backgr_ defines the background color (default: `#f`). _ppi_ determines the number of pixels per inch. By default, _ppi_ is set to 72.

```scheme
(define pc (pdf417-code-image "help@lisppad.app" '() 1.5))
(save-bitmap "pdf417.png" pc 'png)
```
