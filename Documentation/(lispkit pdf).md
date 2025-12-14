# LispKit PDF

Library `(lispkit pdf)` provides an API for manipulating and analyzing PDF documents. The library supports creating PDF documents, managing pages, adding annotations, handling bookmarks, managing outlines, extracting content, and rendering pages.

A PDF document contains pages corresponding to the pages of a printed document. Each page contains text, images, hyperlinks and annotations. The page uses a coordinate system with 72 points to each printed inch. The coordinates originate at the bottom left of the page and increase going upward and to the right (as opposed to the coordinate system used by library `(lispkit draw)`. A PDF document also contains an outline which is a hierarchical data structure that defines the table of contents.


## PDF documents

**(pdf? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an object representing a PDF document. Otherwise, `#f` is returned.

**(pdf-encrypted? _doc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an object representing an encrypted PDF document. Otherwise, `#f` is returned.

**(pdf-locked? _doc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an object representing a locked PDF document. Otherwise, `#f` is returned. Only encrypted documents can be locked. Encrypted documents whose password is the empty string are unlocked automatically upon opening. PDF documents can be unlocked with the `pdf-unlock` procedure.

**(make-pdf)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an object representing a new, empty PDF document.

**(bytevector-\>pdf _bvec_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(bytevector-\>pdf _bvec start_)**  
**(bytevector-\>pdf _bvec start end_)**  

Returns an object representing the PDF document defined by the bytes in bytevector _bvev_ between _start_ and _end_. If _end_ is not provided, it is assumed to be the length of _bvec_. If _start_ is not provided, it is assumed to be 0.

**(load-pdf _filepath_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Loads the PDF file at path _filepath_ and returns an object representing this PDF document. It is an error if the file at the given file path is not a PDF file.

**(save-pdf _filename doc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(save-pdf _filename doc options_)**  

Saves the PDF document represented by object _doc_ into a file at path _filepath_. _options_ is an association list supporting the following symbolic keys:

 - `user-password`: The value is a user password string.
 - `owner-password`: The value is a owner password string.
 - `access-permissions`: The value is either `#f` (no permissions), `#t` (access permissions as defined by _doc_), or a list of the following symbols: `commenting` (allow commenting), `content-accessibility` (allow content accessibility), `content-copying` (allow copying of content), `document-assembly` (allow mutation of page structure), `document-changes` (allow changes to the PDF document), `form-field-entry` (allow setting form field values), `high-quality-printing` (support high quality printing), and `low-quality-printing` (support low quality printing).
 - `burn-in-annotations`: If set to `#t`, annotations are burned into PDF pages.
 - `optimize-for-screen`: If set to `#t`, the PDF document representation is optimized for screen usage.
 - `save-images-as-jpeg`: If set to `#t`, images will all be represented as JPEGs.
 - `save-text-from-ocr`: If set to `#t`, OCR will be applied to all pages and the text will be stored as an invisible but searchable and selectable layer on top of the page.

**(pdf-unlock _doc passwd_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Unlocks the encrypted PDF document _doc_ with password string _passwd_. After unlocking, predicate `pdf-locked?` will return `#f`.

**(pdf-\>string _doc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Extracts all text from the PDF document _doc_ and returns it as a string.

**(pdf-\>bytevector _doc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pdf-\>bytevector _doc options_)**  

Returns a bytevector representing the PDF document _doc_. _options_ is an association list supporting the following symbolic keys:

 - `user-password`: The value is a user password string.
 - `owner-password`: The value is a owner password string.
 - `access-permissions`: The value is either `#f` (no permissions), `#t` (access permissions as defined by _doc_), or a list of the following symbols: `commenting` (allow commenting), `content-accessibility` (allow content accessibility), `content-copying` (allow copying of content), `document-assembly` (allow mutation of page structure), `document-changes` (allow changes to the PDF document), `form-field-entry` (allow setting form field values), `high-quality-printing` (support high quality printing), and `low-quality-printing` (support low quality printing).
 - `burn-in-annotations`: If set to `#t`, annotations are burned into PDF pages.
 - `optimize-for-screen`: If set to `#t`, the PDF document representation is optimized for screen usage.
 - `save-images-as-jpeg`: If set to `#t`, images will all be represented as JPEGs.
 - `save-text-from-ocr`: If set to `#t`, OCR will be applied to all pages and the text will be stored as an invisible but searchable and selectable layer on top of the page.

**(pdf-path _doc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the file path of PDF document _doc_ if it exists. Otherwise, `#f` is returned.

**(pdf-version _doc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the PDF version used for the representation of the PDF document _doc_ as a pair of two numbers: (_major_ . _minor_). Returns `#f` if a version is not available.

**(pdf-attributes _doc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an association list of document-level attributes for the PDF document _doc_. Common attributes include `Creator` (the creator of the content as a string), `Producer` (the one who made this PDF as a string), `Author` (the author of this content as a string), `Title` (the title of this document as a string), `Subject` (the subject of this document as a string), `CreationDate` (the creation date of this document), `ModDate` (the modification date of this document), `Keywords` (a string describing keywords associated with this document).

**(pdf-attribute-ref _doc key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pdf-attribute-ref _doc key default_)**  

Returns a value for a document-level attribute of PDF document _doc_ identified by symbol _key_. Keys of common attributes are: `Creator`, `Producer`, `Author`, `Title`, `Subject`, `CreationDate`, `ModDate`, and `Keywords`. If the attribute identified by _key_ does not exist, _default_ is returned if it was provided. Otherwise, `#f` is returned.

**(pdf-attribute-set! _doc key value_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the value for a document-level attribute of PDF document _doc_ identified by symbol _key_ to _value_. Keys of common attributes are: `Creator`, `Producer`, `Author`, `Title`, `Subject`, `CreationDate`, `ModDate`, and `Keywords`.

**(pdf-attribute-remove! _doc key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes a document-level attribute from PDF document _doc_ identified by symbol _key_. Keys of common attributes are: `Creator`, `Producer`, `Author`, `Title`, `Subject`, `CreationDate`, `ModDate`, and `Keywords`.

**(pdf-access-permissions _doc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of symbols representing permissions enabled for the PDF document _doc_. The following symbols are supported:

 - `commenting`: allow commenting,
 - `content-accessibility`: allow content accessibility,
 - `content-copying`: allow copying of content,
 - `document-assembly`: allow mutation of page structure,
 - `document-changes`: allow changes to the PDF document,
 - `form-field-entry`: allow setting form field values,
 - `high-quality-printing`: support high quality printing, and
 - `low-quality-printing`: support low quality printing.

**(pdf-page-count _doc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of pages of PDF document _doc_.

**(pdf-pages _doc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of PDF pages for PDF document _doc_.

**(pdf-page _doc n_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the PDF page at index _n_ of PDF document _doc_.

**(pdf-insert-page! _doc page_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pdf-insert-page! _doc n page_)**  

Inserts the given PDF _page_ at index _n_ into the PDF document _doc_. If _n_ is not provided, the page is inserted at the end.

**(pdf-remove-page! _doc n_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the page at index _n_ of PDF document _doc_.

**(pdf-swap-page! _doc n m_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Swaps the page at index _n_ with the page at index _m_ of PDF document _doc_.

**(pdf-page-index _doc page_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the index of PDF _page_ in the given PDF document _doc_. If _page_ is not a valid PDF page in _doc_, `#f` is returned.

**(pdf-outline _doc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the outline (table of contents) for the given PDF document _doc_ or `#f` if there is no outline available for _doc_.

**(pdf-outline-set! _doc outline_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the outline (table of contents) for the given PDF document _doc_ to _outline_. If _outline_ is `#f`, then the table of contents of _doc_ is removed.


## Predicates

**(pdf-display-box? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid display box specifier. Supported display box specifiers are: `media-box`, `crop-box`, `bleed-box`, `trim-box`, and `art-box`.

**(pdf-line-style? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid line style specifier. Supported line style specifiers are: `none`, `square`, `circle`, `diamond`, `open-arrow`, and `closed-arrow`.

**(pdf-text-alignment? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid text alignment specifier. Supported text alignment specifiers are: `left`, `right`, `center`, `justified`, and `natural`.

**(pdf-icon-type? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid icon type specifier. Supported icon type specifiers are: `comment`, `key`, `note`, `help`, `new-paragraph`, `paragraph`, and `insert`.

**(pdf-markup-type? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid markup type specifier. Supported markup type specifiers are: `highlight`, `strike-out`, `underline`, and `redact`.


## PDF pages

Pages in a PDF document are represented with `pdf-page` objects. A _PDF page_ can either be inserted into a PDF document or it is standalone. If the page has been inserted, `pdf-page-number` returns the page number. If it has not been inserted, `pdf-page-number` returns `#f`. _Page numbers_ are managed automatically and they potentially change when the pages of a PDF document are rearranged. _Page labels_ on the other hand are customizable names for pages that are stable. The orientation of a PDF page is set by specifying a _rotation_.

A PDF page defines rectangular _bounds_ for different types of _display boxes_ of that page. _Display boxes_ are identified via symbolic specifiers. Supported are: `media-box`, `crop-box`, `bleed-box`, `trim-box`, and `art-box`. The content of a PDF page is defined in terms of up to three different layers: besides the native PDF content, it is possible to set an _underlay drawing_ which is drawn underneith the native content. If is also possible to define an _overlay drawing_ which is drawn on top of the native content.

One type of element of the native content of a PDF page are _PDF annotations_. These are widgets that can be inserted at specified coordinates. Many different types of widgets are supported. They can be identified on the page, inserted, modified, and removed.


**(pdf-page? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an object representing a PDF page. Otherwise, `#f` is returned.

**(make-pdf-page)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-pdf-page _box_)**  
**(make-pdf-page _image_)**  
**(make-pdf-page _image compress_)**  
**(make-pdf-page _image compress rotate_)**  
**(make-pdf-page _image compress rotate media_)**  
**(make-pdf-page _image compress rotate media upscale_)**  

Returns a new PDF page. If _box_ is provided, it is either a size describing the size of the media box of the page, or a rectangle defining the media box of the page. Alternatively, an _image_ can be provided as the basis for the new PDF page. In this case, parameters _compress_, _rotate_, _media_, and _upscale_ can be provided to customize the usage of the image. _compress_ is a flonum between 0.0 (= lowest quality) and 1.0 (= highest quality) defining the compression quality, _rotate_ is a positive or negative multiple of 90 describing the rotation of the image, _media_ is either `#f`, a size object, or a rect describing the media box of the page, and _upscale_ is a boolean to enable the upscaling of the image if it is smaller than the size of the page.

**(pdf-page-copy _page_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of the given PDF _page_.

**(pdf-page-document _page_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the PDF document to which the given PDF _page_ belongs, or `#f` if the page has not been inserted into a PDF document.

**(pdf-page-number _page_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the page index (starting from 0) of the given PDF _page_ in the PDF document in which it was inserted. `pdf-page-number` returns `#f` is the PDF _page_ has not been inserted into a PDF document.

**(pdf-page-label _page_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the label of the given PDF _page_ in the PDF document in which it was inserted. `pdf-page-label` returns `#f` is the PDF _page_ has not been inserted into a PDF document or does not have a label.

**(pdf-page-bounds _page box-spec_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a rect describing the page bounds for the display box specifier _box-spec_ of PDF _page_. _box-spec_ is one of the following symbols: `media-box`, `crop-box`, `bleed-box`, `trim-box`, and `art-box`.

**(pdf-page-bounds-set! _page box-spec box_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the display box specified by _box-spec_ for the given PDF _page_ to the rect _box_. _box-spec_ is one of the following symbols: `media-box`, `crop-box`, `bleed-box`, `trim-box`, and `art-box`.

**(pdf-page-rotation _page_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the rotation angle in degrees (a multiple of 90) of PDF _page_.

**(pdf-page-rotation-set! _page rotate_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the rotation angle of PDF_page_ to _rotate_. _rotate_ is a positive or negative multiple of 90 describing the rotation of _page_.

**(pdf-page-annotations-display _page_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if annotations should be displayed on PDF _page_; otherwise `#f` is returned.

**(pdf-page-annotations-display-set! _page display?_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Enables the display of annotations on PDF _page_ if _display_ is true. If _display_ is `#f`, then the display of annotations gets disabled for _page_.

**(pdf-page-annotations _page_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all PDF annotations on PDF _page_.

**(pdf-page-annotation-ref _page point_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the PDF annotations that can be found at coordinates _point_ on PDF _page_. `#f` is returned if there is no annotation at _point_.

**(pdf-page-annotation-add! _page annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Adds the PDF annotation _annot_ to PDF _page_.

**(pdf-page-annotation-remove! _page annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the annotation _annot_ from PDF _page_.

**(pdf-page-underlay _page_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

PDF pages are drawn in three layers: first an _underlay drawing_ is drawn (if it exists) followed by the PDF page content including its annotations, and finally an _overlay drawing_ is drawn (if it exists). `pdf-page-underlay` returns the underlay drawing for _page_ if it exists, otherwise `#f` is returned.

**(pdf-page-underlay-set! _page drawing_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the underlay drawing of PDF _page_ to _drawing_. An underlay drawing is removed if _drawing_ is `#f`.

**(pdf-page-overlay _page_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

PDF pages are drawn in three layers: first an _underlay drawing_ is drawn (if it exists) followed by the PDF page content including its annotations, and finally an _overlay drawing_ is drawn (if it exists). `pdf-page-underlay` returns the overlay drawing for _page_ if it exists, otherwise `#f` is returned.

**(pdf-page-overlay-set! _page drawing_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the overlay drawing of PDF _page_ to _drawing_. An overlay drawing is removed if _drawing_ is `#f`.

**(pdf-page-images _page_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all images that can be found on PDF _page_.

**(pdf-page-thumbnail _page box size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Generates a thumbnail of _size_ (in points) from the content in the rect _box_ on PDF _page_ and returns a corresponding image.

**(pdf-page-\>bitmap _page box size_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pdf-page-\>bitmap _page box size ppi_)**  
**(pdf-page-\>bitmap _page box size ppi ipol_)**  

Returns a bitmap of _size_ (in points) from the content in rect _box_ on PDF _page_. _ppi_ determines the number of pixels per inch. By default, _ppi_ is set to 72. In this case, the number of pixels of the returned bitmap corresponds to the number of points (since 1 pixel corresponds to 1/72 of an inch). _ipol_ is a fixnum between 0 and 4 indicating the interpolation quality used for creating the bitmap: `0` = default, `1` = none, `2` = low, `3` = high, and `4` = medium.

**(pdf-page-\>string _page_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a string representing all the text on PDF _page_. If no string can be extracted, `#f` is returned.

**(pdf-page-\>styled-text _page_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a styled-text object representing all the text on PDF _page_. If no styled-text can be extracted, `#f` is returned.

**(pdf-page-\>bytevector _page_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a bytevector representing a PDF document with _page_ as the only page of the PDF document. If no binary representation of this page can be created, then `#f` is returned.

**(draw-pdf-page _page box-spec rect drawing_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Draws the content within the display box specified by _box-spec_ of PDF _page_ into _drawing_. Drawings are defined by library `(lispkit draw)`. _box-spec_ is one of the following symbols: `media-box`, `crop-box`, `bleed-box`, `trim-box`, and `art-box`. _rect_ specifies a rectangular region into which _page_ is drawn. _rect_ can be specified in the following ways:

 - `#f`: _rect_ corresponds to the rectangular specified by _box-spec_.
 - `#t`: _rect_ has its origin at `zero-point` and the width and height correspond to the size of the rectangular specified by _box-spec_.
 - `(x . y)`: _rect_ has its origin at point (x, y) and the width and height correspond to the size of the rectangular specified by _box-spec_.
 - `((x . y) . (w . h))`: _rect_ corresponds to the given rectangular.
 - `(#f . (w . h))`: rect_ has its origin at `zero-point` and the width and height correspond to `w` and `h` respectively.


## PDF outlines

A _outline_ is an optional hierarchical component of a PDF document which defines the structure of a document and facilitates navigating within it. A _PDF outline_ object represents a node in the outline hierarchy. Each outline object belongs at most to one PDF document. Except for outline objects representing the root of a document, all outline objects have a parent outline object. This root outline object is not visible to the reader of a PDF document and serves merely as a container for the visible outlines. Each outline object has an index which defines an ordering underneath the parent of the outline object. Outline objects have a customizable label, destination, and are associated with an optional action. Each outline object may have several children.

**(pdf-outline? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a PDF outline object; otherwise `#f` is returned.

**(make-pdf-outline)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-pdf-outline _label_)**  
**(make-pdf-outline _label page_)**  
**(make-pdf-outline _label page point_)**  
**(make-pdf-outline _label page point zoom_)**  

Returns a new PDF outline object. _label_ is a string defining the label of the outline object. _page_ is a PDF page which is the target destination for this outline object. _point_ is a specific point on that page and _zoom_ is a floating-point number defining the zoom factor for that destination. All arguments are optional and `#f` by default.

**(pdf-outline-document _outline_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

If _outline_ is the child of an outline object which is part of the outline hierarchy of a PDF document, then `pdf-outline-document` returns this PDF document.

**(pdf-outline-parent _outline_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the parent outline object of _outline_. For root outline objects, `pdf-outline-parent` returns `#f`.

**(pdf-outline-index _outline_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the index of _outline_ within all the children sharing the same parent as _outline_. `pdf-outline-index` returns 0 for outline objects which do not have a parent.

**(pdf-outline-label _outline_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the label of _outline_ if a label has been defined, or `#f` otherwise.

**(pdf-outline-label-set! _outline str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the label of _outline_ to string _str_.

**(pdf-outline-destination _outline_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the destination of _outline_. A destination is defined in terms of a list with three elements: (_page_ _point_ _zoom_). _page_ is a PDF page which is the target destination for this outline object. _point_ is a specific point on that page and _zoom_ is a floating-point number defining the zoom factor for that destination. All elements can be `#f` if not defined.

**(pdf-outline-destination-set! _outline dest_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pdf-outline-destination-set! _outline page_)**  
**(pdf-outline-destination-set! _outline page point_)**  
**(pdf-outline-destination-set! _outline page point zoom_)**  

Sets the destination of _outline_. If _dest_ is provided, it is either `#f` (in which case the destination will be removed), or a list with exactly three elements: (_page_ _point_ _zoom_). Alternatively, _page_, _point_, and _zoom_ can be provided individually. _page_ is a PDF page which is the target destination for this outline object. _point_ is a specific point on that page and _zoom_ is a floating-point number defining the zoom factor for that destination. _point_ and _zoom_ can be `#f` if not defined.

**(pdf-outline-action _outline_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the action associated with _outline_, or `#f` if no action is defined. The following action types are supported:

 - (`goto` _page point_): Goes to _point_ on _page_.
 - (`goto-remote` _url page-index point_): Opens a PDF document at _url_ and goes to _point_ on the page with index _page-index_.
 - (`goto-url` _url_): Opens the page/document at _url_.
 - (`perform` _action-name_): Performs a named action. _action-name_ is one of the following symbols: `none`, `find`, `go-back`, `go-forward`, `goto-page`, `first-page`, `last-page`, `next-page`, `previous-page`, `print`, `zoom-in`, or `zoom-out`.
 - (`reset-fields` _name ..._): Resets the fields with the names _name ..._. _name ..._ are strings.
 - (`reset-fields-except` _name ..._): Resets all fields except for the ones with the names _name ..._. _name ..._ are strings.

**(pdf-outline-action-set! _outline action_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the action of _outline_ to _action_. _action_ is a PDF outline action matching one of the following supported action types:

 - (`goto` _page point_): Goes to _point_ on _page_.
 - (`goto-remote` _url page-index point_): Opens a PDF document at _url_ and goes to _point_ on the page with index _page-index_.
 - (`goto-url` _url_): Opens the page/document at _url_.
 - (`perform` _action-name_): Performs a named action. _action-name_ is one of the following symbols: `none`, `find`, `go-back`, `go-forward`, `goto-page`, `first-page`, `last-page`, `next-page`, `previous-page`, `print`, `zoom-in`, or `zoom-out`.
 - (`reset-fields` _name ..._): Resets the fields with the names _name ..._. _name ..._ are strings.
 - (`reset-fields-except` _name ..._): Resets all fields except for the ones with the names _name ..._. _name ..._ are strings.

**(pdf-outline-open? _outline_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the _outline_ is currently open, otherwise `#f` is returned.

**(pdf-outline-open-set! _outline_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pdf-outline-open-set! _outline open?_)**  

Closes the _outline_ if _open?_ is `#t`, otherwise _outline_ will be opened. If _open?_ is not provided, _outline_'s open state will be toggled.

**(pdf-outline-child-count _outline_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of children of _outline_.

**(pdf-outline-child-ref _outline n_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the _n_-th child from _outline_. If that outline child does not exist, `#f` is returned.

**(pdf-outline-child-insert! _outline child_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pdf-outline-child-insert! _outline n child_)**  

Inserts a PDF outline object _child_ at index _n_ into _outline_. If _n_ is not provided, _child_ is inserted at the end.

**(pdf-outline-child-remove! _outline n_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the _n_-th child from _outline_. If there is no _n_-th child, then `pdf-outline-child-remove!` fails.


## PDF annotations

PDF annotations are non-intrusive elements added to a PDF document to provide comments, feedback, or interactive features without altering the original content. PDF annotations are represented via its own object by library `(lispkit pdf)`. There are a lot of different types of annotations, but only some are supported by this library. The type of an annotation is represented as a symbol. Supported are annotations of the following types: `circle`, `free-text`, `highlight`, `ink`, `line`, `link`, `popup`, `square`, `stamp`, `strike-out`, `text`, and `underline`.

**(pdf-annotation? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a PDF annotation object; otherwise `#f` is returned.

**(make-pdf-annotation _bounds type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new PDF annotation object with the given _bounds_ and _type_. _bounds_ is a rect that specifies the bounding box for the annotation. _type_ is a symbol specifying the PDF annotation type. Supported are:

 - `circle`: Displays an elliptical or circular shape on the page.
 - `free-text`: Displays text directly on the page within a customizable box, remaining visible without needing to be opened. Callouts are a special type of free text annotation that includes a line or arrow pointing from the text box to a specific area of the document.
 - `highlight`: Marks important text by applying a translucent color over it.
 - `line`: Displays a single straight line on the page.
 - `ink`: Allows for free-form drawing or sketching directly on the document, represented by a custom shape.
 - `link`: Creates a clickable area that can jump to a web URL, a specific page within the document, or an external document.
 - `popup`: Displays text in a pop-up window for entry and editing.
 - `square`: Displays a rectangular shape on the page.
 - `stamp`: Applies pre-defined or custom visual stamps (e.g., "Approved", "Draft", "Confidential") to the document, similar to a physical rubber stamp.
 - `strike-out`: Draws a line through selected text, often indicating it should be removed.
 - `text`: A sticky-note like annotation which adds a small pop-up note or comment icon to the page that opens to display text when clicked.
 - `underline`: Draws a line beneath selected text.
 - `widget`: Displays interactive form elements, including text or signature fields, radio buttons, checkboxes, push buttons, pop-ups, and tables.

Annotations of other types can be managed with `(lispkit pdf)`, but there is no specific support to create or modify them. They typically use symbolic identifiers that start with a capital letter.

Once created, some annotations might require other attributes/metadata to be set. This can be done with the various PDF annotation management procedures.

Annotations created with `make-pdf-annotation` are by default not attached to a PDF page. Adding an annotation to a PDF page can be done with procedure `pdf-page-annotation-add!`.

**(pdf-annotation-page _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the PDF page on which this annotation is shown. If this annotation is not associated with a PDF page, `#f` is returned. 

**(pdf-annotation-type _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a symbol representing the annotation type of _annot_. The officially supported annotation types are `circle`, `free-text`, `highlight`, `ink`, `line`, `link`, `popup`, `square`, `stamp`, `strike-out`, `text`, `underline`, and `widget`.

**(pdf-annotation-name _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the name of PDF annotation _annot_ as a string. If _annot_ does not have a name, `#f` is returned.

**(pdf-annotation-name-set! _annot str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the name of PDF annotation _annot_ to string _str_. If _str_ is `#f`, then the name of _annot_ is cleared.

**(pdf-annotation-bounds _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the bounds of PDF annotation _annot_ as a rect.

**(pdf-annotation-bounds-set! _annot bounds_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the bounds of PDF annotation _annot_ to rect _bounds_.

**(pdf-annotation-padding _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the padding of PDF annotation _annot_ as a list with four elements: `(left-padding top-padding right-padding bottom-padding)`.

**(pdf-annotation-padding-set! _annot padding_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the padding of PDF annotation _annot_ to _padding_. _padding_ is a list of four flonum values: `(left-padding top-padding right-padding bottom-padding)`. If _padding_ is `#f`, padding is removed from _annot_.

**(pdf-annotation-border _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the border of PDF annotation _annot_. If _annot_ does not have a border, `#f` is returned. The border is expressed either as a list of two elements `(border-type line-width)` or three elements `(border-type line-width dash-pattern)` for dashed borders. The supported border types are `solid`, `dashed`, `beveled`, `inset`, and `underline`. The line width is a flonum value in points. The dash pattern is a list of flonum values that specify the lengths (measured in points) of the line segments and gaps in the pattern. The values in the array alternate, starting with the first line segment length, followed by the first gap length.

**(pdf-annotation-border-set! _annot border_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the border of the PDF annotation _annot_ to _border_. _border_ is either a list of two elements `(border-type line-width)` or three elements `(border-type line-width dash-pattern)` for dashed borders. The supported border types are `solid`, `dashed`, `beveled`, `inset`, and `underline`. The line width is a flonum value in points. The dash pattern is a list of flonum values that specify the lengths (measured in points) of the line segments and gaps in the pattern. The values in the array alternate, starting with the first line segment length, followed by the first gap length. _border_ may also be `#f`, in which case the border of _annot_ is cleared.

**(pdf-annotation-contents _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a string representing the textual content associated with the PDF annotation _annot_. `#f` is returned if there is no textual content associated with _annot_.

**(pdf-annotation-contents-set! _annot str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the textual content of PDF annotation _annot_ to string _str_. If _str_ is `#f`, the existing textual content associated with _annot_ is cleared.

**(pdf-annotation-alignment _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a symbol representing the textual alignment used by PDF annotation _annot_. Supported alignment values are `left`, `right`, `center`, `justified`, and `natural`.

**(pdf-annotation-alignment-set! _annot alignment_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the textual alignment used by PDF annotation _annot_ to _alignment_. _alignment_ is one of the following symbols: `left`, `right`, `center`, `justified`, and `natural`.

**(pdf-annotation-text-intent _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the text intent associated with PDF annotation _annot_ as a symbol (for the supported intents) or string (for the unsupported ones). The two supported intents are `callout` and `type-writer`.

**(pdf-annotation-text-intent-set! _annot intent_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the text intent associated with PDF annotation _annot_ to _intent_. _intent_ is either a string, or one of the two supported symbols `callout` and `type-writer`. If _intent_ is `#f`, the text intent is cleared.

**(pdf-annotation-font _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the font associated with PDF annotation _annot_. If no font is associated with _annot_, `#f` is returned.

**(pdf-annotation-font-set! _annot font_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Associates _font_ with PDF annotation _annot_. _font_ is a font object as defined by library `(lispkit draw)`.

**(pdf-annotation-color _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the annotation color of PDF annotation _annot_.

**(pdf-annotation-color-set! _annot color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the annotation color of PDF annotation _annot_ to _color_. _color_ is a color object as defined by library `(lispkit draw)`.

**(pdf-annotation-interior-color _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the interior color of PDF annotation _annot_.

**(pdf-annotation-interior-color-set! _annot color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the interior color of PDF annotation _annot_ to _color_. _color_ is a color object as defined by library `(lispkit draw)`.

**(pdf-annotation-background-color _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the background color of PDF annotation _annot_.

**(pdf-annotation-background-color-set! _annot color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the background color of PDF annotation _annot_ to _color_. _color_ is a color object as defined by library `(lispkit draw)`.

**(pdf-annotation-font-color _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the font color of PDF annotation _annot_.

**(pdf-annotation-font-color-set! _annot color_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the font color of PDF annotation _annot_ to _color_. _color_ is a color object as defined by library `(lispkit draw)`.

**(pdf-annotation-start-point _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a pair consisting of the point where a line begins, in annotation-space coordinates, and a symbol specifying the start line style. Supported line styles are `none`, `square`, `circle`, `diamond`, `open-arrow`, `closed-arrow`. Custom line styles are represented as strings.

**(pdf-annotation-start-point-set! _annot point_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pdf-annotation-start-point-set! _annot style_)**  
**(pdf-annotation-start-point-set! _annot point style_)**  

Sets the starting point of a line represented by PDF annotation _annot_ to _point_ and the corresponding start line style to _style_. Supported line styles are `none`, `square`, `circle`, `diamond`, `open-arrow`, `closed-arrow`. Custom line styles are represented as strings.

**(pdf-annotation-end-point _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a pair consisting of the point where a line ends, in annotation-space coordinates, and a symbol specifying the end line style. Supported line styles are `none`, `square`, `circle`, `diamond`, `open-arrow`, `closed-arrow`. Custom line styles are represented as strings.

**(pdf-annotation-end-point-set! _annot point_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pdf-annotation-end-point-set! _annot style_)**  
**(pdf-annotation-end-point-set! _annot point style_)**  

Sets the end point of a line represented by PDF annotation _annot_ to _point_ and the corresponding end line style to _style_. Supported line styles are `none`, `square`, `circle`, `diamond`, `open-arrow`, `closed-arrow`. Custom line styles are represented as strings.

**(pdf-annotation-icon _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the type of icon to display for a pop-up text annotation represented by PDF annotation _annot_ as a symbol. Supported icon types are `comment`, `key`, `note`, `help`, `new-paragraph`, `paragraph`, and `insert`.

**(pdf-annotation-icon-set! _annot icon_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the icon type for PDF annotation _annot_ to _icon_. _icon_ is one of the following supported icon types: `comment`, `key`, `note`, `help`, `new-paragraph`, `paragraph`, and `insert`. The icon type is only relevant for pop-up text annotations.

**(pdf-annotation-stamp _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the stamp string associated with PDF annotation _annot_. If there is no stamp associated with _annot_, `#f` is returned.

**(pdf-annotation-stamp-set! _annot stamp_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the stamp of PDF annotation _annot_ to _stamp_. _stamp_ is a string or `#f` if no stamp should be associated.

**(pdf-annotation-popup _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a pair consisting of a PDF popup annotation and a boolean value indicating whether the popup annotation is open (`#t`) or not (`#f`). If no popup annotation is associated with PDF annotation _annot_, then `#f` is returned.

**(pdf-annotation-popup-set! _annot popup_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pdf-annotation-popup-set! _annot popup_)**  
**(pdf-annotation-popup-set! _annot popannot open_)**    

Associates a popup annotation with PDF annotation _annot_. _popup_ is either a PDF annotation, or it is a pair consisting of a PDF annotation and a boolean value indicating whether the popup annotation is open (`#t`) or not (`#f`). If two arguments are provided, _popannot_ refers to a PDF annotation and boolean _open_ to its open status. If _popannot_ is `()`, then the current PDF popup annotation remains as is. If _popannot_ is `#f`, it is being removed.

**(pdf-annotation-markup-type _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the markup type of PDF annotation _annot_. The markup type is either `highlight`, `strike-out`, `underline`, `redact`, or `#f` if no markup type is defined.

**(pdf-annotation-markup-type-set! _annot type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the markup type of PDF annotation _annot_ to _type_. _type_ is either `#f` (for no markup type) or one of the following symbols: `highlight`, `strike-out`, `underline`, or `redact`.

**(pdf-annotation-markup-points _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the quadrilateral points that define the marked text region, or `#f` if no text is marked up by PDF annotation _annot_. The quadrilateral points are returned as a list of points bounding the marked-up text.

**(pdf-annotation-markup-points-set! _annot points_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the quadrilateral points that define the text region marked-up by PDF annotation _annot_ to _points_. _points_ is a list of points or `#f`, which will remove existing quadrilateral points.

**(pdf-annotation-callout-points _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the callout points associated with callout annotation _annot_. A callout annotation includes a text box and an arrow that can be moved independently of the text box. The arrow consists of an optional knee line followed by an end-point line defined by 2 or 3 points. These are returned by `pdf-annotation-callout-points` as a list of 2 or 3 points. `#f` is returned if there are no callout points.

**(pdf-annotation-callout-points-set! _annot copts_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the callout points of callout annotation _annot_ to _copts_. A callout annotation includes a text box and an arrow that can be moved independently of the text box. The arrow consists of an optional knee line followed by an end-point line defined by 2 or 3 points. _copts_ is either a 2 or 3 point list. Alternatively, _copts_ can be set to `#f`, in which case potentially existing callout points are being removed.

**(pdf-annotation-shapes _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of shape objects that compose the PDF annotation _annot_. Shape objects are defined by library `(lispkit draw)`. They are provided in annotation-space coordinates.

**(pdf-annotation-shape-add! _annot sh_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Adds a shape object _sh_ to the list of shape objects that compose the PDF annotation _annot_. Shape objects are defined by library `(lispkit draw)`. They are provided in annotation-space coordinates.

**(pdf-annotation-shapes-clear! _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes all shape objects from the PDF annotation _annot_. Shape objects are defined by library `(lispkit draw)`. They define the overall shape of a PDF annotation.

**(pdf-annotation-modification-date _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the modification date of PDF annotation _annot_ as a date-time object as defined by library `(lispkit date-time)`.

**(pdf-annotation-modification-date-set! _annot dt_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the modification date of PDF annotation _annot_ to _dt_. _dt_ is a date-time object as defined by library `(lispkit date-time)`.

**(pdf-annotation-username _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the username associated with PDF annotation _annot_ as a string or `#f` if there is no associated username.

**(pdf-annotation-username-set! _annot name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the username associated with PDF annotation _annot_ to _name_. _name_ is either a string or it is `#f`, in which case a potentially existing username is being removed.

**(pdf-annotation-url _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the URL associated with PDF annotation _annot_ as a string. `#f` is returned if there is no associated URL.

**(pdf-annotation-url-set! _annot url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the URL associated with PDF annotation _annot_ to _url_. _url_ is either a string or it is `#f`, in which case a potentially existing URL is being removed.

**(pdf-annotation-destination _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the document destination for link annotation _annot_. A destination is defined in terms of a list with three elements: (_page_ _point_ _zoom_). _page_ is a PDF page which is the target destination for this outline object. _point_ is a specific point on that page and _zoom_ is a floating-point number defining the zoom factor for that destination. All elements can be `#f` if not defined.

**(pdf-annotation-destination-set! _annot dest_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pdf-annotation-destination-set! _annot page_)**  
**(pdf-annotation-destination-set! _annot page point_)**  
**(pdf-annotation-destination-set! _annot page point zoom_)**  

Sets the document destination for link annotation _annot_ . If _dest_ is provided, it is either `#f` (which will remove the destination), or a list with exactly three elements: (_page_ _point_ _zoom_). Alternatively, _page_, _point_, and _zoom_ can be provided individually. _page_ is a PDF page which is the target destination for this outline object. _point_ is a specific point on that page and _zoom_ is a floating-point number defining the zoom factor for that destination. _point_ and _zoom_ can be `#f` if not defined.

**(pdf-annotation-action _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the action associated with PDF annotation _annot_, or `#f` if no action is defined. The following forms of actions are supported:

 - (`goto` _page point_): Goes to _point_ on _page_.
 - (`goto-remote` _url page-index point_): Opens a PDF document at _url_ and goes to _point_ on the page with index _page-index_.
 - (`goto-url` _url_): Opens the page/document at _url_.
 - (`perform` _action-name_): Performs a named action. _action-name_ is one of the following symbols: `none`, `find`, `go-back`, `go-forward`, `goto-page`, `first-page`, `last-page`, `next-page`, `previous-page`, `print`, `zoom-in`, or `zoom-out`.
 - (`reset-fields` _name ..._): Resets the fields with the names _name ..._. _name ..._ are strings.
 - (`reset-fields-except` _name ..._): Resets all fields except for the ones with the names _name ..._. _name ..._ are strings.

**(pdf-annotation-action-set! _annot action_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the action of PDF annotation _annot_ to _action_. _action_ is a PDF action matching one of the following supported forms of actions:

 - (`goto` _page point_): Goes to _point_ on _page_.
 - (`goto-remote` _url page-index point_): Opens a PDF document at _url_ and goes to _point_ on the page with index _page-index_.
 - (`goto-url` _url_): Opens the page/document at _url_.
 - (`perform` _action-name_): Performs a named action. _action-name_ is one of the following symbols: `none`, `find`, `go-back`, `go-forward`, `goto-page`, `first-page`, `last-page`, `next-page`, `previous-page`, `print`, `zoom-in`, or `zoom-out`.
 - (`reset-fields` _name ..._): Resets the fields with the names _name ..._. _name ..._ are strings.
 - (`reset-fields-except` _name ..._): Resets all fields except for the ones with the names _name ..._. _name ..._ are strings.

**(pdf-annotation-display _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a boolean value indicating whether the PDF annotation _annot_ should be displayed.

**(pdf-annotation-display-set! _annot disp?_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Controls whether the PDF annotation _annot_ should be displayed. If _disp?_ is `#f`, the annotation will be hidden, otherwise shown.

**(pdf-annotation-print _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a boolean value indicating whether the PDF annotation _annot_ should appear when the document is printed.

**(pdf-annotation-print-set! _annot print?_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Controls whether the PDF annotation _annot_ should be printed. If _print?_ is `#f`, the annotation will be hidden while printing, otherwise shown.

**(pdf-annotation-highlighted _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a boolean value indicating whether the PDF annotation _annot_ is in a highlighted state, such as when the mouse is down on a link annotation.

**(pdf-annotation-highlighted-set! _annot highl?_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Controls whether the PDF annotation _annot_ should be put into a highlighted state, such as when the mouse is down on a link annotation. If _highl?_ is `#f`, the annotation will not be in highlighted state, otherwise it will.

**(pdf-annotation-attributes _annot_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the attribute dictionary of the PDF annotation _annot_ as an association list mapping symbols to attribute values.

**(pdf-annotation-attributes-ref _annot attrib_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the attribute value for the given attribute key _attrib_ (a symbol) from the dictionary of the PDF annotation _annot_. `#f` is returned if attribute _attrib_ does not exist.

**(pdf-annotation-attributes-set! _annot attrib val_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the attribute given by symbol _attrib_ to the attribute value _val_ for the dictionary of the PDF annotation _annot_.

**(pdf-annotation-attributes-remove! _annot attrib_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the attribute identified by symbol _attrib_ from the dictionary of the PDF annotation _annot_.

**(draw-pdf-annotation _annot box-spec rect drawing_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Draws the PDF annotation _annot_ within the display box specified by _box-spec_ into _drawing_. Drawings are defined by library `(lispkit draw)`. _box-spec_ is one of the following symbols: `media-box`, `crop-box`, `bleed-box`, `trim-box`, and `art-box`. _rect_ specifies a rectangular region into which _page_ is drawn. _rect_ is a rectangle specified by an expression of this form: `((x . y) . (w . h))`.


## Paper sizes

**letter-size** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  
**legal-size**  
**executive-size**  
**a2-size**  
**a3-size**  
**a4-size**  
**a5-size**  
**a6-size**  
**b3-size**  
**b4-size**  
**b5-size**  
**b6-size**  
**c3-size**  
**c4-size**  
**c5-size**  
**c6-size**  

A size object representing a corresponding standardize size of a page in points. The size object is assuming 72 ppi, i.e. that there are 72 points per inch.

```scheme
;; US sizes
letter-size    ⇒  (612.0 . 792.0)  ; 8.5" × 11"
legal-size     ⇒  (612.0 . 1008.0) ; 8.5" × 14"
executive-size ⇒  (522.0 . 756.0)  ; 7.25" × 10.5"

;; ISO sizes
a2-size        ⇒  (1190.4 . 1683.6)
a3-size        ⇒  (841.8 . 1190.4)
a4-size        ⇒  (595.2 . 841.8)
a5-size        ⇒  (419.4 . 595.2)
a6-size        ⇒  (297.6 . 419.4)
```
