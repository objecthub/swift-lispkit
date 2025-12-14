# LispKit Styled-Text

Library `(lispkit styled-text)` provides an API to define and manipulate styled text. Styled text is a string with individual character ranges being layed out using a range of stylistic attributes. The library defines the layout of text in terms of objects encapsulating these stylistic attributes. There are three different style parameter collections: text styles, text block styles, and paragraph styles. Besides textual content, styled text objects may also contain tables (on macOS) and images.

Library `(lispkit styled-text)` also supports loading styled text from RTF, RTFD, and various Word formats (doc, docx). It can also save styled text in these formats.

## Styled text

A styled text object is mutable and consists of a string and a set of character ranges associated with stylistic attributes determining how the range of characters is layed out. Both the string and the attributed character ranges can be mutated. The following stylistic attributes are supported:

   - `background-color`: Color object defining the background color of the text range.
   - `foreground-color`: Color object defining the text color of the text range.
   - `strikethrough-color`: Color object defining the strike-through color of the text range.
   - `stroke-color`: Color object defining the outline color of the text range, i.e. the stroke color used for text displayed in outlined style.
   - `underline-color`: Color object defining the underline color of the text range for text using underlined style.
   - `baseline-offset`: The vertical offset for the position of the text in points.
   - `expansion`: The expansion factor of the text, i.e. a flonum corresponding to the log of the expansion factor to be applied to the glyphs. 0 is the default, indicating no expansion.
   - `kern`: The kerning of the text, i.e. the number of points by which to adjust kern-pair characters. This can be used to reduce/create space between characters. Kerning gets disabled by setting this attribute to 0.
   - `obliqueness`: The obliqueness of the text expressed as a flonum indicating skew to be applied to the glyphs. 0 is the default, indicating no skew.
   - `stroke-width`: The width of the stroke, i.e. the amount to change the stroke width for outlined text and is specified as a percentage of the font point size. 0 represents the default outline stroke width, negative values make the stroke extend inward, positive values extend it outward.
   - `ligature`: A boolean value indicating whether ligatures should be used in the text range.
   - `font`: Font object defining the font used in the text range.
   - `link`: A string representing the link used for the text range. This is, in most cases, a URL, but could also be a file path.
   - `paragraph-style`: Paragraph style object defining all parameters for laying out paragraphs in the text range.
   - `shadow`: The shadow of the text range. This is defined by a pair whose car is a size object and cdr is a positive flonum representing the shadow blur radius. The size object represents vertical and horizontal offsets of the shadow. Example: `((2.0 . -3.0) . 6.5)`.
   - `superscript`: The superscript of the text expressed as an offset in points.

Library `(lispkit styled-text)` provides functionality to create styled text objects, to compose them, to style them, and to introspect existing stylistic attributes.

**(styled-text? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a styled text object, otherwise `#f` is returned.

**(styled-text _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(styled-text _str style_)**  
**(styled-text _str font_)**  
**(styled-text _str font color_)**  
**(styled-text _str font color pstyle_)**  

Creates and returns a styled text object representing string _str_ using the stylistic attributes provided by text style object _style_, if provided. Alternatively, the given _font_, _color_, and paragraph style _pstyle_ objects are used to define the style of _str_.

**(make-styled-text _str key val ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-styled-text _image_)**  

Creates and returns a styled text object representing string _str_ layed out by the given stylistic attributes provided as key/value pairs. The attributes are applicable to the whole string. The following attribute _key_ symbols are supported:

   - `background-color:` Background color
   - `foreground-color:` Text color
   - `strikethrough-color:` Strike-through color
   - `stroke-color:` Outline color
   - `underline-color:` Underline color
   - `baseline-offset:` Vertical offset for position of the text in points
   - `expansion:` Text expansion factor
   - `kern:` The kerning of the text, i.e. the number of points by which to adjust kern-pair characters
   - `obliqueness:` The skew to be applied to the glyphs
   - `stroke-width:` The relative width of the outline stroke
   - `ligature:` Boolean indicating whether ligatures are used
   - `font:` Font of the text
   - `link:` Link target string, e.g. a URL
   - `paragraph-style:` Paragraph style for laying out paragraphs
   - `shadow:`: The shadow defined by a pair whose car is a size object defining horizontal and vertical offset and cdr is a positive flonum representing the shadow blur radius
   - `superscript:`: The superscript offset in points

The second use case for `make-styled-text` is creating a styled text object for a given image. 

This example shows how to use `make-styled-text`:

```scheme
(make-styled-text "Mauris scelerisque massa erat."
  'font: (font "Helvetica" 12.0)
  'foreground-color: (color 0.3 0.5 0.7)
  'paragraph-style:
    (make-paragraph-style
      'alignment: 'left
      'head-indent: 40.0
      'paragraph-spacing-before: 4.0))
```

**(make-styled-text-table _cols rows_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-styled-text-table _cols rows style_)**  
**(make-styled-text-table _cols rows style pstyle_)**  
**(make-styled-text-table _cols rows style pstyle collapse_)**  
**(make-styled-text-table _cols rows style pstyle collapse hide-empty_)**  

This procedure is only available on macOS. It creates a styled text table with _cols_ number of columns. _rows_ is a list of table rows. Each row is a list of table columns. A table column is either a string, styled text, or a text cell descriptor, which is a list containing at least a prefix of the following four components: (_text col-span tbstyle pstyle_). _text_ is either a string or styled text, _col-span_ is a number indicating how many columns the cell spans, _tbstyle_ is a text block style object and _pstyle_ is a paragraph style object, both of which are used to lay out the cell content.

_style_ is a default text block style object, and _pstyle_ is a default paragraph style object. They are both used to lay out cells which do not come with their own stylistic attributes.  _collapse_ is a boolean argument which collapses table borders if set to `#t` (which is the default). _hide-empty_ is a boolean argument which hides empty cells if set to `#t` (`#f` is the default).

Styled text tables are represented as a styled text object, so `make-styled-text-table` returns styled text.

```scheme
(make-styled-text-table 3
  (list ; list of rows
    (list ; list of columns
      "Cell 1,1"
      "Cell 1,2"
      (styled-text "Cell 1,3" (font "Helvetica" 11.0)))
    (list ; list of columns
      "Cell 2,1"
      (list ; column spanning 2 cells
        (styled-text "Cell 2,3" (font "Times" 10.0) red)
        2
        tbstyle
        pstyle)))
    def-tbstyle ; default text block style
    def-pstyle  ; default paragraph style
    #t)
```

**(load-styled-text _path format_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Loads the document at file _path_ and returns its content as styled text. _format_ specifies the file format to load. _format_ is one of the following symbols:

   - `plain`: Plain text file
   - `doc`: Microsoft Word file
   - `docx`: ECMA Office Open XML text document
   - `rtf`: RTF file
   - `rtfd`: RTFD file

**(save-styled-text _path txt format_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Saves the styled text _txt_ in a new file at file _path_ in file _format_. _format_ is one of the following symbols:

   - `plain`: Plain text file
   - `doc`: Microsoft Word file
   - `docx`: ECMA Office Open XML text document
   - `rtf`: RTF file
   - `rtfd`: RTFD file

**(copy-styled-text _txt_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(copy-styled-text _txt start_)**  
**(copy-styled-text _txt start end_)**  

Returns a copy of styled text _txt_ between positions _start_ (inclusive) and _end_ (exclusive). _start_ is an index between 0 and the length of _txt_ (default is 0). _end_ is an index between _start_ and the length of _txt_ (default is the length of _txt_).

**(html-\>styled-text _html_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns styled text for the given _html_ string.

**(styled-text-\>html _txt_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(styled-text-\>html _txt start_)**  
**(styled-text-\>html _txt start end_)**  

Returns HTML as a string representing the styled text _txt_ between position _start_ and _end_. If _end_ is not provided, it is assumed to be the length of _txt_. If _start_ is not provided, it is assumed to be 0.

**(bytevector-\>styled-text _bvec format_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(bytevector-\>styled-text _bvec format start_)**  
**(bytevector-\>styled-text _bvec format start end_)**  

`bytevector->styled-text` interprets bytevector _bvec_ between _start_ and _end_ as a file of text _format_ and returns its content as a new styled text object. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.  

**(styled-text-\>bytevector _txt format_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(styled-text-\>bytevector _txt format start_)**  
**(styled-text-\>bytevector _txt format start end_)**  

Stores the styled text _txt_ between position _start_ and _end_ in the given _format_ in a new bytevector and returns that bytevector. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0. _format_ is one of the following symbols:

   - `plain`: Plain text file
   - `doc`: Microsoft Word file
   - `docx`: ECMA Office Open XML text document
   - `rtf`: RTF file

**(styled-text=? _txt0 txt1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _txt0_, _txt1_, ... are all equals, otherwise `#f` is returned.

**(styled-text-string _txt_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a string for the given styled text _txt_.

**(styled-text-insert! _txt obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(styled-text-insert! _txt obj start_)**  
**(styled-text-insert! _txt obj start end_)**  

Inserts _obj_ into the styled text _txt_ replacing the characters between the position _start_ and _end_ with _obj_. If _obj_ is `#f`, then the characters between _start_ and _end_ are being deleted. If _obj_ is a string, styled text or an image, _obj_ gets converted into styled text and inserted accordingly. If _start_ is not provided, it is assumed to be 0. If _end_ is not provided, it is assumed to be the same like _start_.

**(styled-text-append! _txt obj ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Appends the objects _obj_, ... to the styled text _txt_ in the given order. _obj_ may be either a string, styled text, or an image.

**(styled-text-ref _txt index_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a text style object encapsulating all stylistic attributes that are applicable to the character at _index_ of styled text _txt_.

**(styled-text-set! _txt start end style_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(styled-text-set! _txt start end key val_)**  

If _style_ is provided, sets the stylistic attributes of styled text _txt_ in the character range from _start_ (inclusive) to _end_ (exclusive) to the attributes encapsulated by _style_. If _key_ and _val_ are provided instead, procedure `styled-text-set!` sets a single attribute _key_ to the value _val_ for the given character range of _txt_. Supported keys are: `background-color`, `foreground-color`, `strikethrough-color`, `stroke-color`, `underline-color`, `baseline-offset`, `expansion`, `kern`, `obliqueness`, `stroke-width`, `ligature`, `font`, `link`, `paragraph-style`, `shadow`, and `superscript`.

**(styled-text-add! _txt start end style_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(styled-text-add! _txt start end key val_)**  

If _style_ is provided, adds the stylistic attributes encapsulated by _style_ to the existing stylistic attributes of styled text _txt_ for the character range from _start_ (inclusive) to _end_ (exclusive). If _key_ and _val_ are provided instead, procedure `styled-text-add!` adds a single attribute _key_ to the value _val_ for the given character range of _txt_. Supported keys are: `background-color`, `foreground-color`, `strikethrough-color`, `stroke-color`, `underline-color`, `baseline-offset`, `expansion`, `kern`, `obliqueness`, `stroke-width`, `ligature`, `font`, `link`, `paragraph-style`, `shadow`, and `superscript`.

**(styled-text-remove! _txt start end key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the stylistic attribute _key_ from the styled text _txt_ in the character range from _start_ (inclusive) to _end_ (exclusive).

**(styled-text-attribute _txt key index_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(styled-text-attribute _txt key index start_)**  
**(styled-text-attribute _txt key index start end_)**  

This procedure returns two values. The first is the stylistic attribute value for attribute _key_ at character _index_ in the styled text _txt_ within the character range from _start_ (inclusive) to _end_ (exclusive). The second return value is the longest effective range of this attribute. If the attribute is not set at the given _index_, `styled-text-attribute` returns two `#f` values.

**(styled-text-attributes _txt index_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(styled-text-attributes _txt index start_)**  
**(styled-text-attributes _txt index start end_)**  

This procedure returns two values. The first is a text style object capturing all stylistic attributes at character _index_ in the styled text _txt_ within the character range from _start_ (inclusive) to _end_ (exclusive). The second return value is the longest effective range of this text style. Two `#f` values are returned if no attributes are found.

**(styled-text-first-attribute _text key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(styled-text-first-attribute _text key start_)**  
**(styled-text-first-attribute _text key start end_)**  

This procedure returns two values. The first is the first stylistic attribute value for attribute _key_ in the styled text _txt_ within the character range from _start_ (inclusive) to _end_ (exclusive). The second return value is the longest effective range of this attribute. If the attribute is not set in range _start_ to _end_, `styled-text-first-attribute` returns two `#f` values.

**(styled-text-first-attributes _txt_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(styled-text-first-attributes _txt start_)**  
**(styled-text-first-attributes _txt start end_)**  

This procedure returns two values. The first is a text style object capturing all stylistic attributes that were found first in the styled text _txt_ within the character range from _start_ (inclusive) to _end_ (exclusive). The second return value is the longest effective range of this text style. Two `#f` values are returned if no attributes are found.

## Text styles

Text style objects encapsulate a set of stylistic attributes, such as `font`, `background-color`, `baseline-offset`, `kern`, `obli	queness`, etc. Text style objects are mutable. Attributes can be inspected, added, and removed.

**(text-style? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a text style object; otherwise `#f` is returned.

**(make-text-style _key val ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a text style object encapsulating the given stylistic attributes provided as key/value pairs. The following attribute _key_ symbols are supported:

   - `background-color:` Background color
   - `foreground-color:` Text color
   - `strikethrough-color:` Strike-through color
   - `stroke-color:` Outline color
   - `underline-color:` Underline color
   - `baseline-offset:` Vertical offset for position of the text in points
   - `expansion:` Text expansion factor
   - `kern:` The kerning of the text, i.e. the number of points by which to adjust kern-pair characters
   - `obliqueness:` The skew to be applied to the glyphs
   - `stroke-width:` The relative width of the outline stroke
   - `ligature:` Boolean indicating whether ligatures are used
   - `font:` Font of the text
   - `link:` Link target string, e.g. a URL
   - `paragraph-style:` Paragraph style for laying out paragraphs
   - `shadow:`: The shadow defined by a pair whose car is a size object defining horizontal and vertical offset and cdr is a positive flonum representing the shadow blur radius
   - `superscript:`: The superscript offset in points

This example shows how to use `make-text-style`:

```scheme
(make-text-style
  'font: (font "Times" 13.5)
  'foreground-color: (color 0.8 0.8 0.8)
  'paragraph-style:
    (make-paragraph-style
      'alignment: 'left
      'paragraph-spacing-before: 5.0))
```

**(copy-text-style _style_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of _style_.

**(text-style-empty? _style_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the _style_ object does not include any stylistic attributes; otherwise `#f` is returned.

**(text-style-ref _style key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the attribute value for stylistic attribute _key_ defined by _style_. `#f` is returned if no value is set.

**(text-style-set! _style key value_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the stylistic attribute _key_ to _value_ in _style_.

**(text-style-merge! _style style1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Merges all the text style objects _style1 ..._ into _style_. The text style objects are merged in the order provided, i.e. later values override values in earlier style objects.

**(text-style-remove! _style key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the stylistic attribute _key_ from _style_.

**(text-style-attributes _expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the stylistic attributes of _style_ as an association list. The result is a list of key/value pairs.

```scheme
(text-style-attributes
  (make-text-style
    'font: (font "Times" 13.5)
    'foreground-color: (color 0.8 0.8 0.8)
    'obliqueness: 1.2))
⇒ ((font . #<font Times-Roman 13.5>)
   (obliqueness . 1.2)
   (foreground-color . #<color 0.8 0.8 0.8>))
```

## Text block styles

Text block style objects encapsulate attributes describing how text is layed out in a box. Text block styles are currently only used for defining the layout of cells in a table. The following text block style attributes are supported:

   - `width`: The width of the text block in points or as a percentage.
   - `height`: The height of the text block in points or as a percentage.
   - `margin`: The margin around the text block. This is either one value representing the same margin in points or as a percentage for all four sides, or it is a list of four values `(left right top bottom)`.
   - `border`: The "thickness"/size of the border in points or as a percentage. This is either one value representing the same border size for all four sides, or it is a list of four values `(left right top bottom)`. 0 means "no border".
   - `padding`: The padding within the text block in points or as a percentage. This is either one value representing the same padding for all four sides, or it is a list of four values `(left right top bottom)`.
   - `background-color`: The background color of the text block.
   - `border-color`: The color of the border of the text block.
   - `vertical-alignment`: The vertical alignment of the text within the block. Supported are the following four alignment values: `top`, `middle`, `bottom`, and `baseline`.

Text block style objects are mutable. They define values for all attributes, i.e. there are defaults for all attributes set for newly created text block style objects.


**(percent _num_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Some text block style attributes allow for relative values. Procedure `percent` encodes a fixnum _num_ as a percentage (i.e. `(num . %)`).

**(percent? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an object representing a percentage; otherwise `#f` is returned.

**(text-block-style? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a text block style object; otherwise `#f` is returned.

**(make-text-block-style _key value ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a text block style object encapsulating the given attributes provided as key/value pairs. The following attribute _key_ symbols are supported:

   - `width:` The width of the text block in points or as a percentage.
   - `height:` The height of the text block in points or as a percentage.
   - `margin:` The margin around the text block. This is either one value representing the same margin in points or as a percentage for all four sides, or it is a list of four values `(left right top bottom)`.
   - `border:` The "thickness"/size of the border in points or as a percentage. This is either one value representing the same border size for all four sides, or it is a list of four values `(left right top bottom)`. 0 means "no border".
   - `padding:` The padding within the text block in points or as a percentage. This is either one value representing the same padding for all four sides, or it is a list of four values `(left right top bottom)`.
   - `background-color:` The background color of the text block.
   - `border-color:` The color of the border of the text block.
   - `vertical-alignment:` The vertical alignment of the text within the block. Supported are the following four alignment values: `top`, `middle`, `bottom`, and `baseline`.

**(copy-text-block-style _bstyle_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of _bstyle_.

**(text-block-style=? _bstyle bstyle0 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if all _bstyle0 ..._ are equal to _bstyle_; otherwise `#f` is returned.

**(text-block-style-ref _bstyle key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the value associated with text block style attribute _key_.

**(text-block-style-set! _bstyle key value_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the text block style attribute _key_ to _value_ for the text block style object _bstyle_.


## Paragraph styles

Paragraph style objects define how a paragraph of text is layed out in terms of a number of attributes. The following attributes are supported: 

   - `alignment`: Horizontal text alignment mode. Supported are: `left`, `right`, `center`, `justified`, and `natural`.
   - `first-head-indent`: Distance in points from the leading margin of a text container to the beginning of the paragraph’s first line.
   - `head-indent`: Distance in points from the leading margin of a text container to the beginning of lines other than the first.
   - `tail-indent`: If positive, this is the distance from the leading margin (e.g. the left margin in left-to-right text). If 0 or negative, it’s the distance from the trailing margin. For example, a paragraph style designed to fit exactly in a container has a head indent of 0.0 and a tail indent of 0.0. One designed to fit with a quarter-inch margin has a head indent of 0.25 and a tail indent of –0.25.
   - `line-height-multiple`: Multiplier for the natural line height of the text (if positive), and constrains the resulting value by the minimum and maximum line height. The default value is 0.0.
   - `max-line-height`: This attribute defines the maximum height in points that any line in the paragraph occupies, regardless of the font size or size of any attached image. Default is 0 (= no constaint).
   - `min-line-height`: This attribute defines the minimum height in points that any line in the paragraph occupies, regardless of the font size or size of any attached image. Default is 0.
   - `line-spacing`: The distance in points between the bottom of one line fragment and the top of the next.
   - `paragraph-spacing-after`: Distance between the bottom of a paragraph and the top of the next. The layout algorithm determines the space between paragraphs by adding `paragraph-spacing-after` of the previous paragraph to the next paragraph’s `paragraph-spacing-before`.
   - `paragraph-spacing-before`: The distance between the paragraph’s top and the beginning of its text content.
   - `tab-interval`: The default tab interval in points. Tabs after the last specified tab stops are placed at multiples of this distance (if positive). Default is 0.0.
   - `text-fit-mode`: Attribute that specifies what happens when a line is too long for a container. Supported are: `word-wrap`, `char-wrap`, `clip`, `truncate`, `truncate-head`, and `truncate-tail`.
   - `push-out-line-break`: Boolean value that, if set to `#t`, makes the line layout algorithm push out individual lines to avoid an orphan word on the last line of a paragraph.
   - `hypenation-factor`: A paragraph’s threshold for hyphenation. The line layout algorithm attempts hyphenation when the ratio of the text width (as broken without hyphenation) to the width of the line fragment is less than the hyphenation factor. Default is 0 (= system-defined hyphenation threshold).
   - `writing-direction`: Writing direction of a paragraph. Supported are: `natural` (automatic), `left-to-right`, and `right-to-left`.

Paragraph style objects are mutable. They encapsulate one value for each supported paragraph style attribute.

**(paragraph-style? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a paragraph style object _obj_; otherwise `#f` is returned.

**(make-paragraph-style _key value ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a paragraph style object encapsulating the given attributes provided as key/value pairs. The following attribute _key_ symbols are supported:

   - `alignment`: Horizontal text alignment mode; i.e. either `left`, `right`, `center`, `justified`, and `natural`.
   - `first-head-indent`: Distance in points from the leading margin of a text container to the beginning of the paragraph’s first line.
   - `head-indent`: Distance in points from the leading margin of a text container to the beginning of lines other than the first.
   - `tail-indent`: Distance from the leading margin if positive. If 0 or negative, it’s the distance from the trailing margin.
   - `line-height-multiple`: Multiplier for the natural line height of the text.
   - `max-line-height`: Defines the maximum height in points that any line in the paragraph occupies.
   - `min-line-height`: Defines the minimum height in points that any line in the paragraph occupies.
   - `line-spacing`: The distance in points between the bottom of one line fragment and the top of the next.
   - `paragraph-spacing-after`: Distance between the bottom of a paragraph and the top of the next.
   - `paragraph-spacing-before`: The distance between the paragraph’s top and the beginning of its text content.
   - `tab-interval`: The default tab interval in points.
   - `text-fit-mode`: Specifies what happens when a line is too long; supported are: `word-wrap`, `char-wrap`, `clip`, `truncate`, `truncate-head`, and `truncate-tail`.
   - `push-out-line-break`: Boolean that, if set to `#t`, makes the line layout algorithm push out individual lines to avoid an orphan word on the last line of a paragraph.
   - `hypenation-factor`: A paragraph’s threshold for hyphenation.
   - `writing-direction`: Writing direction of a paragraph; supported are: `natural`, `left-to-right`, and `right-to-left`.

**(copy-paragraph-style _pstyle_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of _pstyle_.

**(paragraph-style=? _pstyle pstyle0 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if all _pstyle0 ..._ are equal to _pstyle_; otherwise `#f` is returned.

**(paragraph-style-ref _pstyle key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the value associated with paragraph style attribute _key_.

**(paragraph-style-set! _pstyle key value_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the paragraph style attribute _key_ to _value_ for the paragraph style object _pstyle_.

**(paragraph-style-tabstops _pstyle_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of tab stops for the given paragraph style. Each tab stop is represented as a pair consisting of a location in points and a text alignment, i.e. either `left`, `right`, `center`, `justified`, and `natural`.

**(paragraph-style-tabstop-add! _pstyle loc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(paragraph-style-tabstop-add! _pstyle loc align_)**  
**(paragraph-style-tabstop-add! _pstyle loc align cs_)**  

Adds a new tab stop to paragraph style _pstyle_. _loc_ is the location of the tab stop in points, _align_ is the alignment of the text at the location (e.g. one of `left`, `right`, `center`, `justified`, and `natural`), and _cs_ is a char-set object that is used to determine the terminating character for a tab column. The tab and newline characters are implied even if they don’t exist in the character set. The default for _align_ is `natural`.

**(paragraph-style-tabstop-remove! _pstyle loc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(paragraph-style-tabstop-remove! _pstyle loc align_)**  

Removes the tab stop in _pstyle_ at the given location _loc_ and using the provided text alignment _align_ (default is `natural`).

**(paragraph-style-tabstops-clear! _pstyle_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes all tab stops from paragraph style _pstyle_.
