# LispKit Markdown

Library `(lispkit markdown)` provides an API for programmatically constructing [Markdown](https://daringfireball.net/projects/markdown/) documents, for parsing strings in Markdown format, as well as for mapping Markdown documents into corresponding HTML. The Markdown syntax supported by this library is based on the [CommonMark Markdown](https://commonmark.org) specification.

## Data Model

Markdown documents are represented using an abstract syntax that is implemented by three algebraic datatypes `block`, `list-item`, and `inline`, via `define-datatype` of library `(lispkit datatype)`.

### Blocks

At the top-level, a Markdown document consist of a list of _blocks_. The following recursively defined datatype shows all the supported block types as variants of type `block`.

```scheme
(define-datatype block markdown-block?
  (document blocks)
      where (markdown-blocks? blocks)
  (blockquote blocks)
      where (markdown-blocks? blocks)
  (list-items start tight items)
      where (and (opt fixnum? start) (markdown-list? items))
  (paragraph text)
      where (markdown-text? text)
  (heading level text)
      where (and (fixnum? level) (markdown-text? text))
  (indented-code lines)
      where (every? string? lines)
  (fenced-code lang lines)
      where (and (opt string? lang) (every? string? lines))
  (html-block lines)
      where (every? string? lines)
  (reference-def label dest title)
      where (and (string? label) (string? dest) (every? string? title))
  (table header alignments rows)
      where (and (every? markdown-text? header)
                 (every? symbol? alignments)
                 (every? (lambda (x) (every? markdown-text? x)) rows))
  (definition-list defs)
      where (every? (lambda (x)
                      (and (markdown-text? (car x))
                           (markdown-list? (cdr x)))) defs)
  (thematic-break))
```

`(document blocks)` represents a full Markdown document consisting of a list of blocks. `(blockquote blocks)` represents a blockquote block which itself has a list of sub-blocks. `(list-items start tight items)` defines either a bullet list or an ordered list. _start_ is `#f` for bullet lists and defines the first item number for ordered lists. _tight_ is a boolean which is `#f` if this is a loose list (with vertical spacing between the list items). _items_ is a list of list items of type `list-item` as defined as follows:

```scheme
(define-datatype list-item markdown-list-item?
  (bullet ch tight? blocks)
      where (and (char? ch) (markdown-blocks? blocks))
  (ordered num ch tight? blocks)
      where (and (fixnum? num) (char? ch) (markdown-blocks? blocks)))
```

The most frequent Markdown block type is a paragraph. `(paragraph text)` represents a single paragraph of text where _text_ refers to a list of inline text fragments of type `inline` (see below). `(heading level text)` defines a heading block for a heading of a given level, where _level_ is a number starting with 1 (up to 6). `(indented-code lines)` represents a code block consisting of a list of text lines each represented by a string. `(fenced-code lang lines)` is similar: it defines a code block with code expressed in the given language _lang_. `(html lines)` defines a HTML block consisting of the given lines of text. `(reference-def label dest title)` introduces a reference definition consisting of a given _label_, a destination URI _dest_, as well as a _title_ string. `(table header alignments rows)` defines a table consisting of _headers_, a list of markdown text describing the header of each column, _alignments_, a list of symbols `l` (= left), `c` (= center), and `r` (= right), and _rows_, a list of lists of markdown text. `(definition-list defs)` represents a definition list where _defs_ refers to a list of definitions. A definition has the form `(name def ...)` where _name_ is markdown text defining a name, and _def_ is a bullet item using `:` as bullet character. Finally, `(thematic-break)` introduces a thematic break block separating the previous and following blocks visually, often via a line.

### Inline Text

Markdown text is represented as lists of inline text segments, each represented as an object of type `inline`. `inline` is defined as follows:

```scheme
(define-datatype inline markdown-inline?
  (text str)
      where (string? str)
  (code str)
      where (string? str)
  (emph text)
      where (markdown-text? text)
  (strong text)
      where (markdown-text? text)
  (link text uri title)
      where (and (markdown-text? text) (string? uri) (string? title))
  (auto-link uri)
      where (string? uri)
  (email-auto-link email)
      where (string? uri)
  (image text uri title)
      where (and (markdown-text? text) (string? uri) (string? title))
  (html tag)
      where (string? tag)
  (line-break hard?))
```

`(text str)` refers to a text segment consisting of string _str_. `(code str)` refers to a code string _str_ (often displayed as verbatim text). `(emph text)` represents emphasized _text_ (often displayed as italics). `(strong text)` represents _text_ in boldface. `(link text uri title)` represents a hyperlink with _text_ linking to _uri_ and _title_ representing a title for the link. `(auto-link uri)` is a link where _uri_ is both the text and the destination URI. `(email-auto-link email)` is a "mailto:" link to the given email address _email_. `(image text uri title)` inserts an image at _uri_ with image description _text_ and image link title _title_. `(html tag)` represents a single HTML tag of the form `<`_tag_`>`. Finally, `(line-break #f)` introduces a "soft line break", whereas `(line-break #t)` inserts a "hard line break".

## Creating Markdown documents

Markdown documents can either be constructed programmatically via the datatypes introduced above, or a string representing a Markdown documents gets parsed into the internal abstract syntax representation via function `markdown`.

For instance, `(markdown "# My title\n\nThis is a paragraph.")` returns a markdown document consisting of two blocks: a _header block_ for header "My title" and a _paragraph block_ for the text "This is a paragraph":

```scheme
(markdown "# My title\n\nThis is a paragraph.")
⟹ #block:(document (#block:(heading 1 (#inline:(text "My title"))) #block:(paragraph (#inline:(text "This is a paragraph.")))))
```

The same document can be created programmatically in the following way:

```scheme
(document
  (list
    (heading 1 (list (text "My title")))
    (paragraph (list (text "This is a paragraph.")))))
⟹ #block:(document (#block:(heading 1 (#inline:(text "My title"))) #block:(paragraph (#inline:(text "This is a paragraph.")))))
```

## Processing Markdown documents

Since the abstract syntax of Markdown documents is represented via algebraic datatypes, pattern matching can be used to deconstruct the data. For instance, the following function returns all the top-level headers of a given Markdown document:

```scheme
(import (lispkit datatype))  ; this is needed to import `match`

(define (top-headings doc)
  (match doc
    ((document blocks)
      (filter-map (lambda (block)
                    (match block
                      ((heading 1 text) (text->raw-string text))
                      (else #f)))
                  blocks))))
```

An example for how `top-headings` can be applied to this Markdown document:

```markdown
# *header* 1
Paragraph.
# __header__ 2
## header 3
The end.
```

is shown here:

```scheme
(top-headings (markdown "# *header* 1\nParagraph.\n# __header__ 2\n## header 3\nThe end."))
⟹  ("header 1" "header 2")
```

## API

**block-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the markdown `block` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all block objects.

**list-item-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the markdown `list-item` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all list item objects.

**inline-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the markdown `inline` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all inline objects.

**(markdown-blocks? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a proper list of objects _o_ for which `(markdown-block? ` _`o`_`)` returns `#t`; otherwise it returns `#f`.

**(markdown-block? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a variant of algebraic datatype `block`.

**(markdown-block=? _lhs rhs_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if markdown blocks _lhs_ and _rhs_ are equals; otherwise it returns `#f`.

**(markdown-list? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a proper list of list items _i_ for which `(markdown-list-item? ` _`i`_`)` returns `#t`; otherwise it returns `#f`.

**(markdown-list-item? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a variant of algebraic datatype `list-item`.

**(markdown-list-item=? _lhs rhs_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if markdown list items _lhs_ and _rhs_ are equals; otherwise it returns `#f`.

**(markdown-text? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a proper list of objects _o_ for which `(markdown-inline? ` _`o`_`)` returns `#t`; otherwise it returns `#f`.

**(markdown-inline? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a variant of algebraic datatype `inline`.

**(markdown-inline=? _lhs rhs_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if markdown inline text _lhs_ and _rhs_ are equals; otherwise it returns `#f`.

**(markdown? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a valid markdown document, i.e. an instance of the `document` variant of datatype `block`; returns `#f` otherwise.

**(markdown=? _lhs rhs_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if markdown documents _lhs_ and _rhs_ are equals; otherwise it returns `#f`.

**(markdown _str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Parses the text in Markdown format in _str_ and returns a representation of the abstract syntax using the algebraic datatypes `block`, `list-item`, and `inline`.

**(markdown-\>html _md_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Converts a Markdown document _md_ into HTML, represented in form of a string. _md_ needs to satisfy the _markdown?_ predicate. 

**(blocks-\>html _bs_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(blocks-\>html _bs tight_)**   

Converts a Markdown block or list of blocks _bs_ into HTML, represented in form of a string. _tight?_ is a boolean and should be set to true if the conversion should consider tight typesetting (see CommonMark specification for details).

**(text-\>html _txt_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Converts Markdown inline text or list of inline texts _txt_ into HTML, represented in form of a string.

**(markdown-\>html-doc _md_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(markdown-\>html-doc _md style_)**  
**(markdown-\>html-doc _md style codestyle_)**  
**(markdown-\>html-doc _md style codestyle cblockstyle_)**  
**(markdown-\>html-doc _md style codestyle cblockstyle colors_)**  

Converts a Markdown document _md_ into a styled HTML document, represented in form of a string. _md_ needs to satisfy the _markdown?_ predicate. _style_ is a list with up to three elements: _(size font color)_. It specifies the default text style of the document. _size_ is the point size of the font, _font_ is a font name, and _color_ is a HTML color specification (e.g. `"#FF6789"`). _codestyle_ specifies the style of inline code in the same format. _colors_ is a list of HTML color specifications for the following document elements in this order: the border color of code blocks, the color of  blockquote "bars", the color of H1, H2, H3 and H4 headers.

**(markdown-\>sxml _md_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Converts a Markdown document _md_ into SXML representation. _md_ needs to satisfy the _markdown?_ predicate.

**(blocks-\>sxml _bs_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(blocks-\>sxml _bs tight_)**   

Converts a Markdown block or list of blocks _bs_ into SXML representation. _tight?_ is a boolean and should be set to true if the conversion should consider tight typesetting (see CommonMark specification for details).

**(text-\>sxml _txt_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Converts Markdown inline text or a list of inline text objects _txt_ into SXML representation.

**(markdown-\>debug-string _md_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Converts a Markdown document _md_ into a debug string representation showing the internal structure. _md_ needs to satisfy the `markdown?` predicate.

**(markdown-\>raw-string _md_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Converts a Markdown document _md_ into raw text, a string ignoring any markup. _md_ needs to satisfy the _markdown?_ predicate.

**(blocks-\>raw-string _bs_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Converts a Markdown block or list of blocks _bs_ into raw text, a string ignoring any markup.

**(text-\>raw-string _text_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Converts given inline text _text_ into raw text, a string representation ignoring any markup in _text_. _text_ needs to satisfy the _markdown-text?_ predicate.

**(text-\>string _text_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Converts given inline text _text_ into a string representation which encodes markup in _text_ using Markdown syntax. _text_ needs to satisfy the _markdown-text?_ predicate.
