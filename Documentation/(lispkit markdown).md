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
      where (and (fixnum? num) (char? ch) (markdown-blocks? blocks))
  (task checked? item)
      where (and (boolean? checked?) (markdown-list-item? item)))
```

`(bullet ch tight? blocks)` is an item of a bullet list with bullet character _ch_ and `(ordered num ch tight? blocks)` is an item of an ordered list with number _num_ and delimiter character _ch_ (`.` or `)`). `(task checked? item)` turns a `bullet` or `ordered` list item _item_ into a _task list item_ (written `- [ ] to do` or `- [x] done` in Markdown) with a checkbox that is checked if _checked?_ is true. A task list item is never nested in another one. Task list items are only created when parsing Markdown with the _full_ parser (see `markdown` below).

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
  (underline text)
      where (markdown-text? text)
  (strikethrough text)
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

`(text str)` refers to a text segment consisting of string _str_. `(code str)` refers to a code string _str_ (often displayed as verbatim text). `(emph text)` represents emphasized _text_ (often displayed as italics). `(strong text)` represents _text_ in boldface. `(underline text)` represents underlined _text_ and `(strikethrough text)` represents _text_ that is struck through; both are only created when parsing Markdown with the _full_ parser (see `markdown` below). `(link text uri title)` represents a hyperlink with _text_ linking to _uri_ and _title_ representing a title for the link. `(auto-link uri)` is a link where _uri_ is both the text and the destination URI. `(email-auto-link email)` is a "mailto:" link to the given email address _email_. `(image text uri title)` inserts an image at _uri_ with image description _text_ and image link title _title_. `(html tag)` represents a single HTML tag of the form `<`_tag_`>`. Finally, `(line-break #f)` introduces a "soft line break", whereas `(line-break #t)` inserts a "hard line break".

## Creating Markdown documents

Markdown documents can either be constructed programmatically via the datatypes introduced above, or a string representing a Markdown documents gets parsed into the internal abstract syntax representation via function `markdown`.

For instance, `(markdown "# My title\n\nThis is a paragraph.")` returns a markdown document consisting of two blocks: a _header block_ for header "My title" and a _paragraph block_ for the text "This is a paragraph":

```scheme
(markdown "# My title\n\nThis is a paragraph.")
⇒  #block:(document (#block:(heading 1 (#inline:(text "My title"))) #block:(paragraph (#inline:(text "This is a paragraph.")))))
```

The same document can be created programmatically in the following way:

```scheme
(document
  (list
    (heading 1 (list (text "My title")))
    (paragraph (list (text "This is a paragraph.")))))
⇒  #block:(document (#block:(heading 1 (#inline:(text "My title"))) #block:(paragraph (#inline:(text "This is a paragraph.")))))
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
⇒  ("header 1" "header 2")
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
**(markdown _str extended_)**   

Parses the text in Markdown format in _str_ and returns a representation of the abstract syntax using the algebraic datatypes `block`, `list-item`, and `inline`.

The optional argument _extended_ selects the Markdown dialect that is parsed:

- `#f` (the default) parses standard [CommonMark](https://commonmark.org).
- `#t` additionally supports tables and definition lists.
- The symbol `full` supports everything supported by `#t`, and in addition underlined text (`~text~`), text that is struck through (`~~text~~`), and task lists (`- [ ] to do` and `- [x] done`). The resulting document may contain `underline` and `strikethrough` inline elements as well as `task` list items.

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
**(markdown-\>html-doc _md style codestyle cblockstyle colors syntax_)**  

Converts a Markdown document _md_ into a styled HTML document, represented in form of a string. _md_ needs to satisfy the _markdown?_ predicate. _style_ is a list with up to three elements: _(size font color)_. It specifies the default text style of the document. _size_ is the point size of the font, _font_ is a font name, and _color_ is a HTML color specification (e.g. `"#FF6789"`). _codestyle_ specifies the style of inline code in the same format. _colors_ is a list of HTML color specifications for the following document elements in this order: the border color of code blocks, the color of  blockquote "bars", the color of H1, H2, H3 and H4 headers. Any of _style_, _codestyle_, _cblockstyle_, and _colors_ can be set to `#f` to use the default for that argument.

_syntax_ configures syntax highlighting for code blocks and is a list with up to four elements: _(theme ignore-syntax-errors? ignored-languages highlight-indented-code-blocks?)_. _theme_ is the name of a syntax highlighting theme (a string), or `#f` to use the default theme. _ignore-syntax-errors?_ is a boolean determining whether syntactic issues in the code being highlighted should be ignored (default: `#t`). _ignored-languages_ is a list of language name strings for which syntax highlighting is skipped. _highlight-indented-code-blocks?_ is a boolean determining whether indented (as opposed to fenced) code blocks are syntax-highlighted (default: `#t`). If _syntax_ is omitted or `#f`, default syntax highlighting is used. If _syntax_ is `#t` or `'()`, syntax highlighting is disabled entirely.

**(markdown-\>string _md_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(markdown-\>string _md width_)**  
**(markdown-\>string _md width ansi_)**  

Converts a Markdown document _md_ into a plain-text string suitable for display on a terminal. _md_ needs to satisfy the _markdown?_ predicate. _width_ specifies the number of columns available for typesetting; if _width_ is omitted or `#f`, the current terminal size (as determined by `terminal-size`) is used if available, falling back to 80 columns otherwise.

If _ansi_ is omitted or `#f`, the output is plain text without ANSI escape sequences. If _ansi_ is `#t`, the output uses ANSI escape sequences with default styling for headers, links, code, emphasis, etc. so that the result can be printed to an ANSI-compatible terminal with visual markup. Alternatively, _ansi_ can be a list configuring the styling and colors used for each document element, as well as syntax highlighting for code blocks. This configuration list has up to fourteen elements, all of which are optional and can be omitted from the end of the list, or set to `#f` individually to fall back to the corresponding default:

1. _header-properties_: a list of text properties (see below), one for each header level, e.g. `(h1-properties h2-properties ...)`. Headers for which no entry is provided are typeset using the properties of the last provided entry, or without styling if the list is empty.
2. _link-properties_: text properties used for typesetting link text.
3. _code-properties_: text properties used for typesetting inline code.
4. _code-block-border-properties_: text properties used for the border framing indented and fenced code blocks.
5. _code-block-lang-properties_: text properties used for typesetting the language tag of fenced code blocks.
6. _emphasis-properties_: text properties used for emphasized text (e.g. `*this*`).
7. _strong-properties_: text properties used for strongly emphasized text (e.g. `**this**`).
8. _def-term-properties_: text properties used for typesetting the term of a definition list entry.
9. _def-descr-properties_: text properties used for typesetting the description of a definition list entry.
10. _blockquote-properties_: text properties used for typesetting block quotes.
11. _break-properties_: text properties used for typesetting thematic breaks (horizontal rules).
12. _syntax-highlighting_: configuration for syntax-highlighting the content of code blocks. `#f` disables syntax highlighting entirely; omitting this element (or ending the list before it) enables syntax highlighting with default settings. Otherwise, this is a list with up to five elements _(theme ignore-syntax-errors? ignored-languages highlight-indented-code-blocks? full-color?)_, all of which are optional: _theme_ is the name of a syntax highlighting theme (a string), or `#f` for the default theme; _ignore-syntax-errors?_ is a boolean determining whether syntactic issues in the code being highlighted should be ignored (default: `#t`); _ignored-languages_ is a list of language name strings for which syntax highlighting is skipped; _highlight-indented-code-blocks?_ is a boolean determining whether indented (as opposed to fenced) code blocks are syntax-highlighted (default: `#t`); _full-color?_ is a boolean determining whether full RGB colors are used for highlighting as opposed to the limited, extended ANSI color palette (default: `#t`).
13. _underline-properties_: text properties used for underlined text (e.g. `~this~`, supported by the _full_ Markdown parser). The default is `underline`. Since this element follows _syntax-highlighting_, specify `#t` for the latter to keep syntax highlighting enabled with default settings.
14. _strikethrough-properties_: text properties used for text that is struck through (e.g. `~~this~~`, supported by the _full_ Markdown parser). The default is `strikethrough`.

Each _properties_ argument above is a "text properties" value in one of the following formats:

- `#f` for using default styling.
- `'()` for no styling. 
- a symbol naming a predefined style or color; supported are: `default, bold, dim, italic, underline, blink, swap, strikethrough, default-color, black, maroon, green, olive, navy, purple, teal, silver, grey, red, lime, yellow, blue, fuchsia, aqua`, and `white`.
- a list _(text-color background-color style ...)_ of up to three elements, where _text-color_ and _background-color_ define the foreground and background color, and the remaining elements are style symbols; supported styles are `bold`, `italic`, `underline`, `dim`, `blink`, `swap`, `strikethrough`, and `default` for the terminal's default style).

_text-color_ can be `#f` (default color), a color name symbol (`default`, `black`, `maroon`, `green`, `olive`, `navy`, `purple`, `teal`, `silver`, `grey`, `red`, `lime`, `yellow`, `blue`, `fuchsia`, `aqua`, or `white`), a hex color string (e.g. `"#F67"` or `"#FF6677"`), a color object as created by library `(lispkit draw)`, or a fixnum between 0 and 255 referring to a color of the extended ANSI 256-color palette. _background-color_ uses the same formats, except that its color name symbols are `default`, `black`, `white`, `red`, `green`, `yellow`, `blue`, `magenta`, `cyan`, `light-black`, `light-red`, `light-green`, `light-yellow`, `light-blue`, `light-magenta`, `light-cyan`, and `light-white`.

**(markdown-\>styled-text _md_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(markdown-\>styled-text _md style_)**  
**(markdown-\>styled-text _md style codestyle_)**  
**(markdown-\>styled-text _md style codestyle cblockstyle_)**  
**(markdown-\>styled-text _md style codestyle cblockstyle colors_)**  
**(markdown-\>styled-text _md style codestyle cblockstyle colors syntax_)**  

Converts a Markdown document _md_ into a `styled-text` object (see library `(lispkit draw)`), e.g. for rendering it with `draw-styled-text`. _md_ needs to satisfy the _markdown?_ predicate. The arguments _style_, _codestyle_, _cblockstyle_, _colors_, and _syntax_ have the same meaning as for `markdown->html-doc`. Returns `#f` if the styled text object could not be created.

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

**(syntax-highlighting-theme _name-or-spec_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the CSS associated with the theme _name-or-spec_. _name-or-spec_ is a string that either refers to the name of the theme, or it is the CSS directly. This makes it possible to use dynamically created CSS code for highlighting purposes.

**(syntax-highlighting-themes)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a list of syntax highlighting theme names that can be used with procedures supporting syntax highlighting.

```scheme
(syntax-highlighting-themes)
⇒ ("zenburn" "xt256" "xcode-dusk" "xcode" "vulcan" "vs2015" "vs"
   "tomorrow-night-eighties" "tomorrow-night-bright" "tomorrow-night-blue"
   "tomorrow-night" "tomorrow" "sunburst" "stackoverflow-light"
   "stackoverflow-dark" "srcery" "solarized-light" "solarized-dark"
   "snazzy" "silk-light" "silk-dark" "shades-of-purple" "school-book"
   "routeros" "rainbow" "railscasts" "qtcreator_light" "qtcreator_dark"
   "purebasic" "pojoaque" "paraiso-light" "paraiso-dark" "ocean"
   "obsidian" "nord" "nnfx-light" "nnfx-dark" "night-owl"
   "monokai-sublime" "monokai" "mono-blue" "markdownkit-dark"
   "markdownkit" "magula" "lioshi" "lightfair" "kimbie-light"
   "kimbie-dark" "isbl-editor-light" "isbl-editor-dark"
   "ir-black" "idea" "hybrid" "hopscotch" "gruvbox-light"
   "gruvbox-dark" "grayscale" "gradient-light" "gradient-dark"
   "googlecode" "gml" "github-gist" "github-dark" "github"
   "foundation" "far" "dracula" "docco" "default" "dark" "darcula"
   "color-brewer" "codepen-embed" "brown-paper" "atom-one-light"
   "atom-one-dark-reasonable" "atom-one-dark" "atelier-sulphurpool-light"
   "atelier-sulphurpool-dark" "atelier-seaside-light"
   "atelier-seaside-dark" "atelier-savanna-light" "atelier-savanna-dark"
   "atelier-plateau-light" "atelier-plateau-dark"
   "atelier-lakeside-light" "atelier-lakeside-dark"
   "atelier-heath-light" "atelier-heath-dark" "atelier-forest-light"
   "atelier-forest-dark" "atelier-estuary-light" "atelier-estuary-dark"
   "atelier-dune-light" "atelier-dune-dark" "atelier-cave-light"
   "atelier-cave-dark" "ascetic" "arta" "arduino-light" "androidstudio"
   "an-old-hope" "agate" "a11y-light" "a11y-dark")
```
