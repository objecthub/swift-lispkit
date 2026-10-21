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
**(markdown-\>html-doc _md config_)**  

Converts a Markdown document _md_ into a styled HTML document, represented in form of a string. _md_ needs to satisfy the _markdown?_ predicate. _config_ is an association list that configures the style of the document, as well as how images and raw HTML get handled; see "Configuring styled documents" below. If _config_ is omitted or `#f`, the default configuration is used. The options for fonts, colors, syntax highlighting, images, and custom styles apply to the generated HTML. The options `timeout` and `text-size-multiplier` only matter for `markdown->styled-text`.

**(markdown-style-config)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(markdown-style-config _style_)**  
**(markdown-style-config _style codestyle_)**  
**(markdown-style-config _style codestyle cblockstyle_)**  
**(markdown-style-config _style codestyle cblockstyle colors_)**  
**(markdown-style-config _style codestyle cblockstyle colors syntax_)**  

Returns a configuration (see "Configuring styled documents" below) for `markdown->html-doc` and `markdown->styled-text` built from positional arguments. This procedure eases porting code from earlier versions of this library, where `markdown->html-doc` and `markdown->styled-text` had these positional arguments. A call `(markdown->styled-text md style codestyle cblockstyle colors syntax)` is replaced by `(markdown->styled-text md (markdown-style-config style codestyle cblockstyle colors syntax))`. Any argument can be omitted or set to `#f` to use the default for that argument.

- _style_ is a list with up to three elements: _(size font color)_. It specifies the default text style of the document. _size_ is the point size of the font, _font_ is a font name, and _color_ is a HTML color specification (e.g. `"#FF6789"`).
- _codestyle_ specifies the style of inline code in the same format.
- _cblockstyle_ specifies the style of code blocks as a list _(size color background)_ of up to three elements: the point size of the font, the text color, and the background color.
- _colors_ is a list of HTML color specifications for the following document elements in this order: the border color of code blocks, the color of blockquote "bars", the color of H1, H2, H3 and H4 headers.
- _syntax_ configures syntax highlighting for code blocks and is a list with up to four elements: _(theme ignore-syntax-errors? ignored-languages highlight-indented-code-blocks?)_. If _syntax_ is `#f`, default syntax highlighting is used. If _syntax_ is `#t` or `()`, syntax highlighting is disabled entirely.

```scheme
(markdown-style-config '(16 "Helvetica" "#222222") #f #f #f '("xcode"))
⇒  ((font-size . 16) (font-family . "Helvetica") (font-color . "#222222")
    (syntax-highlighting (theme . "xcode")))
```

### Configuring styled documents

The style of the documents generated by `markdown->html-doc` and `markdown->styled-text` is configured by an association list _config_ with symbol keys. All entries are optional. `#f` and `()` stand for the default configuration, and if a key appears more than once, the first entry for the key is used. This makes it possible to override entries of a base configuration by adding entries in front of it, e.g. `(cons '(safe-mode . #t) base-config)`. Unknown keys and values of the wrong type result in an error. The following keys are supported; for keys with `#f` as a possible value, the value `#f` selects the default:

| Key | Value | Default |
|---|---|---|
| `style` | `default`, `light`, or `dark`; the preset that the other keys refine. `default` has fonts and colors of the system, `light` has colors for a white background, and `dark` has colors for a dark background; the preset also determines the theme for syntax highlighting, which `syntax-highlighting` refines. The defaults of the keys listed below are those of preset `light`. | `light` |
| `font-size`, `font-family`, `font-color` | point size (a number), font name (a string), and HTML color (a string) of the text | `14`, `"system-ui, -apple-system, 'Helvetica Neue', Helvetica, sans-serif"`, `"#000000"` |
| `line-spacing` | the line height of text as a multiple of the line height of its font; `1` or less keeps the line height of the font | `1.1` |
| `code-font-size`, `code-font-family`, `code-font-color` | the same for inline code | `13`, `"'SF Mono', SFMono-Regular, ui-monospace, Menlo, Consolas, monospace"`, `"#0000AA"` |
| `code-block-font-size`, `code-block-font-color`, `code-block-background` | point size, text color and background color of code blocks | `12`, `"#000099"`, `"#EEE"` |
| `code-block-line-spacing` | the same as `line-spacing` for the lines of code blocks | `1` |
| `border-color`, `blockquote-color` | HTML colors of the border of code blocks and of the bars of blockquotes | `"#DDD"`, `"#BBE"` |
| `h1-color`, `h2-color`, `h3-color`, `h4-color` | HTML colors of headings | `"#aa1111"`, `"#003399"`, `"#0055CC"`, `"#007AFF"` |
| `h1-font-size`, `h2-font-size`, `h3-font-size`, `h4-font-size` | point sizes of headings; `#f` stands for 6, 4, 2 and 1 points more than `font-size` | `#f` |
| `header-font` | font family of headings, a font name or a CSS font list, or `#f` for the font of the text | `#f` |
| `syntax-highlighting` | `#f` disables syntax highlighting, `#t` enables it with default settings, an association list with the optional keys `theme` (a theme name or CSS), `ignore-syntax-errors` (boolean), `ignored-languages` (list of strings), and `highlight-indented-code-blocks` (boolean) configures it; see `markdown->string` | enabled |
| `tight-lists` | boolean; if `#t`, the paragraphs of the items of loose lists are shown without paragraph spacing, too | `#f` |
| `align-list-markers` | boolean; if `#t`, the numbers and bullets of list items are put on the baseline of the first line of their items, also if an image, such as a formula, makes that line taller than the text. Only used by `markdown->styled-text` | `#f` |
| `max-image-width`, `max-image-height` | a CSS length (e.g. `"100%"` or `"300px"`) limiting the size of images, or `#f` | no limit |
| `custom-style` | a string with CSS rules that get added to the style sheet of the document | `""` |
| `image-base-url` | a URL or the path of a directory that relative image paths of the Markdown text are resolved against, or `#f` | the value of `base-url` |
| `preset` | `trusted` or `untrusted`; defines the default for the following options. `untrusted` is meant for Markdown that does not come from a trusted source: raw HTML is omitted, links are limited to `http`, `https` and `mailto`, and no images are loaded | `trusted` |
| `base-url` | a URL or the path of a directory that relative URLs are resolved against, or `#f` | `#f` |
| `local-images` | `none`, `any`, or the path of a directory. Controls which images stored in the local file system are loaded. Images that are not loaded are replaced by their alternative text. A directory restricts images to files inside of this directory. | `any` |
| `remote-images` | `none`, `any`, or an `http` or `https` URL. Controls which images are loaded from the web. A URL restricts images to the web site and path of the URL. | `any` |
| `image-extensions` | a list of path extensions (without dot) that the URLs of loaded images need to have | png, jpg, jpeg, gif, tif, tiff, bmp, ico, heic, heif, webp |
| `safe-mode` | boolean; if `#t`, raw HTML is omitted and links are limited to `http`, `https` and `mailto` | `#f` |
| `timeout` | the maximum time in seconds that `markdown->styled-text` may take for rendering a document (including loading images), or `#f` for no limit | `30` |
| `text-size-multiplier` | a number scaling font sizes, or `#f` | `#f` |
| `mermaid` | `#f`, `#t`, or an association list configuring Mermaid diagrams; only used by `markdown->styled-text` (see "Mermaid diagrams") | `#f` |

Paths are interpreted relative to the current directory, and strings starting with `http:`, `https:` or `file:` are URLs. The restrictions on images do not apply to images referred to by raw HTML or by `custom-style`.

```scheme
(markdown->styled-text
  (markdown "# Title\n\n![Logo](logo.png)")
  '((font-family . "Helvetica")
    (h1-color . "#cc3300")
    (max-image-width . "50%")
    (image-base-url . "images")
    (remote-images . none)))
```

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
**(markdown-\>styled-text _md config_)**  

Converts a Markdown document _md_ into a `styled-text` object (see library `(lispkit draw)`), e.g. for rendering it with `draw-styled-text`. _md_ needs to satisfy the _markdown?_ predicate. _config_ is an association list with the same meaning as for `markdown->html-doc`; see "Configuring styled documents" above.

The following style keys are supported by _config_ (for a full list, see above). Defaults are in parentheses; all keys are optional, and `#f` selects the default for keys that do not list `#f` as a value:

- `style`: preset `default`, `light` or `dark` that the other keys refine (see above); the defaults below are those of preset `light`, which is used if there is no preset (`light`)
- `font-size`: point size of the text (`14`)
- `font-family`: font family of the text, a font name or a CSS font list (`"system-ui, -apple-system, 'Helvetica Neue', Helvetica, sans-serif"`)
- `font-color`: HTML color of the text (`"#000000"`)
- `line-spacing`: line height of text as a multiple of the line height of its font (`1.1`)
- `code-font-size`: point size of inline code (`13`)
- `code-font-family`: font family of inline code (`"'SF Mono', SFMono-Regular, ui-monospace, Menlo, Consolas, monospace"`)
- `code-font-color`: HTML color of inline code (`"#0000AA"`)
- `code-block-font-size`: point size of code blocks (`12`)
- `code-block-font-color`: HTML color of the text of code blocks (`"#000099"`)
- `code-block-line-spacing`: line height of the lines of code blocks as a multiple of the line height of their font (`1`)
- `code-block-background`: HTML color of the background of code blocks (`"#EEE"`)
- `border-color`: HTML color of the border around code blocks (`"#DDD"`)
- `blockquote-color`: HTML color of the bars next to block quotes (`"#BBE"`)
- `h1-color`, `h2-color`, `h3-color`, `h4-color`: HTML colors of headings of levels 1 to 4 (`"#aa1111"`, `"#003399"`, `"#0055CC"`, `"#007AFF"`)
- `h1-font-size`, `h2-font-size`, `h3-font-size`, `h4-font-size`: point sizes of headings of levels 1 to 4, or `#f` for 6, 4, 2 and 1 points more than `font-size` (`#f`)
- `header-font`: font family of headings, a font name or a CSS font list, or `#f` for the font of the text (`#f`)
- `syntax-highlighting`: `#f` disables syntax highlighting of code blocks, `#t` enables it with default settings; an association list with the optional keys `theme` (theme name or CSS), `ignore-syntax-errors` (boolean), `ignored-languages` (list of strings), and `highlight-indented-code-blocks` (boolean) configures it (enabled with default settings)
- `tight-lists`: if `#t`, the paragraphs of list items are shown without paragraph spacing, also in loose lists (`#f`)
- `align-list-markers`: if `#t`, the numbers and bullets of list items are put on the baseline of the first line of their items, also if an image makes that line taller than the text (`#f`)
- `max-image-width`: CSS length (e.g. `"100%"` or `"300px"`) limiting the width of images, or `#f` (no limit)
- `max-image-height`: CSS length limiting the height of images, or `#f` (no limit)
- `image-base-url`: URL or path of a directory against which relative image paths of the Markdown text are resolved, or `#f` (the value of `base-url`)
- `base-url`: URL or path of a directory against which relative URLs are resolved, or `#f` (`#f`)
- `local-images`: `none`, `any`, or the path of a directory; controls which images from the local file system are loaded, images that are not loaded are replaced by their alternative text (`any`)
- `remote-images`: `none`, `any`, or an `http` or `https` URL; controls which images from the web are loaded, a URL restricts images to its web site and path (`any`)
- `timeout`: maximum time in seconds for rendering the document, including loading images, or `#f` for no limit (`30`)
- `text-size-multiplier`: number by which font sizes are scaled, or `#f` (`#f`)
- `mermaid`: draws fenced code blocks with the language `mermaid` as diagrams; `#f` shows them as code, `#t` draws them with default settings, and an association list with the optional keys `theme`, `theme-variables`, `font-family`, `scale`, `width`, `margin`, `max-width`, `spacing`, and `timeout` configures the diagrams; see below (`#f`)

The styled text is generated asynchronously by the HTML rendering engine of the operating system, and `markdown->styled-text` waits for the result. Rendering a document therefore takes a moment, and images from the web are loaded unless `remote-images` is set to `none` (or a restricting URL). If the document cannot be rendered within the time given by `timeout`, or if rendering fails, an error is signaled. If this procedure is called by a thread other than the main thread of the application, the main thread needs to execute its run loop and must not wait for the calling thread. Otherwise, the procedure signals an error after the `timeout` has expired.

#### Mermaid diagrams

If `mermaid` is set, `markdown->styled-text` draws the content of fenced code blocks with the language `mermaid` as diagrams (see [mermaid.js.org](https://mermaid.js.org) for the diagram syntax). The diagrams are rendered to images by a headless web view using the Mermaid script that comes with LispKit; no network connection is needed. A diagram that Mermaid rejects stays a code block, preceded by a note with the reason. Rendered diagrams are cached, so showing a document again is fast. Diagrams are not supported by `markdown->html-doc`, which ignores `mermaid`, and the `timeout` of the diagrams (5 seconds by default) is independent of the `timeout` for rendering the document.

The value of `mermaid` is `#f`, `#t` (default settings), or an association list with the following optional keys; defaults are in parentheses:

- `theme`: Mermaid theme, one of `default`, `neutral`, `dark`, `forest`, and `base` (`default`)
- `theme-variables`: association list overriding variables of the theme, e.g. `((primaryColor . "#ffcc00"))`; names are symbols or strings, values are strings (`()`)
- `font-family`: font family of the text in diagrams, or `#f` for the font of Mermaid's theme (`#f`)
- `scale`: factor by which the resolution of the rendered image is higher than its size (`2`)
- `width`: width in points of the page that diagrams are laid out on (`800`)
- `margin`: margin in points around a diagram; `#f` keeps the margins that Mermaid adds, which depend on the diagram type (`8`)
- `max-width`: maximum width in points of a diagram in the text, or `#f` for the width of the text (`#f`)
- `spacing`: minimum distance in points between a diagram and the text above and below it (`16`)
- `timeout`: maximum time in seconds for rendering a single diagram (`5`)

```scheme
(markdown->styled-text
  (markdown "```mermaid\ngraph LR\n  A[Start] --> B{Choice}\n```")
  '((mermaid . ((theme . forest) (max-width . 400)))))
```

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
