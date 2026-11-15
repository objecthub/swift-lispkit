# SRFI 272: Color Support

Library `(srfi 272 colorize)` provides a framework for adding colors and text attributes to the output of the pretty printer, primarily for terminals supporting ANSI SGR (Select Graphic Rendition) control sequences. It provides facilities for detecting the color capabilities of a terminal, for generating SGR sequences for terminals of different capabilities, and for mapping semantic categories of syntax elements, like comments or keywords, to visual styles.

Libraries [`(srfi 272 advanced)`](<(srfi 272 advanced).md>) and [`(srfi 272 fancy)`](<(srfi 272 fancy).md>) accept semantic color mappers defined by this library as values of parameter object `pp-color`.

```scheme
(import (srfi 272 advanced) (srfi 272 colorize))
(parameterize ((sgr-support-tier 2))
  (pp '(define (sq x) (* x x)) pp-color #t))
```

## Terminal capabilities

The color capabilities of terminals are classified in _support tiers_:

| Tier | Capabilities |
| :--- | :--- |
| `#f` | No support for ANSI SGR sequences |
| 0 | Basic 16-color support (ANSI escape codes 30-37, 90-97) |
| 1 | 256-color palette support |
| 2 | 24-bit true color support |
| 3 | Extended SGR support with additional attributes, such as colored underlines |

**(detect-sgr-tier)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Inspects environment variables like `TERM`, `COLORTERM`, `TERM_PROGRAM`, `NO_COLOR`, `CLICOLOR`, `CLICOLOR_FORCE`, and `FORCE_COLOR` to determine the support tier of the terminal. Environment variable `SGR_TIER` can be used to override the detection: it can be set to a tier between 0 and 3, or to `off`, `none`, `no`, `-1`, or `disable`. `detect-sgr-tier` returns `#f` or an exact integer between 0 and 3. When LispKit is not running in a terminal, e.g. within an app, the result typically is `#f`.

**sgr-support-tier** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

The support tier used for generating SGR sequences. The default is the result of `(detect-sgr-tier)` at the time the library was loaded. The parameter can be set or parameterized in order to generate SGR sequences for a different support tier, e.g. when output is collected in a string or file which is later sent to a terminal.

## SGR sequences

The following procedures construct the parameter strings of SGR sequences (the part between `ESC[` and `m`) for support tiers 0, 1, 2, and 3. Optional arguments default to `#f`.

**(sgr0 _fg16_ [_bold?_ [_uline?_ [_bg16_]]])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an SGR parameter string for 16-color terminals. _fg16_ and _bg16_ are color indices between 0 and 15.

**(sgr1 _fg256_ [_bold?_ [_uline?_ [_bg256_]]])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an SGR parameter string for 256-color terminals. _fg256_ and _bg256_ are color indices between 0 and 255.

**(sgr2 _fgtrue_ [_bold?_ [_uline?_ [_italic?_ [_strike?_ [_bgtrue_]]]]])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an SGR parameter string for true color terminals. _fgtrue_ and _bgtrue_ are 24-bit RGB colors, i.e. integers between `#x000000` and `#xFFFFFF`.

**(sgr3 _fgtrue_ [_bold?_ [_dim?_ [_uline_ [_italic?_ [_strike?_ [_bgtrue_]]]]]])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an SGR parameter string for terminals with extended SGR support. _fgtrue_ and _bgtrue_ are 24-bit RGB colors. _uline_ specifies the underline style: `0` or `off` disables underlining; `1`, `#t`, `straight`, or `_` selects a straight underline; `2`, `double`, or `=` a double underline; `3`, `curly`, or `~` a curly underline; `4`, `dotted`, or `:` a dotted underline; and `5`, `dashed`, or `-` a dashed underline. A pair `(type . rgb)` specifies an underline of the given type with an RGB color.

```scheme
(sgr0 1 #t)                                   ⇒  "31;1"
(sgr1 131)                                    ⇒  "38;5;131"
(sgr2 #xBF616A #f #f #t)                      ⇒  "38;2;191;97;106;3"
(sgr3 #xBF616A #f #f '(curly . #xFF0000))     ⇒  "38;2;191;97;106;4:3;58:2:255:0:0"
```

## SGR bundles

An _ANSI SGR bundle_ (ASB) combines SGR parameter strings for all four support tiers in a vector of four strings. Bundles make it possible to define styles which degrade gracefully on terminals with limited capabilities.

**(make-asb _s0_ [_s1_ [_s2_ [_s3_]]])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new ASB consisting of the SGR parameter strings _s0_, _s1_, _s2_, and _s3_ for tiers 0 to 3. If fewer than four arguments are provided, the last one is used for the remaining tiers.

**(asb->sgr-string _asb_ [_tier_])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the full SGR escape sequence for bundle _asb_ and support tier _tier_, which defaults to `(sgr-support-tier)`. If _tier_ is `#f`, the empty string is returned.

```scheme
(define red (make-asb (sgr0 1) (sgr1 131) (sgr2 #xBF616A)))
red                       ⇒  #("31" "38;5;131" "38;2;191;97;106" "38;2;191;97;106")
(asb->sgr-string red 0)   ⇒  "\x1B;[31m"
(asb->sgr-string red 2)   ⇒  "\x1B;[38;2;191;97;106m"
```

## Palettes

**(make-asb-palette _asb_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a palette of 16 bundles. The procedure accepts either 8 or 16 bundles. If only 8 bundles are provided, they are copied over to the remaining 8 slots.

**(asb-palette-ref _palette i_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the bundle at index _i_ (between 0 and 15) of _palette_.

**default-asb-palette** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[constant]</span>  

The default palette of 16 bundles used by the default semantic color mapper.

## Semantic color mappers

A _semantic color mapper_ is a procedure which receives a semantic color symbol, such as `comment` or `keyword`, and a boolean _start?_ which is `#t` for the start and `#f` for the end of a colored region. It returns either a string which is written to the output verbatim, or an ASB which gets converted into an SGR escape sequence. The following semantic color symbols are used by the pretty printer; the last column shows the corresponding palette index used by the default mapper:

| Symbol | Description | Default |
| :--- | :--- | :--- |
| `comment` | Comments | 8 (gray) |
| `char`, `string` | Character and string literals | 10 (green) |
| `escape`, `char-escape`, `string-escape` | Escape sequences | 11 (yellow) |
| `char-meta`, `string-meta` | Metadata in characters and strings | 2 (green) |
| `symbol-escape` | Escaped symbols | 11 (yellow) |
| `formal` | Formal parameters | 7 (light gray) |
| `variable` | Variables | no styling |
| `defined` | Variables being defined | bold |
| `keyword` | Keywords of special forms | 12 (blue) |
| `number` | Numeric literals | 13 (purple) |
| `literal` | Literal values | 14 (teal) |
| `literal-meta`, `meta` | Metadata | 6 (cyan) |
| `directive` | Directives | 13 (purple) |
| `paren` | Parenthesis | 7 (light gray) |
| `bracket` | Brackets | no styling |
| `warning` | Warnings | 1 (red) |

**(semantic-color-mapper? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a semantic color mapper, i.e. a procedure; otherwise `#f` is returned.

**(make-semantic-color-mapper _palette_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-semantic-color-mapper _palette palette-mapper_)**  

Returns a semantic color mapper which maps semantic color symbols to bundles of _palette_. _palette-mapper_ is a procedure which accepts a semantic color symbol and a palette and returns a bundle. If _palette-mapper_ is not provided, the default mapping shown in the table above is used.

**default-semantic-color-mapper** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[constant]</span>  

The semantic color mapper implementing the default color theme. Setting `pp-color` to `#t` is equivalent to setting it to `default-semantic-color-mapper`.

**(semantic-color->start-string _sc_ [_mapper_ [_tier_]])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(semantic-color->end-string _sc_ [_mapper_ [_tier_]])** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return the sequences starting and ending a region colored with semantic color _sc_, using semantic color mapper _mapper_ (default: `default-semantic-color-mapper`) and support tier _tier_ (default: `(sgr-support-tier)`).

```scheme
(semantic-color->start-string 'keyword default-semantic-color-mapper 0)  ⇒  "\x1B;[34m"
(semantic-color->end-string 'keyword default-semantic-color-mapper 0)    ⇒  "\x1B;[0m"
```
