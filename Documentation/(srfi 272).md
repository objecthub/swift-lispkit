# SRFI 272: Pretty Printing

[SRFI 272](https://srfi.schemers.org/srfi-272/srfi-272.html) follows the traditional Scheme model of pretty printing: pretty printing is a variant of `write` which inserts whitespace to make the printed representation of a value easier to read for humans. The output of the pretty printer can be read back via `read` unless objects are output that do not have a readable representation. SRFI 272 specifies a family of libraries of increasing functionality. More powerful libraries satisfy all requirements of the simpler ones while adding new features:

| Library | Functionality |
| :--- | :--- |
| `(srfi 272)` | Minimalist pretty printer `pp` |
| `(srfi 272 basic)` | Control over the right margin and over the printing of shared and circular structures |
| `(srfi 272 intermediate)` | Keyword arguments, level and length limits, radix control, pretty printing of source files, custom styles for special forms |
| `(srfi 272 advanced)` | Fine-grained control over layout, abbreviations, and colors; style and hook registries; line generators |
| `(srfi 272 fancy)` | Preservation of comments when pretty printing source files, HTML output |

In addition, there are three support libraries:

| Library | Functionality |
| :--- | :--- |
| `(srfi 272 colorize)` | Terminal color support (ANSI SGR sequences, palettes, semantic color mappers) |
| `(srfi 272 measure)` | Customizable measurement of the width of characters |
| `(srfi 272 show)` | Integration of the advanced pretty printer with [SRFI 166](https://srfi.schemers.org/srfi-166/srfi-166.html) |

The libraries export identically named identifiers, which, unless stated otherwise, do not refer to the same bindings. Only one of the libraries `(srfi 272)`, `(srfi 272 basic)`, `(srfi 272 intermediate)`, `(srfi 272 advanced)`, and `(srfi 272 fancy)` should be imported into the same scope. Pick the level of functionality that is needed and import the corresponding library. If several of these libraries are needed, they should be imported with a prefix.

Library [`(lispkit pretty-print)`](<(lispkit pretty-print).md>) provides a LispKit-style front end for the SRFI 272 pretty printer. It is configured via association lists and knows how to format code that uses syntax provided by LispKit libraries.

## Code and data layout

The pretty printer lays out lists either as _data_ or as Scheme _code_. Which layout is used does not depend on the object being printed; the pretty printer never tries to find out whether a list "looks like" code. Instead, the layout is determined by the library and, for the more powerful libraries, by a parameter:

| Library | Layout |
| :--- | :--- |
| `(srfi 272)`, `(srfi 272 basic)` | Always data |
| `(srfi 272 intermediate)` | Always code |
| `(srfi 272 advanced)`, `(srfi 272 fancy)`, `(srfi 272 show)` | Code if parameter `pp-code` is true (the default), data otherwise |

This applies to `pp`, `pp*`, `pprint`, `pprint-shared`, `pprint-simple`, `make-pprint-generator`, and `pprinted` alike. Procedures `pprint-file` and `pprint-file/html` always print the expressions of a file as code, independent of `pp-code`.

With the _data layout_, a list is printed on a single line if it fits. Otherwise, every element is printed on a separate line, aligned with the first element. With the _code layout_, every list is considered to be an expression:

- If the first element of a list is a symbol associated with a style, e.g. `define`, `let`, or `cond`, the list is formatted according to this style. Styles determine, for instance, that the head of a definition stays on the first line and that bodies are indented. Styles can be customized via `pretty-style` (see [`(srfi 272 intermediate)`](<(srfi 272 intermediate).md>) and [`(srfi 272 advanced)`](<(srfi 272 advanced).md>)).
- All other lists are formatted like procedure calls: if the first element is a short symbol, the first argument is printed on the same line and the remaining arguments are aligned with it; otherwise, the arguments fill the lines.
- Quoted subexpressions, i.e. the objects following `'`, are printed using the data layout. In quasiquoted subexpressions, the template is printed using the data layout and unquoted subexpressions, i.e. the objects following `,` or `,@`, are printed using the code layout again.
- Vectors, bytevectors, and atomic values are printed the same way in both layouts.

The following examples use library `(srfi 272 advanced)`, which supports both layouts:

```scheme
(define code
  '(define (fold kons knil ls)
     (let lp ((ls ls) (acc knil))
       (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc))))))

(pp code)
⇒
(define (fold kons knil ls)
  (let lp ((ls ls) (acc knil))
    (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc)))))

(pp code pp-code #f)
⇒
(define
 (fold kons knil ls)
 (let
  lp
  ((ls ls) (acc knil))
  (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc)))))
```

Since the quoted list in the next example is printed using the data layout, each of its elements gets printed on a separate line once the list does not fit into the remaining space:

```scheme
(pp '(define colors '(red green blue cyan magenta yellow black white))
    pp-width 30)
⇒
(define colors
  '(red
    green
    blue
    cyan
    magenta
    yellow
    black
    white))
```

As a consequence, data which happens to start with a symbol that is associated with a style gets formatted like code when the code layout is used. With library `(srfi 272 advanced)`, this can be avoided by setting `pp-code` to `#f`. Library `(srfi 272 intermediate)` always uses the code layout:

```scheme
(pp '(if (> x 0) "positive number" "negative number or zero")
    pp-width 30)
⇒
(if (> x 0)
    "positive number"
    "negative number or zero")

(pp '(if (> x 0) "positive number" "negative number or zero")
    pp-width 30 pp-code #f)
⇒
(if
 (> x 0)
 "positive number"
 "negative number or zero")
```

## LispKit implementation notes

The LispKit implementation of SRFI 272 is a port of the portable R7RS sample implementation by Sergei Egorov. It differs from the sample implementation in the following ways:

- **Pairs are never labeled.** In LispKit, pairs are immutable values without identity: `eq?` compares pairs structurally. Consequently, shared list structure cannot be observed and circular lists cannot be constructed. The pretty printer thus never uses datum labels for pairs, consistent with LispKit's `write` and `write-shared`. Datum labels are used for shared or circular mutable objects such as vectors, strings, bytevectors, boxes, mutable pairs, and growable vectors.
- **LispKit data types.** Boxes, mutable pairs, and growable vectors are printed like `write` prints them, e.g. `#<box (1 2)>`, `#<pair a b>`, and `#g(1 2 3)`, but with line breaks, level and length limits, and the detection of shared and circular structures applied to their contents. Hashtables and records are printed in a list-like form which differs from the output of `write`, e.g. `#<hashtable (key value) ...>` and `#<record point (x 1) (y 2)>`, such that their contents are formatted and checked in the same way. All other LispKit objects are printed as atoms using `write`.
- **Reading.** The LispKit reader does not support datum labels, box syntax `#&`, and square brackets. Output generated with parameters `pp-graph`, `pp-circle`, and `pp-brackets` can therefore not always be read back by LispKit itself.
- **Bug fixes.** The following issues of the sample implementation were fixed in accordance with the SRFI specification: `add-pp-hook` and `pretty-hook` support hook `#f` for removing a hook and hook `#t` for tests returning hooks; hooks created with `rmac-pp-hook` work; `make-pprint-generator` returns all lines independent of the context in which the generator is called; parameters `pp-level-stub`, `pp-length-stub`, and `pp-lines-stub` are exported; `pp-lines-stub` accepts `#f`; the lines limit does not produce an extra empty line; the intermediate `pprint-file` accepts keyword arguments; `pprinted` lets the SRFI 166 `width` override `pp-width`; and `make-asb-palette` accepts 8 or 16 bundles.

## API

**(pp _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pp _obj port_)**  

Writes a pretty-printed representation of _obj_ to _port_, which defaults to the value of `(current-output-port)`. Whitespace is used to format the output within a column width of 80. This width is a soft limit; the printer may write past the margin when printing indivisible tokens such as long symbols or strings. Procedure `pp` assumes that it is called at the beginning of a line. The output is always followed by a newline.

`pp` provides the same machine-readability guarantees as `write`: if an object can be written by `write` and read back by `read` to produce an equal object, then the output of `pp` can be read back as well. Library `(srfi 272)` does not detect shared or circular structures; printing circular vectors does not terminate.

```scheme
(import (srfi 272))
(pp '(define (fold kons knil ls)
       (let lp ((ls ls) (acc knil))
         (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc))))))
⇒
(define
 (fold kons knil ls)
 (let
  lp
  ((ls ls) (acc knil))
  (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc)))))
```

Library `(srfi 272)` formats all lists using the data layout. Libraries `(srfi 272 intermediate)` and above also support the code layout, as explained in the section [Code and data layout](#code-and-data-layout).
