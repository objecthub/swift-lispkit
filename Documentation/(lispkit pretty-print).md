# LispKit Pretty-Print

Library `(lispkit pretty-print)` provides procedures for pretty printing data and Scheme code. Pretty printing is a variant of `write` which inserts whitespace, such that the printed representation of a value is easier to read for humans. Apart from the inserted whitespace, the output corresponds to the output of `write`; data that can be written by `write` and read back by `read` can also be read back after being pretty printed.

The pretty printer can format arbitrary data as well as Scheme code. It knows the conventions for laying out the special forms of Scheme and of the syntax provided by LispKit libraries, such as `let-optionals`, `define-enum`, or `match`. It supports limits on the size of the output, colors, and the reformatting of source code including comments. It can be extended with layout rules for new syntax and with rules for printing new data types.

```scheme
(import (lispkit pretty-print))
(define code
  '(define (fold kons knil ls)
     (let lp ((ls ls) (acc knil))
       (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc))))))
(pretty-print code)
⇒
(define
 (fold kons knil ls)
 (let
  lp
  ((ls ls) (acc knil))
  (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc)))))

(pretty-print code '((code . #t)))
⇒
(define (fold kons knil ls)
  (let lp ((ls ls) (acc knil))
    (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc)))))

(pretty-print code '((code . #t) (width . 40) (brackets . #t)))
⇒
(define (fold kons knil ls)
  (let lp ([ls ls] [acc knil])
    (if (null? ls)
        acc
        (lp (cdr ls) (kons (car ls) acc)))))
```

All procedures accept an optional _configuration_, an association list such as `((code . #t) (width . 40))`, which controls how the output gets formatted. Configurations are described in section [Configurations](#configurations).

## Code and data layout

The pretty printer lays out lists either as _code_ or as _data_. The procedures for printing data, such as `pretty-print`, use the data layout by default. Configuration key `code` selects the layout: `((code . #t))` makes these procedures use the code layout. The procedures for printing source code, `pretty-print-file` and `pretty-print-string`, always use the code layout. The printer never tries to find out whether a list "looks like" code; the layout is solely determined by the procedure and the configuration.

With the _data layout_, a list is printed on a single line if it fits. Otherwise, every element is printed on a separate line, aligned with the first element.

With the _code layout_, every list is considered to be an expression:

- If the first element of a list is a symbol associated with a _style_, e.g. `define`, `let`, `cond`, or `let-optionals`, the list is formatted according to this style. Styles determine, for instance, that the head of a definition stays on the first line and that bodies are indented. Section [Styles](#styles) explains styles and how to define new ones.
- All other lists are formatted like procedure calls: if the first element is a short symbol, the first argument is printed on the same line and the remaining arguments are aligned with it; otherwise, the arguments fill the lines.
- Quoted subexpressions, i.e. the objects following `'`, are printed using the data layout. In quasiquoted subexpressions, the template is printed using the data layout, and unquoted subexpressions, i.e. the objects following `,` or `,@`, are printed using the code layout again.
- Vectors, bytevectors, and atomic values are printed the same way in both layouts.

Since the quoted list in the following example is printed using the data layout, each of its elements gets printed on a separate line once the list does not fit into the remaining space:

```scheme
(pretty-print '(define colors '(red green blue cyan magenta yellow black white))
              '((code . #t) (width . 30)))
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

As a consequence of the rules above, data which happens to start with a symbol that is associated with a style is formatted like code when the code layout is used:

```scheme
(pretty-print '(if (> x 0) "positive number" "negative number or zero")
              '((code . #t) (width . 30)))
⇒
(if (> x 0)
    "positive number"
    "negative number or zero")

(pretty-print '(if (> x 0) "positive number" "negative number or zero")
              '((width . 30)))
⇒
(if
 (> x 0)
 "positive number"
 "negative number or zero")
```

## Shared and circular structures

Mutable objects, such as vectors, strings, bytevectors, boxes, mutable pairs, growable vectors, hashtables, and records, can be shared within a data structure, i.e. the same object can be referenced multiple times, and they can be part of circular structures. The pretty printer represents such structures with _datum labels_: `#N=` labels an object and `#N#` refers back to the labeled object. `pretty-print` labels only circular references, like `write`; `pretty-print-shared` labels all shared and circular references, like `write-shared`; and `pretty-print-simple` does not use datum labels at all, like `write-simple`.

Lists are never labeled. In LispKit, pairs are immutable values without identity: two lists with the same elements are indistinguishable, even for `eq?`. Lists can therefore be neither shared nor circular in a way that would be observable.

Boxes, mutable pairs, and growable vectors are printed like `write` prints them, e.g. `#<box (1 2)>`, `#<pair a b>`, and `#g(1 2)`, but their content is formatted and checked for shared and circular structures like the content of lists.

Hashtables and records are printed in a list-like form which differs from the output of `write`: a hashtable is printed as `#<hashtable` followed by its entries, each as a list `(key value)`, and `>`; a record is printed as `#<record` followed by the name of its record type, its fields, each as a list `(field value)`, and `>`. Their content is formatted, abbreviated, and checked for shared and circular structures like the content of lists. Empty hashtables and records without fields are printed like `write` prints them. The order in which the entries of a hashtable are printed is unspecified.

All other objects, such as procedures, are printed like `write` prints them. User-defined hooks can be used to change how objects are printed (see section [Hooks](#hooks)).

```scheme
(define b (box #f))
(set-box! b (list 1 2 b))
(pretty-print b)                  ⇒  #0=#<box (1 2 #0#)>
(define l (list 1 2))
(pretty-print-shared (list l l))  ⇒  ((1 2) (1 2))
(define-record-type node (make-node value next) node?
  (value node-value) (next node-next set-node-next!))
(define n (make-node 1 #f))
(set-node-next! n n)
(pretty-print n)                  ⇒  #0=#<record node (value 1) (next #0#)>
(define h (make-equal-hashtable))
(hashtable-set! h 'node n)
(pretty-print-shared (list h h))
⇒
(#0=#<hashtable (node #1=#<record node (value 1) (next #1#)>)>
 #0#)
```

Note that the LispKit reader does not support datum labels. Output that contains datum labels can therefore not be read back by LispKit.

## Configurations

A _configuration_ is an association list consisting of pairs `(key . value)`, where _key_ is one of the symbols listed in the following table. All printing procedures of this library accept a configuration as their last, optional argument. The value of a key given by a configuration passed to a procedure takes precedence over the value given by the default configuration, which is stored in parameter object `current-pretty-print-config`. Keys which are defined by neither configuration have the default values shown in the table. An error is signaled if a configuration is not an association list whose keys are symbols, if it contains an unknown key, or if a value is invalid for its key.

| Key | Value | Default |
| :--- | :--- | :--- |
| `width` | Right margin, an exact positive integer | 80 |
| `indent` | Column at which the output starts, an exact non-negative integer | 0 |
| `tab` | Indentation of the bodies of special forms | 1 |
| `max-tab` | Maximum offset for aligning arguments with the first argument | 4 |
| `miser-width` | Width of the zone near the right margin in which indentation is minimized, or `#f` | 20 |
| `inline-width` | Maximum width of subexpressions printed on a single line, or `#f` | 60 |
| `code` | `#t` for the code layout, `#f` for the data layout; ignored when printing source code | `#f` |
| `brackets` | If true, square brackets are used selectively when printing code | `#f` |
| `pretty` | If `#f`, all whitespace is collapsed into single spaces | `#t` |
| `newline` | If `#f`, the terminating newline is omitted | `#t` |
| `radix` | Radix for exact numbers: 2, 8, 10, 16, or `#f` (as `write`) | `#f` |
| `length` | Maximum number of elements shown per list or vector, or `#f` | `#f` |
| `level` | Maximum nesting depth shown, or `#f` | `#f` |
| `lines` | Maximum number of lines printed, or `#f` | `#f` |
| `length-stub` | String printed in place of elements cut off by `length` | `"..."` |
| `level-stub` | String printed in place of objects cut off by `level`, or `#t` | `#t` |
| `lines-stub` | String printed when the output is cut off by `lines`, or `#f` | `".."` |
| `color` | `#f`, `#t`, or a semantic color mapper | `#f` |
| `color-tier` | Terminal color support used by `((color . #t))`: `#f`, 0, 1, 2, or 3 | detected |
| `emit` | Procedure used for writing the regular output | `write-string` |
| `tint` | Procedure used for writing color sequences | `write-string` |
| `decorate` | If `#f`, comments are dropped when pretty printing source code | `#t` |
| `styles` | Style registry, or one of the symbols `lispkit` and `scheme` | `lispkit` |
| `hooks` | Hook registry | the shared hook registry |
| `graph` | If true, shared structures are labeled | `#f` |
| `circle` | If true, circular structures are labeled | `#t` |

Most of the examples in this section format code. They therefore set `code` to `#t`.

Keys `graph` and `circle` are listed for completeness only. They have no effect on the printing procedures of this library, which determine themselves how shared and circular structures are printed (see [above](#shared-and-circular-structures)).

**current-pretty-print-config** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

Parameter object holding the default configuration, an association list as described above, which is used by all procedures of this library. Configurations passed to the procedures take precedence over the default configuration. The default is `'()`. Setting `current-pretty-print-config` to a value which is not a valid configuration signals an error.

```scheme
(parameterize ((current-pretty-print-config '((code . #t) (width . 30))))
  (pretty-print '(let-optionals args ((x 1) (y 2)) (display x) (display y))))
⇒
(let-optionals args
  ((x 1) (y 2))
  (display x)
  (display y))
```

### Layout

`width` defines the right margin. It is a soft limit: the printer exceeds it if a single token, such as a long symbol or string, does not fit.

`indent` informs the printer that the output starts at the given column, e.g. because the current line already contains other output. All subsequent lines are indented accordingly. The printer assumes that it is called at column 0 otherwise.

```scheme
(display "result: ")
(pretty-print code '((code . #t) (indent . 8) (width . 50)))
⇒
result: (define (fold kons knil ls)
          (let lp ((ls ls) (acc knil))
            (if (null? ls)
                acc
                (lp (cdr ls) (kons (car ls) acc)))))
```

`tab` specifies by how many columns the bodies of special forms are indented relative to the keyword of the form. Since the opening parenthesis adds one column, the indentation relative to the parenthesis is `tab` + 1. Short keywords like `if` trigger a layout in which the first subexpression appears on the same line as the keyword and the subsequent subexpressions are aligned with it. `max-tab` limits the length of keywords for which this layout is used; with a value of 0, this layout is disabled.

```scheme
(pretty-print code '((code . #t) (tab . 3) (width . 40)))
⇒
(define (fold kons knil ls)
    (let lp ((ls ls) (acc knil))
        (if (null? ls)
            acc
            (lp (cdr ls)
                (kons (car ls) acc)))))

(pretty-print code '((code . #t) (max-tab . 0) (width . 40)))
⇒
(define (fold kons knil ls)
  (let lp ((ls ls) (acc knil))
    (if
      (null? ls)
      acc
      (lp (cdr ls) (kons (car ls) acc)))))
```

`inline-width` is a soft limit on the width of subexpressions that are printed on a single line. Setting it to a value smaller than `width` makes the printer break up long expressions even if they would fit into the line. `miser-width` defines a zone near the right margin: expressions starting within this zone are formatted with minimal indentation in order to save space. With `#f`, both features are disabled.

```scheme
(pretty-print code '((code . #t) (inline-width . 30)))
⇒
(define (fold kons knil ls)
  (let lp ((ls ls) (acc knil))
    (if (null? ls)
        acc
        (lp (cdr ls) (kons (car ls) acc)))))

(pretty-print '(define (square x) (* x x)) '((code . #t) (width . 18)))
⇒
(define (square x)
 (* x x))

(pretty-print '(define (square x) (* x x))
              '((code . #t) (width . 18) (miser-width . #f)))
⇒
(define (square x)
  (* x x))
```

`brackets` makes the printer use square brackets instead of parenthesis for selected subforms of code, such as the bindings of `let` forms, following the conventions of R6RS. Note that the LispKit reader does not support square brackets.

`pretty` set to `#f` disables all line breaks and indentation, producing output similar to `write`, while all other settings, e.g. limits, remain in effect. In this case, the terminating newline is omitted as well. `newline` set to `#f` only omits the terminating newline.

```scheme
(pretty-print code '((code . #t) (pretty . #f) (length . 3)))
⇒ (define (fold kons knil ...) (let lp ((ls ls) (acc knil)) ...))

(display "[") (pretty-print '(a b c) '((newline . #f))) (display "]")
⇒ [(a b c)]
```

### Numbers

`radix` determines the radix used for printing exact numbers. For radices other than 10, prefixes `#b`, `#o`, and `#x` are printed, such that the numbers can be read back. Inexact numbers are always printed in decimal notation. With `#f`, numbers are printed like `write` prints them.

```scheme
(pretty-print '(255 -16 7/3 1.5) '((radix . 16)))
⇒ (#xff #x-10 #x7/3 1.5)
```

### Abbreviations

`length` limits the number of elements shown for each list, vector, or other compound object. The remaining elements get replaced by `length-stub`. `level` limits how deeply the printer descends into nested structures: the printed object is at level 0, its elements are at level 1, their elements at level 2, and so on. Compound objects at a level equal to or exceeding the limit get replaced: if `level-stub` is `#t`, the "shell" of the object is printed with its content replaced by `length-stub`; otherwise the string `level-stub` is printed. Atomic values are not affected by these limits.

```scheme
(define expr '(if (member x y) (+ (car x) 3) '(foo . #(a b c d "Baz"))))
(pretty-print expr '((level . 2) (length . 3)))
⇒ (if (member x y) (+ (...) 3) ...)

(pretty-print expr '((level . 2) (length . 3) (level-stub . "#")))
⇒ (if (member x y) (+ # 3) ...)
```

`lines` limits the total number of printed lines. If the output gets cut off, the last line ends with `lines-stub` (or `length-stub` if `lines-stub` is `#f`) followed by all outstanding closing parenthesis.

```scheme
(pretty-print code '((code . #t) (width . 40) (lines . 3)))
⇒
(define (fold kons knil ls)
  (let lp ((ls ls) (acc knil))
    (if (null? ls) ..)))
```

### Colors

If `color` is set, the printer highlights syntax elements by associating them with _semantic colors_. A semantic color is a symbol denoting a category of syntax elements. The printer uses the following semantic colors:

| Semantic color | Syntax element |
| :--- | :--- |
| `keyword` | Keywords of special forms, e.g. `define` or `let` |
| `defined` | Variables being defined |
| `formal` | Formal parameters and variables being bound |
| `number` | Numeric literals |
| `string` | String literals |
| `char` | Character literals |
| `literal` | Other literal values, including quoted data |
| `warning` | Unquoted expressions outside of a quasiquotation |
| `comment` | Comments when pretty printing source code |

Hooks may use additional semantic colors (see section [Hooks](#hooks)). With `((color . #t))`, colors are rendered via ANSI escape sequences using a default color theme. Which escape sequences are generated depends on the color support of the terminal, which is classified in tiers:

| `color-tier` | Terminal color support |
| :--- | :--- |
| `#f` | No support for ANSI escape sequences; no colors are generated |
| 0 | 16 colors |
| 1 | 256 colors |
| 2 | 24-bit true colors |
| 3 | 24-bit true colors and extended text attributes, such as colored underlines |

By default, the tier is detected when the library is loaded, based on environment variables such as `TERM`, `COLORTERM`, `NO_COLOR`, and `CLICOLOR_FORCE`. If LispKit does not run in a terminal, e.g. within an app, the detected tier typically is `#f`, i.e. `((color . #t))` does not produce any colors. Configuration key `color-tier` overrides the detected tier. This makes it possible to generate escape sequences for a specific kind of terminal independent of the environment in which the output is produced, e.g. when the output is collected in a string or file that is later sent to a terminal. `color-tier` has no effect unless `color` is `#t`.

```scheme
(pretty-print->string '(define (sq x) (* x x))
                      '((code . #t) (color . #t) (color-tier . 0)))
⇒ "(\x1B;[34mdefine\x1B;[0m (\x1B;[1msq\x1B;[0m \x1B;[37mx\x1B;[0m) (* x x))\n"
```

The value of `color` can also be a _semantic color mapper_, i.e. a procedure which receives a semantic color and a boolean that is `#t` at the beginning and `#f` at the end of a colored syntax element. The mapper returns a string which is written to the output. Such mappers can generate other forms of markup, independent of the capabilities of a terminal and of `color-tier`:

```scheme
(define (html-mapper sc start?)
  (if start?
      (string-append "<span class=\"" (symbol->string sc) "\">")
      "</span>"))
(pretty-print '(define (sq x) (* x x)) `((code . #t) (color . ,html-mapper)))
⇒
(<span class="keyword">define</span> (<span class="defined">sq</span> <span class="formal">x</span>) (* x x))
```

The output of the printer is written with two procedures, which can be replaced via `emit` and `tint` in order to post-process the output. Both procedures receive a string and an output port. `emit` writes all regular output; `tint` writes the strings produced for semantic colors. For instance, ``` `((tint . ,(lambda (str port) #f))) ``` filters out all colors.

```scheme
(pretty-print '(define (sq x) (* x x))
              `((emit . ,(lambda (str port) (write-string (string-upcase str) port)))))
⇒ (DEFINE (SQ X) (* X X))
```

## Printing data

**(pretty-print _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pretty-print _obj port_)**  
**(pretty-print _obj config_)**  
**(pretty-print _obj port config_)**  

Writes a pretty-printed representation of _obj_ to an output port. _obj_ is an arbitrary object. _port_ is the textual output port to which the output is written; it defaults to the value of `(current-output-port)`. _config_ is a configuration, i.e. an association list with symbols as keys, such as `((width . 60))`, which controls the formatting; it defaults to `'()` (see section [Configurations](#configurations)). Lists are formatted using the data layout, unless _config_ or the default configuration sets `code` to `#t`. The output is followed by a newline unless the configuration sets `newline` or `pretty` to `#f`. Datum labels are only used for circular structures, like `write` does it.

```scheme
(define v (vector 1 2 3))
(vector-set! v 1 v)
(pretty-print v)  ⇒  #0=#(1 #0# 3)
```

**(pretty-print-shared _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pretty-print-shared _obj port_)**  
**(pretty-print-shared _obj config_)**  
**(pretty-print-shared _obj port config_)**  

Writes a pretty-printed representation of _obj_ to an output port, using datum labels for all shared and circular structures, like `write-shared`. _obj_ is an arbitrary object. _port_ is the textual output port to which the output is written; it defaults to the value of `(current-output-port)`. _config_ is a configuration which controls the formatting; it defaults to `'()`. Apart from the use of datum labels, `pretty-print-shared` behaves like `pretty-print`.

```scheme
(define s (string #\a))
(pretty-print-shared (list s s))  ⇒  (#0="a" #0#)
(pretty-print (list s s))         ⇒  ("a" "a")
```

**(pretty-print-simple _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pretty-print-simple _obj port_)**  
**(pretty-print-simple _obj config_)**  
**(pretty-print-simple _obj port config_)**  

Writes a pretty-printed representation of _obj_ to an output port without using datum labels, like `write-simple`. _obj_ is an arbitrary object. _port_ is the textual output port to which the output is written; it defaults to the value of `(current-output-port)`. _config_ is a configuration which controls the formatting; it defaults to `'()`. Apart from the use of datum labels, `pretty-print-simple` behaves like `pretty-print`. This procedure does not terminate for circular structures, but it is the fastest of the three printing procedures.

**(pretty-print->string _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pretty-print->string _obj config_)**  

Returns a string containing the pretty-printed representation of _obj_, i.e. the output of `(pretty-print obj port config)` for a string port _port_. _obj_ is an arbitrary object. _config_ is a configuration which controls the formatting; it defaults to `'()`.

```scheme
(pretty-print->string '(255 #(16 7/3)) '((radix . 16)))
⇒ "(#xff #(#x10 #x7/3))\n"
```

## Printing source code

The following two procedures reformat Scheme source code provided as a file or a string. They read all expressions of the source code and pretty print them using the code layout, independent of configuration key `code`.

Unless the configuration sets `decorate` to `#f`, comments are preserved as follows: line comments and whitespace between top-level expressions are copied verbatim, line comments at the end of top-level expressions are kept, and line comments preceding or following the expressions of a body, as well as empty lines between them, are preserved. Comments in other positions, e.g. following the head of a definition or preceding the first expression of a body, and block comments `#| ... |#` get dropped. If `decorate` is `#f`, all comments are dropped and top-level expressions are separated by an empty line.

The head of the source code, i.e. the lines before the first expression, may contain an Emacs-style _magic comment_ which configures the pretty printer for this source code. Example: `;; -*- mode: scheme; fill-column: 70; pp-max-tab: 2 -*-`. The following keys are recognized:

| Key in magic comment | Configuration key |
| :--- | :--- |
| `fill-column:`, `pp-width:` | `width` |
| `pp-inline-width:` | `inline-width` |
| `pp-miser-width:` | `miser-width` |
| `pp-tab:` | `tab` |
| `pp-max-tab:` | `max-tab` |
| `pp-brackets:` | `brackets` |

Values are numbers, or the symbols `t` (for `#t`) and `nil` (for `#f`). The settings of a magic comment take precedence over the default configuration, but not over the configuration passed to the procedure.

Top-level line comments of the following form are _style comments_:

```
;; * pp-styles: symbol := style [; symbol := style ...]
```

A style comment associates styles with symbols for the rest of the source code. A style is either a symbol whose style gets copied, e.g. `let`, or an explicit style starting with an underscore, e.g. `(_ e . body)` (see section [Styles](#styles)). Style comments do not modify the style registries of this library.

```scheme
(define source
  (string-append
    ";; -*- mode: scheme; fill-column: 40 -*-\n"
    ";; * pp-styles: with-lock := when; my-loop := (_ i? fc* . body)\n"
    "\n"
    ";; Transfers money\n"
    "(define (transfer! from to amount)\n"
    "  (check-balance from amount) ; may raise an error\n"
    "  ;; locking is required\n"
    "  (with-lock (lock-of from) (withdraw! from amount) (deposit! to amount)))\n"
    "\n"
    "(my-loop lp ((i 0)) (display i) (lp (+ i 1))) ; loop\n"))
(display (pretty-print-string source))
⇒
;; -*- mode: scheme; fill-column: 40 -*-
;; * pp-styles: with-lock := when; my-loop := (_ i? fc* . body)

;; Transfers money
(define (transfer! from to amount)
  (check-balance from amount) ; may raise an error
  ;; locking is required
  (with-lock (lock-of from)
    (withdraw! from amount)
    (deposit! to amount)))

(my-loop lp ((i 0))
  (display i)
  (lp (+ i 1))) ; loop
```

**(pretty-print-file _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pretty-print-file _path out_)**  
**(pretty-print-file _path config_)**  
**(pretty-print-file _path out config_)**  

Reads the Scheme source code stored in a text file and writes the pretty-printed source code. _path_ is a string specifying the path of the file to read. _out_ determines where the output is written: it is either a string specifying the path of a file to create, or a textual output port. The default for _out_ is the value of `(current-output-port)`. _config_ is a configuration which controls the formatting; it defaults to `'()`. The source code is always formatted using the code layout. Comments, magic comments, and style comments are handled as described at the beginning of this section.

```scheme
(pretty-print-file "fold.scm" "fold-formatted.scm" '((width . 70)))
```

**(pretty-print-string _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pretty-print-string _str config_)**  

Returns a new string containing the pretty-printed version of the Scheme source code in string _str_. _config_ is a configuration which controls the formatting; it defaults to `'()`. The source code is always formatted using the code layout. Comments, magic comments, and style comments are handled as described at the beginning of this section.

```scheme
(display
  (pretty-print-string
    "(define (sq x) (* x x)) ; square\n(when-let* ((x (assq 'a al))) (display x) (newline))"
    '((width . 30))))
⇒
(define (sq x) (* x x)) ; square
(when-let* ((x (assq 'a al)))
  (display x)
  (newline))
```

## Styles

When printing code, i.e. when the code layout is used, a list whose first element is a symbol is formatted according to the _style_ associated with this symbol in a _style registry_. Lists starting with a symbol without style are formatted like procedure calls. Styles have no effect on the data layout.

### Style registries

There are two style registries:

- The _LispKit registry_ is used by default. It contains styles for the syntax of R7RS as well as for the syntax provided by LispKit libraries. It can be modified via `pretty-print-style`.
- The _Scheme registry_ only contains styles for the syntax of R7RS. It is useful for formatting portable Scheme code. It is not affected by `pretty-print-style`.

Configuration key `styles` determines which registry is used: `((styles . scheme))` selects the Scheme registry, `((styles . lispkit))` the LispKit registry. Its value can also be a registry that was derived from one of the two registries via `pretty-print-style-add`.

```scheme
(pretty-print '(let-optionals args ((x 1) (y 2) . rest) (display x) (display y) (newline))
              '((code . #t) (width . 40)))
⇒
(let-optionals args ((x 1) (y 2) . rest)
  (display x)
  (display y)
  (newline))

(pretty-print '(let-optionals args ((x 1) (y 2) . rest) (display x) (display y) (newline))
              '((code . #t) (width . 40) (styles . scheme)))
⇒
(let-optionals args ((x 1) (y 2) . rest)
  (display x) (display y) (newline))
```

### Style syntax

A style is a pattern which is matched against a form. It is an improper list that matches the following grammar:

```
‹style›     ⟶ (_ . ‹fmt-tail›)
‹fmt-tail›  ⟶ body | fill | spread | h* | dc* | ec* | fc* | lc*
             | (i? . ‹fmt-tail›) | (‹fmt› . ‹fmt-tail›)
‹fmt›       ⟶ k | i | h | d | e | f | l | dc | ec | fc | lc | ‹fmt-tail›
```

The underscore stands for the keyword of the form. Each of the other elements describes how the corresponding subform is printed, as shown in the following table. A terminal at the end of an improper list, such as `body` in `(_ e . body)`, applies to all remaining subforms. If the subform corresponding to `i?` is a symbol, `i?` is treated like `i`; otherwise, `i?` is skipped and matching continues with the same subform and the rest of the pattern. Subforms which do not match the pattern are printed as data.

| Terminal | Printing method |
| :--- | :--- |
| `k` | Keyword |
| `i` | Identifier (a bound variable) |
| `d` | Datum, i.e. using the data layout |
| `e` | Expression |
| `f` | Formal parameter or (possibly improper) list of formal parameters |
| `l` | Literal or list of literals |
| `h` | Head of a definition: an identifier or a list like `(name arg ...)` |
| `dc`, `ec`, `fc`, `lc` | Clause consisting of a head followed by expressions, where the head is a datum, an expression, formals, or literals, respectively (e.g. clauses of `cond-expand`, `cond`, `case-lambda`, and `case`) |
| `dc*`, `ec*`, `fc*`, `lc*` | List of the corresponding clauses |
| `h*` | List of definition heads |
| `body` | Sequence of expressions printed on separate lines and indented, like the body of `lambda` |
| `spread` | Like `body`, but with an empty line between expressions, like in `define-library` |
| `fill` | Sequence of expressions filling lines, like the arguments of procedure calls |

For example, the style of `let` is `(_ i? fc* . body)`: an optional loop identifier, followed by a list of bindings (clauses with formals as heads), followed by a body. The style of `case` is `(_ e . lc*)`: an expression followed by clauses whose heads are lists of literals.

The following styles are defined in both registries:

| Forms | Style |
| :--- | :--- |
| `lambda` | `(_ f . body)` |
| `define` | `(_ h . body)` |
| `define-values` | `(_ h* . body)` |
| `define-syntax` | `(_ h . body)` |
| `define-library` | `(_ h* . spread)` |
| `define-record-type` | `(_ h . body)` |
| `let` | `(_ i? fc* . body)` |
| `letrec`, `letrec*`, `let*`, `let-values`, `let*-values`, `let-syntax`, `letrec-syntax`, `parameterize`, `with-syntax` | `(_ fc* . body)` |
| `syntax-rules` | `(_ i? d . ec*)` |
| `do` | `(_ fc* ec . body)` |
| `begin`, `if` | `(_ . body)` |
| `cond` | `(_ . ec*)` |
| `case` | `(_ e . lc*)` |
| `when`, `unless` | `(_ e . body)` |
| `set!` | `(_ e . fill)` |
| `and`, `or`, `import`, `delay` | `(_ . fill)` |
| `guard` | `(_ (f . ec*) . body)` |
| `case-lambda` | `(_ . fc*)` |
| `cond-expand` | `(_ . dc*)` |

In addition, the LispKit registry defines the following styles:

| Category | Forms | Style |
| :--- | :--- | :--- |
| Lambda-like forms | `λ`, `opt-lambda`, `opt*-lambda`, `stream-lambda` | `(_ f . body)` |
| Lambda-like forms | `lambda/tag` | `(_ e f . body)` |
| Lambda-like forms | `case-λ` | `(_ . fc*)` |
| Lambda-like forms | `case-lambda/tag` | `(_ e . fc*)` |
| Lambda-like forms | `thunk`, `thunk*` | `(_ . body)` |
| Binding forms | `let-optionals`, `let*-optionals`, `let-keywords`, `let*-keywords` | `(_ e fc* . body)` |
| Binding forms | `letrec-values`, `if-let*`, `when-let*` | `(_ fc* . body)` |
| Binding forms | `receive` | `(_ f e . body)` |
| Binding forms | `stream-let` | `(_ i? fc* . body)` |
| Definitions | `define-enum`, `define-enumeration` | `(_ i fill . fill)` |
| Definitions | `define-datatype` | `(_ i . body)` |
| Definitions | `define-type` | `(_ h i . body)` |
| Definitions | `define-optionals`, `define-optionals*`, `define-stream`, `define-class`, `define-generic`, `define-pattern` | `(_ h . body)` |
| Control | `while`, `with-mutex` | `(_ e . body)` |
| Control | `dolist`, `dotimes` | `(_ fc . body)` |
| Control | `loop`, `exit-with` | `(_ i . body)` |
| Control | `unwind-protect` | `(_ . body)` |
| Control | `assert`, `time`, `delay-force`, `stream-delay`, `future` | `(_ . fill)` |
| Pattern matching | `match`, `stream-match` | `(_ e . dc*)` |
| Pattern matching | `match-lambda`, `match-lambda*` | `(_ . dc*)` |
| Pattern matching | `match-let` | `(_ i? dc* . body)` |
| Pattern matching | `match-let*`, `match-letrec` | `(_ dc* . body)` |
| Logic programming | `conde`, `conda`, `condu` | `(_ . ec*)` |
| Logic programming | `fresh`, `run*` | `(_ f . body)` |
| Logic programming | `run` | `(_ e f . body)` |
| Iteration | `for/and`, `for/or`, `for/first`, `for/last`, `for/list`, `for/vector`, `for/hash`, `for/hasheq`, `for/hasheqv`, `for/sum`, `for/product`, `for*/and`, `for*/or`, `for*/first`, `for*/last`, `for*/list`, `for*/vector`, `for*/hash`, `for*/hasheq`, `for*/hasheqv`, `for*/sum`, `for*/product` | `(_ fc* . body)` |
| Iteration | `for/fold`, `for*/fold` | `(_ fc* fc* . body)` |
| Objects | `object` | `(_ fc* . fc*)` |
| Drawings | `with-drawing`, `with-shape` | `(_ e . body)` |
| Logging | `log-using`, `log-with-tag`, `log-into-file`, `log-from-severity`, `log-dropping-below-severity` | `(_ e . body)` |
| Testing | `test-group` | `(_ e . body)` |

### Procedures

**(pretty-print-style _symbol_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pretty-print-style _symbol style_)**  

Looks up or defines the style of forms starting with _symbol_ in the LispKit style registry, which is used by default when printing code. _symbol_ is the keyword of the forms, e.g. `let`. If called with a single argument, `pretty-print-style` returns the style associated with _symbol_, or `#f` if there is no style associated with _symbol_. If called with two arguments, `pretty-print-style` associates _style_ with _symbol_. _style_ is either a style, i.e. a pattern like `(_ e . body)` as described in section [Style syntax](#style-syntax), or `#f`, which removes the association, such that forms starting with _symbol_ are formatted like procedure calls. An error is signaled if _style_ is not a valid style.

```scheme
(pretty-print-style 'when)  ⇒  (_ e . body)
(pretty-print-style 'with-lock (pretty-print-style 'when))
(pretty-print '(with-lock (lock-of account) (withdraw! account 100) (deposit! savings 100))
              '((code . #t) (width . 40)))
⇒
(with-lock (lock-of account)
  (withdraw! account 100)
  (deposit! savings 100))
```

**(pretty-print-style-registry)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pretty-print-style-registry _which_)**  

Returns a style registry. _which_ is a symbol selecting the registry: `lispkit` (the default) selects the current LispKit registry, which contains styles for the syntax of R7RS and of LispKit libraries; `scheme` selects the Scheme registry, which only contains styles for the syntax of R7RS. Registries are immutable values; `pretty-print-style` replaces the LispKit registry with a new one. Registries can be passed to the printing procedures via configuration key `styles`.

**(pretty-print-style-lookup _registry symbol_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the style associated with _symbol_ in a style registry, or `#f` if there is no such association. _registry_ is a style registry, e.g. as returned by `pretty-print-style-registry`. _symbol_ is the keyword of the forms whose style is looked up.

```scheme
(pretty-print-style-lookup (pretty-print-style-registry 'scheme) 'let-optionals)
⇒ #f
```

**(pretty-print-style-add _registry symbol style_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new style registry which is derived from a given registry by associating a style with a symbol. _registry_ is the style registry to derive from, e.g. as returned by `pretty-print-style-registry`; it is not modified. _symbol_ is the keyword of the forms whose style is defined. _style_ is either a style, i.e. a pattern like `(_ e . body)` as described in section [Style syntax](#style-syntax), or `#f`, in which case the returned registry does not associate a style with _symbol_. An error is signaled if _style_ is neither `#f` nor a valid style. The returned registry can be passed to the printing procedures via configuration key `styles`.

```scheme
(pretty-print '(my-loop lp ((i 0)) (display i) (lp (+ i 1)))
  `((code . #t)
    (width . 25)
    (styles . ,(pretty-print-style-add (pretty-print-style-registry)
                                       'my-loop '(_ i? fc* . body)))))
⇒
(my-loop lp ((i 0))
  (display i)
  (lp (+ i 1)))
```

## Hooks

_Hooks_ extend the pretty printer to new data types, or customize how existing data types are printed. A hook "explains" an object to the printer in terms the printer understands: as a generalized list, as a vector of numbers, as an atom, or as a read macro. The choice of a hook should be based on the desired printed representation, not on the internal structure of objects. For instance, an object that should be printed as a single token is best described as an atom, even if it is internally a sequence.

Hooks are associated with _test_ procedures in a _hook registry_. When printing an object, the printer applies the tests of the registry in order; the first test that succeeds determines the hook used for the object. Objects for which no test succeeds are printed using the built-in rules. There are two kinds of tests:

- _Predicates_ are associated with a hook. If a predicate returns true for an object, the associated hook is used.
- _Hook-returning procedures_ are registered with `#t` instead of a hook. Such a procedure returns a hook for objects it handles and `#f` for all other objects.

The library maintains a single shared hook registry, which is used by default. It contains hooks for boxes, mutable pairs, growable vectors, hashtables, and records (see section [Shared and circular structures](#shared-and-circular-structures)). Hooks registered by users are applied before these hooks. Configuration key `hooks` can be used to provide a different registry for an individual call.

**(pretty-print-hook _test_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pretty-print-hook _test hook_)**  

Looks up or registers a test in the shared hook registry, which is used by default by all printing procedures. _test_ is a procedure accepting one argument, an object to print. If called with a single argument, `pretty-print-hook` returns the hook associated with _test_, `#t` if _test_ is registered as a hook-returning procedure, or `#f` if _test_ is not registered. If called with two arguments, `pretty-print-hook` modifies the shared hook registry depending on _hook_: if _hook_ is a hook, created by one of the procedures `pretty-print-hook-glst`, `pretty-print-hook-bvec`, `pretty-print-hook-atom`, or `pretty-print-hook-rmac`, _test_ is registered as a predicate associated with _hook_; if _hook_ is `#t`, _test_ is registered as a hook-returning procedure; if _hook_ is `#f`, _test_ is removed from the registry. New tests are added to the beginning of the registry, i.e. they are applied before all existing ones; re-registering an existing test keeps its position.

```scheme
(define-record-type point (make-point x y) point? (x point-x) (y point-y))
(pretty-print-hook point?
  (pretty-print-hook-atom #f
    (lambda (p) 15)
    (lambda (p radix)
      (list "#<point " 'number (number->string (point-x p) radix) " "
            'number (number->string (point-y p) radix) ">"))))
(pretty-print (list (make-point 1 2) (make-point 30 40)) '((radix . 16)))
⇒ (#<point 1 2> #<point 1e 28>)
```

**(pretty-print-hook-registry)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the current shared hook registry, which is used by default by all printing procedures. Registries are immutable values; `pretty-print-hook` replaces the shared registry with a new one. A registry derived from the shared registry via `pretty-print-hook-add` can be passed to the printing procedures via configuration key `hooks`.

**(pretty-print-hook-lookup _registry test_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the hook associated with a test in a hook registry, `#t` if the test is registered as a hook-returning procedure, or `#f` if the test is not registered. _registry_ is a hook registry, e.g. as returned by `pretty-print-hook-registry`. _test_ is the test procedure to look up.

**(pretty-print-hook-add _registry test hook_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new hook registry which is derived from a given registry by registering a test. _registry_ is the hook registry to derive from, e.g. as returned by `pretty-print-hook-registry`; it is not modified. _test_ is a procedure accepting one argument, an object to print. _hook_ determines how _test_ is registered: if _hook_ is a hook, _test_ is registered as a predicate associated with _hook_; if _hook_ is `#t`, _test_ is registered as a hook-returning procedure; if _hook_ is `#f`, the returned registry does not contain _test_. New tests are added to the beginning of the registry; re-registering an existing test keeps its position. The returned registry can be passed to the printing procedures via configuration key `hooks`.

```scheme
(define (point-vector-hook x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) 'point)
       (pretty-print-hook-glst "#<point "
                               (lambda (v) (cdr (vector->list v)))
                               (lambda (l) (list->vector (cons 'point l)))
                               ">")))
(pretty-print (list (vector 'point 1 2) (vector 1 2))
  `((hooks . ,(pretty-print-hook-add (pretty-print-hook-registry) point-vector-hook #t))))
⇒ (#<point 1 2> #(1 2))
```

**(pretty-print-hook-glst _prefix cf xf suffix_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a hook which presents objects as generalized lists. Their elements are printed like the elements of a list, i.e. with line breaks, abbreviations, and datum labels if needed. _prefix_ is the string printed before the elements, e.g. `"#["`. _cf_ is a procedure which converts an object into a list of its elements. _xf_ is a procedure which recreates an object from a list of elements; the printer uses it when it replaces elements, e.g. by datum labels. _suffix_ is the string printed after the elements, e.g. `"]"`.

```scheme
(define-record-type bag (make-bag items) bag? (items bag-items))
(pretty-print (make-bag '(alpha beta gamma delta))
  `((width . 15)
    (hooks . ,(pretty-print-hook-add (pretty-print-hook-registry) bag?
                (pretty-print-hook-glst "#[" bag-items make-bag "]")))))
⇒
#[alpha
  beta
  gamma
  delta]
```

**(pretty-print-hook-bvec _prefix lf rf suffix_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a hook which presents objects as vectors of numbers, like bytevectors. The numbers are printed filling lines, using the radix defined by configuration key `radix`. _prefix_ is the string printed before the numbers, e.g. `"#u8("`. _lf_ is a procedure which returns the number of elements of an object. _rf_ is a procedure which takes an object and an index and returns the number at that index. _suffix_ is the string printed after the numbers, e.g. `")"`.

```scheme
(define-record-type samples (make-samples data) samples? (data samples-data))
(pretty-print (make-samples (vector 10 20 30 40 50 60 70 80 90 100))
  `((width . 20)
    (hooks . ,(pretty-print-hook-add (pretty-print-hook-registry) samples?
                (pretty-print-hook-bvec "#samples("
                  (lambda (s) (vector-length (samples-data s)))
                  (lambda (s i) (vector-ref (samples-data s) i))
                  ")")))))
⇒
#samples(10 20 30 40
         50 60 70 80
         90 100)
```

**(pretty-print-hook-atom _shareable? wf df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a hook which presents objects as indivisible atoms. _shareable?_ is a boolean determining whether objects can be labeled with datum labels by `pretty-print-shared`. _wf_ is a procedure which returns the width of the printed representation of an object in columns; the printer uses it for deciding on line breaks. _df_ is a procedure which takes an object and a radix (an exact integer, e.g. 10) and returns a list of strings which make up the printed representation of the object. Each string in this list can be preceded by a semantic color symbol (see section [Colors](#colors)), which determines the color of the string.

**(pretty-print-hook-rmac _prefix ef xf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a hook which presents objects as read macros, like `'a` representing `(quote a)`. _prefix_ is the string which represents the read macro, e.g. `"'"`. _ef_ is a procedure which extracts the object to print after the prefix, e.g. `cadr`. _xf_ is a procedure which is the reverse of _ef_, e.g. `(lambda (x) (list 'quote x))`. Objects presented as read macros are colored like quoted data.

```scheme
(define (lazy? x)
  (and (pair? x) (eq? (car x) 'delay) (pair? (cdr x)) (null? (cddr x))))
(pretty-print '(f (delay x) (delay (g y)))
  `((hooks . ,(pretty-print-hook-add (pretty-print-hook-registry) lazy?
                (pretty-print-hook-rmac "~" cadr (lambda (x) (list 'delay x)))))))
⇒ (f ~x ~(g y))
```

## Relationship to SRFI 272

Library `(lispkit pretty-print)` is implemented on top of the pretty printer of [SRFI 272](https://srfi.schemers.org/srfi-272/srfi-272.html), more specifically libraries `(srfi 272 advanced)` and `(srfi 272 fancy)`. It differs from these libraries in the following ways:

- The procedures for printing data use the data layout by default, whereas the procedures of SRFI 272 use the code layout by default.
- The procedures are configured via association lists instead of keyword arguments and parameter objects. Each configuration key corresponds to a parameter object of SRFI 272 whose name is the key prefixed with `pp-`, e.g. key `width` corresponds to parameter object `pp-width`; key `color-tier` corresponds to parameter object `sgr-support-tier` of library `(srfi 272 colorize)`. Except for `code`, the defaults of the configuration keys are the values of these parameter objects.
- The LispKit style registry is used by default; the Scheme style registry is the registry of SRFI 272. Hooks are shared with SRFI 272.
- Procedures `pp`, `pp*`, and `make-pprint-generator` are not provided. Procedures `pretty-print->string` and `pretty-print-string` have no counterpart in SRFI 272.

Library `(lispkit pretty-print)` supersedes library `(lispkit prettify)`, which is kept for compatibility reasons. Directive `~Y` of SRFI 48 uses procedure `pretty-print` of this library with the code layout.
