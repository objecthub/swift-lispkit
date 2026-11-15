# SRFI 272: Fancy Pretty Printing

Library `(srfi 272 fancy)` provides all the functionality of library `(srfi 272 advanced)`. It enhances procedure `pprint-file` such that comments within expressions are preserved, it adds support for "style comments", which define formatting styles on a per-file basis, and it provides a variant of `pprint-file` which generates HTML. Library `(srfi 272 fancy)` can be imported together with `(srfi 272 advanced)`; all identifiers exported by both libraries, except for `pprint-file`, refer to the same bindings.

## Procedures

**(pprint-file _infile [key value] ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pprint-file _infile outfile [key value] ..._)**  

Reads all expressions from the text file at path _infile_ and pretty prints them as code to a new text file at path _outfile_. If _outfile_ is not provided, the output is written to `(current-output-port)`. The procedure can be configured via keyword arguments and parameter objects.

If parameter `pp-decorate` is true (the default), `pprint-file` uses a reader which collects line comments and associates them with the expressions they precede or follow. The pretty printer then emits these comments when printing the expressions. Line comments and whitespace between top-level expressions are copied verbatim, and line comments at the end of top-level expressions are preserved. Comments preceding or following expressions which are part of a sequence of body expressions, as well as empty lines between such expressions, are preserved as well. Comments in other positions, e.g. following the head of a definition or preceding the first expression of a body, and block comments `#| ... |#` get dropped. If `pp-decorate` is `#f`, all comments are dropped and top-level expressions are separated by an empty line.

While processing the head of the file, i.e. the lines before the first expression, `pprint-file` recognizes an Emacs-style "magic comment" for setting parameters of the pretty printer on a per-file basis, e.g. `;; -*- mode: scheme; fill-column: 70; pp-max-tab: 2 -*-`. The following keys are mapped to parameter objects: `pp-width:` and `fill-column:` to `pp-width`, `pp-inline-width:` to `pp-inline-width`, `pp-miser-width:` to `pp-miser-width`, `pp-tab:` to `pp-tab`, `pp-max-tab:` to `pp-max-tab`, and `pp-brackets:` to `pp-brackets`. Values are numbers or the symbols `t` (for `#t`) and `nil` (for `#f`). These settings take precedence over the values of the parameter objects, but they are overridden by keyword arguments of `pprint-file`.

Top-level line comments of the following form are interpreted as "style comments":

```
;; * pp-styles: symbol := style [; symbol := style ...]
```

A style comment associates a style with a symbol for the rest of the file. A style is either a symbol whose style gets copied, e.g. `let`, or an explicit style starting with an underscore, e.g. `(_ e . body)`. The grammar of styles is described in the documentation of [`(srfi 272 advanced)`](<(srfi 272 advanced).md>). Style comments do not affect the style registry `(pp-styles)` outside of `pprint-file`.

For example, consider a file with the following content:

```scheme
;; -*- mode: scheme; fill-column: 40 -*-
;; * pp-styles: with-lock := when

(define (transfer! from to amount)
  (check-balance from amount) ; may raise an error
  ;; locking is required
  (with-lock (lock-of from) (withdraw! from amount) (deposit! to amount)))
(transfer! a b 100) ; example
```

Pretty printing this file with `pprint-file` results in the following output:

```scheme
;; -*- mode: scheme; fill-column: 40 -*-
;; * pp-styles: with-lock := when

(define (transfer! from to amount)
  (check-balance from amount) ; may raise an error
  ;; locking is required
  (with-lock (lock-of from)
    (withdraw! from amount)
    (deposit! to amount)))
(transfer! a b 100) ; example
```

**(pprint-file/html _infile [key value] ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pprint-file/html _infile outfile [key value] ..._)**  

Reads all expressions from the text file at path _infile_ and pretty prints them into a new HTML file at path _outfile_. If _outfile_ is not provided, the HTML document is written to `(current-output-port)`. This procedure is configured like `pprint-file` and handles comments and whitespace in the same way. The generated document consists of a `pre` element containing the pretty-printed code. If `pp-color` is enabled, the code is marked up using `span` elements whose CSS classes correspond to the semantic color symbols described in the documentation of `(srfi 272 colorize)`. The document defines a style sheet for these classes derived from the default color theme.

```scheme
(pprint-file/html "transfer.scm" "transfer.html" pp-color #t)
```

## Other exports

All other procedures and parameter objects are imported from `(srfi 272 advanced)` and re-exported: `pp`, `pp*`, `pprint`, `pprint-shared`, `pprint-simple`, `make-pprint-generator`, `pretty-style`, `lookup-pp-style`, `add-pp-style`, `pretty-hook`, `lookup-pp-hook`, `add-pp-hook`, `rmac-pp-hook`, `glst-pp-hook`, `bvec-pp-hook`, `atom-pp-hook`, `pp-width`, `pp-circle`, `pp-graph`, `pp-radix`, `pp-length`, `pp-level`, `pp-lines`, `pp-level-stub`, `pp-length-stub`, `pp-lines-stub`, `pp-indent`, `pp-tab`, `pp-max-tab`, `pp-miser-width`, `pp-inline-width`, `pp-brackets`, `pp-code`, `pp-pretty`, `pp-newline`, `pp-color`, `pp-emit`, `pp-tint`, `pp-decorate`, `pp-styles`, and `pp-hooks`.
