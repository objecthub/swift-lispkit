# SRFI 272: Intermediate Pretty Printing

Library `(srfi 272 intermediate)` provides all the functionality of library `(srfi 272 basic)`. It extends the printing procedures with keyword arguments, adds procedure `pp*` accepting a list of keyword arguments, procedure `pprint-file` for pretty printing source files, parameter objects for controlling the radix of numbers and for limiting the depth and length of the printed output, and procedure `pretty-style` for customizing the formatting of special forms.

Unlike `(srfi 272)` and `(srfi 272 basic)`, this library formats lists as Scheme code, following established conventions for the layout of special forms such as `define`, `let`, `if`, or `cond`.

## Keyword arguments

All printing procedures of this library accept _keyword arguments_ in the form of alternating keys and values following the object to print and the optional port. Each key is one of the parameter objects exported by this library, e.g. `pp-width`, and the value following it overrides the value of the parameter object for the duration of the call. Keyword arguments are scanned from left to right; the first occurrence of a parameter object determines its value. Keyword arguments are an alternative to wrapping a call in `parameterize`. They enable procedural abstractions, such as bundling parameter values into configuration lists.

```scheme
(import (srfi 272 intermediate))
(pp '(255 #(16 7/3) -42) pp-radix 16)
⇒ (#xff #(#x10 #x7/3) #x-2a)
```

## Procedures

**(pp _obj_ [_port_] [_key value_] ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Writes a pretty-printed representation of _obj_ to _port_, which defaults to the value of `(current-output-port)`. The output is always followed by a newline. Keyword arguments _key value_ ... override the values of the corresponding parameter objects.

```scheme
(pp '(define (fold kons knil ls)
       (let lp ((ls ls) (acc knil))
         (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc)))))
    pp-level 2 pp-length 3)
⇒
(define (fold kons knil ...) (let lp (...) ...))
```

**(pp* _obj_ [_port_] [_key value_] ... _kv-list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

This variant of `pp` accepts keyword arguments followed by a final argument _kv-list_ which is a list of alternating keys and values. This final list is spliced into the call, as if its contents were passed to `pp` via `apply`.

```scheme
(define my-config (list pp-width 40 pp-radix 2))
(pp* '(1 2 3 4 5 6 7 8 9 10) (current-output-port) my-config)
⇒
(#b1 #b10 #b11 #b100 #b101 #b110 #b111
  #b1000 #b1001 #b1010)
```

**(pprint _obj_ [_port_] [_key value_] ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pprint-shared _obj_ [_port_] [_key value_] ...)**  
**(pprint-simple _obj_ [_port_] [_key value_] ...)**  

These procedures correspond to the procedures of the same name in library `(srfi 272 basic)`, extended to accept keyword arguments. The values of `pp-graph` and `pp-circle`, which are hardwired into these procedures, take precedence over values supplied via keyword arguments or parameter objects. Use `apply` for passing a list of keyword arguments.

**(pprint-file _infile_ [_outfile_] [_key value_] ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Reads all expressions from the text file at path _infile_ and pretty prints them to a new text file at path _outfile_. If _outfile_ is not provided, the output is written to `(current-output-port)`. Like `pprint`, this procedure can be configured via keyword arguments and parameter objects. Line comments preceding top-level expressions and empty lines between top-level expressions are preserved; all other comments are dropped.

## Parameter objects

**pp-width** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  
**pp-graph**  
**pp-circle**  

These parameter objects are described in the documentation of library [`(srfi 272 basic)`](<(srfi 272 basic).md>).

**pp-radix** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

The radix used for printing exact numbers. Supported values are 2, 8, 10, and 16. For non-decimal radices, the prefixes `#b`, `#o`, and `#x` get printed to keep the output readable. Inexact numbers are always printed in decimal notation. The default is 10.

**pp-level** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  
**pp-length**  

`pp-level` limits how deeply the printer descends into nested structures. Its value is either `#f` (no limit) or an exact non-negative integer. The object passed to the printer is at level 0; if it is a list or vector, its elements are at level 1, their elements at level 2, and so on. When the printer encounters a compound object at a level equal to or exceeding the limit, it prints the "shell" of the object with its content replaced by `...`. Atomic values are not affected by this limit.

`pp-length` limits how many elements are shown in a list, vector, or other list-like object. Its value is either `#f` (no limit) or an exact non-negative integer. If an object contains more elements than permitted, the printer writes the leading elements followed by `...`. For dotted lists that contain exactly as many elements as the limit, the terminating atom is printed instead. The default of both parameter objects is `#f`.

## Styles

**(pretty-style _symbol_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pretty-style _symbol style_)**  

Forms starting with a special keyword, such as `define`, `let`, or `cond`, are formatted following conventions dictated by their semantics. `pretty-style` provides control over such formatting by associating styles with the leading symbols of forms.

If called with a single argument, `pretty-style` returns the style associated with _symbol_, or `#f` if no style is associated with _symbol_. A style is an opaque object distinct from `#f` in this library. If called with two arguments, `pretty-style` associates _style_ with _symbol_. _style_ is either a style returned by a one-argument call of `pretty-style`, or `#f`, which removes the association, resetting the formatting of forms starting with _symbol_ to the default used for applications.

```scheme
(pp '(my-when (> x 100) (display "x is large") (newline)
              (set! x (- x 100)))
    pp-width 30)
⇒
(my-when (> x 100)
  (display "x is large")
  (newline) (set! x (- x 100)))

(pretty-style 'my-when (pretty-style 'when))
(pp '(my-when (> x 100) (display "x is large") (newline)
              (set! x (- x 100)))
    pp-width 30)
⇒
(my-when (> x 100)
  (display "x is large")
  (newline)
  (set! x (- x 100)))
```
