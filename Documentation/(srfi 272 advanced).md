# SRFI 272: Advanced Pretty Printing

Library `(srfi 272 advanced)` provides all the functionality of library `(srfi 272 intermediate)`. It adds a large number of parameter objects for controlling the layout, abbreviation, and colorization of the output, an explicit style registry and an explicit hook registry, constructors for hooks that extend the pretty printer to new data types, and procedure `make-pprint-generator`, which returns a generator for the lines of a pretty-printed representation.

## Procedures

**(pp _obj_ [_port_] [_key value_] ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pp* _obj_ [_port_] [_key value_] ... _kv-list_)**  
**(pprint _obj_ [_port_] [_key value_] ...)**   
**(pprint-shared _obj_ [_port_] [_key value_] ...)**  
**(pprint-simple _obj_ [_port_] [_key value_] ...)**  

These procedures behave like the ones of library `(srfi 272 intermediate)`, taking into account all the parameter objects of this library. Keys of keyword arguments may be any parameter object exported by this library.

```scheme
(import (srfi 272 advanced))
(define code
  '(define (fold kons knil ls)
     (let lp ((ls ls) (acc knil))
       (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc))))))
(pp code)
⇒
(define (fold kons knil ls)
  (let lp ((ls ls) (acc knil))
    (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc)))))

(pp code pp-tab 2 pp-width 40)
⇒
(define (fold kons knil ls)
   (let lp ((ls ls) (acc knil))
      (if (null? ls)
          acc
          (lp (cdr ls)
              (kons (car ls) acc)))))

(pp code pp-level 2 pp-length 3 pp-level-stub "#" pp-length-stub "…")
⇒
(define (fold kons knil …) (let lp # …))
```

**(pprint-file _infile_ [_outfile_] [_key value_] ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Reads all expressions from the text file at path _infile_ and pretty prints them as code to a new text file at path _outfile_. If _outfile_ is not provided, the output is written to `(current-output-port)`. The procedure can be configured via keyword arguments and parameter objects. If `pp-decorate` is true (the default), line comments and whitespace between top-level expressions are copied verbatim to the output; other comments are dropped. If `pp-decorate` is `#f`, all comments are dropped and top-level expressions are separated by an empty line.

In decoration mode, `pprint-file` recognizes an Emacs-style "magic comment" in the head of the file, i.e. before the first expression, such as `;; -*- mode: scheme; fill-column: 70; pp-max-tab: 2 -*-`. The following keys are mapped to parameter objects: `pp-width:` and `fill-column:` to `pp-width`, `pp-inline-width:` to `pp-inline-width`, `pp-miser-width:` to `pp-miser-width`, `pp-tab:` to `pp-tab`, `pp-max-tab:` to `pp-max-tab`, and `pp-brackets:` to `pp-brackets`. Values are numbers or the symbols `t` (for `#t`) and `nil` (for `#f`). These settings take precedence over the values of the parameter objects, but they are overridden by keyword arguments of `pprint-file`.

**(make-pprint-generator _obj_ [_key value_] ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a generator in the sense of [SRFI 158](https://srfi.schemers.org/srfi-158/srfi-158.html), i.e. a procedure without arguments, which returns the lines of the pretty-printed representation of _obj_, one line at a time. All lines except for the last one are strings ending in a newline character; whether the last line ends in a newline depends on `pp-newline`. Once all lines are returned, the generator returns an end-of-file object. Keyword arguments are processed like in `pp`. If `pp-color` is enabled, lines may contain color sequences; they can be filtered out via a custom `pp-tint` procedure.

```scheme
(define gen (make-pprint-generator code pp-width 40))
(gen)  ⇒  "(define (fold kons knil ls)\n"
(gen)  ⇒  "  (let lp ((ls ls) (acc knil))\n"
```

## Parameter objects

**pp-width** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  
**pp-graph**  
**pp-circle**  
**pp-level**  
**pp-length**  

These parameter objects are described in the documentation of libraries [`(srfi 272 basic)`](<(srfi 272 basic).md>) and [`(srfi 272 intermediate)`](<(srfi 272 intermediate).md>).

**pp-radix** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

The radix used for printing exact numbers. Supported values are 2, 8, 10, 16, and `#f`. For non-decimal radices, the prefixes `#b`, `#o`, and `#x` get printed. Value `#f`, the default, makes the printer follow the conventions of `write`.

**pp-lines** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

Limits the total number of lines printed. Its value is either `#f` (no limit, the default) or an exact positive integer. If the limit is reached, printing gets terminated with a lines stub followed by all outstanding closing parenthesis.

**pp-level-stub** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  
**pp-length-stub**  
**pp-lines-stub**  

These parameter objects define the stubs printed when the output is truncated due to `pp-level`, `pp-length`, and `pp-lines` limits. `pp-length-stub` is a string; the default is `"..."`. `pp-level-stub` is either a string, or `#t` (the default), which prints the "shell" of a compound object with its content replaced by `pp-length-stub`. `pp-lines-stub` is either a string, or `#f`, which makes the printer use `pp-length-stub`. The default is `".."`.

**pp-newline** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

If set to `#f`, the newline which is normally printed at the end of the output is suppressed. The default is `#t`.

```scheme
(display "[") (pp '(a b c) pp-newline #f) (display "]")
⇒ [(a b c)]
```

**pp-inline-width** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  
**pp-miser-width**  

Both parameter objects accept `#f` or an exact non-negative integer. If `pp-inline-width` is an integer, it acts as a soft limit on the width of subexpressions which are printed on a single line. The default is 60. If `pp-miser-width` is an integer, it specifies a zone near the right margin in which expressions are formatted with minimal indentation in order to save horizontal space. The default is 20.

**pp-indent** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

Informs the printer that the output starts at the given column. This is useful when embedding pretty-printed text within a line that already contains content. All subsequent lines are indented accordingly. The default is 0.

```scheme
(display "result: ")
(pp code pp-indent 8 pp-width 50)
⇒
result: (define (fold kons knil ls)
          (let lp ((ls ls) (acc knil))
            (if (null? ls)
                acc
                (lp (cdr ls) (kons (car ls) acc)))))
```

**pp-tab** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  
**pp-max-tab**  

`pp-tab` specifies the number of columns by which the bodies of most forms are indented relative to the start of the form's keyword. Since an opening parenthesis adds one column, the total indentation relative to the parenthesis is `(pp-tab)` + 1. The default is 1. Short keywords like `if` may trigger a layout in which the first subexpression appears on the same line as the keyword, with subsequent subexpressions aligned under it. `pp-max-tab` puts an upper limit on such offsets. The default is 4.

**pp-code** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  
**pp-brackets**  

If `pp-code` is true (the default), the printed object is considered to be Scheme code and conventional code layout rules are applied. If it is `#f`, the object is printed as data. If `pp-brackets` is true, R6RS-style square brackets are used selectively instead of parenthesis, e.g. for the bindings of `let` forms. The default is `#f`. Note that the LispKit reader does not support square brackets.

```scheme
(pp code pp-brackets #t)
⇒
(define (fold kons knil ls)
  (let lp ([ls ls] [acc knil])
    (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc)))))
```

**pp-pretty** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

If set to `#f`, all whitespace introduced by the pretty printer is collapsed into single spaces, producing output similar to `write`. All other parameters remain in effect. The terminating newline is omitted as well, independent of `pp-newline`. The default is `#t`.

**pp-color** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

Enables syntax highlighting. Supported values are `#f` (no colors, the default), `#t` (use the default color theme), or a semantic color mapper as defined by library `(srfi 272 colorize)`. Whether and how colors are rendered for terminals depends on the parameter object `sgr-support-tier` of library `(srfi 272 colorize)`. A semantic color mapper is a procedure accepting a semantic color symbol and a boolean indicating whether the color starts or ends; it returns a string which is written to the output verbatim. This makes it possible to generate other forms of markup:

```scheme
(define (html-mapper sc start?)
  (if start?
      (string-append "<span class=\"" (symbol->string sc) "\">")
      "</span>"))
(pp '(define (sq x) (* x x)) pp-color html-mapper)
⇒
(<span class="keyword">define</span> (<span class="defined">sq</span> <span class="formal">x</span>) (* x x))
```

**pp-emit** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  
**pp-tint**  

The values of these parameter objects are procedures accepting a string and a port. `pp-emit` is used for writing all regular output of the pretty printer. `pp-tint` is used for writing the color sequences generated by the colorizer. The default for both is `write-string`. Custom procedures can be used for post-processing the output, e.g. for adding line numbers or filtering out color sequences.

```scheme
(pp '(a b c) pp-emit (lambda (s port) (write-string (string-upcase s) port)))
⇒ (A B C)
```

**pp-decorate** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

If set to `#f`, `pprint-file` ignores all comments in the input file and separates top-level expressions with an empty line. If set to `#t` (the default), `pprint-file` preserves comments as described above.

**pp-styles** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

Holds the style registry used by the pretty printer. The registry is a functional data structure which can be modified via `add-pp-style`. Procedure `pretty-style` modifies the registry by changing the value of `pp-styles`.

**pp-hooks** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

Holds the hook registry used by the pretty printer. The registry is a functional data structure which can be modified via `add-pp-hook`. Procedure `pretty-hook` modifies the registry by changing the value of `pp-hooks`. In LispKit, the default registry contains hooks for boxes, mutable pairs, and growable vectors.

## Styles

When printing code, forms starting with a symbol associated with a style in the style registry are formatted according to this style. In this library, styles are explicit data structures: a style is an improper list matching the following grammar, which is given in terms of the printed representation of styles:

```
‹style›     ⟶ (_ . ‹fmt-tail›)
‹fmt-tail›  ⟶ body | fill | spread | h* | dc* | ec* | fc* | lc*
             | (i? . ‹fmt-tail›) | (‹fmt› . ‹fmt-tail›)
‹fmt›       ⟶ k | i | h | d | e | f | l | dc | ec | fc | lc | ‹fmt-tail›
```

A style acts as a pattern that is matched against a form. If the form does not match the pattern, the non-matching part is printed as a datum. All terminals except for `i?` match any subform which is then printed according to the following table. If the subform corresponding to `i?` is a symbol, `i?` is treated like `i`; otherwise, it is skipped and matching continues with the same subform and the rest of the pattern.

| Terminal | Printing method |
| :--- | :--- |
| `k` | keyword |
| `i` | identifier (a bound variable) |
| `d` | datum |
| `e` | expression |
| `f` | formal parameter or (possibly improper) list of formal parameters |
| `l` | literal or list of literals |
| `h` | head of a definition (identifier or MIT-style list) |
| `dc`, `ec`, `fc`, `lc` | clause with a datum, expression, formals, or literals head followed by expressions (e.g. the clauses of `cond`, `case-lambda`, and `case`) |
| `dc*`, `ec*`, `fc*`, `lc*` | list of the corresponding clauses |
| `h*` | list of definition heads |
| `body` | list of expressions, typically indented (as in `lambda`) |
| `spread` | like `body`, but with an empty line between expressions (as in `define-library`) |
| `fill` | compact list of expressions, filling lines (as in applications) |

The following table shows a few of the predefined styles:

| Keyword | Style |
| :--- | :--- |
| `define` | `(_ h . body)` |
| `lambda` | `(_ f . body)` |
| `let` | `(_ i? fc* . body)` |
| `if` | `(_ . body)` |
| `or` | `(_ . fill)` |
| `do` | `(_ fc* ec . body)` |
| `cond` | `(_ . ec*)` |
| `case` | `(_ e . lc*)` |
| `when` | `(_ e . body)` |
| `guard` | `(_ (f . ec*) . body)` |
| `case-lambda` | `(_ . fc*)` |

**(pretty-style _symbol_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pretty-style _symbol style_)**  

If called with a single argument, `pretty-style` returns the style associated with _symbol_ in the registry `(pp-styles)`, or `#f` if no style is associated with _symbol_. If called with two arguments, it associates _style_ with _symbol_ by changing the value of `pp-styles`. If _style_ is `#f`, the association is removed.

```scheme
(pretty-style 'define)  ⇒  (_ h . body)
(pretty-style 'with-lock (pretty-style 'when))
(pp '(with-lock (lock-of account) (withdraw! account 100)
                (deposit! savings 100))
    pp-width 40)
⇒
(with-lock (lock-of account)
  (withdraw! account 100)
  (deposit! savings 100))
```

**(add-pp-style _registry symbol style_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new style registry which is derived from _registry_ by associating _style_ with _symbol_. If _style_ is `#f`, the returned registry does not associate a style with _symbol_. _registry_ is not modified. An error is signaled if _style_ is neither `#f` nor a valid style.

```scheme
(pp '(my-let loop ((i 0)) (when (< i 10) (display i) (loop (+ i 1))))
    pp-width 30
    pp-styles (add-pp-style (pp-styles) 'my-let '(_ i? fc* . body)))
⇒
(my-let loop ((i 0))
  (when (< i 10)
    (display i)
    (loop (+ i 1))))
```

**(lookup-pp-style _registry symbol_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the style associated with _symbol_ in style registry _registry_, or `#f` if there is no such association.

## Hooks

Hooks extend the pretty printer to new data types or customize how existing data types are printed. A hook "explains" an object to the printer in terms it understands: as a generalized list, as a binary vector, as an atom, or as a read macro. Hooks are associated with test procedures in a hook registry. When printing an object, the printer applies the tests in the order of the registry. The first test that succeeds determines the hook. Tests are either predicates, or procedures which return a hook object for objects they match.

**(pretty-hook _test_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pretty-hook _test hook_)**  

If called with a single argument, `pretty-hook` returns the hook associated with procedure _test_ in the registry `(pp-hooks)`, `#t` if _test_ is a procedure returning hooks, or `#f` if there is no association. If called with two arguments, `pretty-hook` associates _hook_ with _test_ by changing the value of `pp-hooks`. If _hook_ is `#t`, _test_ is a procedure returning a hook object for matching objects. If _hook_ is `#f`, the association for _test_ is removed.

```scheme
(define-record-type point (make-point x y) point? (x point-x) (y point-y))
(pretty-hook point?
  (atom-pp-hook #f
    (lambda (p) 15)
    (lambda (p radix)
      (list "#<point " (number->string (point-x p) radix) " "
            (number->string (point-y p) radix) ">"))))
(pp (list (make-point 1 2) (make-point 30 40)) pp-radix 16)
⇒ (#<point 1 2> #<point 1e 28>)
```

**(add-pp-hook _registry test hook_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new hook registry which is derived from _registry_ by associating _hook_ with procedure _test_. If _hook_ is a hook object, _test_ is a predicate. If _hook_ is `#t`, _test_ is a procedure returning a hook for matching objects and `#f` otherwise. If _hook_ is `#f`, the returned registry does not have an association for _test_. New associations are added to the beginning of the registry; existing associations for _test_ are replaced in place. _registry_ is not modified.

**(lookup-pp-hook _registry test_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the hook associated with _test_ in hook registry _registry_, `#t` if _test_ was registered as a procedure returning hooks, or `#f` if there is no association.

**(glst-pp-hook _prefix cf xf suffix_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a hook which presents objects as generalized lists. The elements are printed between strings _prefix_ and _suffix_. _cf_ converts an object into a list of its elements; _xf_ recreates an object from such a list.

```scheme
(define-record-type bag (make-bag items) bag? (items bag-items))
(pp (make-bag '(alpha beta gamma delta epsilon))
    pp-hooks (add-pp-hook (pp-hooks) bag? (glst-pp-hook "#[" bag-items make-bag "]"))
    pp-width 20)
⇒
#[alpha
  beta
  gamma
  delta
  epsilon]
```

**(bvec-pp-hook _prefix lf rf suffix_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a hook which presents objects as binary vectors, i.e. vectors of numbers, like bytevectors. The elements are printed between strings _prefix_ and _suffix_. _lf_ returns the number of elements of an object; _rf_ takes an object and an index and returns the number at that index.

**(atom-pp-hook _shareable? wf df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a hook which presents objects as indivisible atoms. _shareable?_ is a boolean indicating whether the object can be labeled with a datum label in `pp-graph` mode. _wf_ returns the width of the printed representation of an object. _df_ takes an object and a radix and returns a list of strings, each optionally preceded by a semantic color symbol (see [`(srfi 272 colorize)`](<(srfi 272 colorize).md>)), which make up the printed representation of the object.

**(rmac-pp-hook _prefix ef xf_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a hook which presents objects as read macros, like `'a` representing `(quote a)`. _prefix_ is the prefix string of the read macro, _ef_ extracts the object to print after the prefix, and _xf_ is the reverse of _ef_. User-defined read macros are colored like `quote`.

```scheme
(define (lazy? x)
  (and (pair? x) (eq? (car x) 'delay) (pair? (cdr x)) (null? (cddr x))))
(pp '(f (delay x) (delay (g y)))
    pp-hooks (add-pp-hook (pp-hooks) lazy?
               (rmac-pp-hook "~" cadr (lambda (x) (list 'delay x)))))
⇒ (f ~x ~(g y))
```
