# SRFI 272: Character Width Measurement

Library `(srfi 272 measure)` makes it possible to customize how the pretty printers of libraries [`(srfi 272 intermediate)`](<(srfi 272 intermediate).md>), [`(srfi 272 advanced)`](<(srfi 272 advanced).md>), and [`(srfi 272 fancy)`](<(srfi 272 fancy).md>) determine the number of terminal columns needed for displaying characters. This matters for strings and symbols which contain wide characters (e.g. Chinese, Japanese, or Korean characters), combining characters, or zero-width characters.

**char-width-procedure** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

Holds a procedure which accepts a character and returns the number of columns it takes on a terminal (0, 1, or 2), or `#f` if the character is not printable. A custom procedure can be installed by setting or parameterizing `char-width-procedure`, e.g. after probing the actual behavior of a terminal.

In LispKit, the default procedure is based on procedure `string-display-width` of library [`(lispkit string)`](<(lispkit string).md>), which implements the Unicode rules for East Asian wide characters, emoji presentation, and zero-width characters such as combining marks. Control characters are considered to be not printable.

LispKit represents strings as sequences of UTF-16 code units, i.e. characters outside of the Basic Multilingual Plane, such as most emoji, consist of two surrogate characters, each of which is assigned width 1 by the default procedure. As long as `char-width-procedure` refers to the default procedure, the pretty printers therefore measure strings and symbols as a whole via `string-display-width`. This measures characters outside of the Basic Multilingual Plane as well as grapheme clusters, like emoji sequences, correctly. Custom procedures are always applied to each character individually.

```scheme
(import (srfi 272 measure))
(map (char-width-procedure) (list #\a #\x300 #\x4E00 #\x2600 #\x231A #\x7))
⇒ (1 0 2 1 2 #f)
```
