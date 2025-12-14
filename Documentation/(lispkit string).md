# LispKit String

Strings are sequences of characters. In LispKit, characters are UTF-16 code units. Strings are written as sequences of characters enclosed within quotation marks (`"`). Within a string literal, various escape sequences represent characters other than themselves. Escape sequences always start with a backslash `\`:

- `\a`: alarm (U+0007)
- `\b`: backspace (U+0008)
- `\t`: character tabulation (U+0009)
- `\n`: linefeed (U+000A)
- `\r`: return (U+000D)
- `\"`: double quote (U+0022)
- `\\`: backslash (U+005C)
- `\|`: vertical line (U+007C)
- `\`_line-end_: used for encoding multi-line string literals
- `\x`_hex-scalar-value_`;`: specified character

The result is unspecified if any other character in a string occurs after a backslash.

Except for a line ending, any character outside of an escape sequence stands for itself in the string literal. A line ending which is preceded by a backslash expands to nothing and can be used to encode multi-line string literals.

```scheme
(display "The word \"recursion\" has many meanings.")  ⇒
The word "recursion" has many meanings.
(display "Another example:\ntwo lines of text.")  ⇒
Another example:
two lines of text.
(display "\x03B1; is named GREEK SMALL LETTER ALPHA.")  ⇒
α is named GREEK SMALL LETTER ALPHA.
```

The length of a string is the number of characters, i.e. UTF-16 code units, that it contains. This number is an exact, non-negative integer that is fixed when the string is created. The valid indexes of a string are the exact non-negative integers less than the length of the string. The first character of a string has index 0, the second has index 1, and so on.

Some of the procedures that operate on strings ignore the difference between upper and lower case. The names of the versions that ignore case end with `-ci` (for “case insensitive”).

LispKit only supports mutable strings.

## Basic constructors and procedures

**(make-string _k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-string _k char_)**  

The `make-string` procedure returns a newly allocated string of length _k_. If _char_ is given, then all the characters of the string are initialized to _char_, otherwise the contents of the string are unspecified.

**(string _char ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a newly allocated string composed of the arguments. It is analogous to procedure `list`.

**(list-\>string _list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a newly allocated string composed of the characters contained in _list_.

**(string-ref _str k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `string-ref` procedure returns character _k_ of string _str_ using zero-origin indexing. It is an error if _k_ is not a valid index of string _str_.

**(string-set! _str k char_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `string-set!` procedure stores _char_ in element _k_ of string _str_. It is an error if _k_ is not a valid index of string _str_.

**(string-length _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of characters in the given string _str_.

## Predicates

**(string? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a string; otherwise returns `#f`.

**(string-empty? _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _str_ is an empty string, i.e. a string of length 0. Otherwise, `string-empty?` returns `#f`.

**(string=? _str ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if all the strings have the same length and contain exactly the same characters in the same positions; otherwise `string=?` returns `#f`.

**(string\<? _str ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string\>? _str ..._)**  
**(string\<=? _str ..._)**  
**(string\>=? _str ..._)**  

These procedures return `#t` if their arguments are (respectively): monotonically increasing, monotonically decreasing, monotonically non-decreasing, or monotonically non-increasing. These predicates are transitive.

These procedures compare strings in a lexicographic fashion; i.e. `string<?` implements a the lexicographic ordering on strings induced by the ordering `char<?` on characters. If two strings differ in length but are the same up to the length of the shorter string, the shorter string would be considered to be lexicographically less than the longer string.

A pair of strings satisfies exactly one of `string<?`, `string=?`, and `string>?`. A pair of strings satisfies `string<=?` if and only if they do not satisfy `string>?`. A pair of strings satisfies `string>=?` if and only if they do not satisfy `string<?`.

**(string-ci=? )** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if, after case-folding, all the strings have the same length and contain the same characters in the same positions; otherwise `string-ci=?` returns `#f`.

**(string-ci\<? _str ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-ci\<=? _str ..._)**   
**(string-ci\>? _str ..._)**   
**(string-ci\>=? _str ..._)**   

These procedures compare strings in a case-insensitive fashion. The "-ci" procedures behave as if they applied `string-foldcase` to their arguments before invoking the corresponding procedures without "-ci".

**(string-contains? _str sub_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if string _str_ contains string _sub_; returns `#f` otherwise.

**(string-prefix? _str sub_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if string _str_ has string _sub_ as a prefix; returns `#f` otherwise.

**(string-suffix? _str sub_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if string _str_ has string _sub_ as a suffix; returns `#f` otherwise.

## Composing and extracting strings

Many of the following procedures accept an optional _start_ and _end_ argument as their last two arguments. If both or one of these optional arguments are not provided, _start_ defaults to `0` and _end_ defaults to the length of the corresponding string.

**(string-contains _str sub_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-contains _str sub start_)**  
**(string-contains _str sub start end_)**  

This procedure checks whether string _sub_ is contained in string _str_ within the index range _start_ to _end_. It returns the first index into _str_ at which _sub_ is fully contained within _start_ and _end_. If _sub_ is not contained in the substring of _str_, then `#f` is returned.

**(substring _str start end_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `substring` procedure returns a newly allocated string
formed from the characters of string _str_ beginning with index _start_ and ending with index _end_. This is equivalent to calling `string-copy` with the same arguments, but is provided for backward compatibility and stylistic flexibility.

**(string-append _str ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a newly allocated string whose characters are the concatenation of the characters in the given strings _str ..._.

**(string-concatenate _list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-concatenate _list sep_)**   

Returns a newly allocated string whose characters are the concatenation of the characters in the strings contained in _list_. _sep_ is either a character or string, which, if provided, is used as a separator between two strings that get concatenated. It is an error if _list_ is not a proper list containing only strings as elements.

**(string-upcase _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-downcase _str_)**  
**(string-titlecase _str_)**  
**(string-foldcase _str_)**  

These procedures apply the Unicode full string uppercasing, lowercasing, titlecasing, and case-folding algorithms to their argument string _str_ and return the result as a newly allocated string. It is not guaranteed that the resulting string has the same lenght like _str_. Language-sensitive string mappings and foldings are not used.

**(string-normalize-diacritics _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Procedure `string-normalize-diacritics` transforms the given string _str_ by normalizing diacritics and returning the result as a newly allocated string.

```scheme
(string-normalize-diacritics "Meet Chloë at São Paulo Café")
⇒  "Meet Chloe at Sao Paulo Cafe"
```

**(string-normalize-separators _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-normalize-separators _str sep_)**  
**(string-normalize-separators _str sep cset_)**  

Procedure `string-normalize-separators` normalizes string _str_ by replacing sequences of separation characters from character set _cset_ with string or character _sep_. If _sep_ is not provided, `" "` is used as a default. If _cset_ is not provided, all unicode newline and whitespace characters are used as a default for _cset_. _cset_ is either a string of separation characters or a character set as defined by library `(lispkit char-set)`.

**(string-encode-named-chars _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-encode-named-chars _str required-only?_)**  

Procedure `string-encode-named-chars` returns a new string, replacing characters with their corresponding named XML entity in string _str_. If parameter `required-only?` is set to `#f`, all characters with corresponding named XML entities are being replaced, otherwise only the required characters are replaced.

```scheme
(string-encode-named-chars "<one> & two = 3")
⇒  "&LT;one&gt; &AMP; two &equals; 3"
(string-encode-named-chars "<one> & two = 3" #t)
⇒  "&lt;one&gt; &amp; two = 3"
```

**(string-decode-named-chars _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Procedure `string-decode-named-chars` returns a new string, replacing named XML entities with their corresponding character.

```scheme
(string-decode-named-chars "2&Hat;&lcub;3&rcub; &equals; 8")
⇒  "2^{3} = 8"
```

**(string-copy _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-copy _str start_)**  
**(string-copy _str start end_)**  

Returns a newly allocated copy of the part of the given string _str_ between _start_ and _end_. The default for _start_ is 0, for _end_ it is the length of _str_. Calling `string-copy` is equivalent to calling `substring` with the same arguments. `substring` is provided primarily for backward compatibility.

**(string-split _str sep allow-empty?_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Procedure `string-split` splits string _str_ using the separator _sep_ and returns a list of the component strings, in order. _sep_ is either a string or a character. Boolean argument _allow-empty?_ determines whether empty component strings are dropped. _allow-empty?_ is `#t` by default.

```scheme
(string-split "name-|-street-|-zip-|-city-|-" "-|-")  ⇒  ("name" "street" "zip" "city" "")
(string-split "name-|-street-|-zip-|-city-|-" "-|-" #f)  ⇒  ("name" "street" "zip" "city")
```

**(string-trim _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-trim _str chars_)**

Returns a newly allocated string by removing all characters from the beginning and end of string _str_ that are contained in _chars_. _chars_ is either a string or it is a character set. If _chars_ is not provided, whitespaces and newlines are being removed.

```scheme
(string-trim "  lispkit is fun ")                             ⇒  "lispkit is fun"
(string-trim "________" "_")                                  ⇒  ""
(string-trim "712+72=784" (char-set->string char-set:digit))  ⇒  "+72="
(string-trim "712+72=784" char-set:digit)                     ⇒  "+72="
```

**(string-pad-right _str char k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-pad-right _str char k force-length?_)**

Procedure `string-pad-right` returns a newly allocated string created by padding string _str_ at the beginning of the string with character _char_ until it is of length _k_. If _k_ is less than the length of string _str_, the resulting string gets truncated at length _k_ if boolean argument _force-length?_ is `#t`; otherwise, the string _str_ gets returned as is. 

```scheme
(string-pad-right "scheme" #\space 8)    ⇒  "scheme  "
(string-pad-right "scheme" #\x 4)        ⇒  "scheme"
(string-pad-right "scheme" #\x 4 #t)     ⇒  "sche"
(string-pad-right "scheme" "_" 10)       ⇒  "scheme____"
```

**(string-pad-left _str char k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-pad-left _str char k force-length?_)**

Procedure `string-pad-left` returns a newly allocated string created by padding string _str_ at the beginning of the string with character _char_ until it is of length _k_. If _k_ is less than the length of string _str_, the resulting string gets truncated at length _k_ if boolean argument _force-length?_ is `#t`; otherwise, the string _str_ gets returned as is. 

```scheme
(string-pad-left "scheme" #\space 8)    ⇒  "  scheme"
(string-pad-left "scheme" #\x 4)        ⇒  "scheme"
(string-pad-left "scheme" #\x 4 #t)     ⇒  "heme"
(string-pad-left "scheme" "_" 10)       ⇒  "____scheme"
```

**(string-pad-center _str char k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-pad-center _str char k force-length?_)**

Procedure `string-pad-center` returns a newly allocated string created by padding string _str_ at the beginning and end with character _char_ until it is of length _k_, such that _str_ is centered in the middle. If _k_ is less than the length of string _str_, the resulting string gets truncated at length _k_ if boolean argument _force-length?_ is `#t`; otherwise, the string _str_ gets returned as is. 

```scheme
(string-pad-center "scheme" #\space 8)  ⇒  " scheme "
(string-pad-center "scheme" #\x 4)      ⇒  "scheme"
(string-pad-center "scheme" #\x 4 #t)   ⇒  "heme"
(string-pad-center "scheme" "_" 10)     ⇒  "__scheme__"
```

## Manipulating strings

**(string-replace! _str sub repl_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-replace! _str sub repl start_)**  
**(string-replace! _str sub repl start end_)**  

Replaces all occurences of string _sub_ in string _str_ between indices _start_ and _end_ with string _repl_ and returns the number of occurences of _sub_ that were replaced.

**(string-replace-first! _str sub repl_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-replace-first! _str sub repl start_)**  
**(string-replace-first! _str sub repl start end_)**  

Replaces the first occurence of string _sub_ in string _str_ between indices _start_ and _end_ with string _repl_ and returns the index at which the first occurence of _sub_ was replaced.

**(string-insert! _str repl_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-insert! _str repl start_)**  
**(string-insert! _str repl start end_)**  

Replaces the part of string _str_ between index _start_ and _end_ with string _repl_. The default for _start_ is 0, for _end_ it is _start_ (i.e. if not provided, _end_ is equals to _start_). If both _start_ and _end_ are not provided, `string-insert!` inserts _repl_ at the beginning of _str_. If _start_ is provided alone (without _end_), `string-insert!` inserts _repl_ at position _start_.

```scheme
(define s "Zenger is my name")
(string-insert! s "Matthias ")
s  ⇒  "Matthias Zenger is my name"
(string-insert! s "has always been" 16 18)
s  ⇒  "Matthias Zenger has always been my name"
```

**(string-append! _str other ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>   

Appends the strings _other_, ... to mutable string _str_ in the given order.

**(string-copy! _to at from_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-copy! _to at from start_)**  
**(string-copy! _to at from start end_)**  

Copies the characters of string _from_ between index _start_ and _end_ to string _to_, starting at index _at_. If the source and destination overlap, copying takes place as if the source is first copied into a temporary string and then into the destination. It is an error if _at_ is less than zero or greater than the length of string _to_. It is also an error if `(- (string-length to) at)` is less than `(- end start)`.

**(string-fill! _str fill_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-fill! _str fill start_)**  
**(string-fill! _str fill start end_)**  

The `string-fill!` procedure stores _fill_ in the elements of string _str_ between index _start_ and _end_. It is an error if _fill_ is not a character.

## Iterating over strings

**(string-map _proc str ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `string-map` procedure applies procedure _proc_ element-wise to the characters of the strings _str ..._ and returns a string of the results, in order. If more than one string _str_ is given and not all strings have the same length, `string-map` terminates when the shortest string runs out. It is an error if _proc_ does not accept as many arguments as there are strings and returns a single character.

```scheme
(string-map char-foldcase "AbdEgH")  ⇒  "abdegh"
(string-map (lambda (c) (integer->char (+ 1 (char->integer c)))) "HAL")  ⇒  "IBM"
```

**(string-for-each _proc str ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The arguments to `string-for-each` are like the arguments to `string-map`, but `string-for-each` calls _proc_ for its side effects rather than for its values. Unlike `string-map`, `string-for-each` is guaranteed to call _proc_ on the characters of the strings in order from the first character to the last. If more than one string _str_ is given and not all strings have the same length, `string-for-each` terminates when the shortest string runs out. It is an error for _proc_ to mutate any of the strings. It is an error if _proc_ does not accept as many arguments as there are strings.

## Converting strings

**(string-\>list _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string-\>list _str start_)**  
**(string-\>list _str start end_)**  

The `string->list` procedure returns a list of the characters of string _str_ between _start_ and _end_ preserving the order of the characters.

## Input/Output

**(read-file _path_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Reads the text file at _path_ and stores its content in a newly allocated string which gets returned by `read-file`.

**(write-file _path str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Writes the characters of string _str_ into a new text file at _path_. `write-file` returns `#t` if the file could be written successfully; otherwise `#f` is returned.
