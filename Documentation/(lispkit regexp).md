# LispKit Regexp

Library `(lispkit regexp)` provides an API for defining regular expressions and applying them to strings. Supported are both matching as well as search/replace.

## Regular expressions

The regular expression syntax supported by this library corresponds to the one of [`NSRegularExpression`](https://developer.apple.com/documentation/foundation/nsregularexpression) of Apple's _Foundation_ framework. This is also the origin of the documentation of this section.

### Meta-characters

**`\a`** : Match a _bell_ (`\u0007`)  
**`\A`** : Match at the beginning of the input. Differs from `^` in that `\A` will not match after a new line within the input.  
**`\b`** : Outside of a [Set], match if the current position is a word boundary. Boundaries occur at the transitions between word (`\w`) and non-word (`\W`) characters, with combining marks ignored. Inside of a [Set], match a _backspace_ (`\u0008`).  
**`\B`** : Match if the current position is not a word boundary.  
**`\cX`** : Match a control-X character.  
**`\d`** : Match any character with the unicode general category of `Nd`, i.e. numbers and decimal digits.  
**`\D`** : Match any character that is not a decimal digit.  
**`\e`** : Match an _escape_ (`\u001B`).  
**`\E`** : Terminates a `\Q ... \E` quoted sequence.  
**`\f`** : Match a _form feed_ (`\u000C`).  
**`\G`** : Match if the current position is at the end of the previous match.  
**`\n`** : Match a _line feed_ (`\u000A`).  
**`\N{unicode character}`** : Match the named character.  
**`\p{unicode property}`** : Match any character with the specified unicode property.  
**`\P{unicode property}`** : Match any character not having the specified unicode property.  
**`\Q`** : Quotes all following characters until \E.  
**`\r`** : Match a _carriage return_ (`\u000D`).  
**`\s`** : Match a whitespace character. Whitespace is defined as `[\t\n\f\r\p{Z}]`.  
**`\S`** : Match a non-whitespace character.  
**`\t`** : Match a horizontal tabulation (`\u0009`).  
**`\uhhhh`** : Match the character with the hex value `hhhh`.  
**`\Uhhhhhhhh`** : Match the character with the hex value `hhhhhhhh`. Exactly eight hex digits must be provided, even though the largest Unicode code point is `\U0010ffff`.  
**`\w`** : Match a word character. Word characters are `[\p{Ll}\p{Lu}\p{Lt}\p{Lo}\p{Nd}]`.  
**`\W`** : Match a non-word character.  
**`\x{hhhh}`** : Match the character with hex value `hhhh`. From one to six hex digits may be supplied. 
**`\xhh`** : Match the character with two digit hex value `hh`.  
**`\X`** : Match a grapheme cluster.  
**`\Z`** : Match if the current position is at the end of input, but before the final line terminator, if one exists.  
**`\z`** : Match if the current position is at the end of input.  
**\\**_**n**_ : Back Reference. Match whatever the _n_-th capturing group matched. _n_ must be a number ≥ 1 and ≤ total number of capture groups in the pattern.  
**`\0ooo`** : Match an octal character. _ooo_ is from one to three octal digits. `0377` is the largest allowed octal character. The leading zero is required and distinguishes octal constants from back references.  
**`[pattern]`** : Match any one character from the pattern.  
**`.`** : Match any character.  
**`^`** : Match at the beginning of a line.  
**`$`** : Match at the end of a line.  
**\\** : Quotes the following character. Characters that must be quoted to be treated as literals are `* ? + [ ( ) { } ^ $ | \ . /`.

### Regular expression operators

**`|`** : Alternation. `A|B` matches either `A` or `B`.  
**`*`** : Match 0 or more times, as many times as possible.  
**`+`** : Match 1 or more times, as many times as possible.  
**`?`** : Match zero or one times, preferring one time if possible.  
**`{n}`** : Match exactly `n` times.  
**`{n,}`** : Match at least `n` times, as many times as possible.  
**`{n,m}`** : Match between `n` and `m` times, as many times as possible, but not more than `m` times.  
**`*?`** : Match zero or more times, as few times as possible.  
**`+?`** : Match one or more times, as few times as possible.  
**`??`** : Match zero or one times, preferring zero.  
**`{n}?`** : Match exactly `n` times.  
**`{n,}?`** : Match at least `n` times, but no more than required for an overall pattern match.  
**`{n,m}?`** : Match between `n` and `m` times, as few times as possible, but not less than `n`.  
**`*+`** : Match zero or more times, as many times as possible when first encountered, do not retry with fewer even if overall match fails (possessive match).
**`++`** : Match one or more times (possessive match).  
**`?+`** : Match zero or one times (possessive match).  
**`{n}+`** : Match exactly `n` times.  
**`{n,}+`** : Match at least `n` times (possessive match).  
**`{n,m}+`** : Match between `n` and `m` times (possessive match).  
**`(...)`** : Capturing parentheses; the range of input that matched the parenthesized subexpression is available after the match.  
**`(?:...)`** : Non-capturing parentheses; groups the included pattern, but does not provide capturing of matching text (more efficient than capturing parentheses).  
**`(?>...)`** : Atomic-match parentheses; first match of the parenthesized subexpression is the only one tried. If it does not lead to an overall pattern match, back up the search for a match to a position before the `"(?>"`.  
**`(?# ... )`** : Free-format comment (?# comment).  
**`(?= ... )`** : Look-ahead assertion. True, if the parenthesized pattern matches at the current input position, but does not advance the input position.  
**`(?! ... )`** : Negative look-ahead assertion. True, if the parenthesized pattern does not match at the current input position. Does not advance the input position.  
**`(?<= ... )`** : Look-behind assertion. True, if the parenthesized pattern matches text preceding the current input position, with the last character of the match being the input character just before the current position. Does not alter the input position. The length of possible strings matched by the look-behind pattern must not be unbounded (no `*` or `+` operators).  
**`(?<! ... )`** : Negative _look-behind assertion_. True, if the parenthesized pattern does not match text preceding the current input position, with the last character of the match being the input character just before the current position. Does not alter the input position. The length of possible strings matched by the look-behind pattern must not be unbounded (no `*` or `+` operators).  
**`(?ismwx-ismwx: ... )`** : Flag settings. Evaluate the parenthesized expression with the specified flags enabled or disabled.  
**`(?ismwx-ismwx)`** : Flag settings. Change the flag settings. Changes apply to the portion of the pattern following the setting. For example, `(?i)` changes to a case insensitive match.  

### Template Matching

**`$n`** : The text of capture group `n` will be substituted for `$n`. `n` must be ≥ 0 and not greater than the number of capture groups. A `$` not followed by a digit has no special meaning, and will appear in the substitution text as itself, i.e. `$`.  
**\\** : Treat the following character as a literal, suppressing any special meaning. Backslash escaping in substitution text is only required for `$` and `\`, but may be used on any other character.  

### Flag options

The following flags control various aspects of regular expression matching. These flags get specified within the pattern using the `(?ismx-ismx)` pattern options.

**`i`** : If set, matching will take place in a case-insensitive manner.  
**`x`** : If set, allow use of white space and #comments within patterns.  
**`s`** : If set, a "." in a pattern will match a line terminator in the input text. By default, it will not. Note that a carriage-return/line-feed pair in text behave as a single line terminator, and will match a single "." in a regular expression pattern.  
**`m`** : Control the behavior of `^` and `$` in a pattern. By default these will only match at the start and end, respectively, of the input text. If this flag is set, `^` and `$` will also match at the start and end of each line within the input text.  
**`w`** : Controls the behavior of `\b` in a pattern. If set, word boundaries are found according to the definitions of word found in _Unicode UAX 29, Text Boundaries_. By default, word boundaries are identified by means of a simple classification of characters as either _word_ or _non-word_, which approximates traditional regular expression behavior.

## API

**(regexp? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a regular expression object; otherwise `#f` is returned.

**regexp-type-tag** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  

Symbol representing the `regexp` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all regular expression objects.

**(regexp _str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(regexp _str opt ..._)**   

Returns a new regular expression object from the given regular expression pattern _str_ and matching options _opt_, ... . _str_ is a string, matching options _opt_ are symbols. The following matching options are supported:

- `case-insensitive`: Match letters in the regular expression independent of their case.
- `allow-comments`: Ignore whitespace and `#`-prefixed comments in the regular expression pattern.
- `ignore-meta`: Treat the entire regular expression pattern as a literal string.
- `dot-matches-line-separator`: Allow `.` to match any character, including line separators.
- `anchors-match-lines`: Allow `^` and `$` to match the start and end of lines.
- `unix-only-line-separators`: Treat only `\n` as a line separator; otherwise, all standard line separators are used.
- `unicode-words`: Use Unicode TR#29 to specify word boundaries; otherwise, all traditional regular expression word boundaries are used.

**(regexp-pattern _regexp_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the regular expression pattern for the given regular expression object _regexp_. A regular expression pattern is a string matching the regular expression syntax supported by library `(lispkit regexp)`.

**(regexp-capture-groups _regexp_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the number of capture groups of the given regular expression object _regexp_.

**(escape-regexp-pattern _str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a regular expression pattern string by adding backslash escapes to pattern _str_ as necessary to protect any characters that would match as pattern meta-characters.

```scheme
(escape-regexp-pattern "(home/objecthub)")
⟹ "\\(home\\/objecthub\\)"
```

**(escape-regexp-template _str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a regular expression pattern template string by adding backslash escapes to pattern template _str_ as necessary to protect any characters that would match as pattern meta-characters. 

**(regexp-matches _regexp str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(regexp-matches _regexp str start_)**  
**(regexp-matches _regexp str start end_)**  

Returns a _matching spec_ if the regular expression object _regexp_ successfully matches the entire string _str_ from position _start_ (inclusive) to _end_ (exclusive); otherwise, `#f` is returned. The default for _start_ is 0; the default for _end_ is the length of the string.

A _matching spec_ returned by `regexp-matches` consists of pairs of fixnum positions _(startpos . endpos)_ in _str_. The first pair is always representing the full match (i.e. _startpos_ is 0 and _endpos_ is the length of _str_), all other pairs represent the positions of the matching capture groups of _regexp_.

```scheme
(define email
  (regexp "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}"))
(regexp-matches email "matthias@objecthub.net")
⟹ ((0 . 22))
(define series
  (regexp "Season\\s+(\\d+)\\s+Episode\\s+(\\d+)"))
(regexp-matches series "Season 3  Episode 12")
⟹ ((0 . 20) (7 . 8) (18 . 20))
```

**(regexp-matches? _regexp str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(regexp-matches? _regexp str start_)**  
**(regexp-matches? _regexp str start end_)**  

Returns `#t` if the regular expression object _regexp_ successfully matches the entire string _str_ from position _start_ (inclusive) to _end_ (exclusive); otherwise, `#f` is returned. The default for _start_ is 0; the default for _end_ is the length of the string.

**(regexp-search _regexp str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(regexp-search _regexp str start_)**  
**(regexp-search _regexp str start end_)**  

Returns a _matching spec_ for the first match of the regular expression  _regexp_ with a part of string _str_ between position _start_ (inclusive) and _end_ (exclusive). If _regexp_ does not match any part of _str_ between _start_ and _end_, `#f` is returned. The default for _start_ is 0; the default for _end_ is the length of the string.

A _matching spec_ returned by `regexp-search` consists of pairs of fixnum positions _(startpos . endpos)_ in _str_. The first pair is always representing the full match of the pattern, all other pairs represent the positions of the matching capture groups of _regexp_.

```scheme
(define email
  (regexp "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}"))
(regexp-search email "Contact matthias@objecthub.net or foo@bar.org")
⟹ ((8 . 30))
(define series
  (regexp "Season\\s+(\\d+)\\s+Episode\\s+(\\d+)"))
(regexp-search series "New Season 3 Episode 12: Pilot")
⟹ ((4 . 23) (11 . 12) (21 . 23))
```

**(regexp-search-all _regexp str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(regexp-search-all _regexp str start_)**  
**(regexp-search-all _regexp str start end_)**  

Returns a list of all _matching specs_ for matches of the regular expression _regexp_ with parts of string _str_ between position _start_ (inclusive) and _end_ (exclusive). If _regexp_ does not match any part of _str_ between _start_ and _end_, the empty list is returned. The default for _start_ is 0; the default for _end_ is the length of the string.

A _matching spec_ returned by `regexp-search` consists of pairs of fixnum positions _(startpos . endpos)_ in _str_. The first pair is always representing the full match of the pattern, all other pairs represent the positions of the matching capture groups of _regexp_.

```scheme
(define email
  (regexp "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}"))
(regexp-search-all email "Contact matthias@objecthub.net or foo@bar.org")
⟹ (((8 . 30)) ((34 . 45)))
(define series
  (regexp "Season\\s+(\\d+)\\s+Episode\\s+(\\d+)"))
(regexp-search-all series "New Season 3 Episode 12: Pilot")
⟹ (((4 . 23) (11 . 12) (21 . 23)))
```

**(regexp-extract _regexp str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(regexp-extract _regexp str start_)**  
**(regexp-extract _regexp str start end_)**  

Returns a list of substrings from _str_ which all represent full matches of the regular expression _regexp_ with parts of string _str_ between position _start_ (inclusive) and _end_ (exclusive). If _regexp_ does not match any part of _str_ between _start_ and _end_, the empty list is returned. The default for _start_ is 0; the default for _end_ is the length of the string.

```scheme
(define email
  (regexp "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}"))
(regexp-extract email "Contact matthias@objecthub.net or foo@bar.org" 10)
⟹ ("tthias@objecthub.net" "foo@bar.org")
(define series
  (regexp "Season\\s+(\\d+)\\s+Episode\\s+(\\d+)"))
(regexp-extract series "New Season 3 Episode 12: Pilot")
⟹ ("Season 3 Episode 12")
```

**(regexp-split _regexp str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(regexp-split _regexp str start_)**  
**(regexp-split _regexp str start end_)**  

Splits string _str_ into a list of possibly empty substrings separated by non-empty matches of regular expression _regexp_ within position _start_ (inclusive) and _end_ (exclusive). If _regexp_ does not match any part of _str_ between _start_ and _end_, a list with _str_ as its only element is returned. The default for _start_ is 0; the default for _end_ is the length of the string.

```scheme
(define email
  (regexp "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}"))
(regexp-split email "Contact matthias@objecthub.net or foo@bar.org" 10)
⟹ ("Contact ma" " or " "")
(define series
  (regexp "Season\\s+(\\d+)\\s+Episode\\s+(\\d+)"))
(regexp-split series "New Season 3 Episode 12: Pilot")
⟹ ("New " ": Pilot")
```

**(regexp-partition _regexp str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(regexp-partition _regexp str start_)**  
**(regexp-partition _regexp str start end_)**  

Partitions string _str_ into a list of non-empty strings matching regular expression _regexp_ within position _start_ (inclusive) and _end_ (exclusive), interspersed with the unmatched portions of the whole string. The first and every odd element is an unmatched substring, which will be the empty string if _regexp_ matches at the beginning of the string or end of the previous match. The second and every even element will be a substring fully matching _regexp_. If _str_ is the empty string or if there is no match at all, the result is a list with _str_ as its only element.

```scheme
(define email
  (regexp "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}"))
(regexp-partition email "Contact matthias@objecthub.net or foo@bar.org" 10)
⟹ ("Contact ma" "tthias@objecthub.net" " or " "foo@bar.org" "")
(define series
  (regexp "Season\\s+(\\d+)\\s+Episode\\s+(\\d+)"))
(regexp-partition series "New Season 3 Episode 12: Pilot")
⟹ ("New " "Season 3 Episode 12" ": Pilot")
```

**(regexp-replace _regexp str subst_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(regexp-replace _regexp str subst start_)**  
**(regexp-replace _regexp str subst start end_)**  

Returns a new string replacing all matches of regular expression _regexp_ in string _str_ within position _start_ (inclusive) and _end_ (exclusive) with string _subst_. `regexp-replace` will always return a new string, even if there are no matches and replacements.

The optional parameters _start_ and _end_ restrict both the matching and the substitution, to the given positions, such that the result is equivalent to omitting these parameters and replacing on `(substring` _str start end_`)`.

```scheme
(define email
  (regexp "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}"))
(regexp-replace email "Contact matthias@objecthub.net or foo@bar.org" "<omitted>" 10)
⟹ "Contact ma<omitted> or <omitted>"
(define series
  (regexp "Season\\s+(\\d+)\\s+Episode\\s+(\\d+)"))
(regexp-replace series "New Season 3 Episode 12: Pilot" "Series")
⟹ "New Series: Pilot"
```

**(regexp-replace! _regexp str subst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(regexp-replace! _regexp str subst start_)**  
**(regexp-replace! _regexp str subst start end_)**  

Mutates string _str_ by replacing all matches of regular expression _regexp_ within position _start_ (inclusive) and _end_ (exclusive) with string _subst_. The optional parameters _start_ and _end_ restrict both the matching and the substitution. `regexp-replace!` returns the number of replacements that were applied.

The optional parameters _start_ and _end_ restrict both the matching and the substitution, to the given positions, such that the result is equivalent to omitting these parameters and replacing on `(substring` _str start end_`)`.

```scheme
(define email
  (regexp "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}"))
(define str "Contact matthias@objecthub.net or foo@bar.org")
(regexp-replace! email str "<omitted>" 10) ⇒ 2
str ⇒ "Contact ma<omitted> or <omitted>"
```

**(regexp-fold _regexp kons knil str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(regexp-fold _regexp kons knil str finish_)**  
**(regexp-fold _regexp kons knil str finish start_)**  
**(regexp-fold _regexp kons knil str finish start end_)**  

`regexp-fold` is the most fundamental and generic regular expression matching iterator. It repeatedly searches string _str_ for the regular expression _regexp_ so long as a match can be found. On each successful match, it applies `(kons` _i regexp-match str acc_`)` where _i_ is the index since the last match (beginning with _start_), _regexp-match_ is the resulting _matching spec_, and _acc_ is the result of the previous _kons_ application, beginning with _knil_. When no more matches can be found, `regexp-fold` calls _finish_ with the same arguments, except that _regexp-match_ is `#f`. By default, _finish_ just returns _acc_.

```scheme
(regexp-fold (regexp "(\\w+)")
             (lambda (i m str acc)
               (let ((s (substring str (caar m) (cdar m))))
                 (if (zero? i) s (string-append acc "-" s))))
             ""
             "to  be  or  not  to  be")
⟹ "to-be-or-not-to-be"
```
