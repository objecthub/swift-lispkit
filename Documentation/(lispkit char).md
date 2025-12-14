# LispKit Char

Characters are objects that represent printed characters such as letters and digits. In LispKit, characters are UTF-16 code units. 

Character literals are written using the notation `#\`_character_, or `#\`_character-name_, or `#\x`_hex-scalar-value_. Characters written using this `#\` notation are self-evaluating, i.e. they do not have to be quoted.

The following standard character names are supported by LispKit:

   - `#\alarm`: U+0007
   - `#\backspace`: U+0008
   - `#\delete`: U+007F
   - `#\escape`: U+001B
   - `#\newline`: the linefeed character U+000A
   - `#\null`: the null character U+0000
   - `#\return`: the return character U+000D
   - `#\space`: the space character U+0020
   - `#\tab`: the tab character U+0009

Here are some examples using the `#\` notation:

   - `#\m`: lowercase letter 'm'
   - `#\M`: uppercase letter 'M'
   - `#\(`: left parenthesis ')'
   - `#\\`: backslash '\\'
   - `#\ `: space character ' '
   - `#\x03BB`: the lambda character 'λ' (equivalent to `#\λ`)

Case is significant in `#\`_character_, and in `#\`_character-name_, but not in `#\x`_hex-scalar-value_. If _character_ in `#\`_character_ is alphabetic, then any character immediately following _character_ cannot be one that can appear in an identifier. This rule resolves the ambiguous case where, for example, the sequence of characters `#\space` could be taken to be either a representation of the space character or a representation of the character `#\s` followed by a representation of the symbol `pace`.

Some of the procedures that operate on characters ignore the difference between upper case and lower case. The procedures that ignore case have "-ci" (for "case insensitive") embedded in their names.


## Predicates

**(char? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a character, otherwise returns `#f`.

**(char=? _char ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(char\<? _char ..._)**  
**(char\>? _char ..._)**  
**(char\<=? _char ..._)**  
**(char\>=? _char ..._)**  

These procedures return `#t` if the results of passing their arguments to `char->integer` are respectively equal, monotonically increasing, monotonically decreasing, monotonically non-decreasing, or monotonically non-increasing. These predicates are transitive.

**(char-ci=? _char ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(char-ci\<? _char ..._)**  
**(char-ci\>? _char ..._)**  
**(char-ci\<=? _char ..._)**  
**(char-ci\>=? _char ..._)**  

These procedures are similar to `char=?` etc., but they treat upper case and lower case letters as the same. For example, `(char-ci=? #\A #\a)` returns `#t`. Specifically, these procedures behave as if `char-foldcase` were applied to their arguments before they were compared.

**(char-alphabetic? _char_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Procedure `char-alphabetic?` returns `#t` if its argument is a alphabetic character, otherwise it returns `#f`. Note that many Unicode characters are alphabetic but neither upper nor lower case.

**(char-numeric? _char_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Procedure `char-numeric?` returns `#t` if its argument is a numeric character, otherwise it returns `#f`.

**(char-whitespace? _char_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Procedure `char-whitespace?` returns `#t` if its argument is a whitespace character, otherwise it returns `#f`.

**(char-upper-case? _char_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Procedure `char-upper-case??` returns `#t` if its argument is an upper-case character, otherwise it returns `#f`.

**(char-lower-case? _char_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Procedure `char-lower-case?` returns `#t` if its argument is a lower-case character, otherwise it returns `#f`.


## Transforming characters

**(char-upcase _char_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `char-upcase` procedure, given an argument that is the lowercase part of a Unicode casing pair, returns the uppercase member of the pair, provided that both characters are supported by LispKit. Note that language-sensitive casing pairs are not used. If the argument is not the lowercase member of such a pair, it is returned.

**(char-downcase _char_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `char-downcase` procedure, given an argument that is the uppercase part of a Unicode casing pair, returns the lowercase member of the pair, provided that both characters are supported by LispKit. If the argument is not the uppercase member of such a pair, it is returned.

**(char-foldcase _char_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `char-foldcase` procedure applies the Unicode simple case-folding algorithm to its argument and returns the result. Note that language-sensitive folding is not used. If the argument is an uppercase letter, the result will be either a lowercase letter or the same as the argument if the lowercase letter does not exist or is not supported by LispKit.


## Converting characters

**(digit-value _char_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Procedure `digit-value` returns the numeric value (0 to 9) of its argument if it is a numeric digit (that is, if `char-numeric?` returns `#t`), or `#f` on any other character.

```scheme
(digit-value #\3)      ⇒  3
(digit-value #\x0EA6)  ⇒  #f
```

**(char-\>integer _char_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(integer-\>char _n_)**  

Given a Unicode character, `char->integer` returns an exact integer between 0 and #xD7FF or between #xE000 and #x10FFFF which is equal to the Unicode scalar value of that character. Given a non-Unicode character, it returns an exact integer greater than #x10FFFF.

Given an exact integer that is the value returned by a character when `char->integer` is applied to it, `integer->char` returns that character.

**(char-name _char_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(char-name _char encoded?_)**  

If character _char_ has a corresponding named XML entity, then procedure `char-name` returns the name of this entity. Otherwise, `char-name` returns `#f`. If parameter _encoded_ is set to `#t`, then the name is returned including the full entity encoding.

```scheme
(char-name #\<)     ⇒  "LT"
(char-name #\&)     ⇒  "AMP"
(char-name #\")     ⇒  "quot"
(char-name #\a)     ⇒  #f
(char-name #\> #t)  ⇒  "&gt;"
```
