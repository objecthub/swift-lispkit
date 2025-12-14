# LispKit Format

Library `(lispkit format)` provides an implementation of [Common Lisp's `format` procedure](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node200.html#SECTION002633000000000000000) for LispKit. Procedure `format` can be used for creating formatted text using a format string similar to `printf`. The formatting formalism, though, is significantly more expressive, allowing users to display numbers in various formats (e.g. hex, binary,
octal, roman numerals, natural language), applying conditional formatting, outputting text in a tabular format, iterating over data structures, and even applying `format` recursively to handle data that includes its own preferred formatting strings.

## Usage overview

In its most simple form, procedure `format` gets invoked with a _control string_ followed by an arbitrary number of _arguments_. The _control string_ consists of characters that are copied verbatim into the output as well as _formatting directives_. All formatting directives start with a tilde (`~`) and end with a single character identifying the type of the directive. Directives may also take prefix _parameters_ written immediately after the tilde character, separated by comma as well as _modifiers_ (see below for details).

For example, the call of `format` below injects two integer arguments into the control string via directive `~D` and returns the resulting string:

```scheme
(format "There are ~D warnings and ~D errors." 12 7)
⇒ "There are 12 warnings and 7 errors."
```

### Simple Directives

Here is a simple control string which injects a readable description of an argument via the directive `~A`: `"I received ~A as a response"`. Directive `~A` refers to a the _next argument_ provided to `format` when compiling the formatted output:

```scheme
(format "I received ~A as a response" "nothing")
⇒ "I received nothing as a response"
(format "I received ~A as a response" "a long email")
⇒ "I received a long email as a response"
```

Directive `~A` may be given parameters to influence the formatted output. The first parameter of `~A`-directives defines the minimal length. If the length of the textual representation of the next argument is smaller than the minimal length, padding characters are inserted:

```scheme
(format "|Name: ~10A|Location: ~13A|" "Smith" "New York")
⇒ "|Name: Smith     |Location: New York     |"
(format "|Name: ~10A|Location: ~13A|" "Williams" "San Francisco")
⇒ "|Name: Williams  |Location: San Francisco|"
(format "|Name: ~10,,,'_@A|Location: ~13,,,'-A|" "Garcia" "Los Angeles")
⇒ "|Name: ____Garcia|Location: Los Angeles--|"
```

The third example above utilizes more than one parameter and, in one case, includes a `@` modifier. The directive `~13,,,'-A` defines the first and the fourth parameter. The second and third parameter are omitted and thus defaults are used. The fourth parameter defines the padding character. If character literals are used in the parameter list, they are prefixed with a quote `'`. The directive `~10,,,'_@A` includes an `@` modifier which will result in padding of the output on the left.

It is possible to inject a parameter from the list of arguments. The following examples show how parameter `v` is used to do this for formatting a floating-point number with a configurable number of fractional digits.

```scheme
(format "length = ~,vF" 2 pi)
⇒ "length = 3.14"
(format "length = ~,vF" 4 pi)
⇒ "length = 3.1416"
```

Here `v` is used as the second parameter of the fixed floating-point directive `~F`, indicating the number of fractional digits. It refers to the next provided argument (which is
either 2 or 4 in the examples above).

### Composite Directives

The next example shows how one can refer to the total number of arguments that are not yet consumed in the formatting process by using `#` as a parameter value.

```scheme
(format "~A left for formatting: ~#[none~;one~;two~:;many~]."
        "Arguments" "eins" 2)
⇒ "Arguments left for formatting: two."
(format "~A left for formatting: ~#[none~;one~;two~:;many~]."
        "Arguments")
⇒ "Arguments left for formatting: none."
(format "~A left for formatting: ~#[none~;one~;two~:;many~]."
        "Arguments", "eins", 2, "drei", "vier")
⇒ "Arguments left for formatting: many."
```

In these examples, the _conditional directive_ `~[` is used. It is followed by _clauses_ separared by directive `~;` until `~]` is reached. Thus, there are four clauses in the example above: `none`, `one`, `two`, and `many`. The parameter in front of the `~[` directive determines which of the clauses is being output. All other clauses will be discarded. For instance, `~1[zero~;one~;two~:;many~]` will output `one` as clause 1 is chosen (which is the second one, given that numbering starts with zero). The last clause is special because it is prefixed with the `~;` directive using a `:` modifier: this is a _default clause_ which is chosen when none of the others are applicable. Thus, `~8[zero~;one~;two~:;many~]` outputs `many`. This also explains how the example above works: here `#` refers to the number of arguments that are still available and this number drives what is being returned in this directive: `~#[...~]`.

Another powerful composite directive is the _iteration directive_ `~{`. With this directive it is possible to iterate over all elements of a sequence. The control string between `~{` and `~}` gets repeated as long as there are still elements left in the sequence which is provided as an argument. For instance, `Numbers:~{ ~A~}` applied to argument `("one" "two" "three")` results in the output `Numbers: one two three`. The control string between `~{` and `~}` can also consume more than one element of the sequence. Thus, `Numbers:~{ ~A=>~A~}` applied to argument `("one" 1 "two" 2)` outputs `Numbers: one=>1 two=>2`.

Of course, it is also possible to nest arbitrary composite directives. Here is an example for a control string that uses a combination of iteration and conditional directives to output the elements of a sequence separated by a comma: `(~{~#[~;~A~:;~A, ~]~})`. When this control string is used with the argument `("one" "two" "three")`, the following formatted output is generated: `(one, two, three)`.

## Formatting language

_Control strings_ consist of characters that are copied verbatim into the output as well as _formatting directives_. All formatting directives start with a tilde (`~`) and end with a single character identifying the type of the directive. Directives may take prefix _parameters_ written immediately after the tilde character, separated by comma. Both integers and characters are allowed as parameters. They may be followed by formatting _modifiers_ `:`, `@`, and `+`. This is the general format of a formatting directive:

```ebnf
~param1,param2,...mX
```

where

  - `m` is a potentially empty modifier, consisting of an arbitrary sequence of modifier characters `:`, `@`, and `+`
  - `X` is a character identifying a directive type
  - `paramN` is either a nummeric or character parameter according to the specification below.

The following grammar describes the syntax of directives formally in BNF:

```ebnf
<directive>  ::= "~" <modifiers> <char>
               | "~" <parameters> <modifiers> <char>
<modifiers>  ::= <empty>
               | ":" <modifiers>
               | "@" <modifiers>
               | "+" <modifiers>
<parameters> ::= <parameter>
               | <parameter> "," <parameters>
<parameter>  ::= <empty>
               | "#"
               | "v"
               | <number>
               | "-" <number>
               | <character>
<number>     ::= <digit>
               | <digit> <number>
<digit>      ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<character>  ::= "'" <char>
```

## Formatting directives

The formatting directives supported by library `(lispkit format)` are based on the directives specified in
[Common Lisp the Language, 2nd Edition](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node1.html)
by Guy L. Steele Jr. Some directives have been extended to meet today's formatting requirements
(e.g. to support localization) and to enable a powerful usage throughout LispKit. Extensions were introduced in a way to not impact backward compatibility.

<table>
  <thead>
    <tr>
      <th>Directive</th>
      <th>Explanation</th>
    </tr>
  </thead>
  <tbody>
    <tr valign="top">
      <td><strong>~a</strong><br>
      <strong>~A</strong></td>
      <td>
        ASCII:&nbsp;&nbsp;<strong>~<em>mincol,colinc,minpad,padchar,maxcol,elchar</em>A</strong>
        <p>The next argument <em>arg</em> is output as if procedure <code>display</code> was used, i.e. the output is without escape characters and if <em>arg</em> is a string, its characters will be output verbatim without surrounding quotes.</p>
        <p><em>mincol</em> (default: 0) specifies the minimal "width" of the output of the directive in characters, <em>maxcol</em> (default: &infin;) specifies the maximum width. <em>padchar</em> (default: '&nbsp;') defines the character that is used to pad the output to make sure it is at least <em>mincol</em> characters long. By default, the output is padded on the right with at least <em>minpad</em> (default: 0) copies of <em>padchar</em>. Padding characters are then inserted <em>colinc</em> (default: 1) characters at a time until the total width is at least <em>mincol</em>. Padding is capped such that the output never exceeds <em>maxcol</em> characters. If, without padding, the output is already longer than <em>maxcol</em>, the output is truncated at width <em>maxcol - 1</em> and the ellipsis character <em>elchar</em> (default: '&hellip;') is inserted at the end.</p>
        <p>Modifier <code>@</code> enables padding on the left to right-align the output.</p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~w</strong><br>
      <strong>~W</strong></td>
      <td>
        WRITE:&nbsp;&nbsp;<strong>~<em>mincol,colinc,minpad,padchar,maxcol,elchar</em>W</strong>
        <p>The next argument <em>arg</em> is output as if procedure <code>write</code> was used, i.e. the output is with escape characters and if <em>arg</em> is a string, its characters will be output surrounded by quotes.</p>
        <p>Parameters <em>mincol</em> (default: 0), <em>colinc</em> (default: 1), <em>minpad</em> (default: 0), <em>padchar</em> (default: '&nbsp;'), <em>maxcol</em> (default: &infin;), and <em>elchar</em> (default: '&hellip;') are used just as described for the <em>ASCII directive</em> <code>~A</code>. Modifier <code>@</code> enables padding on the left to right-align the output.</p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~s</strong><br>
      <strong>~S</strong></td>
      <td>
        SOURCE:&nbsp;&nbsp;<strong>~<em>mincol,colinc,minpad,padchar,maxcol,elchar</em>S</strong>
        <p>The next argument <em>arg</em> is output using a type-specific control string. If no control string is registered for the type of <em>arg</em>, then <code>~S</code> behaves like <code>~W</code> for <em>arg</em>.</p>
        <p>Parameters <em>mincol</em> (default: 0), <em>colinc</em> (default: 1), <em>minpad</em> (default: 0), <em>padchar</em> (default: '&nbsp;'), <em>maxcol</em> (default: &infin;), and <em>elchar</em> (default: '&hellip;') are used just as described for the <em>ASCII directive</em> <code>~A</code>. Modifier <code>@</code> enables padding on the left to right-align the output.</p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~c</strong><br>
      <strong>~C</strong></td>
      <td>
        CHARACTER:&nbsp;&nbsp;<strong>~C</strong>
        <p>The next argument <em>arg</em> should be a character or a string consisting of one character. Directive <code>~C</code> outputs <em>arg</em> in a form dependent on the modifiers used. Without any modifiers, <em>arg</em> is output as if the character was used in a string without any escaping.</p>
        <p>If the <code>@</code> modifier is provided alone, the character is output using Scheme's syntax for character literals. The modifier combination <code>@:</code> will lead to <em>arg</em> being output as Unicode code points. The combination <code>@:+</code> will output <em>arg</em> as a sequence of Unicode scalar property names, separated by comma.</p>
        <p>If the <code>:</code> modifier is used (without <code>@</code>), a representation of <em>arg</em> for the usage in XML documents is chosen. By default, a Unicode-based XML character encoding is used, unless <code>:</code> is combined with <code>+</code>, in which case the character is represented as a XML named character entity when possible, otherwise, the character is output in raw form.</p>
        <p>If the <code>+</code> modifiers is used alone, the character is output as if it is a character of a string, escaped if necessary, and surrounded by quotes.</p>
        <p>&nbsp;&nbsp;<code>(format "~C" #\A)</code> &#10233; <code>A</code><br>
        &nbsp;&nbsp;<code>(format "~+C" #\A)</code> &#10233; <code>"A"</code><br>
        &nbsp;&nbsp;<code>(format "~+C" #\newline)</code> &#10233; <code>"\n"</code><br>
        &nbsp;&nbsp;<code>(format "~@C" "A")</code> &#10233; <code>#\A</code><br>
        &nbsp;&nbsp;<code>(format "~@C" "\t")</code> &#10233; <code>#\tab</code><br>
        &nbsp;&nbsp;<code>(format "~@:C" "©")</code> &#10233; <code>U+00A9</code><br>
        &nbsp;&nbsp;<code>(format "~@:+C" "©")</code> &#10233; <code>COPYRIGHT SIGN</code><br>
        &nbsp;&nbsp;<code>(format "~:C" "©")</code> &#10233; <code>&amp;#xA9;</code><br>
        &nbsp;&nbsp;<code>(format "~:+C" "©")</code> &#10233; <code>&amp;copy;</code></p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~d</strong><br>
      <strong>~D</strong></td>
      <td>
        DECIMAL:&nbsp;&nbsp;<strong>~<em>mincol,padchar,groupchar,groupcol</em>D</strong>
        <p>The next argument <em>arg</em> is output in decimal radix. <em>arg</em> should be an integer, in which case no decimal point is printed. For floating-point numbers which do not represent an integer, a decimal point and a fractional part are output.</p>
        <p><em>mincol</em> (default: 0) specifies the minimal "width" of the output of the directive in characters with <em>padchar</em> (default: '&nbsp;') defining the character that is used to pad the output on the left to make sure it is at least <em>mincol</em> characters long.</p>
        <p>&nbsp;&nbsp;<code>(format "Number: ~D" 8273)</code> &#10233; <code>Number:&nbsp;8273</code><br>
        &nbsp;&nbsp;<code>(format "Number: ~6D" 8273)</code> &#10233; <code>Number:&nbsp;&nbsp;&nbsp;8273</code><br>
        &nbsp;&nbsp;<code>(format "Number: ~6,'0D" 8273)</code> &#10233; <code>Number:&nbsp;008273</code></p>
        <p>By default, the number is output without grouping separators. <em>groupchar</em> specifies which character should be used to separate sequences of <em>groupcol</em> digits in the output. Grouping of digits gets enabled with the <code>:</code> modifier.</p>
        <p>&nbsp;&nbsp;<code>(format "|~10:D|" 1734865)</code> &#10233; <code>|&nbsp;1,734,865|</code><br>
        &nbsp;&nbsp;<code>(format "|~10,,'.:D|" 1734865)</code> &#10233; <code>|&nbsp;1.734.865|</code></p>
        <p>A sign is output only if the number is negative. With the modifier <code>@</code> it is possible to force output also of positive signs. To facilitate the localization of output, procedure <code>format</code> supports a locale parameter, which is also available via format config objects. Locale-specific output can be enabled for the <code>~D</code> directive by using the <code>+</code> modifier.</p>
        <p>&nbsp;&nbsp;<code>(format 'de_CH "~+D" 14321)</code> &#10233; <code>14'321</code></p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~b</strong><br>
      <strong>~B</strong></td>
      <td>
        BINARY:&nbsp;&nbsp;<strong>~<em>mincol,padchar,groupchar,groupcol</em>B</strong>
        <p>Binary directive <code>~B</code> is just like decimal directive <code>~D</code> but it outputs the next argument in binary radix (radix 2) instead of decimal. It uses the space character as the default for <em>groupchar</em> and has a default grouping size of 4 as the default for <em>groupcol</em>.</p>
        <p>&nbsp;&nbsp;<code>(format "bin(~D) = ~B" 178 178)</code> &#10233; <code>bin(178) = 10110010</code><br>
        &nbsp;&nbsp;<code>(format "~:B" 59701)</code> &#10233; <code>1110 1001 0011 0101</code><br>
        &nbsp;&nbsp;<code>(format "~19,'0,'.:B" 31912)</code> &#10233; <code>0111.1100.1010.1000</code></p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~o</strong><br>
      <strong>~O</strong></td>
      <td>
        OCTAL:&nbsp;&nbsp;<strong>~<em>mincol,padchar,groupchar,groupcol</em>O</strong>
        <p>Octal directive <code>~O</code> is just like decimal directive <code>~D</code> but it outputs the next argument in octal radix (radix 8) instead of decimal. It uses the space character as the default for <em>groupchar</em> and has a default grouping size of 4 as the default for <em>groupcol</em>.</p>
        <p>&nbsp;&nbsp;<code>(format "bin(~D) = ~O" 178 178)</code> &#10233; <code>bin(178) = 262</code><br>
        &nbsp;&nbsp;<code>(format "~:O" 59701)</code> &#10233; <code>16 4465</code><br>
        &nbsp;&nbsp;<code>(format "~9,'0,',:O" 31912)</code> &#10233; <code>0007,6250</code></p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~x</strong><br>
      <strong>~X</strong></td>
      <td>
        HEXADECIMAL:&nbsp;&nbsp;<strong>~<em>mincol,padchar,groupchar,groupcol</em>X</strong>
        <p>Hexadecimal directive <code>~X</code> is just like decimal directive <code>~D</code> but it outputs the next argument in hexadecimal radix (radix 16) instead of decimal. It uses the colon character as the default for <em>groupchar</em> and has a default grouping size of 2 as the default for <em>groupcol</em>. With modifier <code>+</code>, upper case characters are used for representing hexadecimal digits.</p>
        <p>&nbsp;&nbsp;<code>(format "bin(~D) = ~X" 9968 9968)</code> &#10233; <code>bin(9968) = 26f0</code><br>
        &nbsp;&nbsp;<code>(format "~:X" 999701)</code> &#10233; <code>f:41:15</code><br>
        &nbsp;&nbsp;<code>(format "~+X" 999854)</code> &#10233; <code>F41AE</code></p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~r</strong><br>
      <strong>~R</strong></td>
      <td>
        RADIX:&nbsp;&nbsp;<strong>~<em>radix,mincol,padchar,groupchar,groupcol</em>R</strong>
        <p>The next argument <em>arg</em> is expected to be a fixnum number. It will be output with radix <em>radix</em> (default: 10). <em>mincol</em> (default: 0) specifies the minimal "width" of the output of the directive in characters with <em>padchar</em> (default: '&nbsp;') defining the character that is used to pad the output on the left to make it at least <em>mincol</em> characters long.</p>
        <p>&nbsp;&nbsp;<code>(format "Number: ~10R" 1272)</code> &#10233; <code>Number:&nbsp;1272</code><br>
        &nbsp;&nbsp;<code>(format "Number: ~16,8,'0R" 7121972)</code> &#10233; <code>Number:&nbsp;006cac34</code><br>
        &nbsp;&nbsp;<code>(format "Number: ~2R" 173)</code> &#10233; <code>Number:&nbsp;10101101</code></p>
        <p>By default, the number is output without grouping separators. <em>groupchar</em> specifies which character should be used to separate sequences of <em>groupcol</em> digits in the output. Grouping of digits is enabled with the <code>:</code> modifier.</p>
        <p>&nbsp;&nbsp;<code>(format "~16,8,,':,2:R" 7121972)</code> &#10233; <code>6c:ac:34</code><br>
        &nbsp;&nbsp;<code>(format "~2,14,'0,'.,4:R" 773)</code> &#10233; <code>0011.0000.0101</code></p>
        <p>A sign is output only if the number is negative. With the modifier <code>@</code> it is possible to force output also of positive signs.</p>
        <p>If parameter <em>radix</em> is not specified at all, then an entirely different interpretation is given. <code>~R</code> outputs <em>arg</em> as a cardinal number in natural language. The form <code>~:R</code> outputs <em>arg</em> as an ordinal number in natural language. <code>~@R</code> outputs <em>arg</em> as a Roman numeral.</p>
        <p>&nbsp;&nbsp;<code>(format "~R" 572)</code> &#10233; <code>five hundred seventy-two</code><br>
        &nbsp;&nbsp;<code>(format "~:R" 3)</code> &#10233; <code>3rd</code><br>
        &nbsp;&nbsp;<code>(format "~@R" 1272)</code> &#10233; <code>MCCLXXII</code></p>
        <p>Whenever output is provided in natural language, English is used as the language by default. By specifying the <code>+</code> modifier, it is possible to switch the language to the language of the locale provided to procedure <code>format</code>. In fact, modifier <code>+</code> plays two different roles: If the given radix is greater than 10, upper case characters are used for representing alphabetic digits. If the radix is omitted, usage of modifier <code>+</code> enables locale-specific output determined by the <code>locale:</code> parameter of procedure <code>format</code>.</p>
        <p>&nbsp;&nbsp;<code>(format 'de_DE "~+R" 572)</code> &#10233; <code>fünf­hundert­zwei­und­siebzig</code><br>
        &nbsp;&nbsp;<code>(format 'de_CH "~10+R" 14321)</code> &#10233; <code>14'321</code><br>
        &nbsp;&nbsp;<code>(format "~16R vs ~16+R" 900939 900939)</code> &#10233; <code>dbf4b vs DBF4B</code></p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~f</strong><br>
      <strong>~F</strong></td>
      <td>
        FIXED FLOAT:&nbsp;&nbsp;<strong>~<em>w,d,k,overchar,padchar,groupchar,groupcol</em>F</strong>
        <p>The next argument <em>arg</em> is output as a floating-point number in a fixed format (ideally without exponent) of exactly <em>w</em> characters, if <em>w</em> is specified. First, leading <em>padchar</em> characters (default: '&nbsp;') are output, if necessary, to pad the field on the left. If <em>arg</em> is negative, then a minus sign is printed. If <em>arg</em> is not negative, then a plus sign is printed if and only if the <code>@</code> modifier was specified. Then a sequence of digits, containing a single embedded decimal point, is printed. If parameter <em>d</em> is provided, then exactly <em>d</em> decimal places are output. This represents the magnitude of the value of <em>arg</em> times 10<sup><em>k</em></sup>, rounded to <em>d</em> fractional digits. There are no leading zeros, except that a single zero digit is output before the decimal point if the printed value is less than 1.0, and this single zero digit is not output after all if <em>w = d</em> + 1.</p>
        <p>If it is impossible to print the value in the required format in a field of width <em>w</em>, then one of two actions is taken: If the parameter <em>overchar</em> is specified, then <em>w</em> copies of this character are printed. If <em>overchar</em> is omitted, then the scaled value of <em>arg</em> is printed using more than <em>w</em> characters.</p>
        <p>If the width parameter <em>w</em> is omitted, then the output is of variable width and a value is chosen for <em>w</em> in such a way that no leading padding characters are needed and exactly <em>d</em> characters will follow the decimal point. For example, the directive <code>~,2F</code> will output exactly two digits after the decimal point and as many as necessary before the decimal point.</p>
        <p>If <em>d</em> is omitted, then there is no constraint on the number of digits to appear after the decimal point. A value is chosen for <em>d</em> in such a way that as many digits as possible may be printed subject to the width constraint imposed by <em>w</em> and the constraint that no trailing zero digits may appear in the fraction, except that if the fraction is zero, then a single zero digit should appear after the decimal point if permitted by the width constraint.</p>
        <p>If <em>w</em> is omitted, then if the magnitude of <em>arg</em> is so large (or, if <em>d</em> is also omitted, so small) that more than 100 digits would have to be printed, then <em>arg</em> is output using exponential notation instead.</p>
        <p>The <code>~F</code> directive also supports grouping of the integer part of <em>arg</em>; this can be enabled via the <code>:</code> modifier. <em>groupchar</em> (default: ',') specifies which character should be used to separate sequences of <em>groupcol</em> (default: 3) digits in the integer part of the output. If locale-specific settings should be used, the <code>+</code> modifier needs to be set.</p>
        <p>&nbsp;&nbsp;<code>(format "~F" 123.1415926)</code> &#10233; <code>123.1415926</code><br>
        &nbsp;&nbsp;<code>(format "~8F" 123.1415926)</code> &#10233; <code>123.1416</code><br>
        &nbsp;&nbsp;<code>(format "~8,,,'-F" 123.1415926)</code> &#10233; <code>123.1416</code><br>
        &nbsp;&nbsp;<code>(format "~8,,,'-F" 123456789.12)</code> &#10233; <code>--------</code><br>
        &nbsp;&nbsp;<code>(format "~8,,,,'0F" 123.14)</code> &#10233; <code>00123.14</code><br>
        &nbsp;&nbsp;<code>(format "~8,3,,,'0F" 123.1415926)</code> &#10233; <code>0123.142</code><br>
        &nbsp;&nbsp;<code>(format "~,4F" 123.1415926)</code> &#10233; <code>123.1416</code><br>
        &nbsp;&nbsp;<code>(format "~,2@F" 123.1415926)</code> &#10233; <code>+123.14</code><br>
        &nbsp;&nbsp;<code>(format "~,2,-2@F" 314.15926)</code> &#10233; <code>+3.14</code><br>
        &nbsp;&nbsp;<code>(format "~,2:F" 1234567.891)</code> &#10233; <code>1,234,567.89</code><br>
        &nbsp;&nbsp;<code>(format "~,2,,,,'',3:F" 1234567.891)</code> &#10233; <code>1'234'567.89</code></p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~e</strong><br>
      <strong>~E</strong></td>
      <td>
        EXPONENTIAL FLOAT:&nbsp;&nbsp;<strong>~<em>w,d,e,k,overchar,padchar,expchar</em>E</strong>
        <p>The next argument <em>arg</em> is output as a floating-point number in an exponential format of exactly <em>w</em> characters, if <em>w</em> is specified. Parameter <em>d</em> is the number of digits to print after the decimal point, <em>e</em> is the number of digits to use when printing the exponent, and <em>k</em> is a scale factor that defaults to 1.</p>
        <p>First, leading <em>padchar</em> (default: '&nbsp;') characters are output, if necessary, to pad the output on the left. If <em>arg</em> is negative, then a minus sign is printed. If <em>arg</em> is not negative, then a plus sign is printed if and only if the <code>@</code> modifier was specified. Then a sequence of digits, containing a single embedded decimal point, is output. The form of this sequence of digits depends on the scale factor <em>k</em>. If <em>k</em> is zero, then <em>d</em> digits are printed after the decimal point, and a single zero digit appears before the decimal point. If <em>k</em> is positive, then it must be strictly less than <em>d</em> + 2 and <em>k</em> significant digits are printed before the decimal point, and <em>d − k</em> + 1 digits are printed after the decimal point. If <em>k</em> is negative, then it must be strictly greater than <em>−d</em>. A single zero digit appears before the decimal point and after the decimal point, first <em>−k</em> zeros are output followed by <em>d + k</em> significant digits.</p>
        <p>Following the digit sequence, the exponent is output following character <em>expchar</em> (default: 'E') and the sign of the exponent, i.e. either the plus or the minus sign. The exponent consists of <em>e</em> digits representing the power of 10 by which the fraction must be multiplied to properly represent the rounded value of <em>arg</em>.</p>
        <p>If it is impossible to print the value in the required format in a field of width <em>w</em>, then one of two actions is taken: If the parameter <em>overchar</em> is specified, then <em>w</em> copies of this character are printed instead of <em>arg</em>. If <em>overchar</em> is omitted, then <em>arg</em> is printed using more than <em>w</em> characters, as many more as may be needed. If <em>d</em> is too small for the specified <em>k</em> or <em>e</em> is too small, then a larger value is used for <em>d</em> or <em>e</em> as may be needed.</p>
        <p>If the <em>w</em> parameter is omitted, then the output is of variable width and a value is chosen for <em>w</em> in such a way that no leading padding characters are needed.</p>
        <p>&nbsp;&nbsp;<code>(format "~E" 31.415926)</code> &#10233; <code>3.1415926E+1</code><br>
        &nbsp;&nbsp;<code>(format "~,5E" 0.0003141592)</code> &#10233; <code>3.14159E-4</code><br>
        &nbsp;&nbsp;<code>(format "~,4,2E" 0.0003141592)</code> &#10233; <code>3.1416E-04</code><br>
        &nbsp;&nbsp;<code>(format "~9E" 31.415926)</code> &#10233; <code>3.1416E+1</code><br>
        &nbsp;&nbsp;<code>(format "~10,3,,,,'#E" 31.415926)</code> &#10233; <code>##3.142E+1</code><br>
        &nbsp;&nbsp;<code>(format "~10,4,,3,,'#E" 31.415926)</code> &#10233; <code>#314.16E-1</code><br>
        &nbsp;&nbsp;<code>(format "~7,3,2,,'-E" 31.415926)</code> &#10233; <code>-------</code><br>
        &nbsp;&nbsp;<code>(format "~10,4,,4,,'#@E" 31.415926)</code> &#10233; <code>+3141.6E-2</code></p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~g</strong><br>
      <strong>~G</strong></td>
      <td>
        GENERAL FLOAT:&nbsp;&nbsp;<strong>~<em>w,d,e,k,overchar,padchar,expchar</em>G</strong>
        <p>The next argument <em>arg</em> is output as a floating-point number in either fixed-format or exponential notation as appropriate. The format in which to print <em>arg</em> depends on the magnitude (absolute value) of <em>arg</em>. Let <em>n</em> be an integer such that 10<sup>n−1</sup> ≤ <em>arg</em> &lt; 10<sup>n</sup>. If <em>arg</em> is zero, let <em>n</em> be 0. Let <em>ee</em> equal <em>e</em> + 2, or 4 if <em>e</em> is omitted. Let <em>ww</em> equal <em>w</em> − <em>ee</em>, or nil if <em>w</em> is omitted. If <em>d</em> is omitted, first let <em>q</em> be the number of digits needed to print <em>arg</em> with no loss of information and without leading or trailing zeros; then let <em>d</em> equal <em>max(q, min(n, 7))</em>. Let <em>dd</em> equal <em>d − n</em>.</p>
        <p>If 0 ≤ <em>dd</em> ≤ <em>d</em>, then <em>arg</em> is output as if by the format directives:<br>
        &nbsp;&nbsp;&nbsp;&nbsp;<code>~ww,dd,,overchar,padcharF~ee@T</code><br>
        Note that the scale factor <em>k</em> is not passed to the <code>~F</code> directive. For all other values of <em>dd</em>, <em>arg</em> is printed as if by the format directive:<br>
        &nbsp;&nbsp;&nbsp;&nbsp;<code>~w,d,e,k,overchar,padchar,expcharE</code><br>
        In either case, an <code>@</code> modifier is specified to the <code>~F</code> or <code>~E</code> directive if and only if one was specified to the <code>~G</code> directive.</p>
        <p>&nbsp;&nbsp;<code>(format "|~G|" 712.72)</code> &#10233; <code>|712.72&nbsp;&nbsp;&nbsp;&nbsp;|</code><br>
        &nbsp;&nbsp;<code>(format "|~12G|" 712.72)</code> &#10233; <code>|&nbsp;&nbsp;712.72&nbsp;&nbsp;&nbsp;&nbsp;|</code><br>
        &nbsp;&nbsp;<code>(format "|~9,2G|~9,3,2,3G|~9,3,2,0G|" 0.031415 0.031415 0.031415)</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>|&nbsp;&nbsp;3.14E-2|314.2E-04|0.314E-01|</code><br>
        &nbsp;&nbsp;<code>(format "|~9,2G|~9,3,2,3G|~9,3,2,0G|" 0.314159 0.314159 0.314159)</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>|&nbsp;0.31&nbsp;&nbsp;&nbsp;&nbsp;|0.314&nbsp;&nbsp;&nbsp;&nbsp;|0.314&nbsp;&nbsp;&nbsp;&nbsp;|</code><br>
        &nbsp;&nbsp;<code>(format "|~9,2G|~9,3,2,3G|~9,3,2,0G|" 3.14159 3.14159 3.14159)</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>|&nbsp;&nbsp;3.1&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;3.14&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;3.14&nbsp;&nbsp;&nbsp;&nbsp;|</code><br>
        &nbsp;&nbsp;<code>(format "|~9,2G|~9,3,2,3G|~9,3,2,0G|" 314.159 314.159 314.159)</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>|&nbsp;&nbsp;3.14E+2|&nbsp;&nbsp;314&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;314&nbsp;&nbsp;&nbsp;&nbsp;|</code><br>
        &nbsp;&nbsp;<code>(format "|~9,2G|~9,3,2,3G|~9,3,2,0G|" 3141.59 3141.59 3141.59)</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>|&nbsp;&nbsp;3.14E+3|314.2E+01|0.314E+04|</code></p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~$</strong></td>
      <td>
        DOLLARS FLOAT:&nbsp;&nbsp;<strong>~<em>d,n,w,padchar,curchar,groupchar,groupcol</em>$</strong>
        <p>The next argument <em>arg</em> is output as a floating-point number in a fixed-format notation that is particularly well suited for outputting monetary values. Parameter <em>d</em> (default: 2) defines the number of digits to print after the decimal point. Parameter <em>n</em> (default: 1) defines the minimum number of digits to print before the decimal point. Parameter <em>w</em> (default: 0) is the minimum total width of the output.</p>
        <p>First, padding and the sign are output. If <em>arg</em> is negative, then a minus sign is printed. If <em>arg</em> is not negative, then a plus sign is printed if and only if the <code>@</code> modifier was specified. If the <code>:</code> modifier is used, the sign appears before any padding, and otherwise after the padding. If the number of characters, including the sign and a potential currency symbol is below width <em>w</em>, then character <em>padchar</em> (default: '&nbsp;') is used for padding the number in front of the integer part such that the overall output has <em>w</em> characters. After the padding, the currency symbol <em>curchar</em> is inserted, if available, followed by <em>n</em> digits representing the integer part of <em>arg</em>, prefixed by the right amount of '0' characters. If either parameter <em>groupchar</em> or <em>groupcol</em> is provided, the integer part is output in groups of <em>groupcol</em> characters (default: 3) separated by <em>groupchar</em> (default: ','). After the integer part, a decimal point is output followed by <em>d</em> digits of fraction, properly rounded.</p>
        <p>If the magnitude of <em>arg</em> is so large that the integer part of <em>arg</em> cannot be output with at most <em>n</em> characters, then more characters are generated, as needed, and the total width might overrun as well.</p>
        <p>For cases where a simple currency symbol is not sufficient, it is possible to use a numeric currency code as defined by ISO 4217 for parameter <em>curchar</em>. For positive codes, the shortest currency symbol is being used. For negative currency codes, the corresponding alphabetic code (ignoring the sign) is being used. Library <code>(lispkit system)</code> provides a conventient API to access currency codes.</p>
        <p>By specifying the <code>+</code> modifier, it is possible to enable locale-specific output of the monetary value using the locale provided to <code>format</code>. In this case, also the currency associated with this locale is being used.</p>
        <p>&nbsp;&nbsp;<code>(format "~$" 4930.351)</code> &#10233; <code>4930.35</code><br>
        &nbsp;&nbsp;<code>(format "~3$" 4930.351)</code> &#10233; <code>4930.351</code><br>
        &nbsp;&nbsp;<code>(format "~,6$" 4930.351)</code> &#10233; <code>004930.35</code><br>
        &nbsp;&nbsp;<code>(format "~,6,12,'_$" 4930.351)</code> &#10233; <code>___004930.35</code><br>
        &nbsp;&nbsp;<code>(format "~,6,12,'_@$" 4930.351)</code> &#10233; <code>__+004930.35</code><br>
        &nbsp;&nbsp;<code>(format "~,6,12,'_@:$" 4930.351)</code> &#10233; <code>+__004930.35</code><br>
        &nbsp;&nbsp;<code>(format "~,6,12,'_,'€$" 4930.351)</code> &#10233; <code>__€004930.35</code><br>
        &nbsp;&nbsp;<code>(format "~,6,12,'_,'€@$" 4930.351)</code> &#10233; <code>_+€004930.35</code><br>
        &nbsp;&nbsp;<code>(format "~,,,,,,3$" 4930.351)</code> &#10233; <code>4,930.35</code><br>
        &nbsp;&nbsp;<code>(format "~,6,,,,,3$" 4930.351)</code> &#10233; <code>004,930.35</code><br>
        &nbsp;&nbsp;<code>(format "~,,,,208$" 1234.567)</code> &#10233; <code>kr&nbsp;1234.57</code><br>
        &nbsp;&nbsp;<code>(format "~,,,,-208$" 1234.567)</code> &#10233; <code>DKK&nbsp;1234.57</code><br>
        &nbsp;&nbsp;<code>(format 'de_CH "~+$" 4930.351)</code> &#10233; <code>CHF&nbsp;4930.35</code><br>
        &nbsp;&nbsp;<code>(format 'en_US "~,,,,,,3+$" 4930.351)</code> &#10233; <code>$4,930.35</code><br>
        &nbsp;&nbsp;<code>(format 'de_DE "~,6,14,'_,,,3+$" 4930.351)</code> &#10233; <code>__004.930,35&nbsp;€</code></p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~%</strong></td>
      <td>
        NEWLINE:&nbsp;&nbsp;<strong>~<em>n</em>%</strong>
        <p>This directive outputs <em>n</em> (default: 1) newline characters, thereby terminating the current output line and beginning a new one. No arguments are being consumed. Simply putting <em>n</em> newline escape characters <code>\n</code> into the control string would also work, but <code>~%</code> is often used because it makes the control string look nicer and more consistent.</p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~&</strong></td>
      <td>
        FRESHLINE:&nbsp;&nbsp;<strong>~<em>n</em>&</strong>
        <p>Unless it can be determined that the output is already at the beginning of a line, this directive outputs a newline if <em>n</em> &gt; 0. This conditional newline is followed by <em>n</em> − 1 newlines, it <em>n</em> &gt; 1. Nothing is output if <em>n</em> = 0.</p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~|</strong></td>
      <td>
        PAGE SEPARATOR:&nbsp;&nbsp;<strong>~<em>n</em>|</strong>
        <p>This directive outputs <em>n</em> (default: 1) page separator characters <code>#\page</code>.</p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~~</strong></td>
      <td>
        TILDE:&nbsp;&nbsp;<strong>~<em>n</em>~</strong>
        <p>This directive outputs <em>n</em> (default: 1) tilde characters.</p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~p</strong><br>
      <strong>~P</strong></td>
      <td>
        PLURAL:&nbsp;&nbsp;<strong>~P</strong>
        <p>Depending on the next argument <em>arg</em>, which is expected to be an integer value, a different string is output. If <em>arg</em> is not equal to 1, a lowercase <code>s</code> is output. If <em>arg</em> is equal to 1, nothing is output.</p>
        <p>If the <code>:</code> modifier is provided, the last argument is used instead for <em>arg</em>. This is useful after outputting a number using <code>~D</code>. With the <code>@</code> modifier, <code>y</code> is output if <em>arg</em> is 1, or <code>ies</code> if it is not.</p>
        <p>&nbsp;&nbsp;<code>(format "~D tr~:@P/~D win~:P" 7 1)</code> &#10233; <code>7 tries/1 win</code><br>
        &nbsp;&nbsp;<code>(format "~D tr~:@P/~D win~:P" 1 0)</code> &#10233; <code>1 try/0 wins</code></p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~t</strong><br>
      <strong>~T</strong></td>
      <td>
        TABULATE:&nbsp;&nbsp;<strong>~<em>colnum,colinc</em>T</strong>
        <p>This directive will output sufficient spaces to move the cursor to column <em>colnum</em> (default: 1). If the cursor is already at or beyond column <em>colnum</em>, the directive will output spaces to move the cursor to column <em>colnum + k × colinc</em> for the smallest positive integer <em>k</em> possible, unless <em>colinc</em> (default: 1) is zero, in which case no spaces are output if the cursor is already at or beyond column <em>colnum</em>.</p>
        <p>If modifier <code>@</code> is provided, relative tabulation is performed. In this case, the directive outputs <code>colnum</code> spaces and then outputs the smallest non-negative number of additional spaces necessary to move the cursor to a column that is a multiple of <em>colinc</em>. For example, the directive <code>~3,8@T</code> outputs three spaces and then moves the cursor to a "standard multiple-of-eight tab stop" if not at one already. If the current output column cannot be determined, however, then <em>colinc</em> is ignored, and exactly <em>colnum</em> spaces are output.</p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~*</strong></td>
      <td>
        IGNORE ARGUMENT:&nbsp;&nbsp;<strong>~<em>n</em>*</strong>
        <p>The next <em>n</em> (default: 1) arguments are ignored. If the <code>:</code> modifier is provided, arguments are "ignored backwards", i.e. <code>~:*</code> backs up in the list of arguments so that the argument last processed will be processed again. <code>~n:*</code> backs up <em>n</em> arguments. When within a <code>~{</code> construct, the ignoring (in either direction) is relative to the list of arguments being processed by the iteration.</p>
        <p>The form <code>~n@*</code> is an "absolute goto" rather than a "relative goto": the directive goes to the <em>n</em>-th argument, where 0 means the first one. <em>n</em> defaults to 0 for this form, so <code>~@*</code> goes back to the first argument. Directives after a <code>~n@*</code> will take arguments in sequence beginning with the one gone to. When within a <code>~{</code> construct, the "goto" is relative to the list of arguments being processed by the iteration.</p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~?</strong></td>
      <td>
        INDIRECTION:&nbsp;&nbsp;<strong>~?</strong>
        <p>The next argument <em>arg</em> must be a string, and the one after it <em>lst</em> must be a sequence (e.g. an array). Both arguments are consumed by the directive. <em>arg</em> is processed as a format control string, with the elements of the list <em>lst</em> as the arguments. Once the recursive processing of the control string has been finished, then processing of the control string containing the <code>~?</code> directive is resumed.</p>
        <p>&nbsp;&nbsp;<code>(format "~? ~D" "[~A ~D]" '("Foo" 5) 7)</code> &#10233; <code>[Foo 5] 7</code><br>
        &nbsp;&nbsp;<code>(format "~? ~D" "[~A ~D]" '("Foo" 5 14) 7)</code> &#10233; <code>[Foo 5] 7</code></p>
        <p>Note that in the second example, three arguments are supplied to the control string <code>"(~A ~D)"</code>, but only two are processed and the third is therefore ignored.</p>
        <p>With the <code>@</code> modifier, only one argument is directly consumed. The argument must be a string. It is processed as part of the control string as if it had appeared in place of the <code>~@?</code> directive, and any directives in the recursively processed control string may consume arguments of the control string containing the <code>~@?</code> directive.</p>
        <p>&nbsp;&nbsp;<code>(format "~@? ~D" "[~A ~D]" "Foo" 5 7)</code> &#10233; <code>[Foo 5] 7</code><br>
        &nbsp;&nbsp;<code>(format "~@? ~D" "[~A ~D]" "Foo" 5 14 7)</code> &#10233; <code>[Foo 5] 14</code></p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~(&hellip;~)</strong></td>
      <td>
        CONVERSION:&nbsp;&nbsp;<strong>~(<em>str</em>~)</strong>
        <p>The contained control string <em>str</em> is processed, and what it produces is subject to a conversion. Without the <code>+</code> modifier, a <em>case conversion</em> is performed. <code>~(</code> converts every uppercase character to the corresponding lowercase character, <code>~:(</code> capitalizes all words, <code>~@(</code> capitalizes just the first word and forces the rest to lowercase, and <code>~:@(</code> converts every lowercase character to the corresponding uppercase character. In the following example, <code>~@(</code> is used to cause the first word produced by <code>~R</code> to be capitalized:</p>
        <p>&nbsp;&nbsp;<code>(format "~@(~R~) error~:P" 0)</code> &#10233; <code>Zero errors</code><br>
        &nbsp;&nbsp;<code>(format "~@(~R~) error~:P" 1)</code> &#10233; <code>One error</code><br>
        &nbsp;&nbsp;<code>(format "~@(~R~) error~:P" 23)</code> &#10233; <code>Twenty-three errors</code></p>
        <p>If the <code>+</code> modifier is provided together with the <code>:</code> modifier, all characters corresponding to named XML entities are being converted into names XML entities. If modifier <code>@</code> is added, then only those characters are converted which conflict with XML syntax. The modifier combination <code>+@</code> converts the output by stripping off all diacritics. Modifier <code>+</code> only will escape characters such that the result can be used as a Scheme string literal.</p>
        <p>&nbsp;&nbsp;<code>(format "~+:(~A~)" "© 2021–2023 TÜV")</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>&amp;copy; 2021&amp;ndash;2023 T&amp;Uuml;V</code><br>
        &nbsp;&nbsp;<code>(format "~+:@(~A~)" "&lt;a href=\"t.html\"&gt;© TÜV&lt;/a&gt;")</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>&amp;lt;a href=&amp;quot;t.html&amp;quot;&amp;gt;© TÜV&amp;lt;/a&amp;gt;</code><br>
        &nbsp;&nbsp;<code>(format "~+@(~A~)" "épistèmê")</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>episteme</code><br>
        &nbsp;&nbsp;<code>(format "~+(~A~)" "Hello \"World\"\n")</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>Hello \"World\"\n</code></p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~[&hellip;~]</strong></td>
      <td>
        CONDITIONAL:&nbsp;&nbsp;<strong>~[<em>str<sub>0</sub></em>~;<em>str<sub>1</sub></em>~;&hellip;~;<em>str<sub>n</sub></em>~]</strong>
        <p>This is a set of control strings, called clauses, one of which is chosen and used. The clauses are separated by <code>~;</code> and the construct is terminated by <code>~]</code>.</p>
        <p><em>Without default:</em>&nbsp; From a conditional directive ~[<em>str<sub>0</sub></em>~;<em>str<sub>1</sub></em>~;&hellip;~;<em>str<sub>n</sub></em>~], the <em>arg</em>-th clause is selected, where the first clause is number 0. If a prefix parameter is given as <code>~n[</code>, then the parameter <em>n</em> is used instead of an argument. This is useful only if the parameter is specified by <code>#</code>, to dispatch on the number of arguments remaining to be processed. If <em>arg</em> or <em>n</em> is out of range, then no clause is selected and no error is signaled. After the selected alternative has been processed, the control string continues after the <code>~]</code>.</p>
        <p><em>With default:</em>&nbsp; Whenever the directive has the form ~[<em>str<sub>0</sub></em>~;<em>str<sub>1</sub></em>~;&hellip;~:;<em>default</em>~], i.e. the last clause is separated via <code>~:;</code>, then the conditional directive has a default clause which gets performed whenever no other clause could be selected.</p>
        <p><em>Optional selector:</em>&nbsp; Whenever the directive has the form ~:[<em>none</em>~;<em>some</em>~] the <em>none</em> control string is chosen if <em>arg</em> is <code>nil</code>, otherwise the <em>some</em> control string is chosen.</p>
        <p><em>Boolean selector:</em>&nbsp; Whenever the directive has the form ~+[<em>false</em>~;<em>true</em>~] the <em>false</em> control string is chosen if <em>arg</em> is the boolean value <code>false</code>, otherwise the <em>some</em> control string is chosen.</p>
        <p><em>Selector test:</em>&nbsp; Whenever the directive has the form ~@[<em>true</em>~], the next argument <em>arg</em> is tested for being non-<code>nil</code>. If <em>arg</em> is not <code>nil</code>, then the argument is not used up by the <code>~@[</code> directive but remains as the next one to be processed, and the one clause <em>true</em> is processed. If <em>arg</em> is <code>nil</code>, then the argument is used up, and the clause is not processed. The clause therefore should normally use exactly one argument, and may expect it to be non-<code>nil</code>.</p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~{&hellip;~}</strong></td>
      <td>
        ITERATION:&nbsp;&nbsp;<strong>~<em>n</em>{<em>str</em>~}</strong>
        <p>The iteration directive is used to control how a sequence is output. Thus, the next argument <em>arg</em> should be a sequence which is used as a list of arguments as if for a recursive call to <code>format</code>. The string <em>str</em> is used repeatedly as the control string until all elements from <em>arg</em> are consumed. Each iteration can absorb as many elements of <em>arg</em> as it needs. For instance, if <em>str</em> uses up two arguments by itself, then two elements of <em>arg</em> will get used up each time around the loop. If before any iteration step the sequence is empty, then the iteration is terminated. Also, if a prefix parameter <em>n</em> is given, then there will be at most <em>n</em> repetitions of processing of <em>str</em>. Finally, the <code>~^</code> directive can be used to terminate the iteration prematurely. If the iteration is terminated before all the remaining arguments are consumed, then any arguments not processed by the iteration remain to be processed by any directives following the iteration construct.</p>
        <p>&nbsp;&nbsp;<code>(format "Winners:~{ ~A~}." '("Fred" "Harry" "Jill"))</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>Winners: Fred Harry Jill.</code><br>
        &nbsp;&nbsp;<code>(format "Winners: ~{~#[~;~A~:;~A, ~]~}." '("Fred" "Harry" "Jill"))</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>Winners: Fred, Harry, Jill.</code><br>
        &nbsp;&nbsp;<code>(format "Pairs:~{ &lt;~A,~S&gt;~}." '("A" 1 "B" 2 "C" 3))</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>Pairs: &lt;A, 1&gt; &lt;B, 2&gt; &lt;C, 3&gt;.</code></p>
        <p><code>~:<em>n,m</em>{<em>str</em>~}</code> is similar, but the argument should be a list of sublists. At each repetition step (capped by <em>n</em>), one sublist is used as the list of arguments for processing <em>str</em> with an iteration cap of <em>m</em>. On the next repetition, a new sublist is used, whether or not all elements of the last sublist had been processed.</p>
        <p>&nbsp;&nbsp;<code>(format "Pairs:~:{ &lt;~A,~S&gt;~}." '(("A" 1) ("B" 2) ("C" 3)))</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>Pairs: &lt;A, 1&gt; &lt;B, 2&gt; &lt;C, 3&gt;.</code></p>
        <p><code>~@{<em>str</em>~}</code> is similar to <code>~{<em>str</em>~}</code>, but instead of using one argument that is a sequence, all the remaining arguments are used as the list of arguments for the iteration.</p>
        <p>&nbsp;&nbsp;<code>(format "Pairs:~@{ &lt;~A,~S&gt;~}." "A" 1 "B" 2 "C" 3)</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>Pairs: &lt;A, 1&gt; &lt;B, 2&gt; &lt;C, 3&gt;.</code></p>
        <p><code>~:@{<em>str</em>~}</code> combines the features of <code>~:{<em>str</em>~}</code> and <code>~@{<em>str</em>~}</code>. All the remaining arguments are used, and each one must be a sequence. On each iteration, the next argument is used as a list of arguments to <em>str</em>.</p>
        <p>&nbsp;&nbsp;<code>(format "Pairs:~:@{ &lt;~A,~S&gt;~}." '("A" 1) '("B" 2) '("C" 3))</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>Pairs: &lt;A, 1&gt; &lt;B, 2&gt; &lt;C, 3&gt;.</code></p>
        <p>Terminating the repetition directive with <code>~:}</code> instead of <code>~}</code> forces <em>str</em> to be processed at least once, even if the initial sequence is empty. However, it will not override an explicit prefix parameter of zero. If <em>str</em> is empty, then an argument is used as <em>str</em>. It must be a string and precede any arguments processed by the iteration.</p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~&lt;&hellip;~&gt;</strong></td>
      <td>
        JUSTIFICATION:&nbsp;&nbsp; <strong>~<em>mincol,colinc,minpad,padchar,maxcol,elchar</em>&lt;<em>str</em>~&gt;</strong>
        <p>This directive justifies the text produced by processing control string <em>str</em> within a field which is at least <em>mincol</em> columns wide (default: 0). <em>str</em> may be divided up into segments via directive <code>~;</code>, in which case the spacing is evenly divided between the text segments.</p>
        <p>With no modifiers, the leftmost text segment is left-justified in the field and the rightmost text segment is right-justified. If there is only one text element, it is right-justified. The <code>:</code> modifier causes spacing to be introduced before the first text segment. The <code>@</code> modifier causes spacing to be added after the last text segment. The <em>minpad</em> parameter (default: 0) is the minimum number of padding characters to be output between each segment. Whenever padding is needed, the padding character <em>padchar</em> (default: '&nbsp;') is used. If the total width needed to satisfy the constraints is greater than <em>mincol</em>, then the width used is <em>mincol + k × colinc</em> for the smallest possible non-negative integer <em>k</em> with <em>colinc</em> defaulting to 1.</p>
        <p>&nbsp;&nbsp;<code>(format "|~10,,,'.&lt;foo~;bar~&gt;|")</code> &#10233; <code>|foo....bar|</code><br>
        &nbsp;&nbsp;<code>(format "|~10,,,'.:&lt;foo~;bar~&gt;|")</code> &#10233; <code>|..foo..bar|</code><br>
        &nbsp;&nbsp;<code>(format "|~10,,,'.:@&lt;foo~;bar~&gt;|")</code> &#10233; <code>|..foo.bar.|</code><br>
        &nbsp;&nbsp;<code>(format "|~10,,,'.&lt;foobar~&gt;|")</code> &#10233; <code>|....foobar|</code><br>
        &nbsp;&nbsp;<code>(format "|~10,,,'.:&lt;foobar~&gt;|")</code> &#10233; <code>|....foobar|</code><br>
        &nbsp;&nbsp;<code>(format "|~10,,,'.@&lt;foobar~&gt;|")</code> &#10233; <code>|foobar....|</code><br>
        &nbsp;&nbsp;<code>(format "|~10,,,'.:@&lt;foobar~&gt;|")</code> &#10233; <code>|..foobar..|</code></p>
        <p>Note that <em>str</em> may include format directives. All the clauses in <em>str</em> are processed in order. It is the resulting pieces of text that are justified. The <code>~^</code> directive may be used to terminate processing of the clauses prematurely, in which case only the completely processed clauses are justified.</p>
        <p>If the first clause of a <code>~&lt;</code> directive is terminated with <code>~:;</code> instead of <code>~;</code>, then it is used in a special way. All of the clauses are processed, but the first one is not used in performing the spacing and padding. When the padded result has been determined, then, if it fits on the current line of output, it is output, and the text for the first clause is discarded. If, however, the padded text does not fit on the current line, then the text segment for the first clause is output before the padded text. The first clause ought to contain a newline (such as a <code>~%</code> directive). The first clause is always processed, and so any arguments it refers to will be used. The decision is whether to use the resulting segment of text, not whether to process the first clause. If the <code>~:;</code> has a prefix parameter <em>n</em>, then the padded text must fit on the current line with <em>n</em> character positions to spare to avoid outputting the first clause’s text.</p>
        <p>For example, the control string in the following example can be used to print a list of items separated by comma without breaking items over line boundaries, beginning each line with <code>;;</code>. The prefix parameter 1 in <code>~1:;</code> accounts for the width of the comma that will follow the justified item if it is not the last element in the list, or the period if it is. If <code>~:;</code> has a second prefix parameter, like below, then it is used as the width of the line, overriding the line width as specified by <code>format</code>'s <code>linewidth:</code> parameter (default: 80).</p>
        <p>&nbsp;&nbsp;<code>(format "~%;; ~{~&lt;~%;; ~1,30:; ~S~&gt;~^,~}.~%"</code><br>
        &nbsp;&nbsp;<code>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'("first line" "second" "a long third line"</code><br>
        &nbsp;&nbsp;<code>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"fourth" "fifth"))</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233;&nbsp;<br>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<code>;; "first line", "second",</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<code>;; "a long third line",</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<code>;; "fourth", "fifth".</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</p>
        <p>If there is only one text segment <em>str</em> and parameter <em>maxcol</em> is provided and the length of the output of <em>str</em> is exceeding <em>maxcol</em>, then the output is truncated at width <em>maxcol - 1</em> and the ellipsis character <em>elchar</em> (default: '&hellip;') is inserted at the end.</p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~^</strong></td>
      <td>
        UP AND OUT:&nbsp;&nbsp;<strong>~^</strong>
        <p><em>Continue:</em>&nbsp; The <code>~^</code> directive is an escape construct. If there are no more arguments remaining to be processed, then the immediately enclosing <code>~{</code> or <code>~&lt;</code> directive is terminated. If there is no such enclosing directive, then the entire formatting operation is terminated. In the case of <code>~&lt;</code>, the formatting is performed, but no more segments are processed before doing the justification. The <code>~^</code> directive should appear only at the beginning of a <code>~&lt;</code> clause, because it aborts the entire clause it appears in, as well as all following clauses. <code>~^</code> may appear anywhere in a <code>~{</code> construct.</p>
        <p>&nbsp;&nbsp;<code>(format "Done.~^ ~D warning~:P.~^ ~D error~:P.")</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>Done.</code><br>
        &nbsp;&nbsp;<code>(format "Done.~^ ~D warning~:P.~^ ~D error~:P." 3)</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>Done. 3 warnings.</code><br>
        &nbsp;&nbsp;<code>(format "Done.~^ ~D warning~:P.~^ ~D error~:P." 1 5)</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>Done. 1 warning. 5 errors.</code></p>
        <p>If the directive has the form <code>~<em>n</em>^</code>, then termination occurs if <em>n</em> is zero. If the directive has the form <code>~<em>n,m</em>^</code>, termination occurs if the value of <em>n</em> equals the value of <em>m</em>. If the directive has the form <code>~<em>n,m,o</em>^</code>, termination occurs if <em>n</em> ≤ <em>m</em> ≤ <em>o</em>. Of course, this is useless if all the prefix parameters are literals. At least one of them should be a <code>#</code> or a <code>v</code> parameter.</p>
        <p><em>Break:</em>&nbsp; If <code>~^</code> is used within a <code>~:{</code> directive, then it merely terminates the current iteration step because in the standard case, it tests for remaining arguments of the current step only and the next iteration step commences immediately. To terminate the entire iteration process, use <code>~:^</code>. <code>~:^</code> may only be used if the directive it would terminate is <code>~:{</code> or <code>~:@{</code>. The entire iteration process is terminated if and only if the sublist that is supplying the arguments for the current iteration step is the last sublist (in the case of terminating a <code>~:{</code> directive) or the last argument to that call to format (in the case of terminating a <code>~:@{</code> directive).</p>
        <p>Note that while <code>~^</code> is equivalent to <code>~#^</code> in all circumstances, <code>~:^</code> is not equivalent to <code>~#:^</code> because the latter terminates the entire iteration if and only if no arguments remain for the current iteration step, as opposed to no arguments remaining for the entire iteration process.</p>
        <p>&nbsp;&nbsp;<code>(format "~:{/~A~^ &hellip;~}",</code><br>
        &nbsp;&nbsp;<code>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'(("hot" "dog") ("hamburger") ("ice" "cream") ("french" "fries")))</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>/hot &hellip;/hamburger/ice &hellip;/french &hellip;</code><br>
        &nbsp;&nbsp;<code>(format "~:{/~A~:^ &hellip;~}"</code><br>
        &nbsp;&nbsp;<code>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'(("hot" "dog") ("hamburger") ("ice" "cream") ("french" "fries")))</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>/hot &hellip;/hamburger &hellip;/ice &hellip;/french</code><br>
        &nbsp;&nbsp;<code>(format "~:{/~A~#:^ &hellip;~}"</code><br>
        &nbsp;&nbsp;<code>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'(("hot" "dog") ("hamburger") ("ice" "cream") ("french" "fries")))</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>/hot &hellip;/hamburger</code></p>
      </td>
    </tr>
    <tr valign="top">
      <td><strong>~`&hellip;~‘</strong></td>
      <td>
        UPACK:&nbsp;&nbsp;<strong>~`<em>str</em>~‘</strong>
        <p>This directive is used to format composite objects, such as rational numbers, complex numbers, colors, date-time objects, error objects, records, etc. Such objects get decomposed into a sequence of individual values which are formatted by the <em>str</em> control string.</p>
       <p>The next argument <em>arg</em> can be any Scheme object. If there is a decomposition predefined for this type of objects, it is applied to <em>arg</em> and <em>str</em> is used to format the resulting sequence of values. If no decomposition is possible, <em>str</em> is output assuming there is one argument <em>arg</em>.</p>
        <p>&nbsp;&nbsp;<code>(format "~S~:* = ~`(~S, ~S)~‘" 17/3)</code> &#10233; <code>17/3 = (17, 3)</code><br>
        &nbsp;&nbsp;<code>(format "Bits =~`~*~{ ~D~}~‘" (bitset 1 2 7))</code> &#10233; <code>Bits = 1 2 7</code><br>
        &nbsp;&nbsp;<code>(format "Color: ~`R=~F, G=~F, B=~F~‘" (color 0.3 1.0 0.74))</code><br>
        &nbsp;&nbsp;&nbsp;&nbsp;&#10233; <code>Color: R=0.3, G=1.0, B=0.74</code></p>
      </td>
    </tr>
  </tbody>
</table>


## Formatting configurations

A few formatting directives provided by procedure `format` require access to environment variables such as the locale, the width of tab characters, the length of lines, etc. Also the type-specific customization of the formatting of native and user-defined objects, e.g. via the `~S` directive, is based on a formatting control registry defined by an environment variable.

All relevant environment variables are bundled together into
_format config_ objects. Format configurations are organized hierarchically. Each format configuration optionally refers to a parent configuration. It inherits all environment variables and allows their values to be overridden.

The root of this format configuration hierarchy constitutes `base-format-config`. Typically, changes to this object impact all invocations of `format`, unless format is called with a custom format config object which is not derived from `base-format-config`. Without a custom format config, `format` reads the environment variables from the _current format config_ parameter `current-format-config` (which, by default, inherits from `base-format-config`). Like every other parameter object, it is possible to define a new config dynamically via `parameterize`.

Format config objects are also used in combination with type-specific formatting as provided by the `~S` directive, as explained in the next section.


## Type-specific formatting

Procedure `format` provides great means to format numbers, characters, strings, as well as sequences, i.e. lists and vectors. But as soon as values of data types encapsulating their state have to be output, only the default textual representation is supported, which is also used when a value is output via procedure `write`.

For this reason, procedure `format` supports the customization of how composite objects are formatted. The approach for doing this is simple: Internally, a composite object can be mapped ("unpacked") into a vector of "field values". These field values are then interpreted as arguments for an object type-specific control string which defines how the field values of such objects are formatted. If there is no object type-specific control string available, the object is output as if it was written via procedure `write`.

The following example shows how to customize the formatting of objects defined by a record type. The following record is used to model colored 2-dimensional points:

```scheme
(define-record-type <point>
  (make-point x y c)
  point?
  (x point-x)
  (y point-y)
  (c point-color))
```

By default, objects of type `<point>` are output in the following way:

```scheme
(define pt (make-point 7 13 (color 0.5 0.9 0)))
(format "~S" pt)
 ⇒ "#<record <point>: x=7, y=13, c=#<color 0.5 0.9 0.0>>"
```

LispKit defines a type tag for every type. This type tag will later be used to define a custom format for records of type `<point>`. We can retrieve the type tag for type `<point>` via procedure `record-type-tag`:

```scheme
(define point-type-tag (record-type-tag <point>))
```

Now we can define a custom format for objects of type `<point>` in which we refer to the unpacked fields in the order as defined in the `<point>` record type definition following a fixnum value denoting the identity of the record. The following control string formats `<point>` records in this way: `point{x=?,y=?,color=?}`. Note that it skips the record identity via the `~*` directive.

```scheme
"point{x=~*~S,y=~S,c=~S}"
```

`format` refers to a number of environment variables via a formatting configuration (see previous section). The default configuration is defined by definition `base-format-config` and it includes custom type-specific formats. With procedure `format-config-control-set!` we can declare that all objects of type `<point>` should be formatted with the control string shown above:

```scheme
(format-config-control-set!
  base-format-config
  point-type-tag
  "point{x=~*~S,y=~S,c=~S}")
```

Formatting records of type `<point>` via the `~S` directive is now based on this new control string.

```scheme
(format "~S" pt)
 ⇒ "point{x=7,y=13,c=#<color 0.5 0.9 0.0>}"
```

If we wanted to also change how colors are formatted, we could do that in a similar way:

```scheme
(format-config-control-set!
  base-format-config
  color-type-tag
  "color{~S, ~S, ~S}")
```

Now colors are formatted differently:

```scheme
(format "~S" pt)  ⇒ "point{x=7,y=13,c=#<color 0.5 0.9 0.0>}"
(format "~S" (color 1.0 0.3 0.7))  ⇒ "color{1.0, 0.3, 0.7}"
```

If we wanted to change the way how colors are formatted only in the context of formatting points, we could do that by creating a formatting configuration for colors and associate it only with the formatting control string for points. The following code first removes the global color format so that colors are formatted again using the default mechanism. Then it redefines the formatting control for points by also specifying a format configuration that is used while applying the point formatting control string.

```scheme
(format-config-control-remove! base-format-config color-type-tag)
(format-config-control-set!
  base-format-config
  point-type-tag 
  "point{x=~*~S,y=~S,c=~S}"
  (format-config (list color-type-tag "color{~S, ~S, ~S}")))
(format "~S" (color 1.0 0.3 0.7))  ⇒ "#<color 1.0 0.3 0.7>"
(format "~S" pt)  ⇒ "point{x=7,y=13,c=color{0.5, 0.9, 0.0}}"
```


## API

**format-config-type-tag** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `format-config` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all formatting configurations objects.

**base-format-config** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Formatting configurations can have parent configurations from which all formatting environment variables are being inherited. `base-format-config` is the root formatting configuration for `repl-format-config` and `current-format-config`.

**repl-format-config** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The formatting configuration that a read-eval-print loop might use for displaying the result of an evaluation. Initially, `repl-format-config` is set to an empty formatting configuration with parent `base-format-config`.

**current-format-config** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

Parameter object referring to the current formatting configuration that is used as a default whenever no specific formatting configuration is specified, e.g. by procedure `format`. Initially, `current-format-config` is set to an empty formatting configuration with parent `base-format-config`.

**(format _[port] [config] [locale] [tabw [linew]] cntrl arg ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`format` is the universal formatting procedure provided by library `(lispkit format)`. `format` creates formatted output by outputting the characters of the control string _cntrl_ while interpreting formatting directives embedded in _cntrl_. Each formatting directive is prefixed with a tilde which might be preceded by formatting parameters and modifiers. The next character identifies the formatting directive and thus determines what output is being generated by the directive. Most directives use one or more arguments _arg_ as input.

Formatting configuration _config_ defines environment variables influencing the output of some formatting directives. If _config_ is not provided, the formatting configuration from parameter object `current-format-config` is used. For convenience, some environment variables, such as _locale_, can be overridden if they are provided when `format` is being invoked. _locale_ refers to a locale identifier like `en_US` that is used by locale-specific formatting directives. _tabw_ defines the maximum number of space characters that correspond to a single tab character. _linew_ specifies the number of characters per line; this is used by the justification directive only.

**(format-config? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a formatting configuration; otherwise `#f` is returned.

**(format-config _[parent] [locale] [tabw [linew]] (tag cntrl [config]) ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new formatting configuration with _parent_ as parent configuration. If _parent_ is not provided explicitly, `current-format-config` is used. If _parent_ is `#f`, the new formatting configuation will not have a parent configuration. _locale_ refers to a locale identifier like `en_US` that is used by locale-specific formatting directives. _tabw_ defines the maximum number of space characters that correspond to a single tab character. _linew_ specifies the maximum number of characters per line.

**(make-format-config _parent_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-format-config _parent locale_)**  
**(make-format-config _parent locale tabw_)**  
**(make-format-config _parent locale tabw linew_)**  

Creates a new formatting configuration with _parent_ as parent configuration. If _parent_ is `#f`, the new formatting configuation does not have a parent configuration. The remaining arguments define overrides for the environment variables inherited from _parent_.

_locale_ refers to a locale identifier like `en_US` that is used by locale-specific formatting directives. _tabw_ defines the maximum number of space characters that correspond to a single tab character. _linew_ specifies the maximum number of characters per line.

**(copy-format-config _config_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(copy-format-config _config collapse?_)**  

Returns a copy of formatting configuration _config_. If either _collapse?_ is omitted or set to `#f`, a 1:1 copy of _config_ is being made. If _collapse?_ is set to true, a new format config without parent configuration is created which contains the same values for the supported formatting environment variables as _config_.

**(merge-format-config _child parent_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Merges the format configurations _child_ and _parent_ by creating a new collapsed copy of _child_ whose parent configuration _parent_ is.

**(format-config-locale)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(format-config-locale _config_)**  

Returns the locale defined by format configuration _config_. If _config_ defines a locale itself, it is being returned. Otherwise, the locale of the parent configuration of _config_ gets returned. If _config_ is not provided, the default configuration `current-format-config` is used.

**(format-config-locale-set! _locale_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(format-config-locale-set! _config locale_)**  

Sets the locale of the format configuration _config_ to _locale_. If _locale_ is `#f`, the locale setting gets removed from _config_ (but might still get inherited from _config_'s parents). If _config_ is not provided, the default configuration `current-format-config` gets mutated.

**(format-config-tabwidth)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(format-config-tabwidth _config_)**  

Returns the width of a tab character in terms of space characters defined by format configuration _config_. If _config_ defines a tab width itself, it is being returned. Otherwise, the tab width of the parent configuration of _config_ gets returned. If _config_ is not provided, the default configuration `current-format-config` is used.

**(format-config-tabwidth-set! _tabw_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(format-config-tabwidth-set! _config tabw_)**  

Sets the tab width of the format configuration _config_ to _tabw_. If _tabw_ is `#f`, the tab width setting gets removed from _config_ (but might still get inherited from _config_'s parents). If _config_ is not provided, the default configuration `current-format-config` gets mutated. The "tab width" is the maximum number of space characters representing one tab character.

**(format-config-linewidth)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(format-config-linewidth _config_)**  

Returns the maximum number of characters per line defined by format configuration _config_. If _config_ defines a line width itself, it is being returned. Otherwise, the line width of the parent configuration of _config_ gets returned. If _config_ is not provided, the default configuration `current-format-config` is used.

**(format-config-linewidth-set! _linew_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(format-config-linewidth-set! _config linew_)**  

Sets the line width of the format configuration _config_ to _linew_. If _linew_ is `#f`, the line width setting gets removed from _config_ (but might still get inherited from _config_'s parents). If _config_ is not provided, the default configuration `current-format-config` gets mutated. The "line width" is the maximum number of characters per line.

**(format-config-control-set! _tag cntrl_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(format-config-control-set! _tag cntrl sconf_)**  
**(format-config-control-set! _config tag cntrl_)**  
**(format-config-control-set! _config tag cntrl sconf_)**  

Declares for formatting configuration _config_ that objects whose type has type tag _tag_ are being formatted with control string _cntrl_ by formatting directive `~S`. If formatting configuration _sconf_ is provided, it is used as a type-specific configuration that is merged with the current configuration when `~S` formats objects of type tag _tag_. If _cntrl_ is `#f`, type-specific formatting rules for _tag_ are being removed from _conf_ (but might still be inherited from the parent of _conf_). If _cntrl_ is `#t`, native formatting is being forced for _tag_, no matter what is inherited from the parent of _config_. If _config_ is not provided, the default configuration `current-format-config` gets mutated.

**(format-config-control-remove! _tag_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(format-config-control-remove! _config tag_)**  

Removes any type-specific formatting with directive `~S` for objects whose type has tag _tag_ from formatting configuration _config_. If _config_ is not provided, the default configuration `current-format-config` gets mutated.

**(format-config-controls)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(format-config-controls _config_)**  

Returns a list of type tags, i.e. symbols, for which there is a type-specific formatting control string defined by formatting configuration _config_ or its parents. If _config_ is not provided, the default configuration `current-format-config` gets mutated.

**(format-config-parent)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(format-config-parent _config_)**  

Returns the parent configuration of format configuration _config_. If _config_ is not provided, the default configuration `current-format-config` is used. `format-config-parent` returns `#f` if _config_ does not have a parent formatting configuration.
