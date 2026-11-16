# SRFI 275: Utilities

Library `(srfi 275 utils)` provides two procedures for percent-encoding and decoding strings, and four predicates that tell whether a component of an IRI or URI can be set without violating the consistency of the identifier.

```scheme
(import (srfi 275 iri) (srfi 275 utils))

(define example (string->iri "http://example.org/some/where/place"))
(iri->string (update-iri-user example "person!pass"))
  ⇒  "http://person!pass@example.org/some/where/place"
(iri->string (update-iri-user example (encode-string "person!pass")))
  ⇒  "http://person%21pass@example.org/some/where/place"
(user-settable? example "joe")   ⇒  #t
(host-settable? (string->iri "urn://example.org//p") #f)   ⇒  #f
```

## Percent-encoding

**(encode-string _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(encode-string _str cset_)**  

Returns a new string in which all characters of _str_ that are members of the SRFI 14 character set _cset_ are replaced by percent-encoded bytes: each such character is converted to its UTF-8 encoding, and each byte is written as `%XX` with upper-case hexadecimal digits. The default for _cset_ is the complement of `char-set:iri-unreserved`; with it, the reserved characters of RFC 3986, such as `@` and `!`, are encoded, and the characters which an IRI can contain without special meaning are kept.

The percent character is treated like any other character and not as the start of an escape: it is _always_ encoded as `%25`, independent of _cset_. An error is raised if _str_ is not a string or _cset_ is not a character set.

Setters of IRIs and URIs escape the characters that cannot appear in a component, but they leave reserved characters alone, since these can be meaningful. Use `encode-string` to encode them explicitly.

```scheme
(encode-string "user@host")
  ⇒  "user%40host"
(encode-string "user%40host")
  ⇒  "user%2540host"
(encode-string "user@host" char-set:full)
  ⇒  "%75%73%65%72%40%68%6F%73%74"
(encode-string "a béc♂d😎e" (char-set-complement char-set:uri))
  ⇒  "a%20b%C3%A9c%E2%99%82d%F0%9F%98%8Ee"
(encode-string "%a" (char-set-complement (char-set #\%)))
  ⇒  "%25%61"
```

**(decode-string _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(decode-string _str cset_)**  

Returns a new string in which sequences of percent-encoded bytes in _str_ are replaced by the characters they encode in UTF-8, provided that the character is a member of the SRFI 14 character set _cset_. The default for _cset_ is `char-set:full`, which means that all escapes are decoded. Escapes which stand for characters that are not in _cset_ are kept. An error is raised if _str_ contains a `%` that is not followed by two hexadecimal digits, or if the escaped bytes are not a valid UTF-8 sequence: overlong encodings, truncated sequences, surrogates, and values above U+10FFFF are rejected. The irritants of the error are _str_ and the index of the escape that caused it.

```scheme
(decode-string (encode-string "user%40host"))   ⇒  "user%40host"
(decode-string "%41%62%43")                    ⇒  "AbC"
(decode-string "%41%62%43" char-set:upper-case)  ⇒  "A%62C"
(decode-string "%c3%a9%2f" (char-set #\é))     ⇒  "é%2F"
(decode-string "a%20b%C3%A9c%E2%99%82d%F0%9F%98%8Ee" (char-set-complement char-set:uri))
⇒  "a béc♂d😎e"
(decode-string "a%E0%80%80%A0b") ⇒  ERROR  ; overlong encoding
(decode-string "a%F0%9F%98")     ⇒  ERROR  ; incomplete sequence
(decode-string "100%")           ⇒  ERROR
```

The specification of the SRFI contains two examples which cannot be reproduced literally. It gives `"%25%61"` as the result of `(encode-string "%a")`, which is only the result if the second argument is a set that contains `a`, since `a` is unreserved and not encoded by default. Furthermore, it shows `"a béc%E2%99%82d😎e"` as the result of decoding with the complement of `char-set:uri`, although `♂` (U+2642) is a member of this set. The implementation follows the description of the procedures and the tests of the SRFI.

## Setting components

The predicates of this section check the conditions under which the authority components and the path of an IRI or URI can be replaced without producing an inconsistent identifier. The conditions are described in section "Setters" of library `(srfi 275 iri)`. The setters `update-iri-user`, `update-uri-path`, etc. raise an error if these conditions are not satisfied. The predicates make it possible to test the conditions without handling errors. They work for both IRIs and URIs. An error is raised if the first argument is neither an IRI nor a URI.

**(user-settable? _ident obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(host-settable? _ident obj_)**  
**(port-settable? _ident obj_)**  

Return `#t` if the user, host, or port of the IRI or URI _ident_, respectively, can be set to _obj_ with the corresponding setter, with regard to the existing path and the other two authority components. For efficiency, _obj_ is not parsed or checked: the only property that matters is whether it is `#f`, which means that the component is removed, or not. Procedures that actually set the component may still raise an error if _obj_ has the wrong type or is not valid.

```scheme
(define example (string->iri "http://example.org/p"))
(user-settable? example "joe")              ⇒  #t
(user-settable? (string->iri "a/p") "joe")
  ⇒  #f ; relative path next to an authority
(host-settable? (string->iri "urn://example.org//p") #f)
  ⇒  #f   ; path would start with //
(port-settable? (string->iri "urn:a/b") 80) ⇒  #f
```

**(path-settable? _ident path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the path of the IRI or URI _ident_ can be set to the path object _path_ with regard to the existing authority components, and `#f` if not. It also returns `#f` if _path_ is not a path object.

```scheme
(path-settable? (string->iri "urn:/a") (string->path "//b")) ⇒  #f
(path-settable? (string->iri "//h/a") (string->path "b"))    ⇒  #f
(path-settable? (string->iri "a") (string->path "/b"))       ⇒  #t
(path-settable? (string->iri "a") (string->path "b:c"))
  ⇒  #f   ; would look like a scheme
(path-settable? (string->iri "urn:a") (string->path "b:c"))  ⇒  #t
```
