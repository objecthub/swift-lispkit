# SRFI 275: Normalization and Resolution

Library `(srfi 275 normalize)` provides procedures for normalizing IRIs and URIs, for resolving relative references against a base IRI or URI, for the inverse operation, which computes a relative reference, and for converting between IRIs and URIs. Each procedure comes in two variants, one for IRIs (as defined by library `(srfi 275 iri)` and one for URIs (library `(srfi 275 uri)`. IRIs and URIs are disjoint types, and procedures raise an error when they are called with an object of the wrong type.

```scheme
(import (srfi 275 iri) (srfi 275 normalize))

(iri->string (normalize-iri (string->iri "HTTP://Example.ORG/a/./b/../%7euser/%c3%a9")))
⇒  "http://example.org/a/~user/é"

(define base (string->iri "http://a/bb/ccc/d;p?q"))
(iri->string (resolve-iri base (string->iri "../g")))                    ⇒  "http://a/bb/g"
(iri->string (relativize-iri base (string->iri "http://a/bb/g")))        ⇒  "../g"
```

In the examples of this page an IRI or URI object is written as `<...>`, enclosing the text it was parsed from, as in the SRFI.

## Normalization

Three kinds of normalization are supported, which are independent of the scheme of an IRI or URI:

- **Case normalization.** Scheme and host are case-insensitive; they are converted to lower-case. Only ASCII characters are affected: for instance, `É` does not become `é`. Escapes always use upper-case hexadecimal digits, which is already guaranteed when IRIs and URIs are parsed. Other components are case-sensitive and are not changed.
- **Escape normalization.** Escapes (percent-encoded bytes) that stand for characters which are permitted unescaped in the component and that are not reserved characters are decoded. Escapes of reserved characters are never decoded, and reserved characters in the text are never escaped. For IRIs, sequences of escapes that form a UTF-8 encoded character are decoded if this character is permitted in an IRI. For URIs, only escapes of ASCII characters are decoded; all other characters remain escaped.
- **Path segment normalization.** The dot segments `.` and `..` are removed from the path as described by `remove-dot-segments` in library `(srfi 275 path)`. This only applies to IRIs and URIs with a scheme. The paths of relative references are not normalized, since the meaning of dot segments depends on the base against which a reference is resolved. Procedures that normalize paths therefore return relative references unchanged. Relative paths in non-relative IRIs, such as `foo:a/b/../.././../../e`, are normalized as well; the result of the latter is `foo:e`.

All normalization procedures are idempotent. They preserve the structure of their argument: a normalization never makes a path conflict with the authority and never turns a non-relative IRI into a relative reference.

**(normalize-iri-case _iri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(normalize-uri-case _uri_)**  

Return a copy of an IRI or URI in which the scheme and the host are lower-case.

```scheme
(normalize-iri-case (string->iri "HttP://Example.ORG/Ex#test"))   ⇒  <http://example.org/Ex#test>
(normalize-iri-case (string->iri "http://CRÊPES.example.org"))    ⇒  <http://crÊpes.example.org>
(normalize-iri-case (string->iri "http://USER@example.org/Some/Where/Place?Query#test"))
⇒  <http://USER@example.org/Some/Where/Place?Query#test>
```

**(normalize-iri-escape _iri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(normalize-uri-escape _uri_)**  

Return a copy of an IRI or URI with normalized escapes in all components.

```scheme
(normalize-iri-escape (string->iri "http://crepes.example.org/in/Rennes?Dim.%E2%80%A5Sam."))
⇒  <http://crepes.example.org/in/Rennes?Dim.‥Sam.>
(normalize-iri-escape (string->iri "https://en.wiktionary.org/wiki/%E1%BF%AC%CF%8C%CE%B4%CE%BF%CF%82"))
⇒  <https://en.wiktionary.org/wiki/Ῥόδος>
(normalize-iri-escape (string->iri "http://my%40name@example.org/ex#test"))
⇒  <http://my%40name@example.org/ex#test>
(normalize-iri-escape (string->iri "http://example.org/%7Efoo%2f%41"))
⇒  <http://example.org/~foo%2FA>
(normalize-uri-escape (string->uri "http://example.org/%7Efoo%2f%41%E2%80%A5"))
⇒  <http://example.org/~foo%2FA%E2%80%A5>
```

**(normalize-iri-path _iri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(normalize-uri-path _uri_)**  

Return a copy of an IRI or URI whose path was normalized by removing dot segments. Relative references are returned unchanged.

```scheme
(normalize-iri-path (string->iri "http://example.org/a/./b/../c"))   ⇒  <http://example.org/a/c>
(normalize-iri-path (string->iri "foo:a/b/../.././../../e"))         ⇒  <foo:e>
(normalize-iri-path (string->iri "a/b/../c"))                        ⇒  <a/b/../c>
```

**(normalize-iri _iri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(normalize-uri _uri_)**  

Normalize an IRI or URI with respect to escapes, case, and path segments. This is equivalent to the composition of the three procedures above, but the checks that ensure the consistency of authority and path are only done once.

```scheme
(normalize-iri (string->iri "HTTP://Example.ORG/a/./b/../%7euser/%c3%a9"))
⇒  <http://example.org/a/~user/é>
(normalize-uri (string->uri "HTTP://Example.ORG/a/./b/../%7euser/%c3%a9"))
⇒  <http://example.org/a/~user/%C3%A9>
```

## Resolution and relativization

**(resolve-iri _base iri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(resolve-uri _base uri_)**  

Resolve the IRI or relative reference _iri_ against the non-relative IRI _base_ and return the resulting IRI. This implements the algorithm of section 5.2 of RFC 3986 (strict variant), with dot segments removed from the resulting path. If _iri_ has a scheme, the result is _iri_ with dot segments removed from the path. An error is raised if _base_ is not a non-relative IRI.

```scheme
(define base (string->iri "http://a/bb/ccc/d;p?q"))
(define (resolve ref) (iri->string (resolve-iri base (string->iri ref))))

(resolve "g:h")        ⇒  "g:h"
(resolve "g")          ⇒  "http://a/bb/ccc/g"
(resolve "./g")        ⇒  "http://a/bb/ccc/g"
(resolve "g/")         ⇒  "http://a/bb/ccc/g/"
(resolve "/g")         ⇒  "http://a/g"
(resolve "//g")        ⇒  "http://g"
(resolve "?y")         ⇒  "http://a/bb/ccc/d;p?y"
(resolve "g?y")        ⇒  "http://a/bb/ccc/g?y"
(resolve "#s")         ⇒  "http://a/bb/ccc/d;p?q#s"
(resolve "")           ⇒  "http://a/bb/ccc/d;p?q"
(resolve "..")         ⇒  "http://a/bb/"
(resolve "../../../g") ⇒  "http://a/g"
```

**(relativize-iri _base iri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(relativize-uri _base uri_)**  

The inverse of `resolve-iri`: returns a relative reference which resolves against the non-relative IRI _base_ to _iri_, which has to be a non-relative IRI as well. Note that the argument order is the same as for `resolve-iri`; relativization functions like `relative-from` in other libraries have their arguments flipped. Both arguments are normalized with respect to dot segments in the process. The relative reference is constructed as follows:

1. If the schemes differ, _iri_ itself is returned.
2. If the authorities differ, the result is a network-path reference (it starts with `//`) with the components of _iri_.
3. If the paths differ, the result is a relative reference with a relative path, no authority, and the query and fragment of _iri_. The path consists of `..` segments to leave the part of the base path that is not shared with the path of _iri_, followed by the remaining segments of the path of _iri_.
4. If the paths are the same but the queries differ, the result has an empty path, the query and the fragment of _iri_.
5. Otherwise, path and query are the same. The result then consists of the last segment of the path (or `.` if this segment is empty), the fragment of _iri_, but no query.

```scheme
(define base (string->iri "http://a/bb/ccc/d;p?q"))
(define (relativize iri) (iri->string (relativize-iri base (string->iri iri))))

(relativize "http://a/bb/ccc/g")       ⇒  "g"
(relativize "http://a/bb/g")           ⇒  "../g"
(relativize "http://a/x/y?z")          ⇒  "../../x/y?z"
(relativize "http://a/bb/ccc/d;p?r")   ⇒  "?r"
(relativize "http://other/x")          ⇒  "//other/x"
(relativize "https://a/bb/ccc/g")      ⇒  "https://a/bb/ccc/g"
(relativize "http://a/bb/ccc/d;p?q#f") ⇒  "d;p#f"
```

An error is raised, with the normalized path and the offending argument as irritants, if the normalized path of one of the arguments is neither absolute nor empty, as it is the case for IRIs like `foo:a/b`. Another error is raised if _base_ or _iri_ is not a non-relative IRI.

As a consequence of the algorithm specified by the SRFI, `resolve-iri` is not an exact inverse of `relativize-iri` if the paths of _base_ and _iri_ are equal and _base_ has a query. Resolving the relative reference does not reproduce the query of _iri_ in case 5, and an empty reference in case 4 resolves to _base_ including its query:

```scheme
(define base (string->iri "http://a/bb/ccc/d;p?q"))
(define (round-trip iri)
  (let ((rel (relativize-iri base (string->iri iri))))
    (list (iri->string rel) (iri->string (resolve-iri base rel)))))

(round-trip "http://a/bb/ccc/g?y#z")    ⇒  ("g?y#z" "http://a/bb/ccc/g?y#z")
(round-trip "http://a/bb/ccc/d;p?q")    ⇒  ("d;p" "http://a/bb/ccc/d;p")
(round-trip "http://a/bb/ccc/d;p")      ⇒  ("" "http://a/bb/ccc/d;p?q")
```

## Conversion

**(iri->uri _iri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Converts an IRI into a URI by escaping all characters that are not permitted in URIs as sequences of percent-encoded UTF-8 bytes. The conversion preserves the structure: non-relative IRIs become non-relative URIs and relative references become relative references.

```scheme
(uri->string (iri->uri (string->iri "https://en.wiktionary.org/wiki/Ῥόδος")))
⇒  "https://en.wiktionary.org/wiki/%E1%BF%AC%CF%8C%CE%B4%CE%BF%CF%82"
(uri->string (iri->uri (string->iri "foo:a/é?ü#ö")))
⇒  "foo:a/%C3%A9?%C3%BC#%C3%B6"
```

**(uri->iri _uri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Converts a URI into an IRI. The URI is first interpreted as an IRI, which is then normalized with respect to escapes: sequences of escapes that form characters permitted in an IRI are decoded. Escapes of reserved characters remain.

```scheme
(iri->string (uri->iri (string->uri "https://en.wiktionary.org/wiki/%E1%BF%AC%CF%8C%CE%B4%CE%BF%CF%82")))
⇒  "https://en.wiktionary.org/wiki/Ῥόδος"
(iri->string (uri->iri (string->uri "foo:a/%C3%A9?%C3%BC#%C3%B6%20")))
⇒  "foo:a/é?ü#ö%20"
```

## Notes

- The SRFI specifies the examples for `normalize-uri-escape` and some other procedures informally. An example like `(normalize-uri-escape (string->uri "http://dosh£@crepes.example.org"))` cannot be reproduced literally: `string->uri` does not escape automatically and raises an error for the character `£`. Convert IRIs to URIs with `iri->uri` instead.
- Relativization follows the algorithm of procedure `relative-from` of the Chicken Scheme library `uri-generic`, on which the specification and the tests of the SRFI are based.
