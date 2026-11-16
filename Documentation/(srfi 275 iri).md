# SRFI 275: IRIs

[SRFI 275](https://srfi.schemers.org/srfi-275/srfi-275.html) provides a programming interface for Internationalized Resource Identifiers (IRIs, [RFC 3987](https://www.rfc-editor.org/rfc/rfc3987)), Uniform Resource Identifiers (URIs, [RFC 3986](https://www.rfc-editor.org/rfc/rfc3986)), and basic paths. It is split into five libraries which can be imported independently of each other. There is no umbrella library `(srfi 275)`.

| Library | Functionality |
| :--- | :--- |
| `(srfi 275 iri)` | IRIs: parsing, serialization, getters, persistent setters, and character sets |
| `(srfi 275 uri)` | URIs: the same interface as for IRIs, restricted to the characters of RFC 3986 |
| `(srfi 275 normalize)` | Normalization, relative reference resolution, relativization, and conversion between IRIs and URIs |
| `(srfi 275 path)` | Path objects: construction, access, persistent update, and dot segment removal |
| `(srfi 275 utils)` | Percent-encoding of strings and predicates that check whether authority components or paths can be set |

Library `(srfi 275 iri)` implements IRIs and IRI relative references. It is the main reference for the concepts shared by all libraries of the SRFI; the other pages refer to this page.

## Components

An IRI is an immutable object consisting of the following components, all of which can be absent (`#f`) except for the path:

| Component | Type | Example in `http://user:pw@example.org:8080/a/b?q#frag` |
| :--- | :--- | :--- |
| scheme | string | `"http"` |
| user | string | `"user:pw"` |
| host | string | `"example.org"` |
| port | non-negative integer | `8080` |
| path | path object (see `(srfi 275 path)` | path with segments `("a" "b")` |
| query | string | `"q"` |
| fragment | string | `"frag"` |

User, host, and port form the _authority_ of an IRI. Components that are present but empty are distinct from absent components: for `http://example.org?` the query is `""`, whereas for `http://example.org` it is `#f`. There is no empty port; a port in the text of an IRI that consists of no digits is absent. IP literals are kept in square brackets, e.g. `"[::1]"`, and are neither parsed nor normalized.

An IRI with a scheme is called _non-relative_. An IRI without scheme is a _relative reference_. Like RFC 3987's "IRI-reference", the type tested by `iri?` covers both. An IRI that is non-relative and has no fragment is called an _absolute_ IRI in the sense of RFC 3987.

In the examples of this page an IRI object is written as `<...>`, enclosing the text it was parsed from, exactly as in the SRFI. IRI objects themselves print as records in LispKit; use `iri->string` to get their text.

## Strings, escapes, and errors

Components are strings in which characters are either literal or percent-encoded as a sequence of `%XX` escapes (with upper-case hexadecimal digits) representing bytes of the UTF-8 encoding of a character. Getters return these strings as they are stored: escapes are not decoded. Whenever escapes appear, the bytes they stand for have to form valid UTF-8; otherwise an error is raised.

All errors, including those that the SRFI calls assertion violations, are raised as regular error objects with `error`. The message of such an error object starts with the name of the procedure that raised the error, e.g. `"string->iri: illegal character"`. The irritants are those specified by the SRFI:

- Parsing procedures that find an illegal character have the character, its index in the parsed text, and the parsed text as irritants. If the text ends prematurely, the irritants are the EOF object and the length of the text.
- Procedures that reject their arguments, e.g. because setting a component would produce an inconsistent IRI, have the IRI and the arguments as irritants.

## LispKit implementation notes

The LispKit implementation of SRFI 275 is a port of the reference implementation by Duncan Guthrie, rewritten as pure Scheme libraries. It differs from the reference implementation in the following ways:

- **UTF-16 code units.** LispKit strings are sequences of UTF-16 code units, and characters have values between 0 and 65535. Characters outside of the basic multilingual plane, such as `😎`, are represented by pairs of surrogate code units; the IRI libraries always treat these pairs as one character when parsing, escaping, and unescaping. SRFI 14 character sets in LispKit also contain code units. For this reason, the character sets exported by this library contain all surrogate code units whenever they contain characters outside of the basic multilingual plane. Indices in error irritants count code units. `\x1F600;` cannot be used in string literals to create supplementary characters; write the character itself.
- **Canonical form.** IRIs and URIs are canonicalized when they are parsed or when a component is set: hexadecimal digits of escapes become upper-case, but escapes are not decoded and characters are not escaped, unless a setter has to escape a character that cannot appear in the component. As a consequence, `iri=?` compares the canonical forms.
- **Authority.** The text `//` yields the empty host `""`, whereas `//@` yields the empty user `""` and no host, and `//:` yields the empty host and no port. If the host text is empty and a user or a port is present, the host is `#f`; e.g. `//:80` has authority `#f #f 80`.
- **Deviations from the reference implementation.** Where the reference implementation does not behave as specified by the SRFI, the specification and the tests of the SRFI were followed. This concerns in particular the authority setters, `path-settable?`, the check of path and authority during serialization, the escaping of paths by `update-iri-path`, and relativization. The absolute path `/` (an absolute path without segments) is accepted by all setters, even if the authority is absent.
- **Character set `char-set:iri-unreserved`.** It does not contain the bidirectional formatting characters U+200E, U+200F, and U+202A to U+202E, which RFC 3987 prohibits in IRIs, even though they are in the ranges of `char-set:ucschar`. Such characters are percent-encoded by the setters and are rejected by the parser.

## API

### Predicates

**(iri? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(non-relative-iri? _obj_)**  
**(relative-iri? _obj_)**  
**(rfc3987-absolute-iri? _obj_)**  

`iri?` returns `#t` if _obj_ is an IRI, i.e. a non-relative IRI or an IRI relative reference. `non-relative-iri?` returns `#t` if _obj_ is an IRI with a scheme component. `relative-iri?` returns `#t` if _obj_ is an IRI without scheme component. `rfc3987-absolute-iri?` returns `#t` if _obj_ is a non-relative IRI without fragment, which corresponds exactly to the production "absolute-IRI" of RFC 3987. All four procedures return `#f` if _obj_ is not an IRI. In particular, a URI created with library `(srfi 275 uri)` is not an IRI.

```scheme
(define example (string->iri "/ex#IRI"))   ; <ex#IRI>
(iri? example)                  ⇒  #t
(non-relative-iri? example)     ⇒  #f
(relative-iri? example)         ⇒  #t

(map rfc3987-absolute-iri?
     (map string->iri '("http://example.org/ex?cond"
                        "http://example.org/ex#title"
                        "//example.org/ex?cond"
                        "//example.org/ex#title")))
⇒  (#t #f #f #f)
```

**(empty-iri)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a relative reference in which all components are absent and the path is empty. It is the result of parsing the empty string.

```scheme
(iri->string (empty-iri))                ⇒  ""
(iri=? (string->iri "") (empty-iri))     ⇒  #t
```

**(iri=? _iri1 iri2 iri3_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if all arguments are IRIs which have equal components. A non-relative IRI is never equal to a relative reference. Scheme, user, host, query, and fragment components are equal if they consist of exactly the same characters and escapes. Ports are compared with `eqv?`, paths with `path=?`. Returns `#f` otherwise.

```scheme
(iri=? (string->iri "/some/where/place") (string->iri "urn:/some/where/place"))
  ⇒  #f
(iri=? (string->iri "http://example.org") (string->iri "http://example.org:"))  
  ⇒  #t
(iri=? (string->iri "http://example.org?a") (string->iri "http://example.org?"))
  ⇒  #f
(iri=? (empty-iri) (string->iri "") (empty-iri))                                
  ⇒  #t
```

### Parsing and serialization

**(string->iri _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string->non-relative-iri _str_)**  
**(string->rfc3987-absolute-iri _str_)**  

`string->iri` parses string _str_ into an IRI. There is no automatic escaping: an error is raised with the offending character and its index as irritants if _str_ contains a character that is not permitted in the component in which it appears, or if percent-encoded bytes are part of an invalid sequence of UTF-8 octets. If _str_ ends prematurely, the irritants of the error are the EOF object and the length of _str_.

`string->non-relative-iri` requires the scheme component. It raises an error if _str_ is a relative reference. `string->rfc3987-absolute-iri` additionally forbids the fragment component and raises an error if _str_ contains a `#`.

```scheme
(string->iri "urn:/some/where/place")
  ⇒  <urn:/some/where/place>
(string->iri "http://example.org#fragment")
  ⇒  <http://example.org#fragment>
(string->non-relative-iri "http://example.org#fragment")
  ⇒  <http://example.org#fragment>
(string->rfc3987-absolute-iri "http://example.org#fragment")
  ⇒  ERROR
(string->iri "/some/where/place")
  ⇒  </some/where/place>
(string->non-relative-iri "/some/where/place")
  ⇒  ERROR
(string->iri "http://example.org/a b")
  ⇒  ERROR (space is illegal)
(iri->string (string->iri "http://example.org/%e2%82%ac"))
  ⇒  "http://example.org/%E2%82%AC"
```

**(get-iri _port_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(get-iri _port stop_)**  
**(get-non-relative-iri _port_)**  
**(get-non-relative-iri _port stop_)**  
**(get-rfc3987-absolute-iri _port_)**  
**(get-rfc3987-absolute-iri _port stop_)**  

These procedures parse an IRI from the textual input port _port_, in the same way as the string procedures above. They read characters until the port is exhausted or until the next character is _stop_, which can be a character or the EOF object (the default). The stop object is not consumed: the port is left positioned immediately before it. Positions in error irritants are indices relative to the characters read. These procedures are intended for streaming parsers of formats that embed IRIs.

```scheme
(define port (open-input-string "<http://example.org/some/where/place> ."))
(read-char port)                  ⇒  #\<
(iri->string (get-iri port #\>))  ⇒  "http://example.org/some/where/place"
(read-char port)                  ⇒  #\>
```

**(iri->string _iri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the text of _iri_ by concatenating its components with the necessary delimiters. Components are not decoded.

```scheme
(iri->string (string->iri "http://example.org/some/where/place"))  ⇒  "http://example.org/some/where/place"
(iri->string (string->iri "urn:/some/where/place"))                ⇒  "urn:/some/where/place"
```

### Getters

**(iri-scheme _iri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the scheme of the non-relative IRI _iri_ as a string. An error is raised if _iri_ is a relative reference.

```scheme
(iri-scheme (string->iri "urn:a/b/c"))  ⇒  "urn"
(iri-scheme (string->iri "a/b/c"))      ⇒  ERROR
```

**(iri-user _iri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(iri-host _iri_)**  
**(iri-query _iri_)**  
**(iri-fragment _iri_)**  

Return the respective component of _iri_ as a string or `#f` if the component is absent. The strings are returned as stored, i.e. escapes are not decoded and the case of the host is not changed.

**(iri-port _iri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the port of _iri_ as a non-negative integer or `#f` if there is no port.

**(iri-path _iri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the path of _iri_ as a path object, see `(srfi 275 path)`. Unlike the other components, the path is never `#f`.

```scheme
(define example (string->iri "http://example.org:80/ex#IRI"))
(iri-user example)                 ⇒  #f
(iri-host example)                 ⇒  "example.org"
(iri-port example)                 ⇒  80
(path-segments (iri-path example)) ⇒  ("ex")
(iri-query example)                ⇒  #f
(iri-fragment example)             ⇒  "IRI"
```

**(iri-authority _iri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the three authority components of _iri_ as multiple values: user, host, and port. Each of them can be `#f`.

```scheme
(iri-authority (string->iri "http://example.org:8080/some/where"))
  ⇒  #f "example.org" 8080
(iri-authority (string->iri "http://user@example.org/some/where"))
  ⇒  "user" "example.org" #f
(iri-authority (string->iri "urn:/some/where/place"))
  ⇒  #f #f #f
```

**(iri-username+password _iri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Splits the user component of _iri_ at the first colon and returns the two parts, username and password, as two values. If there is no colon, the whole user component and `#f` are returned. If the user component is absent, both values are `#f`. A colon in the form of an escape (`%3A`) does not separate username and password.

```scheme
(iri-username+password (string->iri "//foo:bar:qux@host"))
  ⇒  "foo" "bar:qux"
(iri-username+password (string->iri "//foo%3Abar:qux@host"))
  ⇒  "foo%3Abar" "qux"
(iri-username+password (string->iri "//foo@host"))
  ⇒  "foo" #f
(iri-username+password (string->iri "//@host"))
  ⇒  "" #f
(iri-username+password (string->iri "//"))
  ⇒  #f #f
```

**(iri-path-segment-ref _iri k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the path segment with index _k_ of _iri_ as a string. This is equivalent to `(path-ref (iri-path iri) k)`.

```scheme
(iri-path-segment-ref (string->iri "http://example.org/a/b/c") 1) ⇒  "b"
```

### Setters

The setters are persistent: they do not modify their argument, but return a modified copy. Strings given to setters are escaped as necessary, i.e. characters that cannot appear in the component are replaced by percent-encoded UTF-8 bytes. Escapes that are already present in a string are validated, but kept. An error is raised if they are invalid UTF-8.

The path and the authority of an IRI are not independent of each other. A setter raises an error if it would produce an IRI whose text would be parsed differently, e.g. a relative path in an IRI that has an authority. Procedures `user-settable?`, `host-settable?`, `port-settable?`, and `path-settable?` of library `(srfi 275 utils)` check these conditions without raising an error. In detail:

- If at least one authority component is present, the path must be absolute or empty (`rfc3987-path-abempty?`).
- If no authority component is present, the path must not start with an empty segment (`//`). A non-relative IRI may have an absolute path, a relative path with a non-empty first segment, or the empty path. A relative reference may in addition not have a colon in the first segment of a relative path, since this would make the first segment look like a scheme.
- The same conditions apply when the path is set, given the existing authority.

**(update-iri-scheme _iri str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of the non-relative IRI _iri_ with scheme _str_. An error is raised if _iri_ is a relative reference, if _str_ is not a string, or if _str_ is not a valid scheme, i.e. empty or not a letter followed by letters, digits, `+`, `-`, or `.`. Characters of a scheme are never escaped.

```scheme
(define example (string->iri "http://example.org:80/ex#IRI"))
(update-iri-scheme example "file")      ⇒  <file://example.org:80/ex#IRI>
(update-iri-scheme example #f)          ⇒  ERROR
(update-iri-scheme example "http%40a")  ⇒  ERROR
(update-iri-scheme (string->iri "/some/where/place") "file")
  ⇒  ERROR
```

**(update-iri-user _iri obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(update-iri-host _iri obj_)**  
**(update-iri-port _iri obj_)**  

Return a copy of _iri_ in which user, host, or port is replaced by _obj_. For user and host, _obj_ is a string or `#f`; for the port, _obj_ is a non-negative integer or `#f`. An error is raised for objects of other types and if the path of _iri_ would conflict with the new authority.

```scheme
(update-iri-host (string->iri "urn:/some/where/place") "example.org")
  ⇒  <urn://example.org/some/where/place>
(update-iri-host (string->iri "urn://example.org:80") #f)
  ⇒  <urn://:80>
(update-iri-host (string->iri "urn://example.org//some/where/place") #f)
  ⇒  ERROR
(update-iri-host (string->iri "urn:some/where/place") "example.org")
  ⇒  ERROR
(update-iri-user (string->iri "http://example.org/p") "ré su")
  ⇒  <http://ré%20su@example.org/p>
(update-iri-port (string->iri "http://example.org/p") 8080)
  ⇒  <http://example.org:8080/p>
```

**(update-iri-authority _iri user host port_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets all three authority components at once. _user_ and _host_ are strings or `#f`, _port_ is a non-negative integer or `#f`. The consistency check is applied to the result of setting all three components, and not to the intermediate IRIs.

```scheme
(iri->string (update-iri-authority (string->iri "/p") "u" "h" 1))  ⇒  "//u@h:1/p"
```

**(update-iri-query _iri obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(update-iri-fragment _iri obj_)**  

Return a copy of _iri_ in which query or fragment is replaced by _obj_, which is a string or `#f`. Characters that are not permitted are escaped. The empty string sets an empty component. There is no check against the path.

```scheme
(define example (string->iri "urn:/some/where/place"))
(update-iri-query example "query")       ⇒  <urn:/some/where/place?query>
(update-iri-query example "")            ⇒  <urn:/some/where/place?>
(update-iri-query example "a b&c#d")     ⇒  <urn:/some/where/place?a%20b&c%23d>
(update-iri-query example #f)            ⇒  <urn:/some/where/place>
(update-iri-fragment example "nonquery") ⇒  <urn:/some/where/place#nonquery>
```

**(update-iri-path _iri path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of _iri_ with path _path_, which has to be a path object, see `(srfi 275 path)`. Characters in the segments of _path_ that are not permitted in an IRI path segment, such as `?` and `#`, are escaped. An error is raised, with _iri_ and _path_ as irritants, if _path_ would conflict with the authority of _iri_.

```scheme
(update-iri-path (string->iri "urn:/some/where/place") (string->path "//some/where/place"))
  ⇒  ERROR
(update-iri-path (string->iri "urn:/some/where/place") (empty-path))          
  ⇒  <urn:>
(update-iri-path (string->iri "http://example.org/some/where/place") (string->path "//some/where/place"))
  ⇒  <http://example.org//some/where/place>
```

**(iri-path-segment-update _iri k str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of _iri_ in which the path segment with index _k_ is replaced by string _str_. Slashes in _str_ and characters that cannot appear in an IRI path segment are escaped. Index _k_ has to be a valid segment index.

```scheme
(iri->string (iri-path-segment-update (string->iri "http://example.org/a/b/c") 1 "x/y"))
  ⇒  "http://example.org/a/x%2Fy/c"
```

### Character sets

The following SRFI 14 character sets correspond to the productions of the ABNF grammar of RFC 3987. Library `(srfi 275 uri)` exports the counterparts for URIs. Sets that are the same for IRIs and URIs (`char-set:gen-delims`, `char-set:sub-delims`, `char-set:reserved`, and `char-set:scheme`) are exported by both libraries, with identical bindings.

**char-set:iri-unreserved** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[constant]</span>   
**char-set:ucschar**  
**char-set:iri-private**  
**char-set:iri**  

`char-set:iri-unreserved` is the set of characters that can be used literally anywhere in an IRI without a special meaning (production `iunreserved`): the ASCII letters and digits, `-`, `.`, `_`, `~`, and characters in `char-set:ucschar`. `char-set:ucschar` contains the characters of the Universal Character Set that are permitted in IRIs, and `char-set:iri-private` the additional private-use characters that are allowed in queries. `char-set:iri` contains all characters that can appear unescaped in an IRI.

**char-set:gen-delims** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[constant]</span>   
**char-set:sub-delims**  
**char-set:reserved**  

The reserved characters of RFC 3986 and RFC 3987: `char-set:gen-delims` contains `:/?#[]@`, `char-set:sub-delims` contains `!$&'()*+,;=`, and `char-set:reserved` is the union of both.

**char-set:scheme** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[constant]</span>   
**char-set:iri-userinfo**  
**char-set:iri-reg-name**  
**char-set:iri-segment**  
**char-set:iri-query**  
**char-set:iri-fragment**  

The characters permitted in the respective components of an IRI: `char-set:scheme` for the scheme, `char-set:iri-userinfo` for the user, `char-set:iri-reg-name` for the host, `char-set:iri-segment` for path segments, and `char-set:iri-query` and `char-set:iri-fragment` for query and fragment. Queries of IRIs may contain private-use characters, fragments cannot.
