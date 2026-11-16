# SRFI 275: URIs

Library `(srfi 275 uri)` implements Uniform Resource Identifiers (URIs) according to [RFC 3986](https://www.rfc-editor.org/rfc/rfc3986) and URI relative references. Its interface is identical to the interface of library `(srfi 275 iri)` for IRIs, with each `iri` in the name of a procedure replaced by `uri`. The only difference is the repertoire of characters: while an IRI can contain characters of the Universal Character Set directly, a URI is restricted to ASCII characters. Characters outside of this repertoire need to be percent-encoded as sequences of `%XX` escapes representing UTF-8 bytes. Setters do this automatically, and the parser raises an error if it encounters such a character.

The concepts shared by IRIs and URIs, such as the components, the canonical string form, error conventions, and implementation notes, are described on the page of library `(srfi 275 iri)`. URIs and IRIs are disjoint types: `uri?` does not hold for IRIs, and `iri?` does not hold for URIs. Procedures `iri->uri` and `uri->iri` of library `(srfi 275 normalize)` convert between them. Libraries `(srfi 275 iri)` and `(srfi 275 uri)` can both be imported without prefixes, since their procedures have different names and the character sets that both export have the same bindings.

In the examples of this page a URI object is written as `<...>`, enclosing the text it was parsed from, as in the SRFI.

```scheme
(import (srfi 275 uri))

(define example (string->uri "http://user:pw@example.org:8080/a/b?q#frag"))
(uri-scheme example)                        ⇒  "http"
(uri-host example)                          ⇒  "example.org"
(uri-port example)                          ⇒  8080
(uri->string (update-uri-host example "example.com"))
⇒  "http://user:pw@example.com:8080/a/b?q#frag"
(string->uri "http://example.org/Ῥόδος")    ⇒  ERROR (not a URI character)
(uri->string (update-uri-user example "ré su"))
⇒  "http://r%C3%A9%20su@example.org:8080/a/b?q#frag"
```

## API

### Predicates

**(uri? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(non-relative-uri? _obj_)**  
**(relative-uri? _obj_)**  
**(rfc3986-absolute-uri? _obj_)**  

`uri?` returns `#t` if _obj_ is a URI, i.e. a non-relative URI or a URI relative reference. `non-relative-uri?` returns `#t` if _obj_ is a URI with a scheme component, and `relative-uri?` if it is a URI without scheme component. `rfc3986-absolute-uri?` returns `#t` if _obj_ is a non-relative URI without fragment, which corresponds to the production "absolute-URI" of RFC 3986. All four procedures return `#f` for objects that are not URIs, including IRIs.

**(empty-uri)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a URI relative reference in which all components are absent and the path is empty, which is the result of parsing the empty string.

**(uri=? _uri1 uri2 uri3_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if all arguments are URIs with equal components, see `iri=?`.

### Parsing and serialization

**(string->uri _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string->non-relative-uri _str_)**  
**(string->rfc3986-absolute-uri _str_)**  

Parse string _str_ into a URI, a non-relative URI, or an absolute URI without fragment, respectively. Errors are raised in the same situations as for `string->iri`. Since there is no automatic escaping, characters outside of the repertoire of URIs are illegal.

**(get-uri _port_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(get-uri _port stop_)**  
**(get-non-relative-uri _port_)**  
**(get-non-relative-uri _port stop_)**  
**(get-rfc3986-absolute-uri _port_)**  
**(get-rfc3986-absolute-uri _port stop_)**  

Parse a URI from the textual input port _port_, see `get-iri`. The optional argument _stop_ is a character or the EOF object (default) which terminates the URI. It is not consumed.

**(uri->string _uri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the text of _uri_. Components are not decoded.

### Getters

**(uri-scheme _uri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(uri-user _uri_)**  
**(uri-host _uri_)**  
**(uri-port _uri_)**  
**(uri-path _uri_)**  
**(uri-query _uri_)**  
**(uri-fragment _uri_)**  

Return the respective component of _uri_. `uri-scheme` raises an error if _uri_ is a relative reference. `uri-port` returns a non-negative integer or `#f`, and `uri-path` returns a path object, see `(srfi 275 path)`. The other getters return a string or `#f` if the component is absent.

**(uri-authority _uri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns user, host, and port of _uri_ as three values.

**(uri-username+password _uri_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Splits the user component of _uri_ at the first colon and returns username and password as two values, see `iri-username+password`.

**(uri-path-segment-ref _uri k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the path segment with index _k_ of _uri_. This is equivalent to `(path-ref (uri-path uri) k)`.

### Setters

The setters of this library are persistent and perform the same checks as the setters for IRIs, see `(srfi 275 iri)`. They additionally escape all characters in the strings or paths they are given that are not allowed in a URI component, including non-ASCII characters. Escapes that are present already are validated and kept.

**(update-uri-scheme _uri str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(update-uri-user _uri obj_)**  
**(update-uri-host _uri obj_)**  
**(update-uri-port _uri obj_)**  
**(update-uri-authority _uri user host port_)**  
**(update-uri-path _uri path_)**  
**(update-uri-query _uri obj_)**  
**(update-uri-fragment _uri obj_)**  
**(uri-path-segment-update _uri k str_)**  

Return a copy of _uri_ in which the respective component is replaced.

```scheme
(define example (string->uri "http://example.org/p"))
(update-uri-user example "ré su")
  ⇒  <http://r%C3%A9%20su@example.org/p>
(update-uri-fragment example "é#")
  ⇒  <http://example.org/p#%C3%A9%23>
(update-uri-path example (string->path "/x?y#z/é"))
  ⇒  <http://example.org/x%3Fy%23z/%C3%A9>
(uri-path-segment-update (string->uri "http://a/b/c") 0 "é/x")
  ⇒  <http://a/%C3%A9%2Fx/c>
```

### Character sets

The following SRFI 14 character sets correspond to the productions of the ABNF grammar of RFC 3986.

**char-set:uri-unreserved** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[constant]</span>   
**char-set:uri**  

`char-set:uri-unreserved` consists of the ASCII letters and digits and the characters `-`, `.`, `_`, and `~`. `char-set:uri` contains all characters that can appear unescaped in a URI, i.e. the unreserved and the reserved characters.

**char-set:gen-delims** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[constant]</span>   
**char-set:sub-delims**  
**char-set:reserved**  
**char-set:scheme**  

The reserved characters `:/?#[]@` (`char-set:gen-delims`) and `!$&'()*+,;=` (`char-set:sub-delims`), their union (`char-set:reserved`), and the characters permitted in a scheme (`char-set:scheme`). The IRI library exports the same bindings.

**char-set:uri-userinfo** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[constant]</span>   
**char-set:uri-reg-name**  
**char-set:uri-segment**  
**char-set:uri-query**  
**char-set:uri-fragment**  

The characters permitted in the user component (`char-set:uri-userinfo`), in the host (`char-set:uri-reg-name`), in path segments (`char-set:uri-segment`), and in query and fragment. Unlike for IRIs, query and fragment of URIs have the same repertoire.
