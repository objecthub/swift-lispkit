# LispKit JSON

Library `(lispkit json)` defines an API for representing, querying, and manipulating generic JSON values. The framework includes:

  - A representation for mutable and immutable JSON values as defined by [RFC 8259](https://datatracker.ietf.org/doc/html/rfc8259/)
  - Functionality for creating and manipulating JSON values including support for reading and writing JSON data
  - An implementation of _JSON Pointer_ as defined by [RFC 6901](https://datatracker.ietf.org/doc/html/rfc6901/) for locating values within a JSON document
  - An implementation of _JSON Path_ as defined by [RFC 9535](https://datatracker.ietf.org/doc/html/rfc9535/) for querying JSON data
  - An implementation of _JSON Patch_ as defined by [RFC 6902](https://datatracker.ietf.org/doc/html/rfc6902/) for mutating JSON data
  - An implementation of _JSON Merge Patch_ as defined by [RFC 7396](https://datatracker.ietf.org/doc/html/rfc7396/) for merging JSON data with JSON patches

## JSON values

Library `(lispkit json)` provides an abstract data type `json` encapsulating potentially very large JSON values. A JSON value has one of the following six types:

  - **null**: the "empty" value
  - **boolean**: representing `#t` and `#f`
  - **number**: either an integer (fixnum) or a floating-point number (flonum)
  - **string**: a sequence of unicode characters
  - **array**: a fixed length sequence of JSON values
  - **object**: a collection of name/value pairs

Library `(lispkit json)` implements a rich API for creating, accessing, and transforming JSON values. There are both mutable and immutable JSON values.

**json-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `json` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all JSON values.

**(json? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(json? _obj strict?_)**  

Returns `#t` if _obj_ is a mutable or immutable JSON value; `#f` otherwise. If argument _strict?_ is provided and set to true, then `#t` is returned only if _obj_ is an immutable JSON value. For checking if a given object is a mutable JSON value, procedure `mutable-json?` can be used.

**(json-null? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ corresponds to the JSON _null_ value; `#f` otherwise.

**(json-boolean? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a boolean JSON value; `#f` otherwise.

**(json-number? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a numeric JSON value, i.e. an integer or floating-point number. Otherwise, `#f` is returned.

**(json-string? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a JSON string value; `#f` otherwise.

**(json-array? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a JSON array; `#f` otherwise.

**(json-object? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a JSON object; `#f` otherwise.

**(json)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(json _x_)**  
**(json _x ..._)**  

Procedure `json` maps Scheme data structures to _immutable JSON values_. If no argument is provided, then the JSON null value is returned. If one argument _x_ is provided, then _x_ is mapped to a JSON value following the rules stated below. If more than one arguments _x ..._ is provided, then a JSON array is returned whose elements have been created by mapping the corresponding argument.

The following mapping rules are being used for regular Scheme values _x_:

  - The symbol `null` is mapped to the JSON null value
  - `#f` and `#t` are mapped to the corresponding JSON boolean values
  - Fixnum values are mapped to corresponding JSON integer values
  - Flonum values are mapped to corresponding JSON floating-point values
  - Symbols (other than `null`) are mapped to JSON string values
  - Vectors and growable vectors are mapped to JSON arrays
  - Association lists are mapped to JSON objects; association lists need to have the form `(("key1" . value1)("key2" . value2) ...)` or `((key1 . value1)(key2 . value2) ...)`.
  - Instances of record types are mapped to JSON objects where each record field and value correspond to a key and mapped JSON value
  - Hashtables are mapped to a corresponding JSON object if all keys can be mapped to JSON object keys (symbols and strings) and values can be mapped according to these rules
  - JSON values map to itself
  - Mutable JSON values map to a corresponding immutable JSON value
  - For all other Scheme values, an error is signaled

The inverse mapping is implemented by procedure `json->value`.

**(json-object (_name value_) ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Creates a JSON object with the given name/value pairs. _name_ should be a symbol and _value_ is an expression that evaluates to a value that can be converted to JSON. This is a syntax form that expands to a call to `json` with an association list.

```scheme
(json-object (name "John") (age 30) (city "New York"))
⇒  #json-object with members: name, age, city
```

**(make-json-array _len_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-json-array _len default_)**  

Returns a new immutable JSON array of length _len_. If JSON value _default_ is provided, then it is used as the default element value of the new array. Otherwise, all elements of the new JSON array are set to the JSON null value.

**(json=? _json ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if all JSON values _json ..._ are structurally equivalent; otherwise `#f` is returned.

**(json-refinement? _a b_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if JSON value _a_ is a _refinement_ of JSON value _b_; otherwise, `#f` is returned. _a_ is a _refinement_ of _b_ if

  1. Both _a_ and _b_ are JSON values of the same type,
  2. If _a_ and _b_ are arrays, they have the same length _n_ and _a<sub>i</sub>_ is a refinement of _b<sub>i</sub>_ holds for every i ∈ \[0; _n_\[,
  3. If _a_ and _b_ are objects, for every member _m_ of _b_ with value _b<sub>m</sub>_, there is a member _m_ of _a_ with value _a<sub>m</sub>_ such that _a<sub>m</sub>_ is a refinement of _b<sub>m</sub>_,
  4. For all other types, _a_ and _b_ are the same.

This relationship intuitively models that whenever it is possible to read a value at a given location from _b_, it is also possible to read a value at the same location from _a_ and the value that is read for _a_ is a refinement of the value read from _b_. The following example showcases this relationship:

```scheme
(define a
  (string->json "{ \"a\": [1, { \"b\": 2 }], \"c\": { \"d\": [{}] }}"))
(define b
  (string->json "{ \"a\": [1, { \"b\": 2, \"e\": 4 }], \"c\": { \"d\": [{\"f\": 5}] }}"))
(json-refinement? a b)  ⇒  #f
(json-refinement? b a)  ⇒  #t
```

**(string-\>json _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a JSON value for the data structure represented in string _str_.

**(bytevector-\>json _bvec_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(bytevector-\>json _bvec start_)**  
**(bytevector-\>json _bvec start end_)**  

Decodes the given bytevector _bvec_ between _start_ and _end_ and returns a JSON value for the encoded value. If is an error if _bvec_ between _start_ and _end_ does not represent a JSON value encoded in a UTF8-encoded string. If _end_ is not provided, it is assumed to be the length of _bvec_. If _start_ is not provided, it is assumed to be 0.

**(cbor-\>json _bvec_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(cbor-\>json _bvec start_)**  
**(cbor-\>json _bvec start end_)**  

Decodes a CBOR (Concise Binary Object Representation) encoded value from bytevector _bvec_ between _start_ and _end_ and returns the corresponding JSON value. CBOR is a binary data format defined in RFC 8949. If _end_ is not provided, it is assumed to be the length of _bvec_. If _start_ is not provided, it is assumed to be 0.

**(load-json _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Loads a text file at _path_, parses its content as JSON and returns it as a JSON value.

**(json-members _json_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

If _json_ represents a JSON object, then `json-members` returns a list of all members of this object. Each member is represented as a symbol. For all other JSON values, `json-members` returns an empty list.

**(json-member? _obj member_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the JSON value _obj_ is an object and has a member with the given _member_ name (a string or symbol); `#f` otherwise.

**(json-member _json member_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(json-member _json member default_)**  
**(json-member _json members_)**  
**(json-member _json members default_)**  

Returns the value of the given _member_ (a string or symbol) in JSON object _json_. If _member_ is a list of member names, follows the path through nested JSON objects. If the member is not found, returns _default_, or `#f` if _default_ is not provided.

```scheme
(define x (string->json "{ \"a\": { \"b\": 42 } }"))
(json-member x 'a)      ⇒  ((b . 42))
(json-member x '(a b))  ⇒  42
(json-member x "c" 99)  ⇒  99
```

**(json-children _json_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns all the direct children of _json_ as a list. For null, boolean, numeric and string values, an empty list is returned. For JSON arrays, the array itself is returned. For JSON objects, the values of the object (without their corresponding keys) are returned in an undefined order.

**(json-children-count _json_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

For JSON objects, `json-children-count` returns the number of members. For JSON arrays, `json-children-count` returns the length of the array. For all other JSON types, zero is returned.

**(json-ref _json ref ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies JSON references _ref ..._ sequentially to _json_ and returns the result of the last application. `(json-ref x r1 ... rn)` is equivalent to `(json-ref ... (json-ref (json-ref x r1) r2) ... rn)`.

**(json-replace _json ref1 v1 ref2 v2..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Within _json_, replaces the value at JSON reference _ref1_ with _v1_ followed by replacing the value at JSON reference _ref2_ with _v2_ etc. This procedure does not mutate _json_ returning a new JSON value with the changes applied.

**(json-replace-all _json updates_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies update list _updates_ to _json_. An update list is a list of pairs consisting of a JSON reference and a JSON value. The values replace the content at the given reference. They are applied in order. This procedure does not mutate _json_ returning a new JSON value with the changes applied.

**(json-\>value _json_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Converts a JSON value into corresponding Scheme values. This procedure implements the inverse mapping of procedure `json`, i.e. null values are mapped to the symbol `null`, boolean values are mapped to `#t` and `#f`, numbers are mapped to fixnum and flonum values, strings are mapped to regular Scheme strings, arrays are mapped to immutable vectors, and objects are mapped to association lists with symbols representing the member names.

**(json->string _json_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(json->string _json pretty?_)**  
**(json->string _json pretty? sort?_)**  
**(json->string _json pretty? sort? slash?_)**  

Returns a string representation of _json_. The output is pretty-printed if _pretty?_ is provided and set to true. If _sort?_ is provided and set to true, the members of an object are printed in sorted order allowing for a deterministic output. If _slash?_ is provided and set to true, slashes get escaped in strings, allowing outputted JSON to be safely embedded within HTML/XML. 

**(json->bytevector _json_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(json->bytevector _json pretty?_)**  
**(json->bytevector _json pretty? sort?_)**  
**(json->bytevector _json pretty? sort? slash?_)**  

Returns a bytevector of a UTF8-encoded string representation of _json_. The output options _pretty?_, _sort?_, and _slash?_ correspond to the options of procedure `json->string`.

**(json-\>cbor _json_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a bytevector containing the CBOR (Concise Binary Object Representation) encoding of _json_. CBOR is a binary data format defined in RFC 8949 that provides a compact representation of JSON-like data structures.

**(json-for-each-element _f arr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies procedure _f_ to every element of JSON array _arr_. _f_ is a procedure accepting one JSON value as argument.

**(json-for-each-member _f obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies procedure _f_ to every member of JSON object _obj_. _f_ is a procedure accepting two arguments: a symbol representing the member name and a JSON value representing the value of the object member.

## Mutable JSON values

**mutable-json-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `json` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all JSON values.

**(mutable-json? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a mutable JSON value; `#f` otherwise.

**(mutable-json _expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(mutable-json _expr force?_)**  

Procedure `mutable-json` maps Scheme data structures _expr_ to _mutable JSON values_ using the same mapping rules as `json`. This procedure can also be used to turn an immutable JSON value into a mutable JSON value if _expr_ is an immutable JSON value already. The value only gets copied if there are other references to _expr_. If _expr_ is already a mutable JSON value, then that value is returned by `mutable-json` unless argument `force?` is provided and set to true. In this case, even if _expr_ is a mutable JSON value already, a copy is created and returned.

**(json-set! _json ref value_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Within _json_, sets the value at JSON reference _ref_ to _value_, mutating _json_ in place. If _json_ is not a mutable JSON value, then an error is signaled.

**(json-append! _json ref x ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

This procedure appends the JSON values _x ..._ to the JSON array at the JSON reference _ref_ of mutable JSON value _json_. If _ref_ does not refer to an array, no change is made to _json_ and no error is signaled.

**(json-insert! _json ref index x ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

This procedure inserts the JSON values _x ..._ in the JSON array at the JSON reference _ref_ of mutable JSON value _json_. If _ref_ does not refer to an array, no change is made to _json_ and no error is signaled.

**(json-remove! _json ref ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the values at the JSON references _ref ..._ from mutable JSON value _json_. If a JSON reference does not refer to location where a value can be removed, no change is made and no error is signaled.

## JSON references

### Supported formalisms

Library `(lispkit json)` supports multiple abstractions for referring to values within a JSON document. These abstractions are called _JSON references_. The most established formalism for referring to a location within a JSON document is [JSON Pointer](https://datatracker.ietf.org/doc/html/rfc6901/). Here is the complete list of supported _JSON references_:

  - Strings containing a valid **_JSON Pointer_** reference as defined by [RFC 6901](https://datatracker.ietf.org/doc/html/rfc6901/); e.g. `"/store/book/0/title"` is a valid JSON reference.
  - Strings containing a valid **_JSON Location_** reference. JSON location syntax is based on how values are uniquely identified in [JSON Path](https://datatracker.ietf.org/doc/html/rfc9535/).
  - A fixnum value _i_ refers to the _i_-th element of a JSON array if _i >= 0_. If _i < 0_, then this refers to the _n+i_-th element assuming _n_ is the length of the array.
  - A symbol _s_ refers to member _s_ of a JSON object.
  - The empty list `()` refers to the root of a JSON document.
  - A list of symbols and integers refers to a sequence of member and array index selections; e.g. `(store book 0 title)`.

### JSON locations

It is recommended to use _JSON locations_ were possible since their semantics is independent of the JSON value they are applied to (unlike _JSON Pointer_ references which have an ambiguous interpretation for their numeric segments).

A _JSON location_ is a path to an element in a JSON structure. Each element of the path is called a _segment_. The JSON location syntax supports two different forms to express such sequences of segments. Each sequence starts with `$` indicating the "root" of a JSON document. The most common form for expressing the segment sequence is using the dot notation:

```
$.store.book[0].title
```  

While accessing an array index is always done using bracket notation, it is possible to also express the access of members of an object using bracket notation as well:

```
$['store']['book'][0]['title']
```

Is is also possible to mix the dot and bracket notation. Dots are only used before property names and never together with brackets:

```
$['store'].book[-1].title
```

The previous example also shows the usage of negative indices, which are interpreted as offsets from the end of arrays with -1 referring to the last element.

### API

**(json-location? _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if string _str_ contains a valid JSON location specification; `#f` otherwise.

**(json-pointer? _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if string _str_ contains a valid JSON pointer specification; `#f` otherwise.

**(json-reference? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid JSON reference; i.e. it is either:

  - A string containing a valid _JSON Pointer_ reference,
  - A string containing a valid _JSON Location_ reference,
  - A fixnum value _i_ referring to the _i_-th element of a JSON array if _i >= 0_, or to the _n+i_-th element if _i < 0_ and _n_ being the length of the array,
  - A symbol _s_ referring to member _s_ of a JSON object,
  - A list of symbols and integers referring to a sequence of member and array index selections.

**(json-self-reference? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid JSON reference that refers to the root of a JSON document; `#f` otherwise.

**(json-location _ref_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a string with a JSON location representation of the JSON reference _ref_.

**(json-pointer _ref_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a string with a JSON pointer representation of the JSON reference _ref_.

```scheme
(json-pointer "$.store.book[0].title")
⇒  "/store/book/0/title"
```

**(json-reference-segments _ref_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of symbols and integers representing the sequence of segments for the given JSON reference _ref_.

```scheme
(json-reference-segments "$.store.book[0].title")
⇒  ("store" "book" 0 "title")
(json-reference-segments "/store/book/0/title")
⇒  ("store" "book" 0 "title")
(json-reference-segments '(a -1 c 2))
⇒  ("a" -1 "c" 2)
```

## JSON Path

The full _JSON Path_ standard as defined by [RFC 9535](https://datatracker.ietf.org/doc/html/rfc9535/) is supported by library `(lispkit json)`. JSON Path queries are simply represented as strings. They can be applied to JSON values with procedures `json-query`, `json-query-results`, and `json-query-locations`. To illustrate the usage of JSON Path queries, the following JSON value is being defined:

```scheme
(define jval (string->json (string-append
  "{ \"store\": {\n"
  "   \"book\": [\n"
  "     { \"category\": \"reference\",\n"
  "       \"author\": \"Nigel Rees\","
  "       \"title\": \"Sayings of the Century\",\n"
  "       \"price\": 8.95 },\n"
  "     { \"category\": \"fiction\",\n"
  "       \"author\": \"Evelyn Waugh\",\n"
  "       \"title\": \"Sword of Honour\",\n"
  "       \"price\": 12.99 },\n"
  "     { \"category\": \"fiction\",\n"
  "       \"author\": \"Herman Melville\",\n"
  "       \"title\": \"Moby Dick\",\n"
  "       \"isbn\": \"0-553-21311-3\",\n"
  "       \"price\": 8.99 },\n"
  "     { \"category\": \"fiction\",\n"
  "       \"author\": \"J. R. R. Tolkien\",\n"
  "       \"title\": \"The Lord of the Rings\",\n"
  "       \"isbn\": \"0-395-19395-8\",\n"
  "       \"price\": 22.99 }\n"
  "   ],\n"
  "     \"bicycle\": {\n"
  "     \"color\": \"red\",\n"
  "     \"price\": 399\n"
  "   }\n"
  " }\n"
  "}")))
```

Now a JSON Path query `$.store.book[?@.price < 10].title` can be applied to `jval` via procedure `json-query` to extract pairs of matching JSON values and the corresponding JSON references:

```scheme
(json-query jval "$.store.book[?@.price < 10].title")
⇒  ((#<json "Moby Dick"> "store" "book" 2 "title")
    (#<json "Sayings of the Century"> "store" "book" 0 "title"))
```

**(json-path? _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(json-path? _str strict?_)**  

Returns `#t` if string _str_ constitutes a valid JSON query string; `#f` otherwise. If argument `strict?` is provided and set to false, the syntax and semantics of the JSON Path string is slighly relaxed. For instance, non-singular queries are supported in query filters.

**(json-path-singular? _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(json-path-singular? _str strict?_)**  

Returns `#t` if string _str_ constitutes a valid singular JSON query string; `#f` otherwise. A JSON query is singular if it refers to exactly one location in any JSON document. If argument `strict?` is provided and set to false, the syntax and semantics of the JSON Path string is slighly relaxed. For instance, non-singular queries are supported in query filters.

**(json-query _json query_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies JSON Path _query_ to _json_ returning the values matching the query together with the corresponding locations of the matching values. `json-query` returns a list of pairs of JSON values and JSON locations.

**(json-query-results _json query_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies JSON Path _query_ to _json_ returning the values matching the query in a list.

**(json-query-locations _json query_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies JSON Path _query_ to _json_ returning the locations of matching values in a list.


## JSON Patch

_JSON Patch_ defines a JSON document structure for expressing a sequence of operations to apply to a JSON document. Each operation mutates parts of the JSON document. The supported operations specified by [RFC 6902](https://datatracker.ietf.org/doc/html/rfc6902/) are represented by the following 6 shapes of lists:

  - **(add _ref json_)**: Add _json_ to the JSON value the JSON pointer _ref_ is referring to
  - **(remove _ref_)**:  Remove the JSON value at the location the JSON pointer _ref_ is referring to
  - **(replace _ref json_)**: Replace the value at the location the JSON pointer _ref_ is referring to with `json`
  - **(move _from to_)**: Move the value at the location at which the JSON pointer `to` is referring to with the value at `from`. This is equivalent to first removing the value at `from` and then adding it to `path`.
  - **(copy _from to_)**: Copy the value at the location at which the JSON pointer `to` is referring to with the value at `from`. This is equivalent to first looking up the value at `from` and then adding it to `path`.
  - **(test _ref json_)**: Compares value at `ref` with `json` and fails if the two are different.

From lists of operations it is possible to construct JSON patch objects, which are mutable containers for sequences of operations. They are created with procedure `json-patch`:

```scheme
(define jp (json-patch
  `((test "/a/b/c" ,(json "foo"))
    (remove "/a/b/c")
    (add "/a/b/c" ,(json "foo" "bar"))
    (replace "/a/b/c" ,(json 42))
    (move "/a/b/d" "/a/b/c")
    (copy "/a/b/e" "/a/b/d"))))
```

A more conventional approach would be to define the JSON patch operations in JSON directly and using again procedure `json-patch`:

```scheme
(define jp2 (json-patch (string->json (string-append
  "["
  " { \"op\": \"test\", \"path\": \"/a/b/c\", \"value\": \"foo\" },"
  " { \"op\": \"remove\", \"path\": \"/a/b/c\" },"
  " { \"op\": \"add\", \"path\": \"/a/b/c\", \"value\": [ \"foo\", \"bar\" ] },"
  " { \"op\": \"replace\", \"path\": \"/a/b/c\", \"value\": 42 },"
  " { \"op\": \"move\", \"from\": \"/a/b/c\", \"path\": \"/a/b/d\" },"
  " { \"op\": \"copy\", \"from\": \"/a/b/d\", \"path\": \"/a/b/e\" }"
  "]"
))))
(json-patch=? jp jp2)  ⇒  #t
```

JSON patch objects can be applied to mutable JSON values with procedure `json-apply!`.

**json-patch-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `json-patch` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all JSON patch objects.

**(json-patch? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a JSON patch object; `#f` otherwise.

**(json-patch)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(json-patch _expr_)**  

Returns a new JSON patch object. JSON patch objects are mutable containers for lists of operations. If no argument is provided to `json-patch`, then an empty JSON patch object is returned which can be extended via procedure `json-patch-append!`. If _expr_ is provided, it is either an already existing JSON patch object and a copy is returned, or a JSON value representing the JSON patch operation list according to [RFC 6902](https://datatracker.ietf.org/doc/html/rfc6902/), or it is a list of operations. Each operation has one of the following 6 shapes:

  - **(add _ref json_)**: Add _json_ to the JSON value at _ref_
  - **(remove _ref_)**:  Remove the JSON value at _ref_
  - **(replace _ref json_)**: Replace the value at _ref_ with `json`
  - **(move _from to_)**: Move the value at `from` to `to`
  - **(copy _from to_)**: Copy the value at `from` to `to`
  - **(test _ref json_)**: Compares value at `ref` with `json` and fails if the two are different.

JSON pointer is used for specifying references to values within a larger JSON document.

**(json-patch-clear! _patch_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes all operations from the JSON patch object _patch_.

**(json-patch-append! _patch oper ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Appends operations _oper_ to the JSON patch object _patch_. Each operation has one of the following 6 shapes: (add _ref json_), (remove _ref_), (replace _ref json_), (move _from to_), (copy _from to_), and (test _ref json_).

**(json-patch=? _patch ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Compares the JSON patch objects _patch ..._ and return `#t` if all patch objects are equivalent; otherwise return `#f`.

**(json-patch->list _patch_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the list of operations for JSON patch object _patch_.

**(json-patch->json _patch_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a JSON value representing the operations of the JSON patch object _patch_ as defined by [RFC 7396](https://datatracker.ietf.org/doc/html/rfc7396/).

**(json-apply! _json patch_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies a JSON patch object _patch_ to mutable JSON value _json_ using application rules as defined by [RFC 7396](https://datatracker.ietf.org/doc/html/rfc7396/).


## Merging JSON values

**(json-merge _json other_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Merges the JSON value _json_ with the JSON value _other_ such that the result _res_ of the merge is the "smallest" JSON value that is a _refinement_ of both _json_ and _other_; i.e. both `(json-refinement? res json)` and `(json-refinement? res other)` hold. If such a merged value does not exist, then `json-merge` will return `#f`.

This approach is also called a _symmetrical merge_. Intuitively, it combines two JSON values by adding all non-existing values to the merged value and merging overlapping values or failing whenever a symmetrical merge is not possible.

**(json-override _json other_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Merges the JSON value _json_ with the JSON value _other_ using merge semantics which let _other_ override values of _json_ whenever merging as implemented by `json-merge` would fail otherwise. As opposed to procedure `json-merge`, combining arrays does not require the arrays to be of the same length. The resulting array has always the length of the longest of the two arrays and individual elements are combined using `json-override` whenever two elements are available.

**(json-merge-patch _json patch_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Merges the JSON value _json_ with the JSON value _patch_ using the rules defined by [RFC 7396](https://datatracker.ietf.org/doc/html/rfc7396/). The merged JSON value is returned.

The _patch_ value describes changes to be made to _json_ using a syntax that closely mimics the document being modified. Recipients of a merge _patch_ value determine the exact set of changes being requested by comparing the content of the provided patch against the current value _json_. If the provided _patch_ value contains members that do not appear within _json_, those members are added. If _json_ does contain the member, the value is replaced. Null values in _patch_ are given special meaning to indicate the removal of existing values in the target.
