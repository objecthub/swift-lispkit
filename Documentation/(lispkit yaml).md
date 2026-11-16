# LispKit YAML

Library `(lispkit yaml)` defines an API for representing, querying, and manipulating generic YAML values. It is based on the Swift framework [YamlKit](https://github.com/objecthub/swift-yamlkit), a YAML 1.2.2 parser and emitter. The library includes:

  - A representation for mutable and immutable YAML values as defined by the [YAML 1.2.2 specification](https://yaml.org/spec/1.2.2/)
  - Functionality for creating, converting, and transforming YAML values, including support for reading and writing YAML text, YAML files, and streams with multiple YAML documents
  - Support for the three standard schemas of YAML (_core_, _JSON_, and _failsafe_), and for custom schemas defined by Scheme procedures
  - Support for anchors, aliases, merge keys, tags, scalar styles, and source positions
  - A streaming API based on YAML events for parsing and writing very large YAML streams or for handling YAML at a lower level of abstraction

## YAML values

Library `(lispkit yaml)` provides an abstract data type `yaml` encapsulating potentially very large YAML values. A YAML value is a tree of _nodes_. Every node is of one of the following three kinds:

  - **scalar**: a text, which is interpreted according to the node's _tag_ as a null value, a boolean, an integer, a floating-point number, or a string
  - **sequence**: an ordered list of nodes
  - **mapping**: an ordered collection of key/value pairs of nodes; in contrast to JSON, keys can be arbitrary YAML values and not just strings

Besides its content, every node carries a number of properties that are relevant for reading and writing YAML text:

  - A _tag_ determines the type of a node. A tag is represented as a string. Tags of the YAML tag repository are written in shorthand notation, e.g. `"!!str"`, `"!!int"`, `"!!float"`, `"!!bool"`, `"!!null"`, `"!!seq"`, or `"!!map"`. Local tags start with a single exclamation mark, e.g. `"!color"`, and tags from other repositories are written as `"!<tag:example.com,2000:point>"`. When a tag is provided as an argument, a full URI such as `"tag:yaml.org,2002:str"` can be used as well.
  - A _style_ determines how a node is written. Scalars have one of the styles `plain`, `single-quoted`, `double-quoted`, `literal`, or `folded`. Sequences and mappings have either style `block` or style `flow`. Styles are represented as symbols. The value `#f` stands for "unspecified"; the serializer then chooses a suitable style.
  - An _anchor_ is a name (a string) that other parts of a YAML document can refer to via an alias.
  - A _source position_ (called _mark_) locates a node within the parsed YAML text. It is represented as a list `(line column offset)`, where line and column start at 1 and offset is the number of characters preceding the node, starting at 0. Nodes that were not created by parsing YAML text have no mark.

Library `(lispkit yaml)` implements a rich API for creating, accessing, and transforming YAML values. There are both mutable and immutable YAML values.

**yaml-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `yaml` type. The `type-of` procedure of library `(lispkit type)` returns a list containing this symbol for all immutable YAML values.

**mutable-yaml-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `mutable-yaml` type. The `type-of` procedure of library `(lispkit type)` returns a list containing this symbol for all mutable YAML values.

**(yaml? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(yaml? _obj strict?_)**  

Returns `#t` if _obj_ is a mutable or immutable YAML value; `#f` otherwise. If argument _strict?_ is provided and set to true, then `#t` is returned only if _obj_ is an immutable YAML value. For checking if a given object is a mutable YAML value, procedure `mutable-yaml?` can be used.

**(yaml-null? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a YAML scalar with the null value; `#f` otherwise.

**(yaml-boolean? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a boolean YAML scalar; `#f` otherwise.

**(yaml-number? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a numeric YAML scalar, i.e. an integer or floating-point number. Otherwise, `#f` is returned.

**(yaml-string? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a YAML scalar with the string type; `#f` otherwise.

**(yaml-scalar? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a YAML scalar of any type, including scalars with local tags; `#f` otherwise. Null, boolean, number, and string values are all scalars.

**(yaml-sequence? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a YAML sequence; `#f` otherwise.

**(yaml-mapping? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a YAML mapping; `#f` otherwise.

**(yaml-member? _obj key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a YAML mapping containing an entry for _key_. The key is specified like a mapping key in a YAML reference (see below), i.e. it is either a symbol, a string, a fixnum, or a YAML value. Returns `#f` if _obj_ is not a mapping or if there is no such entry.

**(yaml=? _yaml1 yaml2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if all arguments are YAML values that are equal. Two scalars are equal if they have the same tag and denote the same value (e.g. `0x10` and `16` are equal integers). Two sequences are equal if they have equal elements in the same order, and two mappings are equal if they have the same entries, regardless of the order of the entries. Styles, anchors, and source positions are not taken into account.

## Creating and converting YAML values

**(yaml)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(yaml _x_)**  
**(yaml _x ..._)**  

Procedure `yaml` maps Scheme data structures to _immutable YAML values_. Without an argument, the YAML null value is returned. With one argument _x_, the value is converted to a YAML value. With more than one argument, a YAML sequence consisting of the converted arguments is returned. The following rules define how Scheme values are converted:

| Scheme value | YAML value |
|---|---|
| symbol `null` | null (tag `!!null`) |
| booleans | boolean (tag `!!bool`) |
| fixnums and bignums | integer (tag `!!int`) |
| flonums | floating-point number (tag `!!float`); `+inf.0`, `-inf.0`, and `+nan.0` are written as `.inf`, `-.inf`, and `.nan` |
| strings and other symbols | string (tag `!!str`) |
| vectors and growable vectors | sequence (tag `!!seq`) with converted elements |
| association lists, hash tables, and records | mapping (tag `!!map`) with converted values; keys that are strings or symbols are converted to strings, all other keys are converted to YAML values |
| the empty list | empty mapping |
| YAML values | the YAML value itself; mutable YAML values are converted to an immutable snapshot |

Entries of association lists with the void value (the result of `(if #f #f)`) are skipped. Mappings created from association lists keep the order of the list; the order of the entries of mappings created from hash tables is arbitrary. An error is signaled if a value cannot be converted, e.g. when it is a procedure or a list that is not an association list.

```scheme
(yaml->string (yaml '((name . "demo") (ports . #(80 443)) (debug . #f))))
⇒  "name: demo\nports:\n  - 80\n  - 443\ndebug: false\n"
```

**(yaml->value _yaml_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(yaml->value _yaml default_)**  

Maps the YAML value _yaml_ back to Scheme data. This is the inverse of procedure `yaml` in the following sense:

  - null values are mapped to the symbol `null`, booleans to booleans, integers to fixnums or bignums, and floating-point numbers to flonums; integers can be written in decimal, octal (`0o17`), or hexadecimal notation (`0xFF`)
  - sequences are mapped to immutable vectors
  - mappings are mapped to association lists; keys that are strings are mapped to symbols, other keys are converted recursively
  - all other scalars, i.e. strings and scalars with tags that are not part of the YAML core schema, are mapped to their text; for instance `!!binary aGVsbG8=` is mapped to the string `"aGVsbG8="`

This conversion is lossy: tags, styles, anchors, and source positions get lost. Use the access procedures described below if this information is needed.

If _yaml_ is `#f`, then _default_ is returned if it is provided. This makes it easy to handle missing nodes, as `yaml-ref` returns `#f` if there is no node at a given reference.

```scheme
(yaml->value (string->yaml "{a: 1, b: [x, 2.5, ~], c: {d: yes}}"))
⇒  ((a . 1) (b . #("x" 2.5 null)) (c (d . "yes")))
(yaml->value (yaml-ref (string->yaml "a: 1") 'b) 'missing)
⇒  missing
```

**(yaml-with-tag _yaml tag_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of YAML value _yaml_ with the tag of the outermost node set to _tag_, which is a string. Tag `"!!int"` makes a scalar an integer, `"!color"` defines a local tag.

```scheme
(yaml->string (yaml-with-tag (yaml "#ff8800") "!color"))
⇒  "!color '#ff8800'\n"
```

**(yaml-with-style _yaml style_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of YAML value _yaml_ with the style of the outermost node set to _style_. For scalars, _style_ is one of the symbols `plain`, `single-quoted`, `double-quoted`, `literal`, `folded`, and for sequences and mappings, it is one of `block` and `flow`. If _style_ is `#f`, the style is left to the serializer. A style is a request: if the serializer cannot use the requested style for a given value, it picks another one. For instance, the plain style cannot be used for the string `" x"`, which is written as `' x'` instead.

```scheme
(yaml->string (yaml-with-style (yaml #(1 2 3)) 'flow))
⇒  "[1, 2, 3]\n"
(yaml->string (yaml-with-style (yaml "a\nb\n") 'literal))
⇒  "|\n  a\n  b\n"
```

**(yaml-with-anchor _yaml anchor_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of YAML value _yaml_ with the anchor of the outermost node set to the string _anchor_. If _anchor_ is `#f`, an existing anchor is removed. When YAML text is written, the first node with a given anchor is written with this anchor. Any further node with the same anchor is written as an alias referring to it.

```scheme
(let ((point (yaml-with-anchor (yaml '((x . 1) (y . 2))) "origin")))
  (yaml->string (yaml (list (cons 'a point) (cons 'b point)))))
⇒  "a: &origin\n  x: 1\n  y: 2\nb: *origin\n"
```

## Accessing YAML values

**(yaml-tag _yaml_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the tag of the outermost node of _yaml_ as a string. Values parsed from plain scalars are tagged according to the schema, for instance with `"!!int"`. Explicit tags are returned in their shorthand notation if possible, for instance `"!!str"`, or `"!color"`.

**(yaml-style _yaml_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the style of the outermost node of _yaml_ as a symbol, or `#f` if the node has no specified style. Values that were not created by parsing text usually have no specified style.

**(yaml-anchor _yaml_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the anchor of the outermost node of _yaml_ as a string, or `#f` if the node has no anchor.

**(yaml-mark _yaml_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the source position of the outermost node of _yaml_ as a list `(line column offset)`, or `#f` if the node was not created by parsing YAML text. The position of a mapping or sequence is the position of its first character.

```scheme
(let ((doc (string->yaml "a:\n  b: 1")))
  (list (yaml-mark doc) (yaml-mark (yaml-ref doc 'a 'b))))
⇒  ((1 1 0) (2 6 8))
```

**(yaml-scalar-text _yaml_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the text of YAML scalar _yaml_ as a string. For scalars that were created from Scheme values, this is the canonical text, e.g. `"null"`, `"true"`, or `"2.5"`. For parsed scalars, this is the text as written, without quotes, e.g. `"~"` or `"0x10"`. Returns `#f` if _yaml_ is a sequence or mapping.

**(yaml-children _yaml_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the children of _yaml_ in the order in which they appear. For a sequence, these are its elements; for a mapping, these are its values. Scalars have no children, so the empty list is returned.

**(yaml-children-count _yaml_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of children of _yaml_, i.e. the number of elements of a sequence or the number of entries of a mapping. Returns 0 for scalars.

**(yaml-keys _yaml_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the keys of mapping _yaml_ as YAML values, in the order in which they appear. If _yaml_ is not a mapping, the empty list is returned.

**(yaml-entries _yaml_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the entries of mapping _yaml_. Each entry is a pair consisting of a key and a value, both represented as YAML values. If _yaml_ is not a mapping, the empty list is returned.

**(yaml-for-each-element _proc yaml_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies procedure _proc_ to every element of the YAML sequence _yaml_ in order. An error is signaled if _yaml_ is not a sequence.

**(yaml-for-each-entry _proc yaml_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies procedure _proc_ to the key and the value of every entry of the YAML mapping _yaml_ in order. Both are YAML values. An error is signaled if _yaml_ is not a mapping.

```scheme
(yaml-for-each-entry
  (lambda (key value) (display* (yaml->value key) " = " (yaml->value value)) (newline))
  (string->yaml "{width: 80, height: 24}"))
```

## YAML references

**(yaml-ref _yaml ref ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Selects a node within _yaml_. A _YAML reference_ is applied to a node to select one of its children. Each _ref_ is one of the following:

  - A fixnum, which selects the element of a sequence at this index (counting from 0), or the entry of a mapping with the integer as key
  - A symbol or a string, which selects the value of the mapping entry with a scalar key whose text is equal to the symbol or string
  - A YAML value, which selects the value of the mapping entry with an equal key. This can be used for keys that are sequences or mappings.
  - A list of references, which are applied one after the other, i.e. a path

All references are applied in order, so `(yaml-ref doc 'a 'b)` and `(yaml-ref doc '(a b))` are equivalent. Procedure `yaml-ref` returns a YAML value, or `#f` if there is no node at the given reference. Without any reference, the node _yaml_ itself is returned. Aliases are already resolved when YAML text is parsed, so references see the nodes that aliases refer to. If merge keys were enabled for parsing, the merged entries are part of the mapping.

```scheme
(define doc (string->yaml "name: demo\nflags: [true, null, 3]\nnested: {list: [a, {b: 2}]}"))
(yaml->value (yaml-ref doc 'flags 2))
⇒  3
(yaml->value (yaml-ref doc 'nested 'list 1 'b))
⇒  2
(yaml->value (yaml-ref doc '(nested list 0)))
⇒  "a"
(yaml-ref doc 'nope)
⇒  #f
```

**(yaml-replace _yaml ref1 value1 ref2 value2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of _yaml_ in which the nodes at the references _ref1_, _ref2_, and so on are replaced with the corresponding values _value1_, _value2_, and so on. Each reference is a single reference or a list of references as supported by `yaml-ref`. The values are converted with procedure `yaml`. A reference to a mapping entry that does not exist yet adds a new entry at the end of the mapping, and a reference to the index that is equal to the length of a sequence appends a new element. An error is signaled if a reference cannot be resolved otherwise. The updates are applied from left to right. The value _yaml_ is not changed. Nodes that are not replaced keep their style, anchor, and tag, which is why the result in the example below is still written in flow style.

```scheme
(yaml->string (yaml-replace (string->yaml "{a: 1, b: [1, 2]}") 'a 10 '(b 1) 20 'c "new"))
⇒  "{a: 10, b: [1, 20], c: new}\n"
```

## Mutable YAML values

Mutable YAML values can be changed in place. Their mutation procedures accept the same kinds of references as `yaml-ref` and `yaml-replace`. In contrast to the corresponding procedures of library `(lispkit json)`, mutation procedures signal an error if a reference cannot be resolved or if it refers to a node of the wrong kind.

**(mutable-yaml? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a mutable YAML value; `#f` otherwise.

**(mutable-yaml _expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(mutable-yaml _expr force?_)**  

Procedure `mutable-yaml` maps Scheme data structures _expr_ to _mutable YAML values_ using the same mapping rules as `yaml`. This procedure can also be used to turn an immutable YAML value into a mutable YAML value. If _expr_ is a mutable YAML value already, then that value is returned by `mutable-yaml` unless argument _force?_ is provided and set to true. In this case, a copy is created and returned. Mutating a mutable YAML value never changes the immutable YAML value it was created from.

**(yaml-set! _yaml ref value_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Within mutable YAML value _yaml_, sets the node at reference _ref_ to _value_, mutating _yaml_ in place. The value is converted with procedure `yaml`. For references to a mapping entry that does not exist, a new entry is added. A reference to the empty list `()` replaces the whole value. An error is signaled if _yaml_ is not a mutable YAML value.

**(yaml-append! _yaml ref x ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Appends the values _x ..._ to the sequence at reference _ref_ of mutable YAML value _yaml_. An error is signaled if _ref_ does not refer to a sequence.

**(yaml-insert! _yaml ref index x ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Inserts the values _x ..._ at position _index_ of the sequence at reference _ref_ of mutable YAML value _yaml_. The index has to be between 0 and the length of the sequence. An error is signaled if _ref_ does not refer to a sequence or if _index_ is out of range.

**(yaml-remove! _yaml ref ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the nodes at the references _ref ..._ from mutable YAML value _yaml_. Elements of sequences are removed together with their position, and entries of mappings are removed together with their key. References are processed from left to right. An error is signaled if a reference cannot be resolved.

```scheme
(define cfg (mutable-yaml '((name . "demo") (hosts . #("a" "b")))))
(yaml-set! cfg 'name "prod")
(yaml-append! cfg 'hosts "c")
(yaml-insert! cfg 'hosts 0 "z")
(yaml-remove! cfg '(hosts 1))
(yaml->value cfg)
⇒  ((name . "prod") (hosts . #("z" "b" "c")))
```

## Reading and writing YAML

Procedures for reading YAML text accept an optional argument _options_, which is an association list of options. The set of supported options is described in the section [Options](#options). Reading procedures signal an error if the YAML text is malformed. The error message includes the kind of the error and the line and column of the problem, for instance `scanner error at line 2, column 1: unterminated flow collection`.

**(string->yaml _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string->yaml _str options_)**  

Parses the YAML text _str_ and returns it as an immutable YAML value. If the text does not contain any document, `#f` is returned. It is an error if the text contains more than one document; procedure `string->yaml-documents` has to be used in this case.

```scheme
(yaml->value (string->yaml "a: 1\nb: [x, y]\n"))
⇒  ((a . 1) (b . #("x" "y")))
(string->yaml "# just a comment")
⇒  #f
```

**(bytevector->yaml _bvec_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(bytevector->yaml _bvec options_)**  

Parses the YAML text encoded in bytevector _bvec_. The encoding (UTF-8, UTF-16, or UTF-32 in either byte order) is detected automatically via a byte order mark or the pattern of null bytes. In all other aspects, this procedure behaves like `string->yaml`.

**(load-yaml _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(load-yaml _path options_)**  

Loads the YAML file at file path _path_ and returns its content as an immutable YAML value. Relative paths are resolved against the current directory. The file's encoding is detected as for `bytevector->yaml`. Returns `#f` if the file does not contain any document. An error is signaled if the file contains more than one document.

**(string->yaml-documents _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string->yaml-documents _str options_)**  

Parses a YAML stream _str_ containing zero or more documents and returns a list of immutable YAML values, one for each document. The documents of a stream are separated by `---` lines. An empty document, such as `---` followed by another `---`, results in a null value.

**(bytevector->yaml-documents _bvec_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(bytevector->yaml-documents _bvec options_)**  

Like `string->yaml-documents`, but reads the YAML stream from a bytevector with automatic detection of the encoding.

**(load-yaml-documents _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(load-yaml-documents _path options_)**  

Like `string->yaml-documents`, but reads the YAML stream from the file at file path _path_.

**(yaml->string _yaml_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(yaml->string _yaml options_)**  

Serializes _yaml_ to YAML text. Argument _yaml_ can be a mutable or immutable YAML value, or any Scheme value that can be converted with procedure `yaml`. The text is written in block style by default, unless a node was parsed from flow-style text or has an explicit style; it ends with a newline character. Strings are only quoted if they would be read back as another type. Anchors and tags of the nodes are preserved; tags that follow from the schema are omitted.

```scheme
(yaml->string '((a . 1) (b . #("true" "x" 2.5))))
⇒  "a: 1\nb:\n  - \"true\"\n  - x\n  - 2.5\n"
(yaml->string (yaml #(1 2 3)) '((flow-style . #t)))
⇒  "[1, 2, 3]\n"
```

**(yaml->bytevector _yaml_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(yaml->bytevector _yaml options_)**  

Like `yaml->string`, but returns the UTF-8 encoded YAML text as a bytevector.

**(yaml-documents->string _docs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(yaml-documents->string _docs options_)**  

Serializes the list of documents _docs_ into a YAML stream. Each element of _docs_ can be a YAML value or any Scheme value that can be converted with procedure `yaml`. Documents are separated by `---` lines.

```scheme
(yaml-documents->string (list 1 "a" '((k . v))))
⇒  "1\n--- a\n---\nk: v\n"
```

**(yaml-documents->bytevector _docs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(yaml-documents->bytevector _docs options_)**  

Like `yaml-documents->string`, but returns the UTF-8 encoded YAML stream as a bytevector.

**(save-yaml _path yaml_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(save-yaml _path yaml options_)**  

Serializes _yaml_ like `yaml->string` and saves the UTF-8 encoded YAML text in the file at file path _path_. Argument _yaml_ can be a mutable or immutable YAML value, or any Scheme value that can be converted with procedure `yaml`. Relative paths are resolved against the current directory. An existing file is replaced atomically: if the value cannot be serialized or the file cannot be written, an existing file stays unchanged and an error is signaled.

```scheme
(save-yaml "config.yaml" '((name . "demo") (ports . #(80 443))))
(yaml->value (load-yaml "config.yaml"))
⇒  ((name . "demo") (ports . #(80 443)))
```

**(save-yaml-documents _path docs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(save-yaml-documents _path docs options_)**  

Serializes the list of documents _docs_ like `yaml-documents->string` and saves the UTF-8 encoded YAML stream in the file at file path _path_. This is the counterpart of `load-yaml-documents`.

## Options

Options are provided as association lists. Each entry maps an option name (a symbol) to its value. Passing `#f` or `()` selects the default values for all options. Options that are not relevant for a procedure, e.g. formatting options when reading YAML, are accepted and ignored. Unknown options and values of the wrong type are rejected with an error.

| Option | Value | Default | Description |
|---|---|---|---|
| `schema` | `core`, `json`, `failsafe`, or a procedure | `core` | Defines how plain scalars are resolved to types, and when strings need to be quoted when written. See [Schemas](#schemas). |
| `duplicate-keys` | boolean | `#f` | Allows duplicate keys in mappings when reading. If set to `#f`, duplicate keys are an error. |
| `merge-keys` | boolean | `#f` | Resolves merge keys (`<<`) when reading; the entries of the merged mappings become part of the mapping. If `#f`, `<<` is an ordinary key. |
| `max-alias-expansion` | fixnum | 1000000 | Limits the number of nodes that aliases may expand to when reading. This protects against "billion laughs" attacks. |
| `max-depth` | fixnum | 512 | Limits the nesting depth of collections when reading. |
| `indentation` | integer from 2 to 9 | 2 | Number of spaces per indentation level when writing. |
| `line-width` | fixnum or `#f` | 80 | Preferred maximum line width when writing. Longer plain, quoted, and folded scalars are broken across lines. With `#f`, lines are never broken. |
| `unicode` | boolean | `#t` | If `#f`, non-ASCII characters are written as escape sequences within double-quoted scalars. |
| `indent-sequences` | boolean | `#t` | Controls whether block sequences nested in block mappings are indented. |
| `explicit-start` | boolean | `#f` | If `#t`, every document starts with an explicit `---` marker. |
| `sort-keys` | boolean | `#f` | If `#t`, the entries of all mappings are sorted by their keys when writing. |
| `flow-style` | boolean | `#f` | If `#t`, all collections are written in flow style (`[1, 2]` and `{a: 1}`) when writing. |

```scheme
(yaml->string '((a . ((b . #(1 2)))) (c . 1)) '((indentation . 4)))
⇒  "a:\n    b:\n        - 1\n        - 2\nc: 1\n"
(yaml->string '((a . #(1 2))) '((indent-sequences . #f)))
⇒  "a:\n- 1\n- 2\n"
(yaml->string '((b . 1) (a . ((z . 1) (y . 2)))) '((sort-keys . #t)))
⇒  "a:\n  y: 2\n  z: 1\nb: 1\n"
(yaml->string (yaml "é") '((unicode . #f)))
⇒  "\"\\xE9\"\n"
```

## Schemas

A schema determines how the text of a plain (unquoted) scalar is resolved to a type when YAML is parsed. When YAML is written, the schema decides if a string has to be quoted so that it is read back as a string. The following schemas are available via option `schema`:

  - **`core`**: The YAML 1.2 core schema is the default. It resolves `null`, `Null`, `NULL`, `~`, and the empty text to null; `true`, `True`, `TRUE`, `false`, `False`, `FALSE` to booleans; decimal, octal (`0o17`), and hexadecimal (`0x1F`) numbers to integers; floating-point numbers including `.inf`, `-.inf`, and `.nan` to floats; and everything else to strings. In particular, `yes`, `no`, `on`, and `off` are strings.
  - **`json`**: The JSON schema is more restrictive than the core schema and only resolves the JSON literals `null`, `true`, `false`, and numbers.
  - **`failsafe`**: The failsafe schema only knows strings, sequences, and mappings. All plain scalars are strings.
  - **a procedure** of one argument: A custom schema. The procedure is called with the text of a plain scalar and returns the tag of the scalar as a string, e.g. `"!!bool"` or `"!ticket"`. If it returns `#f`, the core schema decides. Errors raised by the procedure are propagated to the caller of the reading or writing procedure.

```scheme
(yaml->value (string->yaml "[yes, 12, 1.5, null]"))
⇒  #("yes" 12 1.5 null)
(yaml->value (string->yaml "[yes, 12, 1.5, null]" '((schema . failsafe))))
⇒  #("yes" "12" "1.5" "null")
```

A custom schema is, for instance, useful for reading YAML files written for YAML 1.1, in which `yes`, `no`, `on`, and `off` are boolean values:

```scheme
(define (yaml-1.1-schema text)
  (and (member (string-downcase text) '("yes" "no" "on" "off" "y" "n")) "!!bool"))
(define legacy (list (cons 'schema yaml-1.1-schema)))
(yaml->value (string->yaml "{debug: yes, verbose: off, name: no-op}" legacy))
⇒  ((debug . #t) (verbose . #f) (name . "no-op"))
(yaml->string (yaml #("yes" "maybe")) legacy)
⇒  "- \"yes\"\n- maybe\n"
```

## YAML events

YAML text is processed in layers: text is parsed into a sequence of _events_, and events are composed into nodes. This library exposes both layers. The event layer is useful for processing very large streams without building a tree, for reformatting YAML text, and for observing details that nodes abstract away, like the difference between an alias and a copy of a node.

Events are represented by objects of type `yaml-event`. There are events for the beginning and the end of a stream (`stream-start`, `stream-end`) and of a document (`document-start`, `document-end`), for scalars (`scalar`), aliases (`alias`), and for the beginning and the end of sequences (`sequence-start`, `sequence-end`) and mappings (`mapping-start`, `mapping-end`). A stream with a single document `a: [1, 2]` is represented by the following events:

```
stream-start, document-start, mapping-start, scalar "a", sequence-start, scalar "1",
scalar "2", sequence-end, mapping-end, document-end, stream-end
```

**yaml-event-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `yaml-event` type.

**(yaml-event? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a YAML event; `#f` otherwise.

**(make-yaml-event _kind arg ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a new YAML event of the given kind. _kind_ is one of the symbols listed below. The remaining arguments are optional unless noted. Arguments that are not provided or are `#f` are unspecified. An error is signaled for an unknown kind, for arguments of the wrong type, and for too many arguments.

| Kind | Arguments |
|---|---|
| `stream-start`, `stream-end` | none |
| `document-start` | _implicit?_ (default `#t`), _version_ (a pair `(major . minor)`, e.g. `(1 . 2)`), _tag-directives_ (an association list mapping handles to prefixes) |
| `document-end` | _implicit?_ (default `#t`) |
| `alias` | _anchor_ (required) |
| `scalar` | _value_ (required), _style_ (default `plain`), _tag_, _anchor_ |
| `sequence-start`, `mapping-start` | _style_ (default `block`), _tag_, _anchor_ |
| `sequence-end`, `mapping-end` | none |

**(yaml-event-kind _event_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the kind of _event_ as a symbol, i.e. one of `stream-start`, `stream-end`, `document-start`, `document-end`, `alias`, `scalar`, `sequence-start`, `sequence-end`, `mapping-start`, `mapping-end`.

**(yaml-event-value _event_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the text of a `scalar` event, or the name of the referenced anchor for an `alias` event. Returns `#f` for all other events.

**(yaml-event-style _event_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the style of a `scalar`, `sequence-start`, or `mapping-start` event as a symbol. Returns `#f` for all other events.

**(yaml-event-tag _event_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the explicit tag of a `scalar`, `sequence-start`, or `mapping-start` event as a string, or `#f` if there is none or for other events.

**(yaml-event-anchor _event_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the anchor defined by a `scalar`, `sequence-start`, or `mapping-start` event as a string, or `#f` if there is none or for other events.

**(yaml-event-implicit? _event_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if a `document-start` or `document-end` event is implicit, i.e. there is no explicit `---` or `...` marker. Returns `#f` for all other events.

**(yaml-event-version _event_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the YAML version declared by the `%YAML` directive of a `document-start` event as a pair `(major . minor)`, or `#f` if there is no such directive.

**(yaml-event-tag-directives _event_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the `%TAG` directives of a `document-start` event as an association list mapping tag handles to prefixes, e.g. `(("!e!" . "tag:example.com,2000:"))`.

**(yaml-event-start _event_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(yaml-event-end _event_)**  

Return the positions where _event_ starts and ends in the parsed YAML text as lists `(line column offset)`. For events not created by parsing, the position is `(1 1 0)`.

**(string->yaml-events _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string->yaml-events _str options_)**  

Parses the YAML text _str_ and returns the list of all events, from `stream-start` to `stream-end`. Only options relevant for parsing, e.g. `max-depth`, are used.

**(bytevector->yaml-events _bvec_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(bytevector->yaml-events _bvec options_)**  

Like `string->yaml-events`, but reads the YAML text from a bytevector with automatic detection of the encoding.

**(yaml-events->string _events_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(yaml-events->string _events options_)**  

Writes the list of YAML events _events_ as YAML text. The events have to form a valid stream, i.e. they start with `stream-start` and end with `stream-end`, and are properly nested. Formatting options such as `indentation` and `line-width` are used. The following code normalizes the whitespace of YAML text:

```scheme
(yaml-events->string (string->yaml-events "{a: 1,   b: [x,y]}"))
⇒  "{a: 1, b: [x, y]}\n"
```

Events preserve the style information of the input. Hence, the text is written in flow style as it was read.

### Pull parser

**yaml-parser-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `yaml-parser` type.

**(yaml-parser? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a YAML pull parser; `#f` otherwise.

**(make-yaml-parser _source_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-yaml-parser _source options_)**  

Creates a pull parser for YAML text, which is delivered either as a string or as a bytevector. Events are produced one at a time on demand by `yaml-parser-next!`, so only a small part of the YAML text is processed at any time. Of the _options_, only `max-depth` is used.

**(yaml-parser-next! _parser_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the next event of the YAML stream, or the end-of-file object after the last event. A syntax error is signaled when the parser reaches the malformed part of the YAML text.

```scheme
(define parser (make-yaml-parser "- 1\n- 2"))
(let loop ((event (yaml-parser-next! parser)) (kinds '()))
  (if (eof-object? event)
      (reverse kinds)
      (loop (yaml-parser-next! parser) (cons (yaml-event-kind event) kinds))))
⇒  (stream-start document-start sequence-start scalar scalar sequence-end
    document-end stream-end)
```

### Push emitter

**yaml-emitter-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `yaml-emitter` type.

**(yaml-emitter? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a YAML emitter; `#f` otherwise.

**(make-yaml-emitter)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-yaml-emitter _options_)**  

Creates a YAML emitter, which converts a sequence of events to YAML text. The emitter is configured with the formatting options `indentation`, `line-width`, `unicode`, `indent-sequences`, and `explicit-start`.

**(yaml-emitter-emit! _emitter event ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Passes the events _event ..._ to _emitter_. Events have to arrive in a valid order, which is: `stream-start`, then any number of documents, and finally `stream-end`; each document is enclosed in `document-start` and `document-end`, and contains exactly one node, which is either a scalar, an alias, or a sequence or mapping with its nested nodes. An error is signaled if an event does not fit in the current state of the emitter.

**(yaml-emitter-output _emitter_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the YAML text that was generated by _emitter_ so far as a string.

```scheme
(define emitter (make-yaml-emitter))
(for-each (lambda (event) (yaml-emitter-emit! emitter event))
          (list (make-yaml-event 'stream-start)
                (make-yaml-event 'document-start)
                (make-yaml-event 'mapping-start 'block)
                (make-yaml-event 'scalar "greeting")
                (make-yaml-event 'scalar "Hello, YAML!" 'double-quoted)
                (make-yaml-event 'mapping-end)
                (make-yaml-event 'document-end)
                (make-yaml-event 'stream-end)))
(yaml-emitter-output emitter)
⇒  "greeting: \"Hello, YAML!\"\n"
```

## Differences to library (lispkit json)

Library `(lispkit yaml)` is modeled after library `(lispkit json)`. Because YAML is a much richer format, there are a few differences:

  - Mapping keys are YAML values rather than strings, and they keep their order. YAML references accept symbols and strings for the common case of string keys.
  - Mutation procedures signal errors instead of silently doing nothing if a reference cannot be resolved.
  - YAML supports multiple documents per stream, tags, anchors and aliases, scalar styles, and source positions. These are exposed by dedicated procedures and by the event API.
  - Options are provided as association lists, similar to the options of library `(lispkit markdown)`.
  - YAML text is a superset of JSON text. Every JSON text can be read with `string->yaml`, and procedures `json` and `yaml` can convert the values of one library into the other via Scheme data using `json->value` and `yaml->value`.
