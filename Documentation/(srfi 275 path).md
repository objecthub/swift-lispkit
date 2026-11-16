# SRFI 275: Paths

Library `(srfi 275 path)` provides _path objects_: immutable values that represent the path component of an IRI or URI. A path is a sequence of segments which is either absolute, i.e. it starts with a slash, or relative. The library offers constructors, access to the segments, persistent updates, and the operations of RFC 3986 for merging paths and removing dot segments. It is used by libraries `(srfi 275 iri)` and `(srfi 275 uri)`, whose procedures `iri-path` and `uri-path` return path objects and whose procedures `update-iri-path` and `update-uri-path` accept them, but it can also be used independently of them.

```scheme
(import (srfi 275 path))

(define p (string->path "/some/where/place"))
(path-segments p)                        ⇒  ("some" "where" "place")
(path-ref p 2)                           ⇒  "place"
(path->string (path-update p 2 "else"))  ⇒  "/some/where/else"
(path->string (build-path p "other"))    ⇒  "/some/where/other"
```

## Representation

Path objects are disjoint from all other types. A path has a list of segments and a flag indicating whether the path is absolute. The leading slash of an absolute path is not part of the segments: both `"some/where"` and `"/some/where"` have the segments `("some" "where")`. The absolute path `"/"` has no segments, the relative empty path `""` has no segments either. Empty segments are possible in the middle and at the end of a path; the path `"a//b/"` has segments `("a" "" "b" "")`, and the path `"//a"` is an absolute path with segments `("" "a")`.

A relative path never starts with an empty segment, as it would otherwise be mistaken for an absolute path. Procedures raise an error instead of constructing such a path.

Segments are strings. Their characters are either literal characters from `char-set:path` or percent-encoded bytes (`%XX` with upper-case hexadecimal digits), which stand for UTF-8 encoded characters. A slash that belongs to a segment is represented by `%2F`. Escapes are neither decoded when paths are accessed nor when they are compared; the segments `"%41"` and `"A"` are different.

As described for `(srfi 275 iri)`, errors are raised with `error`. Their messages start with the name of the procedure.

## Predicates

**(path? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(relative-path? _obj_)**  
**(absolute-path? _obj_)**  
**(empty-path? _obj_)**  

`path?` returns `#t` if _obj_ is a path object. `relative-path?` and `absolute-path?` return `#t` for relative and absolute paths, respectively. `empty-path?` returns `#t` if _obj_ is a relative path without segments. An absolute path without segments is not empty, since it is represented by the string `"/"`. All four procedures return `#f` for objects that are not paths.

```scheme
(empty-path? (empty-path))            ⇒  #t
(empty-path? (empty-absolute-path))   ⇒  #f
(absolute-path? (string->path "/a"))  ⇒  #t
(relative-path? "a")                  ⇒  #f
```

**(rfc3987-path-abempty? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(rfc3987-path-absolute? _obj_)**  
**(rfc3987-path-rootless? _obj_)**  
**(rfc3987-path-noscheme? _obj_)**  

Predicates for the shapes of paths that are distinguished by the ABNF grammars of RFC 3986 and 3987. They return `#f` for objects that are not paths.

- `rfc3987-path-abempty?` holds if the path is absolute or empty. It is the shape of a path that follows an authority.
- `rfc3987-path-absolute?` holds if the path is absolute and its first segment is not empty, i.e. the path does not start with `//`. It does not hold for the absolute path without segments.
- `rfc3987-path-rootless?` holds if the path is relative and has at least one non-empty segment.
- `rfc3987-path-noscheme?` holds if the path is rootless and its first segment does not contain a colon.

```scheme
(define all (map string->path '("//some/where/place" "/some/where/place" "some:thing/where/place"
                                "some/where/place" "" "/")))
(map absolute-path? all)          ⇒  (#t #t #f #f #f #t)
(map empty-path? all)             ⇒  (#f #f #f #f #t #f)
(map rfc3987-path-abempty? all)   ⇒  (#t #t #f #f #t #t)
(map rfc3987-path-absolute? all)  ⇒  (#f #t #f #f #f #f)
(map rfc3987-path-rootless? all)  ⇒  (#f #f #t #t #f #f)
(map rfc3987-path-noscheme? all)  ⇒  (#f #f #f #t #f #f)
```

**(path-string? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a path object or a string consisting only of characters of `char-set:path`. Returns `#f` for all other objects. No error is raised for objects that are not strings. The percent character is not a member of `char-set:path`, which means that strings with escapes are not path strings in this sense, although they can be converted with `string->path`.

```scheme
(path-string? "some/where/place")                ⇒  #t
(path-string? (string->path "some/where/place")) ⇒  #t
(path-string? "a b")                             ⇒  #f
(path-string? 12)                                ⇒  #f
```

## Constructors

**(empty-path)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(empty-absolute-path)**  

Return the relative path without segments (`""`) and the absolute path without segments (`"/"`), respectively.

**(string->path _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Parses string _str_ into a path. A string that starts with a slash yields an absolute path, any other string a relative path. Paths with a colon in the first segment and absolute paths with an empty first segment (`//`) are accepted. An error is raised if _str_ is not a string, contains a character which is not in `char-set:path` or `%`, or contains escapes that are not valid UTF-8.

```scheme
(path-segments (string->path "a//b/"))   ⇒  ("a" "" "b" "")
(path->string (string->path "/a%c3%a9")) ⇒  "/a%C3%A9"
(string->path "a b")                     ⇒  ERROR
(string->path "a%zz")                    ⇒  ERROR
```

**(string->relative-path _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string->relative-path _str noscheme?_)**  

Like `string->path`, but raises an error if _str_ starts with a slash. If _noscheme?_ is not `#f` and the first segment contains a colon, the path is prefixed with `./` so that it cannot be mistaken for a scheme.

```scheme
(path->string (string->relative-path "some:where/place"))
  ⇒  "some:where/place"
(path->string (string->relative-path "some:where/place" #t))
  ⇒  "./some:where/place"
(string->relative-path "/some/where/place")
  ⇒  ERROR
```

**(string->absolute-path _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string->absolute-path _str nonempty-init?_)**  

Like `string->path`, but raises an error if _str_ does not start with a slash. If _nonempty-init?_ is not `#f`, an error is raised if the first segment is empty, i.e. if _str_ starts with `//`.

```scheme
(path->string (string->absolute-path "//some/where/place"))
  ⇒  "//some/where/place"
(string->absolute-path "//some/where/place" #t)
  ⇒  ERROR
(path->string (string->absolute-path "/some/where/place" #t))
  ⇒  "/some/where/place"
```

**(list->relative-path _list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(list->relative-path _list noscheme?_)**  
**(list->absolute-path _list_)**  
**(list->absolute-path _list nonempty-init?_)**  

Create a relative or absolute path from a list of strings, which become the segments. Unlike in `string->path`, slashes in the strings do not separate segments but are escaped. Characters that are not in `char-set:path` are escaped, too. An error is raised if _list_ is not a proper list, or if a segment is not a string; in the latter case the irritants are the segment, its position, and _list_. For relative paths, an error is also raised if the first segment is empty. If _noscheme?_ is not `#f` and the first segment of a relative path contains a colon, a `.` segment is prepended. If _nonempty-init?_ is not `#f`, the first segment of an absolute path must not be empty.

```scheme
(path-segments (list->relative-path '("some" "where" "place")))
  ⇒  ("some" "where" "place")
(path-segments (list->relative-path '("some:where" "place") #t))
  ⇒  ("." "some:where" "place")
(path->string (list->absolute-path '("a/b" "c")))
  ⇒  "/a%2Fb/c"
(path-segments (list->absolute-path '("" "some" "where")))
  ⇒  ("" "some" "where")
(list->absolute-path '("" "some" "where") #t)
  ⇒  ERROR
(list->relative-path '("" "some"))
  ⇒  ERROR
```

**(build-path)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(build-path _base str_ ...)**  

Builds a path from _base_, which is a path or a string, and any number of strings. Without arguments, the empty path is returned. If _base_ is a string, the arguments are interpreted as segments like in `list->relative-path`. If _base_ is a path, a relative path is created from the remaining arguments in this way and it is merged with _base_ by `merge-paths`: the last segment of _base_ is replaced by the new segments. In order to add segments to a path, the last segment of _base_ has to be empty. Slashes in segments are always escaped. To build an absolute path from segments, use the absolute path without segments as base.

```scheme
(path->string (build-path "some" "where" "place"))                          ⇒  "some/where/place"
(path->string (build-path (empty-absolute-path) "some" "where" "place"))   ⇒  "/some/where/place"
(path->string (build-path (empty-absolute-path) "" "res"))                 ⇒  "//res"
(path->string (build-path (string->path "/a/b") "x"))                      ⇒  "/a/x"
(path->string (build-path (string->path "/a/b/") "x"))                     ⇒  "/a/b/x"
(build-path "" "res")                                                       ⇒  ERROR
```

## Access and update

**(path-length _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of segments of _path_. The leading slash of an absolute path is not counted.

```scheme
(path-length (empty-absolute-path))     ⇒  0
(path-length (string->path "a/b/c"))    ⇒  3
(path-length (string->path "//a"))      ⇒  2
```

**(path-segments _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the segments of _path_ as a list of strings. Segments are not decoded, and the leading slash of absolute paths is not included.

**(path-ref _path k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the segment of _path_ with index _k_. An error is raised, with _k_ and _path_ as irritants, if _k_ is not a valid index.

```scheme
(path-ref (string->path "some/where/place") 0)   ⇒  "some"
(path-ref (string->path "/some/where/place") 2)  ⇒  "place"
(path-ref (string->path "a/b") 2)                ⇒  ERROR
(path-ref (empty-path) 0)                        ⇒  ERROR
```

**(path-update _path k str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of _path_ in which the segment with index _k_ is replaced by string _str_. Slashes and all characters that are not in `char-set:path` are escaped. Escapes in _str_ are validated and kept. An error is raised if _k_ is not a valid index, if _str_ contains an escape that is not valid UTF-8, or if the update would make the first segment of a relative path empty.

```scheme
(path->string (path-update (string->path "some/where/place") 2 "else"))  ⇒  "some/where/else"
(path->string (path-update (string->path "/some/where/place") 0 "in/any"))  ⇒  "/in%2Fany/where/place"
(path->string (path-update (string->path "a/b") 0 "x y"))                ⇒  "x%20y/b"
(path-update (string->path "a/b") 0 "")                                  ⇒  ERROR
```

**(path=? _path1 path2 path3_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if all arguments are paths that are either all absolute or all relative, and whose segments are pairwise equal, character by character. Escapes are not interpreted.

```scheme
(path=? (string->path "some/where") (string->path "some/where"))   ⇒  #t
(path=? (string->path "some/where") (string->path "/some/where"))  ⇒  #f
(path=? (string->path "a%41") (string->path "aA"))                 ⇒  #f
(path=? (empty-path) (empty-path) (empty-path))                    ⇒  #t
```

**(path->string _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the text of _path_, with escapes left as they are.

```scheme
(path->string (empty-path))                       ⇒  ""
(path->string (empty-absolute-path))              ⇒  "/"
(path->string (string->path "/some/where/place")) ⇒  "/some/where/place"
```

## Merging and normalization

**(remove-dot-segments _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a path in which the dot segments `.` and `..` of _path_ are removed by interpreting them as references to the current and the parent directory. This corresponds to the algorithm of section 5.2.4 of RFC 3986. In contrast to the RFC, relative paths stay relative. Parent segments that would leave the root of an absolute path or the start of a relative path are dropped. If the last segment is a dot segment, a trailing slash is kept, unless no segments are left.

```scheme
(path->string (remove-dot-segments (string->path "/a/b/../x")))              ⇒  "/a/x"
(path->string (remove-dot-segments (string->path "/a/b/.././.././..")))      ⇒  "/"
(path->string (remove-dot-segments (string->path "/a/b/../.././../../e")))   ⇒  "/e"
(path->string (remove-dot-segments (string->path "a/b/../.././../../e")))    ⇒  "e"
(path->string (remove-dot-segments (string->path "a/b/..")))                 ⇒  "a/"
```

**(merge-paths _base path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(merge-paths _base path base-has-authority?_)**  

Merges the relative path _path_ with the path _base_ of a base IRI or URI, as described in section 5.2.3 of RFC 3986: the result consists of all but the last segment of _base_ followed by the segments of _path_. If _base-has-authority?_ is not `#f` and _base_ is empty, the result is _path_ made absolute. The default is that there is no authority. If _path_ is absolute, it is returned unchanged. Dot segments are not removed; see `remove-dot-segments`.

```scheme
(path->string (merge-paths (string->path "/a/b") (string->path "x")))                ⇒  "/a/x"
(path->string (merge-paths (string->path "/b/c/d;p") (string->path "../../../g")))   ⇒  "/b/c/../../../g"
(path->string (merge-paths (empty-path) (string->path "a/b/c")))                     ⇒  "a/b/c"
(path->string (merge-paths (empty-path) (string->path "a/b/c") #t))                  ⇒  "/a/b/c"
```

## Character set

**char-set:path** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[constant]</span>   

The union of `char-set:uri` and `char-set:ucschar`, i.e. all characters that can appear unescaped in a path of an IRI or URI. It is not restrictive: it contains characters such as `#` and `?`, which cannot appear in a path segment of an IRI or URI. The setters of the IRI and URI libraries, such as `update-iri-path`, escape these characters.
