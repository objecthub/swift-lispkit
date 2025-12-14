# Lispkit URL

Library `(lispkit url)` defines procedures for creating and decomposing URLs. URLs are represented as strings which conform to the syntax of a generic URI. Each URI consists of five components organized hierarchically in order of decreasing significance from left to right:

```
url = scheme ":" ["//" authority] path ["?" query] ["#" fragment]
```

A component is _undefined_ if it has an associated delimiter and the delimiter does not appear in the URI. The scheme and path components are always defined. A component is _empty_ if it has no characters. The scheme component is always non-empty. The authority component consists of several _subcomponents_:

```
authority = [userinfo "@"] host [":" port]
userinfo = username [":" password]
```

The following illustration shows all components:

![URL Syntax](x-devonthink-item://54C1ABDC-D6F4-4E5A-B96B-CF58D5C1E5DC)


## Generic URLs

**(url? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a string containing a valid URL; `#f` otherwise.

**(make-url _proto_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-url _proto scheme_)**  
**(make-url _proto scheme auth_)**  
**(make-url _proto scheme auth path_)**  
**(make-url _proto scheme auth path query_)**  
**(make-url _proto scheme auth path query fragment_)**  

Returns a string representing the URL defined by merging the given URL components with URL prototype _proto_. _proto_ is either `#f` (no prototype) or it is a string which is interpreted as a partially defined URL. The URL components provided as arguments of `make-url` overwrite the respective components of _proto_. If a URL component such as _scheme_, _auth_, _path_, etc. is set to `#f`, no changes are made to the respective component of _proto_. If they are set to `#t`, then the respective component in _proto_ is removed. If a non-boolean value is provided, it replaces the respective value in _proto_. The result of applying all given URL components to _proto_ is returned as the result of `make-url`. The result is not guaranteed to be a valid URL. It could, for instance, be used as a prototype for other `make-url` calls. If it is not possible to return a result that can be parsed back into a URL prototype, `#f` is returned.

_scheme_ is a string defining the URL scheme. _auth_ is defining the URL authority. The following formats are supported for _auth_:

  - `host`: A simple string defines the host of the URL without port.
  - `(host)`: A list with one element has the same effect than just using string `host` alone.
  - `(host port)`: Specifies both the host of the URL as a string followed by the port of the URL as a fixnum.
  - `(user host port)`: Defines the username as a string followed by the host of the URL as a string followed by a port number.
  - `(user passwd host port)`: The username followed by a password, followed by a hostname followed by a port number. The first three elements of the list are strings, the last element is a number.

If a list is used to specify the URL authority, again `#f` and `#t` can be used to either not modify the respective authority component from _proto_ or to remove it. _path_ and _fragment_ define a path or fragment of a URL as a string. _query_ defines the query component of a URL using two possible formats:

  - `query`: A simple string defining the full query component of the URL
  - `((name . value) ...)`: An association list of query items consisting of name/value pairs of strings provides a structured representation of a query. It gets automatically mapped to a query string in which items are represented in the form `name=value`, separated by `&`.

If the URL extensibility mechanism via prototype _proto_ is not used and it is the goal to defined a valid URL, then using procedures `url` and `url-copy` should be preferred over using `make-url`.

```scheme
(make-url #f "https" "lisppad.app")
  ⇒  "https://lisppad.app"
(make-url #f "https" "lisppad.app" "/libraries/lispkit")
  ⇒  "https://lisppad.app/libraries/lispkit"
(make-url #f "https" "lisppad.app" "/libraries/lispkit"
          '(("lang" . "en")("c" . "US")))
  ⇒  "https://lisppad.app/libraries/lispkit?lang=en&c=US"
```

**(url)** &nbsp;&nbsp;&nbsp; <span style="float:right;text -align:rigth;">[procedure]</span>  
**(url _scheme_)**  
**(url _scheme auth_)**  
**(url _scheme auth path_)**  
**(url _scheme auth path query_)**  
**(url _scheme auth path query fragment_)**  

Returns a string representing the URL defined by the given URL components. Providing `#f` for a component means the component does not exist. _scheme_ is a string defining the URL scheme. _auth_ is defining the URL authority supporting the following formats: `host`, `(host)`, `(host port)`, `(user host port)`, and `(user passwd host port)`. _path_ and _fragment_ define a path or fragment of a URL as a string. _query_ defines the query component of a URL either as a query string or an association list of query items consisting of name/value pairs of strings.

`(url scheme ...)` is similar to `(make-url #f scheme ...)`, but it guarantees that the result is a valid URL. Invalid combinations of URL components result in procedure `url` returning `#f`.

**(url-copy _url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(url-copy _url scheme_)**  
**(url-copy _url scheme auth_)**  
**(url-copy _url scheme auth path_)**  
**(url-copy _url scheme auth path query_)**  
**(url-copy _url scheme auth path query fragment_)**  

Returns a new string representing the URL defined by merging the given URL components with URL prototype _url_. _url_ is a string which is interpreted as a partially defined URL. The URL components provided as arguments of `url-copy` overwrite the respective components of _url_. If a URL component such as _scheme_, _auth_, _path_, etc. is set to `#f`, no changes are made to the respective component of _proto_. If they are set to `#t`, then the respective component in _proto_ is removed. If a non-boolean value is provided, it replaces the respective value in _url_. The result of applying all given URL components to _url_ is returned as the result of `url-copy` if it constitutes a valid URL, otherwise `#f` is returned.

_scheme_ is a string defining the URL scheme. _auth_ is defining the URL authority supporting the following formats: `host`, `(host)`, `(host port)`, `(user host port)`, and `(user passwd host port)`. _path_ and _fragment_ define a path or fragment of a URL as a string. _query_ defines the query component of a URL either as a query string or an association list of query items consisting of name/value pairs of strings.

**(url-scheme _url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the scheme of the URL string _url_.

**(url-authority _url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(url-authority _url url-encoded?_)**  

Returns the authority of the URL string _url_ as a list of four components: username, password, host and port. URL components that do not exist are represented as `#f`. If _url-encoded?_ is provided and set to true, then the authority components are returned in percent-encoded form.

**(url-user _url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(url-user _url url-encoded?_)**  

Returns the user name of the URL string _url_ as a string, or `#f` if there is no user name defined. If _url-encoded?_ is provided and set to true, then the user name is returned in percent-encoded form.

**(url-password _url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(url-password _url url-encoded?_)**  

Returns the password of the URL string _url_ as a string, or `#f` if there is no password defined. If _url-encoded?_ is provided and set to true, then the password is returned in percent-encoded form.

**(url-host _url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(url-host _url url-encoded?_)**  

Returns the host of the URL string _url_ as a string, or `#f` if there is no host defined. If _url-encoded?_ is provided and set to true, then the host is returned in percent-encoded form.

**(url-port _url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the port of the URL string _url_ as a fixnum, or `#f` if there is no port defined.

**(url-path _url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(url-path _url url-encoded?_)**  

Returns the path of the URL string _url_ as a string, or `#f` if there is no path defined. If _url-encoded?_ is provided and set to true, then the path is returned in percent-encoded form.

**(url-query _url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(url-query _url url-encoded?_)**  

Returns the query of the URL string _url_ as a string, or `#f` if there is no query defined. If _url-encoded?_ is provided and set to true, then the query is returned in percent-encoded form.

**(url-query-items _url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the query of the URL string _url_ as an association list of string pairs, mapping URL query parameters to values. `#f` is returned, if the query cannot be parsed.

**(url-fragment _url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(url-fragment _url url-encoded?_)**  

Returns the fragment of the URL string _url_ as a string, or `#f` if there is no fragment defined. If _url-encoded?_ is provided and set to true, then the fragment is returned in percent-encoded form.

**(url-format _url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(url-format _url schc_)**  
**(url-format _url schc usrc_)**  
**(url-format _url schc usrc passc_)**  
**(url-format _url schc usrc passc hostc_)**  
**(url-format _url schc usrc passc hostc portc_)**  
**(url-format _url schc usrc passc hostc portc pathc_)**  
**(url-format _url schc usrc passc hostc portc pathc qc_)**  
**(url-format _url schc usrc passc hostc portc pathc qc fragc_)**  

Formats the given URL string _url_ using the provided component-level configurations _schc_, _usrc_, _passc_, _hostc_, _portc_, _pathc_, _qc_, and _fragc_, and returns the result as a string. Each configuration has one of the following forms:

  - `#f`: The component is omitted in the formatted URL.
  - `#t`: The component is always included in the formatted URL.
  - `"..."`: The component is omitted if the component of _url_ matches the string.
  - `("..." ...)` or `("..." ... . #t)`: The component is omitted if the list contains a string matching the component of _url_.
  - `("..." ... . #f)`: The component is only displayed if the list contains a string matching the component of _url_.
  - `("..." ... . comp)` where _comp_ is a symbol: The component is omitted if the list contains a string matching the component specified via _comp_ of _url_. The following specifiers can be used: `scheme`, `user`, `password`, `host`, `port`, `path`, `query`, and `fragment`. The component is only displayed if the list contains a string matching the component specified via _comp_ of _url_. The following specifiers can be used: `scheme?`, `user?`, `password?`, `host?`, `port?`, `path?`, `query?`, and `fragment?`.

```scheme
(url-format "http://x@lisppad.app:80/index.html?lang=en"
  #t #f #f #t "80" '("/index.html" "/index.htm") #t)
  ⇒  "http://lisppad.app?lang=en"
```

**(url-parse _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(url-parse _str schs_)**  
**(url-parse _str schs usrs_)**  
**(url-parse _str schs usrs pass_)**  
**(url-parse _str schs usrs pass ports_)**  
**(url-parse _str schs usrs pass hosts ports_)**  
**(url-parse _str schs usrs pass hosts ports paths_)**  
**(url-parse _str schs usrs pass hosts ports paths qs_)**  
**(url-parse _str schs usrs pass hosts ports paths qs frags_)**  

Parses a string _str_ using the provided component-level parsing settings _schs_, _usrs_, _pass_, _hosts_, _ports_, _paths_, _qs_, and _frags_, and returns the result as a string. Each setting has one of the following forms:

  - `#f`: The component is optional.
  - `#t`: The component is required.
  - `"..."` or number: The component is optional and if it is missing, this string or number is used as a default.

```scheme
(url-parse " http://lisppad.app:80?lang=en ok")
  ⇒  "http://lisppad.app:80?lang=en"
(url-parse " http://lisppad.app/?lang=en ok"
  #t #f #f #t 80 #t #f "target")
  ⇒  "http://lisppad.app:80/?lang=en#target"
```

## File URLs

**(file-url? _url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(file-url? _url dir?_)**  

If _dir?_ is not provided, `file-url?` returns `#t` if _obj_ is a string containing a valid file URL; `#f` otherwise. If _dir?_ is provided and set to true, `file-url?` returns `#t` if _obj_ is a string containing a valid file URL and the file URL refers to a directory; `#f` otherwise. If _dir?_ is provided and set to `#f`, `file-url?` returns `#t` if _obj_ is a string containing a valid file URL and the file URL refers to a regular file; `#f` otherwise. 

**(file-url _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(file-url _path base_)**  
**(file-url _path base expand?_)**  

Returns a file URL string for the given file _path_. If file URL _base_ is provided, _path_ is considered to be relative to _base_. If _base_ is not provided or set to `#f` and _path_ is a relative path, it is considered relative to the current directory. If _expand?_ is given and set to true, symbolic links are getting resolved in the resulting file URL.

**(file-url-standardize _url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(file-url-standardize _url expand?_)**  

Returns a standardized version of file URL _url_. If _expand?_ is given and set to true, symbolic links are getting resolved in the resulting file URL.

## URL encoding

**(url-encode _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(url-encode _str allowed-char_)**  
**(url-encode _str allowed-char force?_)**  

Returns a URL/percent-encoded version of the string _str_. _allowed-char_ defines the characters that are exempted from the encoding. By default, _allowed-char_ corresponds to all characters that are allowed to be unencoded in URL queries. Argument _allowed-char_ can either be:

  - `#f`: All characters get encoded
  - `#t`: The default characters are allowed to be unencoded.
  - Symbols `user`, `password`, `host`, `path`, `query`, `fragment`: The characters that are allowed in the respective URL components.
  - String: All characters included in the string are allowed to be unencoded.
  - Character set: All characters included in the character set (as defined by library `(lispkit char-set)` are allowed to be unencoded.

If argument _force?_ is set to `#t`, it is guaranteed that `url-encode` returns a string. If argument _force?_ is set to `#f` (the default), then `url-encode` might return `#f` if encoding fails.

**(url-decode _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(url-decode _str force?_)**  

Returns a decoded version of the URL/percent-encoded string _str_. If argument _force?_ is set to `#t`, it is guaranteed that `url-decode` returns a string. If argument _force?_ is set to `#f` (the default), then `url-decode` might return `#f` if decoding fails.
