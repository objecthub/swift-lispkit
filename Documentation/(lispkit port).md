# LispKit Port

Ports represent abstractions for handling input and output. They are used to access files, devices, and similar things on the host system on which LispKit is running.

An _input port_ is a LispKit object that can deliver data upon command, while an _output port_ is an object that can accept data. In LispKit, input and output port types are disjoint, i.e. a port is either an input or an output port.

Different port types operate on different data. LispKit provides two differnt types of ports: _textual ports_ and _binary ports_. Textual ports and binary ports are disjoint, i.e. a port is either textual or binary.

A _textual port_ supports reading or writing of individual characters from or to a backing store containing characters using `read-char` and `write-char`, and it supports operations defined in terms of characters, such as `read` and `write`.

A _binary port_ supports reading or writing of individual bytes from or to a backing store containing bytes using `read-u8` and `write-u8` below, as well as operations defined in terms of bytes.

## Default ports

**current-output-port** <span style="float:right;text-align:rigth;">[parameter object]</span>   
**current-input-port**   
**current-error-port**   

These parameter objects represent the current default input port, output port, or error port (an output port), respectively. These parameter objects can be overridden with `parameterize`.

**default-output-port** <span style="float:right;text-align:rigth;">[constant]</span>   
**default-input-port**   

These two ports are the initial values of `current-output-port` and `current-input-port` when LispKit gets initialized. They are typically referring to the default output and input device of the system on which LispKit is running.

## Predicates

**(port? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a port object; otherwise `#f` is returned.

**(input-port? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(output-port? _obj_)**   

These predicates return `#t` if _obj_ is an input port or output port; otherwise they return `#f`.

**(textual-port? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(binary-port? _obj_)**  

These predicates return `#t` if _obj_ is a textual or a binary port; otherwise they return `#f`.

**(input-port-open? _port_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(output-port-open? _port_)**  

Returns `#t` if _port_ is still open and capable of performing input or output, respectively, and `#f` otherwise.

**(eof-object? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is an end-of-file object, otherwise returns `#f`.

## General ports

**(close-port _port_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(close-input-port _port_)**   
**(close-output-port _port_)**   

Closes the resource associated with _port_, rendering the port incapable of delivering or accepting data. It is an error to apply `close-input-port` and `close-output-port` to a port which is not an input or output port, respectively. All procedures for closing ports have no effect if the provided _port_ has already been closed.

**(with-input-from-port _port thunk_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(with-output-to-port _port thunk_)**  

The given port is made to be the value returned by `current-input-port` or `current-output-port` (as used by `(read)`, `(write obj)`, and so forth). The _thunk_ is then called with no arguments. When the _thunk_ returns, the port is closed and the previous default is restored. It is an error if _thunk_ does not accept zero arguments. Both procedures return the values yielded by _thunk_. If an escape procedure is used to escape from the continuation of these procedures, they behave exactly as if the current input or output port had been bound dynamically with `parameterize`.

**(call-with-port _port proc_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

The `call-with-port` procedure calls _proc_ with _port_ as an argument. It is an error if _proc_ does not accept one argument.

If _proc_ returns, then the port is closed automatically and the values yielded by _proc_ are returned. If _proc_ does not return, then the port will not be closed automatically unless it is possible to prove that the port will never again be used for a read or write operation.

This is necessary, because LispKit’s escape procedures have unlimited extent and thus it is possible to escape from the current continuation but later to resume it. If LispKit would be permitted to close the port on any escape from the current continuation, then it would be impossible to write portable code using both `call-with-current-continuation` and `call-with-port`.

## File ports

**(open-input-file _filepath_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(open-input-file _filepath fail_)**   

Takes a _filepath_ referring to an existing file and returns a textual input port that is capable of delivering data from the file. If the file does not exist or cannot be opened, an error that satisfies `file-error?` is signaled if argument _fail_ is not provided. If _fail_ is provided, it is returned in case an error occured.

**(open-binary-input-file _filepath_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(open-binary-input-file _filepath fail_)**   

Takes a _filepath_ referring to an existing file and returns a binary input port that is capable of delivering data from the file. If the file does not exist or cannot be opened, an error that satisfies `file-error?` is signaled if argument _fail_ is not provided. If _fail_ is provided, it is returned in case an error occured.

**(open-output-file _filepath_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(open-output-file _filepath fail_)**   

Takes a _filepath_ referring to an output file to be created and returns a textual output port that is capable of writing data to the new file. If a file with the given name exists already, the effect is unspecified. If the file cannot be opened, an error that satisfies `file-error?` is signaled if argument _fail_ is not provided. If _fail_ is provided, it is returned in case an error occured.

**(open-binary-output-file _filepath_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(open-binary-output-file _filepath fail_)**   

Takes a _filepath_ referring to an output file to be created and returns a binary output port that is capable of writing data to the new file. If a file with the given name exists already, the effect is unspecified. If the file cannot be opened, an error that satisfies `file-error?` is signaled if argument _fail_ is not provided. If _fail_ is provided, it is returned in case an error occured.

**(with-input-from-file _filepath thunk_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(with-output-to-file _filepath thunk_)**  

The file determined by _filepath_ is opened for input or output as if by `open-input-file` or `open-output-file`, and the new port is made to be the value returned by `current-input-port` or `current-output-port` (as used by `(read)`, `(write obj)`, and so forth). The _thunk_ is then called with no arguments. When the _thunk_ returns, the port is closed and the previous default is restored. It is an error if _thunk_ does not accept zero arguments. Both procedures return the values yielded by _thunk_. If an escape procedure is used to escape from the continuation of these procedures, they behave exactly as if the current input or output port had been bound dynamically with `parameterize`.

**(call-with-input-file _filepath proc_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(call-with-output-file _filepath proc_)**  

These procedures create a textual port obtained by opening the file referred to by _filepath_ (a string) for input or output as if by `open-input-file` or `open-output-file`. This port and _proc_ are then passed to a procedure equivalent to `call-with-port`. It is an error if _proc_ does not accept one argument.

## String ports

**(open-input-string _str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Takes a string and returns a textual input port that delivers characters from the string. If the string is modified, the effect is unspecified.

**(open-output-string)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a textual output port that will accumulate characters for retrieval by `get-output-string`.

```scheme
(parameterize ((current-output-port (open-output-string)))
  (display "piece")
  (display " by piece ")
  (display "by piece.")
  (get-output-string (current-output-port)))
    ⇒ "piece by piece by piece."
```

**(get-output-string _port_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

It is an error if _port_ was not created with `open-output-string`.

Returns a string consisting of the characters that have been output to _port_ so far in the order they were output.

```scheme
(parameterize ((current-output-port (open-output-string)))
  (display "piece")
  (display " by piece ")
  (display "by piece.")
  (newline)
  (get-output-string (current-output-port)))
    ⇒ "piece by piece by piece.\n"
```

**(with-input-from-string _str thunk_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

String _str_ is opened for input as if by `open-input-string`, and the new textual string port is made to be the value returned by `current-input-port`. The _thunk_ is then called with no arguments. When the _thunk_ returns, the port is closed and the previous default is restored. It is an error if _thunk_ does not accept zero arguments. `with-input-from-string` returns the values yielded by _thunk_. If an escape procedure is used to escape from the continuation of these procedures, they behave exactly as if the current input port had been bound dynamically with `parameterize`.

**(with-output-to-string _thunk_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

A new string output port is created as if by calling `open-output-string`, and the new port is made to be the value returned by `current-output-port`. The _thunk_ is then called with no arguments. When the _thunk_ returns, the port is closed and the previous default is restored. It is an error if _thunk_ does not accept zero arguments. Both procedures return the values yielded by _thunk_. If an escape procedure is used to escape from the continuation of these procedures, they behave exactly as if the current input or output port had been bound dynamically with `parameterize`.

**(call-with-output-string _proc_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

The procedure _proc_ is called with one argument, a textual output port. The values yielded by _proc_ are ignored. When _proc_ returns, `call-with-output-string` returns the port’s accumulated output as a string.

This procedure is defined as follows:

```scheme
(define (call-with-output-string procedure)
  (let ((port (open-output-string)))
    (procedure port)
    (get-output-string port)))
```

## Bytevector ports

**(open-input-bytevector _bvector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Takes a bytevector _bvector_ and returns a binary input port that delivers bytes from the bytevector _bvector_.

**(open-output-bytevector)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a binary output port that will accumulate bytes for retrieval by `get-output-bytevector`.

**(get-output-bytevector _port_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

It is an error if _port_ was not created with `open-output-bytevector`. `get-output-bytevector` returns a bytevector consisting of the bytes that have been output to the port so far in the order they were output.

**(call-with-output-bytevector _proc_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

The procedure _proc_ gets called with one argument, a binary output port. The values yielded by procedure _proc_ are ignored. When it returns, `call-with-output-bytevector` returns the port’s accumulated output as a newly allocated bytevector.

This procedure is defined as follows:

```scheme
(define (call-with-output-bytevector procedure)
  (let ((port (open-output-bytevector)))
    (procedure port)
    (get-output-bytevector port)))
```

## URL ports

**(open-input-url _url_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(open-input-url _url timeout_)**  
**(open-input-url _url timeout fail_)**  

Takes a _url_ referring to an existing resource and returns a textual input port that is capable of reading data from the resource (e.g. via HTTP). _timeout_ specifies a timeout in seconds as a flonum for the operation to wait. If no data is available, the procedure will fail either by throwing an exception or by returning value _fail_ if provided.

**(open-binary-input-url _url_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(open-binary-input-url _url timeout_)**  
**(open-binary-input-url _url timeout fail_)**   

Takes a _url_ referring to an existing resource and returns a binary input port that is capable of reading data from the resource (e.g. via HTTP). _timeout_ specifies a timeout in seconds as a flonum for the operation to wait. If no data is available, the procedure will fail either by throwing an exception or by returning value _fail_ if provided.

**(with-input-from-url _url thunk_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

The given _url_ is opened for input as if by `open-input-url`, and the new input port is made to be the value returned by `current-input-port`. The _thunk_ is then called with no arguments. When the _thunk_ returns, the port is closed and the previous default is restored. It is an error if _thunk_ does not accept zero arguments. The procedure returns the values yielded by _thunk_. If an escape procedure is used to escape from the continuation of this procedure, they behave exactly as if `current-input-port` had been bound dynamically with `parameterize`.

**(call-with-input-url _url proc_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

`call-with-input-url` creates a textual input port by opening the resource at _url_ for input as if by `open-input-url`. This port and _proc_ are then passed to a procedure equivalent to `call-with-port`. It is an error if _proc_ does not accept one argument. Here is an implementation of `call-with-input-url`:

```scheme
(define (call-with-input-url url proc)
  (let* ((port (open-input-url url))
         (res (proc port)))
    (close-input-port port)
    res))
```

**(try-call-with-input-url _url proc thunk_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

`try-call-with-input-url` creates a textual input port by opening the resource at _url_ for input as if by `open-input-url`. This port and _proc_ are then passed to a procedure equivalent to `call-with-port` in case it was possible to open the port. If the port couldn't be opened, _thunk_ gets invoked. It is an error if _proc_ does not accept one argument and if _thunk_ requires at least one argument. Here is an implementation of `try-call-with-input-url`:

```scheme
(define (try-call-with-input-url url proc thunk)
  (let ((port (open-input-url url 60.0 #f)))
    (if port
        (car (cons (proc port) (close-input-port port)))
        (thunk))))
```

## Asset ports

**(open-input-asset _name type_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(open-input-asset _name type dir_)**   

This function can be used to open a textual LispKit asset file located in one of LispKit's asset paths. An asset is identified via a file _name_, a file _type_, and an optional directory path _dir_. _name_, _type_, and _dir_ are all strings. `open-input-asset` constructs a relative file path in the following way (assuming _name_ does not have a suffix already):

&nbsp;&nbsp;&nbsp;_dir/name.type_

It then searches the asset paths in their given order for a file matching this relative file path. Once the first matching file is found, the file is opened as a text file and a corresponding textual input port that is capable of reading data from the file is returned. It is an error if no matching asset is found.

**(open-binary-input-asset _name type_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(open-binary-input-asset _name type dir_)**   

This function can be used to open a binary LispKit asset file located in one of LispKit's asset paths. An asset is identified via a file _name_, a file _type_, and an optional directory path _dir_. _name_, _type_, and _dir_ are all strings. `open-input-asset` constructs a relative file path in the following way (assuming _name_ does not have a suffix already):

&nbsp;&nbsp;&nbsp;_dir/name.type_

It then searches the asset paths in their given order for a file matching this relative file path. Once the first matching file is found, the file is opened as a binary file and a corresponding binary input port that is capable of reading data from the file is returned. It is an error if no matching asset is found.

## Reading from ports

If port is omitted from any input procedure, it defaults to the value returned by `(current-input-port)`. It is an error to attempt an input operation on a closed port.

**(read)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(read _port_)**  

The `read` procedure converts external representations of Scheme objects into the objects themselves by parsing the input. `read` returns the next object parsable from the given textual input _port_, updating _port_ to point to the first character past the end of the external representation of the object.

If an end of file is encountered in the input before any characters are found that can begin an object, then an end-of-file object is returned. The port remains open, and further attempts to read will also return an end-of-file object. If an end of file is encountered after the beginning of an object’s external representation, but the external representation is incomplete and therefore not parsable, an error that satisfies `read-error?` is signaled.

**(read-char)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(read-char _port_)**   

Returns the next character available from the textual input _port_, updating _port_ to point to the following character. If no more characters are available, an end-of-file object is returned.

**(peek-char)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(peek-char _port_)**   

Returns the next character available from the textual input _port_, but without updating _port_ to point to the following character. If no more characters are available, an end-of-file object is returned.

Note: The value returned by a call to `peek-char` is the same as the value that would have been returned by a call to `read-char` with the same port. The only difference is that the very next call to `read-char` or `peek-char` on that port will return the value returned by the preceding call to `peek-char`. In particular, a call to `peek-char` on an interactive port will hang waiting for input whenever a call to `read-char` would have hung.

**(char-ready?)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(char-ready? _port_)**   

Returns `#t` if a character is ready on the textual input _port_ and returns `#f` otherwise. If `char-ready?` returns `#t` then the next `read-char` operation on the given _port_ is guaranteed not to hang. If the _port_ is at end of file, then `char-ready?` returns #t.

Rationale: The `char-ready?` procedure exists to make it possible for a program to accept characters from interactive ports without getting stuck waiting for input. Any input editors associated with such ports must ensure that characters whose existence has been asserted by `char-ready?` cannot be removed from the input. If `char-ready?` were to return `#f` at end of file, a port at end of file would be indistinguishable from an interactive port that has no ready characters.

**(read-token)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(read-token _port_)**   
**(read-token _port charset_)**  

Returns the next token of text available from the textual input _port_, updating _port_ to point to the following character. A token is a non-empty sequence of characters delimited by characters from character set _charset_. Tokens never contain characters from _charset_. _charset_ defaults to the set of all whitespace and newline characters.

**(read-line _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(read-line _port_)**   

Returns the next line of text available from the textual input _port_, updating _port_ to point to the following character. If an end of line is read, a string containing all of the text up to (but not including) the end of line is returned, and _port_ is updated to point just past the end of line. If an end of file is encountered before any end of line is read, but some characters have been read, a string containing those characters is returned. If an end of file is encountered before any characters are read, an end-of-file object is returned. For the purpose of this procedure, an end of line consists of either a linefeed character, a carriage return character, or a sequence of a carriage return character followed by a linefeed character.

**(read-string _k_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(read-string _k port_)**   

Reads the next _k_ characters, or as many as are available before the end of file, from the textual input _port_ into a newly allocated string in left-to-right order and returns the string. If no characters are available before the end of file, an end-of-file object is returned.

**(read-u8)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(read-u8 _port_)**

Returns the next byte available from the binary input _port_, updating _port_ to point to the following byte. If no more bytes are available, an end-of-file object is returned.

**(peek-u8 _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the next byte available from the binary input _port_, but without updating _port_ to point to the following byte. If no more bytes are available, an end-of-file object is returned.

**(u8-ready?)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(u8-ready? _port_)**   

Returns `#t` if a byte is ready on the binary input _port_ and returns `#f` otherwise. If `u8-ready?` returns `#t` then the next `read-u8` operation on the given _port_ is guaranteed not to hang. If the _port_ is at end of file then `u8-ready?` returns `#t`.

**(read-bytevector _k_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(read-bytevector _k port_)**   

Reads the next _k_ bytes, or as many as are available before the end of file, from the binary input _port_ into a newly allocated bytevector in left-to-right order and returns the bytevector. If no bytes are available before the end of file, an end-of-file object is returned.

**(read-bytevector! _bvector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(read-bytevector! _bvector port_)**   
**(read-bytevector! _bvector port start_)**   
**(read-bytevector! _bvector port start end_)**   

Reads the next _end − start_ bytes, or as many as are available before the end of file, from the binary input _port_ into bytevector _bvector_ in left-to-right order beginning at the _start_ position. If _end_ is not supplied, reads until the end of bytevector _bvector_ has been reached. If _start_ is not supplied, reads beginning at position 0. Returns the number of bytes read. If no bytes are available, an end-of-file object is returned.

## Writing to ports

If _port_ is omitted from any output procedure, it defaults to the value returned by `(current-output-port)`. It is an error to attempt an output operation on a closed port.

**(write _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(write _obj port_)**   

Writes a representation of _obj_ to the given textual output _port_. Strings that appear in the written representation are enclosed in quotation marks, and within those strings backslash and quotation mark characters are escaped by backslashes. Symbols that contain non-ASCII characters are escaped with vertical lines. Character objects are writ- ten using the `#\` notation.

If _obj_ contains cycles which would cause an infinite loop using the normal written representation, then at least the objects that form part of the cycle will be represented using datum labels. Datum labels will not be used if there are no cycles.

**(write-shared _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(write-shared _obj port_)**  

The `write-shared` procedure is the same as `write`, except that shared structures will be represented using datum labels for all pairs and vectors that appear more than once in the output.

**(write-simple _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(write-simple _obj port_)**   

The `write-simple` procedure is the same as `write`, except that shared structures will never be represented using datum labels. This can cause `write-simple` not to terminate if _obj_ contains circular structures.

**(write-formatted _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(write-formatted _obj config_)**   
**(write-formatted _obj config port_)**   

Writes a representation of _obj_ to the given textual output _port_ using formatting configuration _config_ as defined by library `(lispkit format)`. _config_ defines how objects of different types are being written. Using this procedure makes it possible to use custom formatting logic instead of the hardcoded logic as provided by procedure `write`.

**(display _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(display _obj port_)**   

Writes a representation of _obj_ to the given textual output _port_. Strings that appear in the written representation are output as if by `write-string` instead of by `write`. Symbols are not escaped. Character objects appear in the representation as if written by `write-char` instead of by `write`. `display` will not loop forever on self-referencing pairs, vectors, or records.

The `write` procedure is intended for producing machine-readable output and `display` for producing human-readable output.

**(display\* _obj ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Writes a representation of _obj ..._ to the current default textual output port. Strings that appear in the written representation are output as if by `write-string` instead of by `write`. Symbols are not escaped. Character objects appear in the representation as if written by `write-char` instead of by `write`. `display*` will not loop forever on self-referencing pairs, vectors, or records.

**(display-format _[port] [config] [locale] [tabw [linew]] cntrl arg ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`display-format` writes formatted output to a textual output _port_, outputting the characters of the control string _cntrl_ while interpreting formatting directives embedded in _cntrl_. The control string syntax and the semantics of the provided arguments matches procedure `format` of library `(lispkit format)`. _config_ refers to a `format-config` object which defines environment variables influencing the output of some formatting directives. _locale_ refers to a locale identifier like symbol `en_US` that is used by locale-specific formatting directives. _tabw_ defines the maximum number of space characters that correspond to a single tab character. _linew_ specifies the number of characters per line; this is used by the justification directive only.

**(newline)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(newline _port_)**   

Writes an end of line to textual output _port_.

**(write-char _char_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(write-char _char port_)**  

Writes the character _char_ (not an external representation of the character) to the given textual output _port_.

**(write-string _str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(write-string _str port_)**  
**(write-string _str port start_)**  
**(write-string _str port start end_)**  

Writes the characters of string _str_ from index _start_ to _end_ (exclusive) in left-to-right order to the textual output _port_. The default of _start_ is 0, the default of _end_ is the length of _str_.

**(write-u8 _byte_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(write-u8 _byte_)**  

Writes the _byte_ to the given binary output _port_.

**(write-bytevector _bvector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(write-bytevector _bvector port_)**  
**(write-bytevector _bvector port start_)**  
**(write-bytevector _bvector port start end_)**  

Writes the bytes of bytevector _bvector_ from _start_ to _end_ (exclusive) in left-to-right order to the binary output _port_. The default of _start_ is 0, the default of _end_ is the length of _bvector_.

**(flush-output-port)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(flush-output-port _port_)**  

Flushes any buffered output from the buffer of the given output _port_ to the underlying file or device.

**(eof-object)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns an end-of-file object.
