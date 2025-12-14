# LispKit Bytevector

_Bytevectors_ represent blocks of binary data. They are fixed-length sequences of bytes, where a _byte_ is a fixnum in the range from 0 to 255 inclusive. A bytevector is typically more space-efficient than a vector containing the same values.

The _length_ of a bytevector is the number of elements that it contains. The length is a non-negative integer that is fixed when the bytevector is created. The _valid indexes_ of a bytevector are the exact non-negative integers less than the length of the bytevector, starting at index zero as with vectors.

Bytevectors are written using the notation `#u8(byte ...)`. For example, a bytevector of length 3 containing the byte 0 in element 0, the byte 10 in element 1, and the byte 5 in element 2 can be written as follows: `#u8(0 10 5)`. Bytevector constants are self-evaluating, so they do not need to be quoted.

## Basic

**(bytevector? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a bytevector; otherwise, `#f` is returned.

**(bytevector _byte ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a newly allocated bytevector containing its arguments as bytes in the given order.

```scheme
(bytevector 1 3 5 1 3 5)  ⇒  #u8(1 3 5 1 3 5)
(bytevector)              ⇒  #u8()
```

**(make-bytevector _k_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(make-bytevector _k byte_)**   

The `make-bytevector` procedure returns a newly allocated bytevector of length _k_. If _byte_ is given, then all elements of the bytevector are initialized to _byte_, otherwise the contents of each element are unspecified.

```scheme
(make-bytevector 3 12)  ⇒  #u8(12 12 12)
```

**(bytevector=? _bytevector ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if all _bytevector ..._ contain the same sequence of bytes, otherwise `#f` is returned.

**(bytevector-length _bytevector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the length of bytevector in bytes as an exact integer.

**(bytevector-u8-ref _bytevector k_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the _k_-th byte of _bytevector_. It is an error if _k_ is not a valid index of bytevector. 

```scheme
(bytevector-u8-ref #u8(1 1 2 3 5 8 13 21) 5)  ⇒  8
```

**(bytevector-u8-set! _bytevector k byte_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Stores _byte_ as the _k_-th byte of bytevector. It is an error if _k_ is not a valid index of bytevector.

```scheme
(let ((bv (bytevector 1 2 3 4)))
  (bytevector-u8-set! bv 1 3)
  bv)
⇒  #u8(1 3 3 4)
```

**(bytevector-copy _bytevector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(bytevector-copy _bytevector start_)**   
**(bytevector-copy _bytevector start end_)**   

Returns a newly allocated bytevector containing the bytes in _bytevector_ between _start_ and _end_. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

```scheme
(define a #u8(1 2 3 4 5))
(bytevector-copy a 2 4))   ⇒  #u8(3 4)
```

**(bytevector-copy! _to at from_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(bytevector-copy! _to at from start_)**   
**(bytevector-copy! _to at from start end_)**   

Copies the bytes of bytevector _from_ between _start_ and _end_ to bytevector _to_, starting at _at_. The order in which bytes are copied is unspecified, except that if the source and destination overlap, copying takes place as if the source is first copied into a temporary bytevector and then into the destination. This can be achieved without allocating storage by making sure to copy in the correct direction in such circumstances.

It is an error if _at_ is less than zero or greater than the length of _to_. It is also an error if `(- (bytevector-length to) at)` is less than `(- end start)`.

```scheme
(define a (bytevector 1 2 3 4 5))
(define b (bytevector 10 20 30 40 50))
(bytevector-copy! b 1 a 0 2)
b  ⇒  #u8(10 1 2 40 50)
```

**(bytevector-append _bytevector ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a newly allocated bytevector whose elements are the concatenation of the elements in the given bytevectors.

```scheme
(bytevector-append #u8(0 1 2) #u8(3 4 5))
  ⇒  #u8(0 1 2 3 4 5)
```

## Input/Output

**(read-binary-file _path_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Reads the file at _path_ and stores its content in a new bytevector which gets returned by `read-binary-file`.

**(write-binary-file _path bytevector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(write-binary-file _path bytevector start_)**   
**(write-binary-file _path bytevector start end_)**   

Writes the bytes of _bytevector_ between _start_ and _end_ into a new binary file at _path_. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

## Compression

**(bytevector-deflate _bytevector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(bytevector-deflate _bytevector start_)**   
**(bytevector-deflate _bytevector start end_)**   

`bytevector-deflate` encodes _bytevector_ between _start_ and _end_ using the _Deflate_ data compression alogrithm returning a new compressed bytevector. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(bytevector-inflate _bytevector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(bytevector-inflate _bytevector start_)**   
**(bytevector-inflate _bytevector start end_)**   

`bytevector-inflate` assumes _bytevector_ is encoded using the _Deflate_ data compression alogrithm between _start_ and _end_. The procedure returns a corresponding new decoded bytevector.

If is an error if _bytevector_, between _start_ and _end_, is not encoded using _Deflate_. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(bytevector-zip _bytevector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(bytevector-zip _bytevector start_)**   
**(bytevector-zip _bytevector start end_)**   

`bytevector-zip` encodes _bytevector_ between _start_ and _end_ using the _Deflate_ data compression alogrithm returning a new compressed bytevector which is using a _zlib_ wrapper. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(bytevector-unzip _bytevector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(bytevector-unzip _bytevector start_)**   
**(bytevector-unzip _bytevector start end_)**   

`bytevector-unzip` assumes _bytevector_ is using a _zlib_ wrapper for data encoded using the _Deflate_ data compression alogrithm between _start_ and _end_. The procedure returns a corresponding new decoded bytevector.

If is an error if _bytevector_, between _start_ and _end_, is not encoded using _Deflate_ or is not using the _zlib_ wrapper format. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(bytevector-zip-header? _bytevector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(bytevector-zip-header? _bytevector start_)**   
**(bytevector-zip-header? _bytevector start end_)**   

`bytevector-zip-header?` returns `#t` if the _bytevector_ is using a _zlib_ wrapper for data encoded using the _Deflate_ data compression alogrithm between _start_ and _end_. The procedure returns `#f` otherwise. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(bytevector-gzip _bytevector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(bytevector-gzip _bytevector start_)**   
**(bytevector-gzip _bytevector start end_)**   

`bytevector-gzip` encodes _bytevector_ between _start_ and _end_ using the _Deflate_ data compression alogrithm returning a new compressed bytevector which is using a _gzip_ wrapper. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(bytevector-gunzip _bytevector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(bytevector-gunzip _bytevector start_)**   
**(bytevector-gunzip _bytevector start end_)**   

`bytevector-gunzip` assumes _bytevector_ is using a _gzip_ wrapper for data encoded using the _Deflate_ data compression alogrithm between _start_ and _end_. The procedure returns a corresponding new decoded bytevector.

If is an error if _bytevector_, between _start_ and _end_, is not encoded using _Deflate_ or is not using the _gzip_ wrapper format. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(bytevector-gzip-header? _bytevector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(bytevector-gzip-header? _bytevector start_)**   
**(bytevector-gzip-header? _bytevector start end_)**   

`bytevector-gzip-header?` returns `#t` if the _bytevector_ is using a _gzip_ wrapper for data encoded using the _Deflate_ data compression alogrithm between _start_ and _end_. The procedure returns `#f` otherwise. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.


## Advanced

**(utf8-\>string _bytevector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(utf8-\>string _bytevector start_)**   
**(utf8-\>string _bytevector start end_)**   
**(string-\>utf8 _string_)**   
**(string-\>utf8 _string start_)**   
**(string-\>utf8 _string start end_)**   

These procedures translate between strings and bytevectors that encode those strings using the UTF-8 encoding. The `utf8->string` procedure decodes the bytes of a _bytevector_ between _start_ and _end_ and returns the corresponding string. The `string->utf8` procedure encodes the characters of a _string_ between _start_ and _end_ and returns the corresponding bytevector.

It is an error for _bytevector_ to contain invalid UTF-8 byte sequences.

```scheme
(utf8->string #u8(#x41))  ⇒  "A"
(string->utf8 "λ")        ⇒  #u8(#xCE #xBB)
```

**(bytevector-\>base64 _bytevector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(bytevector-\>base64 _bytevector start_)**   
**(bytevector-\>base64 _bytevector start end_)**   

`bytevector->base64` encodes _bytevector_ between _start_ and _end_ as a string consisting of ASCII characters using the _Base64_ encoding scheme. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(base64-\>bytevector _str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(base64-\>bytevector _str start_)**   
**(base64-\>bytevector _str start end_)**   

`base64->bytevector` assumes string _str_ is encoded using _Base64_ between _start_ and _end_ and returns a corresponding new decoded bytevector.

If is an error if _str_ between _start_ and _end_ is not a valid _Base64_-encoded string. If _end_ is not provided, it is assumed to be the length of _str_. If _start_ is not provided, it is assumed to be 0.

**(bytevector-\>hex _bytevector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(bytevector-\>hex _bytevector start_)**   
**(bytevector-\>hex _bytevector start end_)**   

Returns a string representation of _bytevector_ in which every byte between _start_ and _end_ is represented by two characters denoting the value of the byte in hexadecimal form. The characters representing the individual bytes are concatenated such that a bytevector is represented by a hex string of length _end - start_. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

```scheme
(bytevector->hex #u8(7 8 9 10 11 12)) ⇒ "0708090a0b0c"
```

**(hex-\>bytevector _str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(hex-\>bytevector _str start_)**   
**(hex-\>bytevector _str start end_)**   

Returns a bytevector for a given hex string between _start_ and _end_. Such strings encode every byte with two characters representing the value of the byte in hexadecimal form.

If is an error if _str_ between _start_ and _end_ is not a valid hex string. If _end_ is not provided, it is assumed to be the length of _str_. If _start_ is not provided, it is assumed to be 0.

```scheme
(hex->bytevector "1718090a0b0c") ⇒ #u8(23 24 9 10 11 12)
```

**(bytevector-adler32 _bytevector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(bytevector-adler32 _bytevector start_)**   
**(bytevector-adler32 _bytevector start end_)**   

`bytevector-adler32` computes the Adler32 checksum for _bytevector_ between _start_ and _end_. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(bytevector-crc32 _bytevector_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(bytevector-crc32 _bytevector start_)**   
**(bytevector-crc32 _bytevector start end_)**   

`bytevector-crc32` computes the CRC32 checksum for _bytevector_ between _start_ and _end_. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.
