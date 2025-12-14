# LispKit Serialize

Library `(lispkit serialize)` provides a simple API for serializing and deserializing Scheme expressions. With procedure `serialize`, Scheme expressions are serialized into binary data represented as bytevectors. Such bytevectors can be deserialized back into their original value with procedure `deserialize`.

Only the following types of expressions can be serialized:

  - Booleans
  - Numbers
  - Characters
  - Strings
  - Symbols
  - Bytevectors
  - Lists
  - Vectors
  - Hashtables
  - Bitsets
  - Date-time values
  - JSON values

**(serializable? _expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _expr_ is an expression which can be serialized via procedure `serialize` into a bytevector. `serializable?` returns `#f` otherwise.

**(deserializable? _bvec_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(deserializable? _bvec start_)**  
**(deserializable? _bvec start end_)**  

Returns `#t` if bytevector _bvec_ between _start_ and _end_ can be deserialized into a valid Scheme expression via procedure `deserialize`. Otherwise, `deserializable?` returns `#f`.

**(serialize _expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(serialize _expr default_)**  

Serializes expression _expr_ into a binary representation returned in form of a bytevector. Only the following types of expressions can be serialized: booleans, numbers, characters, strings, symbols, bytevectors, lists, vectors, hashtables, bitsets, date-time and JSON values. If _default_ is not provided, `serialize` raises an error whenever a value that cannot be serialized is encountered. If _default_ is provided and is serializable, then _default_ is serialized instead of each value that is not serializable.

**(deserialize _bvec_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(deserialize _bvec start_)**  
**(deserialize _bvec start end_)**  

Deserializes a bytevector _bvec_ between _start_ and _end_ into a Scheme expression. `deserialize` raises an error if the bytevector cannot be deserialized successfully.
