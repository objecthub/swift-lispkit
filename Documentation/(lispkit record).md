# LispKit Record

Library `(lispkit record)` implements extensible record types for LispKit which are compatible with R7RS and SRFI 131. A record provides a simple and flexible mechanism for building structures with named components wrapped in distinct types.

## Declarative API

`record-type` syntax is used to introduce new _record types_ in a declarative fashion. Like other definitions, `record-type` can either appear at the outermost level or locally within a body. The values of a _record type_ are called _records_ and are aggregations of zero or more fields, each of which holds a single location. A predicate, a constructor, and field accessors and mutators are defined for each record type.

### Syntax

**(define-record-type _\<type\> \<constr\> \<pred\> \<field\> ..._)** <span style="float:right;text-align:rigth;">\[syntax\]</span>   

_\<type\>_ defines the name of the new record type and potentially the supertype if the new type is an extension of an existing record type. Thus, it has one of the two possible forms, where _\<type name\>_ is an identifier and _\<supertype\>_ an arbitrary expression evaluating to a record type descriptor:

&nbsp;&nbsp;&nbsp;_\<type name\>_, or  
&nbsp;&nbsp;&nbsp;(_\<type name\> \<supertype\>_).

The _\<constructor\>_ is of one of the two possible forms:

&nbsp;&nbsp;&nbsp;_\<constructor name\>, or  
&nbsp;&nbsp;&nbsp;(_\<constructor name\> \<field name\> ..._)

When a constructor is of the form (_\<constructor name\> \<field name\> ..._), the following holds:

  - Each of the field names can be either a field name declared in the same `define-record-type` form, or any of its ancestors' field names.
  - If the record definition contains the same field name as one of its ancestors, it shadows the ancestor's field name for the purposes of the constructor. The constructor's argument initializes the child's slot, and the ancestor's slot of the same name is left uninitialized.
  - It is an error if the same identifier appears more than once in the field names of the constructor spec.  

_\<pred\>_ is the identifier denoting the type predicate that recognizes instances of the new record type and its subtypes. Each _\<field\>_ is either of the form:

&nbsp;&nbsp;&nbsp;(_\<field name\> \<accessor name\>_), or  
&nbsp;&nbsp;&nbsp;(_\<field name\> \<accessor name\> \<modifier name\>_).

It is an error for the same identifier to occur more than once as a field name. It is also an error for the same identifier to occur more than once as an accessor or mutator name.

The `define-record-type` construct is generative: each use creates a new record type that is distinct from all existing types, including the predefined types and other record types - even record types of the same name or structure.

An instance of `define-record-type` is equivalent to the following definitions:

- _\<type name\>_ is bound to a representation of the record type itself. If a \<supertype\> expression is specified, then it must evaluate to a record type descriptor that serves as the parent record type for the record type being defined.

- _\<constructor name\>_ is bound to a procedure that takes as many arguments as there are _\<field name\>_ elements in the (_\<constructor name\> ..._) subexpression and returns a new record of type _\<name\>_. Fields whose names are listed with _\<constructor name\>_ have the corresponding argument as their initial value. The initial values of all other fields are unspecified. It is an error for a field name to appear in _\<constructor\>_ but not as a _\<field name\>_.

- _\<pred\>_ is bound to a predicate that returns `#t` when given a value returned by the procedure bound to _\<constructor name\>_ or any constructor of a subtype. It returns `#f` for everything else.

- Each _\<accessor name\>_ is bound to a procedure that takes a record of type _\<name\>_ and returns the current value of the corresponding field. It is an error to pass an accessor a value which is not a record of the appropriate type.

- Each _\<modifier name\>_ is bound to a procedure that takes a record of type _\<name\>_ and a value which becomes the new value of the corresponding field. It is an error to pass a modifier a first argument which is not a record of the appropriate type.

### Examples

The following record-type definition:

```scheme
(define-record-type <pare>
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))
```

defines `kons` to be a constructor, `kar` and `kdr` to be accessors, `set-kar!` to be a modifier, and `pare?` to be a type predicate for instances of `<pare>`.

```scheme
(pare? (kons 1 2))        ⇒  #t
(pare? (cons 1 2))        ⇒  #f
(kar (kons 1 2))          ⇒  1
(kdr (kons 1 2))          ⇒  2
(let ((k (kons 1 2)))
  (set-kar! k 3) (kar k)) ⇒  3
```

Below is another example showcasing how record types can be extended. First a new record type `<point>` is defined:

```scheme
(define-record-type <point>
  (make-point x y)
  point?
  (x point-x set-point-x!)
  (y point-y set-point-y!))
```

Next, a new record type `<color-point>` is defined to extend record type `<point>`:

```scheme
(define-record-type (<color-point> <point>)
  (make-color-point x y color)
  color-point?
  (color point-color))
```

The following transcript shows that `<color-point>` objects are also considered `<point>` objects which inherit all fields from `<point>`:

```scheme
(define cp (make-color-point 12 3 red))
(color-point? cp)    ⇒  #t
(point? cp)          ⇒  #t
(color-point?
  (make-point 1 2))  ⇒  #f
(point-x cp)         ⇒  12
(set-point-x! cp 1)
(point-x cp)         ⇒  1
(point-color cp)     ⇒  #<color 1.0 0.0 0.0>
```


## Procedural API

Besides the syntactical `define-record-type` abstraction for defining record types in a declarative fashion, LispKit provides a low-level, procedural API for creating and instantiating records and record types. Record types are represented in form of _record type descriptor_ objects which itself are records.

**(record? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a record of any type; returns `#f` otherwise.

**(record-type? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a record type descriptor; returns `#f` otherwise.

**(record-type _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the record type descriptor for objects _obj_ which are records; returns `#f` for all non-record values.

**(make-record-type _name fields_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(make-record-type _name fields parent_)**   

Returns a record type descriptor which represents a new data type that is disjoint from all other types. _name_ is a string which is only used for debugging purposes, such as the printed representation of a record of the new type. _fields_ is a list of symbols naming the fields of a record of the new type. It is an error if the list contains duplicate symbols. _parent_ refers to a record type descriptor for the supertype of the newly created record type. By default, _parent_ is `#f`, i.e. the new record type does not have a parent type.

**(record-type-name _rtd_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the type name (a string) associated with the type represented by the record type descriptor _rtd_. The returned value is `eqv?` to the _name_ argument given in the call to `make-record-type` that created the type represented by _rtd_.

**(record-type-field-names _rtd_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(record-type-field-names _rtd all?_)**   

Returns a list of the symbols naming the fields in members of the type represented by the record type descriptor _rtd_. The returned value is `equal?` to the _fields_ argument given in the call to `make-record-type` that created the type represented by _rtd_. If boolean argument _all?_ is true (default is false), then a list of all symbols, also defined by parent types of _rtd_, is returned.

**(record-type-parent _rtd_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a record type descriptor for the parent type of the record type represented by the record type descriptor _rtd_. `record-type-parent` returns `#f` if _rtd_ does not have a parent type.

**(record-type-tag _rtd_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the type tag, i.e. an uninterned symbol, representing the type of records defined by _rtd_. The result of `record-type-tag` can be used together with procedure `type-of` of library `(lispkit type)`.

**(make-record _rtd_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns an uninitialized instance of the record type for which _rtd_ is the record type descriptor.

**(record-constructor _rtd fields_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a procedure for constructing new members of the type represented by the record type descriptor _rtd_. The returned procedure accepts exactly as many arguments as there are symbols in the given _fields_ list; these are used, in order, as the initial values of those fields in a new record, which is returned by the constructor procedure. The values of any fields not named in _fields_ are unspecified. It is an error if _fields_ contain any duplicates or any symbols not in the _fields_ list of the record type descriptor _rtd_.

**(record-predicate _rtd_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a procedure for testing membership in the type represented by the record type descriptor _rtd_. The returned procedure accepts exactly one argument and returns `#t` if the argument is a member of the indicated record type; it returns `#f` otherwise.

**(record-field-accessor _rtd field_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a procedure for reading the value of a particular field of a member of the type represented by the record type descriptor _rtd_. The returned procedure accepts exactly one argument which must be a record of the appropriate type; it returns the current value of the field named by the symbol _field_ in that record. The symbol _field_ must be a member of the list of field names in the call to `make-record-type` that created the type represented by _rtd_.

**(record-field-mutator _rtd field_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a procedure for writing the value of a particular field of a member of the type represented by the record type descriptor _rtd_. The returned procedure accepts exactly two arguments: first, a record of the appropriate type, and second, an arbitrary Scheme value; it modifies the field named by the symbol _field_ in that record to contain the given value. The returned value of the modifier procedure is unspecified. The symbol _field_ must be a member of the list of field names in the call to `make-record-type` that created the type represented by _rtd_.
