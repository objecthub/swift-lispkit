# LispKit Type

Library `(lispkit type)` provides a simple, lightweight type abstraction mechanism. It allows for creating new types at runtime that are disjoint from all existing types. The library provides two different types of APIs: a purely procedural API for type creation and management, as well as a declarative API which allows for introducing extensible types in a declarative fashion.

A core feature of the type abstraction mechanism is a means that allows for determining the type of any LispKit value. Procedure `type-of` implements this type introspection feature. It returns a _type tag_, i.e. a symbol representing the type, for any given object.


## Usage of the procedural API

New types are created with procedure `make-type`. `make-type` accepts one argument, which is a type label. The type label is either a string or a symbol that is used for debugging purposes but also for defining the string representation of a _type tag_ that represents the new type.

The following line introduces a new type for _intervals_:

```scheme
(define-values (interval-type-tag
                new-interval
                interval?
                interval-ref
                make-interval-subtype)
  (make-type 'interval))
```

`(make-type 'interval)` returns five values:

   - `interval-type-tag` is an uninterned symbol with the string representation `"interval"` which represents the new type.
   - `new-interval` is a procedure which takes one argument, the internal representation of the interval, and returns a new object of the new interval type.
   - `interval?` is a type test predicate which accepts one argument and returns `#t` if the argument is of the new interval type, and `#f` otherwise.
   - `interval-ref` takes one object of the new interval type and returns its internal representation. `interval-ref` is the inverse operation of `new-interval`.
   - `make-interval-subtype` is a type generator (similar to `make-type`), a function that takes a type label and returns five values representing a new subtype of the interval type.

Now it is possible to implement a constructor `make-interval` for intervals:

```scheme
(define (make-interval lo hi)
  (if (and (real? lo) (real? hi) (<= lo hi))
      (new-interval (cons (inexact lo) (inexact hi)))
      (error "make-interval: illegal arguments" lo hi)))
```

`make-interval` first checks that the constructor arguments are valid and then calls `new-interval` to create a new interval object. Interval objects are represented via pairs whose _car_ is the lower bound, and _cdr_ is the upper bound. Nevertheless, pairs and interval objects are distinct values as the following code shows:

```scheme
(define interval-obj (make-interval 1.0 9.5))
(define pair-obj (cons 1.0 9.5))

(interval? interval-obj)        ⇒ #t
(interval? pair-obj)            ⇒ #f
(equal? interval-obj pair-obj)  ⇒ #f
```

The type is displayed along with the representation in the textual representation of interval objects: `#interval:(1.0 . 9.5)`.

Below are a few functions for interval objects. They all use `interval-ref` to extract the internal representation from an interval object and then operate on the internal representation.

```scheme
(define (interval-length interval)
  (let ((bounds (interval-ref interval)))
    (- (cdr bounds) (car bounds))))

(define (interval-empty? interval)
  (zero? (interval-length interval)))
```

The following function calls show that `interval-ref` fails with a type error if its argument is not an interval object.

```scheme
(interval-length interval-obj)
⇒ 8.5
(interval-empty? '(1.0 . 1.0))
⇒ [error] not an instance of type interval: (1.0 . 1.0)
```


## Usage of the declarative API

The procedural API provides the most flexible way to define a new type in LispKit. On the other hand, this approach comes with two problems:

   1. a lot of boilerplate needs to be written, and
   2. programmers need to be experienced to correctly encapsulate new data types and to provide means to extend them.

These problems are addressed by the declarative API of `(lispkit type)`. At the core, this API defines a syntax `define-type` for declaring new types of data. `define-type` supports defining simple, encapsulated types as well as provides a means to make types extensible.

The syntax for defining a simple, non-extensible type has the following form:

`(define-type` _name_ _name?_   
&nbsp;&nbsp;&nbsp; `((`_make-name_ _x_ ...`)` _expr_ ...`)`   
&nbsp;&nbsp;&nbsp; _name-ref_   
&nbsp;&nbsp;&nbsp; _functions_`)`

_name_ is a symbol. It defines the name of the new type and the identifier _name_ is bound to a type tag representing the new type. _name?_ is a predicate for testing whether a given object is of type _name_. _make-name_ defines a constructor which returns a value representing the data of the new type. _name-ref_ is a function to unwrap values of type _name_. It is optional and normally not needed since _functions_ can be declared such that the unwrapping happens implicitly. All functions defined via `define-type` take an object (usually called `self`) of the defined type as their first argument.

There are two forms to declare a function as part of `define-type`: one providing access to `self` directly, and one only providing access to the unwrapped data value:

`((`_name-func self y ..._`)` _expr_ ...`)`

provides access directly to _self_ (which is a value of type _name_), and

`((`_name-func_ `(`_repr_`)` _y ..._`)` _expr_ ...`)`

which provides access only to the unwrapped data `repr`.

With this new syntax, type `interval` from the section describing the procedural API, can now be re-written like this:

```scheme
(define-type interval
  interval?
  ((make-interval lo hi)
    (if (and (real? lo) (real? hi) (<= lo hi))
        (cons (inexact lo) (inexact hi))
        (error "make-interval: illegal arguments" lo hi)))
  ((interval-length (bounds))
    (- (cdr bounds) (car bounds)))
  ((interval-empty? self)
    (zero? (interval-length self))))
```

`interval` is a standalone type which cannot be extended. `define-type` provides a simple means to make types extensible such that subtypes can be created reusing the base type definition. This is done with a small variation of the `define-type` syntax:

`(define-type` (_name_ _super_) _name?_   
&nbsp;&nbsp;&nbsp; `((`_make-name_ _x_ ...`)` _expr_ ...`)`   
&nbsp;&nbsp;&nbsp; _name-ref_   
&nbsp;&nbsp;&nbsp; _functions_`)`

In this syntax, _super_ refers to the type extended by _name_. All extensible types extend another extensible type and there is one supertype called `obj` provided by library `(lispkit type)` as a primitive.

With this syntactic facility, `interval` can be easily re-defined to be extensible:

```scheme
(define-type (interval obj)
  interval?
  ((make-interval lo hi)
    (if (and (real? lo) (real? hi) (<= lo hi))
        (cons (inexact lo) (inexact hi))
        (error "make-interval: illegal arguments" lo hi)))
  ((interval-length (bounds))
    (- (cdr bounds) (car bounds)))
  ((interval-empty? self)
    (zero? (interval-length self))))
```

It is now possible to define a `tagged-interval` data structure which inherits all functions from `interval` and encapsulates a tag with the interval:

```scheme
(define-type (tagged-interval interval)
  tagged-interval?
  ((make-tagged-interval lo hi tag)
    (values lo hi tag))
  ((interval-tag (bounds tag))
    tag))
```

`tagged-interval` is a subtype of `interval`; i.e. values of type `tagged-interval` are also considered to be of type `interval`. Thus, `tagged-interval` inherits all function definitions from `interval` and defines a new function `interval-tag` just for `tagged-interval` values. Here is some code explaining the usage of `tagged-interval`:

```scheme
(define ti (make-tagged-interval 4.0 9.0 'inclusive))
(tagged-interval? ti)       ⇒ #t
(interval? ti)              ⇒ #t
(interval-length ti)        ⇒ 5.0
(interval-tag ti)           ⇒ inclusive
(interval-tag interval-obj)
⇒ [error] not an instance of type tagged-interval: #interval:((1.0 . 9.5))
```

Constructors of extended types, such as `make-tagged-interval` return multiple values: all the parameters for a super-constructor call and one additional value (the last value) representing the data provided by the extended type. In the example above, `make-tagged-interval` returns three values: `lo`, `hi`, and `tag`. After the constructor `make-tagged-interval` is called, the super-constructor is invoked with arguments `lo` and `hi`. The result of `make-tagged-interval` is a `tagged-interval` object consisting of two state values contained in a list: one for the supertype `interval` (consisting of the bounds `(lo . hi)`) and one for the subtype `tagged-interval` (consisting of the tag). This can also be seen when displaying a `tagged-interval` value:

```scheme
ti ⇒ #<tagged-interval (4.0 . 9.0) inclusive>
```

This is also the reason why function `interval-tag` gets access to two unwrapped values, `bounds` and `tag`: one (`bounds`) corresponds to the value associated with type `interval`, and the other one (`tag`) corresponds to the value associated with type `tagged-interval`.


## Type introspection

LispKit defines a type tag for every different type of object. The type of objects created with the `make-type` facility are represented with unintered symbols whose string representation matches the type label provided to `make-type`. Enum and record objects come with a corresponding type tag as well. All standard, built-in types use interned symbols as type tags. Procedure `type-of` returns the type tag associated with the value of the argument of `type-of`.

**(type-of _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of type tags, i.e. symbols, associated with the type of _obj_. The type tags in the list are sorted, starting with the most specific type. If no type can be determined, `type-of` returns the empty list.

Type tags of custom types are returned as the first value of `make-type`. Type tags of records can be retrieved via `record-type-tag` from the corresponding record type object. Similarly, type tags of enum objects are accessible via `enum-type-type-tag`. Type tags of native types are typically exported by the libraries providing access to the type. All standard, built-in types are represented by the following interned symbols: `void`, `end-of-file`, `null`, `boolean`, `symbol`, `fixnum`, `bignum`, `integer`, `rational`, `flonum`, `real`, `complex`, `number`, `char`, `string`, `bytevector`, `pair`, `list`, `box`, `mpair`, `array`, `vector`, `gvector`, `values`, `procedure`, `parameter`, `promise`, `environment`, `hashtable`, `port`, `input-port`, `output-port`, `record-type`, and `error`. For undefined values `#f` is returned.


## Type management

**(make-type _type-label_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Based on a string or symbol _type-label_, creates a new, unique type, and returns a type tag representing the new type as well as five values dealing with this new type:

  1. The first value is a type tag, i.e. an uninterned symbol representing the new type which has the same string representation as _type-label_.
  2. The second value is a unary procedure returning a new object of the new type which is wrapping the argument of the procedure (i.e. the internal representation of the new type).
  2. The third value is a type test predicate which accepts one argument and returns `#t` if the argument is of the new type, and `#f` otherwise.
  3. The fourth value is a procedure which takes one object of the new type and returns its internal representation (that was passed to the procedure returned as the second value).
  4. The fifth value is a type generator procedure (similar to `make-type`), a function that takes a type label and returns five values representing a new subtype of the new type.

_type-label_ has two uses: it is used for debugging purposes and determines the string representation of the corresponding type tag. It is shown when an object's textual representation is used. In particular, calling the third procedure (the type de-referencing function) will result in an error message exposing the type label if the argument is of a different type than expected.

**(define-type _name name?_ ((_make-name x ..._) _e ..._) _func_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  
**(define-type _name name?_ ((_make-name x ..._) _e ..._) _ref_ _func_ ...)**

Defines a new standalone type _name_ consisting of a type test predicate _name?_, a constructor _make-name_, and an optional function _ref_ used to unwrap values of type _name_. _ref_ is optional and normally not needed since functions _func_ can be declared such that the unwrapping happens implicitly. All functions _func_ defined via `define-type` take an object (usually called `self`) of the defined type as their first argument.

There are two ways to declare a function as part of `define-type`: one providing access to `self` directly, and one only providing access to the unwrapped data value:

   - `((`_name-func self y ..._`)` _expr_ ...`)` provides access directly to _self_ (which is a value of type _name_), and
   - `((`_name-func_ `(`_repr_`)` _y ..._`)` _expr_ ...`)` provides access only to the unwrapped data `repr`.

**(define-type (_name super_) _name?_ ((_make-name x ..._) _e ..._) _func_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  
**(define-type (_name super_) _name?_ ((_make-name x ..._) _e ..._) _ref_ _func_ ...)**

This variant of `define-type` defines a new extensible type _name_ extending supertype _super_, which also needs to be an extensible type. A new extensible type _name_ comes with a type test predicate _name?_, a constructor _make-name_, and an optional function _ref_ used to unwrap values of type _name_. _ref_ is optional and normally not needed since functions _func_ can be declared such that the unwrapping happens implicitly. All functions _func_ defined via `define-type` take an object (usually called `self`) of the defined type as their first argument.

There are two ways to declare a function as part of `define-type`: one providing access to `self` directly, and one providing access to the unwrapped data values (one for each type in the supertype chain):

   - `((`_name-func self y ..._`)` _expr_ ...`)` provides access directly to _self_ (which is a value of type _name_), and
   - `((`_name-func_ `(`_repr_ ...`)` _y ..._`)` _expr_ ...`)` provides access only to the unwrapped data values `repr`.

Constructors of extended types return multiple values: all the parameters for a super-constructor call and one additional value (the last value) representing the data provided by the extended type.

**obj** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[constant]</span>   

The supertype of all extensible types defined via `define-type`. The type tag of `obj` can be retrieved via `(type-of obj)`.

**(obj-type-tag _etype_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[constant]</span>  

Returns the type tag associated with the supertype of all extensible types `obj`.

**(extensible-type? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an instance of an extensible type. For example, `(extensible-type? obj)` returns `#t`.

**(extensible-type-tag _etype_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the type tag associated with the extensible type `etype` defined by the `define-type` form.
