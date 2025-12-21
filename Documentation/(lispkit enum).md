# LispKit Enum

Library `(lispkit enum)` implements an API for enumeration types, enumeration values, and enumeration sets.

An enumeration type is defined by a list of tagged enumeration names. It encapsulates enumeration values which can be accesses either by name or ordinal value. Sets of these enumeration values are called enumeration sets. Each enumeration set is based on an enumeration type and contains a set of enumeration values.


## Declarative API

**(define-enum _type-name (symbol ...) constructor_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  
**(define-enumeration _type-name (symbol ...) constructor_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

The `define-enum` and `define-enumeration` forms defines an enumeration type and provide two macros for constructing its members and sets of its members.

_type-name_ is an identifier that is bound as a syntactic keyword; _symbol ..._ are the symbols that comprise the universe of the enumeration (in order).

`(`_type-name_ _symbol_`)` checks whether the name of _symbol_ is in the universe associated with _type-name_. If it is, `(`_type-name_ _symbol_`)` is equivalent to _symbol_. It is a syntax violation if it is not. `(`_type-name_`)` returns the type of the enumeration.

_constructor_ is an identifier that is bound to a syntactic form that, given any finite sequence of the symbols in the universe, possibly with duplicates, expands into an expression that evaluates to the enumeration set of those symbols.

`(`_constructor_ _symbol ..._`)` checks whether every _symbol_ ... is in the universe associated with _type-name_. It is a syntax violation if one or more is not. Otherwise `(`_constructor_ _symbol ..._`)` is equivalent to `((enum-set-constructor (`_constructor-syntax_`)) '(`_symbol ..._`))`.

Here is a complete example:

```scheme
(define-enumeration color
  (black white purple maroon)
  color-set)
(color black)                 ⇒ black
(color purpel)                ⇒ error: symbol not in enumeration universe
(enum-set->list (color-set))  ⇒ ()
(enum-set->list
  (color-set maroon white))   ⇒ (white maroon)
```

## Enum types

**(enum-type? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an enum type, and `#f` otherwise.

**(make-enum-type _list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-enum-type _name list_)**  
**(make-enum-type _name list tag_)**  

Returns a newly allocated enum type containing a fixed set of newly allocated enums. Both enums and enum types are immutable, and it is not possible to create an enum except as part of creating an enum type. _name_ is the name of the enumeration as a string or symbol, _tag_ is an arbitrary object attached to the enum type (which can be accessed via `enum-type-tag`).

The elements of _list_ are either symbols or two-element lists, where each list has a symbol as the first element and any value as the second element (this is the enum's tag). Each list element causes a single enum to be generated, and the enum's name is specified by the symbol. It is an error unless all the symbols are distinct within an enum type. The position of the element in _list_ is the ordinal of the corresponding enum, so ordinals within an enum type are also distinct. If a value is given, it becomes the value of the enum; otherwise the enum’s value is the same as the ordinal.

Here are a few examples:

```scheme
(define color
  (make-enum-type
    '(red orange yellow green cyan blue violet)))

(define us-traffic-light
  (make-enum-type '(red yellow green)))

(define pizza
  (make-enum-type
    '((margherita "tomato and mozzarella")
      (funghi "mushrooms")
      (chicago "deep-dish")
      (hawaiian "pineapple and ham"))))
```

**(enum-type-type-tag _enum-type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the type tag, i.e. an uninterned symbol, representing the type of enums as defined by _enum-type_.

**(enum-type-size _enum-type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an exact integer equal to the number of enums in _enum-type_.

**(enum-min _enum-type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the enum belonging to _enum-type_ whose ordinal is 0.

**(enum-max _enum-type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the enum belonging to _enum-type_ whose ordinal is equal to the number of enums in the enum type minus 1.

**(enum-type-tag _enum-type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the tag associated with _enum-type_.

**(enum-type-enums _enum-type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the enums belonging to _enum-type_ ordered by increasing ordinal.

**(enum-type-names _enum-type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the names of the enums belonging to _enum-type_ ordered by increasing ordinal.

**(enum-type-tags _enum-type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the values of the enums belonging to _enum-type_ ordered by increasing ordinal.

**(enum-type-contains? _enum-type enum_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _enum_ belongs to _enum-type_, and `#f` otherwise.

```scheme
(enum-type-contains? color
  (enum-name->enum color 'red)) ⇒ #t
(enum-type-contains? pizza
  (enum-name->enum color 'red)) ⇒ #f
```

**(enum-type-test-predicate _enum-type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a procedure which given an object, checks that this object is an enum that belongs to _enum-type_.

**(enum-set-type-test-predicate _enum-type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a procedure which given an object, checks that this object is an enum set whose type matches _enum-type_.

**(enum-name-\>enum _enum-type symbol_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

If there exists an enum belonging to _enum-type_ named _symbol_, returns it; otherwise return `#f`.

**(enum-name-\>ordinal _enum-type symbol_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the ordinal of the enum belonging to _enum-type_ whose name is _symbol_. It is an error if there is no such enum.

**(enum-name-\>tag _enum-type symbol_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the value of the enum belonging to _enum-type_ whose name is _symbol_. It is an error if there is no such enum.

**(enum-ordinal-\>enum _enum-type exact-integer_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

If there exists an enum belonging to _enum-type_ whose ordinal is _exact-integer_, returns it; otherwise return `#f`.

**(enum-ordinal-\>name _enum-type exact-integer_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the name of the enum belonging to _enum-type_ whose ordinal is _exact-integer_. It is an error if there is no such enum.

**(enum-ordinal-\>tag _enum-type exact-integer_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the value of the enum belonging to _enum-type_ whose ordinal is _exact-integer_. It is an error if there is no such enum.

**(enum-tag-mapper _enum-type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(enum-tag-mapper _enum-type makeht_)**  
**(enum-tag-mapper _enum-type hash equal_)**  

Returns a procedure that maps enum tags to enum values for the given enumeration type _enum-type_. The returned procedure accepts a tag and returns the corresponding enum value, or `#f` if not found. _makeht_ is an optional procedure that creates a hashtable from an association list (defaults to `alist->equal-hashtable`). Alternatively, _hash_ and _equal_ procedures can be provided to create a custom hashtable for the mapping.

**(enum-type-\>enum-set _enum-type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an enum set containing all enum values from enumeration type _enum-type_.

## Enum values

**(enum? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an enum, and `#f` otherwise.

**(enum-type _enum_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the enum type to which _enum_ belongs.  

**(enum-name _enum_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the name (symbol) associated with _enum_.

**(enum-ordinal _enum_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the ordinal (exact integer) associated with _enum_.

**(enum-tag _enum_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the tag associated with _enum_.

**(enum-next _enum_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the enum that belongs to the same enum type as _enum_ and has an ordinal one greater than _enum_. Returns `#f` if there is no such enum.

```scheme
(enum-name (enum-next color-red)) ⇒ orange
(enum-next (enum-max color))      ⇒ #f
```

**(enum-prev _enum_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the enum that belongs to the same enum type as _enum_ and has an ordinal one less than _enum_. Returns `#f` if there is no such enum.  

**(enum=? _enum0 enum1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if all the arguments are the same enum in the sense of `eq?` (which is equivalent to having the same name and ordinal) and `#f` otherwise. It is an error to apply `enum=?` to enums belonging to different enum types.

```scheme
(enum=? color-red color-blue)      ⇒ #f
(enum=? pizza-funghi
  (enum-name->enum pizza 'funghi)) ⇒ #t  
(enum=? color-red
  (enum-name->enum color 'red)
  color-blue)                      ⇒ #f  
```

**(enum\<? _enum0 enum1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(enum\>? _enum0 enum1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(enum\<=? _enum0 enum1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(enum\>=? _enum0 enum1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

These predicates return `#t` if their arguments are enums whose ordinals are in increasing, decreasing, non-decreasing, and non-increasing order respectively, and `#f` otherwise. It is an error unless all of the arguments belong to the same enum type.  


## Enum sets

**(enum-set? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an enum set and `#f` otherwise.

**(enum-set _enum-type enum ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an enum set that can contain enums of the type _enum-type_ and containing the _enums_. It is an error unless all the _enums_ belong to _enum-type_.

```scheme
(enum-set-contains?
  (enum-set color color-red color-blue)
  color-red)     ⇒  #t
(enum-set-contains?
  (enum-set color color-red color-blue)
  color-orange)  ⇒  #f
```

**(list-\>enum-set _enum-type list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an enum set with the specified _enum-type_ that contains the members of _list_. _list_ may contain enums, enum names, or enum ordinals. It is an error unless all the members refer to enums belonging to _enum-type_.

**(enum-set-copy _enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of _enum-set_.

**(enum-set-empty? _enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _enum-set_ is empty, and `#f` otherwise.

**(enum-set-contains? _enum-set enum_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _enum_ is a member of _enum-set_. It is an error if _enum_ does not belong to the same enum type as the members of _enum-set_.

**(enum-set-disjoint? _enum-set1 enum-set2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _enum-set1_ and _enum-set2_ do not have any enum objects in common, and `#f` otherwise.

```scheme
(define reddish
  (list->enum-set
    (map (lambda (name) (enum-name->enum color name))
         '(red orange))))
(define ~reddish
  (list->enum-set
    (map (lambda (name) (enum-name->enum color name))
         '(yellow green cyan blue violet))))
(enum-set-disjoint? color-set reddish) ⇒ #f
(enum-set-disjoint? reddish ~reddish)  ⇒ #t
```

**(enum-set-projection _enum-set1 enum-set2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Projects _enum-set1_ into the universe of _enum-set2_, dropping any elements of _enum-set1_ that do not belong to the universe of _enum-set2_. If _enum-set1_ is a subset of the universe of its second, no elements are dropped, and the injection is returned.

```scheme
(let ((e1 (make-enumeration '(red green blue black)))
      (e2 (make-enumeration '(red black white))))
  (enum-set->list (enum-set-projection e1 e2))))
⇒ (red black)
```

**(enum-set-member? _symbol enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(enum-set-subset? _enum-set1 enum-set2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `enum-set-member?` procedure returns `#t` if its first argument is an element of its second argument, `#f` otherwise.

The `enum-set-subset?` procedure returns `#t` if the universe of _enum-set1_ is a subset of the universe of _enum-set2_ (considered as sets of symbols) and every element of _enum-set1_ is a member of _enum-set2_. It returns `#f` otherwise.

```scheme
(let* ((e (make-enumeration '(red green blue)))
       (c (enum-set-constructor e)))
  (list (enum-set-member? 'blue (c '(red blue)))
        (enum-set-member? 'green (c '(red blue)))
        (enum-set-subset? (c '(red blue)) e)
        (enum-set-subset? (c '(red blue)) (c '(blue red)))
        (enum-set-subset? (c '(red blue)) (c '(red)))
        (enum-set=? (c '(red blue)) (c '(blue red)))))
⇒ (#t #f #t #t #f #t)
```

**(enum-set=? _enum-set1 enum-set2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the members of _enum-set-1_ are the same as of _enum-set-2_. It is an error if the members of the enum sets do not belong to the same type.

**(enum-set\<? _enum-set1 enum-set2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the members of _enum-set-1_ are a proper subset of _enum-set-2_. It is an error if the members of the enum sets do not belong to the same type.

**(enum-set\>? _enum-set1 enum-set2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the members of _enum-set-1_ are a proper superset of _enum-set-2_. It is an error if the members of the enum sets do not belong to the same type.

**(enum-set\<=? _enum-set1 enum-set2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the members of _enum-set-1_ are a subset of _enum-set-2_. It is an error if the members of the enum sets do not belong to the same type.

**(enum-set\>=? _enum-set1 enum-set2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the members of _enum-set-1_ are a superset of _enum-set-2_. It is an error if the members of the enum sets do not belong to the same type.

**(enum-set-\>list _enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the names of enums that belong to _enum-set_. The list is in increasing order of the enums.

```scheme
(let* ((e (make-enumeration '(red green blue)))
       (c (enum-set-constructor e)))
  (enum-set->list (c '(blue red))))
⇒ (red blue)
```

**(enum-set-\>enum-list _enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list containing the enum members of _enum-set_. The list is in increasing order of the enums.

```scheme
(let* ((e (make-enumeration '(red green blue)))
       (c (enum-set-constructor e)))
  (enum-set->enum-list (c '(blue red))))
⇒ (#<enum enum-3: 0> #<enum enum-3: 2>)
```

**(enum-set-next _enum-set e_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the ordinally next enum in _enum-set_ following enum _e_. _e_ is either a name, ordinal, or enum value. `enum-set-next` returns `#f` if there is no next enum.

**(enum-set-type _enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the enum type associated with _enum-set_.

**(enum-set-bitset _enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a bit set (as defined by library `(lispkit bitset)`) representing all ordinals of _enum-set_.

**(enum-set-size _enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of elements in _enum-set_.

**(enum-set-adjoin! _enum-set e ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Adds enums _e ..._ to _enum-set_. Enums are defined either via a name, an ordinal, or an enum object. It is an error if the enums denoted by _e ..._ do not all belong to the same enum type.

**(enum-set-adjoin-all! _enum-set list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

_list_ is a list of enums. Enums are defined either via a name, an ordinal, or an enum object. `enum-set-adjoin-all!` adds all enums of _list_ to _enum-set_. It is an error if the enums denoted by _list_ do not all belong to the same enum type.

**(enum-set-delete! _enum-set e ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes enums _e ..._ from _enum-set_. Enums are defined either via a name, an ordinal, or an enum object. It is an error if the enums denoted by _e ..._ do not all belong to the same enum type.

**(enum-set-delete-all! _enum-set list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

_list_ is a list of enums. Enums are defined either via a name, an ordinal, or an enum object. `enum-set-delete-all!` removes all enums of _list_ from _enum-set_. It is an error if the enums denoted by _list_ do not all belong to the same enum type.

**(enum-set-union _enum-set1 enum-set2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(enum-set-intersection _enum-set1 enum-set2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(enum-set-difference _enum-set1 enum-set2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Arguments _enum-set1_ and _enum-set2_ must be enumeration sets that have the same enumeration type.

The `enum-set-union` procedure returns the union of _enum-set1_ and _enum-set2_. The `enum-set-intersection` procedure returns the intersection of _enum-set1_ and _enum-set2_. The `enum-set-difference` procedure returns the difference of _enum-set1_ and _enum-set2_.

```scheme
(let* ((e (make-enumeration '(red green blue)))
       (c (enum-set-constructor e)))
  (list (enum-set->list (enum-set-union (c '(blue)) (c '(red))))
        (enum-set->list
          (enum-set-intersection (c '(red green)) (c '(red blue))))
        (enum-set->list
         (enum-set-difference (c '(red green)) (c '(red blue))))))
⇒ ((red blue) (red) (green))
```

**(enum-set-xor _enum-set1 enum-set2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Procedure `enum-set-xor` returns the exclusive disjunction of _enum-set1_ and _enum-set2_. Arguments _enum-set1_ and _enum-set2_ must be enumeration sets that have the same enumeration type.

**(enum-set-complement _enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns _enum-set_'s complement with respect to its universe.

```scheme
(let* ((e (make-enumeration '(red green blue)))
       (c (enum-set-constructor e)))
  (enum-set->list (enum-set-complement (c '(red)))))
⇒ (green blue)
```

**(enum-set-union! _enum-set1 enum-set2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates the union of _enum-set1_ and _enum-set2_ and stores its result in _enum-set1_. _enum-set1_ and _enum-set2_ must be enumeration sets that have the same enumeration type. 

**(enum-set-intersection! _enum-set1 enum-set2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates the intersection of _enum-set1_ and _enum-set2_ and stores its result in _enum-set1_. _enum-set1_ and _enum-set2_ must be enumeration sets that have the same enumeration type. 

**(enum-set-difference! _enum-set1 enum-set2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates the set difference between _enum-set1_ and _enum-set2_ and stores its result in _enum-set1_. _enum-set1_ and _enum-set2_ must be enumeration sets that have the same enumeration type.

**(enum-set-xor! _enum-set1 enum-set2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates the exclusive disjunction between _enum-set1_ and _enum-set2_ and stores its result in _enum-set1_. _enum-set1_ and _enum-set2_ must be enumeration sets that have the same enumeration type. 

**(enum-set-complement! _enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Replaces _enum-set_ with its complement with respect to the type of _enum-set_.

**(enum-set-indexer _enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a unary procedure that, given a symbol that is in the universe of _enum-set_, returns its 0-origin index within the canonical ordering of the symbols in the universe; given a value not in the universe, the unary procedure returns `#f`.

```scheme
(let* ((e (make-enumeration '(red green blue)))
       (i (enum-set-indexer e)))
  (list (i 'red) (i 'green) (i 'blue) (i 'yellow)))
 ⇒ (0 1 2 #f)
```

The `enum-set-indexer` procedure could be defined as follows using the `memq` procedure:

```scheme
(define (enum-set-indexer set)
  (let* ((symbols (enum-set->list (enum-set-universe set)))
         (cardinality (length symbols)))
    (lambda (x)
      (cond ((memq x symbols) =>
              (lambda (probe) (- cardinality (length probe))))
            (else #f)))))
```

**(enum-set-any? _pred enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if any application of _pred_ to the elements of _enum-set_ returns true, and `#f` otherwise.

**(enum-set-every? _pred enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if every application of _pred_ to the elements of _enum-set_ returns true, and `#f` otherwise.

**(enum-set-count _pred enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an exact integer, the number of elements of _enum-set_ that satisfy _pred_.

**(enum-set-map-\>list _proc enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Invokes _proc_ on each member of _enum-set_ in increasing ordinal order. The results are returned as a list.

**(enum-set-for-each _proc enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Invokes _proc_ on each member of _enum-set_ in increasing ordinal order and discards the rest.

**(enum-set-fold _proc nil enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The current state is initialized to _nil_, and _proc_ is invoked on each element of _enum-set_ in increasing ordinal order and the current state, setting the current state to the result. The algorithm is repeated until all the elements of _enum-set_ have been processed. Then the current state is returned.

**(enum-set-filter _pred enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an enum set containing the enums in _enum-set_ that satisfy _pred_.

**(enum-set-remove _pred enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an enum set containing the enums in _enum-set_ that do not satisfy _pred_.


## R6RS Compatibility

**(make-enumeration _symbol-list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Argument _symbol-list_ must be a list of symbols. The `make-enumeration` procedure creates a new enumeration type whose universe consists of those symbols (in canonical order of their first appearance in the list) and returns that universe as an enumeration set whose universe is itself and whose enumeration type is the newly created enumeration type.

**(enum-set-universe _enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the set of all symbols that comprise the universe of its argument _enum-set_, as an enumeration set.

**(enum-set-constructor _enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a unary procedure that, given a list of symbols that belong to the universe of _enum-set_, returns a subset of that universe that contains exactly the symbols in the list. The values in the list must all belong to the universe.

**(enum-constructor _enum-set_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Given an enum set, `enum-constructor` returns a procedure which takes an enum name and returns the corresponding enum object.
