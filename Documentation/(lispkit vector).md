# LispKit Vector

Vectors are heterogeneous data structures whose elements are indexed by a range of integers. A vector typically occupies less space than a list of the same length, and a randomly chosen element can be accessed in constant time vs. linear time for lists.

The _length_ of a vector is the number of elements that it contains. This number is a non-negative integer that is fixed when the vector is created. The valid indexes of a vector are the exact, non-negative integers less than the length of the vector. The first element in a vector is indexed by zero, and the last element is indexed by one less than the length of the vector.

Two vectors are `equal?` if they have the same length, and if the values in corresponding slots of the vectors are `equal?`.

A vector can be _mutable_ or _immutable_. Trying to change the state of an _immutable vector_, e.g. via `vector-set!` will result in an error being raised.

Vectors are written using the notation `#(obj ...)`. For example, a vector of length 3 containing the number zero in element 0, the list (1 2 3 4) in element 1, and the string "Lisp" in element 2 can be written as follows: `#(0 (1 2 3 4) "Lisp")`.

Vector constants are self-evaluating, so they do not need to be quoted in programs. Vector constants, i.e. vectors created with a vector literal, are _immutable_.

LispKit also supports _growable vectors_ via library `(lispkit gvector)`. As opposed to regular vectors, a growable vector does not have a fixed size and supports adding and removing elements. While a growable vector does not satisfay the `vector?` predicate, this library also accepts growable vectors as parameters whenever a vector is expected. Use predicate `mutable-vector?` for determining whether a vector is either a regular mutable vector or a growable vector.

## Predicates

**(vector? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a regular vector; otherwise returns `#f`. This function returns `#f` for growable vectors; see library `(lispkit gvector)`.

**(mutable-vector? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is either a mutable regular vector or a growable vector (see library `(lispkit gvector)`); otherwise returns `#f`.

**(immutable-vector? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an immutable vector; otherwise returns `#f`.

**(vector= _eql vector ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Procedure `vector=` is a generic comparator for vectors. Vectors _a_ and _b_ are considered equal by `vector=` if their lengths are the same, and for each respective elements _ai_ and _bi_, `(eql ai bi)` evaluates to true. _eql_ is always applied to two arguments.

If there are only zero or one vector argument, `#t` is automatically returned. The dynamic order in which comparisons of elements and of vectors are performed is unspecified.

```scheme
(vector= eq? #(a b c d) #(a b c d))  ⇒  #t 
(vector= eq? #(a b c d) #(a b d c))  ⇒  #f 
(vector= = #(1 2 3 4 5) #(1 2 3 4))  ⇒  #f 
(vector= = #(1 2 3 4) #(1.0 2.0 3.0 4.0))  ⇒  #t 
(vector= eq?) ⇒  #t 
(vector= eq? '#(a))  ⇒  #t 
```

## Constructors

**(make-vector _k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-vector _k fill_)**  

Returns a newly allocated vector of _k_ elements. If a second argument is given, then each element is initialized to _fill_. Otherwise the initial contents of each element is unspecified.

**(vector _obj ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a newly allocated mutable vector whose elements contain
the given arguments. It is analogous to `list`.

```scheme
(vector ’a ’b ’c)  ⇒  #(a b c)
```

**(immutable-vector _obj ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a newly allocated immutable vector whose elements contain
the given arguments in the given order.

**(list->vector _list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `list->vector` procedure returns a newly created mutable vector initialized to the elements of the list _list_ in the order of the list.

```scheme
(list->vector ’(a b c))  ⇒  #(a b c)
```

**(list->immutable-vector _list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `list->vector` procedure returns a newly created immutable vector initialized to the elements of the list _list_ in the order of the list.

**(string->vector _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(string->vector _str start_)**  
**(string->vector _str start end_)**  

The `string->vector` procedure returns a newly created mutable vector initialized to the elements of the string _str_ between _start_ and _end_ (i.e. including all characters from index _start_ to index _end_-1).

```scheme
(string->vector "ABC")  ⇒  #(#\A #\B #\C)
```

**(vector-copy _vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(vector-copy _vector mutable_)**  
**(vector-copy _vector start_)**  
**(vector-copy _vector start end_)**  
**(vector-copy _vector start end mutable_)**  

Returns a newly allocated copy of the elements of the given vector between _start_ and _end_, but excluding the element at index _end_. The elements of the new vector are the same (in the sense of `eqv?`) as the elements of the old.

_mutable_ is a boolean argument. If it is set to `#f`, an immutable copy of _vector_ will be created. The type of the second argument of `vector-copy` is used to disambiguate between the second and third version of the function. An exact integer will always be interpreted as _start_, a boolean value will always be interpreted as _mutable_.

```scheme
(define a #(1 8 2 8))         ; a may be immutable
(define b (vector-copy a))    ; creates a mutable copy of a
(vector-set! b 0 3)           ; b is mutable
b  ⇒  #(3 8 2 8)
(define c (vector-copy a #f)) ; creates an immutable copy of a
(vector-set! c 0 3)  ⇒  error  ; error, since c is immutable
(define d (vector-copy b 1 3))
d  ⇒  #(8 2)
```

**(vector-append _vector ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a newly allocated mutable vector whose elements are the concatenation of the elements of the given vectors.

```scheme
(vector-append #(a b c) #(d e f))  ⇒  #(a b c d e f)
```

**(vector-concatenate _vector xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a newly allocated mutable vector whose elements are the concatenation of the elements of the vectors in _xs_. _xs_ is a proper list of vectors. 

```scheme
(vector-concatenate '(#(a b c) #(d) #(e f)))  ⇒  #(a b c d e f)
```

**(vector-map _f vector1 vector2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Constructs a new mutable vector of the shortest size of the vector arguments _vector1_, _vector2_, etc. Each element at index _i_ of the new vector is mapped from the old vectors by `(f (vector-ref vector1 i) (vector-ref vector2 i) ...)`. The dynamic order of the application of f is unspecified. 

```scheme
(vector-map + #(1 2 3 4 5) #(10 20 30 40))  ⇒  #(11 22 33 44)
```

**(vector-map/index _f vector1 vector2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Constructs a new mutable vector of the shortest size of the vector arguments _vector1_, _vector2_, etc. Each element at index _i_ of the new vector is mapped from the old vectors by `(f i (vector-ref vector1 i) (vector-ref vector2 i) ...)`. The dynamic order of the application of f is unspecified. 

```scheme
(vector-map/index (lambda (i x y) (cons i (+ x y))) #(1 2 3) #(10 20 30))
⇒  #((0 . 11) (1 . 22) (2 . 33))
```

**(vector-sort _pred vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(vector-sort _pred vector start_)**  
**(vector-sort _pred vector start end_)**  

Procedure `vector-sort` returns a new vector containing the elements of _vector_ in sorted order using _pred_ as the "less than" predicate. If _start_ and _end_ are given, they indicate the sub-vector that should be sorted.

```scheme
(vector-sort < (vector 7 4 9 1 2 8 5))
⇒  #(1 2 4 5 7 8 9)
```

## Iterating over vectors

**(vector-for-each _f vector1 vector2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`vector-for-each` implements a simple vector iterator: it applies _f_ to the corresponding list of parallel elements from _vector1 vector2 ..._ in the range _[0, length)_, where _length_ is the length of the smallest vector argument passed. In contrast with `vector-map`, _f_ is reliably applied to each subsequent element, starting at index 0, in the vectors.

```scheme
(vector-for-each (lambda (x) (display x) (newline)) 
                 #("foo" "bar" "baz" "quux" "zot"))
⇒
foo
bar
baz
quux
zot
```

**(vector-for-each/index _f vector1 vector2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`vector-for-each/index` implements a simple vector iterator: it applies _f_ to the index _i_ and the corresponding list of parallel elements from _vector1 vector2 ..._ in the range _[0, length)_, where _length_ is the length of the smallest vector argument passed. The only difference to `vector-for-each` is that `vector-for-each/index` always passes the current index as the first argument of _f_ in addition to the elements from the vectors _vector1 vector2 ..._.

```scheme
(vector-for-each/index
  (lambda (i x) (display i)(display ": ")(display x)(newline))
  #("foo" "bar" "baz" "quux" "zot"))
⇒
0: foo
1: bar
2: baz
3: quux
4: zot
```

## Managing vector state

**(vector-length _vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of elements in _vector_ as an exact integer.

**(vector-ref _vector k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `vector-ref` procedure returns the contents of element _k_ of _vector_. It is an error if _k_ is not a valid index of _vector_.

```scheme
(vector-ref ’#(1 1 2 3 5 8 13 21) 5) ⇒  8
(vector-ref ’#(1 1 2 3 5 8 13 21) (exact (round (* 2 (acos -1)))))  ⇒  13
```

**(vector-set! _vector k obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `vector-set!` procedure stores _obj_ in element _k_ of _vector_.
It is an error if _k_ is not a valid index of _vector_. 

```scheme
(let ((vec (vector 0 '(2 2 2 2) "Anna")))
  (vector-set! vec 1 '("Sue" "Sue"))
  vec)
  ⇒  #(0 ("Sue" "Sue") "Anna")
(vector-set! '#(0 1 2) 1 "doe")
  ⇒  error    ;; constant/immutable vector
```

**(vector-swap! _vector j k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `vector-swap!` procedure swaps the element _j_ of _vector_ with the element _k_ of _vector_.

## Destructive vector operations

Procedures which operate only on a part of a vector specify the applicable range in terms of an index interval [_start_; _end_[; i.e. the _end_ index is always exclusive.

**(vector-copy! _to at from_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(vector-copy! _to at from start_)**  
**(vector-copy! _to at from start end_)**  

Copies the elements of vector _from_ between _start_ and _end_ to vector _to_, starting at _at_. The order in which elements are copied is unspecified, except that if the source and destination overlap, copying takes place as if the source is first copied into a temporary vector and then into the destination. _start_ defaults to 0 and _end_ defaults to the length of _vector_.

It is an error if _at_ is less than zero or greater than the length of _to_. It is also an error if `(- (vector-length to) at)` is less than `(- end start)`.

```scheme
(define a (vector 1 2 3 4 5))
(define b (vector 10 20 30 40 50)) (vector-copy! b 1 a 0 2)
b  ⇒  #(10 1 2 40 50)
```

**(vector-fill! _vector fill_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(vector-fill! _vector fill start_)**  
**(vector-fill! _vector fill start end_)**  

The `vector-fill!` procedure stores _fill_ in the elements of _vector_ between _start_ and _end_. _start_ defaults to 0 and _end_ defaults to the length of _vector_.

```scheme
(define a (vector 1 2 3 4 5))
(vector-fill! a ’smash 2 4)
a  ⇒  #(1 2 smash smash 5)
```

**(vector-reverse! _vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(vector-reverse! _vector start_)**  
**(vector-reverse! _vector start end_)**  

Procedure `vector-reverse!` destructively reverses the contents of _vector_ between _start_ and _end_. _start_ defaults to 0 and _end_ defaults to the length of _vector_.

```scheme
(define a (vector 1 2 3 4 5))
(vector-reverse! a)
a  ⇒  #(5 4 3 2 1)
```

**(vector-sort! _pred vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(vector-sort! _pred vector start_)**  
**(vector-sort! _pred vector start end_)**  

Procedure `vector-sort!` destructively sorts the elements of _vector_ using the "less than" predicate _pred_ between the indices _start_ and _end_. Default for _start_ is 0, for _end_ it is the length of the vector.

```scheme
(define a (vector 7 4 9 1 2 8 5))
(vector-sort! < a)
a  ⇒  #(1 2 4 5 7 8 9)
```

**(vector-map! _f vector1 vector2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Similar to `vector-map` which maps the various elements into a new vector via function _f_, procedure `vector-map!` destructively inserts the mapped elements into _vector1_. The dynamic order in which _f_ gets applied to the elements is unspecified.

```scheme
(define a (vector 1 2 3 4))
(vector-map! + a #(10 20 30))
a  ⇒  #(11 22 33 4)
```

**(vector-map/index! _f vector1 vector2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Similar to `vector-map/index` which maps the various elements together with their index into a new vector via function _f_, procedure `vector-map/index!` destructively inserts the mapped elements into _vector1_. The dynamic order in which _f_ gets applied to the elements is unspecified.

```scheme
(define a (vector 1 2 3 4))
(vector-map/index! (lambda (i x y) (cons i (+ x y))) a #(10 20 30))
a  ⇒  #((0 . 11) (1 . 22) (2 . 33) 4)
```

## Converting vectors

**(vector->list _vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(vector->list _vector start_)**  
**(vector->list _vector start end_)**  

The `vector->list` procedure returns a newly allocated list of the objects contained in the elements of _vector_ between _start_ and _end_ in the same order line in _vector_.

```scheme
(vector->list ’#(dah dah didah))  ⇒  (dah dah didah)
(vector->list ’#(dah dah didah) 1 2)  ⇒  (dah)
```

**(vector->string _vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(vector->string _vector start_)**  
**(vector->string _vector start end_)**  

The `vector->string` procedure returns a newly allocated string of the objects contained in the elements of _vector_ between _start_ and _end_. This procedure preserves the order of the characters. It is an error if any element of vector between _start_ and _end_ is not a character.

```scheme
(vector->string #(#\1 #\2 #\3)  ⇒  "123"
```
