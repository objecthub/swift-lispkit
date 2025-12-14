# LispKit Gvector

This library defines an API for _growable vectors_. Just like regular vectors, _growable vectors_ are heterogeneous sequences of elements which are indexed by a range of integers. Unlike for regular vectors, the length of a _growable vector_ is not fixed. Growable vectors may expand or shrink in length. Nevertheless, growable vectors are fully compatible to regular vectors and all operations from library `(lispkit vector)` may also be used in combination with growable vectors. The main significance of library `(lispkit gvector)` is in providing functions to construct growable vectors. Growable vectors are always _mutable_ by design.

Just like for vectors with a fixed length, the valid indexes of a growable vector are the exact, non-negative integers less than the length of the vector. The first element in a vector is indexed by zero, and the last element is indexed by one less than the length of the growable vector.

Two growable vectors are `equal?` if they have the same length, and if the values in corresponding slots of the vectors are `equal?`. A growable vector is never `equal?` a regular vector of fixed length.

Growable vectors are written using the notation `#g(obj ...)`. For example, a growable vector of initial length 3 containing the number one as element 0, the list (8 16 32) as element 1, and the string "Scheme" as element 2 can be written as follows: `#g(1 (8 16 32) "Scheme")`.

Growable vector constants are self-evaluating, so they do not need to be quoted in programs.

## Predicates

**(gvector? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a growable vector; otherwise returns `#f`.

**(gvector-empty? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a growable vector of length zero; otherwise returns `#f`.

## Constructors

**(make-gvector)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-gvector _c_)**  

Returns a newly allocated growable vector of capacity _c_. The capacity is used to pre-allocate space for up to _c_ elements.

**(gvector _obj ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a newly allocated growable vector whose elements contain the given arguments.

```scheme
(gvector ’a ’b ’c)  ⇒  #g(a b c)
```

**(list->gvector _list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>    

The `list->gvector` procedure returns a newly created growable vector initialized to the elements of the list _list_ in the order of the list.

```scheme
(list->gvector ’(a b c))  ⇒  #g(a b c)
```

**(vector-\>gvector _vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a newly allocated growable vector initialized to the elements of the vector _vector_ in the order of _vector_.

**(gvector-copy _vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(gvector-copy _vector start_)**  
**(gvector-copy _vector start end_)**  

Returns a newly allocated copy of the elements of the given growable vector between _start_ and _end_, but excluding the element at index _end_. The elements of the new vector are the same (in the sense of `eqv?`) as the elements of the old.

**(gvector-append _vector ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a newly allocated growable vector whose elements are the concatenation of the elements of the given vectors.

```scheme
(gvector-append #(a b c) #g(d e f))  ⇒  #g(a b c d e f)
```

**(gvector-concatenate _vector xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a newly allocated growable vector whose elements are the concatenation of the elements of the vectors in _xs_. _xs_ is a proper list of vectors. 

```scheme
(gvector-concatenate '(#g(a b c) #(d) #g(e f)))  ⇒  #g(a b c d e f)
```

**(gvector-map _f vector1 vector2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Constructs a new growable vector of the shortest size of the vector arguments _vector1_, _vector2_, etc. Each element at index _i_ of the new vector is mapped from the old vectors by `(f (vector-ref vector1 i) (vector-ref vector2 i) ...)`. The dynamic order of the application of f is unspecified. 

```scheme
(gvector-map + #(1 2 3 4 5) #g(10 20 30 40))  ⇒  #g(11 22 33 44)
```

**(gvector-map/index _f vector1 vector2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Constructs a new growable vector of the shortest size of the vector arguments _vector1_, _vector2_, etc. Each element at index _i_ of the new vector is mapped from the old vectors by `(f i (vector-ref vector1 i) (vector-ref vector2 i) ...)`. The dynamic order of the application of f is unspecified. 

```scheme
(gvector-map/index (lambda (i x y) (cons i (+ x y))) #g(1 2 3) #(10 20 30))  ⇒  #g((0 . 11) (1 . 22) (2 . 33))
```

## Iterating over vector elements

**(gvector-for-each _f vector1 vector2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`gvector-for-each` implements a simple vector iterator: it applies _f_ to the corresponding list of parallel elements from vectors _vector1 vector2 ..._ in the range _[0, length)_, where _length_ is the length of the smallest vector argument passed. In contrast with `gvector-map`, _f_ is reliably applied to each subsequent element, starting at index 0, in the vectors.

```scheme
(gvector-for-each (lambda (x) (display x) (newline)) 
                  #g("foo" "bar" "baz" "quux" "zot"))
⇒
foo
bar
baz
quux
zot
```

**(gvector-for-each/index _f vector1 vector2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`gvector-for-each/index` implements a simple vector iterator: it applies _f_ to the index _i_ and the corresponding list of parallel elements from _vector1 vector2 ..._ in the range _[0, length)_, where _length_ is the length of the smallest vector argument passed. The only difference to `gvector-for-each` is that `gvector-for-each/index` always passes the current index as the first argument of _f_ in addition to the elements from the vectors _vector1 vector2 ..._.

```scheme
(gvector-for-each/index
  (lambda (i x) (display i)(display ": ")(display x)(newline))
  #g("foo" "bar" "baz" "quux" "zot"))
⇒
0: foo
1: bar
2: baz
3: quux
4: zot
```

## Managing vector state

**(gvector-length _vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of elements in growable vector _vector_ as an exact integer.

**(gvector-ref _vector k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `gvector-ref` procedure returns the contents of element _k_ of _vector_. It is an error if _k_ is not a valid index of _vector_ or if _vector_ is not a growable vector.

```scheme
(gvector-ref ’#g(1 1 2 3 5 8 13 21) 5) ⇒  8
(gvector-ref ’#g(1 1 2 3 5 8 13 21) (exact (round (* 2 (acos -1)))))  ⇒  13
```

**(gvector-set! _vector k obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `vector-set!` procedure stores _obj_ in element _k_ of growable vector _vector_. It is an error if _k_ is not a valid index of _vector_ or if _vector_ is not a growable vector.

```scheme
(let ((vec (gvector 0 '(2 2 2 2) "Anna")))
  (gvector-set! vec 1 '("Sue" "Sue"))
  vec)
  ⇒  #g(0 ("Sue" "Sue") "Anna")
```

**(gvector-add! _vector obj ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Appends the values _obj_, ... to growable vector _vector_. This increases the length of the growable vector by the number of _obj_ arguments.

```scheme
(let ((vec (gvector 0 '(2 2 2 2) "Anna")))
  (gvector-add! vec "Micha")
  vec)
  ⇒  #g(0 (2 2 2 2) "Anna" "Micha")
```

**(gvector-insert! _vector k obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Inserts the value _obj_ into growable vector _vector_ at index _k_. This increases the length of the growable vector by one.

```scheme
(let ((vec (gvector 0 '(2 2 2 2) "Anna")))
  (gvector-insert! vec 1 "Micha")
  vec)
  ⇒  #g(0 "Micha" (2 2 2 2) "Anna")
```

**(gvector-remove! _vector k_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the element at index _k_ from growable vector _vector_. This decreases the length of the growable vector by one.

```scheme
(let ((vec (gvector 0 '(2 2 2 2) "Anna")))
  (gvector-remove! vec 1)
  vec)
  ⇒  #g(0 "Anna")
```

**(gvector-remove-last! _vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the last element of the growable vector _vector_. This decreases the length of the growable vector by one.

```scheme
(let ((vec (gvector 0 '(2 2 2 2) "Anna")))
  (gvector-remove-last! vec)
  vec)
  ⇒  #g(0 (2 2 2 2))
```

## Destructive growable vector operations

Procedures which operate only on a part of a growable vector specify the applicable range in terms of an index interval [_start_; _end_[; i.e. the _end_ index is always exclusive.

**(gvector-copy! _to at from_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(gvector-copy! _to at from start_)**  
**(gvector-copy! _to at from start end_)**  

Copies the elements of vector _from_ between _start_ and _end_ to growable vector _to_, starting at _at_. The order in which elements are copied is unspecified, except that if the source and destination overlap, copying takes place as if the source is first copied into a temporary vector and then into the destination. _start_ defaults to 0 and _end_ defaults to the length of _vector_.

It is an error if _at_ is less than zero or greater than the length of _to_. It is also an error if `(- (gvector-length to) at)` is less than `(- end start)`.

```scheme
(define a (vector 1 2 3 4 5))
(define b (gvector 10 20 30 40 50))
(gvector-copy! b 1 a 0 2)
b  ⇒  #g(10 1 2 40 50)
```

**(gvector-append! _vector v1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Appends the elements of the vectors _v1 ..._ to the growable vector _vector_ in the given order.

**(gvector-reverse! _vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(gvector-reverse! _vector start_)**  
**(gvector-reverse! _vector start end_)**  

Procedure `gvector-reverse!` destructively reverses the contents of growable _vector_ between _start_ and _end_. _start_ defaults to 0 and _end_ defaults to the length of _vector_.

```scheme
(define a (gvector 1 2 3 4 5))
(vector-reverse! a)
a  ⇒  #g(5 4 3 2 1)
```

**(gvector-sort! _pred vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Procedure `gvector-sort!` destructively sorts the elements of growable vector _vector_ using the "less than" predicate _pred_.

```scheme
(define a (gvector 7 4 9 1 2 8 5))
(gvector-sort! < a)
a  ⇒  #g(1 2 4 5 7 8 9)
```

**(gvector-map! _f vector1 vector2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Similar to `gvector-map` which maps the various elements into a new vector via function _f_, procedure `gvector-map!` destructively inserts the mapped elements into growable vector _vector1_. The dynamic order in which _f_ gets applied to the elements is unspecified.

```scheme
(define a (gvector 1 2 3 4))
(gvector-map! + a #(10 20 30))
a  ⇒  #g(11 22 33 4)
```

**(gvector-map/index! _f vector1 vector2 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Similar to `gvector-map/index` which maps the various elements together with their index into a new vector via function _f_, procedure `gvector-map/index!` destructively inserts the mapped elements into growable vector _vector1_. The dynamic order in which _f_ gets applied to the elements is unspecified.

```scheme
(define a #g(1 2 3 4))
(gvector-map/index! (lambda (i x y) (cons i (+ x y))) a #(10 20 30))
a  ⇒  #g((0 . 11) (1 . 22) (2 . 33) 4)
```

## Converting growable vectors

**(gvector-\>list _vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(gvector-\>list _vector start_)**  
**(gvector-\>list _vector start end_)**  

The `gvector->list` procedure returns a newly allocated list of the objects contained in the elements of growable _vector_ between _start_ and _end_ in the same order as in _vector_.

```scheme
(gvector->list ’#g(dah dah didah))  ⇒  (dah dah didah)
(gvector->list ’#g(dah dah didah) 1 2)  ⇒  (dah)
```

**(gvector-\>vector _vector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(gvector-\>vector _vector start_)**  
**(gvector-\>vector _vector start end_)**  

The `gvector->list` procedure returns a newly allocated list of the objects contained in the elements of growable vector _vector_ between _start_ and _end_ in the same order as in _vector_.

```scheme
(gvector->list ’#(dah dah didah))  ⇒  error since the argument is not a gvector
(gvector->list ’#g(dah dah didah) 1 2)  ⇒  (dah)
```
