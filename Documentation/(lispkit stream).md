# LispKit Stream

Streams are a sequential data structure containing elements computed only on demand. They are sometimes also called _lazy lists_.

Streams get constructed with list-like constructors. A stream is either _null_ or is a _pair_ with a stream in its cdr. Since elements of a stream are computed only when accessed, streams can be infinite. Once computed, the value of a stream element is cached in case it is needed again.

## Benefits of using streams

When used effectively, the primary benefit of streams is improved modularity. Consider a process that takes a sequence of items, operating on each in turn. If the operation is complex, it may be useful to split it into two or more procedures in which the partially-processed sequence is an intermediate result. If that sequence is stored as a list, the entire intermediate result must reside in memory all at once; however, if the intermediate result is stored as a stream, it can be generated piecemeal, using only as much memory as required by a single item. This leads to a programming style that uses many small operators, each operating on the sequence of items as a whole, similar to a pipeline of unix commands.

In addition to improved modularity, streams permit a clear exposition of backtracking algorithms using the “stream of successes” technique, and they can be used to model generators and co-routines. The implicit memoization of streams makes them useful for building persistent data structures, and the laziness of streams permits some multi-pass algorithms to be executed in a single pass. Savvy programmers use streams to enhance their programs in countless ways.

There is an obvious space/time trade-off between lists and streams; lists take more space, but streams take more time (to see why, look at all the type conversions in the implementation of the stream primitives). Streams are appropriate when the sequence is truly infinite, when the space savings are needed, or when they offer a clearer exposition of the algorithms that operate on the sequence.

## Stream abstractions

The `(lispkit stream)` library provides two mutually-recursive abstract data types: An object of type `stream` is a promise that, when forced, is either `stream-null` or is an object of type `stream-pair`. An object of the `stream-pair` type contains a `stream-car` and a `stream-cdr`, which must be a stream. The essential feature of streams is the systematic suspensions of the recursive promises between the two data types.

The object stored in the `stream-car` of a `stream-pair` is a promise that is forced the first time the `stream-car` is accessed; its value is cached in case it is needed again. The object may have any type, and different stream elements may have different types. If the `stream-car` is never accessed, the object stored there is never evaluated. Likewise, the `stream-cdr` is a promise to return a stream, and is only forced on demand.

## Stream API

The design of the API of library `(lispkit stream)` is based on Philip Bewig's SRFI 41. The implementation of the library is LispKit-specific.

**stream-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `stream` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all stream objects.

**stream-null** <span style="float:right;text-align:rigth;">[object]</span>   

`stream-null` is a stream that, when forced, is a single object, distinguishable from all other objects, that represents the null stream. `stream-null` is immutable and unique.

**(stream? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a stream; otherwise `#f` is returned.

**(stream-null? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

`stream-null?` is a procedure that takes an object _obj_ and returns `#t` if the object is the distinguished null stream and `#f` otherwise. If object _obj_ is a stream, `stream-null?` must force its promise in order to distinguish `stream-null` from `stream-pair`.

**(stream-pair? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

`stream-pair?` is a procedure that takes an object and returns `#t` if the object is a `stream-pair` constructed by `stream-cons` and `#f` otherwise. If object is a stream, `stream-pair?` must force its promise in order to distinguish `stream-null` from `stream-pair`.

**(stream-cons _obj strm_)** <span style="float:right;text-align:rigth;">[syntax]</span>   

`stream-cons` is a special form that accepts an object _obj_ and a stream _strm_ and creates a newly-allocated stream containing a stream that, when forced, is a `stream-pair` with the object in its `stream-car` and the stream in its `stream-cdr`. `stream-cons` must be syntactic, not procedural, because neither object _obj_ nor stream is evaluated when `stream-cons` is called. Since `strm` is not evaluated, when the `stream-pair` is created, it is not an error to call `stream-cons` with a stream that is not of type stream; however, doing so will cause an error later when the `stream-cdr` of the `stream-pair` is accessed. Once created, a `stream-pair` is immutable.

```scheme
(define s (stream-cons 1 (stream-cons 2 (stream-cons 3 stream-null))))
(stream-car s)               ⇒  1
(stream-car (stream-cdr s))  ⇒  2
```

**(stream-car _strm_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

`stream-car` is a procedure that takes a stream _strm_ and returns the object stored in the `stream-car` of the stream. `stream-car` signals an error if the object passed to it is not a `stream-pair`. Calling `stream-car` causes the object stored there to be evaluated if it has not yet been; the object’s value is cached in case it is needed again.

**(stream-cdr _strm_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

`stream-cdr` is a procedure that takes a stream _strm_ and returns the stream stored in the `stream-cdr` of the stream. `stream-cdr` signals an error if the object passed to it is not a `stream-pair`. Calling `stream-cdr` does not force the promise containing the stream stored in the `stream-cdr` of the stream.

**(stream _obj ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>   

`stream` is syntax that takes zero or more objects _obj_ and creates a newly-allocated stream containing in its elements the objects, in order. Since `stream` is syntactic, the objects are evaluated when they are accessed, not when the stream is created. If no objects are given, as in `(stream)`, the null stream is returned.

**(stream-lambda _formals expr0 expr1 ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>   

`stream-lambda` creates a procedure that returns a stream to evaluate the body of the procedure. The last body expression to be evaluated must yield a stream. As with the regular `lambda`, _formals_ may be a single variable name, in which case all the formal arguments are collected into a single list, or it is a list of variable names, which may be `null` if there are no arguments, proper if there are an exact number of arguments, or dotted, if a fixed number of arguments is to be followed by zero or more arguments collected into a list. The body _expr0 expr1 ..._ must contain at least one expression, and may contain internal definitions preceding any expressions to be evaluated.

```scheme
(define iter (stream-lambda (f x) (stream-cons x (iter f (f x)))))
(define nats (iter (lambda (x) (+ x 1)) 0))
(stream-car (stream-cdr nats))               ⇒  1

(define stream-add
  (stream-lambda (s1 s2)
    (stream-cons (+ (stream-car s1) (stream-car s2))
                 (stream-add (stream-cdr s1) (stream-cdr s2)))))
(define evens (stream-add nats nats))

(stream-car evens)                           ⇒  0
(stream-car (stream-cdr evens))              ⇒  2
(stream-car (stream-cdr (stream-cdr evens))) ⇒  4
```

**(define-stream _(name arg ...) expr0 expr1 ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>   

`define-stream` creates a procedure _name_ that returns a stream, and may appear anywhere a normal define may appear, including as an internal definition, and may have internal definitions of its own, including other `define-streams`. The defined procedure takes arguments _arg ..._ in the same way as `stream-lambda`. `define-stream` is syntactic sugar on `stream-lambda`.

**(stream-let _tag ((var val) ...) expr1 expr2 ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>   

`stream-let` creates a local scope that binds each variable _var_ to the value of its corresponding expression _val_. It additionally binds _tag_ to a procedure which takes the bound variables as arguments and body as its defining expressions, binding the tag with `stream-lambda`. _tag_ is in scope within body, and may be called recursively. When the expanded expression defined by the `stream-let` is evaluated, `stream-let` evaluates the expressions _expr1 expr2 ..._ in its body in an environment containing the newly-bound variables, returning the value of the last expression evaluated, which must yield a stream.

`stream-let` provides syntactic sugar on `stream-lambda`, in the same manner as normal `let` provides syntactic sugar on normal `lambda`. However, unlike normal `let`, the `tag` is required, not optional, because unnamed `stream-let` is meaningless.

**(display-stream _strm_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(display-stream _strm n_)**  
**(display-stream _strm n sep_)**  
**(display-stream _strm n sep port_)**  

`display-stream` displays the first _n_ elements of stream _strm_ on port _port_ using string _sep_ as a separator string. If _n_ is not provided, all elements are getting displayed. If _sep_ is not provided, `", "` is used as a default. If _port_ is not provided, the current output port is used.

**(list-\>stream _lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

`list->stream` takes a list of objects _lst_ and returns a newly-allocated stream containing in its elements the objects in the list. Since the objects are given in a list, they are evaluated when `list->stream` is called, before the stream is created. If the list of objects is null, as in `(list->stream '())`, the null stream is returned.

**(port-\>stream)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(port-\>stream _port_)**   

`port->stream` takes a port _port_ and returns a newly-allocated stream containing in its elements the characters on the port. If the port is not given, it defaults to the current input port. The returned stream has finite length and is terminated by `stream-null`.

**(stream-\>list _strm_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(stream-\>list _strm n_)**  

`stream->list` takes a natural number _n_ and a stream _strm_ and returns a newly-allocated list containing in its elements the first _n_ items in the stream. If the stream has less than _n_ items, all the items in the stream will be included in the returned list. If _n_ is not given, it defaults to infinity, which means that unless the stream is finite, `stream->list` will never return.

**(stream-append _strm ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-append` returns a newly-allocated stream containing in its elements those elements contained in its argument streams _strm ..._, in order of input. If any of the input streams is infinite, no elements of any of the succeeding input streams will appear in the output stream; thus, if `x` is infinite, `(stream-append x y)` ≡ `x`.

**(stream-concat _strms_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-concat` takes a stream _strms_ consisting of one or more streams and returns a newly-allocated stream containing all the elements of the input streams. If any of the streams in the input stream is infinite, any remaining streams in the input stream will never appear in the output stream.

**(stream-constant _obj ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-constant` takes one or more objects _obj ..._ and returns a newly-allocated stream containing in its elements the objects, repeating the objects in succession forever.

**(stream-drop _strm n_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-drop` returns the suffix of the input stream _strm_ that starts at the next element after the first _n_ elements. The output stream shares structure with the input stream; thus, promises forced in one instance of the stream are also forced in the other instance of the stream. If the input stream has less than _n_ elements, `stream-drop` returns the null stream.

**(stream-drop-while _pred? strm_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-drop-while` returns the suffix of the input stream that starts at the first element _x_ for which `(pred?` _x_`)` is `#f`. The output stream shares structure with the input stream.

**(stream-filter _pred? strm_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-filter` returns a newly-allocated stream that contains only those elements _x_ of the input stream for which `(pred?` _x_`)` is non-`#f`.

**(stream-fold _proc base strm_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-fold` applies a binary procedure _proc_ to _base_ and the first element of stream _strm_ to compute a new base, then applies the procedure _proc_ to the new base (1st argument of _proc_) and the next element of stream (2nd argument of _proc_) to compute a succeeding base, and so on, accumulating a value that is finally returned as the value of `stream-fold` when the end of the stream is reached. _strm_ must be finite, or `stream-fold` will enter an infinite loop.

See also `stream-scan`, which is similar to `stream-fold`, but useful for infinite streams. `stream-fold` is a left-fold; there is no corresponding `right-fold`, since `right-fold` relies on finite streams that are fully-evaluated, at which time they may as well be converted to a list.

**(stream-for-each _proc strm ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-for-each` applies a procedure _proc_ elementwise to corresponding elements of the input streams _strm ..._ for its side-effects. `stream-for-each` stops as soon as any of its input streams is exhausted.

**(stream-from _first_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(stream-from _first delta_)**   

`stream-from` creates a newly-allocated stream that contains _first_ as its first element and increments each succeeding element by _delta_. If _delta_ is not given it defaults to 1. _first_ and _delta_ may be of any numeric type. `stream-from` is frequently useful as a generator in `stream-of` expressions. See also `stream-range` for a similar procedure that creates finite streams.

**(stream-iterate _proc base_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-iterate` creates a newly-allocated stream containing _base_ in its first element and applies _proc_ to each element in turn to determine the succeeding element.

**(stream-length _strm_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-length` takes an input stream _strm_ and returns the number of elements in the stream. It does not evaluate its elements. `stream-length` may only be used on finite streams as it enters an infinite loop with infinite streams.

**(stream-map _proc strm ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-map` applies a procedure _proc_ elementwise to corresponding elements of the input streams _strm ..._, returning a newly-allocated stream containing elements that are the results of those procedure applications. The output stream has as many elements as the minimum-length input stream, and may be infinite.

**(stream-match _strm-expr (pattern [fender] expr) ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

`stream-match` provides the syntax of pattern-matching for streams. The input stream _strm-expr_ is an expression that evaluates to a stream and is matched against a number of clauses. Each clause `(pattern [fender] expr)` consists of a pattern that matches a stream of a particular shape, an optional _fender_ that must succeed if the pattern is to match, and an expression that is evaluated if the pattern matches.

There are four types of patterns:

- `()`: Matches the null stream
- `(pat0 pat1 ...)`: Matches a finite stream with length exactly equal to the number of pattern elements
- `(pat0 pat1 ... . patrest)`: Matches an infinite stream, or a finite stream with length at least as great as the number of pattern elements before the literal dot
- `pat`: Matches an entire stream. Should always appear last in the list of clauses; it’s not an error to appear elsewhere, but subsequent clauses could never match

Each pattern element pati may be either:

- _An identifier_: Matches any stream element. Additionally, the value of the stream element is bound to the variable named by the identifier, which is in scope in the _fender_ and expression of the corresponding clause. Each identifier in a single pattern must be unique.
- _A literal underscore_: Matches any stream element, but creates no bindings.

The patterns are tested in order, left-to-right, until a matching pattern is found. If _fender_ is present, it must evaluate as non-`#f` for the match to be successful. Pattern variables are bound in the corresponding _fender_ and expression. Once the matching pattern is found, the corresponding expression is evaluated and returned as the result of the match. An error is signaled if no pattern matches the input stream.

**(stream-of _expr rest ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

`stream-of` provides the syntax of stream comprehensions, which generate streams by means of looping expressions. The result is a stream of objects of the type returned by _expr_. There are four types of clauses:

- `(var in stream-expr)`: Loop over the elements of `stream-expr`, in order from the start of the stream, binding each element of the stream in turn to `var`. `stream-from` and `stream-range` are frequently useful as generators.
- `(var is expr)`: Bind `var` to the value obtained by evaluating `expr`.
- `(pred? expr)`: Include in the output stream only those elements `x` for which `(pred? x)` is non-`#f`.

The scope of variables bound in the stream comprehension is the clauses to the right of the binding clause (but not the binding clause itself) plus the result expression. When two or more generators are present, the loops are processed as if they are nested from left to right; i.e. the rightmost generator varies fastest. A consequence of this is that only the first generator may be infinite and all subsequent generators must be finite. If no generators are present, the result of a stream comprehension is a stream containing the result expression; thus, `(stream-of 1)` produces a finite stream containing only the element 1.

**(stream-range _first past_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(stream-range _first past delta_)**  

`stream-range` creates a newly-allocated stream that contains _first_ as its first element and increments each succeeding element by _step_. The stream is finite and ends before _past_, which is not an element of the stream. If _step_ is not given it defaults to 1 if _first_ is less than past and `-1` otherwise. First, _past_ and _step_ may be of any numeric type. `stream-range` is frequently useful as a generator in `stream-of` expressions.

**(stream-ref _strm n_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-ref` returns the _n_-th element of stream, counting from zero. An error is signaled if _n_ is greater than or equal to the length of stream.

**(stream-reverse _strm_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-reverse` returns a newly-allocated stream containing the elements of the input stream _strm_ but in reverse order. `stream-reverse` may only be used with finite streams; it enters an infinite loop with infinite streams. `stream-reverse` does not force evaluation of the elements of the stream.

**(stream-scan _proc base strm_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-scan` accumulates the partial folds of an input stream _strm_ into a newly-allocated output stream. The output stream is the _base_ followed by `(stream-fold proc base (stream-take i stream))` for each of the first `i` elements of stream.

**(stream-take _strm n_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-take` takes a non-negative integer _n_ and a stream and returns a newly-allocated stream containing the first _n_ elements of the input stream. If the input stream has less than _n_ elements, so does the output stream.

**(stream-take-while _pred? strm_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-take-while` takes a predicate _pred?_ and a stream _strm_ and returns a newly-allocated stream containing those elements `x` that form the maximal prefix of the input stream for which `(pred? x)` is non-`#f`.

**(stream-unfold _mapper pred? generator base_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-unfold` is the fundamental recursive stream constructor. It constructs a stream by repeatedly applying _generator_ to successive values of _base_, in the manner of `stream-iterate`, then applying _mapper_ to each of the values so generated, appending each of the mapped values to the output stream as long as `(pred? base)` is non-`#f`.

**(stream-unfolds _proc seed_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-unfolds` returns _n_ newly-allocated streams containing those elements produced by successive calls to the generator _proc_, which takes the current `seed` as its argument and returns _n+1_ values:

&nbsp;&nbsp;&nbsp; (_proc seed_) &nbsp; → &nbsp; _seed result0 ... resultn-1_

where the returned seed is the input seed to the next call to the generator and _resulti_ indicates how to produce the next element of the _i_-th result stream:

- `(value)`: value is the next car of the result stream
- `#f`: no value produced by this iteration of the generator `proc` for the result stream
- `()`: the end of the result stream

It may require multiple calls of `proc` to produce the next element of any particular result stream.

**(stream-zip _strm ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

`stream-zip` takes one or more input streams _strm ..._ and returns a newly-allocated stream in which each element is a list (not a stream) of the corresponding elements of the input streams. The output stream is as long as the shortest input stream, if any of the input streams is finite, or is infinite if all the input streams are infinite.
