# LispKit Sequence

Library `(lispkit sequence)` provides a uniform way to iterate over lists, vectors, strings, bytevectors, hash tables, sets, streams, ranges of numbers, and other data sources. It consists of

   - a family of _iteration forms_ (`for`, `for/list`, `for/fold`, ...) that combine iteration over one or more _sequences_ with the accumulation of results (a list, a vector, a sum, a hash table, ...), and
   - _sequence constructors_ (`in-range`, `in-list`, `in-hash`, ...) and _sequence operations_ (`sequence->list`, `sequence-map`, ...) for creating and transforming sequences.

The design of the library is inspired by the sequence and iteration forms of Racket. It is implemented purely with `syntax-rules` macros and ordinary procedures. It does not require any changes to the reader or the compiler of LispKit.

```scheme
(import (lispkit base)
        (lispkit sequence))
(for/list ((i (in-range 5))) (* i i))
  ⇒  (0 1 4 9 16)
(for/sum ((x '(1 2 3)) (y #(10 20 30))) (* x y))
  ⇒  140
(for*/list ((i 3) (j 2)) (list i j))
  ⇒  ((0 0) (0 1) (1 0) (1 1) (2 0) (2 1))
(for/first ((x (in-naturals 1)) :when (> (* x x) 50)) x)
  ⇒  8
```

## Syntax conventions

The iteration forms use only standard Scheme syntax:

   - **Clauses are written with parentheses.** A _for-clause_ has the form `(id seq)`, and the clauses of an iteration form are collected in a list: `(for ((x lst) (y vec)) ...)`.
   - **Keywords are symbols with a leading colon.** The guards `:when`, `:unless`, `:break` and `:final` can be used between the clauses and in the body of iteration forms. The keyword `:result` is used by `for/fold`, and the keywords `:length` and `:fill` by `for/vector`. They are ordinary symbols which are only recognized by the iteration forms in the positions described below; no special reader syntax is involved. SRFI 42 uses the same convention.
   - **Hash tables are mutable.** `for/hash`, `for/hasheq` and `for/hasheqv` return mutable hash tables of library `(lispkit hashtable)`.
   - **New sequences are created from existing ones.** There is no mechanism for defining new kinds of sequences. Instead, sequences can be built with constructors such as `in-producer`, `in-stream`, `in-sequences` or `sequence-map`. A sequence can be turned into a stream with `sequence->stream`.

Library `(lispkit iterate)` defines a different, unrelated syntactical form `for` (e.g. `(for i from 1 to 10 ...)`). Since both libraries export `for`, they cannot be imported without renaming or excluding one of the two bindings:

```scheme
(import (lispkit base)
        (except (lispkit iterate) for)
        (lispkit sequence))
```

## Sequences

The following values are _sequences_ and can be used wherever a sequence is expected:

   - lists (elements in order),
   - vectors, strings and bytevectors (elements in order),
   - non-negative exact integers _n_ (the integers from 0 up to _n - 1_),
   - hash tables (each element consists of the two values _key_ and _value_; the order is unspecified),
   - sets of library `(lispkit set)` (the order is unspecified),
   - streams of library `(lispkit stream)`, and
   - sequence objects created by the constructors of this library, such as `(in-range 10)`.

An element of a sequence can consist of multiple values, as in the case of hash tables or `in-indexed`. In a for-clause, the identifiers for such elements are written as a list: `(for (((k v) (in-hash table))) ...)`. A clause with a single identifier requires a sequence with exactly one value per element.

Since sequences are created lazily, they can be infinite, e.g. `(in-naturals)`. Using a sequence in an iteration form never mutates it; a sequence can be used multiple times, unless it wraps a stateful source such as a port (`in-port`, `in-lines`) or a procedure with side effects (`in-producer`).

## Iteration forms

All iteration forms have the shape `(form (clause ...) body ...)`. They differ in how they combine the results of the body:

| Form | Result |
|:-----|:-------|
| `for`, `for*` | unspecified; used for side effects only |
| `for/list`, `for*/list` | a list of the results of the body |
| `for/vector`, `for*/vector` | a vector of the results of the body |
| `for/fold`, `for*/fold` | the final values of the accumulators |
| `for/sum`, `for*/sum`, `for/product`, `for*/product` | the sum or the product of the results of the body |
| `for/and`, `for*/and`, `for/or`, `for*/or` | a conjunction or a disjunction of the results of the body |
| `for/first`, `for*/first`, `for/last`, `for*/last` | the result of the first or of the last iteration |
| `for/hash`, `for*/hash`, `for/hasheq`, `for*/hasheq`, `for/hasheqv`, `for*/hasheqv` | a new mutable hash table |

The forms without a star iterate over the sequences of their clauses in parallel. The forms with a star (`for*`, `for*/list`, ...) iterate over them in a nested fashion.

**(for _(clause ...) body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(for\* _(clause ...) body ..._)**  

Iterates over the sequences specified by the clauses, evaluating _body ..._ for each element. The result of the form is unspecified.

Every _clause_ is either a _sequence clause_ `(id seq)` or one of the following guards:

   - `:when expr`: The iteration continues with the following clauses and the body only if _expr_ evaluates to true.
   - `:unless expr`: The iteration continues with the following clauses and the body only if _expr_ evaluates to false.
   - `:break expr`: If _expr_ evaluates to true, the whole iteration stops.
   - `:final expr`: If _expr_ evaluates to true, the current element is the last one: the body is evaluated and the iteration stops afterwards.

A sequence clause `(id seq)` binds identifier _id_ to the elements of the sequence _seq_. If _seq_ produces multiple values per element, the clause is written `((id ...) seq)`, binding one identifier per value.

Consecutive sequence clauses of `for` are iterated in _parallel_: the iteration stops as soon as the shortest sequence is exhausted. In `for*`, clauses are _nested_: the last clause forms the innermost loop. In both forms, identifiers bound by a clause are visible in later clauses and in the body.

The body of an iteration form may contain `:break` and `:final` clauses interleaved with expressions. They take effect at the point where they appear in the body.

```scheme
(for ((i (in-range 3)) (c "abc"))
  (display (list i c)))                  ; prints (0 a)(1 b)(2 c)
(for* ((i (in-range 3)) (j (in-range i)))
  (display (list i j)))                  ; prints (1 0)(2 0)(2 1)
(for/list ((i (in-range 10)) :when (even? i) :break (> i 6)) i)
  ⇒  (0 2 4 6)
```

Guards apply to everything that follows them. The next examples show how parallel and nested iteration differ, and how guards filter or stop the iteration:

```scheme
(for/list ((x '(1 2 3)) (y '(a b))) (list x y))
  ⇒  ((1 a) (2 b))
(for*/list ((x '(1 2 3)) (y '(a b))) (list x y))
  ⇒  ((1 a) (1 b) (2 a) (2 b) (3 a) (3 b))
(for/list ((x (in-range 10)) :unless (odd? x) :when (> x 2)) x)
  ⇒  (4 6 8)
(for/list ((x '(1 2 3 4 5)) :break (= x 4)) x)
  ⇒  (1 2 3)
(for/list ((x (in-range 10)) :final (= x 3)) x)
  ⇒  (0 1 2 3)
(for*/list ((a (in-range 1 20)) (b (in-range a 20)) (c (in-range b 20))
            :when (= (+ (* a a) (* b b)) (* c c)))
  (list a b c))
  ⇒  ((3 4 5) (5 12 13) (6 8 10) (8 15 17) (9 12 15))
```

**(for/list _(clause ...) body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(for\*/list _(clause ...) body ..._)**  

Iterates like `for` and `for*`, respectively, and returns the list of the values of the last body expression of every iteration.

**(for/vector _(clause ...) body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(for/vector :length _n_ _(clause ...) body ..._)**  
**(for/vector :length _n_ :fill _v_ _(clause ...) body ..._)**  
**(for\*/vector _(clause ...) body ..._)**  
**(for\*/vector :length _n_ _(clause ...) body ..._)**  
**(for\*/vector :length _n_ :fill _v_ _(clause ...) body ..._)**  

Like `for/list` and `for*/list`, but returns a vector. If `:length` is provided, the result is a vector of length _n_: the iteration stops once _n_ elements have been produced, and if fewer elements are produced, the remaining slots are filled with _v_ (or `0` if `:fill` is not provided).

```scheme
(for/vector ((i (in-range 4))) (* i i))              ⇒  #(0 1 4 9)
(for/vector :length 3 ((i (in-range 10))) i)         ⇒  #(0 1 2)
(for/vector :length 4 :fill 'x ((i (in-range 2))) i) ⇒  #(0 1 x x)
```

**(for/fold _((acc init) ...) (clause ...) body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(for/fold _((acc init) ... :result expr) (clause ...) body ..._)**  
**(for\*/fold _((acc init) ...) (clause ...) body ..._)**  
**(for\*/fold _((acc init) ... :result expr) (clause ...) body ..._)**  

Iterates like `for` and `for*` while threading accumulators through the iteration. Every accumulator _acc_ is initialized with the value of _init_. The last body expression has to return as many values as there are accumulators; these are the new values of the accumulators for the next iteration. The form returns the final accumulator values as multiple values, unless `:result` is provided: in this case, _expr_ is evaluated with the accumulators in scope and its values are returned.

```scheme
(for/fold ((sum 0) (cnt 0)) ((x '(3 4 5)))
  (values (+ sum x) (+ cnt 1)))
  ⇒  12 and 3
(for/fold ((acc '()) :result (reverse acc)) ((x "abc"))
  (cons (char-upcase x) acc))
  ⇒  (#\A #\B #\C)
(for/fold ((n 0)) ((c "hello world") :when (char=? c #\o))
  (+ n 1))
  ⇒  2
```

Since the result consists of multiple values if there is more than one accumulator, it is typically received with `let-values` or `call-with-values`:

```scheme
(let-values (((evens odds)
                (for/fold ((e '()) (o '())) ((x (in-range 6)))
                  (if (even? x)
                      (values (cons x e) o)
                      (values e (cons x o))))))
  (list (reverse evens) (reverse odds)))
  ⇒  ((0 2 4) (1 3 5))
```

**(for/sum _(clause ...) body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(for\*/sum _(clause ...) body ..._)**  
**(for/product _(clause ...) body ..._)**  
**(for\*/product _(clause ...) body ..._)**  

Iterates like `for` and `for*`, respectively, and returns the sum (or the product) of the numbers produced by the body. The sum of no elements is 0, the product of no elements is 1.

```scheme
(for/sum ((x '(1 2 3))) x)       ⇒  6
(for/product ((x '(1 2 3 4))) x) ⇒  24
```

**(for/and _(clause ...) body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(for\*/and _(clause ...) body ..._)**  
**(for/or _(clause ...) body ..._)**  
**(for\*/or _(clause ...) body ..._)**  

`for/and` and `for*/and` stop the iteration as soon as the body returns `#f` and return `#f`. If this never happens, they return the value of the last evaluation of the body, or `#t` if the iteration is empty. `for/or` and `for*/or` stop the iteration as soon as the body returns a true value, which is then returned. They return `#f` if this never happens.

```scheme
(for/and ((x '(1 2 3))) (< x 5))                 ⇒  #t
(for/and ((x '(1 2 3))) (< x 3))                 ⇒  #f
(for/or ((x '(1 2 3))) (and (even? x) (* x 10))) ⇒  20
```

**(for/first _(clause ...) body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(for\*/first _(clause ...) body ..._)**  
**(for/last _(clause ...) body ..._)**  
**(for\*/last _(clause ...) body ..._)**  

`for/first` and `for*/first` return the value of the body for the first iteration and stop the iteration. `for/last` and `for*/last` return the value of the body for the last iteration. All forms return `#f` if the iteration is empty.

```scheme
(for/first ((x '(1 2 3 4)) :when (even? x)) x)  ⇒  2
(for/last ((x '(1 2 3 4)) :when (odd? x)) x)    ⇒  3
(for/first ((x '())) x)                         ⇒  #f
```

**(for/hash _(clause ...) body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(for\*/hash _(clause ...) body ..._)**  
**(for/hasheq _(clause ...) body ..._)**  
**(for\*/hasheq _(clause ...) body ..._)**  
**(for/hasheqv _(clause ...) body ..._)**  
**(for\*/hasheqv _(clause ...) body ..._)**  

Iterates like `for` and `for*` and returns a new mutable hash table. The last body expression of each iteration has to return two values, a key and a value, which are added to the hash table. `for/hash` creates an `equal?`-based hash table, `for/hasheq` an `eq?`-based and `for/hasheqv` an `eqv?`-based hash table.

```scheme
(let ((h (for/hash ((k '(a b c)) (v (in-naturals)))
           (values k v))))
  (hashtable-ref h 'b #f))
  ⇒  1
```

## Sequence constructors

**(in-range _end_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(in-range _start end_)**  
**(in-range _start end step_)**  

Returns a sequence of numbers starting at _start_ (default 0), increasing by _step_ (default 1), and stopping before _end_. For a negative _step_ the sequence is decreasing and stops before _end_ is reached from above. A _step_ of 0 produces an infinite sequence.

```scheme
(sequence->list (in-range 4))        ⇒  (0 1 2 3)
(sequence->list (in-range 2 6))      ⇒  (2 3 4 5)
(sequence->list (in-range 10 0 -3))  ⇒  (10 7 4 1)
(sequence->list (in-range 0 1 1/4))  ⇒  (0 1/4 1/2 3/4)
```

**(in-inclusive-range _start end_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(in-inclusive-range _start end step_)**  

Like `in-range`, but the sequence includes _end_ if it is reached by repeatedly adding _step_ to _start_.

```scheme
(sequence->list (in-inclusive-range 1 5))    ⇒  (1 2 3 4 5)
(sequence->list (in-inclusive-range 1 7 3))  ⇒  (1 4 7)
```

**(in-naturals)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(in-naturals _start_)**  

Returns the infinite sequence of integers starting at _start_ (default 0). It is typically combined with a finite sequence in a parallel iteration, which ends with the shortest sequence.

```scheme
(for/list ((i (in-naturals 5)) (x '(a b c))) (cons i x))
  ⇒  ((5 . a) (6 . b) (7 . c))
```

**(in-list _lst_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a sequence of the elements of list _lst_.

**(in-vector _vec_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(in-vector _vec start_)**  
**(in-vector _vec start stop_)**  
**(in-vector _vec start stop step_)**  
**(in-string _str_)**  
**(in-string _str start_)**  
**(in-string _str start stop_)**  
**(in-string _str start stop step_)**  
**(in-bytevector _bvec_)**  
**(in-bytevector _bvec start_)**  
**(in-bytevector _bvec start stop_)**  
**(in-bytevector _bvec start stop step_)**  

Returns a sequence of the elements of a vector, string or bytevector. The optional arguments restrict the sequence to the index range from _start_ (inclusive) to _stop_ (exclusive), using _step_ as increment. With a negative _step_, _start_ has to be greater than _stop_.

```scheme
(sequence->list (in-vector #(a b c d e) 1 4))
  ⇒  (b c d)
(sequence->list (in-vector #(a b c d e) 3 0 -1))
  ⇒  (d c b)
(sequence->list (in-string "hello" 1 4))
  ⇒  (#\e #\l #\l)
(sequence->list (in-bytevector (bytevector 1 2 3 4) 1))
  ⇒  (2 3 4)
```

**(in-hash _table_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(in-hash-keys _table_)**  
**(in-hash-values _table_)**  
**(in-hash-pairs _table_)**  

Returns a sequence of the entries of hash table _table_. `in-hash` produces two values per element (key and value), `in-hash-keys` produces the keys, `in-hash-values` the values, and `in-hash-pairs` pairs of the form `(key . value)`. The order of the elements is unspecified. The entries are determined when the iteration starts.

```scheme
(let ((h (make-equal-hashtable)))
  (hashtable-set! h 'a 1)
  (list (for/list (((k v) (in-hash h))) (list k v))
        (sequence->list (in-hash-keys h))
        (sequence->list (in-hash-values h))
        (sequence->list (in-hash-pairs h))))
  ⇒  (((a 1)) (a) (1) ((a . 1)))
```

**(in-set _set_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a sequence of the elements of set _set_ (see library `(lispkit set)`).

**(in-stream _stream_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a sequence of the elements of stream _stream_ (see library `(lispkit stream)`). The elements of the stream are forced on demand.

**(in-value _x_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a sequence with the single element _x_.

**(in-indexed _seq_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a sequence with two values per element: an element of _seq_ and its index, starting with 0.

```scheme
(for/list (((c i) (in-indexed "abc"))) (cons i c))
  ⇒  ((0 . #\a) (1 . #\b) (2 . #\c))
```

**(in-cycle _seq ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an infinite sequence which repeats the elements of the sequences _seq ..._, one sequence after the other, forever.

```scheme
(for/list ((x (in-cycle '(1 2 3))) (i 7)) x)
  ⇒  (1 2 3 1 2 3 1)
(for/list ((x (in-cycle '(a) '(b c))) (i 5)) x)
  ⇒  (a b c a b)
```

**(in-sequences _seq ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a sequence which consists of the elements of the sequences _seq ..._, one sequence after the other.

```scheme
(sequence->list (in-sequences '(1 2) #(3 4) "ab"))
  ⇒  (1 2 3 4 #\a #\b)
```

**(in-parallel _seq ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a sequence with one element for each position in the sequences _seq ..._. Each element consists of as many values as there are sequences. The sequence ends when the shortest sequence is exhausted.

```scheme
(for/list (((x y) (in-parallel '(1 2 3) "ab"))) (list x y))
  ⇒  ((1 #\a) (2 #\b))
```

**(in-slice _n seq_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a sequence of lists of _n_ consecutive elements of _seq_. The last list may be shorter than _n_.

```scheme
(sequence->list (in-slice 2 '(1 2 3 4 5)))
  ⇒  ((1 2) (3 4) (5))
```

**(in-producer _producer_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(in-producer _producer stop arg ..._)**  

Returns a sequence whose elements are the results of calling procedure _producer_ with arguments _arg ..._ repeatedly. Without _stop_, the sequence is infinite. Otherwise, the sequence ends when the producer returns _stop_ (compared via `eq?`) or, if _stop_ is a procedure, when _stop_ returns true for a result of the producer. The value which ended the sequence is not an element.

```scheme
(let ((n 0))
  (sequence->list
    (in-producer (lambda () (set! n (+ n 1)) (* n n)) 25)))
  ⇒  (1 4 9 16)
(let ((n 0))
  (sequence->list
    (in-producer (lambda () (set! n (+ n 1)) n)
                 (lambda (x) (> x 3)))))
  ⇒  (1 2 3)
```

**(in-port)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(in-port _reader_)**  
**(in-port _reader port_)**  
**(in-lines)**  
**(in-lines _port_)**  

`in-port` returns a sequence of the objects read from textual input port _port_ (default: the current input port) via procedure _reader_ (default: `read`) until the end of file. `in-lines` returns the sequence of lines read from _port_ via `read-line`.

```scheme
(for/list ((line (in-lines (open-input-string "one\ntwo\nthree"))))
  (string-length line))
  ⇒  (3 3 5)
(for/list ((x (in-port read (open-input-string "1 (a b) foo")))) x)
  ⇒  (1 (a b) foo)
```

**empty-sequence** <span style="float:right;text-align:rigth;">[object]</span>  

The sequence without any elements.

## Sequence operations

**(sequence? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ can be used as a sequence; otherwise `#f` is returned.

```scheme
(sequence? '(1 2))  ⇒  #t
(sequence? 5)       ⇒  #t
(sequence? 'a)      ⇒  #f
```

**(sequence->list _seq_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the elements of _seq_. Sequences with more than one value per element are not supported by this procedure.

**(sequence->stream _seq_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a stream (see library `(lispkit stream)`) with the elements of _seq_. It is the counterpart of `in-stream`. The elements are produced on demand: the sequence is advanced only when the corresponding element of the stream is forced, and each element is produced exactly once, no matter how often the stream is traversed. Thus, `sequence->stream` also works for infinite sequences, and for sequences with side effects, such as `in-producer` or `in-lines`. If _seq_ is already a stream, it is returned unchanged. Sequences with more than one value per element are not supported by this procedure.

```scheme
(import (lispkit base) (lispkit sequence) (lispkit stream))

(stream->list (sequence->stream (in-range 3)))
  ⇒  (0 1 2)
(stream->list (stream-take (sequence->stream (in-naturals 5)) 3))
  ⇒  (5 6 7)
```

**(sequence-length _seq_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of elements of _seq_. This procedure iterates over the whole sequence and thus does not terminate for infinite sequences.

```scheme
(sequence-length #(1 2 3))  ⇒  3
```

**(sequence-ref _seq k_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the element of _seq_ at position _k_, starting with 0. It is an error if the sequence has fewer than _k + 1_ elements.

```scheme
(sequence-ref '(a b c) 1)  ⇒  b
```

**(sequence-tail _seq k_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a sequence which consists of the elements of _seq_ without the first _k_ elements. It is an error if _seq_ has fewer than _k_ elements.

```scheme
(sequence->list (sequence-tail '(1 2 3 4) 2))  ⇒  (3 4)
```

**(sequence-append _seq ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a sequence which consists of the elements of the sequences _seq ..._, one after the other. This procedure is equivalent to `in-sequences`.

**(sequence-map _f seq_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a sequence whose elements are the results of applying procedure _f_ to the elements of _seq_. The procedure is applied lazily, whenever an element is requested.

```scheme
(sequence->list (sequence-map (lambda (x) (* x 10)) '(1 2 3)))
  ⇒  (10 20 30)
(for/list ((x (sequence-map (lambda (x) (* x x)) (in-naturals 1))) (i 4)) x)
  ⇒  (1 4 9 16)
```

**(sequence-filter _pred seq_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a sequence of the elements of _seq_ for which predicate _pred_ returns true.

```scheme
(sequence->list (sequence-filter odd? (in-range 10)))
  ⇒  (1 3 5 7 9)
```

**(sequence-for-each _f seq_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies procedure _f_ to each element of _seq_.

**(sequence-fold _f init seq_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Folds the elements of _seq_ from left to right: _f_ is called with the current accumulator value (initially _init_) and an element of _seq_, and returns the next value of the accumulator. Returns the final accumulator value.

```scheme
(sequence-fold + 0 (in-range 5))
  ⇒  10
(sequence-fold (lambda (acc x) (cons x acc)) '() '(1 2 3))
  ⇒  (3 2 1)
```

**(sequence-count _pred seq_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of elements of _seq_ for which predicate _pred_ returns true.

```scheme
(sequence-count even? '(1 2 3 4))  ⇒  2
```

**(sequence-andmap _pred seq_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(sequence-ormap _pred seq_)**  

`sequence-andmap` returns `#f` as soon as _pred_ returns `#f` for an element of _seq_; otherwise it returns the result of the last call to _pred_ (or `#t` if _seq_ is empty). `sequence-ormap` returns the first true result of _pred_, or `#f` if there is none.

```scheme
(sequence-andmap number? '(1 2 3))  ⇒  #t
(sequence-andmap number? '(1 a 3))  ⇒  #f
(sequence-ormap (lambda (x) (and (even? x) (* x 10))) '(1 2 3))  ⇒  20
```

**(sequence-add-between _seq between_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a sequence of the elements of _seq_ with _between_ inserted between each pair of consecutive elements.

```scheme
(sequence->list (sequence-add-between '(1 2 3) 0))
  ⇒  (1 0 2 0 3)
```

**(sequence-generate _seq_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns two thunks, `more?` and `next`, for iterating over _seq_ by hand. `more?` returns `#t` if there is another element, without advancing the iteration. `next` returns the next element (as multiple values, if the sequence has more than one value per element) and advances the iteration.

```scheme
(let-values (((more? next) (sequence-generate '(a b))))
  (let loop ((acc '()))
    (if (more?) (loop (cons (next) acc)) (reverse acc))))   ⇒  (a b)
```
