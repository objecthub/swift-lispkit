# SRFI 117: Queues based on Lists

Library `(srfi 117)` implements _list queues_ as defined by [SRFI 117](https://srfi.schemers.org/srfi-117/srfi-117.html). List queues are mutable ordered collections that can contain any Scheme object. It is cheap to add elements to the front and to the back of a list queue, and to remove elements from both ends. List queues are disjoint from all other types of Scheme objects. Procedures convert between list queues and lists, so that all list procedures, e.g. of [SRFI 1](https://srfi.schemers.org/srfi-1/srfi-1.html), can be used to process the elements of a list queue. The R7RS (large) library `(scheme list-queue)` re-exports all procedures of this library.

```scheme
(import (lispkit base) (srfi 117))

(define q (list-queue 1 2 3))

(list-queue-add-back! q 4)
(list-queue-add-front! q 0)
(list-queue-list q)                          ⇒  (0 1 2 3 4)
(list-queue-remove-front! q)                 ⇒  0
(list-queue-remove-back! q)                  ⇒  4
(list-queue-list (list-queue-map (lambda (x) (* x x)) q))  ⇒  (1 4 9)
(list-queue-front q)                         ⇒  1
(list-queue-back q)                          ⇒  3
```

## List queues in LispKit

According to SRFI 117, a list queue is based on an ordinary list, and a record holds pointers to the first and last pair of this list. Operations like `list-queue-list` return this list without copying, and `make-list-queue` shares storage with the list it is given. Since pairs are immutable in LispKit, this is not possible. In LispKit, a list queue keeps its elements in a growable circular buffer instead. This has the following consequences:

  - Adding elements to the front or to the back, and removing elements from the front or from the back, take constant time (amortized). In particular, `list-queue-remove-back!` is not slower than the other operations, unlike in the reference implementation. `list-queue-front`, `list-queue-back`, `list-queue-empty?` and `list-queue?` take constant time as well.
  - List queues never share storage with lists. `make-list-queue`, `list-queue-set-list!` and the other procedures that take a list copy its elements, and modifying a list queue never has an effect on the list it was created from. The optional argument _last_ of `make-list-queue` and `list-queue-set-list!`, which gives the last pair of the list, is accepted, but ignored.
  - `list-queue-list`, `list-queue-first-last`, `list-queue-remove-all!` and all procedures that process the elements of a list queue as a whole, are linear in the size of the queue. `list-queue-list` returns a _snapshot_: the list does not change when the queue is modified later. The snapshot is cached until the queue is modified, so repeatedly accessing an unmodified queue is cheap.
  - `list-queue-append!` modifies only the first queue. The other queues keep their elements.
  - `list-queue-for-each`, `list-queue-map` and `list-queue-map!` operate on a snapshot of the queue. It is safe to modify the queue from within the procedure passed to them; such modifications do not affect the iteration.
  - A list queue is printed in a compact form, without its elements. Use `list-queue-list` to get the elements.

```scheme
(define q (list-queue 1 2 3))
(define l (list-queue-list q))

(list-queue-add-back! q 4)
l                                  ⇒  (1 2 3)
(list-queue-list q)                ⇒  (1 2 3 4)

(define q1 (make-list-queue '(a b c)))
(define q2 (list-queue-copy q1))
(list-queue-remove-front! q2)
(list-queue-list q1)               ⇒  (a b c)
(list-queue-list q2)               ⇒  (b c)
```

## Constructors

**(make-list-queue _list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-list-queue _list last_)**  

Returns a newly allocated list queue containing the elements of _list_ in order. It is an error if _list_ is not a proper list. In LispKit, the list is copied. If the argument _last_ is provided, it is ignored.

```scheme
(list-queue-list (make-list-queue '(1 2 3)))    ⇒  (1 2 3)
(list-queue-list (make-list-queue '()))         ⇒  ()
```

**(list-queue _element_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a newly allocated list queue containing the given elements in order.

```scheme
(list-queue-list (list-queue 1 2 3))            ⇒  (1 2 3)
(list-queue-list (list-queue))                  ⇒  ()
(list-queue-list (list-queue '(1 2) #f '()))    ⇒  ((1 2) #f ())
```

**(list-queue-copy _list-queue_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a newly allocated list queue containing the elements of _list-queue_. The copy is independent of the original.

```scheme
(let* ((a (list-queue 1 2 3))
       (b (list-queue-copy a)))
  (list-queue-add-front! b 0)
  (list (list-queue-list a) (list-queue-list b)))    ⇒  ((1 2 3) (0 1 2 3))
```

**(list-queue-unfold _stop? mapper successor seed_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(list-queue-unfold _stop? mapper successor seed queue_)**  
**(list-queue-unfold-right _stop? mapper successor seed_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(list-queue-unfold-right _stop? mapper successor seed queue_)**  

`list-queue-unfold` and `list-queue-unfold-right` construct a list queue from a seed value. If the result of applying the predicate _stop?_ to the seed is true, the list queue is returned. Otherwise, the procedure _mapper_ is applied to the seed, yielding the next element, and a new seed is obtained by applying _successor_ to the seed. This is repeated until _stop?_ returns true.

`list-queue-unfold` adds the elements to the front of the list queue such that the element for the first seed ends up at the very front. `list-queue-unfold-right` adds the elements to the back such that the element for the last seed ends up at the very back. If _queue_ is provided, the elements are added to this list queue, which is then returned. Otherwise, a newly allocated list queue is used.

```scheme
(list-queue-list
  (list-queue-unfold (lambda (x) (> x 3)) (lambda (x) (* x 2)) (lambda (x) (+ x 1)) 0))
  ⇒  (0 2 4 6)
(list-queue-list
  (list-queue-unfold-right (lambda (x) (> x 3)) (lambda (x) (* x 2)) (lambda (x) (+ x 1)) 0))
  ⇒  (6 4 2 0)
(list-queue-list
  (list-queue-unfold (lambda (x) (> x 3)) (lambda (x) (* x 2)) (lambda (x) (+ x 1)) 0
                     (list-queue 8)))
  ⇒  (0 2 4 6 8)
(list-queue-list
  (list-queue-unfold-right (lambda (x) (> x 3)) (lambda (x) (* x 2)) (lambda (x) (+ x 1)) 0
                           (list-queue 8)))
  ⇒  (8 6 4 2 0)
```

## Predicates

**(list-queue? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a list queue, and `#f` otherwise. List queues are disjoint from all other types, such as lists, pairs and vectors.

```scheme
(list-queue? (list-queue 1 2))   ⇒  #t
(list-queue? '(1 2))             ⇒  #f
(list-queue? (vector 1 2))       ⇒  #f
```

**(list-queue-empty? _list-queue_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _list-queue_ has no elements, and `#f` otherwise.

```scheme
(list-queue-empty? (list-queue))        ⇒  #t
(list-queue-empty? (list-queue #f))     ⇒  #f
```

## Accessors

**(list-queue-front _list-queue_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(list-queue-back _list-queue_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Return the first, respectively the last element of _list-queue_. It is an error if _list-queue_ is empty.

```scheme
(list-queue-front (list-queue 1 2 3))   ⇒  1
(list-queue-back (list-queue 1 2 3))    ⇒  3
(list-queue-front (list-queue 7))       ⇒  7
```

**(list-queue-list _list-queue_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list that contains the elements of _list-queue_ in order. In LispKit, the result is a snapshot that does not change if _list-queue_ is modified later.

```scheme
(list-queue-list (list-queue 'a 'b 'c))    ⇒  (a b c)
```

**(list-queue-first-last _list-queue_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns two values: the first and the last pair of the list that contains the elements of _list-queue_ in order. If _list-queue_ is empty, two empty lists are returned. Since the pairs are immutable, they are not shared with the list queue.

```scheme
(call-with-values (lambda () (list-queue-first-last (list-queue 1 2 3))) list)  ⇒  ((1 2 3) (3))
(call-with-values (lambda () (list-queue-first-last (list-queue))) list)        ⇒  (() ())
```

## Mutators

**(list-queue-add-front! _list-queue element_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(list-queue-add-back! _list-queue element_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Add _element_ to the beginning, respectively the end of _list-queue_. The result is unspecified.

```scheme
(let ((q (list-queue 2)))
  (list-queue-add-front! q 1)
  (list-queue-add-back! q 3)
  (list-queue-list q))                ⇒  (1 2 3)
```

**(list-queue-remove-front! _list-queue_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(list-queue-remove-back! _list-queue_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Remove the first, respectively the last element of _list-queue_ and return it. It is an error if _list-queue_ is empty.

```scheme
(let ((q (list-queue 1 2 3)))
  (list (list-queue-remove-front! q)
        (list-queue-remove-back! q)
        (list-queue-list q)))         ⇒  (1 3 (2))
```

**(list-queue-remove-all! _list-queue_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes all elements of _list-queue_ and returns them in order as a list.

```scheme
(let ((q (list-queue 1 2 3)))
  (let ((all (list-queue-remove-all! q)))
    (list all (list-queue-empty? q))))   ⇒  ((1 2 3) #t)
```

**(list-queue-set-list! _list-queue list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(list-queue-set-list! _list-queue list last_)**  

Replaces the elements of _list-queue_ with the elements of _list_. The result is unspecified. In LispKit, the elements of _list_ are copied. If the argument _last_ is provided, it is ignored. To apply a list procedure to the elements of a list queue and store the result in the queue, use `(list-queue-set-list! queue (proc (list-queue-list queue)))`.

```scheme
(let ((q (list-queue 1 2 3)))
  (list-queue-set-list! q (reverse (list-queue-list q)))
  (list-queue-list q))               ⇒  (3 2 1)
```

## Whole list queue

**(list-queue-append _list-queue_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(list-queue-concatenate _list-of-list-queues_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`list-queue-append` returns a newly allocated list queue containing the elements of all _list-queue_ arguments in order. `list-queue-concatenate` does the same for a list of list queues. The result does not share storage with any of the arguments.

```scheme
(list-queue-list (list-queue-append (list-queue 1 2) (list-queue) (list-queue 3)))  ⇒  (1 2 3)
(list-queue-list (list-queue-append))                                              ⇒  ()
(list-queue-list (list-queue-concatenate (list (list-queue 1) (list-queue 2 3))))   ⇒  (1 2 3)
```

**(list-queue-append! _list-queue_ ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list queue containing the elements of all _list-queue_ arguments in order. In LispKit, the elements of the second and all following arguments are added to the back of the first argument, which is also the result. The other arguments are not modified. Without arguments, a new empty list queue is returned. It is an error to rely on the contents of the arguments after the procedure returns, as portable code should not make assumptions about them.

```scheme
(let* ((a (list-queue 1 2))
       (b (list-queue 3))
       (r (list-queue-append! a b (list-queue 4))))
  (list (eq? r a) (list-queue-list a)))     ⇒  (#t (1 2 3 4))
```

## Mapping

**(list-queue-map _proc list-queue_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(list-queue-map! _proc list-queue_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

`list-queue-map` applies _proc_ to each element of _list-queue_ and returns a newly allocated list queue containing the results. `list-queue-map!` applies _proc_ to each element of _list-queue_ and replaces the elements of _list-queue_ with the results. In LispKit, _proc_ is applied to the elements in front-to-back order.

```scheme
(list-queue-list (list-queue-map (lambda (x) (* x 10)) (list-queue 1 2 3)))   ⇒  (10 20 30)
(let ((q (list-queue 1 2 3)))
  (list-queue-map! (lambda (x) (+ x 1)) q)
  (list-queue-list q))                                                        ⇒  (2 3 4)
```

**(list-queue-for-each _proc list-queue_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Applies _proc_ to each element of _list-queue_ in front-to-back order, discarding the results. The result is unspecified. Changes of _list-queue_ made by _proc_ do not affect the iteration.

```scheme
(let ((sum 0))
  (list-queue-for-each (lambda (x) (set! sum (+ sum x))) (list-queue 1 2 3))
  sum)                                     ⇒  6
(let ((q (list-queue 1 2)))
  (list-queue-for-each (lambda (x) (list-queue-add-back! q (* x 10))) q)
  (list-queue-list q))                     ⇒  (1 2 10 20)
```

## Library (scheme list-queue)

The R7RS (large) library `(scheme list-queue)` is part of the Scheme Red edition. It exports exactly the same bindings as `(srfi 117)`, and it is also included in the library `(scheme red)`.
