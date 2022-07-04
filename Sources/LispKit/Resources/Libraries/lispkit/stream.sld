;;; LISPKIT STREAM
;;;
;;; Implementation of streams based on the SRFI 41 API. A stream is a sequential
;;; data structure whose elements are computed only on demand. Streams get constructed
;;; with list-like constructors. A stream is either _null_ or is a _pair_ with a stream
;;; in its cdr. Since elements of a stream are computed only when accessed, streams can
;;; be infinite. Once computed, the value of a stream element is cached in case it is
;;; needed again.
;;;
;;; Author of LispKit implementation: Matthias Zenger
;;; Copyright © 2019 Matthias Zenger. All rights reserved.
;;;
;;; Specification of the API, its documentation, and some parts of the implementation:
;;; Copyright © 2007 Philip L. Bewig. All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software, and to permit
;;; persons to whom the Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies or
;;; substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
;;; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
;;; FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(define-library (lispkit stream)

  (export stream?
          stream-type-tag
          stream-null
          stream-null?
          stream-cons
          stream-pair?
          stream-car
          stream-cdr
          stream
          stream-lambda
          define-stream
          stream-let
          display-stream
          list->stream
          port->stream
          stream->list
          stream-append
          stream-concat
          stream-constant
          stream-drop
          stream-drop-while
          stream-filter
          stream-fold
          stream-for-each
          stream-from
          stream-iterate
          stream-length
          stream-map
          stream-match
          stream-of
          stream-range
          stream-ref
          stream-reverse
          stream-scan
          stream-take
          stream-take-while
          stream-unfold
          stream-unfolds
          stream-zip)

  (import (lispkit base))

  (begin
    
    (define (exists proc strms)
      (cond ((null? strms) #f)
            ((pair? strms) (or (proc (car strms)) (exists proc (cdr strms))))
            (else          (error "exists: not a proper list" strms))))

    (define-values (stream-type-tag new-stream-list stream-list? stream-list-ref
                    make-stream-list-subtype)
      (make-type 'stream-list))

    ;; `stream-null` is a stream that, when forced, is a single object, distinguishable
    ;; from all other objects, that represents the null stream. `stream-null` is immutable
    ;; and unique.
    (define stream-null (stream-delay (new-stream-list '())))

    ;; `stream-null?` is a procedure that takes an object and returns `#t` if the object
    ;; is the distinguished null stream and `#f` otherwise. If object is a stream,
    ;; `stream-null?` must force its promise in order to distinguish `stream-null`
    ;; from `stream-pair`.
    (define (stream-null? obj)
      (and (stream? obj) (eqv? (force obj) (force stream-null))))

    ;; `stream-cons` is syntax that accepts an object and a stream and creates a
    ;; newly-allocated stream containing a stream that, when forced, is a stream-pair with
    ;; the object in its `stream-car` and the stream in its `stream-cdr`. `stream-cons`
    ;; must be syntactic, not procedural, because neither object nor stream is evaluated
    ;; when `stream-cons` is called. Since `strm` is not evaluated, when the `stream-pair`
    ;; is created, it is not an error to call `stream-cons` with a stream that is not of
    ;; type stream; however, doing so will cause an error later when the `stream-cdr` of
    ;; the `stream-pair` is accessed. Once created, a `stream-pair` is immutable.
    (define-syntax stream-cons
      (syntax-rules ()
        ((stream-cons obj strm)
         (stream-eager (new-stream-list (cons (stream-delay obj) (stream-lazy strm)))))))

    ;; `stream-pair?` is a procedure that takes an object and returns `#t` if the object
    ;; is a `stream-pair` constructed by `stream-cons` and `#f` otherwise. If object is
    ;; a stream, `stream-pair?` must force its promise in order to distinguish `stream-null`
    ;; from `stream-pair`.
    (define (stream-pair? obj)
      (and (stream? obj)
           (let ((forced (force obj)))
             (and (stream-list? forced) (pair? (stream-list-ref forced))))))

    ;; `stream-car` is a procedure that takes a stream and returns the object stored in
    ;; the `stream-car` of the stream. `stream-car` signals an error if the object passed
    ;; to it is not a `stream-pair`. Calling `stream-car` causes the object stored there
    ;; to be evaluated if it has not yet been; the object’s value is cached in case it is
    ;; needed again.
    (define (stream-car strm)
      (if (stream? strm)
          (force (car (stream-list-ref (force strm))))
          (error "stream-car: $0 not a stream" strm)))

    ;; `stream-cdr` is a procedure that takes a stream and returns the stream stored in
    ;; the `stream-cdr` of the stream. `stream-cdr` signals an error if the object passed
    ;; to it is not a `stream-pair`. Calling `stream-cdr` does not force the promise
    ;; containing the stream stored in the `stream-cdr` of the stream.
    (define (stream-cdr strm)
      (if (stream? strm)
          (cdr (stream-list-ref (force strm)))
          (error "stream-cdr: $0 not a stream" strm)))

    ;; `stream` is syntax that takes zero or more objects and creates a newly-allocated
    ;; stream containing in its elements the objects, in order. Since `stream` is syntactic,
    ;; the objects are evaluated when they are accessed, not when the stream is created.
    ;; If no objects are given, as in `(stream)`, the null stream is returned.
    (define-syntax stream
      (syntax-rules ()
        ((stream)
          stream-null)
        ((stream x y ...)
          (stream-cons x (stream y ...)))))

    ;; `stream-lambda` creates a procedure that returns a stream to evaluate the body of
    ;; the procedure. The last body expression to be evaluated must yield a stream. As with
    ;; normal lambda, arguments may be a single variable name, in which case all the formal
    ;; arguments are collected into a single list, or a list of variable names, which may be
    ;; `null` if there are no arguments, proper if there are an exact number of arguments,
    ;; or dotted if a fixed number of arguments is to be followed by zero or more arguments
    ;; collected into a list. The body must contain at least one expression, and may contain
    ;; internal definitions preceding any expressions to be evaluated.
    (define-syntax stream-lambda
      (syntax-rules ()
        ((stream-lambda formals expr0 expr1 ...)
          (lambda formals (stream-lazy (let () expr0 expr1 ...))))))

    ;; `define-stream` creates a procedure that returns a stream, and may appear anywhere
    ;; a normal define may appear, including as an internal definition, and may have
    ;; internal definitions of its own, including other `define-streams`. The defined
    ;; procedure takes arguments in the same way as `stream-lambda`. `define-stream` is
    ;; syntactic sugar on `stream-lambda`; see also `stream-let`, which is also a sugaring
    ;; of `stream-lambda`.
    (define-syntax define-stream
      (syntax-rules ()
        ((define-stream (name . formal) expr0 expr1 ...)
          (define name (stream-lambda formal expr0 expr1 ...)))))

    ;; `stream-let` creates a local scope that binds each variable to the value of its
    ;; corresponding expression. It additionally binds `tag` to a procedure which takes
    ;; the bound variables as arguments and body as its defining expressions, binding
    ;; the tag with `stream-lambda`. `tag` is in scope within body, and may be called
    ;; recursively. When the expanded expression defined by the `stream-let` is evaluated,
    ;; `stream-let` evaluates the expressions in its body in an environment containing
    ;; the newly-bound variables, returning the value of the last expression evaluated,
    ;; which must yield a stream.
    ;;
    ;; `stream-let` provides syntactic sugar on `stream-lambda`, in the same manner as
    ;; normal `let` provides syntactic sugar on normal `lambda`. However, unlike normal
    ;; `let`, the `tag` is required, not optional, because unnamed `stream-let` is
    ;; meaningless.
    (define-syntax stream-let
      (syntax-rules ()
        ((stream-let tag ((name val) ...) expr1 expr2 ...)
          ((letrec ((tag (stream-lambda (name ...) expr1 expr2 ...))) tag) val ...))))

    ;; `display-stream` displays the first `n` elements of stream `strm` on port `port`
    ;; using string `separator` as a separator. If `n` is not provided, all elements are
    ;; getting displayed. If `separator` is not provided, ", " is used as a default.
    ;; If `port` is not provided, the current output port is used.
    (define (display-stream strm . args)
      (let-optionals args ((n         fx-greatest)
                           (separator ", ")
                           (port      (current-output-port)))
        (assert (stream? strm) (integer? n) (not (negative? n)) (port? port))
        (if (positive? n)
            (unless (stream-null? strm)
              (display (stream-car strm) port)
              (do ((strm (stream-cdr strm) (stream-cdr strm))
                   (n (- n 1) (- n 1)))
                  ((zero? n))
                (display separator port)
                (display (stream-car strm) port))))))

    ;; `list->stream` takes a list of objects and returns a newly-allocated stream
    ;; containing in its elements the objects in the list. Since the objects are
    ;; given in a list, they are evaluated when `list->stream` is called, before
    ;; the stream is created. If the list of objects is null, as in `(list->stream '())`,
    ;; the null stream is returned.
    (define (list->stream lst)
      (assert (list? lst))
      (_list->stream lst))

    (define-stream (_list->stream lst)
      (if (null? lst)
          stream-null
          (stream-cons (car lst) (_list->stream (cdr lst)))))

    ;; `port->stream` takes a port and returns a newly-allocated stream containing
    ;; in its elements the characters on the port. If the port is not given it defaults
    ;; to the current input port. The returned stream has finite length and is terminated
    ;; by `stream-null`.
    (define (port->stream . args)
      (let-optionals args ((p (current-input-port)))
        (assert (input-port? p))
        (_port->stream p)))

    (define-stream (_port->stream p)
      (let ((c (read-char p)))
        (if (eof-object? c)
            stream-null
            (stream-cons c (_port->stream p)))))

    ;; `stream->list` takes a natural number `n` and a stream and returns a newly-allocated
    ;; list containing in its elements the first `n` items in the stream. If the stream has
    ;; less than `n` items, all the items in the stream will be included in the returned
    ;; list. If `n` is not given, it defaults to infinity, which means that unless the
    ;; stream is finite, `stream->list` will never return.
    (define (stream->list strm . args)
      (let-optionals args ((n #f))
        (assert (stream? strm) (or (not n) (and (integer? n) (not (negative? n)))))
        (let loop ((n (if n n -1))
                   (strm strm))
          (if (or (zero? n) (stream-null? strm))
              '()
              (cons (stream-car strm) (loop (- n 1) (stream-cdr strm)))))))

    ;; `stream-append` returns a newly-allocated stream containing in its elements those
    ;; elements contained in its input streams, in order of input. If any of the input
    ;; streams is infinite, no elements of any of the succeeding input streams will appear
    ;; in the output stream; thus, if `x` is infinite, `(stream-append x y)` ≡ `x`.
    (define (stream-append . strms)
      (if (null? strms)
          stream-null
          (_stream-append strms)))

    (define-stream (_stream-append strms)
      (assert (stream? (car strms)))
      (cond ((null? (cdr strms))
              (car strms))
            ((stream-null? (car strms))
              (_stream-append (cdr strms)))
            (else
              (stream-cons (stream-car (car strms))
                           (_stream-append (cons (stream-cdr (car strms)) (cdr strms)))))))

    ;; `stream-concat` takes a stream consisting of one or more streams and returns a
    ;; newly-allocated stream containing all the elements of the input streams. If any
    ;; of the streams in the input stream is infinite, any remaining streams in the
    ;; input stream will never appear in the output stream.
    (define (stream-concat strms)
      (assert (stream? strms))
      (_stream-concat strms))

    (define-stream (_stream-concat strms)
      (cond ((stream-null? strms)
              stream-null)
            ((not (stream? (stream-car strms)))
              (error "stream-concat: object $0 in input stream not a stream" strms))
            ((stream-null? (stream-car strms))
              (_stream-concat (stream-cdr strms)))
            (else
              (stream-cons (stream-car (stream-car strms))
                           (_stream-concat (stream-cons (stream-cdr (stream-car strms))
                                                        (stream-cdr strms)))))))


    ;; `stream-constant` takes one or more objects and returns a newly-allocated
    ;; stream containing in its elements the objects, repeating the objects in
    ;; succession forever.
    (define-stream (stream-constant . objs)
      (cond ((null? objs)
              stream-null)
            ((null? (cdr objs))
              (stream-cons (car objs) (stream-constant (car objs))))
            (else
              (stream-cons (car objs)
                           (apply stream-constant (append (cdr objs) (list (car objs))))))))

    ;; `stream-drop` returns the suffix of the input stream that starts at the next
    ;; element after the first `n` elements. The output stream shares structure with
    ;; the input stream; thus, promises forced in one instance of the stream are also
    ;; forced in the other instance of the stream. If the input stream has less than
    ;; `n` elements, `stream-drop` returns the null stream.
    (define (stream-drop strm n)
      (assert (stream? strm) (integer? n) (not (negative? n)))
      (_stream-drop strm n))

    (define-stream (_stream-drop strm n)
      (if (or (zero? n) (stream-null? strm))
          strm
          (_stream-drop (stream-cdr strm) (- n 1))))

    ;; `stream-drop-while` returns the suffix of the input stream that starts at the
    ;; first element `x` for which `(pred? x)` is `#f`. The output stream shares structure
    ;; with the input stream.
    (define (stream-drop-while pred? strm)
      (assert (procedure? pred?) (stream? strm))
      (_stream-drop-while pred? strm))

    (define-stream (_stream-drop-while pred? strm)
      (if (and (stream-pair? strm) (pred? (stream-car strm)))
          (_stream-drop-while pred? (stream-cdr strm))
          strm))

    ;; `stream-filter` returns a newly-allocated stream that contains only those elements
    ;; `x` of the input stream for which `(pred? x)` is non-`#f`.
    (define (stream-filter pred? strm)
      (assert (procedure? pred?) (stream? strm))
      (letrec ((_stream-filter
                  (stream-lambda (strm)
                    (cond ((stream-null? strm)
                            stream-null)
                          ((pred? (stream-car strm))
                            (stream-cons (stream-car strm) (_stream-filter (stream-cdr strm))))
                          (else
                            (_stream-filter (stream-cdr strm)))))))
        (_stream-filter strm)))

    ;; `stream-fold` applies a binary procedure `proc` to `base` and the first element of
    ;; stream `strm` to compute a new base, then applies the procedure `proc` to the new base
    ;; (1st argument) and the next element of stream (2nd argument) to compute a succeeding
    ;; base, and so on, accumulating a value that is finally returned as the value of
    ;; `stream-fold` when the end of the stream is reached. `strm` must be finite, or
    ;; `stream-fold` will enter an infinite loop.
    ;;
    ;; See also `stream-scan`, which is similar to `stream-fold`, but useful for
    ;; infinite streams. `stream-fold` is a left-fold; there is no corresponding `right-fold`,
    ;; since `right-fold` relies on finite streams that are fully-evaluated, at which time
    ;; they may as well be converted to a list.
    (define (stream-fold proc base strm)
      (assert (procedure? proc) (stream? strm))
      (do ((base base (proc base (stream-car strm)))
           (strm strm (stream-cdr strm)))
          ((stream-null? strm) base)))

    ;; `stream-for-each` applies a procedure element-wise to corresponding elements of
    ;; the input streams for its side-effects. `stream-for-each` stops as soon as any
    ;; of its input streams is exhausted.
    (define (stream-for-each proc . strms)
      (assert (procedure? proc))
      (letrec ((_stream-for-each
                  (lambda (strms)
                    (if (not (exists stream-null? strms))
                        (begin (apply proc (map stream-car strms))
                               (_stream-for-each (map stream-cdr strms)))))))
        (cond ((null? strms)
                (error "stream-for-each: stream argument missing"))
              ((every? stream? strms)
                (_stream-for-each strms))
              (else
                (error "stream-for-each: not all arguments $,0 are streams" strms)))))

    ;; `stream-from` creates a newly-allocated stream that contains `first` as its first
    ;; element and increments each succeeding element by `step`. If `step` is not given
    ;; it defaults to 1. First and step may be of any numeric type. `stream-from` is
    ;; frequently useful as a generator in `stream-of` expressions. See also
    ;; `stream-range` for a similar procedure that creates finite streams.
    (define (stream-from first . args)
      (let-optionals args ((delta 1))
        (assert (number? first) (number? delta))
        (_stream-from first delta)))

    (define-stream (_stream-from first delta)
      (stream-cons first (_stream-from (+ first delta) delta)))


    ;; `stream-iterate` creates a newly-allocated stream containing `base` in its
    ;; first element and applies `proc` to each element in turn to determine the
    ;; succeeding element.
    (define (stream-iterate proc base)
      (assert (procedure? proc))
      (letrec ((_stream-iterate
                  (stream-lambda (base) (stream-cons base (_stream-iterate (proc base))))))
        (_stream-iterate base)))

    ;; `stream-length` takes an input stream and returns the number of elements in
    ;; the stream. It does not evaluate its elements. `stream-length` may only be used
    ;; on finite streams as it enters an infinite loop with infinite streams.
    (define (stream-length strm)
      (assert (stream? strm))
      (do ((len 0 (+ len 1))
           (strm strm (stream-cdr strm)))
          ((stream-null? strm) len)))

    ;; `stream-map` applies a procedure element-wise to corresponding elements of the
    ;; input streams, returning a newly-allocated stream containing elements that are
    ;; the results of those procedure applications. The output stream has as many
    ;; elements as the minimum-length input stream, and may be infinite.
    (define (stream-map proc . strms)
      (assert (procedure? proc) (pair? strms) (every? stream? strms))
      (letrec ((_stream-map
                 (stream-lambda (strms)
                   (if (exists stream-null? strms)
                       stream-null
                       (stream-cons (apply proc (map stream-car strms))
                                    (_stream-map (map stream-cdr strms)))))))

        (_stream-map strms)))

    ;; `stream-match` provides the syntax of pattern-matching for streams. The input
    ;; stream is an expression that evaluates to a stream. Clauses are of the form
    ;; `(pattern [fender] expr)`, consisting of a pattern that matches a stream of
    ;; a particular shape, an optional `fender` that must succeed if the pattern is
    ;; to match, and an expression that is evaluated if the pattern matches.
    ;; There are four types of patterns:
    ;;
    ;;   - `()`: Matches the null stream
    ;;   - `(pat0 pat1 ...)`: Matches a finite stream with length exactly equal to the number
    ;;                        of pattern elements
    ;;   - `(pat0 pat1 ... . patrest)`: Matches an infinite stream, or a finite stream with
    ;;                                  length at least as great as the number of pattern
    ;;                                  elements before the literal dot
    ;;   - `pat`: Matches an entire stream. Should always appear last in the list of clauses;
    ;;            it’s not an error to appear elsewhere, but subsequent clauses could never match
    ;;
    ;; Each pattern element pati may be either:
    ;;
    ;;   - _An identifier_: Matches any stream element. Additionally, the value of the stream
    ;;     element is bound to the variable named by the identifier, which is in scope in the
    ;;     `fender` and expression of the corresponding clause. Each identifier in a single
    ;;     pattern must be unique.
    ;;   - _A literal underscore_: Matches any stream element, but creates no bindings.
    ;;
    ;; The patterns are tested in order, left-to-right, until a matching pattern is found.
    ;; If `fender` is present, it must evaluate as non-`#f` for the match to be successful.
    ;; Pattern variables are bound in the corresponding `fender` and expression. Once the
    ;; matching pattern is found, the corresponding expression is evaluated and returned as
    ;; the result of the match. An error is signaled if no pattern matches the input stream.
    (define-syntax stream-match
      (syntax-rules ()
        ((stream-match strm-expr clause ...)
          (let ((strm strm-expr))
            (cond ((not (stream? strm))
                    (error "stream-match: argument $0 not a stream" strm))
                  ((stream-match-test strm clause) => car)
                    ...
                  (else
                    (error "stream-match: no matching pattern")))))))

    (define-syntax stream-match-test
      (syntax-rules ()
        ((stream-match-test strm (pattern fender expr))
          (stream-match-pattern strm pattern () (and fender (list expr))))
        ((stream-match-test strm (pattern expr))
          (stream-match-pattern strm pattern () (list expr)))))

    (define-syntax stream-match-pattern
      (syntax-rules (_)
        ((stream-match-pattern strm () (binding ...) body)
          (and (stream-null? strm) (let (binding ...) body)))
        ((stream-match-pattern strm (_ . rest) (binding ...) body)
          (and (stream-pair? strm)
               (let ((strm (stream-cdr strm)))
                 (stream-match-pattern strm rest (binding ...) body))))
        ((stream-match-pattern strm (var . rest) (binding ...) body)
          (and (stream-pair? strm)
               (let ((temp (stream-car strm))
                     (strm (stream-cdr strm)))
                 (stream-match-pattern strm rest ((var temp) binding ...) body))))
        ((stream-match-pattern strm _ (binding ...) body)
          (let (binding ...) body))
        ((stream-match-pattern strm var (binding ...) body)
          (let ((var strm) binding ...) body))))


    ;; `stream-of` provides the syntax of stream comprehensions, which generate streams
    ;; by means of looping expressions. The result is a stream of objects of the type
    ;; returned by `expr`. There are four types of clauses:
    ;;
    ;;   - `(var in stream-expr)`: Loop over the elements of `stream-expr`, in order from
    ;;     the start of the stream, binding each element of the stream in turn to `var`.
    ;;     `stream-from` and `stream-range` are frequently useful as generators.
    ;;   - `(var is expr)`: Bind `var` to the value obtained by evaluating `expr`.
    ;;   - `(pred? expr)`: Include in the output stream only those elements `x` for which
    ;;     `(pred? x)` is non-`#f`.
    ;;
    ;; The scope of variables bound in the stream comprehension is the clauses to the right
    ;; of the binding clause (but not the binding clause itself) plus the result expression.
    ;;
    ;; When two or more generators are present, the loops are processed as if they are nested
    ;; from left to right; i.e. the rightmost generator varies fastest. A consequence of this
    ;; is that only the first generator may be infinite and all subsequent generators must
    ;; be finite. If no generators are present, the result of a stream comprehension is a
    ;; stream containing the result expression; thus, `(stream-of 1)` produces a finite stream
    ;; containing only the element 1.
    (define-syntax stream-of
      (syntax-rules ()
        ((_ expr rest ...)
          (stream-of-aux expr stream-null rest ...))))

    (define-syntax stream-of-aux
      (syntax-rules (in is)
        ((stream-of-aux expr base)
          (stream-cons expr base))
        ((stream-of-aux expr base (var in stream) rest ...)
          (stream-let loop ((strm stream))
            (if (stream-null? strm)
                base
                (let ((var (stream-car strm)))
                  (stream-of-aux expr (loop (stream-cdr strm)) rest ...)))))
        ((stream-of-aux expr base (var is exp) rest ...)
          (let ((var exp)) (stream-of-aux expr base rest ...)))
        ((stream-of-aux expr base pred? rest ...)
          (if pred? (stream-of-aux expr base rest ...) base))))

    ;; `stream-range` creates a newly-allocated stream that contains `first` as its first
    ;; element and increments each succeeding element by `step`. The stream is finite and
    ;; ends before `past`, which is not an element of the stream. If `step` is not given it
    ;; defaults to 1 if `first` is less than past and -1 otherwise. First, `past` and `step`
    ;; may be of any numeric type. `stream-range` is frequently useful as a generator in
    ;; `stream-of` expressions.
    (define (stream-range first past . args)
      (let-optionals args ((delta (if (< first past) 1 -1)))
        (assert (number? first) (number? past) (number? delta))
        (let ((lt? (if (< 0 delta) < >)))
          (_stream-range first past delta lt?))))

    (define-stream (_stream-range first past delta lt?)
      (if (lt? first past)
          (stream-cons first (_stream-range (+ first delta) past delta lt?))
          stream-null))

    ;; `stream-ref` returns the `n`-th element of stream, counting from zero. An error is
    ;; signaled if `n` is greater than or equal to the length of stream.
    (define (stream-ref strm n)
      (assert (stream-pair? strm) (integer? n) (not (negative? n)))
      (do ((strm strm (stream-cdr strm))
           (n n (- n 1)))
          ((zero? n) (stream-car strm))
        (if (stream-null? strm)
            (error "stream-ref: beyond end of stream $0" strm))))

    ;; `stream-reverse` returns a newly-allocated stream containing the elements of the
    ;; input stream but in reverse order. `stream-reverse` may only be used with finite
    ;; streams; it enters an infinite loop with infinite streams. `stream-reverse` does
    ;; not force evaluation of the elements of the stream.
    (define (stream-reverse strm)
      (assert (stream? strm))
      (_stream-reverse strm stream-null))

    (define-stream (_stream-reverse strm rev)
      (if (stream-null? strm)
          rev
          (_stream-reverse (stream-cdr strm) (stream-cons (stream-car strm) rev))))

    ;; `stream-scan` accumulates the partial folds of an input stream into a
    ;; newly-allocated output stream. The output stream is the `base` followed by
    ;; `(stream-fold proc base (stream-take i stream))` for each of the first `i`
    ;; elements of stream.
    (define (stream-scan proc base strm)
      (assert (procedure? proc) (stream? strm))
      (letrec ((_stream-scan
                 (stream-lambda (base strm)
                   (if (stream-null? strm)
                       (stream base)
                       (stream-cons base (_stream-scan (proc base (stream-car strm))
                                                       (stream-cdr strm)))))))
        (_stream-scan base strm)))

    ;; `stream-take` takes a non-negative integer `n` and a stream and returns a
    ;; newly-allocated stream containing the first `n` elements of the input stream. If
    ;; the input stream has less than `n` elements, so does the output stream.
    (define (stream-take strm n)
      (assert (stream? strm) (integer? n) (not (negative? n)))
      (_stream-take strm n))

    (define-stream (_stream-take strm n)
      (if (or (stream-null? strm) (zero? n))
          stream-null
          (stream-cons (stream-car strm) (_stream-take (stream-cdr strm) (- n 1)))))

    ;; `stream-take-while` takes a predicate `pred?` and a stream and returns a
    ;; newly-allocated stream containing those elements `x` that form the maximal prefix
    ;; of the input stream for which `(pred? x)` is non-`#f`.
    (define (stream-take-while pred? strm)
      (assert (procedure? pred?) (stream? strm))
      (_stream-take-while strm))

    (define-stream (_stream-take-while pred? trm)
      (cond ((stream-null? strm)
              stream-null)
            ((pred? (stream-car strm))
              (stream-cons (stream-car strm) (_stream-take-while pred? (stream-cdr strm))))
            (else
              stream-null)))

    ;; `stream-unfold` is the fundamental recursive stream constructor. It constructs a
    ;; stream by repeatedly applying `generator` to successive values of `base`, in the
    ;; manner of `stream-iterate`, then applying `mapper` to each of the values so generated,
    ;; appending each of the mapped values to the output stream as long as `(pred? base)`
    ;; is non-`#f`.
    (define (stream-unfold mapper pred? generator base)
      (assert (procedure? mapper) (procedure? pred?) (procedure? generator))
      (letrec ((_stream-unfold
                  (stream-lambda (base)
                    (if (pred? base)
                        (stream-cons (mapper base) (_stream-unfold (generator base)))
                        stream-null))))
        (_stream-unfold base)))

    ;; `stream-unfolds` returns n newly-allocated streams containing those elements produced
    ;; by successive calls to the generator `proc`, which takes the current `seed` as its
    ;; argument and returns n+1 values:
    ;;
    ;; `(proc seed)` → `seed result0 ... resultn-1`
    ;;
    ;; where the returned seed is the input seed to the next call to the generator and
    ;; `resulti` indicates how to produce the next element of the i-th result stream:
    ;;
    ;;   - `(value)`: value is the next car of the result stream
    ;;   - `#f`: no value produced by this iteration of the generator `proc` for the
    ;;     result stream
    ;;   - `()`: the end of the result stream
    ;;
    ;; It may require multiple calls of `proc` to produce the next element of any particular
    ;; result stream.
    (define (stream-unfolds gen seed)
      (define (len-values gen seed)
        (call-with-values (lambda () (gen seed)) (lambda vs (- (length vs) 1))))
      (define unfold-result-stream
        (stream-lambda (gen seed)
          (call-with-values (lambda () (gen seed))
                            (lambda (next . results)
                              (stream-cons results (unfold-result-stream gen next))))))
      (define result-stream->output-stream
        (stream-lambda (result-stream i)
          (let ((result (list-ref (stream-car result-stream) (- i 1))))
            (cond ((pair? result)
                     (stream-cons (car result)
                                  (result-stream->output-stream (stream-cdr result-stream) i)))
                  ((not result)
                     (result-stream->output-stream (stream-cdr result-stream) i))
                  ((null? result)
                     stream-null)
                  (else
                     (error 'stream-unfolds "can't happen"))))))
      (define (result-stream->output-streams result-stream)
        (let loop ((i (len-values gen seed)) (outputs '()))
          (if (zero? i)
              (apply values outputs)
              (loop (- i 1) (cons (result-stream->output-stream result-stream i) outputs)))))
      (assert (procedure? gen))
      (result-stream->output-streams (unfold-result-stream gen seed)))

    ;; `stream-zip` takes one or more input streams and returns a newly-allocated stream
    ;; in which each element is a list (not a stream) of the corresponding elements of the
    ;; input streams. The output stream is as long as the shortest input stream, if any of
    ;; the input streams is finite, or is infinite if all the input streams are infinite.
    (define (stream-zip . strms)
      (assert (pair? strms))
      (_stream-zip strms))

    (define-stream (_stream-zip strms)
      (if (exists stream-null? strms)
          stream-null
          (stream-cons (map stream-car strms) (_stream-zip (map stream-cdr strms)))))
  )
)

