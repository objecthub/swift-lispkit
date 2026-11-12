;;; LISPKIT SEQUENCE
;;;
;;; A sequence encapsulates an ordered collection of values. The elements of
;;; a sequence can be extracted with one of the for syntactic forms, with
;;; the procedures returned by sequence-generate, or by converting the
;;; sequence into a stream. This library is modeled after Racket's
;;; `racket/sequence` library and includes: the `for` family of iteration
;;; forms, sequence constructors (`in-range`, `in-list`, `in-vector`,
;;; `in-hash`, ...) and sequence operations.
;;;
;;; Differences to `racket/sequence`:
;;;   - LispKit does not have keyword syntax. The for-clause and body keywords
;;;     of Racket are written as symbols with a leading colon: `:when`, `:unless`,
;;;     `:break`, `:final`, `:result`, `:length` and `:fill`.
;;;   - `for/hash`, `for/hasheq` and `for/hasheqv` create mutable hash tables.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not
;;; use this file except in compliance with the License. You may obtain a copy
;;; of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed
;;; under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
;;; CONDITIONS OF ANY KIND, either express or implied. See the License for the
;;; specific language governing permissions and limitations under the License.

(define-library (lispkit sequence)

  (export ;; Iteration forms
          for
          for*
          for/list
          for*/list
          for/vector
          for*/vector
          for/fold
          for*/fold
          for/sum
          for*/sum
          for/product
          for*/product
          for/and
          for*/and
          for/or
          for*/or
          for/first
          for*/first
          for/last
          for*/last
          for/hash
          for*/hash
          for/hasheq
          for*/hasheq
          for/hasheqv
          for*/hasheqv
          
          ;; Sequence constructors
          in-range
          in-inclusive-range
          in-naturals
          in-list
          in-vector
          in-string
          in-bytevector
          in-hash
          in-hash-keys
          in-hash-values
          in-hash-pairs
          in-set
          in-stream
          in-value
          in-indexed
          in-cycle
          in-sequences
          in-parallel
          in-slice
          in-producer
          in-port
          in-lines
          empty-sequence
          
          ;; Sequence operations
          sequence?
          sequence->list
          sequence->stream
          sequence-length
          sequence-ref
          sequence-tail
          sequence-append
          sequence-map
          sequence-filter
          sequence-for-each
          sequence-fold
          sequence-count
          sequence-andmap
          sequence-ormap
          sequence-add-between
          sequence-generate)

  (import (lispkit base)
          (only (lispkit set) set? set->list)
          (only (lispkit stream)
                stream-pair? stream-car stream-cdr stream-cons stream-null define-stream))

  (begin

    ;; ----------------------------------------------------------------------------------------
    ;; Iterators
    ;;
    ;; An iterator is a pair of two thunks: `(more? . next)`. Calling `more?` determines whether
    ;; there is another element; calling `next` returns the next element (as multiple values for
    ;; sequences with more than one value per element) and advances the iterator. `more?` may be
    ;; called repeatedly without advancing the iterator.
    ;; ----------------------------------------------------------------------------------------

    (define-record-type <sequence>
      (make-sequence start)
      %sequence?
      (start sequence-start))

    ;; Iterator for a list
    (define (list-iterator lst)
      (let ((pos lst))
        (cons (lambda () (pair? pos))
              (lambda () (let ((x (car pos))) (set! pos (cdr pos)) x)))))

    ;; Iterator for a range of numbers
    (define (range-iterator start end step inclusive?)
      (let ((pos start))
        (cons (cond ((> step 0) (if inclusive? (lambda () (<= pos end)) (lambda () (< pos end))))
                    ((< step 0) (if inclusive? (lambda () (>= pos end)) (lambda () (> pos end))))
                    (else (lambda () #t)))
              (lambda () (let ((x pos)) (set! pos (+ pos step)) x)))))

    ;; Iterator for indexable collections (vectors, strings, bytevectors)
    (define (indexed-iterator ref len start stop step)
      (let* ((stop (or stop len))
             (pos start))
        (when (eqv? step 0)
          (error "sequence step must not be zero"))
        (cons (if (> step 0)
                  (lambda () (< pos stop))
                  (lambda () (> pos stop)))
              (lambda () (let ((x (ref pos))) (set! pos (+ pos step)) x)))))

    ;; Iterator for a hash table; `proc` maps key and value to the produced values
    (define (hashtable-iterator table kind)
      (let-values (((keys vals) (hashtable-entries table)))
        (let ((pos 0)
              (len (vector-length keys)))
          (cons (lambda () (< pos len))
                (lambda ()
                  (let ((k (vector-ref keys pos))
                        (v (vector-ref vals pos)))
                    (set! pos (+ pos 1))
                    (case kind
                      ((entries) (values k v))
                      ((keys)    k)
                      ((values)  v)
                      (else      (cons k v)))))))))

    ;; Iterator for a stream
    (define (stream-iterator strm)
      (let ((pos strm))
        (cons (lambda () (stream-pair? pos))
              (lambda () (let ((x (stream-car pos))) (set! pos (stream-cdr pos)) x)))))

    ;; Returns an iterator for the given sequence
    (define (iterator seq)
      (cond ((%sequence? seq)
              ((sequence-start seq)))
            ((or (pair? seq) (null? seq))
              (list-iterator seq))
            ((vector? seq)
              (indexed-iterator (lambda (i) (vector-ref seq i))
                                (vector-length seq) 0 #f 1))
            ((string? seq)
              (indexed-iterator (lambda (i) (string-ref seq i))
                                (string-length seq) 0 #f 1))
            ((bytevector? seq)
              (indexed-iterator (lambda (i) (bytevector-u8-ref seq i))
                                (bytevector-length seq) 0 #f 1))
            ((and (exact-integer? seq) (>= seq 0))
              (range-iterator 0 seq 1 #f))
            ((hashtable? seq)
              (hashtable-iterator seq 'entries))
            ((set? seq)
              (list-iterator (set->list seq)))
            ((stream? seq)
              (stream-iterator seq))
            (else
              (error "not a sequence" seq))))

    ;; Returns an iterator that is not shared with other users of the sequence
    (define (more? it) ((car it)))
    (define (next! it) ((cdr it)))

    ;; ----------------------------------------------------------------------------------------
    ;; Sequence constructors
    ;; ----------------------------------------------------------------------------------------

    (define in-range
      (case-lambda
        ((end)
          (make-sequence (lambda () (range-iterator 0 end 1 #f))))
        ((start end)
          (make-sequence (lambda () (range-iterator start end 1 #f))))
        ((start end step)
          (make-sequence (lambda () (range-iterator start end step #f))))))

    (define in-inclusive-range
      (case-lambda
        ((start end)
          (make-sequence (lambda () (range-iterator start end 1 #t))))
        ((start end step)
          (make-sequence (lambda () (range-iterator start end step #t))))))

    (define (naturals-iterator start)
      (let ((pos start))
        (cons (lambda () #t)
              (lambda () (let ((x pos)) (set! pos (+ pos 1)) x)))))

    (define in-naturals
      (case-lambda
        (()
          (make-sequence (lambda () (naturals-iterator 0))))
        ((start)
          (make-sequence (lambda () (naturals-iterator start))))))

    (define (in-list lst)
      (make-sequence (lambda () (list-iterator lst))))

    (define in-vector
      (case-lambda
        ((vec)
          (make-sequence (lambda () (iterator vec))))
        ((vec start)
          (make-sequence
            (lambda ()
              (indexed-iterator (lambda (i) (vector-ref vec i)) (vector-length vec) start #f 1))))
        ((vec start stop)
          (make-sequence
            (lambda ()
              (indexed-iterator (lambda (i) (vector-ref vec i)) (vector-length vec) start stop 1))))
        ((vec start stop step)
          (make-sequence
            (lambda ()
              (indexed-iterator (lambda (i) (vector-ref vec i))
                                (vector-length vec) start stop step))))))

    (define in-string
      (case-lambda
        ((str)
          (make-sequence (lambda () (iterator str))))
        ((str start)
          (make-sequence
            (lambda () (indexed-iterator (lambda (i) (string-ref str i))
                                         (string-length str) start #f 1))))
        ((str start stop)
          (make-sequence
            (lambda () (indexed-iterator (lambda (i) (string-ref str i))
                                         (string-length str) start stop 1))))
        ((str start stop step)
          (make-sequence
            (lambda () (indexed-iterator (lambda (i) (string-ref str i))
                        (string-length str) start stop step))))))

    (define in-bytevector
      (case-lambda
        ((bv)
          (make-sequence (lambda () (iterator bv))))
        ((bv start)
          (make-sequence
            (lambda () (indexed-iterator (lambda (i) (bytevector-u8-ref bv i))
                                         (bytevector-length bv) start #f 1))))
        ((bv start stop)
          (make-sequence
            (lambda () (indexed-iterator (lambda (i) (bytevector-u8-ref bv i))
                                         (bytevector-length bv) start stop 1))))
        ((bv start stop step)
          (make-sequence
            (lambda () (indexed-iterator (lambda (i) (bytevector-u8-ref bv i))
                                         (bytevector-length bv) start stop step))))))

    (define (in-hash table)
      (make-sequence (lambda () (hashtable-iterator table 'entries))))

    (define (in-hash-keys table)
      (make-sequence (lambda () (hashtable-iterator table 'keys))))

    (define (in-hash-values table)
      (make-sequence (lambda () (hashtable-iterator table 'values))))

    (define (in-hash-pairs table)
      (make-sequence (lambda () (hashtable-iterator table 'pairs))))

    (define (in-set s)
      (make-sequence (lambda () (list-iterator (set->list s)))))

    (define (in-stream s)
      (make-sequence (lambda () (stream-iterator s))))

    ;; Sequence with just one element `x`
    (define (in-value x)
      (make-sequence (lambda () (list-iterator (list x)))))

    ;; Sequence of the elements of `seq` paired with their indices (as two values)
    (define (in-indexed seq)
      (make-sequence
        (lambda ()
          (let ((it (iterator seq))
                (idx 0))
            (cons (car it)
                  (lambda ()
                    (let ((x (next! it)) (i idx))
                      (set! idx (+ idx 1))
                      (values x i))))))))

    ;; Sequence that repeats the concatenation of the given sequences forever
    (define (in-cycle . seqs)
      (make-sequence
        (lambda ()
          (let ((rest seqs)
                (it #f))
            (define (advance!)
              ;; Moves on to the next non-empty iterator; gives up after a full pass without
              ;; finding an element
              (let loop ((tries (length seqs)))
                (cond ((and it (more? it)) #t)
                      ((zero? tries) (set! it #f) #f)
                      (else
                        (when (null? rest) (set! rest seqs))
                        (set! it (iterator (car rest)))
                        (set! rest (cdr rest))
                        (loop (- tries 1))))))
            (cons (lambda () (and (pair? seqs) (advance!)))
                  (lambda () (advance!) (next! it)))))))

    ;; Sequence that is the concatenation of the given sequences
    (define (in-sequences . seqs)
      (make-sequence
        (lambda ()
          (let ((rest seqs)
                (it #f))
            (define (advance!)
              (let loop ()
                (cond ((and it (more? it)) #t)
                      ((null? rest) #f)
                      (else (set! it (iterator (car rest)))
                            (set! rest (cdr rest))
                            (loop)))))
            (cons advance!
                  (lambda () (advance!) (next! it)))))))

    ;; Sequence that iterates over all sequences in parallel; ends with the shortest one
    (define (in-parallel . seqs)
      (make-sequence
        (lambda ()
          (let ((its (map iterator seqs)))
            (cons (lambda () (let loop ((l its))
                                 (or (null? l) (and (more? (car l)) (loop (cdr l))))))
                  (lambda () (apply values (map next! its))))))))

    ;; Sequence of lists with `n` elements of `seq` each (the last one may be shorter)
    (define (in-slice n seq)
      (make-sequence
        (lambda ()
          (let ((it (iterator seq)))
            (cons (lambda () (more? it))
                  (lambda ()
                    (let loop ((k n) (acc '()))
                      (if (and (> k 0) (more? it))
                          (loop (- k 1) (cons (next! it) acc))
                          (reverse acc)))))))))

    ;; Sequence of the results of repeatedly calling `producer` with `args`. The sequence
    ;; ends when `stop` is returned (compared with `eq?`) or, if `stop` is a procedure, when
    ;; `stop` returns true for the result. Without a stop value, the sequence is infinite.
    (define in-producer
      (case-lambda
        ((producer)
          (make-sequence (lambda () (cons (lambda () #t) producer))))
        ((producer stop . args)
          (make-sequence
            (lambda ()
              (let ((ready? #f)
                    (buf #f))
                (define (fetch!)
                  (unless ready?
                    (set! buf (apply producer args))
                    (set! ready? #t)))
                (cons (lambda ()
                        (fetch!)
                        (not (if (procedure? stop) (stop buf) (eq? buf stop))))
                      (lambda ()
                        (fetch!)
                        (set! ready? #f)
                        buf))))))))

    ;; Sequence of the results of reading from port `in` with `reader` until the end of file
    (define in-port
      (case-lambda
        (()            (in-port read (current-input-port)))
        ((reader)      (in-port reader (current-input-port)))
        ((reader in)   (in-producer (lambda () (reader in)) eof-object?))))

    ;; Sequence of the lines read from port `in`
    (define in-lines
      (case-lambda
        (()    (in-lines (current-input-port)))
        ((in)  (in-port read-line in))))

    (define empty-sequence (make-sequence (lambda () (list-iterator '()))))

    ;; ----------------------------------------------------------------------------------------
    ;; Sequence operations
    ;; ----------------------------------------------------------------------------------------

    (define (sequence? obj)
      (or (%sequence? obj)
          (pair? obj)
          (null? obj)
          (vector? obj)
          (string? obj)
          (bytevector? obj)
          (and (exact-integer? obj) (>= obj 0))
          (hashtable? obj)
          (set? obj)
          (stream? obj)))

    (define (sequence->list seq)
      (let ((it (iterator seq)))
        (let loop ((acc '()))
          (if (more? it)
              (loop (cons (next! it) acc))
              (reverse acc)))))

    ;; Stream whose elements are produced by iterator `it`. The iterator is advanced exactly
    ;; once per stream element, when the corresponding stream pair is forced. The element has
    ;; to be fetched before `stream-cons` is called because `stream-cons` delays its arguments.
    (define-stream (iterator-stream it)
      (if (more? it)
          (let ((x (next! it)))
            (stream-cons x (iterator-stream it)))
          stream-null))

    (define (sequence->stream seq)
      (if (stream? seq)
          seq
          (iterator-stream (iterator seq))))

    (define (sequence-length seq)
      (let ((it (iterator seq)))
        (let loop ((n 0))
          (if (more? it)
              (begin (next! it) (loop (+ n 1)))
              n))))

    (define (sequence-ref seq k)
      (let ((it (iterator seq)))
        (let loop ((i k))
          (cond ((not (more? it))
                 (error "sequence-ref: sequence ended before index" k))
                ((zero? i) (next! it))
                (else (next! it) (loop (- i 1)))))))

    (define (sequence-tail seq k)
      (make-sequence
        (lambda ()
          (let ((it (iterator seq)))
            (let loop ((i k))
              (when (> i 0)
                (if (more? it)
                    (begin (next! it) (loop (- i 1)))
                    (error "sequence-tail: sequence ended before index" k))))
            it))))

    (define (sequence-append . seqs)
      (apply in-sequences seqs))

    (define (sequence-map f seq)
      (make-sequence
        (lambda ()
          (let ((it (iterator seq)))
            (cons (car it)
                  (lambda () (call-with-values (cdr it) f)))))))

    (define (sequence-filter pred seq)
      (make-sequence
        (lambda ()
          (let ((it (iterator seq))
                (ready? #f)
                (buf '()))
            (define (fetch!)
              ;; Looks ahead for the next element satisfying `pred`
              (let loop ()
                (cond (ready? #t)
                      ((more? it)
                       (let ((vals (call-with-values (cdr it) list)))
                         (if (apply pred vals)
                             (begin (set! buf vals) (set! ready? #t) #t)
                             (loop))))
                      (else #f))))
            (cons fetch!
                  (lambda ()
                    (fetch!)
                    (set! ready? #f)
                    (apply values buf)))))))

    (define (sequence-for-each f seq)
      (let ((it (iterator seq)))
        (let loop ()
          (when (more? it)
            (call-with-values (cdr it) f)
            (loop)))))

    (define (sequence-fold f init seq)
      (let ((it (iterator seq)))
        (let loop ((acc init))
          (if (more? it)
              (loop (call-with-values (cdr it) (lambda args (apply f acc args))))
              acc))))

    (define (sequence-count pred seq)
      (let ((it (iterator seq)))
        (let loop ((n 0))
          (if (more? it)
              (loop (if (call-with-values (cdr it) pred) (+ n 1) n))
              n))))

    (define (sequence-andmap pred seq)
      (let ((it (iterator seq)))
        (let loop ((res #t))
          (if (more? it)
              (let ((r (call-with-values (cdr it) pred)))
                (and r (loop r)))
              res))))

    (define (sequence-ormap pred seq)
      (let ((it (iterator seq)))
        (let loop ()
          (and (more? it)
               (or (call-with-values (cdr it) pred)
                   (loop))))))

    (define (sequence-add-between seq between)
      (make-sequence
        (lambda ()
          (let ((it (iterator seq))
                (started? #f)
                (sep? #f))
            (cons (lambda () (more? it))
                  (lambda ()
                    (cond ((and started? (not sep?))
                           (set! sep? #t)
                           between)
                          (else
                           (set! started? #t)
                           (set! sep? #f)
                           (next! it)))))))))

    ;; Returns two thunks `(more?)` and `(next)` for iterating over `seq`
    (define (sequence-generate seq)
      (let ((it (iterator seq)))
        (values (car it) (cdr it))))

    ;; ----------------------------------------------------------------------------------------
    ;; Iteration forms
    ;;
    ;; All forms are defined in terms of the internal macro `%for-nest`:
    ;;
    ;;   (%for-nest mode kind exit (acc ...) (clause ...) (body ...))
    ;;
    ;; where `mode` is `m:par` (clauses form groups of parallel sequences) or `m:one` (every
    ;; sequence clause is nested; this is `for*`), `kind` determines how the value of the body
    ;; updates the accumulators, `exit` is a prefix of an expression which is called with the
    ;; accumulators once the nest is finished, and `acc ...` are the accumulator variables.
    ;; All loops are in tail position. Halting the whole iteration (`:break`, `for/first`, ...)
    ;; is thus done by just returning the accumulators.
    ;; ----------------------------------------------------------------------------------------

    ;; Binds the accumulator variables to the values returned by `expr`
    (define-syntax %let-accs
      (syntax-rules ()
        ((_ (a) expr body) (let ((a expr)) body))
        ((_ (a ...) expr body) (let-values (((a ...) expr)) body))))

    ;; Evaluates `seg ...` for effect (if there are any), followed by `code`
    (define-syntax %seq-then
      (syntax-rules ()
        ((_ () code) code)
        ((_ (seg ...) code) (begin (let () seg ... (void)) code))))

    ;; Wraps the inner code of a loop into the tests and bindings for each sequence
    (define-syntax %for-frames
      (syntax-rules ()
        ((_ () (ex ...) (a ...) inner) inner)
        ((_ ((test (id) expr) frame ...) (ex ...) (a ...) inner)
          (if test
              (let ((id expr)) (%for-frames (frame ...) (ex ...) (a ...) inner))
              (ex ... a ...)))
        ((_ ((test (id ...) expr) frame ...) (ex ...) (a ...) inner)
          (if test
              (let-values (((id ...) expr)) (%for-frames (frame ...) (ex ...) (a ...) inner))
              (ex ... a ...)))))

    ;; Computes the new accumulator values from the value of the body and continues
    (define-syntax %for-finish
      (syntax-rules (k:fold k:effect k:list k:sum k:product k:last k:and k:or k:first k:hash k:vec)
        ((_ k:fold (ex ...) (a ...) (seg ...))
          (%let-accs (a ...) (let () seg ...) (ex ... a ...)))
        ((_ k:effect (ex ...) (a ...) (seg ...))
          (begin (let () seg ...) (ex ... a ...)))
        ((_ k:list (ex ...) (a) (seg ...))
          (let ((a (cons (let () seg ...) a))) (ex ... a)))
        ((_ k:sum (ex ...) (a) (seg ...))
          (let ((a (+ a (let () seg ...)))) (ex ... a)))
        ((_ k:product (ex ...) (a) (seg ...))
          (let ((a (* a (let () seg ...)))) (ex ... a)))
        ((_ k:last (ex ...) (a) (seg ...))
          (let ((a (let () seg ...))) (ex ... a)))
        ((_ k:and (ex ...) (a) (seg ...))
          (let ((a (let () seg ...))) (if a (ex ... a) (values a))))
        ((_ k:or (ex ...) (a) (seg ...))
          (let ((a (let () seg ...))) (if a (values a) (ex ... a))))
        ((_ k:first (ex ...) (a) (seg ...))
          (let ((a (let () seg ...))) (values a)))
        ((_ k:hash (ex ...) (a) (seg ...))
          (let-values (((key val) (let () seg ...)))
            (hashtable-set! a key val)
            (ex ... a)))
        ((_ (k:vec v len) (ex ...) (i) (seg ...))
          (let ((x (let () seg ...)))
            (vector-set! v i x)
            (let ((i (+ i 1)))
              (if (>= i len) (values i) (ex ... i)))))))

    ;; Processes the body forms, which may contain :break and :final clauses
    (define-syntax %for-body
      (syntax-rules (:break :final)
        ((_ kind ex accs (seg ...) ())
          (%for-finish kind ex accs (seg ...)))
        ((_ kind ex (a ...) (seg ...) (:break g . rest))
          (%seq-then (seg ...)
            (if g (values a ...) (%for-body kind ex (a ...) () rest))))
        ((_ kind (ex ...) (a ...) (seg ...) (:final g . rest))
          (%seq-then (seg ...)
            (let ((f g))
              (let ((k (lambda (a ...) (if f (values a ...) (ex ... a ...)))))
                (%for-body kind (k) (a ...) () rest)))))
        ((_ kind ex accs (seg ...) (b . rest))
          (%for-body kind ex accs (seg ... b) rest))))

    ;; Generates a loop for a group of sequence clauses
    (define-syntax %for-emit
      (syntax-rules ()
        ((_ (mode kind (ex ...) (a ...) bodies) (o ...) ((ln li) ...) (s ...) (fr ...) rest)
          (let* (o ...)
            (let loop ((ln li) ... (a a) ...)
              (%for-frames (fr ...) (ex ...) (a ...)
                (%for-nest mode kind (loop s ...) (a ...) rest bodies)))))))

    (define-syntax %for-gather-next
      (syntax-rules (m:par m:one)
        ((_ (m:par . c) o l s f more) (%for-gather (m:par . c) o l s f more))
        ((_ (m:one . c) o l s f more) (%for-emit (m:one . c) o l s f more))))

    ;; Collects the sequence clauses of a group (a single clause for `for*`) and determines
    ;; for each clause the outer bindings, the loop variables, the step expressions and the
    ;; frame, i.e. test, variables and value expression.
    (define-syntax %for-gather
      (syntax-rules (in-range in-naturals in-list in-vector in-string in-bytevector)
        ;; Normalization of single variables in parentheses
        ((_ ctx o l s f (((id) seq) . more))
          (%for-gather ctx o l s f ((id seq) . more)))
        ;; Clauses with multiple variables
        ((_ ctx (o ...) (l ...) (s ...) (f ...) (((id1 id2 ...) seq) . more))
          (%for-gather-next ctx (o ... (it (iterator seq))) (l ...) (s ...)
                            (f ... (((car it)) (id1 id2 ...) ((cdr it)))) more))
        ;; in-range
        ((_ ctx (o ...) (l ...) (s ...) (f ...) ((id (in-range e)) . more))
          (%for-gather-next ctx (o ... (stop e)) (l ... (pos 0)) (s ... (+ pos 1))
                            (f ... ((< pos stop) (id) pos)) more))
        ((_ ctx (o ...) (l ...) (s ...) (f ...) ((id (in-range e1 e2)) . more))
          (%for-gather-next ctx (o ... (start e1) (stop e2)) (l ... (pos start)) (s ... (+ pos 1))
                            (f ... ((< pos stop) (id) pos)) more))
        ((_ ctx (o ...) (l ...) (s ...) (f ...) ((id (in-range e1 e2 e3)) . more))
          (%for-gather-next ctx (o ... (start e1) (stop e2) (step e3)) (l ... (pos start))
                            (s ... (+ pos step))
                            (f ... ((cond ((> step 0) (< pos stop)) ((< step 0) (> pos stop)) (else #t))
                                    (id) pos))
                            more))
        ;; in-naturals
        ((_ ctx (o ...) (l ...) (s ...) (f ...) ((id (in-naturals)) . more))
          (%for-gather-next ctx (o ...) (l ... (pos 0)) (s ... (+ pos 1))
                            (f ... (#t (id) pos)) more))
        ((_ ctx (o ...) (l ...) (s ...) (f ...) ((id (in-naturals e)) . more))
          (%for-gather-next ctx (o ...) (l ... (pos e)) (s ... (+ pos 1))
                            (f ... (#t (id) pos)) more))
        ;; in-list
        ((_ ctx (o ...) (l ...) (s ...) (f ...) ((id (in-list e)) . more))
          (%for-gather-next ctx (o ... (lst e)) (l ... (pos lst)) (s ... (cdr pos))
                            (f ... ((pair? pos) (id) (car pos))) more))
        ;; in-vector, in-string and in-bytevector (without optional arguments)
        ((_ ctx (o ...) (l ...) (s ...) (f ...) ((id (in-vector e)) . more))
          (%for-gather-next ctx (o ... (vec e) (len (vector-length vec))) (l ... (pos 0))
                            (s ... (+ pos 1))
                            (f ... ((< pos len) (id) (vector-ref vec pos))) more))
        ((_ ctx (o ...) (l ...) (s ...) (f ...) ((id (in-string e)) . more))
          (%for-gather-next ctx (o ... (str e) (len (string-length str))) (l ... (pos 0))
                            (s ... (+ pos 1))
                            (f ... ((< pos len) (id) (string-ref str pos))) more))
        ((_ ctx (o ...) (l ...) (s ...) (f ...) ((id (in-bytevector e)) . more))
          (%for-gather-next ctx (o ... (bv e) (len (bytevector-length bv))) (l ... (pos 0))
                            (s ... (+ pos 1))
                            (f ... ((< pos len) (id) (bytevector-u8-ref bv pos))) more))
        ;; Generic sequences
        ((_ ctx (o ...) (l ...) (s ...) (f ...) ((id seq) . more))
          (%for-gather-next ctx (o ... (it (iterator seq))) (l ...) (s ...)
                            (f ... (((car it)) (id) ((cdr it)))) more))
        ;; End of group
        ((_ ctx o l s f rest)
          (%for-emit ctx o l s f rest))))

    (define-syntax %for-nest
      (syntax-rules (:when :unless :break :final)
        ;; Guards
        ((_ mode kind (ex ...) (a ...) (:when g . rest) bodies)
          (if g
              (%for-nest mode kind (ex ...) (a ...) rest bodies)
              (ex ... a ...)))
        ((_ mode kind (ex ...) (a ...) (:unless g . rest) bodies)
          (if g
              (ex ... a ...)
              (%for-nest mode kind (ex ...) (a ...) rest bodies)))
        ((_ mode kind (ex ...) (a ...) (:break g . rest) bodies)
          (if g
              (values a ...)
              (%for-nest mode kind (ex ...) (a ...) rest bodies)))
        ((_ mode kind (ex ...) (a ...) (:final g . rest) bodies)
          (let ((f g))
            (let ((k (lambda (a ...) (if f (values a ...) (ex ... a ...)))))
              (%for-nest mode kind (k) (a ...) rest bodies))))
        ;; Sequence clauses
        ((_ mode kind ex accs ((id seq) . more) bodies)
          (%for-gather (mode kind ex accs bodies) () () () () ((id seq) . more)))
        ;; Invalid clauses
        ((_ mode kind ex accs (bad . more) bodies)
          (syntax-error "invalid for-clause" bad))
        ;; Body
        ((_ mode kind ex accs () bodies)
          (%for-body kind ex accs () bodies))))

    (define-syntax for
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((u (void)))
            (%for-nest m:par k:effect (values) (u) clauses bodies)))))

    (define-syntax for*
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((u (void)))
            (%for-nest m:one k:effect (values) (u) clauses bodies)))))

    (define-syntax for/fold
      (syntax-rules (:result)
        ((_ ((a i) ... :result r) clauses . bodies)
          (let ((a i) ...)
            (%let-accs (a ...) (%for-nest m:par k:fold (values) (a ...) clauses bodies) r)))
        ((_ ((a i) ...) clauses . bodies)
          (let ((a i) ...)
            (%for-nest m:par k:fold (values) (a ...) clauses bodies)))))

    (define-syntax for*/fold
      (syntax-rules (:result)
        ((_ ((a i) ... :result r) clauses . bodies)
          (let ((a i) ...)
            (%let-accs (a ...) (%for-nest m:one k:fold (values) (a ...) clauses bodies) r)))
        ((_ ((a i) ...) clauses . bodies)
          (let ((a i) ...)
            (%for-nest m:one k:fold (values) (a ...) clauses bodies)))))

    (define-syntax for/list
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc '()))
            (reverse (%for-nest m:par k:list (values) (acc) clauses bodies))))))

    (define-syntax for*/list
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc '()))
            (reverse (%for-nest m:one k:list (values) (acc) clauses bodies))))))

    (define-syntax %for/vector
      (syntax-rules ()
        ((_ mode n fill clauses bodies)
          (let* ((len n)
                 (vec (make-vector len fill)))
            (if (> len 0)
                (let ((i 0))
                  (%for-nest mode (k:vec vec len) (values) (i) clauses bodies)))
            vec))))

    (define-syntax for/vector
      (syntax-rules (:length :fill)
        ((_ :length n :fill fill clauses . bodies)
          (%for/vector m:par n fill clauses bodies))
        ((_ :length n clauses . bodies)
          (%for/vector m:par n 0 clauses bodies))
        ((_ clauses . bodies)
          (let ((acc '()))
            (list->vector
              (reverse (%for-nest m:par k:list (values) (acc) clauses bodies)))))))

    (define-syntax for*/vector
      (syntax-rules (:length :fill)
        ((_ :length n :fill fill clauses . bodies)
          (%for/vector m:one n fill clauses bodies))
        ((_ :length n clauses . bodies)
          (%for/vector m:one n 0 clauses bodies))
        ((_ clauses . bodies)
          (let ((acc '()))
            (list->vector
              (reverse (%for-nest m:one k:list (values) (acc) clauses bodies)))))))

    (define-syntax for/sum
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc 0)) (%for-nest m:par k:sum (values) (acc) clauses bodies)))))

    (define-syntax for*/sum
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc 0)) (%for-nest m:one k:sum (values) (acc) clauses bodies)))))

    (define-syntax for/product
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc 1)) (%for-nest m:par k:product (values) (acc) clauses bodies)))))

    (define-syntax for*/product
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc 1)) (%for-nest m:one k:product (values) (acc) clauses bodies)))))

    (define-syntax for/and
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc #t)) (%for-nest m:par k:and (values) (acc) clauses bodies)))))

    (define-syntax for*/and
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc #t)) (%for-nest m:one k:and (values) (acc) clauses bodies)))))

    (define-syntax for/or
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc #f)) (%for-nest m:par k:or (values) (acc) clauses bodies)))))

    (define-syntax for*/or
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc #f)) (%for-nest m:one k:or (values) (acc) clauses bodies)))))

    (define-syntax for/first
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc #f)) (%for-nest m:par k:first (values) (acc) clauses bodies)))))

    (define-syntax for*/first
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc #f)) (%for-nest m:one k:first (values) (acc) clauses bodies)))))

    (define-syntax for/last
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc #f)) (%for-nest m:par k:last (values) (acc) clauses bodies)))))

    (define-syntax for*/last
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc #f)) (%for-nest m:one k:last (values) (acc) clauses bodies)))))

    (define-syntax for/hash
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc (make-equal-hashtable))) (%for-nest m:par k:hash (values) (acc) clauses bodies)))))

    (define-syntax for*/hash
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc (make-equal-hashtable))) (%for-nest m:one k:hash (values) (acc) clauses bodies)))))

    (define-syntax for/hasheq
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc (make-eq-hashtable))) (%for-nest m:par k:hash (values) (acc) clauses bodies)))))

    (define-syntax for*/hasheq
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc (make-eq-hashtable))) (%for-nest m:one k:hash (values) (acc) clauses bodies)))))

    (define-syntax for/hasheqv
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc (make-eqv-hashtable))) (%for-nest m:par k:hash (values) (acc) clauses bodies)))))

    (define-syntax for*/hasheqv
      (syntax-rules ()
        ((_ clauses . bodies)
          (let ((acc (make-eqv-hashtable))) (%for-nest m:one k:hash (values) (acc) clauses bodies)))))
  )
)
