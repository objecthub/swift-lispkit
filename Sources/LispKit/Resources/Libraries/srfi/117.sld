;;; SRFI 117
;;; Queues based on lists
;;;
;;; List queues are mutable ordered collections that can contain any Scheme object. Each list
;;; queue is based on an ordinary Scheme list containing the elements of the list queue by
;;; maintaining pointers to the first and last pairs of the list. It's cheap to add or remove
;;; elements from the front of the list or to add elements to the back, but not to remove
;;; elements from the back. List queues are disjoint from other types of Scheme objects.
;;;
;;; Copyright © 2014 John Cowan. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to the following
;;; conditions:
;;; The above copyright notice and this permission notice shall be included in all copies
;;; or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Implementation notes:
;;;   Pairs are immutable in LispKit. Therefore, list queues cannot share storage with lists
;;;   or maintain pointers into the pairs of a list. Instead, a list queue is a record that
;;;   holds its elements in a growable circular buffer. This makes adding and removing
;;;   elements at both ends O(1) (amortized), including `list-queue-remove-back!`. The list
;;;   returned by `list-queue-list` is a snapshot, which is cached until the queue is
;;;   modified. Conversions from lists, e.g. by `make-list-queue`, copy the list.

(define-library (srfi 117)

  (export make-list-queue
          list-queue
          list-queue-copy
          list-queue-unfold
          list-queue-unfold-right
          list-queue?
          list-queue-empty?
          list-queue-front
          list-queue-back
          list-queue-list
          list-queue-first-last
          list-queue-add-front!
          list-queue-add-back!
          list-queue-remove-front!
          list-queue-remove-back!
          list-queue-remove-all!
          list-queue-set-list!
          list-queue-append
          list-queue-append!
          list-queue-concatenate
          list-queue-map
          list-queue-map!
          list-queue-for-each)

  (import (lispkit base))

  (begin

    ;; === The list queue type ===

    ;; A list queue is an object of a new type whose payload is a procedure returning the state
    ;; of the queue; this way, queues are printed compactly instead of showing their contents.
    ;; The state is a vector with the following slots: a circular buffer storing the elements
    ;; of the queue, the index in the buffer of the first element, the number of elements, and a
    ;; cache of the elements as a list. The cache is #f whenever it is out of date (the cache of
    ;; the empty queue is '()).
    (define-values (queue-type-tag new-queue-object list-queue? queue-ref make-queue-subtype)
      (make-type 'list-queue))

    (define-syntax st-buffer
      (syntax-rules () ((_ st) (vector-ref st 0))))
    (define-syntax st-head
      (syntax-rules () ((_ st) (vector-ref st 1))))
    (define-syntax st-size
      (syntax-rules () ((_ st) (vector-ref st 2))))
    (define-syntax st-snapshot
      (syntax-rules () ((_ st) (vector-ref st 3))))
    (define-syntax set-st-buffer!
      (syntax-rules () ((_ st x) (vector-set! st 0 x))))
    (define-syntax set-st-head!
      (syntax-rules () ((_ st x) (vector-set! st 1 x))))
    (define-syntax set-st-size!
      (syntax-rules () ((_ st x) (vector-set! st 2 x))))
    (define-syntax set-st-snapshot!
      (syntax-rules () ((_ st x) (vector-set! st 3 x))))

    (define min-capacity 8)

    (define (new-queue)
      (let ((state (vector (make-vector min-capacity #f) 0 0 '())))
        (new-queue-object (lambda () state))))

    ;; Returns the state of `queue`; signals an error if `queue` is not a list queue
    (define (state who queue)
      (if (list-queue? queue)
          ((queue-ref queue))
          (error (string-append who ": argument is not a list queue:") queue)))

    ;; Returns the state of `queue`; signals an error if `queue` is not a non-empty list queue
    (define (state/nonempty who queue)
      (let ((st (state who queue)))
        (if (eqv? (st-size st) 0)
            (error (string-append who ": list queue is empty") queue)
            st)))

    (define (check-list who obj)
      (if (not (list? obj))
          (error (string-append who ": argument is not a proper list:") obj)))

    ;; Doubles the capacity of the buffer
    (define (grow! st)
      (let* ((buffer (st-buffer st))
             (capacity (vector-length buffer))
             (head (st-head st))
             (size (st-size st))
             (new-buffer (make-vector (* 2 capacity) #f)))
        (do ((i 0 (+ i 1)))
            ((= i size))
          (vector-set! new-buffer i (vector-ref buffer (modulo (+ head i) capacity))))
        (set-st-buffer! st new-buffer)
        (set-st-head! st 0)))

    ;; Replaces the contents of the queue with the elements of the proper list `lst`
    (define (fill! st lst)
      (let* ((size (length lst))
             (buffer (make-vector (max min-capacity (+ size (quotient size 2))) #f)))
        (do ((i 0 (+ i 1))
             (rest lst (cdr rest)))
            ((null? rest))
          (vector-set! buffer i (car rest)))
        (set-st-buffer! st buffer)
        (set-st-head! st 0)
        (set-st-size! st size)
        (set-st-snapshot! st lst)))

    ;; Returns the elements of the queue as a list
    (define (elements st)
      (or (st-snapshot st)
          (let* ((buffer (st-buffer st))
                 (capacity (vector-length buffer))
                 (head (st-head st)))
            (let loop ((i (- (st-size st) 1)) (acc '()))
              (if (< i 0)
                  (begin (set-st-snapshot! st acc) acc)
                  (loop (- i 1)
                        (cons (vector-ref buffer (modulo (+ head i) capacity)) acc)))))))

    ;; === Constructors ===

    ;; The optional argument `last` is accepted for compatibility with the reference
    ;; implementation. Since lists are copied, it is ignored.
    (define (make-list-queue lst . rest)
      (check-list "make-list-queue" lst)
      (let ((queue (new-queue)))
        (if (pair? lst) (fill! ((queue-ref queue)) lst))
        queue))

    (define (list-queue . elems)
      (make-list-queue elems))

    (define (list-queue-copy queue)
      (make-list-queue (elements (state "list-queue-copy" queue))))

    ;; Returns the mapped seeds as list in reverse order
    (define (unfold-reverse stop? mapper successor seed)
      (let loop ((seed seed) (acc '()))
        (if (stop? seed)
            acc
            (loop (successor seed) (cons (mapper seed) acc)))))

    (define (list-queue-unfold stop? mapper successor seed . rest)
      (let ((queue (if (null? rest) (new-queue) (car rest))))
        (state "list-queue-unfold" queue)
        (for-each (lambda (elem) (list-queue-add-front! queue elem))
                  (unfold-reverse stop? mapper successor seed))
        queue))

    (define (list-queue-unfold-right stop? mapper successor seed . rest)
      (let ((queue (if (null? rest) (new-queue) (car rest))))
        (state "list-queue-unfold-right" queue)
        (for-each (lambda (elem) (list-queue-add-back! queue elem))
                  (unfold-reverse stop? mapper successor seed))
        queue))

    ;; === Predicates ===

    (define (list-queue-empty? queue)
      (eqv? (st-size (state "list-queue-empty?" queue)) 0))

    ;; === Accessors ===

    (define (list-queue-front queue)
      (let ((st (state/nonempty "list-queue-front" queue)))
        (vector-ref (st-buffer st) (st-head st))))

    (define (list-queue-back queue)
      (let* ((st (state/nonempty "list-queue-back" queue))
             (buffer (st-buffer st)))
        (vector-ref buffer
                    (modulo (+ (st-head st) (- (st-size st) 1)) (vector-length buffer)))))

    ;; The result is a snapshot: it does not change if the queue is modified later
    (define (list-queue-list queue)
      (elements (state "list-queue-list" queue)))

    (define (list-queue-first-last queue)
      (let ((lst (elements (state "list-queue-first-last" queue))))
        (if (null? lst)
            (values '() '())
            (values lst
                    (let loop ((rest lst))
                      (if (null? (cdr rest)) rest (loop (cdr rest))))))))

    ;; === Mutators ===

    (define (list-queue-add-front! queue elem)
      (let ((st (state "list-queue-add-front!" queue)))
        (if (= (st-size st) (vector-length (st-buffer st)))
            (grow! st))
        (let* ((buffer (st-buffer st))
               (head (modulo (- (st-head st) 1) (vector-length buffer))))
          (vector-set! buffer head elem)
          (set-st-head! st head)
          (set-st-size! st (+ (st-size st) 1))
          (set-st-snapshot! st #f))))

    (define (list-queue-add-back! queue elem)
      (let ((st (state "list-queue-add-back!" queue)))
        (if (= (st-size st) (vector-length (st-buffer st)))
            (grow! st))
        (let ((buffer (st-buffer st)))
          (vector-set! buffer
                       (modulo (+ (st-head st) (st-size st)) (vector-length buffer))
                       elem)
          (set-st-size! st (+ (st-size st) 1))
          (set-st-snapshot! st #f))))

    (define (list-queue-remove-front! queue)
      (let* ((st (state/nonempty "list-queue-remove-front!" queue))
             (buffer (st-buffer st))
             (head (st-head st))
             (elem (vector-ref buffer head)))
        (vector-set! buffer head #f)
        (set-st-head! st (modulo (+ head 1) (vector-length buffer)))
        (set-st-size! st (- (st-size st) 1))
        (set-st-snapshot! st #f)
        elem))

    (define (list-queue-remove-back! queue)
      (let* ((st (state/nonempty "list-queue-remove-back!" queue))
             (buffer (st-buffer st))
             (index (modulo (+ (st-head st) (- (st-size st) 1)) (vector-length buffer)))
             (elem (vector-ref buffer index)))
        (vector-set! buffer index #f)
        (set-st-size! st (- (st-size st) 1))
        (set-st-snapshot! st #f)
        elem))

    (define (list-queue-remove-all! queue)
      (let* ((st (state "list-queue-remove-all!" queue))
             (result (elements st)))
        (fill! st '())
        result))

    ;; The optional argument `last` is accepted for compatibility with the reference
    ;; implementation. Since lists are copied, it is ignored.
    (define (list-queue-set-list! queue lst . rest)
      (let ((st (state "list-queue-set-list!" queue)))
        (check-list "list-queue-set-list!" lst)
        (fill! st lst)))

    ;; === The whole list queue ===

    (define (list-queue-append . queues)
      (list-queue-concatenate queues))

    (define (list-queue-concatenate queues)
      (check-list "list-queue-concatenate" queues)
      (make-list-queue
        (apply append
               (map (lambda (queue) (elements (state "list-queue-concatenate" queue)))
                    queues))))

    ;; Appends the elements of all other queues to the first queue and returns it. The other
    ;; queues are not modified.
    (define (list-queue-append! . queues)
      (if (null? queues)
          (list-queue)
          (let ((target (car queues))
                (others (map (lambda (queue) (elements (state "list-queue-append!" queue)))
                             (cdr queues))))
            (state "list-queue-append!" target)
            (for-each (lambda (elems)
                        (for-each (lambda (elem) (list-queue-add-back! target elem)) elems))
                      others)
            target)))

    ;; === Mapping ===

    ;; Applies `proc` to the elements in front-to-back order
    (define (map-in-order proc lst)
      (let loop ((rest lst) (acc '()))
        (if (null? rest)
            (reverse acc)
            (loop (cdr rest) (cons (proc (car rest)) acc)))))

    (define (list-queue-map proc queue)
      (make-list-queue (map-in-order proc (elements (state "list-queue-map" queue)))))

    (define (list-queue-map! proc queue)
      (let ((st (state "list-queue-map!" queue)))
        (fill! st (map-in-order proc (elements st)))))

    (define (list-queue-for-each proc queue)
      (for-each proc (elements (state "list-queue-for-each" queue))))
  )
)
