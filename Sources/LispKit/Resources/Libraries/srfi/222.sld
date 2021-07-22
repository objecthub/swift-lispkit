;;; SRFI 221
;;; Compound objects
;;;
;;; Compound objects are analogous to R6RS compound conditions, and are suitable
;;; for use in creating and handling conditions on non-R6RS systems, among other
;;; purposes. They encapsulate an immutable sequence of subobjects, which can be
;;; any object except another compound object. It is possible to implement R6RS
;;; compound conditions on top of compound objects, but not vice versa.
;;; Note that this SRFI does not provide any analogue to R6RS simple conditions,
;;; which are just records.
;;;
;;; Compound objects serve as a kind of poor man's multiple inheritance without
;;; the usual complications of multiple inheritance. A compound object can be
;;; used to represent multiple otherwise unrelated aspects of a value or
;;; situation. Because they are sequences, they can be used to represent priorities
;;; of interpretation from higher to lower. Most of the operations described in
;;; this section treat a non-compound object identically to a compound object with
;;; the simple object as its sole component.
;;;
;;; Copyright © 2021 John Cowan (text), Arvydas Silanskas (implementation). All rights reserved.
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
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(define-library (srfi 222)

  (export make-compound
          compound?
          compound-subobjects
          compound-length
          compound-ref
          compound-map
          compound-map->list
          compound-filter
          compound-predicate
          compound-access)

  (import (lispkit base))

  (begin

    (define-record-type <compound-object>
      (raw-compound-object subobjs)
      compound?
      (subobjs raw-object-subobjects))

    ;; private
    ;; flatten list of objects and potentially other compounds into simple list of
    ;; objects without compounds
    (define (assemble-subobjects in)
      (let loop ((in in)
                 (out '()))
        (if (null? in)
            (reverse out)
            (loop (cdr in)
                  (if (compound? (car in))
                      (append (reverse (compound-subobjects (car in))) out)
                      (cons (car in) out))))))

    (define (make-compound . subobjs)
      (raw-compound-object (assemble-subobjects subobjs)))

    (define (compound-subobjects obj)
      (if (compound? obj)
          (raw-object-subobjects obj)
          (list obj)))

    (define (compound-length obj)
      (if (compound? obj)
          (length (raw-object-subobjects obj))
          1))

    (define (compound-ref obj k)
      (if (compound? obj)
          (list-ref (compound-subobjects obj) k)
          obj))

    (define (compound-map mapper obj)
      (if (compound? obj)
          (apply make-compound (compound-map->list mapper obj))
          (make-compound (mapper obj))))

    (define (compound-map->list mapper obj)
      (map mapper (compound-subobjects obj)))

    (define (internal-filter pred list)
      (let loop ((list list) (result '()))
        (cond
          ((null? list)
            (reverse result))
          ((pred (car list))
            (loop (cdr list) (cons (car list) result)))
          (else
            (loop (cdr list) result)))))

    (define (compound-filter pred obj)
      (define subobjs (internal-filter pred (compound-subobjects obj)))
      (raw-compound-object subobjs))

    (define (compound-predicate pred obj)
      (and
        (or
          ;; compound itself satisfies pred
          (pred obj)
          ;; compound has subobj that satisfies pred
          (let loop ((subobjs (compound-subobjects obj)))
            (cond
              ((null? subobjs) #f)
              ((pred (car subobjs)) #t)
              (else (loop (cdr subobjs))))))
        ;; if matched pred, convert result to #t
        #t))

    (define (compound-access pred accessor default obj)
      (cond
        ((pred obj)
          (accessor obj))
        ((compound? obj)
          (let loop ((subobjs (compound-subobjects obj)))
            (cond
              ((null? subobjs) default)
              ((pred (car subobjs)) (accessor (car subobjs)))
              (else (loop (cdr subobjs))))))
        (else default)))
    )
  )
