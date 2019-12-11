;;; SRFI 167 ENGINE
;;; Ordered Key Value Store
;;;
;;; This library describes an interface for an ordered key-value store that is suitable
;;; for implementing a storage engine for the generic tuple-store SRFI. It maps cleanly
;;; to existing ordered key-value databases that may or may not provide transactions.
;;;
;;; Copyright © 2019 Amirouche Boubekki. All rights reserved.
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
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2019 Matthias Zenger. All rights reserved.

(define-library (srfi 167 engine)

  (export make-engine
          engine?
          engine-open
          engine-close
          engine-in-transaction
          engine-ref
          engine-set!
          engine-delete!
          engine-range-remove!
          engine-range
          engine-prefix-range
          engine-hook-on-transaction-begin
          engine-hook-on-transaction-commit
          engine-pack
          engine-unpack)

  (import (lispkit base))

  (begin

    (define-record-type <engine>
      (make-engine open
                   close
                   in-transaction
                   ref
                   set
                   delete
                   range-remove
                   range
                   prefix-range
                   hook-on-transaction-begin
                   hook-on-transaction-commit
                   pack
                   unpack)
      engine?
      (open %engine-open)
      (close %engine-close)
      (in-transaction %engine-in-transaction)
      (ref %engine-ref)
      (set %engine-set)
      (delete %engine-delete)
      (range-remove %engine-range-remove)
      (range %engine-range)
      (prefix-range %engine-prefix-range)
      (hook-on-transaction-begin %engine-hook-on-transaction-begin)
      (hook-on-transaction-commit %engine-hook-on-transaction-commit)
      (pack %engine-pack)
      (unpack %engine-unpack))

    (define (make-invoker accessor)
      (lambda (engine . args)
        (apply (accessor engine) args)))

    (define engine-open (make-invoker %engine-open))
    (define engine-close (make-invoker %engine-close))
    (define engine-in-transaction (make-invoker %engine-in-transaction))
    (define engine-ref (make-invoker %engine-ref))
    (define engine-set! (make-invoker %engine-set))
    (define engine-delete! (make-invoker %engine-delete))
    (define engine-range-remove! (make-invoker %engine-range-remove))
    (define engine-range (make-invoker %engine-range))
    (define engine-prefix-range (make-invoker %engine-prefix-range))
    (define engine-hook-on-transaction-begin (make-invoker %engine-hook-on-transaction-begin))
    (define engine-hook-on-transaction-commit (make-invoker %engine-hook-on-transaction-commit))
    (define engine-pack (make-invoker %engine-pack))
    (define engine-unpack (make-invoker %engine-unpack))
  )
)

