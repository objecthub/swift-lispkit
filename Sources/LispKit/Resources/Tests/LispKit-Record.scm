;;; LISPKIT RECORD REGRESSION TEST SUITE
;;;
;;; This is the test suite for library `(lispkit record)`. It is based on the
;;; test suite of SRFI 136.
;;;
;;; Copyright © 2016 Marc Nieper-Wißkirchen. All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2023 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test))

(test-begin "LispKit Record")

(test-assert "Predicate"
  (let ()
    (define-record-type <rec>
      (make-rec)
      rec?)
    (rec? (make-rec))))

(test-assert "Disjoint type"
  (let ()
    (define-record-type <record>
      (make-record)
      record?)
    (not (vector? (make-record)))))

(test-equal "Record fields"
  '(1 2 3)
  (let ()
    (define-record-type <record>
      (make-record foo baz)
      record?
      (foo foo)
      (bar bar set-bar!)
      (baz baz))
    (define record (make-record 1 3))
    (set-bar! record 2)
    (list (foo record) (bar record) (baz record))))

(test-equal "Subtypes"
  '(#t #f)
  (let ()
    (define-record-type <parent>
      (make-parent)
      parent?)
    (define-record-type (<child> <parent>)
      (make-child)
      child?)
    (list (parent? (make-child)) (child? (make-parent)))))

(test-equal "Inheritance of constructor"
  '(1 2)
  (let ()
    (define-record-type <parent>
      (make-parent foo)
      parent?
      (foo foo))
    (define-record-type (<child> <parent>)
      (make-child foo bar)
      child?
      (bar bar))
    (define child (make-child 1 2))
    (list (foo child) (bar child))))

  (define-record-type <parent>
    (make-parent foo)
    parent?
    (foo foo))

(test-equal "Default constructors"
  1
  (let ()
  (define-record-type <parent>
    (make-parent foo)
    #f
    (foo foo))
  (define-record-type (<child-type> <parent>)
    make-child
    child?)
  (define child (make-child 1))
  (foo child)))

(test-equal "Named fields"
  1
  (let ()
    (define-record-type <record>
      (make-record)
      record?
      (foo foo set-foo!))
    (define record (make-record))
    (set-foo! record 1)
    (foo record)))

(test-assert "Missing parent"
  (let ()
    (define-record-type (<record> #f)
      (make-record)
      record?)
    (record? (make-record))))

(test-equal "Accessor names in constructor"
  '(1 2 3)
  (let ()
    (define-record-type <record>
      (make-record bar foo quux)
      record?
      (bar foo)
      (foo bar)
      (quux baz))
    (define record (make-record 1 2 3))
    (list (foo record) (bar record) (baz record))))

(test-assert "Runtime record-type descriptors"
  (let ()
    (define-record-type <record> (make-foo) foo?)
  (record-type? <record>)))

(test-equal "Predicate for records"
  '(#t #f)
  (let ()
    (define-record-type <record>
      (make-foo)
      foo?)
    (list (record? (make-foo)) (record? (vector)))))

(test-assert "Runtime record-type descriptor from instance"
  (let ()
    (define-record-type <record> (make-record) record?)
    (eq? (record-type (make-record)) <record>)))

(test-assert "Record-type predicate"
  (let ()
    (define-record-type <record>
      (make-record)
      record?)
    ((record-predicate <record>) (make-record))))

(test-equal "Introspection of record-type name"
  "<record>"
  (let ()
    (define-record-type <record>
      (make-record)
      record?)
    (record-type-name <record>)))

(test-assert "Introspection of record-type parent"
  (let ()
    (define-record-type <parent>
      #f
      parent?)
    (define-record-type (<child> <parent>)
      #f
      child?)
    (eq? (record-type-parent <child>) <parent>)))

(test-equal "Introspection of record-type fields"
  '(#t 1 2)
  (let ()
    (define-record-type <record>
      (make-record foo)
      record?
      (foo foo)
      (bar bar set-bar!))
    (define fields (record-type-field-names <record>))
    (define record (make-record 1))
    (define get-foo (record-field-accessor <record> (car fields)))
    (define get-bar (record-field-accessor <record> (cadr fields)))
    (define set-bar (record-field-mutator <record> 'bar))
    (set-bar record 2)
    (list (record? record)
          (get-foo record)
          (get-bar record))))

(test-equal "Constructor name in subrecord-type"
  '(#t 3 4)
  (let ()
    (define-record-type foo (make-foo a b) foo?
      (a foo-a)
      (b foo-b))
    (define-record-type (bar foo) make-bar bar?
      (c bar-a)
      (d bar-b))
    (define record (make-bar 1 2 3 4))
      (list (bar? record) (bar-a record) (bar-b record))))

(test-assert "Middle record type has no constructor and two more fields"
  (let ()
    (define-record-type <base> (make-base) base?)
    (define-record-type (<middle> <base>)
      #f
      middle?
      (a middle-a middle-a-set!)
      (b middle-b middle-b-set!))
    (define-record-type (<leaf> <middle>)
      (make-leaf d)
      leaf?
      (d leaf-d))
    (define-record-type (<leaf2> <middle>)
      make-leaf2
      leaf2?
      (d leaf2-d))
    (and (middle? (make-leaf2 1 2 3))
         (middle? (make-leaf 4))
         (base? (make-leaf2 1 2 3))
         (base? (make-leaf 4))
         (leaf2? (make-leaf2 1 2 3))
         (not (leaf? (make-leaf2 1 2 3))))))

(test-end "LispKit Record")
