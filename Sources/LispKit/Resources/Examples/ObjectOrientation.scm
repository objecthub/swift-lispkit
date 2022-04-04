;;; Object-Oriented Programming CLOS-style
;;; 
;;; LispKit supports an object-oriented programming approach that is
;;; based on the API and the programming conventions of the "Common
;;; Lisp Object System" (CLOS). Especially for use cases with complex
;;; domain models, using CLOS has many advantages: it allows for modular
;;; implementations, programs are resemble more closely what they model,
;;; and programs are highly extensible.
;;;
;;; A great introduction into CLOS is provided by Sonya E. Keene's
;;; book on "Object-Oriented Programming in Common Lisp" (Addison-Wesley
;;; Publishing Company).
;;; 
;;; Library (lispkit clos) is based on Tiny CLOS, a version of the CLOS
;;; kernel written in Scheme by Gregor Kiczales. It differs from CLOS
;;; in terms of the API, but the object-oriented abstractions are
;;; equivalent to CLOS.
;;;
;;; For now, this file contains examples from the original Tiny CLOS
;;; distribution.
;;;
;;; Copyright © 1992 Xerox Corporation. All rights reserved.
;;; 
;;; Use, reproduction, and preparation of derivative works are permitted.
;;; Any copy of this software or of any derivative work must include the
;;; above copyright notice of Xerox Corporation, this paragraph and the
;;; one after it.  Any distribution of this software or derivative works
;;; must comply with all applicable United States export control laws.
;;; 
;;; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
;;; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
;;; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
;;; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
;;; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
;;; OF THE POSSIBILITY OF SUCH DAMAGES.
;;; 
;;; Adaptation to LispKit
;;;   Copyright © 2022 Matthias Zenger. All rights reserved.

(import (lispkit clos)
        (lispkit clos support))

; Useful utility

(define (initialize-slots object initargs)
  (let ((not-there (list 'shes-not-there)))
    (for-each (lambda (slot)
                (let ((name (car slot)))
                  (let ((value  (getl initargs name not-there)))
                    (if (eq? value not-there)
                        'do-nothing
                        (slot-set! object name value)))))
              (class-slots (class-of object)))))

; A simple class, just an instance of <class>.  Note that we are using
; make and <class> rather than make-class to make it.  See Section 2.4
; of AMOP for more on this.

(define <pos> (make <class>
                    'name '<pos>
                    'direct-superclasses (list <object>)
                    'direct-slots  (list 'x 'y)))

(add-method initialize
  (make-method
    (list <pos>)
    (lambda (call-next-method pos initargs)
      (call-next-method)
      (initialize-slots pos initargs))))

(define p1 (make <pos> 'x 1 'y 2))
(define p2 (make <pos> 'x 3 'y 5))

; Another way of writing that class definition, that achives better
; `encapsulation' by using slot names that are unique keys, rather
; than symbols.

(define <pos> '())
(define pos-x (make-generic))
(define pos-y (make-generic))
(define move  (make-generic))

(let ((x (vector 'x))
      (y (vector 'y)))
  (set! <pos> (make <class>
                    'name '<pos2>
                    'direct-superclasses (list <object>)
                    'direct-slots  (list x y)))
  (add-method pos-x
    (make-method
      (list <pos>)
      (lambda (call-next-method pos) (slot-ref pos x))))
  (add-method pos-y
    (make-method
      (list <pos>)
      (lambda (call-next-method pos) (slot-ref pos y))))
  (add-method move
    (make-method
      (list <pos>)
      (lambda (call-next-method pos new-x new-y)
        (slot-set! pos x new-x)
        (slot-set! pos y new-y))))
  (add-method initialize
    (make-method
      (list <pos>)
      (lambda (call-next-method pos initargs)
        (move pos (getl initargs 'x 0) (getl initargs 'y 0))))))

(define p3 (make <pos> 'x 1 'y 2))
(define p4 (make <pos> 'x 3 'y 5))

; Class allocated slots.
;
; In Scheme, this extension isn't worth a whole lot, but what the hell.

(define <class-slots-class>
  (make-class '<class-slots-class> (list <class>) (list)))

(add-method compute-accessors
  (make-method
    (list <class-slots-class>)
    (lambda (call-next-method class slot allocator)
      (if (null? (memq ':class-allocation slot))
          (call-next-method)
          (let ((cell #f))
            (list (lambda (o) cell)
                  (lambda (o new) (set! cell new) new)))))))

; Here's a silly program that uses class allocated slots.

(define <ship>
    (make <class-slots-class>
          'name '<ship>
          'direct-superclasses (list <object>)
          'direct-slots  (list 'name
                   '(all-ships :class-allocation))))

(add-method initialize
    (make-method (list <ship>)
      (lambda (call-next-method ship initargs)
  (call-next-method)
  (initialize-slots ship initargs)
  (slot-set! ship
       'all-ships
       (cons ship (slot-ref ship 'all-ships))))))

(define siblings (make-generic 'siblings))
(add-method siblings
    (make-method (list <ship>)
      (lambda (call-next-method ship) (remove ship (slot-ref ship 'all-ships)))))

(define s1 (make <ship> 'name 's1))
(define s2 (make <ship> 'name 's2))
(define s3 (make <ship> 'name 's3))

; Here's a class of class that allocates some slots dynamically.
;
; It has a layered protocol (dynamic-slot?) that decides whether a given
; slot should be dynamically allocated.  This makes it easy to define a
; subclass that allocates all its slots dynamically.

(define <dynamic-class>
  (make-class '<dynamic-class> (list <class>) (list 'alist-g-n-s)))

(define dynamic-slot? (make-generic))

(add-method dynamic-slot?
  (make-method
    (list <dynamic-class>)
      (lambda (call-next-method class slot)
        (memq :dynamic-allocation (cdr slot)))))

(define alist-getter-and-setter
  (lambda (dynamic-class allocator)
    (let ((old (slot-ref dynamic-class 'alist-g-n-s)))
      (if (pair? old)
          old
          (let ((new (allocator (lambda () '()))))
            (slot-set! dynamic-class 'alist-g-n-s new)
            new)))))

(add-method compute-accessors
  (make-method
    (list <dynamic-class>)
    (lambda (call-next-method class slot allocator)
      (if (null? (dynamic-slot? class slot))
          (call-next-method)
          (let* ((name (car slot))
                 (g-n-s (alist-getter-and-setter class allocator))
                 (alist-getter (car g-n-s))
                 (alist-setter (cadr g-n-s)))
            (list (lambda (o)
                    (let ((entry (assq name (alist-getter o))))
                      (if (pair? entry)
                          (cdr entry)
                          '())))
                  (lambda (o new)
                    (let* ((alist (alist-getter o))
                           (entry (assq name alist)))
                      (if (pair? entry)
                          (set! entry (cons (car entry) new))
                          (alist-setter o (cons (cons name new) alist)))
                      new))))))))

(define <all-dynamic-class>
  (make-class '<all-dynamic-class> (list <dynamic-class>) (list)))

(add-method dynamic-slot?
  (make-method
    (list <all-dynamic-class>)
    (lambda (call-next-method class slot) #t)))

; A silly program that uses this.

(define <person>
  (make <all-dynamic-class>
        'name '<person>
        'direct-superclasses (list <object>)
        'direct-slots  (list 'name 'age 'address)))

(add-method initialize
  (make-method
    (list <person>)
      (lambda (call-next-method person initargs)
        (initialize-slots person initargs))))

(define person1 (make <person> 'name 'sally))
(define person2 (make <person> 'name 'betty))
(define person3 (make <person> 'name 'sue))

; A ``database'' class that stores slots externally.

(define <db-class>
  (make-class '<db-class> (list <class>) (list 'id-g-n-s)))

(define id-getter-and-setter
  (lambda (db-class allocator)
    (let ((old (slot-ref db-class 'id-g-n-s)))
      (if (null? old)
          (let ((new (allocator db-allocate-id)))
            (slot-set! class 'id-g-n-s new)
            new)
          old))))

(add-method compute-accessors
    (make-method (list <db-class>)
      (lambda (call-next-method class slot allocator)
  (let* ((id-g-n-s (id-getter-and-setter class allocator))
         (id-getter (car id-g-n-s))
         (id-setter (cadr id-g-n-s))
         (slot-name (car slot)))
    (list (lambda (o)
      (db-lookup (id-getter o) slot-name)) 
    (lambda (o new)
      (db-store  (id-getter o) slot-name new)))))))

; A kind of generic that supports around methods

(define <around-generic>
  (make <entity-class>
        'name '<around-generic>
        'direct-superclasses (list <generic>)))

(define (make-around-generic)
  (make <around-generic>))

(define <around-method>
  (make <class>
        'name '<around-method>
        'direct-superclasses (list <method>)))

(define (make-around-method specializers procedure)
  (make <around-method>
        'specializers specializers
        'procedure procedure))

(define around-method? (make-generic))

(add-method around-method?
  (make-method
    (list <method>)
    (lambda (call-next-method x) #f)))

(add-method around-method?
  (make-method
    (list <around-method>)
    (lambda (call-next-method x) #t)))

(add-method compute-methods
  (make-method
    (list <around-generic>)
    (lambda (call-next-method generic)
      (let ((normal-compute-methods (call-next-method)))
        (lambda (args)
          (let ((normal-methods (normal-compute-methods args)))
            (append (filter around-method? normal-methods)
                    (filter (lambda (m) (not (around-method? m)))
                            normal-methods))))))))

; And a simple example of using it

(define <baz> (make-class '<baz> (list <object>) (list)))
(define <bar> (make-class '<bar> (list <baz>)    (list)))
(define <foo> (make-class '<foo> (list <bar>)    (list)))

(define (test-around generic)
  (add-method generic
    (make-method
      (list <foo>)
      (lambda (cnm x) (cons 'foo (cnm)))))
  (add-method generic
    (make-around-method (list <bar>)
    (lambda (cnm x) (cons 'bar (cnm)))))
  (add-method generic
    (make-method
      (list <baz>)
      (lambda (cnm x) '(baz))))
  (generic (make <foo>)))

(equal? (test-around (make-generic))        '(foo bar baz))
(equal? (test-around (make-around-generic)) '(bar foo baz))
