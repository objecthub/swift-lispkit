;;; LISPKIT OBJECT
;;;
;;; This library implements a simple, delegation-based object system for LispKit.
;;; It provides procedural and declarative interfaces for objects and classes. The
;;; class system is optional. It mostly provides means to define and manage new
;;; object types and construct objects using object constructors.
;;;
;;; Similar to other Scheme and Lisp-based object systems, methods of objects are
;;; defined in terms of object/class-specific specializations of generic procedures.
;;; A generic procedure consists of methods for the various objects/classes it
;;; supports. A generic procedure performs a dynamic dispatch on the first parameter
;;; (the `self` parameter) to determine the applicable method.
;;;
;;; GENERIC PROCEDURES
;;; 
;;; Generic procedures can be defined using the `define-generic` form. Here is an
;;; example which defines three generic methods, one with only a `self` parameter,
;;; and two with three parameters `self`, `x` and `y`. The last generic procedure
;;; definition includes a `default` method which is applicable to all objects for
;;; which there is no specific method. When a generic procedure without default
;;; is applied to an object that does not define its own method implementation,
;;; an error gets signaled.
;;;
;;;   (define-generic (point-coordinates self))
;;;   (define-generic (set-point-coordinates! self x y))
;;;   (define-generic (point-move! self x y)
;;;     (let ((coord (point-coordinate self)))
;;;       (set-point-coordinate! self (+ (car coord) x) (+ (cdr coord) y))))
;;;
;;; OBJECTS
;;;
;;; An object encapsulates a list of methods each implementing a generic procedure.
;;; These methods are regular closures which can share mutable state. Objects do
;;; not have an explicit notion of a field or slot as in other Scheme or Lisp-based
;;; object systems. Fields/slots need to be implemented via generic procedures and
;;; method implementations sharing state. Here is an example explaining this
;;; approach:
;;;
;;;   (define (make-point x y)
;;;     (object ()
;;;       ((point-coordinates self) (cons x y))
;;;       ((set-point-coordinates! self nx ny) (set! x nx) (set! y ny))
;;;       ((object->string self) (string-append (object->string x) "/"
;;;                                             (object->string y)))))
;;;
;;; This is a function creating new point objects. The `x` and `y` parameters of
;;; the constructor function are used for representing the state of the point
;;; object. The created point objects implement three generic procedures:
;;; `point-coordinates`, `set-point-coordinates`, and `object->string`. The
;;; latter procedure is defined directly by the library and, in general, used
;;; for creating a string representation of any object. By implementing the
;;; `object->string` method, the behavior gets customized for the object.
;;; 
;;; The following lines of code illustrate how point objects can be used:
;;;
;;;   (define pt (make-point 25 37))
;;;   pt                                              => #object:#<box (...)>
;;;   (object->string pt)                             => "25/37"
;;;   (point-coordinates pt)                          => (25 . 37)
;;;   (set-point-coordinates! pt 5 6)
;;;   (object->string pt)                             => "5/6"
;;;   (point-coordinates pt)                          => (5 . 6)
;;;
;;; INHERITANCE
;;;
;;; The LispKit object system supports inheritance via delegation. The following
;;; code shows how colored points can be implemented by delegating all point
;;; functionality to the previous implementation and by simply adding only
;;; color-related logic.
;;;
;;;   (define-generic (point-color self) #f)
;;;   (define (make-colored-point x y color)
;;;     (object ((super (make-point x y)))
;;;       ((point-color self) color)
;;;       ((object->string self)
;;;         (string-append (object->string color) ":"
;;;                        (invoke (super object->string) self)))))
;;;
;;; The object created in function `make-colored-point` inherits all methods from
;;; object `super` which gets set to a new point object. It adds a new method to
;;; generic procedure `point-color` and redefines the `object->string` method. The
;;; redefinition is implemented in terms of the inherited `object->string` method
;;; for points. The form `invoke` can be used to refer to overridden methods in
;;; delegatee objects. Thus, `(invoke (super object->string) self)` calls the
;;; `object->string` method of the `super` object but with the identity (`self`)
;;; of the colored point.
;;;
;;; The following interaction illustrates the behavior:
;;;
;;;   (define cpt (make-colored-point 100 50 'red))
;;;   (point-color cpt)                               => red
;;;   (point-coordinates cpt)                         => (100 . 50)
;;;   (set-point-coordinates! cpt 101 51)
;;;   (object->string cpt)                            => "red:101/51"
;;;
;;; Objects can delegate functionality to multiple delegatees. The order in which
;;; they are listed determines the methods which are being inherited in case there
;;; are conflicts, i.e. multiple delegatees implement a method for the same
;;; generic procedure.
;;;
;;; CLASSES
;;;
;;; Classes add syntactic sugar, simplying the creation and management of objects.
;;; They play the following role in the object-system of LispKit:
;;;
;;;   1) A class defines a constructor for objects represented by this class.
;;;   2) Each class defines an object type, which can be used to distinguish objects
;;;      created by the same constructor and supporting the same methods.
;;;   3) A class also defines super-types for which the object type of a class is
;;;      considered a subtype.
;;;   4) A class can inherit methods implementing generic procedures from several
;;;      other classes, making it easy to reuse functionality. Inheritance is
;;;      orthogonal to subtyping.
;;;   5) A class defines methods implementing a number of generic procedures (which
;;;      potentially override inherited methods).
;;;   6) Classes are first-class objects supporting a number of class-related
;;;      procedures.
;;;
;;; The following code defines a `point` class with similar functionality as above:
;;;
;;;   (define-class (point x y) ()
;;;     (object ()
;;;       ((point-coordinates self) (cons x y))
;;;       ((set-point-coordinates! self nx ny) (set! x nx) (set! y ny))
;;;       ((object->string self) (string-append (object->string x) "/" (object->string y)))))
;;;
;;; Instances of this class are created by using the generic procedure `make-instance`
;;; which is implemented by all class objects:
;;;
;;;   (define pt2 (make-instance point 82 10))
;;;   pt2                                             => #point:#<box (...)>
;;;   (object->string pt2)                            => "82/10"
;;;
;;; Each object created by a class implements a generic procedure `object-class`
;;; referring to the class of the object. Since classes are objects themselves we
;;; can obtain their name with generic procedure `class-name`:
;;;
;;;   (object-class pt2)                              => #class:#<box (...)>
;;;   (class-name (object-class pt2))                 => point
;;;   (instance-of? point pt2)                        => #t
;;;   (instance-of? point pt)                         => #f
;;;
;;; Generic procedure `instance-of?` can be used to determine whether an object
;;; is a direct or indirect instance of a given class. The last two lines above
;;; show that `pt2` is an instance of `point`, but `pt` is not, even though it
;;; is functionally equivalent.
;;;
;;; The following definition re-implements the colored point example from above
;;; using a class:
;;;
;;;   (define-class (colored-point x y color) (point)
;;;     (if (or (< x 0) (< y 0))
;;;         (error "coordinates are negative: ($0; $1)" x y))
;;;     (object ((super (make-instance point x y)))
;;;       ((point-color self) color)
;;;       ((object->string self)
;;;         (string-append (object->string color) ":"
;;;                        (invoke (super object->string) self)))))
;;;
;;; The following lines illustrate the behavior of `colored-point` objects vs
;;; `point` objects:
;;;
;;;   (define cpt2 (make-instance colored-point 128 256 'blue))
;;;   (point-color cpt2)                              => blue
;;;   (point-coordinates cpt2)                        => (128 . 256)
;;;   (set-point-coordinates! cpt2 64 32)
;;;   (object->string cpt2)                           => "blue:64/32"
;;;   (instance-of? point cpt2)                       => #t
;;;   (instance-of? colored-point cpt2)               => #t
;;;   (instance-of? colored-point cpt)                => #f
;;;   (class-name (object-class cpt2))                => colored-point
;;;
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2017-2022 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not
;;; use this file except in compliance with the License. You may obtain a copy of
;;; the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed under the
;;; License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;;; either express or implied. See the License for the specific language governing permissions
;;; and limitations under the License.

(define-library (lispkit object)

  ;; Procedural object interface
  (export make-object
          object?
          object-type-tag
          method
          object-methods
          add-method!
          delete-method!
          make-generic-procedure)

  ;; Declarative object interface
  (export object
          define-generic
          invoke)

  ;; Procedural class interface
  (export make-class
          class?
          class-type-tag
          root
          object-class
          object-equal?
          object->string
          class-name
          class-direct-superclasses
          subclass?
          make-instance
          instance-of?)

  ;; Declarative class interface
  (export define-class)

  (import (except (lispkit base) object))

  (begin

    ;;; Procedural object functionality

    (define-values (object-type-tag new-object object? object-ref make-object-subtype)
      (make-type 'object))

    (define (make-object . delegates)
      (make-custom-object new-object delegates))

    (define (make-custom-object constructor delegates)
      (constructor (box (concatenate (map object-methods delegates)))))

    (define (object-methods obj)
      (unbox (object-ref obj)))

    (define (method obj generic)
      (let ((impl (assq generic (object-methods obj))))
        (and impl (cdr impl))))

    (define (add-method! obj generic method)
      (let ((object-box (object-ref obj)))
        (set-box! object-box (cons (cons generic method) (unbox object-box)))))

    (define (delete-method! obj generic)
      (let ((object-box (object-ref obj)))
        (set-box! object-box
          (let delete ((methods (unbox object-box)))
            (if (null? methods)
                methods
                (if (eq? (caar methods) generic)
                    (cdr methods)
                    (cons (car methods) (delete (cdr methods)))))))))

    (define (make-generic-procedure . impl)
      (letrec* ((proc (if (pair? impl)
                          (if (procedure? (car impl))
                              (car impl)
                              (error "generic implementation not a procedure: $0" (car impl)))
                          (lambda (obj . params) (error "method not supported for object $0" obj))))
                (generic (lambda (obj . args)
                           (apply (or (and (object? obj) (method obj generic)) proc)
                                  obj
                                  args))))
        generic))

    ;;; Declarative objects interface

    (define-syntax object
      (syntax-rules ()
        ((_ ((s delegate) ...) ((method self arg ... . rest) e1 e2 ...) ...)
           (let* ((s delegate)
                  ...
                  (obj (make-object s ...)))
             (add-method! obj method (lambda (self arg ... . rest) e1 e2 ...))
             ...
             obj))))

    (define-syntax define-generic
      (syntax-rules ()
        ((_ (name self arg ... . rest))
           (define name (make-generic-procedure)))
        ((_ (name self arg ... . rest) e1 e2 ...)
           (define name (make-generic-procedure (lambda (self arg ... . rest) e1 e2 ...))))))

    (define-syntax invoke
      (syntax-rules ()
        ((_ (obj generic) self arg ...)
          ((method obj generic) self arg ...))))

    ;;; Procedural class interface

    (define-values (class-type-tag new-class class? class-ref make-class-subtype)
      (make-object-subtype 'class))

    ;; Instance methods
    (define-generic (object-class self) #f)
    (define-generic (object-equal? self obj) (equal? self obj))
    (define-generic (object->string self)
      (let ((out (open-output-string)))
        (write self out)
        (get-output-string out)))

    ;; Class methods
    (define-generic (class-name self) #f)
    (define-generic (class-direct-superclasses self) '())
    (define-generic (subclass? self other) (eq? self other))
    (define-generic (make-instance self . args))
    (define-generic (instance-of? self obj))

    ;; Root class
    (define root
      (let ((root (make-custom-object new-class '())))
        (add-method! root class-name
          (lambda (self) 'root))
        (add-method! root make-instance
          (lambda (self)
            (let ((obj (make-object)))
              (add-method! obj object-class (lambda (this) self))
              obj)))
        (add-method! root instance-of?
          (lambda (self obj) (subclass? (object-class obj) self)))
        root))

    ;; New sub-classes
    (define (make-class name superclasses constructor)
      (if (not (symbol? name))
          (error "class name required to be a symbol: $0" name))
      (for-each
        (lambda (super) (if (not (class? super)) (error "illegal superclass of $0: $1" name super)))
        superclasses)
      (if (not (procedure? constructor))
          (error "class constructor required to be a procedure" name constructor))
      (let ((class (make-custom-object new-class '())))
        (let-values (((new-instance instance? instance-ref make-instance-subtype)
                        (make-object-subtype name)))
          (add-method! class class-name
            (lambda (self) name))
          (add-method! class class-direct-superclasses
            (lambda (self) superclasses))
          (add-method! class make-instance
            (lambda (self . args)
              (let-values (((delegates initializer) (apply constructor args)))
                (check-delegates superclasses delegates)
                (let ((obj (make-custom-object new-instance delegates)))
                  (add-method! obj object-class (lambda (this) self))
                  (initializer obj)
                  obj))))
          (add-method! class subclass?
            (lambda (self other)
              (or (eq? self other)
                  (do ((s superclasses (cdr s)))
                      ((or (null? s) (subclass? (car s) other)) (not (null? s)))))))
          (add-method! class instance-of?
            (lambda (self obj) (subclass? (object-class obj) self)))
          class)))


     ;;; Declarative class interface

     (define-syntax define-class
       (syntax-rules (object)
         ((_ (name . args) (super ...)
               init ...
               (object ((s delegate) ...)
                 ((method self arg ... . rest) e1 e2 ...) ...))
            (define name
               (make-class (quote name) (list super ...)
                 (lambda args
                   init ...
                   (let* ((s delegate) ...)
                     (values (list s ...)
                             (lambda (obj)
                               (add-method! obj method (lambda (self arg ... . rest) e1 e2 ...))
                               ... )))))))
         ((_ (name . args) pred? (super ...)
               init ...
               (object ((s delegate) ...)
                 ((method self arg ... . rest) e1 e2 ...) ...))
            (begin
              (define name
                 (make-class (quote name) (list super ...)
                   (lambda args
                     init ...
                     (let* ((s delegate) ...)
                       (values (list s ...)
                               (lambda (obj)
                                 (add-method! obj method (lambda (self arg ... . rest) e1 e2 ...))
                                 ... ))))))
              (define (pred? x) (subclass? (object-class obj) name))))))

    ;;; Utilities

    (define (check-delegates classes delegates)
      (let ((typematches (map instance-of? classes delegates)))
        (if (< (length typematches) (length classes))
            (error "missing object delegates" (map class-name classes) delegates)
            (if (pair? (filter not typematches))
                (error "some delegates not matching class" classes delegates)))))
  )
)
