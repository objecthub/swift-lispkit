;;; Tiny CLOS walk-through
;;;
;;; Library `(lispkit clos)` implements "Tiny CLOS", a compact version of the
;;; Common Lisp Object System (CLOS) written by Gregor Kiczales. It provides
;;; classes with multiple inheritance, generic functions with multi-methods,
;;; and a metaobject protocol (MOP) for customizing the object system itself.
;;;
;;; This program is a guided tour through the most important features of the
;;; library. It uses a small role-playing game world as running example:
;;; there are things (creatures and items), creatures fight each other, and
;;; items can be used on other things. The tour consists of the following parts:
;;;
;;;   1. Classes and instances            6. Built-in classes
;;;   2. Initialization                   7. Introspection
;;;   3. Generic functions and methods    8. The metaobject protocol
;;;   4. Multiple inheritance             9. Syntactic sugar
;;;   5. Multiple dispatch
;;;
;;; Each part prints some results so that it is possible to follow what happens.
;;; Read the code and the output side by side. The library is documented in
;;; `Documentation/(lispkit clos).md`, which also contains a tutorial.
;;;
;;; Tiny CLOS is copyright © 1992 Xerox Corporation.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may
;;; not use this file except in compliance with the License. You may obtain
;;; a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(import (lispkit base)
        (lispkit clos)
        (only (lispkit clos support) getl))

;; Utilities for printing the results of this tour

(define (section title)
  (newline)
  (display "=== ")
  (display title)
  (display " ===")
  (newline))

(define (show label value)
  (display "  ")
  (display label)
  (display ": ")
  (write value)
  (newline))

;; Calls thunk and returns the message of an error if there is one

(define (error-message thunk)
  (guard (condition (#t (error-object-message condition)))
    (thunk)
    "no error"))


;;; ----------------------------------------------------------------------------
;;; 1. Classes and instances
;;; ----------------------------------------------------------------------------
;;;
;;; A class is created with `make-class`, which takes the name of the class, a
;;; list of direct superclasses, and a list of slot names. Slots are the fields
;;; of the instances of a class. Classes are first-class values and bound to
;;; variables like any other value. By convention, their names are written in
;;; angle brackets. Every class inherits from `<object>` (directly or indirectly)
;;; unless it is explicitly created without superclasses.

(section "1. Classes and instances")

(define <thing>    (make-class '<thing> '(name)))
(define <creature> (make-class '<creature> (list <thing>) '(hp)))
(define <player>   (make-class '<player> (list <creature>) '(inventory)))
(define <monster>  (make-class '<monster> (list <creature>) '(strength)))
(define <item>     (make-class '<item> (list <thing>) '()))
(define <sword>    (make-class '<sword> (list <item>) '(damage)))
(define <potion>   (make-class '<potion> (list <item>) '(healing)))

;; Instances are created with `make`, and slots are accessed with `slot-ref` and
;; `slot-set!`. Without further ado, all slots of a new instance are `#f`.

(define stranger (make <player>))

(show "initial name of a player" (slot-ref stranger 'name))
(slot-set! stranger 'name "Stranger")
(show "name after slot-set!" (slot-ref stranger 'name))

;; Instances are printed in an unhelpful way (they are implemented as procedures).
;; But we can ask for their class and the name of the class:

(show "class name" (class-name (class-of stranger)))
(show "instance of <creature>?" (instance-of? stranger <creature>))
(show "instance of <monster>?" (instance-of? stranger <monster>))
(show "slots of a player" (map car (class-slots <player>)))

;; Accessing slots that don't exist is an error:

(show "unknown slot" (error-message (lambda () (slot-ref stranger 'strength))))


;;; ----------------------------------------------------------------------------
;;; 2. Initialization
;;; ----------------------------------------------------------------------------
;;;
;;; The arguments following the class in a call of `make` are called initargs; they
;;; are alternating names and values. They are passed to the generic function
;;; `initialize` after the instance has been allocated. By default, the initargs
;;; are ignored. To make use of them, we add our own `initialize` methods.
;;;
;;; A method is created with `make-method` from a list of specializers (classes)
;;; and a procedure. The procedure always has an additional first parameter: it is
;;; bound to a procedure without parameters that calls the next method. Initialize
;;; methods should call it first, so that the superclasses can do their part.
;;;
;;; The first method is specialized for `<thing>`. Since it is inherited by all
;;; classes of our world, we only have to write it once. It copies the values of the
;;; initargs into the slots with the same names. All methods of `initialize` that
;;; follow this one set defaults for slots that have not been initialized.

(section "2. Initialization")

(define missing (list 'missing))

(add-method initialize
  (make-method (list <thing>)
    (lambda (call-next-method thing initargs)
      (call-next-method)
      (for-each (lambda (slot)
                  (let ((value (getl initargs (car slot) missing)))
                    (if (not (eq? value missing))
                        (slot-set! thing (car slot) value))))
                (class-slots (class-of thing))))))

(define (default! object slot value)
  (if (not (slot-ref object slot))
      (slot-set! object slot value)))

(add-method initialize
  (make-method (list <creature>)
    (lambda (call-next-method creature initargs)
      (call-next-method)
      (default! creature 'hp 10))))

(add-method initialize
  (make-method (list <player>)
    (lambda (call-next-method player initargs)
      (call-next-method)
      (default! player 'inventory '()))))

(add-method initialize
  (make-method (list <monster>)
    (lambda (call-next-method monster initargs)
      (call-next-method)
      (default! monster 'strength 2))))

(define hero   (make <player> 'name "Aria" 'hp 20))
(define goblin (make <monster> 'name "Goblin"))
(define sword  (make <sword> 'name "Rusty Sword" 'damage 4))
(define potion (make <potion> 'name "Red Potion" 'healing 7))

(show "hero" (list (slot-ref hero 'name) (slot-ref hero 'hp) (slot-ref hero 'inventory)))
(show "goblin" (list (slot-ref goblin 'name) (slot-ref goblin 'hp) (slot-ref goblin 'strength)))
(show "sword" (list (slot-ref sword 'name) (slot-ref sword 'damage)))


;;; ----------------------------------------------------------------------------
;;; 3. Generic functions and methods
;;; ----------------------------------------------------------------------------
;;;
;;; A generic function is an ordinary function whose behavior is defined by a
;;; set of methods. It is created with `make-generic`, and methods are attached
;;; to it with `add-method`. When a generic function is called, the most specific
;;; applicable method is selected based on the classes of the arguments. Within
;;; a method, `call-next-method` invokes the next most specific method with the
;;; same arguments. This makes it possible to extend the behavior of superclasses.
;;;
;;; The following generic function `describe` returns a description of a thing.
;;; Every method adds a bit of information to the result of the next method.

(section "3. Generic functions and methods")

(define describe (make-generic 'describe))

(add-method describe
  (make-method (list <thing>)
    (lambda (call-next-method thing)
      (slot-ref thing 'name))))

(add-method describe
  (make-method (list <creature>)
    (lambda (call-next-method creature)
      (string-append (call-next-method)
                     " [hp " (number->string (slot-ref creature 'hp)) "]"))))

(add-method describe
  (make-method (list <monster>)
    (lambda (call-next-method monster)
      (string-append (call-next-method)
                     " [strength " (number->string (slot-ref monster 'strength)) "]"))))

(add-method describe
  (make-method (list <sword>)
    (lambda (call-next-method sword)
      (string-append (call-next-method)
                     " [damage " (number->string (slot-ref sword 'damage)) "]"))))

(show "describe hero" (describe hero))
(show "describe goblin" (describe goblin))
(show "describe sword" (describe sword))
(show "describe potion" (describe potion))

;; Generic functions are normal procedures: they can be passed around.

(show "map describe" (map describe (list hero goblin potion)))

;; If no method is applicable, an error is signaled:

(show "describe 42" (error-message (lambda () (describe 42))))


;;; ----------------------------------------------------------------------------
;;; 4. Multiple inheritance
;;; ----------------------------------------------------------------------------
;;;
;;; A class can have several direct superclasses. This is often used to define
;;; small reusable classes, so-called mixins, which contribute state and behavior.
;;; The order of the superclasses is important: the class precedence list of a
;;; class determines in which order inherited methods are called. By convention,
;;; mixins are listed before the regular superclasses.

(section "4. Multiple inheritance")

(define <flying> (make-class '<flying> '(altitude)))
(define <fire-breathing> (make-class '<fire-breathing> '(fire-damage)))

(define <dragon>
  (make-class '<dragon> (list <flying> <fire-breathing> <monster>) '()))

(add-method initialize
  (make-method (list <flying>)
    (lambda (call-next-method flyer initargs)
      (call-next-method)
      (default! flyer 'altitude 100))))

(add-method initialize
  (make-method (list <fire-breathing>)
    (lambda (call-next-method breather initargs)
      (call-next-method)
      (default! breather 'fire-damage 5))))

(add-method describe
  (make-method (list <flying>)
    (lambda (call-next-method flyer)
      (string-append (call-next-method)
                     ", flying at " (number->string (slot-ref flyer 'altitude)) "m"))))

(add-method describe
  (make-method (list <fire-breathing>)
    (lambda (call-next-method breather)
      (string-append (call-next-method) ", breathing fire"))))

(define smaug (make <dragon> 'name "Smaug" 'hp 60 'strength 9 'altitude 300))

(show "class precedence list" (map class-name (class-precedence-list <dragon>)))
(show "slots of a dragon" (map car (class-slots <dragon>)))
(show "describe smaug" (describe smaug))
(show "a dragon is a monster" (instance-of? smaug <monster>))
(show "a dragon is a thing" (instance-of? smaug <thing>))
(show "a dragon is an item" (instance-of? smaug <item>))


;;; ----------------------------------------------------------------------------
;;; 5. Multiple dispatch
;;; ----------------------------------------------------------------------------
;;;
;;; Methods can be specialized on more than one argument. Which method is called
;;; then depends on the classes of all arguments. This is where generic functions
;;; shine: an operation such as "use an item on something" doesn't belong to either
;;; the item or its target. If several methods are applicable, the one is chosen that
;;; is most specific for the leftmost argument where the methods differ.

(section "5. Multiple dispatch")

(define use (make-generic 'use))

(define (name-of thing)
  (slot-ref thing 'name))

;; The fallback for every kind of item used on any kind of thing

(add-method use
  (make-method (list <item> <thing>)
    (lambda (call-next-method item target)
      (string-append "Nothing happens when using the " (name-of item)
                     " on " (name-of target) "."))))

;; Swords hurt creatures

(add-method use
  (make-method (list <sword> <creature>)
    (lambda (call-next-method sword target)
      (slot-set! target 'hp (- (slot-ref target 'hp) (slot-ref sword 'damage)))
      (string-append (name-of target) " is hit and has " (number->string (slot-ref target 'hp))
                     " hit points left."))))

;; ... but not the player themselves

(add-method use
  (make-method (list <sword> <player>)
    (lambda (call-next-method sword target)
      "Better not.")))

;; ... and they are useless against dragons. This method is more specific than the
;; one for swords and creatures because of its second specializer: a dragon is a
;; creature (via monster), but not the other way around.

(add-method use
  (make-method (list <sword> <dragon>)
    (lambda (call-next-method sword target)
      (string-append "The " (name-of sword) " bounces off the scales of "
                     (name-of target) "."))))

;; Potions heal creatures, except for monsters

(add-method use
  (make-method (list <potion> <creature>)
    (lambda (call-next-method potion target)
      (slot-set! target 'hp (+ (slot-ref target 'hp) (slot-ref potion 'healing)))
      (string-append (name-of target) " feels better and has "
                     (number->string (slot-ref target 'hp)) " hit points."))))

(add-method use
  (make-method (list <potion> <monster>)
    (lambda (call-next-method potion target)
      (slot-set! target 'hp (- (slot-ref target 'hp) 1))
      (string-append "The " (name-of potion) " is poison for " (name-of target) "!"))))

(show "sword on goblin" (use sword goblin))
(show "potion on hero" (use potion hero))
(show "potion on goblin" (use potion goblin))
(show "sword on hero" (use sword hero))
(show "sword on smaug" (use sword smaug))
(show "hit points of smaug" (slot-ref smaug 'hp))
(show "sword on potion" (use sword potion))
(show "potion on sword" (use potion sword))
(show "hero on sword" (error-message (lambda () (use hero sword))))


;;; ----------------------------------------------------------------------------
;;; 6. Built-in classes
;;; ----------------------------------------------------------------------------
;;;
;;; There are classes for the primitive data types of Scheme, so methods can be
;;; added for numbers, strings, lists, etc. `<top>` is the superclass of all classes
;;; and can thus be used for providing a default method for any kind of value.
;;; There is no class for lists: lists are either pairs (`<pair>`) or the empty list
;;; (`<null>`).

(section "6. Built-in classes")

(define double (make-generic 'double))

(add-method double
  (make-method (list <top>)
    (lambda (call-next-method x)
      (string-append "Don't know how to double " (if (instance-of? x <thing>) (name-of x) "that")))))
(add-method double (make-method (list <number>) (lambda (call-next-method x) (* 2 x))))
(add-method double (make-method (list <string>) (lambda (call-next-method x) (string-append x x))))
(add-method double (make-method (list <vector>) (lambda (call-next-method x) (vector-append x x))))
(add-method double (make-method (list <null>) (lambda (call-next-method x) '())))
(add-method double
  (make-method (list <pair>)
    (lambda (call-next-method x)
      (cons (double (car x)) (double (cdr x))))))

(show "double a number" (double 21))
(show "double a string" (double "ab"))
(show "double a vector" (double #(1 2)))
(show "double a list" (double '(1 "x" (2))))
(show "double a symbol" (double 'x))
(show "double the hero" (double hero))
(show "classes of values"
      (map (lambda (x) (class-name (class-of x)))
           (list 1 2.5 "text" 'sym #\a '() '(1) #(1) #t car)))
(show "class of a class" (class-name (class-of <thing>)))
(show "class of a generic function" (class-name (class-of describe)))
(show "class precedence list of <number>"
      (map class-name (class-precedence-list <number>)))


;;; ----------------------------------------------------------------------------
;;; 7. Introspection
;;; ----------------------------------------------------------------------------
;;;
;;; Classes, generic functions, and methods are objects that can be inspected.

(section "7. Introspection")

(define (inspect-class class)
  (display "  class ") (display (class-name class)) (newline)
  (display "    direct superclasses: ")
  (write (map class-name (class-direct-superclasses class))) (newline)
  (display "    direct slots:        ")
  (write (map car (class-direct-slots class))) (newline)
  (display "    all slots:           ")
  (write (map car (class-slots class))) (newline)
  (display "    precedence list:     ")
  (write (map class-name (class-precedence-list class))) (newline))

(define (inspect-generic generic)
  (display "  generic function ") (display (generic-name generic))
  (display " with ") (display (length (generic-methods generic))) (display " methods:")
  (newline)
  (for-each (lambda (method)
              (display "    ")
              (write (map class-name (method-specializers method)))
              (newline))
            (reverse (generic-methods generic))))

(inspect-class <dragon>)
(inspect-class <player>)
(inspect-generic use)

(show "<dragon> is a subclass of <thing>" (subclass? <dragon> <thing>))
(show "<thing> is a subclass of <dragon>" (subclass? <thing> <dragon>))
(show "<dragon> is a class" (class? <dragon>))
(show "describe is a generic function" (generic? describe))
(show "smaug is a class" (class? smaug))


;;; ----------------------------------------------------------------------------
;;; 8. The metaobject protocol
;;; ----------------------------------------------------------------------------
;;;
;;; Tiny CLOS is built from the same ingredients that it provides to its users:
;;; classes are instances of the class `<class>`, generic functions are instances
;;; of `<generic>`, and methods are instances of `<method>`. Their behavior is
;;; defined by generic functions: the metaobject protocol (MOP). By subclassing
;;; `<class>` or `<generic>` and adding methods to the generic functions of the
;;; MOP, it is possible to change how the object system works. The following
;;; examples show three typical extensions.

(section "8. The metaobject protocol")

;;; 8a. Slot options: default values and shared slots
;;;
;;; Slot specifications are lists that consist of the name of the slot followed by
;;; options. The default implementation ignores the options. Here we define a new
;;; kind of class, a "metaclass", which gives meaning to the options `:init` and
;;; `:shared`. The new metaclass `<rich-class>` is a subclass of `<class>`.
;;;
;;; Generic function `compute-accessors` is called for each slot of a new class.
;;; It returns a getter and a setter procedure for the slot. The third parameter
;;; `allocator` is a procedure that reserves a new field in the instances of the
;;; class; it takes a procedure returning the initial value of the field. Slots with
;;; the option `:shared` don't use the instances for storing their values: all
;;; instances of the class share a single cell. (A subclass of such a class would
;;; get a cell of its own, since `compute-accessors` is called once for every slot
;;; of every class.)

(define <rich-class> (make-class '<rich-class> (list <class>) '()))

(add-method compute-accessors
  (make-method (list <rich-class>)
    (lambda (call-next-method class slot allocator)
      (let ((init (memq ':init (cdr slot)))
            (shared (memq ':shared (cdr slot))))
        (cond (init
                (allocator (lambda () (cadr init))))
              (shared
                (let ((cell (cadr shared)))
                  (list (lambda (object) cell)
                        (lambda (object value) (set! cell value)))))
              (else
                (call-next-method)))))))

;; Instances of a metaclass are classes, so they are created with `make`.

(define <orc>
  (make <rich-class>
        'name '<orc>
        'direct-superclasses (list <monster>)
        'direct-slots '((hp :init 15) (strength :init 4) (population :shared 0))))

(add-method initialize
  (make-method (list <orc>)
    (lambda (call-next-method orc initargs)
      (call-next-method)
      (slot-set! orc 'population (+ 1 (slot-ref orc 'population))))))

(define orc1 (make <orc> 'name "Grok"))
(define orc2 (make <orc> 'name "Mog"))
(define orc3 (make <orc> 'name "Snaga" 'strength 6))

(show "class of <orc>" (class-name (class-of <orc>)))
(show "orc1" (describe orc1))
(show "orc3" (describe orc3))
(show "population via orc1" (slot-ref orc1 'population))
(show "population via orc3" (slot-ref orc3 'population))

;;; 8b. Tracing generic functions
;;;
;;; Generic functions are instances of `<generic>`. A generic function is an
;;; applicable object, so a subclass of `<generic>` needs to be an instance of
;;; `<entity-class>` instead of `<class>`. When a method is added to a generic
;;; function, `compute-apply-generic` is called to compute the procedure that is
;;; applied when the generic function is called. We specialize it for a new kind
;;; of generic function that logs its calls.

(define <traced-generic>
  (make <entity-class>
        'name '<traced-generic>
        'direct-superclasses (list <generic>)
        'direct-slots '()))

(define trace-depth 0)

(define (trace-line . items)
  (display "  ")
  (display (make-string (* 2 trace-depth) #\space))
  (for-each display items)
  (newline))

(add-method compute-apply-generic
  (make-method (list <traced-generic>)
    (lambda (call-next-method generic)
      (let ((apply-generic (call-next-method)))
        (lambda args
          (trace-line "-> " (generic-name generic) " "
                      (map (lambda (arg) (class-name (class-of arg))) args))
          (set! trace-depth (+ trace-depth 1))
          (let ((result (apply apply-generic args)))
            (set! trace-depth (- trace-depth 1))
            (trace-line "<- " result)
            result))))))

(define power (make <traced-generic> 'name 'power))
(add-method power
  (make-method (list <number> <number>)
    (lambda (call-next-method base exponent)
      (if (= exponent 0)
          1
          (* base (power base (- exponent 1)))))))

(show "2 to the power of 3" (power 2 3))

;;; 8c. Method combination: around methods
;;;
;;; In CLOS, `:around` methods run before all other methods and decide whether
;;; and how the remaining methods are executed. Tiny CLOS does not provide them, but
;;; we can add them. The generic function `compute-methods` returns the sorted list
;;; of the methods that are applicable to a call. We create a new kind of generic
;;; function that moves all around methods to the front of this list.

(define <around-generic>
  (make <entity-class>
        'name '<around-generic>
        'direct-superclasses (list <generic>)
        'direct-slots '()))

(define <around-method>
  (make <class>
        'name '<around-method>
        'direct-superclasses (list <method>)
        'direct-slots '()))

(define (around-method? method)
  (instance-of? method <around-method>))

(add-method compute-methods
  (make-method (list <around-generic>)
    (lambda (call-next-method generic)
      (let ((compute (call-next-method)))
        (lambda (args)
          (let ((methods (compute args)))
            (append (filter around-method? methods)
                    (filter (lambda (method) (not (around-method? method)))
                            methods))))))))

;; The same methods are added to a regular and to an around-generic function.
;; Note that a method replaces an existing one with the same specializers.
;; That's why the around method is specialized for `<thing>`, while the primary
;; methods are specialized for `<creature>` and `<monster>`.

(define (make-attack-generic generic)
  (add-method generic
    (make-method (list <creature>)
      (lambda (call-next-method creature)
        (list 'attack (name-of creature)))))
  (add-method generic
    (make-method (list <monster>)
      (lambda (call-next-method monster)
        (cons 'growl (call-next-method)))))
  (add-method generic
    (make <around-method>
          'specializers (list <thing>)
          'procedure (lambda (call-next-method thing)
                       (cons 'around (call-next-method)))))
  generic)

(define plain-attack (make-attack-generic (make-generic 'plain-attack)))
(define around-attack (make-attack-generic (make <around-generic> 'name 'around-attack)))

(show "plain generic function" (plain-attack goblin))
(show "around generic function" (around-attack goblin))

;;; The generic functions of the MOP can also be used in isolation, e.g.
;;; to see how the class precedence list or the slots would be computed.

(show "compute-class-precedence-list"
      (map class-name (compute-class-precedence-list <dragon>)))


;;; ----------------------------------------------------------------------------
;;; 9. Syntactic sugar
;;; ----------------------------------------------------------------------------
;;;
;;; Tiny CLOS is a procedural API, so there are no special forms for defining
;;; classes, generic functions, and methods. If desired, it is easy to define such
;;; forms. Note that the name of the `call-next-method` procedure needs to be
;;; provided by the user of `define-method`; if the macro introduced it, it would
;;; not be visible in the body of the method because of hygiene.

(section "9. Syntactic sugar")

(define-syntax define-class
  (syntax-rules ()
    ((_ name (super ...) (slot ...))
      (define name (make-class 'name (list super ...) '(slot ...))))))

(define-syntax define-generic
  (syntax-rules ()
    ((_ name)
      (define name (make-generic 'name)))))

(define-syntax define-method
  (syntax-rules ()
    ((_ generic (next (arg class) ...) body ...)
      (add-method generic
        (make-method (list class ...)
          (lambda (next arg ...) body ...))))))

(define-class <wizard> (<player>) (mana))
(define-generic cast)

(define-method describe (next (wizard <wizard>))
  (string-append (next) " [mana " (number->string (slot-ref wizard 'mana)) "]"))

(define-method cast (next (wizard <wizard>) (target <creature>))
  (slot-set! wizard 'mana (- (slot-ref wizard 'mana) 5))
  (slot-set! target 'hp (- (slot-ref target 'hp) 3))
  (string-append (name-of wizard) " casts a spell on " (name-of target) "."))

(define-method cast (next (wizard <wizard>) (target <wizard>))
  (string-append (name-of wizard) " respects " (name-of target) " and doesn't cast."))

(define merlin (make <wizard> 'name "Merlin" 'hp 30 'mana 50))
(define morgana (make <wizard> 'name "Morgana" 'hp 25 'mana 40))

(show "describe merlin" (describe merlin))
(show "merlin casts at goblin" (cast merlin goblin))
(show "merlin casts at morgana" (cast merlin morgana))
(show "describe merlin" (describe merlin))
(show "describe goblin" (describe goblin))
(show "cast on item" (error-message (lambda () (cast merlin sword))))
