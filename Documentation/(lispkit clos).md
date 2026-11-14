# LispKit CLOS

Library `(lispkit clos)` implements _Tiny CLOS_, a compact object system for Scheme that is modeled after the _Common Lisp Object System_ (CLOS). Tiny CLOS was written by Gregor Kiczales at Xerox PARC in 1992 as a vehicle for teaching CLOS-style programming and for experimenting with _metaobject protocols_. It differs from CLOS in terms of its API, but the underlying object-oriented abstractions are equivalent. The version provided by LispKit is a slightly extended, R7RS-compatible adaptation of the original code.

The object system supports the following features:

   - **Classes** with named instance slots and **multiple inheritance**.
   - **Generic functions** with **multi-methods**: methods are not owned by a single class; instead, a generic function selects the most specific method based on the classes of _all_ of its arguments.
   - **Primary methods** and `call-next-method`. Other forms of method combination, such as `:before`, `:after` and `:around` methods, can be added via the metaobject protocol.
   - **Built-in classes** for the standard Scheme data types, so that methods can also be defined for numbers, strings, lists, etc.
   - **First-class classes, generic functions and methods.** There is no global registry or special syntax. Classes and generic functions are Scheme values that are bound to variables like any other value; Scheme's lexical scoping is used as the naming mechanism.
   - A simple but powerful **metaobject protocol** (MOP), which makes it possible to customize how objects are allocated and initialized, how slots are stored, how the class precedence list is computed, and how generic functions select and combine their methods.

Compared to CLOS, Tiny CLOS is deliberately minimal. In particular, there are no macros such as `defclass`, `defgeneric` and `defmethod` (everything is done via procedures), no slot options such as `:initform` or `:accessor`, no `eql` specializers, no `:before`/`:after`/`:around` methods and no way to change the class of an existing instance. The tutorial below shows how to get a lot of mileage out of the basic API, and the section on the metaobject protocol explains how to add missing features.

The library is optimized for clarity, not for speed. In particular, generic functions determine the applicable methods anew on every call.

## Tutorial

This tutorial introduces the programming model of Tiny CLOS step by step. All of the examples build on each other and use the following imports:

```scheme
(import (lispkit base)
        (lispkit math)
        (lispkit clos)
        (only (lispkit clos support) getl))
```

Library `(lispkit clos support)` provides some supporting procedures. Of those, only `getl` is typically needed (it looks up values in a list of alternating names and values, which is how initialization arguments are passed around).

### Generic functions instead of messages

In many object-oriented languages, such as Java or Smalltalk, a method belongs to exactly one class, and a method call such as `shape.area()` _sends a message_ to an object. The class of the receiver determines which code is executed.

CLOS-style object systems turn this around. Operations are represented by _generic functions_, which are ordinary functions that are applied like any other function: `(area shape)`. A generic function consists of a collection of _methods_, each one implementing the operation for a specific combination of classes. When a generic function is called, it looks at the classes of the arguments and selects the most specific method that is applicable. This design has two important consequences:

   - Methods do not "live in" classes. A method can be added to a generic function at any time, also for classes that were defined by somebody else. It is even possible to add methods for built-in types such as numbers or strings.
   - Since all arguments take part in the method selection, an operation that involves two objects (e.g. "draw this shape on that canvas") does not have to be assigned to one of the two classes.

### Classes and instances

A class is created with `make-class`. The simplest form takes the name of the class and a list of slot names. _Slots_ are the named storage locations of the instances of a class; they correspond to instance variables or fields in other languages. By convention, the names of classes are enclosed in angle brackets.

```scheme
(define <shape> (make-class '<shape> '()))
```

Classes are first-class values, just like procedures or lists. The name passed to `make-class` is only used as a label for the class; the class itself is referred to via the variable it is bound to. `<shape>` is a class without slots. It is the root of the class hierarchy for geometric shapes defined in this tutorial. The following definitions create subclasses; the second argument of `make-class` is now a list of _direct superclasses_, and the third argument is the list of slots that are added by the new class:

```scheme
(define <circle>    (make-class '<circle>    (list <shape>) '(radius)))
(define <rectangle> (make-class '<rectangle> (list <shape>) '(width height)))
(define <square>    (make-class '<square>    (list <rectangle>) '()))
```

A `<circle>` has one slot `radius`, a `<rectangle>` has the two slots `width` and `height`. A `<square>` inherits the slots of `<rectangle>` and does not define any new ones. If the list of superclasses is left out, `make-class` uses `<object>`, the root class of all user-defined classes, as the only superclass. Thus, `<shape>` is a direct subclass of `<object>`.

Instances are created with `make`. The first argument of `make` is the class to instantiate. Slots can be read with `slot-ref` and assigned with `slot-set!`:

```scheme
(define c (make <circle>))
(slot-ref c 'radius)           ⇒  #f
(slot-set! c 'radius 2)
(slot-ref c 'radius)           ⇒  2
```

Slots are untyped and initially hold the value `#f`. Accessing a slot that does not exist is an error:

```scheme
(slot-ref c 'width)            ⇒  [error] no slot `width` in instances of <circle>
(slot-bound? c 'radius)        ⇒  #t
(slot-bound? c 'width)         ⇒  #f
```

Procedure `class-of` returns the class of any object, and `instance-of?` checks whether an object is an instance of a class or of one of its subclasses. Since instances are represented as procedures, they are not printed in a helpful way. Use `class-name` to get the name of a class:

```scheme
(class-name (class-of c))      ⇒  <circle>
(eq? (class-of c) <circle>)    ⇒  #t
(instance-of? c <circle>)      ⇒  #t
(instance-of? c <shape>)       ⇒  #t
(instance-of? c <rectangle>)   ⇒  #f
(class-slots <rectangle>)      ⇒  ((width) (height))
```

Objects, classes and generic functions are compared with `eq?`.

### Initializing instances

Right now, instances are created with all slots set to `#f`. The arguments that follow the class in a call to `make` are ignored:

```scheme
(slot-ref (make <circle> 'radius 7) 'radius)  ⇒  #f
```

These arguments, the _initialization arguments_ or _initargs_, are passed to the generic function `initialize` after a new instance has been allocated. The call `(make <circle> 'radius 7)` is essentially equivalent to

```scheme
(let ((instance (allocate-instance <circle>)))
  (initialize instance '(radius 7))
  instance)
```

Initialization arguments are alternating names and values. The default method of `initialize` does nothing with them. To make use of them, we define our own method. Methods are created with `make-method`, which takes a list of _specializers_ (classes, one for each argument that is used for the selection of the method) and a procedure implementing the method. Method procedures have one more parameter than the generic function has arguments: the first parameter is always bound to a procedure which is called `call-next-method` in this tutorial. We will talk about it in a moment. A method is attached to a generic function with `add-method`:

```scheme
(add-method initialize
  (make-method (list <circle>)
    (lambda (call-next-method circle initargs)
      (call-next-method)
      (slot-set! circle 'radius (getl initargs 'radius 1)))))
```

This method is applicable whenever the first argument of `initialize` is a `<circle>` (the second argument is not specialized). It first calls `call-next-method`, which invokes the next method of `initialize`, in this case the default method that is applicable to all objects. This is a general rule: **initialization methods should call `(call-next-method)` first**, so that the superclasses get a chance to initialize their parts of the instance. Then the method stores the value associated with `radius` in the initialization arguments in the `radius` slot. Procedure `getl` looks up a name in a list of alternating names and values; its third argument is returned if the name is not found:

```scheme
(slot-ref (make <circle> 'radius 5) 'radius)   ⇒  5
(slot-ref (make <circle>) 'radius)             ⇒  1
```

Methods for `<rectangle>` and `<square>` are defined in the same way. A square is created with a `side` argument, which is used to initialize both the `width` and the `height` slot. The `<square>` method calls `call-next-method` first, which, in turn, runs the `<rectangle>` method on the same arguments, and then overwrites the two slots:

```scheme
(add-method initialize
  (make-method (list <rectangle>)
    (lambda (call-next-method rect initargs)
      (call-next-method)
      (slot-set! rect 'width (getl initargs 'width 1))
      (slot-set! rect 'height (getl initargs 'height 1)))))

(add-method initialize
  (make-method (list <square>)
    (lambda (call-next-method square initargs)
      (call-next-method)
      (let ((side (getl initargs 'side 1)))
        (slot-set! square 'width side)
        (slot-set! square 'height side)))))

(define sq (make <square> 'side 4))
(list (slot-ref sq 'width) (slot-ref sq 'height))  ⇒  (4 4)
```

Writing an `initialize` method for every class gets tedious if all it does is to copy initialization arguments into slots of the same name. Since methods are inherited, this can be done once for a common superclass. The following class can be used as a superclass (preferably the first one) of classes whose slots are initialized by name:

```scheme
(define <initializable> (make-class '<initializable> '()))

(add-method initialize
  (make-method (list <initializable>)
    (lambda (call-next-method object initargs)
      (call-next-method)
      (let ((missing (list 'missing)))
        (for-each (lambda (slot)
                    (let ((value (getl initargs (car slot) missing)))
                      (if (not (eq? value missing))
                          (slot-set! object (car slot) value))))
                  (class-slots (class-of object)))))))

(define <point> (make-class '<point> (list <initializable>) '(x y)))
(define pt (make <point> 'x 3 'y 4))
(list (slot-ref pt 'x) (slot-ref pt 'y))             ⇒  (3 4)
(slot-ref (make <point> 'y 1) 'x)                    ⇒  #f
```

Do not add methods for `initialize` that are specialized for `<object>`, `<class>`, `<generic>` or `<method>`: this would replace the default methods that make the object system work (see the section on pitfalls below).

### Generic functions and methods

A generic function is created with `make-generic`, optionally giving it a name (which is only used for printing and debugging). A new generic function does not have any methods yet; calling it is an error:

```scheme
(define area (make-generic 'area))
(generic? area)      ⇒  #t
(generic-name area)  ⇒  area
(area 1)             ⇒  [error] has no methods
```

Methods are added by means of `add-method`. Each method specifies the classes of the arguments for which it is applicable and a procedure implementing it. The first parameter of the procedure is the `call-next-method` procedure, the remaining parameters are the arguments of the generic function:

```scheme
(add-method area
  (make-method (list <circle>)
    (lambda (call-next-method c)
      (* pi (slot-ref c 'radius) (slot-ref c 'radius)))))

(add-method area
  (make-method (list <rectangle>)
    (lambda (call-next-method r)
      (* (slot-ref r 'width) (slot-ref r 'height)))))
```

Now `area` can be applied like any other procedure to circles and to rectangles:

```scheme
(area (make <circle> 'radius 2))                 ⇒  12.566370614359172
(area (make <rectangle> 'width 3 'height 4))     ⇒  12
(area sq)                                        ⇒  16
(map area (list sq (make <rectangle> 'width 2 'height 5)))  ⇒  (16 10)
(procedure? area)                                ⇒  #t
```

The call `(area sq)` shows that methods are inherited: there is no method for `<square>`, but since a square is also a rectangle, the method for `<rectangle>` is applicable. When there is no applicable method at all, an error is signaled:

```scheme
(area 42)  ⇒  [error] no applicable methods/next methods
```

Unlike `defmethod` in CLOS, `add-method` does not create a generic function if it doesn't exist yet. It always needs to be created explicitly with `make-generic`. If a method gets added with the same specializers as an existing one, it replaces the existing one. Methods can be inspected with `generic-methods`, `method-specializers` and `method-procedure`.

Since slots are an implementation detail, it is good practice to hide them behind generic functions and to hand out only these to the clients of a class:

```scheme
(define radius (make-generic 'radius))
(add-method radius
  (make-method (list <circle>)
    (lambda (call-next-method c) (slot-ref c 'radius))))
(radius (make <circle> 'radius 3))  ⇒  3
```

An accessor does not need to correspond to a slot: it may as well compute its result from other slots. This allows for changing the representation of a class without having to change its users.

### Inheritance and call-next-method

A generic function can have many applicable methods for a call. They are sorted from the most specific to the least specific one, and the most specific method is executed first. If that method wants to make use of the behavior of a less specific method, it can call `call-next-method`, a procedure without parameters, which invokes the next applicable method with the _same_ arguments and returns its result. The called method can do something before and/or after the call, it can modify the result, or it can decide not to call the next method at all.

The following generic function `describe` returns a description of a shape. The method for `<shape>` provides the base case. The methods for the subclasses extend the result of the method of their superclass:

```scheme
(define describe (make-generic 'describe))

(add-method describe
  (make-method (list <shape>)
    (lambda (call-next-method s) "a shape")))
(add-method describe
  (make-method (list <circle>)
    (lambda (call-next-method c)
      (string-append "a circle, which is " (call-next-method)))))
(add-method describe
  (make-method (list <rectangle>)
    (lambda (call-next-method r)
      (string-append "a rectangle, which is " (call-next-method)))))
(add-method describe
  (make-method (list <square>)
    (lambda (call-next-method s)
      (string-append "a square, which is " (call-next-method)))))

(describe (make <circle>))  ⇒  "a circle, which is a shape"
(describe sq)               ⇒  "a square, which is a rectangle, which is a shape"
```

Calling `call-next-method` when there is no next method is an error. Thus, the least specific method of a generic function should not call it, as `<shape>` does above.

### Multiple inheritance

A class may have more than one direct superclass. This is often used to define small, reusable classes, so-called _mixins_, that contribute some state and behavior to the classes inheriting from them. Here is a mixin that adds a color:

```scheme
(define <colored> (make-class '<colored> '(color)))

(add-method initialize
  (make-method (list <colored>)
    (lambda (call-next-method obj initargs)
      (call-next-method)
      (slot-set! obj 'color (getl initargs 'color 'black)))))

(add-method describe
  (make-method (list <colored>)
    (lambda (call-next-method obj)
      (string-append (call-next-method)
                     " (" (symbol->string (slot-ref obj 'color)) ")"))))

(define <colored-circle>
  (make-class '<colored-circle> (list <colored> <circle>) '()))

(define cc (make <colored-circle> 'radius 3 'color 'red))
(slot-ref cc 'radius)       ⇒  3
(slot-ref cc 'color)        ⇒  red
(area cc)                   ⇒  28.274333882308138
(describe cc)               ⇒  "a circle, which is a shape (red)"
```

Instances of `<colored-circle>` have the slots of both superclasses; the methods of both superclasses are applicable. The order in which they are called is determined by the _class precedence list_ of `<colored-circle>`. It lists the class itself and all of its superclasses from the most specific to the least specific one:

```scheme
(map class-name (class-precedence-list <colored-circle>))
  ⇒  (<colored-circle> <colored> <circle> <shape> <object> <top>)
```

The class precedence list is computed from the lists of direct superclasses, in such a way that

   - every class precedes its superclasses,
   - the order of the direct superclasses in the class definition is respected (`<colored>` precedes `<circle>` because it is listed first), and
   - every class occurs only once, even if it can be reached via multiple paths.

The last two classes of every class precedence list are `<object>` and `<top>`: `<object>` is the root of all classes that are created with `make-class`, and `<top>` is the root of all classes, including the built-in ones such as `<number>` or `<string>`.

The order matters. If `<colored-circle>` had been defined as `(list <circle> <colored>)`, `<circle>` would precede `<colored>`. Since the `describe` method of `<circle>` calls the method of `<shape>`, which does not call `call-next-method`, the method of `<colored>` would never be reached and the color would be missing from the description. The convention is to list mixins before the "real" superclasses.

Instance slots are collected along the class precedence list as well. If a slot with the same name is declared by multiple classes in the hierarchy, the instances have only one such slot:

```scheme
(class-slots <colored-circle>)  ⇒  ((color) (radius))
```

### Multiple dispatch

So far, all methods have been specialized on just a single argument. Methods can be specialized on any number of arguments, though. This is called _multiple dispatch_, and methods that make use of it are _multi-methods_. A typical use case is an operation that involves objects of two different hierarchies, where the implementation depends on both. The following generic function `render` produces a string that represents a shape on a certain kind of canvas:

```scheme
(define <canvas>      (make-class '<canvas> '()))
(define <text-canvas> (make-class '<text-canvas> (list <canvas>) '()))
(define <svg-canvas>  (make-class '<svg-canvas> (list <canvas>) '()))

(define render (make-generic 'render))

(add-method render
  (make-method (list <shape> <canvas>)
    (lambda (call-next-method shape canvas) "?")))

(add-method render
  (make-method (list <circle> <text-canvas>)
    (lambda (call-next-method c canvas)
      (string-append "(" (number->string (slot-ref c 'radius)) ")"))))

(add-method render
  (make-method (list <circle> <svg-canvas>)
    (lambda (call-next-method c canvas)
      (string-append "<circle r=\"" (number->string (slot-ref c 'radius)) "\"/>"))))

(add-method render
  (make-method (list <rectangle> <text-canvas>)
    (lambda (call-next-method r canvas)
      (string-append "[" (number->string (slot-ref r 'width)) "x"
                         (number->string (slot-ref r 'height)) "]"))))

(define text (make <text-canvas>))
(define svg (make <svg-canvas>))

(render (make <circle> 'radius 2) text) ⇒  "(2)"
(render (make <circle> 'radius 2) svg)  ⇒  "<circle r=\"2\"/>"
(render sq text)                        ⇒  "[4x4]"
(render sq svg)                         ⇒  "?"
(render text sq)                        ⇒  [error] no applicable methods/next methods
```

A method is applicable if every specializer is a superclass of (or the same as) the class of the corresponding argument. Square `sq` is rendered on a text canvas with the method for `<rectangle>` and `<text-canvas>`. For the SVG canvas, there is no specific method, so the catch-all method for `<shape>` and `<canvas>` gets selected.

If multiple methods are applicable, they are sorted by comparing their specializers from left to right: the first argument where the specializers differ decides, and the method whose specializer is more specific for this argument (i.e. which comes earlier in the class precedence list of the argument's class) wins. As a consequence, **the leftmost arguments are more important than the arguments to their right**:

```scheme
(add-method render
  (make-method (list <square> <canvas>)
    (lambda (call-next-method s canvas) "a square on some canvas")))
(add-method render
  (make-method (list <rectangle> <svg-canvas>)
    (lambda (call-next-method r canvas) "a rectangle on an SVG canvas")))

(render sq svg)  ⇒  "a square on some canvas"
```

Both new methods are applicable to `sq` and `svg`. Since the first argument is a `<square>`, the method specialized for `<square>` wins even though the other method is more specific in its second argument. A method may also have fewer specializers than the generic function has arguments. The remaining arguments are then not taken into account when selecting the method.

### Built-in classes

Tiny CLOS defines classes for the primitive data types of Scheme. These can be used as specializers just as classes created with `make-class`. All of them are direct subclasses of `<top>`.

| Class | Instances |
|:------|:----------|
| `<pair>` | pairs and non-empty lists |
| `<null>` | the empty list |
| `<boolean>` | `#t` and `#f` |
| `<symbol>` | symbols |
| `<number>` | all numbers (there are no subclasses such as `<integer>`) |
| `<char>` | characters |
| `<string>` | strings |
| `<vector>` | vectors |
| `<bytevector>` | bytevectors |
| `<hashtable>` | hash tables |
| `<procedure>` | procedures (but not objects, generic functions or classes) |
| `<input-port>`, `<output-port>` | ports |

Here is how to derive those class names from literals:

```scheme
(map (lambda (x) (class-name (class-of x))) (list 1 2.5 "s" 'a '() '(1) #(1) car))
  ⇒  (<number> <number> <string> <symbol> <null> <pair> <vector> <procedure>)
```

Using these classes, it is possible to add new operations to existing types. Here is a generic function `size` that returns the number of elements of a collection. The method for `<top>` serves as default for everything that is not a collection:

```scheme
(define size (make-generic 'size))
(add-method size
  (make-method (list <top>)
    (lambda (call-next-method x) 1)))
(add-method size
  (make-method (list <string>)
    (lambda (call-next-method s) (string-length s))))
(add-method size
  (make-method (list <vector>)
    (lambda (call-next-method v) (vector-length v))))
(add-method size
  (make-method (list <null>)
    (lambda (call-next-method x) 0)))
(add-method size
  (make-method (list <pair>)
    (lambda (call-next-method p) (+ 1 (size (cdr p))))))

(map size (list "hello" #(1 2 3) '(a b c) '() 'x 3.5))   ⇒  (5 3 3 0 1 1)
```

Not every Scheme value has a class. Values of types without a corresponding class, such as promises, records, or the end-of-file object, are not instances of any class: `class-of` returns `#f` for them, and they cannot be used as arguments of a generic function. Classes, generic functions and methods are objects like any other:

```scheme
(class-name (class-of <circle>))  ⇒  <class>
(class-name (class-of area))      ⇒  <generic>
(class-of (delay 1))              ⇒  #f
```

### Introspection

Classes, generic functions and methods can be inspected at runtime:

```scheme
(map class-name (class-direct-superclasses <colored-circle>))
  ⇒  (<colored> <circle>)
(class-direct-slots <circle>)       ⇒  ((radius))
(class-slots <colored-circle>)      ⇒  ((color) (radius))
(subclass? <square> <shape>)        ⇒  #t
(subclass? <shape> <square>)        ⇒  #f
(class? <square>)                   ⇒  #t
(class? sq)                         ⇒  #f
(map (lambda (m)
       (map class-name (method-specializers m)))
     (generic-methods render))
  ⇒  ((<rectangle> <svg-canvas>)
      (<square> <canvas>)
      (<rectangle> <text-canvas>)
      (<circle> <svg-canvas>)
      (<circle> <text-canvas>)
      (<shape> <canvas>))
```

Since slot specifications are lists with the name of the slot as their first element, `(map car (class-slots c))` returns the names of the slots of class `c`. The remaining elements of a slot specification are _slot options_. They are ignored by the default implementation but can be used by extensions of the object system, which we will discuss next.

### Customizing the object system

The Tiny CLOS _metaobject protocol_ (MOP) makes it possible to change the behavior of the object system itself. The key idea is that classes, generic functions and methods are themselves objects, and instances of classes: `<class>` is the class of all classes, `<generic>` the class of all generic functions, and `<method>` the class of all methods. The procedures `make-class`, `make-generic` and `make-method` are just convenient shortcuts for calling `make` with the respective class:

```scheme
(define <point3d>
  (make <class>
        'name '<point3d>
        'direct-superclasses (list <object>)
        'direct-slots '(x y z)))
```

This is equivalent to `(make-class '<point3d> (list <object>) '(x y z))`. Since `<class>` is an ordinary class, it can be subclassed. A subclass of `<class>` is a _metaclass_, a class whose instances are classes. By adding methods to the generic functions of the MOP for such a metaclass, the behavior of its instances can be changed. The generic functions of the MOP are structured into three protocols:

```
make                                  the instance creation protocol
  allocate-instance
  initialize
initialize (for classes)              the class initialization protocol
  compute-class-precedence-list
  compute-slots
  compute-accessors                   (called once for every slot)
add-method                            the generic invocation protocol
  compute-apply-generic
    (when a generic function is called:)
    compute-methods
      compute-method-more-specific?
    compute-apply-methods
```

**Slot options.** Slots can be declared as a list whose first element is the slot's name and whose remaining elements are options, e.g. `(balance :init 0)`. Default values for slots are not supported out of the box, but they are easy to add. Slots are allocated by `compute-accessors`, which is called by the class initialization once for every slot, and returns a getter and a setter procedure for it. The `allocator` argument is a procedure that allocates a new field in the instances and returns a getter and a setter. It takes as an argument a procedure without parameters that returns the initial value of the field. The default method calls it with a procedure that returns `#f`. The following metaclass overrides `compute-accessors` to look for an `:init` option:

```scheme
(define <initializing-class> (make-class '<initializing-class> (list <class>) '()))

(add-method compute-accessors
  (make-method (list <initializing-class>)
    (lambda (call-next-method class slot allocator)
      (let ((init (memq ':init (cdr slot))))
        (if init
            (allocator (lambda () (cadr init)))
            (call-next-method))))))
```

Instances of this metaclass have to be created via `make`:

```scheme
(define <account>
  (make <initializing-class>
        'name '<account>
        'direct-superclasses (list <object>)
        'direct-slots '((owner) (balance :init 0) (limit :init 100))))

(define <savings>
  (make <initializing-class>
        'name '<savings>
        'direct-superclasses (list <account>)
        'direct-slots '((rate :init 1/50) (limit :init 0))))

(define acc (make <account>))
(list (slot-ref acc 'owner) (slot-ref acc 'balance) (slot-ref acc 'limit))
  ⇒  (#f 0 100)
(define sav (make <savings>))
(list (slot-ref sav 'balance) (slot-ref sav 'limit) (slot-ref sav 'rate))
  ⇒  (0 0 1/50)
```

Slots with the same name that are declared in multiple classes are merged; the options are concatenated, with those of the most specific class first. Therefore, `memq` finds the `:init` option of the most specific class, and `<savings>` overrides the default for `limit`.

**Tracing generic functions.** The behavior of generic functions is customized in a similar way, with a subclass of `<generic>`. Since generic functions are applicable, they are instances of a class of class `<entity-class>`. A subclass of `<generic>` is thus created by instantiating `<entity-class>`. Procedure `compute-apply-generic` returns the procedure that is called when a generic function is applied. By default, this procedure determines the applicable methods and calls the first one. The following method wraps this procedure for traced generic functions so that the calls get logged:

```scheme
(define <traced-generic>
  (make <entity-class>
        'name '<traced-generic>
        'direct-superclasses (list <generic>)
        'direct-slots '()))

(add-method compute-apply-generic
  (make-method (list <traced-generic>)
    (lambda (call-next-method generic)
      (let ((apply-generic (call-next-method)))
        (lambda args
          (display "enter ")
          (write (cons (generic-name generic)
                       (map (lambda (arg)
                              (class-name (class-of arg))) args)))
          (newline)
          (let ((result (apply apply-generic args)))
            (display "leave ")
            (write (generic-name generic))
            (display " => ")
            (write result)
            (newline)
            result))))))
```

Traced generic functions are created with `make`, giving the class and a name. They are used like any other generic function:

```scheme
(define traced-area
  (make <traced-generic> 'name 'traced-area))
(add-method traced-area
  (make-method (list <shape>)
    (lambda (call-next-method s) 0)))
(add-method traced-area
  (make-method (list <circle>)
    (lambda (call-next-method c) (+ 1 (call-next-method)))))

(traced-area (make <circle>))
```

This prints

```
enter (traced-area <circle>)
leave traced-area => 1
```

and returns `1`. Note that the procedure returned by `compute-apply-generic` is computed when `add-method` is called. A method for `compute-apply-generic` therefore needs to be in place before methods are added to the generic functions it is supposed to affect.

**Further extensions.** The following table lists some typical uses of the generic functions of the protocol. LispKit's collection of example programs includes two files that demonstrate some of them: `ObjectOrientation.scm` contains the original examples of Tiny CLOS, and `TinyClos.scm` provides a walk-through of the library, including examples of the MOP.

| Generic function | Typical use |
|:-----------------|:------------|
| `allocate-instance` | Keep track of all instances of a class; limit the number of instances |
| `compute-accessors` | Default values for slots; class-allocated (shared) slots; slots stored in an association list or in an external database; read-only slots |
| `compute-slots` | Add slots to all instances of a class, e.g. an identity |
| `compute-class-precedence-list` | Use a different algorithm for ordering the superclasses |
| `compute-apply-generic` | Tracing, profiling, caching of the method selection |
| `compute-methods` | Method combination, e.g. `:around` methods that are called before all other methods |
| `compute-method-more-specific?` | Change the ordering of the applicable methods |
| `compute-apply-methods` | Change the way how the methods are chained, e.g. to run all methods |

## API

### Objects and slots

**(make _class initarg ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates and returns a new instance of _class_. First, `make` calls `allocate-instance` with _class_ to allocate the instance. Then, it calls `initialize` with the instance and the list `(initarg ...)` of initialization arguments. The initialization arguments are alternating names (usually symbols, compared with `eq?`) and values. Methods of `initialize` can retrieve the values of the arguments with `getl`.

```scheme
(define <pos> (make-class '<pos> '(x y)))
(add-method initialize
  (make-method (list <pos>)
    (lambda (call-next-method pos initargs)
      (call-next-method)
      (slot-set! pos 'x (getl initargs 'x 0))
      (slot-set! pos 'y (getl initargs 'y 0)))))
(slot-ref (make <pos> 'x 1 'y 3) 'y)  ⇒  3
```

The default method of `initialize` ignores all initialization arguments. `make` is also used for creating classes, generic functions and methods (see the descriptions of `make-class`, `make-generic` and `make-method` below).

**(class-of _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the class of _obj_. For instances of classes, this is the class that was used for creating them. The class of a built-in value, e.g. a number or a string, is the corresponding built-in class, such as `<number>` or `<string>`. For values of other types, such as records or promises, `#f` is returned.

**(instance? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ has a class, i.e. if `(class-of obj)` is not `#f`. Since the built-in values of Scheme have classes, too, this includes numbers, strings, etc. as well.

**(instance-of? _obj class_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a direct or an indirect instance of _class_, i.e. if _class_ is in the class precedence list of the class of _obj_. It is an error if _obj_ does not have a class.

```scheme
(instance-of? 42 <number>)   ⇒  #t
(instance-of? 42 <top>)      ⇒  #t
(instance-of? 42 <string>)   ⇒  #f
```

**(slot-ref _obj slot-name_)**  <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the value of the slot _slot-name_ of object _obj_. It is an error if instances of the class of _obj_ do not have a slot with this name. Slots that were not initialized have the value `#f`, unless the metaobject protocol was used to provide a different default.

**(slot-set! _obj slot-name value_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the slot _slot-name_ of object _obj_ to _value_. It is an error if instances of the class of _obj_ do not have a slot with this name.

**(slot-bound? _obj slot-name_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the class of _obj_ declares or inherits a slot named _slot-name_, otherwise `#f`. In contrast to CLOS, this procedure does not tell whether the slot holds a value.

**(slot-accessors _class slot-name_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list `(getter setter)` consisting of two procedures for reading and writing the slot _slot-name_ of the instances of _class_. The _getter_ takes an instance as its argument, the _setter_ takes an instance and a new value. It is an error if instances of _class_ do not have a slot with this name. `slot-ref` and `slot-set!` are implemented in terms of these procedures. Looking up the accessors once and reusing them is faster than calling `slot-ref` or `slot-set!` repeatedly.

```scheme
(let ((accessors (slot-accessors <pos> 'x))
      (p (make <pos> 'x 7)))
  ((car accessors) p))      ⇒  7
```

**(slot-accessor-procedures _class slot-name_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Like `slot-accessors`, but returns the getter and the setter procedure as two values.

```scheme
(let-values (((get set) (slot-accessor-procedures <pos> 'x))
             ((p) (make <pos>)))
  (set p 5)
  (get p))                  ⇒  5
```

### Classes

**(make-class _slots_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-class _name_)**  
**(make-class _superclasses slots_)**  
**(make-class _name slots_)**  
**(make-class _name superclasses slots_)**  

Creates and returns a new class. _name_ is the name of the class, usually a symbol of the form `<name>`; the default is `#f`. _superclasses_ is a list of the direct superclasses of the new class; the default is `(list <object>)`. _slots_ is the list of the slots that are added by the new class (in addition to those inherited from the superclasses). Each slot is either a symbol or a list whose first element is a symbol: the name of the slot. The remaining elements of a list are slot options, which are ignored by the default implementation. Plain symbols are converted to lists, i.e. `x` is the same as `(x)`.

Procedure `make-class` accepts one, two or three arguments. If there are two arguments, the first one is interpreted as the list of superclasses if it is a (non-empty) list, and as the name otherwise. The following expressions are equivalent:

```scheme
(make-class '<pos> (list <object>) '(x y))
(make-class '<pos> '(x y))
(make-class (list <object>) '(x y))   ; anonymous class
(make-class '(x y))                   ; anonymous class
(make <class>
      'name '<pos>
      'direct-superclasses (list <object>)
      'direct-slots '(x y))
```

Classes are created by calling `make` on `<class>`, or on a subclass of `<class>` for classes with special behavior. The following initialization arguments are supported: `name`, `direct-superclasses` and `direct-slots`. If `direct-superclasses` is missing, the class has no superclasses at all, not even `<object>`. Such classes cannot be instantiated.

```scheme
(define <a> (make-class '<a> '(x)))
(define <b> (make-class '<b> (list <a>) '(y z)))
(class-slots <b>)    ⇒  ((y) (z) (x))
```

**(class? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a class, i.e. an instance of `<class>`; otherwise `#f` is returned. This is also the case for the built-in classes.

**(subclass? _class superclass_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _superclass_ is in the class precedence list of _class_, i.e. if _class_ is _superclass_ itself or inherits from it directly or indirectly.

**(class-name _class_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the name of _class_, or `#f` if _class_ has no name.

**(class-direct-superclasses _class_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the list of the direct superclasses of _class_ in the order in which they were specified.

**(class-direct-slots _class_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the list of the slots that were declared by _class_ itself. Each slot is represented by a list: the name of the slot followed by the slot options.

**(class-slots _class_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the list of all slots of the instances of _class_, i.e. the slots of _class_ and the ones inherited from its superclasses. The list is computed by `compute-slots`. Each slot is represented by a list: the name of the slot followed by the slot options.

**(class-precedence-list _class_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the class precedence list of _class_: a list of classes, starting with _class_ itself, followed by all of its superclasses, from the most specific one to the least specific one. The last element of the list is `<top>`. The list is computed by `compute-class-precedence-list`.

### Generic functions and methods

**(make-generic)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-generic _name_)**  

Creates and returns a new generic function without any methods. _name_ is a label for the generic function, usually a symbol, which is available via `generic-name`. Generic functions are procedures. Applying a generic function to arguments selects the applicable methods based on the classes of the arguments, sorts them by specificity and calls the most specific one. Calling a generic function without methods is an error.

A generic function can also be created by calling `make` on `<generic>`, or on a subclass of `<generic>`. The initialization argument `name` specifies the name.

```scheme
(define area (make-generic 'area))
(define area (make <generic> 'name 'area))   ; equivalent
```

**(generic? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a generic function, i.e. an instance of `<generic>`; otherwise `#f` is returned.

**(generic-name _generic_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the name of generic function _generic_, or `#f` if it does not have a name.

**(generic-methods _generic_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the list of the methods of generic function _generic_. The most recently added method is first.

**(make-method _specializers proc_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates and returns a new method. _specializers_ is a list of classes. A method is applicable to a call of a generic function if the class of the _i_-th argument is _specializers[i]_ or inherits from it, for all _i_. The number of specializers does not need to be the same as the number of arguments. A method with fewer specializers than arguments ignores the remaining arguments for the purpose of the method selection; it is not applicable if it has more specializers than there are arguments.

_proc_ is the procedure implementing the method. It is called with the arguments of the generic function, preceded by an additional argument: a procedure without parameters called `call-next-method`. Calling it invokes the next most specific applicable method with the same arguments and returns its result. It is an error to call it if there is no such method.

A method can also be created by calling `make` on `<method>`, or on a subclass of `<method>` with the initialization arguments `specializers` and `procedure`.

```scheme
(make-method (list <number> <number>)
             (lambda (call-next-method x y) (+ x y)))
(make <method>
      'specializers (list <number> <number>)
      'procedure (lambda (call-next-method x y) (+ x y)))   ; equivalent
```

**(method? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a method, i.e. an instance of `<method>`; otherwise `#f` is returned.

**(method-specializers _method_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the list of the specializers of _method_.

**(method-procedure _method_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the procedure implementing _method_. The first parameter of this procedure is the `call-next-method` procedure.

**(add-method _generic method_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Adds _method_ to generic function _generic_. A method of _generic_ that has the same specializers as _method_ is replaced by _method_. Specializer lists are compared pairwise up to the length of the shorter list; so, a method for `(<top>)` is replaced by a method for `(<top> <top>)` and vice versa. Methods of the same generic function should therefore always use the same number of specializers, unless the shorter methods are meant to be replaced.

`add-method` calls `compute-apply-generic` to compute the procedure that gets applied when the generic function is called. The result is unspecified. It is an error if _generic_ is not a generic function.

Note that `add-method` is not a generic function. This is why the protocol cannot be customized to change how methods are added.

### The instance creation and initialization protocol

**initialize** <span style="float:right;text-align:rigth;">[generic procedure]</span>  
**(initialize _instance initargs_)**  

Generic function that is called by `make` for initializing a newly allocated _instance_. _initargs_ is the list of initialization arguments that were passed to `make`: alternating names and values. The result of `initialize` is ignored. Methods for `initialize` should call `call-next-method` first and then initialize the slots introduced by the class they are specialized for.

Methods are provided for the following classes:

   - `<object>`: does nothing.
   - `<class>`: initializes the name, the direct superclasses and the direct slots from the initialization arguments `name`, `direct-superclasses` and `direct-slots`, and then calls `compute-class-precedence-list`, `compute-slots` and `compute-accessors` to complete the class.
   - `<generic>`: initializes the name from the initialization argument `name` and removes all methods.
   - `<method>`: initializes the specializers and the procedure from the initialization arguments `specializers` and `procedure`.

Do not call `initialize` directly. Add methods to it instead.

**allocate-instance** <span style="float:right;text-align:rigth;">[generic procedure]</span>  
**(allocate-instance _class_)**  

Generic function that is called by `make` for creating a new instance of _class_ with uninitialized slots. There are methods for `<class>`, which allocate regular instances, and for `<entity-class>`, which allocate instances that are procedures (such as generic functions). Customizing `allocate-instance` makes it possible, for example, to keep track of all instances of a class.

### The class initialization protocol

The following generic functions are called by the `initialize` method for `<class>` when a new class is created. They can be customized by adding methods for subclasses of `<class>`.

**compute-class-precedence-list** <span style="float:right;text-align:rigth;">[generic procedure]</span>  
**(compute-class-precedence-list _class_)**  

Returns the class precedence list for _class_: a list of classes, starting with _class_ and ending with `<top>`. The default method uses a topological sort of the superclasses of _class_ that respects the order of the direct superclasses of every class; ties are broken in favor of the class that is the direct superclass of the rightmost class that was already placed. At this point, the direct superclasses of _class_ are already initialized. The result is stored in the class and returned by `class-precedence-list`.

**compute-slots** <span style="float:right;text-align:rigth;">[generic procedure]</span>  
**(compute-slots _class_)**  

Returns the list of all slots of the instances of _class_. Each slot is a list consisting of the slot's name followed by its options. The default method collects the direct slots of all classes in the class precedence list of _class_ (which is available at this point), starting with the most specific class. Slots with the same name are merged into one slot whose options are the concatenation of the options of all the slots. The result is stored in the class and returned by `class-slots`.

**compute-accessors** <span style="float:right;text-align:rigth;">[generic procedure]</span>  
**(compute-accessors _class slot allocator_)**  

Returns a list `(getter setter)` of two procedures for reading and writing _slot_ in the instances of _class_. _slot_ is one element of the result of `compute-slots`. It is called once for every slot of the class. _allocator_ is a procedure with one parameter _init_, which is a procedure without parameters returning the initial value of the slot. The result of calling _allocator_ is a list `(getter setter)` of procedures providing access to a newly allocated field of the instances of the class. The default method allocates a new field for every slot with `#f` as its initial value.

Methods for `compute-accessors` can implement different strategies for storing slots: they can provide another initial value, allocate fields that are shared between all instances of a class, put slots into one field in an association list, or implement the access to a slot via a different representation, such as a table in a database. The resulting accessors are also available via `slot-accessors`.

### The generic invocation protocol

The following generic functions determine how generic functions work. They are called with the generic function they are working on as the first argument. They can be customized by adding methods for subclasses of `<generic>`; see the section on customizing the object system in the tutorial. The protocol cannot be applied to itself: if the generic function that is passed as an argument is one of the four generic functions listed in `generic-invocation-generics`, only the default method is used, no matter which other methods were added.

**compute-apply-generic** <span style="float:right;text-align:rigth;">[generic procedure]</span>  
**(compute-apply-generic _generic_)**  

Returns the procedure that implements calls of _generic_. This procedure is called with all arguments passed to the generic function. The default method returns a procedure that calls `compute-methods` to get the applicable methods and `compute-apply-methods` to run them. `add-method` calls `compute-apply-generic` every time a method gets added to a generic function, and caches the result.

**compute-methods** <span style="float:right;text-align:rigth;">[generic procedure]</span>  
**(compute-methods _generic_)**  

Returns a procedure with one parameter, the list of arguments of a call of _generic_. The procedure returns the list of the applicable methods of _generic_ sorted by specificity, starting with the most specific method. The default method filters the methods of _generic_ for the applicable ones, and sorts them with the predicate returned by `compute-method-more-specific?`.

**compute-method-more-specific?** <span style="float:right;text-align:rigth;">[generic procedure]</span>  
**(compute-method-more-specific? _generic_)**  

Returns a procedure `(lambda (method1 method2 args) ...)` that returns `#t` if _method1_ is more specific than _method2_ for the list of arguments _args_. The default method compares the specializers from left to right; for the first one that differs, it checks which one comes first in the class precedence list of the class of the corresponding argument. It is an error if two methods have equal specializers or specializer lists of different length.

**compute-apply-methods** <span style="float:right;text-align:rigth;">[generic procedure]</span>  
**(compute-apply-methods _generic_)**  

Returns a procedure `(lambda (methods args) ...)` that applies the first of the _methods_ (as returned by `compute-methods`) to _args_ and returns its result. The procedure of the method is called with a `call-next-method` procedure as its first argument, which calls the next method in the list of _methods_. If _methods_ is empty, or if the last method calls `call-next-method`, the error `no applicable methods/next methods` is signaled.

**generic-invocation-generics** <span style="float:right;text-align:rigth;">[constant]</span>  

List consisting of the four generic functions of the generic invocation protocol: `compute-apply-generic`, `compute-methods`, `compute-method-more-specific?`, and `compute-apply-methods`.

### Predefined classes

**&lt;top&gt;** <span style="float:right;text-align:rigth;">[class]</span>  

The root of the class hierarchy. All other classes, including the built-in ones, are subclasses of `<top>`. Methods specialized for `<top>` are applicable to all objects that have a class.

**&lt;object&gt;** <span style="float:right;text-align:rigth;">[class]</span>  

The root of all user-defined classes. It is the default superclass for classes created by `make-class`, and a direct subclass of `<top>`. Instances of `<object>` have no slots.

**&lt;class&gt;** <span style="float:right;text-align:rigth;">[class]</span>  

The class of all classes, including itself. Its instances are classes. Instantiating `<class>` or one of its subclasses creates a new class: `(make <class> 'name ... 'direct-superclasses ... 'direct-slots ...)`. The slots of `<class>` are `name`, `direct-superclasses`, `direct-slots`, `cpl` (the class precedence list), `slots`, `nfields`, `field-initializers` and `accessors`. They should be accessed via the procedures of this library, such as `class-name` or `class-slots`.

**&lt;procedure-class&gt;** <span style="float:right;text-align:rigth;">[class]</span>  

Subclass of `<class>`. The class of all classes whose instances are procedures, including the built-in class `<procedure>`.

**&lt;entity-class&gt;** <span style="float:right;text-align:rigth;">[class]</span>  

Subclass of `<procedure-class>`. The class of all classes whose instances are applicable objects that carry slots and a class: so-called _entities_. `<generic>` is an instance of `<entity-class>`. A subclass of `<generic>` has to be created as an instance of `<entity-class>`; see the tutorial for an example.

**&lt;generic&gt;** <span style="float:right;text-align:rigth;">[class]</span>  

The class of generic functions. It is an instance of `<entity-class>` and a subclass of `<object>`. The slots of `<generic>` are `name` and `methods`. Generic functions are created with `make-generic`.

**&lt;method&gt;** <span style="float:right;text-align:rigth;">[class]</span>  

The class of methods. The slots of `<method>` are `specializers` and `procedure`. Methods are created with `make-method`.

**&lt;pair&gt;**, **&lt;null&gt;**, **&lt;boolean&gt;**, **&lt;symbol&gt;**, **&lt;number&gt;**, **&lt;char&gt;**, **&lt;string&gt;**, **&lt;vector&gt;**, **&lt;bytevector&gt;**, **&lt;hashtable&gt;**, **&lt;procedure&gt;**, **&lt;input-port&gt;**, **&lt;output-port&gt;** <span style="float:right;text-align:rigth;">[class]</span>  

The built-in classes for the corresponding Scheme data types. They are direct subclasses of `<top>` and have no slots. `<procedure>` is an instance of `<procedure-class>`, the other classes are instances of a class that is not exported. Objects such as instances of user-defined classes, generic functions and classes are never members of `<procedure>`, even though they are implemented via procedures.

### Support library

Library `(lispkit clos support)` provides utility procedures which are used by the implementation of `(lispkit clos)`. Some of them are useful when writing code that uses it. Since procedures `last` and `every` have a slightly different semantics than procedures of the same name in other libraries, it is best to import only the procedures that are needed, as in `(import (only (lispkit clos support) getl))`.

**(getl _args name_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(getl _args name default_)**  

Looks up _name_ in the list of alternating names and values _args_ and returns the value following the first occurrence of _name_. Names are compared with `eq?`. If _name_ is not found, _default_ is returned. If no _default_ is provided in this case, an error is signaled.

```scheme
(getl '(a 1 b 2) 'b)       ⇒  2
(getl '(a 1 b 2) 'c 0)     ⇒  0
(getl '(a 1 b 2) 'c)       ⇒  [error] could not find named argument c in argument list (a 1 b 2)
```

**(position-of _obj list_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the position of the first element of _list_ that is `eq?` to _obj_. It is an error if _obj_ is not in _list_.

**(last _list_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the last element of _list_, or `#f` if _list_ is empty.

**(every _test list ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _test_ returns true for all tuples of corresponding elements of the lists. The iteration stops at the end of the shortest list.

**(compute-std-cpl _class get-direct-superclasses_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Computes the class precedence list for _class_ by a topological sort over the graph defined by the procedure _get-direct-superclasses_, which returns the list of the direct superclasses of a class. This is used by the default method of `compute-class-precedence-list`.

## Pitfalls and notes

   - **Do not replace the built-in methods.** `add-method` replaces methods with identical specializers. Adding a method to `initialize` for `<object>`, `<class>`, `<generic>` or `<method>` therefore replaces a method that is required by the object system. The same holds for the methods of the other generic functions of the MOP that are specialized for `<class>` or `<generic>`, e.g. `allocate-instance` or `compute-slots`. Define a subclass instead (such as a metaclass) and add methods that are specialized for it.
   - **Always call `call-next-method` in `initialize`**, before initializing slots. Otherwise, the initialization of superclasses is skipped. The last method in the chain must not call it.
   - **A method replaces another one with the same specializers.** This is a feature, as it allows for redefining methods. But it also means that a method of a different "kind" (e.g. an `:around` method implemented via the MOP) with the same specializers as a regular method replaces the latter.
   - **Mixins come first.** List mixin classes before regular superclasses, so that their methods are called before the ones of the regular classes. If a method of a regular class doesn't call `call-next-method`, the methods of the classes following it in the class precedence list will not be reached.
   - **The leftmost arguments win.** If methods are ambiguous, the specializers are compared from left to right. This is the same rule as in CLOS.
   - **Objects without classes cannot be dispatched on.** Records, promises, the end-of-file object, and some other types of values do not have a class. Passing them to a generic function, or to `instance-of?`, results in an error message that mentions a missing slot in instances of `<boolean>`.
   - **Instances are printed as procedures.** Objects, classes and generic functions are all represented by tagged procedures. They are displayed as `#<procedure ...>`. Use `class-name` and `generic-name` for getting a readable label. Procedure `procedure?` returns `#t` for all of them, but calling an instance of a regular class is an error. Compare objects with `eq?`; classes contain circular structures and instances refer to their classes.
   - **Existing instances are not updated.** Classes cannot be redefined. Defining a new class with the same name creates an unrelated class, and existing instances keep referring to the old one.

## Further reading

   - The example programs `TinyClos.scm` and `ObjectOrientation.scm` of LispKit.
   - Gregor Kiczales, Jim des Rivières, Daniel G. Bobrow: _The Art of the Metaobject Protocol_. MIT Press, 1991. This book explains the metaobject protocol of CLOS, which was the model for Tiny CLOS.
   - Sonya E. Keene: _Object-Oriented Programming in Common Lisp: A Programmer's Guide to CLOS_. Addison-Wesley, 1989. An introduction to the programming style of CLOS.
   - Stephen Bloch: [Tiny CLOS Tutorial](https://home.adelphi.edu/sbloch/class/archive/272/spring1997/tclos/tutorial.html). A short introduction to Tiny CLOS, including a larger example.
   - [The Tiny CLOS sources](https://gitea.scheme.org/conservatory/tinyclos) in the Scheme Conservatory, including the original examples.

Tiny CLOS is copyright © 1992 Xerox Corporation. The adaptation to LispKit is copyright © 2018 Matthias Zenger.
