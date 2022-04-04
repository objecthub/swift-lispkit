;;; LISPKIT CLOS
;;;
;;; This is a slightly extended R7RS-compatible version of Tiny CLOS. CLOS is an
;;; acronym for "Common Lisp Object System". It is a standard component of Common Lisp
;;; providing abstractions for object-oriented programming. Tiny CLOS is a Scheme
;;; version of CLOS written in 1992 by Gregor Kiczales. It differs from CLOS in terms
;;; of the API, but the object-oriented abstractions are equivalent to CLOS.
;;;
;;; A very simple CLOS-like language, embedded in Scheme, with a simple MOP. The
;;; features of the default base language are:
;;;
;;;   * Classes, with instance slots, but no slot options.
;;;   * Multiple-inheritance.
;;;   * Generic functions with multi-methods and class specializers only.
;;;   * Primary methods and call-next-method; no other method combination.
;;;   * Uses Scheme's lexical scoping facilities as the class and generic
;;;     function naming mechanism.  Another way of saying this is that
;;;     class, generic function and methods are first-class (meta)objects.
;;;
;;; While the MOP is simple, it is essentially equal in power to both MOPs in AMOP. This
;;; implementation is not at all optimized, but the MOP is designed so that it can be
;;; optimized. In fact, this MOP allows better optimization of slot access extenstions
;;; than those in AMOP.
;;;
;;; In addition to calling a generic, the entry points to the default base language are:
;;;
;;;   (MAKE-CLASS list-of-superclasses list-of-slot-names)
;;;   (MAKE-GENERIC)
;;;   (MAKE-METHOD list-of-specializers procedure)
;;;   (ADD-METHOD generic method)
;;;
;;;   (MAKE class . initargs)
;;;   (INITIALIZE instance initargs)            ;Add methods to this,
;;;                                             ;don't call it directly.
;;;
;;;   (SLOT-REF  object slot-name)
;;;   (SLOT-SET! object slot-name new-value)
;;;
;;; So, for example, one might do:
;;;
;;;   (define <position> (make-class (list <object>) (list 'x 'y)))
;;;   (add-method initialize
;;;       (make-method (list <position>)
;;;         (lambda (call-next-method pos initargs)
;;;           (for-each (lambda (initarg-name slot-name)
;;;                       (slot-set! pos
;;;                                  slot-name
;;;                                  (getl initargs initarg-name 0)))
;;;                     '(x y)
;;;                     '(x y)))))
;;;
;;;   (set! p1 (make <position> 'x 1 'y 3))
;;;
;;; NOTE!  Do not use EQUAL? to compare objects!  Use EQ? or some hand
;;;        written procedure.  Objects have a pointer to their class,
;;;        and classes are circular structures, and ...
;;;
;;; The introspective part of the MOP looks like the following.  Note that
;;; these are ordinary procedures, not generics.
;;;
;;;   CLASS-OF
;;;
;;;   CLASS-DIRECT-SUPERCLASSES
;;;   CLASS-DIRECT-SLOTS
;;;   CLASS-PRECEDENCE-LIST
;;;   CLASS-SLOTS
;;;
;;;   GENERIC-METHODS
;;;
;;;   METHOD-SPECIALIZERS
;;;   METHOD-PROCEDURE
;;; 
;;; The intercessory protocol looks like (generics in uppercase):
;;;
;;;   make
;;;     ALLOCATE-INSTANCE
;;;     INITIALIZE                   (really a base-level generic)
;;;
;;;   class initialization
;;;     COMPUTE-CLASS-PRECEDENCE-LIST
;;;     COMPUTE-SLOTS
;;;     COMPUTE-ACCESSORS
;;;
;;;   add-method                     (Notice this is not a generic!)
;;;     COMPUTE-APPLY-GENERIC
;;;       COMPUTE-METHODS
;;;         COMPUTE-METHOD-MORE-SPECIFIC?
;;;       COMPUTE-APPLY-METHODS
;;; 
;;; OK, now let's get going.  But, as usual, before we can do anything
;;; interesting, we have to muck around for a bit first.  First, we need
;;; to load the support library.
;;; 
;;; Then, we need to build what, in a more real implementation, would be
;;; the interface to the memory subsystem: instances and entities.  The
;;; former are used for instances of instances of <class>; the latter
;;; are used for instances of instances of <entity-class>.  In this MOP,
;;; none of this is visible to base- or MOP-level programmers.
;;;
;;; A few things to note, that have influenced the way all this is done:
;;;
;;;   - R4RS doesn't provide a mechanism for specializing the
;;;     behavior of the printer for certain objects.
;;;
;;;   - Some Scheme implementations bomb when printing circular
;;;     structures -- that is, arrays and/or lists that somehow
;;;     point back to themselves.
;;;
;;; So, the natural implementation of instances -- vectors whose first
;;; field point to the class -- is straight on out.  Instead, we use a
;;; procedure to `encapsulate' that natural representation.
;;;
;;; Having gone that far, it makes things simpler to unify the way normal
;;; instances and entities are handled, at least in the lower levels of
;;; the system.  Don't get faked out by this -- the user shouldn't think
;;; of normal instances as being procedures, they aren't. (At least not
;;; in this language.)  If you are using this to teach, you probably want
;;; to hide the implementation of instances and entities from people.
;;;
;;;
;;; EDIT HISTORY:
;;;
;;;      10/**/92  Gregor  Originally Written
;;; 1.0  11/10/92  Gregor  Changed names of generic invocation generics.
;;;                        Changed compute-getters-and-setters protocol.
;;;                        Made comments match the code.
;;;                        Changed maximum line width to 72.
;;; 1.1  11/24/92  Gregor  Fixed bug in compute-method-more-specific?,
;;;                        wrt the use of for-each.
;;;                        Both methods on allocate instance failed to
;;;                        initialize fields properly.
;;;                        The specializers and procedure initargs are
;;;                        now required when creating a method, that is,
;;;                        they no longer default.  No working program
;;;                        should notice this change.
;;; 1.2  12/02/92  Gregor  Fix minor things that improve portability:
;;;                         - DEFINE needs 2 args in R4Rs
;;;                         - Conditionalize printer hooks.
;;;                         - () doesn't evaluate to ()
;;;
;;; 1.3  12/08/92  Gregor  More minor things:
;;;                         - () really doesn't evaluate to () damnit!
;;;                         - It turns out DEFINE-MACRO is never used.
;;;                         - Confusion over the "failure" return value
;;;                           of ASSQ -- ASSQ returns #f if the key is
;;;                           not found.
;;;                         - SEQUENCE   --> BEGIN
;;;                         - LAST-PAIR  --> last now in support
;;;                        Change instance rep to protect Schemes that
;;;                        don't detect circular structures when
;;;                        printing.
;;;                        A more reasonable error message when there
;;;                        are no applicable methods or next methods.
;;; 1.4  12/10/92  Gregor  Flush filter-in for collect-if.  Add news
;;;                        classes <input-port> and <output-port>.
;;;                        Also add
;;;
;;; 1.5  12/17/92  Gregor  Minor changes to class of and primitive
;;;                        classes to try and deal with '() and #f
;;;                        better.
;;;
;;; 1.6   9/9/93   Gregor  Fix a monstrous bug in the bootstrap of
;;;                        compute-apply-generic which sometimes ran
;;;                        user methods on this generic function when
;;;                        it shouldn't.
;;;
;;; 1.7   8/9/94   Gregor  Add Scheme 48 to support.scm.
;;;
;;; **********************************************************************
;;; Copyright (c) 1992 Xerox Corporation.
;;; All Rights Reserved.
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
;;; **********************************************************************
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2018 Matthias Zenger. All rights reserved.

(define-library (lispkit clos)

  (export instance?
          instance-of?
          class-of
          make
          slot-ref
          slot-set!
          slot-bound?
          slot-accessors
          slot-accessor-procedures
          class?
          subclass?
          class-name
          class-direct-slots
          class-direct-superclasses
          class-slots
          class-precedence-list
          generic?
          generic-name
          generic-methods
          method?
          method-specializers
          method-procedure
          make-class
          make-generic
          make-method)
  
  (export initialize
          allocate-instance
          compute-accessors
          compute-class-precedence-list
          compute-slots
          compute-apply-generic
          compute-methods
          compute-method-more-specific?
          compute-apply-methods
          generic-invocation-generics
          compute-apply-generic
          compute-methods
          compute-method-more-specific?
          compute-apply-methods
          add-method)

  (export <top>
          <class>
          <object>
          <procedure-class>
          <entity-class>
          <generic>
          <method>
          <pair>
          <null>
          <symbol>
          <boolean>
          <procedure>
          <number>
          <vector>
          <char>
          <string>
          <bytevector>
          <hashtable>
          <input-port>
          <output-port>)

  (import (lispkit base)
          (lispkit clos support))

  (begin
    
    (define (get-vector instance)
      (and (procedure/tag? instance) (procedure-tag instance)))
    
    (define (%set-instance-proc! closure proc)
      (let ((vector (get-vector closure)))
        (if (vector-ref vector 1)
            (error "cannot set procedure of $0 to $1" closure proc)
            (vector-set! vector 0 proc))))
    
    (define (%instance-ref closure index)
      (cond ((get-vector closure) => (lambda (vec) (vector-ref vec (+ index 3))))
            (else (error "cannot refer to index $1 of $0" closure index))))
    
    (define (%instance-set! closure index new-value)
      (cond ((get-vector closure) => (lambda (vec) (vector-set! vec (+ index 3) new-value)))
            (else (error "cannot set field of $0 at index $1 to $2" closure index new-value))))
    
    (define (%allocate-instance-internal class lock proc nfields)
      (let* ((vector (make-vector (+ nfields 3) #f))
             (instance (lambda/tag vector args (apply (vector-ref vector 0) args))))
        (vector-set! vector 0 proc)
        (vector-set! vector 1 lock)
        (vector-set! vector 2 class)
        instance))
    
    (define (%allocate-instance class nfields)
      (%allocate-instance-internal
        class
        #t
        (lambda args (error "instance is not a procedure; cannot apply it"))
        nfields))

    (define (%allocate-entity class nfields)
      (%allocate-instance-internal
        class
        #f
        (lambda args (error "tried to call an entity before its procedure is set"))
        nfields))
    
    ; %allocate-instance, %allocate-entity, %instance-ref, %instance-set!
    ; and class-of are the normal interface, from the rest of the code, to
    ; the low-level memory system.  One thing to take note of is that the
    ; protocol does not allow the user to add low-level instance
    ; representations.  I have never seen a way to make that work.
    ;
    ; Note that this implementation of class-of assumes the primitive classes are
    ; set up later.

    (define (class-of x)
      (cond ((get-vector x) => (lambda (vec) (vector-ref vec 2)))
            ((pair? x)        <pair>)
            ((null? x)        <null>)
            ((boolean? x)     <boolean>)
            ((symbol? x)      <symbol>)
            ((procedure? x)   <procedure>)
            ((number? x)      <number>)
            ((vector? x)      <vector>)
            ((char? x)        <char>)
            ((string? x)      <string>)
            ((bytevector? x)  <bytevector>)
            ((hashtable? x)   <hashtable>)
            ((input-port? x)  <input-port>)
            ((output-port? x) <output-port>)))

    ; Now we can get down to business.  First, we initialize the braid.
    ; For Bootstrapping, we define an early version of MAKE.  It will be
    ; changed to the real version later on.  String search for ``set! make''.

    (define (make class . initargs)
      (cond
        ((or (eq? class <class>) (eq? class <entity-class>))
   	      (let* ((new (%allocate-instance class (length slots-of-a-class)))
   	             (name (getl initargs 'name #f))
   	             (dsupers (getl initargs 'direct-superclasses '()))
   	             (dslots (map list (getl initargs 'direct-slots '())))
   	             (cpl (let loop ((sups dsupers)
   	                             (so-far (list new)))
   	                    (if (null? sups)
   	                        (reverse so-far)
   	                        (loop (class-direct-superclasses (car sups))
   	                              (cons (car sups) so-far)))))
   	             (slots (apply append (cons dslots (map class-direct-slots (cdr cpl)))))
   	             (nfields 0)
   	             (field-initializers '())
   	             (allocator (lambda (init)
   	                          (let ((f nfields))
   	                            (set! nfields (+ nfields 1))
   	                            (set! field-initializers (cons init field-initializers))
   	                            (list (lambda (o) (%instance-ref o f))
   	                                  (lambda (o n) (%instance-set! o f n))))))
   	             (accessors
   	               (map (lambda (s) (cons (car s) (allocator (lambda () #f)))) slots)))
   	        (slot-set! new 'name               name)
   	        (slot-set! new 'direct-superclasses      dsupers)
   	        (slot-set! new 'direct-slots       dslots)
   	        (slot-set! new 'cpl                cpl)
   	        (slot-set! new 'slots              slots)
   	        (slot-set! new 'nfields            nfields)
   	        (slot-set! new 'field-initializers (reverse field-initializers))
   	        (slot-set! new 'accessors  accessors)
   	        new))
        ((eq? class <generic>)
          (let ((new (%allocate-entity class (length (class-slots class))))
                (name (getl initargs 'name #f)))
            (slot-set! new 'name name)
            (slot-set! new 'methods '())
    	      new))
        ((eq? class <method>)
          (let ((new (%allocate-instance class (length (class-slots class)))))
            (slot-set! new 'specializers (getl initargs 'specializers))
            (slot-set! new 'procedure (getl initargs 'procedure))
            new))))

    ; These are the real versions of slot-ref and slot-set!.  Because of the
    ; way the new slot access protocol works, with no generic call in line,
    ; they can be defined up front like this. Cool eh?
        
    (define (slot-ref object slot-name)
      ((car (slot-accessors (class-of object) slot-name)) object))
    
    (define (slot-set! object slot-name new-value)
      ((cadr (slot-accessors (class-of object) slot-name)) object new-value))

    (define (slot-bound? object slot-name)
      (let ((class (class-of object)))
        (and (assq slot-name (if (eq? class <class>)
                                 accessors-for-class
                                 (slot-ref class 'accessors)))
             #t)))
    
    (define (slot-accessor-procedures class slot-name)
      (apply values (slot-accessors class slot-name)))

    (define (slot-accessors class slot-name)
      (cdr (or (assq slot-name (if (eq? class <class>)
                                   accessors-for-class
                                   (slot-ref class 'accessors)))
               (error "no slot `$0` in instances of $1" slot-name (class-name class)))))

    ; Given that the early version of MAKE is allowed to call accessors on
    ; class metaobjects, the definitions for them come here, before the
    ; actual class definitions, which are coming up right afterwards.

    (define (class-name class)
      (slot-ref class 'name))

    (define (class-direct-slots class)
      (slot-ref class 'direct-slots))
    
    (define (class-direct-superclasses class)
      (slot-ref class 'direct-superclasses))
    
    (define (class-slots class)
      (slot-ref class 'slots))
    
    (define (class-precedence-list class)
      (slot-ref class 'cpl))
    
    (define (instance-of? x class)
      (and (memq class (class-precedence-list (class-of x))) #t))

    (define (instance? obj)
      (and (class-of obj) #t))

    (define (class? obj)
      (instance-of? obj <class>))
    
    (define (subclass? class super)
      (and (memq super (class-precedence-list class)) #t))
    
    (define (generic? obj)
      (instance-of? obj <generic>))

    (define (generic-name generic)
      (slot-ref generic 'name))

    (define (generic-methods generic)
      (slot-ref generic 'methods))

    (define (method? obj)
      (instance-of? obj <method>))

    (define (method-specializers method)
      (slot-ref method 'specializers))
    
    (define (method-procedure method)
      (slot-ref method 'procedure))

    ; The next 7 clusters define the 6 initial classes.  It takes 7 to 6
    ; because the first and fourth both contribute to <class>.

    (define slots-of-a-class
      '(name                       ; symbol
        direct-superclasses              ; (class ...)
        direct-slots               ; ((name . options) ...)
        cpl                        ; (class ...)
        slots                      ; ((name . options) ...)
        nfields                    ; an integer
        field-initializers         ; (proc ...)
        accessors))        ; ((slot-name getter setter) ...)

    (define accessors-for-class   ; see slot-accessors
      (let ((make-em (lambda (s f)
                       (list s
                             (lambda (o) (%instance-ref  o f))
                             (lambda (o n) (%instance-set! o f n))))))
        (map (lambda (s) (make-em s (position-of s slots-of-a-class)))
    	       slots-of-a-class)))

    (define <class>
      (let ((class (%allocate-instance #f (length slots-of-a-class))))
        (cond ((get-vector class) => (lambda (vec) (vector-set! vec 2 class)))
              (else (error "cannot set instance class of non-instance" class)))
        class))
    
    (define <top> (make <class>
                        'name '<top>
                        'direct-superclasses (list)
                        'direct-slots (list)))

    (define <object> (make <class>
                           'name '<object>
                           'direct-superclasses (list <top>)
                           'direct-slots (list)))
    
    ; This cluster, together with the first cluster above that defines
    ; <class> and sets its class, have the effect of:
    ;
    ;   (define <class>
    ;     (make <class>
    ;           'name          <sym>
    ;           'direct-superclasses (list <object>)
    ;           'direct-slots  (list 'direct-superclasses ...)))

    (slot-set! <class> 'name                '<class>)
    (slot-set! <class> 'direct-superclasses (list <object>))
    (slot-set! <class> 'direct-slots        (map list slots-of-a-class))
    (slot-set! <class> 'cpl                 (list <class> <object> <top>))
    (slot-set! <class> 'slots               (map list slots-of-a-class))
    (slot-set! <class> 'nfields             (length slots-of-a-class))
    (slot-set! <class> 'field-initializers  (map (lambda (s) (lambda () '())) slots-of-a-class))
    (slot-set! <class> 'accessors   '())
    
    (define <procedure-class> (make <class>
                                    'name '<procedure-class>
                                    'direct-superclasses (list <class>)
                                    'direct-slots (list)))

    (define <entity-class> (make <class>
                                 'name '<entity-class>
                                 'direct-superclasses (list <procedure-class>)
                                 'direct-slots  (list)))

    (define <generic> (make <entity-class>
                            'name '<generic>
                            'direct-superclasses (list <object>)
                            'direct-slots  (list 'name 'methods)))

    (define <method> (make <class>
                           'name '<method>
                           'direct-superclasses (list <object>)
                           'direct-slots  (list 'specializers 'procedure)))

    ; These are the convenient syntax we expose to the base-level user.

    (define make-class
      (case-lambda
        ((fst)
          (if (pair? fst)
              (make-class #f (list <object>) fst)
              (make-class fst (list <object>) (list))))
        ((fst snd)
          (if (pair? fst)
              (make-class #f fst snd)
              (make-class fst (list <object>) snd)))
        ((name direct-superclasses direct-slots)
          (make <class>
                'name name
                'direct-superclasses direct-superclasses
                'direct-slots  direct-slots))))
    
    (define make-generic
      (case-lambda
        (()
          (make-generic #f))
        ((name)
          (make <generic>
                'name name
                'methods '()))))
    
    (define (make-method specializers procedure)
      (make <method>
            'specializers specializers
            'procedure procedure))

    ; The initialization protocol

    (define initialize (make-generic 'initialize))

    ; The instance structure protocol.

    (define allocate-instance (make-generic 'allocate-instance))
    (define compute-accessors (make-generic 'compute-accessors))

    ; The class initialization protocol.

    (define compute-class-precedence-list   (make-generic 'compute-class-precedence-list))
    (define compute-slots (make-generic 'compute-slots))

    ; The generic invocation protocol.

    (define compute-apply-generic         (make-generic 'compute-apply-generic))
    (define compute-methods               (make-generic 'compute-methods))
    (define compute-method-more-specific? (make-generic 'compute-method-more-specific?))
    (define compute-apply-methods         (make-generic 'compute-apply-methods))

    ; The next thing to do is bootstrap generic functions.

    (define generic-invocation-generics
      (list compute-apply-generic
            compute-methods
            compute-method-more-specific?
            compute-apply-methods))

    (define (add-method generic method)
      (slot-set! generic
                 'methods
                 (cons method
                       (filter (lambda (m)
                                 (not (every eq? (method-specializers m)
                                                 (method-specializers method))))
            			             (slot-ref generic 'methods))))
      (%set-instance-proc! generic (compute-apply-generic generic)))

    ; Adding a method calls COMPUTE-APPLY-GENERIC, the result of which calls the other
    ; generics in the generic invocation protocol.  Two, related, problems come up.
    ; A chicken and egg problem and a infinite regress problem.
    ;
    ; In order to add our first method to COMPUTE-APPLY-GENERIC, we need something
    ; sitting there, so it can be called.  The first definition below does that.
    ;
    ; Then, the second definition solves both the infinite regress and the not having
    ; enough of the protocol around to build itself problem the same way: it special
    ; cases invocation of generics in the invocation protocol.

    (%set-instance-proc! compute-apply-generic
      (lambda (generic)
        (let ((method (car (generic-methods generic))))
          ((method-procedure method) #f generic))))
    
    (add-method compute-apply-generic
      (make-method
        (list <generic>)
        (lambda (call-next-method generic)
          (lambda args
            (if (and (memq generic generic-invocation-generics)        ;* G  c
                     (memq (car args) generic-invocation-generics))    ;* r  a
                (apply (method-procedure                               ;* o  s
                         (last (generic-methods generic)))             ;* u  e
                         (cons #f args))                               ;* n
      	                                                               ;* d
                ((compute-apply-methods generic)
                  ((compute-methods generic) args) args))))))

    (add-method compute-methods
      (make-method
        (list <generic>)
        (lambda (call-next-method generic)
          (lambda (args)
            (sort (lambda (m1 m2) ((compute-method-more-specific? generic) m1 m2 args))
                  (filter (lambda (method)
                            (let loop ((specs (method-specializers method))
                                       (args args))
                              (cond ((null? args) (null? specs))
                                    ((null? specs) #t)
                                    ((applicable? (car specs) (car args))
                                       (loop (cdr specs) (cdr args)))
                                    (else #f))))
                          (generic-methods generic)))))))
    
    (add-method compute-method-more-specific?
      (make-method
        (list <generic>)
        (lambda (call-next-method generic)
          (lambda (m1 m2 args)
            (let loop ((specls1 (method-specializers m1))
                       (specls2 (method-specializers m2))
                       (args args))
              (cond ((and (null? specls1) (null? specls2))
                      (error "two methods are equally specific"))
                    ((or  (null? specls1) (null? specls2))
                      (error "two methods have a different number of specializers"))
                    ((null? args)
                      (error "fewer arguments than specializers"))
                    (else
                      (let ((c1  (car specls1))
                            (c2  (car specls2))
                            (arg (car args)))
                        (if (eq? c1 c2)
                            (loop (cdr specls1) (cdr specls2) (cdr args))
                            (more-specific? c1 c2 arg))))))))))
    
    (add-method compute-apply-methods
      (make-method
        (list <generic>)
        (lambda (call-next-method generic)
          (lambda (methods args)
            (letrec ((one-step (lambda (tail)
                                 (lambda ()
                                   (if (null? tail)
                                       (error "no applicable methods/next methods")
                                       (apply (method-procedure (car tail))
                                              (cons (one-step (cdr tail)) args)))))))
              ((one-step methods)))))))
    
    (define (applicable? c arg)
      (memq c (class-precedence-list (class-of arg))))

    (define (more-specific? c1 c2 arg)
      (memq c2 (memq c1 (class-precedence-list (class-of arg)))))

    (add-method initialize
      (make-method
        (list <object>)
        (lambda (call-next-method object initargs) object)))

    (add-method initialize
      (make-method
        (list <class>)
        (lambda (call-next-method class initargs)
          (call-next-method)
          (slot-set! class 'name (getl initargs 'name #f))
          (slot-set! class 'direct-superclasses (getl initargs 'direct-superclasses '()))
          (slot-set! class 'direct-slots
                     (map (lambda (s) (if (pair? s) s (list s)))
                          (getl initargs 'direct-slots '())))
          (slot-set! class 'cpl (compute-class-precedence-list class))
          (slot-set! class 'slots (compute-slots class))
          (let* ((nfields 0)
                 (field-initializers '())
                 (allocator (lambda (init)
                              (let ((f nfields))
                                (set! nfields (+ nfields 1))
                                (set! field-initializers (cons init field-initializers))
                                (list (lambda (o) (%instance-ref o f))
                                      (lambda (o n) (%instance-set! o f n))))))
                 (accessors (map (lambda (slot)
                                           (cons (car slot)
                                                 (compute-accessors class slot allocator)))
                                         (slot-ref class 'slots))))
            (slot-set! class 'nfields nfields)
            (slot-set! class 'field-initializers field-initializers)
            (slot-set! class 'accessors accessors)))))

    (add-method initialize
      (make-method
        (list <generic>)
        (lambda (call-next-method generic initargs)
          (call-next-method)
          (slot-set! generic 'name (getl initargs 'name #f))
          (slot-set! generic 'methods '())
          (%set-instance-proc! generic (lambda args (error "has no methods"))))))

    (add-method initialize
      (make-method
        (list <method>)
        (lambda (call-next-method method initargs)
          (call-next-method)
          (slot-set! method 'specializers (getl initargs 'specializers))
          (slot-set! method 'procedure    (getl initargs 'procedure)))))

    (add-method allocate-instance
      (make-method
        (list <class>)
        (lambda (call-next-method class)
          (let* ((field-initializers (slot-ref class 'field-initializers))
                 (new (%allocate-instance class (length field-initializers))))
            (let loop ((n 0)
                       (inits field-initializers))
              (if (pair? inits)
                  (begin (%instance-set! new n ((car inits)))
                         (loop (+ n 1) (cdr inits)))
                  new))))))
  
    (add-method allocate-instance
      (make-method
        (list <entity-class>)
        (lambda (call-next-method class)
          (let* ((field-initializers (slot-ref class 'field-initializers))
                 (new (%allocate-entity class (length field-initializers))))
            (let loop ((n 0) (inits field-initializers))
              (if (pair? inits)
                  (begin (%instance-set! new n ((car inits)))
                         (loop (+ n 1) (cdr inits)))
                  new))))))

    (add-method compute-class-precedence-list
      (make-method
        (list <class>)
        (lambda (call-next-method class) (compute-std-cpl class class-direct-superclasses))))

    (add-method compute-slots
      (make-method
        (list <class>)
        (lambda (call-next-method class)
          (let collect ((to-process (apply append (map class-direct-slots
                                                       (class-precedence-list class))))
                        (result '()))
            (if (null? to-process)
                (reverse result)
                (let* ((current (car to-process))
                       (name (car current))
                       (others '())
                       (remaining-to-process
                         (filter (lambda (o)
                                   (if (eq? (car o) name)
                                       (begin (set! others (cons o others)) #f)
                                       #t))
                                 (cdr to-process))))
                  (collect remaining-to-process
                           (cons (append current (apply append (map cdr others))) result))))))))

    (add-method compute-accessors
      (make-method
        (list <class>)
        (lambda (call-next-method class slot allocator) (allocator (lambda () #f)))))

    ; Now everything works, both generic functions and classes, so we can
    ; turn on the real MAKE.

    (set! make (lambda (class . initargs)
                 (let ((instance (allocate-instance class)))
                   (initialize instance initargs)
                   instance)))

    ; Now define what CLOS calls `built in' classes.

    (define <primitive-class> (make <class>
                                    'name '<primitive-class>
                                    'direct-superclasses (list <class>)
                                    'direct-slots (list)))

    (define (make-primitive-class name . class)
      (make (if (null? class) <primitive-class> (car class))
            'name name
            'direct-superclasses (list <top>)
            'direct-slots (list)))

    (define <pair>        (make-primitive-class '<pair>))
    (define <null>        (make-primitive-class '<null>))
    (define <symbol>      (make-primitive-class '<symbol>))
    (define <boolean>     (make-primitive-class '<boolean>))
    (define <procedure>   (make-primitive-class '<procedure> <procedure-class>))
    (define <number>      (make-primitive-class '<number>))
    (define <vector>      (make-primitive-class '<vector>))
    (define <char>        (make-primitive-class '<char>))
    (define <string>      (make-primitive-class '<string>))
    (define <bytevector>  (make-primitive-class '<bytevector>))
    (define <hashtable>   (make-primitive-class '<hashtable>))
    (define <input-port>  (make-primitive-class '<input-port>))
    (define <output-port> (make-primitive-class '<output-port>))
  )
)
