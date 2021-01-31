;;; LISPKIT PROLOG
;;;
;;; Library `(lispkit prolog)` provides an embedding of Prolog into Scheme. The implementation
;;; of this library corresponds to the R6RS version of _Schelog_ by Dorai Sitaram which has
;;; been ported to R7RS by Peter Lane.
;;; 
;;; The following document provides an introduction and overview of Shelog:
;;; http://ds26gte.github.io/schelog
;;; Example code can be found here:
;;; https://github.com/ds26gte/schelog/tree/master/examples
;;; 
;;; R7RS port by Peter Lane in 2017
;;; 
;;; Copyright (c) 1993-2015, Dorai Sitaram.
;;; All rights reserved.
;;; 
;;; Permission to distribute and use this work for any
;;; purpose is hereby granted provided this copyright
;;; notice is included in the copy.  This work is provided
;;; as is, with no warranty of any kind.
;;; 
;;; Adaptation to LispKit
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(define-library (lispkit prolog)
  
  (export %true
          %fail
          %which
          %rel !
          %more
          %let
          %or
          %and
          %is
          %=
          %=:=
          %>
          %>=
          %<
          %<=
          %=/=
          %var
          %nonvar
          %/=
          %==
          %/==
          %freeze
          %melt
          %melt-new
          %copy
          %free-vars
          %not
          %assert
          %assert-a
          %bag-of
          %bag-of-1
          %set-of
          %set-of-1
          %member
          %if-then-else
          %constant
          %compound
          %empty-rel
          %append
          %repeat)
  
  (import (lispkit base))
  
  (begin
    
    ; logic variables and their manipulation
    (define ! (make-parameter #f)) ; for R6RS
    
    (define *ref* "ref")
    
    (define *unbound* '_)
    
    (define make-ref
      ; makes a fresh unbound refs; unbound refs point to themselves
      (lambda opt
        (vector *ref*
                (if (null? opt) *unbound*
                    (car opt)))))
    
    (define ref?
      (lambda (r)
        (and (vector? r)
             (eq? (vector-ref r 0) *ref*))))
    
    (define deref
      (lambda (r)
        (vector-ref r 1)))
    
    (define set-ref!
      (lambda (r v)
        (vector-set! r 1 v)))
    
    (define unbound-ref?
      (lambda (r)
        (eq? (deref r) *unbound*)))
    
    (define unbind-ref!
      (lambda (r)
        (set-ref! r *unbound*)))
    
    ; frozen logic vars
    
    (define schelog:*frozen* "frozen")
    
    (define schelog:freeze-ref
      (lambda (r)
        (make-ref (vector schelog:*frozen* r))))
    
    (define schelog:thaw-frozen-ref
      (lambda (r)
        (vector-ref (deref r) 1)))
    
    (define schelog:frozen-ref?
      (lambda (r)
        (let ((r2 (deref r)))
          (and (vector? r2)
               (eq? (vector-ref r2 0) schelog:*frozen*)))))
    
    ; deref a structure completely (except the frozen ones, i.e.)
    
    (define deref*
      (lambda (s)
        (cond ((ref? s)
                (if (schelog:frozen-ref? s) s
                    (deref* (deref s))))
              ((pair? s) (cons (deref* (car s))
                               (deref* (cdr s))))
              ((vector? s)
                (list->vector (map deref* (vector->list s))))
              (else s))))
    
    ; %let introduces new logic variables
    
    (define-syntax %let
      (syntax-rules ()
        ((%let (x ...) . e)
          (let ((x (make-ref)) ...)
            . e))))
    
    ; the unify predicate
    
    (define *schelog-use-occurs-check?* #f)
    
    (define schelog:occurs-in? 
      (lambda (var term)
        (and *schelog-use-occurs-check?*
             (let loop ((term term))
               (cond ((eqv? var term) #t)
                     ((ref? term)
                       (cond ((unbound-ref? term) #f)
                             ((schelog:frozen-ref? term) #f)
                             (else (loop (deref term)))))
                     ((pair? term)
                       (or (loop (car term)) (loop (cdr term))))
                     ((vector? term)
                       (loop (vector->list term)))
                     (else #f))))))
    
    (define schelog:unify
      (lambda (t1 t2)
        (lambda (fk)
          (letrec
            ((cleanup-n-fail
               (lambda (s)
                 (for-each unbind-ref! s)
                 (fk 'fail)))
             (unify1
               (lambda (t1 t2 s)
                 (cond ((eqv? t1 t2) s)
                       ((ref? t1)
                         (cond ((unbound-ref? t1)
                                 (cond ((schelog:occurs-in? t1 t2)
                                         (cleanup-n-fail s))
                                       (else 
                                         (set-ref! t1 t2)
                                         (cons t1 s))))
                               ((schelog:frozen-ref? t1)
                                 (cond ((ref? t2)
                                         (cond ((unbound-ref? t2)
                                                 (unify1 t2 t1 s))
                                               ((schelog:frozen-ref? t2)
                                                 (cleanup-n-fail s))
                                               (else
                                                 (unify1 t1 (deref t2) s))))
                                       (else (cleanup-n-fail s))))
                               (else 
                                 (unify1 (deref t1) t2 s))))
                       ((ref? t2) (unify1 t2 t1 s))
                       ((and (pair? t1) (pair? t2))
                         (unify1 (cdr t1) (cdr t2)
                                 (unify1 (car t1) (car t2) s)))
                       ((and (string? t1) (string? t2))
                         (if (string=? t1 t2) s
                             (cleanup-n-fail s)))
                       ((and (vector? t1) (vector? t2))
                         (unify1 (vector->list t1)
                                 (vector->list t2) s))
                       (else
                         (for-each unbind-ref! s)
                         (fk 'fail))))))
            (let ((s (unify1 t1 t2 '())))
              (lambda (d)
                (cleanup-n-fail s)))))))
    
    (define %= schelog:unify)
    
    ; disjunction
    
    (define-syntax %or
      (syntax-rules ()
        ((%or g ...)
          (lambda (__fk)
            (call-with-current-continuation
              (lambda (__sk)
                (call-with-current-continuation
                  (lambda (__fk)
                    (__sk ((deref* g) __fk))))
                ...
                (__fk 'fail)))))))
    
    
    ; conjunction
    
    (define-syntax %and
      (syntax-rules ()
        ((%and g ...)
          (lambda (__fk)
            (let* ((__fk ((deref* g) __fk))
                   ...)
              __fk)))))
    
    ; cut
    
    (define-syntax %cut-delimiter
      (syntax-rules ()
        ((%cut-delimiter ! g)
          (lambda (__fk)
            (! (lambda (__fk2) __fk))
            ((deref* g) __fk)))))
    
    ; Prolog-like sugar
    
    (define-syntax %rel
      (syntax-rules ()
        ((%rel (v ...) ((a ...) subgoal ...) ...)
          (lambda __fmls
            (lambda (__fk)
              (call-with-current-continuation
                (lambda (__sk)
                  (! (lambda (fk1) __fk))
                  (%let (v ...)
                        (call-with-current-continuation
                          (lambda (__fk)
                            (let* ((__fk ((%= __fmls (list a ...)) __fk))
                                   (__fk ((deref* subgoal) __fk))
                                   ...)
                              (__sk __fk))))
                        ...
                        (__fk 'fail)))))))))
    
    ; the fail and true preds
    
    (define %fail
      (lambda (fk) (fk 'fail)))
    
    (define %true
      (lambda (fk) fk))
    
    ; for structures ("functors"), use Scheme's list and vector
    ; functions and anything that's built using them.
    
    ; arithmetic
    
    (define-syntax %is
      (syntax-rules (quote)
        ((%is v e)
          (lambda (__fk)
            ((%= v (%is (1) e __fk)) __fk)))        
        ((%is (1) (quote x) fk) (quote x))
        ((%is (1) (x ...) fk)
          ((%is (1) x fk) ...))
        ((%is (1) x fk)
          (if (and (ref? x) (unbound-ref? x))
              (fk 'fail) (deref* x)))))
    
    ; defining arithmetic comparison operators
    
    (define schelog:make-binary-arithmetic-relation
      (lambda (f)
        (lambda (x y)
          (%is #t (f x y)))))
    
    (define %=:= (schelog:make-binary-arithmetic-relation =))
    (define %> (schelog:make-binary-arithmetic-relation >))
    (define %>= (schelog:make-binary-arithmetic-relation >=))
    (define %< (schelog:make-binary-arithmetic-relation <))
    (define %<= (schelog:make-binary-arithmetic-relation <=))
    (define %=/= (schelog:make-binary-arithmetic-relation
                   (lambda (m n) (not (= m n)))))
    
    ; type predicates
    
    (define schelog:constant?
      (lambda (x)
        (cond ((ref? x)
                (cond ((unbound-ref? x) #f)
                      ((schelog:frozen-ref? x) #t)
                      (else (schelog:constant? (deref x)))))
              ((pair? x) #f)
              ((vector? x) #f)
              (else #t))))
    
    (define schelog:compound?
      (lambda (x)
        (cond ((ref? x) (cond ((unbound-ref? x) #f)
                              ((schelog:frozen-ref? x) #f)
                              (else (schelog:compound? (deref x)))))
              ((pair? x) #t)
              ((vector? x) #t)
              (else #f))))
    
    (define %constant
      (lambda (x)
        (lambda (fk)
          (if (schelog:constant? x) fk (fk 'fail)))))
    
    (define %compound
      (lambda (x)
        (lambda (fk)
          (if (schelog:compound? x) fk (fk 'fail)))))
    
    ; metalogical type predicates
    
    (define schelog:var?
      (lambda (x)
        (cond ((ref? x)
                (cond ((unbound-ref? x) #t)
                      ((schelog:frozen-ref? x) #f)
                      (else (schelog:var? (deref x)))))
              ((pair? x) (or (schelog:var? (car x)) (schelog:var? (cdr x))))
              ((vector? x) (schelog:var? (vector->list x)))
              (else #f))))
    
    (define %var
      (lambda (x)
        (lambda (fk) (if (schelog:var? x) fk (fk 'fail)))))
    
    (define %nonvar
      (lambda (x)
        (lambda (fk) (if (schelog:var? x) (fk 'fail) fk))))
    
    ; negation of unify
    
    (define schelog:make-negation ;basically inlined cut-fail
      (lambda (p)
        (lambda args
          (lambda (fk)
            (if (call-with-current-continuation
                  (lambda (k)
                    ((apply p args) (lambda (d) (k #f)))))
                (fk 'fail)
                fk)))))
    
    (define %/=
      (schelog:make-negation %=))
    
    ; identical
    
    (define schelog:ident?
      (lambda (x y)
        (cond ((ref? x)
                (cond ((unbound-ref? x)
                        (cond ((ref? y)
                                (cond ((unbound-ref? y) (eq? x y))
                                      ((schelog:frozen-ref? y) #f)
                                      (else (schelog:ident? x (deref y)))))
                              (else #f)))
                      ((schelog:frozen-ref? x)
                        (cond ((ref? y)
                                (cond ((unbound-ref? y) #f)
                                      ((schelog:frozen-ref? y) (eq? x y))
                                      (else (schelog:ident? x (deref y)))))
                              (else #f)))
                      (else (schelog:ident? (deref x) y))))
              ((pair? x)
                (cond ((ref? y)
                        (cond ((unbound-ref? y) #f)
                              ((schelog:frozen-ref? y) #f)
                              (else (schelog:ident? x (deref y)))))
                      ((pair? y)
                        (and (schelog:ident? (car x) (car y))
                             (schelog:ident? (cdr x) (cdr y))))
                      (else #f)))
              ((vector? x)
                (cond ((ref? y)
                        (cond ((unbound-ref? y) #f)
                              ((schelog:frozen-ref? y) #f)
                              (else (schelog:ident? x (deref y)))))
                      ((vector? y)
                        (schelog:ident? (vector->list x)
                                        (vector->list y)))
                      (else #f)))
              (else
                (cond ((ref? y)
                        (cond ((unbound-ref? y) #f)
                              ((schelog:frozen-ref? y) #f)
                              (else (schelog:ident? x (deref y)))))
                      ((pair? y) #f)
                      ((vector? y) #f)
                      (else (eqv? x y)))))))
    
    (define %==
      (lambda (x y)
        (lambda (fk) (if (schelog:ident? x y) fk (fk 'fail)))))
    
    (define %/==
      (lambda (x y)
        (lambda (fk) (if (schelog:ident? x y) (fk 'fail) fk))))
    
    ; variables as objects
    
    (define schelog:freeze
      (lambda (s)
        (let ((dict '()))
          (let loop ((s s))
            (cond ((ref? s)
                    (cond ((or (unbound-ref? s) (schelog:frozen-ref? s))
                            (let ((x (assq s dict)))
                              (if x (cdr x)
                                  (let ((y (schelog:freeze-ref s)))
                                    (set! dict (cons (cons s y) dict))
                                    y))))
                          (else (loop (deref s)))))
                  ((pair? s) (cons (loop (car s)) (loop (cdr s))))
                  ((vector? s)
                    (list->vector (map loop (vector->list s))))
                  (else s))))))
    
    (define schelog:melt
      (lambda (f)
        (cond ((ref? f)
                (cond ((unbound-ref? f) f)
                      ((schelog:frozen-ref? f) (schelog:thaw-frozen-ref f))
                      (else (schelog:melt (deref f)))))
              ((pair? f)
                (cons (schelog:melt (car f)) (schelog:melt (cdr f))))
              ((vector? f)
                (list->vector (map schelog:melt (vector->list f))))
              (else f))))
    
    (define schelog:melt-new
      (lambda (f)
        (let ((dict '()))
          (let loop ((f f))
            (cond ((ref? f)
                    (cond ((unbound-ref? f) f)
                          ((schelog:frozen-ref? f)
                            (let ((x (assq f dict)))
                              (if x (cdr x)
                                  (let ((y (make-ref)))
                                    (set! dict (cons (cons f y) dict))
                                    y))))
                          (else (loop (deref f)))))
                  ((pair? f) (cons (loop (car f)) (loop (cdr f))))
                  ((vector? f)
                    (list->vector (map loop (vector->list f))))
                  (else f))))))
    
    (define schelog:copy
      (lambda (s)
        (schelog:melt-new (schelog:freeze s))))
    
    (define %freeze
      (lambda (s f)
        (lambda (fk)
          ((%= (schelog:freeze s) f) fk))))
    
    (define %melt
      (lambda (f s)
        (lambda (fk)
          ((%= (schelog:melt f) s) fk))))
    
    (define %melt-new
      (lambda (f s)
        (lambda (fk)
          ((%= (schelog:melt-new f) s) fk))))
    
    (define %copy
      (lambda (s c)
        (lambda (fk)
          ((%= (schelog:copy s) c) fk))))
    
    ; negation as failure
    
    (define %not
      (lambda (g)
        (lambda (fk)
          (if (call-with-current-continuation
                (lambda (k)
                  ((deref* g) (lambda (d) (k #f)))))
              (fk 'fail) fk))))
    
    ; assert, asserta
    
    (define %empty-rel
      (lambda args
        %fail))
    
    (define-syntax %assert
      (syntax-rules (!)
        ((%assert rel-name (v ...) ((a ...) subgoal ...) ...)
          (set! rel-name
                (let ((__old-rel rel-name)
                      (__new-addition (%rel (v ...) ((a ...) subgoal ...) ...)))
                  (lambda __fmls
                    (%or (apply __old-rel __fmls)
                         (apply __new-addition __fmls))))))))
    
    (define-syntax %assert-a
      (syntax-rules (!)
        ((%assert-a rel-name (v ...) ((a ...) subgoal ...) ...)
          (set! rel-name
                (let ((__old-rel rel-name)
                      (__new-addition (%rel (v ...) ((a ...) subgoal ...) ...)))
                  (lambda __fmls
                    (%or (apply __new-addition __fmls)
                         (apply __old-rel __fmls))))))))
    
    ; set predicates
    
    (define schelog:set-cons
      (lambda (e s)
        (if (member e s) s (cons e s))))
    
    (define-syntax %free-vars
      (syntax-rules ()
        ((%free-vars (v ...) g)
          (cons 'schelog:goal-with-free-vars
                (cons (list v ...) g)))))
    
    (define schelog:goal-with-free-vars?
      (lambda (x)
        (and (pair? x) (eq? (car x) 'schelog:goal-with-free-vars))))
    
    (define schelog:make-bag-of
      (lambda (kons)
        (lambda (lv goal bag)
          (let ((fvv '()))
            (if (schelog:goal-with-free-vars? goal)
                (begin (set! fvv (cadr goal))
                       (set! goal (cddr goal))))
            (schelog:make-bag-of-aux kons fvv lv goal bag)))))
    
    (define schelog:make-bag-of-aux
      (lambda (kons fvv lv goal bag)
        (lambda (fk)
          (call-with-current-continuation
            (lambda (sk)
              (let ((lv2 (cons fvv lv)))
                (let* ((acc '())
                       (fk-final
                         (lambda (d)
                           (sk ((schelog:separate-bags fvv bag acc) fk))))
                       (fk-retry (goal fk-final)))
                  (set! acc (kons (deref* lv2) acc))
                  (fk-retry 'retry))))))))
    
    (define schelog:separate-bags
      (lambda (fvv bag acc)
        (let ((bags (let loop ((acc acc)
                               (current-fvv #f) (current-bag '())
                               (bags '()))
                      (if (null? acc)
                          (cons (cons current-fvv current-bag) bags)
                          (let ((x (car acc)))
                            (let ((x-fvv (car x)) (x-lv (cdr x)))
                              (if (or (not current-fvv) (equal? x-fvv current-fvv))
                                  (loop (cdr acc) x-fvv (cons x-lv current-bag) bags)
                                  (loop (cdr acc) x-fvv (list x-lv)
                                        (cons (cons current-fvv current-bag) bags)))))))))
          (if (null? bags) (%= bag '())
              (let ((fvv-bag (cons fvv bag)))
                (let loop ((bags bags))
                  (if (null? bags) %fail
                      (%or (%= fvv-bag (car bags))
                           (loop (cdr bags))))))))))
    
    (define %bag-of (schelog:make-bag-of cons))
    (define %set-of (schelog:make-bag-of schelog:set-cons))
    
    ; %bag-of-1, %set-of-1 hold if there's at least one solution
    
    (define %bag-of-1
      (lambda (x g b)
        (%and (%bag-of x g b)
              (%= b (cons (make-ref) (make-ref))))))
    
    (define %set-of-1
      (lambda (x g s)
        (%and (%set-of x g s)
              (%= s (cons (make-ref) (make-ref))))))
    
    ; user interface
    
    ; (%which (v ...) query) returns #f if query fails and instantiations of v ... if query
    ; succeeds.  In the latter case, type (%more) to retry query for more instantiations.
    
    (define *more-k* (make-parameter 'forward))
    (define *more-fk* (make-parameter 'forward))
    
    (define-syntax %which
      (syntax-rules (*more-k* *more-fk*)
        ((%which (v ...) g)
          (%let (v ...)
                (call-with-current-continuation
                  (lambda (__qk)
                    (*more-k* __qk)
                    (*more-fk*
                      ((deref* g)
                       (lambda (d)
                         (*more-fk* #f)
                         ((*more-k*) #f))))
                    ((*more-k*)
                     (map (lambda (nam val) (list nam (deref* val)))
                          '(v ...)
                          (list v ...)))))))))
    (define %more
      (lambda ()
        (call-with-current-continuation
          (lambda (k)
            (*more-k* k)
            (if (*more-fk*) ((*more-fk*) 'more)
                #f)))))
    
    ; end of embedding code. The following are some utilities, in Prolog-like fashion.
    
    (define %member
      (%rel (x xs y ys)
            ((x (cons x xs)))
            ((x (cons y ys)) (%member x ys))))
    
    (define %if-then-else
      (%rel (p q r)
            ((p q r) p ! q)
            ((p q r) r)))
    
    (define %append
      (%rel (x xs ys zs)
            (('() ys ys))
            (((cons x xs) ys (cons x zs))
             (%append xs ys zs))))
    
    (define %repeat
      ;;failure-driven loop
      (%rel ()
            (())
            (() (%repeat))))
  )
)
