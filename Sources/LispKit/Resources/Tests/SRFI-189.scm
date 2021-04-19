;;; SRFI 189 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 189.
;;;
;;; Copyright © 2020 Wolfgang Corcoran-Mathe. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies
;;; or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
;;; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT
;;; OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test)
        (srfi 189))

;;;; Utilities

(define-syntax check
  (syntax-rules (=>)
    ((check expr)
      (check expr => #t))
    ((check expr => expected)
      (test expected expr))))

(define (identity x) x)

(define (print-header message)
  (newline)
  (display ";;; ")
  (display message)
  (newline))

(define-syntax constantly
  (syntax-rules ()
    ((_ obj) (lambda _ obj))))

;; Gives the values of expr as a list.
(define-syntax values->list
  (syntax-rules ()
    ((_ expr)
      (call-with-values (lambda () expr) list))))

(define always (constantly #t))
(define never (constantly #f))

;; Verify that a Maybe is a Just of 'z, a dummy object.
(define (just-of-z? m)
  (and (maybe? m) (maybe= eqv? m (just 'z))))

;; Verify that an Either is a Right of 'z, a dummy object.
(define (right-of-z? e)
  (and (either? e) (either= eqv? e (right 'z))))

;; Verify that an Either is a Left of 'z, a dummy object.
(define (left-of-z? e)
  (and (either? e) (either= eqv? e (left 'z))))

;;;; Begin tests

;;;; Tests

(define (check-constructors)
  (test-begin "Constructors")
  ;; Uniqueness of the Nothing object.
  (check (eq? (nothing) (nothing)) => #t)
  ;; list->just and list->right
  (check (maybe= eqv? (just #t #t) (list->just '(#t #t)))    => #t)
  (check (either= eqv? (right #t #t) (list->right '(#t #t))) => #t)
  (check (either= eqv? (left #t #t) (list->left '(#t #t)))   => #t)
  ;; maybe->either and either->maybe
  (check (left-of-z? (maybe->either (nothing) 'z))                    => #t)
  (check (right-of-z? (maybe->either (just 'z) #f))                   => #t)
  (check (either= eqv? (right #t #t) (maybe->either (just #t #t) #f)) => #t)
  (check (nothing? (either->maybe (left #t)))                         => #t)
  (check (just-of-z? (either->maybe (right 'z)))                      => #t)
  (check (maybe= eqv? (just #t #t) (either->maybe (right #t #t)))     => #t)
  ;; either-swap
  (check (either= eqv? (right #t #t) (either-swap (left #t #t))) => #t)
  (check (either= eqv? (left #t #t) (either-swap (right #t #t))) => #t)
  (test-end))

;;;; Predicates

(define (check-predicates)
  (test-begin "Predicates")
  (check (just? (just 'z))    => #t)
  (check (just? (nothing))    => #f)
  (check (nothing? (just 'z)) => #f)
  (check (nothing? (nothing)) => #t)
  (check (maybe? (just 'z))   => #t)
  (check (maybe? (nothing))   => #t)
  (check (right? (right 'z))  => #t)
  (check (right? (left 'z))   => #f)
  (check (left? (right 'z))   => #f)
  (check (left? (left 'z))    => #t)
  (check (either? (right 'z)) => #t)
  (check (either? (left 'z))  => #t)
  (check (maybe= eqv? (just #t) (just #t)) => #t)
  (check (maybe= eqv? (just #t) (just #f)) => #f)
  (check (maybe= eqv? (nothing) (nothing)) => #t)
  (check (maybe= eqv? (just #t) (nothing)) => #f)
  (check (maybe= eqv? (just #t #f) (just #t #f)) => #t)
  (check (maybe= eqv? (just #t #f) (just #t 'z)) => #f)
  (check (maybe= eqv? (just #t #f) (just #t))    => #f)
  (check (maybe= eqv? (just #t) (just #t) (just #t))          => #t)
  (check (maybe= eqv? (nothing) (nothing) (nothing))          => #t)
  (check (maybe= eqv? (just #t) (just #t) (nothing))          => #f)
  (check (maybe= eqv? (just #t) (just #t) (just #f))          => #f)
  (check (maybe= eqv? (just #t 'z) (just #t 'z) (just #t 'z)) => #t)
  (check (either= eqv? (right #t) (right #t)) => #t)
  (check (either= eqv? (right #t) (right #f)) => #f)
  (check (either= eqv? (left #t) (left #t))   => #t)
  (check (either= eqv? (left #t) (left #f))   => #f)
  (check (either= eqv? (right #t) (left #t))  => #f)
  (check (either= eqv? (right #t #f) (right #t #f)) => #t)
  (check (either= eqv? (right #t #f) (right #t 'z)) => #f)
  (check (either= eqv? (right #t #f) (right #t))    => #f)
  (check (either= eqv? (left #t #f) (left #t #f))   => #t)
  (check (either= eqv? (left #t #f) (left #t 'z))   => #f)
  (check (either= eqv? (left #t #f) (left #t))      => #f)
  (check (either= eqv? (left #t #f) (right #t #f))  => #f)
  (check (either= eqv? (right #t) (right #t) (right #t))          => #t)
  (check (either= eqv? (left #t) (left #t) (left #t))             => #t)
  (check (either= eqv? (right #t) (right #t) (left #t))           => #f)
  (check (either= eqv? (right #t) (right #t) (right #f))          => #f)
  (check (either= eqv? (right #t 'z) (right #t 'z) (right #t 'z)) => #t)
  (test-end))

;;;; Accessors

(define (check-accessors)
  (test-begin "Accessors")
  (check (maybe-ref (nothing) (lambda () #f))        => #f)
  (check (maybe-ref (just #t) (lambda () #f) values) => #t)
  (check (maybe-ref (nothing) (lambda () #f) values) => #f)
  (check (values->list (maybe-ref (just #t #f) (lambda () #f))) => '(#t #f))
  (check (maybe-ref (just #t #f) (lambda () #f) list)           => '(#t #f))
  (check (either-ref (left #t) (constantly #f))         => #f)
  (check (either-ref (right #t) (constantly #f) values) => #t)
  (check (either-ref (left #t) values (constantly #f))  => #t)
  (check (either-ref (right #t #f) (constantly #f) list) => '(#t #f))
  (check (either-ref (left #t #f) list (constantly #f))  => '(#t #f))
  (check (maybe-ref/default (just #t) #f) => #t)
  (check (maybe-ref/default (nothing) #f) => #f)
  (check (values->list (maybe-ref/default (just #t #t) #f #f)) => '(#t #t))
  (check (values->list (maybe-ref/default (nothing) #f #f))    => '(#f #f))
  (check (either-ref/default (right #t) #f) => #t)
  (check (either-ref/default (left #t) #f)  => #f)
  (check (values->list (either-ref/default (right #t #t) #f #f))
         => '(#t #t))
  (check (values->list (either-ref/default (left #t) #f #f))
         => '(#f #f))
  (test-end))

;;;; Join and bind

(define (check-join-and-bind)
  (test-begin "Join and bind")
  ;; maybe-join
  (check (just-of-z? (maybe-join (just (just 'z)))) => #t)
  (check (nothing? (maybe-join (just (nothing))))   => #t)
  (check (nothing? (maybe-join (nothing)))          => #t)
  ;; either-join
  (check (right-of-z? (either-join (right (right 'z)))) => #t)
  (check (left-of-z? (either-join (right (left 'z))))   => #t)
  (check (left-of-z? (either-join (left 'z)))           => #t)
  ;; maybe-bind
  (check (nothing? (maybe-bind (nothing) just)) => #t)
  (check (just-of-z? (maybe-bind (just 'z) just)) => #t)
  (check (let ((m (just #t #f)))
           (maybe= eqv? m (maybe-bind m just)))
         => #t)
  ;; Associativity of bind.
  (let ((k (lambda (n) (just (* n 2))))
        (h (lambda (n) (just (+ n 5))))
        (m (just 1)))
    (check (maybe= eqv?
                   (maybe-bind m (lambda (n) (maybe-bind (k n) h)))
                   (maybe-bind (maybe-bind m k) h))
           => #t))
  ;; Bind with multiple mprocs.
  (let ((neg (lambda (b) (just (not b)))))
    (check (maybe= eqv? (just #f) (maybe-bind (just #t) neg neg neg))
           => #t)
    (check (nothing? (maybe-bind (just #t) neg (constantly (nothing)) neg))
           => #t))
  ;; maybe-compose
  (check (nothing? ((maybe-compose (constantly (nothing))) 'z)) => #t)
  (check (just-of-z? ((maybe-compose just) 'z))                 => #t)
  ;; Compose with multiple mprocs.
  (let ((neg (lambda (b) (just (not b)))))
    (check (maybe= eqv? (just #t) ((maybe-compose neg neg neg) #f))
           => #t))
  ;; either-bind
  (check (left? (either-bind (left #f) right)) => #t)
  (check (right-of-z? (either-bind (right 'z) right)) => #t)
  (check (let ((e (right #t #f)))
           (either= eqv? e (either-bind e right)))
         => #t)
  ;; Associativity of bind.
  (let ((k (lambda (n) (right (* n 2))))
        (h (lambda (n) (right (+ n 5))))
        (e (right 1)))
    (check
      (either= eqv? (either-bind e (lambda (n) (either-bind (k n) h)))
               (either-bind (either-bind e k) h))
      => #t))
  ;; Bind with multiple mprocs.
  (let ((neg (lambda (b) (right (not b)))))
    (check (either= eqv? (right #f) (either-bind (right #t) neg neg neg))
           => #t)
    (check (either= eqv? (left #f) (either-bind (right #t) neg left neg))
           => #t))
  ;; either-compose
  (check (left-of-z? ((either-compose left) 'z))               => #t)
  (check (either= eqv? (right #t) ((either-compose right) #t)) => #t)
  ;; Compose with multiple mprocs.
  (let ((neg (lambda (b) (right (not b)))))
    (check (either= eqv? (right #t) ((either-compose neg neg neg) #f))
           => #t))
  (test-end))

;;;; Sequence operations

(define (check-sequence-operations)
  (define (both b c) (and b c))
  (test-begin "Sequence operations")
  (check (maybe-length (nothing)) => 0)
  (check (maybe-length (just #t)) => 1)
  (check (either-length (left #t))  => 0)
  (check (either-length (right #t)) => 1)
  ;; maybe-filter & maybe-remove
  (check (just-of-z? (maybe-filter always (just 'z))) => #t)
  (check (nothing? (maybe-filter never (just #t)))    => #t)
  (check (nothing? (maybe-filter always (nothing)))   => #t)
  (check (maybe= eqv? (just #t #t) (maybe-filter both (just #t #t))) => #t)
  (check (just-of-z? (maybe-remove never (just 'z))) => #t)
  (check (nothing? (maybe-remove always (just #t)))  => #t)
  (check (nothing? (maybe-remove always (nothing)))  => #t)
  (check (maybe= eqv? (just #t #f) (maybe-remove both (just #t #f))) => #t)
  ;; maybe-sequence
  (check (maybe= equal? (maybe-sequence (map just '(#t #f)) map identity)
                 (just '(#t #f)))
         => #t)
  (check (maybe= equal? (maybe-sequence (list (just 1 #t) (just 2 #f))
                                        map
                                        list)
                 (just '((1 #t) (2 #f))))
         => #t)
  (check (nothing? (maybe-sequence (list (just #t) (nothing)) map identity))
         => #t)
  ;; either-filter & either-remove
  (check (right-of-z? (either-filter always (right 'z) #f)) => #t)
  (check (left-of-z? (either-filter never (right #t) 'z))   => #t)
  (check (left-of-z? (either-filter always (left #t) 'z))   => #t)
  (check (either= eqv? (right #t #t) (either-filter both (right #t #t) #f))
         => #t)
  (check (right-of-z? (either-remove never (right 'z) #f)) => #t)
  (check (left-of-z? (either-remove always (right #t) 'z)) => #t)
  (check (left-of-z? (either-remove never (left #t) 'z))   => #t)
  (check (either= eqv? (right #t #f) (either-remove both (right #t #f) #f))
         => #t)
  ;; either-sequence
  (check (either= equal? (either-sequence (map right (list 1 2)) map identity)
                  (right (list 1 2)))
         => #t)
  (check (left-of-z? (either-sequence (list (right #t) (left 'z)) map identity))
         => #t)
  (check (either= equal? (either-sequence (list (right 1 #t) (right 2 #f))
                                          map
                                          list)
                  (right '((1 #t) (2 #f))))
         => #t)
  (test-end))

;;;; Protocol conversion procedures

(define (check-conversions)
  (test-begin "Conversions")
  (check (maybe->list (nothing))      => '())
  (check (maybe->list (just #t #t))   => '(#t #t))
  (check (either->list (right #t #t)) => '(#t #t))
  (check (either->list (left #t #t))  => '(#t #t))
  (check (nothing? (list->maybe '()))         => #t)
  (check (just-of-z? (list->maybe '(z)))      => #t)
  (check (left-of-z? (list->either '() 'z))   => #t)
  (check (right-of-z? (list->either '(z) #f)) => #t)
  (check (maybe->truth (nothing))   => #f)
  (check (maybe->truth (just 'z))   => 'z)
  (check (either->truth (left 'z))  => #f)
  (check (either->truth (right 'z)) => 'z)
  (check (nothing? (truth->maybe #f))        => #t)
  (check (just-of-z? (truth->maybe 'z))      => #t)
  (check (left-of-z? (truth->either #f 'z))  => #t)
  (check (right-of-z? (truth->either 'z #f)) => #t)
  (check (maybe->list-truth (just 'z #t))   => '(z #t))
  (check (maybe->list-truth (nothing))      => #f)
  (check (either->list-truth (right 'z #t)) => '(z #t))
  (check (either->list-truth (left 'z))     => #f)
  (check (just-of-z? (list-truth->maybe '(z)))   => #t)
  (check (nothing? (list-truth->maybe #f))       => #t)
  (check (right-of-z? (list-truth->either '(z))) => #t)
  (check (left-of-z? (list-truth->either #f 'z)) => #t)
  (check (eof-object? (maybe->generation (nothing)))         => #t)
  (check (maybe->generation (just #t))                       => #t)
  (check (nothing? (generation->maybe (eof-object)))         => #t)
  (check (just-of-z? (generation->maybe 'z))                 => #t)
  (check (eof-object? (either->generation (left)))         => #t)
  (check (either->generation (right #t))                   => #t)
  (check (left-of-z? (generation->either (eof-object) 'z)) => #t)
  (check (right-of-z? (generation->either 'z #f))          => #t)
  ;; maybe->values and friends
  (check (maybe->values (just #t))                => #t)
  (check (values->list (maybe->values (nothing))) => '())
  
  (check (values->list (maybe->two-values (nothing)))        => '(#f #f))
  (check (values->list (maybe->two-values (just #t)))        => '(#t #t))
  
  (check (just-of-z? (two-values->maybe (lambda () (values 'z #t)))) => #t)
  (check (nothing? (two-values->maybe (lambda () (values 'z #f))))   => #t)
  
  (check (nothing? (values->maybe (lambda () (values)))) => #t)
  (check (just-of-z? (values->maybe (lambda () 'z)))     => #t)
  (check (maybe->values (values->maybe (lambda () #t)))  => #t)
  (check (just-of-z? (values->maybe (lambda ()
                                      (maybe->values (just 'z)))))
         => #t)
  ;; either->values and friends
  (check (either->values (right #t)) => #t)
  (check (values->list (either->values (left 'z))) => '())
  (check (left-of-z? (values->either (lambda () (values)) 'z)) => #t)
  (check (right-of-z? (values->either (lambda () 'z) #f))      => #t)
  (check (either->values (values->either (lambda () #t) #f))   => #t)
  (check (right-of-z? (values->either (lambda ()
                                        (either->values (right 'z)))
                                      #f))
         => #t)
  (check (left-of-z? (exception->either symbol? (lambda () (raise 'z))))
         => #t)
  (check (right-of-z? (exception->either symbol? (lambda () 'z))) => #t)
  (check (guard (obj ((symbol? obj) obj))
                (exception->either number?
                                   (lambda () (raise-continuable 'z))))
         => 'z)
  (check (either= eqv?
                  (with-exception-handler
                    not
                    (lambda ()
                      (exception->either string?
                                         (lambda ()
                                           (not (raise-continuable #t))))))
                  (right #t))
         => #t)
  (test-end))

;;;; Map, fold, and unfold

(define (check-map-fold-and-unfold)
  (test-begin "Maps, folds, and unfolds")
  ;; maybe-map
  (check (nothing? (maybe-map not (nothing)))              => #t)
  (check (maybe= eqv? (just #f) (maybe-map not (just #t))) => #t)
  (check (maybe= eqv? (just #t #f) (maybe-map values (just #t #f))) => #t)
  ;; either-map
  ;; Verify that the result is the same Left (in the sense of eqv?).
  (check (let ((e (left #t))) (eqv? e (either-map not e)))     => #t)
  (check (either= eqv? (right #f) (either-map not (right #t))) => #t)
  
  (check (let ((e (right #t #f)))
           (either= eqv? e (either-map values e)))
         => #t)
  ;; maybe-for-each
  (check (let ((x #f))
           (maybe-for-each (lambda (y) (set! x y)) (just #t))
           x)
         => #t)
  ; Given Nothing, ensure the proc argument is not executed.
  (check (let ((x #f))
           (maybe-for-each (lambda (_) (set! x #t)) (nothing))
           x)
         => #f)
  ;; either-for-each
  (check (let ((x #f))
           (either-for-each (lambda (y) (set! x y)) (right #t))
           x)
         => #t)
  ;; Given a Left, ensure the proc argument is not executed.
  (check (let ((x #f))
           (either-for-each (lambda (_) (set! x #t)) (left 'z))
           x)
         => #f)
  (check (maybe-fold cons '() (nothing)) => '())
  (check (maybe-fold cons '() (just #t)) => '(#t))
  (check (maybe-fold * 2 (just 3 4))     => 24)
  (check (either-fold cons '() (left #t))  => '())
  (check (either-fold cons '() (right #t)) => '(#t))
  (check (either-fold * 2 (right 3 4))     => 24)
  (check (nothing? (maybe-unfold always not always #f))           => #t)
  (check (maybe= eqv? (just #t) (maybe-unfold values not not #f)) => #t)
  (check (maybe= eqv? (just #t 'z)
                 (maybe-unfold (lambda (b _) (not b))
                               values
                               (lambda (b x) (values (not b) x))
                               #t
                               'z))
         => #t)
  (check (left-of-z? (either-unfold always not always 'z))          => #t)
  (check (either= eqv? (right #t) (either-unfold values not not #f)) => #t)
  (check (either= eqv? (right #t 'z)
                  (either-unfold (lambda (b _) (not b))
                                 values
                                 (lambda (b x) (values (not b) x))
                                 #t
                                 'z))
         => #t)
  (test-end))

;;;; Conditional syntax

(define-syntax raises-error-object
  (syntax-rules ()
    ((_ expr)
      (guard (obj ((error-object? obj) #t)
                  (else #f))
             expr))))

(define (check-syntax)
  (test-begin "Syntax")
  (check (maybe-if (just #t) #t #f) => #t)
  (check (maybe-if (nothing) #t #f) => #f)
  (check (raises-error-object (maybe-if #t #t #f)) => #t)
  ;;; maybe-and, -or, -let*, and -let*-values
  (check (just? (maybe-and))                                  => #t)
  (check (just-of-z? (maybe-and (just 'z)))                   => #t)
  (check (just-of-z? (maybe-and (just #t) (just 'z)))         => #t)
  (check (nothing? (maybe-and (just #t) (nothing) (just 'z))) => #t)
  ;; and / bind identities
  (check (maybe= eqv?
                 (maybe-bind (just #f) (constantly (just #t)))
                 (maybe-and (just #f) (just #t))
                 (just #t))
         => #t)
  (check (maybe= eqv?
                 (maybe-bind (nothing) (constantly (just #t)))
                 (maybe-and (nothing) (just #t))
                 (nothing))
         => #t)
  (check (raises-error-object (maybe-and #t (just #t))) => #t)
  (check (nothing? (maybe-or))                               => #t)
  (check (just-of-z? (maybe-or (just 'z)))                   => #t)
  (check (just-of-z? (maybe-or (nothing) (just 'z)))         => #t)
  (check (nothing? (maybe-or (nothing) (nothing) (nothing))) => #t)
  (check (raises-error-object (maybe-or (nothing) #t))       => #t)
  (check (just-of-z?
           (maybe-let* (((maybe-bind (just #t) just)))
                       'z)) => #t)
  (check (nothing?
           (maybe-let* ((x (just #t))
                        (y (nothing)))
                       x))
         => #t)
  (check (maybe= eqv?
                 (maybe-let* ((x (just 2))
                              (y (just 3)))
                             (* x y))
                 (just 6))
         => #t)
  (check (nothing?
           (maybe-let* ((x (just 2))
                        ((maybe-bind (just 'z) (constantly (nothing)))))
                       x))
         => #t)
  (check (maybe= eqv?
                 (maybe-let* ((b (just #t)) ((truth->maybe b)))
                             b)
                 (just #t))
         => #t)
  ;; Behavior of bound-variable claws.
  (let ((just-of-z (just 'z)) (zilch (nothing)))
    (check (just-of-z? (maybe-let* (just-of-z) 'z)) => #t)
    (check (maybe= eqv?
                   (maybe-let* ((x (just 2)) just-of-z (y (just 3)))
                               (* x y))
                   (just 6))
           => #t)
    (check (just-of-z? (maybe-let* (just-of-z ((just 'x)))
                                   'z))
           => #t)
    (check (nothing? (maybe-let* ((x (just 2)) zilch (y (just 3)))
                                 (* x y)))
           => #t))
  ;; let* / bind identities.
  (let ((just-neg (lambda (b) (just (not b)))))
    (check (maybe= eqv?
                   (maybe-bind (just #t) just-neg)
                   (maybe-let* ((b (just #t))) (not b))
                   (just #f))
           => #t)
    (check (maybe= eqv?
                   (maybe-bind (nothing) just-neg)
                   (maybe-let* ((b (nothing))) (not b))
                   (nothing))
           => #t))
  (check (raises-error-object (maybe-let* ((b #t)) 'z)) => #t)
  (check (raises-error-object
           (maybe-let* ((b (just #t)) ('nothing)) #t))
         => #t)
  (check (just-of-z?
           (maybe-let*-values (((maybe-bind (just #t) just)))
                              'z)) => #t)
  (check (nothing?
           (maybe-let*-values (((x) (just #t))
                               (y (nothing)))
                              x))
         => #t)
  (check (maybe= eqv?
                 (maybe-let*-values (((x y) (just 2 3)))
                                    (* x y))
                 (just 6))
         => #t)
  (check (nothing?
           (maybe-let*-values (((x) (just 2))
                               ((maybe-bind (just 'z) (constantly (nothing)))))
                              x))
         => #t)
  (check (maybe= eqv?
                 (maybe-let*-values (((b) (just #t)) ((truth->maybe b)))
                                    b)
                 (just #t))
         => #t)
  (check (just-of-z? (maybe-let*-values ((vals (just 'z #t)))
                                        (car vals)))
         => #t)
  (check (just-of-z? (maybe-let*-values (((x . _) (just 'z #t)))
                                        x))
         => #t)
  (check (maybe= eqv?
                 (maybe-let*-values ((vals (just 'z #t))
                                     ((b c . _) (just #t 'y #f)))
                                    (values (car vals) b))
                 (just 'z #t))
         => #t)
  ;; Behavior of bound-variable claws.
  (let ((just-of-z (just 'z)) (zilch (nothing)))
    (check (just-of-z? (maybe-let*-values (just-of-z) 'z)) => #t)
    (check (maybe= eqv?
                   (maybe-let*-values (((x) (just 2))
                                       just-of-z
                                       ((y) (just 3)))
                                      (* x y))
                   (just 6))
           => #t)
    (check (just-of-z? (maybe-let*-values (just-of-z ((just 'x)))
                                          'z))
           => #t)
    (check (nothing? (maybe-let*-values (((x) (just 2))
                                         zilch
                                         ((y) (just 3)))
                                        (* x y)))
           => #t))
  ;; let*-values / bind identities.
  (let* ((neg-both (lambda (b c) (values (not b) (not c))))
         (just-neg-both (lambda (b c)
                          (call-with-values (lambda () (neg-both b c))
                                            just))))
    (check (maybe= eqv?
                   (maybe-bind (just #t #t) just-neg-both)
                   (maybe-let*-values (((b c) (just #t #t)))
                                      (neg-both b c))
                   (just #f #f))
           => #t)
    (check (maybe= eqv?
                   (maybe-bind (nothing) just-neg-both)
                   (maybe-let*-values (((b c) (nothing))) (neg-both b c))
                   (nothing))
           => #t))
  (check (raises-error-object (maybe-let*-values (((b) #t)) #t)) => #t)
  (check (raises-error-object
           (maybe-let*-values (((b) (just #t)) ('nothing)) #t))
         => #t)
  ;;; either-and, -or, and -let*
  (check (right? (either-and))                               => #t)
  (check (right-of-z? (either-and (right 'z)))               => #t)
  (check (right-of-z? (either-and (right #t) (right 'z)))    => #t)
  (check (left-of-z? (either-and (right) (left 'z) (right))) => #t)
  ;; and / bind identities
  (check (either= eqv?
                  (either-bind (right #f) (constantly (right #t)))
                  (either-and (right #f) (right #t))
                  (right #t))
         => #t)
  (check (either= eqv?
                  (either-bind (left #f) (constantly (right #t)))
                  (either-and (left #f) (right #t))
                  (left #f))
         => #t)
  (check (raises-error-object (either-and #t (right #t))) => #t)
  
  (check (left? (either-or))                              => #t)
  (check (right-of-z? (either-or (right 'z)))             => #t)
  (check (right-of-z? (either-or (left) (right 'z)))      => #t)
  (check (left-of-z? (either-or (left) (left) (left 'z))) => #t)
  (check (raises-error-object (either-or (left #f) #t))   => #t)
  (check (right-of-z?
           (either-let* (((either-bind (right #t) right)))
                        'z))
         => #t)
  (check (left-of-z? (either-let* ((x (right #t)) (y (left 'z)))
                                  x))
         => #t)
  (check (either= eqv?
                  (either-let* ((x (right 2)) (y (right 3)))
                               (* x y))
                  (right 6))
         => #t)
  (check (left-of-z?
           (either-let* ((x (right 2))
                         ((either-swap (right 'z))))
                        x))
         => #t)
  (check (either= eqv?
                  (either-let* ((b (right #t)) ((truth->either b)))
                               b)
                  (right #t))
         => #t)
  ;; Behavior of bound-variable claws.
  (let ((right-of-z (right 'z)) (left-of-z (left 'z)))
    (check (right-of-z? (either-let* (right-of-z) 'z)) => #t)
    (check (either= eqv?
                    (either-let* ((x (right 2))
                                  right-of-z
                                  (y (right 3)))
                                 (* x y))
                    (right 6))
           => #t)
    (check (right-of-z?
             (either-let* (right-of-z ((right 'x)))
                          'z))
           => #t)
    (check (left-of-z?
             (either-let* ((x (right 2)) left-of-z (y (right 3)))
                          (* x y)))
           => #t))
  ;; let* / bind identities.
  (let ((right-neg (lambda (b) (right (not b)))))
    (check (either= eqv?
                    (either-bind (right #t) right-neg)
                    (either-let* ((b (right #t))) (not b))
                    (right #f))
           => #t)
    (check (either= eqv?
                    (either-bind (left #t) right-neg)
                    (either-let* ((b (left #t))) (not b))
                    (left #t))
           => #t))
  (check (raises-error-object (either-let* ((b #t)) 'z)) => #t)
  (check (raises-error-object
           (either-let* ((b (right #t)) ('left)) #t))
         => #t)
  (check (right-of-z?
           (either-let*-values (((either-bind (right #t) right)))
                               'z))
         => #t)
  (check (left-of-z?
           (either-let*-values (((x) (right #t))
                                (y (left 'z)))
                               x))
         => #t)
  (check (either= eqv?
                  (either-let*-values (((x y) (right 2 3)))
                                      (* x y))
                  (right 6))
         => #t)
  (check (left-of-z?
           (either-let*-values (((x) (right 2))
                                ((either-swap (right 'z))))
                               x))
         => #t)
  (check (either= eqv?
                  (either-let*-values (((b) (right #t)) ((truth->either b)))
                                      b)
                  (right #t))
         => #t)
  (check (right-of-z? (either-let*-values ((vals (right 'z #t)))
                                          (car vals)))
         => #t)
  (check (right-of-z? (either-let*-values (((x . _) (right 'z #t)))
                                          x))
         => #t)
  (check (either= eqv?
                  (either-let*-values ((vals (right 'z #t))
                                       ((b c . _) (right #t 'y #f)))
                                      (values (car vals) b))
                  (right 'z #t))
         => #t)
  ;; Behavior of bound-variable claws.
  (let ((right-of-z (right 'z)) (left-of-z (left 'z)))
    (check (right-of-z? (either-let*-values (right-of-z)
                                            'z))
           => #t)
    (check (either= eqv?
                    (either-let*-values (((x) (right 2))
                                         right-of-z
                                         ((y) (right 3)))
                                        (* x y))
                    (right 6))
           => #t)
    (check (right-of-z? (either-let*-values (right-of-z ((right 'x)))
                                            'z))
           => #t)
    (check (left-of-z? (either-let*-values (((x) (right 2))
                                            left-of-z
                                            ((y) (right 3)))
                                           (* x y)))
           => #t))
  ;; let*-values / bind identities.
  (let* ((neg-both (lambda (b c) (values (not b) (not c))))
         (right-neg-both (lambda (b c)
                           (call-with-values (lambda () (neg-both b c))
                                             right))))
    (check (either= eqv?
                    (either-bind (right #t #t) right-neg-both)
                    (either-let*-values (((b c) (right #t #t)))
                                        (neg-both b c))
                    (right #f #f))
           => #t)
    (check (either= eqv?
                    (either-bind (left #t #t) right-neg-both)
                    (either-let*-values (((b c) (left #t #t)))
                                        (neg-both b c))
                    (left #t #t))
           => #t))
  (check (raises-error-object
           (either-let*-values (((b) #t)) 'z))
         => #t)
  (check (raises-error-object
           (either-let*-values (((b) (right #t)) ('left)) 'z))
         => #t)
  (check (left-of-z? (either-guard symbol? (raise 'z))) => #t)
  (check (right-of-z? (either-guard symbol? 'z)) => #t)
  (check (guard (obj ((symbol? obj) obj))
                (either-guard number? (raise-continuable 'z)))
         => 'z)
  (check (either= eqv?
                  (with-exception-handler
                    not
                    (lambda ()
                      (either-guard string? (not (raise-continuable #t)))))
                  (right #t))
         => #t)
  (test-end))

;;;; Trivalent logic

(define (check-trivalent)
  (define (tri-true? m)
    (and (just? m) (maybe-ref/default m 'z)))
  (define (tri-false? m)
    (and (just? m) (not (maybe-ref/default m 'z))))
  (test-begin "Trivalent logic")
  (check (tri-true? (tri-not (just #f)))  => #t)
  (check (tri-false? (tri-not (just #t))) => #t)
  (check (nothing? (tri-not (nothing)))   => #t)
  (check (tri-true? (tri=? (just #t) (just 1) (just 'x))) => #t)
  (check (tri-true? (tri=? (just #f) (just #f)))          => #t)
  (check (tri-true? (tri=? (just #f) (just #f)))          => #t)
  (check (tri-false? (tri=? (just #f) (just #t)))         => #t)
  (check (tri-false? (tri=? (just #f) (nothing)))         => #t)
  (check (tri-true? (tri-and (just #t) (just 1) (just 'x))) => #t)
  (check (nothing? (tri-and (just #t) (nothing)))           => #t)
  (check (tri-false? (tri-and (just #f) (just #t)))         => #t)
  (check (tri-true? (tri-and))                              => #t)
  (check (tri-false? (tri-or (just #f) (just #f) (just #f))) => #t)
  (check (nothing? (tri-or (just #f) (nothing)))             => #t)
  (let ((m-true (just 'x)))
    (check (maybe= eqv? m-true (tri-or (just #f) m-true))    => #t))
  (check (tri-false? (tri-or))                               => #t)
  (check (nothing? (tri-merge (nothing) (nothing) (nothing)))  => #t)
  (let ((m-true (just 'x)))
    (check (maybe= eqv? m-true (tri-merge (nothing) m-true))   => #t))
  (let ((m-false (just #f)))
    (check (maybe= eqv? m-false (tri-merge (nothing) m-false)) => #t))
  (check (nothing? (tri-merge))                                => #t)
  (test-end))

(test-begin "SRFI 189: ???")

(check-constructors)
(check-predicates)
(check-accessors)
(check-join-and-bind)
(check-sequence-operations)
(check-conversions)
(check-map-fold-and-unfold)
(check-syntax)
(check-trivalent)

(test-end)
