;;; Simple Prolog interpreter
;;;
;;; This code is a modified version of a tiny Prolog interpreter for MacLisp, originally
;;; written by Ken Kahn and later modified for XLISP by David Betz. It was inspired by other
;;; tiny Lisp-based Prologs by Par Emanuelson and Martin Nilsson. This Prolog implementation
;;; does not use any side-effects. Though it is VERY slow of course.
;;;
;;; The predicates are defined in a database of clauses (see example database `clauses`
;;; below). Function `prolog` implements a small read-eval-print loop which can be used
;;; to make requests. `prolog` either responds with a solution, or prints "no" if no
;;; solution is found. If a solution is found, the system asks if more solutions should
;;; be determined.
;;;
;;; Here is a sample dialog with the system:
;;;
;;; ⟹ (prolog clauses)
;;; ?- (father madelyn ernest)
;;; yes
;;; ?- (father madelyn madelyn)
;;; no
;;; ?- (grandparent rachel G)
;;;    G = virginia
;;; more? y
;;;    G = ernest
;;; more? y
;;;    G = pauline
;;; more? y
;;;    G = arnold
;;; more? y
;;; no
;;; ?- (prod (s (s (s 0))) (s (s 0)) N)
;;;    N = (s (s (s (s (s (s 0))))))
;;; more? y
;;; no
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2017 Matthias Zenger. All rights reserved.

(import (lispkit base))

(define (prolog clauses)
  (do ((goal (read-goal) (read-goal)))
      ((not (pair? goal)))
    (if (not (prove (list (instantiate goal 0)) '((bottom)) clauses 1))
        (begin (display "no") (newline)))))

(define (prove goals env clauses level)
  (if (pair? goals)
      (try-each clauses clauses (cdr goals) (car goals) env level)
      (if (pair? (cdr env))
          (begin (print-bindings env env) (not (read-y/n "more? ")))
          (begin (display "yes") (newline) #t))))

(define (try-each clauses-left clauses goals-left goal env level)
  (if (pair? clauses-left)
      (let* ((assertion (instantiate (car clauses-left) level))
             (new-env (unify goal (car assertion) env)))
        (cond ((null? new-env)
                (try-each (cdr clauses-left) clauses goals-left goal env level))
              ((prove (append (cdr assertion) goals-left) new-env clauses (+ 1 level)) #t)
              (else
                (try-each (cdr clauses-left) clauses goals-left goal env level))))
      #f))

(define (unify x y env)
  (let ((xv (value-of x env))
        (yv (value-of y env)))
    (cond ((variable? xv)
            (cons (list xv yv) env))
          ((variable? yv)
            (cons (list yv xv) env))
          ((and (pair? xv) (pair? yv))
            (let ((new-env (unify (car xv) (car yv) env)))
              (if (pair? new-env) (unify (cdr xv) (cdr yv) new-env) '())))
          (else
            (if (equal? xv yv) env '())))))

(define (instantiate term level)
  (cond ((source-variable? term) (list '? term level))
        ((pair? term)            (cons (instantiate (car term) level)
                                       (instantiate (cdr term) level)))
        (else                    term)))

(define (value-of x env)
  (cond ((variable? x)
          (let ((binding (assoc x env equal?)))
            (if (pair? binding) (value-of (cadr binding) env) x)))
        ((pair? x)
          (cons (value-of (car x) env) (value-of (cdr x) env)))
        (else
          x)))

(define (variable? x)
  (and (pair? x) (eq? (car x) '?)))

(define (source-variable? x)
  (and (symbol? x) (char-upper-case? (string-ref (symbol->string x) 0))))

(define (print-bindings env-left env)
  (if (pair? (cdr env-left))
      (begin (cond ((= 0 (caddr (caar env-left)))
                     (display "   ")
                     (display (cadr (caar env-left)))
                     (display " = ")
                     (display (value-of (caar env-left) env))
                     (newline)))
             (print-bindings (cdr env-left) env))))

(define (read-goal . args)
  (let-optionals args ((prompt "?- "))
    (display prompt)
    (read)))

(define (read-y/n . args)
  (let-optionals args ((prompt "y/n? "))
    (display prompt)
    (eq? (read) 'y)))

;; Sample database of predicates
(define clauses '(
  ((father madelyn ernest))
  ((mother madelyn virginia))
  ((father david arnold))
  ((mother david pauline))
  ((father rachel david))
  ((mother rachel madelyn))
  ((grandparent Grandchild Grandparent)
     (parent Grandchild Parent)
     (parent Parent Grandparent))
  ((parent Child Parent)
     (mother Child Parent))
  ((parent Child Parent)
     (father Child Parent))
  ((int 0))
  ((int (s N))
     (int N))
  ((sum 0 N N))
  ((sum (s N) M (s K))
     (sum N M K))
  ((prod 0 N 0))
  ((prod (s N) M P)
     (sum M K P)
     (prod M N K))
))
