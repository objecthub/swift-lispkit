;;; Symbolic Mathematics with Polynomials
;;;
;;; The code below implements a symbolic canonicalizer for polynomials. A polynomial is
;;; a function of one or more variables that can be computed using only addition and
;;; multiplication. For instance, `4*x^3 + b*x^2 + 1` is a polynomial whose main variable
;;; is `x`, the degree, i.e. the highest power of x, is 3, and the coefficients are
;;; 4, b, 0, and 1.
;;;
;;; Here are a few sample interactions with the canonicalizer/simplifier of expressions
;;; involving polynomials. Procedure `canon` expects polynomials in infix form and returns
;;; their canonicalized form again in infix form:
;;;
;;;   > (canon '(3 + x + 4 - x))
;;;   7
;;;   > (canon '(x + y + y + x))
;;;   ((2 * x) + (2 * y))
;;;   > (canon '(3 * x + 4 * x))
;;;   (7 * x)
;;;   > (canon '(3 * x + y + x + 4 * x))
;;;   ((8 * x) + y)
;;;   > (canon '(3 * x + y + z + x + 4 * x))
;;;   ((8 * x) + y + z)
;;;   > (canon '((x + 1) ^ 10 + (x - 1) ^ 10))
;;;   ((2 * (x ^ 10)) + (90 * (x ^ 8)) + (420 * (x ^ 6)) + (420 * (x ^ 4)) + (90 * (x ^ 2)) + 2)
;;;   > (canon '((x + 1) ^ 10 - (x - 1) ^ 10))
;;;   ((20 * (x ^ 9)) + (240 * (x ^ 7)) + (504 * (x ^ 5)) + (240 * (x ^ 3)) + (20 * x))
;;;   > (canon '(3 * x ^ 3 + 4 * x * y * (x - 1) + x ^ 2 * (x + y)))
;;;   ((4 * (x ^ 3)) + (5 * y * (x ^ 2)) + (-4 * y * x))
;;;   > (canon '(3 * x ^ 3 + 4 * x * w * (x - 1) + x ^ 2 * (x + w)))
;;;   ((((5 * (x ^ 2)) + (-4 * x)) * w) + (4 * (x ^ 3)))
;;;
;;; The design of the data structures, functions and algorithms is based on chapter 15 of
;;; Peter Norvig's book on "Paradigms of Artificial Intelligence Programming: Case Studies
;;; in Common Lisp": https://github.com/norvig/paip-lisp/blob/master/docs/chapter15.md .
;;; The book has been published under an MIT license at https://github.com/norvig/paip-lisp .
;;;
;;; The code below is a LispKit-specific port of the code in Peter Norvig's book. The data
;;; structures and algorithms closely follow the presentation in the book, but the actual
;;; implementations sometimes differ significantly.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2020 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed under the
;;; License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;;; either express or implied. See the License for the specific language governing permissions
;;; and limitations under the License.

(import (lispkit base))

;; Prefix expressions are represented as lists with at least two elements: the operator and
;; at least one argument.
(define (exp? x)
  (and (pair? x) (pair? (cdr x))))

;; Arguments of an expression
(define exp-args cdr)

;; The procedures implementing operators
(define (exp-op-proc x)
  (case (car x)
    ((+) poly+)
    ((-) poly-)
    ((*) poly*poly)
    ((^) poly^n)
    (else (error "not a valid operator" (car x)))))

;; The precedence of operators
(define (exp-op-precedence x)
  (case x
    ((+)  1)
    ((-)  1)
    ((*)  2)
    ((^)  3)
    (else #f)))

;; Converts an expression in infix form into prefix form.
(define (infix->prefix expr)
  (define (push-exp expr stack)
    (if (null? expr)
        (reduce-stack stack 0)
        (if (pair? (car expr))
            (push-exp (cdr expr) (cons (infix->prefix (car expr)) stack))
            (let ((prec (exp-op-precedence (car expr))))
              (if prec
                  (push-exp (cdr expr) (cons (car expr) (reduce-stack stack prec)))
                  (push-exp (cdr expr) (cons (car expr) stack)))))))
  (define (reduce-stack stack prec)
    (if (null? stack)
        (error "not a valid infix expression: $0" expr)
        (if (null? (cdr stack))
            stack
            (let ((next-prec (exp-op-precedence (cadr stack))))
              (if next-prec
                  (if (<= prec next-prec)
                      (reduce-stack (cons (list (cadr stack) (caddr stack) (car stack))
                                          (cdddr stack)) prec)
                      stack)
                  (error "not a valid infix expression: $0" expr))))))
  (car (push-exp expr '())))

;; Converts an expression in prefix form into infix form.
(define (prefix->infix expr)
  (if (pair? expr)
      (cons (prefix->infix (cadr expr))
            (append-map (lambda (x) (list (car expr) (prefix->infix x))) (cddr expr)))
      expr))

;; Type `poly` defines a type for representing a polynomial dependent on a main variable.
;; Such polynomials are represented as a vector whose first element is the main variable,
;; and the second to _n_ elements are the coefficients of the polynomial.
;;
;; Example: Polynomial `5*x^3 + 4*x + 1` can be defined as follows:
;; `(prefix->canon '(+ (* 5 (^ x 3)) (+ (* 4 x) 1)))`. This expression returns
;; `#poly:#(x 1 4 0 5)`.
(define-type poly
  poly?
  ((new-poly vec)
    vec)
  ((main-var (p))
    (vector-ref p 0))
  ((coef (p) i)
    (vector-ref p (+ i 1)))
  ((set-coef! (p) i c)
    (vector-set! p (+ i 1) c))
  ((degree (p))
    (- (vector-length p) 2))
  ((copy-poly (p))
    (new-poly (vector-copy p)))
  ((cut-poly (p) n)
    (new-poly (vector-copy p 0 (+ n 2)))))

;; Make the polynomial 0 + 0*x + 0*x^2 + ... 0*x^degree
(define (make-poly x degree)
  (let ((p (make-vector (+ degree 2) 0)))
    (vector-set! p 0 x)
    (new-poly p)))

;; Return a polynomial with main variable `var` and coefficients `c`, ...
(define (poly var c . cs)
  (let ((p (make-poly var (length cs))))
    (set-coef! p 0 c)
    (do ((i 1 (+ i 1))
         (coefs cs (cdr coefs)))
        ((null? coefs) p)
      (set-coef! p i (car coefs)))))

;; Convert a prefix expression to canonical form.
(define (prefix->canon x)
  (cond ((number? x) x)
        ((symbol? x) (poly x 0 1))
        ((exp? x)    (apply (exp-op-proc x) (map prefix->canon (exp-args x))))
        (else        (error "not a polynomial: $0" x))))

;; Unary or binary polynomial addition.
(define poly+
  (case-lambda
    ((p) p)
    ((p q) (poly+poly p q))))

;; Unary or binary polynomial subtraction.
(define poly-
  (case-lambda
    (() 0)
    ((p) (poly*poly -1 p))
    ((p q) (poly+poly p (poly*poly -1 q)))))

(define (var= x y) (eq? x y))
(define (var> x y) (string>? (symbol->string x) (symbol->string y)))

;; Add two polynomials.
(define (poly+poly p q)
  (normalize-poly
    (cond ((number? p)                      (k+poly p q))
          ((number? q)                      (k+poly q p))
          ((var= (main-var p) (main-var q)) (poly+same p q))
          ((var> (main-var q) (main-var p)) (k+poly q p))
          (else                             (k+poly p q)))))

;; Add a constant k to a polynomial p.
(define (k+poly k p)
  (cond ((and (number? k) (zero? k))   p)               ;; 0 + p = p
        ((and (number? k) (number? p)) (+ k p))         ;; Add numbers
        (else
          (let ((r (copy-poly p)))                      ;; Add k to x^0 term of p
            (set-coef! r 0 (poly+poly (coef r 0) k))
            r))))

;; Add two polynomials with the same main variable.
(define (poly+same p q)
  ;; First assure that q is the higher degree polynomial
  (if (> (degree p) (degree q))
      (poly+same q p)
      ;; Add each element of p into r (which is a copy of q).
      (do ((r (copy-poly q))
           (i 0 (+ i 1)))
          ((> i (degree p)) r)
        (set-coef! r i (poly+poly (coef r i) (coef p i))))))

;; Multiply two polynomials.
(define (poly*poly p q)
  (normalize-poly
    (cond ((number? p)                      (k*poly p q))
          ((number? q)                      (k*poly q p))
          ((var= (main-var p) (main-var q)) (poly*same p q))
          ((var> (main-var q) (main-var p)) (k*poly q p))
          (else                             (k*poly p q)))))

;; Multiply a polynomial p by a constant factor k.
(define (k*poly k p)
  (cond ((eq? k 0)                     0)              ;; 0 * p = 0
        ((eq? k 1)                     p)              ;; 1 * p = p
        ((and (number? k) (number? p)) (* k p))        ;; Multiply numbers
        (else                                          ;; Multiply each coefficient
          (do ((r (make-poly (main-var p) (degree p))) ;; Accumulate result in r; r[i] = k*p[i]
               (i 0 (+ i 1)))
              ((> i (degree p)) r)
            (set-coef! r i (poly*poly k (coef p i)))))))

;; Multiply two polynomials with the same variable.
(define (poly*same p q)
  ;; r[i] = p[0]*q[i] + p[1]*q[i-1] + ...
  (let* ((p-degree (degree p))
         (q-degree (degree q))
         (r (make-poly (main-var p) (+ p-degree q-degree))))
    (do ((i 0 (+ i 1)))
        ((> i p-degree) r)
      (unless (eq? (coef p i) 0)
        (do ((j 0 (+ j 1)))
            ((> j q-degree))
          (set-coef! r (+ i j) (poly+poly (coef r (+ i j)) (poly*poly (coef p i) (coef q j)))))))))

;; Alter a polynomial by dropping trailing zeros.
(define (normalize-poly p)
  (if (number? p)
      p
      (do ((i (degree p) (- i 1)))
          ((or (zero? i) (not (eq? (coef p i) 0)))
           (cond ((zero? i)        (normalize-poly (coef p 0)))
                 ((= i (degree p)) p)
                 (else
                   (cut-poly p i)))))))

;; Raise polynomial p to the n-th power where needs to be >=0.
(define (poly^n p n)
  (if (fixnum? n)
      (cond ((zero? n)    1)
            ((number? p)  (expt p n))
            (else         (poly*poly p (poly^n p (- n 1)))))
      (error "not valid polynomial: #0" '(^ p n))))

;; Convert a canonical polynomial to a prefix expression.
(define (canon->prefix p)
  (if (number? p)
      p
      (args->prefix '+ 0
        (do ((i 0 (+ i 1))
             (args '() (cons (args->prefix '* 1 (list (canon->prefix (coef p i))
                                                      (exponent->prefix (main-var p) i))) args)))
            ((> i (degree p)) args)))))

;; Convert canonical `base^exponent` to prefix expression.
(define (exponent->prefix base exponent)
  (cond ((= exponent 0)  1)
        ((= exponent 1)  base)
        (else            (list '^ base exponent))))

;; Convert `arg1 op arg2 op ...` to prefix expression.
(define (args->prefix op identity args)
  (let ((useful-args (delq identity args)))
    (cond ((null? useful-args)              identity)
          ((and (eq? op '*) (memq 0 args))  0)
          ((null? (cdr useful-args))        (car useful-args))
          (else
            (cons op (append-map (lambda (exp)
                                   (if (and (pair? exp) (eq? (car exp) op))
                                       (exp-args exp)
                                       (list exp)))
                                 useful-args))))))

;; Canonicalize given infix expression and convert it back to infix form.
(define (canon infix-exp)
  (prefix->infix (canon->prefix (prefix->canon (infix->prefix infix-exp)))))

