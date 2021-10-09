;;; PAIP Symbolic Algebraic Simplification
;;; 
;;; Library `(paip simplifier)` implements the symbolic algebraic simplifier described
;;; in chapter 8 of Peter Norvig's book "Paradigms of Artificial Intelligence Programming:
;;; Case Studies in Common Lisp" (1992). It was ported from the original Common Lisp code.
;;; 
;;; Examples:
;;;   (infix->prefix '(1 + 2 * 3))           ==> (+ 1 (* 2 3))
;;;   (infix->prefix '(Int 3 * (x + 2) d x)) ==> (int (* 3 (+ x 2)) x)
;;;   (simp '(4 * (9 / x) * x - 10))         ==>  26
;;;   (simp '(9 - 3 * (x + x) * x))          ==> (9 - (6 * (x ^ 2)))
;;;   (simp '(Int x * sin(x ^ 2) d x))       ==> (-1/2 * (cos (x ^ 2)))
;;; 
;;; Interactive:
;;;   > (simplifier)
;;;   simplifier> (2 * x * x * 3)
;;;   (6 * (x ^ 2))
;;;   simplifier> (2 * x * 3 * y * 4 * z * 5 * 6)
;;;   (720 * (x * (y * z)))
;;;   simplifier> (3 + x + 4 + x)
;;;   ((2 * x) + 7)
;;;   simplifier> (2 * x * 3 * x * 4 * (1 / x) * 5 * 6)
;;;   (720 * x)
;;;   simplifier> (d (x + x) / d x)
;;;   2
;;;   simplifier> (d (a * x ^ 2 + b * x + c) / d x)
;;;   ((2 * (a * x)) + b)
;;;   simplifier> (log(x + x) - log x)
;;;   (log 2)
;;;   simplifier> (x ^ cos pi)
;;;   (1 / x)
;;;   simplifier> (d (3 * x + (cos x) / x) / d x)
;;;   ((((x * (- (sin x))) - (cos x)) / (x ^ 2)) + 3)
;;;   simplifier> (d (3 * x ^ 2 + 2 * x + 1) / d x)
;;;   ((6 * x) + 2)
;;;   simplifier> (sin(x + x) ^ 2 + cos(d x ^ 2 / d x) ^ 2)
;;;   1
;;;   simplifier> (Int x * sin(x ^ 2) d x)
;;;   (-1/2 * (cos (x ^ 2)))
;;;   simplifier> (Int (3 * x + 2) ^ -2/3 d x)
;;;   (((3 * x) + 2) ^ 1/3)
;;;   simplifier> (Int sin(x) ^ 2 * cos(x) d x)
;;;   (1/3 * ((sin x) ^ 3))
;;;   simplifier> (Int (2 * x + 1) / (x ^ 2 + x - 1) d x)
;;;   (log ((x ^ 2) + (x - 1)))
;;;   simplifier> q
;;; 
;;; 
;;; Author of Scheme implementation: Matthias Zenger
;;; 
;;; Original code from Paradigms of AI Programming
;;; File macsyma.lisp: The implementation of MACSYMA in Chapter 8
;;; 
;;; Copyright (c) 2018 Peter Norvig
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(define-library (paip simplifier)
  
  (export infix->prefix
          prefix->infix
          simplifier
          simplify
          simp
          *simplification-rules*
          integration-table)
  
  (import (lispkit base)
          (paip util)
          (paip patmatch))
  
  (begin
    
    ;;;; Simplification plugins
    
    (define simp-fn-proc (make-eq-hashtable))
    
    (define (simp-fn op)
      (hashtable-ref simp-fn-proc op #f))
    
    (define (set-simp-fn op fn)
      (hashtable-set! simp-fn-proc op fn))
    
    ;; Variables are the symbols M through Z.
    (define *variable-map* '((m . ?m)(n . ?n)(o . ?o)(p . ?p)(q . ?q)
                             (r . ?r)(s . ?s)(t . ?t)(u . ?u)(v . ?v)
                             (w . ?w)(x . ?x)(y . ?y)(z . ?z)))
    
    (define (map-variables tree)
      (sublis *variable-map* tree))
    
    (define rule-pattern car)
    (define rule-response cadr)
    
    ;;; Expressions
    
    (define (mkexp lhs op rhs)
      (list op lhs rhs))
    
    (define exp-args cdr)
    (define exp-op car)
    (define exp-lhs cadr)
    (define exp-rhs caddr)
    
    (define exp? pair?)
    
    (define (binary-exp? x)
      (and (exp? x) (= (length (exp-args x)) 2)))
    
    ;; Translate prefix to infix expressions
    (define (prefix->infix exp)
      (if (pair? exp)
          (map prefix->infix
               (if (binary-exp? exp)
                   (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
                   exp))
          exp))
    
    ;; Define x+ and y+ as a sequence:
    (pat-match-abbrev 'x+ '(?+ ?x))
    (pat-match-abbrev 'y+ '(?+ ?y))
    
    ;; Define n and m as numbers; s as a non-number:
    (pat-match-abbrev '?n (list '?is '?n number?))
    (pat-match-abbrev '?m (list '?is '?m number?))
    (pat-match-abbrev '?s (list '?is '?s not-number?))
    
    (define *infix->prefix-rules*
      (make-parameter
        (map (lambda (rule) (expand-pat-match-abbrev (map-variables rule)))
          '(((x+ = y+)     (= x y))
            ((- x+)        (- x))
            ((+ x+)        (+ x))
            ((x+ + y+)     (+ x y))
            ((x+ - y+)     (- x y))
            ((d y+ / d x)  (d y x))        ;*** New rule
            ((Int y+ d x)  (int y x))      ;*** New rule
            ((x+ * y+)     (* x y))
            ((x+ / y+)     (/ x y))
            ((x+ ^ y+)     (^ x y))))))
    
    ;; Translate an infix expression into prefix notation.
    (define (infix->prefix exp)
      ; Note we cannot do implicit multiplication in this system
      (cond ((not (pair? exp))
              exp)
            ((= (length exp) 1)
              (infix->prefix (car exp)))
            ((rule-based-translator
               exp
               (*infix->prefix-rules*)
               pat-match
               rule-pattern
               rule-response
               (lambda (bindings response)
                 (sublis (map (lambda (pair)
                                (cons (car pair) (infix->prefix (cdr pair)))) bindings)
                         response))))
            ((symbol? (car exp))
              (list (car exp) (infix->prefix (cdr exp))))
            (else
              (error "illegal expression" exp))))
    
    (define *simplification-rules* (make-parameter '()))
    
    ;; Read a mathematical expression, simplify it, and print the result. Entering `q` will
    ;; terminate the loop.
    (define (simplifier)
      (let ((read-exp (lambda () (display "simplifier> ") (read))))
        (do ((exp (read-exp) (read-exp)))
            ((eq? exp 'q))
          (display (simp exp))
          (newline))))
    
    ;; Simplify an expression in infix form.
    (define (simp inf)
      (prefix->infix (simplify (infix->prefix inf))))
    
    ;; Simplify an expression by first simplifying its components.
    (define (simplify exp)
      (if (pair? exp)
          (simplify-exp (map simplify exp))
          exp))
    
    ;; Is this an arithmetic expression that can be evaluated?
    (define (evaluable exp)
      (and (every? number? (exp-args exp))
           (or (member (exp-op exp) '(+ - * /))
               (and (eq? (exp-op exp) '^) (integer? (cadr (exp-args exp)))))))
    
    ;; Transform a rule into proper format.
    (define (simp-rule rule)
      (let ((exp (infix->prefix (map-variables rule))))
        (mkexp (expand-pat-match-abbrev (exp-lhs exp))
               (exp-op exp)
               (exp-rhs exp))))
    
    ;; Simplify using a rule, or by doing arithmetic, or by using the simp function
    ;; supplied for this operator.
    (define (simplify-exp exp)
      (cond ((simplify-by-fn exp))
            ((rule-based-translator exp
                                    (*simplification-rules*)
                                    pat-match
                                    exp-lhs
                                    exp-rhs
                                    (lambda (bindings response)
                                      (simplify (sublis bindings response)))))
            ((evaluable exp) (eval exp))
            (else exp)))
    
    ;; If there is a simplification fn for this exp, and if applying it gives a non-null
    ;; result, then simplify the result and return that."
    (define (simplify-by-fn exp)
      (let* ((fn (simp-fn (exp-op exp)))
             (result (and fn (fn exp))))
        (and result (simplify result))))
    
    ;; Is this a list whose first element is x?
    (define (starts-with? lst x)
      (and (pair? lst) (equal? (car lst) x)))
    
    ;; Is x a list of length 1?
    (define (length=1? x)
      (and (pair? x) (null? (cdr x))))
    
    ;; Return a list of the factors of exp^n, where each factor is of the form (^ y n).
    (define (factorize exp)
      (letrec ((factors '())
               (constant 1)
               (ins (lambda (x n exps)
                      (cond ((null? exps)
                              (list (list '^ x n)))
                            ((equal? x (exp-lhs (car exps)))
                              (cons (list '^ x (+ (exp-rhs (car exps)) n)) (cdr exps)))
                            (else
                              (cons (car exps) (ins x n (cdr exps)))))))  ;;;;; was "inc"???
               (fac (lambda (x n)
                      (cond ((number? x)
                              (set! constant (* constant (^ x n))))
                            ((starts-with? x '*)
                              (fac (exp-lhs x) n)
                              (fac (exp-rhs x) n))
                            ((starts-with? x '/)
                              (fac (exp-lhs x) n)
                              (fac (exp-rhs x) (- n)))
                            ((and (starts-with? x '-) (length=1? (exp-args x)))
                              (set! constant (- constant))
                              (fac (exp-lhs x) n))
                            ((and (starts-with? x '^) (number? (exp-rhs x)))
                              (fac (exp-lhs x) (* n (exp-rhs x))))
                            (else
                              (set! factors (ins x n factors)))))))
         (fac exp 1)
         (case constant
           ((0)  '((^ 0 1)))
           ((1)  factors)
           (else (cons (list '^ constant 1) factors)))))
    
    ;; Convert a list of factors back into prefix form.
    (define (unfactorize factors)
      (cond ((null? factors)
              1)
            ((length=1? factors)
              (car factors))
            (else
              (list '* (car factors) (unfactorize (cdr factors))))))
    
    ;; Divide a list of factors by another, producing a third.
    (define (divide-factors numer denom)
      (letrec ((divide (lambda (nmr lhs rhs)
                         (cond ((zero? rhs)
                                 nmr)
                               ((null? nmr)
                                 (list (list '^ lhs (- rhs))))
                               ((equal? lhs (exp-lhs (car nmr)))
                                 (let ((nrhs (- (exp-rhs (car nmr)) rhs)))
                                   (if (zero? nrhs)
                                       (cdr nmr)
                                       (cons (list (exp-op (car nmr)) lhs nrhs) (cdr nmr)))))
                               (else
                                 (cons (car nmr) (divide (cdr nmr) lhs rhs)))))))
        (if (pair? denom)
            (divide-factors (divide numer (exp-lhs (car denom)) (exp-rhs (car denom))) (cdr denom))
            numer)))
    
    ;; True if expression has no occurrence of var.
    (define (free-of exp var)
      (not (find-anywhere var exp)))
    
    ;; Does item occur anywhere in tree?  If so, return it.
    (define (find-anywhere item tree)
      (cond ((eq? item tree) tree)
            ((not (pair? tree)) #f)
            ((find-anywhere item (car tree)))
            ((find-anywhere item (cdr tree)))))
    
    (define (some f xs)
      (and (pair? xs) (or (f (car xs)) (some f (cdr xs)))))
    
    (define (integrate exp x)
      ; First try some trivial cases
      (cond ((free-of exp x)
              (list '* exp x))                                ; Int c dx = c*x
            ((starts-with? exp '+)                            ; Int f + g  =
              (list '+ (integrate (exp-lhs exp) x)            ; Int f + Int g
                       (integrate (exp-rhs exp) x)))
            ((starts-with? exp '-)
              (case (length (exp-args exp))
                ((1) (list '- (integrate (exp-lhs exp) x)))   ; Int - f = - Int f
                ((2) (list '- (integrate (exp-lhs exp) x)     ; Int f - g  =
                              (integrate (exp-rhs exp) x)))
                (else (error "illegal expression" exp))))     ; Int f - Int g
            ; Now move the constant factors to the left of the integral
            ((let-values (((const-factors x-factors)
                             (partition-if (lambda (factor)
                                             (free-of factor x)) (factorize exp))))
               (identity ; simplify
                 (list '*
                       (unfactorize const-factors)
                       ; And try to integrate:
                       (cond ((null? x-factors) x)
                             ((some (lambda (factor)
                                      (deriv-divides factor x-factors x)) x-factors))
                             ; <other methods here>
                             (else
                               (list 'int? (unfactorize x-factors) x)))))))))
    
    ;; Return 2 values: elements of list that satisfy pred, and elements that don't.
    (define (partition-if pred list)
      (let ((yes-list '())
            (no-list '()))
        (do ((xs list (cdr xs)))
            ((null? xs) (values (reverse yes-list) (reverse no-list)))
          (if (pred (car xs))
              (set! yes-list (cons (car xs) yes-list))
              (set! no-list (cons (car xs) no-list))))))
    
    (define (deriv-divides factor factors x)
      (assert (starts-with? factor '^))
      (let* ((u (exp-lhs factor))              ; factor = u^n
             (n (exp-rhs factor))
             (k (divide-factors factors (factorize (list '* factor (deriv u x))))))
        (cond ((free-of k x)
                 ;; Int k*u^n*du/dx dx = k*Int u^n du
                 ;;                    = k*u^(n+1)/(n+1) for n/=1
                 ;;                    = k*log(u) for n=1
                 (if (= n -1)
                     (list '* (unfactorize k) (list 'log u))
                     (list '/ (list '* (unfactorize k) (list '^ u (+ n 1))) (+ n 1))))
              ((and (= n 1) (in-integral-table? u))
                 ;; Int y'*f(y) dx = Int f(y) dy
                 (let ((k2 (divide-factors factors
                                           (factorize (list '* u (deriv (exp-lhs u) x))))))
                   (if (free-of k2 x)
                       (list '*
                             (integrate-from-table (exp-op u) (exp-lhs u))
                             (unfactorize k2))
                       #f)))
             (else
               #f))))
    
    (define (deriv y x)
      (simplify (list 'd y x)))
    
    (define int-proc (make-eq-hashtable))
    
    (define (integration-table rules)
      (do ((rs rules (cdr rs)))
          ((null? rs))
        ;; changed infix->prefix to simp-rule - norvig Jun 11 1996
        (let ((rule (simp-rule (car rs))))
          (hashtable-set! int-proc (exp-op (exp-lhs (exp-lhs rule))) rule))))
    
    (define (in-integral-table? exp)
      (and (exp? exp) (hashtable-ref int-proc (exp-op exp) #f)))
    
    (define (integrate-from-table op arg)
      (let ((rule (hashtable-ref int-proc op #f)))
        (subst arg (exp-lhs (exp-lhs (exp-lhs rule))) (exp-rhs rule))))
    
    (set-simp-fn 'int (lambda (exp)
                        (unfactorize
                          (factorize
                            (integrate (exp-lhs exp) (exp-rhs exp))))))
    
    ;; Configure simplification rules
    
    (*simplification-rules*
      (map simp-rule
           '(;
             (x + 0             = x)
             (0 + x             = x)
             (x + x             = 2 * x)
             (x - 0             = x)
             (0 - x             = - x)
             (x - x             = 0)
             (- - x             = x)
             (x * 1             = x)
             (1 * x             = x)
             (x * 0             = 0)
             (0 * x             = 0)
             (x * x             = x ^ 2)
             (x / 0             = undefined)
             (0 / x             = 0)
             (x / 1             = x)
             (x / x             = 1)
             (0 ^ 0             = undefined)
             (x ^ 0             = 1)
             (0 ^ x             = 0)
             (1 ^ x             = 1)
             (x ^ 1             = x)
             (x ^ -1            = 1 / x)
             (x * (y / x)       = y)
             ((y / x) * x       = y)
             ((y * x) / x       = y)
             ((x * y) / x       = y)
             (x + - x           = 0)
             ((- x) + x         = 0)
             (x + y - x         = y)
             ;
             (s * n             = n * s)
             (n * (m * x)       = (n * m) * x)
             (x * (n * y)       = n * (x * y))
             ((n * x) * y       = n * (x * y))
             (n + s             = s + n)
             ((x + m) + n       = x + n + m)
             (x + (y + n)       = (x + y) + n)
             ((x + n) + y       = (x + y) + n)
             ;
             (log 1             = 0)
             (log 0             = undefined)
             (log e             = 1)
             (sin 0             = 0)
             (sin pi            = 0)
             (cos 0             = 1)
             (cos pi            = -1)
             (sin(pi / 2)       = 1)
             (cos(pi / 2)       = 0)
             (log (e ^ x)       = x)
             (e ^ (log x)       = x)
             ((x ^ y) * (x ^ z) = x ^ (y + z))
             ((x ^ y) / (x ^ z) = x ^ (y - z))
             (log x + log y     = log(x * y))
             (log x - log y     = log(x / y))
             ((sin x) ^ 2 + (cos x) ^ 2 = 1)
             ;
             (d x / d x         = 1)
             (d (u + v) / d x   = (d u / d x) + (d v / d x))
             (d (u - v) / d x   = (d u / d x) - (d v / d x))
             (d (- u) / d x     = - (d u / d x))
             (d (u * v) / d x   = u * (d v / d x) + v * (d u / d x))
             (d (u / v) / d x   = (v * (d u / d x) - u * (d v / d x)) / v ^ 2)
             (d (u ^ n) / d x   = n * u ^ (n - 1) * (d u / d x))
             (d (u ^ v) / d x   = v * u ^ (v - 1) * (d u / d x) + u ^ v * (log u) * (d v / d x))
             (d (log u) / d x   = (d u / d x) / u)
             (d (sin u) / d x   = (cos u) * (d u / d x))
             (d (cos u) / d x   = - (sin u) * (d u / d x))
             (d (e ^ u) / d x   = (e ^ u) * (d u / d x))
             (d u / d x         = 0))))
     
    ;; Configure integration table
    
    (integration-table
      '((Int log(x) d x   = x * log(x) - x)
        (Int exp(x) d x   = exp(x))
        (Int sin(x) d x   = - cos(x))
        (Int cos(x) d x   = sin(x))
        (Int tan(x) d x   = - log(cos(x)))
        (Int sinh(x) d x  = cosh(x))
        (Int cosh(x) d x  = sinh(x))
        (Int tanh(x) d x  = log(cosh(x)))))
  )
)
