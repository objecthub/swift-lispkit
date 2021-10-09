;;; PAIP Pattern Matching Library
;;; 
;;; Library `(paip patmatch)` implements the pattern matcher described in chapter 6.2
;;; of Peter Norvig's book "Paradigms of Artificial Intelligence Programming: Case
;;; Studies in Common Lisp" (1992). It was ported from the original Common Lisp code.
;;; 
;;; Examples:
;;; 
;;;   > (pat-match '(i need a ?x) '(i need a vacation))
;;;   ((?x . vacation))
;;;   > (pat-match '(i need a ?x) '(i really need a vacation))
;;;   #f
;;;   > (pat-match '(this is easy) '(this is easy))
;;;   ()
;;;   > (pat-match '(?x is ?x) '((2 + 2) is 4))
;;;   #f
;;;   > (pat-match '(?x is ?x) '((2 + 2) is (2 + 2)))
;;;   ((?x 2 + 2))
;;;   > (pat-match '(?p need . ?x) '(i need a long vacation))
;;;   ((?x a long vacation) (?p . i))
;;;   > (pat-match '((?* ?p) need (?* ?x)) '(Mr Hulot and I need a vacation))
;;;   ((?x a vacation) (?p Mr Hulot and I))
;;;   > (pat-match '((?* ?x) is a (?* ?y)) '(what he is is a fool))
;;;   ((?y fool) (?x what he is))
;;;   > (pat-match '((?* ?x) a b (?* ?x)) '(1 2 a b a b 1 2 a b))
;;;   ((?x 1 2 a b))
;;;   > (pat-match '((?* ?x) like ?y best (?* ?z))
;;;                '(I really like playing like the best players in the world))
;;;   ((?z players in the world) (?y . the) (?x I really like playing))
;;; 
;;; Author of Scheme implementation: Matthias Zenger
;;; 
;;; Original code from Paradigms of AI Programming
;;; File pat-match.lisp: Pattern matcher from section 6.2
;;; Two bug fixes By Richard Fateman, rjf@cs.berkeley.edu, October 92.
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

(define-library (paip patmatch)
  
  (export pat-match
          pat-match-abbrev
          expand-pat-match-abbrev
          rule-based-translator)
  
  (import (lispkit base)
          (paip util))
  
  (begin
    
    ;; Is x a variable (a symbol beginning with `?')?
    (define (variable? x)
      (and (symbol? x) (string-prefix? (symbol->string x) "?")))
    
    ;; Does VAR match input? Uses (or updates) and returns bindings.
    (define (match-variable var input bindings)
      (let ((binding (assoc var bindings)))
        (or (and (not binding) (cons (cons var input) bindings))
            (and (equal? input (cdr binding)) bindings))))
    
    ;; Match pattern against input in the context of the bindings
    (define (pat-match pattern input . args)
      (let-optionals args ((bindings '()))
        (cond ((not bindings)
                #f)
              ((variable? pattern)
                (match-variable pattern input bindings))
              ((eqv? pattern input)
                bindings)
              ((segment-pattern? pattern)
                (segment-matcher pattern input bindings))
              ((single-pattern? pattern)                  ; ***
                (single-matcher pattern input bindings))  ; ***
              ((and (pair? pattern) (pair? input))
                (pat-match (cdr pattern)
                           (cdr input)
                           (pat-match (car pattern) (car input) bindings)))
              (else
                #f))))
    
    ;; Is this a segment-matching pattern like ((?* var) . pat)?
    (define (segment-pattern? pattern)
      (and (pair? pattern) (pair? (car pattern))
           (symbol? (car (car pattern)))
           (segment-match (car (car pattern)))))
    
    ;; Is this a single-matching pattern? e.g.
    ;; (?is x predicate) (?and . patterns) (?or . patterns).
    (define (single-pattern? pattern)
      (and (pair? pattern)
           (single-match (car pattern))))
    
    ; Segment-match functions
    (define segment-match (make-proc-map))
    
    ;; Call the right function for this kind of segment pattern.
    (define (segment-matcher pattern input bindings)
      ((segment-match (car (car pattern))) pattern input bindings))
    
    ; Single-match functions
    (define single-match (make-proc-map))
    
    ;; Call the right function for this kind of single pattern.
    (define (single-matcher pattern input bindings)
      ((single-match (car pattern)) (cdr pattern) input bindings))
        
    ;; Succeed and bind var if the input satisfies pred, where var-and-pred is
    ;; the list (var pred).
    (define (match-is var-and-pred input bindings)
      (let* ((var (car var-and-pred))
             (pred (cadr var-and-pred))
             (new-bindings (pat-match var input bindings)))
        (and new-bindings (pred input) new-bindings)))
    
    ;; Succeed if all the patterns match the input.
    (define (match-and patterns input bindings)
      (and bindings
           (if (pair? patterns)
               bindings
               (match-and (cdr patterns) input (pat-match (car patterns) input bindings)))))
    
    ;; Succeed if any one of the patterns match the input.
    (define (match-or patterns input bindings)
      (and (pair? patterms)
           (or (pat-match (car patterns) input bindings)
               (match-or (cdr patterns) input bindings))))
    
    ;; Succeed if none of the patterns match the input. This will never bind any variables.
    (define (match-not patterns input bindings)
      (and (not (match-or patterns input bindings)) bindings))
    
    ;; Match the segment pattern ((?* var) . pat) against input.
    (define (segment-match* pattern input bindings . args)
      (let-optionals args ((start 0))
        (let ((var (cadar pattern))
              (pat (cdr pattern)))
          (if (null? pat)
              (match-variable var input bindings)
              (let ((i (first-match-pos (car pat) input start)))
                (and i
                     (or (pat-match pat (list-tail input i)
                                        (match-variable var (take input i) bindings))
                         (segment-match* pattern input bindings (+ i 1)))))))))
    
    ;; Find the first position that pat1 could possibly match input, starting at position start.
    ;; If pat1 is non-constant, then just return start.
    (define (first-match-pos pat1 input start)
      (if (or (pair? pat1) (variable? pat1))
          (if (<= start (length input)) start #f)
          (position pat1 input start)))
    
    ;; Match one or more elements of input.
    (define (segment-match+ pattern input bindings)
      (segment-match* pattern input bindings 1))
    
    ;; Match zero or one element of input.
    (define (segment-match? pattern input bindings)
      (let ((var (cadar pattern))
            (pat (cdr pattern)))
        (or (pat-match (cons var pat) input bindings)
            (pat-match pat input bindings))))
    
    ;; Test an arbitrary expression involving variables. The pattern looks
    ;; like ((?if code) . rest).
    (define (match-if pattern input bindings)
      (and (eval (list 'let
                       (map (lambda (b) (list (car b) (list 'quote (cdr b)))) bindings)
                       (cadar pattern))
                 (interaction-environment))
           (pat-match (cdr pattern) input bindings)))
    
    (define expand-pat-match-abbrev-cache (make-eq-hashtable))
    
    ;; Define symbol as a macro standing for a pat-match pattern.
    (define (pat-match-abbrev symbol expansion)
      (hashtable-set! expand-pat-match-abbrev-cache symbol (expand-pat-match-abbrev expansion)))
    
    ;; Expand out all pattern matching abbreviations in pat.
    (define (expand-pat-match-abbrev pat)
      (or (hashtable-ref expand-pat-match-abbrev-cache pat #f)
          (and (not (pair? pat)) pat)
          (cons (expand-pat-match-abbrev (car pat))
                (expand-pat-match-abbrev (cdr pat)))))
    
    ;; Find the first rule in rules that matches input, and apply the action to that rule.
    (define (rule-based-translator input rules . args)
      (let-optionals args ((matcher pat-match)
                           (rule-if car)
                           (rule-then cdr)
                           (action sublis))
        (fold-left
          (lambda (res rule)
            (or res (let ((result (matcher (rule-if rule) input)))
                      (and result (action result (rule-then rule))))))
          #f
          rules)))
    
    ;; Configure the procedure maps
    
    (single-match '?is match-is)
    (single-match '?or match-or)
    (single-match '?and match-and)
    (single-match '?not match-not)
    
    (segment-match '?* segment-match*)
    (segment-match '?+ segment-match+)
    (segment-match '?? segment-match?)
    (segment-match '?if match-if)
  )
)
