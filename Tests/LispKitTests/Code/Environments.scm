;;; Environments.scm
;;; Regression test data
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2021 ObjectHub. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;      http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(
  "Basic environments"
  (#t #f #t #f #t "a" "b" (a b) ((a "a")(b "b")))
  (define (esort xs)
    (sort (lambda (x y) (string<? (symbol->string (car x)) (symbol->string (car y)))) xs))
  (define (nsort xs)
    (sort (lambda (x y) (string<? (symbol->string x) (symbol->string y))) xs))
  (define e (environment))
  (environment-define e 'a "a")
  (environment-define e 'b "c")
  (environment-assign! e 'b "b")
  (list (environment? e)
        (environment? 1)
        (environment-bound? e 'b)
        (environment-bound? e 'c)
        (environment-assignable? e 'a)
        (environment-lookup e 'a)
        (environment-lookup e 'b)
        (nsort (environment-bound-names e))
        (esort (environment-bindings e)))
)

(
  "Environments eval"
  (#f 17 30 #t)
  (define e1 (environment '(lispkit core) '(lispkit control)))
  (eval '(define (foo) 10) e1)
  (environment-import e1 '(lispkit math))
  (list (environment-bound? e1 'a)
        (eval '(+ (foo) 7) e1)
        (eval '(begin (define a (* (foo) 3)) a) e1)
        (environment-bound? e1 'a))
)

(
  "Definition documentation"
  ("a" "this is a" 8 "this is succ" "this is while" 1 "this is b" 2 #f 3 "this is d")
  (define a "a" "this is a")
  (define (succ n) "this is succ" (+ n 1))
  (define-values (b "this is b" c d "this is d") (values 1 2 3))
  (define-syntax while "this is while"
    (syntax-rules ()
      ((while condition body ...) (let loop () (if condition (begin body ... (loop)) #f)))))
  (list (environment-lookup (interaction-environment) 'a)
        (environment-documentation (interaction-environment) 'a)
        ((environment-lookup (interaction-environment) 'succ) 7)
        (environment-documentation (interaction-environment) 'succ)
        (environment-documentation (interaction-environment) 'while)
        (environment-lookup (interaction-environment) 'b)
        (environment-documentation (interaction-environment) 'b)
        (environment-lookup (interaction-environment) 'c)
        (environment-documentation (interaction-environment) 'c)
        (environment-lookup (interaction-environment) 'd)
        (environment-documentation (interaction-environment) 'd))
)
