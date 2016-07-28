;;; HashTables.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2016 ObjectHub. All rights reserved.
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
  "Eq hash tables"
  (3 #t #t 1 2 3 0)
  (define eqh (make-eq-hashtable))
  (define firstkey "one")
  (define key1 firstkey)
  (define key2a "two")
  (define key2b (list->string '(#\t #\w #\o)))
  (hashtable-set! eqh key1 1)
  (hashtable-set! eqh key2a 2)
  (hashtable-set! eqh key2b 3)
  (list (hashtable-size eqh)
        (eq? (hashtable-equivalence-function eqh) eq?)
        (eq? (hashtable-hash-function eqh) #f)
        (hashtable-ref eqh key1 0)
        (hashtable-ref eqh key2a 0)
        (hashtable-ref eqh key2b 0)
        (hashtable-ref eqh "three" 0))
)

(
  "Eqv hash tables"
  (3 #t #t 1 4 3 0)
  (define eqvh (make-eqv-hashtable 101))
  (define key1 firstkey)
  (define key2a "two")
  (define key2b (list->string '(#\t #\w #\o)))
  (hashtable-set! eqvh key1 1)
  (hashtable-set! eqvh key2a 2)
  (hashtable-set! eqvh key2b 3)
  (hashtable-set! eqvh key2a 4)
  (list (hashtable-size eqvh)
        (eq? (hashtable-equivalence-function eqvh) eqv?)
        (eq? (hashtable-hash-function eqvh) #f)
        (hashtable-ref eqvh key1 0)
        (hashtable-ref eqvh key2a 0)
        (hashtable-ref eqvh key2b 0)
        (hashtable-ref eqvh "three" 0))
)

(
  "Equal hash tables"
  (2 #t #t 4 3 3 0)
  (define equalh (make-equal-hashtable 51))
  (define key1 firstkey)
  (define key2a "two")
  (define key2b (list->string '(#\t #\w #\o)))
  (hashtable-set! equalh key1 1)
  (hashtable-set! equalh key2a 2)
  (hashtable-set! equalh key2b 3)
  (hashtable-set! equalh "one" 4)
  (list (hashtable-size equalh)
        (eq? (hashtable-equivalence-function equalh) equal?)
        (eq? (hashtable-hash-function equalh) #f)
        (hashtable-ref equalh key1 0)
        (hashtable-ref equalh key2a 0)
        (hashtable-ref equalh key2b 0)
        (hashtable-ref equalh "three" 0))
)

(
  "Custom hash tables"
  (2 #t #t #t (("two" . 3) ("one" . 4)) 4 3 3 0)
  (define (my-equal-hash x) (equal-hash x))
  (define (my-equal? x y) (equal? x y))
  (define h (make-hashtable my-equal-hash my-equal?))
  (define key1 firstkey)
  (define key2a "two")
  (define key2b (list->string '(#\t #\w #\o)))
  (hashtable-set! h key1 1)
  (hashtable-set! h key2a 2)
  (hashtable-set! h key2b 3)
  (hashtable-set! h "one" 4)
  (list (hashtable-size h)
        (eq? (hashtable-equivalence-function h) my-equal?)
        (eq? (hashtable-hash-function h) my-equal-hash)
        (equal? h (hashtable-copy h #t))
        (hashtable->alist h)
        (hashtable-ref h key1 0)
        (hashtable-ref h key2a 0)
        (hashtable-ref h key2b 0)
        (hashtable-ref h "three" 0))
)

(
  "Eq hash table update"
  101
  (hashtable-update! eqh firstkey (lambda (n) (+ n 100)) 0)
  (hashtable-ref eqh firstkey 0)
)

(
  "Eq hash table second update"
  (3 #t #f 201)
  (hashtable-update! eqh firstkey (lambda (n) (+ n 100)) 0)
  (list (hashtable-size eqh)
        (hashtable-contains eqh firstkey)
        (hashtable-contains eqh "four")
        (hashtable-ref eqh firstkey 0))
)

(
  "Custom hash table update"
  (2 (("two" . 3) ("one" . 104)) #t #f 104)
  (hashtable-update! h firstkey (lambda (n) (+ n 100)) 0)
  (list (hashtable-size h)
        (hashtable->alist h)
        (hashtable-contains h firstkey)
        (hashtable-contains h "three")
        (hashtable-ref h firstkey 0))
)
