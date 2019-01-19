;;; Vectors.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2019 ObjectHub. All rights reserved.
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
  "Basic vectors"
  (3 3 3 0 1 2 3 #(1 2 3) #(1 2 3) #(1 2 3) #() #(1) #t #t #t #f #f #f #t)
  (define v0 #(1 2 3))
  (define v1 (vector 1 2 3))
  (define v2 (make-vector 3))
  (vector-set! v2 0 1)
  (vector-set! v2 1 7)
  (vector-set! v2 2 3)
  (vector-set! v2 1 2)
  (define v3 (vector))
  (define v4 (immutable-vector 1))
  (list (vector-length v0)
        (vector-length v1)
        (vector-length v2)
        (vector-length v3)
        (vector-ref v0 0)
        (vector-ref v1 1)
        (vector-ref v2 2)
        v0
        v1
        v2
        v3
        v4
        (vector? v1)
        (vector? v4)
        (immutable-vector? v0)
        (immutable-vector? v1)
        (immutable-vector? v2)
        (immutable-vector? v3)
        (immutable-vector? v4))
)

(
  "Vector operations"
  (#(a b h i j f g h i j) #(x x x y y y y x x x) #(a b c d e f g h i j) #(f g h i) #f #t (f g h i))
  (define v5 (vector 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j))
  (define v7 (vector-copy v5 #f))
  (define v6 (make-vector 10 'x))
  (vector-fill! v6 'y 3 7)
  (vector-copy! v5 2 v5 7)
  (define v8 (vector-copy v7 5 9))
  (list v5 v6 v7 v8 (immutable-vector? v5) (immutable-vector? v7) (vector->list v8))
)

(
  "Character vectors"
  ("abcD" "bc" #(#\h #\e #\l #\l #\o) #(#\l #\l #\o) "abcDabcD" #t #f)
  (define v9 (list->vector (list #\a #\b #\c #\D)))
  (define v10 (list->vector (append (list #\a #\b #\c #\D) (list #\a #\b #\c #\D))))
  (define v11 (vector-append v9 v9))
  (list (vector->string v9)
        (vector->string v9 1 3)
        (string->vector "hello")
        (string->vector "hello world" 2 5)
        (vector->string v10)
        (equal? v10 v11)
        (equal? v9 v10))
)

(
  "Vector syntax"
  (#(5 7 9) #(0 1 4 9 16))
  (list (vector-map + #(1 2 3) ’#(4 5 6 7))
        (let ((v (make-vector 5 0)))
          (vector-for-each
            (lambda (i) (vector-set! v i (* i i)))
            ’#(0 1 2 3 4))
            v))
)

(
  "Basic gvectors"
  (#g(1 2 3) #g(2) #g(x 2 3 4) #g(1 2 3 2 x 2 3 4) #g(s 3 2 1 3 2 1) #g(1 2 3 5 6 7 8)
   #(1 7 2 3 6 5 8)
   #t #f #f #t (2) 2)
  (import (lispkit gvector))
  (define gv (make-gvector 50))
  (gvector-add! gv 1)
  (gvector-add! gv 2)
  (define gv2 (gvector-copy gv))
  (gvector-add! gv 3)
  (gvector-remove! gv2 0)
  (define gv3 (list->gvector '(1 2 3 4 5)))
  (gvector-remove-last! gv3)
  (gvector-set! gv3 0 'x)
  (define gv4 (gvector-append gv gv2 gv3))
  (define gv5 (gvector-concatenate (list gv gv)))
  (gvector-reverse! gv5)
  (gvector-insert! gv5 0 's)
  (define gv6 (gvector-copy gv))
  (gvector-insert! gv6 1 7)
  (gvector-append! gv6 #(6 5) #(8))
  (define gv7 (vector-copy gv6 #f))
  (gvector-sort! < gv6)
  (list gv gv2 gv3 gv4 gv5 gv6 gv7
        (gvector? gv6)
        (gvector? gv7)
        (immutable-vector? gv6)
        (immutable-vector? gv7)
        (vector->list gv2)
        (gvector-ref gv3 1))
)

(
  "Vector equality"
  (#t #t #t #f #f #f #f #t #t)
  (list (vector= eq?)
        (vector= eq? #(1 2))
        (vector= eq? #(1 2 3) #(1 2 3))
        (vector= eq? #(1 2) #(1 2 3))
        (vector= eq? #(1 2 3) #(1 2))
        (vector= eq? #(1 2 3) #() #(1 2 3))
        (vector= eq? #("one" "two") #("one" "two"))
        (vector= string=? #("one" "two") #("one" "two"))
        (vector= string=? #("one" "two") #("one" "two") #("one" "two")))
)
