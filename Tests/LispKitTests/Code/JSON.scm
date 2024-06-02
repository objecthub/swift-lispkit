;;; Format.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2024 ObjectHub. All rights reserved.
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
  "Simple JSON constructors"
  (() 1 2.7 "test" "foo" #t #f #(1 2 3 4) ((a . 0)))
  (import (lispkit json))
  (list (json->value (json '()))
        (json->value (json 1))
        (json->value (json 2.7))
        (json->value (json "test"))
        (json->value (json 'foo))
        (json->value (json #t))
        (json->value (json #f))
        (json->value (json #(1 2 3 4)))
        (json->value (json '((a . 0)))))
)

(
  "Record to JSON conversion"
  (#t #t)
  (define-record-type <point>
    (make-point x y)
    point?
    (x point-x set-point-x!)
    (y point-y set-point-y!))
  (define-record-type (<color-point> <point>)
    (make-color-point x y color)
    color-point?
    (color point-color))
  (define-record-type <color>
    (make-color red green blue)
    color?
    (red color-red)
    (green color-green)
    (blue color-blue))
  (define pt (make-point 1 2))
  (define cpt (make-color-point 3 4 (make-color 0.3 0.5 0.7)))
  (define ptj (json '((x . 1)(y . 2))))
  (define cptj (json '((x . 3)(y . 4)(color . ((red . 0.3)(green . 0.5)(blue . 0.7))))))
  (list (json=? (json pt) ptj)
        (json=? (json cpt) cptj))
)

(
  "JSON references"
  (#t #t #f #t #t #t #t #t "$[1]['foo']['bar'][2]" "$[1]['foo']['bar'][2]['goo']"
   "/1/foo/bar/2" "/foo/12/bar" (1 "foo" "bar" 2 "goo"))
  (list (json-pointer? "/foo/12/bar/-/1")
        (json-location? "$[1].foo.bar[2]")
        (json-location? "/foo")
        (json-reference? "$[1][2].bar")
        (json-reference? "/1/2/bar")
        (json-reference? 3)
        (json-reference? 'test)
        (json-reference? '(1 2 bar foo))
        (json-location '(1 foo bar 2))
        (json-location "$[1].foo.bar[2]['goo']")
        (json-pointer '(1 foo bar 2))
        (json-pointer "/foo/12/bar")
        (json-reference-segments "$[1].foo.bar[2]['goo']"))
)

(
  "JSON data access"
  (#t #f #t #t #t #t (array boolean number object string) (2 3 4) #t)
  (define j (json '((number . 1)
                    (string . "foo")
                    (boolean . #t)
                    (array . #(2 3 4))
                    (object . ((foo . #(null ()))
                               (bar . ((ten . 10)
                                       (eleven . #(1.1 2.2)))))))))
  (define (symbol<=? a b)
    (string<=? (symbol->string a) (symbol->string b)))
  (define (json->values s)
    (map json->value s))
  (define k (mutable-json j))
  (json-remove! k 'array 'object)
  (define l (json '((number . 1)
                    (string . "foo")
                    (boolean . #t))))
  (list (json-object? (json-ref j "$.object.bar"))
        (json-object? (json-ref j "$.object.foo"))
        (json-object? (json-ref j "$.object"))
        (json-array? (json-ref j "$.object.foo"))
        (json-array? (json-ref j 'array))
        (json-number? (json-ref j '(array 2)))
        (sort symbol<=? (json-members j))
        (sort < (json->values (json-children (json-ref j 'array))))
        (json=? k l))
)

(
  "JSON mutations"
  (#t #t #t)
  (define mj (mutable-json '()))
  (json-set! mj 'first (json 1))
  (json-set! mj "$.second" (json "2nd"))
  (json-set! mj "/third" (json (make-vector 3 'null)))
  (define j (json '((first . 1) (second . "2nd") (third . #(null null null)))))
  (define fst (json=? mj j))
  (json-insert! mj "$.third" 0 (json "zero") (json "one"))
  (json-append! mj 'third (json "last"))
  (define snd (json-remove! mj '(second sub) '(first) '(third 3) '(third 10) '(third x) '(third 3)))
  (set! j (json '((second . "2nd") (third . #("zero" "one" null "last")))))
  (list fst snd (json=? mj j))
)

(
  "JSON predicates"
  (#t #t #t #t #t #t #t #t #t #t)
  (list (json-null? (json 'null))
        (json-object? (json '()))
        (json-number? (json 1))
        (json-number? (json 2.7))
        (json-string? (json "test"))
        (json-string? (json 'foo))
        (json-boolean? (json #t))
        (json-boolean? (json #f))
        (json-array? (json #(1 2 3 4)))
        (json-object? (json '((a . 0)))))
)
