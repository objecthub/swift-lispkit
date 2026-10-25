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

(
  "JSON conversion of hash tables"
  (((a . 1) (b . "two") (c . #(1 2))) #t)
  (import (lispkit hashtable))
  (define ht (make-equal-hashtable))
  (hashtable-set! ht "a" 1)
  (hashtable-set! ht 'b "two")
  (hashtable-set! ht 'c (vector 1 2))
  (list (sort (lambda (x y) (string<? (symbol->string (car x)) (symbol->string (car y))))
              (json->value (json ht)))
        (json=? (json ht) (json '((a . 1) (b . "two") (c . #(1 2))))))
)

(
  "JSON path queries return results in document order"
  ((10 20 30 40) (("items" 1 "n") ("items" 2 "n") ("items" 3 "n")) (10 20 30 40) (("items" 0 "n") ("items" 3 "n")))
  (define doc
    (string->json "{\"items\":[{\"n\":10},{\"n\":20},{\"n\":30},{\"n\":40}]}"))
  (list (map json->value (json-query-results doc "$.items[*].n"))
        (json-query-locations doc "$.items[?@.n > 10].n")
        (map (lambda (r) (json->value (car r))) (json-query doc "$..n"))
        (json-query-locations doc "$.items[0,3].n"))
)

(
  "JSON references with negative array indices"
  (3 1 #f 3 3 #f)
  (define arr (json #(1 2 3)))
  (define obj (json '((l . #(1 2 3)))))
  (list (json->value (json-ref arr -1))
        (json->value (json-ref arr -3))
        (json-ref arr -4)
        (json->value (json-ref obj 'l -1))
        (json->value (json-ref obj '(l -1)))
        (json-ref obj 'l -5))
)

(
  "JSON merge of conflicting values"
  (#f #f #f #f #f #t #t #t)
  (define (merge a b) (json-merge (json a) (json b)))
  (list (merge 1 2)
        (merge "a" "b")
        (merge #(1 2) #(1 3))
        (merge 1 "1")
        (merge '((a . 1) (b . #(1 2))) '((a . 1) (b . #(1 3))))
        (json=? (merge 1 1) (json 1))
        (json=? (merge #(1 2) #(1 2)) (json #(1 2)))
        (json=? (merge '((a . 1)) '((b . 2))) (json '((a . 1) (b . 2)))))
)

(
  "CBOR conversion of null values"
  (#t #t #t #t)
  (define (roundtrip x) (json=? (cbor->json (json->cbor x)) x))
  (list (roundtrip (json 'null))
        (roundtrip (json #(1 null "a")))
        (roundtrip (json '((a . null) (b . #(null 1 null)) (c . ((d . null))))))
        (roundtrip (json #(#(null) #() ((e . null))))))
)

(
  "JSON schema validation results contain nested tags"
  (5 (("$['id']" read-only)
      ("$['list'][0]" deprecated)
      ("$['list'][1]" deprecated)
      ("$['old']" deprecated)
      ("$['secret']" write-only)))
  (import (lispkit json schema))
  (define schema
    (json-schema
      (string->json
        (string-append
          "{\"type\":\"object\",\"properties\":{"
          "\"old\":{\"deprecated\":true},"
          "\"id\":{\"readOnly\":true},"
          "\"secret\":{\"writeOnly\":true},"
          "\"list\":{\"items\":{\"deprecated\":true}},"
          "\"plain\":{}}}"))))
  (define res
    (json-validate
      (string->json "{\"old\":1,\"id\":2,\"secret\":3,\"list\":[1,2],\"plain\":4}")
      schema))
  (list (length (validation-result-tags res))
        (sort (lambda (x y) (string<? (car x) (car y)))
              (map (lambda (tag) (cons (cdar tag) (caddr tag)))
                   (validation-result-tags res))))
)

(
  "JSON schema validation results report failing oneOf and anyOf"
  ("$['oneOf']" "$['anyOf']" "$['oneOf']" #t)
  (import (lispkit json schema))
  (define (rule-locations schema-text value)
    (map (lambda (err) (cdadr err))
         (validation-result-errors
           (json-validate (json value) (json-schema (string->json schema-text))))))
  (define one-of "{\"oneOf\":[{\"type\":\"string\"},{\"type\":\"integer\"}]}")
  (define any-of "{\"anyOf\":[{\"type\":\"string\"},{\"type\":\"integer\"}]}")
  (list (car (rule-locations one-of #t))
        (car (rule-locations any-of #t))
        (car (rule-locations "{\"oneOf\":[{\"type\":\"integer\"},{\"minimum\":0}]}" 5))
        (null? (rule-locations one-of 5)))
)
