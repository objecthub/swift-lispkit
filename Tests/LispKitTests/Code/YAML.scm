;;; YAML.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 ObjectHub. All rights reserved.
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
  "Imports and helpers"
  #t
  (import (lispkit yaml)
          (lispkit type)
          (lispkit hashtable)
          (lispkit gvector)
          (only (lispkit system) system-directory path file-exists? delete-file-or-directory))
  ;; Returns #t if calling `thunk` raises an error
  (define (call-raises? thunk)
    (guard (x (#t #t)) (thunk) #f))
  ;; Returns #t if calling `thunk` raises an error whose message contains `text`
  (define (call-raises-with? thunk text)
    (guard (x ((error-object? x) (and (string-contains? (error-object-message x) text) #t))
              (#t #f))
      (thunk)
      #f))
  ;; Evaluates to #t if evaluating `expr` raises an error
  (define-syntax raises?
    (syntax-rules ()
      ((_ expr) (call-raises? (lambda () expr)))))
  ;; Evaluates to #t if evaluating `expr` raises an error whose message contains `text`
  (define-syntax raises-with
    (syntax-rules ()
      ((_ expr text) (call-raises-with? (lambda () expr) text))))
  ;; Collects the kinds of all events produced by a pull parser
  (define (parser-kinds parser)
    (let loop ((acc '()))
      (let ((event (yaml-parser-next! parser)))
        (if (eof-object? event) (reverse acc) (loop (cons (yaml-event-kind event) acc))))))
  ;; A sample document used by several tests
  (define sample
    (string->yaml "name: demo\nversion: 1.5\nflags: [true, null, 3]\nnested:\n  list:\n    - a\n    - b: 2\n1: one\n"))
  #t
)

(
  "Library type tags"
  ((#t #t #t #t #t) #t)
  (list (map (lambda (obj tag) (equal? (type-of obj) (list tag)))
             (list (yaml 1)
                   (mutable-yaml 1)
                   (make-yaml-event 'stream-start)
                   (make-yaml-parser "a")
                   (make-yaml-emitter))
             (list yaml-type-tag
                   mutable-yaml-type-tag
                   yaml-event-type-tag
                   yaml-parser-type-tag
                   yaml-emitter-type-tag))
        (not (equal? yaml-type-tag mutable-yaml-type-tag)))
)

;;; Constructors and conversion of Scheme values

(
  "Simple YAML constructors"
  (null 1 2.7 "test" "foo" #t #f #(1 2 3 4) ((a . 0)) () #())
  (list (yaml->value (yaml 'null))
        (yaml->value (yaml 1))
        (yaml->value (yaml 2.7))
        (yaml->value (yaml "test"))
        (yaml->value (yaml 'foo))
        (yaml->value (yaml #t))
        (yaml->value (yaml #f))
        (yaml->value (yaml #(1 2 3 4)))
        (yaml->value (yaml '((a . 0))))
        (yaml->value (yaml '()))
        (yaml->value (yaml (make-vector 0))))
)

(
  "YAML constructor without and with multiple arguments"
  (null #(1 2 3) #("a" #(1)))
  (list (yaml->value (yaml))
        (yaml->value (yaml 1 2 3))
        (yaml->value (yaml "a" #(1))))
)

(
  "Nested conversion"
  (((a . #(1 2)) (b (c . #t) (d . null))) #t)
  (let ((value '((a . #(1 2)) (b . ((c . #t) (d . null))))))
    (list (yaml->value (yaml value))
          (equal? (yaml->value (yaml value)) value)))
)

(
  "Tags of constructed values"
  ("!!null" "!!bool" "!!int" "!!float" "!!str" "!!str" "!!seq" "!!map" "!!int" "!!float")
  (map (lambda (x) (yaml-tag (yaml x)))
       (list 'null #t 1 2.5 "s" 'sym #(1) '((a . 1)) (expt 10 30) +inf.0))
)

(
  "Conversion of hash tables, growable vectors, records and void values"
  (((k . 1)) #(1 2 3) ((x . 1) (y . 2)) ((a . 1)))
  (define-record-type <pt> (make-pt x y) pt? (x pt-x) (y pt-y))
  (define table (make-equal-hashtable))
  (hashtable-set! table "k" 1)
  (list (yaml->value (yaml table))
        (yaml->value (yaml (gvector 1 2 3)))
        (yaml->value (yaml (make-pt 1 2)))
        (yaml->value (yaml (list (cons 'a 1) (cons 'b (if #f #f))))))
)

(
  "YAML values are passed through by the constructor"
  (#t #t #t #t)
  (let ((node (yaml 1))
        (mutable (mutable-yaml 1)))
    (list (eq? node (yaml node))
          (yaml? (yaml mutable) #t)
          (equal? (yaml->value (yaml mutable)) 1)
          (equal? (yaml->value (yaml (list (cons 'a node) (cons 'b mutable)))) '((a . 1) (b . 1)))))
)

(
  "Keys of mappings"
  ("1: one\n\"null\": n\nk: 2\n" ("a" "b") (1 "x"))
  (list (yaml->string (yaml (list (cons 1 "one") (cons 'null "n") (cons "k" 2))))
        (map yaml->value (yaml-keys (yaml '((a . 1) ("b" . 2)))))
        (map yaml->value (yaml-keys (yaml (list (cons 1 'x) (cons "x" 1))))))
)

(
  "Values that cannot be converted"
  (#t #t #t)
  (list (raises-with (yaml (lambda (x) x)) "cannot convert to YAML")
        (raises-with (yaml '(1 2 3)) "cannot convert to YAML")
        (raises-with (yaml->string (lambda (x) x)) "cannot convert to YAML"))
)

;;; Conversion to Scheme values

(
  "Conversion of YAML nodes to Scheme values"
  (null #t 12 1.5 "abc" #(1 "x") ((a . 1) (b . #(2 3))))
  (map (lambda (text) (yaml->value (string->yaml text)))
       '("~" "true" "12" "1.5" "abc" "[1, x]" "{a: 1, b: [2, 3]}"))
)

(
  "Numeric scalar conversions"
  (#(123456789012345678901234567890 15 255 5 -7 7 4722366482869645213695)
   #(1000.0 0.5 5.0 -0.015 +inf.0 -inf.0))
  (list (yaml->value
          (string->yaml "[123456789012345678901234567890, 0o17, 0xFF, +5, -7, 007, 0xFFFFFFFFFFFFFFFFFF]"))
        (yaml->value (string->yaml "[1e3, .5, 5., -1.5e-2, .inf, -.inf]")))
)

(
  "Default value of yaml->value for missing nodes"
  (null dflt dflt 1 3 #t)
  (list (yaml->value (yaml 'null))
        (yaml->value #f 'dflt)
        (yaml->value (yaml-ref sample 'nope) 'dflt)
        (yaml->value (yaml 1) 'dflt)
        (yaml->value (yaml-ref sample 'flags 2) 0)
        (raises? (yaml->value #f)))
)

(
  "Unknown tags are not interpreted by yaml->value"
  ("aGVsbG8=" "2001-12-14" "red")
  (list (yaml->value (string->yaml "!!binary aGVsbG8="))
        (yaml->value (string->yaml "!!timestamp 2001-12-14"))
        (yaml->value (string->yaml "!color red")))
)

(
  "Mappings with complex keys"
  (((#(1 2) . "v")))
  (list (yaml->value (string->yaml "? [1, 2]\n: v\n")))
)

;;; Predicates

(
  "Type predicates"
  (#t #f #t #f #t #f #t #f)
  (let ((m (mutable-yaml 1)))
    (list (yaml? sample)
          (yaml? 1)
          (yaml? m)
          (yaml? m #t)
          (mutable-yaml? m)
          (mutable-yaml? sample)
          (yaml? (yaml 1) #t)
          (mutable-yaml? 1)))
)

(
  "Node kind predicates"
  ((#t #f #f #f #t #f #f)
   (#f #t #f #f #t #f #f)
   (#f #f #t #f #t #f #f)
   (#f #f #t #f #t #f #f)
   (#f #f #f #t #t #f #f)
   (#f #f #f #f #f #t #f)
   (#f #f #f #f #f #f #t)
   (#f #f #f #t #t #f #f)
   (#t #f #f #f #t #f #f)
   (#f #f #f #t #t #f #f)
   (#f #f #f #t #t #f #f))
  (map (lambda (text)
         (let ((n (string->yaml text)))
           (list (yaml-null? n) (yaml-boolean? n) (yaml-number? n) (yaml-string? n)
                 (yaml-scalar? n) (yaml-sequence? n) (yaml-mapping? n))))
       '("null" "true" "1" "1.5" "abc" "[1]" "{a: 1}" "''" "~" "!!str 1" "'1'"))
)

(
  "Membership in mappings"
  (#t #t #f #t #f #f #t #f)
  (list (yaml-member? sample 'name)
        (yaml-member? sample "name")
        (yaml-member? sample 'nope)
        (yaml-member? sample 1)
        (yaml-member? (yaml #(1 2)) 1)
        (yaml-member? (yaml 1) 1)
        (yaml-member? sample (yaml "name"))
        (yaml-member? 1 1))
)

(
  "YAML equality"
  (#t #f #t #f #t)
  (list (yaml=? (yaml '((a . 1) (b . 2))) (yaml '((b . 2) (a . 1))))
        (yaml=? (yaml 1) (yaml "1"))
        (yaml=? (string->yaml "0x10") (string->yaml "16"))
        (yaml=? (yaml #(1 2)) (yaml #(2 1)))
        (yaml=? (yaml #(1 2)) (yaml #(1 2))))
)

;;; Access to node properties

(
  "Scalar text, tags, styles and anchors"
  (("1" "2.5" "true" "null" "s" #f) ("!!str" "!!int") (#f #f) #f)
  (list (list (yaml-scalar-text (yaml 1))
              (yaml-scalar-text (yaml 2.5))
              (yaml-scalar-text (yaml #t))
              (yaml-scalar-text (yaml 'null))
              (yaml-scalar-text (yaml "s"))
              (yaml-scalar-text (yaml #(1))))
        (list (yaml-tag (string->yaml "a")) (yaml-tag (string->yaml "12")))
        (list (yaml-style (yaml 1)) (yaml-style (yaml #(1))))
        (yaml-anchor (yaml 1)))
)

(
  "Styles of parsed nodes"
  (single-quoted literal folded double-quoted flow flow block plain)
  (map yaml-style
       (list (string->yaml "'a'")
             (string->yaml "|\n a")
             (string->yaml ">\n a")
             (string->yaml "\"a\"")
             (string->yaml "[a]")
             (string->yaml "{a: 1}")
             (string->yaml "- a")
             (string->yaml "a")))
)

(
  "Tags of parsed nodes"
  ("!color" "!!str" "!<tag:example.com,2000:x>" "!<tag:example.com,2000:foo>")
  (map (lambda (text) (yaml-tag (string->yaml text)))
       '("!color 'red'"
         "!!str 12"
         "!<tag:example.com,2000:x> v"
         "%TAG !e! tag:example.com,2000:\n---\n!e!foo bar"))
)

(
  "Source marks"
  ((1 1 0) (2 6 8) #f (1 1 0) (2 3 6))
  (list (yaml-mark (string->yaml "a:\n  b: 1"))
        (yaml-mark (yaml-ref (string->yaml "a:\n  b: 1") 'a 'b))
        (yaml-mark (yaml 1))
        (yaml-mark (string->yaml "- 1\n- 2"))
        (yaml-mark (yaml-ref (string->yaml "- 1\n- 2") 1)))
)

(
  "Anchors of parsed nodes"
  ("x" #f)
  (let ((doc (string->yaml "a: &x 1\nb: 2")))
    (list (yaml-anchor (yaml-ref doc 'a))
          (yaml-anchor (yaml-ref doc 'b))))
)

(
  "Functional presentation updates"
  ("a" #f #f "!!str" "!!str" single-quoted flow #f)
  (list (yaml-anchor (yaml-with-anchor (yaml 1) "a"))
        (yaml-anchor (yaml-with-anchor (yaml-with-anchor (yaml 1) "a") #f))
        (yaml-anchor (yaml 1))
        (yaml-tag (yaml-with-tag (yaml 1) "!!str"))
        (yaml-tag (yaml-with-tag (yaml 1) "tag:yaml.org,2002:str"))
        (yaml-style (yaml-with-style (yaml "x") 'single-quoted))
        (yaml-style (yaml-with-style (yaml #(1)) 'flow))
        (yaml-style (yaml-with-style (yaml 1) #f)))
)

(
  "Invalid presentation updates"
  (#t #t #t)
  (list (raises? (yaml-with-tag (yaml 1) 5))
        (raises-with (yaml-with-style (yaml 1) 'bogus) "not a valid YAML scalar style")
        (raises-with (yaml-with-style (yaml #(1)) 'literal) "not a valid YAML collection style"))
)

;;; Children, keys and entries

(
  "Children, keys, entries and counts"
  (("demo" 1.5 #(#t null 3) ((list . #("a" ((b . 2))))) "one")
   ("demo" 1.5 #(#t null 3) ((list . #("a" ((b . 2))))) "one")
   ("name" "version" "flags" "nested" 1)
   (#t null 3)
   (1 2)
   (("a" . 1) ("b" . #(2)))
   (5 3 0 0)
   (() ()))
  (list (map yaml->value (yaml-children sample))
        (map (lambda (e) (yaml->value (cdr e))) (yaml-entries sample))
        (map yaml->value (yaml-keys sample))
        (map yaml->value (yaml-children (yaml-ref sample 'flags)))
        (map yaml->value (yaml-children (yaml '((a . 1) (b . 2)))))
        (map (lambda (e) (cons (yaml->value (car e)) (yaml->value (cdr e))))
             (yaml-entries (yaml '((a . 1) (b . #(2))))))
        (list (yaml-children-count sample)
              (yaml-children-count (yaml #(1 2 3)))
              (yaml-children-count (yaml 1))
              (yaml-children-count (yaml '())))
        (list (yaml-children (yaml 1))
              (yaml-keys (yaml #(1)))))
)

(
  "Iterating over elements and entries"
  ((3 2 1) (("b" . 2) ("a" . 1)) #t #t)
  (list (let ((acc '()))
          (yaml-for-each-element (lambda (e) (set! acc (cons (yaml->value e) acc)))
                                 (yaml #(1 2 3)))
          acc)
        (let ((acc '()))
          (yaml-for-each-entry (lambda (k v)
                                 (set! acc (cons (cons (yaml->value k) (yaml->value v)) acc)))
                               (yaml '((a . 1) (b . 2))))
          acc)
        (raises? (yaml-for-each-element (lambda (e) #t) (yaml '((a . 1)))))
        (raises? (yaml-for-each-entry (lambda (k v) #t) (yaml #(1)))))
)

;;; References

(
  "Resolving references"
  ("demo" 1.5 3 2 #f #f #f "one" "a" #f)
  (list (yaml->value (yaml-ref sample 'name))
        (yaml->value (yaml-ref sample "version"))
        (yaml->value (yaml-ref sample 'flags 2))
        (yaml->value (yaml-ref sample 'nested 'list 1 'b))
        (yaml-ref sample 'nope)
        (yaml-ref sample 'flags 10)
        (yaml-ref sample 'flags -1)
        (yaml->value (yaml-ref sample (yaml 1)))
        (yaml->value (yaml-ref sample '(nested list 0)))
        (yaml-ref sample 'name 'x))
)

(
  "References into the empty path, indices and path lists"
  (#t "one" 1)
  (list (yaml=? sample (yaml-ref sample))
        (yaml->value (yaml-ref sample 1))
        (yaml->value (yaml-ref (yaml '((a . ((b . 1))))) '(a b))))
)

;;; Functional updates

(
  "Functional updates with yaml-replace"
  ("name: new\nversion: 1.5\nflags: [true, null, 3]\nnested:\n  list:\n    - a\n    - b: 2\n1: one\n"
   "- 1\n- x\n- 3\n"
   "name: demo\nversion: 1.5\nflags: [true, null, 3]\nnested:\n  list:\n    - a\n    - b: 99\n1: one\n"
   "a: 1\nz: 2\n"
   "- 1\n- 2\n- 3\n"
   "a: 10\nb: 20\n")
  (list (yaml->string (yaml-replace sample 'name "new"))
        (yaml->string (yaml-replace (yaml #(1 2 3)) 1 'x))
        (yaml->string (yaml-replace sample '(nested list 1 b) 99))
        (yaml->string (yaml-replace (yaml '((a . 1))) 'z 2))
        (yaml->string (yaml-replace (yaml #(1 2)) 2 3))
        (yaml->string (yaml-replace (yaml '((a . 1) (b . 2))) 'a 10 'b 20)))
)

(
  "yaml-replace does not modify its argument"
  ("a: 1\n" "a: 2\n")
  (let* ((node (yaml '((a . 1))))
         (changed (yaml-replace node 'a 2)))
    (list (yaml->string node) (yaml->string changed)))
)

(
  "Errors of yaml-replace"
  (#t #t #t 1)
  (list (raises-with (yaml-replace (yaml #(1 2)) 5 3) "cannot find YAML reference")
        (raises-with (yaml-replace (yaml 1) 'a 2) "cannot find YAML reference")
        (raises-with (yaml-replace (yaml '((a . 1))) '(a b) 3) "cannot find YAML reference")
        (yaml->value (yaml-replace (yaml 1) 5)))
)

;;; Mutable values

(
  "Mutable YAML values"
  (#t #f #t ((a . 1) (b . #(1 2 3))) "a: 1\nb:\n  - 1\n  - 2\n  - 3\n")
  (let ((m (mutable-yaml '((a . 1) (b . #(1 2 3))))))
    (list (mutable-yaml? m)
          (yaml? m #t)
          (yaml? m)
          (yaml->value m)
          (yaml->string m)))
)

(
  "Mutating YAML values"
  ((a . 10) (b . #("zero" 1 "two" 3 4)))
  (let ((m (mutable-yaml '((a . 1) (b . #(1 2 3))))))
    (yaml-set! m 'a 10)
    (yaml-set! m 'c "new")
    (yaml-set! m '(b 1) 'two)
    (yaml-append! m 'b 4)
    (yaml-insert! m 'b 0 'zero)
    (yaml-remove! m 'c)
    (yaml->value m))
)

(
  "Mutating nested YAML values"
  (((x (y (z . 2) (w . 3)))) 1 ((b . 2)) #(2 3) 5)
  (list (let ((m (mutable-yaml '((x . ((y . ((z . 1)))))))))
          (yaml-set! m '(x y z) 2)
          (yaml-set! m '(x y w) 3)
          (yaml->value m))
        (yaml->value (yaml-ref (mutable-yaml '((a . ((b . 1))))) 'a 'b))
        (let ((m (mutable-yaml '((a . 1) (b . 2) (c . 3)))))
          (yaml-remove! m 'a 'c)
          (yaml->value m))
        (let ((m (mutable-yaml #(1 2 3))))
          (yaml-remove! m 0)
          (yaml->value m))
        (let ((m (mutable-yaml 1)))
          (yaml-set! m '() 5)
          (yaml->value m)))
)

(
  "Appending and inserting multiple elements"
  (#(1 2 3 4) #(1 8 9 2 3))
  (list (let ((m (mutable-yaml #(1 2))))
          (yaml-append! m '() 3 4)
          (yaml->value m))
        (let ((m (mutable-yaml #(1 2 3))))
          (yaml-insert! m '() 1 8 9)
          (yaml->value m)))
)

(
  "Converting between immutable and mutable YAML values"
  (#f #f 1 #t)
  (let* ((y (yaml 1))
         (m1 (mutable-yaml y))
         (m2 (mutable-yaml m1))
         (m3 (mutable-yaml m1 #t)))
    (list (eq? m1 y)
          (eq? m1 m3)
          (yaml->value m3)
          (eq? m1 m2)))
)

(
  "Immutable YAML values are not affected by mutations of derived values"
  (1 2)
  (let* ((y (yaml '((a . 1))))
         (m (mutable-yaml y)))
    (yaml-set! m 'a 2)
    (list (yaml->value (yaml-ref y 'a))
          (yaml->value (yaml-ref m 'a))))
)

(
  "Errors of mutating procedures"
  (#t #t #t #t #t #t)
  (let ((m (mutable-yaml '((a . 1) (b . #(1 2 3))))))
    (list (raises-with (yaml-set! (yaml 1) 'a 1) "mutable-yaml")
          (raises-with (yaml-set! m '(b 99) 'x) "cannot find YAML reference")
          (raises-with (yaml-append! m 'a 1) "not a YAML sequence")
          (raises? (yaml-insert! m 'b 99 1))
          (raises-with (yaml-remove! m 'nope) "cannot find YAML reference")
          (raises-with (yaml-remove! m '(b 99)) "cannot find YAML reference")))
)

;;; Parsing

(
  "Parsing strings"
  ((a . 1) (b . #("x" "y")) (c . ((d . "e"))))
  (yaml->value
    (string->yaml "a: 1\nb:\n  - x\n  - y\nc:\n  d: e\n"))
)

(
  "Block, flow, literal and folded scalars"
  (("a b" "a\nb\n" "a b\n" "plain text" "quoted \"text\"\n" "it's")
   ((a . 1) (b . #(1 2))))
  (list (map (lambda (text) (yaml->value (string->yaml text)))
             '("'a b'" "|\n  a\n  b\n" ">\n  a\n  b\n" "plain\n  text" "\"quoted \\\"text\\\"\\n\"" "'it''s'"))
        (yaml->value (string->yaml "{a: 1, b: [1, 2]}")))
)

(
  "Comments and empty values"
  ((a . 1) (b . null) (c . null))
  (yaml->value (string->yaml "# comment\na: 1 # trailing\nb:\nc: ~\n"))
)

(
  "Single and multiple documents"
  (("a" "b" #(1)) () #f #f null ("a") (null "x"))
  (list (map yaml->value (string->yaml-documents "a\n---\nb\n---\n[1]\n"))
        (string->yaml-documents "")
        (string->yaml "")
        (string->yaml "# only comment\n")
        (yaml->value (string->yaml "---\n"))
        (map yaml->value (string->yaml-documents "a"))
        (map yaml->value (string->yaml-documents "--- \n--- x\n")))
)

(
  "Parsing multiple documents with string->yaml is an error"
  #t
  (raises-with (string->yaml "a: 1\n---\nb: 2") "single document")
)

(
  "Parsing bytevectors with different encodings"
  ("a" "a" "a" ((a . 1)) ((a . 1)) (1 2))
  (list (yaml->value (bytevector->yaml (string->utf8 "a")))
        (yaml->value (bytevector->yaml (bytevector #xEF #xBB #xBF #x61)))
        (yaml->value (bytevector->yaml (bytevector #xFF #xFE 0 0 #x61 0 0 0)))
        (yaml->value (bytevector->yaml (bytevector #xFF #xFE #x61 0 #x3A 0 #x20 0 #x31 0)))
        (yaml->value (bytevector->yaml (bytevector #xFE #xFF 0 #x61 0 #x3A 0 #x20 0 #x31)))
        (map yaml->value (bytevector->yaml-documents (string->utf8 "1\n---\n2"))))
)

(
  "Loading YAML files"
  (((k . #(1 2))) (((k . #(1 2)))) #f #t)
  (define yaml-test-dir (car (system-directory 'temporary)))
  (define yaml-test-file (path yaml-test-dir "lispkit-yaml-load-test.yaml"))
  (define yaml-test-empty (path yaml-test-dir "lispkit-yaml-load-test-empty.yaml"))
  (call-with-output-file yaml-test-file
    (lambda (port) (write-string "k: [1, 2]\n" port)))
  (call-with-output-file yaml-test-empty
    (lambda (port) (write-string "# nothing\n" port)))
  (define yaml-test-results
    (list (yaml->value (load-yaml yaml-test-file))
          (map yaml->value (load-yaml-documents yaml-test-file))
          (load-yaml yaml-test-empty)
          (raises? (load-yaml (path yaml-test-dir "lispkit-yaml-nonexistent-file.yaml")))))
  (delete-file-or-directory yaml-test-file)
  (delete-file-or-directory yaml-test-empty)
  yaml-test-results
)

(
  "Saving YAML files"
  ("a: 1\nb:\n  - x\n  - y\n"
   ((a . 1) (b . #("x" "y")))
   "a: 2\n"
   "{a: 1, b: [x, y]}\n"
   "é\n"
   "1\n--- two\n---\nk: v\n"
   (1 "two" ((k . "v")))
   #t #t)
  (define yaml-save-dir (car (system-directory 'temporary)))
  (define yaml-save-file (path yaml-save-dir "lispkit-yaml-save-test.yaml"))
  (define (yaml-save-text)
    (call-with-input-file yaml-save-file (lambda (port) (read-string 1000 port))))
  (define yaml-save-results
    (let* ((first (begin (save-yaml yaml-save-file '((a . 1) (b . #("x" "y"))))
                         (yaml-save-text)))
           (loaded (yaml->value (load-yaml yaml-save-file)))
           (replaced (begin (save-yaml yaml-save-file (yaml '((a . 2))))
                            (yaml-save-text)))
           (flow (begin (save-yaml yaml-save-file '((a . 1) (b . #("x" "y")))
                                   '((flow-style . #t)))
                        (yaml-save-text)))
           (unicode (begin (save-yaml yaml-save-file "é") (yaml-save-text)))
           (docs (begin (save-yaml-documents yaml-save-file (list 1 "two" '((k . v))))
                        (yaml-save-text)))
           (loaded-docs (map yaml->value (load-yaml-documents yaml-save-file)))
           (unchanged (begin (save-yaml yaml-save-file 1)
                             (raises? (save-yaml yaml-save-file (lambda (x) x)))
                             (string=? (yaml-save-text) "1\n")))
           (no-dir (raises? (save-yaml (path yaml-save-dir "lispkit-yaml-no-such-dir" "f.yaml") 1))))
      (list first loaded replaced flow unicode docs loaded-docs unchanged no-dir)))
  (delete-file-or-directory yaml-save-file)
  yaml-save-results
)

;;; Schemas and parse options

(
  "Scalar resolution with the core schema"
  ("!!bool" "!!int" "!!null" "!!float" "!!str" "!!str" "!!null" "!!str" "!!int" "!!float")
  (map yaml-tag (yaml-children (string->yaml "[true, 1, null, 2.5, abc, 'q', ~, '', 0x1f, .inf]")))
)

(
  "Scalar resolution with the JSON and failsafe schemas"
  (#(#t 1 null 2.5 "abc" "q")
   ("!!bool" "!!int" "!!null" "!!float" "!!str" "!!str")
   #("true" "1" "null" "2.5" "abc")
   ("!!str" "!!str" "!!str" "!!str" "!!str"))
  (let ((text "[true, 1, null, 2.5, abc, 'q']"))
    (list (yaml->value (string->yaml text '((schema . json))))
          (map yaml-tag (yaml-children (string->yaml text '((schema . json)))))
          (yaml->value (string->yaml "[true, 1, null, 2.5, abc]" '((schema . failsafe))))
          (map yaml-tag (yaml-children (string->yaml "[true, 1, null, 2.5, abc]"
                                                     '((schema . failsafe)))))))
)

(
  "Serialization with different schemas"
  ("\"true\"\n" "true\n" "\"true\"\n")
  (list (yaml->string (yaml "true"))
        (yaml->string (yaml "true") '((schema . failsafe)))
        (yaml->string (yaml "true") '((schema . json))))
)

(
  "Custom schema for YAML 1.1 booleans"
  (#(#t #f #t #f "y" #t 1) "- \"yes\"\n- \"no\"\n- maybe\n")
  (define (bool-schema text)
    (and (member text '("yes" "on" "no" "off")) "!!bool"))
  (list (yaml->value (string->yaml "[yes, no, on, off, y, true, 1]"
                                   (list (cons 'schema bool-schema))))
        (yaml->string (yaml #("yes" "no" "maybe")) (list (cons 'schema bool-schema))))
)

(
  "Custom schema with local tags"
  (("!ticket" "!!int" "!!str") "T-17\n")
  (define (ticket-schema text)
    (and (> (string-length text) 2) (string=? (substring text 0 2) "T-") "!ticket"))
  (list (map yaml-tag (yaml-children (string->yaml "[T-17, 17, x]"
                                                   (list (cons 'schema ticket-schema)))))
        (yaml->string (yaml-with-tag (yaml "T-17") "!ticket")
                      (list (cons 'schema ticket-schema))))
)

(
  "Errors in custom schema procedures"
  (#t #t #t)
  (list (raises-with (string->yaml "a" (list (cons 'schema (lambda (s) (error "boom"))))) "boom")
        (raises-with (string->yaml "a" (list (cons 'schema (lambda (s) 42))))
                     "tag string or #f")
        (raises? (yaml->string (yaml "a") (list (cons 'schema (lambda (s) (raise 'oops)))))))
)

(
  "Duplicate keys"
  (#t ((a . 1) (a . 2)))
  (list (raises-with (string->yaml "a: 1\na: 2") "duplicate mapping key")
        (yaml->value (string->yaml "a: 1\na: 2" '((duplicate-keys . #t)))))
)

(
  "Aliases and merge keys"
  (((base (x . 1) (y . 2)) (use (x . 1) (y . 2)) (merged (x . 1) (y . 3)))
   ((base (x . 1)) (merged (<< (x . 1)) (y . 3)))
   ((a (x . 1)) (b (y . 2)) (c (x . 1) (y . 2) (z . 3)))
   "base: &b\n  x: 1\n  y: 2\nuse: *b\n")
  (list (yaml->value
          (string->yaml "base: &b\n  x: 1\n  y: 2\nuse: *b\nmerged:\n  <<: *b\n  y: 3\n"
                        '((merge-keys . #t))))
        (yaml->value (string->yaml "base: &b\n  x: 1\nmerged:\n  <<: *b\n  y: 3\n"))
        (yaml->value
          (string->yaml "a: &a {x: 1}\nb: &b {y: 2}\nc:\n  <<: [*a, *b]\n  z: 3\n"
                        '((merge-keys . #t))))
        (yaml->string (string->yaml "base: &b\n  x: 1\n  y: 2\nuse: *b\n")))
)

(
  "Limits on depth and alias expansion"
  (#t #t #t)
  (list (raises-with (string->yaml "[[[[1]]]]" '((max-depth . 3))) "nesting depth")
        (raises-with (string->yaml "a: &a [1,2,3]\nb: &b [*a,*a,*a]\nc: &c [*b,*b,*b]\nd: [*c,*c,*c]"
                                   '((max-alias-expansion . 10)))
                     "aliases expand")
        (equal? (yaml->value (string->yaml "[[[1]]]" '((max-depth . 4)))) #(#(#(1)))))
)

(
  "Syntax errors report positions"
  (#t #t #t)
  (list (raises-with (string->yaml "a: [1, 2") "line 2, column 1")
        (raises-with (string->yaml "a: *x") "undefined alias")
        (raises-with (string->yaml "a: 1\na: 2") "line 2"))
)

;;; Serialization

(
  "Serializing scalars and empty collections"
  ("null\n" "true\n" "1.5\n" "a b\n" "[]\n" "{}\n" "123456789012345678901234567890\n")
  (list (yaml->string 'null)
        (yaml->string #t)
        (yaml->string 1.5)
        (yaml->string "a b")
        (yaml->string #())
        (yaml->string '())
        (yaml->string 123456789012345678901234567890))
)

(
  "Serializing special floating-point numbers"
  ("[.inf, -.inf, .nan]\n" "123456789012345678901234567890")
  (list (yaml->string (yaml #(+inf.0 -inf.0 +nan.0)) '((flow-style . #t)))
        (yaml-scalar-text (yaml 123456789012345678901234567890)))
)

(
  "Quoting of strings that would be read back differently"
  ("- \"true\"\n- \"1.0\"\n- \"null\"\n- \"~\"\n- \"\"\n- 'a: b'\n- '- x'\n- '#c'\n- |-\n  multi\n  line\n- yes\n- \"0x1F\"\n")
  (list (yaml->string #("true" "1.0" "null" "~" "" "a: b" "- x" "#c" "multi\nline" "yes" "0x1F")))
)

(
  "Round trip of strings"
  #t
  (let ((strings '("true" "1.0" "null" "" " lead" "trail " "a: b" "multi\nline\n" "tab\there"
                   "é" "- x" "[x]" "{x}" "!tag" "&a" "*a" "%x" "@x" "`x" "'" "\"" "x #y"
                   "x: y" "0o17" "+1" ".5" "1e3" "~" "yes" "NaN" ".nan" "--- x" "...")))
    (equal? (yaml->value (string->yaml (yaml->string (list->vector strings))))
            (list->vector strings)))
)

(
  "Round trip of documents"
  (#t #t)
  (let* ((text "a: 1\nb:\n  - x\n  - y: [1, 2]\n  - 'q'\nc: |\n  line1\n  line2\n")
         (node (string->yaml text)))
    (list (string=? text (yaml->string node))
          (yaml=? node (string->yaml (yaml->string node)))))
)

(
  "Serializing multiple documents"
  ("1\n--- a\n---\nk: v\n" "--- 1\n--- a\n" #u8(49 10 45 45 45 32 50 10) #u8(97 58 32 49 10))
  (list (yaml-documents->string (list 1 "a" '((k . v))))
        (yaml-documents->string (list 1 "a") '((explicit-start . #t)))
        (yaml-documents->bytevector (list 1 2))
        (yaml->bytevector '((a . 1))))
)

(
  "Serialization option indentation"
  ("a:\n    b:\n        - 1\n        - 2\nc: 1\n" "a:\n  b:\n    - 1\n    - 2\nc: 1\n")
  (list (yaml->string '((a . ((b . #(1 2)))) (c . 1)) '((indentation . 4)))
        (yaml->string '((a . ((b . #(1 2)))) (c . 1)) '((indentation . 2))))
)

(
  "Serialization option indent-sequences"
  ("a:\n- 1\n- 2\n" "a:\n  - 1\n  - 2\n")
  (list (yaml->string '((a . #(1 2))) '((indent-sequences . #f)))
        (yaml->string '((a . #(1 2))) '((indent-sequences . #t))))
)

(
  "Serialization options sort-keys and flow-style"
  ("a:\n  y: 2\n  z: 1\nb: 1\n"
   "a:\n  - a: 2\n    z: 1\nb: 1\n"
   "{b: 1, a: [1, 2]}\n"
   "{a: {b: [1, 2]}}\n")
  (list (yaml->string '((b . 1) (a . ((z . 1) (y . 2)))) '((sort-keys . #t)))
        (yaml->string '((b . 1) (a . #(((z . 1) (a . 2))))) '((sort-keys . #t)))
        (yaml->string '((b . 1) (a . #(1 2))) '((flow-style . #t)))
        (yaml->string '((a . ((b . #(1 2))))) '((flow-style . #t))))
)

(
  "Serialization options line-width, unicode and explicit-start"
  ("aaa bbb ccc\n  ddd eee fff\n  ggg\n"
   "aaa bbb ccc ddd eee fff ggg\n"
   ("é\n" "\"\\xE9\"\n")
   "--- x\n")
  (list (yaml->string (yaml "aaa bbb ccc ddd eee fff ggg") '((line-width . 10)))
        (yaml->string (yaml "aaa bbb ccc ddd eee fff ggg") '((line-width . #f)))
        (list (yaml->string (yaml "é"))
              (yaml->string (yaml "é") '((unicode . #f))))
        (yaml->string (yaml "x") '((explicit-start . #t))))
)

(
  "Serialization of presentation properties"
  ("!color '#ff8800'\n"
   "\"text\"\n"
   "'text'\n"
   "[1, 2, 3]\n"
   "|\n  a\n  b\n"
   "a: &n\n  - 1\n  - 2\nb: *n\n"
   "&x\n- 1\n- 2\n"
   "!point\n- 1\n- 2\n"
   "!point\nx: 1\n")
  (let ((n (yaml-with-anchor (yaml #(1 2)) "n")))
    (list (yaml->string (yaml-with-tag (yaml "#ff8800") "!color"))
          (yaml->string (yaml-with-style (yaml "text") 'double-quoted))
          (yaml->string (yaml-with-style (yaml "text") 'single-quoted))
          (yaml->string (yaml-with-style (yaml #(1 2 3)) 'flow))
          (yaml->string (yaml-with-style (yaml "a\nb\n") 'literal))
          (yaml->string (yaml (list (cons 'a n) (cons 'b n))))
          (yaml->string (yaml-with-anchor (yaml #(1 2)) "x"))
          (yaml->string (yaml-with-tag (yaml #(1 2)) "!point"))
          (yaml->string (yaml-with-tag (yaml '((x . 1))) "!point"))))
)

(
  "Empty options"
  ("a" "a" "a")
  (list (yaml->value (string->yaml "a" #f))
        (yaml->value (string->yaml "a" '()))
        (yaml->value (string->yaml "a" '((indentation . 4) (sort-keys . #t)))))
)

(
  "Invalid options"
  (#t #t #t #t #t #t #t)
  (list (raises-with (string->yaml "a" '((bogus . 1))) "unknown YAML option")
        (raises-with (string->yaml "a" '((merge-keys . 1))) "requires a boolean")
        (raises-with (yaml->string 1 '((indentation . 1))) "within the range")
        (raises-with (yaml->string 1 '((indentation . 10))) "within the range")
        (raises? (yaml->string 1 '((line-width . "x"))))
        (raises-with (yaml->string 1 '((schema . bogus))) "unknown YAML schema")
        (raises? (yaml->string 1 1)))
)

;;; Events

(
  "Parsing events"
  ((stream-start document-start mapping-start scalar sequence-start scalar scalar sequence-end
    mapping-end document-end stream-end)
   (stream-start document-start scalar document-end stream-end)
   (stream-start document-start scalar document-end stream-end))
  (list (map yaml-event-kind (string->yaml-events "a: [1, 2]"))
        (map yaml-event-kind (string->yaml-events "a"))
        (map yaml-event-kind (bytevector->yaml-events (string->utf8 "a"))))
)

(
  "Event properties"
  ((scalar "v" single-quoted "!!str" "x" (1 1 0) (1 13 12))
   (#f #t #f #t #f)
   (#f (1 . 2) (("!e!" . "tag:example.com,2000:")))
   (scalar "v" double-quoted "!!str" "anc" (1 1 0))
   (alias "a" #f)
   (#f (1 . 1) (("!e!" . "tag:x,2000:")))
   (#t #t #t #t #f))
  (list (let ((e (list-ref (string->yaml-events "&x !!str 'v'") 2)))
          (list (yaml-event-kind e) (yaml-event-value e) (yaml-event-style e)
                (yaml-event-tag e) (yaml-event-anchor e)
                (yaml-event-start e) (yaml-event-end e)))
        (map yaml-event-implicit? (string->yaml-events "a"))
        (let ((e (cadr (string->yaml-events "%YAML 1.2\n%TAG !e! tag:example.com,2000:\n---\na"))))
          (list (yaml-event-implicit? e) (yaml-event-version e) (yaml-event-tag-directives e)))
        (let ((e (make-yaml-event 'scalar "v" 'double-quoted "!!str" "anc")))
          (list (yaml-event-kind e) (yaml-event-value e) (yaml-event-style e)
                (yaml-event-tag e) (yaml-event-anchor e) (yaml-event-start e)))
        (let ((e (make-yaml-event 'alias "a")))
          (list (yaml-event-kind e) (yaml-event-value e) (yaml-event-anchor e)))
        (let ((e (make-yaml-event 'document-start #f '(1 . 1) '(("!e!" . "tag:x,2000:")))))
          (list (yaml-event-implicit? e) (yaml-event-version e) (yaml-event-tag-directives e)))
        (list (yaml-event? (make-yaml-event 'stream-start))
              (yaml-event? (car (string->yaml-events "a")))
              (yaml-event? (make-yaml-event 'mapping-start 'flow "!t" "a"))
              (yaml-event? (make-yaml-event 'sequence-end))
              (yaml-event? 1)))
)

(
  "Event kinds"
  (stream-start stream-end document-start document-start document-end alias sequence-start
   sequence-end mapping-start mapping-end scalar)
  (map yaml-event-kind
       (list (make-yaml-event 'stream-start)
             (make-yaml-event 'stream-end)
             (make-yaml-event 'document-start)
             (make-yaml-event 'document-start #f '(1 . 1) '(("!e!" . "tag:x,2000:")))
             (make-yaml-event 'document-end #f)
             (make-yaml-event 'alias "a")
             (make-yaml-event 'sequence-start)
             (make-yaml-event 'sequence-end)
             (make-yaml-event 'mapping-start 'flow "!t" "a")
             (make-yaml-event 'mapping-end)
             (make-yaml-event 'scalar "x")))
)

(
  "Invalid events"
  (#t #t #t)
  (list (raises-with (make-yaml-event 'bogus) "not a valid YAML event kind")
        (raises-with (make-yaml-event 'scalar "v" 'bogus) "not a valid YAML scalar style")
        (raises? (yaml-event-kind 1)))
)

(
  "Parse errors while producing events"
  #t
  (raises-with (string->yaml-events "a: [1") "line 2, column 1")
)

(
  "Serializing events"
  ("---\ntitle: \"Release notes\"\nbody: |\n  Line one\n  Line two\ntags: [swift, yaml]\n"
   "{a: 1, b: [x, y]}\n")
  (list (yaml-events->string
          (list (make-yaml-event 'stream-start)
                (make-yaml-event 'document-start #f)
                (make-yaml-event 'mapping-start 'block)
                (make-yaml-event 'scalar "title")
                (make-yaml-event 'scalar "Release notes" 'double-quoted)
                (make-yaml-event 'scalar "body")
                (make-yaml-event 'scalar "Line one\nLine two\n" 'literal)
                (make-yaml-event 'scalar "tags")
                (make-yaml-event 'sequence-start 'flow)
                (make-yaml-event 'scalar "swift")
                (make-yaml-event 'scalar "yaml")
                (make-yaml-event 'sequence-end)
                (make-yaml-event 'mapping-end)
                (make-yaml-event 'document-end)
                (make-yaml-event 'stream-end)))
        (yaml-events->string (string->yaml-events "{a: 1,   b: [x,y]}")))
)

(
  "Pull parser"
  (#t #f (stream-start document-start sequence-start scalar scalar sequence-end document-end
          stream-end)
   (stream-start) #t #t)
  (list (yaml-parser? (make-yaml-parser "a"))
        (yaml-parser? 1)
        (parser-kinds (make-yaml-parser "- 1\n- 2"))
        (let ((p (make-yaml-parser (string->utf8 "x"))))
          (list (yaml-event-kind (yaml-parser-next! p))))
        (let ((p (make-yaml-parser "a")))
          (parser-kinds p)
          (eof-object? (yaml-parser-next! p)))
        (raises-with (parser-kinds (make-yaml-parser "a: [1")) "unterminated flow collection"))
)

(
  "Push emitter"
  (#t #f "a: 1\n" "x\n" "---\na: 1\n" "[1, 2]\n")
  (list (yaml-emitter? (make-yaml-emitter))
        (yaml-emitter? 1)
        (let ((e (make-yaml-emitter)))
          (for-each (lambda (ev) (yaml-emitter-emit! e ev)) (string->yaml-events "a: 1"))
          (yaml-emitter-output e))
        (let ((e (make-yaml-emitter)))
          (yaml-emitter-emit! e
                              (make-yaml-event 'stream-start)
                              (make-yaml-event 'document-start)
                              (make-yaml-event 'scalar "x")
                              (make-yaml-event 'document-end)
                              (make-yaml-event 'stream-end))
          (yaml-emitter-output e))
        (let ((e (make-yaml-emitter '((explicit-start . #t)))))
          (for-each (lambda (ev) (yaml-emitter-emit! e ev)) (string->yaml-events "a: 1"))
          (yaml-emitter-output e))
        (let ((e (make-yaml-emitter)))
          (for-each (lambda (ev) (yaml-emitter-emit! e ev)) (string->yaml-events "[1, 2]"))
          (yaml-emitter-output e)))
)

(
  "Emitter errors for events in the wrong order"
  #t
  (let ((e (make-yaml-emitter)))
    (raises-with (yaml-emitter-emit! e (make-yaml-event 'scalar "x")) "emitter error"))
)

;;; Interoperability

(
  "Conversion between YAML and JSON"
  (#t #t)
  (import (lispkit json))
  (let* ((value '((a . 1) (b . #(#t null "x"))))
         (from-json (yaml (json->value (json value))))
         (back (json->value (json (yaml->value (string->yaml "{a: 1, b: [true, null, x]}"))))))
    (list (yaml=? from-json (yaml value))
          (yaml=? (yaml back) (yaml value))))
)
