;;; Create, query, change, and validate JSON data
;;;
;;; This code showcases the API provided by libraries `(lispkit json)` and
;;; `(lispkit json schema)`. It creates JSON values from Scheme data, parses and
;;; serializes them, accesses their parts via JSON references and JSON Path
;;; queries, changes them destructively and non-destructively, with JSON Patch
;;; and by merging, and it finally validates JSON data using JSON schema.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2024 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may
;;; not use this file except in compliance with the License. You may obtain
;;; a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(import (lispkit base)
        (lispkit hashtable)
        (lispkit json)
        (lispkit json schema))

;;; Output

;; Prints the result of an evaluation. JSON values are printed as JSON text with
;; sorted object members, which makes the output deterministic.
(define (display-result x)
  (cond ((json? x)
         (display (json->string x #f #t)))
        ((pair? x)
         (display "(")
         (display-result (car x))
         (let loop ((rest (cdr x)))
           (cond ((null? rest))
                 ((pair? rest) (display " ") (display-result (car rest)) (loop (cdr rest)))
                 (else (display " . ") (display-result rest))))
         (display ")"))
        (else
         (write x))))

;; Writes an expression like `write`, but abbreviates quoted data.
(define (write-expr expr)
  (cond ((and (pair? expr) (eq? (car expr) 'quote) (pair? (cdr expr)) (null? (cddr expr)))
         (display "'")
         (write-expr (cadr expr)))
        ((pair? expr)
         (display "(")
         (write-expr (car expr))
         (let loop ((rest (cdr expr)))
           (cond ((null? rest))
                 ((pair? rest) (display " ") (write-expr (car rest)) (loop (cdr rest)))
                 (else (display " . ") (write-expr rest))))
         (display ")"))
        (else
         (write expr))))

;; Prints expression `expr`, evaluates it, and prints the result.
(define-syntax show
  (syntax-rules ()
    ((_ expr)
     (begin
       (write-expr 'expr)
       (newline)
       (display "  ⇒ ")
       (display-result expr)
       (newline)))))

;; Prints a headline for the next section.
(define (section title)
  (newline)
  (display* "=== " title " ===")
  (newline))

;;; Creating and converting JSON values

(section "Creating JSON values")

;; Association lists are mapped to JSON objects and vectors to JSON arrays. The
;; symbol `null` stands for the JSON null value. Procedure `json` returns an
;; immutable JSON value.
(define address-book
  (json '((owner . "John Doe")
          (people . #(((name . "John Doe")
                       (birthday . "1983-03-19")
                       (numChildren . 2)
                       (email . #("john@doe.com" "john.doe@gmail.com")))
                      ((name . "Jane Roe")
                       (birthday . "1990-07-02")
                       (email . #("jane@roe.org")))
                      ((name . "Max Mustermann")
                       (birthday . "1975-11-30")
                       (numChildren . 3)
                       (address . ((street . "Hauptstr. 1")
                                   (city . "Berlin")
                                   (postalCode . "10115")))))))))

(show (json '((ints . #(1 -2)) (float . 2.5) (bool . #t) (nothing . null) (text . "abc"))))
(show (json "text" 42 #f))
(show (json-object (name "John") (age 30) (tags #("a" "b"))))
(show (make-json-array 3 (json 0)))

;; Records are mapped to JSON objects as well, with the fields as members.
(define-record-type point
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(show (json (make-point 3 4)))

;; So are hash tables with symbols or strings as keys.
(define table (make-equal-hashtable))
(hashtable-set! table 'language "Scheme")
(hashtable-set! table "year" 1975)

(show (json table))

;; JSON text can be parsed into JSON values, and JSON values can be converted
;; back into Scheme data.
(show (string->json "{\"a\": [1, 2.5, null, true], \"b\": {\"c\": \"d\"}}"))
(show (json->value (json '((list . #(1 "two" null))))))

;;; Inspecting JSON values

(section "Inspecting JSON values")

;; Returns the type of a JSON value as a symbol
(define (json-type x)
  (cond ((json-null? x) 'null)
        ((json-boolean? x) 'boolean)
        ((json-number? x) 'number)
        ((json-string? x) 'string)
        ((json-array? x) 'array)
        ((json-object? x) 'object)))

;; Returns the sorted names of the members of JSON object `x`
(define (member-names x)
  (sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
        (json-members x)))

(define john (json-ref address-book '(people 0)))

(show (member-names john))
(show (map (lambda (name) (json-type (json-ref john name))) (member-names john)))
(show (json-member? john 'email))
(show (json-children-count (json-ref address-book 'people)))

;; `json-member` returns Scheme values, `json-ref` returns JSON values.
(show (json-member john 'name))
(show (json-member john 'phone "unknown"))
(show (json-member (json-ref address-book '(people 2)) '(address city)))
(show (json-ref address-book 'owner))

;; Iterate over the elements of arrays and the members of objects
(display "people:\n")
(json-for-each-element
  (lambda (person) (display* "  - " (json-member person 'name) "\n"))
  (json-ref address-book 'people))

(let ((members '()))
  (json-for-each-member
    (lambda (name value) (set! members (cons (list name (json-type value)) members)))
    john)
  (show (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b))))
              members)))

;;; JSON references

(section "JSON references")

;; A JSON reference is a JSON Pointer, a JSON location, a symbol, a number, or
;; a list of symbols and numbers. All of them refer to a value within a document.
(show (json-ref address-book "/people/0/name"))
(show (json-ref address-book "$.people[1].name"))
(show (json-ref address-book '(people 2 address city)))
(show (json-ref address-book 'people -1 'name))
(show (json-ref address-book '(people 7)))

(show (json-pointer "$.people[0].email[1]"))
(show (json-location '(people 0 email 1)))
(show (json-reference-segments "$.people[0].email[-1]"))
(show (list (json-pointer? "/people/0") (json-location? "$.people[0]")
            (json-reference? 'people) (json-reference? 2.5)))
(show (list (json-self-reference? '()) (json-self-reference? "$") (json-self-reference? "/people")))

;;; JSON Path

(section "Querying with JSON Path")

(show (json-query-results address-book "$.people[*].name"))
(show (json-query-results address-book "$.people[?@.numChildren > 0].name"))
(show (json-query-results address-book "$.people[0:2].birthday"))
(show (json-query-results address-book "$..email[*]"))
(show (json-query-results address-book "$.people[-1].address.city"))
(show (json-query-results address-book "$.people[?match(@.name, \"J.*\")].name"))
(show (json-query-results address-book "$.people[?length(@.email) > 1].name"))
(show (json-query-locations address-book "$.people[?@.address].name"))
(show (json-query address-book "$.people[?@.numChildren > 2].name"))
(show (list (json-path? "$.people[?@.name]") (json-path? "people.name")
            (json-path-singular? "$.people[0].name") (json-path-singular? "$.people[*].name")))

;;; Changing JSON values

(section "Changing JSON values")

;; `json-replace` and `json-replace-all` return new values; the original is not changed.
(define updated
  (json-replace address-book
                "$.people[0].name" (json "Johnny Doe")
                '(people 1 numChildren) (json 1)))

(show (json-query-results updated "$.people[0:2]['name','numChildren']"))
(show (json-query-results address-book "$.people[0:2]['name','numChildren']"))
(show (json-ref (json-replace-all address-book
                                  (list (cons "/owner" (json "Jane Roe"))
                                        (cons '(people 0 numChildren) (json 5))))
                'owner))

;; A mutable JSON value can be changed in place. `mutable-json` makes a copy if the
;; value is referenced elsewhere.
(define book (mutable-json address-book))

(json-set! book "$.people[1].address" (json "12 Main Street, 17445 Noname"))
(json-append! book 'people (json '((name . "Eve Adams") (email . #("eve@adams.org")))))
(json-insert! book 'people 1 (json '((name . "Zoe Zed") (birthday . "1999-12-31"))))
(json-remove! book '(people 0 numChildren) "$.people[2].email")

(show (json-query-results book "$.people[*].name"))
(show (json-query-results book "$.people[*].address"))
(show (list (mutable-json? book) (json? book) (json? book #t)))
(show (list (json-children-count (json-ref address-book 'people))
            (json-children-count (json-ref book 'people))))

;;; JSON Patch

(section "JSON Patch")

;; A JSON patch is a sequence of operations on a JSON value. They can be given as
;; lists, or as JSON in the format of RFC 6902.
(define patch
  (json-patch
    `((test "/owner" ,(json "John Doe"))
      (replace "/people/0/numChildren" ,(json 3))
      (add "/people/0/email/-" ,(json "jdoe@work.com"))
      (copy "/people/1/primaryEmail" "/people/1/email/0")
      (move "/people/2/residence" "/people/2/address")
      (remove "/people/2/numChildren"))))

(show (json-patch->json patch))
(show (json-patch=? patch (json-patch (json-patch->json patch))))

;; Patches can also be built step by step.
(define incremental (json-patch))
(json-patch-append! incremental
                    `(replace "/owner" ,(json "Jane Roe"))
                    '(remove "/people/2/address"))

(show (list (json-patch? incremental) (json-patch->list incremental)))
(json-patch-clear! incremental)
(show (json-patch->list incremental))

;; Patches are applied to mutable JSON values. Either all operations succeed or the
;; value is not changed.
(define patched (mutable-json address-book #t))
(json-apply! patched patch)

(show (json-ref patched '(people 0 email)))
(show (json-ref patched '(people 1 primaryEmail)))
(show (json-ref patched '(people 2 residence city)))
(show (json-ref patched '(people 2 address)))
(show (json-ref patched '(people 2 numChildren)))
(show (json=? patched address-book))

(define failing
  (json-patch `((replace "/owner" ,(json "Max Mustermann"))
                (test "/people/0/name" ,(json "Somebody Else")))))

(show (guard (e (#t 'patch-failed)) (json-apply! patched failing) 'patch-applied))
(show (json-ref patched 'owner))

;;; Merging

(section "Merging JSON values")

(define jane (json-ref address-book '(people 1)))
(define contact (json '((name . "Jane Roe") (phone . "555-1234") (address . ((city . "Mcity"))))))

;; A symmetrical merge combines the members of two values. It returns `#f` if the
;; values cannot be combined, e.g. arrays of different length or different scalars.
(show (json-merge jane contact))
(show (json-merge jane (json '((email . #("jane@roe.org" "j@work.com"))))))
(show (json-merge (json '((age . 30))) (json '((age . 31)))))
(show (list (json-refinement? (json-merge jane contact) jane) (json-refinement? jane contact)))

;; `json-override` lets the second value win, and `json-merge-patch` implements
;; RFC 7396, in which `null` removes a member.
(show (json-override jane (json '((name . "J. Roe") (email . #("jane@roe.org" "j@work.com"))))))
(show (json-merge-patch jane (json '((birthday . null) (phone . "555-1234")))))

;;; Reading and writing JSON data

(section "Reading and writing JSON data")

;; Without the third argument, the members of objects are written in no particular order.
(show (json->string jane #f #t))
(display (json->string jane #t #t))
(newline)

(define bytes (json->bytevector jane #f #t))
(define cbor (json->cbor jane))

(show (list (bytevector-length bytes) (bytevector-length cbor)))
(show (list (json=? (bytevector->json bytes) jane) (json=? (cbor->json cbor) jane)))

;; CBOR preserves `null` values, also when they are nested in arrays and objects.
(define nulls (json '((a . null) (b . #(null 1 null)))))
(show (json=? (cbor->json (json->cbor nulls)) nulls))
(show (json->string (json "a/b") #f #f #t))

;; JSON files are loaded with `load-json`. This one is the JSON schema used below.
(define schema-document (load-json (asset-file-path "person" "" "JSON/Schema/custom")))

(show (json-ref schema-document 'title))
(show (member-names schema-document))

;;; Validating JSON with JSON schema

(section "Validating JSON with JSON schema")

;; Make a new JSON schema registry. The first `#t` argument makes the schema
;; definitions in the asset directory `JSON/Schema/2020-12` available via
;; the Schema identifier base `https://json-schema.org/draft/2020-12`. The
;; second `#t` argument makes the schema definitions in the asset directory
;; `JSON/Schema/custom` available via the Schema identifier base
;; `https://lisppad.app/schema`.
(define registry (make-schema-registry json-draft2020-default #t #t))

(define person-schema (schema-registry-ref "https://lisppad.app/schema/person" registry))

(define person0 (json '(
  (name . "John Doe")
  (birthday . "1983-03-19")
  (numChildren . 2)
  (email . #("john@doe.com" "john.doe@gmail.com"))
)))

(display (json->string person0 #t #t))
(newline)
(display* "valid person: " (json-valid? person0 person-schema #t registry) " [#t]")
(newline)

(define person1 (json '(
  (name . "John Doe")
  (email . #("john@doe.com" "john.doe@gmail.com"))
)))

(display (json->string person1 #t #t))
(newline)
(display* "valid person: " (json-valid? person1 person-schema #t registry) " [#f]")
(newline)

;; The validation result of an invalid value lists the errors with the locations
;; of the values that are affected.
(define res1 (json-validate person1 person-schema #t registry))
(for-each
  (lambda (err)
    (display* "  error: " (caddr err) " at " (cdar err))
    (newline))
  (validation-result-errors res1))

;; The counts of errors, tags, and format constraints of a validation result are
;; returned as multiple values.
(call-with-values
  (lambda () (validation-result-counts res1))
  (lambda (errors tags formats)
    (display* "errors: " errors ", tags: " tags ", format constraints: " formats)
    (newline)))

;; Applying a JSON merge patch with the missing member fixes the value.
(define person1-fixed
  (json-merge-patch person1 (json '((birthday . "1983-03-19")))))
(display* "valid person: " (json-valid? person1-fixed person-schema #t registry) " [#t]")
(newline)

(define person2 (json '(
  (name . "John Doe")
  (birthday . "1983-03-19")
  (address . "12 Main Street, 17445 Noname")
)))

(display (json->string person2 #t #t))
(newline)

(define res (json-validate person2 person-schema #t registry))
(display* "valid person: " (validation-result-valid? res) " [#t]")
(newline)

;; Returns the text of a JSON value
(define (json-text x)
  (json->string x #f #t))

;; Validation results also provide the default values of members of the schema.
;; The defaults of a member are listed in no particular order.
(for-each
  (lambda (x)
    (if (cadr x)
        (display* (car x) " exists; defaults: ")
        (display* (car x) " does not exist; defaults: "))
    (display-result (sort (lambda (a b) (string<? (json-text a) (json-text b))) (caddr x)))
    (newline))
  (sort (lambda (x y) (string<? (car x) (car y))) (validation-result-defaults res)))

;; Adds the defaults of members that do not exist yet, as far as the default is unique.
(define (with-defaults person)
  (let ((result (json-validate person person-schema #t registry))
        (completed (mutable-json person #t)))
    (for-each
      (lambda (x)
        (if (and (not (cadr x)) (null? (cdr (caddr x))))
            (json-set! completed (car x) (car (caddr x)))))
      (validation-result-defaults result))
    completed))

(show (with-defaults person2))

;; Schemas have an identity and can be inspected. A schema can also be created from
;; Scheme data. By default, a format like `email` is only an annotation, but dialect
;; `json-draft2020-all` asserts formats.
(show (list (json-schema-id person-schema) (json-schema-title person-schema)))
(show (schema-registry-schemas registry))

(define email-schema (json-schema '((type . "string") (format . "email"))))

(show (list (json-valid? (json "nope") email-schema)
            (json-valid? (json "nope") email-schema json-draft2020-all)
            (json-valid? (json "me@example.org") email-schema json-draft2020-all)))

;; Schemas can tag values as deprecated, read-only, or write-only. Validation results
;; list the tagged values with their locations, including values of nested schemas.
(define tagged-schema
  (json-schema '((type . "object")
                 (properties . ((id . ((readOnly . #t)))
                                (nick . ((deprecated . #t)))
                                (tags . ((items . ((deprecated . #t))))))))))

(for-each
  (lambda (tag)
    (display* "  " (cdar tag) ": " (caddr tag))
    (newline))
  (sort (lambda (x y) (string<? (cdar x) (cdar y)))
        (validation-result-tags
          (json-validate (json '((id . 7) (nick . "jd") (tags . #("x" "y")))) tagged-schema))))

;; Validate all people of the address book that was changed above.
(show (map (lambda (person) (json-valid? person person-schema #t registry))
           (json-children (json-ref book 'people))))
(show (json-query-results book "$.people[?!@.birthday].name"))
