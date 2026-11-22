;;; DataFrame.scm
;;; Regression test data for library (lispkit data-frame)
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
  (import (lispkit data-frame)
          (lispkit date-time)
          (only (lispkit system) system-directory path file-exists? delete-file-or-directory))
  ;; Returns #t if calling `thunk` raises an error
  (define (call-raises? thunk)
    (guard (x (#t #t)) (thunk) #f))
  (define-syntax raises?
    (syntax-rules ()
      ((_ expr) (call-raises? (lambda () expr)))))
  (define tmp-dir (car (system-directory 'temporary)))
  (define (tmp-file name)
    (path tmp-dir name))
  (define (delete-if-exists file)
    (if (file-exists? file) (delete-file-or-directory file)))
  (define csv
    (string-append "name,city,age,score,member\n"
                   "Alice,Zurich,34,88.5,true\n"
                   "Bob,Basel,,72.0,false\n"
                   "Carol,Zurich,29,NA,true\n"
                   "Dave,Bern,41,95.25,false\n"
                   "Eve,Basel,23,64.0,true\n"))
  (define df (csv->data-frame csv))
  #t
)

(
  "Reading CSV"
  (#t 5 5 ("name" "city" "age" "score" "member")
   (("name" . string) ("city" . string) ("age" . integer) ("score" . double) ("member" . boolean)))
  (list (data-frame? df)
        (data-frame-row-count df)
        (data-frame-column-count df)
        (data-frame-column-names df)
        (data-frame-column-types df))
)

(
  "Accessing values"
  (#(34 #f 29 41 23) 34 #f #t #f #f ("Alice" "Zurich" 34 88.5 #t) 1 ("Zurich" "Basel" "Bern") #t #f)
  (list (data-frame-column df "age")
        (data-frame-ref df 0 "age")
        (data-frame-ref df 1 "age")
        (data-frame-missing? df 1 "age")
        (data-frame-ref df 1 'member)
        (data-frame-missing? df 1 'member)
        (data-frame-row df 0)
        (data-frame-missing-count df "score")
        (data-frame-distinct-values df "city")
        (data-frame-has-column? df 'age)
        (data-frame-has-column? df "height"))
)

(
  "Rows and columns"
  ((("Alice" 34) ("Bob" #f))
   (("a" . #(1 2)) ("b" . #("x" "y"))))
  (list (data-frame->rows (data-frame-head (data-frame-select-columns df '(name age)) 2))
        (data-frame->columns (make-data-frame '((a 1 2) (b "x" "y")))))
)

(
  "Creating data frames"
  ((("a" . integer) ("b" . double) ("c" . string) ("d" . boolean) ("e" . date))
   ((1 1.5 "x" #t #t) (2 2.0 #f #f #f))
   (("x" . double) ("y" . string))
   ((1.0 "a") (2.5 #f))
   ((1.0) (2.0))
   (0 0))
  (define d1
    (make-data-frame `((a 1 2) (b 1.5 2) (c "x" #f) (d #t #f) (e ,(date-time 2024 1 15) #f))))
  (define d2 (rows->data-frame '(x y) '((1 "a") (2.5 #f))))
  (list (data-frame-column-types d1)
        (map (lambda (row) (map (lambda (x) (if (date-time? x) #t x)) row)) (data-frame->rows d1))
        (data-frame-column-types d2)
        (data-frame->rows d2)
        (data-frame->rows (make-data-frame '((a 1 2)) '((a . double))))
        (let ((empty (make-data-frame '())))
          (list (data-frame-row-count empty) (data-frame-column-count empty))))
)

(
  "Sorting"
  (("Eve" "Bob" "Dave" "Alice" "Carol")
   ("Eve" "Carol" "Alice" "Dave" "Bob")
   ("Dave" "Alice" "Carol" "Eve" "Bob"))
  (define (names d) (vector->list (data-frame-column d "name")))
  (list (names (data-frame-sort df '(("city" . ascending) ("age" . descending))))
        (names (data-frame-sort df "age"))
        (names (data-frame-sort df '("age" . descending))))
)

(
  "Filtering and slicing"
  (("Alice" "Dave") ("Eve" "Alice") ("Bob" "Carol") ("Dave" "Eve") ("Alice" "Bob" "Carol"))
  (define (names d) (vector->list (data-frame-column d "name")))
  (list (names (data-frame-filter df (lambda (age) (and age (> age 30))) '("age")))
        (names (data-frame-select-rows df '(4 0)))
        (names (data-frame-slice df 1 3))
        (names (data-frame-tail df 2))
        (names (data-frame-head df 3)))
)

(
  "Changing columns"
  (("name" "city" "age") ("name" "city" "age" "points" "member")
   #(68 #f 58 82 46) #("Alice@Zurich" "Bob@Basel" "Carol@Zurich" "Dave@Bern" "Eve@Basel")
   #(1 2 3 4 5) ("name" "city" "age" "score" "member" "id"))
  (list (data-frame-column-names (data-frame-drop-columns df '("score" "member")))
        (data-frame-column-names (data-frame-rename-column df "score" "points"))
        (data-frame-column (data-frame-update-column df "age" (lambda (a) (and a (* a 2)))) "age")
        (data-frame-column
          (data-frame-compute-column df "label" '("name" "city")
                                     (lambda (n c) (string-append n "@" c)))
          "label")
        (data-frame-column (data-frame-set-column df "age" '(1 2 3 4 5)) "age")
        (data-frame-column-names (data-frame-set-column df "id" #(1 2 3 4 5))))
)

(
  "Appending"
  ((("Alice" 34) ("Eve" 23))
   (("Alice" 34) ("Zoe" 50))
   (("a" . integer) ("b" . string)))
  (define small (data-frame-select-columns df '("name" "age")))
  (list (data-frame->rows (data-frame-append (data-frame-head small 1) (data-frame-tail small 1)))
        (data-frame->rows (data-frame-append-rows (data-frame-head small 1) '(("Zoe" 50))))
        (data-frame-column-types
          (data-frame-append (make-data-frame '((a 1) (b "x")))
                             (make-data-frame '((b "y") (a 2))))))
)

(
  "Aggregating"
  ((("Zurich" 2 31.5 88.5 63 "Alice") ("Basel" 2 23.0 72.0 23 "Bob") ("Bern" 1 41.0 95.25 41 "Dave"))
   (("city" . string) ("n" . integer) ("avg-age" . double) ("max-score" . double)
    ("total" . integer) ("first-name" . string))
   (("Zurich" 2) ("Basel" 2) ("Bern" 1))
   ((5 4 3)))
  (define agg
    (data-frame-aggregate df "city"
      '((n count) (avg-age mean age) (max-score max score) (total sum age) (first-name first name))))
  (list (data-frame->rows agg)
        (data-frame-column-types agg)
        (data-frame->rows (data-frame-aggregate df 'city))
        (data-frame->rows (data-frame-aggregate df '() '((n count) (ages count age) (cities distinct city)))))
)

(
  "Statistics"
  (31.5 7.632168761236874 5 #t)
  (define stats (data-frame-aggregate df '() '((m median age) (s std age))))
  (define summary (data-frame-summary df '(age score)))
  (list (data-frame-ref stats 0 "m")
        (data-frame-ref stats 0 "s")
        (data-frame-row-count (data-frame-summary df))
        (equal? (data-frame-column summary "column") #("age" "score")))
)

(
  "Joining"
  (("left.name" "city" "left.age" "left.score" "left.member" "right.canton")
   #("ZH" "BS" "ZH" #f "BS")
   4)
  (define cantons (make-data-frame '(("city" "Zurich" "Basel" "Geneva") ("canton" "ZH" "BS" "GE"))))
  (define joined (data-frame-join df cantons "city" 'left))
  (list (data-frame-column-names joined)
        (data-frame-column joined "right.canton")
        (data-frame-row-count (data-frame-join df cantons "city")))
)

(
  "Writing CSV and JSON"
  ("name,city,age,score,member\nAlice,Zurich,34,88.5,true\nBob,Basel,,72.0,false\n"
   "name;age\nAlice;34\nBob;NA\n"
   "[{\"age\":34,\"name\":\"Alice\"},{\"age\":null,\"name\":\"Bob\"}]")
  (define two (data-frame-head df 2))
  (list (data-frame->csv two)
        (data-frame->csv (data-frame-select-columns two '(name age))
                         '((delimiter . #\;) (missing . "NA")))
        (data-frame->json (data-frame-select-columns two '(name age)) '((sort-keys . #t))))
)

(
  "Reading options"
  ((("d" . date) ("v" . integer)) "2024-01-15,1\n2023-06-01,2\n"
   (("a" . string) ("b" . integer)) (("b" . integer)) (("x" 1)))
  (define dated
    (csv->data-frame "d;v\n2024-01-15;1\n2023-06-01;2\n"
                     '((delimiter . #\;) (date-format "yyyy-MM-dd"))))
  (list (data-frame-column-types dated)
        (data-frame->csv dated '((header . #f) (date-format . "yyyy-MM-dd")))
        (data-frame-column-types (csv->data-frame "a,b\n1,2\n" '((types ("a" . string)))))
        (data-frame-column-types (csv->data-frame "a,b\n1,2\n" '((columns "b"))))
        (data-frame->rows (csv->data-frame "x 1\n" '((header . #f) (delimiter . " ")
                                                     (types ("Column 0" . string))))))
)

(
  "Reading JSON"
  ((("a" . integer) ("b" . array) ("c" . object)) ((1 (1 2) ((x . 1))) (2 () ())))
  (define j (json->data-frame "[{\"a\":1,\"b\":[1,2],\"c\":{\"x\":1}},{\"a\":2,\"b\":[],\"c\":{}}]"))
  (list (data-frame-column-types j)
        (data-frame->rows j))
)

(
  "Files"
  (#t ("age" "city" "member" "name" "score") #t)
  (define csv-file (tmp-file "lispkit-data-frame-test.csv"))
  (define json-file (tmp-file "lispkit-data-frame-test.json"))
  (delete-if-exists csv-file)
  (delete-if-exists json-file)
  (save-csv-data-frame csv-file df)
  (save-json-data-frame json-file df)
  (let ((d1 (load-csv-data-frame csv-file))
        (d2 (load-json-data-frame json-file '((types ("member" . boolean))))))
    (delete-if-exists csv-file)
    (delete-if-exists json-file)
    (list (equal? d1 df)
          (data-frame-column-names d2)
          (equal? (data-frame->rows (data-frame-select-columns d2 (data-frame-column-names df)))
                  (data-frame->rows df))))
)

(
  "Formatting and equality"
  (#t #t #f #t)
  (list (string? (data-frame->string df '((max-rows . 2) (types . #f))))
        (equal? (data-frame-head df 2) (data-frame-slice df 0 2))
        (equal? (data-frame-head df 2) (data-frame-head df 3))
        (call-with-values (lambda () (data-frame-random-split df 0.6 42))
          (lambda (a b) (= (+ (data-frame-row-count a) (data-frame-row-count b)) 5))))
)

(
  "Errors"
  (#t #t #t #t #t #t #t #t #t #t #t #t #t #t)
  (list (raises? (make-data-frame '((a 1 2) (b 1))))
        (raises? (make-data-frame '((a 1 "x"))))
        (raises? (make-data-frame '((a 1) (a 2))))
        (raises? (data-frame-column df "height"))
        (raises? (data-frame-ref df 5 "age"))
        (raises? (data-frame-set-column df "x" '(1 2)))
        (raises? (data-frame-append df (data-frame-select-columns df '(name))))
        (raises? (data-frame-join df (make-data-frame '((city 1))) "city"))
        (raises? (data-frame-aggregate df "city" '((s sum name))))
        (raises? (csv->data-frame "a,b\n1,2\n" '((separator . #\;))))
        (raises? (csv->data-frame "a,b\n1\n"))
        (raises? (data-frame-rename-column df "age" "name"))
        (raises? (make-data-frame '((x 0.5)) '((x . float))))
        (raises? (csv->data-frame "x\n0.5\n" '((types ("x" . float))))))
)
