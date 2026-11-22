;;; TextTable.scm
;;; Regression test data for library (lispkit text-table)
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
  (import (lispkit text-table)
          (lispkit data-frame))
  ;; Returns #t if calling `thunk` raises an error
  (define (call-raises? thunk)
    (guard (x (#t #t)) (thunk) #f))
  (define-syntax raises?
    (syntax-rules ()
      ((_ expr) (call-raises? (lambda () expr)))))
  (define df
    (csv->data-frame
      (string-append "name,city,age,score,member\n"
                     "Alice,Zurich,34,88.5,true\n"
                     "Bob,Basel,,72.0,\n"
                     "Carol,Zurich,29,NA,true\n"
                     "Dave,Bern,41,95.25,false\n")))
  #t
)

(
  "Text tables"
  (#t #t #f #t #t)
  (define tt
    (make-text-table '(("ID" center right) ("Name" center left) ("Address" center left 10 20))
                     double-line-sep))
  (add-text-table-row! tt '("1" "Mark Smith" "2600 Windsor Road\nRedwood City, CA"))
  (add-text-table-separator! tt line-sep)
  (add-text-table-row! tt '("2" "Emily Armstrong" "160 Randy Rock Way, Mountain View, CA"))
  (list (text-table? tt)
        (text-table-header? '("A" ("B" left) ("C" center right 2 10)))
        (text-table-header? '(("A" up)))
        (text-table-row? '("x" "y"))
        (text-table-separator? line-sep))
)

(
  "Rendering text tables"
  "┌────┬─────────────────┬──────────────────────┐\n│ ID │      Name       │       Address        │\n╞════╪═════════════════╪══════════════════════╡\n│  1 │ Mark Smith      │ 2600 Windsor Road    │\n│    │                 │ Redwood City, CA     │\n├────┼─────────────────┼──────────────────────┤\n│  2 │ Emily Armstrong │ 160 Randy Rock Way,… │\n└────┴─────────────────┴──────────────────────┘"
  (text-table->string tt)
)

(
  "Rendering text tables without border"
  "╷    ╷                 ╷                      ╷\n│ ID │      Name       │       Address        │\n╞════╪═════════════════╪══════════════════════╡\n│  1 │ Mark Smith      │ 2600 Windsor Road    │\n│    │                 │ Redwood City, CA     │\n├────┼─────────────────┼──────────────────────┤\n│  2 │ Emily Armstrong │ 160 Randy Rock Way,… │\n╵    ╵                 ╵                      ╵"
  (text-table->string tt #f)
)

(
  "Cell alignment"
  (#t #t #f #f
   "┌────────────┬──────────────┐\n│    Name    │     Note     │\n┝━━━━━━━━━━━━┿━━━━━━━━━━━━━━┥\n│ plain      │ left default │\n│       list │   centered   │\n│     dotted │     two      │\n│            │    lines     │\n│ empty      │ x            │\n└────────────┴──────────────┘")
  (define at (make-text-table '(("Name" center left 10) ("Note" center left 12))))
  (add-text-table-row! at '("plain" "left default"))
  (add-text-table-row! at '(("list" right) ("centered" center)))
  (add-text-table-row! at '(("dotted" . right) ("two\nlines" . center)))
  (add-text-table-row! at '(("empty") ("x")))
  (list (text-table-row? '(("a" right)))
        (text-table-row? '(("a" . right)))
        (text-table-row? '(("a" up)))
        (text-table-row? '(("a" right left)))
        (text-table->string at))
)

(
  "Data frames: default rendering"
  "┌───────┬────────┬─────┬───────┬────────┐\n│ name  │  city  │ age │ score │ member │\n┝━━━━━━━┿━━━━━━━━┿━━━━━┿━━━━━━━┿━━━━━━━━┥\n│ Alice │ Zurich │  34 │  88.5 │  true  │\n│ Bob   │ Basel  │     │  72.0 │        │\n│ Carol │ Zurich │  29 │       │  true  │\n│ Dave  │ Bern   │  41 │ 95.25 │ false  │\n└───────┴────────┴─────┴───────┴────────┘"
  (text-table->string (data-frame->text-table df))
)

(
  "Data frames: columns, headers, missing values, separators, edges, row limit, indices"
  "╭───┬───────┬────────┬────────╮\n│   │ Name  │ Age    │ member │\n╞═══╪═══════╪════════╪════════╡\n│ 0 │ Alice │     34 │  true  │\n├───┼───────┼────────┼────────┤\n│ 1 │ Bob   │      – │   –    │\n│ … │ …     │      … │   …    │\n╰───┴───────┴────────┴────────╯"
  (text-table->string
    (data-frame->text-table df
      '((columns name age member)
        (headers (name . "Name") ("age" "Age" left right 6))
        (missing . "–")
        (row-sep . line-sep)
        (edges . round-edges)
        (header-sep . double-line-sep)
        (max-rows . 2)
        (row-indices . #t))))
)

(
  "Data frames: column formatters and alignment"
  "╷       ╷       ╷\n│ score │ name  │\n│    88 │ Alice │\n│    72 │   Bob │\n│       │ Carol │\n│    95 │  Dave │\n╵       ╵       ╵"
  (text-table->string
    (data-frame->text-table df
      `((columns "score" "name")
        (format ("score" . ,(lambda (x) (number->string (exact (round x))))))
        (align (name . right))
        (header-sep . no-sep)))
    #f)
)

(
  "Data frames: global formatter and maximum cell width"
  "┌──────┬──────┐\n│ name │ city │\n┝━━━━━━┿━━━━━━┥\n│ Ali… │ ZUR… │\n│ Bob  │ BAS… │\n│ Car… │ ZUR… │\n│ Dave │ BERN │\n└──────┴──────┘"
  (text-table->string
    (data-frame->text-table df
      `((columns name city)
        (format . ,(lambda (x name) (if (string=? name "city") (string-upcase x) x)))
        (max-cell-width . 4))))
)

(
  "Data frames: multi-line cells and lists"
  "┌───────┬───────────┐\n│ note  │   tags    │\n┝━━━━━━━┿━━━━━━━━━━━┥\n│ two   │ (\"a\" \"b\") │\n│ lines │           │\n└───────┴───────────┘"
  (text-table->string
    (data-frame->text-table (make-data-frame '((note "two\nlines") (tags (a b))))))
)

(
  "Data frames: without rows"
  #t
  (string? (text-table->string (data-frame->text-table (make-data-frame '((a) (b))))))
)

(
  "Data frames: errors"
  (#t #t #t #t #t)
  (list (raises? (data-frame->text-table 'df))
        (raises? (data-frame->text-table df '((colour . red))))
        (raises? (data-frame->text-table df '((row-sep . zigzag))))
        (raises? (data-frame->text-table df '((columns "height"))))
        (raises? (data-frame->text-table (make-data-frame '()))))
)
