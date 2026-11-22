;;; Analyze tabular data with data frames
;;;
;;; This code showcases the API provided by library `(lispkit data-frame)`. It
;;; analyzes the orders of a small online bookshop. The orders are read from CSV
;;; data, the customers from JSON data. The code inspects the data, cleans it up,
;;; derives new columns, filters and sorts orders, computes revenue statistics by
;;; category, city, and month, joins orders with customers, writes the results as
;;; CSV and JSON, and displays data frames as text tables with the help of library
;;; `(lispkit text-table)`.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
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
        (lispkit date-time)
        (lispkit format)
        (lispkit data-frame)
        (lispkit text-table)
        (only (lispkit system)
          path system-directory file-exists? delete-file-or-directory))

;;; Output

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
       (write expr)
       (newline)))))

;; Formats dates as year-month-day, independent of the locale of the system.
(define (format-date d)
  (date-time->string d 'en_US "yyyy-MM-dd"))

;; Formats numbers with two decimal places.
(define (format-amount x)
  (format "~,2F" x))

;; Displays data frame `df` as a text table. Dates and the columns listed in `amounts`
;; are formatted explicitly; all other options are passed on to `data-frame->text-table`.
(define (display-frame df . args)
  (let-optionals args ((amounts '()) (options '()))
    (let* ((types (data-frame-column-types df))
           (formats (map (lambda (entry)
                           (cons (car entry)
                                 (if (member (car entry) amounts) format-amount format-date)))
                         (filter (lambda (entry)
                                   (or (eq? (cdr entry) 'date) (member (car entry) amounts)))
                                 types))))
      (display (text-table->string
                 (data-frame->text-table df (cons (cons 'format formats) options))))
      (newline))))

;; Prints a multi-line text indented by two spaces.
(define (display-text-lines text)
  (for-each (lambda (line)
              (if (not (string=? line ""))
                  (begin (display* "  | " line) (newline))))
            (string-split text #\newline)))

;; Prints a headline for the next section.
(define (section title)
  (newline)
  (display* "=== " title " ===")
  (newline))

;;; Reading data

(section "Reading data")

(define (lines . strings)
  (apply string-append (map (lambda (str) (string-append str "\n")) strings)))

;; Orders of the bookshop as CSV data. Some values are missing: the price of order
;; 1007 is unknown ("n/a") and the payment status of three orders is empty.
(define orders-csv
  (lines "order;date;customer;category;title;quantity;price;paid"
         "1001;2026-01-05;C01;Fiction;The Silent River;1;18.50;yes"
         "1002;2026-01-09;C02;Science;Cosmic Patterns;2;32.00;yes"
         "1003;2026-01-17;C03;Fiction;Night Train;3;12.90;no"
         "1004;2026-01-28;C01;Cooking;Seasonal Kitchen;1;27.00;yes"
         "1005;2026-02-02;C04;Science;The Living Cell;1;45.00;"
         "1006;2026-02-11;C02;Fiction;The Silent River;2;18.50;yes"
         "1007;2026-02-14;C05;History;Empires of Salt;1;n/a;no"
         "1008;2026-02-20;C03;Cooking;Bread and Time;2;24.00;yes"
         "1009;2026-03-03;C06;Science;Cosmic Patterns;1;32.00;"
         "1010;2026-03-08;C04;Fiction;Night Train;4;12.90;yes"
         "1011;2026-03-15;C01;History;Empires of Salt;1;29.50;yes"
         "1012;2026-03-22;C05;Science;Quantum Gardens;2;38.00;"))

;; The CSV data uses semicolons as delimiters, "yes" and "no" for booleans, and "n/a"
;; for missing values. Dates are recognized via a date format. The `order` column
;; should be a string column, not an integer column.
(define orders
  (csv->data-frame orders-csv
    '((delimiter . #\;)
      (true "yes")
      (false "no")
      (missing "" "n/a")
      (date-format "yyyy-MM-dd")
      (types ("order" . string)))))

(display-frame orders '("price"))

;; Customers as JSON data: an array of objects, one object per row. Columns of data
;; frames created from JSON are sorted by name.
(define customers
  (json->data-frame
    (string-append
      "[{\"customer\": \"C01\", \"name\": \"Ada\", \"city\": \"Zurich\", \"since\": 2019},"
      " {\"customer\": \"C02\", \"name\": \"Ben\", \"city\": \"Basel\", \"since\": 2021},"
      " {\"customer\": \"C03\", \"name\": \"Chloé\", \"city\": \"Zurich\", \"since\": 2023},"
      " {\"customer\": \"C04\", \"name\": \"Dario\", \"city\": \"Lugano\", \"since\": 2020},"
      " {\"customer\": \"C05\", \"name\": \"Elif\", \"city\": \"Basel\", \"since\": 2024}]")))

(display-frame customers)

;;; Inspecting data frames

(section "Inspecting data frames")

(show (data-frame? orders))
(show (data-frame-row-count orders))
(show (data-frame-column-names orders))
(show (data-frame-column-types orders))
(show (data-frame-column orders "quantity"))
(show (data-frame-row orders 1))
(show (data-frame-ref orders 3 "title"))
(show (data-frame-distinct-values orders "category"))

;; Missing values are represented by #f. In boolean columns, #f is also the value
;; false. Procedure `data-frame-missing?` tells the two apart.
(show (data-frame-ref orders 6 "price"))
(show (data-frame-missing-count orders "paid"))
(show (list (data-frame-ref orders 2 "paid") (data-frame-missing? orders 2 "paid")))
(show (list (data-frame-ref orders 4 "paid") (data-frame-missing? orders 4 "paid")))

;; Summary statistics for the numeric columns: the summary is a data frame itself.
(display (data-frame->string (data-frame-summary orders '("quantity" "price"))
                             '((max-width . 110))))

;;; Cleaning up and deriving data

(section "Cleaning up and deriving data")

;; The price of order 1007 is missing. Since the same title appears in order 1011,
;; its price is taken from there.
(define prices
  (let ((titles (data-frame-column orders "title"))
        (prices (data-frame-column orders "price")))
    (let loop ((i 0) (res '()))
      (if (= i (vector-length titles))
          res
          (loop (+ i 1)
                (if (and (vector-ref prices i) (not (assoc (vector-ref titles i) res)))
                    (cons (cons (vector-ref titles i) (vector-ref prices i)) res)
                    res))))))

(define cleaned
  (data-frame-compute-column orders "price" '("title" "price")
    (lambda (title price) (or price (cdr (assoc title prices))))))

(show (data-frame-ref cleaned 6 "price"))

;; New columns are computed from existing ones: the revenue of each order and the
;; month in which it was placed.
(define enriched
  (data-frame-compute-column
    (data-frame-compute-column cleaned "revenue" '("quantity" "price") *)
    "month" '("date")
    (lambda (date) (date-time->string date 'en_US "yyyy-MM"))))

(display-frame (data-frame-select-columns enriched '("order" "month" "title" "revenue"))
               '("revenue"))

;; Columns can be renamed, dropped, and replaced.
(show (data-frame-column-names
        (data-frame-rename-column (data-frame-drop-columns enriched '("date" "title")) "paid" "settled")))
(show (data-frame-column (data-frame-update-column enriched "category" string-upcase) "category"))

;;; Filtering, sorting, and slicing

(section "Filtering, sorting, and slicing")

;; Large orders: at least two books or a revenue above 40.
(define large
  (data-frame-filter enriched
    (lambda (quantity revenue) (or (>= quantity 2) (> revenue 40.0)))
    '("quantity" "revenue")))

(display-frame (data-frame-select-columns large '("order" "customer" "quantity" "revenue"))
               '("revenue"))

;; Orders sorted by category, then by descending revenue.
(display-frame (data-frame-select-columns
                 (data-frame-sort enriched '("category" ("revenue" . descending)))
                 '("category" "title" "revenue"))
               '("revenue"))

;; The three orders with the highest revenue.
(show (data-frame-column
        (data-frame-head (data-frame-sort enriched '(("revenue" . descending))) 3)
        "order"))
(show (data-frame-column (data-frame-slice enriched 4 7) "order"))
(show (data-frame-column (data-frame-select-rows enriched '(11 0)) "order"))

;; Orders whose payment status is still unknown. `data-frame-filter` passes #f for
;; both false and missing values, so the rows are checked with `data-frame-missing?`.
(show (filter (lambda (i) (data-frame-missing? enriched i "paid"))
              (iota (data-frame-row-count enriched))))

;; `data-frame-for-each` iterates over rows, e.g. for printing them.
(data-frame-for-each (data-frame-head enriched 3)
  (lambda (order title quantity)
    (display* "  order " order ": " quantity " × " title)
    (newline))
  '("order" "title" "quantity"))

;;; Aggregating data

(section "Aggregating data")

;; Revenue statistics by category. Groups appear in the order of their first row.
(display-frame
  (data-frame-sort
    (data-frame-aggregate enriched "category"
      '((orders count) (books sum quantity) (revenue sum revenue)
        (average mean revenue) (largest max revenue)))
    '(("revenue" . descending)))
  '("revenue" "average" "largest"))

;; Revenue by month.
(display-frame
  (data-frame-aggregate enriched "month"
    '((orders count) (customers distinct customer) (revenue sum revenue)))
  '("revenue"))

;; Without grouping columns, all rows form a single group.
(display-frame
  (data-frame-aggregate enriched '()
    '((orders count) (paid count paid) (first first date) (last last date)
      (median median revenue) (std std revenue)))
  '("median" "std"))

;;; Joining data frames

(section "Joining data frames")

;; A left join keeps all orders, also the one of customer C06, who is not listed
;; in the customers data frame. Columns other than the join column get prefixes.
(define joined (data-frame-join enriched customers "customer" 'left))

(show (data-frame-column-names joined))

;; Revenue by city; orders of unknown customers end up in a group with a missing city.
(display-frame
  (data-frame-aggregate
    (data-frame-rename-column joined "right.city" "city")
    "city" '((customers distinct customer) (revenue sum left.revenue)))
  '("revenue")
  '((missing . "(unknown)")))

;; An inner join only keeps orders with a matching customer.
(show (data-frame-row-count (data-frame-join enriched customers "customer")))

;;; Creating data frames from Scheme data

(section "Creating data frames from Scheme data")

;; Column types are inferred from the values. #f denotes missing values, except in
;; columns which only consist of booleans.
(define targets
  (make-data-frame
    '(("category" "Fiction" "Science" "Cooking" "History")
      ("target" 120 150 60 #f)
      ("promoted" #t #f #f #t))))

(show (data-frame-column-types targets))

;; Data frames can also be built row by row.
(define extra
  (rows->data-frame '("category" "target" "promoted")
                    '(("Poetry" 40 #f) ("Travel" 75.5 #t))))

(show (data-frame-column-types extra))

;; Appending data frames requires columns with equal names and types. Explicit types
;; make the target column of `targets` a double column.
(define all-targets
  (data-frame-append
    (make-data-frame (data-frame->columns targets) '(("target" . double)))
    extra))

(display-frame all-targets '() '((missing . "–")))

;; Rows can be appended directly as well.
(show (data-frame->rows (data-frame-append-rows extra '(("Art" 30.0 #f)))))

;; Comparing the revenue per category with the targets.
(display-frame
  (data-frame-compute-column
    (data-frame-join
      (data-frame-aggregate enriched "category" '((revenue sum revenue)))
      all-targets "category")
    "progress" '("left.revenue" "right.target")
    (lambda (revenue target) (and target (/ (round (* 1000 (/ revenue target))) 10)))
    'double)
  '("left.revenue")
  '((headers ("left.revenue" . "revenue") ("right.target" . "target")
             ("right.promoted" . "promoted") ("progress" . "progress (%)"))
    (missing . "–")
    (row-sep . line-sep)
    (edges . round-edges)))

;;; Writing data

(section "Writing data")

(define summary
  (data-frame-select-columns
    (data-frame-aggregate enriched "category" '((orders count) (books sum quantity)))
    '("category" "orders" "books")))

(display-text-lines (data-frame->csv summary))
(display-text-lines (data-frame->csv summary '((delimiter . #\tab) (header . #f))))
(display-text-lines (data-frame->json (data-frame-head summary 2) '((pretty . #t) (sort-keys . #t))))

;; Data frames written to files can be read back.
(define tmp-file (path (car (system-directory 'temporary)) "bookshop-orders.csv"))
(save-csv-data-frame tmp-file enriched '((date-format . "yyyy-MM-dd")))
(define reloaded (load-csv-data-frame tmp-file '((date-format "yyyy-MM-dd") (types ("order" . string)))))
(delete-file-or-directory tmp-file)

(show (equal? (data-frame->rows reloaded) (data-frame->rows enriched)))

;; Splitting the orders randomly, e.g. into training and test data. A seed makes the
;; split reproducible.
(show (call-with-values (lambda () (data-frame-random-split enriched 0.75 7))
        (lambda (training test)
          (list (data-frame-row-count training) (data-frame-row-count test)))))

;;; Errors

(section "Errors")

(define-syntax try
  (syntax-rules ()
    ((_ expr)
     (guard (condition (#t (list 'error (error-object-message condition))))
       expr))))

(show (try (data-frame-column orders "discount")))
(show (try (make-data-frame '(("a" 1 2) ("b" 3)))))
(show (try (make-data-frame '(("a" 1 "two")))))
(show (try (data-frame-aggregate orders "category" '((total sum title)))))
(show (try (data-frame-append orders customers)))
(show (try (csv->data-frame "a,b\n1\n")))
