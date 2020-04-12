;;; LISPKIT CSV
;;;
;;; Simple API for reading and writing data in CSV format. The API provides two different
;;; levels of abstraction: reading and writing at 1) line-level (lower-level API), and 2)
;;; record-level (higher-level API). Both levels use a `csv-port` to configure the textual
;;; input/output port, the separator and quotation character.
;;;
;;; The line-level API provides means to read a whole CSV file via `csv-read` and write
;;; data in CSV format via `csv-write`:
;;;
;;;   (csv-read csv-port)
;;;   (csv-read csv-port readheader?)
;;;     Reads from `csv-port` first the header (if `readheader?` is set to `#t`) and then
;;;     all the lines until the end of the input is reached. This function returns two
;;;     values: the header line (a list of strings representing the column names), and
;;;     a vector of all data lines (which itself are lists of strings representing the
;;;     individual field values).
;;;
;;;   (csv-write csv-port header lines)
;;;     First writes the header (a list of strings representing the column names) unless
;;;     the header is set to `#f`. Then `csv-write` writes each line of `lines` (which is
;;;     a vector of lists representing the individual field values).
;;;
;;; The higher level API has a notion of records. The default representation for records
;;; are association lists. The functions for reading and writing records are
;;; `csv-read-records` and `csv-write-records`:
;;;
;;;   (csv-read-records csv-port)
;;;   (csv-read-records csv-port make-column)
;;;   (csv-read-records csv-port make-column make-record)
;;;     Reads from `csv-port` first the header and then all the data lines until the end
;;;     of the input is reached. Header names are mapped via `make-column` into column
;;;     identifiers. With `make-record` a list of column identifiers and a list of the
;;;     fields of a data line are mapped into a record. `csv-read-records` returns a
;;;     vector of records. The default `make-column` function is `make-symbol-column`.
;;;     The default `make-record` function is `make-alist-record/excess`.
;;;
;;;   (csv-write-records csv-port header records)
;;;   (csv-write-records csv-port header records column->string)
;;;   (csv-write-records csv-port header records column->string field->string)
;;;     First writes the header by mapping `header` (which is a list of column
;;;     identifiers) to a list of header names using function `column->string`.
;;;     Then writes all the records from the vector `records` by mapping each
;;;     record to a data line. This is done by applying `field->string` to all
;;;     column identifiers for the record. `field->string` takes two arguments:
;;;     a column identifier and the record. The default implementation for procedure
;;;     `column->string` is `symbol->string`. The default implementation for
;;;     procedure `field-string` is `alist-field->string`.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2019 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed under the
;;; License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;;; either express or implied. See the License for the specific language governing permissions
;;; and limitations under the License.

(define-library (lispkit csv)

  (export csv-port?
          csv-input-port?
          csv-output-port?
          make-csv-port
          csv-base-port
          csv-separator
          csv-quotechar
          csv-read-records
          csv-read
          csv-read-line
          csv-write-records
          csv-write
          csv-write-line
          make-symbol-column
          make-alist-record
          make-alist-record/excess
          alist-field->string)

  (import (lispkit base))

  (begin

    (define-record-type csv-port
                        (new-csv-port port separator quotechar)
                        csv-port?
                        (port csv-base-port)
                        (separator csv-separator)
                        (quotechar csv-quotechar))

    (define (make-csv-port . args)
      (let-optionals args ((port (current-input-port))
                           (separator #\,)
                           (quotechar #\"))
        (if (not (textual-port? port))
            (error "make-csv-port: port expected to be a textual port" port))
        (if (not (char? separator))
            (error "make-csv-port: separator expected to be a character" separator))
        (if (not (char? quotechar))
            (error "make-csv-port: quotechar expected to be a character" quotechar))
        (new-csv-port port separator quotechar)))

    (define (csv-input-port? csvp)
      (and (csv-port? csvp) (output-port? (csv-base-port csvp))))

    (define (csv-output-port? csvp)
      (and (csv-port? csvp) (output-port? (csv-base-port csvp))))

    (define (csv-read-records csvp . args)
      (let-optionals args ((make-column make-symbol-column)
                           (make-record make-alist-record/excess))
        (let-values (((header lines) (csv-read csvp (procedure? make-column))))
          (let ((columns (if (procedure? make-column)
                             (map make-column header)
                             make-column)))
            (vector-map! (lambda (line) (make-record columns line)) lines)
            lines))))

    (define (make-symbol-column str)
      (let ((name (string-trim str)))
        (if (zero? (string-length name))
            #t
            (string->symbol name))))

    (define (make-alist-record/excess columns fields)
      (if (pair? columns)
          (let ((vals (if (pair? fields) fields (list #f))))
            (cond ((procedure? (car columns))
                    (let ((association ((car columns) (car vals))))
                      (if association
                          (cons association (make-alist-record/excess (cdr columns) (cdr vals)))
                          (make-alist-record/excess (cdr columns) (cdr vals)))))
                  ((car columns)
                    (cons (cons (car columns) (car vals))
                          (make-alist-record/excess (cdr columns) (cdr vals))))
                  (else
                    (make-alist-record/excess (cdr columns) (cdr vals)))))
          (if (pair? fields)
              (cons (cons #f (car fields)) (make-alist-record/excess '() (cdr fields)))
              '())))

    (define (make-alist-record columns fields)
      (if (pair? columns)
          (let ((vals (if (pair? fields) fields (list #f))))
            (cond ((procedure? (car columns))
                    (let ((association ((car columns) (car vals))))
                      (if association
                          (cons association (make-alist-record (cdr columns) (cdr vals)))
                          (make-alist-record (cdr columns) (cdr vals)))))
                  ((car columns)
                    (cons (cons (car columns) (car vals))
                          (make-alist-record (cdr columns) (cdr vals))))
                  (else
                    (make-alist-record (cdr columns) (cdr vals)))))
          '()))

    (define (csv-read csvp . args)
      (let-optionals args ((readheader #t))
        (let ((columns (if readheader
                           (or (csv-read-header csvp) (error "csv-read: missing header"))
                           #f)))
          (do ((cur (csv-read-line csvp) (csv-read-line csvp))
               (lines '() (if (csv-valid-line? cur)
                              (cons cur lines)
                              lines)))
              ((eof-object? cur)
               (let ((res (list->vector lines)))
                 (vector-reverse! res)
                 (values columns res)))))))

    (define (csv-read-header csvp)
      (let ((line (csv-read-line csvp)))
        (if (csv-valid-line? line)
            line
            (if (eof-object? line)
                #f
                (csv-read-header csvp)))))

    (define (csv-valid-line? line)
      (and (pair? line) (not (equal? line '("")))))

    (define (csv-read-line csvp)
      (let ((port      (csv-base-port csvp))
            (separator (csv-separator csvp)))
        (if (eof-object? (peek-char port))
            (read-char port)
            (do ((fields (list (csv-read-field csvp)) (cons (csv-read-field csvp) fields))
                 (ch     (peek-char port)             (peek-char port)))
                ((cond ((eof-object? ch)
                         #t)
                       ((char=? #\newline ch)
                         (read-char port))
                       ((char=? #\return ch)
                         (read-char port)
                         (if (char=? #\newline (peek-char port))
                             (read-char port)
                             #t))
                       ((char=? separator ch)
                         (read-char port)
                         #f)
                       (else
                         (error "csv-read-line: broken record terminator" ch)))
                 (reverse fields))))))

    (define (csv-read-field csvp)
      (let ((port        (csv-base-port csvp))
            (terminators (list #\newline #\return (csv-separator csvp) (eof-object)))
            (quotechar   (csv-quotechar csvp)))
        (cond ((eof-object? (peek-char port))
                "")
              ((char=? quotechar (peek-char port))
                (read-char port)
                (do ((chars '() (cons (read-char port) chars)))
                    ((cond ((eof-object? (peek-char port))
                             (error "csv-read-field: field terminated without closing quote"))
                           ((char=? quotechar (peek-char port))
                             (read-char port)
                             (cond ((memq (peek-char port) terminators) #t)
                                   ((char=? quotechar (peek-char port)) #f)
                                   (else (error "csv-read-field: unbalanced quotes"
                                                (peek-char port)))))
                           (else
                             #f))
                     (list->string (reverse chars)))))
              (else
                (do ((chars '() (cons (read-char port) chars)))
                    ((memq (peek-char port) terminators)
                     (list->string (reverse chars))))))))

    (define (csv-write-records csvp header records . args)
      (let-optionals args ((column->string symbol->string)
                           (field->string alist-field->string))
        (csv-write-line csvp (map column->string header))
        (vector-for-each
          (lambda (record)
            (csv-write-line csvp (map (lambda (column) (field->string record column)) header)))
          records)))

    (define (alist-field->string record column)
      (cdr (assq column record)))

    (define (csv-write csvp header lines)
      (if header
          (csv-write-line csvp header))
      (vector-for-each (lambda (line) (csv-write-line csvp line)) lines))

    (define (csv-write-line csvp line)
      (let* ((port      (csv-base-port csvp))
             (separator (csv-separator csvp))
             (quotechar (csv-quotechar csvp))
             (quotestr  (string quotechar))
             (escquote  (string quotechar quotechar)))
        (do ((fields line (cdr fields)))
            ((null? fields))
          (let ((str (string-copy (car fields))))
            (write-char quotechar port)
            (string-replace! str quotestr escquote)
            (write-string str port)
            (write-char quotechar port))
          (if (null? (cdr fields))
              (write-char #\newline port)
              (write-char separator port)))))
  )
)
