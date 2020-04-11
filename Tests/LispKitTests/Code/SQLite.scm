;;; SQLite.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2020 ObjectHub. All rights reserved.
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
  "SQLite new table"
  #t
  ;; Import library `(lispkit sqlite)`
  (import (lispkit sqlite))
  ;; Make a new in-memory sqlite database
  (define db (make-database))
  ;; Create a new table in the database
  (define stmt0
    (prepare-statement db
      (string-append "CREATE TABLE Contacts (id INTEGER PRIMARY KEY,"
                     "                       name TEXT NOT NULL,"
                     "                       email TEXT NOT NULL UNIQUE,"
                     "                       phone TEXT);")))
  (process-statement stmt0)
)

(
  "SQLite insert values"
  (#t #t)
  ;; Enter two rows into the new table.
  (define stmt1 (prepare-statement db "INSERT INTO Contacts VALUES (?, ?, ?, ?);"))
  (bind-parameter stmt1 1 1000)
  (bind-parameter stmt1 2 "Mickey Mouse")
  (bind-parameter stmt1 3 "mickey@disney.net")
  (bind-parameter stmt1 4 "+1 101-123-456")
  (define result (process-statement stmt1))
  (bind-parameters stmt1 '(1001 "Donald Duck" "donald@disney.net" "+1 101-123-456"))
  (list result (process-statement stmt1))
)

(
  "SQLite count"
  (#f 1 #t)
  ;; Count the number of distinct phone numbers.
  (define stmt2 (prepare-statement db "SELECT COUNT(DISTINCT phone) FROM Contacts;"))
  (list (process-statement stmt2)
        (column-value stmt2 0)
        (process-statement stmt2))
)

(
  "SQLite select"
  (("Donald Duck" "donald@disney.net") ("Mickey Mouse" "mickey@disney.net"))
  ;; Show all names and email addresses from the `Contacts` table.
  (define stmt3 (prepare-statement db "SELECT name, email FROM Contacts;"))
  (do ((res '() (cons (row-values stmt3) res)))
      ((process-statement stmt3) res))
)
