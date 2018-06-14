;;; SRFI69.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2018 ObjectHub. All rights reserved.
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
  "eq?-based hash-tables: insert and lookup"
  (0 3 #t #t #t #f white black pink error grey grey (cat dog elephant) (black pink white)
    ((cat . black) (dog . white) (elephant . pink)))
  (import (srfi 69))
  (define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))
  (define (assoc<? a b) (symbol<? (car a) (car b)))
  (define ht (make-hash-table eq?))
  (list (hash-table-size ht)
        (begin (hash-table-set! ht 'cat 'black)
               (hash-table-set! ht 'dog 'white)
               (hash-table-set! ht 'elephant 'pink)
               (hash-table-size ht))
        (hash-table-exists? ht 'dog)
        (hash-table-exists? ht 'cat)
        (hash-table-exists? ht 'elephant)
        (hash-table-exists? ht 'goose)
        (hash-table-ref ht 'dog)
        (hash-table-ref ht 'cat)
        (hash-table-ref ht 'elephant)
        (guard (e (#t 'error)) (hash-table-ref ht 'goose))
        (hash-table-ref ht 'goose (lambda () 'grey))
        (hash-table-ref/default ht 'goose 'grey)
        (sort symbol<? (hash-table-keys ht))
        (sort symbol<? (hash-table-values ht))
        (sort assoc<? (hash-table->alist ht)))
)

(
  "eq?-based hash-tables: remove"
  (2 #f #t #t error black pink (cat elephant) (black pink) ((cat . black) (elephant . pink)))
  (hash-table-delete! ht 'dog)
  (list (hash-table-size ht)
        (hash-table-exists? ht 'dog)
        (hash-table-exists? ht 'cat)
        (hash-table-exists? ht 'elephant)
        (guard (e (#t 'error)) (hash-table-ref ht 'dog))
        (hash-table-ref ht 'cat)
        (hash-table-ref ht 'elephant)
        (sort symbol<? (hash-table-keys ht))
        (sort symbol<? (hash-table-values ht))
        (sort assoc<? (hash-table->alist ht)))
)

(
  "eq?-based hash-tables: remove non-existent"
  (2 #f)
  (hash-table-delete! ht 'dog)
  (list (hash-table-size ht)
        (hash-table-exists? ht 'dog))
)

(
  "eq?-based hash-tables: overwrite an existing element"
  (2 #f #t #t error calico pink (cat elephant) (calico pink) ((cat . calico) (elephant . pink)))
  (hash-table-set! ht 'cat 'calico)
  (list (hash-table-size ht)
        (hash-table-exists? ht 'dog)
        (hash-table-exists? ht 'cat)
        (hash-table-exists? ht 'elephant)
        (guard (e (#t 'error)) (hash-table-ref ht 'dog))
        (hash-table-ref ht 'cat)
        (hash-table-ref ht 'elephant)
        (sort symbol<? (hash-table-keys ht))
        (sort symbol<? (hash-table-values ht))
        (sort assoc<? (hash-table->alist ht)))
)

(
  "eq?-based hash-tables: walk and fold"
  (((cat . calico) (elephant . pink)) ((cat . calico) (elephant . pink)))
  (list  (let ((a '()))
           (hash-table-walk ht (lambda (k v) (set! a (cons (cons k v) a))))
           (sort assoc<? a))
        (sort assoc<? (hash-table-fold ht (lambda (k v a) (cons (cons k v) a)) '())))
)

(
  "eq?-based hash-tables: copy"
  (2 #f #t #t error calico pink (cat elephant) (calico pink) ((cat . calico) (elephant . pink)))
  (define ht2 (hash-table-copy ht))
  (list (hash-table-size ht2)
        (hash-table-exists? ht2 'dog)
        (hash-table-exists? ht2 'cat)
        (hash-table-exists? ht2 'elephant)
        (guard (e (#t 'error)) (hash-table-ref ht2 'dog))
        (hash-table-ref ht2 'cat)
        (hash-table-ref ht2 'elephant)
        (sort symbol<? (hash-table-keys ht2))
        (sort symbol<? (hash-table-values ht2))
        (sort assoc<? (hash-table->alist ht2)))
)

(
  "eq?-based hash-tables: merge"
  (1 #f #t 3 #t #t #t #f brown calico pink error grey (bear cat elephant) (brown calico pink)
    ((bear . brown) (cat . calico) (elephant . pink)))
  (define ht2 (make-hash-table eq?))
  (hash-table-set! ht2 'bear 'brown)
  (list (hash-table-size ht2)
        (hash-table-exists? ht2 'dog)
        (hash-table-exists? ht2 'bear)
        (begin (hash-table-merge! ht2 ht)
               (hash-table-size ht2))
        (hash-table-exists? ht2 'bear)
        (hash-table-exists? ht2 'cat)
        (hash-table-exists? ht2 'elephant)
        (hash-table-exists? ht2 'goose)
        (hash-table-ref ht2 'bear)
        (hash-table-ref ht2 'cat)
        (hash-table-ref ht2 'elephant)
        (guard (e (#t 'error)) (hash-table-ref ht2 'goose))
        (hash-table-ref/default ht2 'goose 'grey)
        (sort symbol<? (hash-table-keys ht2))
        (sort symbol<? (hash-table-values ht2))
        (sort assoc<? (hash-table->alist ht2)))
)

(
  "eq?-based hash-tables: alist"
  ((cat . calico) (elephant . pink))
  (sort assoc<? (hash-table->alist (alist->hash-table '((cat . calico) (elephant . pink)))))
)

(
  "eq?-based hash-tables: update"
  (2 3)
  (define ht (make-hash-table eq?))
  (define (add1 x) (+ x 1))
  (hash-table-set! ht 'sheep 0)
  (hash-table-update! ht 'sheep add1)
  (hash-table-update! ht 'sheep add1)
  (list (hash-table-ref ht 'sheep)
        (begin (hash-table-update!/default ht 'crows add1 0)
               (hash-table-update!/default ht 'crows add1 0)
               (hash-table-update!/default ht 'crows add1 0)
               (hash-table-ref ht 'crows)))
)

(
  "eq?-based hash-tables: string keys"
  (white white black pink error grey ("cat" "dog" "elephant") (black pink white))
  (define ht (make-hash-table equal?))
  (hash-table-set! ht "cat" 'black)
  (hash-table-set! ht "dog" 'white)
  (hash-table-set! ht "elephant" 'pink)
  (list (hash-table-ref/default ht "dog" #f)
        (hash-table-ref ht "dog")
        (hash-table-ref ht "cat")
        (hash-table-ref ht "elephant")
        (guard (e (#t 'error)) (hash-table-ref ht "goose"))
        (hash-table-ref/default ht "goose" 'grey)
        (sort string<? (hash-table-keys ht))
        (sort symbol<? (hash-table-values ht)))
)

(
  "eq?-based hash-tables: string ci keys"
  (white white black pink error grey ("cat" "dog" "elephant") (black pink white))
  (define ht (make-hash-table string-ci=? string-ci-hash))
  (hash-table-set! ht "cat" 'black)
  (hash-table-set! ht "dog" 'white)
  (hash-table-set! ht "elephant" 'pink)
  (list (hash-table-ref/default ht "DOG" #f)
        (hash-table-ref ht "DOG")
        (hash-table-ref ht "Cat")
        (hash-table-ref ht "eLePhAnT")
        (guard (e (#t 'error)) (hash-table-ref ht "goose"))
        (hash-table-ref/default ht "goose" 'grey)
        (sort string<? (hash-table-keys ht))
        (sort symbol<? (hash-table-values ht)))
)
