;;; LISPKIT LIST UTIL
;;;
;;; Library providing list utilities that are not covered by R7RS or the SRFIs supported by
;;; LispKit, such as `flatten`, `group-by` and the combinatorial procedures `permutations`,
;;; `combinations` and `cartesian-product`.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
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

(define-library (lispkit list util)

  (export take
          drop
          flatten
          add-between
          shuffle
          list-set
          list-update
          argmin
          argmax
          group-by
          check-duplicates
          permutations
          combinations
          cartesian-product)

  (import (lispkit base))

  (begin
  
    ;; Returns the first `i` elements from list `x`
    (define (take x i)
      (if (zero? i)
          '()
          (cons (car x) (take (cdr x) (- i 1)))))

    ;; Returns the list `x` after dropping `i` elements.
    (define (drop x i)
      (if (zero? i) x (drop (cdr x) (- i 1))))
    
    ;; Returns a list with all non-list elements of tree `x` in the order in which they occur
    ;; in `x`. The empty list is ignored; improper tails are treated as elements.
    (define (flatten x)
      (let loop ((stack (list x)) (acc '()))
        (if (null? stack)
            (reverse acc)
            (let ((top (car stack))
                  (rest (cdr stack)))
              (cond ((null? top) (loop rest acc))
                    ((pair? top) (loop (cons (car top) (cons (cdr top) rest)) acc))
                    (else        (loop rest (cons top acc))))))))

    ;; Returns a list that is like `lst`, but with `sep` inserted between all elements.
    (define (add-between lst sep)
      (if (null? lst)
          '()
          (let loop ((rest (cdr lst))
                     (acc (list (car lst))))
            (if (null? rest)
                (reverse acc)
                (loop (cdr rest) (cons (car rest) (cons sep acc)))))))

    ;; Returns a list with the elements of `lst` in random order.
    (define (shuffle lst)
      (let* ((vec (list->vector lst))
             (n (vector-length vec)))
        (do ((i (- n 1) (- i 1)))
            ((< i 1) (vector->list vec))
          (let* ((j (random (+ i 1)))
                 (tmp (vector-ref vec i)))
            (vector-set! vec i (vector-ref vec j))
            (vector-set! vec j tmp)))))

    ;; Returns a new list that is like `lst`, but with the element at index `k` replaced by
    ;; the result of applying `proc` to it.
    (define (list-update lst k proc)
      (unless (and (exact-integer? k) (>= k 0))
        (error "list-update: invalid index" k))
      (let loop ((l lst) (i k))
        (cond ((not (pair? l))
               (error "list-update: index out of range" k lst))
              ((zero? i)
               (cons (proc (car l)) (cdr l)))
              (else
               (cons (car l) (loop (cdr l) (- i 1)))))))

    ;; Returns a new list that is like `lst`, but with the element at index `k` replaced by
    ;; `obj`.
    (define (list-set lst k obj)
      (list-update lst k (lambda (x) obj)))

    ;; Returns the first element of `lst` for which `proc` returns the smallest number.
    (define (argmin proc lst)
      (when (null? lst)
        (error "argmin: expected non-empty list" lst))
      (let loop ((best (car lst))
                 (best-val (proc (car lst)))
                 (rest (cdr lst)))
        (if (null? rest)
            best
            (let ((val (proc (car rest))))
              (if (< val best-val)
                  (loop (car rest) val (cdr rest))
                  (loop best best-val (cdr rest)))))))

    ;; Returns the first element of `lst` for which `proc` returns the largest number.
    (define (argmax proc lst)
      (when (null? lst)
        (error "argmax: expected non-empty list" lst))
      (let loop ((best (car lst))
                 (best-val (proc (car lst)))
                 (rest (cdr lst)))
        (if (null? rest)
            best
            (let ((val (proc (car rest))))
              (if (> val best-val)
                  (loop (car rest) val (cdr rest))
                  (loop best best-val (cdr rest)))))))

    ;; Partitions `lst` into groups of elements for which `key` returns equivalent values
    ;; (according to `same?`, which defaults to `equal?`). The groups are ordered by the first
    ;; appearance of their key in `lst`, and each group preserves the order of `lst`.
    (define (group-add same? groups k x)
      (cond ((null? groups)
              (list (list k x)))
            ((same? (caar groups) k)
              (cons (cons (caar groups) (cons x (cdar groups))) (cdr groups)))
            (else
              (cons (car groups) (group-add same? (cdr groups) k x)))))

    (define group-by
      (case-lambda
        ((key lst)       (group-by key lst equal?))
        ((key lst same?)
          (let loop ((rest lst) (groups '()))
            (if (null? rest)
                (map (lambda (group) (reverse (cdr group))) groups)
                (loop (cdr rest) (group-add same? groups (key (car rest)) (car rest))))))))

    ;; Returns the first element of `lst` which is equivalent (according to `same?`, which
    ;; defaults to `equal?`) to an element before it. Returns `#f` if there is no such element.
    (define check-duplicates
      (case-lambda
        ((lst)       (check-duplicates lst equal?))
        ((lst same?)
          (let loop ((rest lst) (seen '()))
            (cond ((null? rest) #f)
                  ((member (car rest) seen same?) (car rest))
                  (else (loop (cdr rest) (cons (car rest) seen))))))))

    ;; Returns a list of all permutations of `lst`. The permutations are ordered by the
    ;; positions of the elements in `lst`. Elements are distinguished by position, i.e. a
    ;; list with `n` elements always has `n!` permutations.
    (define (permutations lst)
      (if (null? lst)
          '(())
          (append-map
            (lambda (i)
              (let ((x (list-ref lst i))
                    (others (append (take lst i) (drop lst (+ i 1)))))
                (map (lambda (p) (cons x p)) (permutations others))))
            (iota (length lst)))))

    ;; Returns a list of all combinations of `k` elements of `lst`. The combinations preserve
    ;; the order of `lst` and are ordered by the positions of their elements. If `k` is not
    ;; provided, `combinations` returns all sublists of `lst` (the "power set"), ordered by
    ;; size first.
    (define combinations
      (case-lambda
        ((lst)
          (append-map (lambda (k) (combinations lst k)) (iota (+ (length lst) 1))))
        ((lst k)
          (cond ((zero? k)     '(()))
                ((null? lst)   '())
                (else
                  (append (map (lambda (c) (cons (car lst) c))
                               (combinations (cdr lst) (- k 1)))
                          (combinations (cdr lst) k)))))))

    ;; Returns the cartesian product of the given lists as a list of lists.
    (define (cartesian-product . lists)
      (if (null? lists)
          '(())
          (let ((rest (apply cartesian-product (cdr lists))))
            (append-map (lambda (x) (map (lambda (r) (cons x r)) rest))
                        (car lists)))))
  )
)
