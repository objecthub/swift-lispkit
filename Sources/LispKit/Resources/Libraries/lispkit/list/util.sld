;;; LISPKIT LIST UTIL
;;;
;;; Library providing list utilities that are not covered by R7RS or the SRFIs supported by
;;; LispKit, such as `flatten`, `group-by` and the combinatorial procedures `permutations`,
;;; `combinations` and `cartesian-product`. The library also provides a selection of the most
;;; commonly used list procedures of SRFI 1 (`take`, `drop`, `take-right`, `drop-right`,
;;; `take-while`, `drop-while`, `split-at`, `first`, `second`, `third`, `fourth`, `fifth`,
;;; `last`, `find`, `count`, `fold`, `reduce`, `any`, `every`, `delete-duplicates`,
;;; `list-index`, and `zip`). They follow the semantics of SRFI 1, but are implemented
;;; independently of it, so that this library only depends on `(lispkit base)`.
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
          take-right
          drop-right
          take-while
          drop-while
          split-at
          (rename car first)
          (rename cadr second)
          (rename caddr third)
          (rename cadddr fourth)
          fifth
          last
          find
          count
          fold
          reduce
          any
          every
          delete-duplicates
          list-index
          zip
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

    ;; Returns the last `k` elements of `lst`. The result shares structure with `lst`, and `lst`
    ;; may be an improper list, in which case the final cdr is part of the result.
    (define (take-right lst k)
      (let loop ((lag lst) (lead (drop lst k)))
        (if (pair? lead)
            (loop (cdr lag) (cdr lead))
            lag)))

    ;; Returns a new list with all elements of `lst` except for the last `k` elements. If `lst`
    ;; is an improper list, its final cdr is ignored.
    (define (drop-right lst k)
      (let loop ((lag lst) (lead (drop lst k)) (acc '()))
        (if (pair? lead)
            (loop (cdr lag) (cdr lead) (cons (car lag) acc))
            (reverse acc))))

    ;; Returns the longest prefix of `lst` whose elements all satisfy `pred`.
    (define (take-while pred lst)
      (let loop ((l lst) (acc '()))
        (cond ((null? l)       (reverse acc))
              ((pred (car l))  (loop (cdr l) (cons (car l) acc)))
              (else            (reverse acc)))))

    ;; Returns the list that remains after dropping the longest prefix of `lst` whose elements
    ;; all satisfy `pred`. The result shares structure with `lst`.
    (define (drop-while pred lst)
      (let loop ((l lst))
        (cond ((null? l)       l)
              ((pred (car l))  (loop (cdr l)))
              (else            l))))

    ;; Returns two values: the first `k` elements of `lst` and the list of remaining elements.
    (define (split-at lst k)
      (let loop ((l lst) (i k) (acc '()))
        (if (zero? i)
            (values (reverse acc) l)
            (loop (cdr l) (- i 1) (cons (car l) acc)))))

    ;; Returns `#t` if at least one of the lists in `lists` is empty. Used for iterating over
    ;; several lists until the shortest list is exhausted.
    (define (some-null? lists)
      (and (pair? lists)
           (or (null? (car lists))
               (some-null? (cdr lists)))))

    ;; Returns `#t` if `x` is equivalent (with respect to `same?`) to an element of `seen`,
    ;; a list of elements that precede `x`. These elements are passed as first argument.
    (define (seen? same? x seen)
      (and (pair? seen)
           (or (same? (car seen) x)
               (seen? same? x (cdr seen)))))

    ;; Returns the fifth element of list `lst`. Procedures `first`, `second`, `third` and
    ;; `fourth` do not need a definition: they are exported as renamed versions of `car`,
    ;; `cadr`, `caddr` and `cadddr`.
    (define (fifth lst) (car (cddddr lst)))

    ;; Returns the last element of the non-empty list `lst`.
    (define (last lst)
      (unless (pair? lst)
        (error "last: expected non-empty list" lst))
      (let loop ((l lst))
        (if (pair? (cdr l))
            (loop (cdr l))
            (car l))))

    ;; Returns the first element of `lst` that satisfies `pred`, or `#f` if there is none.
    (define (find pred lst)
      (let loop ((l lst))
        (cond ((null? l)       #f)
              ((pred (car l))  (car l))
              (else            (loop (cdr l))))))

    ;; Returns the number of elements of `lst` (or of the elements at the same position of
    ;; `lst` and `lsts`, up to the length of the shortest list) that satisfy `pred`.
    (define (count pred lst . lsts)
      (if (null? lsts)
          (let loop ((l lst) (n 0))
            (cond ((null? l)       n)
                  ((pred (car l))  (loop (cdr l) (+ n 1)))
                  (else            (loop (cdr l) n))))
          (let loop ((ls (cons lst lsts)) (n 0))
            (cond ((some-null? ls)            n)
                  ((apply pred (map car ls))  (loop (map cdr ls) (+ n 1)))
                  (else                       (loop (map cdr ls) n))))))

    ;; Left fold over `lst` (or over several lists, up to the length of the shortest list):
    ;; combines the elements with `kons`, which is called as `(kons elem acc)`, starting with
    ;; `knil` as the initial accumulator value.
    (define (fold kons knil lst . lsts)
      (if (null? lsts)
          (let loop ((l lst) (acc knil))
            (if (null? l)
                acc
                (loop (cdr l) (kons (car l) acc))))
          (let loop ((ls (cons lst lsts)) (acc knil))
            (if (some-null? ls)
                acc
                (loop (map cdr ls) (apply kons (append (map car ls) (list acc))))))))

    ;; Like `fold` with a single list, but uses the first element of `lst` as initial value.
    ;; Returns `ridentity` if `lst` is empty.
    (define (reduce f ridentity lst)
      (if (null? lst)
          ridentity
          (fold f (car lst) (cdr lst))))

    ;; Returns the first true value returned by `pred` when applied to the elements of `lst`
    ;; (or the elements at the same position of several lists), and `#f` if there is none. The
    ;; last application of `pred` is a tail call.
    (define (any pred lst . lsts)
      (if (null? lsts)
          (let loop ((l lst))
            (cond ((null? l)        #f)
                  ((null? (cdr l))  (pred (car l)))
                  (else             (or (pred (car l)) (loop (cdr l))))))
          (let loop ((ls (cons lst lsts)))
            (if (some-null? ls)
                #f
                (let ((cdrs (map cdr ls)))
                  (if (some-null? cdrs)
                      (apply pred (map car ls))
                      (or (apply pred (map car ls)) (loop cdrs))))))))

    ;; Returns `#f` if `pred` returns `#f` for an element of `lst` (or for the elements at
    ;; the same position of several lists). Otherwise, returns the value of the last
    ;; application of `pred`, or `#t` if there are no elements. The last application of `pred`
    ;; is a tail call.
    (define (every pred lst . lsts)
      (if (null? lsts)
          (let loop ((l lst))
            (cond ((null? l)        #t)
                  ((null? (cdr l))  (pred (car l)))
                  (else             (and (pred (car l)) (loop (cdr l))))))
          (let loop ((ls (cons lst lsts)))
            (if (some-null? ls)
                #t
                (let ((cdrs (map cdr ls)))
                  (if (some-null? cdrs)
                      (apply pred (map car ls))
                      (and (apply pred (map car ls)) (loop cdrs))))))))

    ;; Returns the index of the first element of `lst` (or of the first elements at the same
    ;; position of several lists) that satisfies `pred`, or `#f` if there is none.
    (define (list-index pred lst . lsts)
      (if (null? lsts)
          (let loop ((l lst) (i 0))
            (cond ((null? l)       #f)
                  ((pred (car l))  i)
                  (else            (loop (cdr l) (+ i 1)))))
          (let loop ((ls (cons lst lsts)) (i 0))
            (cond ((some-null? ls)            #f)
                  ((apply pred (map car ls))  i)
                  (else                       (loop (map cdr ls) (+ i 1)))))))

    ;; Returns a list that is like `lst`, but without duplicates: if several elements are
    ;; equivalent according to `same?` (default: `equal?`), only the first of them is kept.
    (define delete-duplicates
      (case-lambda
        ((lst)       (delete-duplicates lst equal?))
        ((lst same?)
          (let loop ((rest lst) (kept '()))
            (cond ((null? rest)
                    (reverse kept))
                  ((seen? same? (car rest) kept)
                    (loop (cdr rest) kept))
                  (else
                    (loop (cdr rest) (cons (car rest) kept))))))))

    ;; Returns a list of lists, where the i-th list contains the i-th elements of `lst` and
    ;; `lsts`. The result is as long as the shortest list.
    (define (zip lst . lsts)
      (let loop ((ls (cons lst lsts)) (acc '()))
        (if (some-null? ls)
            (reverse acc)
            (loop (map cdr ls) (cons (map car ls) acc)))))

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
