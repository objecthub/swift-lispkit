;;; SRFI 196 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 196.
;;;
;;; Copyright © 2020 Wolfgang Corcoran-Mathe. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2020 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test)
        (srfi 1)
        (srfi 128)
        (only (srfi 158) generator->list)
        (srfi 196))

(define-syntax check
  (syntax-rules (=>)
    ((check expr)
      (check expr => #t))
    ((check expr => expected)
      (test expected expr))))

;;; Utility

(define (identity x) x)

(define real-comparator (make-comparator real? = < #f))

(define-syntax constantly
  (syntax-rules ()
    ((_ obj) (lambda _ obj))))

;; Returns the value of `expr', or 'exception if an exception was raised.
(define-syntax catch-exceptions
  (syntax-rules ()
   ((_ expr)
    (call-with-current-continuation
     (lambda (k)
       (with-exception-handler
        (lambda (_) (k 'exception))
        (lambda () expr)))))))

(define always (constantly #t))
(define never (constantly #f))

;;; Test ranges

(define test-num-range (numeric-range 10 30))

(define test-empty-range (numeric-range 0 0))

(define test-bool-range
  (range (make-comparator boolean?
                          boolean=?
                          (lambda (x y) (and (not x) y))
                          #f)
         #f
         2
         (lambda (b x) (if (zero? x) b (not b)))))

;;; Conversion

;;; Test these first, as range->list is used extensively in later tests.

(define (check-conversion)
  (check (range->list test-empty-range) => '())
  (check (range->list test-bool-range)  => '(#f #t))

  (check (equal? (generator->list (range->generator test-num-range))
                 (range->list test-num-range))
   => #t))

;;; Constructors

(define (check-constructors)
  ;; range rejects ill-typed lower bounds.
  (check (catch-exceptions
           (range real-comparator 'z 100 (lambda (b n) (+ b n))))
   => 'exception)

  (check (range? (catch-exceptions (numeric-range 1 -5 -1)))   => #t)
  (check (range? (catch-exceptions (numeric-range 1.3 5.3 1))) => #t)
  (check (catch-exceptions (numeric-range 1 5.4))              => 'exception)
  (check (catch-exceptions (numeric-range 1 -5 1))             => 'exception)
  (check (catch-exceptions (numeric-range 1 5 0.3))            => 'exception))

;;; Predicates

(define (check-predicates)
  (check (range-contains? test-num-range (range-start test-num-range)) => #t)
  (check (range-contains? test-bool-range (range-start test-bool-range)) => #t)
  (check (range-contains? test-num-range (+ (range-start test-num-range) 0.1))
   => #f)
  (check (range-contains? test-num-range (+ (range-end test-num-range) 1))
   => #f)

  (check (range-includes? test-num-range (range-start test-num-range)) => #t)
  (check (range-includes? test-bool-range (range-start test-bool-range)) => #t)
  (check (range-includes? test-num-range (+ (range-start test-num-range) 0.1))
   => #t)
  (check (range-includes? test-num-range (+ (range-end test-num-range) 1))
   => #f)

  (check (range-empty? test-empty-range) => #t)
  (check (range-empty? test-num-range)   => #f))

;;; Accessors

(define (check-accessors)
  (check (range-ref test-num-range 0)  => 10)
  (check (range-ref test-bool-range 1) => #t)

  (check (catch-exceptions (range-ref test-bool-range -1)) => 'exception)
  (check (catch-exceptions (range-ref test-bool-range
                                      (range-length test-bool-range)))
   => 'exception))

;;; Iteration

(define (check-iteration)
  ;; Check lengths of ranges returned by range-split-at.
  (let ((n 10))
    (check (let-values (((ra rb) (range-split-at test-num-range n)))
             (list (range-length ra) (range-length rb)))
     => (list n (- (range-length test-num-range) n))))

  ;; Joining the two ranges returned by range-split-at gives the
  ;; original range.
  (check (let-values (((ra rb) (range-split-at test-bool-range 1)))
           (append (range->list ra) (range->list rb)))
   => (range->list test-bool-range))

  ;; range-take r n returns a range of length n.
  (check (range-length (range-take test-num-range 10)) => 10)

  ;; range-take-right r n returns a range of length n.
  (check (range-length (range-take-right test-num-range 10)) => 10)

  ;; range-drop r n returns a range of length (range-length r) - n.
  (check (range-length (range-drop test-num-range 10))
   => (- (range-length test-num-range) 10))

  ;; range-drop-right r n returns a range of length (range-length r) - n.
  (check (range-length (range-drop-right test-num-range 10))
   => (- (range-length test-num-range) 10))

  (check (range-count always test-num-range) => (range-length test-num-range))
  (check (range-count never test-num-range)  => 0)

  (check (range-any even? test-num-range) => #t)
  (check (range-any never test-num-range) => #f)

  (check (range-every number? test-num-range) => #t)
  (check (range-every even? test-num-range)   => #f)

  ;; (range-map->list f r) = (map f (range->list r))
  (let ((f not))
    (check (equal? (range-map->list f test-bool-range)
                   (map f (range->list test-bool-range)))
     => #t))

  (check (let ((v #f))
           (range-for-each (lambda (x) (set! v x)) test-bool-range)
           v)
   => #t)

  (check (equal? (range-filter->list always test-bool-range)
                 (range->list test-bool-range))
   => #t)

  (check (null? (range-filter->list never test-bool-range)) => #t)

  ;; (range-filter->list pred r) = (filter pred (range->list r))
  (let ((pred even?))
    (check (equal? (range-filter->list pred test-num-range)
                   (filter pred (range->list test-num-range)))
     => #t))

  (check (equal? (range-remove->list never test-bool-range)
                 (range->list test-bool-range))
   => #t)

  (check (null? (range-remove->list always test-bool-range)) => #t)

  ;; (range-remove->list pred r) = (remove pred (range->list r))
  (let ((pred even?))
    (check (equal? (range-remove->list pred test-num-range)
                   (remove pred (range->list test-num-range)))
     => #t))

  ;; (range-fold (lambda (b) (+ 1 b)) 0 r) = (range-length r)
  (check (= (range-fold (lambda (_ b) (+ b 1)) 0 test-num-range)
            (range-length test-num-range))
   => #t)

  ;; (range-fold proc nil r) = (fold proc nil (range->list r))
  (let ((proc +) (nil 0))  ; sum over range
    (check (equal? (range-fold proc nil test-num-range)
                   (fold proc nil (range->list test-num-range)))
     => #t))

  ;; (range-fold-right (lambda (b) (+ 1 b)) 0 r) = (range-length r)
  (check (= (range-fold-right (lambda (_ b) (+ b 1)) 0 test-num-range)
            (range-length test-num-range))
   => #t)

  ;; (range-fold-right r proc nil) = (fold-right proc nil (range->list r))
  (let ((proc +) (nil 0))  ; sum over range
    (check (equal? (range-fold-right proc nil test-num-range)
                   (fold-right proc nil (range->list test-num-range)))
     => #t))

  (check (eqv? (range-start (range-reverse test-bool-range))
               (range-end test-bool-range))
   => #t)

  (check (eqv? (range-end (range-reverse test-bool-range))
               (range-start test-bool-range))
   => #t)

  (check (equal? (range->list (range-reverse test-num-range))
                 (reverse (range->list test-num-range)))
   => #t))

;;; Searching

(define (check-searching)
  (check (range-index always test-num-range) => 0)
  (check (range-index never test-num-range)  => #f)

  (check (eqv? (range-index-right always test-num-range)
               (- (range-length test-num-range) 1))
   => #t)
  (check (range-index-right never test-num-range)  => #f)

  ;; (range-take-while always r) = r
  (check (equal? (range->list (range-take-while always test-bool-range))
                 (range->list test-bool-range))
   => #t)

  ;; (range-take-while never r) = [empty range]
  (check (range-empty? (range-take-while never test-bool-range)) => #t)

  ;; (range-drop-while always r) = [empty range]
  (check (range-empty? (range-drop-while always test-bool-range)) => #t)

  ;; (range-drop-while never r) = r
  (check (equal? (range->list (range-drop-while never test-bool-range))
                 (range->list test-bool-range))
   => #t)

  ;; Given a (non-existant) range-append function,
  ;;
  ;; (range-append (range-take-while p r) (range-drop-while p r)) = r
  (let ((pred (lambda (n) (< n 10))))
    (check (equal?
            (append (range->list (range-take-while pred test-num-range))
                    (range->list (range-drop-while pred test-num-range)))
            (range->list test-num-range))
     => #t))

  ;; (range-take-while-right always r) = r
  (check (equal? (range->list (range-take-while-right always test-bool-range))
                 (range->list test-bool-range))
   => #t)

  ;; (range-take-while-right never r) = [empty range]
  (check (range-empty? (range-take-while-right never test-bool-range)) => #t)

  ;; (range-drop-while-right always r) = [empty range]
  (check (range-empty? (range-drop-while-right always test-bool-range)) => #t)

  ;; (range-drop-while-right never r) = r
  (check (equal? (range->list (range-drop-while-right never test-bool-range))
                 (range->list test-bool-range))
   => #t)

  ;; Given a (non-existant) range-append function,
  ;;
  ;; (range-append (range-drop-while-right p r)
  ;;               (range-take-while-right p r)) = r
  (let ((pred (lambda (n) (< n 10))))
    (check (equal?
            (append (range->list (range-drop-while-right pred test-num-range))
                    (range->list (range-take-while-right pred test-num-range)))
            (range->list test-num-range))
     => #t)))

(test-begin "SRFI 196: Range objects")

(check-conversion)
(check-constructors)
(check-predicates)
(check-accessors)
(check-iteration)
(check-searching)

(test-end)

