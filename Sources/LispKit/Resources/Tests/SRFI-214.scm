;;; SRFI 214 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 214.
;;;
;;; Copyright © 2020-2021 Adam Nelson. All rights reserved.
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
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test)
        (srfi 214))

(test-begin "SRFI 214: Flexvectors")

(test-equal "flexvector?" #t (flexvector? (flexvector)))
(test-equal "flexvector-length" 3 (flexvector-length (make-flexvector 3 #f)))
(test-equal "flexvector" 3 (flexvector-length (flexvector 1 2 3)))

(let ((fv (flexvector 'a 'b 'c)))
  (test-equal "flexvector-ref" 'b (flexvector-ref fv 1))
  (test-equal "flexvector-front" 'a (flexvector-front fv))
  (test-equal "flexvector-back" 'c (flexvector-back fv))
  (test-equal "flexvector-set! return" 'b (flexvector-set! fv 1 'd))
  (test-equal "flexvector-set! mutate" 'd (flexvector-ref fv 1))
  (test-equal "flexvector-add-back! return" fv (flexvector-add-back! fv 'e))
  (test-equal "flexvector-add-back! mutate" '(4 . e)
              (cons (flexvector-length fv)
                    (flexvector-ref fv (- (flexvector-length fv) 1))))
  (test-equal "flexvector-remove! return" 'd (flexvector-remove! fv 1))
  (test-equal "flexvector-remove! mutate" '(3 . c)
              (cons (flexvector-length fv)
                    (flexvector-ref fv 1)))
  (test-equal "flexvector-clear! return" fv (flexvector-clear! fv))
  (test-equal "flexvector-clear! mutate" 0 (flexvector-length fv))
  (test-equal "flexvector-empty?" #t (flexvector-empty? fv)))

(test-equal "flexvector=? same symbols" #t
            (flexvector=? eq? (flexvector 'a 'b) (flexvector 'a 'b)))
(test-equal "flexvector=? different symbols" #f
            (flexvector=? eq? (flexvector 'a 'b) (flexvector 'b 'a)))
(test-equal "flexvector=? different lengths" #f
            (flexvector=? = (flexvector 1 2 3 4 5) (flexvector 1 2 3 4)))
(test-equal "flexvector=? same numbers" #t
            (flexvector=? = (flexvector 1 2 3 4) (flexvector 1 2 3 4)))
(test-equal "flexvector=? 0 arguments" #t
            (flexvector=? eq?))
(test-equal "flexvector=? 1 argument" #t
            (flexvector=? eq? (flexvector 'a)))

(test-equal "make-flexvector" #(a a a) (flexvector->vector (make-flexvector 3 'a)))

(test-equal "flexvector-unfold"
            #(1 4 9 16 25 36 49 64 81 100)
            (flexvector->vector
              (flexvector-unfold (lambda (x) (> x 10))
                                 (lambda (x) (* x x))
                                 (lambda (x) (+ x 1))
                                 1)))
(test-equal "flexvector-unfold-right"
            #(100 81 64 49 36 25 16 9 4 1)
            (flexvector->vector
              (flexvector-unfold-right (lambda (x) (> x 10))
                                       (lambda (x) (* x x))
                                       (lambda (x) (+ x 1))
                                       1)))

(test-equal "string->flexvector" #(#\a #\b #\c)
            (flexvector->vector (string->flexvector "abc")))
(test-equal "flexvector->string" "abc" (flexvector->string (flexvector #\a #\b #\c)))

(define genlist '(a b c))
(define (mock-generator)
  (if (pair? genlist)
      (let ((value (car genlist)))
        (set! genlist (cdr genlist))
        value)
      (eof-object)))

(test-equal "generator->flexvector" #(a b c)
            (flexvector->vector (generator->flexvector mock-generator)))
(test-equal "flexvector->generator" '(a b c #t)
            (let* ((gen (flexvector->generator (flexvector 'a 'b 'c)))
                   (one (gen))
                   (two (gen))
                   (three (gen))
                   (four (eof-object? (gen))))
              (list one two three four)))

; Nondestructive operations on one vector
(let ((fv (flexvector 10 20 30)))
  (test-equal "flexvector->vector" #(10 20 30) (flexvector->vector fv))
  (test-equal "flexvector->list" '(10 20 30) (flexvector->list fv))
  (test-equal "reverse-flexvector->list" '(30 20 10) (reverse-flexvector->list fv))
  (test-equal "flexvector-copy" #t
              (let ((copy (flexvector-copy fv)))
                (and (= (flexvector-length fv) (flexvector-length copy))
                     (not (eq? fv copy)))))
  (test-equal "flexvector-reverse-copy" #(30 20 10)
              (flexvector->vector (flexvector-reverse-copy fv)))
  (test-equal "flexvector-copy start" #(20 30)
              (flexvector->vector (flexvector-copy fv 1)))
  (test-equal "flexvector-copy start end" #(20)
              (flexvector->vector (flexvector-copy fv 1 2)))
  (test-equal "flexvector-for-each" '(30 20 10)
              (let ((res '()))
                (flexvector-for-each (lambda (x) (set! res (cons x res))) fv)
                res))
  (test-equal "flexvector-for-each/index" '(34 22 10)
              (let ((res '()))
                (flexvector-for-each/index
                  (lambda (i x) (set! res (cons (+ x (* i 2)) res)))
                  fv)
                res))
  (test-equal "flexvector-map" #(100 200 300)
              (flexvector->vector (flexvector-map (lambda (x) (* x 10)) fv)))
  (test-equal "flexvector-map/index" #(10 22 34)
              (flexvector->vector (flexvector-map/index (lambda (i x) (+ x (* i 2))) fv)))
  (test-equal "flexvector-append-map" #(10 100 20 200 30 300)
              (flexvector->vector
                (flexvector-append-map (lambda (x) (flexvector x (* x 10))) fv)))
  (test-equal "flexvector-append-map/index" #(0 10 10 1 20 22 2 30 34)
              (flexvector->vector
                (flexvector-append-map/index
                  (lambda (i x) (flexvector i x (+ x (* i 2))))
                  fv)))
  (test-equal "flexvector-filter" #(10)
              (flexvector->vector (flexvector-filter (lambda (x) (< x 15)) fv)))
  (test-equal "flexvector-filter/index" #(10 30)
              (flexvector->vector (flexvector-filter/index (lambda (i x) (not (= i 1))) fv)))
  (test-equal "flexvector-fold" '(30 20 10)
              (flexvector-fold (lambda (x y) (cons y x)) '() fv))
  (test-equal "flexvector-fold-right" '(10 20 30)
              (flexvector-fold-right (lambda (x y) (cons y x)) '() fv))
  (test-equal "flexvector-count" 2
              (flexvector-count (lambda (x) (< x 25)) fv))
  (test-equal "flexvector-cumulate" #(3 4 8 9 14 23 25 30 36)
              (flexvector->vector
                (flexvector-cumulate + 0 (flexvector 3 1 4 1 5 9 2 5 6))))
  (test-equal "flexvector-any" '(#t . #f)
              (cons (flexvector-any (lambda (x) (= x 20)) fv)
                    (flexvector-any (lambda (x) (= x 21)) fv)))
  (test-equal "flexvector-every" '(#t . #f)
              (cons (flexvector-every (lambda (x) (< x 40)) fv)
                    (flexvector-every (lambda (x) (< x 30)) fv)))
  (test-equal "flexvector-index" 1
              (flexvector-index (lambda (x) (> x 10)) fv))
  (test-equal "flexvector-index-right" 2
              (flexvector-index-right (lambda (x) (> x 10)) fv))
  (test-equal "flexvector-skip" 1
              (flexvector-skip (lambda (x) (< x 20)) fv))
  (test-equal "flexvector-skip-right" 0
              (flexvector-skip-right (lambda (x) (> x 10)) fv))
  (test-equal "flexvector-partition" '(#(10 20) #(30))
              (call-with-values
                (lambda () (flexvector-partition (lambda (x) (< x 25)) fv))
                (lambda vs (map flexvector->vector vs)))))

(let ((fv (flexvector #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j))
      (cmp (lambda (char1 char2)
             (cond ((char<? char1 char2) -1)
                   ((char=? char1 char2) 0)
                   (else 1)))))
  (test-equal "flexvector-binary-search" 3
              (flexvector-binary-search fv #\d cmp))
  (test-equal "flexvector-binary-search first" 0
              (flexvector-binary-search fv #\a cmp))
  (test-equal "flexvector-binary-search last" 9
              (flexvector-binary-search fv #\j cmp))
  (test-equal "flexvector-binary-search not found" #f
              (flexvector-binary-search fv #\k cmp))
  
  (test-equal "flexvector-binary-search in range" 5
              (flexvector-binary-search fv #\f cmp 2 6))
  (test-equal "flexvector-binary-search out of range" #f
              (flexvector-binary-search fv #\f cmp 1 5)))

; Nondestructive operations on multiple vectors
(test-equal "flexvector-append" #(10 20 30 40 50 60)
            (flexvector->vector
              (flexvector-append (flexvector 10 20)
                                 (flexvector)
                                 (flexvector 30 40)
                                 (flexvector 50 60))))
(test-equal "flexvector-concatenate" #(10 20 30 40 50 60)
            (flexvector->vector
              (flexvector-concatenate
                (list (flexvector 10 20)
                      (flexvector)
                      (flexvector 30 40)
                      (flexvector 50 60)))))
(test-equal "flexvector-append-subvectors" #(a b h i)
            (flexvector->vector
              (flexvector-append-subvectors
                (flexvector 'a 'b 'c 'd 'e) 0 2
                (flexvector 'f 'g 'h 'i 'j) 2 4)))

; Destructive operations on one vector
(define-syntax mutate-as
  (syntax-rules ()
    ((_ name vec expr)
      (let ((name (vector->flexvector vec)))
        expr
        (flexvector->vector name)))))

(test-equal "flexvector-add! empty" '#(foo)
            (mutate-as x '#() (flexvector-add! x 0 'foo)))
(test-equal "flexvector-add! empty multiple" '#(foo bar baz)
            (mutate-as x '#() (flexvector-add! x 0 'foo 'bar 'baz)))
(test-equal "flexvector-add! start" '#(foo bar baz)
            (mutate-as x '#(bar baz) (flexvector-add! x 0 'foo)))
(test-equal "flexvector-add! start multiple" '#(foo bar baz qux quux)
            (mutate-as x '#(qux quux) (flexvector-add! x 0 'foo 'bar 'baz)))
(test-equal "flexvector-add! middle" '#(foo bar baz)
            (mutate-as x '#(foo baz) (flexvector-add! x 1 'bar)))
(test-equal "flexvector-add! middle multiple" '#(foo bar baz qux quux)
            (mutate-as x '#(foo quux) (flexvector-add! x 1 'bar 'baz 'qux)))
(test-equal "flexvector-add! end" '#(foo bar baz)
            (mutate-as x '#(foo bar) (flexvector-add! x 2 'baz)))
(test-equal "flexvector-add! end multiple" '#(foo bar baz qux quux)
            (mutate-as x '#(foo bar) (flexvector-add! x 2 'baz 'qux 'quux)))

(test-equal "flexvector-add-all!" '#(foo bar baz qux)
            (mutate-as x '#(foo qux) (flexvector-add-all! x 1 '(bar baz))))

(test-equal "flexvector-add-front! empty" '#(foo)
            (mutate-as x '#() (flexvector-add-front! x 'foo)))
(test-equal "flexvector-add-front! empty multiple" '#(foo bar baz)
            (mutate-as x '#() (flexvector-add-front! x 'foo 'bar 'baz)))
(test-equal "flexvector-add-front!" '#(foo bar baz)
            (mutate-as x '#(bar baz) (flexvector-add-front! x 'foo)))
(test-equal "flexvector-add-front! multiple" '#(foo bar baz qux quux)
            (mutate-as x '#(qux quux) (flexvector-add-front! x 'foo 'bar 'baz)))

(test-equal "flexvector-add-back! empty" '#(foo)
            (mutate-as x '#() (flexvector-add-back! x 'foo)))
(test-equal "flexvector-add-back! empty multiple" '#(foo bar baz)
            (mutate-as x '#() (flexvector-add-back! x 'foo 'bar 'baz)))
(test-equal "flexvector-add-back!" '#(foo bar baz)
            (mutate-as x '#(foo bar) (flexvector-add-back! x 'baz)))
(test-equal "flexvector-add-back! multiple" '#(foo bar baz qux quux)
            (mutate-as x '#(foo bar) (flexvector-add-back! x 'baz 'qux 'quux)))

(test-equal "flexvector-append!" '#(foo bar baz qux)
            (mutate-as x '#(foo bar) (flexvector-append! x (flexvector 'baz 'qux))))
(test-equal "flexvector-append! multiple" '#(foo bar baz qux quux)
            (mutate-as x '#(foo bar) (flexvector-append! x (flexvector 'baz 'qux) (flexvector 'quux))))

(test-equal "flexvector-remove!" '#(foo baz)
            (mutate-as x '#(foo bar baz) (flexvector-remove! x 1)))
(test-equal "flexvector-remove! only" '#()
            (mutate-as x '#(foo) (flexvector-remove! x 0)))

(test-equal "flexvector-remove-front!" '#(bar baz)
            (mutate-as x '#(foo bar baz) (flexvector-remove-front! x)))
(test-equal "flexvector-remove-front! only" '#()
            (mutate-as x '#(foo) (flexvector-remove-front! x)))

(test-equal "flexvector-remove-back!" '#(foo bar)
            (mutate-as x '#(foo bar baz) (flexvector-remove-back! x)))
(test-equal "flexvector-remove-back! only" '#()
            (mutate-as x '#(foo) (flexvector-remove-back! x)))

(test-equal "flexvector-remove-range!" '#(a e f)
            (mutate-as x '#(a b c d e f) (flexvector-remove-range! x 1 4)))
(test-equal "flexvector-remove-range! empty range" '#(a b c d e f)
            (mutate-as x '#(a b c d e f) (flexvector-remove-range! x 1 1)))
(test-equal "flexvector-remove-range! overflow left" '#(e f)
            (mutate-as x '#(a b c d e f) (flexvector-remove-range! x -1 4)))
(test-equal "flexvector-remove-range! overflow right" '#(a b)
            (mutate-as x '#(a b c d e f) (flexvector-remove-range! x 2 10)))

(test-equal "flexvector-map!" '#(100 200 300)
            (mutate-as fv '#(10 20 30) (flexvector-map! (lambda (x) (* x 10)) fv)))
(test-equal "flexvector-map/index!" '#(10 22 34)
            (mutate-as fv '#(10 20 30) (flexvector-map/index! (lambda (i x) (+ x (* i 2))) fv)))
(test-equal "flexvector-filter!" '#(10)
            (mutate-as fv '#(10 20 30) (flexvector-filter! (lambda (x) (< x 15)) fv)))
(test-equal "flexvector-filter/index!" '#(10 30)
            (mutate-as fv '#(10 20 30) (flexvector-filter/index! (lambda (i x) (not (= i 1))) fv)))

(test-equal "flexvector-swap!" #(10 30 20)
            (mutate-as fv '#(10 20 30) (flexvector-swap! fv 1 2)))
(test-equal "flexvector-reverse!" #(30 20 10)
            (mutate-as fv '#(10 20 30) (flexvector-reverse! fv)))

(test-equal "flexvector-copy!" #(1 20 30 40 5)
            (mutate-as fv '#(1 2 3 4 5) (flexvector-copy! fv 1 (flexvector 20 30 40))))
(test-equal "flexvector-copy! bounded" #(1 20 30 40 5)
            (mutate-as fv '#(1 2 3 4 5) (flexvector-copy! fv 1 (flexvector 10 20 30 40 50) 1 4)))
(test-equal "flexvector-copy! overflow" #(1 2 30 40 50)
            (mutate-as fv '#(1 2 3) (flexvector-copy! fv 2 (flexvector 30 40 50))))
(test-equal "flexvector-reverse-copy!" #(1 40 30 20 5)
            (mutate-as fv '#(1 2 3 4 5) (flexvector-reverse-copy! fv 1 (flexvector 20 30 40))))
(test-equal "flexvector-reverse-copy! bounded" #(1 40 30 20 5)
            (mutate-as fv '#(1 2 3 4 5) (flexvector-reverse-copy! fv 1 (flexvector 10 20 30 40 50) 1 4)))
(test-equal "flexvector-reverse-copy! overflow" #(1 2 50 40 30)
            (mutate-as fv '#(1 2 3) (flexvector-reverse-copy! fv 2 (flexvector 30 40 50))))

(test-equal "flexvector-fill!" '#(foo foo foo)
            (mutate-as x '#(1 2 3) (flexvector-fill! x 'foo)))
(test-equal "flexvector-fill! start" '#(1 2 bar bar bar)
            (mutate-as x '#(1 2 3 4 5) (flexvector-fill! x 'bar 2)))
(test-equal "flexvector-fill! start end" '#(1 2 baz baz 5)
            (mutate-as x '#(1 2 3 4 5) (flexvector-fill! x 'baz 2 4)))
(test-equal "flexvector-fill! clamped" '#(qux qux qux)
            (mutate-as x '#(1 2 3) (flexvector-fill! x 'qux -1 10)))

(test-end "SRFI 214: Flexvectors")
