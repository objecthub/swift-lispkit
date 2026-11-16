;;; LISPKIT PRETTY-PRINT REGRESSION TEST SUITE
;;;
;;; This is the test suite for library `(lispkit pretty-print)`.
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
        (lispkit test)
        (lispkit pretty-print)
        (prefix (srfi 272 advanced) srfi:)
        (srfi 48))

(define fold-code
  '(define (fold kons knil ls)
     (let lp ((ls ls) (acc knil))
       (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc))))))

(define (capture thunk)
  (let ((port (open-output-string)))
    (parameterize ((current-output-port port)) (thunk))
    (get-output-string port)))

(define (pp-to-string proc obj . args)
  (let ((port (open-output-string)))
    (apply proc obj port args)
    (get-output-string port)))

(define (temp-path name)
  (string-append (car (system-directory 'temporary)) name))

(define (write-file path content)
  (call-with-output-file path (lambda (port) (write-string content port))))

(define (read-file path)
  (call-with-input-file path (lambda (port) (read-string 100000 port))))

(test-begin "LispKit Pretty-Print")

(test-group "Printing procedures"
  (test "(define (fold kons knil ls)\n  (let lp ((ls ls) (acc knil))\n    (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc)))))\n"
        (capture (lambda () (pretty-print fold-code '((code . #t))))))
  ; data layout by default
  (test "(define\n (fold kons knil ls)\n (let\n  lp\n  ((ls ls) (acc knil))\n  (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc)))))\n"
        (capture (lambda () (pretty-print fold-code))))
  (test "(define\n (fold kons knil ls)\n (let\n  lp\n  ((ls ls) (acc knil))\n  (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc)))))\n"
        (pretty-print->string fold-code))
  (test "(define (fold kons knil ls)\n  (let lp ([ls ls] [acc knil])\n    (if (null? ls)\n        acc\n        (lp (cdr ls) (kons (car ls) acc)))))\n"
        (capture (lambda () (pretty-print fold-code '((code . #t) (width . 40) (brackets . #t))))))
  (test "(define\n (fold kons knil ls)\n (let\n  lp\n  ((ls ls) (acc knil))\n  (if\n   (null? ls)\n   acc\n   (lp (cdr ls) (kons (car ls) acc)))))\n"
        (pp-to-string pretty-print fold-code '((code . #f) (width . 50))))
  (test "(a b c)\n" (pp-to-string pretty-print '(a b c)))
  (test "(#xff #x10)\n" (pretty-print->string '(255 16) '((radix . 16))))
  (test "(a b c)\n" (pretty-print->string '(a b c)))
  (test "(a b c)" (pretty-print->string '(a b c) '((newline . #f))))
  (test "(a b ...)\n" (pretty-print->string '(a b c d) '((length . 2))))
  (test "(define (fold kons knil …) (let lp # …))\n"
        (pretty-print->string fold-code
          '((level . 2) (length . 3) (level-stub . "#") (length-stub . "…")))))

(test-group "Shared and circular structures"
  (let ((v (vector 1 2))
        (s (string #\a #\b)))
    (vector-set! v 0 v)
    (test "#0=#(#0# 2)\n" (pp-to-string pretty-print v))
    (test "(\"ab\" \"ab\")\n" (pp-to-string pretty-print (list s s)))
    (test "(#0=\"ab\" #0#)\n" (pp-to-string pretty-print-shared (list s s)))
    (test "(\"ab\" \"ab\")\n" (pp-to-string pretty-print-simple (list s s)))
    (test "(#0=\"ab\" #0#)\n" (capture (lambda () (pretty-print-shared (list s s)))))
    (test "(\"ab\" \"ab\")\n" (capture (lambda () (pretty-print-simple (list s s) '()))))
    ; graph and circle are determined by the printing procedure
    (test "(\"ab\" \"ab\")\n" (pp-to-string pretty-print (list s s) '((graph . #t))))))

(test-group "Configurations"
  (test 'error (guard (e (#t 'error)) (pretty-print->string 1 '((widht . 3)))))
  (test 'error (guard (e (#t 'error)) (pretty-print->string 1 '(width 3))))
  (test 'error (guard (e (#t 'error)) (pretty-print->string 1 '((width . 0)))))
  (test 'error (guard (e (#t 'error)) (pretty-print->string 1 'width)))
  (test 'error (guard (e (#t 'error)) (current-pretty-print-config '((foo . 1)))))
  (test 'error (guard (e (#t 'error)) (pretty-print 1 (current-output-port) '() '())))
  (test '() (current-pretty-print-config))
  (parameterize ((current-pretty-print-config '((width . 30) (radix . 2) (code . #t))))
    (test "(define (fold kons knil ls)\n  (let lp ((ls ls) (acc knil))\n    (if (null? ls)\n        acc\n        (lp (cdr ls)\n            (kons (car ls) acc)))))\n"
          (pretty-print->string fold-code '((radix . 10))))
    (test "(#b11)\n" (pretty-print->string '(3)))
    (test "(#x3)\n" (pretty-print->string '(3) '((radix . 16))))))

(test-group "Style registries"
  (test "(let-optionals args ((x 1) (y 2) . rest)\n  (display x)\n  (display y)\n  (newline))\n"
        (pretty-print->string
          '(let-optionals args ((x 1) (y 2) . rest) (display x) (display y) (newline))
          '((code . #t) (width . 40))))
  (test "(let-optionals args ((x 1) (y 2) . rest)\n  (display x) (display y) (newline))\n"
        (pretty-print->string
          '(let-optionals args ((x 1) (y 2) . rest) (display x) (display y) (newline))
          '((code . #t) (width . 40) (styles . scheme))))
  (test '(_ e fc* . body) (pretty-print-style 'let-optionals))
  (test #f (pretty-print-style-lookup (pretty-print-style-registry 'scheme) 'let-optionals))
  (test '(_ e fc* . body)
        (pretty-print-style-lookup (pretty-print-style-registry 'lispkit) 'let-optionals))
  (test #t (eq? (pretty-print-style-registry) (pretty-print-style-registry 'lispkit)))
  (test #t (eq? (pretty-print-style-registry 'scheme) (srfi:pp-styles)))
  (test 'error (guard (e (#t 'error)) (pretty-print-style-registry 'racket)))
  (test "(my-when (> x 100)\n  (display \"x is large\")\n  (newline) (set! x (- x 100)))\n"
        (pretty-print->string
          '(my-when (> x 100) (display "x is large") (newline) (set! x (- x 100)))
          '((code . #t) (width . 30))))
  (pretty-print-style 'my-when (pretty-print-style 'when))
  (test '(_ e . body) (pretty-print-style 'my-when))
  (test "(my-when (> x 100)\n  (display \"x is large\")\n  (newline)\n  (set! x (- x 100)))\n"
        (pretty-print->string
          '(my-when (> x 100) (display "x is large") (newline) (set! x (- x 100)))
          '((code . #t) (width . 30))))
  ; the SRFI 272 registry is not affected
  (test #f (srfi:pretty-style 'my-when))
  (test "(my-when (> x 100)\n  (display \"x is large\")\n  (newline) (set! x (- x 100)))\n"
        (pretty-print->string
          '(my-when (> x 100) (display "x is large") (newline) (set! x (- x 100)))
          '((code . #t) (width . 30) (styles . scheme))))
  (pretty-print-style 'my-when #f)
  (test #f (pretty-print-style 'my-when))
  (test "(my-loop lp ((i 0))\n  (display i)\n  (lp (+ i 1)))\n"
        (pretty-print->string
          '(my-loop lp ((i 0)) (display i) (lp (+ i 1)))
          `((code . #t) (width . 25)
            (styles . ,(pretty-print-style-add (pretty-print-style-registry)
                                               'my-loop '(_ i? fc* . body)))))))

(test-group "LispKit styles"
  (test "(define-enum color\n  (red green blue yellow magenta\n    cyan)\n  color-value)\n"
        (pretty-print->string
          '(define-enum color (red green blue yellow magenta cyan) color-value)
          '((code . #t) (width . 36))))
  (test "(receive (q r . rest) (floor/ 17 5)\n  (display q)\n  (display r))\n"
        (pretty-print->string
          '(receive (q r . rest) (floor/ 17 5) (display q) (display r))
          '((code . #t) (width . 36))))
  (test "(match lst\n  ((a b) (+ a b))\n  ((a . rest) (display a) (f rest))\n  (_ 0))\n"
        (pretty-print->string
          '(match lst ((a b) (+ a b)) ((a . rest) (display a) (f rest)) (_ 0))
          '((code . #t) (width . 36))))
  (test "(dolist (x '(1 2 3 4 5) result)\n  (display x)\n  (newline))\n"
        (pretty-print->string
          '(dolist (x '(1 2 3 4 5) result) (display x) (newline))
          '((code . #t) (width . 36))))
  (test "(for/fold ((sum 0) (count 0))\n  ((x (in-list xs)))\n  (values (+ sum x) (+ count 1)))\n"
        (pretty-print->string
          '(for/fold ((sum 0) (count 0)) ((x (in-list xs))) (values (+ sum x) (+ count 1)))
          '((code . #t) (width . 36))))
  (test "(case-λ\n  ((x) (f x 1))\n  ((x y) (display x) (+ x y)))\n"
        (pretty-print->string
          '(case-λ ((x) (f x 1)) ((x y) (display x) (+ x y)))
          '((code . #t) (width . 36))))
  (test "(define-type point point?\n  ((make-point x y) (cons x y))\n  point-x\n  point-y)\n"
        (pretty-print->string
          '(define-type point point? ((make-point x y) (cons x y)) point-x point-y)
          '((code . #t) (width . 36)))))

(test-group "Hooks"
  (let ()
    (define-record-type bag (make-bag items) bag? (items bag-items))
    (define bag-hook (pretty-print-hook-glst "#[" bag-items make-bag "]"))
    (test "#[alpha\n  beta\n  gamma\n  delta]\n"
          (pretty-print->string (make-bag '(alpha beta gamma delta))
            `((width . 15)
              (hooks . ,(pretty-print-hook-add (pretty-print-hook-registry) bag? bag-hook)))))
    (test #f (pretty-print-hook bag?))
    (pretty-print-hook bag? bag-hook)
    (test #t (eq? (pretty-print-hook bag?) bag-hook))
    (test #t (eq? (pretty-print-hook-lookup (pretty-print-hook-registry) bag?) bag-hook))
    (test "#[1 2 3]\n" (pretty-print->string (make-bag '(1 2 3))))
    (pretty-print-hook bag? #f)
    (test #f (pretty-print-hook bag?))
    (test "(f ~x ~(g y))\n"
          (pretty-print->string '(f (delay x) (delay (g y)))
            `((hooks . ,(pretty-print-hook-add (pretty-print-hook-registry)
                          (lambda (x) (and (pair? x) (eq? (car x) 'delay)
                                           (pair? (cdr x)) (null? (cddr x))))
                          (pretty-print-hook-rmac "~" cadr (lambda (x) (list 'delay x))))))))
    (test "#n(#xa #xff)\n"
          (pretty-print->string (make-bag '(10 255))
            `((radix . 16)
              (hooks . ,(pretty-print-hook-add (pretty-print-hook-registry) bag?
                          (pretty-print-hook-bvec "#n("
                            (lambda (b) (length (bag-items b)))
                            (lambda (b i) (list-ref (bag-items b) i)) ")"))))))
    (test "(#<bag 3>)\n"
          (pretty-print->string (list (make-bag '(1 2 3)))
            `((hooks . ,(pretty-print-hook-add (pretty-print-hook-registry) bag?
                          (pretty-print-hook-atom #f (lambda (b) 8)
                            (lambda (b radix)
                              (list "#<bag " (number->string (length (bag-items b)) radix)
                                    ">"))))))))))

(test-group "Hashtables and records"
  (let ()
    (define-record-type point (make-point x y) point? (x point-x) (y point-y set-point-y!))
    (define h (make-equal-hashtable))
    (hashtable-set! h 'origin (make-point 0 0))
    (test "#<hashtable (origin #<record point (x 0) (y 0)>)>\n" (pretty-print->string h))
    (test "#<record point\n         (x (a b c d e f))\n         (y 2)>\n"
          (pretty-print->string (make-point '(a b c d e f) 2) '((width . 26))))
    (test "#<record point\n         (x\n          (a b c d e f))\n         (y 2)>\n"
          (pretty-print->string (make-point '(a b c d e f) 2) '((width . 25))))
    (let ((p (make-point 1 #f)))
      (set-point-y! p p)
      (test "#0=#<record point (x 1) (y #0#)>\n" (pretty-print->string p))
      (test "(#0=#<record point (x 1) (y #0#)> #0#)\n"
            (let ((port (open-output-string)))
              (pretty-print-shared (list p p) port)
              (get-output-string port))))
    (hashtable-set! h 'self h)
    (test #t (and (member (pretty-print->string h)
                          '("#0=#<hashtable (origin #<record point (x 0) (y 0)>) (self #0#)>\n"
                            "#0=#<hashtable (self #0#) (origin #<record point (x 0) (y 0)>)>\n"))
                  #t))
    (test "#<hashtable>\n" (pretty-print->string (make-eq-hashtable)))
    ; user-defined hooks take precedence over the built-in hooks for records
    (test "#<pt 1 2>\n"
          (pretty-print->string (make-point 1 2)
            `((hooks . ,(pretty-print-hook-add (pretty-print-hook-registry) point?
                          (pretty-print-hook-atom #f (lambda (p) 8)
                            (lambda (p radix)
                              (list "#<pt " (number->string (point-x p) radix) " "
                                    (number->string (point-y p) radix) ">"))))))))))

(test-group "Source code"
  ; comments following the head of a definition are dropped
  (test "(define (f x)\n  (dolist (y x) (display y))\n  (newline))\n(f '(1 2)) ; call\n"
        (pretty-print-string
          "(define (f x) ; comment\n (dolist (y x) (display y)) (newline))\n(f '(1 2)) ; call\n"
          '((width . 30))))
  (test "(define (f x)\n  (dolist (y x) (display y))\n  (newline))\n\n(f '(1 2))\n\n"
        (pretty-print-string
          "(define (f x) ; comment\n (dolist (y x) (display y)) (newline))\n(f '(1 2)) ; call\n"
          '((width . 30) (decorate . #f))))
  (test ";; * pp-styles: with-lock := when\n(with-lock (lock-of account)\n  (withdraw! account 100)\n  (deposit! savings 100))\n"
        (pretty-print-string
          ";; * pp-styles: with-lock := when\n(with-lock (lock-of account) (withdraw! account 100) (deposit! savings 100))\n"
          '((width . 40))))
  ; style comments do not modify the registry
  (test #f (pretty-print-style 'with-lock))
  (test "(let-optionals args ((x 1) (y 2))\n  (list x y))\n"
        (pretty-print-string "(let-optionals args ((x 1) (y 2)) (list x y))" '((width . 40))))
  ; narrow widths fall into the miser zone (20 columns by default)
  (test "(let-optionals args\n ((x 1) (y 2))\n (list x y))\n"
        (pretty-print-string "(let-optionals args ((x 1) (y 2)) (list x y))" '((width . 20))))
  (test "" (pretty-print-string ""))
  ; magic comments take precedence over the default configuration, but not over the
  ; configuration passed to the procedure
  (let ((src ";; -*- fill-column: 20 -*-\n(define (f x) (display x) (newline) (+ x 1))\n")
        (narrow ";; -*- fill-column: 20 -*-\n(define (f x)\n (display x)\n (newline)\n (+ x 1))\n")
        (wide ";; -*- fill-column: 20 -*-\n(define (f x) (display x) (newline) (+ x 1))\n"))
    (test narrow (pretty-print-string src))
    (test narrow (parameterize ((current-pretty-print-config '((width . 100))))
                   (pretty-print-string src)))
    (test wide (pretty-print-string src '((width . 100)))))
  (let ((in (temp-path "lispkit-pretty-print-in.scm"))
        (out (temp-path "lispkit-pretty-print-out.scm")))
    (write-file in ";; Example\n(define (sq x) (* x x))\n(sq 3) ; nine\n")
    (test ";; Example\n(define (sq x) (* x x))\n(sq 3) ; nine\n"
          (capture (lambda () (pretty-print-file in))))
    (test ";; Example\n(define (sq x)\n (* x x))\n(sq 3) ; nine\n"
          (capture (lambda () (pretty-print-file in '((width . 15))))))
    (test ";; Example\n(define (sq x)\n (* x x))\n(sq 3) ; nine\n"
          (let ((port (open-output-string)))
            (pretty-print-file in port '((width . 15)))
            (get-output-string port)))
    (test ";; Example\n(define (sq x) (* x x))\n(sq 3) ; nine\n"
          (begin
            (pretty-print-file in out)
            (read-file out)))
    (test ";; Example\n(define (sq x)\n (* x x))\n(sq 3) ; nine\n"
          (begin
            (pretty-print-file in out '((width . 15)))
            (read-file out)))
    (delete-file in)
    (delete-file out)))

(test-group "Colors"
  (let ((sq '(define (sq x) (* x x))))
    (test "(define (sq x) (* x x))\n"
          (pretty-print->string sq '((code . #t) (color . #t) (color-tier . #f))))
    (test "(\x1b;[34mdefine\x1b;[0m (\x1b;[1msq\x1b;[0m \x1b;[37mx\x1b;[0m) (* x x))\n"
          (pretty-print->string sq '((code . #t) (color . #t) (color-tier . 0))))
    (test "(\x1b;[38;5;109mdefine\x1b;[0m (\x1b;[1msq\x1b;[0m \x1b;[38;5;254mx\x1b;[0m) (* x x))\n"
          (pretty-print->string sq '((code . #t) (color . #t) (color-tier . 1))))
    (test "(\x1b;[38;2;129;161;193mdefine\x1b;[0m (\x1b;[1msq\x1b;[0m \x1b;[38;2;229;233;240mx\x1b;[0m) (* x x))\n"
          (pretty-print->string sq '((code . #t) (color . #t) (color-tier . 2))))
    ; color-tier alone does not enable colors
    (test "(define (sq x) (* x x))\n" (pretty-print->string sq '((color-tier . 0))))
    (parameterize ((current-pretty-print-config '((color-tier . 0) (code . #t))))
      (test "(\x1b;[34mdefine\x1b;[0m (\x1b;[1msq\x1b;[0m \x1b;[37mx\x1b;[0m) (* x x))\n"
            (pretty-print->string sq '((color . #t))))
      (test "(define (sq x) (* x x))\n"
            (pretty-print->string sq '((code . #t) (color . #t) (color-tier . #f)))))
    (test "\x1b;[90m;; c\x1b;[0m\n(f \x1b;[35m1\x1b;[0m)\n"
          (pretty-print-string ";; c\n(f 1)\n" '((code . #t) (color . #t) (color-tier . 0))))
    (test "(define (sq x) (* x x))\n"
          (pretty-print->string sq `((code . #t) (color . #t) (color-tier . 0)
                                     (tint . ,(lambda (str port) #f)))))
    (test 'error (guard (e (#t 'error)) (pretty-print->string sq '((color-tier . 4)))))
    (test 'error (guard (e (#t 'error)) (current-pretty-print-config '((color-tier . yes)))))))

(test-group "SRFI 48"
  (test "x = (define (fold kons knil ls)\n  (let lp ((ls ls) (acc knil))\n    (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc)))))\n"
        (format #f "x = ~Y" fold-code)))

(test-end)
