;;; Schelog Examples
;;; 
;;; The code below showcases how Prologe-like programming is possible with library
;;; `(lispkit prolog)`. This library provides an implementation of Schelog by 
;;; Dorai Sitaram.
;;; 
;;; The following document provides an introduction and overview of Shelog:
;;; http://ds26gte.github.io/schelog
;;; Example code can be found here:
;;; https://github.com/ds26gte/schelog/tree/master/examples
;;; 
;;; Below are two examples explaining how `(lispkit prolog)` is being used:
;;;   1. Family relationships are being modeled as Schelog predicates (to explain the
;;;      syntax and semantics of abstractions provided by `(lispkit prolog)`)
;;;   2. List functions are being modeled in a relational fashion via Schelog
;;;      predicates
;;; 
;;; Copyright © 1993-2015, Dorai Sitaram.
;;; All rights reserved.
;;; 
;;; Permission to distribute and use this work for any
;;; purpose is hereby granted provided this copyright
;;; notice is included in the copy.  This work is provided
;;; as is, with no warranty of any kind.
;;; 
;;; Adaptation to LispKit
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit prolog))

;; The following is a simple database about relationships within a known family in England,
;; together with a number of predicates.
;; 
;; Here is a dialog showcasing how to query the database:
;; 
;;   > (%which () (%brother-of 'anne 'charles))
;;   #f
;;   > (%which () (%brother-of 'charles 'anne))
;;   () ; indicates success
;;   > (%which (child) (%mother-of 'diana child))
;;   ((child william))
;;   > (%more)
;;   ((child harry))
;;   > (%more)
;;   #f
;;   > (%which (x y) (%married-to x y))
;;   ((x philip) (y elizabeth))
;;   > (%more)
;;   ((x charles) (y diana))
;;   > (%more)
;;   ((x mark) (y anne))
;;   > (%more)
;;   ((x andrew) (y sarah))
;;   ... 

(define %male
  (%rel ()
    (('philip))
    (('charles))
    (('andrew))
    (('edward))
    (('mark))
    (('william))
    (('harry))
    (('peter))))

(define %female
  (%rel ()
    (('elizabeth))
    (('anne))
    (('diana))
    (('sarah))
    (('zara))))

(define %husband-of
  (%rel ()
    (('philip 'elizabeth))
    (('charles 'diana))
    (('mark 'anne))
    (('andrew 'sarah))))

(define %wife-of
  (%rel (w h)
    ((w h) (%husband-of h w))))

(define %married-to
  (%rel (x y)
    ((x y) (%husband-of x y))
    ((x y) (%wife-of x y))))

(define %father-of
  (%rel ()
   (('philip 'charles))
   (('philip 'anne))
   (('philip 'andrew))
   (('philip 'edward))
   (('charles 'william))
   (('charles 'harry))
   (('mark 'peter))
   (('mark 'zara))))

(define %mother-of
  (%rel (m c f)
    ((m c) (%wife-of m f) (%father-of f c))))

(define %child-of
  (%rel (c p)
    ((c p) (%father-of p c))
    ((c p) (%mother-of p c))))

(define %parent-of
  (%rel (p c)
    ((p c) (%child-of c p))))

(define %brother-of
  (%rel (b x f)
    ((b x) (%male b) (%father-of f b) (%father-of f x) (%/= b x))))

;; The following predicates implement list functions in a relational fashion.
;; Here is a dialog showcasing how to query the database:
;; 
;;   > (%which (l) (%length '(1 2 3 4 5) l))
;;   ((l 5))
;;   > (%which (l) (%delete 1 '(1 2 3 1 1 4 5 1) l))
;;   ((l (2 3 4 5)))
;;   > (%which (l) (%append '(1 2 3) '(4 5 6 7) l))
;;   ((l (1 2 3 4 5 6 7)))
;;   > (%more)
;;   #f

; (%length l n) holds if length(l) = n

(define %length
  (%rel (h t n m)
    (('() 0))
    (((cons h t) n) (%length t m) (%is n (+ m 1)))))

; (%delete x y z) holds if z is y with all x's removed

(define %delete
  (%rel (x y z w)
    ((x '() '()))
    ((x (cons x w) y) (%delete x w y))
    ((x (cons z w) (cons z y)) (%not (%= x z)) (%delete x w y))))

; (%remdup x y) holds if y is x without duplicates

(define %remdup
  (%rel (x y z w)
    (('() '()))
    (((cons x y) (cons x z)) (%delete x y w) (%remdup w z))))

; (%count x n) holds if n is the number of elements in x without counting duplicates

'(define %count
  (%rel (x n y)
    ((x n) (%remdup x y) (%length y n))))

; Same thing

(define %count
  (letrec ((countaux
	     (%rel (m n m+1 x y z)
	       (('() m m))
	       (((cons x y) m n)
		(%delete x y z) (%is m+1 (+ m 1)) (countaux z m+1 n)))))
    (%rel (x n)
      ((x n) (countaux x 0 n)))))

; (%append x y z) holds if z is the concatenation of x and y

(define %append
  (%rel (x y z w)
    (('() x x))
    (((cons x y) z (cons x w)) (%append y z w))))

; (%reverse x y) holds if the y is the reversal of x

'(define %reverse
  (%rel (x y z yy)
    (('() '()))
    (((cons x y) z) (%reverse y yy) (%append yy (list x) z))))

; Same thing, but tailcall optimizing

(define %reverse
  (letrec ((revaux
	     (%rel (x y z w)
	       (('() y y))
	       (((cons x y) z w) (revaux y (cons x z) w)))))
    (%rel (x y)
      ((x y) (revaux x '() y)))))

; (%fact n m) holds if m = n!

#;(define %fact
  (%rel (n n! n-1 n-1!)
    ((0 1))
    ((n n!) (%is n-1 (- n 1)) (%fact n-1 n-1!) (%is n! (* n n-1!)))))

; Same thing, but tailcall optimizing

(define %fact
  (letrec ((factaux
	     (%rel (n! m x m-1 xx)
	       ((0 n! n!))
	       ((m x n!) (%is m-1 (- m 1)) (%is xx (* x m))
		(factaux m-1 xx n!)))))
    (%rel (n n!)
      ((n n!) (factaux n 1 n!)))))
