;;; SRFI 144
;;; Flonums
;;; 
;;; This SRFI describes numeric procedures applicable to flonums, a subset
;;; of the inexact real numbers provided by LispKit. Just like in most Schemes,
;;; the flonums and the inexact reals are the same in LispKit. The procedures
;;; of SRFI 144 are semantically equivalent to the corresponding generic
;;; procedures, but allow more efficient implementations.
;;; 
;;; Specification: John Cowan, Will Clinger
;;; 
;;; Original implementation:
;;; Copyright © 2016 William D Clinger. All rights reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;; this software and associated documentation files (the "Software"), to deal in the
;;; Software without restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
;;; Software, and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;; 
;;; LispKit Port:
;;;   Copyright © 2022 Matthias Zenger. All rights reserved.

(define-library (srfi 144)

  (export ;; Mathematical Constants
          fl-e
          fl-1/e
          fl-e-2
          fl-e-pi/4
          fl-log2-e
          fl-log10-e
          fl-log-2
          fl-1/log-2
          fl-log-3
          fl-log-pi
          fl-log-10
          fl-1/log-10
          fl-pi
          fl-1/pi
          fl-2pi
          fl-pi/2
          fl-pi/4
          fl-2/sqrt-pi
          fl-pi-squared
          fl-degree
          fl-2/pi
          fl-sqrt-2
          fl-sqrt-3
          fl-sqrt-5
          fl-sqrt-10
          fl-1/sqrt-2
          fl-cbrt-2
          fl-cbrt-3
          fl-4thrt-2
          fl-phi
          fl-log-phi
          fl-1/log-phi
          fl-euler
          fl-e-euler
          fl-sin-1
          fl-cos-1
          fl-gamma-1/2
          fl-gamma-1/3
          fl-gamma-2/3
          ;; Implementation Constants
          fl-greatest
          fl-least
          fl-epsilon
          fl-fast-fl+*
          fl-integer-exponent-zero
          fl-integer-exponent-nan
          ;; Constructors
          flonum
          fladjacent
          flcopysign
          make-flonum
          ;; Accessors
          flinteger-fraction
          flexponent
          flinteger-exponent
          flnormalized-fraction-exponent
          flsign-bit
          ;; Predicates
          flonum?
          fl=?
          fl<?
          fl>?
          fl<=?
          fl>=?
          flunordered?
          flmax
          flmin
          flinteger?
          flzero?
          flpositive?
          flnegative?
          flodd?
          fleven?
          flfinite?
          flinfinite?
          flnan?
          flnormalized?
          fldenormalized?
          ;; Arithmetic
          fl+
          fl*
          fl+*
          fl-
          fl/
          flabs
          flabsdiff
          flposdiff
          flsgn
          flnumerator
          fldenominator
          flfloor
          flceiling
          flround
          fltruncate
          ;; Exponents and logarithsm
          flexp
          flexp2
          flexp-1
          flsquare
          flsqrt
          flcbrt
          flhypot
          flexpt
          fllog
          fllog1+
          fllog2
          fllog10
          make-fllog-base
          ;; Trigonometric functions
          flsin
          flcos
          fltan
          flasin
          flacos
          flatan
          flsinh
          flcosh
          fltanh
          flasinh
          flacosh
          flatanh
          ;; Integer division
          flquotient
          flremainder
          flremquo
          ;; Special functions
          flgamma
          flloggamma
          flfirst-bessel
          flsecond-bessel
          flerf
          flerfc)

  (import (rename (lispkit base)
            (infinite? flinfinite?)
            (fl> fl>?)
            (fl< fl<?)
            (fl>= fl>=?)
            (fl<= fl<=?)
            (fl= fl=?)
            (flexponent flinteger-exponent)
            (integer? flinteger?)
            (sqrt flsqrt)
            (floor flfloor)
            (log fllog)
            (truncate fltruncate)
            (nan? flnan?)
            (sin flsin)
            (exp flexp)
            (ceiling flceiling)
            (round flround)
            (even? fleven?)
            (asin flasin)
            (finite? flfinite?)
            (tan fltan)
            (expt flexpt)
            (cos flcos)
            (acos flacos)
            (odd? flodd?)
            (atan flatan)))
  
  ;; Flonum constants
  (begin
    (define fl-e          2.7182818284590452353602874713526624977572) ; e
    (define fl-1/e        0.3678794411714423215955237701614608674458) ; 1/e
    (define fl-e-2        7.3890560989306502272304274605750078131803) ; e^2
    (define fl-e-pi/4     2.1932800507380154565597696592787382234616) ; e^(pi/4)
    (define fl-log2-e     1.44269504088896340735992468100189214)      ; log_2(e)
    (define fl-log10-e    0.434294481903251827651128918916605082)     ; log_10(e)
    (define fl-log-2      0.6931471805599453094172321214581765680755) ; ln(2)
    (define fl-1/log-2    1.4426950408889634073599246810018921374266) ; 1/ln(2)
    (define fl-log-3      1.0986122886681096913952452369225257046475) ; ln(3)
    (define fl-log-pi     1.1447298858494001741434273513530587116473) ; ln(pi)
    (define fl-log-10     2.3025850929940456840179914546843642076011) ; ln(10)
    (define fl-1/log-10   0.4342944819032518276511289189166050822944) ; 1/ln(10)
    (define fl-pi         3.1415926535897932384626433832795028841972) ; pi
    (define fl-1/pi       0.3183098861837906715377675267450287240689) ; 1/pi
    (define fl-2pi        6.2831853071795862319959269370883703231812) ; pi * 2
    (define fl-pi/2       1.57079632679489661923132169163975144)      ; pi/2
    (define fl-2/pi       0.636619772367581343075535053490057448)     ; 2/pi
    (define fl-pi/4       0.785398163397448309615660845819875721)     ; pi/4
    (define fl-2/sqrt-pi  1.12837916709551257389615890312154517)      ; 2/sqrt(pi)
    (define fl-sqrt-pi    1.7724538509055160272981674833411451827975) ; sqrt(pi)
    (define fl-pi-squared 9.8696044010893586188344909998761511353137) ; pi^2
    (define fl-degree     0.0174532925199432957692369076848861271344) ; pi/180
    (define fl-gamma-1/2  1.7724538509055160272981674833411451827975) ; gamma(1/2)
    (define fl-gamma-1/3  2.6789385347077476336556929409746776441287) ; gamma(1/3)
    (define fl-gamma-2/3  1.3541179394264004169452880281545137855193) ; gamma(2/3)
    (define fl-sqrt-2     1.4142135623730950488016887242096980785697) ; sqrt(2)
    (define fl-sqrt-3     1.7320508075688772935274463415058723669428) ; sqrt(3)
    (define fl-sqrt-5     2.2360679774997896964091736687312762354406) ; sqrt(5)
    (define fl-sqrt-10    3.1622776601683793319988935444327185337196) ; sqrt(10)
    (define fl-cbrt-2     1.2599210498948731647672106072782283505703) ; cubert(2)
    (define fl-cbrt-3     1.4422495703074083823216383107801095883919) ; cubert(3)
    (define fl-4thrt-2    1.1892071150027210667174999705604759152930) ; fourthrt(2)
    (define fl-1/sqrt-2   0.7071067811865475244008443621048490392848) ; 1/sqrt(2)
    (define fl-phi        1.6180339887498948482045868343656381177203) ; phi
    (define fl-log-phi    0.4812118250596034474977589134243684231352) ; ln(phi)
    (define fl-1/log-phi  2.0780869212350275376013226061177957677422) ; 1/ln(phi)
    (define fl-euler      0.5772156649015328606065120900824024310422) ; euler
    (define fl-e-euler    1.7810724179901979852365041031071795491696) ; e^euler
    (define fl-sin-1      0.8414709848078965066525023216302989996226) ; sin(1)
    (define fl-cos-1      0.5403023058681397174009366074429766037323) ; cos(1)
  )
  
  ;; Preserve usage of original functions; for now, these are just synonyms
  (begin
    (define expt flexpt)
    (define finite? flfinite?)
    (define log fllog)
    (define even? fleven?)
    (define integer? flinteger?)
    (define round flround)
  )
  
  ;; Support procedures
  (begin
    (define precision-bits    ; IEEE double has 53 bits of precision
      (let loop ((bits 0)
                 (x 1.0))
        (if (= x (+ x 1.0))
            bits
            (loop (+ bits 1)
                  (* 2.0 x)))))
    
    (define (check-flonum! name x)
      (if (not (flonum? x))
          (error (string-append "non-flonum argument passed to "
                                (symbol->string name))
                 x)))
    
    ;;; Given a symbol naming a flonum procedure and a generic operation,
    ;;; returns a flonum procedure that restricts the generic operation
    ;;; to flonum arguments and result.
    (define (flop1 name op)
      (lambda (x)
        (check-flonum! name x)
        (let ((result (op x)))
          (if (not (flonum? result))
              (error (string-append "non-flonum result from "
                                    (symbol->string name))
                     result))
          result)))
    
    (define (flop2 name op)
      (lambda (x y)
        (check-flonum! name x)
        (check-flonum! name y)
        (let ((result (op x y)))
          (if (not (flonum? result))
              (error (string-append "non-flonum result from "
                                    (symbol->string name))
                     result))
          result)))
    
    (define (flop3 name op)
      (lambda (x y z)
        (check-flonum! name x)
        (check-flonum! name y)
        (check-flonum! name z)
        (let ((result (op x y z)))
          (if (not (flonum? result))
              (error (string-append "non-flonum result from "
                                    (symbol->string name))
                     result))
          result)))
        
    ;;; Given a flonum x and a list of flonum coefficients for a polynomial,
    ;;; in order of increasing degree, returns the value of the polynomial at x.
    (define (polynomial-at x coefs)
      (if (null? coefs)
          0.0
          (fl+ (car coefs)
               (fl* x (polynomial-at x (cdr coefs))))))
    
    ;;; This uses Simpson's rule.
    (define (definite-integral lower upper f . rest)
      (let* ((range (fl- upper lower))
             (kmax (if (or (null? rest)
                           (not (and (exact-integer? (car rest))
                                     (even? (car rest))
                                     (positive? (car rest)))))
                       1024 ; FIXME: must be even, should be power of 2
                       (car rest)))
             (n2 (inexact kmax))
             (h (fl/ range n2)))
        (define (loop k n sum)    ; n = (inexact k)
          (cond ((= k 0)
                 (loop 1 1.0 (f lower)))
                ((= k kmax)
                 (fl+ sum (f upper)))
                (else
                 (let ((fn (f (+ lower (fl/ (fl* n range) n2)))))
                   (loop (+ k 1)
                         (fl+ n 1.0)
                         (fl+ sum (fl* (if (even? k) 2.0 4.0) fn)))))))
        (fl/ (fl* h (loop 0 0.0 0.0))
             3.0)))
    
    ;;; Given x between x0 and x1, interpolates between f0 and f1.
    ;;; Can also extrapolate.
    (define (interpolate x x0 x1 f0 f1)
      (let ((delta (fl- x1 x0)))
        (fl+ (fl* (fl/ (fl- x1 x) delta) f0)
             (fl* (fl/ (fl- x x0) delta) f1))))
    
    ;;; Given a exact non-negative integer, returns its factorial.
    (define (fact x)
      (if (zero? x)
          1
          (* x (fact (- x 1)))))
    
    ;;; Given a non-negative integral flonum x, returns its factorial.
    (define (factorial x)
      (if (flzero? x)
          1.0
          (fl* x (factorial (fl- x 1.0)))))
    
  )
  
  ;;; References
  ;;;
  ;;; Milton Abramowitz and Irene A Stegun [editors].
  ;;; Handbook of Mathematical Functions With Formulas, Graphs, and
  ;;; Mathematical Tables.  United States Department of Commerce.
  ;;; National Bureau of Standards Applied Mathematics Series, 55,
  ;;; June 1964.  Fifth Printing, August 1966, with corrections.
  ;;;
  ;;; R W Hamming.  Numerical Methods for Scientists and Engineers.
  ;;; McGraw-Hill, 1962.
  ;;;
  ;;; Donald E Knuth.  The Art of Computer Programming, Volume 2,
  ;;; Seminumerical Algorithms, Second Edition.  Addison-Wesley, 1981.
  ;;;
  ;;; J N Newman.  Approximations for the Bessel and Struve Functions.
  ;;; Mathematics of Computation, 43(168), October 1984, pages 551-556.
  
  ;;; I have deliberately avoided recent references, and have also
  ;;; avoided looking at any code or pseudocode for these or similar
  ;;; functions.
  
  ;;; Quick-and-dirty implementation of a draft of SRFI 144 (flonums),
  ;;; as specified at http://vrici.lojban.org/~cowan/temp/srfi-144.html
  ;;; as of 4 June 2017.
  ;;;
  ;;; FIXME: not as accurate as it should be
  ;;; FIXME: not as fast as it should be
  ;;; FIXME: assumes IEEE arithmetic or similar
  ;;; FIXME: assumes all inexact reals are flonums
  ;;; FIXME: assumes (scheme inexact)
  (begin
  
    ;; Implementation Constants
    
    (define fl-fast-fl+* #f)
    
    (define fl-integer-exponent-zero                ; arbitrary
      (exact (- (log fl-least 2.0) 1.0)))
    
    (define fl-integer-exponent-nan                 ; arbitrary
      (- fl-integer-exponent-zero 1))
    
    ;;; Constructors
    
    ; Implements post-finalization note 1
    (define (flonum x)
      (if (real? x)
          (inexact x)
          +nan.0))
    
    (define fladjacent
      (flop2 'fladjacent
             (lambda (x y)
               (define (loop y)
                 (let* ((y3 (fl+ (fl* 0.999755859375 x) (fl* 0.000244140625 y))))
                   (cond ((fl<? x y3 y)
                          (loop y3))
                         ((fl<? y y3 x)
                          (loop y3))
                         (else
                          (loop2 y)))))
               (define (loop2 y)
                 (let* ((y2 (fl/ (fl+ x y) 2.0))
                        (y2 (if (flinfinite? y2)
                                (fl+ (fl* 0.5 x) (fl* 0.5 y))
                                y2)))
                   (cond ((fl=? x y2)
                          y)
                         ((fl=? y y2)
                          y)
                         (else
                          (loop2 y2)))))
               (cond ((flinfinite? x)
                      (cond ((fl<? x y) (fl- fl-greatest))
                            ((fl>? x y) fl-greatest)
                            (else x)))
                     ((fl=? x y)
                      x)
                     ((flzero? x)
                      (if (flpositive? y)
                          fl-least
                          (fl- fl-least)))
                     ((fl<? x y)
                      (loop (flmin y
                                   fl-greatest
                                   (flmax (* 2.0 x)
                                          (* 0.5 x)))))
                     ((fl>? x y)
                      (loop (flmax y
                                   (fl- fl-greatest)
                                   (flmin (* 2.0 x)
                                          (* 0.5 x)))))
                     (else    ; x or y is a NaN
                      x)))))
    
    (define flcopysign
      (flop2 'flcopysign
             (lambda (x y)
               (if (= (flsign-bit x) (flsign-bit y))
                   x
                   (fl- x)))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;; Accessors
    
    (define (flexponent x)
      (inexact (flinteger-exponent x)))
    
    (define (flinteger-fraction x)
      (check-flonum! 'flinteger-fraction x)
      (let* ((result1 (fltruncate x))
             (result2 (fl- x result1)))
        (values result1 result2)))
    
    (define (flnormalized-fraction-exponent x)
      (define (return result1 result2)
        (cond ((fl<? result1 0.5)
               (values (fl* 2.0 result1) (- result2 1)))
              ((fl>=? result1 1.0)
               (values (fl* 0.5 result1) (+ result2 1)))
              (else
               (values result1 result2))))
      (check-flonum! 'flnormalized-fraction-exponent x)
      (cond ((flnan? x)    ; unspecified for NaN
             (values x 0))
            ((fl<? x 0.0)
             (call-with-values
              (lambda () (flnormalized-fraction-exponent (fl- x)))
              (lambda (y n)
                (values (fl- y) n))))
            ((fl=? x 0.0)    ; unspecified for 0.0
             (values 0.0 0))
            ((flinfinite? x)
             (values 0.5 (+ 3 (exact (round (fllog2 fl-greatest))))))
            ((flnormalized? x)
             (let* ((result2 (exact (flround (fllog2 x))))
                    (result2 (if (integer? result2)
                                 result2
                                 (round result2)))
                    (two^result2 (inexact (expt 2.0 result2))))
               (if (flinfinite? two^result2)
                   (call-with-values
                    (lambda () (flnormalized-fraction-exponent (fl/ x 4.0)))
                    (lambda (y n)
                      (values y (+ n 2))))
                   (return (fl/ x two^result2) result2))))
            (else
             (let* ((k (+ 2 precision-bits))
                    (two^k (expt 2 k)))
               (call-with-values
                (lambda ()
                  (flnormalized-fraction-exponent (fl* x (inexact two^k))))
                (lambda (y n)
                  (return y (- n k))))))))
    
    (define (flsign-bit x)
      (check-flonum! 'flsign-bit x)
      (cond ((fl<? x 0.0)
             1)
            ((eqv? x -0.0)
             1)
            (else
             0)))
    
    
    ;;; Predicates
    
    (define (flunordered? x y)
      (or (flnan? x) (flnan? y)))
    
    (define (flnormalized? x)
      (check-flonum! 'flnormalized? x)
      (let ((x (flabs x)))
        (and (flfinite? x)
             (fl<? (fl/ fl-greatest) x))))
    
    (define (fldenormalized? x)
      (check-flonum! 'fldenormalized? x)
      (let ((x (flabs x)))
        (and (flfinite? x)
             (fl<? 0.0 x)
             (fl<=? x (fl/ fl-greatest)))))
    
    ;;; Arithmetic
    
    ;;; Spec says "as if to infinite precision and rounded only once".
    
    (define fl+*
      (flop3 'fl+*
             (lambda (x y z)
               (cond ((and (flfinite? x) (flfinite? y))
                      (if (flfinite? z)
                          (let ((x (exact x))
                                (y (exact y))
                                (z (exact z)))
                            (flonum (+ (* x y) z)))
                          z))
                     (else
                      (fl+ (fl* x y) z))))))
    
    (define (flabsdiff x y)
      (flabs (fl- x y)))
    
    (define (flposdiff x y)
      (let ((diff (fl- x y)))
        (if (flnegative? diff)
            0.0
            diff)))
    
    (define (flsgn x)
      (flcopysign 1.0 x))
    
    (define flnumerator
      (flop1 'flnumerator
             (lambda (x)
               (cond ((flnan? x) x)
                     ((flinfinite? x) x)
                     (else (inexact (numerator (exact x))))))))
    
    (define fldenominator
      (flop1 'fldenominator
             (lambda (x)
               (cond ((flnan? x) x)
                     ((flinfinite? x) 1.0)
                     ((flzero? x) 1.0)
                     (else (inexact (denominator (exact x))))))))
    
    ;;; Exponents and logarithms
    
    (define flexp2 (flop1 'flexp2 (lambda (x) (flexpt 2.0 x))))
    
    ;;; e^x = \sum_n (z^n / (n!))
    ;;;
    ;;; FIXME: the number of terms and the constant 0.5 seem reasonable
    ;;; for IEEE double precision, but the number of terms might need
    ;;; to be increased for higher precisions.
    
    (define flexp-1
      (flop1 'flexp-1
             (let ((coefs (cons 0.0
                                (map fl/
                                     (map factorial
                                          '(1.0 2.0 3.0 4.0 5.0
                                            6.0 7.0 8.0 9.0 10.0
                                            11.0 12.0 13.0 14.0 15.0))))))
               (lambda (x)
                 (cond ((fl<? (flabs x) 0.5)    ; FIXME
                        (polynomial-at x coefs))
                       (else
                        (fl- (flexp x) 1.0)))))))
    
    (define flsquare (flop1 'flsquare (lambda (x) (fl* x x))))
    
    (define flcbrt
      (flop1 'flcbrt
             (lambda (x)
               (cond ((flnegative? x)
                      (fl- (flcbrt (fl- x))))
                     (else
                      (flexpt x (fl/ 3.0)))))))
    
    (define flhypot
      (flop2 'flhypot
             (lambda (x y)
               (cond ((flzero? x) (flabs y))
                     ((flzero? y) (flabs x))
                     ((or (flinfinite? x) (flinfinite? y)) +inf.0)
                     ((flnan? x) x)
                     ((flnan? y) y)
                     ((fl>? y x) (flhypot y x))
                     (else
                      (let* ((y/x (fl/ y x))
                             (root (flsqrt (fl+ 1.0 (fl* y/x y/x)))))
                        (fl* (flabs x) root)))))))
    
    ;;; Returns log(x+1), as in C99 log1p.
    
    ;;; See https://stat.ethz.ch/pipermail/r-devel/2003-August/027396.html
    ;;; https://books.google.com/books?id=OjUyDwAAQBAJ&pg=PA290&lpg=PA290&dq=beebe+log1p&source=bl&ots=VLxmiSk1fA&sig=ACfU3U0_8tqKemomSjKW73iJ0zUO1u3p3Q&hl=en&sa=X&ved=2ahUKEwjfxZbE8LvhAhVNm-AKHWScB7w4ChDoATAAegQICRAB#v=onepage&q=beebe%20log1p&f=false
    
    ;;; for justification
    
    (define fllog1+
      (flop1 'fllog1+
             (lambda (x)
               (let ((u (fl+ 1.0 x)))
                 (cond ((fl=? u 1.0)
                        x) ;; gets sign of zero result correct
                       ((fl=? u x)
                        (fllog u)) ;; large arguments and infinities
                       (else
                        (fl* (fllog u) (fl/ x (fl- u 1.0)))))))))
    
    
    (define fllog2 (flop1 'fllog2 (lambda (x) (log x 2.0))))
    
    (define fllog10 (flop1 'fllog10 (lambda (x) (log x 10.0))))
    
    (define (make-fllog-base base)
      (check-flonum! 'make-fllog-base base)
      (if (fl>? base 1.0)
          (flop1 'procedure-created-by-make-fllog-base
                 (lambda (x) (log x base)))
          (error "argument to make-fllog-base must be greater than 1.0" base)))
    
    ;;; Trigonometric functions
    
    (define flsinh
      (flop1 'flsinh
             (lambda (x)
               (cond ((not (flfinite? x)) x)
                     ((fl<? (flabs x) 0.75)
                      (fl/ (fl- (flexp-1 x) (flexp-1 (fl- x))) 2.0))
                     (else
                      (fl/ (fl- (flexp x) (flexp (fl- x))) 2.0))))))
    
    (define flcosh
      (flop1 'flcosh
             (lambda (x)
               (cond ((not (flfinite? x)) (flabs x))
                     ((fl<? (flabs x) 0.75)
                      (fl+ 1.0 (fl/ (fl+ (flexp-1 x) (flexp-1 (fl- x))) 2.0)))
                     (else
                      (fl/ (fl+ (flexp x) (flexp (fl- x))) 2.0))))))
    
    (define fltanh
      (flop1 'fltanh
             (lambda (x)
               (cond ((flinfinite? x) (flcopysign 1.0 x))
                     ((flnan? x) x)
                     (else
                      (let ((a (flsinh x))
                            (b (flcosh x)))
                        (cond ((fl=? a b)
                               1.0)
                              ((fl=? a (fl- b))
                               -1.0)
                              (else
                               (fl/ (flsinh x) (flcosh x))))))))))
    
    ;;; inverse hyperbolic functions
    
    (define flasinh
      (flop1 'flasinh
             (lambda (x)
               (cond ((or (flinfinite? x)
                          (flnan? x))
                      x)
                     ((flnegative? x)
                      (fl- (flasinh (fl- x))))
                     ((fl<? x 3.725290298461914e-9)   ;; (flexpt 2. -28.)
                      x)
                     ((fl<? x 2.)
                      ;; the naive formula is
                      ;; (log (+ x (sqrt (+ (square x) 1))))
    
                      ;; We want to
                      ;; 1.  Use exact operations when possible (no roundoff)
                      ;; 2.  Add or subtract things of differing magnitudes,
                      ;;     so for most arguments at most one roundoff error.
    
                      ;; Biggeset possible problem near x=0, so we write
                      ;; (sqrt (+ 1 (square x)))
                      ;; as
                      ;; (+ 1 (- (sqrt (+ 1 (square x))) 1))
                      ;; and then multiply the second part in
                      ;; numerator and denominator by
                      ;; (+ (sqrt (+ 1 (square x))) 1)
    
                      (let ((x^2 (flsquare x)))
                        (fllog1+ (fl+ x
                                      (fl/ x^2
                                           (fl+ 1.
                                                (flsqrt (fl+ 1.0 x^2))))))))
                     ((fl<? x 268435456.) ;; (flexpt 2. 28.)
                      (let ((x^2 (flsquare x)))
                        (fllog (fl+ (fl* 2. x) ;; exact
                                    ;; the rest is small
                                    (fl/ 1.
                                         (fl+ x
                                              (flsqrt (fl+ 1.0 x^2))))))))
                     (else
                      (fl+ (fllog x) fl-log-2))))))
    
    (define flacosh
      (flop1 'flacosh
             (lambda (x)
               (cond ((flnan? x) x)
                     ((fl<? x 1.0) +nan.0)
                     ((fl<? x 2.0)
                      ;; the naive formula is
                      ;; (log (+ x (sqrt (- (square x) 1))))
    
                      ;; We want to
                      ;; 1.  Use exact operations when possible (no roundoff)
                      ;; 2.  Add or subtract things of differing magnitudes,
                      ;;     so for most arguments at most one roundoff error.
    
                      (let ((x-1 (fl- x 1.))) ;; exact
                        (fllog1+ (fl+ x-1 ;; smaller than next expression
                                      (flsqrt (fl+ (fl* 2. x-1) ;; exact
                                                   ;; relatively small
                                                   (flsquare x-1)))))))
                     ((fl<? x 268435456.) ;; (flexpt 2. 28.)
                      (fllog (fl- (fl* 2. x) ;; exact
                                  ;; next is smaller
                                  (fl/ (fl+ x (flsqrt (fl* (fl- x 1.) ;; exact
                                                           (fl+ x 1.) ;; exact
                                                           )))))))
                     (else
                      (fl+ (fllog x) fl-log-2))))))
    
    (define flatanh
      (flop1 'flatanh
             (lambda (x)
               (cond ((fl<? x 0.)
                      (fl- (flatanh (fl- x))))
                     ;; we rewrite
                     ;; (/ (+ 1 x) (- 1 x))
                     ;; as
                     ;; (+ 1 (* 2 (/ x (- 1 x))))
                     ;; and call fllog1+ instead of fllog
                     (else
                      (fl* +0.5                                    ;; exact
                           (fllog1+ (fl* +2.0                      ;; exact
                                         (fl/ x
                                              (fl- 1.0 x)))))))))) ;; exact
    
    ;;; Integer division
    
    (define flquotient
      (flop2 'flquotient
             (lambda (x y)
               (fltruncate (fl/ x y)))))
    
    ;;; FIXME: should probably implement the following part of the C spec:
    ;;; "If the returned value is 0, it will have the same sign as x."
    
    (define flremainder
      (flop2 'flremainder
             (lambda (x y)
               (fl- x (fl* y (flquotient x y))))))
    
    (define (flremquo x y)
      (check-flonum! 'flremquo x)
      (check-flonum! 'flremquo y)
      (let* ((quo (flround (fl/ x y)))
             (rem (fl- x (fl* y quo))))
        (values rem
                (exact quo))))
  )
  
  ;;; References
  ;;;
  ;;; Milton Abramowitz and Irene A Stegun [editors].
  ;;; Handbook of Mathematical Functions With Formulas, Graphs, and
  ;;; Mathematical Tables.  United States Department of Commerce.
  ;;; National Bureau of Standards Applied Mathematics Series, 55,
  ;;; June 1964.  Fifth Printing, August 1966, with corrections.
  ;;;
  ;;; R W Hamming.  Numerical Methods for Scientists and Engineers.
  ;;; McGraw-Hill, 1962.
  ;;;
  ;;; Donald E Knuth.  The Art of Computer Programming, Volume 2,
  ;;; Seminumerical Algorithms, Second Edition.  Addison-Wesley, 1981.
  ;;;
  ;;; J N Newman.  Approximations for the Bessel and Struve Functions.
  ;;; Mathematics of Computation, 43(168), October 1984, pages 551-556.
  
  ;;; I have deliberately avoided recent references, and have also
  ;;; avoided looking at any code or pseudocode for these or similar
  ;;; functions.
  (begin
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;
    ;;; Gamma function
    ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;; Abramowitz and Stegun 6.1.5 ::  z! = Gamma(z+1)
    ;;; Abramowitz and Stegun 6.1.15 :  Gamma(z+1) = z Gamma(z)
    ;;;
    ;;; Gamma(x+2) = (x+1) Gamma(x+1) = (x+1) x Gamma(x)
    ;;; Gamma(x) = Gamma(x+2) / (x (x + 1))
    ;;;
    ;;; Those equations reduce the computation of Gamma(x) to the range
    ;;;     1.0 <= x <= 2.0
    ;;;
    ;;; The following definition is more accurate than C99 tgamma
    ;;; with gcc, Linux, and double precision.  The alarmingly large
    ;;; absolute errors near 16.0 and -2.0e-16 are small relative
    ;;; errors.  At x=16.0, tgamma returns a non-integer result,
    ;;; but flgamma returns the correct integer result.
    
    (define (flgamma x)
      (check-flonum! 'flgamma x)
      (cond ((fl>=? x flgamma:upper-cutoff)
             +inf.0)
            ((fl<=? x flgamma:lower-cutoff)
             (cond ((= x -inf.0)
                    +nan.0)
                   ((flinteger? x)    ; pole error
                    +nan.0)
                   ((flodd? (fltruncate x)) 0.0)
                   (else -0.0)))
            (else (Gamma x))))
    
    (define (Gamma x)
      (cond ((fl>? x 2.0)
             (let ((x (fl- x 2.0)))
               (fl* x (fl+ x 1.0) (Gamma x))))
            ((fl=? x 2.0)
             1.0)
            ((fl>? x 1.0)
             (let ((x (fl- x 1.0)))
               (fl* x (Gamma x))))
            ((fl=? x 1.0)
             1.0)
            ((fl=? x 0.0)
             +inf.0)
            ((fl<? x 0.0)
             (if (flinteger? x)    ; pole error
                 +nan.0
                 (fl/ (Gamma (fl+ x 2.0)) x (fl+ x 1.0))))
            (else
             (fl/ (polynomial-at x gamma-coefs)))))
    
    ;;; Series expansion for 1/Gamma(x), from Abramowitz and Stegun 6.1.34
    
    (define gamma-coefs
      '(0.0
        1.0
        +0.5772156649015329
        -0.6558780715202538
        -0.0420026350340952
        +0.1665386113822915 ; x^5
        -0.0421977345555443
        -0.0096219715278770
        +0.0072189432466630
        -0.0011651675918591
        -0.0002152416741149 ; x^10
        +0.0001280502823882
        -0.0000201348547807
        -0.0000012504934821
        +0.0000011330272320
        -0.0000002056338417 ; x^15
        +0.0000000061160950
        +0.0000000050020075
        -0.0000000011812746
        +0.0000000001043427
        +0.0000000000077823 ; x^20
        -0.0000000000036968
        +0.0000000000005100
        -0.0000000000000206
        -0.0000000000000054
        +0.0000000000000014 ; x^25
        +0.0000000000000001
        ))
    
    ;;; If x >= flgamma:upper-cutoff, then (Gamma x) is +inf.0
    
    (define flgamma:upper-cutoff
      (do ((x 2.0 (+ x 1.0)))
          ((flinfinite? (Gamma x))
           x)))
    
    ;;; If x <= flgamma:lower-cutoff, then (Gamma x) is a zero or NaN
    
    (define flgamma:lower-cutoff
      (do ((x -2.0 (- x 1.0)))
          ((flzero?
            (Gamma (fladjacent x 0.0)))
           x)))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;
    ;;; log (Gamma (x))
    ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;; Returns two values:
    ;;;     log ( |Gamma(x)| )
    ;;;     sgn (Gamma(x))
    ;;;
    ;;; The draft spec is unclear concerning sgn (Gamma(x)),
    ;;; but (flsgn x) returns (flcopysign 1.0 x) so I'm assuming
    ;;; sgn (Gamma(x)) is +1.0 for positive and -1.0 for negative.
    ;;; For real x, Gamma(x) is never actually zero, but it is
    ;;; undefined if x is zero or a negative integer.
    
    ;;; For small absolute values, this is trivial.
    ;;; Abramowitz and Stegun give several asymptotic formulas
    ;;; that might be good enough for large values of x.
    ;;;
    ;;; 6.1.41 :  As x goes to positive infinity, log (Gamma (x)) goes to
    ;;;
    ;;;     (x - 1/2) log x - x + 1/2 log (2 pi)
    ;;;         + 1/(12x) - 1/(360x^3) + 1/(1260x^5) - 1/(1680x^7) + ...
    ;;;
    ;;; 6.1.48 states a continued fraction.
    
    (define (flloggamma x)
      (check-flonum! 'flloggamma x)
      (cond ((flinfinite? x)
             (if (flpositive? x)
                 (values x 1.0)
                 (values +inf.0 +nan.0)))
            ((fl>=? x flloggamma:upper-threshold)
             (values (eqn6.1.48 x) 1.0))
            ((fl>? x 0.0)
             (let ((g (flgamma x)))
               (values (log g) 1.0)))
            (else
             (let ((g (flgamma x)))
               (values (log (flabs g))
                       (flcopysign 1.0 g))))))
    
    ;;; This doesn't seem to be as accurate as the continued fraction
    ;;; of equation 6.1.48, so it's commented out for now.
    
    #;
    (define (eqn6.1.41 x)
      (let* ((x^2 (fl* x x))
             (x^3 (fl* x x^2))
             (x^5 (fl* x^2 x^3))
             (x^7 (fl* x^2 x^5)))
        (fl+ (fl* (fl- x 0.5) (fllog x))
             (fl- x)
             (fl* 0.5 (fllog fl-2pi))
             (fl/ (fl* 12.0 x))
             (fl/ (fl* 360.0 x^3))
             (fl/ (fl* 1260.0 x^5))
             (fl/ (fl* 1680.0 x^7)))))
    
    (define (eqn6.1.48 x)
      (let ((+ fl+)
            (/ fl/))
        (+ (fl* (fl- x 0.5) (fllog x))
           (fl- x)
           (fl* 0.5 (fllog fl-2pi))
           (/ #i1/12
              (+ x
                 (/ #i1/30
                    (+ x
                       (/ #i53/210
                          (+ x
                             (/ #i195/371
                                (+ x
                                   (/ #i22999/22737
                                      (+ x
                                         (/ #i29944523/19733142
                                            (+ x
                                               (/ #i109535241009/48264275462
                                                  (+ x)))))))))))))))))
    
    ;;; With IEEE double precision, eqn6.1.48 is at least as accurate as
    ;;; (log (flgamma x)) starting around x = 20.0
    
    (define flloggamma:upper-threshold 20.0)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;
    ;;; Bessel functions
    ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;; FIXME: This isn't as accurate as it should be, it's hard to test
    ;;; because it combines so many different algorithms and intervals,
    ;;; and it underflows to zero too soon.
    ;;;
    ;;; FIXME: To reduce discontinuities at the boundaries, results
    ;;; near boundaries should be computed as weighted averages of
    ;;; the results returned by algorithms used on the two sides of
    ;;; the boundary.
    ;;;
    ;;; FIXME: Several numerical constants, flagged by FIXME comments,
    ;;; seem to work for Larceny's double precision but might need to
    ;;; be changed for other precisions.  They are unlikely to be
    ;;; optimal even for double precision.
    
    (define (flfirst-bessel n x)
      (define (nan-protected y)
        (if (flfinite? y) y 0.0))
      (check-flonum! 'flfirst-bessel x)
      (cond ((< n 0)
             (let ((result (flfirst-bessel (- n) x)))
               (if (even? n) result (- result))))
    
            ((< x 0)
             (let ((result (flfirst-bessel n (- x))))
               (if (even? n) result (- result))))
    
            ((= x +inf.0)
             0.0)
    
            (else
             (case n
              ((0)    (cond ((fl<? x 4.5)     ; FIXME
                             (eqn9.1.10 n x))
                            ((fl<? x 93.0)    ; FIXME
                             (eqn9.1.18 n x))
                            (else
                             (eqn9.2.5 n x))))
              ((1)    (cond ((fl<? x 11.0)    ; FIXME
                             (eqn9.1.10-fast n x))
                            ((fl<? x 300.0)   ; FIXME
                             (eqn9.1.75 n x))
                            ((fl<? x 1e12)    ; FIXME
                             (eqn9.2.5 n x))
                            (else
                             (eqn9.2.1 n x))))
              ((2)    (cond ((fl<? x 10.0)    ; FIXME
                             (eqn9.1.10-fast n x))
                            ((fl<? x 1e19)    ; FIXME
                             (eqn9.1.27-first-bessel n x))
                            (else
                             ;; FIXME
                             0.0)))
              ((3)    (cond ((fl<? x 10.0)    ; FIXME
                             (eqn9.1.10-fast n x))
                            ((fl<? x 1e6)     ; FIXME
                             (eqn9.1.27-first-bessel n x))
                            (else
                             (nan-protected (eqn9.2.5 n x)))))
              (else   (cond ((fl<? x 12.0)    ; FIXME
                             (nan-protected (eqn9.1.10-fast n x)))
                            ((fl<? x 150.0)   ; FIXME
                             (nan-protected (if (fl>? (inexact n) x)
                                                (method9.12ex1 n x)
                                                (eqn9.1.75 n x))))
                            ((fl<? x 1e18)    ; FIXME
                             (nan-protected (eqn9.1.27-first-bessel n x)))
                            (else
                             ;; FIXME
                             0.0)))))))
    
    (define (flsecond-bessel n x)
      (check-flonum! 'flsecond-bessel x)
      (cond ((< n 0)
             (let ((result (flsecond-bessel (- n) x)))
               (if (even? n) result (- result))))
    
            ((fl<? x 0.0)
             +nan.0)
    
            ((fl=? x 0.0)
             -inf.0)
    
            ((fl=? x +inf.0)
             0.0)
    
            (else
             (case n
              ((0)    (cond ((fl<? x 14.5)        ; FIXME
                             (eqn9.1.13 0 x))
                            (else
                             (eqn9.2.6 0 x))))
              ((1)    (cond ((fl<? x 1e12)        ; FIXME
                             (eqn9.1.16 n x))
                            (else
                             (eqn9.2.6 n x))))
              ((2 3)  (cond (else
                             (eqn9.1.27-second-bessel n x))))
              (else   (let ((ynx (eqn9.1.27-second-bessel n x)))
                        (if (flnan? ynx)
                            -inf.0
                            ynx)))))))
    
    ;;; For multiples of 1/16:
    ;;;
    ;;; For n = 0, this agrees with C99 jn for 0 <= x <= 1.5
    ;;; and disagrees by no more than 1 bit for 0 <= x <= 2.0.
    ;;; For n = 1, this disagrees by no more than 1 bit for 0 <= x <= 2.5.
    ;;;
    ;;;     n    0 <= x <= xmax    bits
    ;;;
    ;;;     0    0 <= x <= 1.5      0
    ;;;     0    0 <= x <= 2.0      1
    ;;;     1    0 <= x <= 2.5      1
    ;;;     2    0 <= x <= 3.0      2
    ;;;     3    0 <= x <= 2.5      4
    ;;;     4    0 <= x <= 2.5      4
    ;;;     5    0 <= x <= 3.0      3
    ;;;     6    0 <= x <= 4.0      3
    ;;;     7    0 <= x <= 6.5      4
    ;;;     8    0 <= x <= 2.0      3
    ;;;     9    0 <= x <= 1.5      4
    ;;;    10    0 <= x <= 4.5      4
    ;;;    20    0 <= x <= 3.5      6
    ;;;    20    0 <= x <= 6.0      8
    ;;;    30    0 <= x <= 1.0      6
    ;;;    40    0 <= x <= 1.5      6
    ;;;    50    0 <= x <= 1.0      6
    
    
    ;;; It should become more accurate for larger n but less accurate for
    ;;; larger x.  Should be okay if n > x.
    
    (define (eqn9.1.10 n x)
      (fl* (inexact (expt (* 0.5 x) n))
           (polynomial-at (flsquare x)
                          (cond ((= n 0)
                                 eqn9.1.10-coefficients-0)
                                ((= n 1)
                                 eqn9.1.10-coefficients-1)
                                (else
                                 (eqn9.1.10-coefficients n))))))
    
    (define (eqn9.1.10-coefficients n)
      (define (loop k prev)
        (if (flzero? (inexact prev))
            '()
            (let ((c (/ (* -1/4 prev) k (+ n k))))
              (cons c (loop (+ k 1) c)))))
      (let ((c (/ (fact n))))
        (map inexact (cons c (loop 1 c)))))
    
    (define eqn9.1.10-coefficients-0
      (eqn9.1.10-coefficients 0))
    
    (define eqn9.1.10-coefficients-1
      (eqn9.1.10-coefficients 1))
    
    ;;; This is faster than using exact arithmetic to compute coefficients
    ;;; at call time, and it seems to be about as accurate.
    
    (define (eqn9.1.10-fast n x)
      (let* ((y (fl* 0.5 x))
             (y2 (fl- (fl* y y)))
             (bound (+ 25.0 (inexact n))))
        (define (loop k n+k)
          (if (fl>? n+k bound)
              1.0
              (fl+ 1.0
                   (fl* (fl/ y2 (fl* k n+k))
                        (loop (fl+ 1.0 k) (fl+ 1.0 n+k))))))
        (fl/ (fl* (inexact (expt y n))
                  (loop 1.0 (fl+ 1.0 (inexact n))))
             (factorial (inexact n)))))
    
    ;;; Equation 9.1.11 :
    ;;;
    ;;;     Y_n(x) = - (1 / (pi (x/2)^n))
    ;;;                  \sum_{k=0}^{n-1} ((n - k - 1)!/(k!)) (x^2/4)^k
    ;;;              + (2/pi) log (x/2) J_n(x)
    ;;;              - ((x/2)^n / pi)
    ;;;                  \sum_{k=0}^\infty ((psi(k+1) + psi(n+k+1)) / (k! (n+k)!))
    ;;;                                    (x^/4)^k
    ;;; where
    ;;;
    ;;;     psi (1) = - gamma
    ;;;     psi (n) = - gamma + \sum_{k=1}^{n-1} (1/k)
    
    ;;; Equation 9.1.13 is the special case for Y_0(x) :
    ;;;
    ;;;     Y_0(x) = (2/pi) (log (x/2) + gamma) J_0(x)
    ;;;            + (2/pi) ((x^2/4)^1 / (1!)^2
    ;;;                          - (1 + 1/2) (x^2/4)^2 / (2!)^2
    ;;;                          + (1 + 1/2 + 1/3) (x^2/4)^3 / (3!)^2
    ;;;                          - ...)
    
    (define (eqn9.1.13 n x)
      (if (not (= n 0)) (error "eqn9.1.13 requires n=0"))
      (fl* 2.0
           fl-1/pi
           (fl+ (fl* (fl+ (fllog (fl/ x 2.0)) fl-euler)
                     (flfirst-bessel 0 x))
                (polynomial-at (fl* 0.25 x x)
                               eqn9.1.13-coefficients))))
    
    (define eqn9.1.13-coefficients
      (map (lambda (k)
             (cond ((= k 0) 0.0)
                   ((= k 1) 1.0)
                   (else
                    ;; (1 + 1/2 + 1/3 + ... + 1/k) / (k!)^2
                    (let ((c (/ (apply + (map / (cdr (iota (+ k 1)))))
                                (let ((k! (fact k)))
                                  (* k! k!)))))
                      (inexact (if (even? k) (- c) c))))))
           (iota 25))) ; FIXME
    
    ;;; Equation 9.1.16 :
    ;;;
    ;;;     J_{n+1}(x) Y_n(x) - J_n(x) Y_{n+1}(x) = 2 / (pi x)
    ;;; so
    ;;;     Y_{n+1}(x) = (J_{n+1}(x) Y_n(x) - (2 / (pi x))) / J_n(x)
    
    (define (eqn9.1.16 n+1 x)
      (if (= 0 n+1)
          (flsecond-bessel 0 x)
          (let ((n (- n+1 1)))
            (fl/ (fl- (fl* (flfirst-bessel n+1 x) (flsecond-bessel n x))
                      (fl/ 2.0 (fl* fl-pi x)))
                 (flfirst-bessel n x)))))
    
    ;;; Equation 9.1.18 :
    ;;;
    ;;;     J_0(x) = (1 / \pi) \int_0^\pi cos (x sin \theta) d\theta
    ;;;            = (1 / \pi) \int_0^\pi cos (x cos \theta) d\theta
    
    (define (eqn9.1.18 n x)
      (if (> n 0)
          (flfirst-bessel n x)
          (fl* fl-1/pi
               (definite-integral 0.0
                                  fl-pi
                                  (lambda (theta)
                                    (flcos (fl* x (flsin theta))))
                                  128))))
    
    ;;; Equation 9.1.27 says
    ;;;
    ;;; J_{n-1}(x) + J_{n+1}(x) = (2n/x) J_n(x)
    ;;;
    ;;; J_{n+1}(x) = (2n/x) J_n(x) - J_{n-1}(x)
    ;;;
    ;;; J_{n-1}(x) = (2n/x) J_n(x) - J_{n+1}(x)
    ;;;
    ;;; Y_{n-1}(x) + Y_{n+1}(x) = (2n/x) Y_n(x)
    ;;;
    ;;; Y_{n+1}(x) = (2n/x) Y_n(x) - Y_{n-1}(x)
    ;;;
    ;;; Y_{n-1}(x) = (2n/x) Y_n(x) - Y_{n+1}(x)
    ;;;
    ;;; This has too much roundoff error if n > x or if x and n have
    ;;; the same magnitude.
    
    (define (eqn9.1.27-first-bessel n x)
      (eqn9.1.27 flfirst-bessel n x))
    
    (define (eqn9.1.27-second-bessel n x)
      (eqn9.1.27 flsecond-bessel n x))
    
    (define (eqn9.1.27 f n0 x)
      (define (loop n jn jn-1)
        (cond ((= n n0)
               jn)
              (else
               (loop (+ n 1)
                     (fl- (fl* (fl/ (inexact (+ n n)) x) jn)
                          jn-1)
                     jn))))
      (if (<= n0 1)
          (f n0 x)
          (loop 1 (f 1 x) (f 0 x))))
    
    ;;; For x < n, Abramowitz and Stegun 9.12 Example 1 suggests this method:
    ;;;
    ;;;     1.  Choose odd N large enough so J_N(x) is essentially zero.
    ;;;     2.  Choose an arbitrary trial value, say 1.0, for J_{N-1}(x).
    ;;;     3.  Use equation 9.1.27 to estimate the relative values
    ;;;         of J_{N-2}(x), J_{N-3}(x), ...
    ;;;     4.  Normalize using equation 9.1.46 :
    ;;;
    ;;;             1 = J_0(x) + 2 J_2(x) + 2 J_4(x) + 2 J_6(x) + ...
    
    (define (method9.12ex1 n0 x)
      (define (loop n jn jn+1 jn0 sumEvens)
        (if (= n 0)
            (fl/ jn0 (+ jn sumEvens sumEvens))
            (let ((jn-1 (fl- (fl/ (fl* 2.0 (inexact n) jn) x) jn+1)))
              (loop (- n 1)
                    jn-1
                    jn
                    (if (= n n0) jn jn0)
                    (if (even? n) (fl+ jn sumEvens) sumEvens)))))
      (let* ((n (min 200 (+ n0 20))) ; FIXME
             (jn+1 (fl/ x (fl* 2.0 (inexact n))))
             (jn 1.0))
        (loop (- n 1) jn jn+1 0.0 0.0)))
    
    ;;; Equation 9.1.75 states an equality between J_n(x)/J_{n-1}(x)
    ;;; and a continued fraction.
    ;;;
    ;;; Precondition: |x| > 0
    ;;;
    ;;; This works very well provided (flfirst-bessel x 0) is accurate
    ;;; and x is small enough for it to run in reasonable time.
    
    (define (eqn9.1.75 n x)
      (define k (max 10 (* 2 (exact (flceiling x)))))
      (define (loop x2 m i)
        (if (> i k)
            (fl/ 1.0 (fl* m x2))
            (fl/ 1.0
                 (fl- (fl* m x2)
                      (loop x2 (+ m 1.0) (+ i 1))))))
      (if (and (> n 0)
               (flpositive? x)
               (fl<? x 1e3))
    ; (if (and (> n 3) (flpositive? x))
          (fl* (eqn9.1.75 (- n 1) x)
               (loop (fl/ 2.0 x) (inexact n) 0))
          (flfirst-bessel n x)))
    
    ;;; Equation 9.1.89 :
    ;;;
    ;;;     Y_0(x) = 2/pi (log (x/2) + gamma) J_0(x)
    ;;;                 - 4/pi \sum_{k=1}^\infty (-1)^k J_{2k}(x)/k
    ;;;
    ;;; To reduce roundoff error, the infinite sum is computed
    ;;; non-tail-recursively.
    ;;;
    ;;; FIXME: not used at present, so I've commented it out.
    
    #;
    (define (eqn9.1.89 n x)
      (define (sum k)
        (let* ((k2 (+ k k))
               (j2k (flfirst-bessel k2 x))
               (y (if (even? k) j2k (fl- j2k))))
          (if (flzero? y)
              y
              (fl+ y (sum (+ k 1))))))
      (if (not (= n 0)) (error "eqn9.1.89 requires n=0"))
      (fl- (fl* 2.0
                fl-1/pi
                (fl+ (fllog (fl/ x 2.0)) fl-euler)
                (flfirst-bessel 0 x))
           (fl* 4.0 fl-1/pi (sum 1))))
                
                
    
    ;;; Equation 9.2.1 states an asymptotic approximation that agrees
    ;;; with C99 jn to 6 decimal places for n = 0 and x = 1e6.
    
    (define (eqn9.2.1 n x)
      (fl* (flsqrt (/ 2.0 (fl* fl-pi x)))
           (flcos (fl- x (fl* fl-pi (fl+ (fl* 0.5 (inexact n)) 0.25))))))
    
    ;;; Equation 9.2.2 states an asymptotic approximation for Y_n.
    ;;;
    ;;; FIXME: not used at present, so I've commented it out.
    
    #;
    (define (eqn9.2.2 n x)
      (fl* (flsqrt (/ 2.0 (fl* fl-pi x)))
           (flsin (fl- x (fl* fl-pi (fl+ (fl* 0.5 (inexact n)) 0.25))))))
    
    ;;; Equation 9.2.5 : For large x,
    ;;;
    ;;;     J_n(x) = sqrt (2/(pi x)) [ P(n, x) cos theta - Q (n, x) sin theta ]
    ;;;
    ;;; where
    ;;;
    ;;;     theta = x - (n/2 + 1/4) pi
    ;;;
    ;;; and P(n, x) and Q(n, x) are defined by equations 9.2.9 and 9.2.10.
    
    (define (eqn9.2.5 n x)
      (let ((theta (fl- x (fl* (fl+ (/ n 2.0) 0.25) fl-pi))))
        (fl* (flsqrt (fl/ 2.0 (fl* fl-pi x)))
             (fl- (fl* (eqn9.2.9 n x) (flcos theta))
                  (fl* (eqn9.2.10 n x) (flsin theta))))))
    
    ;;; Equation 9.2.6 : For large x,
    ;;;
    ;;;     Y_n(x) = sqrt (2/(pi x)) [ P(n, x) sin theta + Q (n, x) cos theta ]
    ;;;
    ;;; where
    ;;;
    ;;;     theta = x - (n/2 + 1/4) pi
    ;;;
    ;;; and P(n, x) and Q(n, x) are defined by equations 9.2.9 and 9.2.10.
    
    (define (eqn9.2.6 n x)
      (let ((theta (fl- x (fl* (fl+ (/ n 2.0) 0.25) fl-pi))))
        (fl* (flsqrt (fl/ 2.0 (fl* fl-pi x)))
             (fl+ (fl* (eqn9.2.9 n x) (flsin theta))
                  (fl* (eqn9.2.10 n x) (flcos theta))))))
    
    (define (eqn9.2.9 n x) ; returns P(n, x)
      (define mu (fl* 4.0 (flsquare (inexact n))))
      (define (coefficients k2 p fact2k)
        (let ((c (fl/ p fact2k)))
          (if (fl>? k2 20.0) ; FIXME
              (list c)
              (cons c (coefficients (fl+ k2 2.0)
                                    (fl* p
                                         (fl- mu (flsquare (fl+ k2 1.0)))
                                         (fl- mu (flsquare (fl+ k2 3.0))))
                                    (fl* fact2k
                                         (fl+ k2 1.0)
                                         (fl+ k2 2.0)))))))
      (polynomial-at (fl- (fl/ (flsquare (fl* 8.0 x))))
                     (coefficients 0.0 1.0 1.0)))
    
    (define (eqn9.2.10 n x) ; returns Q(n, x)
      (define mu (fl* 4.0 (flsquare (inexact n))))
      (define (coefficients k2+1 p fact2k+1)
        (let ((c (fl/ p fact2k+1)))
          (if (fl>? k2+1 20.0) ; FIXME
              (list c)
              (cons c (coefficients (fl+ k2+1 2.0)
                                    (fl* p
                                         (fl- mu (flsquare (fl+ k2+1 2.0)))
                                         (fl- mu (flsquare (fl+ k2+1 4.0))))
                                    (fl* fact2k+1
                                         (fl+ k2+1 1.0)
                                         (fl+ k2+1 2.0)))))))
      (fl* (fl/ (fl* 8.0 x))
           (polynomial-at (fl- (fl/ (flsquare (fl* 8.0 x))))
                          (coefficients 1.0 (fl- mu 1.0) 1.0))))
    
    ;;; Equation 9.4.3 is a polynomial approximation attributed to
    ;;; E E Allen, Analytical approximations, Math. Tables Aids Comp. 8,
    ;;; 240-241 (1954), and Polynomial approximations to some modified
    ;;; Bessel functions, Math. Tables Aids Comp. 10, 162-164 (156)
    ;;; (with permission).
    ;;;
    ;;; This is commented out because Newman's similar polynomial
    ;;; approximation is simpler and has better error bounds.
    
    #;
    (define (eqn9.4.3 n x)
      (if (> n 0)
          (flfirst-bessel n x)
          (fl* (fl/ (flsqrt x))                              ; modulus
               (polynomial-at (fl/ 3.0 x)
                              '(+0.79788456
                                -0.00000077
                                -0.00552740
                                -0.00009512
                                +0.00137237
                                -0.00072805
                                +0.00014476))
               (flcos (fl+ x                                 ; phase
                           (polynomial-at (fl/ 3.0 x)
                                          '(-0.78539816
                                            -0.04166397
                                            -0.00003954
                                            +0.00262573
                                            -0.00054125
                                            -0.00029333
                                            +0.00013558)))))))
    
    ;;; J N Newman's polynomial approximation for x >= 3, from Table 4.
    
    #;
    (define (newman-table4 n x)
      (if (> n 0)
          (flfirst-bessel n x)
          (fl* (fl/ (flsqrt x))
               (polynomial-at (flsquare (fl/ 3.0 x))
                              '(+0.79788454
                                -0.00553897
                                +0.00099336
                                -0.00044346
                                +0.00020445
                                -0.00004959))
               (flcos (fl+ x
                           (fl- fl-pi/4)
                           (fl* (fl/ 3.0 x)
                                (polynomial-at (flsquare (fl/ 3.0 x))
                                               '(-0.04166592
                                                 +0.00239399
                                                 +0.00073984
                                                 -0.00031099
                                                 -0.00007605))))))))
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;
    ;;; Error functions
    ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (flerf x)
      (check-flonum! 'flerf x)
      (cond ((flnegative? x)
             (fl- (flerf (fl- x))))
            ((fl<? x 2.0)
             (eqn7.1.6 x))
            ((fl<? x +inf.0)
             (- 1.0 (eqn7.1.14 x)))
            ((fl=? x +inf.0)
             1.0)
            (else x)))
    
    (define (flerfc x)
      (check-flonum! 'flerfc x)
      (cond ((flnegative? x)
             (fl- 2.0 (flerfc (fl- x))))
            ((fl<? x 2.0)
             (eqn7.1.2 x))
            ((fl<? x +inf.0)
             (eqn7.1.14 x))
            ((fl=? x +inf.0)
             0.0)
            (else x)))
    
    ;;; Equation numbers are from Abramowitz and Stegun.
    
    ;;; If the step size is small enough for good accuracy,
    ;;; the integration is pretty slow.
    ;;;
    ;;; FIXME: not used at present, so I've commented it out.
    
    #;
    (define (eqn7.1.1 x)
      (fl* fl-2/sqrt-pi
           (definite-integral 0.0 x (lambda (t) (flexp (fl- (flsquare t)))))))
    
    (define (eqn7.1.2 x)
      (fl- 1.0 (flerf x)))
    
    ;;; Equation 7.1.6 :
    ;;;
    ;;;     erf x = (2 / sqrt(pi))
    ;;;             exp(-x^2)
    ;;;             \sum_{n=0}^\infty (2^n / (1 * 3 * ... * (2n+1))) x^(2n+1)
    ;;;
    ;;;           = (2 / sqrt(pi))
    ;;;             exp(-x^2)
    ;;;             x
    ;;;             \sum_{n=0}^\infty (2^n / (1 * 3 * ... * (2n+1))) (x^2)^n
    
    (define (eqn7.1.6 x)
      (let ((x^2 (flsquare x)))
        (fl* fl-2/sqrt-pi
             (flexp (fl- x^2))
             x
             (polynomial-at x^2 eqn7.1.6-coefficients))))
    
    (define eqn7.1.6-coefficients
      (let ()
        (define (loop n p)
          (if (> n 32) ; FIXME
              '()
              (let ((p (fl* p (inexact (+ (* 2 n) 1)))))
                (cons (fl/ (inexact (expt 2.0 n)) p)
                      (loop (+ n 1) p)))))
        (loop 0 1.0)))
    
    ;;; Equation 7.1.14 :
    ;;;
    ;;;     2 e^(x^2) \int_x^\infty e^(-t^2) dt
    ;;;   = 1 / (x + (1/2 / (x + (1 / (x + (3/2 / (x + (2 / (x + ...
    ;;;   = x (1/x) (1 / (x + (1/2 / (x + (1 / (x + (3/2 / (x + (2 / (x + ...
    ;;;   = x (1 / (x (x + (1/2 / (x + (1 / (x + (3/2 / (x + (2 / (x + ...
    ;;;   = x (1 / (x^2 + (1/2 / (1 + (1 / (x^2 + (3/2 / (1 + (2 / (x^2 + ...
    ;;;
    ;;;     erfc(x) = (2 / sqrt(pi)) \int_x^\infty e^(-t^2) dt
    ;;; so
    ;;;     erfc(x) = (1 / (sqrt(pi) e^(x^2)))
    ;;;                   times the continued fraction
    
    (define (eqn7.1.14 x)
      (define (continued-fraction x)
        (fl/ 1.0 (fl+ x (loop 1 0.5))))
      (define (loop k frac)
        (if (> k 70) ; FIXME
            1.0
            (fl/ frac (fl+ x (loop (+ k 1) (fl+ frac 0.5))))))
      (fl/ (continued-fraction x)
           (fl* (flsqrt fl-pi)
                (flexp (flsquare x)))))
  )
)
