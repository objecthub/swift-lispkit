;;; SRFI 194 RANDOM DATA GENERATORS
;;;
;;; This is the test suite for SRFI 194.
;;;
;;; Copyright © 2020 Arvydas Silanskas, John Cowan, Linas Vepštas. All rights reserved.
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
        (srfi 27)
        (srfi 133)
        (srfi 194)
        (srfi 158))

(define-syntax test-approximate
  (syntax-rules ()
    ((_ target value max-delta)
      (test-assert (and (<= value (+ target max-delta))
                        (>= value (- target max-delta)))))))

;; syntax just we can plop it at top and still allow internal `define`s
(define-syntax reset-source!
  (syntax-rules ()
    ((_)
     (define _ (random-source-pseudo-randomize! (current-random-source) 0 0)))))

(define (reset-source!*)
  (random-source-pseudo-randomize! (current-random-source) 0 0))

(define (assert-number-generator/all-in-range gen from to)
  (test-assert
    (generator-every
      (lambda (num)
        (and (>= num from)
             (< num to)))
      (gtake gen 1000))))

(define (assert-number-generator gen from to)
  (define range (- to from))
  (define lower-quarter (+ from (* 0.25 range)))
  (define upper-quarter (- to (* 0.25 range)))
  (assert-number-generator/all-in-range gen from to)

  (test-assert
    (generator-any
      (lambda (num)
        (and (>= num from)
             (< num lower-quarter)))
      (gtake gen 1000)))

  (test-assert
    (generator-any
      (lambda (num)
        (and (>= num lower-quarter)
             (< num upper-quarter)))
      (gtake gen 1000)))

  (test-assert
    (generator-any
      (lambda (num)
        (and (>= num upper-quarter)
             (< num to)))
      (gtake gen 1000))))

(define (assert-int-generator gen byte-size signed?)
  (define from (if signed?
                   (- (expt 2 (- byte-size 1)))
                   0))
  (define to (if signed?
                 (expt 2 (- byte-size 1))
                 (expt 2 byte-size)))
  (assert-number-generator gen from to))


(test-begin "SRFI 194: Random data generators")

(test-group "Test clamp real number"
  (reset-source!)
  (test-equal 10.0 (clamp-real-number 5.0 10.0 11))
  (test-equal 5.0 (clamp-real-number 5.0 10.0 2.0))
  (test-equal 7.5 (clamp-real-number 5.0 10.0 7.5)))

(test-group "Test with-random-source basic syntax"
  (reset-source!)
  (with-random-source default-random-source
                      (lambda () (make-random-integer-generator 0 10))))

(test-group "Test make-random-source-generator"
  (reset-source!)
  (define (make-numbers src-gen)
    (define gen1 (with-random-source (src-gen) (lambda () (make-random-integer-generator 0 100))))
    (define gen2 (with-random-source (src-gen) (lambda () (make-random-real-generator 0. 100.))))
    (generator->list
      (gappend
        (gtake gen1 10)
        (gtake gen2 10))))
  (test-equal
    (make-numbers (make-random-source-generator 0))
    (make-numbers (make-random-source-generator 0)))
  (test-assert
    (not (equal? (make-numbers (make-random-source-generator 0))
                 (make-numbers (make-random-source-generator 1))))))

(test-group "Test random int"
  (reset-source!)
  (assert-number-generator
    (make-random-integer-generator 1 100)
    1 100)
 
  (for-each
    (lambda (testcase)
      (define make-gen (car testcase))
      (define byte-size (cadr testcase))
      (define signed? (caddr testcase))
      (assert-int-generator (make-gen) byte-size signed?))
    (list
      (list make-random-u8-generator 8 #f)
      (list make-random-s8-generator 8 #t)
      (list make-random-u16-generator 16 #f)
      (list make-random-s16-generator 16 #t)
      (list make-random-u32-generator 32 #f)
      (list make-random-s32-generator 32 #t)
      (list make-random-u64-generator 64 #f)
      (list make-random-s64-generator 64 #t)))
 
  ;;test u1 separately, since it will fail quarter checks due to small range
  (assert-number-generator/all-in-range (make-random-u1-generator) 0 2)
  (test-assert
    (generator-any
      (lambda (v) (= v 0))
      (gtake (make-random-u1-generator) 100)))
  (test-assert
    (generator-any
      (lambda (v) (= v 1))
      (gtake (make-random-u1-generator) 100))))

(test-group "Test random real"
  (reset-source!)
  (assert-number-generator
    (make-random-real-generator 1.0 5.0)
    1.0 5.0)
  (test-assert
    (generator-any
      (lambda (v)
        (not (= v (floor v))))
      (make-random-real-generator 1.0 5.0))))

(test-group "Test complex rectangular"
  (reset-source!)
  (assert-number-generator
    (gmap
      real-part
      (make-random-rectangular-generator -10.0 10.0 -100.0 100.0))
    -10 10)
  (assert-number-generator
    (gmap
      imag-part
      (make-random-rectangular-generator -100.0 100.0 -10.0 10.0))
    -10 10)
  (test-assert
    (generator-any
      (lambda (num)
        (and (not (= 0 (real-part num)))
             (not (= 0 (imag-part num)))))
      (make-random-rectangular-generator -10.0 10.0 -10.0 10.0))))

(test-group "Test complex polar"
  (reset-source!)
  (define PI (* 4 (atan 1.0)))
  (define (test-polar g origin mag-from mag-to angle-from angle-to test-converge-origin)
    (assert-number-generator
      (gmap
        (lambda (num)
          (angle (- num origin)))
        g)
      angle-from angle-to)

    (assert-number-generator
      (gmap
        (lambda (num)
          (magnitude (- num origin)))
        g)
      mag-from mag-to)
    ;; split generated area through circle at 0.5*(mag-from + mag-to)
    ;; and validate generated points in them proportional to their area
    (let* ((outter-count 0)
           (inner-count 0)
           (donut-area (lambda (r1 r2) (- (* r1 r1) (* r2 r2))))
           (mag-mid (/ (+ mag-from mag-to) 2.))
           (expected-fraction (/ (donut-area mag-to mag-mid)
                                 (donut-area mag-mid mag-from))))
      (generator-for-each
        (lambda (point)
          (if (< (magnitude (- point origin)) mag-mid)
              (set! inner-count (+ 1 inner-count))
              (set! outter-count (+ 1 outter-count))))
        (gtake g 10000))
      (test-approximate expected-fraction (/ outter-count inner-count) 0.2))
    ;; test points converge to center
    (when test-converge-origin
      (let ((sum 0+0i))
        (generator-for-each
          (lambda (point) (set! sum (+ point sum)))
          (gtake g 1000))
        (test-approximate (real-part origin) (real-part (/ sum 1000.)) 0.1)
        (test-approximate (imag-part origin) (imag-part (/ sum 1000.)) 0.1))))
  (test-polar (make-random-polar-generator 0. 1.)
              0+0i 0. 1. (- PI) PI #t)
  (test-polar (make-random-polar-generator 2+5i 1. 2.)
              2+5i 1. 2. (- PI) PI #t)
  (test-polar (make-random-polar-generator 1. 2. -1. 1.)
              0+0i 1. 2. -1. 1. #f)
  (test-polar (make-random-polar-generator -1+3i 0. 2. (- PI) PI)
              -1+3i 0. 2. (- PI) PI #t))

(test-group "Test random bool"
  (reset-source!)
  (test-assert
    (generator-every
      (lambda (v)
        (or (eq? v #t)
            (eq? v #f)))
      (gtake (make-random-boolean-generator) 10000)))
  (test-assert
    (generator-any
      (lambda (v)
        (eq? #t v))
      (make-random-boolean-generator)))
  (test-assert
    (generator-any
      (lambda (v)
        (eq? #f v))
      (make-random-boolean-generator))))

(test-group "Test random char"
  (reset-source!)
  (test-assert
    (generator-every
      (lambda (v)
        (or (equal? v #\a)
            (equal? v #\b)))
      (gtake (make-random-char-generator "ab")
             10000)))
  (test-assert
    (generator-any
      (lambda (v)
        (equal? v #\a))
      (make-random-char-generator "ab")))
  (test-assert
    (generator-any
      (lambda (v)
        (equal? v #\b))
      (make-random-char-generator "ab"))))

(test-group "Test random string"
  (reset-source!)
  (test-assert
    (generator-every
      (lambda (str)
        (and (< (string-length str) 5)
             (every (lambda (c)
                      (or (equal? c #\a)
                          (equal? c #\b)))
                    (string->list str))))
      (gtake (make-random-string-generator 5 "ab")
             10000)))
  (test-assert
    (generator-any
      (lambda (str)
        (equal? "abb" str))
      (make-random-string-generator 4 "ab"))))

(test-group "Test Bernoulli"
  (reset-source!)
  (define g (make-bernoulli-generator 0.7))
  (define expect 7000)
  (define actual (generator-count
                   (lambda (i) (= i 1))
                   (gtake g 10000)))
  (define ratio (inexact (/ actual expect)))
  (test-assert (> ratio 0.9))
  (test-assert (< ratio 1.1)))

(test-group "Test categorical"
  (reset-source!)
  (define result-vec (vector 0 0 0))
  (define expect-vec (vector 2000 5000 3000))
  (define wvec (vector 20 50 30))
  (define g (make-categorical-generator wvec))
  (generator-for-each
    (lambda (i)
      (vector-set! result-vec i (+ 1 (vector-ref result-vec i))))
    (gtake g 10000))
  (vector-for-each
    (lambda (result expect)
      (define ratio (inexact (/ result expect)))
      (test-approximate 1.0 ratio 0.1))
    result-vec
    expect-vec))

(test-group "Test poisson"
  (reset-source!)
  ;;TODO import from somewhere?
  (define (fact k)
    (cond
      ((<= k 1) 1)
      (else (* k (fact (- k 1))))))
  (define (expected-fraction L k)
    (/ (* (exact (expt L k)) (exact (exp (- L))))
       (fact k)))

  (define (test-poisson L poisson-gen test-points)
    (generator-every
      (lambda (k)
        (define expect (expected-fraction L k))
        (define actual (/ (generator-count
                            (lambda (i) (= i k))
                            (gtake poisson-gen 10000))
                          10000))
        (define ratio (/ actual expect))
        (test-assert (> ratio 8/10))
        (test-assert (< ratio 12/10)))
      (list->generator test-points)))

  (test-poisson 2 (make-poisson-generator 2) '(1 2 3))
  (test-poisson 40 (make-poisson-generator 40) '(30 40 50))  
  ; (test-poisson 280 (make-poisson-generator 280) '(260 280 300))
)

(test-group "Test normal"
  (reset-source!)
  (define frac-at-1dev 0.34134)
  (define frac-at-2dev 0.47725)
  (define frac-at-3dev 0.49865)
  (define (test-normal-at-point gen count-from count-to expected-fraction)
    (define actual (/ (generator-count
                        (lambda (n)
                          (and (>= n count-from)
                               (< n count-to)))
                        (gtake gen 10000))
                      10000.0))
    (test-assert (and (> actual (* 0.9 expected-fraction))
                      (< actual (* 1.1 expected-fraction)))))

  (define (test-normal gen mean deviation)
    (test-normal-at-point gen mean (+ mean deviation) frac-at-1dev)
    (test-normal-at-point gen mean (+ mean (* 2 deviation)) frac-at-2dev)
    (test-normal-at-point gen mean (+ mean (* 3 deviation)) frac-at-3dev))
  (test-normal (make-normal-generator) 0.0 1.0)
  (test-normal (make-normal-generator 1.0) 1.0 1.0)
  (test-normal (make-normal-generator 1.0 2.0) 1.0 2.0))

(test-group "Test exponential"
  (reset-source!)
  (define (expected-fraction mean x)
    (- 1 (exp (* (- (/ 1.0 mean)) x))))
  (define (test-exp-at-point gen count-to expected)
    (define actual (/ (generator-count
                        (lambda (n)
                          (< n count-to))
                        (gtake gen 10000))
                      10000.0))
    (test-assert (> actual (* 0.9 expected)))
    (test-assert (< actual (* 1.1 expected))))
  (define (test-exp gen mean)
    (test-exp-at-point gen 1 (expected-fraction mean 1))
    (test-exp-at-point gen 2 (expected-fraction mean 2))
    (test-exp-at-point gen 3 (expected-fraction mean 3)))
  (test-exp (make-exponential-generator 1) 1)
  (test-exp (make-exponential-generator 1.5) 1.5))

(test-group "Test geometric"
  (reset-source!)
  (define (expected-fraction p x)
    (* (expt (- 1 p) (- x 1)) p))
  (define (test-geom-at-point gen p x)
    (define expected (expected-fraction p x))
    (define actual (/ (generator-count
                        (lambda (n)
                          (= n x))
                        (gtake gen 100000))
                      100000))
    (define ratio (/ actual expected))
    (test-assert (> ratio 0.9))
    (test-assert (< ratio 1.1)))
  (define (test-geom gen p)
    (test-geom-at-point gen p 1)
    (test-geom-at-point gen p 3)
    (test-geom-at-point gen p 5))
  (test-geom (make-geometric-generator 0.5) 0.5))

(test-group "Test uniform sampling"
  (reset-source!)
  (test-equal
    '()
    (generator->list (gsampling)))
  (test-equal
    '()
    (generator->list (gsampling (generator) (generator))))
  (test-equal
    '(1 1 1)
    (generator->list (gsampling (generator) (generator 1 1 1))))
  (test-assert
    (generator-any
      (lambda (el)
        (= el 1))
      (gsampling (circular-generator 1) (circular-generator 2))))
  (test-assert
    (generator-any
      (lambda (el)
        (= el 2))
      (gsampling (circular-generator 1) (circular-generator 2)))))

(test-group "Test Zipf sampling"
  (reset-source!)
  
  ; Simple test harness for exploring paramter space.
  ;
  ; Take REPS samples from the zeta distribution (ZGEN NVOCAB ESS QUE)
  ; Accumulate them into NVOCAB bins.
  ; Normalize to unit probability (i.e. divide by NVOCAB)
  ;
  ; The resulting distribution should uniformly converge to C/(k+q)^s
  ; for 1 <= k <= NVOCAB where C is a normalization constant.
  ;
  ; This compares the actual distribution to the expected convergent
  ; and reports an error if it is not within TOL of the convergent.
  ; i.e. it computes the Banach l_0 norm of (distribution-convergent)
  ; TOL is to be given in units of standard deviations. So, for example,
  ; setting TOL to 6 gives a six-sigma bandpass, allowsing the tests to
  ; usually pass.
  
  (define (test-zipf ZGEN NVOCAB ESS QUE REPS TOL)
    ; Bin-counter containing accumulated histogram.
    (define bin-counts
      (let ((bin-counts (make-vector NVOCAB 0)))
       ; Accumulate samples into the histogram.
       (generator-for-each
         (lambda (SAMP)
           (define offset (- SAMP 1))
           (vector-set! bin-counts offset (+ 1 (vector-ref bin-counts offset))))
         (gtake (ZGEN NVOCAB ESS QUE) REPS))
       bin-counts))
    ; Verify the distribution is within tolerance.
    ; This is written out long-hand for easier debuggability.
    ; Frequency is normalized to be 0.0 to 1.0
    (define frequency (vector-map (lambda (n) (/ n REPS)) bin-counts))
    (define probility (vector-map (lambda (n) (inexact n)) frequency))
    ; Sequence 1..NVOCAB
    (define seq (vector-unfold (lambda (i x) (values x (+ x 1))) NVOCAB 1))
    ; Sequence  1/(k+QUE)^ESS
    (define inv-pow (vector-map (lambda (k) (expt (+ k QUE) (- (inexact ESS)))) seq))
    ; Hurwicz harmonic number sum_1..NVOCAB 1/(k+QUE)^ESS
    (define hnorm (vector-fold (lambda (sum cnt) (+ sum cnt)) 0 inv-pow))
    ; The expected distribution
    (define expect (vector-map (lambda (x) (/ x hnorm)) inv-pow))
    ; Convert to floating point.
    (define prexpect (vector-map (lambda (x) (inexact x)) expect))
    ; The difference
    (define diff (vector-map (lambda (x y) (- x y)) probility prexpect))
    ; Re-weight the tail by k^{s/2}. This seems give a normal error
    ; distribution. ... at least, for small q. Problems for large q
    ; and with undersampling; so we hack around that.
    (define err-dist
      (if (< 10 QUE) diff
          (vector-map (lambda (i x) (* x (expt (+ i 1) (* 0.5 ESS))))
                      (list->vector (iota (vector-length diff)))
                      diff)))
    ; Normalize to unit root-mean-square.
    (define rms (/ 1 (sqrt (* 2 3.141592653 REPS))))
    (define norm-dist (vector-map (lambda (x) (/ x rms)) err-dist))
    ; Maximum deviation from expected distribution (l_0 norm)
    (define l0-norm (vector-fold (lambda (sum x) (if (< sum (abs x)) (abs x) sum)) 0 norm-dist))
    ; The mean
    (define mean (/ (vector-fold (lambda (sum x) (+ sum x)) 0 norm-dist) NVOCAB))
    (define root-mean-square (sqrt (/ (vector-fold (lambda (sum x) (+ sum (* x x))) 0 norm-dist)
                                      NVOCAB)))
    ; The total counts in the bins should be equal to REPS
    (test-assert
      (equal? REPS
              (vector-fold
                (lambda (sum cnt) (+ sum cnt)) 0 bin-counts)))
    ; Test for uniform convergence
    (test-assert (<= l0-norm TOL))
    ; Should not random walk too far away.
    ; Could tighten this with a proper theory of the error distribution.
    (test-assert (< (abs mean) 3))
    ; I don't understand the error distribution ....
    ; (test-assert (and (< 0.4 root-mean-square) (< root-mean-square 1.5)))
    (test-assert (<= l0-norm TOL))
    #f)

  ; The unit test is computes something that is "almost" a standard deviation for the error
  ; distribution. Except, maybe not quite, I don't fully understand the theory. So most tests
  ; seem to come in fine in well-under a six-sigma deviation, but some of the wilder paramter
  ; choices misbehave, so six-sigma doesn't always work. Also, when the number of bins is large,
  ; its easy to under-sample, some bins end up empty and the std-dev is thrown off as a result.
  ; Thus, the tolerance bounds below are hand-adjusted.
  (define six-sigma 6.0)
  (define hack-que 3.0)
  ; Zoom into s->1
  (test-zipf make-zipf-generator 30 1.1     0 1000 six-sigma)
  (test-zipf make-zipf-generator 30 1.01    0 1000 six-sigma)
  (test-zipf make-zipf-generator 30 1.001   0 1000 six-sigma)
  (test-zipf make-zipf-generator 30 1.0001  0 1000 six-sigma)
  ; (test-zipf make-zipf-generator 30 1.00001 0 1000 six-sigma)
  (test-zipf make-zipf-generator 30 (+ 1 1e-6)  0 1000 six-sigma)
  (test-zipf make-zipf-generator 30 (+ 1 1e-8)  0 1000 six-sigma)
  (test-zipf make-zipf-generator 30 (+ 1 1e-10) 0 1000 six-sigma)
 (test-zipf make-zipf-generator 30 (+ 1 1e-12) 0 1000 six-sigma)
  (test-zipf make-zipf-generator 30 (+ 1 1e-14) 0 1000 six-sigma)
  ; (test-zipf make-zipf-generator 30 1           0 1000 six-sigma)
  ; Verify improving uniform convergence
  (test-zipf make-zipf-generator 30 1  0 10000   six-sigma)
  (test-zipf make-zipf-generator 30 1  0 100000  six-sigma)
  ; Larger vocabulary
  (test-zipf make-zipf-generator 300 1.1     0 10000 six-sigma)
  (test-zipf make-zipf-generator 300 1.01    0 10000 six-sigma)
  (test-zipf make-zipf-generator 300 1.001   0 10000 six-sigma)
  (test-zipf make-zipf-generator 300 1.0001  0 10000 six-sigma)
  ; (test-zipf make-zipf-generator 300 1.00001 0 10000 six-sigma)
  ; Larger vocabulary. Take more samples....
  ; (test-zipf make-zipf-generator 3701 1.1     0 40000 six-sigma)
  (test-zipf make-zipf-generator 3701 1.01    0 40000 six-sigma)
  (test-zipf make-zipf-generator 3701 1.001   0 40000 six-sigma)
  (test-zipf make-zipf-generator 3701 1.0001  0 40000 six-sigma)
  ; (test-zipf make-zipf-generator 3701 1.00001 0 40000 six-sigma)
  ; Large s, small range
  (test-zipf make-zipf-generator 5 1.1     0 1000 six-sigma)
  (test-zipf make-zipf-generator 5 2.01    0 1000 six-sigma)
  ; (test-zipf make-zipf-generator 5 4.731   0 1000 six-sigma)
  (test-zipf make-zipf-generator 5 9.09001 0 1000 six-sigma)
  (test-zipf make-zipf-generator 5 13.45   0 1000 8.0)
  ; Large s, larger range. Most histogram bins will be empty
  ; so allow much larger error margins
  (test-zipf make-zipf-generator 130 1.5     0 30000 six-sigma)
  (test-zipf make-zipf-generator 130 2.03    0 30000 9.0)
  (test-zipf make-zipf-generator 130 4.5     0 30000 16.0)
  (test-zipf make-zipf-generator 130 6.66    0 30000 24.0)
  ; Verify that accuracy improves with more samples
  (test-zipf make-zipf-generator 129 1.1     0 10000 six-sigma)
  (test-zipf make-zipf-generator 129 1.01    0 10000 six-sigma)
  (test-zipf make-zipf-generator 129 1.001   0 10000 six-sigma)
  (test-zipf make-zipf-generator 129 1.0001  0 10000 six-sigma)
  ; (test-zipf make-zipf-generator 129 1.00001 0 10000 six-sigma)
  ; Non-zero Hurwicz parameter
  (test-zipf make-zipf-generator 131 1.1     0.3    10000 six-sigma)
  (test-zipf make-zipf-generator 131 1.1     1.3    10000 six-sigma)
  (test-zipf make-zipf-generator 131 1.1     6.3    10000 six-sigma)
  (test-zipf make-zipf-generator 131 1.1     20.23  10000 six-sigma)
  ; Negative Hurwicz parameter. Must be greater than branch point at -0.5
  (test-zipf make-zipf-generator 81 1.1     -0.1   1000 six-sigma)
  (test-zipf make-zipf-generator 81 1.1     -0.3   1000 six-sigma)
  (test-zipf make-zipf-generator 81 1.1     -0.4   1000 six-sigma)
  (test-zipf make-zipf-generator 81 1.1     -0.499 1000 six-sigma)
  ; A walk into a stranger corner of the parameter space
  (test-zipf make-zipf-generator 131 1.1     41.483 10000 hack-que)
  (test-zipf make-zipf-generator 131 2.1     41.483 10000 hack-que)
  (test-zipf make-zipf-generator 131 6.1     41.483 10000 hack-que)
  (test-zipf make-zipf-generator 131 16.1    41.483 10000 hack-que)
  (test-zipf make-zipf-generator 131 46.1    41.483 10000 hack-que)
  (test-zipf make-zipf-generator 131 96.1    41.483 10000 hack-que)
  ; A still wilder corner of the parameter space
  (test-zipf make-zipf-generator 131 1.1     1841.4 10000 hack-que)
  (test-zipf make-zipf-generator 131 1.1     1.75e6 10000 hack-que)
  (test-zipf make-zipf-generator 131 2.1     1.75e6 10000 hack-que)
  (test-zipf make-zipf-generator 131 12.1    1.75e6 10000 hack-que)
  (test-zipf make-zipf-generator 131 42.1    1.75e6 10000 hack-que)
  ; Lets try s less than 1
  (test-zipf make-zipf-generator 35 0.9     0 1000 six-sigma)
  (test-zipf make-zipf-generator 35 0.99    0 1000 six-sigma)
  (test-zipf make-zipf-generator 35 0.999   0 1000 six-sigma)
  (test-zipf make-zipf-generator 35 0.9999  0 1000 six-sigma)
  (test-zipf make-zipf-generator 35 0.99999 0 1000 six-sigma)
  ; Attempt to force an overflow
  (test-zipf make-zipf-generator 437 (- 1 1e-6)  0 1000 six-sigma)
  (test-zipf make-zipf-generator 437 (- 1 1e-7)  0 1000 six-sigma)
  (test-zipf make-zipf-generator 437 (- 1 1e-9)  0 1000 six-sigma)
  (test-zipf make-zipf-generator 437 (- 1 1e-12) 0 1000 six-sigma)
  ; Almost flat distribution
  (test-zipf make-zipf-generator 36 0.8     0 1000 six-sigma)
  (test-zipf make-zipf-generator 36 0.5     0 1000 six-sigma)
  (test-zipf make-zipf-generator 36 0.1     0 1000 six-sigma)
  ; A visit to crazy-town -- increasing, not decreasing exponent
  (test-zipf make-zipf-generator 36 0.0     0 1000 six-sigma)
  (test-zipf make-zipf-generator 36 -0.1    0 1000 six-sigma)
  (test-zipf make-zipf-generator 36 -1.0    0 1000 six-sigma)
  (test-zipf make-zipf-generator 36 -3.0    0 1000 six-sigma)
  ; More crazy with some Hurwicz on top
  (test-zipf make-zipf-generator 16 0.0     0.5 1000 six-sigma)
  (test-zipf make-zipf-generator 16 -0.2    2.5 1000 six-sigma)
  (test-zipf make-zipf-generator 16 -1.3    10  1000 six-sigma)
  (test-zipf make-zipf-generator 16 -2.9    100 1000 six-sigma)
)

(test-group "Test binomial"
  (reset-source!)
  (define (factorial n)
    (if (<= n 1) 1 (* n (factorial (- n 1)))))
  (define (C n k)
    (/ (factorial n)
       (* (factorial k) (factorial (- n k)))))
  (define (expected-frac n p k)
    (* (C n k) (expt p k) (expt (- 1 p) (- n k))))
  (define (test-binomial n p count)
    (define g (make-binomial-generator n p))
    (define counts (make-vector (+ n 1) 0))
    (generator-for-each
      (lambda (x)
        (vector-set! counts x (+ 1 (vector-ref counts x))))
      (gtake g count))
    (for-each
      (lambda (k)
        (define expected (* count (expected-frac n p k) ))
        (define actual (vector-ref counts k))
        (cond
          ((= expected 0)
           (test-equal 0 actual))
          ;; hacky.. testing values with very low probability fails
          ((> expected (* 1/1000 count))
           (test-approximate 1.0 (/ actual expected) 0.2))))
      (iota (+ n 1))))
  (test-binomial 1 0 100)
  (test-binomial 1 1 100)
  (test-binomial 1 0. 100)
  (test-binomial 1 1. 100)
  (test-binomial 10 0 100)
  (test-binomial 10 1 100)
  (test-binomial 10 0. 100)
  (test-binomial 10 1. 100)
  (test-binomial 10 0.25 100000)
  ; (test-binomial 40 0.375 1000000)
)

(test-end)

