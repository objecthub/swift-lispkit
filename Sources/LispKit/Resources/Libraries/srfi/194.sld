;;; SRFI 194
;;; Random data generators
;;;
;;; This SRFI defines a set of SRFI 158 generators and generator makers that yield random data
;;; of specific ranges and distributions. It is intended to be implemented on top of SRFI 27,
;;; which provides the underlying source of random integers and floats.
;;; 
;;; Copyright © 2020 Shiro Kawai, Arvydas Silanskas, Linas Vepštas, John Cowan.
;;; All rights reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software, and to permit
;;; persons to whom the Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies or
;;; substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
;;; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
;;; FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2020 Matthias Zenger. All rights reserved.

(define-library (srfi 194)
  
  (export clamp-real-number
          current-random-source
          with-random-source
          make-random-integer-generator
          make-random-u1-generator
          make-random-u8-generator
          make-random-s8-generator
          make-random-u16-generator
          make-random-s16-generator
          make-random-u32-generator
          make-random-s32-generator
          make-random-u64-generator
          make-random-s64-generator
          make-random-boolean-generator
          make-random-char-generator
          make-random-string-generator
          make-random-real-generator
          make-random-rectangular-generator
          make-random-polar-generator
          make-bernoulli-generator
          make-binomial-generator
          make-categorical-generator
          make-normal-generator
          make-exponential-generator
          make-geometric-generator
          make-poisson-generator
          make-zipf-generator
          make-sphere-generator
          make-ellipsoid-generator
          make-ball-generator
          make-random-source-generator
          gsampling)

  (import (except (lispkit base) vector-map!)
          (srfi 27)
          (srfi 133)
          (srfi 158))

  (begin
    
    ;;
    ;; Parameters
    ;;
    (define current-random-source (make-parameter default-random-source))
    
    (define (with-random-source random-source thunk)
      (unless (random-source? random-source)
        (error "expected random source"))
      (parameterize ((current-random-source random-source))
                    (thunk)))
    
    ;;
    ;; Carefully return consecutive substreams of the s'th
    ;; SRFI 27 stream of random numbers.  See Sections 1.2 and
    ;; 1.3 of "An object-oriented random-number package with many
    ;; long streams and substreams", by Pierre L'Ecuyer, Richard
    ;; Simard, E. Jack Chen, and W. David Kelton, Operations Research,
    ;; vol. 50 (2002), pages 1073-1075.
    ;; https://doi.org/10.1287/opre.50.6.1073.358
    ;;
    
    (define (make-random-source-generator s)
      (if (not (and (exact? s)
                    (integer? s)
                    (not (negative? s))))
          (error "make-random-source-generator: Expect nonnegative exact integer argument: " s)
          (let ((substream 0))
            (lambda ()
              (let ((new-source (make-random-source))) ;; deterministic
                (random-source-pseudo-randomize! new-source s substream)
                (set! substream (+ substream 1))
                new-source)))))
    
    ;;
    ;; Primitive randoms
    ;;
    
    (define (make-random-integer-generator low-bound up-bound)
      (unless (and (integer? low-bound)
                   (exact? low-bound))
        (error "expected exact integer for lower bound"))
      (unless (and (integer? up-bound)
                   (exact? up-bound))
        (error "expected exact integer for upper bound"))
      (unless (< low-bound up-bound)
        (error "upper bound should be greater than lower bound"))
      (let ((rand-int-proc (random-source-make-integers (current-random-source)))
            (range (- up-bound low-bound)))
        (lambda ()
          (+ low-bound (rand-int-proc range)))))
    
    (define (make-random-u1-generator)
      (make-random-integer-generator 0 2))
    (define (make-random-u8-generator)
      (make-random-integer-generator 0 256))
    (define (make-random-s8-generator)
      (make-random-integer-generator -128 128))
    (define (make-random-u16-generator)
      (make-random-integer-generator 0 65536))
    (define (make-random-s16-generator)
      (make-random-integer-generator -32768 32768))
    (define (make-random-u32-generator)
      (make-random-integer-generator 0 (expt 2 32)))
    (define (make-random-s32-generator)
      (make-random-integer-generator (- (expt 2 31)) (expt 2 31)))
    (define (make-random-u64-generator)
      (make-random-integer-generator 0 (expt 2 64)))
    (define (make-random-s64-generator)
      (make-random-integer-generator (- (expt 2 63)) (expt 2 63)))
    
    (define (clamp-real-number lower-bound upper-bound value)
      (cond ((not (real? lower-bound))
             (error "expected real number for lower bound"))
            ((not (real? upper-bound))
             (error "expected real number for upper bound"))
            ((not (<= lower-bound upper-bound))
             (error "lower bound must be <= upper bound"))
            ((< value lower-bound) lower-bound)
            ((> value upper-bound) upper-bound)
            (else value)))
    
    (define (make-random-real-generator low-bound up-bound)
      (unless (and (real? low-bound)
                   (finite? low-bound))
        (error "expected finite real number for lower bound"))
      (unless (and (real? up-bound)
                   (finite? up-bound))
         (error "expected finite real number for upper bound"))
      (unless (< low-bound up-bound)
         (error "lower bound must be < upper bound"))
      (let ((rand-real-proc (random-source-make-reals (current-random-source))))
       (lambda ()
         (define t (rand-real-proc))
         ;; alternative way of doing lowbound + t * (up-bound - low-bound)
         ;; is susceptible to rounding errors and would require clamping to be safe
         ;; (which in turn requires 144 for adjacent float function)
         (+ (* t low-bound)
            (* (- 1.0 t) up-bound)))))
    
    (define (make-random-rectangular-generator
              real-lower-bound real-upper-bound
              imag-lower-bound imag-upper-bound)
      (let ((real-gen (make-random-real-generator real-lower-bound real-upper-bound))
            (imag-gen (make-random-real-generator imag-lower-bound imag-upper-bound)))
        (lambda ()
          (make-rectangular (real-gen) (imag-gen)))))
    
    (define make-random-polar-generator
      (case-lambda
        ((magnitude-lower-bound magnitude-upper-bound)
         (make-random-polar-generator 0+0i magnitude-lower-bound magnitude-upper-bound 0 (* 2 PI)))
        ((origin magnitude-lower-bound magnitude-upper-bound)
         (make-random-polar-generator origin magnitude-lower-bound
           magnitude-upper-bound 0 (* 2 PI)))
        ((magnitude-lower-bound magnitude-upper-bound angle-lower-bound angle-upper-bound)
         (make-random-polar-generator 0+0i magnitude-lower-bound magnitude-upper-bound
           angle-lower-bound angle-upper-bound))
        ((origin magnitude-lower-bound magnitude-upper-bound angle-lower-bound angle-upper-bound)
         (unless (complex? origin)
           (error "origin should be complex number"))
         (unless (and (real? magnitude-lower-bound)
                      (real? magnitude-upper-bound)
                      (real? angle-lower-bound)
                      (real? angle-upper-bound))
           (error "magnitude and angle bounds should be real numbers"))
         (unless (and (<= 0 magnitude-lower-bound)
                      (<= 0 magnitude-upper-bound))
           (error "magnitude bounds should be positive"))
         (unless (< magnitude-lower-bound magnitude-upper-bound)
           (error "magnitude lower bound should be less than upper bound"))
         (when (= angle-lower-bound angle-upper-bound)
           (error "angle bounds shouldn't be equal"))
         (let* ((b (square magnitude-lower-bound))
                (m (- (square magnitude-upper-bound) b))
                (t-gen (make-random-real-generator 0. 1.))
                (phi-gen (make-random-real-generator angle-lower-bound angle-upper-bound)))
           (lambda ()
             (let* ((t (t-gen))
                    (phi (phi-gen))
                    (r (sqrt (+ (* m t) b))))
              (+ origin (make-polar r phi))))))))
    
    (define (make-random-boolean-generator)
      (define u1 (make-random-u1-generator))
      (lambda ()
        (zero? (u1))))
    
    (define (make-random-char-generator str)
      (when (not (string? str))
        (error "expected string"))
      (unless (> (string-length str) 0)
        (error "given string is of length 0"))
      (let* ((int-gen (make-random-integer-generator 0 (string-length str))))
       (lambda ()
         (string-ref str (int-gen)))))
    
    (define (make-random-string-generator k str)
      (let ((char-gen (make-random-char-generator str))
            (int-gen (make-random-integer-generator 0 k)))
        (lambda ()
          (generator->string char-gen (int-gen)))))
    
    ;;
    ;; Non-uniform distributions
    ;;
    
    (define PI (* 4 (atan 1.0)))
    
    (define (make-bernoulli-generator p)
      (unless (real? p)
        (error "expected p to be real"))
      (unless (<= 0 p 1)
        (error "expected 0 <= p <= 1"))
      (let ((rand-real-proc (random-source-make-reals (current-random-source))))
       (lambda ()
         (if (<= (rand-real-proc) p)
             1
             0))))
    
    (define (make-categorical-generator weights-vec)
      (define weight-sum
        (vector-fold
          (lambda (sum p)
            (unless (and (number? p)
                         (> p 0))
              (error "parameter must be a vector of positive numbers"))
            (+ sum p))
          0
          weights-vec))
      (define length (vector-length weights-vec))
      (let ((real-gen (make-random-real-generator 0 weight-sum)))
       (lambda ()
         (define roll (real-gen))
         (let it ((sum 0)
                  (i 0))
           (define newsum (+ sum (vector-ref weights-vec i)))
           (if (or (< roll newsum)
                   ;; in case of rounding errors and no matches, return last element
                   (= i (- length 1)))
               i
               (it newsum
                   (+ i 1)))))))
    
    ;; Normal distribution (continuous - generates real numbers)
    ;; Box-Muller algorithm
    ;; NB: We tested Ziggurat method, too,
    ;; only to find out Box-Muller is faster about 12% - presumably
    ;; the overhead of each ops is larger in Gauche than C/C++, and
    ;; so the difference of cost of log or sin from the primitive
    ;; addition/multiplication are negligible.
    
    ;; NOTE: this implementation is not thread safe
    (define make-normal-generator
      (case-lambda
        (()
         (make-normal-generator 0.0 1.0))
        ((mean)
         (make-normal-generator mean 1.0))
        ((mean deviation)
         (let ((rand-real-proc (random-source-make-reals (current-random-source)))
               (state #f))
           (unless (and (real? mean)
                        (finite? mean))
             (error "expected mean to be finite real number"))
           (unless (and (real? deviation)
                        (finite? deviation)
                        (> deviation 0))
             (error "expected deviation to be positive finite real number"))
           (lambda ()
             (if state
                 (let ((result state))
                  (set! state #f)
                  result)
                 (let ((r (sqrt (* -2 (log (rand-real-proc)))))
                       (theta (* 2 PI (rand-real-proc))))
                   (set! state (+ mean (* deviation r (cos theta))))
                   (+ mean (* deviation r (sin theta))))))))))
    
    (define (make-exponential-generator mean)
      (unless (and (real? mean)
                   (finite? mean)
                   (positive? mean))
        (error "expected mean to be finite positive real number"))
      (let ((rand-real-proc (random-source-make-reals (current-random-source))))
       (lambda ()
         (- (* mean (log (rand-real-proc)))))))
    
    (define (make-geometric-generator p)
    
      (define (log1p x)
        ;; Adapted from SRFI 144
        (let ((u (+ 1.0 x)))
          (cond ((= u 1.0)
                 x) ;; gets sign of zero result correct
                ((= u x)
                 (log u)) ;; large arguments and infinities
                (else
                 (* (log u) (/ x (- u 1.0)))))))
    
      (unless (and (real? p)
                   (> p 0)
                   (<= p 1))
              (error "expected p to be real number, 0 < p <= 1"))
      (if (zero? (- p 1.))
          ;; p is indistinguishable from 1.
          (lambda () 1)
          (let ((c (/ (log1p (- p))))
                (rand-real-proc (random-source-make-reals (current-random-source))))
            (lambda ()
              (exact (ceiling (* c (log (rand-real-proc)))))))))
    
    ;; Draw from poisson distribution with mean L, variance L.
    ;; For small L, we use Knuth's method.  For larger L, we use rejection
    ;; method by Atkinson, The Computer Generation of Poisson Random Variables,
    ;; J. of the Royal Statistical Society Series C (Applied Statistics), 28(1),
    ;; pp29-35, 1979.  The code here is a port by John D Cook's C++ implementation
    ;; (http://www.johndcook.com/stand_alone_code.html )
    
    ;; NOTE: this implementation calculates and stores a table of log(n!) on first invocation
    ;; of L >= 36 and therefore is not entirely thread safe (should still produce correct result,
    ;; but with performance hit if table is recalculated multiple times)
    (define (make-poisson-generator L)
      (unless (and (real? L)
                   (finite? L)
                   (> L 0))
        (error "expected L to be finite positive real number"))
      (let ((rand-real-proc (random-source-make-reals (current-random-source))))
       (if (< L 30)
           (make-poisson/small rand-real-proc L)
           (make-poisson/large rand-real-proc L))))
    
    ;; private
    (define (make-poisson/small rand-real-proc L)
      (lambda ()
        (do ((exp-L (exp (- L)))
             (k 0 (+ k 1))
             (p 1.0 (* p (rand-real-proc))))
            ((<= p exp-L) (- k 1)))))
    
    ;; private
    (define (make-poisson/large rand-real-proc L)
      (let* ((c (- 0.767 (/ 3.36 L)))
             (beta (/ PI (sqrt (* 3 L))))
             (alpha (* beta L))
             (k (- (log c) L (log beta))))
        (define (loop)
          (let* ((u (rand-real-proc))
                 (x (/ (- alpha (log (/ (- 1.0 u) u))) beta))
                 (n (exact (floor (+ x 0.5)))))
            (if (< n 0)
                (loop)
                (let* ((v (rand-real-proc))
                       (y (- alpha (* beta x)))
                       (t (+ 1.0 (exp y)))
                       (lhs (+ y (log (/ v (* t t)))))
                       (rhs (+ k (* n (log L)) (- (log-of-fact n)))))
                  (if (<= lhs rhs)
                      n
                      (loop))))))
        loop))
    
    ;; private
    ;; log(n!) table for n 1 to 256. Vector, where nth index corresponds to log((n+1)!)
    ;; Computed on first invocation of `log-of-fact`
    (define log-fact-table #f)
    
    ;; private
    ;; computes log-fact-table
    ;; log(n!) = log((n-1)!) + log(n)
    (define (make-log-fact-table!)
      (define table (make-vector 256))
      (vector-set! table 0 0)
      (do ((i 1 (+ i 1)))
          ((> i 255) #t)
          (vector-set! table i (+ (vector-ref table (- i 1))
                                  (log (+ i 1)))))
      (set! log-fact-table table))
    
    ;; private
    ;; returns log(n!)
    ;; adapted from https://www.johndcook.com/blog/2010/08/16/how-to-compute-log-factorial/
    (define (log-of-fact n)
      (when (not log-fact-table)
        (make-log-fact-table!))
      (cond
        ((<= n 1) 0)
        ((<= n 256) (vector-ref log-fact-table (- n 1)))
        (else (let ((x (+ n 1)))
               (+ (* (- x 0.5)
                     (log x))
                  (- x)
                  (* 0.5
                     (log (* 2 PI)))
                  (/ 1.0 (* x 12.0)))))))
    
    
    
    (define (gsampling . generators-lst)
      (let ((gen-vec (list->vector generators-lst))
            (rand-int-proc (random-source-make-integers (current-random-source))))
    
        ;remove exhausted generator at index
        (define (remove-gen index)
          (define new-vec (make-vector (- (vector-length gen-vec) 1)))
          ;when removing anything but first, copy all elements before index
          (when (> index 0)
            (vector-copy! new-vec 0 gen-vec 0 index))
          ;when removing anything but last, copy all elements after index
          (when (< index (- (vector-length gen-vec) 1))
            (vector-copy! new-vec index gen-vec (+ 1 index)))
          (set! gen-vec new-vec))
    
        ;randomly pick generator. If it's exhausted remove it, and pick again
        ;returns value (or eof, if all generators are exhausted)
        (define (pick)
          (let* ((index (rand-int-proc (vector-length gen-vec)))
                 (gen (vector-ref gen-vec index))
                 (value (gen)))
            (if (eof-object? value)
                (begin
                  (remove-gen index)
                  (if (= (vector-length gen-vec) 0)
                      (eof-object)
                      (pick)))
                value)))
    
        (lambda ()
          (if (= 0 (vector-length gen-vec))
              (eof-object)
              (pick)))))
    
    
    ;;; Code for binomial random variable generation.
    
    ;;; binomial-geometric is somewhat classical, the
    ;;; "First waiting time algorithm" from page 525 of
    ;;; Devroye, L. (1986), Non-Uniform Random Variate
    ;;; Generation, Springer-Verlag, New York.
    
    ;;; binomial-rejection is algorithm BTRS from
    ;;; Hormann, W. (1993), The generation of binomial
    ;;; random variates, Journal of Statistical Computation
    ;;; and Simulation, 46:1-2, 101-110,
    ;;; DOI: https://doi.org/10.1080/00949659308811496
    ;;; stirling-tail is also from that paper.
    
    ;;; Another implementation of the same algorithm is at
    ;;; https://github.com/tensorflow/tensorflow/blob/master/tensorflow/core/kernels/random_binomial_op.cc
    ;;; That implementation pointed out at least two bugs in the
    ;;; BTRS paper.
    
    (define (stirling-tail k)
      ;; Computes
      ;;
      ;; \log(k!)-[\log(\sqrt{2\pi})+(k+\frac12)\log(k+1)-(k+1)]
      ;;
      (let ((small-k-table
             ;; Computed using computable reals package
             ;; Matches values in paper, which are given
             ;; for 0\leq k < 10
             '#(.08106146679532726
                .0413406959554093
                .02767792568499834
                .020790672103765093
                .016644691189821193
                .013876128823070748
                .01189670994589177
                .010411265261972096
                .009255462182712733
                .00833056343336287
                .007573675487951841
                .00694284010720953
                .006408994188004207
                .0059513701127588475
                .005554733551962801
                .0052076559196096404
                .004901395948434738
                .004629153749334028
                .004385560249232324
                .004166319691996922)))
        (if (< k 20)
            (vector-ref small-k-table k)
            ;; the correction term (+ (/ (* 12 (+ k 1))) ...)
            ;; in Stirling's approximation to log(k!)
            (let* ((inexact-k+1 (inexact (+ k 1)))
                   (inexact-k+1^2 (square inexact-k+1)))
              (/ (- #i1/12
                    (/ (- #i1/360
                          (/ #i1/1260 inexact-k+1^2))
                       inexact-k+1^2))
                 inexact-k+1)))))
    
    (define (make-binomial-generator n p)
      (if (not (and (real? p)
                    (<= 0 p 1)
                    (exact-integer? n)
                    (positive? n)))
           (error "make-binomial-generator: Bad parameters: " n p)
           (cond ((< 1/2 p)
                  (let ((complement (make-binomial-generator n (- 1 p))))
                    (lambda ()
                      (- n (complement)))))
                 ((zero? p)
                  (lambda () 0))
                 ((< (* n p) 10)
                  (binomial-geometric n p))
                 (else
                  (binomial-rejection n p)))))
    
    (define (binomial-geometric n p)
      (let ((geom (make-geometric-generator p)))
        (lambda ()
          (let loop ((X -1)
                     (sum 0))
            (if (< n sum)
                X
                (loop (+ X 1)
                      (+ sum (geom))))))))
    
    (define (binomial-rejection n p)
      ;; call when p <= 1/2 and np >= 10
      ;; Use notation from the paper
      (let* ((spq
              (inexact (sqrt (* n p (- 1 p)))))
             (b
              (+ 1.15 (* 2.53 spq)))
             (a
              (+ -0.0873
                 (* 0.0248 b)
                 (* 0.01 p)))
             (c
              (+ (* n p) 0.5))
             (v_r
              (- 0.92
                 (/ 4.2 b)))
             (alpha
              ;; The formula in BTRS has 1.5 instead of 5.1;
              ;; The formula for alpha in algorithm BTRD and Table 1
              ;; and the tensorflow code uses 5.1, so we use 5.1
              (* (+ 2.83 (/ 5.1 b)) spq))
             (lpq
              (log (/ p (- 1 p))))
             (m
              (exact (floor (* (+ n 1) p))))
             (rand-real-proc
              (random-source-make-reals (current-random-source))))
        (lambda ()
          (let loop ((u (rand-real-proc))
                     (v (rand-real-proc)))
            (let* ((u
                    (- u 0.5))
                   (us
                    (- 0.5 (abs u)))
                   (k
                    (exact
                     (floor
                      (+ (* (+ (* 2. (/ a us)) b) u) c)))))
              (cond ((or (< k 0)
                         (< n k))
                     (loop (random-real)
                           (random-real)))
                    ((and (<= 0.07 us)
                          (<= v v_r))
                     k)
                    (else
                     (let ((v
                            ;; The tensorflow code notes that BTRS doesn't have
                            ;; this logarithm; BTRS is incorrect (see BTRD, step 3.2)
                            (log (* v (/ alpha
                                         (+ (/ a (square us)) b))))))
                       (if (<=  v
                                (+ (* (+ m 0.5)
                                      (log (* (/ (+ m 1.)
                                                 (- n m -1.)))))
                                   (* (+ n 1.)
                                      (log (/ (- n m -1.)
                                              (- n k -1.))))
                                   (* (+ k 0.5)
                                      (log (* (/ (- n k -1.)
                                                 (+ k 1.)))))
                                   (* (- k m) lpq)
                                   (- (+ (stirling-tail m)
                                         (stirling-tail (- n m)))
                                      (+ (stirling-tail k)
                                         (stirling-tail (- n k))))))
                           k
                           (loop (random-real)
                                 (random-real)))))))))))
  )
  
  (begin

    ; zipf-zri.scm
    ; Create a Zipf random distribution.
    ;
    ; Created by Linas Vepstas 10 July 2020
    ; Nominated for inclusion in srfi-194
    ;
    ; Not optimized for speed!
    ;
    ; Implementation from ZRI algorithm presented in the paper:
    ; "Rejection-inversion to generate variates from monotone discrete
    ; distributions", Wolfgang Hörmann and Gerhard Derflinger
    ; ACM TOMACS 6.3 (1996): 169-184
    ;
    ; Hörmann and Derflinger use "q" everywhere, when they really mean "s".
    ; Thier "q" is not the standard q-series deformation. Its just "s".
    ; The notation in the code below differs from the article to reflect
    ; conventional usage.
    ;
    ;------------------------------------------------------------------
    ;
    ; The Hurwicz zeta distribution 1 / (k+q)^s for 1 <= k <= n integer
    ; The Zipf distribution is recovered by setting q=0.
    ;
    ; The exponent `s` must be a real number not equal to 1.
    ; Accuracy is diminished for |1-s|< 1e-6. The accuracy is roughly
    ; equal to 1e-15 / |1-s| where 1e-15 == 64-bit double-precision ULP.

    (define (make-zipf-generator/zri n s q)
    
      ; The hat function h(x) = 1 / (x+q)^s
      (define (hat x)
        (expt (+ x q) (- s)))
    
      (define _1-s (- 1 s))
      (define oms (/ 1 _1-s))
    
      ; The integral of hat(x)
      ; H(x) = (x+q)^{1-s} / (1-s)
      ; Note that H(x) is always negative.
      (define (big-h x)
        (/ (expt (+ q x) _1-s) _1-s))
    
      ; The inverse function of H(x)
      ; H^{-1}(y) = -q + (y(1-s))^{1/(1-s)}
      (define (big-h-inv y)
        (- (expt (* y _1-s) oms) q))
    
      ; Lower and upper bounds for the uniform random generator.
      (define big-h-half (- (big-h 1.5) (hat 1)))
      (define big-h-n (big-h (+ n 0.5)))
    
      ; Rejection cut
      (define cut (- 1 (big-h-inv (- (big-h 1.5) (hat 1)))))
    
      ; Uniform distribution
      (define dist (make-random-real-generator big-h-half big-h-n))
    
      ; Attempt to hit the dartboard. Return #f if we fail,
      ; otherwise return an integer between 1 and n.
      (define (try)
        (define u (dist))
        (define x (big-h-inv u))
        (define kflt (floor (+ x 0.5)))
        (define k (exact kflt))
        (if (and (< 0 k)
                 (or
                   (<= (- k x) cut)
                   (>= u (- (big-h (+ k 0.5)) (hat k))))) k #f))
    
      ; Did we hit the dartboard? If not, try again.
      (define (loop-until)
        (define k (try))
        (if k k (loop-until)))
    
      ; Return the generator.
      loop-until)
    
    ; The Hurwicz zeta distribution 1 / (k+q)^s for 1 <= k <= n integer
    ; The Zipf distribution is recovered by setting q=0.
    ;
    ; The exponent `s` must be a real number close to 1.
    ; Accuracy is diminished for |1-s|> 2e-4. The accuracy is roughly
    ; equal to 0.05 * |1-s|^4 due to exp(1-s) being expanded to 4 terms.
    ;
    ; This handles the special case of s==1 perfectly.
    (define (make-zipf-generator/one n s q)
    
      (define _1-s (- 1 s))
    
      ; The hat function h(x) = 1 / (x+q)^s
      ; Written for s->1 i.e. 1/(x+q)(x+q)^{s-1}
      (define (hat x)
        (define xpq (+ x q))
        (/ (expt xpq _1-s) xpq))
    
      ; Expansion of exn(y) = [exp(y(1-s))-1]/(1-s) for s->1
      ; Expanded to 4th order.
      ; Should equal this:
      ;;; (define (exn lg) (/ (- (exp (* _1-s lg)) 1) _1-s))
      ; but more accurate for s near 1.0
      (define (exn lg)
        (define (trm n u lg) (* lg (+ 1 (/ (* _1-s u) n))))
        (trm 2 (trm 3 (trm 4 1 lg) lg) lg))
    
      ; Expansion of lg(y) = [log(1 + y(1-s))] / (1-s) for s->1
      ; Expanded to 4th order.
      ; Should equal this:
      ;;; (define (lg y) (/ (log (+ 1 (* y _1-s))) _1-s))
      ; but more accurate for s near 1.0
      (define (lg y)
        (define yms (* y _1-s))
        (define (trm n u r) (- (/ 1 n) (* u r)))
        (* y (trm 1 yms (trm 2 yms (trm 3 yms (trm 4 yms 0))))))
    
      ; The integral of hat(x) defined at s==1
      ; H(x) = [exp{(1-s) log(x+q)} - 1]/(1-s)
      ; Should equal this:
      ;;;  (define (big-h x) (/ (- (exp (* _1-s (log (+ q x)))) 1)  _1-s))
      ; but expanded so that it's more accurate for s near 1.0
      (define (big-h x)
        (exn (log (+ q x))))
    
      ; The inverse function of H(x)
      ; H^{-1}(y) = -q + (1 + y(1-s))^{1/(1-s)}
      ; Should equal this:
      ;;; (define (big-h-inv y) (- (expt (+ 1 (* y _1-s)) (/ 1 _1-s)) q ))
      ; but expanded so that it's more accurate for s near 1.0
      (define (big-h-inv y)
        (- (exp (lg y)) q))
    
      ; Lower and upper bounds for the uniform random generator.
      (define big-h-half (- (big-h 1.5) (hat 1)))
    
      (define big-h-n (big-h (+ n 0.5)))
    
      ; Rejection cut
      (define cut (- 1 (big-h-inv (- (big-h 1.5) (/ 1 (+ 1 q))))))
    
      ; Uniform distribution
      (define dist (make-random-real-generator big-h-half big-h-n))
    
      ; Attempt to hit the dartboard. Return #f if we fail,
      ; otherwise return an integer between 1 and n.
      (define (try)
        (define u (dist))
        (define x (big-h-inv u))
        (define kflt (floor (+ x 0.5)))
        (define k (exact kflt))
        (if (and (< 0 k)
                 (or
                   (<= (- k x) cut)
                   (>= u (- (big-h (+ k 0.5)) (hat k))))) k #f))
    
      ; Did we hit the dartboard? If not, try again.
      (define (loop-until)
        (define k (try))
        (if k k (loop-until)))
    
      ; Return the generator.
      loop-until)
    
    ; (make-zipf-generator n [s [q]])
    ;
    ; The Hurwicz zeta distribution 1 / (k+q)^s for 1 <= k <= n integer
    ; The Zipf distribution is recovered by setting q=0.
    ; If `q` is not specified, 0 is assumed.
    ; If `s` is not specified, 1 is assumed.
    ;
    ; Valid for real -10 < s < 100 (otherwise overflows likely)
    ; Valid for real -0.5 < q < 2e8 (otherwise overflows likely)
    ; Valid for integer 1 <= k < int-max
    ;
    ; Example usage:
    ;    (define zgen (make-zipf-generator 50 1.01 0))
    ;    (generator->list zgen 10)

    (define make-zipf-generator
      (case-lambda
        ((n)
         (make-zipf-generator n 1.0 0.0))
        ((n s)
         (make-zipf-generator n s 0.0))
        ((n s q)
         (if (< 1e-5 (abs (- 1 s)))
             (make-zipf-generator/zri n s q)
             (make-zipf-generator/one n s q)))))
  )
  
  (begin
    
    ; sphere.scm
    ; Uniform distributions on a sphere, and a ball.
    ; Submitted for inclusion in srfi-194
    ;
    ; Algorithm based on BoxMeuller as described in
    ; http://extremelearning.com.au/how-to-generate-uniformly-random-points-on-n-spheres-and-n-balls/
    ;
    
    ; make-sphere-generator N - return a generator of points uniformly
    ; distributed on an N-dimensional sphere.
    ; This implements the BoxMeuller algorithm, that is, of normalizing
    ; N+1 Gaussian random variables.
    (define (make-sphere-generator arg)
      (cond
        ((integer? arg) (make-ellipsoid-generator* (make-vector (+ 1 arg) 1.0)))
        (else (error "expected argument to be an integer dimension"))))
    
    (define (make-ellipsoid-generator arg)
      (cond
        ((vector? arg) (make-ellipsoid-generator* arg))
        (else (error "expected argument to be a vector of axis lengths"))))
    
    ; Generator of points uniformly distributed on an N-dimensional ellipsoid.
    ;
    ; The `axes` should be a vector of floats, specifying the axes of the
    ; ellipsoid. The algorithm used is an accept/reject sampling algo,
    ; wherein the acceptance rate is proportional to the measure of a
    ; surface element on the ellipsoid. The measure is straight-forward to
    ; arrive at, and the 3D case is described by `mercio` in detail at
    ; https://math.stackexchange.com/questions/973101/how-to-generate-points-uniformly-distributed-on-the-surface-of-an-ellipsoid
    ;
    ; Note that sampling means that performance goes as
    ; O(B/A x C/A x D/A x ...) where `A` is the shorest axis,
    ; and `B`, `C`, `D`, ... are the other axes. Maximum performance
    ; achieved on spheres.
    
    (define (make-ellipsoid-generator* axes)
    
      ; A vector of normal gaussian generators
      (define gaussg-vec
        (make-vector (vector-length axes) (make-normal-generator 0 1)))
    
      ; Banach l2-norm of a vector
      (define (l2-norm VEC)
        (sqrt (vector-fold
                (lambda (sum x) (+ sum (* x x)))
                0
                VEC)))
    
      ; Generate one point on a sphere
      (define (sph)
        ; Sample a point
        (define point
          (vector-map (lambda (gaussg) (gaussg)) gaussg-vec))
        ; Project it to the unit sphere (make it unit length)
        (define norm (/ 1.0 (l2-norm point)))
        (vector-map (lambda (x) (* x norm)) point))
    
      ; Distance from origin to the surface of the
      ; ellipsoid along direction RAY.
      (define (ellipsoid-dist RAY)
        (sqrt (vector-fold
                (lambda (sum x a) (+ sum (/ (* x x) (* a a))))
                0 RAY axes)))
    
      ; Find the shortest axis.
      (define minor
        (vector-fold
            (lambda (mino l) (if (< l mino) l mino))
            1e308 axes))
    
      ; Uniform generator [0,1)
      (define uni (make-random-real-generator 0.0 1.0))
    
      ; Return #t if the POINT can be kept; else must resample.
      (define (keep POINT)
        (< (uni) (* minor (ellipsoid-dist POINT))))
    
      ; Sample until a good point is found. The returned sample is a
      ; vector of unit length (we already normed up above).
      (define (sample)
        (define vect (sph))
        (if (keep vect) vect (sample)))
    
      (lambda ()
      ; Find a good point, and rescale to ellipsoid.
        (vector-map
             (lambda (x a) (* x a)) (sample) axes))
    )
    
    ; make-ball-generator N - return a generator of points uniformly
    ; distributed inside an N-dimensional ball.
    ; This implements the Harman-Lacko-Voelker Dropped Coordinate method.
    (define (make-ball-generator arg)
      (define dim-sizes
        (cond
          ((integer? arg) (make-vector (+ 2 arg) 1.0))
          ((vector? arg) (vector-append arg (vector 1.0 1.0)))
          (else (error "expected argument to either be a number (dimension), or vector (axis length for the dimensions)"))))
      (define N (- (vector-length dim-sizes) 2))
      (define sphereg (make-sphere-generator (+ N 2)))
      ; Create a vector of N+2 values, and drop the last two.
      ; (The sphere already added one, so we only add one more)
      (lambda ()
        (vector-map
          (lambda (el dim-size _) (* el dim-size))
          (sphereg)
          dim-sizes
          (make-vector N #f))))
  )
)

