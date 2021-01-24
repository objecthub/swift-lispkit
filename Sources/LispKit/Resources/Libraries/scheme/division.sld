;;; SCHEME DIVISION
;;;
;;; Library implementing various division-related procedures. This library is part of the
;;; Scheme Tangerine edition of the R7RS large language.
;;; This library is based on division functionality from `(lispkit math)`. The procedures
;;; were written by Alex Shinn.

(define-library (scheme division)

  (export ceiling-quotient
          ceiling-remainder
          ceiling/
          euclidean-quotient
          euclidean-remainder
          euclidean/
          round-quotient
          round-remainder
          round/
          balanced-quotient
          balanced-remainder
          balanced/)
  
  (import (lispkit base))
  
  ;; Portable R7RS (scheme division) implementation.
  ;; This code is written by Alex Shinn and placed in the Public Domain.
  ;; All warranties are disclaimed.
  ;; 
  ;; This is basically the simplest possible implementation. Note the
  ;; code below assumes that exact ratios are supported and are handled
  ;; correctly by floor, ceiling and round.
  
  (begin
  
    ;; Floor, ceiling and round just compose their corresponding function
    ;; with division to determine the quotient, and compute the remainder
    ;; from that.

    (define (ceiling-quotient n m)
      (ceiling (/ n m)))

    (define (ceiling-remainder n m)
      (- n (* m (ceiling-quotient n m))))

    (define (ceiling/ n m)
      (values (ceiling-quotient n m) (ceiling-remainder n m)))

    (define (round-quotient n m)
      (round (/ n m)))

    (define (round-remainder n m)
      (- n (* m (round-quotient n m))))
    
    (define (round/ n m)
      (values (round-quotient n m) (round-remainder n m)))

    ;; Euclidean is defined as floor if the divisor is negative, and ceiling otherwise.

    (define (euclidean-quotient n m)
      (if (> m 0) (floor-quotient n m) (ceiling-quotient n m)))

    (define (euclidean-remainder n m)
      (- n (* m (euclidean-quotient n m))))

    (define (euclidean/ n m)
      (values (euclidean-quotient n m) (euclidean-remainder n m)))

    ;; Balanced places the remainder in the half-open interval [-m/2, m/2).

    (define (balanced-remainder n m)
      (let ((r (euclidean-remainder n m))
            (m/2 (abs (/ m 2))))
        (cond ((< r (- m/2)) (+ r (abs m)))
              ((>= r m/2) (- r (abs m)))
              (else r))))

    (define (balanced-quotient n m)
      (quotient (- n (balanced-remainder n m)) m))

    (define (balanced/ n m)
      (values (balanced-quotient n m) (balanced-remainder n m)))
  )
)
