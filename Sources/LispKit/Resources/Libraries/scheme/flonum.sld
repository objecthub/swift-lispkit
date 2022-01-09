;;; SCHEME FLONUM
;;;
;;; Scheme flonum library. This library is part of the Scheme Tangerine edition of
;;; the R7RS large language.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2022 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use
;;; this file except in compliance with the License. You may obtain a copy of the
;;; License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed
;;; under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
;;; CONDITIONS OF ANY KIND, either express or implied. See the License for the
;;; specific language governing permissions and limitations under the License.

(define-library (scheme flonum)
  
  (export fl-e
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
          fl-greatest
          fl-least
          fl-epsilon
          fl-fast-fl+*
          fl-integer-exponent-zero
          fl-integer-exponent-nan
          flonum
          fladjacent
          flcopysign
          make-flonum
          flinteger-fraction
          flexponent
          flinteger-exponent
          flnormalized-fraction-exponent
          flsign-bit
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
          flquotient
          flremainder
          flremquo
          flgamma
          flloggamma
          flfirst-bessel
          flsecond-bessel
          flerf
          flerfc)
  
  (import (srfi 144))
)
