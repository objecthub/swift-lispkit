;;; SCHEME R5RS
;;;
;;; Library exporting definitions from R5RS to provide backward compatibility to this
;;; universally supported Scheme standard. This library is part of the R7RS standard.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2017 Matthias Zenger. All rights reserved.
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

(define-library (scheme r5rs)
  (export +
          -
          *
          /
          <
          <=
          =
          >
          >=
          ; ...                       Not a syntax form in LispKit
          ; =>                        Not a syntax form in LispKit
          ; _                         Not a syntax form in LispKit
          abs
          acos
          and
          angle
          append
          apply
          asin
          assoc
          assq
          assv
          atan
          begin
          boolean?
          caaaar
          caaadr
          caaar
          caadar
          caaddr
          caadr
          caar
          cadaar
          cadadr
          cadar
          caddar
          cadddr
          caddr
          cadr
          call-with-current-continuation
          call-with-input-file
          call-with-output-file
          call-with-values
          car
          case
          cdaaar
          cdaadr
          cdaar
          cdadar
          cdaddr
          cdadr
          cdar
          cddaar
          cddadr
          cddar
          cdddar
          cddddr
          cdddr
          cddr
          cdr
          ceiling
          char->integer
          char-alphabetic?
          char-ci<?
          char-ci<=?
          char-ci=?
          char-ci>?
          char-ci>=?
          char-downcase
          char-lower-case?
          char-numeric?
          char-ready?
          char-upcase
          char-upper-case?
          char-whitespace?
          char?
          char<?
          char<=?
          char=?
          char>?
          char>=?
          close-input-port
          close-output-port
          complex?
          cond
          cons
          cos
          current-input-port
          current-output-port
          define
          define-syntax
          delay
          denominator
          display
          do
          dynamic-wind
          ; else                      Not a syntax form in LispKit
          eof-object?
          eq?
          equal?
          eqv?
          eval
          even?
          exact->inexact
          exact?
          exp
          expt
          floor
          for-each
          force
          gcd
          if
          imag-part
          inexact->exact
          inexact?
          input-port?
          integer->char
          integer?
          interaction-environment
          lambda
          lcm
          length
          let
          let-syntax
          let*
          letrec
          letrec-syntax
          list
          list->string
          list->vector
          list-ref
          list-tail
          list?
          load
          log
          magnitude
          make-polar
          make-rectangular
          make-string
          make-vector
          map
          max
          member
          memq
          memv
          min
          modulo
          negative?
          newline
          not
          null-environment
          null?
          number->string
          number?
          numerator
          odd?
          open-input-file
          open-output-file
          or
          output-port?
          pair?
          peek-char
          positive?
          procedure?
          quasiquote
          quote
          quotient
          rational?
          rationalize
          read
          read-char
          real-part
          real?
          remainder
          reverse
          round
          scheme-report-environment
          ; set-car!                  Not a syntax form in LispKit
          ; set-cdr!                  Not a syntax form in LispKit
          set!
          sin
          sqrt
          string
          string->list
          string->number
          string->symbol
          string-append
          string-ci<?
          string-ci<=?
          string-ci=?
          string-ci>?
          string-ci>=?
          string-copy
          string-fill!
          string-length
          string-ref
          string-set!
          string?
          string<?
          string<=?
          string=?
          string>?
          string>=?
          substring
          symbol->string
          symbol?
          syntax-rules
          tan
          truncate
          values
          vector
          vector->list
          vector-fill!
          vector-length
          vector-ref
          vector-set!
          vector?
          with-input-from-file
          with-output-to-file
          write
          write-char
          zero?)
  (import (rename (lispkit base)
            (exact inexact->exact)
            (inexact exact->inexact)))
)
