;;; SCHEME BASE
;;;
;;; Library exporting all definitions of the R7RS standard.
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

(define-library (scheme base)
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
          and
          append
          apply
          assoc
          assq
          assv
          begin
          binary-port?
          boolean?
          boolean=?
          bytevector
          bytevector-append
          bytevector-copy
          bytevector-copy!
          bytevector-length
          bytevector-u8-ref
          bytevector-u8-set!
          bytevector?
          caar
          cadr
          call-with-current-continuation
          call-with-port
          call-with-values
          call/cc
          car
          case
          cdar
          cddr
          cdr
          ceiling
          char->integer
          char-ready?
          char?
          char<?
          char<=?
          char=?
          char>?
          char>=?
          close-input-port
          close-output-port
          close-port
          complex?
          cond
          cond-expand
          cons
          current-error-port
          current-input-port
          current-output-port
          define
          define-record-type
          define-syntax
          define-values
          denominator
          do
          dynamic-wind
          ; else                      Not a syntax form in LispKit
          eof-object
          eof-object?
          eq?
          equal?
          eqv?
          error
          error-object-irritants
          error-object-message
          error-object?
          even?
          exact
          exact-integer-sqrt
          exact-integer?
          exact?
          expt
          features
          file-error?
          floor
          floor-quotient
          floor-remainder
          floor/
          flush-output-port
          for-each
          gcd
          get-output-bytevector
          get-output-string
          guard
          if
          include
          ; include-ci                Undefined in LispKit
          inexact
          inexact?
          input-port-open?
          input-port?
          integer->char
          integer?
          lambda
          lcm
          length
          let
          let-syntax
          let-values
          let*
          let*-values
          letrec
          letrec-syntax
          letrec*
          list
          list->string
          list->vector
          ; list-copy                 Undefined in LispKit
          list-ref
          ; list-set!                 Undefined in LispKit
          list-tail
          list?
          make-bytevector
          make-list
          make-parameter
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
          null?
          number->string
          number?
          numerator
          odd? 
          open-input-bytevector
          open-input-string
          open-output-bytevector
          open-output-string
          or
          output-port-open?
          output-port?
          pair?
          parameterize
          peek-char
          peek-u8
          port?
          positive?
          procedure?
          quasiquote
          quote
          quotient
          raise
          raise-continuable
          rational?
          rationalize
          read-bytevector
          read-bytevector!
          read-char
          read-error?
          read-line
          read-string
          read-u8
          real?
          remainder
          reverse
          round
          ; set-car!                  Undefined in LispKit
          ; set-cdr!                  Undefined in LispKit
          set!
          square
          string
          string->list
          string->number
          string->symbol
          string->utf8
          string->vector
          string-append
          string-copy
          string-copy!
          string-fill!
          string-for-each
          string-length
          string-map
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
          symbol=?
          syntax-error
          syntax-rules
          textual-port?
          truncate
          truncate-quotient
          truncate-remainder
          truncate/
          u8-ready?
          unless
          ; unquote                   Not a syntax form in LispKit
          ; unquote-splicing          Not a syntax form in LispKit
          utf8->string
          values
          vector
          vector->list
          vector->string
          vector-append
          vector-copy
          vector-copy!
          vector-fill!
          vector-for-each
          vector-length
          vector-map
          vector-ref
          vector-set!
          vector?
          when
          with-exception-handler
          write-bytevector
          write-char
          write-string
          write-u8
          zero?)

  (import (lispkit base))
)
