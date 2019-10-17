;;; SRFI 174
;;; POSIX Timespecs
;;;
;;; This SRFI defines the trivial type timespec, which is used to represent the struct
;;; timespec defined by the POSIX `<time.h>` header.
;;;
;;; The reason for putting this very simple and straightforward type into a SRFI (and
;;; library) of its own is that timespecs are part of the interface for more than one
;;; SRFI. If they are defined in just one SRFI and imported by the rest, that produces
;;; an otherwise useless and unnecessary dependency on the defining SRFI. This arises
;;; particularly in R6RS and R7RS because record types are generative (distinct
;;; definitions lead to distinct record types) and because most implementations report
;;; a warning or even an error if the same identifier is imported from different
;;; libraries, unless they have both imported it in turn from the same original library.
;;;
;;; Author of spec: John Cowan
;;;
;;; Copyright © 2019 Matthias Zenger. All rights reserved.
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

(define-library (srfi 174)

  (import (lispkit base)
          (lispkit comparator))

  (export timespec
          timespec?
          timespec-seconds
          timespec-nanoseconds
          timespec=
          timespec<
          timespec-hash
          timespec-comparator)

  (begin

    (define-type timespec timespec?

      ((timespec sec nano)
        (unless (exact-integer? sec)
          (error "timespec expects exact integer for its second"))
        (unless (and (exact-integer? nano) (<= 0 nano 999999999))
          (error "timespec expects exact integer for its nano second"))
        (cons sec nano))

      ((timespec-seconds (ts)) (car ts))

      ((timespec-nanoseconds (ts)) (cdr ts))

      ((timespec= a b)
         (and (= (timespec-seconds a) (timespec-seconds b))
              (= (timespec-nanoseconds a) (timespec-nanoseconds b))))

      ((timespec< a b)
         (or (< (timespec-seconds a) (timespec-seconds b))
             (and (= (timespec-seconds a) (timespec-seconds b))
                  (< (timespec-nanoseconds a) (timespec-nanoseconds b)))))

      ((timespec-hash (ts))
         (combine-hash (number-hash (car ts)) (number-hash (cdr ts)))))

    (define timespec-comparator
      (make-comparator
        timespec?
        timespec=
        timespec<
        timespec-hash))
  )
)
