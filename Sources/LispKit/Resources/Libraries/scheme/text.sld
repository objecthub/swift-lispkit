;;; SCHEME TEXT
;;;
;;; Scheme text library. This library is part of the Scheme Red edition of the R7RS
;;; large language.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2018 Matthias Zenger. All rights reserved.
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

(define-library (scheme text)

  (export text?
          textual?
          textual-null?
          textual-every
          textual-any
          make-text
          text
          text-tabulate
          text-unfold
          text-unfold-right
          textual->text
          textual->string
          textual->vector
          textual->list
          string->text
          vector->text
          list->text
          reverse-list->text
          textual->utf8
          textual->utf16be
          textual->utf16
          textual->utf16le
          utf8->text
          utf16be->text
          utf16->text
          utf16le->text
          text-length
          textual-length
          text-ref
          textual-ref
          subtext
          subtextual
          textual-copy
          textual-take
          textual-take-right
          textual-drop
          textual-drop-right
          textual-pad
          textual-pad-right
          textual-trim
          textual-trim-right
          textual-trim-both
          textual-replace
          textual=?
          textual-ci=?
          textual<?
          textual-ci<?
          textual>?
          textual-ci>?
          textual<=?
          textual-ci<=?
          textual>=?
          textual-ci>=?
          textual-prefix-length
          textual-suffix-length
          textual-prefix?
          textual-suffix?
          textual-index
          textual-index-right
          textual-skip
          textual-skip-right
          textual-contains
          textual-contains-right
          textual-upcase
          textual-downcase
          textual-foldcase
          textual-titlecase
          textual-append
          textual-concatenate
          textual-concatenate-reverse
          textual-join
          textual-fold
          textual-fold-right
          textual-map
          textual-for-each
          textual-map-index
          textual-for-each-index
          textual-count
          textual-filter
          textual-remove
          textual-replicate
          textual-split)

  (import (srfi 135))
)

