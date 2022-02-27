;;; SRFI 149
;;; Basic syntax-rules template extensions
;;;
;;; The rules for valid `<template>`s of `<syntax rules>` are slightly softened
;;; to allow for more than one consecutive `<ellipsis>` in subtemplates, and to
;;; allow pattern variables in subtemplates to be followed by more instances of
;;; the identifier `<ellipsis>` than they are followed in the subpattern in which
;;; they occur.
;;;
;;; Author of spec: Marc Nieper-Wißkirchen
;;;
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

(define-library (srfi 149)

  (export syntax-rules)

  (import (lispkit core))

  ;; `syntax-rules` is implemented natively already in LispKit with the extensions of
  ;; SRFI 149.
)
