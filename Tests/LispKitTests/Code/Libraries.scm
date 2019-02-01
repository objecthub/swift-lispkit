;;; Libraries.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2019 ObjectHub. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;      http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(
  "Library definition"
  (8 7)
  (define-library (alib)
    (export foo var)
    (import (lispkit base))
    (begin
      (define var 8)
      (define (foo) var)))
  (import (alib))
  (define x (foo))
  (set! var 7)
  (list x var)
)

(
  "Duplicate imports"
  (3)
  (define-library (blib)
    (export foo)
    (import (lispkit base))
    (begin (define (foo) 3)))
  (define-library (clib)
    (export foo)
    (import (blib)))
  (define-library (dlib)
    (export bar)
    (import (lispkit base) (blib) (clib))
    (begin (define (bar) (list (foo)))))
  (import (dlib))
  (bar)
)

(
  "Inconsistent duplicate imports"
  <error>
  (define-library (blib)
    (export foo)
    (import (lispkit base))
    (begin (define (foo) 3)))
  (define-library (clib)
    (export foo)
    (import (lispkit base))
    (begin (define (foo) 4)))
  (define-library (dlib)
    (export bar)
    (import (lispkit base) (clib) (blib))
    (begin (define (bar) (list (foo)))))
  (import (dlib))
  (bar)
)

(
  "Mutual library dependencies"
  ((foo (bar 1)) (bar (foo 0)))
  (define-library (elib)
    (export foo)
    (import (lispkit base) (flib))
    (begin (define (foo . x) (list 'foo (if (null? x) 0 (bar))))))
  (define-library (flib)
    (export bar)
    (import (lispkit base) (elib))
    (begin (define (bar . x) (list 'bar (if (null? x) 1 (foo))))))
  (import (elib))
  (import (flib))
  (list (foo 8) (bar 9))
)

(
  "Mutual cyclic library dependencies"
  <error>
  (define-library (glib)
    (export foo)
    (import (lispkit base) (hlib)))
  (define-library (hlib)
    (export foo)
    (import (lispkit base) (glib)))
  (import (glib))
  (foo)
)

(
  "Incomplete library"
  <error>
  (define-library (ilib)
    (export foo)
    (import (lispkit base)))
  (import (ilib))
  (foo)
)

(
  "Hidden library import"
  <error>
  (define-library (jlib)
    (export foo)
    (import (lispkit base))
    (begin (define (foo) 3)))
  (define-library (klib)
    (export foo)
    (import (lispkit base) (jlib))
    (begin (define (foo) 4)))
  (import (klib))
  (foo)
)
