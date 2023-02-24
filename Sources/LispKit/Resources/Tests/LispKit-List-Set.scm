;;; LISPKIT LIST SET REGRESSION TEST SUITE
;;;
;;; This is the test suite for library `(lispkit list set)`.
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2023 Matthias Zenger. All rights reserved.
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

(import (lispkit base)
        (lispkit test)
        (lispkit list set))

(test-begin "LispKit List Set")

(test-assert (lset<=? eq? '(a) '(a b a) '(a b c c)))
(test-assert (lset<=? eq?))
(test-assert (lset<=? eq? '(a)))
(test-assert (lset=? eq? '(b e a) '(a e b) '(e e b a)))
(test-assert (lset=? eq?))
(test-assert (lset=? eq? '(a)))
(test-equal '(u o i a b c d c e) (lset-adjoin eq? '(a b c d c e) 'a 'e 'i 'o 'u))
(test-equal '(u o i a b c d e) (lset-union eq? '(a b c d e) '(a e i o u)))
(test-equal '(x a a c) (lset-union eq? '(a a c) '(x a x)))
(test-equal '() (lset-union eq?))
(test-equal '(a b c) (lset-union eq? '(a b c)))
(test-equal '(a e) (lset-intersection eq? '(a b c d e) '(a e i o u)))
(test-equal '(a x a) (lset-intersection eq? '(a x y a) '(x a x z)))
(test-equal '(a b c) (lset-intersection eq? '(a b c)))
(test-equal '(b c d) (lset-difference eq? '(a b c d e) '(a e i o u)))
(test-equal '(a b c) (lset-difference eq? '(a b c)))
(test-assert (lset=? eq? '(d c b i o u) (lset-xor eq? '(a b c d e) '(a e i o u))))
(test-equal '() (lset-xor eq?))
(test-equal '(a b c d e) (lset-xor eq? '(a b c d e)))

(test-end)
