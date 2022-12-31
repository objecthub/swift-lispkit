;;; SRFI 228 REGRESSION TEST SUITE
;;;
;;; This is the test suite for SRFI 228.
;;;
;;; Copyright © 2021 Daphne Preston-Kendal. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2022 Matthias Zenger. All rights reserved.

(import (lispkit base)
        (lispkit test)
        (srfi 1)
        (srfi 128)
        (srfi 228)
        (only (srfi 132) list-sort))

(define-record-type Person
  (make-person first-name last-name)
  person?
  (first-name person-first-name)
  (last-name person-last-name))

(define person-name-comparator
  (make-product-comparator
    (make-wrapper-comparator person? person-last-name string-ci-comparator)
    (make-wrapper-comparator person? person-first-name string-ci-comparator)))

(test-begin "SRFI 228: Composing comparators")

(test-group "simple"
  (test-equal eq?
              #t
              (<? person-name-comparator
                  (make-person "John" "Cowan")
                  (make-person "Daphne" "Preston-Kendal")))
  (test-equal eq?
              #t
              (>? person-name-comparator
                  (make-person "Tom" "Smith")
                  (make-person "John" "Smith"))))

(define-record-type Book
  (make-book author title)
  book?
  (author book-author)
  (title book-title))

(define book-comparator
  (make-product-comparator
    (make-wrapper-comparator book? book-author person-name-comparator)
    (make-wrapper-comparator book? book-title string-ci-comparator)))

(define-record-type CD
  (make-cd artist title)
  cd?
  (artist cd-artist)
  (title cd-title))

(define cd-comparator
  (make-product-comparator
    (make-wrapper-comparator cd? cd-artist person-name-comparator)
    (make-wrapper-comparator cd? cd-title string-ci-comparator)))

(define item-comparator
  (make-sum-comparator book-comparator cd-comparator))

(test-group "nested"
  (let* ((beatles (make-person "The" "Beatles"))
         (abbey-road (make-cd beatles "Abbey Road"))
         (deutsche-grammatik
           (make-book (make-person "Jacob" "Grimm") "Deutsche Grammatik"))
         (sonnets (make-book (make-person "William" "Shakespeare") "Sonnets"))
         (mnd (make-book (make-person "William" "Shakespeare")
                         "A Midsummer Night’s Dream"))
         (bob (make-cd (make-person "Bob" "Dylan") "Blonde on Blonde"))
         (revolver (make-cd (make-person "The" "Beatles") "Revolver")))
    (test-equal
      equal?
      (list deutsche-grammatik
            mnd
            sonnets
            abbey-road
            revolver
            bob)
      (list-sort
        (lambda (a b) (<? item-comparator a b))
        (list abbey-road
              deutsche-grammatik
              sonnets
              mnd
              bob
              revolver)))))

(test-end)
