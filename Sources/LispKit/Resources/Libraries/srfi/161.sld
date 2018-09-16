;;; SRFI 161
;;; Unifiable Boxes
;;;
;;; Unifiable boxes are, like the boxes of SRFI 111, objects with a single mutable state.
;;; A constructor, predicate, accessor, and mutator are provided.
;;; In addition to the state, an equality predicate and union operations (link, union, unify)
;;; are provided. Applying a union operation to two unifiable boxes makes the two boxes equal
;;; (in the sense of the equality predicate). As a consequence, their state will also become
;;; identical. In the case of link and union, it will be the state of one of the two unioned
;;; boxes. In the case of unify, the state is determined by a supplied unification procedure.
;;;
;;; Copyright (C) Marc Nieper-Wißkirchen (2018). All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software, and to permit
;;; persons to whom the Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies or
;;; substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2018 Matthias Zenger. All rights reserved.

(define-library (srfi 161)

  (export ubox
          ubox?
          ubox-ref
          ubox-set!
          ubox=?
          ubox-unify!
          ubox-union!
          ubox-link!)

  (import (lispkit base))

  (begin
    (define-record-type <ubox>
      (make-ubox parent rank value)
      ubox?
      (parent ubox-parent ubox-set-parent!)
      (rank ubox-rank ubox-set-rank!)
      (value ubox-value ubox-set-value!))

    (define (ubox-find ubox)
      (let ((parent (ubox-parent ubox)))
        (if parent
            (let ((root (ubox-find parent)))
              (ubox-set-parent! ubox root)
              root)
            ubox)))

    (define (ubox value)
      (make-ubox #f 0 value))

    (define (ubox-ref ubox)
      (ubox-value (ubox-find ubox)))

    (define (ubox-set! ubox val)
      (ubox-set-value! (ubox-find ubox) val))

    (define (ubox=? ubox1 ubox2)
      (eq? (ubox-find ubox1) (ubox-find ubox2)))

    (define (ubox-unify! proc ubox1 ubox2)
      (let ((value (proc (ubox-ref ubox1) (ubox-ref ubox2))))
        (ubox-union! ubox1 ubox2)
        (ubox-set! ubox1 value)))

    (define (ubox-union! ubox1 ubox2)
      (let ((root1 (ubox-find ubox1))
            (root2 (ubox-find ubox2)))
        (unless (eq? root1 root2)
          (cond ((< (ubox-rank root1) (ubox-rank root2))
                  (ubox-set-parent! root1 root2)
                  (ubox-set-value! root1 #f)
                  (ubox-set-rank! root1 #f))
                (else
                  (when (= (ubox-rank root1) (ubox-rank root2))
                    (ubox-set-rank! root1 (+ (ubox-rank root1) 1)))
                  (ubox-set-parent! root2 root1)
                  (ubox-set-value! root2 #f)
                  (ubox-set-rank! root2 #f))))))

    (define (ubox-link! ubox1 ubox2)
      (ubox-unify! (lambda (x y) y) ubox1 ubox2))
  )
)

