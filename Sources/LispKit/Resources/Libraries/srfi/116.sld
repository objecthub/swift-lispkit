;;; SRFI 116
;;; Immutable List Library
;;;
;;; Scheme currently does not provide immutable pairs corresponding to its existing mutable
;;; pairs, although most uses of pairs do not exploit their mutability. This SRFI takes the
;;; approach of leaving Scheme's pairs unchanged and providing a full set of routines for
;;; creating and dealing with immutable pairs. The library is a variant of SRFI 1 for
;;; immutable pairs (ipairs) and immutable lists (ilists). Procedure names are prefixed with
;;; "i". Only a few functional update procedures are provided.
;;;
;;; Copyright © 2014 John Cowan. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to the following
;;; conditions:
;;; The above copyright notice and this permission notice shall be included in all copies
;;; or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Implementation notes:
;;;   Pairs are immutable in LispKit. The SRFI recommends that on systems with immutable
;;;   quotation, quoted pairs are the immutable pairs of this SRFI, and that the standard list
;;;   operations accept immutable pairs. Therefore, the immutable pairs of this library are the
;;;   ordinary pairs of LispKit, and ilists are ordinary lists. Most procedures are SRFI 1
;;;   procedures exported under their SRFI 116 names. The only mutable pairs in LispKit are
;;;   those created by `mcons`; they are not ipairs, and the conversion procedures of this
;;;   library convert them into ipairs.

(define-library (srfi 116)

  ;; Syntax
  (export iq)

  ;; Constructors
  (export (rename cons ipair)
          (rename list ilist)
          (rename xcons xipair)
          (rename cons* ipair*)
          (rename make-list make-ilist)
          (rename list-tabulate ilist-tabulate)
          ilist-copy
          (rename iota iiota))

  ;; Predicates
  (export (rename pair? ipair?)
          (rename proper-list? proper-ilist?)
          (rename list? ilist?)
          (rename dotted-list? dotted-ilist?)
          (rename not-pair? not-ipair?)
          (rename null-list? null-ilist?)
          (rename list= ilist=))

  ;; Selectors
  (export (rename car icar) (rename cdr icdr)
          (rename caar icaar) (rename cadr icadr) (rename cdar icdar) (rename cddr icddr)
          (rename caaar icaaar) (rename caadr icaadr) (rename cadar icadar)
          (rename caddr icaddr) (rename cdaar icdaar) (rename cdadr icdadr)
          (rename cddar icddar) (rename cdddr icdddr)
          (rename caaaar icaaaar) (rename caaadr icaaadr) (rename caadar icaadar)
          (rename caaddr icaaddr) (rename cadaar icadaar) (rename cadadr icadadr)
          (rename caddar icaddar) (rename cadddr icadddr) (rename cdaaar icdaaar)
          (rename cdaadr icdaadr) (rename cdadar icdadar) (rename cdaddr icdaddr)
          (rename cddaar icddaar) (rename cddadr icddadr) (rename cdddar icdddar)
          (rename cddddr icddddr)
          (rename list-ref ilist-ref)
          (rename first ifirst) (rename second isecond) (rename third ithird)
          (rename fourth ifourth) (rename fifth ififth) (rename sixth isixth)
          (rename seventh iseventh) (rename eighth ieighth) (rename ninth ininth)
          (rename tenth itenth)
          (rename car+cdr icar+icdr)
          (rename take itake) (rename drop idrop) (rename drop ilist-tail)
          (rename take-right itake-right) (rename drop-right idrop-right)
          (rename split-at isplit-at)
          (rename last ilast) (rename last-pair last-ipair))

  ;; Miscellaneous: length, append, concatenate, reverse, zip & count
  (export (rename length ilength)
          (rename append iappend)
          (rename concatenate iconcatenate)
          (rename reverse ireverse)
          (rename append-reverse iappend-reverse)
          (rename zip izip)
          (rename unzip1 iunzip1) (rename unzip2 iunzip2) (rename unzip3 iunzip3)
          (rename unzip4 iunzip4) (rename unzip5 iunzip5)
          (rename count icount))

  ;; Fold, unfold & map
  (export (rename map imap)
          (rename for-each ifor-each)
          (rename fold ifold)
          (rename unfold iunfold)
          (rename pair-fold ipair-fold)
          (rename reduce ireduce)
          (rename fold-right ifold-right)
          (rename unfold-right iunfold-right)
          (rename pair-fold-right ipair-fold-right)
          (rename reduce-right ireduce-right)
          (rename append-map iappend-map)
          (rename pair-for-each ipair-for-each)
          (rename filter-map ifilter-map)
          (rename map-in-order imap-in-order))

  ;; Filtering & partitioning
  (export (rename filter ifilter)
          (rename partition ipartition)
          (rename remove iremove))

  ;; Searching
  (export (rename member imember) (rename memq imemq) (rename memv imemv)
          (rename find ifind)
          (rename find-tail ifind-tail)
          (rename any iany)
          (rename every ievery)
          (rename list-index ilist-index)
          (rename take-while itake-while)
          (rename drop-while idrop-while)
          (rename span ispan)
          (rename break ibreak))

  ;; Deleting
  (export (rename delete idelete)
          (rename delete-duplicates idelete-duplicates))

  ;; Immutable association lists
  (export (rename assoc iassoc) (rename assq iassq) (rename assv iassv)
          (rename alist-cons ialist-cons)
          (rename alist-delete ialist-delete))

  ;; Replacement
  (export replace-icar
          replace-icdr)

  ;; Conversion
  (export pair->ipair
          ipair->pair
          list->ilist
          ilist->list
          tree->itree
          itree->tree
          gtree->itree
          gtree->tree)

  ;; Procedure application
  (export (rename apply iapply))

  ;; Comparators
  (export ipair-comparator
          ilist-comparator
          make-ipair-comparator
          make-ilist-comparator
          make-improper-ilist-comparator
          make-icar-comparator
          make-icdr-comparator)

  (import (lispkit base)
          (srfi 1)
          (srfi 128))

  (begin

    ;; === Syntax ===

    ;; `(iq datum ...)` returns an ilist containing the given literals. Quoted pairs are
    ;; already immutable, so no conversion is necessary.
    (define-syntax iq
      (syntax-rules ()
        ((_ x ...) (list (quote x) ...))))

    ;; === Constructors ===

    ;; Copies the spine of a proper or dotted ilist, including the tail.
    (define (ilist-copy dilist)
      (let loop ((lst dilist) (acc '()))
        (if (pair? lst)
            (loop (cdr lst) (cons (car lst) acc))
            (let rev ((acc acc) (res lst))
              (if (null? acc)
                  res
                  (rev (cdr acc) (cons (car acc) res)))))))

    ;; === Replacement ===

    (define (replace-icar ipair obj)
      (if (pair? ipair)
          (cons obj (cdr ipair))
          (error "replace-icar: not an ipair:" ipair)))

    (define (replace-icdr ipair obj)
      (if (pair? ipair)
          (cons (car ipair) obj)
          (error "replace-icdr: not an ipair:" ipair)))

    ;; === Conversion ===

    ;; Ordinary pairs are ipairs; mutable pairs created by `mcons` are converted.
    (define (pair->ipair pair)
      (cond ((pair? pair) pair)
            ((mpair? pair) (cons (mcar pair) (mcdr pair)))
            (else (error "pair->ipair: not a pair:" pair))))

    (define (ipair->pair ipair)
      (if (pair? ipair)
          ipair
          (error "ipair->pair: not an ipair:" ipair)))

    ;; Converts the spine of a list built from pairs and mutable pairs into an ilist. The
    ;; elements and the tail of dotted lists are not converted. A list that contains no
    ;; mutable pairs is returned as is.
    (define (list->ilist flist)
      (let loop ((lst flist) (acc '()) (changed #f))
        (cond ((pair? lst)
               (loop (cdr lst) (cons (car lst) acc) changed))
              ((mpair? lst)
               (loop (mcdr lst) (cons (mcar lst) acc) #t))
              ((not changed)
               flist)
              (else
               (let rev ((acc acc) (res lst))
                 (if (null? acc)
                     res
                     (rev (cdr acc) (cons (car acc) res))))))))

    (define (ilist->list dilist) dilist)

    ;; Walks a tree of mutable pairs and returns a deep copy consisting of ipairs. Objects
    ;; that are not mutable pairs, including ipairs, are returned unchanged.
    (define (tree->itree obj)
      (if (mpair? obj)
          (convert-tree obj #f)
          obj))

    ;; Ipairs are the pairs of LispKit.
    (define (itree->tree obj) obj)

    ;; Walks a generalized tree consisting of ipairs and mutable pairs and returns a deep copy
    ;; consisting of ipairs only. Objects that are neither pairs nor mutable pairs are returned
    ;; unchanged.
    (define (gtree->itree obj)
      (if (or (pair? obj) (mpair? obj))
          (convert-tree obj #t)
          obj))

    (define gtree->tree gtree->itree)

    ;; Converts the mutable pairs of a tree into ipairs. Ipairs are converted as well
    ;; (generalized trees) or are left alone (if `general` is #f). The spine is processed
    ;; iteratively, so that long lists can be converted.
    (define (convert-tree obj general)
      (let loop ((obj obj) (acc '()))
        (cond ((mpair? obj)
               (loop (mcdr obj) (cons (convert-element (mcar obj) general) acc)))
              ((and general (pair? obj))
               (loop (cdr obj) (cons (convert-element (car obj) general) acc)))
              (else
               (let rev ((acc acc) (res obj))
                 (if (null? acc)
                     res
                     (rev (cdr acc) (cons (car acc) res))))))))

    (define (convert-element obj general)
      (cond ((mpair? obj) (convert-tree obj general))
            ((and general (pair? obj)) (convert-tree obj general))
            (else obj)))

    ;; === Comparators ===

    (define ipair-comparator
      (make-pair-comparator default-comparator default-comparator))

    (define ilist-comparator list-comparator)

    (define (make-ipair-comparator icar-comparator icdr-comparator)
      (make-pair-comparator icar-comparator icdr-comparator))

    (define (make-ilist-comparator comparator)
      (make-list-comparator comparator list? null? car cdr))

    ;; Returns a comparator for arbitrary objects: the empty list precedes all ipairs, which
    ;; precede all other objects. Ipairs are compared lexicographically, comparing the icars
    ;; with `comparator` and the icdrs with the improper ilist comparator itself, so that
    ;; the tails of ilists are compared as well. All other objects, including the tails of
    ;; dotted ilists, are compared with `comparator`.
    (define (make-improper-ilist-comparator comparator)
      (let ((elem-test (comparator-type-test-predicate comparator))
            (elem=? (comparator-equality-predicate comparator))
            (elem<? (comparator-ordering-predicate comparator))
            (elem-hash (comparator-hash-function comparator)))
        (define (rank obj)
          (cond ((null? obj) 0) ((pair? obj) 1) (else 2)))
        (make-comparator
          (lambda (obj)
            (let loop ((obj obj))
              (cond ((null? obj) #t)
                    ((pair? obj) (and (elem-test (car obj)) (loop (cdr obj))))
                    (else (elem-test obj)))))
          (lambda (a b)
            (let loop ((a a) (b b))
              (let ((ra (rank a)) (rb (rank b)))
                (cond ((not (= ra rb)) #f)
                      ((= ra 0) #t)
                      ((= ra 1) (and (elem=? (car a) (car b)) (loop (cdr a) (cdr b))))
                      (else (elem=? a b))))))
          (and (comparator-ordered? comparator)
               (lambda (a b)
                 (let loop ((a a) (b b))
                   (let ((ra (rank a)) (rb (rank b)))
                     (cond ((not (= ra rb)) (< ra rb))
                           ((= ra 0) #f)
                           ((= ra 1) (cond ((elem=? (car a) (car b)) (loop (cdr a) (cdr b)))
                                           (else (elem<? (car a) (car b)))))
                           (else (elem<? a b)))))))
          (and (comparator-hashable? comparator)
               (lambda (obj)
                 (let loop ((obj obj) (acc (hash-salt)))
                   (cond ((null? obj) acc)
                         ((pair? obj)
                          (loop (cdr obj)
                                (+ (modulo (* acc 33) (hash-bound)) (elem-hash (car obj)))))
                         (else
                          (+ (modulo (* acc 33) (hash-bound)) (elem-hash obj))))))))))

    ;; Returns a comparator for ipairs that only compares their icars.
    (define (make-icar-comparator comparator)
      (make-field-comparator comparator car))

    ;; Returns a comparator for ipairs that only compares their icdrs.
    (define (make-icdr-comparator comparator)
      (make-field-comparator comparator cdr))

    (define (make-field-comparator comparator field)
      (make-comparator
        (lambda (obj)
          (and (pair? obj)
               ((comparator-type-test-predicate comparator) (field obj))))
        (lambda (a b)
          ((comparator-equality-predicate comparator) (field a) (field b)))
        (and (comparator-ordered? comparator)
             (lambda (a b)
               ((comparator-ordering-predicate comparator) (field a) (field b))))
        (and (comparator-hashable? comparator)
             (lambda (obj)
               ((comparator-hash-function comparator) (field obj))))))
  )
)
