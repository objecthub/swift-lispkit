;;; SRFI 224
;;; Integer Mappings
;;;
;;; Integer maps, or `fxmappings`, are finite sets, where each element is an
;;; association between a fixnum (exact integer) key and an arbitrary Scheme
;;; object. They are similar to the general mappings of SRFI 146, but the
;;; restricted key-type allows implementations of fxmappings to benefit from
;;; optimized structures and algorithms.
;;; 
;;; This library provides a rich set of operations on fxmappings, including
;;; analogues of most of the forms provided by SRFI 146. Fxmappings have no
;;; intrinsic order, but may be treated as ordered sets, using the natural
;;; ordering on keys; a substantial sublibrary for working with fxmappings
;;; in this fashion is included.
;;; 
;;; Copyright © 2021 Wolfgang Corcoran-Mathe. All rights reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;; 
;;; Adaptation to LispKit
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(define-library (srfi 224)
  
  (export ;; Constructors
          fxmapping
          fxmapping-unfold
          fxmapping-accumulate
          alist->fxmapping
          alist->fxmapping/combinator
          ;; Predicates
          fxmapping?
          fxmapping-contains?
          fxmapping-empty?
          fxmapping-disjoint?
          ;; Accessors
          fxmapping-min
          fxmapping-max
          fxmapping-ref
          fxmapping-ref/default
          ;; Updaters
          fxmapping-adjoin
          fxmapping-adjoin/combinator
          fxmapping-adjust
          fxmapping-set
          fxmapping-delete
          fxmapping-delete-all
          fxmapping-alter
          fxmapping-update
          fxmapping-delete-min
          fxmapping-delete-max
          fxmapping-update-min
          fxmapping-update-max
          fxmapping-pop-min
          fxmapping-pop-max
          ;; The whole fxmapping
          fxmapping-size
          fxmapping-count
          fxmapping-any?
          fxmapping-find
          fxmapping-every?
          ;; Traversal
          fxmapping-fold
          fxmapping-fold-right
          fxmapping-map
          fxmapping-map->list
          fxmapping-for-each
          fxmapping-relation-map
          ;; Filter
          fxmapping-filter
          fxmapping-remove
          fxmapping-partition
          ;; Copying and conversion
          fxmapping-keys
          fxmapping-values
          fxmapping->alist
          fxmapping->decreasing-alist
          fxmapping->generator
          fxmapping->decreasing-generator
          ;; Comparison
          fxmapping=?
          fxmapping<?
          fxmapping>?
          fxmapping<=?
          fxmapping>=?
          ;; Set theory operations
          fxmapping-union
          fxmapping-intersection
          fxmapping-difference
          fxmapping-xor
          fxmapping-union/combinator
          fxmapping-intersection/combinator
          ;; Submappings
          fxmapping-open-interval
          fxmapping-closed-interval
          fxmapping-open-closed-interval
          fxmapping-closed-open-interval
          fxsubmapping=
          fxsubmapping<
          fxsubmapping<=
          fxsubmapping>=
          fxsubmapping>
          fxmapping-split)
  
  (import (scheme base)
          (scheme case-lambda)
          (only (srfi 1) fold every)
          (only (srfi 128) comparator? =?)
          (srfi 143)
          (srfi 145)
          (only (srfi 158) make-coroutine-generator))

  (begin
    ;;; Pattern-matching macro for trie values.
    ;;;
    ;;; Based on William Byrd's pmatch modification of Oleg Kiselyov's
    ;;; simple linear pattern-matcher.
    
    ;;; Syntax:
    ;;;
    ;;; (tmatch exp <clause> ... [<else-clause>])
    ;;;
    ;;; <clause>      ::= (<pattern> [<guard>] exp ...)
    ;;; <else-clause> ::= (else exp ...)
    ;;; <guard>       ::= (guard boolean-exp ...)
    ;;; <pattern>     ::= empty
    ;;;                 | (leaf <pattern> <pattern>)
    ;;;                 | (branch <pattern> <pattern> <pattern> <pattern>)
    
    ;; (define-syntax tmatch
    ;;   (syntax-rules (else)
    ;;     ((tmatch exp (e ...) ...)
    ;;      (tmatch-aux #f exp (e ...) ...))
    ;;     ((tmatch name exp (e ...) ...)
    ;;      (tmatch-aux name exp (e ...) ...))))
    
    (define-syntax tmatch
      (syntax-rules (else guard)
        ((tmatch (f x ...) cs ...)
         (let ((v (f x ...)))
           (tmatch v cs ...)))
        ((tmatch v)
         (error "tmatch: no clause matched" v))
        ((tmatch _ (else e0 e1 ...)) (begin e0 e1 ...))
        ((tmatch v (pat (guard g ...) e0 e1 ...) cs ...)
         (let ((fk (lambda () (tmatch v cs ...))))
           (tpat v pat (if (and g ...) (begin e0 e1 ...) (fk)) (fk))))
        ((tmatch v (pat e0 e1 ...) cs ...)
         (let ((fk (lambda () (tmatch v cs ...))))
           (tpat v pat (begin e0 e1 ...) (fk))))))
    
    ;; Uses pmatch's `ppat' auxilliary macro, see below.
    (define-syntax tpat
      (syntax-rules (empty leaf branch unquote)
        ((tpat v empty kt kf) (if v kf kt))
        ((tpat v (leaf pkey pval) kt kf)
         (if (leaf? v)
             (let ((key (leaf-key v)) (value (leaf-value v)))
               (ppat key pkey (ppat value pval kt kf) kf))
             kf))
        ((tpat v (branch pp pm pl pr) kt kf)
         (if (branch? v)
             (let ((pfx (branch-prefix v))
                   (bit (branch-branching-bit v))
                   (left (branch-left v))
                   (right (branch-right v)))
               (ppat pfx
                     pp
                     (ppat bit pm (ppat left pl (ppat right pr kt kf) kf) kf)
                     kf))
             kf))))
    
    ;; Shorthands for a unary function that immediately pattern-matches
    ;; its trie parameter.
    (define-syntax tmatch-lambda
      (syntax-rules ()
        ((tmatch-lambda cs ...)
         (lambda (arg) (tmatch arg cs ...)))))
    
    ;;; pmatch, by Oleg Kiselyov, rev. Will Byrd.
    ;;; The original public-domain code can be found at
    ;;; http://okmij.org/ftp/Scheme/match-case-simple.scm
    
    ;; This is a new version of pmatch (August 8, 2012).
    ;; It has two important new features:
    ;; 1.  It allows for a name to be given to the pmatch if an error ensues.
    ;; 2.  A line from the specification has been removed. (see below).  Without
    ;; that line removed, it was impossible for a pattern to be (quote ,x),
    ;; which might be worth having especially when we write an interpreter
    ;; for Scheme, which includes quote as a language form.
    
    ;;; Code written by Oleg Kiselyov
    ;; (http://pobox.com/~oleg/ftp/)
    ;;;
    ;;; Taken from leanTAP.scm
    ;;; http://kanren.cvs.sourceforge.net/kanren/kanren/mini/leanTAP.scm?view=log
    
    ; A simple linear pattern matcher
    ; It is efficient (generates code at macro-expansion time) and simple:
    ; it should work on any R5RS (and R6RS) Scheme system.
    
    ; (pmatch exp <clause> ...[<else-clause>])
    ; <clause> ::= (<pattern> <guard> exp ...)
    ; <else-clause> ::= (else exp ...)
    ; <guard> ::= boolean exp | ()
    ; <pattern> :: =
    ;        ,var  -- matches always and binds the var
    ;                 pattern must be linear! No check is done
    ;         _    -- matches always
    ;        'exp  -- comparison with exp (using equal?)    REMOVED (August 8, 2012)
    ;        exp   -- comparison with exp (using equal?)
    ;        (<pattern1> <pattern2> ...) -- matches the list of patterns
    ;        (<pattern1> . <pattern2>)  -- ditto
    ;        ()    -- matches the empty list
    
    ;; We've removed the name parameter for now, since it seems to cause
    ;; problems for the expander in many Schemes.
    
    ;; (define-syntax pmatch
    ;;   (syntax-rules (else guard)
    ;;     ((pmatch v (e ...) ...)
    ;;      (pmatch-aux #f v (e ...) ...))
    ;;     ((pmatch v name (e ...) ...)
    ;;      (pmatch-aux name v (e ...) ...))))
    
    (define-syntax pmatch
      (syntax-rules (else guard)
        ((pmatch (rator rand ...) cs ...)
         (let ((v (rator rand ...)))     ; avoid multiple evals
           (pmatch v cs ...)))
        ((pmatch v)  ; no more clauses
         (error "pmatch failed" v))
        ((pmatch _ (else e0 e ...)) (begin e0 e ...))
        ((pmatch v (pat (guard g ...) e0 e ...) cs ...)
         (let ((fk (lambda () (pmatch v cs ...))))
           (ppat v pat (if (and g ...) (begin e0 e ...) (fk)) (fk))))
        ((pmatch v (pat e0 e ...) cs ...)
         (let ((fk (lambda () (pmatch v cs ...))))
           (ppat v pat (begin e0 e ...) (fk))))))
    
    (define-syntax ppat
      (syntax-rules (? unquote)
        ((ppat _ ? kt _) kt)  ; the ? wildcard always matches
        ((ppat v () kt kf) (if (null? v) kt kf))
    ;   ((ppat v (quote lit) kt kf) (if (equal? v (quote lit)) kt kf))
        ((ppat v (unquote var) kt _) (let ((var v)) kt))
        ((ppat v (x . y) kt kf)
         (if (pair? v)
           (let ((vx (car v)) (vy (cdr v)))
    	 (ppat vx x (ppat vy y kt kf) kf))
           kf))
        ((ppat v lit kt kf) (if (equal? v (quote lit)) kt kf))))
    
    ;;; Shorthands for functions that immediately pattern-match their
    ;;; parameter(s).
    
    ;; One-argument form.
    (define-syntax pmatch-lambda
      (syntax-rules ()
        ((pmatch-lambda cs ...)
         (lambda (arg) (pmatch arg cs ...)))))
    
    ;; Multi-argument form.
    (define-syntax pmatch-lambda*
      (syntax-rules ()
        ((pmatch-lambda* cs ...)
         (lambda args (pmatch args cs ...)))))
  )
  
  (begin
    ;;; This file implements integer maps as big-endian binary radix
    ;;; trees (AKA Patricia tries), as described by Chris Okasaki and
    ;;; Andrew Gill in "Fast Mergeable Integer Maps" (1998).  Integers
    ;;; in big-endian binary encoding are stored in a trie structure
    ;;; which allows fast lookup, insertion, and set-theoretical
    ;;; operations (union, intersection, etc.)
    ;;;
    ;;; A trie is represented by #f (the empty trie), a leaf, or a branch.
    ;;;
    ;;; Throughout this code, the empty trie (#f) is always returned
    ;;; as an explicit value, not, e.g. as the default value of an
    ;;; (and ...) expression, to clarify its use as a trie value.
    
    ;;;; Utility
    
    (define (swap-last-args proc)
      (lambda (k x y) (proc k y x)))
    
    (define the-empty-trie #f)
    
    (define (trie-empty? t) (not t))
    
    (define-record-type <leaf>
      (leaf key value)
      leaf?
      (key leaf-key)
      (value leaf-value))
    
    (define-record-type <branch>
      (raw-branch prefix branching-bit left right)
      branch?
      (prefix branch-prefix)
      (branching-bit branch-branching-bit)
      (left branch-left)
      (right branch-right))
    
    (define (valid-integer? x) (fixnum? x))
    
    ;; Zero the bits of k at and below (BE) the set bit of m.
    (define (mask k m)
      (if (fx=? m fx-least)
          0
          (fxand k (fxxor (fxnot (fx- m 1)) m))))
    
    ;; Does the m-masked prefix of k match p?
    (define (match-prefix? k p m)
      (fx=? (mask k m) p))
    
    (define (branching-bit p1 m1 p2 m2)
      (if (fxnegative? (fxxor p1 p2))
          fx-least        ; different signs
          (highest-bit-mask (fxxor p1 p2) (fxmax 1 (fx* 2 (fxmax m1 m2))))))
    
    ;; Two's-complement trick.
    (define (lowest-set-bit b)
      (fxand b (fxneg b)))
    
    (define (highest-bit-mask k guess-m)
      (let lp ((x (fxand k (fxnot (fx- guess-m 1)))))
        (let ((m (lowest-set-bit x)))
          (if (fx=? x m)
              m
              (lp (fx- x m))))))
    
    (define (zero-bit? k m)
      (fxzero? (fxand k m)))
    
    ;; Insert the association (key, value) into trie, replacing any old
    ;; association.
    (define (trie-insert trie key value)
      (trie-insert/combine trie key value (lambda (_k new _) new)))
    
    ;; Insert the association (key, value) into trie, preserving any old
    ;; association.
    (define (trie-adjoin trie key value)
      (trie-insert/combine trie key value (lambda (_k _new old) old)))
    
    ;; Insert (key, value) into trie if key doesn't already have an
    ;; association.  If it does, add a new association for key and
    ;; the result of calling combine on the key, new, and old values.
    (define (trie-insert/combine trie key value combine)
      (letrec
       ((new-leaf (leaf key value))
        (insert
         (lambda (t)
           (tmatch t
             (empty (leaf key value))
             ((leaf ,k ,v)
              (if (fx=? key k)
                  (leaf k (combine k value v))
                  (trie-join key 0 new-leaf k 0 t)))
             ((branch ,p ,m ,l ,r)
              (if (match-prefix? key p m)
                  (if (zero-bit? key m)
                      (branch p m (insert l) r)
                      (branch p m l (insert r)))
                  (trie-join key 0 new-leaf p m t)))))))
        (assume (valid-integer? key) "invalid key")
        (insert trie)))
    
    (define (trie-join prefix1 mask1 trie1 prefix2 mask2 trie2)
      (let ((m (branching-bit prefix1 mask1 prefix2 mask2)))
        (if (zero-bit? prefix1 m)
            (branch (mask prefix1 m) m trie1 trie2)
            (branch (mask prefix1 m) m trie2 trie1))))
    
    ;; If (key, value) is an association in trie, then replace it
    ;; with (key, (proc key value)).  Otherwise, return a copy of trie.
    (define (trie-adjust trie key proc)
      (letrec
       ((update
         (lambda (t)
           (tmatch t
             (empty t)
             ((leaf ,k ,v)
              (if (fx=? key k) (leaf k (proc k v)) t))
             ((branch ,p ,m ,l ,r)
              (if (match-prefix? key p m)
                  (if (zero-bit? key m)
                      (branch p m (update l) r)
                      (branch p m l (update r)))
                  t))))))
        (update trie)))
    
    (define (trie-update trie key proc failure wrapper)
      (letrec
       ((update
         (lambda (t build)
           (tmatch t
             (empty (failure))
             ((leaf ,k ,v)
              (if (fx=? key k)
                  (proc k
                        v
                        (lambda (v*) (wrapper (build (leaf k v*))))
                        (lambda () (wrapper (build the-empty-trie))))
                  (failure)))
             ((branch ,p ,m ,l ,r)
              (if (match-prefix? key p m)
                  (if (zero-bit? key m)
                      (update l (lambda (l*) (build (branch p m l* r))))
                      (update r (lambda (r*) (build (branch p m l r*)))))
                  (failure)))))))
        (update trie values)))
    
    (define (trie-alter trie key failure success wrapper)
      (letrec
       ((update
         (lambda (t build)
           (tmatch t
             (empty
              (failure (lambda (v)
                         (wrapper (build (leaf key v))))     ; insert
                       (lambda ()
                         (wrapper (build the-empty-trie))))) ; ignore
             ((leaf ,k ,v)
              (if (fx=? key k)
                  (success k
                           v
                           (lambda (v*)                      ; replace
                             (wrapper (build (leaf k v*))))
                           (lambda ()                        ; delete
                             (wrapper (build the-empty-trie))))
                  (failure (lambda (u)                       ; insert
                             (wrapper
                              (build (trie-join key 0 (leaf key u) k 0 t))))
                           (lambda ()                        ; ignore
                             (wrapper (build t))))))
             ((branch ,p ,m ,l ,r)
              (if (match-prefix? key p m)
                  (if (zero-bit? key m)
                      (update l (lambda (l*)
                                  (build (branch p m l* r))))
                      (update r (lambda (r*)
                                  (build (branch p m l r*)))))
                  (failure (lambda (v)                    ; insert
                             (wrapper
                              (build (trie-join key 0 (leaf key v) p m t))))
                           (lambda ()                     ; ignore
                             (wrapper (build t))))))))))
        (update trie values)))
    
    ;; If `key' has an association in `trie', then call `success' with
    ;; on the associated value.  Otherwise, call `failure'.
    (define (trie-assoc trie key failure success)
      (letrec
       ((search
         (tmatch-lambda
           ((leaf ,k ,v) (guard (fx=? k key)) (success v))
           ((branch ,p ,m ,l ,r) (guard (match-prefix? key p m))
            (if (zero-bit? key m) (search l) (search r)))
           (else (failure)))))
        (search trie)))
    
    ;; If `key' has an association in `trie', then return the associated
    ;; value.  Otherwise, return `default'.
    (define (trie-assoc/default trie key default)
      (letrec
       ((search
         (tmatch-lambda
           ((leaf ,k ,v) (guard (fx=? k key)) v)
           ((branch ,p ,m ,l ,r) (guard (match-prefix? key p m))
            (if (zero-bit? key m) (search l) (search r)))
           (else default))))
        (search trie)))
    
    ;; Return the number of associations in trie.
    (define (trie-size trie)
      (if (trie-empty? trie)
          0
          (let lp ((n 0) (t trie) (kont values))
            (cond ((leaf? t) (kont (+ n 1)))
                  (else (lp n
                            (branch-left t)
                            (lambda (m)
                              (lp m (branch-right t) kont))))))))
    
    (define (trie-contains? trie key)
      (letrec
       ((search
         (tmatch-lambda
           ((leaf ,k ,v) (fx=? k key))
           ((branch ,p ,m ,l ,r) (guard (match-prefix? key p m))
            (if (zero-bit? key m) (search l) (search r)))
           (else #f))))
        (search trie)))
    
    (define (trie-find pred trie failure success)
      (letrec
       ((search
         (lambda (t kont)
           (tmatch t
             ((leaf ,k ,v) (guard (pred k v)) (success k v))
             ((branch ? ? ,l ,r) (search l (lambda () (search r kont))))
             (else (kont))))))
        (tmatch trie
          ((branch ? ,m ,l ,r) (guard (negative? m))
           (search r (lambda () (search l failure))))
          (else (search trie failure)))))
    
    (define (branching-bit-higher? mask1 mask2)
      (if (negative? (fxxor mask1 mask2))  ; signs differ
          (negative? mask1)
          (fx>? mask1 mask2)))
    
    ;; Merge two tries.  `combine' is used to merge duplicated mappings.
    (define (trie-merge combine trie1 trie2)
      (letrec
        ((merge
          (lambda (s t)
            (cond ((trie-empty? s) t)
                  ((trie-empty? t) s)
                  ((leaf? s)
                   (trie-insert/combine t (leaf-key s) (leaf-value s) combine))
                  ((leaf? t)
                   (trie-insert/combine s
                                        (leaf-key t)
                                        (leaf-value t)
                                        (swap-last-args combine)))
                  ((and (branch? s) (branch? t)) (merge-branches s t)))))
         (merge-branches
          (lambda (s t)
            (tmatch s
              ((branch ,p ,m ,s1 ,s2)
               (tmatch t
                 ((branch ,q ,n ,t1 ,t2)
                  (cond ((and (fx=? m n) (fx=? p q))
                         (branch p m (merge s1 t1) (merge s2 t2)))
                        ((and (branching-bit-higher? m n)
                              (match-prefix? q p m))
                         (if (zero-bit? q m)
                             (branch p m (merge s1 t) s2)
                             (branch p m s1 (merge s2 t))))
                        ((and (branching-bit-higher? n m)
                              (match-prefix? p q n))
                         (if (zero-bit? p n)
                             (branch q n (merge s t1) t2)
                             (branch q n t1 (merge s t2))))
                        (else (trie-join p m s q n t))))))))))
        (merge trie1 trie2)))
    
    ;; Construct a branch only if the subtrees are non-empty.
    (define (branch prefix mask trie1 trie2)
      (cond ((not trie1) trie2)
            ((not trie2) trie1)
            (else (raw-branch prefix mask trie1 trie2))))
    
    (define (trie-union s t)
      (trie-merge trie-insert s t))
    
    (define (trie-partition pred trie)
      (letrec
       ((part
         (lambda (t)
           (tmatch t
             (empty (values the-empty-trie the-empty-trie))
             ((leaf ,k ,v)
              (if (pred k v)
                  (values t the-empty-trie)
                  (values the-empty-trie t)))
             ((branch ,p ,m ,l ,r)
              (let-values (((il ol) (part l))
                           ((ir or) (part r)))
                (values (branch p m il ir) (branch p m ol or))))))))
        (part trie)))
    
    ;;;; Map and fold
    
    (define (trie-map proc trie)
      (letrec
       ((tmap
         (tmatch-lambda
           (empty the-empty-trie)
           ((leaf ,k ,v)
            (leaf k (proc k v)))
           ((branch ,p ,m ,l ,r)
            (branch p m (tmap l) (tmap r))))))
        (tmap trie)))
    
    (define (trie-fold-left proc nil trie)
      (if (trie-empty? trie)
          nil
          (let lp ((t trie) (b nil) (kont values))
            (if (leaf? t)
                (kont (proc (leaf-key t) (leaf-value t) b))
                (lp (branch-left t)
                    b
                    (lambda (c)
                      (lp (branch-right t) c kont)))))))
    
    (define (trie-fold-right proc nil trie)
      (if (trie-empty? trie)
          nil
          (let lp ((t trie) (b nil) (kont values))
            (if (leaf? t)
                (kont (proc (leaf-key t) (leaf-value t) b))
                (lp (branch-right t)
                    b
                    (lambda (c)
                      (lp (branch-left t) c kont)))))))
    
    (define (trie-filter pred trie)
      (letrec ((filter
                (lambda (t)
                  (tmatch t
                    (empty the-empty-trie)
                    ((leaf ,k ,v) (guard (pred k v)) t)
                    ((leaf ? ?) the-empty-trie)
                    ((branch ,p ,m ,l ,r)
                     (branch p m (filter l) (filter r)))))))
        (filter trie)))
    
    (define (trie-min trie)
      (letrec
       ((search
         (tmatch-lambda
           ((leaf ,k ,v) (values k v))
           ((branch ? ? ,l ?) (search l)))))
        (tmatch trie
          (empty (error "empty trie"))
          ((leaf ,k ,v) (values k v))
          ((branch ? ,m ,l ,r)
           (if (fxnegative? m) (search r) (search l))))))
    
    (define (trie-update-min trie success wrapper)
      (letrec
       ((update
         (lambda (t build)
           (tmatch t
             (empty (wrapper (build the-empty-trie)))
             ((leaf ,k ,v)
              (success k
                       v
                       (lambda (v*)
                         (wrapper (build (leaf k v*))))
                       (lambda ()
                         (wrapper (build the-empty-trie)))))
             ((branch ,p ,m ,l ,r)
              (update l (lambda (l*)
                          (build (branch p m l* r)))))))))
        (tmatch trie
          ((branch ,p ,m ,l ,r)
           (if (negative? m)
               (update r (lambda (r*) (branch p m l r*)))
               (update l (lambda (l*) (branch p m l* r)))))
          (else (update trie values)))))
    
    (define (trie-pop-min trie)
      (letrec
       ((pop
         (tmatch-lambda
           (empty (error "trie-pop-min: empty trie"))
           ((leaf ,k ,v) (values k v the-empty-trie))
           ((branch ,p ,m ,l ,r)
            (let-values (((k v l*) (pop l)))
              (values k v (branch p m l* r)))))))
        (tmatch trie
          ((branch ,p ,m ,l ,r)
           (if (fxnegative? m)
               (let-values (((k v r*) (pop r)))
                 (values k v (branch p m l r*)))
               (let-values (((k v l*) (pop l)))
                 (values k v (branch p m l* r)))))
          (else (pop trie)))))
    
    (define (trie-max trie)
      (letrec
       ((search
         (tmatch-lambda
           ((leaf ,k ,v) (values k v))
           ((branch ? ? ? ,r) (search r)))))
        (tmatch trie
          (empty (error "empty trie"))
          ((branch ? ,m ,l ,r)
           (if (fxnegative? m) (search l) (search r)))
          ((leaf ,k ,v) (values k v)))))
    
    (define (trie-update-max trie success wrapper)
      (letrec
       ((update
         (lambda (t build)
           (tmatch t
             (empty (wrapper (build the-empty-trie)))
             ((leaf ,k ,v)
              (success k
                       v
                       (lambda (v*)
                         (wrapper (build (leaf k v*))))
                       (lambda ()
                         (wrapper (build the-empty-trie)))))
             ((branch ,p ,m ,l ,r)
              (update r (lambda (r*)
                          (build (branch p m l r*)))))))))
        (tmatch trie
          ((branch ,p ,m ,l ,r)
           (if (negative? m)
               (update l (lambda (l*) (branch p m l* r)))
               (update r (lambda (r*) (branch p m l r*)))))
          (else (update trie values)))))
    
    (define (trie-pop-max trie)
      (letrec
       ((pop
         (tmatch-lambda
           (empty (error "trie-pop-max: empty trie"))
           ((leaf ,k ,v) (values k v the-empty-trie))
           ((branch ,p ,m ,l ,r)
            (let-values (((k v r*) (pop r)))
              (values k v (branch p m l r*)))))))
        (tmatch trie
          ((branch ,p ,m ,l ,r)
           (if (fxnegative? m)
               (let-values (((k v l*) (pop l)))
                 (values k v (branch p m l* r)))
               (let-values (((k v r*) (pop r)))
                 (values k v (branch p m l r*)))))
          (else (pop trie)))))
    
    ;;;; Comparisons
    
    (define (trie=? comp trie1 trie2)
      (let loop ((s trie1) (t trie2))
        (cond ((and (trie-empty? s) (trie-empty? t)) #t)
              ((leaf? s)
               (and (leaf? t)
                    (fx=? (leaf-key s) (leaf-key t))
                    (=? comp (leaf-value s) (leaf-value t))))
              ((and (branch? s) (branch? t))
               (tmatch s
                ((branch ,p ,m ,l1 ,r1)
                 (tmatch t
                   ((branch ,q ,n ,l2 ,r2)
                    (and (fx=? m n)
                         (fx=? p q)
                         (loop l1 l2)
                         (loop r1 r2)))))))
              (else #f))))
    
    ;; Returns the symbol 'less' if trie1 is a proper subset of trie2,
    ;; 'equal' if they are the same, and 'greater' otherwise.  NB that
    ;; disjoint mappings will compare as greater.
    (define (trie-subset-compare comp trie1 trie2)
      (letrec
       ((compare
         (lambda (s t)
           (cond ((eqv? s t) 'equal)
                 ((trie-empty? s) 'less)
                 ((trie-empty? t) 'greater)  ; disjoint
                 ((and (leaf? s) (leaf? t))
                  (if (and (fx=? (leaf-key s) (leaf-key t))
                           (=? comp (leaf-value s) (leaf-value t)))
                      'equal
                      'greater))
                 ((leaf? s)             ; leaf / branch
                  (tmatch t
                    ((branch ,p ,m ,l ,r)
                     (let ((k (leaf-key s)))
                       (if (match-prefix? k p m)
                           (case (compare s (if (zero-bit? k m) l r))
                             ((greater) 'greater)
                             (else 'less)))))))
                 ((leaf? t) 'greater)   ; branch / leaf
                 (else (compare-branches s t)))))
        (compare-branches
         (lambda (s t)
           (tmatch s
             ((branch ,p ,m ,sl ,sr)
              (tmatch t
                ((branch ,q ,n ,tl ,tr)
                 (cond ((branching-bit-higher? m n) 'greater)
                       ((branching-bit-higher? n m)
                        (if (match-prefix? p q n)
                            (let ((res (if (zero-bit? p n)
                                           (compare s tl)
                                           (compare s tr))))
                              (if (eqv? res 'greater) res 'less))
                            'greater))
                       ((fx=? p q)  ; same prefix, compare subtrees
                        (let ((cl (compare sl tl)) (cr (compare sr tr)))
                          (cond ((or (eqv? cl 'greater) (eqv? cr 'greater))
                                 'greater)
                                ((and (eqv? cl 'equal) (eqv? cr 'equal))
                                 'equal)
                                (else 'less))))
                       (else 'greater)))))))))  ; disjoint
        (compare trie1 trie2)))
    
    (define (trie-proper-subset? comp trie1 trie2)
      (eqv? (trie-subset-compare comp trie1 trie2) 'less))
    
    ;; Two tries are disjoint if they have no keys in common.
    (define (trie-disjoint? trie1 trie2)
      (letrec
       ((disjoint?
         (lambda (s t)
           (or (trie-empty? s)
               (trie-empty? t)
               (cond ((leaf? s)
                      (let ((k (leaf-key s)))
                        (if (leaf? t)
                            (not (fx=? k (leaf-key t)))
                            (not (trie-contains? t k)))))
                     ((leaf? t) (not (trie-contains? s (leaf-key t))))
                     (else (branches-disjoint? s t))))))
        (branches-disjoint?
         (lambda (s t)
           (tmatch s
             ((branch ,p ,m ,sl ,sr)
              (tmatch t
                ((branch ,q ,n ,tl ,tr)
                 (cond ((and (fx=? m n) (fx=? p q))
                        (and (disjoint? sl tl) (disjoint? sr tr)))
                       ((and (branching-bit-higher? m n)
                             (match-prefix? q p m))
                        (if (zero-bit? q m)
                            (disjoint? sl t)
                            (disjoint? sr t)))
                       ((and (branching-bit-higher? n m)
                             (match-prefix? p q n))
                        (if (zero-bit? p n)
                            (disjoint? s tl)
                            (disjoint? s tr)))
                       (else #t)))))))))      ; the prefixes disagree
        (disjoint? trie1 trie2)))
    
    (define (trie-delete trie key)
      (letrec
       ((update
         (lambda (t)
           (tmatch t
             (empty the-empty-trie)
             ((leaf ,k ?) (if (fx=? k key) the-empty-trie t))
             ((branch ,p ,m ,l ,r) (guard (match-prefix? key p m))
              (if (zero-bit? key m)
                   (branch p m (update l) r)
                   (branch p m l (update r))))
             (else t)))))  ; key doesn't occur in t
        (update trie)))
    
    (define (trie-intersection combine trie1 trie2)
      (letrec
       ((intersect
         (lambda (s t)
           (cond ((or (trie-empty? s) (trie-empty? t)) the-empty-trie)
                 ((and (leaf? s) (leaf? t))
                  (tmatch s
                    ((leaf ,sk ,sv)
                     (tmatch t
                       ((leaf ,tk ,tv) (guard (fx=? sk tk))
                        (leaf sk (combine sk sv tv)))
                       (else the-empty-trie)))))
                 ((leaf? s) (insert-leaf combine s t))
                 ((leaf? t) (insert-leaf (swap-last-args combine) t s))
                 (else (intersect-branches s t)))))
        (insert-leaf
         (lambda (comb lf t)
           (tmatch lf
             ((leaf ,k ,v)
              (let lp ((t t))
                (tmatch t
                  ((leaf ,tk ,tv) (guard (fx=? k tk))
                   (leaf k (comb k v tv)))
                  ((leaf ? ?) the-empty-trie)
                  ((branch ,p ,m ,l ,r)
                   (and (match-prefix? k p m)
                        (if (zero-bit? k m) (lp l) (lp r))))
                  (else the-empty-trie)))))))
        (intersect-branches
         (lambda (s t)
           (tmatch s
             ((branch ,p ,m ,sl ,sr)
              (tmatch t
                ((branch ,q ,n ,tl ,tr)
                 (cond ((branching-bit-higher? m n)
                        (and (match-prefix? q p m)
                             (if (zero-bit? q m)
                                 (intersect sl t)
                                 (intersect sr t))))
                       ((branching-bit-higher? n m)
                        (and (match-prefix? p q n)
                             (if (zero-bit? p n)
                                 (intersect s tl)
                                 (intersect s tr))))
                       ((fx=? p q)
                        (branch p m (intersect sl tl) (intersect sr tr)))
                       (else the-empty-trie)))))))))
        (intersect trie1 trie2)))
    
    (define (trie-difference trie1 trie2)
      (letrec
       ((difference
         (lambda (s t)
           (cond ((trie-empty? s) the-empty-trie)
                 ((trie-empty? t) s)
                 ((leaf? s)
                  (trie-assoc t
                              (leaf-key s)
                              (lambda () s)
                              (lambda (_) the-empty-trie)))
                 ((leaf? t) (trie-delete s (leaf-key t)))
                 (else (branch-difference s t)))))
        (branch-difference
         (lambda (s t)
           (tmatch s
             ((branch ,p ,m ,sl ,sr)
              (tmatch t
                ((branch ,q ,n ,tl ,tr)
                 (cond ((and (fx=? m n) (fx=? p q))
                        (branch p m (difference sl tl) (difference sr tr)))
                       ((and (branching-bit-higher? m n)
                             (match-prefix? q p m))
                        (if (zero-bit? q m)
                            (branch p m (difference sl t) sr)
                            (branch p m sl (difference sr t))))
                       ((and (branching-bit-higher? n m)
                             (match-prefix? p q n))
                        (if (zero-bit? p n)
                            (difference s tl)
                            (difference s tr)))
                       (else s)))))))))
        (difference trie1 trie2)))
    
    ;; Remove the assoc for key if it exists in trie; otherwise, add the
    ;; assoc (key, value).
    (define (%trie-insert-xor trie key value)
      (trie-alter trie
                  key
                  (lambda (insert _ig) (insert value))
                  (lambda (_k _v _rep delete) (delete))
                  values))
    
    (define (trie-xor trie1 trie2)
      (letrec
        ((merge
          (lambda (s t)
            (cond ((trie-empty? s) t)
                  ((trie-empty? t) s)
                  ((leaf? s)
                   (%trie-insert-xor t (leaf-key s) (leaf-value s)))
                  ((leaf? t)
                   (%trie-insert-xor s (leaf-key t) (leaf-value t)))
                  (else (merge-branches s t)))))
         (merge-branches
          (lambda (s t)
            (tmatch s
              ((branch ,p ,m ,s1 ,s2)
               (tmatch t
                 ((branch ,q ,n ,t1 ,t2)
                  (cond ((and (fx=? m n) (fx=? p q))
                         (branch p m (merge s1 t1) (merge s2 t2)))
                        ((and (branching-bit-higher? m n) (match-prefix? q p m))
                         (if (zero-bit? q m)
                             (branch p m (merge s1 t) s2)
                             (branch p m s1 (merge s2 t))))
                        ((and (branching-bit-higher? n m) (match-prefix? p q n))
                         (if (zero-bit? p n)
                             (branch q n (merge s t1) t2)
                             (branch q n t1 (merge s t2))))
                        (else
                         (trie-join p m s q n t))))))))))
        (merge trie1 trie2)))
    
    ;; Return a trie containing all the elements of `trie' which are
    ;; less than k, if `inclusive' is false, or less than or equal to
    ;; k if `inclusive' is true.
    (define (subtrie< trie k inclusive)
      (letrec
        ((split
          (lambda (t)
            (tmatch t
              (empty the-empty-trie)
              ((leaf ,tk ?)
               (cond ((fx<? tk k) t)
                     ((and (fx=? tk k) inclusive) t)
                     (else the-empty-trie)))
              ((branch ,p ,m ,l ,r)
               (if (match-prefix? k p m)
                   (if (zero-bit? k m)
                       (split l)
                       (trie-union l (split r)))
                   (and (fx<? p k) t)))))))
        (tmatch trie
          ((branch ? ,m ,l ,r) (guard (fxnegative? m))
           (if (fxnegative? k) (split r) (trie-union (split l) r)))
          (else (split trie)))))
    
    ;; Return a trie containing all the elements of `trie' which are
    ;; greater than k, if `inclusive' is false, or greater than or equal
    ;; to k if `inclusive' is true.
    (define (subtrie> trie k inclusive)
      (letrec
       ((split
         (lambda (t)
           (tmatch t
             (empty the-empty-trie)
             ((leaf ,tk ?)
              (cond ((fx>? tk k) t)
                    ((and (fx=? tk k) inclusive) t)
                    (else the-empty-trie)))
             ((branch ,p ,m ,l ,r)
              (if (match-prefix? k p m)
                  (if (zero-bit? k m)
                      (trie-union (split l) r)
                      (split r))
                  (and (fx>? p k) t)))))))
        (tmatch trie
          ((branch ? ,m ,l ,r) (guard (fxnegative? m))
           (if (fxnegative? k) (trie-union (split r) l) (split l)))
          (else (split trie)))))
    
    ;; Return a trie containing all the elements of `trie' which are
    ;; greater than/greater than or equal to a and less than/less than
    ;; or equal to b, depending on the truth values of
    ;; low-/high-inclusive.
    (define (subtrie-interval trie a b low-inclusive high-inclusive)
      (letrec
       ((interval
         (lambda (t)
           (tmatch t
             (empty the-empty-trie)
             ((leaf ,tk ?)
              (and ((if low-inclusive fx>=? fx>?) tk a)
                   ((if high-inclusive fx<=? fx<?) tk b)
                   t))
             (else (branch-interval t)))))
        (branch-interval
         (lambda (t)
           (tmatch t
             ((branch ,p ,m ,l ,r)
              (if (match-prefix? a p m)
                  (if (zero-bit? a m)
                      (if (match-prefix? b p m)
                          (if (zero-bit? b m)
                              (interval l)  ; all x < b is in l
                              (trie-union (subtrie> l a low-inclusive)
                                          (subtrie< r b high-inclusive)))
                          ;; everything or nothing is less than b
                          (and (fx<? b p)
                               (trie-union (subtrie> l a low-inclusive) r)))
                      (interval r)) ; all x > b is in r
                  ;; everything or nothing is greater than a
                  (and (fx>? p a) (subtrie< t b high-inclusive))))))))
        (tmatch trie
          ((branch ? ,m ,l ,r) (guard (fxnegative? m))
           (cond ((and (fxnegative? a) (fxnegative? b)) (interval r))
                 ((and (fxpositive? a) (fxpositive? b)) (interval l))
                  ;; (a, 0) U (0, b)
                  (else (trie-union (subtrie> r a low-inclusive)
                                    (subtrie< l b high-inclusive)))))
          (else (interval trie)))))
    
    (define (trie-split trie pivot)
      (letrec
       ((split
         (lambda (t)
           (tmatch t
             ((leaf ,k ,v)
              (if (fx<=? k pivot)
                  (values t the-empty-trie)
                  (values the-empty-trie t)))
             ((branch ,p ,m ,l ,r)
              (if (match-prefix? pivot p m)
                  (if (zero-bit? pivot m)
                      (let-values (((ta tb) (split l)))
                        (values ta (trie-union tb r)))
                      (let-values (((ta tb) (split r)))
                        (values (trie-union l ta) tb)))
                  (if (fx<=? p pivot)
                      (values t the-empty-trie)
                      (values the-empty-trie t))))))))
    
        (tmatch trie
          (empty (values the-empty-trie the-empty-trie))
          ((branch ? ,m ,l ,r) (guard (fxnegative? m))
           (if (fxnegative? pivot)
               (let-values (((ta tb) (split r)))
                 (values ta (trie-union tb l)))
               (let-values (((ta tb) (split l)))
                 (values (trie-union r ta) tb))))
          (else (split trie)))))
    
    ;;;; Tries as (Integer, *) relations
    
    (define (trie-relation-map proc trie)
      (trie-fold-left (lambda (k v t)
                        (let-values (((k* v*) (proc k v)))
                          (assume (valid-integer? k*))
                          (trie-insert t k* v*)))
                      the-empty-trie
                      trie))    
  )
  
  (begin
    ;;;; Utility
    
    (define (plist-fold proc nil ps)
      (let loop ((b nil) (ps ps))
        (pmatch ps
          (() b)
          ((,k ,v . ,ps*)
           (loop (proc k v b) ps*))
          (else (error "plist-fold: invalid plist")))))
    
    (define (first-arg _k x _y) x)
    (define (second-arg _k _x y) y)
    
    (define (constantly x)
      (lambda (_) x))
    
    ;;;; Type
    
    (define-record-type <fxmapping>
      (raw-fxmapping trie)
      fxmapping?
      (trie fxmapping-trie))
    
    ;;;; Constructors
    
    (define (fxmapping . args)
      (raw-fxmapping
        (plist-fold (lambda (k v trie) (trie-adjoin trie k v))
                    the-empty-trie
                    args)))
    
    (define (pair-or-null? x)
      (or (pair? x) (null? x)))
    
    (define (alist->fxmapping/combinator comb as)
      (assume (procedure? comb))
      (assume (pair-or-null? as))
      (raw-fxmapping
        (fold (lambda (p trie)
                (assume (pair? p) "alist->fxmapping/combinator: not a pair")
                (trie-insert/combine trie (car p) (cdr p) comb))
              the-empty-trie
              as)))
    
    (define (alist->fxmapping as)
      (alist->fxmapping/combinator second-arg as))
    
    (define fxmapping-unfold
      (case-lambda
        ((stop? mapper successor seed)                ; fast path
         (assume (procedure? stop?))
         (assume (procedure? mapper))
         (assume (procedure? successor))
         (let lp ((trie the-empty-trie) (seed seed))
           (if (stop? seed)
               (raw-fxmapping trie)
               (let-values (((k v) (mapper seed)))
                 (assume (valid-integer? k))
                 (lp (trie-adjoin trie k v) (successor seed))))))
        ((stop? mapper successor . seeds)             ; variadic path
         (assume (procedure? stop?))
         (assume (procedure? mapper))
         (assume (procedure? successor))
         (assume (pair? seeds))
         (let lp ((trie the-empty-trie) (seeds seeds))
           (if (apply stop? seeds)
               (raw-fxmapping trie)
               (let-values (((k v) (apply mapper seeds))
                            (seeds* (apply successor seeds)))
                 (assume (valid-integer? k))
                 (lp (trie-adjoin trie k v) seeds*)))))))
    
    (define fxmapping-accumulate
      (case-lambda
        ((proc seed)                                ; fast path
         (assume (procedure? proc))
         (call-with-current-continuation
          (lambda (k)
            (let lp ((trie the-empty-trie) (seed seed))
              (let-values (((k v seed*)
                            (proc (lambda xs (apply k (raw-fxmapping trie) xs))
                                  seed)))
                (lp (trie-adjoin trie k v) seed*))))))
        ((proc . seeds)                             ; variadic path
         (assume (procedure? proc))
         (assume (pair? seeds))
         (call-with-current-continuation
          (lambda (k)
            (let lp ((trie the-empty-trie) (seeds seeds))
              (let-values (((k v . seeds*)
                            (apply proc
                                   (lambda xs (apply k (raw-fxmapping trie) xs))
                                   seeds)))
                (lp (trie-adjoin trie k v) seeds*))))))))
    
    ;;;; Predicates
    
    (define (fxmapping-contains? fxmap n)
      (assume (fxmapping? fxmap))
      (assume (valid-integer? n))
      (trie-contains? (fxmapping-trie fxmap) n))
    
    (define (fxmapping-empty? fxmap)
      (assume (fxmapping? fxmap))
      (eqv? (fxmapping-trie fxmap) the-empty-trie))
    
    (define (fxmapping-disjoint? fxmap1 fxmap2)
      (assume (fxmapping? fxmap1))
      (assume (fxmapping? fxmap2))
      (trie-disjoint? (fxmapping-trie fxmap1) (fxmapping-trie fxmap2)))
    
    ;;;; Accessors
    
    (define fxmapping-ref
      (case-lambda
        ((fxmap key)
         (fxmapping-ref fxmap
                        key
                        (lambda () (error "fxmapping-ref: key not found"
                                          key
                                          fxmap))
                        values))
        ((fxmap key failure)
         (fxmapping-ref fxmap key failure values))
        ((fxmap key failure success)
         (assume (fxmapping? fxmap))
         (assume (valid-integer? key))
         (assume (procedure? failure))
         (assume (procedure? success))
         (trie-assoc (fxmapping-trie fxmap) key failure success))))
    
    (define (fxmapping-ref/default fxmap key default)
      (assume (fxmapping? fxmap))
      (assume (valid-integer? key))
      (trie-assoc/default (fxmapping-trie fxmap) key default))
    
    (define (fxmapping-min fxmap)
      (assume (not (fxmapping-empty? fxmap)))
      (trie-min (fxmapping-trie fxmap)))
    
    (define (fxmapping-max fxmap)
      (assume (not (fxmapping-empty? fxmap)))
      (trie-max (fxmapping-trie fxmap)))
    
    ;;;; Updaters
    
    (define fxmapping-adjoin/combinator
      (case-lambda
        ((fxmap combine key value)      ; one-assoc fast path
         (raw-fxmapping
          (trie-insert/combine (fxmapping-trie fxmap) key value combine)))
        ((fxmap combine . ps)
         (raw-fxmapping
          (plist-fold (lambda (k v t)
                        (trie-insert/combine t k v combine))
                      (fxmapping-trie fxmap)
                      ps)))))
    
    ;; Preserve existing associations for keys.
    (define fxmapping-adjoin
      (case-lambda
        ((fxmap key value)              ; one-assoc fast path
         (raw-fxmapping
          (trie-adjoin (fxmapping-trie fxmap) key value)))
        ((fxmap . ps)
         (raw-fxmapping
          (plist-fold (lambda (k v t) (trie-adjoin t k v))
                      (fxmapping-trie fxmap)
                      ps)))))
    
    ;; Replace existing associations for keys.
    (define fxmapping-set
      (case-lambda
        ((fxmap key value)      ; one-assoc fast path
         (raw-fxmapping
          (trie-insert (fxmapping-trie fxmap) key value)))
        ((fxmap . ps)
         (raw-fxmapping
          (plist-fold (lambda (k v t) (trie-insert t k v))
                      (fxmapping-trie fxmap)
                      ps)))))
    
    (define (fxmapping-adjust fxmap key proc)
      (assume (fxmapping? fxmap))
      (assume (valid-integer? key))
      (assume (procedure? proc))
      (raw-fxmapping (trie-adjust (fxmapping-trie fxmap) key proc)))
    
    (define fxmapping-delete
      (case-lambda
        ((fxmap key)      ; fast path
         (assume (fxmapping? fxmap))
         (assume (valid-integer? key))
         (raw-fxmapping (trie-delete (fxmapping-trie fxmap) key)))
        ((fxmap . keys)
         (fxmapping-delete-all fxmap keys))))
    
    (define (fxmapping-delete-all fxmap keys)
      (assume (or (pair? keys) (null? keys)))
      (let ((key-map (fxmapping-trie
                      (fxmapping-unfold null?
                                        (lambda (ks) (values (car ks) #t))
                                        cdr
                                        keys))))
        (fxmapping-remove (lambda (k _) (trie-contains? key-map k))
                          fxmap)))
    
    (define fxmapping-update
      (case-lambda
        ((fxmap key success)
         (fxmapping-update fxmap
                           key
                           success
                           (lambda ()
                             (error "fxmapping-update: key not found" key fxmap))))
        ((fxmap key success failure)
         (assume (fxmapping? fxmap))
         (assume (valid-integer? key))
         (assume (procedure? success))
         (assume (procedure? failure))
         (trie-update (fxmapping-trie fxmap) key success failure raw-fxmapping))))
    
    (define (fxmapping-alter fxmap key failure success)
      (assume (fxmapping? fxmap))
      (assume (valid-integer? key))
      (assume (procedure? failure))
      (assume (procedure? success))
      (trie-alter (fxmapping-trie fxmap) key failure success raw-fxmapping))
    
    (define (fxmapping-delete-min fxmap)
      (fxmapping-update-min fxmap
                            (lambda (_k _v _rep delete)
                              (delete))))
    
    (define (fxmapping-update-min fxmap success)
      (assume (fxmapping? fxmap))
      (assume (not (fxmapping-empty? fxmap)))
      (assume (procedure? success))
      (trie-update-min (fxmapping-trie fxmap) success raw-fxmapping))
    
    (define (fxmapping-pop-min fxmap)
      (assume (fxmapping? fxmap))
      (assume (not (fxmapping-empty? fxmap)))
      (let-values (((k v trie) (trie-pop-min (fxmapping-trie fxmap))))
        (values k v (raw-fxmapping trie))))
    
    (define (fxmapping-delete-max fxmap)
      (fxmapping-update-max fxmap
                            (lambda (_k _v _rep delete)
                              (delete))))
    
    (define (fxmapping-update-max fxmap success)
      (assume (fxmapping? fxmap))
      (assume (not (fxmapping-empty? fxmap)))
      (assume (procedure? success))
      (trie-update-max (fxmapping-trie fxmap) success raw-fxmapping))
    
    (define (fxmapping-pop-max fxmap)
      (assume (fxmapping? fxmap))
      (assume (not (fxmapping-empty? fxmap)))
      (let-values (((k v trie) (trie-pop-max (fxmapping-trie fxmap))))
        (values k v (raw-fxmapping trie))))
    
    ;;;; The whole fxmapping
    
    (define (fxmapping-size fxmap)
      (assume (fxmapping? fxmap))
      (trie-size (fxmapping-trie fxmap)))
    
    (define fxmapping-find
      (case-lambda
        ((pred fxmap failure)
         (fxmapping-find pred fxmap failure values))
        ((pred fxmap failure success)
         (assume (procedure? pred))
         (assume (fxmapping? fxmap))
         (assume (procedure? failure))
         (assume (procedure? success))
         (trie-find pred (fxmapping-trie fxmap) failure success))))
    
    (define (fxmapping-count pred fxmap)
      (assume (procedure? pred))
      (fxmapping-fold (lambda (k v acc)
                        (if (pred k v) (+ 1 acc) acc))
                      0
                      fxmap))
    
    (define (fxmapping-any? pred fxmap)
      (assume (procedure? pred))
      (call-with-current-continuation
       (lambda (return)
         (fxmapping-fold (lambda (k v _)
                           (and (pred k v) (return #t)))
                         #f
                         fxmap))))
    
    (define (fxmapping-every? pred fxmap)
      (assume (procedure? pred))
      (call-with-current-continuation
       (lambda (return)
         (fxmapping-fold (lambda (k v _)
                           (or (pred k v) (return #f)))
                         #t
                         fxmap))))
    
    ;;;; Mapping and folding
    
    ;; Map proc over the assocs. of fxmap, inserting result values under
    ;; the same key.
    ;; This is *not* the same as SRFI 146's mapping-map.
    (define (fxmapping-map proc fxmap)
      (assume (procedure? proc))
      (assume (fxmapping? fxmap))
      (raw-fxmapping (trie-map proc (fxmapping-trie fxmap))))
    
    (define (unspecified)
      (if #f #f))
    
    (define (fxmapping-for-each proc fxmap)
      (assume (procedure? proc))
      (fxmapping-fold (lambda (k v _)
                        (proc k v)
                        (unspecified))
                      (unspecified)
                      fxmap))
    
    (define (fxmapping-fold proc nil fxmap)
      (assume (procedure? proc))
      (assume (fxmapping? fxmap))
      (let ((trie (fxmapping-trie fxmap)))
        (tmatch trie
          ((branch ? ,m ,l ,r) (guard (negative? m))
           (trie-fold-left proc (trie-fold-left proc nil r) l))
          ((branch ? ? ,l ,r)
           (trie-fold-left proc (trie-fold-left proc nil l) r))
          (else (trie-fold-left proc nil trie)))))
    
    (define (fxmapping-fold-right proc nil fxmap)
      (assume (procedure? proc))
      (assume (fxmapping? fxmap))
      (let ((trie (fxmapping-trie fxmap)))
        (tmatch trie
          ((branch ? ,m ,l ,r) (guard (negative? m))
           (trie-fold-right proc (trie-fold-right proc nil l) r))
          ((branch ? ? ,l ,r)
           (trie-fold-right proc (trie-fold-right proc nil r) l))
          (else (trie-fold-right proc nil trie)))))
    
    (define (fxmapping-map->list proc fxmap)
      (assume (procedure? proc))
      (fxmapping-fold-right (lambda (k v us)
                              (cons (proc k v) us))
                            '()
                            fxmap))
    
    (define (fxmapping-filter pred fxmap)
      (assume (procedure? pred))
      (assume (fxmapping? fxmap))
      (raw-fxmapping (trie-filter pred (fxmapping-trie fxmap))))
    
    (define (fxmapping-remove pred fxmap)
      (fxmapping-filter (lambda (k v) (not (pred k v))) fxmap))
    
    (define (fxmapping-partition pred fxmap)
      (assume (procedure? pred))
      (assume (fxmapping? fxmap))
      (let-values (((tin tout)
                    (trie-partition pred (fxmapping-trie fxmap))))
        (values (raw-fxmapping tin) (raw-fxmapping tout))))
    
    ;;;; Conversion
    
    (define (fxmapping->alist fxmap)
      (fxmapping-fold-right (lambda (k v as) (cons (cons k v) as))
                            '()
                            fxmap))
    
    (define (fxmapping->decreasing-alist fxmap)
      (fxmapping-fold (lambda (k v as) (cons (cons k v) as))
                      '()
                      fxmap))
    
    (define (fxmapping-keys fxmap)
      (fxmapping-fold-right (lambda (k _ ks) (cons k ks)) '() fxmap))
    
    (define (fxmapping-values fxmap)
      (fxmapping-fold-right (lambda (_ v vs) (cons v vs)) '() fxmap))
    
    (define (fxmapping->generator fxmap)
      (assume (fxmapping? fxmap))
      (make-coroutine-generator
       (lambda (yield)
         (fxmapping-fold (lambda (k v _) (yield (cons k v)))
                         #f
                         fxmap))))
    
    (define (fxmapping->decreasing-generator fxmap)
      (assume (fxmapping? fxmap))
      (make-coroutine-generator
       (lambda (yield)
         (fxmapping-fold-right (lambda (k v _) (yield (cons k v)))
                               #f
                               fxmap))))
    
    ;;;; Comparison
    
    (define (fxmapping=? comp fxmap1 fxmap2 . imaps)
      (assume (comparator? comp))
      (assume (fxmapping? fxmap1))
      (let ((fxmap-eq1 (lambda (fxmap)
                         (assume (fxmapping? fxmap))
                         (or (eqv? fxmap1 fxmap)
                             (trie=? comp
                                     (fxmapping-trie fxmap1)
                                     (fxmapping-trie fxmap))))))
        (and (fxmap-eq1 fxmap2)
             (or (null? imaps)
                 (every fxmap-eq1 imaps)))))
    
    (define (fxmapping<? comp fxmap1 fxmap2 . imaps)
      (assume (comparator? comp))
      (assume (fxmapping? fxmap1))
      (assume (fxmapping? fxmap2))
      (let lp ((t1 (fxmapping-trie fxmap1))
               (t2 (fxmapping-trie fxmap2))
               (imaps imaps))
        (and (trie-proper-subset? comp t1 t2)
             (pmatch imaps
               (() #t)
               ((,m . ,imaps*) (lp t2 (fxmapping-trie m) imaps*))))))
    
    (define (fxmapping>? comp fxmap1 fxmap2 . imaps)
      (assume (comparator? comp))
      (assume (fxmapping? fxmap1))
      (assume (fxmapping? fxmap2))
      (let lp ((t1 (fxmapping-trie fxmap1))
               (t2 (fxmapping-trie fxmap2))
               (imaps imaps))
        (and (trie-proper-subset? comp t2 t1)
             (pmatch imaps
               (() #t)
               ((,m . ,imaps*) (lp t2 (fxmapping-trie m) imaps*))))))
    
    (define (fxmapping<=? comp fxmap1 fxmap2 . imaps)
      (assume (comparator? comp))
      (assume (fxmapping? fxmap1))
      (assume (fxmapping? fxmap2))
      (let lp ((t1 (fxmapping-trie fxmap1))
               (t2 (fxmapping-trie fxmap2))
               (imaps imaps))
        (and (memv (trie-subset-compare comp t1 t2) '(less equal))
             (pmatch imaps
               (() #t)
               ((,m . ,imaps*) (lp t2 (fxmapping-trie m) imaps*))))))
    
    (define (fxmapping>=? comp fxmap1 fxmap2 . imaps)
      (assume (comparator? comp))
      (assume (fxmapping? fxmap1))
      (assume (fxmapping? fxmap2))
      (let lp ((t1 (fxmapping-trie fxmap1))
               (t2 (fxmapping-trie fxmap2))
               (imaps imaps))
        (and (memv (trie-subset-compare comp t2 t1) '(less equal))
             (pmatch imaps
               (() #t)
               ((,m . ,imaps*) (lp t2 (fxmapping-trie m) imaps*))))))
    
    ;;;; Set theory operations
    
    (define (fxmapping-union . args)
      (apply fxmapping-union/combinator first-arg args))
    
    (define (fxmapping-intersection . args)
      (apply fxmapping-intersection/combinator first-arg args))
    
    (define fxmapping-difference
      (case-lambda
        ((fxmap1 fxmap2)
         (assume (fxmapping? fxmap1))
         (assume (fxmapping? fxmap2))
         (raw-fxmapping
          (trie-difference (fxmapping-trie fxmap1)
                           (fxmapping-trie fxmap2))))
        ((fxmap . rest)
         (assume (fxmapping? fxmap))
         (assume (pair? rest))
         (raw-fxmapping
          (trie-difference (fxmapping-trie fxmap)
                           (fxmapping-trie
                            (apply fxmapping-union rest)))))))
    
    (define (fxmapping-xor fxmap1 fxmap2)
      (assume (fxmapping? fxmap1))
      (assume (fxmapping? fxmap2))
      (raw-fxmapping
       (trie-xor (fxmapping-trie fxmap1) (fxmapping-trie fxmap2))))
    
    (define (fxmapping-union/combinator proc fxmap . rest)
      (assume (procedure? proc))
      (assume (fxmapping? fxmap))
      (assume (pair? rest))
      (raw-fxmapping
       (fold (lambda (im t)
               (assume (fxmapping? im))
               (trie-merge proc t (fxmapping-trie im)))
             (fxmapping-trie fxmap)
             rest)))
    
    (define (fxmapping-intersection/combinator proc fxmap . rest)
      (assume (procedure? proc))
      (assume (fxmapping? fxmap))
      (assume (pair? rest))
      (raw-fxmapping
       (fold (lambda (im t)
               (assume (fxmapping? im))
               (trie-intersection proc t (fxmapping-trie im)))
             (fxmapping-trie fxmap)
             rest)))
    
    ;;;; Subsets
    
    (define (fxsubmapping= fxmap key)
      (fxmapping-ref fxmap
                     key
                     fxmapping
                     (lambda (v) (fxmapping key v))))
    
    (define (fxmapping-open-interval fxmap low high)
      (assume (fxmapping? fxmap))
      (assume (valid-integer? low))
      (assume (valid-integer? high))
      (assume (fx>=? high low))
      (raw-fxmapping
       (subtrie-interval (fxmapping-trie fxmap) low high #f #f)))
    
    (define (fxmapping-closed-interval fxmap low high)
      (assume (fxmapping? fxmap))
      (assume (valid-integer? low))
      (assume (valid-integer? high))
      (assume (fx>=? high low))
      (raw-fxmapping
       (subtrie-interval (fxmapping-trie fxmap) low high #t #t)))
    
    (define (fxmapping-open-closed-interval fxmap low high)
      (assume (fxmapping? fxmap))
      (assume (valid-integer? low))
      (assume (valid-integer? high))
      (assume (fx>=? high low))
      (raw-fxmapping
       (subtrie-interval (fxmapping-trie fxmap) low high #f #t)))
    
    (define (fxmapping-closed-open-interval fxmap low high)
      (assume (fxmapping? fxmap))
      (assume (valid-integer? low))
      (assume (valid-integer? high))
      (assume (fx>=? high low))
      (raw-fxmapping
       (subtrie-interval (fxmapping-trie fxmap) low high #t #f)))
    
    (define (fxsubmapping< fxmap key)
      (assume (fxmapping? fxmap))
      (assume (valid-integer? key))
      (raw-fxmapping (subtrie< (fxmapping-trie fxmap) key #f)))
    
    (define (fxsubmapping<= fxmap key)
      (assume (fxmapping? fxmap))
      (assume (valid-integer? key))
      (raw-fxmapping (subtrie< (fxmapping-trie fxmap) key #t)))
    
    (define (fxsubmapping> fxmap key)
      (assume (fxmapping? fxmap))
      (assume (valid-integer? key))
      (raw-fxmapping (subtrie> (fxmapping-trie fxmap) key #f)))
    
    (define (fxsubmapping>= fxmap key)
      (assume (fxmapping? fxmap))
      (assume (valid-integer? key))
      (raw-fxmapping (subtrie> (fxmapping-trie fxmap) key #t)))
    
    (define (fxmapping-split fxmap k)
      (assume (fxmapping? fxmap))
      (assume (integer? k))
      (let-values (((trie-low trie-high)
                    (trie-split (fxmapping-trie fxmap) k)))
        (values (raw-fxmapping trie-low) (raw-fxmapping trie-high))))
    
    ;;;; fxmappings as relations
    
    (define (fxmapping-relation-map proc fxmap)
      (assume (procedure? proc))
      (assume (fxmapping? fxmap))
      (raw-fxmapping (trie-relation-map proc (fxmapping-trie fxmap))))
  )
)
