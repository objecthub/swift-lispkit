;;; SRFI 275 INTERNAL RESOLVE
;;; URIs, IRIs, and basic paths: normalization, resolution, and relativization
;;;
;;; This library is internal to the implementation of SRFI 275. It implements the
;;; normalization of URIs and IRIs, the resolution of relative references against
;;; a base (RFC 3986, section 5.2), the inverse operation (relativization), and the
;;; conversion between URIs and IRIs. The relativization algorithm is based on the
;;; function `relative-from` of Chicken's `uri-generic` library. As in the ident
;;; library, procedures take the name `who` of the public procedure for error messages
;;; and a flag `iri?` which determines whether IRIs (true) or URIs (false) are
;;; processed.
;;;
;;; Author of spec: Duncan Guthrie
;;;
;;; Copyright © 2026 Duncan Guthrie. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;; this software and associated documentation files (the "Software"), to deal in the
;;; Software without restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so, subject to the
;;; following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2026 Matthias Zenger. All rights reserved.

(define-library (srfi 275 internal resolve)

  (import (scheme base)
          (srfi 275 internal chars)
          (srfi 275 internal encoding)
          (srfi 275 internal path)
          (srfi 275 internal ident))

  (export normalize-case
          normalize-escape
          normalize-path
          normalize-all
          convert-ident
          resolve-ident
          relativize-ident)

  (begin

    ;;; Helpers

    (define (ip-literal? host)
      (and (> (string-length host) 0) (char=? (string-ref host 0) #\[)))

    (define (map-components x iri? scheme-fn host-fn user-fn segment-fn query-fn fragment-fn)
      (let ((path (ident-path-raw x))
            (opt (lambda (f text) (and text (f text)))))
        (new-ident iri?
                   (opt scheme-fn (ident-scheme-raw x))
                   (opt user-fn (ident-user-raw x))
                   (opt host-fn (ident-host-raw x))
                   (ident-port-raw x)
                   (make-path (absolute-path? path) (map segment-fn (path-segment-list path)))
                   (opt query-fn (ident-query-raw x))
                   (opt fragment-fn (ident-fragment-raw x)))))

    (define (with-path x path)
      (new-ident (ident-iri-flag x) (ident-scheme-raw x) (ident-user-raw x) (ident-host-raw x)
                 (ident-port-raw x) path (ident-query-raw x) (ident-fragment-raw x)))

    (define (has-authority? x)
      (authority-set? (ident-user-raw x) (ident-host-raw x) (ident-port-raw x)))

    ;;; Normalization

    ;; Lowercases ASCII letters in scheme and host
    (define (normalize-case who iri? x)
      (check-ident who iri? x)
      (map-components x iri?
                      ascii-downcase-literals
                      (lambda (host) (ascii-downcase-literals host))
                      (lambda (s) s) (lambda (s) s) (lambda (s) s) (lambda (s) s)))

    ;; Decodes percent-encoded characters which are neither reserved nor otherwise
    ;; not permitted in the component. Characters which are not permitted in
    ;; the component are percent-encoded.
    (define (normalize-escape who iri? x)
      (check-ident who iri? x)
      (let* ((profile (if iri? iri-profile uri-profile))
             (decode? (lambda (cp) (not (cp-reserved? cp))))
             (norm (lambda (which)
                     (let ((pred (profile-predicate profile which)))
                       (lambda (text)
                         (canonicalize-component who text pred #t decode? 0))))))
        (map-components x iri?
                        (lambda (s) s)
                        (let ((f (norm 'host)))
                          (lambda (host) (if (ip-literal? host) host (f host))))
                        (norm 'user) (norm 'segment) (norm 'query) (norm 'fragment))))

    ;; Removes dot segments from the path of a non-relative identifier. The path
    ;; is never turned into one which would be mistaken for an authority.
    (define (normalize-path who iri? x)
      (check-ident who iri? x)
      (if (not (ident-scheme-raw x))
          x
          (let ((path (remove-dot-segments (ident-path-raw x))))
            (cond ((compatible-path? path (has-authority? x) #f)
                   (with-path x path))
                  ((absolute-path? path)
                   ;; the path starts with an empty segment and there is no authority
                   (with-path x (make-path #f (append '("." "") (path-segment-list path)))))
                  (else x)))))

    (define (normalize-all who iri? x)
      (normalize-path who iri? (normalize-case who iri? (normalize-escape who iri? x))))

    ;;; Conversion between URIs and IRIs

    ;; Converts an IRI into a URI (`iri?` is true), or a URI into an IRI (`iri?` is false)
    ;; and normalizes the result.
    (define (convert-ident who iri? x)
      (check-ident who iri? x)
      (let* ((to-iri? (not iri?))
             (profile (if to-iri? iri-profile uri-profile))
             (conv (lambda (which)
                     (let ((pred (profile-predicate profile which)))
                       (lambda (text)
                         (canonicalize-component who text pred #t #f 0))))))
        (normalize-all who to-iri?
                       (map-components x to-iri?
                                       (lambda (s) s)
                                       (let ((f (conv 'host)))
                                         (lambda (host) (if (ip-literal? host) host (f host))))
                                       (conv 'user) (conv 'segment) (conv 'query) (conv 'fragment)))))

    ;;; Resolution of relative references

    (define (resolve-ident who iri? base ref)
      (check-ident who iri? base)
      (check-ident who iri? ref)
      (unless (ident-scheme-raw base)
        (fail who (if iri? "base is not a non-relative IRI" "base is not a non-relative URI") base))
      (let ((norm (lambda (x) (normalize-path who iri? x)))
            (rpath (ident-path-raw ref))
            (rquery (ident-query-raw ref))
            (rfragment (ident-fragment-raw ref))
            (bpath (ident-path-raw base)))
        (cond
          ((ident-scheme-raw ref)
           (norm ref))
          ((has-authority? ref)
           (norm (new-ident iri? (ident-scheme-raw base) (ident-user-raw ref) (ident-host-raw ref)
                            (ident-port-raw ref) rpath rquery rfragment)))
          (else
            (let ((target (lambda (path query)
                            (new-ident iri? (ident-scheme-raw base) (ident-user-raw base)
                                       (ident-host-raw base) (ident-port-raw base)
                                       path query rfragment))))
              (cond ((and (empty-path? rpath) (not rquery))
                     (target bpath (ident-query-raw base)))
                    ((empty-path? rpath)
                     (target bpath rquery))
                    ((absolute-path? rpath)
                     (norm (target rpath rquery)))
                    (else
                      (norm (target (merge-paths bpath rpath (has-authority? base)) rquery)))))))))

    ;;; Relativization

    ;; Segments of a path in which a path "/" has the single empty segment
    (define (full-segments path)
      (let ((segs (path-segment-list path)))
        (if (null? segs) '("") segs)))

    (define (dotted? seg)
      (or (string=? seg ".") (string=? seg "..")))

    (define (last-of lst)
      (if (null? (cdr lst)) (car lst) (last-of (cdr lst))))

    (define (diff-segments base-init init)
      (if (null? base-init)
          init
          (diff-segments (cdr base-init) (cons ".." init))))

    (define (relative-segments-from base-init init)
      (cond ((and (null? base-init) (null? init)) '())
            ((null? base-init) init)
            ((null? init) (diff-segments base-init '()))
            ((string=? (car base-init) (car init))
             (relative-segments-from (cdr base-init) (cdr init)))
            (else (diff-segments base-init init))))

    (define (relativize-segments base-segs segs)
      (let* ((rbase (reverse base-segs))
             (rsegs (reverse segs))
             (base-final (car rbase))
             (final (car rsegs))
             (relative (relative-segments-from (reverse (cdr rbase)) (reverse (cdr rsegs)))))
        (cond ((null? relative)
               (cond ((string=? final base-final) '())
                     ((string=? final "") (list "."))
                     (else (list final))))
              ((dotted? (last-of relative))
               (if (string=? final "")
                   relative
                   (append relative (list final))))
              (else (append relative (list final))))))

    ;; Relativizes `path` of `x` against `base-path` of `base`; both paths are expected to
    ;; be normalized. Raises an error if a path is not path-abempty.
    (define (relativize-path who base base-path x path)
      (cond ((not (rfc3987-path-abempty? base-path))
             (fail who "normalized path of base is not path-abempty" base-path base))
            ((not (rfc3987-path-abempty? path))
             (fail who "normalized path is not path-abempty" path x))
            ((empty-path? base-path) path)
            ((empty-path? path) (empty-path))
            (else
              (let ((extracted (relativize-segments (full-segments base-path)
                                                    (full-segments path))))
                (if (and (pair? extracted) (string=? (car extracted) ""))
                    (make-path #t (cdr extracted))
                    (make-path #f extracted))))))

    ;; Makes sure that a path of a relative reference is not mistaken for a scheme or an
    ;; authority
    (define (finalize-path authority? path)
      (cond (authority? path)
            ((and (absolute-path? path)
                  (pair? (path-segment-list path))
                  (string=? (car (path-segment-list path)) ""))
             (make-path #f (append '("." "") (path-segment-list path))))
            ((rfc3987-path-rootless? path)
             (if (rfc3987-path-noscheme? path)
                 path
                 (make-path #f (cons "." (path-segment-list path)))))
            (else path)))

    (define (authority-equal? a b)
      (and (equal? (ident-user-raw a) (ident-user-raw b))
           (equal? (ident-host-raw a) (ident-host-raw b))
           (eqv? (ident-port-raw a) (ident-port-raw b))))

    (define (relativize-ident who iri? base x)
      (check-ident who iri? base)
      (check-ident who iri? x)
      (unless (ident-scheme-raw base)
        (fail who (if iri? "base is not a non-relative IRI" "base is not a non-relative URI") base))
      (unless (ident-scheme-raw x)
        (fail who (if iri? "not a non-relative IRI" "not a non-relative URI") x))
      (let* ((path (ident-path-raw x))
             (query (ident-query-raw x))
             (fragment (ident-fragment-raw x))
             (ref (lambda (user host port path query fragment)
                    (new-ident iri? #f user host port path query fragment))))
        (cond
          ;; different schemes
          ((not (string=? (ident-scheme-raw base) (ident-scheme-raw x)))
           x)
          ;; different authorities, or an empty path: keep the authority
          ((or (not (authority-equal? base x)) (empty-path? path))
           (ref (ident-user-raw x) (ident-host-raw x) (ident-port-raw x)
                (finalize-path (has-authority? x) path) query fragment))
          ;; different paths
          ((not (path=? path (ident-path-raw base)))
           (let ((rel (relativize-path who base (remove-dot-segments (ident-path-raw base))
                                       x (remove-dot-segments path))))
             (ref #f #f #f (finalize-path #f rel) query fragment)))
          ;; different queries
          ((not (equal? query (ident-query-raw base)))
           (ref #f #f #f (empty-path) query fragment))
          ;; same path and query: refer to the final segment
          (else
            (let ((final (last-of (full-segments path))))
              (ref #f #f #f
                   (finalize-path #f (make-path #f (list (if (string=? final "") "." final))))
                   #f fragment))))))
  )
)
