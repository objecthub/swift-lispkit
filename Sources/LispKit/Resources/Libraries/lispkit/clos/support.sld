;;; LISPKIT CLOS SUPPORT
;;;
;;; This is a R7RS-compatible version of Tiny CLOS. CLOS is an acronym for "Common Lisp
;;; Object System". It is a standard component of Common Lisp providing abstractions for
;;; object-oriented programming. Tiny CLOS is a Scheme version of CLOS written in 1992 by
;;; Gregor Kiczales. It differs from CLOS in terms of the API, but the object-oriented
;;; abstractions are equivalent to CLOS.
;;;
;;; This library provides supporting functions to the core library `(lispkit clos)`.
;;;
;;; *************************************************************************
;;; Copyright (c) 1992 Xerox Corporation.
;;; All Rights Reserved.
;;;
;;; Use, reproduction, and preparation of derivative works are permitted.
;;; Any copy of this software or of any derivative work must include the
;;; above copyright notice of Xerox Corporation, this paragraph and the
;;; one after it.  Any distribution of this software or derivative works
;;; must comply with all applicable United States export control laws.
;;;
;;; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
;;; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
;;; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
;;; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
;;; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
;;; OF THE POSSIBILITY OF SUCH DAMAGES.
;;; *************************************************************************
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2018 Matthias Zenger. All rights reserved.

(define-library (lispkit clos support)

  (export position-of
          last
          every
          getl
          compute-std-cpl)

  (import (except (lispkit base) remove))

  (begin

    (define (position-of x lst)
      (if (eq? x (car lst))
          0
          (+ 1 (position-of x (cdr lst)))))

    (define (last l)
      (if (null? l)
          #f
          (if (null? (cdr l)) (car l) (last (cdr l)))))

    ;; NOT NEEDED
    (define (map-append proc . lists)
      (apply append (apply map (cons proc lists))))

    (define (last l)
      (if (null? l)
          #f
          (if (null? (cdr l)) (car l) (last (cdr l)))))

    (define (every test . lists)
      (let scan ((tails lists))
        (if (member #t (map null? tails))
            #t
    	      (and (apply test (map car tails)) (scan (map cdr tails))))))

    ;; NOT NEEDED
    (define (remove x list)
      (cond ((null? list) '())
            ((eq? (car list) x) (cdr list))
            (else (cons (car list) (remove x (cdr list))))))

    (define (getl args name . not-found)
      (do ((tail args (cddr tail)))
          ((or (not (pair? tail)) (eq? (car tail) name))
           (if (pair? tail)
               (cadr tail)
               (if (pair? not-found)
    		           (car not-found)
    		           (error "could not find named argument $0 in argument list $1" name args))))))

    ;; NOT NEEDED
    (define (union . lists)
      (letrec ((clean (lambda (list result)
                        (cond ((null? list) result)
                              ((memq (car list) result) (clean (cdr list) result))
                              (else (clean (cdr list) (cons (car list) result)))))))
    	  (clean (apply append lists) '())))

    ;; NOT NEEDED
    (define (collect-if test? list)
      (cond ((null? list) '())
            ((test? (car list)) (cons (car list) (collect-if test? (cdr list))))
            (else (collect-if test? (cdr list)))))

    ;; NOT NEEDED
    (define (remove-duplicates list)
      (let loop ((result-so-far '())
                 (remaining list))
        (if (null? remaining)
    	      result-so-far
    	      (if (memq (car remaining) result-so-far)
    	          (loop result-so-far (cdr remaining))
    		        (loop (cons (car remaining) result-so-far) (cdr remaining))))))

    ; A simple topological sort.
    ; It's in this file so that both TinyClos and Objects can use it.
    ; This is a fairly modified version of code I originally got from Anurag
    ; Mendhekar <anurag@moose.cs.indiana.edu>.

    (define (compute-std-cpl c get-direct-supers)
      (top-sort ((build-transitive-closure get-direct-supers) c)
    		        ((build-constraints get-direct-supers) c)
    		        (std-tie-breaker get-direct-supers)))

    (define (top-sort elements constraints tie-breaker)
      (let loop ((elements    elements)
                 (constraints constraints)
                 (result      '()))
        (if (null? elements)
            result
            (let ((can-go-in-now (filter (lambda (x)
                                           (every (lambda (constraint)
                                                    (or (not (eq? (cadr constraint) x))
                                                        (memq (car constraint) result)))
                                                  constraints))
                                         elements)))
              (if (null? can-go-in-now)
                  (error "invalid top-sort constraints: $0" constraints)
                  (let ((choice (if (null? (cdr can-go-in-now))
                                    (car can-go-in-now)
                                    (tie-breaker result can-go-in-now))))
                    (loop (filter (lambda (x) (not (eq? x choice))) elements)
                          constraints
                          (append result (list choice)))))))))

    (define (std-tie-breaker get-supers)
      (lambda (partial-cpl min-elts)
        (let loop ((pcpl (reverse partial-cpl)))
          (let ((current-elt (car pcpl)))
            (let ((ds-of-ce (get-supers current-elt)))
      		    (let ((common (filter (lambda (x) (memq x ds-of-ce)) min-elts)))
                (if (null? common)
                    (if (null? (cdr pcpl))
                        (error "no std-tie-breaker result")
                        (loop (cdr pcpl)))
                    (car common))))))))

    (define (build-transitive-closure get-follow-ons)
      (lambda (x)
        (let track ((result '())
                    (pending (list x)))
    	    (if (null? pending)
    		      result
    		      (let ((next (car pending)))
    		        (if (memq next result)
    		            (track result (cdr pending))
    		            (track (cons next result) (append (get-follow-ons next) (cdr pending)))))))))

    (define (build-constraints get-follow-ons)
      (lambda (x)
        (let loop ((elements ((build-transitive-closure get-follow-ons) x))
    		           (this-one '())
    		           (result   '()))
          (if (or (null? this-one) (null? (cdr this-one)))
              (if (null? elements)
                  result
                  (loop (cdr elements)
                        (cons (car elements) (get-follow-ons (car elements)))
                        result))
              (loop elements
                    (cdr this-one)
                    (cons (list (car this-one) (cadr this-one)) result))))))
  )
)
