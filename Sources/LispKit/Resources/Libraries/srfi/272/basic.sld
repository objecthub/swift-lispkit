;;; SRFI 272 BASIC
;;; Pretty Printing: basic library
;;;
;;; Library `(srfi 272 basic)` provides all the functionality of library `(srfi 272)`,
;;; adding parameters `pp-width`, `pp-graph`, and `pp-circle` for controlling the
;;; right margin and the printing of shared and circular structures via datum
;;; labels, as well as procedures `pprint`, `pprint-shared`, and `pprint-simple`
;;; which behave like `write`, `write-shared`, and `write-simple` with respect to
;;; shared and circular structures.
;;;
;;; Copyright © 2026 Sergei Egorov. All rights reserved.
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

(define-library (srfi 272 basic)
  (import (scheme base) (scheme inexact) (scheme cxr)
    (scheme write) (scheme case-lambda)
    (only (lispkit box) box? box unbox set-box! mpair? mcons mcar mcdr)
    (only (lispkit gvector) gvector? gvector->list list->gvector)
    (only (lispkit hashtable) make-eq-hashtable hashtable-ref hashtable-set!))

  ; procedures
  (export pp pprint pprint-shared pprint-simple)

  ; parameters
  (export pp-width pp-circle pp-graph)

  (begin
    (define (conv-width x)
      (if (and (number? x) (exact? x) (> x 0))
          x
          (error "invalid value for pp-width" x)))
    (define pp-width (make-parameter 80 conv-width))

    ; detect and mark cyclic substructure
    (define pp-circle (make-parameter #t))

    ; detect and mark shared/cyclic substructure
    (define pp-graph (make-parameter #f))


    ; parameters (hidden)

    ; predicate-based hooks for nonstandard data
    (define pp-hooks (make-parameter '()))

    ; adding a hook to the explicit hook registry
    (define (add-pp-hook hooks pred . opt-hook)
      (define hook (if (null? opt-hook) #f (car opt-hook)))
      (if (assv pred hooks)
          (alist-addv pred hook hooks) ; replaces at its original pos
          (cons (cons pred hook) hooks))) ; adds to front

    ; generalized list hook constructor
    (define (glist-pp-hook pfx tolf toxf sfx)
      (list 'gl pfx tolf toxf sfx))

    ; binary vector hook constructor
    (define (bvec-pp-hook pfx lenf reff sfx)
      (list 'bv pfx lenf reff sfx))

    ; atomic hook constructor
    (define (atom-pp-hook sh? widf wrtf)
      (list 'at sh? widf wrtf))


    ; LispKit: eq tables are native eq hashtables

    (define make-eq-table make-eq-hashtable)

    (define table-ref hashtable-ref)

    (define table-set! hashtable-set!)


    ; functional modifications of alists

    (define (alist-addv key val alist)
      (let loop ((l alist))
        (cond ((null? l) (list (cons key val)))
              ((eqv? key (caar l))
               (if (equal? val (cdar l)) l (cons (cons key val) (cdr l))))
              (else
               (let ((rest (loop (cdr l))))
                 (if (eq? rest (cdr l)) l (cons (car l) rest)))))))


    ; locating hooks and calling handlers

    (define (dispatch-on-type x retl retv reta)
      (let loop ((al (pp-hooks)))
        (cond ((null? al) ; dispatch on builtins
               (cond ((pair? x) (retl "(" (lambda (x) x) (lambda (x) x) ")"))
                     ((vector? x) (retl "#(" vector->list list->vector ")"))
                     ((string? x) (reta #t string-length write))
                     ((bytevector? x) ; NB: contents non-markable!
                      (retv "#u8(" bytevector-length bytevector-u8-ref ")"))
                     (else (reta #f atom-width write))))
              (((caar al) x) =>
               (lambda (res)
                 (let ((hk (or (cdar al) res)))
                   (unless (pair? hk) (error "invalid hook!"))
                   (case (car hk)
                     ((gl) (apply retl (cdr hk)))
                     ; binary vector: bvec-pp-hook pfx lenf reff sfx
                     ((bv) (apply retv (cdr hk)))
                     ; atomic: atom-pp-hook sh? widf wrtf
                     ((at) (apply reta (cdr hk)))
                     ; todo: pre-check, this shouldn't happen!
                     (else (error "invalid hook!"))))))
              (else (loop (cdr al))))))

    ; graph sharing/cycles detection

    ; SHARING and pp-graph parameter: if sharing detection and printing is requested
    ; via setting pp-graph parameter to #t, mark-shared is called with the input obj
    ; before doing anything else, then the marked obj is printed. The printer should
    ; always check for marks and measure/print them in #N= / #N# notation

    ; if pp-graph is off, pp-circle determines if it is called with cycles-only? #t
    ; or not at all. If pp-graph is on, mark-shared is called with cycles-only? #f

    (define unique (list 'shared-mark))

    (define (shared-mark? x)
      (and (vector? x) (= (vector-length x) 4)
           (eq? (vector-ref x 0) unique)))

    (define (shared-mark first? count x)
      (vector unique first? count x))

    (define (shared-unmark m)
      (values (vector-ref m 1) (vector-ref m 2) (vector-ref m 3)))

    (define (not-shareable? x)
      (or (symbol? x) (number? x) (boolean? x) (char? x)))

    ; note: env is reserved for the future cutoofs & such
    (define (mark-shared sexp env cycles-only?)
      (let ((counts (make-eq-table)) (marks? #f))
        ; LispKit: pairs are immutable values without identity (eq? on pairs is
        ; structural), so they are never counted, labeled, or used as table keys
        (define (count-ref x) (if (pair? x) 0 (table-ref counts x 0)))
        (define (count-set! x c) (if (not (pair? x)) (table-set! counts x c)))
        (let scan ((x sexp) (v env))
          (unless (not-shareable? x)
            (dispatch-on-type x
              (lambda (pfx tolf toxf sfx)
                (let ((c (count-ref x)))
                  (count-set! x (+ c 1))
                  (if (= c 0)
                      (let ((l (tolf x)))
                        (when (pair? l) (scan (car l) v) (scan (cdr l) v)))
                      (set! marks? #t))))
              (lambda (pfx lenf reff sfx)
                (let ((c (count-ref x)))
                  (count-set! x (+ c 1))
                  (if (> c 0) (set! marks? #t))))
              (lambda (sh? widf wrtf)
                (when sh?
                  (let ((c (count-ref x)))
                    (count-set! x (+ c 1))
                    (if (> c 0) (set! marks? #t))))))))
        ; optionally detect cycles in O(N)
        (when (and marks? cycles-only?)
          (set! marks? #f)
          (let find-cycles ((x sexp) (v env) (up '()))
            (unless (not-shareable? x)
              (let ((c (count-ref x)))
                (cond ((eq? c 'cycle))
                      ((memq x up)
                       (count-set! x 'cycle)
                       (set! marks? #t))
                      ((eq? c 'visited))
                      (else
                       (let ((up (if (> c 1) (cons x up) up)))
                         (count-set! x 'visited)
                         (dispatch-on-type x
                           (lambda (pfx tolf toxf sfx)
                             (let ((l (tolf x)))
                               (when (pair? l)
                                 (find-cycles (car l) v up)
                                 (find-cycles (cdr l) v up))))
                           (lambda (pfx lenf reff sfx) 42) (lambda (sh? widf wrtf) 42)))))))))
        ; rebuild x with sharing marks as needed in O(N)
        (if marks?
            (let ((ids (make-eq-table)) (next-id 0))
              (define (rebuild x)
                (if (not-shareable? x)
                    x
                    (let ((c (count-ref x)))
                      (if (if cycles-only? (eq? c 'cycle) (> c 1))
                          (cond ((table-ref ids x #f) =>
                                 (lambda (id) (shared-mark #f id x)))
                                (else
                                 (let ((id next-id))
                                   (set! next-id (+ next-id 1))
                                   (table-set! ids x id)
                                   (shared-mark #t id (recur x)))))
                          (recur x)))))
              (define (recur x)
                (dispatch-on-type x
                  (lambda (pfx tolf toxf sfx)
                    (let ((l (tolf x)))
                      (if (pair? l)
                          (let* ((h (rebuild (car l))) (t (rebuild (cdr l))))
                            (if (and (eq? h (car l)) (eq? t (cdr l)))
                                x
                                (toxf (cons h t)))))))
                  (lambda (pfx lenf reff sfx) x) (lambda (sh? widf wrtf) x)))
              (rebuild sexp))
            sexp)))


    ; basic formatting operations

    ; guess print length of atoms -- better be fast than exact
    ; call directly only in cases that don't need override
    (define ln-of-10 2.302585092994046)
    (define (atom-width x)
      (cond ((or (null? x) (boolean? x)) 2)
            ((symbol? x) (string-length (symbol->string x))) ; inexact
            ((string? x) (+ (string-length x) 2)) ; ignores escapes!
            ((and (char? x) (char<=? #\! x #\~)) 3)
            ((and (integer? x) (exact? x)) ; don't print bignums
             (cond ((<= 0 x 9) 1)
                   ((<= -9 x 99) 2)
                   ((<= -99 x 999) 3)
                   ((> x 0) (exact (ceiling (/ (log (+ x 0.1)) ln-of-10))))
                   (else
                    (+ 1 (exact (ceiling (/ (log (- 0.1 x)) ln-of-10)))))))
            ((and (rational? x) (exact? x))
             (+ (atom-width (numerator x)) 1 (atom-width (denominator x))))
            ((assv x '((#\tab . 5) (#\newline . 9) (#\space . 7))) => cdr)
            (else
             (let* ((p (open-output-string)) ; slow but exact
                    (s (begin (write x p) (get-output-string p))))
               (close-output-port p)
               (string-length s)))))

    ; indentation ind is either an exact nonnegative integer or #f meaning
    ; "we are printing inline code, so it doesn't matter"

    ; safe increment for indentation (handles #f)
    (define (ind+ i n) (and i (+ i n)))

    (define (abbrev? x)
      (and (pair? x) (pair? (cdr x)) (null? (cddr x))
           (memq (car x) '(quote quasiquote unquote unquote-splicing))))

    ; we use parameters themselves as keys: they are unique procedures
    ; note: pp only searches for and calls parameters it *knows*, not
    ; arbitrary args!!
    (define (param-value args param . oconv)
      (define conv (if (pair? oconv) (car oconv) (lambda (x) x)))
      (let loop ((a args))
        (cond ((null? a) (param)) ; already converted, no need for conv
              ((and (pair? (cdr a)) (eq? (car a) param)) (conv (cadr a)))
              ((and (pair? (cdr a)) (procedure? (car a))) (loop (cddr a)))
              (else (error "invalid pp parameter list" a)))))

    ; the body of the formatter is embeded into pp to allow direct
    ; access to the external parameters through the local environment
    ; instead of threading them through the code
    (define (pp sexp . rest)
      (define-values (*port* kwargs)
        (if (and (pair? rest) (output-port? (car rest)))
            (values (car rest) (cdr rest))
            ; if port is not given as optional, look for the kw
            (values (current-output-port) rest)))

      ; bring in all external parameters as lexical vars
      (define *width* (param-value kwargs pp-width conv-width))
      (define *circle* (param-value kwargs pp-circle))
      (define *graph* (param-value kwargs pp-graph))

      ; shortcut output routines used below
      (define (emit s) (display s *port*))
      ; inserts single space if ind is #f; otherwise prints newline and indents to ind
      (define (space ind v)
        (cond ((not ind) (write-char #\space *port*))
              (else
               (newline *port*)
               (do ((i 0 (+ i 1))) ((>= i ind))
                 (write-char #\space *port*)))))

      ; we calculate width on S-exps directly; this is far from exact,
      ; but should do for our purposes. Stops early if cap is reached,
      ; returning a value larger than cap. Doing it this way helps to
      ; keep fit-ind width calculations O(1) by input size
      ; This version of fit-ind does not use call/cc
      (define (fit-ind x ind)
        ; LispKit: pairs are immutable, so counters are boxes
        (define (make-cnt ind) (box (- *width* ind)))
        (define cnt-val unbox)
        (define cnt-set! set-box!)
        (define (cnt-zero? cnt) (<= (cnt-val cnt) 0))
        (define (cnt-sub cnt val)
          (>= (begin (cnt-set! cnt (- (cnt-val cnt) val)) (cnt-val cnt)) 0))
        (define (fits-tail? l cnt)
          (and (fits? (car l) cnt)
               (let ((t (cdr l)))
                 (or (null? t)
                     (if (pair? t) ; may be improper!
                         (and (cnt-sub cnt 1) (fits-tail? t cnt))
                         (and (cnt-sub cnt 3) (fits? t cnt)))))))
        (define (fits-mark? x cnt)
          (let-values (((first? id val) (shared-unmark x)))
            (and (cnt-sub cnt (+ (atom-width id) 2))
                 (if first? (fits? val cnt) (cnt-sub cnt 0)))))
        (define (fits-abbrev? x cnt)
          (let ((abr (car x)) (arg (cadr x)))
            (and (cnt-sub cnt (case abr ((unquote-splicing) 2) (else 1)))
                 (fits? (cadr x) cnt))))
        (define (fits-list-like? x cnt pfx lst sfx)
          (and (cnt-sub cnt (+ (string-length pfx) (string-length sfx)))
               (or (null? lst) (fits-tail? lst cnt))))
        (define (fits-vector-like? x cnt pfx lenf reff sfx)
          (and (cnt-sub cnt (+ (string-length pfx) (string-length sfx)))
               (let loop ((i 0) (n (lenf x)))
                 (or (= i n) (and (fits? (reff x i) cnt) (loop (+ i 1) n))))))
        (define (fits? x cnt)
          (cond ((shared-mark? x) (fits-mark? x cnt))
                ((abbrev? x) (fits-abbrev? x cnt))
                (else
                 (dispatch-on-type x
                   (lambda (pfx tolf toxf sfx)
                     (fits-list-like? x cnt pfx (tolf x) sfx))
                   (lambda (pfx lenf reff sfx)
                     (fits-vector-like? x cnt pfx lenf reff sfx))
                   (lambda (sh? widf wrtf) (cnt-sub cnt (widf x)))))))
        (if (or (not ind) (fits? x (make-cnt ind))) #f ind))

      (define (print-mark x ind v)
        (let-values (((first? id x) (shared-unmark x)))
          (emit "#")
          (emit id)
          (emit (if first? "=" "#"))
          (when first?
            (let ((ilen (atom-width id)))
              (print-datum x (ind+ ind (+ ilen 2)) v)))))

      (define (print-abbrev x ind v)
        (let ((abr (car x)) (arg (cadr x)))
          (case abr
            ((quote) (emit "'"))
            ((quasiquote) (emit "`"))
            ((unquote) (emit ","))
            ((unquote-splicing) (emit ",@")))
          ; fixme: if abr ends in , and arg is a symbol @xxxx, add space!
          (let
            ((ind (ind+ ind (case abr ((unquote-splicing) 2) (else 1)))))
            (print-datum arg ind v))))

      (define (print-list-like x ind v pfx lst sfx)
        (let ((ind (fit-ind x ind)))
          (emit pfx)
          (do
            ((ind (ind+ ind (string-length pfx)))
             (first? #t #f)
             (lst lst (cdr lst)))
            ((not (pair? lst))
             (unless (null? lst)
               (emit " . ")
               (print-datum lst (ind+ ind 3) v)))
            (unless first? (space ind v))
            (print-datum (car lst) ind v))
          (emit sfx)))

      (define (print-vector-like x ind v pfx lenf reff sfx)
        (let ((ind (fit-ind x ind)) (vlen (lenf x)))
          (emit pfx)
          (do ((ind (ind+ ind (string-length pfx))) (idx 0 (+ idx 1)))
            ((= idx vlen))
            (unless (zero? idx) (space ind v))
            (print-datum (reff x idx) ind v))
          (emit sfx)))

      (define (print-datum x ind v)
        (cond ((shared-mark? x) (print-mark x ind v))
              ((abbrev? x) (print-abbrev x ind v))
              (else
               (dispatch-on-type x
                 (lambda (pfx tolf toxf sfx)
                   (print-list-like x ind v pfx (tolf x) sfx))
                 (lambda (pfx lenf reff sfx)
                   (print-vector-like x ind v pfx lenf reff sfx))
                 (lambda (sh? widf wrtf) (wrtf x *port*))))))

      (let* ((pg (if *graph* 2 (if *circle* 1 0)))
             (env 42) ; environment: reserved for the future
             (x (if (> pg 0) (mark-shared sexp env (= pg 1)) sexp)))
        (print-datum x 0 env)
        (newline *port*)))

    ; ignores pp-graph/pp-circle params; will hang on cycles
    ; this one is the fastest of them all
    (define pprint-simple
      (case-lambda
        ((obj) (pp obj pp-graph #f pp-circle #f))
        ((obj port) (pp obj port pp-graph #f pp-circle #f))))

    ; ignores pp-graph/pp-circle params; only marks cycles
    ; spends time on detecting shared structures, and more on cycles
    (define pprint
      (case-lambda
        ((obj) (pp obj pp-graph #f pp-circle #t))
        ((obj port) (pp obj port pp-graph #f pp-circle #t))))

    ; ignores pp-graph/pp-circle param; marks all shared
    ; this one is actually faster than pprint
    (define pprint-shared
      (case-lambda
        ((obj) (pp obj pp-graph #t pp-circle #t))
        ((obj port) (pp obj port pp-graph #t pp-circle #t))))


    ; initialize format hook registry with LispKit-specific data types;
    ; the representations match the ones used by `write`

    (pp-hooks
      (add-pp-hook (pp-hooks) box?
        (glist-pp-hook "#<box " (lambda (x) (list (unbox x)))
          (lambda (x) (box (car x))) ">")))
    (pp-hooks
      (add-pp-hook (pp-hooks) mpair?
        (glist-pp-hook "#<pair " (lambda (x) (list (mcar x) (mcdr x)))
          (lambda (x) (mcons (car x) (cadr x))) ">")))
    (pp-hooks
      (add-pp-hook (pp-hooks) gvector?
        (glist-pp-hook "#g(" gvector->list list->gvector ")")))))
