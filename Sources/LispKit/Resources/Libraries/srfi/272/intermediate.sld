;;; SRFI 272 INTERMEDIATE
;;; Pretty Printing: intermediate library
;;;
;;; Library `(srfi 272 intermediate)` provides all the functionality of library
;;; `(srfi 272 basic)`, adding support for keyword arguments in `pp`, `pprint`,
;;; `pprint-shared`, and `pprint-simple`, procedure `pp*` accepting a final list of
;;; keyword arguments, procedure `pprint-file` for pretty printing the content of
;;; source files, parameters `pp-radix`, `pp-level`, and `pp-length` for controlling
;;; the printing of numbers and the imposition of level/length limits, as well as
;;; procedure `pretty-style` for customizing the formatting of special forms.
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

(define-library (srfi 272 intermediate)
  (import (scheme base) (scheme char) (scheme cxr)
    (scheme case-lambda) (scheme inexact) (scheme file)
    (scheme read) (scheme write))

  ; char-width, returning #f 0 1 2
  (import (srfi 272 measure)
          (only (lispkit string) string-display-width))

  ; LispKit libraries
  (import (only (lispkit box) box? box unbox set-box! mpair? mcons mcar mcdr)
          (only (lispkit gvector) gvector? gvector->list list->gvector)
          (only (lispkit hashtable) make-eq-hashtable hashtable-ref hashtable-set!
                hashtable? hashtable-size hashtable->alist)
          (only (lispkit record) record? record-type record-type-name
                record-type-field-names record-ref))

  ; procedures
  (export pp pp* pprint pprint-shared pprint-simple pprint-file)

  ; configuration
  (export pretty-style)

  ; parameters
  (export pp-width pp-circle pp-graph pp-radix pp-length
    pp-level)

  (begin
    (define (cv-width x)
      (if (and (number? x) (exact? x) (> x 0))
          x
          (error "invalid value for pp-width" x)))
    (define pp-width (make-parameter 80 cv-width))

    ; detect and mark cyclic substructure
    (define (cv-boolean x) (not (not x)))
    (define pp-circle (make-parameter #t cv-boolean))

    ; detect and mark shared AND cyclic substructure
    (define pp-graph (make-parameter #f cv-boolean))

    ; radix for (at least) exact integers; 2, 8, 10, 16 are supported
    ; if not 10, prefix is printed when needed to keep numbers readable
    ; inexact numbers printed in a system-dependent machine-readable way
    (define (cv-radix x)
      (case x
        ((2 8 10 16) x)
        (else (error "invalid value for pp-radix" x))))
    (define pp-radix (make-parameter 10 cv-radix))

    ; #f or max numbers of subitems to display in a sequence
    (define (cv-length x)
      (if (or (not x) (and (number? x) (exact? x) (>= x 0)))
          x
          (error "invalid value for pp-length" x)))
    (define pp-length (make-parameter #f cv-length)) ; no limit

    ; #f or max depth of subitems to display in a sequence
    (define (cv-level x)
      (if (or (not x) (and (number? x) (exact? x) (>= x 0)))
          x
          (error "invalid value for pp-level" x)))
    (define pp-level (make-parameter #f cv-level)) ; no limit

    ; interface to formatting style registry
    (define (pretty-style sym . args)
      (let ((tab (pp-styles)))
        (if (null? args)
            (table-ref tab sym #f)
            (begin (table-set! tab sym (car args)) #f))))

    ; predicate-based hooks for nonstandard data
    (define pp-hooks (make-parameter '()))

    ; adding a hook to the explicit hook registry
    (define (add-pp-hook hooks pred . opt-hook)
      (define hook (if (null? opt-hook) #f (car opt-hook)))
      (if (assv pred hooks)
          (alist-addv pred hook hooks) ; replaces at its original pos
          (cons (cons pred hook) hooks))) ; adds to front

    ; generalized read macro hook constructor
    (define (rmac-pp-hook pfx reff tomf)
      (list 'rm pfx reff tomf))

    ; generalized list hook constructor
    (define (glist-pp-hook pfx tolf toxf sfx)
      (list 'gl pfx tolf toxf sfx))

    ; binary vector hook constructor
    (define (bvec-pp-hook pfx lenf reff sfx)
      (list 'bv pfx lenf reff sfx))

    ; atomic hook constructor
    (define (atom-pp-hook sh? widf wrtf)
      (list 'at sh? widf wrtf))

    ; portable eq table, similar to R6RS(?) eq-hashtable

    ; LispKit: eq tables are native eq hashtables
    (define make-eq-table make-eq-hashtable)
    (define table-ref hashtable-ref)
    (define table-set! hashtable-set!)


    ; formatting style registry (private)
    (define pp-styles (make-parameter (make-eq-table)))

    ; functional modifications of alists
    (define (alist-addv key val alist)
      (let loop ((l alist))
        (cond ((null? l) (list (cons key val)))
              ((eqv? key (caar l))
               (if (equal? val (cdar l)) l (cons (cons key val) (cdr l))))
              (else
               (let ((rest (loop (cdr l))))
                 (if (eq? rest (cdr l)) l (cons (car l) rest)))))))

    ; in Unicode setting takes 0-wide and 2-wide chars into account
    ; LispKit: while the default char width procedure is in use, whole strings are
    ; measured via string-display-width, which handles surrogate pairs and grapheme
    ; clusters; non-printable characters are counted like in the general case
    (define default-char-width (char-width-procedure))
    (define (string-width s)
      (define char-width (char-width-procedure))
      (define n (string-length s))
      (if (eq? char-width default-char-width)
          (let loop ((i 0) (w (string-display-width s)))
            (if (= i n)
                w
                (let ((cp (char->integer (string-ref s i))))
                  (loop (+ i 1)
                        (cond ((< cp 32) (+ w 2))
                              ((<= 127 cp 159) (+ w 7))
                              (else w))))))
          (let loop ((i 0) (w 0))
            (if (= i n)
                w
                (let* ((ch (string-ref s i)) (cw (char-width ch)))
                  (loop (+ i 1) (+ w (or cw (if (char<? ch #\space) 2 7)))))))))

    (define (quoted-string-width s) (+ 2 (string-width s)))

    ; width of written representation (slow but exact)
    (define (written-width x)
      (let* ((p (open-output-string)) ; slow but exact
             (s (begin (write x p) (get-output-string p))))
        (close-output-port p)
        (string-width s)))

    ; this list should contain rmacs supported by the reader by default
    ; fixme: add a way to space ,@ if followed by a symbol that starts with @
    (define builtin-read-macros
      '((quote "'" d)
        (quasiquote "`" e)
        (unquote "," e)
        (unquote-splicing ",@" e)))

    ; the body of the formatter is embeded into pp to allow direct
    ; access to the external parameters through the local environment
    ; instead of threading them through the code
    (define (pp sexp . rest)
      (define-values (*port* kv*)
        (if (and (pair? rest) (output-port? (car rest)))
            (values (car rest) (cdr rest))
            ; if port is not given as optional, look for the kw
            (values (current-output-port) rest)))

      ; we use parameters themselves as keys: they are unique procedures
      ; note: pp only searches for and calls parameters it *knows*, not
      ; arbitrary args!!
      (define (kval args param . oconv)
        (define conv (if (pair? oconv) (car oconv) (lambda (x) x)))
        (let loop ((a args))
          (cond ((null? a) (param)) ; already converted, no need for conv
                ((and (pair? (cdr a)) (eq? (car a) param)) (conv (cadr a)))
                ((and (pair? (cdr a)) (procedure? (car a))) (loop (cddr a)))
                (else (error "invalid pp parameter list" a)))))

      ; bring in all external parameters as lexical vars
      (define *width* (kval kv* pp-width cv-width))
      (define *circle* (kval kv* pp-circle cv-boolean))
      (define *graph* (kval kv* pp-graph cv-boolean))
      (define *radix* (kval kv* pp-radix cv-radix))
      (define *length* (kval kv* pp-length cv-length))
      (define *level* (kval kv* pp-level cv-level))
      (define *indent* 0)
      (define *tab* 1)
      (define *alt-tab* 4)
      (define *miser-width* #f)
      (define *inline-width* 60)
      (define *brackets* #f)
      (define *code* #t)
      (define *length-stub* "...")
      (define *level-stub* #t) ; string (e.g. "#") or #t for Chez "shells"

      ; params to be zeroed if we are within *miser-width* from width
      (define (std-indent-miser ind)
        (if (and ind (>= (+ ind *miser-width*) *width*)) 0 *tab*))
      (define std-indent
        (if *miser-width* std-indent-miser (lambda (ind) *tab*)))
      (define (alt-indent-miser ind)
        (if (and ind (>= (+ ind *miser-width*) *width*))
            0
            *alt-tab*))
      (define alt-indent
        (if *miser-width* alt-indent-miser (lambda (ind) *alt-tab*)))

      ; indentation ind is either an exact nonnegative integer or #f meaning
      ; "we are printing inline code, so it doesn't matter"
      ; safe increment for indentation (handles #f)
      (define (ind+ i n) (and i (+ i n)))

      ; initial environment and cutoff machinery
      ; level/length environment is #f or a pair (cur-index . cur-level)
      (define nop (lambda (v) v))
      (define non (if *length* (lambda (v) (cons 0 (cdr v))) nop))
      (define step
        (if *length* (lambda (v) (cons (+ (car v) 1) (cdr v))) nop))
      (define nest
        (if *level* (lambda (v) (cons 0 (+ (cdr v) 1))) non))
      ; improper tail is not a step ahead of the previous element!
      (define back (lambda (v) (cons (- (car v) 1) (cdr v))))
      (define nocut (lambda (v) #f))
      (define cuti?
        (if *length* (lambda (v) (>= (car v) *length*)) nocut))
      (define cutd?
        (if *level* (lambda (v) (>= (cdr v) *level*)) nocut))
      (define env (cons 0 0)) ; ignored if no cutoffs are set
      (define cuti-wid (string-width *length-stub*))
      (define cutd-widf
        (if (string? *level-stub*)
            (let ((w (string-width *level-stub*)))
              (lambda (x)
                (dispatch-on-type x (lambda (pfx reff tomf) w)
                  (lambda (pfx tolf toxf sfx) w)
                  (lambda (pfx lenf reff sfx) w)
                  (lambda (sh? widf wrtf) (widf x))))) ; atom
            (lambda (x)
              (dispatch-on-type x
                (lambda (pfx reff tomf) (+ (string-width pfx) cuti-wid))
                (lambda (pfx tolf toxf sfx)
                  (+ (string-width pfx) cuti-wid (string-width sfx)))
                (lambda (pfx lenf reff sfx)
                  (+ (string-width pfx) cuti-wid (string-width sfx)))
                (lambda (sh? widf wrtf) (widf x)))))) ; atom

      ; shortcut output routines used below
      (define (emit s) (display s *port*))
      (define (emit-atom x) (write-atom x *port*))
      ; inserts single space if ind is #f; otherwise prints newline and indents to ind
      (define (space ind v)
        (cond ((not ind) (write-char #\space *port*))
              (else
               (newline *port*)
               (do ((i 0 (+ i 1))) ((>= i ind))
                 (write-char #\space *port*)))))
      (define (emit-lpar) (write-char #\( *port*))
      (define (emit-rpar) (write-char #\) *port*))
      (define (emit-lbra)
        (write-char (if *brackets* #\[ #\() *port*))
      (define (emit-rbra)
        (write-char (if *brackets* #\] #\)) *port*))
      (define (emit-cuti) (display *length-stub* *port*))
      (define print-cutd
        (if (string? *level-stub*) ; CL-like model
            (lambda (x bk?)
              (dispatch-on-type x
                (lambda (pfx reff tomf) (emit *level-stub*))
                (lambda (pfx tolf toxf sfx) (emit *level-stub*))
                (lambda (pfx lenf reff sfx) (emit *level-stub*))
                (lambda (sh? widf wrtf) (wrtf x *port*)))) ; atoms
            ; Chez-like model: print "shell" of the object
            (lambda (x bk?)
              (cond ((and bk? (pair? x)) (emit-lbra) (emit-cuti) (emit-rbra))
                    (else
                     (dispatch-on-type x
                       (lambda (pfx reff tomf) (emit pfx) (emit-cuti))
                       (lambda (pfx tolf toxf sfx)
                         (emit pfx)
                         (emit-cuti)
                         (emit sfx))
                       (lambda (pfx lenf reff sfx)
                         (emit pfx)
                         (emit-cuti)
                         (emit sfx))
                       (lambda (sh? widf wrtf) (wrtf x *port*))))))))

      ; locating hooks and calling handlers
      ; 'normalizes' input objects to simplify and sync all phases
      (define (dispatch-on-type x retm retl retv reta)
        (let loop ((al (pp-hooks)))
          (cond ((null? al) ; dispatch on builtins
                 (cond ((and (pair? x) (symbol? (car x)) (pair? (cdr x)) (null? (cddr x))
                             (assq (car x) builtin-read-macros))
                        =>
                        (lambda (l)
                          (retm (cadr l) cadr (lambda (x) (list (car l) x)))))
                       ((pair? x) (retl "(" (lambda (x) x) (lambda (x) x) ")"))
                       ((vector? x) (retl "#(" vector->list list->vector ")"))
                       ((string? x) (reta #t quoted-string-width write))
                       ((bytevector? x) ; NB: contents non-markable!
                        (retv "#u8(" bytevector-length bytevector-u8-ref ")"))
                       (else (reta #f atom-width write-atom))))
                (((caar al) x) =>
                 (lambda (res)
                   (let ((hk (or (cdar al) res)))
                     (unless (pair? hk) (error "invalid hook!"))
                     (case (car hk)
                       ((rm) (apply retm (cdr hk)))
                       ; list-like: glist-pp-hook pfx tolf toxf sfx
                       ((gl) (apply retl (cdr hk)))
                       ; binary vector: bvec-pp-hook pfx lenf reff sfx
                       ((bv) (apply retv (cdr hk)))
                       ; atomic: atom-pp-hook sh? widf wrtf
                       ((at) (apply reta (cdr hk)))
                       ; TODO: pre-check, this shouldn't happen!
                       (else (error "invalid hook!"))))))
                (else (loop (cdr al))))))

      ; graph sharing/cycles detection
      ; NB: if pp-graph is off, pp-circle determines if it is called with cycles-only? #t
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
      (define (mark-shared sexp env cycles-only?)
        (let ((counts (make-eq-table)) (marks? #f))
          ; LispKit: pairs are immutable values without identity (eq? on pairs is
          ; structural), so they are never counted, labeled, or used as table keys
          (define (count-ref x) (if (pair? x) 0 (table-ref counts x 0)))
          (define (count-set! x c) (if (not (pair? x)) (table-set! counts x c)))
          (let scan ((x sexp) (v env))
            (unless (not-shareable? x)
              (dispatch-on-type x
                (lambda (pfx reff tomf)
                  (unless (cutd? v)
                    (let ((c (count-ref x)))
                      (count-set! x (+ c 1))
                      ; Caveat: surprisingly, no v nesting is done here!
                      ; That's how CL and Chez treat their abbreviations
                      (if (= c 0) (scan (reff x) v) (set! marks? #t)))))
                (lambda (pfx tolf toxf sfx)
                  (unless (cutd? v)
                    (let ((c (count-ref x)))
                      (count-set! x (+ c 1))
                      (if (= c 0)
                          (let ((l (tolf x)))
                            (when (pair? l)
                              (scan (car l) (nest v))
                              (let ((l (cdr l)) (v (step v)))
                                (unless (and (cuti? v) (pair? l)) (scan l v)))))
                          (set! marks? #t)))))
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
                             (lambda (pfx reff tomf)
                               (unless (cutd? v) (find-cycles (reff x) v up)))
                             (lambda (pfx tolf toxf sfx)
                               (unless (cutd? v)
                                 (let ((l (tolf x)))
                                   (when (pair? l)
                                     (find-cycles (car l) (nest v) up)
                                     (let ((l (cdr l)) (v (step v)))
                                       (unless (and (cuti? v) (pair? l))
                                         (find-cycles l v up)))))))
                             (lambda (pfx lenf reff sfx) 42) (lambda (sh? widf wrtf) 42)))))))))
          ; rebuild x with sharing marks as needed in O(N)
          (if marks?
              (let ((ids (make-eq-table)) (next-id 0))
                (define (rebuild x v)
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
                                     (shared-mark #t id (recur x v)))))
                            (recur x v)))))
                (define (recur x v)
                  (dispatch-on-type x
                    (lambda (pfx reff tomf)
                      (if (cutd? v)
                          x
                          (let* ((e (reff x)) (ne (rebuild e v)))
                            (if (eq? e ne) x (tomf ne)))))
                    (lambda (pfx tolf toxf sfx)
                      (if (cutd? v)
                          x
                          (let ((l (tolf x)))
                            (if (pair? l)
                                (let*
                                  ((h (rebuild (car l) (nest v)))
                                   (t
                                    (let ((l (cdr l)) (v (step v)))
                                      (if (and (cuti? v) (pair? l)) l (rebuild l v)))))
                                  (if (and (eq? h (car l)) (eq? t (cdr l)))
                                      x
                                      (toxf (cons h t))))))))
                    (lambda (pfx lenf reff sfx) x) (lambda (sh? widf wrtf) x)))
                (rebuild sexp env))
              sexp)))

      ; guess print length of atoms -- better be fast than exact
      ; call directly only in cases that don't need override
      ; TODO: we can cache some non-trivial cases
      (define ln-of-10 2.302585092994046)
      (define (atom-width x)
        (cond ((or (null? x) (boolean? x)) 2)
              ((symbol? x) (string-width (symbol->string x))) ; inexact
              ((string? x) (quoted-string-width x)) ; may be inexact
              ((and (char? x) (char<=? #\! x #\~)) 3)
              ((and (= *radix* 10) (integer? x) (exact? x)) ; don't print bignums
               (cond ((<= 0 x 9) 1)
                     ((<= -9 x 99) 2)
                     ((<= -99 x 999) 3)
                     ((> x 0) (exact (ceiling (/ (log (+ x 0.1)) ln-of-10))))
                     (else
                      (+ 1 (exact (ceiling (/ (log (- 0.1 x)) ln-of-10)))))))
              ((and (= *radix* 10) (rational? x) (exact? x))
               (+ (atom-width (numerator x)) 1 (atom-width (denominator x))))
              ((assv x '((#\tab . 5) (#\newline . 9) (#\space . 7))) => cdr)
              ((and (number? x) (exact? x) (not (= *radix* 10)))
               (+ 2 (string-length (number->string x *radix*))))
              (else (written-width x))))

      (define (write-atom x port)
        (if (and (number? x) (exact? x) (not (= *radix* 10)))
            (let ((s (number->string x *radix*)))
              (write-char #\# port)
              (write-char (case *radix* ((2) #\b) ((8) #\o) ((16) #\x))
                port)
              (display s port))
            (write x port)))

      ; we calculate width on S-exps directly; this is far from exact,
      ; but should do for our purposes. Stops early if cap is reached,
      ; returning a value larger than cap. Doing it this way helps to
      ; keep fit-ind width calculations O(1) by input size
      ; make counter from an indent, considering width and inline limit
      (define cmake
        (let ((il (if *inline-width* *inline-width* *width*)))
          (lambda (ind)
            (box (if ind (min (- *width* ind) il) +inf.0)))))
      ; subtract n from c in-place, returning false if we felll below 0
      ; LispKit: pairs are immutable, so counters are boxes
      (define (csub c n)
        (>= (begin (set-box! c (- (unbox c) n)) (unbox c)) 0))
      (define (cdup c) (box (unbox c)))
      (define (fits-mark? x c v)
        (let-values (((first? id val) (shared-unmark x)))
          (and (csub c (+ (atom-width id) 2))
               (if first? (fits? val c v) (csub c 0)))))
      (define (fits-read-macro? x c v pfx elt)
        (and (csub c (string-width pfx)) (fits? elt c v))) ; caveat: no nesting here!
      (define (fits-tail? lst c v)
        (let loop ((sep 0) (l lst) (v v))
          (cond ((null? l) #t)
                ((not (pair? l)) (and (csub c 3) (fits? l c (back v))))
                ((cuti? v) (csub c (+ sep cuti-wid)))
                (else
                 (and (csub c sep) (fits? (car l) c v) (loop 1 (cdr l) (step v)))))))
      (define (fits-list-like? x c v pfx lst sfx)
        (and (csub c (+ (string-width pfx) (string-width sfx)))
             (fits-tail? lst c (nest v))))
      (define (fits-vector-like? x c v pfx lenf reff sfx)
        (and (csub c (+ (string-width pfx) (string-width sfx)))
             (let loop ((sep 0) (i 0) (v (nest v)) (n (lenf x)))
               (cond ((= i n) #t)
                     ((cuti? v) (csub c (+ sep cuti-wid)))
                     (else
                      (and (csub c sep) (fits? (reff x i) c v)
                           (loop 1 (+ i 1) (step v) n)))))))
      (define (fits? x c v)
        (cond ((shared-mark? x) (fits-mark? x c v))
              ((cutd? v) (csub c (cutd-widf x)))
              (else
               (dispatch-on-type x
                 (lambda (pfx reff tomf)
                   (fits-read-macro? x c v pfx (reff x)))
                 (lambda (pfx tolf toxf sfx)
                   (fits-list-like? x c v pfx (tolf x) sfx))
                 (lambda (pfx lenf reff sfx)
                   (fits-vector-like? x c v pfx lenf reff sfx))
                 (lambda (sh? widf wrtf) (csub c (widf x)))))))
      (define (fits-flat-or-stacked? x c v sr)
        (define c0 (cdup c))
        (define (fits*/stack? l v)
          (cond ((null? l) (set-box! c 0) sr)
                ((fits? (car l) (cdup c0) v)
                 (fits*/stack? (cdr l) (step v)))
                (else #f)))
        (or (fits? x c v)
            (and (not (cutd? v)) (list? x) (csub c0 1)
                 (fits*/stack? x (nest v)))))
      (define (fit-ind x ind v)
        (if (or (not ind) (fits? x (cmake ind) v)) #f ind))
      (define (fit-ind* x* ind v)
        (if (or (not ind) (fits-tail? x* (cmake ind) v)) #f ind))

      (define (print-mark x ind v print)
        (let-values (((first? id x) (shared-unmark x)))
          (emit "#")
          (emit id)
          (emit (if first? "=" "#"))
          (when first?
            (let ((ilen (atom-width id)))
              (print x (ind+ ind (+ ilen 2)) v)))))

      (define (print-read-macro x ind v pfx elt)
        (emit pfx)
        (let ((ind (ind+ ind (string-width pfx))))
          (print-datum elt ind v))) ; Caveat: no nesting!

      ; fill-style printing of (possibly improper) list contents
      ; if lst is atom, prints dot before it; v is env for lst head
      ; NB: adjacent closers may cause last line overflow!
      (define (print*/fill lst ind v restoff prin1 print)
        (define (fitsi? e c v)
          (if (cuti? v) (csub c cuti-wid) (fits? e c v)))
        (define (cari l v) (if (cuti? v) 42 (car l)))
        (define (prest x ind c v)
          (if (or (not ind) (and (csub c 3) (fitsi? x c v)))
              (begin (emit " . ") (print x #f v))
              (begin
                (space ind v)
                (emit ".")
                (let ((ind1 (fit-ind x (ind+ ind 2) v)))
                  (space (and ind1 ind) v)
                  (print x (and ind1 ind) v)))))
        (define (ploop l e v ind ind1 indi c prt)
          (if (cuti? v) (emit-cuti) (prt e indi v))
          (cond ((cuti? v))
                ((null? (cdr l)))
                ((not (pair? (cdr l))) (prest (cdr l) ind c (back v)))
                (else
                 (let* ((l (cdr l)) (v (step v)) (e (cari l v)))
                   (cond ((and (csub c 1) (fitsi? e c v))
                          (space #f v)
                          (ploop l e v ind ind1 #f c print))
                         (else
                          (let ((c (cmake ind1)))
                            (space ind1 v)
                            (fitsi? e c v)
                            (ploop l e v ind ind1 ind1 c print))))))))
        (cond ((null? lst))
              ((not (pair? lst)) (prest lst ind (cmake ind) (back v)))
              (else
               (let ((e (cari lst v)) (c (cmake ind)))
                 (fitsi? e c v) ; need to bump c!
                 (ploop lst e v ind (ind+ ind restoff) ind c prin1)))))

      ; regular one-per-line-on-overflow printing of list contents
      ; if lst is atom, prints dot before it; v is env for lst head
      ; prin1 is used to print first subexpression, print for the rest
      (define (print*/body lst ind v prin1 print)
        (let loop ((first? #t) (lst lst) (v v) (pr prin1))
          (cond ((null? lst))
                ((not (pair? lst)) ; TODO: better miser wrap?
                 (emit " . ")
                 (print lst (ind+ ind 3) (back v)))
                ((cuti? v) (unless first? (space ind v)) (emit-cuti))
                (else
                 (unless first? (space ind v))
                 (print (car lst) ind v)
                 (loop #f (cdr lst) (step v) print)))))

      (define (print-list-like x ind v pfx lst sfx)
        (let ((ind (fit-ind x ind v)))
          (emit pfx)
          (let ((ind (ind+ ind (string-width pfx))) (v (nest v)))
            (if (vector? x) ; hack: fill-print vectors
                (print*/fill lst ind v 0 print-datum print-datum)
                (print*/body lst ind v print-datum print-datum)))
          (emit sfx)))

      ; fill-style binary vector printing: saves verical space
      ; NB: adjacent closers may cause last-line overflow!
      (define (print-vector-like x ind v pfx lenf reff sfx)
        (define (fitsi? e c v)
          (if (cuti? v) (csub c cuti-wid) (fits? e c v)))
        (define (refi x i v) (if (cuti? v) 42 (reff x i)))
        (emit pfx)
        (let
          ((n (lenf x))
           (ind (ind+ ind (string-width pfx)))
           (v (nest v)))
          (unless (= 0 n)
            (let ((e (refi x 0 v)) (c (cmake ind)))
              (fitsi? e c v) ; we'll print anyway, just bump c!
              ; loop invariant: i is e's index, v is e's env
              (let loop ((i 0) (e e) (v v) (indi ind) (c c))
                (if (cuti? v) (emit-cuti) (print-datum e indi v))
                (unless (or (= i (- n 1)) (cuti? v))
                  (let* ((i (+ i 1)) (v (step v)) (e (refi x i v)))
                    (cond ((and (csub c 1) (fitsi? e c v))
                           (space #f v)
                           (loop i e v #f c))
                          (else
                           (let ((c (cmake ind)))
                             (space ind v)
                             (fitsi? e c v)
                             (loop i e v ind c))))))))))
        (emit sfx))

      (define (print-datum x ind v)
        (cond ((shared-mark? x) (print-mark x ind v print-datum))
              ((cutd? v) (print-cutd x #f)) ; no brackets w/o fmt!
              (else
               (dispatch-on-type x
                 (lambda (pfx reff tomf)
                   (print-read-macro x ind v pfx (reff x)))
                 (lambda (pfx tolf toxf sfx)
                   (print-list-like x ind v pfx (tolf x) sfx))
                 (lambda (pfx lenf reff sfx)
                   (print-vector-like x ind v pfx lenf reff sfx))
                 (lambda (sh? widf wrtf) (wrtf x *port*))))))

      ; default list exp printer; precondition: x is a pair
      (define (print-app x ind v)
        (let ((ind (fit-ind x ind v)))
          (emit-lpar)
          (let ((ind (ind+ ind 1)) (v (nest v)))
            (if (and (symbol? (car x)) (pair? (cdr x)) (not (cuti? v)))
                (let ((oplen (atom-width (car x))))
                  (if (< oplen (alt-indent ind)) ; ind = len + 1 space
                      (begin
                        (emit-atom (car x))
                        (emit " ")
                        (print*/fill (cdr x) (ind+ ind (+ 1 oplen)) (step v) 0
                          print-exp print-exp))
                      (print*/fill x ind v (std-indent ind) print-exp print-exp)))
                (print*/fill x ind v (std-indent ind) print-exp print-exp)))
          (emit-rpar)))

      ; clause printer, prin1 is used for the head
      (define (print-clause x ind v prin1)
        (let ((ind (fit-ind x ind v)))
          (cond ((shared-mark? x) (print-mark x ind v print-exp))
                ((cutd? v) (print-cutd x #t)) ; brackets!
                ((and (list? x) (= (length x) 3) (eq? (cadr x) '=>)) ; (* => *)
                 (emit-lbra)
                 (print*/fill x (ind+ ind 1) (nest v) 0 prin1 print-exp)
                 (emit-rbra))
                ((pair? x)
                 (emit-lbra)
                 (print*/body x (ind+ ind 1) (nest v) prin1 print-exp)
                 (emit-rbra))
                (else (print-datum x ind v)))))

      (define (print-datum-clause x ind v)
        (print-clause x ind v print-datum))
      (define (print-exp-clause x ind v)
        (print-clause x ind v print-exp))

      ; clause block printer, prin1 is used for each clause's head
      (define (print-clauses x ind v prin1)
        (define (print x ind v) (print-clause x ind v prin1))
        (let ((ind (fit-ind x ind v)))
          (cond ((shared-mark? x) (print-mark x ind v print-exp))
                ((cutd? v) (print-cutd x #f)) ; no brackets!
                ((pair? x)
                 (emit-lpar)
                 (print*/body x (ind+ ind 1) (nest v) print print)
                 (emit-rpar))
                (else (print-datum x ind v)))))

      (define (print-exp x ind v)
        (cond ((shared-mark? x) (print-mark x ind v print-exp))
              ((cutd? v) (print-cutd x #f)) ; no brackets w/o fmt!
              (else
               (dispatch-on-type x
                 (lambda (pfx reff tomf)
                   (print-read-macro x ind v pfx (reff x)))
                 (lambda (pfx tolf toxf sfx)
                   (let ((l (tolf x)))
                     (if (and (eq? l x) (pair? l)) ; regular pair
                         (print-list-exp l ind v)
                         (print-list-like x ind v pfx l sfx))))
                 (lambda (pfx lenf reff sfx)
                   (print-vector-like x ind v pfx lenf reff sfx))
                 (lambda (sh? widf wrtf) (wrtf x *port*))))))

      (define (print-list-exp x ind v)
        (cond ((not (symbol? (car x))) (print-app x ind v))
              ((pretty-style (car x)) =>
               (lambda (fmt)
                 (if (eq? (cdr fmt) 'fill)
                     (print-app x ind v) ; special case
                     (print/fmt (cons 'i (cdr fmt)) x ind v))))
              (else (print-app x ind v))))

      (define (print/fmt fmt x ind v)
        (let ((ind (fit-ind x ind v)))
          (cond ((eq? fmt 'e) (print-exp x ind v))
                ((memq fmt '(d i)) (print-datum x ind v))
                ((eq? fmt 'dc) (print-clause x ind v print-datum))
                ((eq? fmt 'ec) (print-clause x ind v print-exp))
                ((eq? fmt 'ec*) (print-clauses x ind v print-exp))
                ((eq? fmt 'dc*) (print-clauses x ind v print-datum))
                (else
                 (emit-lpar)
                 (print/fmt* x (ind+ ind 1) (nest v) fmt)
                 (emit-rpar)))))

      ; print via atomic tail format
      ; if lst is atom, prints dot before it; v is env for lst head
      (define (print/fmt-tail lst ind v fmt)
        (define print
          (case fmt
            ((dc*) print-datum-clause)
            ((ec*) print-exp-clause)
            (else print-exp)))
        (print*/body lst ind v print print))

      ; format-controlled printing of (possibly improper) list contents
      ; if lst is atom, prints dot before it; v is env for lst head
      ; NB: adjacent closers may cause last line overflow!
      (define (print/fmt* lst ind v fmt*)
        (define roff (std-indent ind))
        (define (fitsi? e c v)
          (if (cuti? v) (csub c cuti-wid) (fits? e c v)))
        (define (fitsfsi? e c v sr)
          (if (cuti? v)
              (csub c cuti-wid)
              (fits-flat-or-stacked? e c v sr)))
        (define (cari l v) (if (cuti? v) 42 (car l)))
        (define (fmcdr fmt*) (if (pair? fmt*) (cdr fmt*) fmt*))
        (define (prcar fmt* x ind v)
          (cond ((not (pair? fmt*)) (print-exp x ind v))
                ((shared-mark? x) (print-mark x ind v print-exp))
                ((cutd? v) (print-cutd x (memq (car fmt*) '(dc ec))))
                (else (print/fmt (car fmt*) x ind v))))
        (define (prest x ind c v)
          (if (or (not ind) (and (csub c 3) (fitsi? x c v)))
              (begin (emit " . ") (print-datum x #f v))
              (begin
                (space ind v)
                (emit ".")
                (let ((ind1 (fit-ind x (ind+ ind 2) v)))
                  (space (and ind1 ind) v)
                  (print-datum x (and ind1 ind) v)))))
        (define (pind l e v ind ind1)
          (define aoff
            (and (eq? l lst) (symbol? e)
                 (let ((oplen (atom-width e)))
                   (and (< oplen (alt-indent ind)) (+ 1 oplen)))))
          (space (if aoff #f ind1) v) ; just 1 space if alt
          (if aoff (ind+ ind aoff) ind1))
        (define (ploop l e v ind ind1 indi c fmt*)
          (if (and (pair? fmt*) (eq? (car fmt*) 'i?))
              (if (symbol? e) ; optional id is present, use it
                  (pnext l e v ind ind1 indi c (cons 'i (cdr fmt*)))
                  (ploop l e v ind ind1 indi c (cdr fmt*)))
              (pnext l e v ind ind1 indi c fmt*)))
        (define (pnext l e v ind ind1 indi c fmt*)
          (if (cuti? v) (emit-cuti) (prcar fmt* e indi v))
          (cond ((cuti? v))
                ((null? (cdr l)))
                ((not (pair? (cdr l))) (prest (cdr l) ind c (back v)))
                ((memq (fmcdr fmt*) '(dc* ec* body)) ; tail part ahead
                 (let ((ind2 (pind l e v ind ind1)))
                   (print/fmt-tail (cdr l) ind2 (step v) (fmcdr fmt*))))
                (else
                 (let* ((ll l) (le e) (l (cdr l)) (v (step v)) (e (cari l v)))
                   (cond ((and ind (eq? ll lst) (symbol? le) (equal? '(i dc* . body) fmt*)
                               (let ((oplen (atom-width le)))
                                 (and (< oplen (alt-indent ind)) (csub c 1)
                                      (fitsfsi? e c v (+ oplen 1)))))
                          =>
                          (lambda (r)
                            (define indr (if (integer? r) (ind+ ind r) #f))
                            (space #f v)
                            (ploop l e v ind ind1 indr c (fmcdr fmt*))))
                         ((and (csub c 1) (fitsi? e c v))
                          (space #f v)
                          (ploop l e v ind ind1 #f c (fmcdr fmt*)))
                         (else
                          (let ((c (cmake ind1)))
                            (space ind1 v)
                            (fitsi? e c v)
                            (ploop l e v ind ind1 ind1 c (fmcdr fmt*)))))))))
        (cond ((null? lst))
              ((not (pair? lst)) (prest lst ind (cmake ind) v))
              (else
               (let ((e (cari lst v)) (c (cmake ind)))
                 (fitsi? e c v) ; need to bump c!
                 (ploop lst e v ind (ind+ ind roff) ind c fmt*)))))

      (let* ((pg (if *graph* 2 (if *circle* 1 0)))
             (x (if (> pg 0) (mark-shared sexp env (= pg 1)) sexp)))
        (cond (*code* (print-exp x *indent* env))
              (else (print-datum x *indent* env)))
        (newline *port*)))

    ; accepts a keyword-value list as last argument
    (define (pp* obj arg . args)
      (define (cons* arg . args)
        (let loop ((xs (cons arg args)))
          (if (null? (cdr xs))
              (car xs)
              (cons (car xs) (loop (cdr xs))))))
      (apply pp obj (apply cons* arg args)))

    ; overrides pp-graph/pp-circle params; will hang on cycles
    ; this one is the fastest of them all
    (define (pprint-simple obj . rest)
      (define-values (port kv*)
        (if (and (pair? rest) (output-port? (car rest)))
            (values (car rest) (cdr rest))
            (values (current-output-port) rest)))
      (pp* obj port pp-graph #f pp-circle #f kv*))

    ; overrides pp-graph/pp-circle params; only marks cycles
    ; spends time on detecting shared structures, and more on cycles
    (define (pprint obj . rest)
      (define-values (port kv*)
        (if (and (pair? rest) (output-port? (car rest)))
            (values (car rest) (cdr rest))
            (values (current-output-port) rest)))
      (pp* obj port pp-graph #f pp-circle #t kv*))

    ; overrides pp-graph/pp-circle param; marks all shared
    ; this one is actually faster than pprint
    (define (pprint-shared obj . rest)
      (define-values (port kv*)
        (if (and (pair? rest) (output-port? (car rest)))
            (values (car rest) (cdr rest))
            (values (current-output-port) rest)))
      (pp* obj port pp-graph #t pp-circle #t kv*))

    ; reads input file, pretty-prints it to output file or current output
    ; top-level line comments are preserved
    ; LispKit: supports keyword arguments as required by the SRFI
    (define (pprint-file ifn . rest)
      (define-values (ofn kv*)
        (if (and (pair? rest) (string? (car rest)))
            (values (car rest) (cdr rest))
            (values #f rest)))
      (define (skip-ws ip)
        (let ((c (peek-char ip)))
          (when
            (and (char? c) (not (char=? c #\newline)) (char-whitespace? c))
            (read-char ip)
            (skip-ws ip))))
      (define (copy-top-line-comments ip op postpp)
        (skip-ws ip)
        (let ((c (peek-char ip)))
          (cond ((and (char? c) (char=? c #\;))
                 (display (read-line ip) op)
                 (newline op)
                 (copy-top-line-comments ip op #f))
                ((and (char? c) (char=? c #\newline))
                 (read-char ip)
                 (unless postpp (newline op))
                 (copy-top-line-comments ip op #f)))))
      (define (pf ip op)
        (let loop ((postpp #f))
          (copy-top-line-comments ip op postpp) ; a hack, really!
          (let ((obj (read ip)))
            (unless (eof-object? obj) (pp* obj op kv*) (loop #t)))))
      (call-with-input-file ifn
        (lambda (ip)
          (if (not ofn)
              (pf ip (current-output-port))
              (call-with-output-file ofn (lambda (op) (pf ip op)))))))


    ; initialize format pattern registry

    (for-each (lambda (x) (pretty-style (car x) (cdr x)))
      '((lambda _ d . body)
        (define _ d . body)
        (define-values _ d . body)
        (define-syntax _ d . body)
        (define-library _ d . body)
        (define-record-type _ d . body)
        (let _ i? dc* . body)
        (letrec _ dc* . body)
        (letrec* _ dc* . body)
        (let* _ dc* . body)
        (let-values _ dc* . body)
        (let*-values _ dc* . body)
        (let-syntax _ dc* . body)
        (letrec-syntax _ dc* . body)
        (parameterize _ dc* . body)
        (with-syntax _ dc* . body)
        (syntax-rules _ i? d . ec*)
        (syntax-case _ e d . dc*)
        (do _ dc* ec . body)
        (begin _ . body)
        (if _ . body)
        (cond _ . ec*)
        (case _ e . dc*)
        (when _ e . body)
        (unless _ e . body)
        (set! _ e . fill)
        (and _ . fill)
        (or _ . fill)
        (import _ . fill)
        (delay _ e)
        (guard _ (e . ec*) . body)
        (case-lambda _ . dc*)
        (cond-expand _ . dc*)))


    ; conditionally add s-c read macros and formatters

    (cond-expand
      (syntax-case
       (set! builtin-read-macros
         (append
           '((syntax "#'" e)
             (quasisyntax "#`" e)
             (unsyntax "#," e)
             (unsyntax-splicing "#,@" e))
           builtin-read-macros))
       (for-each (lambda (x) (pretty-style (car x) (cdr x)))
         '((syntax-case _ e d . ec*)
           (with-syntax _ ec* . body)
           (identifier-syntax _ . ec*))))
      (else))

    ; initialize pp hook registry with LispKit-specific data types;
    ; except for hashtables and records, the representations match the ones
    ; used by `write`

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
        (glist-pp-hook "#g(" gvector->list list->gvector ")")))
    ; hashtables are presented as lists of (key value) entries, records as lists
    ; consisting of the record type name followed by (field value) entries, such that their content is formatted and checked for
    ; shared and circular structures. Views represent hashtables and records which were
    ; rebuilt with datum labels; they preserve the order of the entries
    (define-record-type pp-hashtable-view
      (make-pp-hashtable-view entries)
      pp-hashtable-view?
      (entries pp-hashtable-view-entries))
    (define-record-type pp-record-view
      (make-pp-record-view items)
      pp-record-view?
      (items pp-record-view-items))
    (define (hashtable-entry-list ht)
      (map (lambda (p) (list (car p) (cdr p))) (hashtable->alist ht)))
    (define (record-item-list r)
      (let loop ((names (record-type-field-names (record-type r) #t))
                 (i 0)
                 (acc (list (string->symbol (record-type-name (record-type r))))))
        (if (null? names)
            (reverse acc)
            (loop (cdr names) (+ i 1) (cons (list (car names) (record-ref r i)) acc)))))
    (define (lispkit-hook x)
      (cond ((pp-hashtable-view? x)
             (glist-pp-hook "#<hashtable " pp-hashtable-view-entries make-pp-hashtable-view ">"))
            ((pp-record-view? x)
             (glist-pp-hook "#<record " pp-record-view-items make-pp-record-view ">"))
            ((and (hashtable? x) (> (hashtable-size x) 0))
             (glist-pp-hook "#<hashtable " hashtable-entry-list make-pp-hashtable-view ">"))
            ((and (record? x) (pair? (record-type-field-names (record-type x) #t)))
             (glist-pp-hook "#<record " record-item-list make-pp-record-view ">"))
            (else #f)))
    (pp-hooks (add-pp-hook (pp-hooks) lispkit-hook))))
