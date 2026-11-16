;;; SRFI 272 ADVANCED
;;; Pretty Printing: advanced library
;;;
;;; Library `(srfi 272 advanced)` provides all the functionality of library
;;; `(srfi 272 intermediate)`, adding a large number of parameters for controlling
;;; the layout, abbreviation, and colorization of the output, an explicit style
;;; registry (`pp-styles`) and hook registry (`pp-hooks`) which can be manipulated
;;; functionally, hook constructors for extending the pretty printer to new data
;;; types, and procedure `make-pprint-generator` which returns a generator of the
;;; lines of a pretty-printed representation. `pprint-file` preserves top-level
;;; line comments and whitespace.
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

(define-library (srfi 272 advanced)
  (import (scheme base) (scheme char) (scheme cxr)
    (scheme case-lambda) (scheme inexact) (scheme file)
    (scheme read) (scheme write))

  ; we need to be able to modify global parameters
  (import (srfi 39))

  ; char-width, returning #f 0 1 2
  (import (srfi 272 measure)
          (only (lispkit string) string-display-width))
  ; color support
  (import (srfi 272 colorize))

  ; LispKit libraries
  (import (only (lispkit box) box? box unbox set-box! mpair? mcons mcar mcdr)
          (only (lispkit gvector) gvector? gvector->list list->gvector)
          (only (lispkit hashtable) make-eq-hashtable hashtable-ref hashtable-set!
                hashtable? hashtable-size hashtable->alist)
          (only (lispkit record) record? record-type record-type-name
                record-type-field-names record-ref))


  ; procedures
  (export pp pp* pprint pprint-shared pprint-simple pprint-file
    make-pprint-generator)

  ; configuration
  (export pretty-style lookup-pp-style add-pp-style pretty-hook
    lookup-pp-hook add-pp-hook rmac-pp-hook glst-pp-hook
    bvec-pp-hook atom-pp-hook)

  ; parameters
  (export pp-width pp-circle pp-graph pp-radix pp-length
    pp-level pp-lines pp-indent pp-tab pp-max-tab pp-miser-width
    pp-inline-width pp-brackets pp-code pp-pretty pp-newline
    pp-color pp-emit pp-tint pp-decorate pp-styles pp-hooks
    ; LispKit: these parameters are specified, but not exported by the sample
    pp-level-stub pp-length-stub pp-lines-stub)

  (begin
    (define (cv-width x)
      (if (and (number? x) (exact? x) (> x 0))
          x
          (error "invalid value for pp-width" x)))
    (define pp-width (make-parameter 80 cv-width))

    ; detect and mark cyclic substructure
    (define (cv-boolean x) (not (not x)))
    (define pp-circle (make-parameter #t cv-boolean))

    ; detect and mark shared/cyclic substructure
    (define pp-graph (make-parameter #f cv-boolean))

    ; radix for (at least) exact integers; 2, 8, 10, 16 are supported
    ; if not 10, prefix is printed when needed to keep numbers readable
    ; inexact numbers printed in a system-dependent machine-readable way
    ; #f value allows implementation to use different radices as it sees fit
    (define (cv-radix x)
      (case x
        ((2 8 10 16 #f) x)
        (else (error "invalid value for pp-radix" x))))
    (define pp-radix (make-parameter #f cv-radix))

    ; #f or max numbers of subitems to display in a sequence
    (define (cv-length x)
      (if (or (not x) (and (number? x) (exact? x) (>= x 0)))
          x
          (error "invalid value for pp-length" x)))
    (define pp-length (make-parameter #f cv-length)) ; no limit

    ; a string to display when a sequence is cut short
    (define (cv-length-stub x)
      (if (string? x)
          x
          (error "invalid value for pp-length-stub" x)))
    (define pp-length-stub
      (make-parameter "..." cv-length-stub))

    ; #f or max depth of subitems to display in a sequence
    (define (cv-level x)
      (if (or (not x) (and (number? x) (exact? x) (>= x 0)))
          x
          (error "invalid value for pp-level" x)))
    (define pp-level (make-parameter #f cv-level)) ; no limit

    ; a string to display when a nested element is cut short
    ; #t value means "use Chez-like object shells"
    (define (cv-level-stub x)
      (if (or (string? x) (eq? x #t))
          x
          (error "invalid value for pp-level-stub" x)))
    (define pp-level-stub (make-parameter #t cv-level-stub))

    ; if not #f, cut the printing when N lines are printed
    (define (cv-lines x)
      (if (or (not x) (and (number? x) (exact? x) (>= x 1)))
          x
          (error "invalid value for pp-lines" x)))
    (define pp-lines (make-parameter #f cv-lines)) ; no limit

    ; a string to display when printing is cut short
    ; #t value means "use pp-length-stub with CL-like closers"
    ; LispKit: as specified by the SRFI, #f means "use pp-length-stub"; #t, which
    ; is used by the sample implementation for this purpose, is still supported
    (define (cv-lines-stub x)
      (if (or (string? x) (boolean? x))
          x
          (error "invalid value for pp-lines-stub" x)))
    (define pp-lines-stub (make-parameter ".." cv-lines-stub))

    ; initial indent (first line assumption) -- call it pp-indent?
    (define (cv-indent x)
      (if (and (number? x) (exact? x) (>= x 0))
          x
          (error "invalid value for pp-indent" x)))
    (define pp-indent (make-parameter 0 cv-indent))

    ; standard indentation within special forms in spaces
    ; from the start of the first subform after form's open paren
    ; if 0, all subforms are aligned vertically as in data printing
    (define (cv-tab x)
      (if (and (number? x) (exact? x) (>= x 0))
          x
          (error "invalid value for pp-tab" x)))
    (define pp-tab (make-parameter 1 cv-tab))

    ; custom if-style aligned indentation for short symbols
    ; e.g. 4 allows vertically align args of 'if', 'or', 'and' but not 'cond'
    (define (cv-max-tab x)
      (if (and (number? x) (exact? x) (>= x 0))
          x
          (error "invalid value for pp-max-tab" x)))
    (define pp-max-tab (make-parameter 4 cv-max-tab))

    ; #f or remaining amount of space before width to switch to the
    ; compact ('miser') printing mode with minimal indents
    (define (cv-miser-width x)
      (if (or (not x) (and (number? x) (exact? x) (>= x 0)))
          x
          (error "invalid value for pp-miser-width" x)))
    (define pp-miser-width (make-parameter 20 cv-miser-width))

    ; #f or max. length of an inline expression from its first char to last;
    ; if its length is longer than that, expression will print on multiple lines
    ; cf. Chez: pretty-one-line-limit, 50
    (define (cv-inline-width x)
      (if (or (not x) (and (number? x) (exact? x) (>= x 0)))
          x
          (error "invalid value for pp-inline-width" x)))
    (define pp-inline-width (make-parameter 60 cv-inline-width))

    ; print square brackets around selected subforms
    (define pp-brackets (make-parameter #f cv-boolean))

    ; print argument obj as code, opposed to data
    (define pp-code (make-parameter #t cv-boolean))

    ; if false, prints in a single line with 1 space for separation, observing all flags
    ; other than those related to spacing and line wrapping (pp-width and others)
    (define pp-pretty (make-parameter #t cv-boolean))

    ; if #f, newline is NOT added to the last char of printed representation (cf. write)
    (define pp-newline (make-parameter #t cv-boolean))

    ; #f (no color), #t (default color), or semantic color mapper
    (define (cv-color x)
      (cond ((or (not x) (semantic-color-mapper? x)) x)
            ((eq? x #t) default-semantic-color-mapper)
            (else (error "invalid value for pp-color" x))))
    (define pp-color (make-parameter #f cv-color))

    (define (cv-emit x)
      (if (procedure? x) x (error "invalid value for pp-emit" x)))
    (define pp-emit (make-parameter write-string cv-emit))

    (define (cv-tint x)
      (if (procedure? x) x (error "invalid value for pp-tint" x)))
    (define pp-tint (make-parameter write-string cv-tint))

    (define (cv-decorate x)
      (if (or (boolean? x) (procedure? x))
          x
          (error "invalid value for pp-decorate" x)))
    (define pp-decorate (make-parameter #t cv-decorate))

    ; formatting style registry (opaque object)
    (define pp-styles (make-parameter '()))

    (define (lookup-pp-style styles key)
      (cond ((assq key styles) => cdr) (else #f)))

    ; style object validator
    (define (valid-style? obj)
      (define (valid-fmt-tail? x)
        (or (and (memq x '(body spread fill h* dc* ec* fc* lc*)) #t)
            (and (pair? x) (eq? (car x) 'i?) (valid-fmt-tail? (cdr x)))
            (and (pair? x) (valid-fmt? (car x)) (valid-fmt-tail? (cdr x)))))
      (define (valid-fmt? x)
        (or (and (memq x '(k i h d dc e ec f fc l lc)) #t)
            (valid-fmt-tail? x)))
      (and (pair? obj) (eq? (car obj) '_)
           (valid-fmt-tail? (cdr obj))))

    ; adding a style to the explicit hook registry
    (define (add-pp-style styles key style)
      (if style
          (if (valid-style? style)
              (alist-addv key style styles)
              (error "invalid style object" style))
          (alist-remv key styles)))

    ; public interface to formatting style registry
    (define pretty-style
      (case-lambda
        ((key) (lookup-pp-style (pp-styles) key))
        ; ability to change global parameter is in SRFI 39, we rely on it here
        ((key style)
         (pp-styles (add-pp-style (pp-styles) key style)))))


    ; predicate-based hooks for nonstandard data
    (define pp-hooks (make-parameter '()))

    (define (lookup-pp-hook hooks test)
      (cond ((assv test hooks) => cdr) (else #f)))

    ; adding a hook to the explicit hook registry
    ; LispKit: follows the SRFI specification: hook #f removes the association,
    ; hook #t registers test as a procedure returning a hook object (the sample
    ; implementation uses #f for the latter and has no way to remove hooks)
    (define (add-pp-hook hooks test hook)
      (cond ((not hook) (alist-remv test hooks))
            ((assv test hooks)
             (alist-addv test hook hooks)) ; replaces at its original pos
            (else (cons (cons test hook) hooks)))) ; adds to front

    ; generalized list hook constructor
    (define (glst-pp-hook pfx tolf toxf sfx)
      (list 'gl pfx tolf toxf sfx))

    ; binary vector hook constructor
    (define (bvec-pp-hook pfx lenf reff sfx)
      (list 'bv pfx lenf reff sfx))

    ; atomic hook constructor
    (define (atom-pp-hook sh? widf wrtf)
      (list 'at sh? widf wrtf))

    ; generalized read macro hook constructor
    ; LispKit: the printer expects a quasiquotation depth delta and a flag
    ; indicating a splicing variant; user-defined read macros behave like quote
    (define (rmac-pp-hook pfx reff tomf)
      (list 'rm pfx reff tomf 0 #f))

    ; convenient lookup/installation of hooks
    (define pretty-hook
      (case-lambda
        ((test) (lookup-pp-hook (pp-hooks) test))
        ; ability to change global parameter is in SRFI 39, we rely on it here
        ((test hook) (pp-hooks (add-pp-hook (pp-hooks) test hook)))))


    ; portable eq table, similar to R6RS(?) eq-hashtable
    ; NB: better use native eq hash table if available!

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

    (define (alist-remv key alist)
      (let loop ((l alist))
        (cond ((null? l) alist)
              ((eqv? key (caar l)) (cdr l))
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
    ; TODO: use write-simple for chars? Old chibi fails to write
    ; control chars using their names
    (define (written-width x)
      (let* ((p (open-output-string)) ; slow but exact
             (s (begin (write x p) (get-output-string p))))
        (close-output-port p)
        (string-width s)))

    ; check if printing x without spacing would interfere with read macro
    (define (interferes-with-splicing-macro? x)
      (and (symbol? x)
           (let ((s (symbol->string x)))
             (and (> (string-length s) 0) (char=? (string-ref s 0) #\@)))))

    ; this list should contain rmacs supported by the reader by default
    ; NB: last boolean param is hsv, "has splicing variant"
    (define builtin-read-macros
      '((quote "'" 0 #f)
        (quasiquote "`" 1 #f)
        (unquote "," -1 #t)
        (unquote-splicing ",@" -1 #f)))

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
      (define *radix* (or (kval kv* pp-radix cv-radix) 10))
      (define *length* (kval kv* pp-length cv-length))
      (define *level* (kval kv* pp-level cv-level))
      (define *lines* (kval kv* pp-lines cv-lines))
      (define *indent* (kval kv* pp-indent cv-indent))
      (define *tab* (kval kv* pp-tab cv-tab))
      (define *max-tab* (kval kv* pp-max-tab cv-max-tab))
      (define *miser-width*
        (kval kv* pp-miser-width cv-miser-width))
      (define *inline-width*
        (kval kv* pp-inline-width cv-inline-width))
      (define *brackets* (kval kv* pp-brackets cv-boolean))
      (define *code* (kval kv* pp-code cv-boolean))
      (define *pretty* (kval kv* pp-pretty cv-boolean))
      (define *newline*
        (and *pretty* (kval kv* pp-newline cv-boolean)))
      (define *color* (kval kv* pp-color cv-color))
      (define *length-stub*
        (kval kv* pp-length-stub cv-length-stub))
      (define *level-stub* (kval kv* pp-level-stub cv-level-stub))
      (define *lines-stub* (kval kv* pp-lines-stub cv-lines-stub))
      (define *emit* (kval kv* pp-emit cv-emit))
      (define *tint* (kval kv* pp-tint cv-tint))
      (define *decorate* (kval kv* pp-decorate cv-decorate))
      (define *styles* (kval kv* pp-styles)) ; todo: conv- check!
      (define *hooks* (kval kv* pp-hooks)) ; todo: conv- check!

      ; number of newlines produced so far (used by *lines* limit)
      (define newline-count 0)
      ; #f or exit handler (used by *lines* limit)
      (define exit-handler #f)
      ; #f or currently applied semantic color
      (define current-sc #f)

      ; params to be zeroed if we are within *miser-width* from *width*
      (define (std-indent-miser ind)
        (if (and ind (>= (+ ind *miser-width*) *width*)) 0 *tab*))
      (define std-indent
        (if *miser-width* std-indent-miser (lambda (ind) *tab*)))
      (define (alt-indent-miser ind)
        (if (and ind (>= (+ ind *miser-width*) *width*))
            0
            *max-tab*))
      (define alt-indent
        (if *miser-width* alt-indent-miser (lambda (ind) *max-tab*)))

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
      (define incp
        (lambda (v)
          (cons 0 (cons (+ (cadr v) 1) (cons ")" (cddr v))))))
      (define incb
        (lambda (v)
          (cons 0 (cons (+ (cadr v) 1) (cons "]" (cddr v))))))
      (define nest (if (or *level* *lines*) incp non))
      (define nestb (if (or *level* *lines*) incb non))
      ; improper tail is not a step ahead of the previous element!
      (define back (lambda (v) (cons (- (car v) 1) (cdr v))))
      (define nocut (lambda (v) #f))
      (define cuti?
        (if *length* (lambda (v) (>= (car v) *length*)) nocut))
      (define cutd?
        (if *level* (lambda (v) (>= (cadr v) *level*)) nocut))
      (define env (list 0 0)) ; ignored if no cutoffs are set
      (define cuti-wid (string-width *length-stub*))
      (define cutd-widf
        (if (string? *level-stub*)
            (let ((w (string-width *level-stub*)))
              (lambda (x)
                (dispatch-on-type x (lambda (pfx reff tomf dde hsv) w)
                  (lambda (pfx tolf toxf sfx) w)
                  (lambda (pfx lenf reff sfx) w)
                  (lambda (sh? widf wrtf) (widf x))))) ; atom
            (lambda (x)
              (dispatch-on-type x
                (lambda (pfx reff tomf dde hsv)
                  (+ (string-width pfx) cuti-wid))
                (lambda (pfx tolf toxf sfx)
                  (+ (string-width pfx) cuti-wid (string-width sfx)))
                (lambda (pfx lenf reff sfx)
                  (+ (string-width pfx) cuti-wid (string-width sfx)))
                (lambda (sh? widf wrtf) (widf x)))))) ; atom

      ; decorations expects an S-expression and returns either #f or
      ; a pair (tail-comment head-comment ...) where tail comment can be #f
      ; if there is at least one head-comment. Comments are strings that
      ; start with a semicolon and contain no newline chars
      (define decorations
        (if (procedure? *decorate*) *decorate* (lambda (x) #f)))
      ; spacer tracks spacing between body elements in presence of
      ; decorations; it may have the following values:
      ; (#f) -- emit regular single newline
      ; (#f "") -- emit two newlines ('spread?' mode)
      ; (tail-comment head-comment ...) -- emit optional tail comment,
      ; then newline and head-comments, each ending in a newline
      ; This procedure returns spacer to use by (space) while
      ; modifying the first pair of spacer to prepare for the next turn
      ; LispKit: pairs are immutable, so the first element of spacer is a box
      (define (update-spacer! spacer d)
        (define t (unbox (car spacer))) ; trailer from the previous call
        (cond ((pair? d) (set-box! (car spacer) (car d)) (cons t (cdr d)))
              (else (set-box! (car spacer) #f) (cons t (cdr spacer)))))

      ; shortcut output routines used below
      ; note that all output logically goes through emit or tint
      (define (emit s) (*emit* s *port*)) ; ~ (write-string s *port*))
      (define (tint s) (*tint* s *port*)) ; ~ (write-string s *port*))
      (define emit-written
        (if (eq? *emit* write-string) ; default not changed?
            (lambda (x) (write x *port*)) ; no funny stuff
            (lambda (x)
              (let* ((p (open-output-string))
                     (s (begin (write x p) (get-output-string p))))
                (*emit* s *port*)))))
      (define (emit-atom x)
        (if (and (number? x) (exact? x) (not (= *radix* 10)))
            (let ((s (number->string x *radix*)))
              (emit "#")
              (emit (case *radix* ((2) "b") ((8) "o") ((16) "x")))
              (emit s))
            (emit-written x)))
      (define (emit-newline v)
        (set! newline-count (+ newline-count 1))
        (cond ((and exit-handler (eqv? newline-count *lines*))
               (emit " ")
               (emit (if (string? *lines-stub*) *lines-stub* *length-stub*))
               (do ((t (cddr v) (cdr t))) ((null? t)) (emit (car t)))
               ; LispKit: the terminating newline is emitted by pp (if enabled)
               (exit-handler #f))
              (else (emit "\n"))))
      ; inserts single space if ind is #f; otherwise prints newline and indents to ind
      (define (space ind v . osp)
        (define (indent)
          (do ((i 0 (+ i 1))) ((>= i ind)) (emit " ")))
        ; LispKit: empty lines are not indented
        (define (emitc c)
          (unless (string=? c "")
            (indent)
            (emit-px (list 'comment c)))
          (emit-newline v))
        (define sp (if (pair? osp) (car osp) #f))
        (define csc current-sc)
        (when csc (emit/sc-end csc))
        (cond ((not ind) (emit " "))
              (else
               (when (and (pair? sp) (string? (car sp)))
                 (emit-px (list " " 'comment (car sp))))
               (emit-newline v)
               (when (pair? sp) (for-each emitc (cdr sp)))
               (indent)))
        (when csc (emit/sc-start csc)))
      (define (emit-lpar) (emit "("))
      (define (emit-rpar) (emit ")"))
      (define (emit-lbra) (emit (if *brackets* "[" "(")))
      (define (emit-rbra) (emit (if *brackets* "]" ")")))
      (define (emit-cuti) (emit *length-stub*))
      (define print-cutd
        (if (string? *level-stub*) ; CL-like model
            (lambda (x bk?)
              (dispatch-on-type x
                (lambda (pfx reff tomf dde hsv) (emit *level-stub*))
                (lambda (pfx tolf toxf sfx) (emit *level-stub*))
                (lambda (pfx lenf reff sfx) (emit *level-stub*))
                (lambda (sh? widf wrtf) (emit/wrtf wrtf x)))) ; atoms
            ; Chez-like model: print "shell" of the object
            (lambda (x bk?)
              (cond ((and bk? (pair? x)) (emit-lbra) (emit-cuti) (emit-rbra))
                    (else
                     (dispatch-on-type x
                       (lambda (pfx reff tomf dde hsv) (emit pfx) (emit-cuti))
                       (lambda (pfx tolf toxf sfx)
                         (emit pfx)
                         (emit-cuti)
                         (emit sfx))
                       (lambda (pfx lenf reff sfx)
                         (emit pfx)
                         (emit-cuti)
                         (emit sfx))
                       (lambda (sh? widf wrtf) (emit/wrtf wrtf x))))))))
      ; colorized versions of emit primitives
      (define (emit/sc-start-nop sc) 42)
      (define (emit/sc-end-nop sc) 42)
      (define (emit/sc-start-real sc)
        (set! current-sc sc)
        (tint (semantic-color->start-string sc *color*)))
      (define (emit/sc-end-real sc)
        (set! current-sc #f)
        (tint (semantic-color->end-string sc *color*)))
      (define emit/sc-start
        (if *color* emit/sc-start-real emit/sc-start-nop))
      (define emit/sc-end
        (if *color* emit/sc-end-real emit/sc-end-nop))
      (define (emit-atom/sc-stub x sc) (emit-atom x))
      (define (emit-atom/sc-real x sc)
        (emit/sc-start-real sc)
        (emit-atom x)
        (emit/sc-end-real sc))
      (define emit-atom/sc
        (if *color* emit-atom/sc-real emit-atom/sc-stub))
      (define (emit-px px)
        (let loop ((px px))
          (cond ((and (pair? px) (string? (car px)))
                 (emit (car px))
                 (loop (cdr px)))
                ((and (pair? px) (symbol? (car px)) (pair? (cdr px))
                      (string? (cadr px)))
                 (emit/sc-start (car px))
                 (emit (cadr px))
                 (emit/sc-end (car px))
                 (loop (cddr px))))))
      (define (emit/wrtf wrtf x)
        (define px (wrtf x *radix*))
        (if (pair? px) (emit-px px) (emit-atom x)))

      ; locating hooks and calling handlers
      ; 'normalizes' input objects to simplify and sync all phases
      (define (dispatch-on-type x retm retl retv reta)
        (let loop ((al *hooks*))
          (cond ((null? al) ; dispatch on builtins
                 (cond ((and (pair? x) (symbol? (car x)) (pair? (cdr x)) (null? (cddr x))
                             (assq (car x) builtin-read-macros))
                        =>
                        (lambda (l)
                          (retm (cadr l) cadr (lambda (x) (list (car l) x)) (caddr l)
                                (cadddr l))))
                       ((pair? x) (retl "(" (lambda (x) x) (lambda (x) x) ")"))
                       ((vector? x) (retl "#(" vector->list list->vector ")"))
                       ((string? x) ; shareable atomic
                        (reta #t quoted-string-width (lambda (x radix) x)))
                       ((bytevector? x) ; NB: contents non-markable!
                        (retv "#u8(" bytevector-length bytevector-u8-ref ")"))
                       (else (reta #f atom-width (lambda (x radix) x)))))
                (((caar al) x) =>
                 (lambda (res)
                   (let ((hk (if (eq? (cdar al) #t) res (cdar al))))
                     (unless (pair? hk) (error "invalid hook!"))
                     (case (car hk)
                       ((rm) (apply retm (cdr hk)))
                       ; list-like: glst-pp-hook pfx tolf toxf sfx
                       ((gl) (apply retl (cdr hk)))
                       ; binary vector: bvec-pp-hook pfx lenf reff sfx
                       ((bv) (apply retv (cdr hk)))
                       ; atomic: atom-pp-hook sh? widf wrtf
                       ((at) (apply reta (cdr hk)))
                       ; todo: pre-check, this shouldn't happen!
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
                (lambda (pfx reff tomf dde hsv)
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
                             (lambda (pfx reff tomf dde hsv)
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
                    (lambda (pfx reff tomf dde hsv)
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
                 (lambda (pfx reff tomf dde hsv)
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

      (define (print-mark x ind v print)
        (let-values (((first? id x) (shared-unmark x)))
          (emit "#")
          (emit (number->string id))
          (emit (if first? "=" "#"))
          (when first?
            (let ((ilen (atom-width id)))
              (print x (ind+ ind (+ ilen 2)) v)))))

      (define (print-read-macro x ind v pfx hsv elt print)
        (define sep
          (and hsv (interferes-with-splicing-macro? elt) " "))
        (emit pfx)
        (when sep (emit sep))
        (let ((ind (ind+ ind (+ (string-width pfx) (if sep 1 0)))))
          (print elt ind v))) ; Caveat: no nesting!

      ; fill-style printing of (possibly improper) list contents
      ; if lst is atom, prints dot before it; v is env for lst head
      ; NB: adjacent closers may cause last line overflow!
      (define (print*/fill lst ind v restoff print . prints)
        (define (fitsi? e c v)
          (if (cuti? v) (csub c cuti-wid) (fits? e c v)))
        (define (cari l v) (if (cuti? v) 42 (car l)))
        (define prt* (cons print prints))
        (define (cdrp prt*) (if (null? (cdr prt*)) prt* (cdr prt*)))
        (define (prest x ind c v prt)
          (if (or (not ind) (and (csub c 3) (fitsi? x c v)))
              (begin (emit " . ") (prt x #f v))
              (begin
                (space ind v)
                (emit ".")
                (let ((ind1 (fit-ind x (ind+ ind 2) v)))
                  (space (and ind1 ind) v)
                  (prt x (and ind1 ind) v)))))
        (define (ploop l e v ind ind1 indi c prt*)
          (if (cuti? v) (emit-cuti) ((car prt*) e indi v))
          (cond ((cuti? v))
                ((null? (cdr l)))
                ((not (pair? (cdr l)))
                 (prest (cdr l) ind c (back v) (car prt*)))
                (else
                 (let* ((l (cdr l)) (v (step v)) (e (cari l v)))
                   (cond ((and (csub c 1) (fitsi? e c v))
                          (space #f v)
                          (ploop l e v ind ind1 #f c (cdrp prt*)))
                         (else
                          (let ((c (cmake ind1)))
                            (space ind1 v)
                            (fitsi? e c v)
                            (ploop l e v ind ind1 ind1 c (cdrp prt*)))))))))
        (cond ((null? lst))
              ((not (pair? lst))
               (prest lst ind (cmake ind) (back v) print))
              (else
               (let ((e (cari lst v)) (c (cmake ind)))
                 (fitsi? e c v) ; need to bump c!
                 (ploop lst e v ind (ind+ ind restoff) ind c prt*)))))

      ; regular one-per-line-on-overflow printing of list contents
      ; if lst is atom, prints dot before it; v is env for lst head;
      ; prin1 is used to print first subexpression, print for the rest;
      ; optional spread? flag forces insertion of extra newline between forms;
      ; if decorations for element forms exist, they are used is stack
      ; printing is in effect
      (define (print*/body lst ind v prin1 print . osp)
        (define spacer
          (if (and (pair? osp) (car osp)) (list (box #f) "") (list (box #f))))
        (let loop ((first? #t) (lst lst) (v v) (prt prin1))
          (cond ((null? lst))
                ((not (pair? lst)) ; TODO: better miser wrap?
                 (emit " . ")
                 (print-datum lst (ind+ ind 3) (back v)))
                ((cuti? v) (unless first? (space ind v)) (emit-cuti))
                (else
                 (let* ((x (car lst))
                        (d (decorations x))
                        (sp (update-spacer! spacer d)))
                   (unless first? (space ind v sp))
                   (prt (car lst) ind v)
                   (loop #f (cdr lst) (step v) print))))))

      (define (print-list-like x ind v pfx lst sfx print)
        (let ((ind (fit-ind x ind v)))
          (emit pfx)
          (let ((ind (ind+ ind (string-width pfx))) (v (nest v)))
            (if (vector? x) ; hack: fill-print vectors
                (print*/fill lst ind v 0 print)
                (print*/body lst ind v print print)))
          (emit sfx)))

      ; fill-style binary vector printing: saves verical space
      ; NB: adjacent closers may cause last-line overflow!
      (define (print-vector-like x ind v pfx lenf reff sfx print)
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
                (if (cuti? v) (emit-cuti) (print e indi v))
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
                 (lambda (pfx reff tomf dde hsv)
                   (print-read-macro x ind v pfx hsv (reff x) print-datum))
                 (lambda (pfx tolf toxf sfx)
                   (print-list-like x ind v pfx (tolf x) sfx print-datum))
                 (lambda (pfx lenf reff sfx)
                   (print-vector-like x ind v pfx lenf reff sfx print-datum))
                 (lambda (sh? widf wrtf) (emit/wrtf wrtf x))))))

      (define (atom-semantic-color x qsym?)
        (cond ((not *color*) #f)
              ((symbol? x) (and qsym? 'literal))
              ((number? x) 'number)
              ((string? x) 'string)
              ((char? x) 'char)
              (else 'literal)))

      (define (print-literal x ind v)
        (cond ((shared-mark? x) (print-mark x ind v print-literal))
              ((cutd? v) (print-cutd x #f)) ; no brackets w/o fmt!
              (else
               (dispatch-on-type x
                 (lambda (pfx reff tomf dde hsv)
                   (print-read-macro x ind v pfx hsv (reff x) print-literal))
                 (lambda (pfx tolf toxf sfx)
                   (print-list-like x ind v pfx (tolf x) sfx print-literal))
                 (lambda (pfx lenf reff sfx)
                   (print-vector-like x ind v pfx lenf reff sfx print-datum))
                 (lambda (sh? widf wrtf)
                   (define sc (atom-semantic-color x #t))
                   (cond (sc (emit/sc-start sc) (emit/wrtf wrtf x) (emit/sc-end sc))
                         (else (emit/wrtf wrtf x))))))))

      (define (print-template x ind v de)
        (define (subprt de)
          (if (<= de 0)
              (lambda (x ind v)
                (define csc current-sc)
                (when csc (emit/sc-end csc))
                (print-exp x ind v)
                (when csc (emit/sc-start csc)))
              (lambda (x ind v) (print-template x ind v de))))
        (cond ((shared-mark? x) (print-mark x ind v (subprt de)))
              ((cutd? v) (print-cutd x #f)) ; no brackets w/o fmt!
              (else
               (dispatch-on-type x
                 (lambda (pfx reff tomf dde hsv)
                   (print-read-macro x ind v pfx hsv (reff x)
                     (subprt (+ de dde))))
                 (lambda (pfx tolf toxf sfx)
                   (print-list-like x ind v pfx (tolf x) sfx (subprt de)))
                 (lambda (pfx lenf reff sfx)
                   (print-vector-like x ind v pfx lenf reff sfx (subprt de)))
                 (lambda (sh? widf wrtf) (emit/wrtf wrtf x))))))

      (define (print-keyword x ind v)
        (cond ((symbol? x) (emit-atom/sc x 'keyword))
              (else (print-datum x ind v))))

      (define (print-formals x ind v)
        (cond ((symbol? x) (emit-atom/sc x 'formal))
              ((and (pair? x) (symbol? (car x)))
               (let ((ind (fit-ind x ind v)))
                 (emit-lpar)
                 (print*/fill x ind (nest v) 0 print-formals)
                 (emit-rpar)))
              (else (print-datum x ind v))))

      (define (print-def-head x ind v)
        (cond ((symbol? x) (emit-atom/sc x 'defined))
              ((and (pair? x) (symbol? (car x)))
               (let ((ind (fit-ind x ind v)))
                 (emit-lpar)
                 (print*/fill x ind (nest v) 0 print-def-head print-formals)
                 (emit-rpar)))
              (else (print-datum x ind v))))

      (define (print-def-heads x ind v)
        (cond ((symbol? x) (emit-atom/sc x 'defined))
              ((and (pair? x) (symbol? (car x)))
               (let ((ind (fit-ind x ind v)))
                 (emit-lpar)
                 (print*/fill x ind (nest v) 0 print-def-head)
                 (emit-rpar)))
              (else (print-datum x ind v))))

      ; default list exp printer; precondition: x is a pair
      (define (print-app x ind v kw?)
        (let ((ind (fit-ind x ind v)))
          (emit-lpar)
          (let ((ind (ind+ ind 1)) (v (nest v)))
            (if (and (symbol? (car x)) (pair? (cdr x)) (not (cuti? v)))
                (let ((oplen (atom-width (car x))))
                  (if (< oplen (alt-indent ind)) ; ind = len + 1 space
                      (begin
                        (if kw?
                            (emit-atom/sc (car x) 'keyword)
                            (emit-atom (car x)))
                        (emit " ")
                        (print*/fill (cdr x) (ind+ ind (+ 1 oplen)) (step v) 0
                          print-exp))
                      (print*/fill x ind v (std-indent ind)
                        (if kw? print-keyword print-exp) print-exp)))
                (print*/fill x ind v (std-indent ind) print-exp)))
          (emit-rpar)))

      ; clause printer, prin1 is used for the head
      (define (print-clause x ind v prin1)
        (let ((ind (fit-ind x ind v)))
          (cond ((shared-mark? x) (print-mark x ind v print-exp))
                ((cutd? v) (print-cutd x #t)) ; brackets!
                ((and (list? x) (= (length x) 3) (eq? (car x) 'else)
                      (eq? (cadr x) '=>)) ; (else => *)
                 (emit-lbra)
                 (print*/fill x (ind+ ind 1) (nestb v) 0 print-keyword
                   print-keyword print-exp)
                 (emit-rbra))
                ((and (list? x) (= (length x) 3) (eq? (cadr x) '=>)) ; (* => *)
                 (emit-lbra)
                 (print*/fill x (ind+ ind 1) (nestb v) 0 prin1 print-keyword
                   print-exp)
                 (emit-rbra))
                ((and (pair? x) (eq? (car x) 'else))
                 (emit-lbra)
                 (print*/body x (ind+ ind 1) (nestb v) print-keyword print-exp)
                 (emit-rbra))
                ((pair? x)
                 (emit-lbra)
                 (print*/body x (ind+ ind 1) (nestb v) prin1 print-exp)
                 (emit-rbra))
                (else (print-datum x ind v)))))

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
                 (lambda (pfx reff tomf dde hsv)
                   (cond ((<= dde 0)
                          (let ((sc (if (= dde 0) 'literal 'warning)))
                            (emit/sc-start sc)
                            (print-read-macro x ind v pfx hsv (reff x) print-datum)
                            (emit/sc-end sc)))
                         (else
                          (emit/sc-start 'literal)
                          (print-read-macro x ind v pfx hsv (reff x)
                            (lambda (x ind v) (print-template x ind v dde)))
                          (emit/sc-end 'literal))))
                 (lambda (pfx tolf toxf sfx)
                   (let ((l (tolf x)))
                     (if (and (eq? l x) (pair? l)) ; regular pair
                         (print-list-exp l ind v)
                         (print-list-like x ind v pfx l sfx print-datum))))
                 (lambda (pfx lenf reff sfx)
                   (print-vector-like x ind v pfx lenf reff sfx print-datum))
                 (lambda (sh? widf wrtf)
                   (define sc (atom-semantic-color x #f))
                   (cond (sc (emit/sc-start sc) (emit/wrtf wrtf x) (emit/sc-end sc))
                         (else (emit/wrtf wrtf x))))))))

      (define (print-list-exp x ind v)
        (cond ((not (symbol? (car x))) (print-app x ind v #f))
              ((lookup-pp-style *styles* (car x)) =>
               (lambda (fmt)
                 (if (eq? (cdr fmt) 'fill)
                     (print-app x ind v #t) ; special case
                     (print/fmt (cons 'k (cdr fmt)) x ind v))))
              (else (print-app x ind v #f))))

      (define (print/fmt fmt x ind v)
        (let ((ind (fit-ind x ind v)))
          (case fmt
            ((k) (print-keyword x ind v))
            ((h) (print-def-head x ind v))
            ((h*) (print-def-heads x ind v))
            ((d) (print-datum x ind v))
            ((dc) (print-clause x ind v print-datum))
            ((dc*) (print-clauses x ind v print-datum))
            ((e) (print-exp x ind v))
            ((ec) (print-clause x ind v print-exp))
            ((ec*) (print-clauses x ind v print-exp))
            ((f i) (print-formals x ind v))
            ((fc) (print-clause x ind v print-formals))
            ((fc*) (print-clauses x ind v print-formals))
            ((l) (print-literal x ind v))
            ((lc) (print-clause x ind v print-literal))
            ((lc*) (print-clauses x ind v print-literal))
            (else
             (emit-lpar)
             (print/fmt* x (ind+ ind 1) (nest v) fmt)
             (emit-rpar)))))

      ; print via atomic tail format
      ; if lst is atom, prints dot before it; v is env for lst head
      (define (print/fmt-tail lst ind v fmt)
        (define print
          (case fmt
            ((dc*)
             (lambda (x ind v) (print-clause x ind v print-datum)))
            ((ec*) (lambda (x ind v) (print-clause x ind v print-exp)))
            ((fc*)
             (lambda (x ind v) (print-clause x ind v print-formals)))
            ((lc*)
             (lambda (x ind v) (print-clause x ind v print-literal)))
            (else print-exp)))
        (print*/body lst ind v print print (eq? fmt 'spread)))

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
                ((memq (fmcdr fmt*) '(dc* ec* fc* lc* body spread)) ; stacked tail
                 (let ((ind2 (pind l e v ind ind1)))
                   (print/fmt-tail (cdr l) ind2 (step v) (fmcdr fmt*))))
                (else
                 (let* ((ll l) (le e) (l (cdr l)) (v (step v)) (e (cari l v)))
                   (cond ((and ind (eq? ll lst) (symbol? le) (equal? '(k fc* . body) fmt*)
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
             (x (if (> pg 0) (mark-shared sexp env (= pg 1)) sexp))
             (ind (if *pretty* *indent* #f))
             (print (if *code* print-exp print-datum)))
        (if *lines*
            (call/cc (lambda (k) (set! exit-handler k) (print x ind env)))
            (print x ind env))
        ; LispKit: the terminating newline never triggers the lines limit; the
        ; sample implementation emits a lines stub followed by two newlines if the
        ; output has exactly *lines* lines
        (set! exit-handler #f)
        (when *newline* (emit-newline env))))

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

    ; map for Emacs-like file variables, mapped to parameters
    ; see https://www.gnu.org/software/emacs/manual/html_node/emacs/Specifying-File-Variables.html
    (define file-key-map
      `((pp-width: unquote pp-width)
        (fill-column: unquote pp-width) ; Emacs standard
        (pp-inline-width: unquote pp-inline-width)
        (pp-miser-width: unquote pp-miser-width)
        (pp-tab: unquote pp-tab)
        (pp-max-tab: unquote pp-max-tab)
        (pp-brackets: unquote pp-brackets)))

    ; reads input file, pretty-prints it to output file or current output
    ; top-level line comments are preserved, -*- line is recognized in the header
    (define (pprint-file ifn . rest)
      (define-values (ofn kv*)
        (if (and (pair? rest) (string? (car rest)))
            (values (car rest) (cdr rest))
            (values #f rest)))
      (define (getpar pp-xxx)
        (cond ((memq pp-xxx kv*) =>
               (lambda (p) (and (pair? (cdr p)) (cadr p))))
              (else (pp-xxx))))
      (define color (getpar pp-color))
      (define cm
        (if (semantic-color-mapper? color)
            color
            default-semantic-color-mapper))
      (define decorate? (getpar pp-decorate))
      (define emit write-string) ; ignore overrides
      (define tint write-string) ; ignore overrides
      (define (copy-comment line op)
        (when color
          (tint (semantic-color->start-string 'comment cm) op))
        (emit line op)
        (when color
          (tint (semantic-color->end-string 'comment cm) op))
        (emit "\n" op))
      (define (parse-magic-line line)
        (define (skip-while p pred)
          (let ((c (peek-char p)))
            (when (and (not (eof-object? c)) (pred c))
              (read-char p)
              (skip-while p pred))))
        (define (read-or-eof p)
          (define x (guard (ex (else (eof-object))) (read p)))
          (case x ((t) #t) ((nil) #f) (else x)))
        (define (parse-kv p)
          (let* ((key (read-or-eof p)) (m (assq key file-key-map)))
            (and m
                 (let ((val (read-or-eof p)))
                   (and (not (eof-object? val)) (list (cdr m) val))))))
        (define (parse-kvs p)
          (let loop ((kvs '()))
            (skip-while p char-whitespace?)
            (if (eqv? (peek-char p) #\-)
                (reverse kvs)
                (let ((kv (parse-kv p)))
                  (skip-while p char-whitespace?)
                  (when (eqv? (peek-char p) #\;) (read-char p))
                  (loop (if kv (cons kv kvs) kvs))))))
        (call-with-port (open-input-string line)
          (lambda (p)
            (skip-while p (lambda (ch) (char=? ch #\;)))
            (and (eq? (read-or-eof p) '-*-)
                 (let ((kvs (parse-kvs p)))
                   (and (eq? (read-or-eof p) '-*-) (apply append kvs)))))))
      (define (copy-whitespace ip op)
        (let loop ((c (peek-char ip)))
          (when (and (char? c) (char-whitespace? c))
            (write-char (read-char ip) op)
            (loop (peek-char ip)))))
      (define (copy-top-line-comments ip op in-header?)
        (copy-whitespace ip op)
        (when (eqv? (peek-char ip) #\;)
          (let ((line (read-line ip)))
            (cond ((and in-header? (parse-magic-line line)) =>
                   (lambda (kvs)
                     (set! kv* (append kv* kvs))
                     (set! in-header? #f))))
            (copy-comment line op)
            (copy-top-line-comments ip op in-header?))))
      (define (pf ip op)
        (let loop ((in-header? #t))
          (when decorate? (copy-top-line-comments ip op in-header?))
          (let ((obj (read ip)))
            (unless (eof-object? obj)
              (pp* obj op pp-code #t pp-newline #f kv*)
              (unless decorate? (newline op) (newline op))
              (loop #f)))))
      (call-with-input-file ifn
        (lambda (ip)
          (if (not ofn)
              (pf ip (current-output-port))
              (call-with-output-file ofn (lambda (op) (pf ip op)))))))

    (define (make-ppg sexp . kv*)
      (let ((return #f) (resume #f) (done #f))
        (define (emit s port)
          (call/cc (lambda (k) (set! resume k) (return s))))
        (define (tint s port)
          (call/cc (lambda (k) (set! resume k) (return (list s)))))
        ; LispKit: the continuation of the caller is captured on every call;
        ; the sample implementation only captures it on the first call, such
        ; that subsequent lines get returned to the context of the first call
        (lambda ()
          (if done
              (eof-object)
              (call/cc
                (lambda (k)
                  (set! return k)
                  (if resume
                      (resume (if #f #f))
                      (begin
                        (pp* sexp (current-output-port) pp-emit emit pp-tint tint kv*)
                        (set! done #t)
                        (return (eof-object))))))))))

    (define (make-pprint-generator sexp . kv*)
      (define tint? #t) ; do not filter tint channel
      (let ((gen (apply make-ppg sexp kv*)) (l '()))
        (define (line)
          (let ((s (apply string-append (reverse l)))) (set! l '()) s))
        (lambda ()
          (let loop ((s (gen)))
            (cond ((eof-object? s) (if (null? l) (eof-object) (line)))
                  ((pair? s) ; ("str") passed through the tint channel
                   (when tint? (set! l (cons (car s) l)))
                   (loop (gen)))
                  ((string=? s "\n") (set! l (cons s l)) (line))
                  (else (set! l (cons s l)) (loop (gen))))))))


    ; initialize format pattern registry

    (for-each
      (lambda (x) (pretty-style (car x) (cons '_ (cdr x))))
      '((lambda f . body)
        (define h . body)
        (define-values h* . body)
        (define-syntax h . body)
        (define-library h* . spread)
        (define-record-type h . body)
        (let i? fc* . body)
        (letrec fc* . body)
        (letrec* fc* . body)
        (let* fc* . body)
        (let-values fc* . body)
        (let*-values fc* . body)
        (let-syntax fc* . body)
        (letrec-syntax fc* . body)
        (parameterize fc* . body)
        (with-syntax fc* . body)
        (syntax-rules i? d . ec*)
        (do fc* ec . body)
        (begin . body)
        (if . body)
        (cond . ec*)
        (case e . lc*)
        (when e . body)
        (unless e . body)
        (set! e . fill)
        (and . fill)
        (or . fill)
        (import . fill)
        (delay . fill)
        (guard (f . ec*) . body)
        (case-lambda . fc*)
        (cond-expand . dc*)))


    ; conditionally add s-c read macros and formatters

    (cond-expand
      (syntax-case
       (set! builtin-read-macros
         (append
           '((syntax "#'" 0 #f)
             (quasisyntax "#`" 1 #f)
             (unsyntax "#," -1 #t)
             (unsyntax-splicing "#,@" -1 #f))
           builtin-read-macros))
       (for-each
         (lambda (x) (pretty-style (car x) (cons '_ (cdr x))))
         '((syntax-case e d . ec*)
           (with-syntax ec* . body)
           (identifier-syntax . ec*))))
      (else))

    ; initialize pp hook registry with LispKit-specific data types;
    ; except for hashtables and records, the representations match the ones
    ; used by `write`

    (pp-hooks
      (add-pp-hook (pp-hooks) box?
        (glst-pp-hook "#<box " (lambda (x) (list (unbox x)))
          (lambda (x) (box (car x))) ">")))
    (pp-hooks
      (add-pp-hook (pp-hooks) mpair?
        (glst-pp-hook "#<pair " (lambda (x) (list (mcar x) (mcdr x)))
          (lambda (x) (mcons (car x) (cadr x))) ">")))
    (pp-hooks
      (add-pp-hook (pp-hooks) gvector?
        (glst-pp-hook "#g(" gvector->list list->gvector ")")))
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
             (glst-pp-hook "#<hashtable " pp-hashtable-view-entries make-pp-hashtable-view ">"))
            ((pp-record-view? x)
             (glst-pp-hook "#<record " pp-record-view-items make-pp-record-view ">"))
            ((and (hashtable? x) (> (hashtable-size x) 0))
             (glst-pp-hook "#<hashtable " hashtable-entry-list make-pp-hashtable-view ">"))
            ((and (record? x) (pair? (record-type-field-names (record-type x) #t)))
             (glst-pp-hook "#<record " record-item-list make-pp-record-view ">"))
            (else #f)))
    (pp-hooks (add-pp-hook (pp-hooks) lispkit-hook #t))))
