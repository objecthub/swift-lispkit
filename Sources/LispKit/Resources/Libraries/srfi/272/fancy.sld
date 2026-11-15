;;; SRFI 272 FANCY
;;; Pretty Printing: fancy library
;;;
;;; Library `(srfi 272 fancy)` provides all the functionality of library
;;; `(srfi 272 advanced)`. Procedure `pprint-file` is enhanced such that it
;;; preserves comments within expressions, recognizes Emacs-style "magic comments"
;;; for file-local configurations and "style comments" for defining formatting
;;; styles on a per-file basis. Procedure `pprint-file/html` generates HTML output.
;;; All exported identifiers except for `pprint-file` refer to the same bindings
;;; as the ones exported by `(srfi 272 advanced)`.
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

(define-library (srfi 272 fancy)
  (import (scheme base) (scheme char) (scheme cxr)
    (scheme case-lambda) (scheme inexact) (scheme file)
    (scheme read) (scheme write))

  ; main machinery
  (import
    (rename (srfi 272 advanced)
      (pprint-file advanced-pprint-file)))
  ; color support
  (import (srfi 272 colorize))


  ; procedures
  (export pp pp* pprint pprint-shared pprint-simple pprint-file
    make-pprint-generator pprint-file/html)

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
    (define *fold-case* #f)
    (define lpar #\()
    (define lbrk #\[)
    (define rpar #\))
    (define rbrk #\])

    (define (list->bytevector l)
      (define bv (make-bytevector (length l)))
      (do ((l l (cdr l)) (i 0 (+ i 1))) ((null? l) bv)
        (bytevector-u8-set! bv i (car l))))


    (define (read-decorated port)
      (define (r-error p msg . args)
        (apply error "decorated reader:" msg args))

      (define (atomic? form)
        (not (or (pair? form) (and (vector? form) (> (vector-length form) 0))
                 (and (string? form) (> (string-length form) 0)))))

      ; LispKit: all non-atomic forms are recorded (in reverse source order),
      ; also the ones without decorations; see make-decoration-lookup
      (define decorations '())
      (define (decorate! form hc* tc)
        (unless (atomic? form)
          (set! decorations
            (cons (cons form (and (or tc (pair? hc*)) (cons tc hc*)))
                  decorations))))

      (define-values (reader-token? close-paren close-bracket dot)
        (let ((rtm (list 'reader-token)))
          (values
            (lambda (form) (and (pair? form) (eq? (car form) rtm)))
            (cons rtm "right parenthesis") (cons rtm "right bracket")
            (cons rtm "dot"))))

      (define (ws-tail p)
        (let ((c (peek-char p)))
          (cond ((eqv? c #\newline) (read-char p) #f) ; end of tail
                ((and (char? c) (char-whitespace? c))
                 (read-char p)
                 (ws-tail p))
                ((eqv? c #\;) (read-line p))
                (else #f))))

      (define (sub-read-carefully p)
        (let ((form (sub-read p)))
          (cond ((eof-object? form) (r-error p "unexpected end of file"))
                ((reader-token? form)
                 (r-error p "unexpected token" (cdr form)))
                (else form))))

      (define (char-delimiter? c)
        (or (char-whitespace? c) (char=? c lpar) (char=? c rpar)
            (char=? c lbrk) (char=? c rbrk) (char=? c #\")
            (char=? c #\|) (char=? c #\;)))

      (define (char-symbolic? c)
        (or (char-alphabetic? c) (char-numeric? c)
            (memv c
                  '(#\!
                    #\$
                    #\%
                    #\&
                    #\*
                    #\/
                    #\:
                    #\<
                    #\=
                    #\>
                    #\?
                    #\^
                    #\_
                    #\~
                    #\+
                    #\-
                    #\.
                    #\@))))

      (define (sub-scan-to-delimiter c p)
        (let loop ((l (list c)))
          (let ((c (peek-char p)))
            (cond ((or (eof-object? c) (char-delimiter? c))
                   (list->string (reverse l)))
                  (else (loop (cons (read-char p) l)))))))

      (define (sub-read-to-delimiter c p)
        (let ((s (sub-scan-to-delimiter c p)))
          (if (string=? s ".")
              dot
              (read (open-input-string
                      (if *fold-case* (string-append "#!fold-case " s) s))))))

      (define (sub-read-char p)
        (let ((c (read-char p)))
          (if (eof-object? c)
              (r-error p "end of file after #\\")
              (let ((nc (peek-char p)))
                (if (or (eof-object? nc) (char-delimiter? nc))
                    c
                    (let ((s (sub-scan-to-delimiter c p)))
                      (read (open-input-string (string-append "#\\" s)))))))))

      (define (sub-read p)
        (define hc* '())
        (define form
          (let loop ((c (peek-char p)))
            (cond ((eof-object? c) c)
                  ((char=? c #\newline)
                   (read-char p)
                   (set! hc* (cons "" hc*))
                   (loop (peek-char p)))
                  ((char-whitespace? c) (read-char p) (loop (peek-char p)))
                  ((char=? c #\;)
                   (set! hc* (cons (read-line p) hc*))
                   (loop (peek-char p)))
                  ((char=? c lpar)
                   (read-char p)
                   (sub-read-list p close-paren #t))
                  ((char=? c rpar) (read-char p) close-paren)
                  ((char=? c lbrk)
                   (read-char p)
                   (sub-read-list p close-bracket #t))
                  ((char=? c rbrk) (read-char p) close-bracket)
                  ((char=? c #\')
                   (read-char p)
                   (list 'quote (sub-read-carefully p)))
                  ((char=? c #\`)
                   (read-char p)
                   (list 'quasiquote (sub-read-carefully p)))
                  ((char-symbolic? c) (sub-read-to-delimiter (read-char p) p))
                  ((char=? c #\,)
                   (read-char p)
                   (let ((next (peek-char p)))
                     (cond ((eof-object? next) (r-error p "end of file after ,"))
                           ((char=? next #\@)
                            (read-char p)
                            (list 'unquote-splicing (sub-read-carefully p)))
                           (else (list 'unquote (sub-read-carefully p))))))
                  ((char=? c #\") (read p))
                  ((char=? c #\|) (read p))
                  ((char=? c #\#)
                   (read-char p)
                   (let ((c (peek-char p)))
                     (cond ((eof-object? c) (r-error p "end of file after #"))
                           ((char=? c #\!)
                            (let ((str (sub-scan-to-delimiter #\# p)))
                              (cond ((string=? str "#!fold-case") (set! *fold-case* #t))
                                    ((string=? str "#!no-fold-case")
                                     (set! *fold-case* #f))
                                    (else (r-error p "unexpected directive" str))))
                            (loop (peek-char p)))
                           ((or (char-ci=? c #\t) (char-ci=? c #\f) (char-ci=? c #\b)
                                (char-ci=? c #\o) (char-ci=? c #\d) (char-ci=? c #\x)
                                (char-ci=? c #\i) (char-ci=? c #\e))
                            (sub-read-to-delimiter #\# p))
                           ((char=? c #\;) ; not used as a decoration
                            (read-char p)
                            (sub-read-carefully p)
                            (loop (peek-char p)))
                           ((char=? c #\|) ; not used as a decoration
                            (read-char p)
                            (let recur ()
                              (let ((next (read-char p)))
                                (cond ((eof-object? next)
                                       (r-error p "end of file in #| comment"))
                                      ((char=? next #\|)
                                       (let ((next (peek-char p)))
                                         (cond ((eof-object? next)
                                                (r-error p "end of file in #| comment"))
                                               ((char=? next #\#) (read-char p))
                                               (else (recur)))))
                                      ((char=? next #\#)
                                       (let ((next (peek-char p)))
                                         (cond ((eof-object? next)
                                                (r-error p "end of file in #| comment"))
                                               ((char=? next #\|)
                                                (read-char p)
                                                (recur)
                                                (recur))
                                               (else (recur)))))
                                      (else (recur)))))
                            (loop (peek-char p)))
                           ((char=? c lpar)
                            (read-char p)
                            (list->vector (sub-read-list p close-paren #f)))
                           ((char=? c #\u)
                            (read-char p)
                            (if (and (eq? (read-char p) #\8) (eq? (read-char p) lpar))
                                (list->bytevector (sub-read-list p close-paren #f))
                                (r-error p "invalid bytevector syntax")))
                           ((char=? c #\&)
                            (read-char p)
                            ; LispKit: the LispKit reader has no box syntax
                            (r-error p "unexpected box syntax"))
                           ((char=? c #\u)
                            (read-char p)
                            (if (and (eq? (read-char p) #\8) (eq? (read-char p) lpar))
                                (list->bytevector (sub-read-list p close-paren #f)) ; fixme?
                                (r-error p "invalid bytevector syntax")))
                           ((char=? c #\\) (read-char p) (sub-read-char p))
                           ((char-numeric? c)
                            (r-error p "#N=/#N# notation not supported"))
                           (else (r-error p "unknown # syntax" c)))))
                  ; NB: we can fall back on read, but it won't observe *fold-case*
                  (else (read p)))))
        (define tc (if (reader-token? form) #f (ws-tail p)))
        (unless (reader-token? form)
          (decorate! form (reverse hc*) tc))
        form)

      (define (sub-read-list p close-token dot?)
        (let ((form (sub-read p)))
          (if (eq? form dot)
              (r-error p "missing car: ( immediately followed by .") ;)
              (let recur ((form form))
                (cond ((eof-object? form)
                       (r-error p "eof inside list: unbalanced parentheses"))
                      ((eq? form close-token) '())
                      ((eq? form dot)
                       (if dot?
                           (let* ((last-form (sub-read-carefully p))
                                  (another-form (sub-read p)))
                             (if (eq? another-form close-token)
                                 last-form
                                 (r-error p "randomness after form after dot" another-form)))
                           (r-error p "dot in #(...)")))
                      ((reader-token? form)
                       (r-error p "error inside list:" (cdr form)))
                      ; this is the pair that will be in the final object
                      (else (cons form (recur (sub-read p)))))))))

      ; body of read-decorated
      (let ((form (sub-read port)))
        (if (not (reader-token? form))
            (values form decorations)
            (r-error port "unexpected token:" (cdr form)))))


    ; LispKit: pairs are values without identity (eq? on pairs is structural), so
    ; structurally equal forms cannot be distinguished. Decorations are therefore
    ; looked up in source order and each one is consumed when it is used. The
    ; returned procedure also returns the decoration of the top-level form, i.e.
    ; its tail comment, via (lookup #f)
    (define (make-decoration-lookup obj decs)
      (let* ((top (and (pair? decs) (eq? (caar decs) obj) (car decs)))
             (decs (reverse (if top (cdr decs) decs))))
        (lambda (x)
          (if (not x)
              (and top (cdr top) (cadr top))
              (let loop ((ds decs) (acc '()))
                (cond ((null? ds) #f)
                      ((eq? (caar ds) x)
                       (set! decs (append (reverse acc) (cdr ds)))
                       (cdar ds))
                      (else (loop (cdr ds) (cons (car ds) acc)))))))))

    ; helpers for comment string parsing

    (define (skip-while p pred)
      (let ((c (peek-char p)))
        (when (and (not (eof-object? c)) (pred c))
          (read-char p)
          (skip-while p pred))))

    (define (read-or-eof p)
      (define x (guard (ex (else (eof-object))) (read p)))
      (case x ((t) #t) ((nil) #f) (else x)))

    (define (parse-magic-line line)
      (define (parse-magic-kv p)
        (let* ((key (read-or-eof p)) (m (assq key file-key-map)))
          (and m
               (let ((val (read-or-eof p)))
                 (and (not (eof-object? val)) (list (cdr m) val))))))
      (define (parse-magic-kvs p)
        (let loop ((kvs '()))
          (skip-while p char-whitespace?)
          (if (eqv? (peek-char p) #\-)
              (reverse kvs)
              (let ((kv (parse-magic-kv p)))
                (skip-while p char-whitespace?)
                (when (eqv? (peek-char p) #\;) (read-char p))
                (loop (if kv (cons kv kvs) kvs))))))
      (call-with-port (open-input-string line)
        (lambda (p)
          (skip-while p (lambda (ch) (char=? ch #\;)))
          (and (eq? (read-or-eof p) '-*-)
               (let ((kvs (parse-magic-kvs p)))
                 (and (eq? (read-or-eof p) '-*-) (apply append kvs)))))))

    (define (parse-pretty-line line)
      (define (parse-pretty-kv p)
        (let* ((key (read-or-eof p))
               (sep (read-or-eof p))
               (val (read-or-eof p)))
          (and (symbol? key) (eqv? sep ':=)
               (or (symbol? val) (and (pair? val) (eq? (car val) '_)))
               (cons key val))))
      (define (parse-pretty-kvs p)
        (let loop ((kvs '()))
          (skip-while p char-whitespace?)
          (if (eof-object? (peek-char p))
              (reverse kvs)
              (let ((kv (parse-pretty-kv p)))
                (skip-while p char-whitespace?)
                (when (eqv? (peek-char p) #\;) (read-char p))
                (loop (if kv (cons kv kvs) kvs))))))
      (call-with-port (open-input-string line)
        (lambda (p)
          (skip-while p (lambda (ch) (char=? ch #\;)))
          (and (eq? (read-or-eof p) '*)
               (eq? (read-or-eof p) 'pp-styles:)
               (let ((kvs (parse-pretty-kvs p)))
                 (and (eof-object? (peek-char p)) kvs))))))


    ; pretty-printer that optionally uses decorated reader

    (define (pprint-file ifn . rest)
      (define-values (ofn kv*)
        (if (and (pair? rest) (string? (car rest)))
            (values (car rest) (cdr rest))
            (values #f rest)))
      (define (getpar pp-xxx)
        (cond ((memq pp-xxx kv*) =>
               (lambda (p) (and (pair? (cdr p)) (cadr p))))
              (else (pp-xxx))))
      (define styles (getpar pp-styles))
      (define changed-styles? #f)
      (define (copy-style! t&f)
        (define (srcs s)
          (if (symbol? s) (lookup-pp-style styles s) s))
        (set! styles (add-pp-style styles (car t&f) (srcs (cdr t&f))))
        (set! changed-styles? #t))
      (define color (getpar pp-color))
      (define cm
        (if (semantic-color-mapper? color)
            color
            default-semantic-color-mapper))
      (define decorate? (getpar pp-decorate))
      (define emit write-string) ; ignore overrides
      (define tint write-string) ; ignore overrides
      (define (copy-whitespace ip op)
        (let loop ((c (peek-char ip)))
          (when (and (char? c) (char-whitespace? c))
            (write-char (read-char ip) op)
            (loop (peek-char ip)))))
      (define (copy-comment line op)
        (when color
          (tint (semantic-color->start-string 'comment cm) op))
        (emit line op)
        (when color
          (tint (semantic-color->end-string 'comment cm) op))
        (emit "\n" op))
      (define (copy-top-line-comments ip op in-header?)
        (copy-whitespace ip op)
        (when (eqv? (peek-char ip) #\;)
          (let ((line (read-line ip)))
            (cond ((and in-header? (parse-magic-line line)) =>
                   (lambda (kvs)
                     (set! kv* (append kv* kvs))
                     (set! in-header? #f)))
                  ((parse-pretty-line line) =>
                   (lambda (kvs) (for-each copy-style! kvs))))
            (copy-comment line op)
            (copy-top-line-comments ip op in-header?))))
      (define (plain-pf ip op)
        (let loop ()
          (let ((obj (read ip)))
            (unless (eof-object? obj)
              (pp* obj op pp-code #t pp-newline #t kv*)
              (newline op)
              (loop)))))
      (define (decorated-pf ip op)
        (let loop ((in-header? #t))
          (copy-top-line-comments ip op in-header?)
          (let-values (((obj decs) (read-decorated ip)))
            (unless (eof-object? obj)
              ; LispKit: the tail comment of top-level forms is preserved
              (let ((lookup (make-decoration-lookup obj decs)))
                (pp* obj op pp-code #t pp-newline #f pp-decorate lookup
                     (if changed-styles? (cons pp-styles (cons styles kv*)) kv*))
                (cond ((lookup #f) => (lambda (tc) (emit " " op) (copy-comment tc op)))
                      (else (emit "\n" op))))
              (loop #f)))))
      (define pf (if decorate? decorated-pf plain-pf))
      (call-with-input-file ifn
        (lambda (ip)
          (if (not ofn)
              (pf ip (current-output-port))
              (call-with-output-file ofn (lambda (op) (pf ip op)))))))


    (define (html-emit s op)
      (let loop ((i 0) (n (string-length s)))
        (unless (>= i n)
          (let ((c (string-ref s i)))
            (case c
              ((#\<) (write-string "&lt;" op))
              ((#\>) (write-string "&gt;" op))
              ((#\&) (write-string "&amp;" op))
              (else (write-char c op))))
          (loop (+ i 1) n))))

    (define (html-color-mapper sc start?)
      (if start?
          (string-append "<span class=\"" (symbol->string sc) "\">")
          "</span>"))

    ; assumes color-mapper is based on sgr palette
    (define (sc->fg-rgb sc cm)
      (define tier 3) ; get full rgb color
      (define cv (cm sc tier)) ; expect sgr bundle
      (define (parse-cs cs)
        (define p (open-input-string cs))
        (define r #f)
        (define g #f)
        (define b #f)
        (and (eqv? (read p) 38) (eqv? (read-char p) #\;)
             (eqv? (read p) 2) (eqv? (read-char p) #\;)
             (begin (set! r (read p)) (number? r))
             (eqv? (read-char p) #\;)
             (begin (set! g (read p)) (number? g))
             (eqv? (read-char p) #\;)
             (begin (set! b (read p)) (number? b))
             (eof-object? (read-char p)) (+ (* r 65536) (* g 256) b)))
      (and (vector? cv) (= (vector-length cv) 4)
           (string? (vector-ref cv 3))
           (let ((s (vector-ref cv 3)))
             (cond ((string=? s "1") 'bold)
                   ((string=? s "2") 'dim)
                   (else (parse-cs s))))))

    (define (hex rgb)
      (let ((s (number->string rgb 16)))
        (string-append (make-string (- 6 (string-length s)) #\0) s)))

    (define (sc->class-def sc cm)
      (define rgb (sc->fg-rgb sc cm))
      (cond ((number? rgb)
             (string-append "." (symbol->string sc) " { color: #"
               (hex rgb) "; }"))
            ((eq? rgb 'bold)
             (string-append "." (symbol->string sc) " { color: #"
               (hex 15527924) "; font-weight: bold; }"))
            (else #f)))

    (define (display-class-defs cm op)
      (define sc*
        '(comment
          char
          string
          escape
          formal
          variable
          defined
          keyword
          number
          literal
          meta
          directive
          paren
          bracket
          warning))
      (define (pr s) (display s op))
      (let loop ((sc* sc*))
        (unless (null? sc*)
          (let ((cds (sc->class-def (car sc*) cm)))
            (when (string? cds) (pr "  ") (pr cds) (pr "\n")))
          (loop (cdr sc*)))))

    (define (display-html-header cm op)
      (define bg-rgb 1842982)
      (define fg-rgb 13948116)
      (define (pr s) (display s op))
      (define (prl s) (pr s) (pr "\n"))
      (prl "<!DOCTYPE html>")
      (prl "<html>")
      (prl "<head>")
      (prl "<style>") ; todo: takefg / bg colors from the palette
      (pr "  body { background-color: #")
      (pr (hex bg-rgb))
      (pr "; color: #")
      (pr (hex fg-rgb))
      (prl "; margin: 0; padding: 2em; }")
      (prl "  pre {")
      (prl "    font-family: 'Consolas', 'Monaco', 'Andale Mono', 'Ubuntu Mono', monospace;")
      (prl "    font-size: 10pt; line-height: 2.75ex; white-space: pre-wrap;")
      (prl "  }")
      (display-class-defs cm op)
      (prl "</style>")
      (prl "</head>")
      (prl "<body>")
      (pr "<pre>"))

    (define (display-html-footer op)
      (define (pr s) (display s op))
      (define (prl s) (pr s) (pr "\n"))
      (prl "</pre>")
      (prl "</body>")
      (prl "</html>"))

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
    (define (pprint-file/html ifn . rest)
      (define-values (ofn kv*)
        (if (and (pair? rest) (string? (car rest)))
            (values (car rest) (cdr rest))
            (values #f rest)))
      (define (getpar pp-xxx)
        (cond ((memq pp-xxx kv*) =>
               (lambda (p) (and (pair? (cdr p)) (cadr p))))
              (else (pp-xxx))))
      (define styles (getpar pp-styles))
      (define changed-styles? #f)
      (define (copy-style! t&f)
        (define (srcs s)
          (if (symbol? s) (lookup-pp-style styles s) s))
        (set! styles (add-pp-style styles (car t&f) (srcs (cdr t&f))))
        (set! changed-styles? #t))
      (define color (getpar pp-color))
      (define ccm
        (if (semantic-color-mapper? color)
            color
            default-semantic-color-mapper))
      (define cm html-color-mapper)
      (define decorate? (getpar pp-decorate))
      (define emit html-emit)
      (define tint write-string)
      (define (copy-whitespace ip op)
        (let loop ((c (peek-char ip)))
          (when (and (char? c) (char-whitespace? c))
            (write-char (read-char ip) op)
            (loop (peek-char ip)))))
      (define (copy-comment line op)
        (when color
          (tint (semantic-color->start-string 'comment cm) op))
        (emit line op)
        (when color
          (tint (semantic-color->end-string 'comment cm) op))
        (emit "\n" op))
      (define (copy-top-line-comments ip op in-header?)
        (copy-whitespace ip op)
        (when (eqv? (peek-char ip) #\;)
          (let ((line (read-line ip)))
            (cond ((and in-header? (parse-magic-line line)) =>
                   (lambda (kvs)
                     (set! kv* (append kv* kvs))
                     (set! in-header? #f)))
                  ((parse-pretty-line line) =>
                   (lambda (kvs) (for-each copy-style! kvs))))
            (copy-comment line op)
            (copy-top-line-comments ip op in-header?))))
      (define (plain-pf ip op)
        (display-html-header ccm op)
        (let loop ()
          (let ((obj (read ip)))
            (unless (eof-object? obj)
              (pp* obj op pp-code #t pp-newline #t pp-emit html-emit pp-color
                   html-color-mapper kv*)
              (newline op)
              (loop))))
        (display-html-footer op))
      (define (decorated-pf ip op)
        (display-html-header ccm op)
        (let loop ((in-header? #t))
          (copy-top-line-comments ip op in-header?)
          (let-values (((obj decs) (read-decorated ip)))
            (unless (eof-object? obj)
              ; LispKit: the tail comment of top-level forms is preserved
              (let ((lookup (make-decoration-lookup obj decs)))
                (pp* obj op pp-code #t pp-newline #f pp-decorate lookup pp-emit
                     html-emit pp-color html-color-mapper
                     (if changed-styles? (cons pp-styles (cons styles kv*)) kv*))
                (cond ((lookup #f) => (lambda (tc) (emit " " op) (copy-comment tc op)))
                      (else (emit "\n" op))))
              (loop #f))))
        (display-html-footer op))
      (define pf (if decorate? decorated-pf plain-pf))
      (call-with-input-file ifn
        (lambda (ip)
          (if (not ofn)
              (pf ip (current-output-port))
              (call-with-output-file ofn (lambda (op) (pf ip op)))))))))

