;;; LISPKIT PRETTY-PRINT
;;;
;;; Library for pretty printing data and Scheme code. This library is a LispKit-style
;;; front end for the pretty printer of SRFI 272 (libraries `(srfi 272 advanced)` and
;;; `(srfi 272 fancy)`). Instead of the key/value arguments of SRFI 272, all
;;; procedures of this library accept an optional configuration in the form of an
;;; association list whose keys are symbols, e.g. `((width . 60) (radix . 16))`.
;;; The keys correspond to the names of the SRFI 272 parameter objects without the
;;; `pp-` prefix. A default configuration can be provided via the parameter object
;;; `current-pretty-print-config`.
;;;
;;; Procedures for printing data use the data layout by default; procedures for
;;; printing source code always use the code layout. When formatting code, this
;;; library uses its own style registry by default. This
;;; registry extends the registry of SRFI 272 with styles for syntactical forms provided
;;; by LispKit libraries, such as `let-optionals`, `define-enum`, or `match`. The style
;;; registry of SRFI 272 for plain Scheme code remains available via configuration
;;; `((styles . scheme))`.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
;;; file except in compliance with the License. You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed under
;;; the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;; KIND, either express or implied. See the License for the specific language governing
;;; permissions and limitations under the License.

(define-library (lispkit pretty-print)

  ;; Printing
  (export pretty-print
          pretty-print-shared
          pretty-print-simple
          pretty-print->string
          pretty-print-file
          pretty-print-string
          current-pretty-print-config)

  ;; Styles
  (export pretty-print-style
          pretty-print-style-lookup
          pretty-print-style-add
          pretty-print-style-registry)

  ;; Hooks
  (export pretty-print-hook
          pretty-print-hook-lookup
          pretty-print-hook-add
          pretty-print-hook-registry
          pretty-print-hook-rmac
          pretty-print-hook-glst
          pretty-print-hook-bvec
          pretty-print-hook-atom)

  (import (lispkit base)
          (only (srfi 272 fancy)
                pprint pprint-shared pprint-simple
                lookup-pp-style add-pp-style pretty-hook lookup-pp-hook add-pp-hook
                rmac-pp-hook glst-pp-hook bvec-pp-hook atom-pp-hook
                pp-width pp-circle pp-graph pp-radix pp-length pp-level pp-lines
                pp-level-stub pp-length-stub pp-lines-stub pp-indent pp-tab pp-max-tab
                pp-miser-width pp-inline-width pp-brackets pp-code pp-pretty pp-newline
                pp-color pp-emit pp-tint pp-decorate pp-styles pp-hooks)
          (srfi 272 fancy support)
          (only (srfi 272 colorize) sgr-support-tier))

  ;; Style registries
  (begin

    ;; Styles for syntactical forms provided by LispKit libraries
    (define lispkit-style-definitions
      '(; lambda-like forms
        (λ _ f . body)
        (opt-lambda _ f . body)
        (opt*-lambda _ f . body)
        (lambda/tag _ e f . body)
        (case-λ _ . fc*)
        (case-lambda/tag _ e . fc*)
        (thunk _ . body)
        (thunk* _ . body)
        (stream-lambda _ f . body)
        ; binding forms
        (let-optionals _ e fc* . body)
        (let*-optionals _ e fc* . body)
        (let-keywords _ e fc* . body)
        (let*-keywords _ e fc* . body)
        (letrec-values _ fc* . body)
        (receive _ f e . body)
        (if-let* _ fc* . body)
        (when-let* _ fc* . body)
        (stream-let _ i? fc* . body)
        ; definitions
        (define-enum _ i fill . fill)
        (define-enumeration _ i fill . fill)
        (define-datatype _ i . body)
        (define-type _ h i . body)
        (define-optionals _ h . body)
        (define-optionals* _ h . body)
        (define-stream _ h . body)
        (define-class _ h . body)
        (define-generic _ h . body)
        (define-pattern _ h . body)
        ; control
        (while _ e . body)
        (dolist _ fc . body)
        (dotimes _ fc . body)
        (loop _ i . body)
        (exit-with _ i . body)
        (unwind-protect _ . body)
        (with-mutex _ e . body)
        (assert _ . fill)
        (time _ . fill)
        (delay-force _ . fill)
        (stream-delay _ . fill)
        (future _ . fill)
        ; pattern matching
        (match _ e . dc*)
        (match-lambda _ . dc*)
        (match-lambda* _ . dc*)
        (match-let _ i? dc* . body)
        (match-let* _ dc* . body)
        (match-letrec _ dc* . body)
        (stream-match _ e . dc*)
        ; logic programming
        (conde _ . ec*)
        (conda _ . ec*)
        (condu _ . ec*)
        (fresh _ f . body)
        (run _ e f . body)
        (run* _ f . body)
        ; iteration
        (for/and _ fc* . body)
        (for/or _ fc* . body)
        (for/first _ fc* . body)
        (for/last _ fc* . body)
        (for/list _ fc* . body)
        (for/vector _ fc* . body)
        (for/hash _ fc* . body)
        (for/hasheq _ fc* . body)
        (for/hasheqv _ fc* . body)
        (for/sum _ fc* . body)
        (for/product _ fc* . body)
        (for/fold _ fc* fc* . body)
        (for*/and _ fc* . body)
        (for*/or _ fc* . body)
        (for*/first _ fc* . body)
        (for*/last _ fc* . body)
        (for*/list _ fc* . body)
        (for*/vector _ fc* . body)
        (for*/hash _ fc* . body)
        (for*/hasheq _ fc* . body)
        (for*/hasheqv _ fc* . body)
        (for*/sum _ fc* . body)
        (for*/product _ fc* . body)
        (for*/fold _ fc* fc* . body)
        ; objects
        (object _ fc* . fc*)
        ; drawings
        (with-drawing _ e . body)
        (with-shape _ e . body)
        ; logging
        (log-using _ e . body)
        (log-with-tag _ e . body)
        (log-into-file _ e . body)
        (log-from-severity _ e . body)
        (log-dropping-below-severity _ e . body)
        ; testing
        (test-group _ e . body)))

    ;; The LispKit style registry: the SRFI 272 registry extended with LispKit styles.
    ;; `pretty-print-style` modifies this registry only; the SRFI 272 registry remains
    ;; unchanged.
    (define lispkit-styles
      (let loop ((defs lispkit-style-definitions) (styles (pp-styles)))
        (if (null? defs)
            styles
            (loop (cdr defs) (add-pp-style styles (caar defs) (cdar defs))))))

    (define pretty-print-style
      (case-lambda
        ((sym)
          (lookup-pp-style lispkit-styles sym))
        ((sym style)
          (set! lispkit-styles (add-pp-style lispkit-styles sym style)))))

    (define pretty-print-style-registry
      (case-lambda
        (()
          lispkit-styles)
        ((which)
          (case which
            ((lispkit) lispkit-styles)
            ((scheme)  (pp-styles))
            (else      (error "unknown style registry" which))))))

    (define pretty-print-style-lookup lookup-pp-style)
    (define pretty-print-style-add add-pp-style)
  )

  ;; Hooks
  (begin

    (define pretty-print-hook pretty-hook)
    (define pretty-print-hook-lookup lookup-pp-hook)
    (define pretty-print-hook-add add-pp-hook)
    (define pretty-print-hook-rmac rmac-pp-hook)
    (define pretty-print-hook-glst glst-pp-hook)
    (define pretty-print-hook-bvec bvec-pp-hook)
    (define pretty-print-hook-atom atom-pp-hook)

    (define (pretty-print-hook-registry)
      (pp-hooks))
  )

  ;; Configurations
  (begin

    ;; Maps configuration keys to SRFI 272 parameter objects
    (define config-keys
      (list (cons 'width pp-width)
            (cons 'circle pp-circle)
            (cons 'graph pp-graph)
            (cons 'radix pp-radix)
            (cons 'length pp-length)
            (cons 'level pp-level)
            (cons 'lines pp-lines)
            (cons 'level-stub pp-level-stub)
            (cons 'length-stub pp-length-stub)
            (cons 'lines-stub pp-lines-stub)
            (cons 'indent pp-indent)
            (cons 'tab pp-tab)
            (cons 'max-tab pp-max-tab)
            (cons 'miser-width pp-miser-width)
            (cons 'inline-width pp-inline-width)
            (cons 'brackets pp-brackets)
            (cons 'code pp-code)
            (cons 'pretty pp-pretty)
            (cons 'newline pp-newline)
            (cons 'color pp-color)
            (cons 'emit pp-emit)
            (cons 'tint pp-tint)
            (cons 'decorate pp-decorate)
            (cons 'styles pp-styles)
            (cons 'hooks pp-hooks)))

    ;; Converts configuration values which are specific to this library
    (define (config-value key value)
      (if (eq? key 'styles)
          (if (symbol? value) (pretty-print-style-registry value) value)
          value))

    ;; Turns a configuration (an association list) into a list of alternating keys
    ;; (SRFI 272 parameter objects) and values
    (define (config->kv config)
      (let loop ((cs config) (acc '()))
        (cond ((null? cs)
                (reverse acc))
              ((and (pair? cs) (pair? (car cs)) (eq? (caar cs) 'color-tier))
                ; color-tier is not a SRFI 272 parameter; see `with-config`
                (if (memv (cdar cs) '(#f 0 1 2 3))
                    (loop (cdr cs) acc)
                    (error "invalid value for color-tier" (cdar cs))))
              ((and (pair? cs) (pair? (car cs)) (symbol? (caar cs)))
                (let ((e (assq (caar cs) config-keys)))
                  (if e
                      (loop (cdr cs)
                            (cons (config-value (caar cs) (cdar cs)) (cons (cdr e) acc)))
                      (error "unknown pretty-print configuration key" (caar cs)))))
              (else
                (error "invalid pretty-print configuration" config)))))

    ;; Default configuration
    (define current-pretty-print-config
      (make-parameter '() (lambda (config) (config->kv config) config)))

    ;; Parameterizes the parameter objects of the given key/value list
    (define (parameterize* kv thunk)
      (if (null? kv)
          (thunk)
          (parameterize (((car kv) (cadr kv)))
            (parameterize* (cddr kv) thunk))))

    ;; Calls `proc` with the key/value list for `config` in a dynamic environment in
    ;; which the SRFI 272 parameter objects are set according to the LispKit style
    ;; registry and the default configuration. The given configuration thus takes
    ;; precedence over configurations embedded in source code (magic comments), which
    ;; take precedence over the default configuration
    ;; The terminal color support tier (`color-tier`) is defined by the SRFI 272 parameter
    ;; object `sgr-support-tier`, which is read while the output is generated; it gets
    ;; parameterized according to the given or the default configuration
    ;; `code?` determines the default layout (code or data)
    (define (with-config config code? proc)
      (let* ((kv (config->kv config))
             (default (current-pretty-print-config))
             (tier (cond ((assq 'color-tier config) => cdr)
                         ((assq 'color-tier default) => cdr)
                         (else (sgr-support-tier)))))
        (parameterize ((pp-styles lispkit-styles)
                       (pp-code code?)
                       (sgr-support-tier tier))
          (parameterize* (config->kv default)
            (lambda () (proc kv))))))
  )

  ;; Printing
  (begin

    ;; Splits the optional arguments `[port] [config]` of the printing procedures
    (define (port-and-config who args)
      (cond ((null? args)
              (values (current-output-port) '()))
            ((port? (car args))
              (cond ((null? (cdr args)) (values (car args) '()))
                    ((null? (cddr args)) (values (car args) (cadr args)))
                    (else (error "too many arguments" who args))))
            ((null? (cdr args))
              (values (current-output-port) (car args)))
            (else
              (error "invalid arguments" who args))))

    (define (pretty-print obj . args)
      (let-values (((port config) (port-and-config 'pretty-print args)))
        (with-config config #f (lambda (kv) (apply pprint obj port kv)))))

    (define (pretty-print-shared obj . args)
      (let-values (((port config) (port-and-config 'pretty-print-shared args)))
        (with-config config #f (lambda (kv) (apply pprint-shared obj port kv)))))

    (define (pretty-print-simple obj . args)
      (let-values (((port config) (port-and-config 'pretty-print-simple args)))
        (with-config config #f (lambda (kv) (apply pprint-simple obj port kv)))))

    (define (pretty-print->string obj . config)
      (let ((port (open-output-string)))
        (with-config (if (pair? config) (car config) '()) #f
                     (lambda (kv) (apply pprint obj port kv)))
        (get-output-string port)))

    (define (pretty-print-file infile . args)
      (let-values
        (((out config)
          (cond ((null? args)
                  (values (current-output-port) '()))
                ((or (string? (car args)) (port? (car args)))
                  (cond ((null? (cdr args)) (values (car args) '()))
                        ((null? (cddr args)) (values (car args) (cadr args)))
                        (else (error "too many arguments" 'pretty-print-file args))))
                ((null? (cdr args))
                  (values (current-output-port) (car args)))
                (else
                  (error "invalid arguments" 'pretty-print-file args)))))
        (with-config config #t
          (lambda (kv)
            (call-with-input-file infile
              (lambda (ip)
                (if (string? out)
                    (call-with-output-file out (lambda (op) (pprint-port ip op kv)))
                    (pprint-port ip out kv))))))))

    (define (pretty-print-string str . config)
      (let ((op (open-output-string)))
        (with-config (if (pair? config) (car config) '()) #t
                     (lambda (kv) (pprint-port (open-input-string str) op kv)))
        (get-output-string op)))
  )
)
