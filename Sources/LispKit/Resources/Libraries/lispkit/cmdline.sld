;;; LISPKIT CMDLINE
;;;
;;; Provides two different approaches to command-line parsing:
;;;   
;;;   1. Table-driven command-line parsing with automatic help text. The interface of
;;;      `parse-command-line` follows the one of Racket's `racket/cmdline` library, but
;;;      uses only plain data and procedures.
;;;   2. Command-line parsing based on a fold over the arguments is available via SRFI 37's
;;;      `args-fold` procedure.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed under the
;;; License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;;; either express or implied. See the License for the specific language governing permissions
;;; and limitations under the License.

(define-library (lispkit cmdline)

  (export parse-command-line
          option
          option?
          option-names
          option-required-arg?
          option-optional-arg?
          option-processor
          args-fold)

  (import (lispkit base)
          (lispkit list util)
          (srfi 37))

  (begin

    ;; A flag specification
    (define-record-type <spec>
      (make-spec mode group flags handler nargs help)
      spec?
      (mode spec-mode)           ; once-each, once-any, multi or final
      (group spec-group)         ; Group identifier for once-any specifications
      (flags spec-flags)         ; List of flag strings
      (handler spec-handler)     ; Procedure which is called with the flag and its arguments
      (nargs spec-nargs)         ; Number of arguments of the flag
      (help spec-help))          ; Help list

    (define (help-description help)
      (let ((d (car help)))
        (if (list? d) d (list d))))

    (define (help-arg-names help)
      (cdr help))

    ;; Translates the table into a list of specifications and a list of help items, which are
    ;; either strings (or pairs `(usage . string)` and `(ps . string)`) or specifications.
    (define (compile-table who table)
      (let loop ((entries table) (group 0) (specs '()) (items '()) (usage '()) (ps '()))
        (if (null? entries)
            (values (reverse specs) (reverse items) (reverse usage) (reverse ps))
            (let ((entry (car entries)))
              (unless (and (pair? entry) (symbol? (car entry)))
                (error "parse-command-line: invalid table entry" entry))
              (case (car entry)
                ((help-labels)
                  (loop (cdr entries) group specs
                        (append (reverse (cdr entry)) items) usage ps))
                ((usage-help)
                  (loop (cdr entries) group specs items
                        (append (reverse (cdr entry)) usage) ps))
                ((ps)
                  (loop (cdr entries) group specs items usage
                        (append (reverse (cdr entry)) ps)))
                ((once-each once-any multi final)
                  (let ((new (map (lambda (s)
                                    (unless (and (list? s) (= (length s) 3)
                                                 (list? (car s)) (pair? (car s))
                                                 (procedure? (cadr s))
                                                 (pair? (caddr s)))
                                      (error "parse-command-line: invalid flag specification" s))
                                    (let ((nargs (length (help-arg-names (caddr s)))))
                                      (unless (procedure-arity-includes? (cadr s) (+ nargs 1))
                                        (error "parse-command-line: handler arity does not match help"
                                               (car s)))
                                      (make-spec (car entry) group (car s) (cadr s) nargs (caddr s))))
                                  (cdr entry))))
                    (loop (cdr entries) (+ group 1)
                          (append (reverse new) specs)
                          (append (reverse new) items)
                          usage ps)))
                (else
                  (error "parse-command-line: invalid table mode" (car entry))))))))

    ;; Produces the help text
    (define (make-help-text name specs items usage ps arg-help)
      (define (flag-line spec prefix)
        (string-append
          prefix
          (let loop ((fs (spec-flags spec)) (acc ""))
            (cond ((null? fs) acc)
                  ((string=? acc "") (loop (cdr fs) (car fs)))
                  (else (loop (cdr fs) (string-append acc ", " (car fs))))))
          (apply string-append
                 (map (lambda (a) (string-append " <" a ">")) (help-arg-names (spec-help spec))))
          " : "
          (car (help-description (spec-help spec)))))
      (define (extra-lines spec)
        (map (lambda (l) (string-append "     " l)) (cdr (help-description (spec-help spec)))))
      (let ((port (open-output-string)))
        (define (line . strs)
          (for-each (lambda (s) (write-string s port)) strs)
          (newline port))
        (apply line name " [ <option> ... ]"
                    (map (lambda (a) (string-append " <" a ">")) arg-help))
        (for-each (lambda (u) (line " " u)) usage)
        (line " where <option> is one of")
        (let loop ((items items) (prev #f))
          (unless (null? items)
            (let ((item (car items)))
              (cond ((string? item)
                      (line " " item)
                      (loop (cdr items) #f))
                    (else
                      (let* ((next (and (pair? (cdr items)) (spec? (cadr items)) (cadr items)))
                             (grouped? (eq? (spec-mode item) 'once-any))
                             (prefix (cond ((not grouped?)  "  ")
                                           ((and prev (eq? (spec-group prev) (spec-group item))
                                                 next (eq? (spec-group next) (spec-group item)))
                                            "| ")
                                           ((and prev (eq? (spec-group prev) (spec-group item)))
                                            "\\ ")
                                           ((and next (eq? (spec-group next) (spec-group item)))
                                            "/ ")
                                           (else "  "))))
                        (line (flag-line item prefix))
                        (for-each (lambda (l) (line l)) (extra-lines item))
                        (loop (cdr items) item)))))))
        (line "  -h, --help : Show this help")
        (line "  -- : Do not treat any remaining argument as a flag (at this level)")
        (line " Multiple single-letter flags can be combined after one `-'; for")
        (line "  example: `-h-' is the same as `-h --'")
        (for-each (lambda (p) (line " " p)) ps)
        (get-output-string port)))

    (define (flag-like? str)
      (and (> (string-length str) 1)
           (memv (string-ref str 0) '(#\- #\+))
           (not (string->number str))))

    (define (long-flag? str)
      (and (> (string-length str) 2)
           (char=? (string-ref str 1) (string-ref str 0))))

    (define (default-help-proc text)
      (display text)
      (exit 0))

    (define (make-default-unknown-proc name)
      (lambda (flag . args)
        (error (string-append name ": unknown flag") flag)))

    ;; Parses the command-line arguments `argv` (a list or vector of strings) according to
    ;; `table`. Returns the result of `finish-proc`, which is called with a list of the
    ;; (non-void) results of the flag handlers followed by the non-flag arguments.
    (define parse-command-line
      (case-lambda
        ((name argv table finish arg-help)
          (parse-command-line name argv table finish arg-help
                              default-help-proc (make-default-unknown-proc name)))
        ((name argv table finish arg-help help-proc)
          (parse-command-line name argv table finish arg-help
                              help-proc (make-default-unknown-proc name)))
        ((name argv table finish arg-help help-proc unknown-proc)
          (let ((argv (if (vector? argv) (vector->list argv) argv)))
            (let-values (((specs items usage ps) (compile-table name table)))
              (define (lookup flag)
                (let loop ((ss specs))
                  (cond ((null? ss) #f)
                        ((member flag (spec-flags (car ss))) (car ss))
                        (else (loop (cdr ss))))))
              (define (help-text)
                (make-help-text name specs items usage ps arg-help))
              (define (done accum operands)
                (unless (procedure-arity-includes? finish (+ (length operands) 1))
                  (error (string-append name ": the wrong number of arguments were provided")
                         (length operands) arg-help))
                (apply finish (reverse accum) operands))
              (define (add accum result)
                (if (void? result) accum (cons result accum)))
              ;; Applies a flag specification; returns the remaining arguments, the new
              ;; accumulator, the new list of used specifications and whether this was a
              ;; `final` flag.
              (define (apply-spec spec flag args accum used)
                (let ((n (spec-nargs spec)))
                  (when (< (length args) n)
                    (error (string-append name ": the \"" flag "\" flag needs more arguments")
                           n (length args)))
                  (case (spec-mode spec)
                    ((once-each)
                      (when (memq spec used)
                        (error (string-append name ": the \"" flag "\" flag can only be used once")))
                      #t)
                    ((once-any)
                      (let ((other (filter-map (lambda (u)
                                                 (and (eq? (spec-mode u) 'once-any)
                                                      (eqv? (spec-group u) (spec-group spec))
                                                      u))
                                               used)))
                        (unless (null? other)
                          (error (string-append name ": the \"" flag "\" flag is not allowed with the \""
                                                (car (spec-flags (car other))) "\" flag")))))
                    (else #f))
                  (let ((result (apply (spec-handler spec) flag (take args n))))
                    (values (drop args n)
                            (add accum result)
                            (cons spec used)
                            (eq? (spec-mode spec) 'final)))))
              (let loop ((args argv) (accum '()) (used '()))
                (if (null? args)
                    (done accum '())
                    (let ((arg (car args)))
                      (cond
                        ((string=? arg "--")
                          (done accum (cdr args)))
                        ((not (flag-like? arg))
                          (done accum args))
                        ((member arg '("-h" "--help"))
                          (help-proc (help-text))
                          (loop (cdr args) accum used))
                        ((or (long-flag? arg) (= (string-length arg) 2))
                          (let ((spec (lookup arg)))
                            (if spec
                                (let-values (((rest accum used final?)
                                                (apply-spec spec arg (cdr args) accum used)))
                                  (if final? (done accum rest) (loop rest accum used)))
                                (loop (cdr args) (add accum (unknown-proc arg)) used))))
                        (else
                          ;; Cluster of single-letter flags, e.g. "-abc"
                          (let ((prefix (string-ref arg 0))
                                (len (string-length arg)))
                            (let cluster ((i 1) (rest (cdr args)) (accum accum) (used used))
                              (if (>= i len)
                                  (loop rest accum used)
                                  (let* ((flag (string prefix (string-ref arg i)))
                                         (spec (if (string=? flag "-h") #f (lookup flag))))
                                    (cond ((string=? flag "-h")
                                            (help-proc (help-text))
                                            (cluster (+ i 1) rest accum used))
                                          (spec
                                            (let-values (((rest accum used final?)
                                                            (apply-spec spec flag rest accum used)))
                                              (if final?
                                                  (done accum rest)
                                                  (cluster (+ i 1) rest accum used))))
                                          (else
                                            (cluster (+ i 1) rest
                                                     (add accum (unknown-proc flag)) used)))))))))))))))))
  )
)
