;;; SRFI 177
;;; Portable keyword arguments
;;;
;;; Many Scheme implementations have keyword arguments, but they have not been widely
;;; standardized. This SRFI defines the macros `keyword-lambda` and `keyword-call`. They
;;; can be used identically in every major implementation currently in use, making it
;;; safe to use keyword arguments in portable code. The macros expand to native keyword
;;; arguments in Schemes that have them, letting programmers mix portable code and
;;; implementation-specific code.
;;;
;;; Keyword arguments are a very useful tool for managing complexity as programs grow.
;;; They are a natural solution to the "no, wait, this procedure still needs another
;;; argument" problem which is almost guaranteed to pop up many times over the lifetime
;;; of any non-trivial program. Humans simply cannot plan years ahead at this level of
;;; detail, and adding keyword arguments as an afterthought is less objectionable than
;;; accumulating long lists of optional positional arguments or refactoring central APIs
;;; every few years when third-party code depends on them.
;;;
;;; Copyright © 2019 Lassi Kortela. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software, and to permit
;;; persons to whom the Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies or
;;; substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2019 Matthias Zenger. All rights reserved.

(define-library (srfi 177)

  (export keyword-lambda
          keyword-call)

  (import (lispkit base))

  (begin

    (define-syntax let-false
      (syntax-rules ()
        ((_ () body ...) (begin body ...))
        ((_ (var) body ...) (let ((var #f)) body ...))
        ((_ (var vars ...) body ...)
          (let ((var #f)) (let-false (vars ...) body ...)))))

    (define-syntax kw-setters
      (syntax-rules ()
        ((_ sym val)
          (error (if (symbol? sym)
                     (string-append "keyword not known: " (symbol->string sym))
                     "keyword is not a symbol")))
        ((_ sym val keyword keywords ...)
          (if (eq? 'keyword sym)
              (set! keyword val)
              (kw-setters sym val keywords ...)))))

    (define-syntax keyword-lambda
      (syntax-rules (kv val vals loop i)
        ((_ (formals ... (keywords ...)) body ...)
          (lambda (formals ... . kv)
            (let-false (keywords ...)
              (let loop ((kv kv))
                (cond ((null? kv))
                      ((not (pair? (cdr kv)))
                       (error "Keyword without value"))
                      (else
                       (let ((sym (car kv)) (val (cadr kv)))
                         (kw-setters sym val keywords ...))
                      (loop (cddr kv)))))
              ((lambda () body ...)))))))

    (define-syntax kw-call-aux
      (syntax-rules ()
        ((_ kw-lambda (kvs ...) (args ...) ())
          (kw-lambda args ... kvs ...))
        ((_ kw-lambda (kvs ...) (args ...) (key val more-kvs ...))
          (kw-call-aux kw-lambda (kvs ... 'key val) (args ...) (more-kvs ...)))))

    (define-syntax keyword-call
      (syntax-rules ()
        ((_ kw-lambda args ... (kvs ...))
          (kw-call-aux kw-lambda () (args ...) (kvs ...)))))
  )
)

