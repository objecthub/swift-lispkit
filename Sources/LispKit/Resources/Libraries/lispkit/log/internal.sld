;;; LISPKIT LOG INTERNAL
;;;
;;; Implementation of a simple logging library. Log entries are sent to a _logger_.
;;; A logger processes each log entry (e.g. by adding or filtering information) and eventually
;;; persists it if the severity of the log entry is at or above the level of the severity of
;;; the logger. Supported are logging to a port and into a file. A log entry consists
;;; of the following four components: a timestamp, a severity, a sequence of tags, and a
;;; log message. Timestamps are generated via `current-second`. There are five severities
;;; supported by this library: `debug` (0), `info` (1), `warn` (2), `err` (3), and `fatal` (4).
;;; Each tag is represented as a symbol. The sequence of tags is represented as a list of
;;; symbols. A log message is a string.
;;;
;;; Logging functions take the logger as an optional argument. If it is not provided, the
;;; _current logger_ is chosen. The current logger is represented via the parameter object
;;; `current-logger`. The current logger is initially set to the `default-logger`.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2019 Matthias Zenger. All rights reserved.
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

(define-library (lispkit log internal)

  ;; Severities
  (export severity?
          severity->level
          severity->string)

  ;; Logger datatype
  (export logger?
          make-logger
          make-logger-object
          close-logger
          logger-addproc
          logger-closeproc
          logger-state
          logger-severity
          logger-severity-set!)

  ;; Log functions
  (export log
          log-debug
          log-info
          log-warn
          log-error
          log-fatal)

  ;; Logger implementations
  (export current-logger
          make-tag-logger
          make-filter-logger
          make-port-logger
          make-file-logger
          tags->string
          long-log-formatter
          short-log-formatter)

  ;; Syntax extensions
  (export log-using
          log-from-severity
          log-with-tag
          log-dropping-below-severity
          log-from-severity
          log-time)

  (import (except (lispkit base) log)
          (lispkit date-time)
          (lispkit debug))

  (begin

    ;; Logging severities

    (define (severity? severity)
      (or (eq? severity 'debug)
          (eq? severity 'info)
          (eq? severity 'warn)
          (eq? severity 'error)
          (eq? severity 'fatal)))

    (define (severity->level severity)
      (case severity ('debug 0)
                     ('info  1)
                     ('warn  2)
                     ('error 3)
                     ('fatal 4)
                     (else   (error "unknown severity" severity))))

    (define (severity->string severity)
      (case severity ('debug "DEBUG")
                     ('info  "INFO")
                     ('warn  "WARN")
                     ('error "ERROR")
                     ('fatal "FATAL")
                     (else   (error "unknown severity" severity))))

    ;; Logger datatype

    (define-values (new-logger logger? logger-ref make-logger-subtype) (make-type 'logger))

    (define (make-logger-object addproc closeproc state)
      (new-logger (cons (cons addproc closeproc) state)))

    (define make-logger
      (case-lambda
        ((addproc logger)
          (make-logger-object addproc (logger-closeproc logger) (logger-state logger)))
        ((addproc closeproc logger)
          (make-logger-object addproc
                              (lambda () (closeproc) ((logger-closeproc logger)))
                              (logger-state logger)))))

    (define (logger-addproc logger)
      (caar (logger-ref logger)))

    (define (logger-closeproc logger)
      (cdar (logger-ref logger)))

    (define (logger-state logger)
      (cdr (logger-ref logger)))

    (define (logger-severity logger)
      (vector-ref (logger-state logger) 0))

    (define (logger-severity-set! logger severity)
      (if (severity? severity)
          (vector-set! (logger-state logger) 0 severity)
          (error "cannot set severity of logger $0 to $1" logger severity)))

    (define (log severity message . args)
      (if (severity? severity)
          (if (string? message)
              (if (null? args)
                  (log-entry (current-logger) severity message '())
                  (if (symbol? (car args))
                      (if (null? (cdr args))
                          (log-entry (current-logger) severity message (list (car args)))
                          (log-entry (cadr args) severity message (list (car args))))
                      (log-entry (car args) severity message '())))
              (error "log with illegal message" message))
          (error "log with illegal severity" severity)))

    (define (log-entry logger severity message tags)
      (if (>= (severity->level severity) (severity->level (logger-severity logger)))
          ((logger-addproc logger) (current-second) severity message tags)))

    (define (log-debug message . args)
      (apply log 'debug message args))

    (define (log-info message . args)
      (apply log 'info message args))

    (define (log-warn message . args)
      (apply log 'warn message args))

    (define (log-error message . args)
      (apply log 'error message args))

    (define (log-fatal message . args)
      (apply log 'fatal message args))

    (define (close-logger logger)
      ((logger-closeproc logger)))

    ;; Logger implementations

    (define (make-tag-logger tag logger)
      (make-logger
        (lambda (time severity message tags)
          ((logger-addproc logger) time severity message (cons tag tags)))
        logger))

    (define (make-filter-logger filter logger)
      (make-logger
        (lambda (time severity message tags)
          (if (filter time severity message tags)
              ((logger-addproc logger) time severity message tags)))
        logger))

    (define (make-port-logger port formatter logger)
      (make-logger
        (lambda (time severity message tags)
          (display (formatter time severity message tags) port)
          (newline port)
          ((logger-addproc logger) time severity message tags))
        logger))

    (define (make-file-logger path formatter logger)
      (let* ((port (open-output-file path)))
        (make-logger
          (lambda (time severity message tags)
            (display (formatter time severity message tags) port)
            (newline port)
            ((logger-addproc logger) time severity message tags))
          (lambda () (close-output-port port))
          logger)))

    (define (tags->string tags)
      (if (null? tags)
          ""
          (do ((lst (cdr tags) (cdr lst))
               (res (symbol->string (car tags))))
              ((null? lst) res)
            (set! res (string-append res "/" (symbol->string (car lst)))))))

    (define (short-log-formatter time severity message tags)
      (string-append (date-time->string (seconds->date-time time) #f "HH:mm:ss ")
                     (string-pad-right (severity->string severity) #\space 6)
                     (if (null? tags)
                         ""
                         (string-append "|" (tags->string tags)))
                     "| "
                     message))

    (define (long-log-formatter time severity message tags)
      (string-append (date-time->string (seconds->date-time time) #f "yyyy-MM-dd HH:mm:ss ")
                     (string-pad-right (severity->string severity) #\space 6)
                     (if (null? tags)
                         ""
                         (string-append "|" (tags->string tags)))
                     "| "
                     message))

    ;; Current logger; needs to be set by the concrete logger interface, e.g. library
    ;; (lispkit log)

    (define current-logger (make-parameter #f))

    ;; Syntactic sugar

    (define-syntax log-using
      (syntax-rules ()
        ((_ logger expr0 expr1 ...)
          (parameterize ((current-logger logger)) expr0 expr1 ...))))

    (define-syntax log-with-tag
      (syntax-rules ()
        ((_ tag expr0 expr1 ...)
          (parameterize ((current-logger (make-tag-logger tag (current-logger))))
            expr0 expr1 ...))))

    (define-syntax log-dropping-below-severity
      (syntax-rules ()
        ((_ severity expr0 expr1 ...)
          (let ((mins (severity->level severity)))
            (parameterize ((current-logger (make-filter-logger
                                             (lambda (time s message tags)
                                               (> (severity->level s) mins))
                                             (current-logger))))
              expr0 expr1 ...)))))

    (define-syntax log-from-severity
      (syntax-rules ()
        ((_ severity expr0 expr1 ...)
          (let* ((newvalue severity)
                 (logger (current-logger))
                 (oldvalue (logger-severity logger)))
            (dynamic-wind
              (lambda () (logger-severity-set! logger newvalue))
              (lambda () expr0 expr1 ...)
              (lambda () (logger-severity-set! logger oldvalue)))))))
    
    (define-syntax log-time
      (syntax-rules ()
        ((_ expr)
          (log-time expr #f))
        ((_ expr descr)
          (log-time expr descr time))
        ((_ expr descr tag)
          (log-time expr descr tag (current-logger)))
        ((_ expr descr tag logger)
          (apply-with-values
            (lambda (t . args)
              (log
                (quote debug)
                (let ((dscr descr)
                      (port (open-output-string)))
                  (display (number->string t 10 1 6 #t) port)
                  (display "s for " port)
                  (display (if dscr dscr (quote expr)) port)
                  (get-output-string port))
                (quote tag)
                logger)
              (apply values args))
            (time-values expr)))))
  )
)
