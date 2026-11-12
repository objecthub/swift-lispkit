;;; SRFI 37
;;; ARGS-FOLD: a program argument processor
;;;
;;; Program arguments are the primary interface to many programs, so processing
;;; arguments is a common programming task. There are many common (often conflicting)
;;; ways to take care of this task, so a custom processor is often necessary; however,
;;; many programmers would welcome a convenient interface supporting common guidelines.
;;; 
;;; POSIX provides several guidelines for the specification of program options,
;;; option-arguments, and operands. It also notes historical exceptions to these
;;; guidelines. The GNU C Library Reference Manual describes long option extensions
;;; to the POSIX guidelines.
;;; 
;;; This SRFI supports creating programs following the guidelines mentioned above by
;;; 
;;;   - parsing short-options and long-options in any order and possibly repeated,
;;;   - option-arguments (possibly required or forbidden),
;;;   - operands (non-option argument strings), and
;;;   - recognizing option processing termination.
;;; 
;;; It parses argument strings according to the following rules:
;;; 
;;;   - Each short-option name is a single character.
;;;   - One or more short-options are accepted in a single argument string when
;;;     grouped behind one - delimiter character. Examples: -a -bcd
;;;   - A short-option-argument is accepted in a separate argument string immediately
;;;    following a short-option. Examples: -a foo -bcd bar
;;;   - Also, a short-option-argument is accepted immediately following the last
;;;     short-option in a single argument string. Examples: -afoo -bcdbar
;;;   - Each long-option name is one or more characters.
;;;   - One long-option is accepted in a single argument string when preceded by two
;;;     delimiter characters. Example: --help
;;;   - In a single argument string, a long-option followed by one = delimiter
;;;     character followed by a long-option-argument is accepted. Example: --speed=fast
;;;   - The first -- argument string is accepted as a delimiter indicating the end of
;;;     options. It is not treated as an option or an operand. Any argument strings
;;;     following this delimiter are treated as operands, even if they begin with the
;;;     - character.
;;;   - All other argument strings are treated as operands. This includes argument
;;;     strings consisting of a single - character.
;;;   - Operands may be intermingled with options in any order.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use
;;; this file except in compliance with the License. You may obtain a copy of the
;;; License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed
;;; under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
;;; CONDITIONS OF ANY KIND, either express or implied. See the License for the specific
;;; language governing permissions and limitations under the License.

(define-library (srfi 37)

  (export option
          option?
          option-names
          option-required-arg?
          option-optional-arg?
          option-processor
          args-fold)

  (import (lispkit base))

  (begin

    (define-record-type <option>
      (option names required-arg? optional-arg? processor)
      option?
      (names option-names)
      (required-arg? option-required-arg?)
      (optional-arg? option-optional-arg?)
      (processor option-processor))

    ;; Finds the first option with the given name, which is either a character (for short
    ;; options) or a string (for long options).
    (define (find-option options name)
      (let loop ((opts options))
        (cond ((null? opts) #f)
              ((member name (option-names (car opts))) (car opts))
              (else (loop (cdr opts))))))

    ;; Returns the index of the first occurrence of `ch` in `str` at or after `start`;
    ;; `#f` if there is no such character.
    (define (index-of-char str ch start)
      (let ((len (string-length str)))
        (let loop ((i start))
          (cond ((>= i len) #f)
                ((char=? (string-ref str i) ch) i)
                (else (loop (+ i 1)))))))

    (define (args-fold args options unrecognized-proc operand-proc . seeds)

      ;; Applies `proc` to the given arguments and returns all results as a list
      (define (call proc . vals)
        (call-with-values (lambda () (apply proc vals)) list))

      ;; Invokes the processor of the option with the given name; returns the new seeds
      (define (process name arg seeds)
        (let ((opt (find-option options name)))
          (if opt
              (call (lambda (s) (apply (option-processor opt) opt name arg s)) seeds)
              (call (lambda (s)
                      (apply unrecognized-proc
                             (option (list name) #f #f unrecognized-proc)
                             name arg s))
                    seeds))))

      ;; Passes all remaining arguments to `operand-proc`
      (define (operands args seeds)
        (if (null? args)
            (apply values seeds)
            (operands (cdr args)
                      (call (lambda (s) (apply operand-proc (car args) s)) seeds))))

      (let loop ((args args) (seeds seeds))
        (if (null? args)
            (apply values seeds)
            (let* ((arg (car args))
                   (len (string-length arg)))
              (cond
                ;; "--" terminates option processing
                ((string=? arg "--")
                  (operands (cdr args) seeds))
                ;; Long option, possibly with "=" followed by an argument
                ((and (> len 2) (char=? (string-ref arg 0) #\-) (char=? (string-ref arg 1) #\-))
                  (let ((eq (index-of-char arg #\= 2)))
                    (if eq
                        (loop (cdr args)
                              (process (substring arg 2 eq) (substring arg (+ eq 1) len) seeds))
                        (let* ((name (substring arg 2 len))
                               (opt (find-option options name)))
                          (if (and opt (option-required-arg? opt) (pair? (cdr args)))
                              (loop (cddr args) (process name (cadr args) seeds))
                              (loop (cdr args) (process name #f seeds)))))))
                ;; One or more short options
                ((and (> len 1) (char=? (string-ref arg 0) #\-))
                  (let cluster ((i 1) (rest (cdr args)) (seeds seeds))
                    (if (>= i len)
                        (loop rest seeds)
                        (let* ((ch (string-ref arg i))
                               (opt (find-option options ch)))
                          (cond ((and opt (or (option-required-arg? opt) (option-optional-arg? opt)))
                                  (cond ((< (+ i 1) len)
                                          (loop rest (process ch (substring arg (+ i 1) len) seeds)))
                                        ((and (option-required-arg? opt) (pair? rest))
                                          (loop (cdr rest) (process ch (car rest) seeds)))
                                        (else
                                          (loop rest (process ch #f seeds)))))
                                (else
                                  (cluster (+ i 1) rest (process ch #f seeds))))))))
                ;; Operand
                (else
                  (loop (cdr args)
                        (call (lambda (s) (apply operand-proc arg s)) seeds))))))))
  )
)
