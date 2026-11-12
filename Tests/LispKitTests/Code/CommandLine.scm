;;; CommandLine.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;      http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(
  "SRFI 37: args-fold"
  (((verbose) (operand "file1") (output "out.txt") (operand "file2"))
   ((verbose) (output "x") (output "y") (output "z"))
   ((number #f) (operand "5") (number "5") (number "7") (number #f))
   ((unknown #\q) (unknown "zzz") (operand "-v") (operand "a"))
   ((operand "-") (operand "a") (verbose))
   ()
   #t)
  (import (srfi 37))
  (define opts
    (list (option '(#\v "verbose") #f #f (lambda (o n a seeds) (cons (list 'verbose) seeds)))
          (option '(#\o "output") #t #f (lambda (o n a seeds) (cons (list 'output a) seeds)))
          (option '(#\n "number") #f #t (lambda (o n a seeds) (cons (list 'number a) seeds)))))
  (define (run args)
    (reverse
      (args-fold args opts
                 (lambda (o n a seeds) (cons (list 'unknown n) seeds))
                 (lambda (x seeds) (cons (list 'operand x) seeds))
                 '())))
  (list (run '("-v" "file1" "--output" "out.txt" "file2"))
        (run '("-vo" "x" "--output=y" "-oz"))
        (run '("-n" "5" "-n5" "--number=7" "--number"))
        (run '("-q" "--zzz" "--" "-v" "a"))
        (run '("-" "a" "-v"))
        (run '())
        (option? (car opts)))
)

(
  "Command-line parsing"
  (((() "main.c" #t 1 ("a" "b"))
    ((profile) "x" #t 2 ())
    (() "-p" #t 0 ())
    (() "f" #f 0 ())
    (() "-5" #f 0 ())
    "error"
    "error"
    "error"
    "error"
    "error"
    "error"
    #t))
  (import (lispkit cmdline))
  (define verbose #f)
  (define level 0)
  (define links '())
  (define help-shown #f)
  (define (parse argv)
    (set! verbose #f)
    (set! level 0)
    (set! links '())
    (parse-command-line "compile" argv
      `((once-each
          (("-v" "--verbose") ,(lambda (flag) (set! verbose #t)) ("Compile with verbose messages"))
          (("-p" "--profile") ,(lambda (flag) 'profile) ("Compile with profiling")))
        (once-any
          (("-o" "--optimize-1") ,(lambda (flag) (set! level 1)) ("Optimization level 1"))
          (("-O" "--optimize-2") ,(lambda (flag) (set! level 2)) ("Optimization level 2")))
        (multi
          (("-l" "--link-flags")
           ,(lambda (flag lf) (set! links (cons lf links)))
           ("Add a flag for the linker" "lf"))))
      (lambda (accum filename) (list accum filename verbose level (reverse links)))
      '("filename")
      (lambda (text) (set! help-shown text))
      (lambda (flag) (error "bad flag" flag))))
  (define (failing thunk) (guard (e (#t "error")) (thunk) "no error"))
  (list (list (parse '("-v" "-l" "a" "--link-flags" "b" "-o" "main.c"))
              (parse '("-vp" "-O" "x"))
              (parse '("-v" "--" "-p"))
              (parse '("--help" "f"))
              (parse #("-5"))
              (failing (lambda () (parse '("-v" "-v" "f"))))
              (failing (lambda () (parse '("-o" "-O" "f"))))
              (failing (lambda () (parse '("--bogus" "f"))))
              (failing (lambda () (parse '("-v"))))
              (failing (lambda () (parse '("-l"))))
              (failing (lambda () (parse '("a" "b"))))
              (string? help-shown)))
)
