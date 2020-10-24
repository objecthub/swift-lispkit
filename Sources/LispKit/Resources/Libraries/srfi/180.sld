;;; SRFI 180
;;; JSON
;;; 
;;; This library implements a JavaScript Object Notation (JSON) parser and printer.
;;; It supports JSON that may be bigger than memory.
;;; 
;;; Copyright © 2020 Amirouche Boubekki. All rights reserved.
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
;;;   Copyright © 2020 Matthias Zenger. All rights reserved.

(define-library (srfi 180)

  (export json-number-of-character-limit
          json-nesting-depth-limit
          json-null?
          json-error?
          json-error-reason
          json-fold
          json-generator
          json-read
          json-lines-read
          json-sequence-read
          json-accumulator
          json-write)

  (import (lispkit base)
          (lispkit regexp)
          (srfi 145)
          (only (srfi 151) arithmetic-shift bitwise-ior))
  
  (begin
    (define (%read-error? x)
      (and (error-object? x) (memq (exception-kind x) '(user read read-incomplete)) #t))

    (define json-numbers (regexp "-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?"))

    (define (valid-number? str)
      (regexp-matches? json-numbers str))
  )
  
  (begin
    
    (define (pk . args)
      (write args)(newline)
      (car (reverse args)))
    
    (define json-number-of-character-limit (make-parameter +inf.0))
    
    (define json-nesting-depth-limit (make-parameter +inf.0))
    
    (define (json-null? obj)
      (eq? obj 'null))
    
    (define-record-type <json-error>
      (make-json-error reason)
      json-error?
      (reason json-error-reason))
    
    (define (json-whitespace? char)
      (case char
        ((#\x20 ; Space
          #\x09 ; Horizontal tab
          #\x0A ; Line feed or New line
          #\x0D
          #\x1E ; Record Separator
          )
         #t)
        (else #f)))
    
    (define (expect value other)
      (when (eof-object? value)
        (raise (make-json-error "Unexpected end-of-file.")))
      (assume (char? value))
      (assume (char? other))
      (unless (char=? value other)
        (raise (make-json-error "Unexpected character."))))
    
    (define (port->generator port)
      (let ((count 0)
            (limit (json-number-of-character-limit)))
        (lambda ()
          (let ((out (guard (ex ((%read-error? ex) (raise (make-json-error "Read error!"))))
                       (read-char port))))
            (if (= count limit)
                (raise (make-json-error "Maximum number of character reached."))
                (begin
                  (set! count (+ count 1))
                  out))))))
    
    (define (gcons head generator)
      ;; returns a generator that will yield, HEAD the first time, and
      ;; after than, it will yield items from GENERATOR.
      (let ((head? #t))
        (lambda ()
          (if head?
              (begin (set! head? #f) head)
              (generator)))))
    
    (define (%json-tokens generator)
    
      (define (maybe-ignore-whitespace generator)
        (let loop ((char (generator)))
          (if (json-whitespace? char)
              (loop (generator))
              char)))
    
      (define (expect-null generator)
        (expect (generator) #\u)
        (expect (generator) #\l)
        (expect (generator) #\l))
    
      (define (expect-true generator)
        (expect (generator) #\r)
        (expect (generator) #\u)
        (expect (generator) #\e))
    
      (define (expect-false generator)
        (expect (generator) #\a)
        (expect (generator) #\l)
        (expect (generator) #\s)
        (expect (generator) #\e))
    
      (define (maybe-char generator)
        (let ((char (generator)))
          (when (eof-object? char)
            (raise (make-json-error "Unexpected end-of-file.")))
          (when (char=? char #\")
            (raise (make-json-error "Unexpected end of string.")))
          char))
    
      (define (read-unicode-escape generator)
        (let* ((one (maybe-char generator))
               (two (maybe-char generator))
               (three (maybe-char generator))
               (four (maybe-char generator)))
          (let ((out (string->number (list->string (list one two three four)) 16)))
            (if out
                out
                (raise (make-json-error "Invalid code point."))))))
    
      (define ash arithmetic-shift)
    
      (define (read-json-string generator)
        (let loop ((char (generator))
                   (out '()))
    
          (when (eof-object? char)
            (raise (make-json-error "Unexpected end of file.")))
    
          (when (or (char=? char #\x00)
                    (char=? char #\newline)
                    (char=? char #\tab))
            (raise (make-json-error "Unescaped control char.")))
    
          ;; XXX: Here be dragons.
          (if (char=? char #\\)
              (begin
                (let loop-unescape ((char (generator))
                                    (chars-unescaped '()))
                  (case char
                    ((#\" #\\ #\/) (loop (generator)
                                         (cons char (append chars-unescaped
                                                            out))))
                    ((#\b) (loop (generator) (cons #\backspace
                                                        (append chars-unescaped
                                                                out))))
                    ((#\f) (loop (generator) (cons #\x0C
                                                        (append chars-unescaped
                                                                out))))
                    ((#\n) (loop (generator) (cons #\newline
                                                        (append chars-unescaped
                                                                out))))
                    ((#\r) (loop (generator) (cons #\x0D
                                                        (append chars-unescaped
                                                                out))))
                    ((#\t) (loop (generator) (cons #\tab
                                                        (append chars-unescaped
                                                                out))))
                    ((#\u) (let loop-unicode ((code1 (read-unicode-escape generator))
                                              (chars chars-unescaped))
                             (let ((next-char (generator)))
                               (if (and (<= #xd800 code1 #xdbff)
                                        (char=? next-char #\\))
                                   (if (char=? (generator) #\u)
                                       (let ((code2 (read-unicode-escape generator)))
                                         (if (<= #xdc00 code2 #xdfff)
                                             (let ((integer
                                                    (+ #x10000 (bitwise-ior
                                                                (ash (- code1 #xd800) 10)
                                                                (- code2 #xdc00)))))
                                               ;; full escape of unicode is parsed...
                                               (loop (generator)
                                                     (cons (integer->char integer)
                                                           (append chars
                                                                   out))))
                                             ;; This is another unicode char
                                             (loop-unicode (read-unicode-escape generator)
                                                           (cons (integer->char code1) chars))))
                                       ;; The escaped unicode char is
                                       ;; parsed, need to parse another
                                       ;; escape that is not a unicode
                                       ;; escape sequence
                                       (loop-unescape char (cons (integer->char code1)
                                                                 chars)))
                                 ;; This is not a big-ish unicode char and
                                 ;; the next thing is some other char.
                                 (loop next-char
                                       (cons (integer->char code1) (append chars out)))))))
                    (else (raise (make-json-error "Unexpected escaped sequence."))))))
              (cond
               ((char=? char #\")
                (list->string (reverse out)))
               (else
                (loop (generator) (cons char out)))))))
    
      (define (maybe-read-number generator)
        ;; accumulate chars until a control char or whitespace is reached,
        ;; validate that it is JSON number, then intrepret it as Scheme
        ;; number using string->number
        (let loop ((char (generator))
                   (out '()))
          (if (or (eof-object? char)
                  (json-whitespace? char)
                  (char=? char #\,)
                  (char=? char #\])
                  (char=? char #\}))
              (let ((string (list->string (reverse out))))
                (if (valid-number? string)
                    (let ((number (string->number string)))
                      (if number
                          (values number char)
                          (raise (make-json-error "Invalid number."))))
                    (raise (make-json-error "Invalid number."))))
              (loop (generator) (cons char out)))))
    
      ;; gist
      (assume (procedure? generator))
    
      (let ((char (generator)))
        (if (eof-object? char)
            eof-object  ;; return an empty generator
            (begin
    
              (unless (char=? char #\xFEFF)
                ;; if it is not a UTF-8 BOM, put back the char in front of
                ;; the generator
                (set! generator (gcons char generator)))
    
              (lambda ()
    
                (define char (maybe-ignore-whitespace generator))
    
                (if (eof-object? char)
                    char ;; return that eof-object
                    (case char
                      ((#\n) (expect-null generator) 'null)
                      ((#\t) (expect-true generator) #t)
                      ((#\f) (expect-false generator) #f)
                      ((#\:) 'colon)
                      ((#\,) 'comma)
                      ((#\[) 'array-start)
                      ((#\]) 'array-end)
                      ((#\{) 'object-start)
                      ((#\}) 'object-end)
                      ((#\") (read-json-string generator))
                      (else
                       (call-with-values (lambda () (maybe-read-number (gcons char generator)))
                         (lambda (number next)
                           (set! generator (gcons next generator))
                           number))))))))))
    
    (define json-tokens
      (case-lambda
       (() (json-tokens (current-input-port)))
       ((port-or-generator)
        (cond
         ((procedure? port-or-generator)
          (%json-tokens port-or-generator))
         ((and (textual-port? port-or-generator) (input-port? port-or-generator))
           (%json-generator (port->generator port-or-generator)))
         (else (error 'json "json-tokens error, argument is not valid" port-or-generator))))))
    
    (define (%json-generator tokens)
    
      (define limit (json-nesting-depth-limit))
      (define count 0)
    
      (define (handle-limit!)
        (if (= count limit)
            (raise (make-json-error "Maximum JSON nesting depth reached"))
            (set! count (+ count 1))))
    
      (define (array-maybe-continue tokens k)
        (lambda ()
          (let ((token (tokens)))
            (case token
              ((comma) (start tokens (array-maybe-continue tokens k)))
              ((array-end) (values 'array-end k))
              (else (raise (make-json-error "Invalid array, expected comma or array close.")))))))
    
      (define (array-start tokens k)
        (lambda ()
          (handle-limit!)
          (let ((token (tokens)))
            (if (eq? token 'array-end)
                (values 'array-end k)
                (start (gcons token tokens) (array-maybe-continue tokens k))))))
    
      (define (object-maybe-continue tokens k)
        (lambda ()
          (let ((token (tokens)))
            (case token
              ((object-end) (values 'object-end k))
              ((comma) (let ((token (tokens)))
                         (unless (string? token)
                           (raise (make-json-error "Invalid object, expected an object key")))
                         (values token
                                 (object-colon tokens k))))
              (else (raise (make-json-error "Invalid object, expected comma or object close.")))))))
    
      (define (object-colon tokens k)
        (lambda ()
          (let ((token (tokens)))
            (if (eq? token 'colon)
                (let ((token (tokens)))
                  (if (eof-object? token)
                      (raise (make-json-error "Invalid object, expected object value."))
                      (start (gcons token tokens) (object-maybe-continue tokens k))))
                (raise (make-json-error "Invalid object, expected colon."))))))
    
      (define (object-start tokens k)
        (lambda ()
          (handle-limit!)
          (let ((token (tokens)))
            (cond
             ((eq? token 'object-end) (values 'object-end k))
             ((string? token)
              (values token
                      (object-colon tokens k)))
             (else (raise (make-json-error "Invalid object, expected object key or object close.")))))))
    
      (define (start tokens k)
        (let ((token (tokens)))
          (if (eof-object? token)
              (values token k)
              (cond
               ((or (json-null? token)
                    (number? token)
                    (string? token)
                    (boolean? token))
                (values token k))
               ((eq? token 'array-start)
                (values 'array-start (array-start tokens k)))
               ((eq? token 'object-start)
                (values 'object-start (object-start tokens k)))
               (else (raise (make-json-error "Is it JSON text?!")))))))
    
      (define (end-of-top-level-value)
        ;; json-generator returns a generator that reads one top-level
        ;; json. If there is more than one top-level json value in the
        ;; generator separated with space as it is the case of json-lines,
        ;; you need to call json-generator with the same port or
        ;; generator.
        (values (eof-object) #f))
    
      (define (make-trampoline-generator tokens)
        (let ((continuation (lambda () (start tokens end-of-top-level-value))))
          (lambda ()
            (when continuation
              (call-with-values continuation
                (lambda (event new-continuation)
                  (set! continuation new-continuation)
                  event))))))
    
      ;; gist
    
      (assume (procedure? tokens))
    
      (make-trampoline-generator tokens))
    
    (define json-generator-error
      "Argument does not look like a generator and is not a textual input port.")
    
    (define json-generator
      (case-lambda
        (() (json-generator (current-input-port)))
        ((port)
         (%json-generator (json-tokens (port->generator port))))))
    
    ;; XXX: procedure foldts is not used as-is. It was copied here for
    ;; documentation purpose (public domain, by Oleg Kiselyov).
    (define (foldts fdown fup fhere seed tree)
      ;; - fhere is applied to the leafs of the tree
      ;;
      ;; - fdown is invoked when a non-leaf node is entered before any of
      ;; the node's children are visited. fdown action has to generate a
      ;; seed to be passed to the first visited child of the node.
      ;;
      ;; - fup is invoked after all the children of a node have been
      ;; seen. The first argument is the local state at the moment the
      ;; traversal process enters the branch rooted at the current node. The
      ;; second argument is the result of visiting all child branches.  The
      ;; action of fup isto produce a seed that is taken to be the state of
      ;; the traversal after the process leave the currents the current
      ;; branch.
      (cond
        ((null? tree) seed)
        ((not (pair? tree))      ; An atom
          (fhere seed tree))
        (else
          (let loop ((kid-seed (fdown seed tree))
                     (kids (cdr tree)))
            (if (null? kids)
                (fup seed kid-seed tree)
                (loop (foldts fdown fup fhere kid-seed (car kids))
                      (cdr kids)))))))
    
    (define (%json-fold proc array-start array-end object-start object-end seed port-or-generator)
    
      ;; json-fold is inspired from the above foldts definition, unlike
      ;; the above definition, it is continuation-passing-style.  fhere is
      ;; renamed PROC.  Unlike foldts, json-fold will call (proc obj seed)
      ;; everytime a JSON value or complete structure is read from the
      ;; EVENTS generator, where OBJ will be: a) In the case of
      ;; structures, the the result of the recursive call or b) a JSON
      ;; value.
    
      ;; json-fold will terminates in three cases:
      ;;
      ;; - eof-object was generated, return the seed.
      ;;
      ;; - event-type 'array-end is generated, if EVENTS is returned by
      ;; json-generator, it means a complete array was read.
      ;;
      ;; - event-type 'object-end is generated, similarly, if EVENTS is
      ;; returned by json-generator, it means complete array was
      ;; read.
      ;;
      ;; IF EVENTS does not follow the json-generator protocol, the
      ;; behavior is unspecified.
    
      (define events (json-generator port-or-generator))
    
      (define (ruse seed k)
        (lambda ()
          (let loop ((seed seed))
            (let ((event (events)))
              (if (eof-object? event)
                  (begin (k seed) #f)
                  (case event
                    ;; termination cases
                    ((array-end) (k seed))
                    ((object-end) (k seed))
                    ;; recursion
                    ((array-start) (ruse (array-start seed)
                                         (lambda (out) (loop (proc (array-end out) seed)))))
                    ((object-start) (ruse (object-start seed)
                                          (lambda (out) (loop (proc (object-end out) seed)))))
                    (else (loop (proc event seed)))))))))
    
      (define (make-trampoline-fold k)
        (let ((thunk (ruse seed k)))
          (let loop ((thunk thunk))
            (when thunk
              (loop (thunk))))))
    
      (define %unset '(unset))
    
      (let ((out %unset))
        (define (escape out*)
          (set! out out*)
          #f)
        (make-trampoline-fold escape)
        (if (eq? out %unset)
            (error 'json "Is this JSON text")
            out)))
    
    (define json-fold
      (case-lambda
        ((proc array-start array-end object-start object-end seed)
          (json-fold proc array-start array-end object-start object-end seed (current-input-port)))
        ((proc array-start array-end object-start object-end seed port-or-generator)
          (%json-fold proc array-start array-end object-start object-end seed port-or-generator))))
    
    (define (%json-read port-or-generator)
    
      (define %root '(root))
    
      (define (array-start seed)
        ;; array will be read as a list, then converted into a vector in
        ;; array-end.
        '())
    
      (define (array-end items)
        (list->vector (reverse items)))
    
      (define (object-start seed)
        ;; object will be read as a property list, then converted into an
        ;; alist in object-end.
        '())
    
      (define (plist->alist plist)
        ;; PLIST is a list of even items, otherwise json-generator
        ;; would have raised a json-error.
        (let loop ((plist plist)
                   (out '()))
          (if (null? plist)
              out
              (loop (cddr plist) (cons (cons (string->symbol (cadr plist)) (car plist)) out)))))
    
      (define object-end plist->alist)
    
      (define (proc obj seed)
        ;; proc is called when a JSON value or structure was completly
        ;; read.  The parse result is passed as OBJ.  In the case where
        ;; what is parsed is a JSON simple json value then OBJ is simply
        ;; the token that is read that can be 'null, a number or a string.
        ;; In the case where what is parsed is a JSON structure, OBJ is
        ;; what is returned by OBJECT-END or ARRAY-END.
        (if (eq? seed %root)
         ;; It is toplevel, a complete JSON value or structure was read,
         ;; return it.
         obj
         ;; This is not toplevel, hence json-fold is called recursivly,
         ;; to parse an array or object.  Both ARRAY-START and
         ;; OBJECT-START return an empty list as a seed to serve as an
         ;; accumulator.  Both OBJECT-END and ARRAY-END expect a list
         ;; as argument.
         (cons obj seed)))
    
      (let ((out (json-fold proc
                            array-start
                            array-end
                            object-start
                            object-end
                            %root
                            port-or-generator)))
        ;; if out is the root object, then the port or generator is empty.
        (if (eq? out %root)
            (eof-object)
            out)))
    
    (define json-read
      (case-lambda
        (() (json-read (current-input-port)))
        ((port-or-generator) (%json-read port-or-generator))))
    
    ;; json-lines-read
    
    (define json-lines-read
      (case-lambda
        (() (json-lines-read (current-input-port)))
        ((port-or-generator)
         (lambda ()
           (json-read port-or-generator)))))
    
    ;; json-sequence-read
    
    (define json-sequence-read
      (case-lambda
        (() (json-sequence-read (current-input-port)))
        ((port-or-generator)
         (lambda ()
           (let loop ()
             (guard (ex ((json-error? ex) (loop)))
               (json-read port-or-generator)))))))
    
    ;; write procedures
    
    (define (json-accumulator accumulator)
    
      (define (write-json-char char accumulator)
        (case char
          ((#\x00) (accumulator "\\u0000"))
          ((#\") (accumulator "\\\""))
          ((#\\) (accumulator "\\\\"))
          ((#\/) (accumulator "\\/"))
          ((#\return) (accumulator "\\r"))
          ((#\newline) (accumulator "\\n"))
          ((#\tab) (accumulator "\\t"))
          ((#\backspace) (accumulator "\\b"))
          ((#\x0c) (accumulator "\\f"))
          ((#\x0d) (accumulator "\\r"))
          (else (accumulator char))))
    
      (define (write-json-string string accumulator)
        (accumulator #\")
        (string-for-each
         (lambda (char) (write-json-char char accumulator))
         string)
        (accumulator #\"))
    
      (define (write-json-value obj accumulator)
        (cond
         ((eq? obj 'null) (accumulator "null"))
         ((boolean? obj) (if obj
                             (accumulator "true")
                             (accumulator "false")))
         ((string? obj) (write-json-string obj accumulator))
         ((number? obj) (accumulator (number->string obj)))
         (else (raise (make-json-error "Invalid json value.")))))
    
      (define (raise-invalid-event event)
        (raise event))
      ;;(raise (make-json-error "json-accumulator: invalid event.")))
    
      (define (object-start k)
        (lambda (accumulator event)
          (accumulator #\{)
          (case (car event)
            ((json-value)
             (let ((key (cdr event)))
               (unless (symbol? key) (raise-invalid-event event))
               (write-json-string (symbol->string key) accumulator)
               (object-value k)))
            ((json-structure)
             (case (cdr event)
               ((object-end)
                (accumulator #\})
                k)
               (else (raise-invalid-event event))))
            (else (raise-invalid-event event)))))
    
      (define (object-value k)
        (lambda (accumulator event)
          (accumulator #\:)
          (case (car event)
            ((json-value)
             (write-json-value (cdr event) accumulator)
             (object-maybe-continue k))
            ((json-structure)
             (case (cdr event)
               ((array-start)
                (array-start (object-maybe-continue k)))
               ((object-start)
                (object-start (object-maybe-continue k)))
               (else (raise-invalid-event event))))
            (else (raise-invalid-event event)))))
    
      (define (object-maybe-continue k)
        (lambda (accumulator event)
          (case (car event)
            ((json-value)
             (accumulator #\,)
             (let ((key (cdr event)))
               (unless (symbol? key) (raise-invalid-event event))
               (write-json-value (symbol->string key) accumulator)
               (object-value k)))
            ((json-structure)
             (case (cdr event)
               ((object-end)
                (accumulator #\})
                k)
               (else (raise-invalid-event event))))
            (else (raise-invalid-event event)))))
    
      (define (array-start k)
        (lambda (accumulator event)
          (accumulator #\[)
          (case (car event)
            ((json-value)
             (write-json-value (cdr event) accumulator)
             (array-maybe-continue k))
            ((json-structure)
             (case (cdr event)
               ((array-end)
                (accumulator #\])
                k)
               ((array-start) (array-start (array-maybe-continue k)))
               ((object-start) (object-start (array-maybe-continue k)))
               (else (raise-invalid-event event))))
            (else (raise-invalid-event event)))))
    
      (define (array-maybe-continue k)
        (lambda (accumulator event)
          (case (car event)
            ((json-value)
             (accumulator #\,)
             (write-json-value (cdr event) accumulator)
             (array-maybe-continue k))
            ((json-structure)
             (case (cdr event)
               ((array-end)
                (accumulator #\])
                k)
               ((array-start)
                (accumulator #\,)
                (array-start (array-maybe-continue k)))
               ((object-start)
                (accumulator #\,)
                (object-start (array-maybe-continue k)))
               (else (raise-invalid-event event))))
            (else (raise-invalid-event event)))))
    
      (define (start accumulator event)
        (case (car event)
          ((json-value)
           (write-json-value (cdr event) accumulator)
           raise-invalid-event)
          ((json-structure)
           (case (cdr event)
             ((array-start)
              (array-start raise-invalid-event))
             ((object-start)
              (object-start raise-invalid-event))
             (else (raise-invalid-event event))))
          (else (raise-invalid-event event))))
    
      (assume (procedure? accumulator)
              "ACCUMULATOR does look like a valid accumulator.")
    
      (let ((k start))
        (lambda (event)
          (set! k (k accumulator event)))))
    
    (define (%json-write obj accumulator)
    
      (define (void)
        (if #f #f))
    
      (define (raise-unless-valid? obj)
        (cond
         ((null? obj) (void))
         ((eq? obj 'null) (void))
         ((boolean? obj) (void))
         ((string? obj) (void))
         ((and (number? obj)
               (not (infinite? obj))
               (not (nan? obj))
               (real? obj)
               (or (and (exact? obj) (= (denominator obj) 1))
                   (inexact? obj)))
          (void))
         ((vector? obj)
          (vector-for-each (lambda (obj) (raise-unless-valid? obj)) obj))
         ;; XXX: use pair? then recursively check the tail.
         ((pair? obj)
          (for-each (lambda (obj)
                      (unless (pair? obj)
                        (raise (make-json-error "Unexpected object, not a pair.")))
                      (unless (symbol? (car obj))
                        (raise (make-json-error "Unexpected object, not a symbol key.")))
                      (raise-unless-valid? (cdr obj)))
                    obj))
         (else (raise (make-json-error "Unexpected object")))))
    
      (define (write obj accumulator)
        (cond
         ((or (eq? obj 'null)
              (boolean? obj)
              (string? obj)
              (symbol? obj)
              (number? obj))
          (accumulator (cons 'json-value obj)))
         ((vector? obj)
          (accumulator '(json-structure . array-start))
          (vector-for-each (lambda (obj) (write obj accumulator)) obj)
          (accumulator '(json-structure . array-end)))
         ((null? obj)
          (accumulator '(json-structure . object-start))
          (accumulator '(json-structure . object-end)))
         ((pair? obj)
          (accumulator '(json-structure . object-start))
          (for-each (lambda (pair)
                      (write (car pair) accumulator)
                      (write (cdr pair) accumulator))
                    obj)
          (accumulator '(json-structure . object-end)))
         (else (error "Unexpected error!"))))
    
      (assume (procedure? accumulator))
      (raise-unless-valid? obj)
      (write obj (json-accumulator accumulator)))
    
    (define (port->accumulator port)
      (lambda (char-or-string)
        (cond
         ((char? char-or-string) (write-char char-or-string port))
         ((string? char-or-string) (write-string char-or-string port))
         (else (raise (make-json-error "Not a char or string"))))))
    
    (define json-write
      (case-lambda
        ((obj) (json-write obj (current-output-port)))
        ((obj port-or-accumulator)
         (assume (or (procedure? port-or-accumulator)
                      (and (textual-port? port-or-accumulator)
                           (output-port? port-or-accumulator))))
         (if (procedure? port-or-accumulator)
             (%json-write obj port-or-accumulator)
             (%json-write obj (port->accumulator port-or-accumulator)))))) 
  )  
)
