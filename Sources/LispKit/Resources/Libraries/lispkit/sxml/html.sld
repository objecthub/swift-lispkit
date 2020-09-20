;;; LISPKIT SXML HTML
;;; 
;;; A permissive HTML parser supporting scalable streaming with a tree folding interface.
;;; This copies the interface of Oleg Kiselyov's SSAX parser and provides simple convenience
;;; utilities. It correctly handles all invalid HTML, inserting "virtual" starting and closing
;;; tags as needed to maintain the proper tree structure needed for the foldts down/up logic.
;;; A major goal of this parser is bug-for-bug compatibility with the way common web browsers
;;; parse HTML.
;;; 
;;; Copyright information:
;;;   Copyright (c) 2003-2014 Alex Shinn.  All rights reserved.
;;; 
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;   3. The name of the author may not be used to endorse or promote products
;;;      derived from this software without specific prior written permission.
;;; 
;;;   THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;;;   IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;;   IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;;   NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;;   THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; 
;;; Adaptation to LispKit
;;;   Copyright © 2020 Matthias Zenger. All rights reserved.

(define-library (lispkit sxml html)
  
  (export make-html-parser
          html->sxml
          html-strip)
  
  (import (lispkit base))
  
  (begin
    
    ;; TEXT PARSING UTILS
    
    (define (read-while pred . o)
      (let ((in (if (pair? o) (car o) (current-input-port))))
        (call-with-output-string
          (lambda (out)
            (let lp ()
              (let ((c (peek-char in)))
                (cond ((and (not (eof-object? c)) (pred c))
                        (write-char (read-char in) out)
                        (lp)))))))))
    
    (define (read-until pred . o)
      (let ((in (if (pair? o) (car o) (current-input-port))))
        (call-with-output-string
          (lambda (out)
            (let lp ()
              (let ((c (peek-char in)))
                (cond ((not (or (eof-object? c) (pred c)))
                        (write-char (read-char in) out)
                        (lp)))))))))
    
    ;; Generates a KMP reader that works on ports, returning the text read up until the search
    ;; string (or the entire port if the search string isn't found).  This is O(n) in the length
    ;; of the string returned, as opposed to the find-string-from-port? in SSAX which uses
    ;; backtracking for an O(nm) algorithm.  This is hard-coded to case-insensitively match,
    ;; since that's what we need for HTML. A more general utility would abstract the character
    ;; matching predicate and possibly provide a limit on the length of the string read.
    (define (make-string-reader/ci str)
      (let* ((len (string-length str))
             (vec (make-vector len 0)))
        (cond ((> len 0)
                (vector-set! vec 0 -1)
                (cond ((> len 1)
                        (vector-set! vec 1 0)))))
        (let lp ((i 2) (j 0))
          (cond
            ((< i len)
              (let ((c (string-ref str i)))
                (cond ((char-ci=? (string-ref str (- i 1)) (string-ref str j))
                        (vector-set! vec i (+ j 1))
                        (lp (+ i 1) (+ j 1)))
                      ((> j 0)
                        (lp i (vector-ref vec j)))
                      (else
                        (vector-set! vec i 0)
                        (lp (+ i 1) j)))))))
        (lambda o
          (let ((in (if (pair? o) (car o) (current-input-port))))
            (call-with-output-string
              (lambda (out)
                (let lp ((i 0))
                  (cond ((< i len)
                          (let ((c (peek-char in)))
                            (cond ((eof-object? c)
                                    (display (substring str 0 i) out))
                                  ((char-ci=? c (string-ref str i))
                                    (read-char in)
                                    (lp (+ i 1)))
                                  (else
                                    (let* ((i2 (vector-ref vec i))
                                           (i3 (if (= -1 i2) 0 i2)))
                                      (if (> i i3)
                                          (display (substring str 0 (- i i3)) out) #f)
                                      (if (= -1 i2)
                                          (write-char (read-char in) out) #f)
                                      (lp i3))))))))))))))
     
    (define (skip-whitespace x)
      (read-while char-whitespace? x))
    
    ;; HTML-SPECIFIC READERS
    
    (define (char-alphanumeric? c)
      (or (char-alphabetic? c) (char-numeric? c)))
    
    (define (char-hex-numeric? c)
      (or (char-numeric? c)
          (memv (char-downcase c) '(#\a #\b #\c #\d #\e #\f))))
    
    (define (read-entity in)
      (read-char in)
      (cond ((eqv? (peek-char in) #\#)
              (read-char in)
              (cond ((char-numeric? (peek-char in))
                      (let* ((str (read-while char-numeric? in))
                             (num (string->number str)))
                        (cond ((eqv? (peek-char in) #\;)
                               (read-char in)))
                        (cons 'entity num)))
                    ((memv (peek-char in) '(#\x #\X))
                      (read-char in)
                      (let* ((str (read-while char-hex-numeric? in))
                             (num (string->number str 16)))
                        (cond ((eqv? (peek-char in) #\;)
                               (read-char in)))
                        (cons 'entity num)))
                    (else
                      (cons 'text "&#"))))
            ((char-alphabetic? (peek-char in))
              (let ((name (read-while char-alphanumeric? in)))
                (cond ((eqv? (peek-char in) #\;)
                        (read-char in)))
                (cons 'entity name)))
            (else
              (cons 'text "&"))))
    
    (define (read-quoted in entities)
      (let ((terminator (read-char in)))
        (let lp ((res '()))
          (cond ((eqv? terminator (peek-char in))
                  (read-char in)  ; discard terminator
                  (reverse res))
                ((eqv? #\& (peek-char in))
                  (let ((x (read-entity in)))
                    (lp (cons (or (and (eq? 'entity (car x))
                                       (get-entity entities (cdr x)))
                                  (string-append "&" (cdr x)))
                              res))))
                (else
                  (lp (cons (read-until (lambda (c) (or (eqv? #\& c) (eqv? terminator c))) in)
                            res)))))))
    
    (define (read-pi in)
      (let ((tag (read-while char-alphanumeric? in)))
        (skip-whitespace in)
        (list
          (if (equal? tag "")
              #f
              (string->symbol (string-downcase tag)))
          (list->string
            (reverse
              (let loop ((res '()))
                (let ((c (peek-char in)))
                  (cond ((eof-object? c)
                          (read-char in)
                          res)
                        ((eqv? c #\?)
                          (read-char in)
                          (let loop2 ((res res))
                            (cond ((eof-object? (peek-char in))
                                    (cons #\? res))
                                  ((eqv? #\> (peek-char in))
                                    (read-char in)
                                    res)
                                  ((eqv? #\? (peek-char in))
                                    (read-char in)
                                    (loop2 (cons c res)))
                                  (else
                                    (loop (cons c res))))))
                        (else
                          (read-char in)
                          (loop (cons c res)))))))))))
    
    (define read-comment (make-string-reader/ci "-->"))
    
    (define (tag-char? c)
      (and (char? c)
           (or (char-alphanumeric? c) (memv c '(#\- #\+ #\* #\_ #\:)))))
    
    (define (read-attrs in entities)
      (let loop ((attrs '()))
        (skip-whitespace in)
        (let ((c (peek-char in)))
          (cond ((or (eof-object? c) (eqv? c #\>))
                  (read-char in)
                  (list #f (reverse attrs)))
                ((eqv? c #\/)
                  (read-char in)
                  (skip-whitespace in)
                  (cond
                   ((eqv? #\> (peek-char in))
                    (read-char in)
                    (list #t (reverse attrs)))
                   (else
                    (loop attrs))))
                ((eqv? c #\")
                  (read-char in)
                  (loop attrs))
                ((not (tag-char? c))
                  (list #f (reverse attrs)))
                (else
                  (let ((name (read-while tag-char? in)))
                    (if (string=? name "")
                        (loop attrs)
                        (let ((name (string->symbol (string-downcase name))))
                          (cond ((eqv? (peek-char in) #\=)
                                  (read-char in)
                                  (let ((value (if (memv (peek-char in) '(#\" #\'))
                                                   (apply string-append
                                                          (read-quoted in entities))
                                                   (read-until
                                                    (lambda (c)
                                                      (or (char-whitespace? c)
                                                          (memv c '(#\' #\" #\< #\>))))
                                                    in))))
                                    (loop (cons (list name value) attrs))))
                                (else
                                  (loop (cons (list name) attrs))))))))))))
    
    (define (read-start in entities)
      (let ((tag (string->symbol (string-downcase (read-while tag-char? in)))))
        (cons tag (read-attrs in entities))))
    
    (define (read-end in)
      (let ((tag (read-while tag-char? in)))
        (cond ((equal? tag "")
                (read-until (lambda (c) (eqv? c #\>)) in)
                (read-char in)
                #f)
              (else
                ;; discard closing attrs
                (read-attrs in '())
                (string->symbol (string-downcase tag))))))
    
    (define (read-decl in entities)
      (let loop ((res '()))
        (skip-whitespace in)
        (let ((c (peek-char in)))
          (cond ((eof-object? c)
                  (reverse res))
                ((eqv? c #\")
                  (loop (cons (read-quoted in entities) res)))
                ((eqv? c #\>)
                  (read-char in)
                  (reverse res))
                ((eqv? c #\<)
                  (read-char in)
                  (if (eqv? (peek-char in) #\!) (read-char in) #f)
                  (loop (cons (read-decl in entities) res)))
                ((tag-char? c)
                  (loop (cons (string->symbol (read-while tag-char? in)) res)))
                (else
                  (read-char in)
                  (loop res))))))
    
    ;; PARSER
    
    (define *default-entities* '(("amp" . "&")
                                 ("quot" . "\"")
                                 ("lt" . "<")
                                 ("gt" . ">")
                                 ("apos" . "'")
                                 ("nbsp" . " ")))
    
    (define (get-entity entities name)
      (cond ((number? name)
              (string (integer->char name)))
            ((string->number name)
              => (lambda (n) (string (integer->char n))))
            ((assoc name entities)
              => cdr)
            (else
              #f)))
    
    ;; span's and div's can be used at any level
    (define *tag-levels* '(html (head body) table (thead tbody) tr (th td) p (b i u s)))
    
    (define *unnestables* '(p li td tr))
    
    (define *bodyless* '(img hr br))
    
    (define *literals* '(script xmp))
    
    (define *terminators* '(plaintext))
    
    (define (tag-level tag-levels tag)
      (let lp ((ls tag-levels) (i 0))
        (if (null? ls)
            (+ i 1000)
            (if (if (pair? (car ls))
                    (memq tag (car ls))
                    (eq? tag (car ls)))
                i
                (lp (cdr ls) (+ i 1))))))
    
    (define read-cdata (make-string-reader/ci "]]>"))
    
    (define (read-html-token . o)
      (let ((in (if (pair? o) (car o) (current-input-port)))
            (entities (if (and (pair? o) (pair? (cdr o))) (cadr o) '())))
        (let ((c (peek-char in)))
          (if (eof-object? c)
              (cons 'eof c)
              (case c
                ((#\<)
                  (read-char in)
                  (case (peek-char in)
                    ((#\!)
                      (read-char in)
                      (cond ((eqv? #\[ (peek-char in))
                              (read-char in)
                              (let lp ((check '(#\C #\D #\A #\T #\A #\[))
                                       (acc '(#\[ #\! #\<)))
                                (cond ((null? check)
                                        (cons 'text (read-cdata in)))
                                      ((let ((c (peek-char in)))
                                         (and (not (eof-object? c)) (char-ci=? c (car check))))
                                         (lp (cdr check) (cons (read-char in) acc)))
                                      (else
                                        (cons 'text (list->string (reverse acc)))))))
                            ((and (eqv? #\- (peek-char in))
                                  (begin (read-char in)
                                         (eqv? #\- (peek-char in))))
                              (read-char in)
                              (cons 'comment (read-comment in)))
                            (else
                              (cons 'decl (read-decl in entities)))))
                    ((#\?)
                      (read-char in)
                      (cons 'process (read-pi in)))
                    ((#\/)
                      (read-char in)
                      (cons 'end (read-end in)))
                    (else
                      ;; start tags must immediately be followed by an
                      ;; alphabetic charater, or we just treat the < as text
                      (if (and (char? (peek-char in))
                               (char-alphabetic? (peek-char in)))
                          (let ((res (read-start in entities)))
                            (if (cadr res)
                                (cons 'start/end (cons (car res) (cddr res)))
                                (cons 'start (cons (car res) (cddr res)))))
                          (cons 'text "<")))))
                 ((#\&)
                   (read-entity in))
                 (else
                   (cons 'text
                         (read-until (lambda (c) (or (eqv? c #\<) (eqv? c #\&))) in))))))))
            
    (define (%key-ref ls key default)
      (cond ((memq key ls)
              => cadr)
            (else
              default)))
    
    ;;> \procedure{(make-html-parser . keys)}
    ;;>
    ;;> Returns a procedure of two arguments, and initial seed and an
    ;;> optional input port, which parses the HTML document from the port
    ;;> with the callbacks specified in the plist \var{keys} (using
    ;;> normal, quoted symbols, for portability and to avoid making this a
    ;;> macro).  The following callbacks are recognized:
    ;;>
    ;;> \itemlist[
    ;;> \item{
    ;;>   \scheme{(start tag attrs seed virtual?)} -
    ;;>     fdown in foldts, called when a start-tag is encountered.
    ;;>     \itemlist[
    ;;>       \item{\var{tag}:       tag name}
    ;;>       \item{\var{attrs}:     tag attributes as a alist}
    ;;>       \item{\var{seed}:      current seed value}
    ;;>       \item{\var{virtual?}:  #t iff this start tag was inserted to fix the HTML tree}]}
    ;;>
    ;;> \item{
    ;;>   \scheme{(end tag attrs parent-seed seed virtual?)} -
    ;;>     fup in foldts, called when an end-tag is encountered.
    ;;>     \itemlist[
    ;;>       \item{\var{tag}:         tag name}
    ;;>       \item{\var{attrs}:       tag attributes of the corresponding start tag}
    ;;>       \item{\var{parent-seed}: parent seed value (i.e. seed passed to the start tag)}
    ;;>       \item{\var{seed}:        current seed value}
    ;;>       \item{\var{virtual?}:    #t iff this end tag was inserted to fix the HTML tree}]}
    ;;>
    ;;> \item{
    ;;>   \scheme{(text text seed)} -
    ;;>     fhere in foldts, called when any text is encountered.  May be
    ;;>     called multiple times between a start and end tag, so you need
    ;;>     to string-append yourself if desired.
    ;;>     \itemlist[
    ;;>       \item{\var{text}:      entity-decoded text}
    ;;>       \item{\var{seed}:      current seed value}]}
    ;;>
    ;;> \item{
    ;;>   \scheme{(comment text seed)} -
    ;;>     fhere on comment data}
    ;;>
    ;;> \item{
    ;;>   \scheme{(decl name attrs seed)} -
    ;;>     fhere on declaration data}
    ;;>     
    ;;> \item{
    ;;>   \scheme{(process list seed)} -
    ;;>     fhere on process-instruction data}
    ;;>
    ;;> \item{
    ;;>   \scheme{(entity name-or-num seed)} -
    ;;>     convert an entity with a given name, or number via &#NNNN;
    ;;>     to a string}
    ;;> ]
    ;;>     
    ;;> In addition, entity-mappings may be overriden with the
    ;;> \var{entities}: keyword.
    ;;>
    ;;> Example: the parser for html-strip could be defined as:
    ;;>
    ;;> \codeblock{
    ;;>  (make-html-parser
    ;;>   'start: (lambda (tag attrs seed virtual?) seed)
    ;;>   'end:   (lambda (tag attrs parent-seed seed virtual?) seed)
    ;;>   'text:  (lambda (text seed) (display text)))}
    ;;>
    ;;> Also see the parser code for \scheme{html->sxml}.
    
    (define (make-html-parser . o)
      (let* ((start (%key-ref o 'start: (lambda (t a s v) s)))
             (end (%key-ref o 'end: (lambda (t a p s v) s)))
             (text (%key-ref o 'text: (lambda (t s) s)))
             (decl (%key-ref o 'decl: (lambda (t a s) s)))
             (process (%key-ref o 'process: (lambda (t s) s)))
             (comment (%key-ref o 'comment: (lambda (t s) s)))
             (entities (%key-ref o 'entities: *default-entities*))
             (tag-levels (%key-ref o 'tag-levels: *tag-levels*))
             (unnestables (%key-ref o 'unnestables: *unnestables*))
             (bodyless (%key-ref o 'bodyless: *bodyless*))
             (literals
              (map (lambda (x)
                     (cons x (make-string-reader/ci
                              (string-append "</" (symbol->string x) ">"))))
                   (%key-ref o 'literals: *literals*)))
             (terminators (%key-ref o 'terminators: *terminators*))
             (entity (%key-ref o 'entity:
                               (lambda (t s)
                                 (text (or (get-entity entities t)
                                           (string-append "&" t ";"))
                                       s)))))
        (define (entity->string sxml seed out)
          (if (pair? sxml)
              (if (eq? 'entity (car sxml))
                  (entity->string (entity (cdr sxml) seed) seed out)
                  (for-each (lambda (x) (entity->string x seed out)) sxml))
              (display sxml out)))
        (define (fix-attrs ls seed)
          (map
           (lambda (x)
             (cons (car x)
                   (if (pair? (cdr x))
                       (list
                        (call-with-output-string
                          (lambda (out) (entity->string (cadr x) seed out))))
                       (cdr x))))
           ls))
        (define (fix-decl ls seed)
          (map (lambda (x)
                 (if (pair? x)
                     (call-with-output-string
                       (lambda (out) (entity->string x seed out)))
                     x))
               ls))
        (lambda (seed . o)
          (let* ((src (if (pair? o) (car o) (current-input-port)))
                 (in (if (string? src) (open-input-string src) src)))
            (let lp ((tok (read-html-token in entities))
                     (seed seed)
                     (seeds '())
                     (tags '()))
              (case (car tok)
                ((eof)                      ; close all open tags
                 (let lp ((t tags) (s seeds) (seed seed))
                   (if (null? t)
                       seed
                       (lp (cdr t) (cdr s)
                           (end (caar t) (cadar t) (car s) seed 'eof)))))
                ((start/end)
                 (let* ((tag (cadr tok))
                        (rest (cons (fix-attrs (caddr tok) seed) (cdddr tok)))
                        (tok (cons tag rest)))
                   (lp `(end . ,tag)
                       (start tag (car rest) seed #f)
                       (cons seed seeds)
                       (cons tok tags))))
                ((start)
                 (let* ((tag (cadr tok))
                        (rest (cons (fix-attrs (caddr tok) seed) (cdddr tok)))
                        (tok (cons tag rest)))
                   (cond
                    ((memq tag terminators)
                     (lp `(text . ,(read-until (lambda (c) #f) in))
                         (start tag (car rest) seed #f)
                         (cons seed seeds)
                         (cons tok tags)))
                    ((assq tag literals)
                     => (lambda (lit)
                          (let ((body ((cdr lit) in))
                                (seed2 (start tag (car rest) seed #f)))
                            (lp `(end . ,tag)
                                (if (equal? "" body) seed2 (text body seed2))
                                (cons seed seeds)
                                (cons tok tags)))))
                    ((memq tag bodyless)
                     (lp `(end . ,tag)
                         (start tag (car rest) seed #f)
                         (cons seed seeds)
                         (cons tok tags)))
                    ((and (pair? tags) (eq? tag (caar tags))
                          (memq tag unnestables))
                     ;; <p> ... <p> implies siblings, not nesting
                     (let ((seed2
                            (end tag (cadar tags) (car seeds) seed 'sibling)))
                       (lp (read-html-token in entities)
                           (start tag (car rest) seed #f)
                           (cons seed2 (cdr seeds))
                           (cons tok (cdr tags)))))
                    (else
                     (lp (read-html-token in entities)
                         (start tag (car rest) seed #f)
                         (cons seed seeds)
                         (cons tok tags))))))
                ((end)
                 (cond
                  ((not (cdr tok)) ;; nameless closing tag
                   (lp (read-html-token in entities) seed seeds tags))
                  ((and (pair? tags) (eq? (cdr tok) (caar tags)))
                   (lp (read-html-token in entities)
                       (end (cdr tok) (fix-attrs (cadar tags) seed)
                            (car seeds) seed #f)
                       (cdr seeds)
                       (cdr tags)))
                  (else
                   (let ((this-level (tag-level tag-levels (cdr tok)))
                         (expected-level
                          (if (pair? tags)
                              (tag-level tag-levels (caar tags))
                              -1)))
                     (cond
                      ((< this-level expected-level)
                       ;; higher-level tag, forcefully close preceding tags
                       (lp tok
                           (end (caar tags) (fix-attrs (cadar tags) seed)
                                (car seeds) seed 'parent-closed)
                           (cdr seeds)
                           (cdr tags)))
                      ((and (= this-level expected-level) (pair? (cdr tags)))
                       ;; equal, interleave (close prec tag, close this,
                       ;; re-open prec)
                       ;; <b><i></b> => <b><i></i></b><i>
                       ;;                     ^^^^    ^^^
                       ;; XXXX handle backups > 1 here
                       (let* ((seed2 (end (caar tags) (cadar tags)
                                          (car seeds) seed 'interleave))
                              (seed3 (end (caadr tags) (cadadr tags)
                                          (cadr seeds) seed2 #f)))
                         (let ((tok2 (read-html-token in entities)))
                           (cond
                            ((and (eq? 'end (car tok2))
                                  (eq? (caar tags) (cdr tok2)))
                             ;; simple case where the closing tag
                             ;; immediately follows
                             (lp (read-html-token in entities) seed3
                                 (cddr seeds) (cddr tags)))
                            (else
                             (lp tok2
                                 (start (caar tags) (cadar tags) seed3
                                        'interleave)
                                 (cons seed3 (cddr seeds))
                                 (cons (car tags) (cddr tags))))))))
                      (else
                       ;; spurious end for a lower-level tag, add
                       ;; imaginary start
                       (let* ((seed2 (start (cdr tok) '() seed 'no-start))
                              (seed3 (end (cdr tok) '() seed seed2 #f)))
                         (lp (read-html-token in entities) seed3 seeds tags))))))))
                ((text)
                 (lp (read-html-token in entities) (text (cdr tok) seed) seeds tags))
                ((entity)
                 (lp (read-html-token in entities) (entity (cdr tok) seed) seeds tags))
                ((comment)
                 (lp (read-html-token in entities) (comment (cdr tok) seed) seeds tags))
                ((decl)
                 (lp (read-html-token in entities)
                     (decl (cadr tok) (fix-decl (cddr tok) seed) seed) seeds tags))
                ((process)
                 (lp (read-html-token in entities) (process (cdr tok) seed) seeds tags))
                (else
                 (error "invalid token: " tok))))))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; simple conversions
    
    ;;> \procedure{(html->sxml [port])}
    ;;> Returns the SXML representation of the document from \var{port},
    ;;> using the default parsing options.
    
    (define html->sxml
      (let ((parse
             (make-html-parser
              'start: (lambda (tag attrs seed virtual?) '())
              'end:   (lambda (tag attrs parent-seed seed virtual?)
                        `((,tag ,@(if (pair? attrs)
                                      `((@ ,@attrs) ,@(reverse seed))
                                      (reverse seed)))
                          ,@parent-seed))
              'decl:    (lambda (tag attrs seed) `((*DECL* ,tag ,@attrs) ,@seed))
              'process: (lambda (attrs seed) `((*PI* ,@attrs) ,@seed))
              'comment: (lambda (text seed) `((*COMMENT* ,text) ,@seed))
              'text:    (lambda (text seed) (cons text seed))
              )))
        (lambda o
          (cons '*TOP* (reverse (apply parse '() o))))))
    
    ;;> \procedure{(html-strip [port])}
    ;;> Returns a string representation of the document from \var{port}
    ;;> with all tags removed.  No whitespace reduction or other rendering
    ;;> is done.
    
    (define (html-strip . o)
      (call-with-output-string
       (lambda (out)
         (let ((parse
                (make-html-parser
                 'start: (lambda (tag attrs seed virtual?) seed)
                 'end:   (lambda (tag attrs parent-seed seed virtual?) seed)
                 'text:  (lambda (text seed) (display text out)))))
           (apply parse (cons #f #f) o)))))
  )    
)
