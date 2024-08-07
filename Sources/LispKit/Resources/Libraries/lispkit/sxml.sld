;;; LISPKIT SXML
;;; 
;;; Library providing utility procedures to convert valid, expanded SXML to
;;; XML, HTML or plain text. The library supports all the major LispKit objects
;;; that can be turned into HTML or plain text.
;;;
;;; Copyright information:
;;;   Copyright © Alex Shinn.  All rights reserved.
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
;;;   Copyright © 2020-2024 Matthias Zenger. All rights reserved.

(define-library (lispkit sxml)
  
  (export display-sxml
          display-sxml-text
          sxml->xml
          sxml->html
          sxml->text
          sxml-strip
          html-escape
          html-tag->string)
  
  (import (lispkit base)
          (lispkit styled-text)
          (lispkit date-time)
          (lispkit markdown))
  
  (begin
    
    (define (display-to-string x)
      (cond
        ((not x)
          "")
        ((string? x)
          x)
        ((char? x)
          (string x))
        ((symbol? x)
          (symbol->string x))
        ((number? x)
          (number->string x))
        ((styled-text? x)
          (styled-text-string x))
        ((date-time? x)
          (date-time->string x))
        ((or (markdown-inline? x) (markdown-text? x))
          (text->string x))
        ((vector? x)
          (vector->string x))
        (else
          (error "don't know how to display as sxml" x))))
    
    (define (html-display-escaped-attr str . o)
      (let ((start 0)
            (end (string-length str))
            (out (if (pair? o) (car o) (current-output-port))))
        (let lp ((from start) (to start))
          (if (>= to end)
              (display (substring str from to) out)
              (let ((c (string-ref str to)))
                (cond
                 ((eq? c #\<)
                  (display (substring str from to) out)
                  (display "&lt;" out)
                  (lp (+ to 1) (+ to 1)))
                 ((eq? c #\&)
                  (display (substring str from to) out)
                  (display "&amp;" out)
                  (lp (+ to 1) (+ to 1)))
                 ((eq? c #\")
                  (display (substring str from to) out)
                  (display "&quot;" out)
                  (lp (+ to 1) (+ to 1)))
                 (else
                  (lp from (+ to 1)))))))))
    
    (define (html-escape-attr str)
      (call-with-output-string
        (lambda (out) (html-display-escaped-attr (display-to-string str) out))))
    
    (define (html-attr->string attr)
      (if (cdr attr)
          (let ((val (if (pair? (cdr attr)) (cadr attr) (cdr attr))))
            (string-append (symbol->string (car attr)) "=\"" (html-escape-attr val) "\""))
          (symbol->string (car attr))))
    
    (define (html-tag->string tag attrs)
      (let lp ((ls attrs) (res (list (symbol->string tag) "<")))
        (if (null? ls)
            (apply string-append (reverse (cons ">" res)))
            (lp (cdr ls) (cons (html-attr->string (car ls)) (cons " " res))))))
    
    (define (html-display-escaped-string x . o)
      (display (string-encode-named-chars (display-to-string x) #t)
               (if (pair? o) (car o) (current-output-port))))
    
    (define (html-escape str)
      (call-with-output-string
        (lambda (out) (html-display-escaped-string str out))))
    
    ;; Render (valid, expanded) `sxml` as html.
    ;; `@raw` tag is considered safe text and not processed or escaped.
    (define (display-sxml sxml . o)
      (let ((out (if (pair? o) (car o) (current-output-port))))
        (let lp ((sxml sxml))
          (cond
            ((pair? sxml)
              (let ((tag (car sxml)))
                (if (symbol? tag)
                    (let ((rest (cdr sxml)))
                      (cond
                        ((and (pair? rest)
                              (pair? (car rest))
                              (eq? '@ (caar rest)))
                          (display (html-tag->string tag (cdar rest)) out)
                          (for-each lp (cdr rest))
                          (display "</" out)
                          (display tag out)
                          (display ">" out))
                        ((and (eq? '@raw tag)
                              (string? (car rest)))
                          (display (car rest) out))
                        ((null? rest)
                          (display "<" out)
                          (display tag out)
                          (display "/>" out))
                        (else
                          (display (html-tag->string tag '()) out)
                          (for-each lp rest)
                          (display "</" out)
                          (display tag out)
                          (display ">" out))))
                    (for-each lp sxml))))
            ((null? sxml))
            ((markdown? sxml)
              (display (markdown->html sxml) out))
            ((or (markdown-block? sxml) (markdown-blocks? sxml))
              (display (blocks->html sxml) out))
            ((or (markdown-inline? sxml) (markdown-text? sxml))
              (display (text->html sxml) out))
            (else
              (html-display-escaped-string sxml out))))))
    
    ;; Render `sxml` as `xml`.
    ;; `@raw` tag is considered safe text and not processed or escaped.
    (define (sxml->xml sxml)
      (call-with-output-string
        (lambda (out) (display-sxml sxml out))))
    
    ;; Render `sxml` as `html`.
    ;; `@raw` tag is considered safe text and not processed or escaped.
    (define (sxml->html sxml)
      (call-with-output-string
        (lambda (out) (display-sxml sxml out))))
    
    ;; Render `sxml` as simple text, stripping all tags.
    (define (sxml-strip sxml)
      (call-with-output-string
        (lambda (out)
          (let strip ((x sxml))
            (cond
              ((pair? x)
                (for-each strip (if (and (pair? (cdr x)) (eq? '@ (cadr x))) (cddr x) (cdr x))))
              ((string? x)
                (display x out)))))))
    
    ;; Render `sxml` as text for viewing in a terminal.
    (define (display-sxml-text sxml . o)
      (let ((out (if (pair? o) (car o) (current-output-port))))
        (let lp ((sxml sxml))
          (cond
           ((pair? sxml)
            (let ((tag (car sxml)))
              (cond
               ;; skip headers and the menu
               ((or (memq tag '(head style script))
                    (and (eq? 'div tag)
                         (pair? (cdr sxml))
                         (pair? (cadr sxml))
                         (eq? '@ (car (cadr sxml)))
                         (equal? '(id . "menu") (assq 'id (cdr (cadr sxml)))))))
               ;; recurse other tags, appending newlines for new sections
               ((symbol? tag)
                (for-each
                 lp
                 (if (and (pair? (cdr sxml)) (eq? '@ (cadr sxml)))
                     (cddr sxml)
                     (cdr sxml)))
                (if (memq tag '(p li br h1 h2 h3 h4 h5 h6))
                    (newline out)))
               (else
                (for-each lp sxml)))))
           ((null? sxml))
           ((or (markdown-inline? sxml) (markdown-text? sxml))
             (display (text->string sxml) out))
           ((char? sxml)
             (display (string sxml) out))
           ((symbol? sxml)
             (display (symbol->string sxml) out))
           ((number? sxml)
             (display (number->string sxml) out))
           ((styled-text? sxml)
             (display (styled-text-string sxml) out))
           ((date-time? sxml)
             (display (date-time->string sxml) out))
           ((vector? sxml)
             (display (vector->string sxml) out))
           (else
             (display sxml out))))))
    
    ;; Render `sxml` as a simple text.
    ;; `@raw` tag is considered safe text and not processed or escaped.
    (define (sxml->text sxml)
      (call-with-output-string
        (lambda (out) (display-sxml-text sxml out))))
  )
)
