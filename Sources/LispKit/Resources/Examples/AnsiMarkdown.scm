;;; Display a Markdown document on an ANSI terminal
;;; 
;;; This program is loading a Markdown document, parsing it, and
;;  displaying the marked up document on an ANSI terminal.
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing,
;;; software distributed under the License is distributed on an
;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;;; either express or implied. See the License for the specific
;;; language governing permissions and limitations under the
;;; License.

(import (lispkit base)
        (lispkit markdown))

(define ansi-config
  '((("#900" #f bold)         ; h1: "#900", bold
     ("#0012a0" #f bold)      ; h2: "#0012a0", bold
     ("#000" #f bold)         ; h3: black, bold
     (grey #f bold underline) ; h4: grey, bold, underlined
     (grey #f bold))          ; h5: grey, bold
    (blue #f underline)       ; links: blue, underlined
    (green #f)                ; inline code: green
    (black #f dim)            ; code block border: black dim
    (white light-black)       ; code block lang: white light-black dim
    italic                    ; emphasis: italic
    bold                      ; strong: bold
    (purple #f bold)          ; def term: purple, bold
    (grey #f)                 ; def descr: grey
    black                     ; blockquote: black
    (lime #f)                 ; breaks: teal
    (#f #t () #t #t)))        ; syntax highlighting:
                              ;  (theme ignore-syntax-err ignore-lang
                              ;   highl-indent-block full-color)

;; Read a Markdown file
(define doc
  (markdown
    (read-file
      (asset-file-path "SampleText" "md" "Documents"))
    #t))

;; Determine terminal size
(define term-size (terminal-size))

;; If this is an ANSI terminal...
(if term-size
  (begin
    ;; Display terminal size
    (display "Output optimized for terminal size: ")
    (display (car term-size))
    (display "x")
    (display (cdr term-size))
    (newline)
    ;; Display the marked up Markdown document
    (display
      (markdown->string doc (car term-size) ansi-config)))
  ;; Notify user this is not an ANSI terminal
  (display "Not an ANSI terminal."))
(newline)
