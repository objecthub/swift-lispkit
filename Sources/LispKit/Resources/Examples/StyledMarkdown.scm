;;; Convert a Markdown document into styled text, an RTFD document, and a PDF file
;;;
;;; This program loads a Markdown document, parses it, and converts it into
;;; styled text. This styled text is then saved as an RTFD document and drawn
;;; onto the pages of a PDF file, together with page numbers. Both documents are
;;; opened afterwards by the default application of the operating system.
;;;
;;; The Markdown document contains tables, task lists, underlined and struck
;;; through text, syntax-highlighted code, as well as a Mermaid diagram. All of
;;; these are supported by the configuration passed to `markdown->styled-text`.
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
        (lispkit draw)
        (lispkit markdown)
        (lispkit styled-text))

;; Where the generated files are stored
(define output-directory (car (system-directory 'documents)))
(define rtfd-file-path (path output-directory "SampleText.rtfd"))
(define pdf-file-path (path output-directory "SampleText.pdf"))

;; Page layout of the PDF file in points (A4 portrait)
(define page-width 595)
(define page-height 842)
(define page-margin 50)
(define text-width (- page-width (* 2 page-margin)))
(define text-height (- page-height (* 2 page-margin)))

;; The style configuration for `markdown->styled-text`. All entries are
;; optional. See the documentation of `markdown->html-doc` for all keys.
(define config
  '((font-family . "Helvetica")             ; Font of the text and its size
    (font-size . 11)                        ; Overall font size
    (code-font-family . "Menlo")            ; Fonts of inline code and code blocks
    (code-font-size . 10)                   ; Size of inline code font
    (code-block-font-size . 9.5)            ; Code block font size
    (line-spacing . 1.35)                   ; Line height of text and of code blocks as a
    (code-block-line-spacing . 1.15)        ; multiple of the line height of their fonts
    (h1-color . "#990000")                  ; Colors of the headings
    (h2-color . "#0012a0")
    (syntax-highlighting . ((theme . "xcode"))) ; Theme for highlighting code blocks
    (mermaid . ((theme . neutral)           ; Draw Mermaid diagrams laid out for a page
                (width . 400)))             ; width of 400 points; RTFD documents show
                                            ; the images in their natural size
    (remote-images . none)))                ; Do not load images from the web

;; Read the Markdown file. Parser `full` supports tables and definition lists,
;; as well as underlined text, strikethrough text, and task lists.
(define doc
  (markdown
    (read-file (asset-file-path "SampleText" "md" "Documents"))
    'full))

;; Convert the Markdown document into styled text. This renders the document
;; with the help of the operating system, which takes a moment.
(define text (markdown->styled-text doc config))

;; Save the styled text as RTFD document
(save-styled-text rtfd-file-path text 'rtfd)

;; The following code splits the styled text into chunks of paragraphs which fit
;; onto a page. Text is split between paragraphs only; a table, list, or code block
;; which does not fit onto the rest of a page continues on the next page.

;; Returns the heights of the text between `start` and `end`
(define (text-height-between start end)
  (size-height (styled-text-size (copy-styled-text text start end) text-width)))

;; Returns a vector with all positions in `str` where a paragraph ends. The last
;; position is the length of `str`.
(define (paragraph-ends str)
  (let* ((n (string-length str))
         (ends (let loop ((i (- n 1)) (acc '()))
                 (cond ((< i 0) acc)
                       ((char=? (string-ref str i) #\newline) (loop (- i 1) (cons (+ i 1) acc)))
                       (else (loop (- i 1) acc))))))
    (list->vector
      (if (and (> n 0) (char=? (string-ref str (- n 1)) #\newline))
          ends
          (append ends (list n))))))

;; Returns a list of pairs (start . end) of text positions. Text between `start`
;; and `end` fits onto a page (unless a single paragraph is larger than a page).
(define (page-ranges)
  (let* ((ends (paragraph-ends (styled-text-string text)))
         (n (vector-length ends)))
    ;; Finds the largest index into `ends`, starting at index `lo`, for which the text
    ;; from `start` to the end position fits onto a page (via binary search)
    (define (last-fitting start lo)
      (let loop ((lo lo) (hi (- n 1)) (best lo))
        (if (> lo hi)
            best
            (let ((mid (quotient (+ lo hi) 2)))
              (if (<= (text-height-between start (vector-ref ends mid)) text-height)
                  (loop (+ mid 1) hi mid)
                  (loop lo (- mid 1) best))))))
    (let loop ((start 0) (next 0) (ranges '()))
      (if (>= next n)
          (reverse ranges)
          (let ((best (last-fitting start next)))
            (loop (vector-ref ends best)
                  (+ best 1)
                  (cons (cons start (vector-ref ends best)) ranges)))))))

;; Style of the page numbers
(define footer-font (font "Helvetica" 9))
(define footer-style (make-paragraph-style 'alignment: 'center))

;; Returns the drawing of a page showing the text of `range`
(define (page-drawing range number total)
  (drawing
    ;; The text of the page
    (draw-styled-text
      (copy-styled-text text (car range) (cdr range))
      (rect page-margin page-margin text-width text-height))
    ;; The page number
    (draw-styled-text
      (make-styled-text (string-append "Page " (number->string number)
                                       " of " (number->string total))
                        'font: footer-font
                        'foreground-color: gray
                        'paragraph-style: footer-style)
      (rect page-margin 24 text-width 14))))

;; Draw the pages and save them as a PDF file
(define ranges (page-ranges))
(define pages
  (map (lambda (range number)
         (list (page-drawing range number (length ranges))
               (size page-width page-height)))
       ranges
       (iota (length ranges) 1)))
(save-drawings pdf-file-path pages "Sample Text" "LispKit")

;; Report what was generated
(display "Generated ")
(display rtfd-file-path)
(newline)
(display "Generated ")
(display pdf-file-path)
(display " (")
(display (length pages))
(display " pages)")
(newline)

;; Open the documents in a system-specific fashion
(open-file rtfd-file-path)
; (open-file pdf-file-path)
