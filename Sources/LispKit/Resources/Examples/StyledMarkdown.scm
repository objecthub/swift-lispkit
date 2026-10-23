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

;; The configuration for `markdown->styled-text`, an association list with all keys
;; that are supported (see "Configuring styled documents" in the documentation of
;; library `(lispkit markdown)`). All entries are optional. The values are the
;; defaults, except for the entries that this program changes; for these, the
;; default is mentioned in the comment. `#f` selects the default for most keys.
(define config
  '(;; Appearance: the preset `light` has colors for a white background
    (style . light)                         ; Preset: `default`, `light`, or `dark`
    (font-family . "Helvetica")             ; Font of the text; default: system-ui, -apple-system,
                                            ; 'Helvetica Neue', Helvetica, sans-serif
    (font-size . 11)                        ; Overall font size; default: 14
    (font-color . "#000000")                ; Color of the text
    (line-spacing . 1.35)                   ; Line height of text as a multiple of the line
                                            ; height of its font; default: 1.1
    (code-font-family . "Menlo")            ; Font of inline code and code blocks; default:
                                            ; 'SF Mono', SFMono-Regular, ui-monospace, Menlo,
                                            ; Consolas, monospace
    (code-font-size . 10)                   ; Size of inline code font; default: 13
    (code-font-color . "#0000AA")           ; Color of inline code
    (code-block-font-size . 9.5)            ; Code block font size; default: 12
    (code-block-font-color . "#000099")     ; Text color of code blocks
    (code-block-background . "#EEE")        ; Background color of code blocks
    (code-block-line-spacing . 1.15)        ; Line height of code blocks; default: 1
    (border-color . "#DDD")                 ; Border of code blocks
    (blockquote-color . "#BBE")             ; Bars of blockquotes
    (header-font . #f)                      ; Font of headings; #f is the font of the text
    (h1-color . "#990000")                  ; Colors of the headings; default: #aa1111,
    (h2-color . "#0012a0")                  ; #003399, #0055CC, and #007AFF
    (h3-color . "#0055CC")
    (h4-color . "#007AFF")
    (h1-font-size . #f)                     ; Sizes of headings; #f is 6, 4, 2, and 1 points
    (h2-font-size . #f)                     ; larger than the font size of the text
    (h3-font-size . #f)
    (h4-font-size . #f)
    (custom-style . "")                     ; CSS rules that are added to the style sheet
    ;; Code blocks, lists, and images
    (syntax-highlighting
      . ((theme . "xcode")                  ; Theme for highlighting code blocks; default: #f,
                                            ; the theme of the preset
         (ignore-syntax-errors . #t)        ; Highlight code with syntax errors anyway
         (ignored-languages . (""))         ; Do not highlight code blocks without a language
         (highlight-indented-code-blocks . #t)))
    (tight-lists . #f)                      ; Show paragraphs of loose list items tightly
    (align-list-markers . #f)               ; Align markers with the first line of their items
    (max-image-width . #f)                  ; CSS length limiting the width and height of
    (max-image-height . #f)                 ; images, e.g. "100%" or "300px"
    (image-base-url . #f)                   ; Directory or URL relative image paths refer to
    ;; Rendering: `preset`, `base-url`, `timeout`, and the following keys control how
    ;; the system renders the document, and which resources it may load
    (preset . trusted)                      ; `trusted` or `untrusted`
    (base-url . #f)                         ; Directory or URL relative URLs refer to
    (timeout . 30)                          ; Seconds that rendering may take at most
    (local-images . any)                    ; `none`, `any`, or a directory
    (remote-images . none)                  ; `none`, `any`, or a URL; default: any
    (image-extensions . ("png" "jpg" "jpeg" "gif" "tif" "tiff" "bmp" "ico" "heic" "heif"
                         "webp"))           ; Path extensions of images that get loaded
    (safe-mode . #f)                        ; Omit raw HTML and restrict links
    (text-size-multiplier . #f)             ; Factor scaling font sizes
    ;; Mermaid diagrams: not drawn by default (`#f`); this program draws them laid
    ;; out for a page width of 400 points. RTFD documents show the images in their
    ;; natural size.
    (mermaid . ((theme . neutral)           ; Theme: `default`, `neutral`, `dark`, `forest`,
                                            ; or `base`; default: default
                (theme-variables . ())      ; Overrides of variables of the theme
                (font-family . #f)          ; Font of the text of diagrams
                (scale . 2)                 ; Resolution of images relative to their size
                (width . 400)               ; Width of the page; default: 800
                (margin . 8)                ; Margin around a diagram
                (max-width . #f)            ; Maximum width of a diagram in the text
                (spacing . 16)              ; Distance to the text above and below
                (timeout . 5)))             ; Seconds for rendering a diagram
    ;; Formulas of Markdown dialects `math` and `latex` are typeset by default
    (math . ((scale . 3)                    ; Resolution of images relative to their size
             (spacing . 8)                  ; Distance of block formulas to the text
             (max-width . #f)               ; Width that wider formulas are scaled down to
             (timeout . 15)                 ; Seconds for rendering all formulas
             (macros . ())                  ; KaTeX macros: (("\\half" . "\\frac{1}{2}") ...)
             (max-expand . 1000)            ; Maximum number of macro expansions
             (max-size . #f)                ; Maximum size of formulas
             (min-rule-thickness . #f)      ; Minimum thickness of fraction lines and the like
             (max-length . 10000)))))       ; Maximum length of a formula in bytes

;; Read the Markdown file. Dialect `latex` supports tables and definition lists,
;; underlined text, strikethrough text, task lists, and LaTeX formulas.
(define doc
  (markdown
    (read-file (asset-file-path "SampleText" "md" "Documents"))
    'latex))

;; Convert the Markdown document into styled text ready to be exported as RTFD.
;; This renders the document with the help of the operating system, which takes
;; a moment.
(define text (markdown->styled-text doc config #t))

;; Save the styled text as RTFD document.
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
(open-file pdf-file-path)
