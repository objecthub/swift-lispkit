;;; Generate a RTFD document with different text styles
;;;
;;; This is a demo of library (lispkit styled-text). It explains how to
;;; programmatically generate a complex text document, composing it out of
;;; several different paragraph styles, text styles, and text block styles.
;;; The example also shows how to create styled lists and styled text tables
;;; and explains how to nest them. 
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2022 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may
;;; not use this file except in compliance with the License. You may obtain
;;; a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

; Import required libraries

(import (lispkit base)
        (lispkit draw)
        (lispkit styled-text))

; Load all the fonts we need upfront

(define title-font  (font "Times" 20 bold))
(define header-font (font "Times" 17 bold))
(define body-font   (font "Times" 15 normal))
(define bold-font   (font "Times" 15 bold))
(define italic-font (font "Times" 15 normal italic))
(define small-font  (font "Times" 13 bold))

; Define common paragraph styles

(define title-style
  (make-paragraph-style
    'alignment: 'center
    'paragraph-spacing-after: 8
    'paragraph-spacing-before: 14))

(define header-style
  (make-paragraph-style
    'alignment: 'left
    'paragraph-spacing-after: 9
    'paragraph-spacing-before: 14))

(define body-style
  (make-paragraph-style
    'alignment: 'justified
    'paragraph-spacing-after: 6))

(define body-style-left (copy-paragraph-style body-style))
(paragraph-style-set! body-style-left 'alignment 'left)

(define indented-style
  (make-paragraph-style
    'alignment: 'left
    'paragraph-spacing-after: 8
    'paragraph-spacing-before: 8
    'first-head-indent: 20
    'head-indent: 20
    'tail-indent: 500))

; Start document with a title that is centered and shown in blue. Subsequent elements of
; the document are appended to styled text `document`

(define document
  (styled-text "Generated Sample Document\n" title-font blue title-style))

; First section

; Add the section title
(styled-text-append! document
  (styled-text "Overview of the approach\n" header-font black header-style))
; Start with regular text
(styled-text-append! document
  (styled-text "Lorem ipsum dolor sit amet, " body-font black body-style))
; Inject some text in italics
(styled-text-append! document
  (styled-text "consectetur adipiscing elit" italic-font black body-style))
(styled-text-append! document
  (styled-text ". Maecenas lectus orci, accumsan sed arcu ornare, " body-font black body-style))
; Inject some text in bold
(styled-text-append! document
  (styled-text "egestas mattis diam" bold-font black body-style))
; And now two paragraphs are added to showcase the paragraph spacing
(styled-text-append! document
  (styled-text
    (string-append
       ". Vestibulum ut mauris quis nulla laoreet varius. Etiam luctus ac diam tincidunt "
       "pharetra. Nulla a mi sed nunc sagittis venenatis. Curabitur quis lorem et ipsum "
       "aliquam maximus. Duis eget magna mauris. Sed malesuada est rhoncus eros hendrerit "
       "porttitor.\n"
       "Aenean sagittis ac mi vitae vehicula. Nam consequat eros aliquam sem feugiat "
       "aliquet. Cras ultrices pretium eros ut euismod. Aliquam nec turpis sed urna "
       "condimentum ullamcorper a tristique augue.\n")
    body-font black body-style))

; Second section

; We start with a section title and some normal text first
(styled-text-append! document
  (styled-text "Detailed exploration\n" header-font black header-style))
(styled-text-append! document
  (styled-text
    (string-append
       "Donec rhoncus, neque a consequat ultrices, massa erat tempor nulla, et sodales "
       "nulla lectus facilisis enim. Pellentesque habitant morbi tristique senectus et "
       "netus et malesuada fames ac turpis egestas. Nam quis vestibulum augue.\n")
    body-font black body-style-left))
; The following paragraph gets indented and its width gets capped; text is shown
; in italics.
(styled-text-append! document
  (make-styled-text
    (string-append
      "Mauris scelerisque massa erat, maximus luctus purus laoreet nec. In et massa eu "
      "eros porttitor sollicitudin ut nec tortor. Suspendisse gravida magna placerat, "
      "volutpat enim ut, accumsan elit.\n")
    'font: italic-font
    'foreground-color: (color 0.3 0.3 0.3)
    'paragraph-style: indented-style))

; Third section

; The next section shows a table; we define a few text block styles ahead which will be
; used to set up the table. Tables are not available on iOS, so the following section
; is added only on macOS.

(cond-expand (macos

; Headers get centered
(define table-header-style (make-paragraph-style 'alignment: 'center))

; The header row's first column takes 20% of the overall width, has a bold border at the
; bottom and its background is light gray
(define fst-head-style
  (make-text-block-style
     'width: (percent 20)
     'height: 20
     'border: '(1 1 1 2)
     'padding: '(2 2 4 4)
     'margin: 2
     'border-color: gray
     'background-color: (color 0.9 0.9 0.9)
     'vertical-alignment: 'middle))

; The header row's second column takes 80% of the overall width, it spans two regular
; columns (we define this later).
(define snd-head-style (copy-text-block-style fst-head-style))
(text-block-style-set! snd-head-style 'width (percent 80))

; The tables first column has again gray borders, takes up 20%, and has border, padding,
; and margin spaces.
(define fst-col-style
  (make-text-block-style
     'width: (percent 20)
     'border: 1
     'padding: 8
     'margin: 2
     'border-color: gray))

; The other two columns each take up 40% and are otherwise identical to the first column
(define col-style (copy-text-block-style fst-col-style))
(text-block-style-set! col-style 'width (percent 40))

; We also predefine a simple nested table below (embedded in some text):
(define nested-tbl
  (styled-text "Nullam efficitur tellus non molestie:\n" body-font black body-style-left))
(define table-start (string-length (styled-text-string nested-tbl)))
(styled-text-append! nested-tbl
  (make-styled-text-table 2
    (list
      (list "Lorem" "Dolor")
      (list "Ipsum" "Sit"))
    (make-text-block-style
      'width: (percent 100)
      'border: 1
      'padding: 2
      'margin: 2
      'border-color: (color 0.4 0.4 0.9)
      'background-color: (color 0.93 0.93 1.0))
    (make-paragraph-style
      'paragraph-spacing-after: 8
      'paragraph-spacing-before: 2)
    #t))
(define table-end (string-length (styled-text-string nested-tbl)))
(styled-text-append! nested-tbl
  (styled-text "Mauris egestas dolor mauris, id mollis urna ultrices feugiat."
               body-font black
               body-style-left))
; The following lines set all fonts in the nested table to `small-font` and the
; foreground color of the text is set to dark blue. Patching attributes is another
; possibility to change the style of a large section of text quickly.
(styled-text-add! nested-tbl table-start table-end 'font small-font)
(styled-text-add! nested-tbl table-start table-end 'foreground-color (color 0 0 0.6))

; Now we start adding the section title
(styled-text-append! document
  (styled-text "Tabular data\n" header-font black header-style))
; ...followed by the table:
(styled-text-append! document
  (make-styled-text-table 3
    (list
      (list (list (make-styled-text "First column"
                    'font: (font "Helvetica-Bold" 12) 
                    'paragraph-style: table-header-style) 1 fst-head-style)
            (list (make-styled-text "Second column\n(experimental)"
                    'font: (font "Helvetica-Bold" 12) 
                    'paragraph-style: table-header-style) 2 snd-head-style))
      (list (list (styled-text "Maecenas eget nibh felis. Etiam sagittis sit."
                     body-font black body-style-left) 1 fst-col-style)
            (list (styled-text (string-append
                     "Nunc pretium, ante porta varius eleifend, nulla quam tristique nisl, "
                     "id vulputate felis est sed quam.") body-font black body-style-left) 1 col-style)
            (list (styled-text (string-append
                     "Nam ac accumsan elit. Pellentesque habitant morbi tristique senectus "
                     "et netus et malesuada fames ac turpis egestas.") body-font black) 1 col-style))
      (list (list
              (styled-text (string-append
                 "Maecenas non dui scelerisque, tincidunt lacus in, suscipit felis. "
                 "Suspendisse pharetra.") body-font black body-style-left) 1 fst-col-style)
            (list nested-tbl 1 col-style)
            (styled-text (string-append
               "Aliquam velit est, congue sed sollicitudin in, luctus non enim. Nulla "
               "facilisi. Aliquam diam urna, ultrices facilisis diam quis, tempor "
               "pharetra felis. Nullam nec condimentum sapien. Nullam at dolor nec "
               "tortor.") body-font black body-style-left)
            ))
    col-style
    body-style-left
    #t))
))

; Fourth section

; The last section showcases lists. We define the list style upfront here:
(define list-style
  (make-paragraph-style
    'alignment: 'left
    'first-head-indent: 0.0
    'head-indent: 40.0
    'line-spacing: -1.0
    'paragraph-spacing-before: 4.0
    'paragraph-spacing-after: 4.0))
; Tab stops are used to format the table. All predefined tab stops are removed first and
; then two tab stops are added. The first is for the number (it's right aligned), the
; second is for the list content (it's left aligned).
(paragraph-style-tabstops-clear! list-style)
(paragraph-style-tabstop-add! list-style 30 'right)
(paragraph-style-tabstop-add! list-style 40 'left)

; Again, first the section header is added
(styled-text-append! document
  (styled-text "Summary\n" header-font black header-style))
; ...followed by some text
(styled-text-append! document
  (styled-text
    (string-append
       "Donec vestibulum tortor at est venenatis malesuada. Aliquam erat volutpat. "
       "Sed consectetur quam quis odio varius, eget facilisis justo pharetra. "
       "Nulla maximus vestibulum laoreet. Interdum et malesuada fames ac ante "
       "ipsum primis in faucibus.\n")
    body-font black body-style))
; ...followed by the enumerated list.
(styled-text-append! document
  (styled-text
    (string-append
       "\t1.\tLorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas nec "
       "rhoncus erat, in ullamcorper diam. Pellentesque tristique pulvinar sagittis.\n"
       "\t2.\tAliquam id tortor ac nulla tempus auctor a ut sem. Nulla eget aliquam "
       "sem, a consequat nibh. Sed ullamcorper semper semper. Quisque mattis.\n"
       "\t3.\tVulputate accumsan. Morbi euismod dolor turpis, at dignissim augue "
       "maximus sed. Etiam neque tortor, venenatis non eros quis, dignissim rhoncus "
       "lectus. Duis accumsan diam eget neque rutrum bibendum. Suspendisse laoreet, "
       "turpis in tempus congue, sapien dui vulputate elit, vitae porttitor metus "
       "lorem vitae ante. Proin efficitur lorem vel dui finibus pretium. Donec "
       "pretium erat nec turpis laoreet sagittis.\n")
    body-font
    black
    list-style))

; Saving and opening the document

; Path to the generated RTFD file
(define rtfd-file-path
  (path (car (system-directory 'documents)) "sample-doc.rtfd"))

; Save the document
(save-styled-text rtfd-file-path document 'rtfd)

; Open the document in a system-specific fashion
(open-file rtfd-file-path)
