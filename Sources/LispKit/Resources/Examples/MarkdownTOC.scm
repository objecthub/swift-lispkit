;;; Insert a table of contents into a Markdown document
;;;
;;; This program parses a Markdown document and processes its abstract syntax
;;; tree with pattern matching: it extracts the headings of the document, builds
;;; a nested list with links to the headings from them, and demotes the headings
;;; of the document by one level. The result is a new document with a new title,
;;; a table of contents, and the original document as its main part. The new
;;; document is written back as Markdown source, saved as PDF and RTFD document,
;;; and the PDF document is opened afterwards by the default application of the
;;; operating system.
;;;
;;; The links of the table of contents refer to the headings by their anchors,
;;; which are derived from the titles of the headings as GitHub does it. Thus,
;;; they work if the Markdown source is shown by a viewer that supports such
;;; anchors. Links to a place in the same document cannot be followed in PDF
;;; and RTFD documents, so the table of contents of these documents does not
;;; contain any links.
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
        (lispkit datatype)
        (scheme list)
        (lispkit markdown))

;; Where the generated files are stored
(define output-directory (car (system-directory 'documents)))
(define markdown-file-path (path output-directory "SampleTextWithToc.md"))
(define rtfd-file-path (path output-directory "SampleTextWithToc.rtfd"))
(define pdf-file-path (path output-directory "SampleTextWithToc.pdf"))

;; The title of the new document, and the heading of its table of contents
(define document-title "Sample Document")
(define toc-title "Table of Contents")

;; The Markdown dialect used for reading and writing documents. Dialect `latex`
;; supports tables and definition lists, underlined text, strikethrough text,
;; task lists, and LaTeX formulas.
(define dialect 'latex)

;; Read the Markdown file and parse it
(define doc
  (markdown (read-file (asset-file-path "SampleText" "md" "Documents")) dialect))

;; Extracting headings

;; Returns the blocks of a Markdown document
(define (document-blocks md)
  (match md
    ((document blocks) blocks)))

;; Returns a list of headings `(level . title)` for the headings in `blocks`, in the
;; order in which they occur. Only the headings on the top-level of a document are
;; considered; headings within block quotes and lists do not belong into a table of
;; contents. `text->string` turns the text of a heading into plain text.
(define (extract-headings blocks)
  (filter-map
    (lambda (block)
      (match block
        ((heading level text) (cons level (text->string text)))
        (else #f)))
    blocks))

;; Creating anchors

;; Returns the anchor for the title of a heading as it is created by GitHub: the
;; title in lowercase, with spaces and hyphens replaced by hyphens, and without any
;; other characters than letters, digits, and underscores.
(define (anchor title)
  (list->string
    (filter-map
      (lambda (ch)
        (cond ((or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_)) ch)
              ((or (char-whitespace? ch) (char=? ch #\-)) #\-)
              (else #f)))
      (string->list (string-downcase title)))))

;; Turns the headings `(level . title)` into entries `(level title anchor)`. If
;; there are several headings with the same anchor, the anchors of all but the
;; first one get a number as suffix (`summary`, `summary-1`, `summary-2`, ...).
(define (make-entries headings)
  (let ((used (make-equal-hashtable)))
    (map (lambda (heading)
           (let* ((base (anchor (cdr heading)))
                  (count (hashtable-ref used base 0)))
             (hashtable-set! used base (+ count 1))
             (list (car heading)
                   (cdr heading)
                   (if (= count 0)
                       base
                       (string-append base "-" (number->string count))))))
         headings)))

;; Creating the table of contents

;; Returns a bullet list item for `entry`, which has the nested list of items for the
;; entries `children` as sub-list. If `links?` is true, the title of the entry is a
;; link to the anchor of its heading.
(define (toc-item entry children links?)
  (let ((title (cadr entry))
        (anchor (caddr entry)))
    (bullet #\- #t
      (cons (paragraph
              (list (if links?
                        (link (list (text title)) (string-append "#" anchor) #f)
                        (text title))))
            (if (null? children)
                '()
                (list (list-items #f #t (toc-items children links?))))))))

;; Returns the list items for the entries in `entries`. An entry is followed by the
;; entries that have a greater level, which become its children. All other entries
;; are on the same level as the first entry.
(define (toc-items entries links?)
  (if (null? entries)
      '()
      (let* ((level (car (car entries)))
             (deeper? (lambda (entry) (> (car entry) level)))
             (children (take-while deeper? (cdr entries)))
             (rest (drop-while deeper? (cdr entries))))
        (cons (toc-item (car entries) children links?)
              (toc-items rest links?)))))

;; Returns the table of contents for `entries` as a list of blocks: a heading of
;; level 2 followed by a nested list with an item for each entry
(define (table-of-contents entries links?)
  (list (heading 2 (list (text toc-title)))
        (list-items #f #t (toc-items entries links?))))

;; Demoting headings

;; Returns `blocks` with all headings demoted by one level; the level of a heading
;; never gets larger than 6. Headings are also demoted if they are within block
;; quotes and lists.
(define (demote-headings blocks)
  (map (lambda (block)
         (match block
           ((heading level text) (heading (min 6 (+ level 1)) text))
           ((blockquote subblocks) (blockquote (demote-headings subblocks)))
           ((list-items start tight items) (list-items start tight (map demote-item items)))
           (else block)))
       blocks))

;; Returns the list item `item` with all headings within it demoted by one level
(define (demote-item item)
  (match item
    ((bullet ch tight? blocks) (bullet ch tight? (demote-headings blocks)))
    ((ordered num ch tight? blocks) (ordered num ch tight? (demote-headings blocks)))
    ((task checked? subitem) (task checked? (demote-item subitem)))))

;; Putting the new document together

;; Returns a new document with a title, the table of contents, and the demoted blocks
;; of the original document
(define (new-document md links?)
  (let* ((blocks (document-blocks md))
         (entries (make-entries (extract-headings blocks))))
    (document
      (append (list (heading 1 (list (text document-title))))
              (table-of-contents entries links?)
              (demote-headings blocks)))))

;; The document for Markdown files, whose table of contents links to the headings
(define linked-doc (new-document doc #t))

;; The document for PDF and RTFD documents, whose table of contents has no links
(define plain-doc (new-document doc #f))

;; Write the document with the links back as Markdown source. `markdown->source-string`
;; writes a document that is parsed into an equal document again. Documents are
;; compared with `markdown=?`, which does not care how the parser splits text into
;; fragments (`markdown-equals?` compares the structure of documents).
(define source (markdown->source-string linked-doc dialect))
(write-file markdown-file-path source)

(display "Generated ")
(display markdown-file-path)
(display " (reads back as equal document: ")
(display (markdown=? linked-doc (markdown source dialect)))
(display ")")
(newline)
(newline)

;; Show the beginning of the new document: its title, and the table of contents
(display (markdown->source-string (document (take (document-blocks linked-doc) 3)) dialect))
(newline)

;; Save the document without links as RTFD and PDF documents. Both formats keep
;; the diagram and the formulas of the document as images. The PDF document has
;; A4 pages with margins of 20 mm.
(save-markdown rtfd-file-path plain-doc 'rtfd
               '((mermaid . ((theme . neutral) (width . 400)))
                 (remote-images . none))
               #f #f "Sample Document" "LispKit")
(save-markdown pdf-file-path plain-doc 'pdf
               '((font-family . "Times")
                 (font-size . 15)
                 (line-spacing . 1.32)
                 (code-font-family . "Menlo, Consolas, monospace")
                 (code-font-size . 12)
                 (code-block-font-size . 8.9)
                 (code-block-line-spacing . 1.2)
                 (h1-font-size . 24)
                 (h2-font-size . 20)
                 (h3-font-size . 17)
                 (mermaid . ((theme . neutral) (width . 300)))
                 (remote-images . none))
               'a4 "20mm" "Sample Document" "LispKit")
(display "Generated ")
(display rtfd-file-path)
(newline)
(display "Generated ")
(display pdf-file-path)
(newline)

;; Open the PDF document in a system-specific fashion
(open-file pdf-file-path)
