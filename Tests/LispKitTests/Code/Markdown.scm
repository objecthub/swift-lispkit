;;; Markdown.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2019-2026 ObjectHub. All rights reserved.
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
  "Markdown blocks"
  (#t #t #t #t #t #t #t #t #t)
  (import (lispkit markdown))
  (define empty-doc (document '())) ; empty document
  (define empty-par (document (list (paragraph '()))))
  (define full-par (document (list
                     (paragraph (list (text "Line 1") (line-break #f) (text "Line 2"))))))
  (define head-doc (document (list
                     (heading 1 (list (text "First")))
                     (heading 2 (list (text "Second")))
                     (paragraph (list (text "Some text finally"))))))
  (define bq-doc (document (list
                   (blockquote (list
                     (paragraph (list (text "Foo") (line-break #t) (text "Bar"))))))))
  (define list-doc (document (list
                     (list-items #f #t (list
                       (bullet #\- '() (list (paragraph (list (text "Alpha")))))
                       (bullet #\- #t (list (paragraph (list (text "Beta"))))))))))
  (define list2-doc (document (list
                      (list-items 1 #t (list
                        (ordered 1 #\. '() (list (paragraph (list (text "Eins")))))
                        (ordered 2 #\. #t (list (paragraph (list (text "Zwei"))))))))))
  (define icode-doc (document (list (indented-code '("#1\n" "#2")))))
  (define fcode-doc (document (list (fenced-code "swift" '("let x = 1 + 2\n")))))
  (define thematic-doc (document (list
                         (paragraph (list (emph (list (text "a")))))
                         (thematic-break)
                         (paragraph (list (strong (list (text "b"))))))))
  (list
    (markdown=? empty-doc (markdown ""))
    (markdown=? full-par (markdown "Line 1\nLine 2"))
    (markdown=? head-doc (markdown "# First\n## Second\nSome text finally"))
    (markdown=? bq-doc (markdown "> Foo  \n>  Bar"))
    (markdown=? list-doc (markdown "- Alpha\n- Beta"))
    (markdown=? list2-doc (markdown "1. Eins\n2. Zwei"))
    (markdown=? icode-doc (markdown "    #1\n    #2"))
    (markdown=? fcode-doc (markdown "```swift\nlet x = 1 + 2\n```"))
    (markdown=? thematic-doc (markdown "_a_\n***\n__b__")))
)

(
  "Markdown inline text"
  (#t #t #t #t #t #t)
  (define md1 (document (list (paragraph (list (text "text ") (code "code") (text " rest"))))))
  (define md2 (document (list (paragraph (list (emph (list (text "one two"))) (text " three"))))))
  (define md3 (document (list (paragraph (list (strong (list (text "one two"))) (text " three"))))))
  (define md4 (document (list (paragraph (list
                (strong (list (text "one ") (emph (list (text "two"))))) (text " three"))))))
  (define md5 (document (list (paragraph (list (link (list (text "LispPad"))
                                                     "http://lisppad.app"
                                                     "Mac App"))))))
  (define md6 (document (list (paragraph (list (html "a href=\"http://objecthub.com\"")
                                               (text "homepage")
                                               (html "/a"))))))
  (list
    (markdown=? md1 (markdown "text `code` rest"))
    (markdown=? md2 (markdown "*one two* three"))
    (markdown=? md3 (markdown "**one two** three"))
    (markdown=? md4 (markdown "**one _two_** three"))
    (markdown=? md5 (markdown "[LispPad](http://lisppad.app \"Mac App\")"))
    (markdown=? md6 (markdown "<a href=\"http://objecthub.com\">homepage</a>")))
)

(
  "Markdown pattern matching"
  (("text " "code" " rest"))
  (import (lispkit datatype))
  (define md1 (document (list (paragraph (list (text "text ") (code "code") (text " rest"))))))
  (match md1
    ((document blocks)
      (map (lambda (block)
             (match block
               ((paragraph inlines)
                 (map (lambda (inline)
                        (match inline
                          ((text str) str)
                          ((code str) str)
                          (else "")))
                      inlines))
               (else '())))
            blocks))
    (else '()))
)

(
  "Extended markdown blocks"
  #t
  (define table-doc
    (document (list
      (table
        (list (list (text "First")) (list (text "Second")) (list (text "Third ") (code "Column")))
        (list 'l 'c 'r)
        (list
          (list (list (text "Alan")) (list (text "Mathison")) (list (text "Turing")))
          (list (list (text "Arthur ") (emph (list (text "John"))) (text " Robin"))
                (list (strong (list (text "Gorell"))))
                (list (text "Milner"))))))))
  (define def-doc
    (document (list
      (heading 1 (list (text "Header")))
      (definition-list (list
        (list (list (text "Software"))
              (bullet #\: #f (list
                (paragraph (list (text "programs used by a")))
                (list-items #f #t (list
                  (bullet #\- #t (list (paragraph (list (text "device")))))
                  (bullet #\- #t (list (paragraph (list (emph (list (text "computer")))))))))
                (paragraph (list (text "and other systems")))))
              (bullet #\: '() (list
                (paragraph (list (text "operating instructions"))))))
        (list (list (text "Hardware"))
              (bullet #\: #t (list
                (blockquote (list (paragraph (list (text "One")
                                             (line-break #f)
                                             (text "Two")))))))))))))
  (and
    (markdown=? table-doc (markdown (string-append "| First | Second | Third `Column` |\n"
                                                   "| :---- | :----: | ----: |\n"
                                                   "| Alan  | Mathison | Turing |\n"
                                                   "| Arthur *John* Robin | **Gorell** |Milner|\n")
                                    #t))
    (markdown=? def-doc (markdown (string-append "# Header\n\n"
                                                 "Software\n"
                                                 " : programs used by a\n"
                                                 "     - device\n"
                                                 "     - *computer*\n\n"
                                                 "   and other systems\n\n"
                                                 " : operating instructions\n\n"
                                                 "Hardware\n"
                                                 "  :  > One\n"
                                                 "     > Two")
                                  #t)))
)

(
  "Markdown underline and strikethrough"
  (#t #t #t #t #t #t #t #t #t #t #t)
  (define md1 (document (list (paragraph (list (underline (list (text "one"))) (text " two"))))))
  (define md2 (document (list (paragraph (list (strikethrough (list (text "one"))) (text " two"))))))
  (define md3 (document (list (paragraph (list
                (strong (list (text "a ") (strikethrough (list (text "b"))))) (text " c"))))))
  (define md4 (document (list (paragraph (list
                (strong (list (text "a ") (strikethrough (list (underline (list (text "b"))))))) 
                (text " c"))))))
  (list
    (markdown=? md1 (markdown "~one~ two" 'full))
    (markdown=? md2 (markdown "~~one~~ two" 'full))
    (markdown=? md3 (markdown "**a ~~b~~** c" 'full))
    (not (markdown=? md1 (markdown "~one~ two" 'extended)))
    (and (markdown-inline? (underline '())) (markdown-inline? (strikethrough '())))
    (equal? (markdown->html md1) "<p><u>one</u> two</p>\n")
    (equal? (markdown->html md2) "<p><del>one</del> two</p>\n")
    (equal? (markdown->html md4) "<p><strong>a <del><u>b</u></del></strong> c</p>\n")
    (equal? (markdown->sxml md1) '((p (u ("one")) " two")))
    (equal? (markdown->sxml md2) '((p (del ("one")) " two")))
    (equal? (markdown->string md4) "a b c"))
)

(
  "Markdown task lists"
  (#t #t #t #t #t #t #t #t #t #t)
  (define t1 (task #f (bullet #\- '() (list (paragraph (list (text "open")))))))
  (define t2 (task #t (bullet #\- #t (list (paragraph (list (text "done")))))))
  (define t3 (task #t (ordered 1 #\. '() (list (paragraph (list (text "one")))))))
  (define t4 (task #f (ordered 2 #\. #t (list (paragraph (list (text "two")))))))
  (define doc1 (document (list (list-items #f #t (list t1 t2)))))
  (define doc2 (document (list (list-items 1 #t (list t3 t4)))))
  (list
    (markdown=? doc1 (markdown "- [ ] open\n- [x] done" 'full))
    (markdown=? doc2 (markdown "1. [x] one\n2. [ ] two" 'full))
    (not (markdown=? doc1 (markdown "- [ ] open\n- [x] done" 'extended)))
    (and (markdown-list-item? t1) (markdown-list-item? t3))
    (markdown-list-item=? t1 (task #f (bullet #\- '() (list (paragraph (list (text "open")))))))
    (not (markdown-list-item=? t1 t2))
    (equal? (markdown->html doc1)
            (string-append "<ul>\n"
                           "<li><input disabled=\"\" type=\"checkbox\"> open</li>\n"
                           "<li><input checked=\"\" disabled=\"\" type=\"checkbox\"> done</li>\n"
                           "</ul>\n"))
    (equal? (markdown->html doc2)
            (string-append "<ol start=\"1\">\n"
                           "<li><input checked=\"\" disabled=\"\" type=\"checkbox\"> one</li>\n"
                           "<li><input disabled=\"\" type=\"checkbox\"> two</li>\n"
                           "</ol>\n"))
    (equal? (markdown->sxml doc1)
            '((ul (li (input (@ (disabled "") (type "checkbox"))) " " "open")
                  (li (input (@ (checked "") (disabled "") (type "checkbox"))) " " "done"))))
    (equal? (markdown->formatted-string doc1 40) "  [ ] open\n  [x] done"))
)

(
  "Markdown pattern matching of new constructs"
  ((#t "done") (1 "u"))
  (import (lispkit datatype))
  (define item (task #t (bullet #\- #t (list (paragraph (list (text "done")))))))
  (define und (underline (list (text "u"))))
  (list
    (match item
      ((task checked? (bullet ch tight? (list (paragraph (list (text str)))))) (list checked? str))
      (else '()))
    (match und
      ((underline (list (text str))) (list 1 str))
      (else '())))
)

(
  "Markdown task item must wrap a bullet or ordered item"
  <error>
  (task #t (task #t (bullet #\- #t '())))
)

(
  "Markdown underline requires inline text"
  <error>
  (underline "text")
)

(
  "Markdown ANSI strings with underline and strikethrough"
  ("~u~ ~~s~~" "\x1b;[4mu\x1b;[0m \x1b;[9ms\x1b;[0m" "\x1b;[91mu\x1b;[0m \x1b;[96ms\x1b;[0m")
  (define md (markdown "~u~ ~~s~~" 'full))
  (list
    (markdown->formatted-string md 40)
    (markdown->formatted-string md 40 #t)
    (markdown->formatted-string md 40 (list #f #f #f #f #f #f #f #f #f #f #f #t 'red 'aqua)))
)

(
  "Markdown style configurations"
  #t
  (define legacy
    (markdown-style-config '(18 "Helvetica" "#123456") '(11 #f "#222") '(10 "#111" "#eee")
                           '("#aaa" #f "#f00" "#0f0") '("xcode" #f ("scheme") #f)))
  (and
    (equal? (markdown-style-config) '())
    (equal? (markdown-style-config #f #f #f #f #f) '())
    (equal? legacy
            '((font-size . 18) (font-family . "Helvetica") (font-color . "#123456")
              (code-font-size . 11) (code-font-color . "#222")
              (code-block-font-size . 10) (code-block-font-color . "#111")
              (code-block-background . "#eee")
              (border-color . "#aaa") (h1-color . "#f00") (h2-color . "#0f0")
              (syntax-highlighting (theme . "xcode") (ignore-syntax-errors . #f)
                                   (ignored-languages "scheme")
                                   (highlight-indented-code-blocks . #f))))
    (equal? (markdown-style-config #f #f #f #f #t) '((syntax-highlighting . #f)))
    (equal? (markdown-style-config #f #f #f #f '()) '((syntax-highlighting . #f)))
    (equal? (markdown-style-config #f #f #f #f '(#f #f))
            '((syntax-highlighting (ignore-syntax-errors . #f)))))
)

(
  "Markdown HTML documents with configurations"
  (#t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t)
  (import (lispkit string))
  (define md (markdown (string-append "# Head\n\nText ![img](http://example.org/a.png) "
                                      "<b>raw</b>\n\n```scheme\n(+ 1 2)\n```\n")))
  (define (has? str sub) (and (string-contains str sub) #t))
  (define default (markdown->html-doc md))
  (define restricted (markdown->html-doc md '((remote-images . none) (safe-mode . #t))))
  (define styled (markdown->html-doc md '((font-family . "Zapfino") (h1-color . "#abcdef")
                                          (custom-style . "p.mine { x: y }")
                                          (max-image-width . "123px")
                                          (max-image-height . "45px"))))
  (define first-wins (markdown->html-doc md '((font-family . "First") (font-family . "Second"))))
  (define ported (markdown->html-doc md (markdown-style-config '(30 "Verdana" "#654321"))))
  (define untrusted (markdown->html-doc md '((preset . untrusted) (remote-images . any))))
  (define headers (markdown->html-doc md '((header-font . "Georgia, serif") (h1-font-size . 31)
                                           (h2-font-size . 22) (h3-font-size . 17)
                                           (h4-font-size . 19))))
  (define dark (markdown->html-doc md '((style . dark))))
  (define light (markdown->html-doc md '((style . light))))
  (define plain (markdown->html-doc md '((style . default))))
  (define refined (markdown->html-doc md '((style . dark) (font-color . "#123456")
                                           (h1-color . "#abcdef"))))
  (list
    (has? default "example.org/a.png")
    (has? default "<b>raw</b>")
    (not (has? restricted "example.org/a.png"))
    (not (has? restricted "<b>raw</b>"))
    (has? styled "Zapfino")
    (has? styled "#abcdef")
    (has? styled "p.mine { x: y }")
    (and (has? styled "123px") (has? styled "45px"))
    (and (has? first-wins "First") (not (has? first-wins "Second")))
    (and (has? ported "Verdana") (has? ported "#654321"))
    (has? untrusted "example.org/a.png")
    (not (has? untrusted "<b>raw</b>"))
    ; without a preset, preset light is used
    (and (equal? default light) (has? default "system-ui") (has? default "#aa1111")
         (has? default "#DDD") (has? default "#BBE") (has? default "#EEE")
         (not (has? default "Georgia")))
    (and (has? headers "Georgia, serif") (has? headers "31.0px") (has? headers "22.0px")
         (has? headers "17.0px") (has? headers "19.0px"))
    (and (has? dark "#C55") (has? dark "#4AF") (has? dark "#333") (has? dark "system-ui")
         (not (has? dark "Times New Roman")))
    (and (has? light "#aa1111") (has? light "#EEE") (has? light "system-ui"))
    (and (has? plain "system-ui") (has? plain "#cccccc") (has? plain "#abe")
         (not (has? plain "Times New Roman")))
    ; keys refine a preset
    (and (has? refined "#123456") (has? refined "#abcdef") (has? refined "#4AF"))
    ; the preset determines the theme for syntax highlighting, which can be refined or disabled
    (and (has? dark "569cd6") (not (has? light "569cd6")) (not (has? default "569cd6"))
         (has? refined "569cd6")
         (has? (markdown->html-doc md '((style . dark)
                                        (syntax-highlighting . ((ignore-syntax-errors . #f)))))
               "569cd6")
         (not (has? (markdown->html-doc md '((style . dark) (syntax-highlighting . #f)))
                    "569cd6")))
    (string? (markdown->html-doc md #f))
    (string? (markdown->html-doc md '((syntax-highlighting . #f) (tight-lists . #t)))))
)

(
  "Markdown styled text"
  (#t #t #t #t #t #t #t #t)
  (import (lispkit styled-text))
  (define md (markdown "# Title\n\nSome *text* and `code`.\n\n- [x] done\n- [ ] open\n" 'full))
  (list
    (styled-text? (markdown->styled-text md))
    (styled-text? (markdown->styled-text md #f))
    (styled-text? (markdown->styled-text md '()))
    (styled-text? (markdown->styled-text md '((font-size . 20) (font-family . "Helvetica")
                                              (preset . untrusted) (timeout . 20)
                                              (text-size-multiplier . 1.5)
                                              (syntax-highlighting . #f))))
    (styled-text? (markdown->styled-text md (markdown-style-config '(12 "Menlo" "#112233")
                                                                   #f #f #f #t)))
    (styled-text? (markdown->styled-text md '((tight-lists . #t))))
    (styled-text? (markdown->styled-text md '((style . dark) (line-spacing . 1.5)
                                              (code-block-line-spacing . 1.2)
                                              (header-font . "Georgia")
                                              (h1-font-size . 30) (h2-font-size . 24)
                                              (h3-font-size . 20) (h4-font-size . 16))))
    (styled-text? (markdown->styled-text md '((align-list-markers . #t) (tight-lists . #t)))))
)

(
  "Markdown links and images with empty titles"
  (#t #t #t #t #t #t #t #t #t #t #t #t #t #t)
  (import (lispkit string))
  (define (has? str sub) (and (string-contains str sub) #t))
  (define (inline-doc make title)
    (document (list (paragraph (list (make (list (text "a")) "b" title))))))
  (define parsed-link (markdown "[a](b)"))
  (define parsed-image (markdown "![a](b)"))
  (define titled-link (markdown "[a](b \"t\")"))
  (list
    (markdown-equals? (inline-doc link "") (inline-doc link #f))
    (markdown-equals? (inline-doc image "") (inline-doc image #f))
    (markdown-inline=? (link (list (text "a")) "b" "") (link (list (text "a")) "b" #f))
    (markdown-inline-equals? (image (list (text "a")) "b" "") (image (list (text "a")) "b" #f))
    (markdown=? (inline-doc link "") parsed-link)
    (markdown=? (inline-doc image "") parsed-image)
    (not (markdown-equals? (inline-doc link "t") parsed-link))
    (markdown-equals? (inline-doc link "t") titled-link)
    (equal? (markdown->source-string (inline-doc link "")) "[a](b)\n")
    (equal? (markdown->source-string (inline-doc image "")) "![a](b)\n")
    (equal? (markdown->source-string (inline-doc link "t")) "[a](b \"t\")\n")
    (markdown=? (inline-doc link "") (markdown (markdown->source-string (inline-doc link ""))))
    (not (has? (markdown->html (inline-doc link "")) "title"))
    (has? (markdown->html (inline-doc image "t")) "title=\"t\""))
)

(
  "Markdown link with a title that is no string"
  <error>
  (link (list (text "a")) "b" 1)
)

(
  "Markdown image with a title that is no string"
  <error>
  (image (list (text "a")) "b" 'title)
)

(
  "Markdown PDF data"
  (#t #t #t #t #t #t #t)
  (import (lispkit bytevector))
  (define pdf-md (markdown (string-append "# Title\n\nSome *text* and `code` with a "
                                          "[link](https://lisppad.app).\n\n- one\n- two\n\n"
                                          "The area is $\\pi r^2$.\n\n"
                                          "| a | b |\n|---|---|\n| 1 | 2 |\n")
                           'latex))
  (define (pdf-header data) (utf8->string (bytevector-copy data 0 5)))
  (list
    (bytevector? (markdown->pdf-data pdf-md))
    (equal? (pdf-header (markdown->pdf-data pdf-md)) "%PDF-")
    (equal? (pdf-header (markdown->pdf-data pdf-md #f)) "%PDF-")
    (equal? (pdf-header (markdown->pdf-data pdf-md '())) "%PDF-")
    (equal? (pdf-header (markdown->pdf-data pdf-md #f #f #f #f #f)) "%PDF-")
    (equal? (pdf-header (markdown->pdf-data (document '()))) "%PDF-")
    (> (bytevector-length (markdown->pdf-data pdf-md #f 'a4 "10mm" "T" "A"))
       (bytevector-length (markdown->pdf-data (document '())))))
)

(
  "Markdown PDF documents"
  (#t 1 "My Title" "Me" (595.0 842.0) (612.0 792.0) (300.0 400.5) (612.0 792.0) #t #t #t #t)
  (import (only (lispkit pdf) pdf? pdf-page-count pdf-page pdf-page-bounds pdf-attributes)
          (only (lispkit draw) rect-size size-width size-height))
  (define pdf-md (markdown "Hello *world*, this is a PDF document.\n"))
  (define (page-size doc)
    (let ((size (rect-size (pdf-page-bounds (pdf-page doc 0) 'media-box))))
      (list (size-width size) (size-height size))))
  (define titled (markdown->pdf pdf-md #f #f #f "My Title" "Me"))
  (list
    (pdf? (markdown->pdf pdf-md))
    (pdf-page-count (markdown->pdf pdf-md))
    (cdr (assq 'Title (pdf-attributes titled)))
    (cdr (assq 'Author (pdf-attributes titled)))
    (page-size (markdown->pdf pdf-md #f 'a4))
    (page-size (markdown->pdf pdf-md #f "8.5x11in"))
    (page-size (markdown->pdf pdf-md #f '(300 400.5)))
    (page-size (markdown->pdf pdf-md #f 'letter))
    (pdf? (markdown->pdf pdf-md #f "210x297mm" "20mm, 15mm"))
    (pdf? (markdown->pdf pdf-md #f #f 36 #f #f))
    (pdf? (markdown->pdf pdf-md '((font-size . 20)) "a4" '(10 20 30 40) "Title"))
    (equal? (page-size (markdown->pdf pdf-md '((preset . untrusted)) '(200 300) '(10 20) #f "Me"))
            '(200.0 300.0)))
)

(
  "Markdown PDF documents with several pages"
  (#t #t #t)
  (import (only (lispkit pdf) pdf-page-count))
  (define many-md
    (markdown (apply string-append
                     (map (lambda (i) (string-append "Paragraph " (number->string i) ".\n\n"))
                          (iota 60)))))
  (list
    (>= (pdf-page-count (markdown->pdf many-md)) 2)
    (> (pdf-page-count (markdown->pdf many-md #f '(200 300) 20)) 3)
    (> (pdf-page-count (markdown->pdf many-md #f '(200 300) '(20 20 20 20)))
       (pdf-page-count (markdown->pdf many-md #f 'a4))))
)

(
  "Markdown PDF documents with configurations"
  (#t #t #t #t #t #t #t #t)
  (import (only (lispkit pdf) pdf?))
  (define pdf-md (markdown "# Title\n\nSome *text* and `code`.\n\n```scheme\n(define x 1)\n```\n\n- [x] done\n- [ ] open\n"
                           'full))
  (list
    (pdf? (markdown->pdf pdf-md))
    (pdf? (markdown->pdf pdf-md #f))
    (pdf? (markdown->pdf pdf-md '()))
    (pdf? (markdown->pdf pdf-md '((font-size . 20) (font-family . "Helvetica")
                                  (preset . untrusted) (timeout . 20)
                                  (text-size-multiplier . 1.5) (syntax-highlighting . #f))))
    (pdf? (markdown->pdf pdf-md '((style . dark) (line-spacing . 1.5) (header-font . "Georgia")
                                  (h1-font-size . 30) (h2-font-size . 24))))
    (pdf? (markdown->pdf pdf-md (markdown-style-config '(12 "Menlo" "#112233") #f #f #f #t)))
    (pdf? (markdown->pdf pdf-md '((tight-lists . #t) (align-list-markers . #t) (mermaid . #t)
                                  (math . #t))))
    (pdf? (markdown->pdf pdf-md '((safe-mode . #t) (timeout . #f)) 'a4 #f "Doc")))
)

(
  "Markdown PDF documents with diagrams and formulas"
  (#t #t)
  (define plain-md (markdown "Before\n\nAfter\n"))
  (define diagram-md
    (markdown (string-append "Before\n\n```mermaid\ngraph LR\n  A[Start] --> B{Choice}\n"
                             "  B --> C[Done]\n```\n\nAfter\n")))
  (define formula-md (markdown "Before $x^2$\n\n$$\n\\int_0^1 x\\,dx\n$$\n\nAfter\n" 'math))
  (list
    (> (bytevector-length (markdown->pdf-data diagram-md))
       (bytevector-length (markdown->pdf-data plain-md)))
    (> (bytevector-length (markdown->pdf-data formula-md))
       (bytevector-length (markdown->pdf-data plain-md))))
)

(
  "Markdown PDF documents with configurations of diagrams and formulas"
  (#t #t #t #t #t #t #t #t #t #t #t)
  (define (pdf-size md . args) (bytevector-length (apply markdown->pdf-data md args)))
  (define diagram-md
    (markdown (string-append "Before\n\n```mermaid\ngraph LR\n  A[Start] --> B{Choice}\n"
                             "  B --> C[Done]\n```\n\nAfter\n")))
  (define formula-md (markdown "Before $x^2$ and $$\\int_0^1 x\\,dx$$ After" 'math))
  (list
    (< (pdf-size diagram-md '((mermaid . ((scale . 1))))) (pdf-size diagram-md))
    (> (pdf-size diagram-md '((mermaid . ((scale . 4))))) (pdf-size diagram-md))
    (= (pdf-size diagram-md '((mermaid . #t))) (pdf-size diagram-md))
    (= (pdf-size diagram-md '((mermaid . ()))) (pdf-size diagram-md))
    (bytevector? (markdown->pdf-data diagram-md '((mermaid . ((theme . forest) (width . 600)
                                                              (margin . 4) (max-width . 300)
                                                              (spacing . 4) (timeout . 20)
                                                              (theme-variables .
                                                                ((primaryColor . "#ffcc00"))))))))
    (< (pdf-size formula-md '((math . ((scale . 1))))) (pdf-size formula-md))
    (> (pdf-size formula-md '((math . ((scale . 6))))) (pdf-size formula-md))
    (= (pdf-size formula-md '((math . #t))) (pdf-size formula-md))
    (bytevector? (markdown->pdf-data formula-md '((math . ((spacing . 4) (max-width . 100)
                                                           (timeout . 20)
                                                           (max-expand . 500))))))
    (bytevector? (markdown->pdf-data formula-md '((image-base-url . "/tmp/images/"))))
    (bytevector? (markdown->pdf-data formula-md '((image-base-url . #f) (base-url . "/tmp")))))
)

(
  "Markdown PDF documents with disabled diagrams and formulas"
  ("Before\nAfter"
   "Before\ngraph LR\nA[Start] --> B{Choice}\nAfter"
   "Before and After"
   "Before $x^2$ and After"
   "Before\ngraph LR\nA[Start] --> B{Choice}\nAfter"
   "Before $x^2$ and After")
  (import (only (lispkit pdf) pdf->string))
  (define diagram-md
    (markdown (string-append "Before\n\n```mermaid\ngraph LR\n  A[Start] --> B{Choice}\n```\n\n"
                             "After\n")))
  (define formula-md (markdown "Before $x^2$ and After" 'math))
  (list
    (pdf->string (markdown->pdf diagram-md))
    (pdf->string (markdown->pdf diagram-md '((mermaid . #f))))
    (pdf->string (markdown->pdf formula-md))
    (pdf->string (markdown->pdf formula-md '((math . #f))))
    (pdf->string (markdown->pdf diagram-md '((mermaid . #f) (math . #f))))
    (pdf->string (markdown->pdf formula-md '((mermaid . #f) (math . #f)))))
)

(
  "Markdown saved as PDF, RTF and RTFD documents"
  (#t #t "Title" #t #t #t #t #t #t #t #t #t #t #t #t)
  (import (only (lispkit system) system-directory path file-exists? directory-exists?
                                 delete-file-or-directory)
          (only (lispkit pdf) load-pdf pdf-page-count pdf-attributes))
  (define save-dir (car (system-directory 'temporary)))
  (define pdf-file (path save-dir "lispkit-markdown-save-test.pdf"))
  (define rtf-file (path save-dir "lispkit-markdown-save-test.rtf"))
  (define rtfd-file (path save-dir "lispkit-markdown-save-test.rtfd"))
  (define plain-rtfd-file (path save-dir "lispkit-markdown-save-test-plain.rtfd"))
  (define (cleanup)
    (for-each (lambda (file)
                (if (or (file-exists? file) (directory-exists? file))
                    (delete-file-or-directory file)))
              (list pdf-file rtf-file rtfd-file plain-rtfd-file)))
  (define save-md
    (markdown "# Title\n\nSome *text* and $x^2$.\n\n```mermaid\ngraph LR\n  A --> B\n```\n" 'math))
  (cleanup)
  (save-markdown pdf-file save-md 'pdf '((font-size . 16)) 'a4 "20mm" "Title" "Me")
  (save-markdown pdf-file save-md 'pdf)
  (save-markdown pdf-file save-md 'pdf '((font-size . 16)) 'a4 "20mm" "Title" "Me")
  (save-markdown rtf-file save-md 'rtf #f #f #f "Title" "Me")
  (save-markdown rtfd-file save-md 'rtfd '((mermaid . #t)) #f #f "Title" "Me")
  (save-markdown rtfd-file save-md 'rtfd '((mermaid . #t)) #f #f "Title" "Me")
  (save-markdown plain-rtfd-file save-md 'rtfd '((mermaid . #f) (math . #f)))
  (define saved
    (list
      (file-exists? pdf-file)
      (= (pdf-page-count (load-pdf pdf-file)) 1)
      (cdr (assq 'Title (pdf-attributes (load-pdf pdf-file))))
      (file-exists? rtf-file)
      (equal? (call-with-input-file rtf-file (lambda (port) (read-string 5 port))) "{\\rtf")
      (not (directory-exists? rtf-file))
      (directory-exists? rtfd-file)
      (file-exists? (path rtfd-file "TXT.rtf"))
      (file-exists? (path rtfd-file "Image 1.png"))
      (file-exists? (path rtfd-file "Formula 1.png"))
      (directory-exists? plain-rtfd-file)
      (file-exists? (path plain-rtfd-file "TXT.rtf"))
      (not (file-exists? (path plain-rtfd-file "Image 1.png")))
      (not (file-exists? (path plain-rtfd-file "Formula 1.png")))
      (not (directory-exists? pdf-file))))
  (cleanup)
  saved
)

(
  "Markdown saved with unsupported format"
  <error>
  (save-markdown "lispkit-markdown-unsupported.docx" (markdown "text") 'docx)
)

(
  "Markdown saved with format as string"
  <error>
  (save-markdown "lispkit-markdown-unsupported.pdf" (markdown "text") "pdf")
)

(
  "Markdown saved with too few arguments"
  <error>
  (save-markdown "lispkit-markdown-unsupported.pdf" (markdown "text"))
)

(
  "Markdown saved with too many arguments"
  <error>
  (save-markdown "lispkit-markdown-unsupported.pdf" (markdown "text") 'pdf #f #f #f #f #f #f)
)

(
  "Markdown saved with invalid document"
  <error>
  (save-markdown "lispkit-markdown-unsupported.pdf" "text" 'pdf)
)

(
  "Markdown saved with invalid file name"
  <error>
  (save-markdown 42 (markdown "text") 'pdf)
)

(
  "Markdown saved with invalid page size"
  <error>
  (save-markdown "lispkit-markdown-unsupported.pdf" (markdown "text") 'pdf #f 'b5)
)

(
  "Markdown saved with invalid margins"
  <error>
  (save-markdown "lispkit-markdown-unsupported.rtf" (markdown "text") 'rtf #f #f "wide")
)

(
  "Markdown saved with unknown configuration key"
  <error>
  (save-markdown "lispkit-markdown-unsupported.rtfd" (markdown "text") 'rtfd '((colour . red)))
)

(
  "Markdown saved into missing directory"
  <error>
  (save-markdown "/lispkit-markdown-missing-directory/test.pdf" (markdown "text") 'pdf)
)

(
  "Markdown saved as PDF with margins larger than the page"
  <error>
  (save-markdown "lispkit-markdown-unsupported.pdf" (markdown "text") 'pdf #f #f 400)
)

(
  "Markdown PDF with unknown key"
  <error>
  (markdown->pdf (markdown "text") '((colour . red)))
)

(
  "Markdown PDF data with unknown key"
  <error>
  (markdown->pdf-data (markdown "text") '((colour . red)))
)

(
  "Markdown PDF with unknown Mermaid key"
  <error>
  (markdown->pdf (markdown "text") '((mermaid . ((colour . red)))))
)

(
  "Markdown PDF with invalid Mermaid scale"
  <error>
  (markdown->pdf-data (markdown "text") '((mermaid . ((scale . 0)))))
)

(
  "Markdown PDF with unknown math key"
  <error>
  (markdown->pdf (markdown "text") '((math . ((colour . red)))))
)

(
  "Markdown PDF data with invalid math scale"
  <error>
  (markdown->pdf-data (markdown "text") '((math . ((scale . -1)))))
)

(
  "Markdown PDF with wrongly typed image base URL"
  <error>
  (markdown->pdf (markdown "text") '((image-base-url . 5)))
)

(
  "Markdown PDF with title in configuration"
  <error>
  (markdown->pdf (markdown "text") '((title . "Title")))
)

(
  "Markdown PDF data with page size in configuration"
  <error>
  (markdown->pdf-data (markdown "text") '((page-size . a4)))
)

(
  "Markdown PDF with too many arguments"
  <error>
  (markdown->pdf (markdown "text") #f #f #f #f #f #f)
)

(
  "Markdown PDF data with too many arguments"
  <error>
  (markdown->pdf-data (markdown "text") #f #f #f #f #f #f)
)

(
  "Markdown PDF with wrongly typed title"
  <error>
  (markdown->pdf (markdown "text") #f #f #f 1)
)

(
  "Markdown PDF with wrongly typed author"
  <error>
  (markdown->pdf-data (markdown "text") #f #f #f "Title" 'x)
)

(
  "Markdown PDF with unknown page format"
  <error>
  (markdown->pdf (markdown "text") #f 'b5)
)

(
  "Markdown PDF with invalid page size"
  <error>
  (markdown->pdf (markdown "text") #f "wide")
)

(
  "Markdown PDF with page size of wrong length"
  <error>
  (markdown->pdf (markdown "text") #f '(100))
)

(
  "Markdown PDF with negative page size"
  <error>
  (markdown->pdf (markdown "text") #f '(100 -200))
)

(
  "Markdown PDF with page size as number"
  <error>
  (markdown->pdf-data (markdown "text") #f 100)
)

(
  "Markdown PDF with invalid margins"
  <error>
  (markdown->pdf (markdown "text") #f #f "wide")
)

(
  "Markdown PDF with negative margins"
  <error>
  (markdown->pdf (markdown "text") #f #f -1)
)

(
  "Markdown PDF with margins list of wrong length"
  <error>
  (markdown->pdf (markdown "text") #f #f '(1 2 3))
)

(
  "Markdown PDF with margins as symbol"
  <error>
  (markdown->pdf-data (markdown "text") #f #f 'wide)
)

(
  "Markdown PDF with margins larger than the page"
  <error>
  (markdown->pdf (markdown "text") #f #f 400)
)

(
  "Markdown PDF data with margins larger than the page"
  <error>
  (markdown->pdf-data (markdown "text") #f '(100 100) '(60 10))
)

(
  "Markdown PDF of an invalid document"
  <error>
  (markdown->pdf "text")
)

(
  "Markdown PDF data of an invalid document"
  <error>
  (markdown->pdf-data 1)
)

(
  "Markdown configuration with unknown key"
  <error>
  (markdown->html-doc (markdown "text") '((no-such-key . 1)))
)

(
  "Markdown configuration with wrongly typed value"
  <error>
  (markdown->html-doc (markdown "text") '((font-size . "big")))
)

(
  "Markdown configuration with unknown style"
  <error>
  (markdown->html-doc (markdown "text") '((style . neon)))
)

(
  "Markdown configuration with wrongly typed heading size"
  <error>
  (markdown->html-doc (markdown "text") '((h1-font-size . "big")))
)

(
  "Markdown configuration with wrongly typed option for aligning list markers"
  <error>
  (markdown->styled-text (markdown "text") '((align-list-markers . "yes")))
)

(
  "Markdown configuration that is no association list"
  <error>
  (markdown->html-doc (markdown "text") 42)
)

(
  "Markdown configuration in legacy positional form"
  <error>
  (markdown->html-doc (markdown "text") '(18 "Helvetica" "#000000"))
)

(
  "Markdown configuration with invalid image access"
  <error>
  (markdown->html-doc (markdown "text") '((local-images . 5)))
)

(
  "Markdown configuration with invalid timeout"
  <error>
  (markdown->styled-text (markdown "text") '((timeout . -3)))
)

(
  "Markdown positional style arguments are not supported anymore"
  <error>
  (markdown->styled-text (markdown "text") '(14 "Helvetica" "#000000") #f)
)

(
  "Markdown style configuration with invalid argument"
  <error>
  (markdown-style-config '(12 "Menlo" "#112233" "too many"))
)

(
  "Markdown styled text with Mermaid diagrams"
  (#t #t #t #t #t #t)
  (import (lispkit styled-text) (lispkit string))
  (define (has? str sub) (and (string-contains str sub) #t))
  (define md (markdown (string-append "Before\n\n```mermaid\ngraph LR\n  A[Start] --> B{Choice}\n"
                                      "  B --> C[Done]\n```\n\nAfter\n")))
  (define plain (styled-text-string (markdown->styled-text md)))
  (define diagram (styled-text-string (markdown->styled-text md '((mermaid . #t)))))
  (define themed
    (styled-text-string
      (markdown->styled-text md '((mermaid . ((theme . forest) (scale . 1) (margin . #f)
                                              (spacing . 4) (timeout . 20)
                                              (theme-variables . ((primaryColor . "#ffcc00")))))))))
  (define invalid
    (styled-text-string
      (markdown->styled-text (markdown "```mermaid\nthis is not a diagram ->->\n```\n")
                             '((mermaid . #t)))))
  (list
    (has? plain "graph LR")
    (not (has? diagram "graph LR"))
    (< (string-length diagram) (string-length plain))
    (not (has? themed "graph LR"))
    (has? invalid "Diagram could not be rendered")
    (has? (markdown->html-doc md '((mermaid . #t))) "graph LR"))
)

(
  "Markdown configuration with invalid Mermaid theme"
  <error>
  (markdown->styled-text (markdown "text") '((mermaid . ((theme . pink)))))
)

(
  "Markdown configuration with invalid Mermaid scale"
  <error>
  (markdown->styled-text (markdown "text") '((mermaid . ((scale . 0)))))
)

(
  "Markdown configuration with unknown Mermaid key"
  <error>
  (markdown->styled-text (markdown "text") '((mermaid . ((colour . red)))))
)

(
  "Markdown dialects"
  (#t #t #t #t #t #t #t #t #t #t)
  (import (lispkit string))
  (define (to-html str dialect) (markdown->html (markdown str dialect)))
  (define (has? str sub) (and (string-contains str sub) #t))
  (define tab "a | b\n--|--\n1 | 2")
  (list
    (equal? (to-html tab #f) (to-html tab 'standard))
    (not (has? (to-html tab 'standard) "<table>"))
    (has? (to-html tab 'extended) "<table>")
    (not (has? (to-html "~u~" 'extended) "<u>"))
    (has? (to-html "~u~" 'full) "<u>")
    (not (has? (to-html "$x$" 'full) "math"))
    (has? (to-html "$x$" 'math) "math inline")
    (not (has? (to-html "\\(x\\)" 'math) "math inline"))
    (has? (to-html "\\(x\\)" 'latex) "math inline")
    (equal? (to-html "~u~ \\(x\\)" #t) (to-html "~u~ \\(x\\)" 'latex)))
)

(
  "Markdown with unknown dialect"
  <error>
  (markdown "text" 'github)
)

(
  "Markdown with misspelled dialect"
  <error>
  (markdown "text" 'latext)
)

(
  "Markdown with dialect as string"
  <error>
  (markdown "text" "full")
)

(
  "Markdown with dialect as number"
  <error>
  (markdown "text" 1)
)

(
  "Markdown with dialect #t"
  (#t #t)
  (import (lispkit string))
  (define (to-html str dialect) (markdown->html (markdown str dialect)))
  (list
    (and (string-contains (to-html "$x$" #t) "math inline") #t)
    (and (string-contains (to-html "~u~" #t) "<u>") #t))
)

(
  "Markdown without dialect is standard"
  (#t #t #t)
  (import (lispkit string))
  (define tab "a | b\n--|--\n1 | 2")
  (list
    (equal? (markdown->html (markdown tab)) (markdown->html (markdown tab 'standard)))
    (not (string-contains (markdown->html (markdown tab)) "<table>"))
    (equal? (markdown->html (markdown "\\(x\\) ~u~")) "<p>(x) ~u~</p>\n"))
)

(
  "Markdown plain and formatted strings"
  ("T\nsome emph and code\na\nb"
   "some *emph* and `code`"
   "T\n▔\n\nsome *emph* and `code`\n\n  – a\n  – b")
  (define md (markdown "# T\n\nsome *emph* and `code`\n\n- a\n- b"))
  (list
    (markdown->string md)
    (markdown->formatted-string (markdown "some *emph* and `code`") 40)
    (markdown->formatted-string md 40))
)

(
  "Markdown plain string of document without markdown"
  <error>
  (markdown->string "# T")
)

(
  "Markdown source strings"
  ("# Setext Title\n\n* one\n* **two**\n"
   "Costs $5 *only*\n"
   "Costs \\$5 *only*\n"
   "Costs \\$5 *only*\n"
   "Costs \\$5 *only*\n"
   "# x\n")
  (define costs (markdown "Costs $5 *only*"))
  (list
    (markdown->source-string (markdown "Setext Title\n===\n\n* one\n* __two__\n"))
    (markdown->source-string costs 'standard)
    (markdown->source-string costs 'math)
    (markdown->source-string costs 'latex)
    (markdown->source-string costs)
    (markdown->source-string (document (list (heading 1 (list (text "x")))))))
)

(
  "Markdown source strings depend on the dialect"
  ("a | b\n\n1 | 2\n"
   "| a | b |\n| --- | --- |\n| 1 | 2 |\n"
   "a u s b\n"
   "a ~u~ ~~s~~ b\n"
   "- \\[ ] t\n- \\[x] d\n"
   "- [ ] t\n- [x] d\n"
   "$x^2$ and $y$\n\n$$\nz\n$$\n"
   "$x^2$ and \\(y\\)\n\n$$\nz\n$$\n")
  (define tab (markdown "a | b\n--|--\n1 | 2" 'extended))
  (define fmt (markdown "a ~u~ ~~s~~ b" 'full))
  (define tasks (markdown "- [ ] t\n- [x] d" 'full))
  (define formulas (markdown "$x^2$ and \\(y\\)\n\n$$\nz\n$$\n" 'latex))
  (list
    (markdown->source-string tab 'standard)
    (markdown->source-string tab 'extended)
    (markdown->source-string fmt #f)
    (markdown->source-string fmt 'full)
    (markdown->source-string tasks 'extended)
    (markdown->source-string tasks 'full)
    (markdown->source-string formulas 'math)
    (markdown->source-string formulas #t))
)

(
  "Markdown source strings with special characters"
  ("a1b #c\n" "a\\1b #c\n" "a\\1b #c\n")
  (import (lispkit char-set))
  (define md (markdown "a1b #c"))
  (list
    (markdown->source-string md 'standard)
    (markdown->source-string md 'standard char-set:digit)
    (markdown->source-string md #f (char-set #\1)))
)

(
  "Markdown source strings are parsed into the same document"
  (#t #t #t #t #t)
  (define src "Title\n=====\n\nA *paragraph* with **strong**, `code`, and a [link](a.html \"t\").\n\n> quote\n\n1. one\n2. two\n\n```scheme\n(define x 1)\n```\n\n---\n")
  (define md (markdown src))
  (define ext-doc (markdown "a | b\n--|--\n1 | 2\n\nterm\n: definition\n" 'extended))
  (define full-doc (markdown "~u~ and ~~s~~\n\n- [ ] t\n- [x] d\n" 'full))
  (define latex-doc (markdown "$x^2$ and \\(y\\)\n\n\\[\nz\n\\]\n" 'latex))
  (list
    (markdown=? md (markdown (markdown->source-string md)))
    (markdown=? md (markdown (markdown->source-string md 'standard) 'standard))
    (markdown=? ext-doc (markdown (markdown->source-string ext-doc 'extended) 'extended))
    (markdown=? full-doc (markdown (markdown->source-string full-doc 'full) 'full))
    (equal? (markdown->html latex-doc)
            (markdown->html (markdown (markdown->source-string latex-doc 'latex) 'latex))))
)

(
  "Markdown equality normalizes text fragments"
  (#t #f #t #t #f #t #f)
  (define split (markdown "2 * 3 and (x) [y]"))
  (define merged (markdown "2 \\* 3 and \\(x\\) \\[y\\]" 'standard))
  (define other (markdown "2 * 4 and (x) [y]"))
  (list
    (markdown=? split merged)
    (markdown-equals? split merged)
    (markdown=? split (markdown (markdown->source-string split)))
    (markdown=? merged (document (list (paragraph (list (text "2 * 3 and (x) [y]"))))))
    (markdown=? split other)
    (markdown=? (document (list (paragraph (list (emph (list (text "a ") (text "b"))))))) 
                (document (list (paragraph (list (emph (list (text "a b"))))))))
    (markdown=? (document (list (paragraph (list (text "a") (emph (list (text "b")))))))
                (document (list (paragraph (list (text "ab")))))))
)

(
  "Markdown structural equality"
  (#t #t #f #f #t #f)
  (define doc (document (list (paragraph (list (text "a ") (text "b"))))))
  (list
    (markdown-equals? doc doc)
    (markdown-equals? doc (document (list (paragraph (list (text "a ") (text "b")))))) 
    (markdown-equals? doc (document (list (paragraph (list (text "a b"))))))
    (markdown-equals? (markdown "*a*") (markdown "_b_"))
    (markdown-equals? (markdown "*a*") (markdown "_a_"))
    (markdown-equals? (markdown "a") (document '())))
)

(
  "Markdown structural equality requires documents"
  <error>
  (markdown-equals? (markdown "a") "a")
)

(
  "Markdown block, list item, and inline equality"
  (#t #t #f #f #t #t #f #f #t #t #f #f)
  (define (par . frags) (paragraph frags))
  (define split-par (par (text "a ") (text "(") (text "b") (text ")")))
  (define whole-par (par (text "a (b)")))
  (define (item blk) (bullet #\- '() (list blk)))
  (define split-em (emph (list (text "a ") (text "b"))))
  (define whole-em (emph (list (text "a b"))))
  (list
    (markdown-block=? split-par whole-par)
    (markdown-block=? split-par split-par)
    (markdown-block-equals? split-par whole-par)
    (markdown-block=? split-par (par (text "a (c)")))
    (markdown-block-equals? split-par split-par)
    (markdown-list-item=? (item split-par) (item whole-par))
    (markdown-list-item-equals? (item split-par) (item whole-par))
    (markdown-list-item=? (item split-par) (item (par (text "a (c)"))))
    (markdown-inline=? split-em whole-em)
    (markdown-inline-equals? split-em split-em)
    (markdown-inline-equals? split-em whole-em)
    (markdown-inline=? split-em (emph (list (text "a c")))))
)

(
  "Markdown block equality requires blocks"
  <error>
  (markdown-block-equals? (paragraph '()) (text "a"))
)

(
  "Markdown list item equality requires list items"
  <error>
  (markdown-list-item-equals? (bullet #\- '() '()) (paragraph '()))
)

(
  "Markdown inline equality requires inline text"
  <error>
  (markdown-inline-equals? (text "a") (paragraph '()))
)

(
  "Markdown normalization"
  (#t #t #t #t #t #t #t #t #t #t #t #t #t #t)
  (define split-doc (markdown "2 * 3 and (x) *a (b)* [c (d)](e \"f\")"))
  (define whole-doc (markdown "2 \\* 3 and \\(x\\) *a \\(b\\)* [c \\(d\\)](e \"f\")" 'standard))
  (define frag-text (list (text "a") (text " ") (emph (list (text "b") (text "c"))) (text "")))
  (define frag-item (bullet #\- #t (list (paragraph (list (text "a") (text "b"))))))
  (define merged-item (bullet #\- #t (list (paragraph (list (text "ab"))))))
  (list
    (equal? (markdown->debug-string (normalize-markdown split-doc))
            "document(paragraph(text(\"2 * 3 and (x) \"), emph(text(\"a (b)\")), text(\" \"), link(text(\"c (d)\"), \"e\", \"f\")))")
    (not (markdown-equals? split-doc whole-doc))
    (markdown-equals? (normalize-markdown split-doc) (normalize-markdown whole-doc))
    (markdown-equals? (normalize-markdown split-doc) (normalize-markdown (normalize-markdown split-doc)))
    (eq? (markdown=? split-doc whole-doc)
         (markdown-equals? (normalize-markdown split-doc) (normalize-markdown whole-doc)))
    (markdown? (normalize-markdown split-doc))
    (markdown-inline-equals? (car (normalize-markdown (list (emph (list (text "b") (text "c"))))))
                             (emph (list (text "bc"))))
    (equal? (map markdown->debug-string
                 (list (document (list (paragraph (normalize-markdown frag-text))))))
            '("document(paragraph(text(\"a \"), emph(text(\"bc\"))))"))
    (markdown-list-item-equals? (normalize-markdown frag-item) merged-item)
    (every? (lambda (item) (markdown-list-item-equals? item merged-item))
            (normalize-markdown (list frag-item frag-item)))
    (markdown-block-equals? (normalize-markdown (paragraph (list (text "a") (text "b"))))
                            (paragraph (list (text "ab"))))
    (and (markdown-inline-equals? (normalize-markdown (text "")) (text ""))
         (markdown-inline-equals? (normalize-markdown (emph (list (text "b") (text "c"))))
                                  (emph (list (text "bc")))))
    (markdown-block-equals? (normalize-markdown (fenced-code "x" (list "a\r\n" "b\r")))
                            (fenced-code "x" (list "a\n" "b\n")))
    (null? (normalize-markdown '())))
)

(
  "Markdown normalization of lists of objects"
  (#t #t #t)
  (define frag-pars (list (paragraph (list (text "a") (text "b"))) (thematic-break)))
  (define frag-text (list (text "a") (text "b") (code "c") (text "d") (text "e")))
  (list
    (equal? (map markdown->debug-string
                 (map (lambda (block) (document (list block))) (normalize-markdown frag-pars)))
            '("document(paragraph(text(\"ab\")))" "document(thematicBreak)"))
    (equal? (map markdown->debug-string
                 (list (document (list (paragraph (normalize-markdown frag-text))))))
            '("document(paragraph(text(\"ab\"), code(\"c\"), text(\"de\")))"))
    (markdown-text? (normalize-markdown frag-text)))
)

(
  "Markdown normalization of a string"
  <error>
  (normalize-markdown "text")
)

(
  "Markdown normalization of a number"
  <error>
  (normalize-markdown 42)
)

(
  "Markdown normalization of a mixed list"
  <error>
  (normalize-markdown (list (paragraph '()) (text "a")))
)

(
  "Markdown normalization of a list of strings"
  <error>
  (normalize-markdown '("a" "b"))
)

(
  "Markdown equality requires documents"
  <error>
  (markdown=? (markdown "a") (text "a"))
)

(
  "Markdown plain strings of blocks and text"
  ("abc d" "abc d" "b" "x y" "abc d" "a b\nc")
  (define inl (list (text "a") (emph (list (text "b"))) (code "c") (html "br") (line-break #f)
                    (text "d")))
  (list
    (text->string inl)
    (blocks->string (paragraph inl))
    (text->string (emph (list (text "b"))))
    (blocks->string (list (paragraph (list (text "x") (line-break #f) (text "y")))))
    (markdown->string (document (list (paragraph inl))))
    (blocks->string (list (paragraph (list (text "a") (line-break #t) (text "b")))
                          (paragraph (list (text "c"))))))
)

(
  "Markdown debug strings of text"
  ("a*b*`c`<br>\nd" "*b*")
  (define inl (list (text "a") (emph (list (text "b"))) (code "c") (html "br") (line-break #f)
                    (text "d")))
  (list
    (text->debug-string inl)
    (text->debug-string (emph (list (text "b")))))
)

(
  "Markdown plain string of text with wrong argument"
  <error>
  (text->string "text")
)

(
  "Markdown plain string of blocks with wrong argument"
  <error>
  (blocks->string 1)
)

(
  "Markdown debug string of text with wrong argument"
  <error>
  (text->debug-string "text")
)

(
  "Markdown text in SXML plain text"
  "ab\n"
  (import (lispkit sxml))
  (sxml->text (list 'p (list (text "a") (emph (list (text "b"))))))
)

(
  "Markdown source string with unknown dialect"
  <error>
  (markdown->source-string (markdown "text") 'github)
)

(
  "Markdown source string with dialect as string"
  <error>
  (markdown->source-string (markdown "text") "full")
)

(
  "Markdown source string with dialect as number"
  <error>
  (markdown->source-string (markdown "text") 1)
)

(
  "Markdown source string with special characters as string"
  <error>
  (markdown->source-string (markdown "text") 'standard "*")
)

(
  "Markdown source string of an invalid document"
  <error>
  (markdown->source-string "text")
)

(
  "Markdown math formulas"
  (#t #t #t #t #t #t #t #t #t #t #t #t)
  (import (lispkit datatype))
  (define (inline-math? md)
    (match md
      ((document (list (paragraph (list (math tex display? source))))) (list tex display? source))
      (else #f)))
  (define (math-then-text? md)
    (match md
      ((document (list (paragraph (list (math tex display? source) (text rest))))) (list tex display? rest))
      (else #f)))
  (define (block-math? md)
    (match md
      ((document (list (math-block tex))) tex)
      (else #f)))
  (define doc (document (list (paragraph (list (text "Area ") (math "\\pi r^2" #f #f)))
                              (math-block "\\int_0^1 x\\,dx"))))
  (list
    (markdown? doc)
    (markdown=? doc (markdown "Area $\\pi r^2$\n\n$$\n\\int_0^1 x\\,dx\n$$\n" 'math))
    (equal? (inline-math? (markdown "$x$" 'math)) '("x" #f "$x$"))
    (equal? (math-then-text? (markdown "$$x$$ and" 'math)) '("x" #t " and"))
    (equal? (inline-math? (markdown "\\(x\\)" 'latex)) '("x" #f "\\(x\\)"))
    (equal? (math-then-text? (markdown "\\[x\\] and" 'latex)) '("x" #t " and"))
    (equal? (block-math? (markdown "$$\nx^2\n$$" 'math)) "x^2")
    (equal? (block-math? (markdown "```math\nx^2\n```" 'math)) "x^2")
    (equal? (block-math? (markdown "\\[\nx^2\n\\]" 'latex)) "x^2")
    (not (block-math? (markdown "```math\nx^2\n```" 'full)))
    (equal? (inline-math? (markdown "\\(x\\)" 'math)) #f)
    (equal? (inline-math? (markdown "costs $5 and $6" 'math)) #f))
)

(
  "Markdown math formula generators"
  (#t #t #t #t #t #t #t)
  (import (lispkit string))
  (define (has? str sub) (and (string-contains str sub) #t))
  (define md (markdown "Area $\\pi r^2$ and \\[E<m\\]\n\n$$\n\\int_0^1 x\\,dx\n$$\n" 'latex))
  (define markup (markdown->html md))
  (define plain (markdown->formatted-string md))
  (list
    (has? markup "<span class=\"math inline\">\\(\\pi r^2\\)</span>")
    (has? markup "<span class=\"math display\">\\[E&lt;m\\]</span>")
    (has? markup "<div class=\"math display\">\\[\\int_0^1 x\\,dx\\]</div>")
    (has? plain "Area $\\pi r^2$ and \\[E<m\\]")
    (has? plain "$$\n\\int_0^1 x\\,dx\n$$")
    (has? (markdown->html (document (list (paragraph (list (math "a" #t #f)))))) "math display")
    (pair? (markdown->sxml md)))
)

(
  "Markdown math formulas in styled text"
  (#t #t #t #t #t #t #t #t #t)
  (import (lispkit styled-text) (lispkit string))
  (define (has? str sub) (and (string-contains str sub) #t))
  (define md (markdown "Before $\\pi r^2$ after\n\n$$\n\\int_0^1 x\\,dx\n$$\n\nEnd\n" 'math))
  (define typeset (styled-text-string (markdown->styled-text md)))
  (define code (styled-text-string (markdown->styled-text md '((math . #f)))))
  (define explicit (styled-text-string (markdown->styled-text md '((math . #t)))))
  (define configured
    (styled-text-string
      (markdown->styled-text md '((math . ((scale . 2) (spacing . 4) (max-width . 100)
                                           (timeout . 20)
                                           (macros . (("\\half" . "\\frac{1}{2}")))))))))
  (define invalid-block
    (styled-text-string (markdown->styled-text (markdown "$$\n\\notacommand{\n$$\n" 'math))))
  (define invalid-inline
    (styled-text-string (markdown->styled-text (markdown "x $\\notacommand{$ y" 'math))))
  (list
    (not (has? typeset "pi r^2"))
    (not (has? typeset "int_0^1"))
    (has? code "$\\pi r^2$")
    (has? code "\\int_0^1 x\\,dx")
    (string=? typeset explicit)
    (string=? typeset configured)
    (has? invalid-block "Formula could not be rendered")
    (has? invalid-inline "$\\notacommand{$")
    (has? (markdown->html-doc md) "pi r^2"))
)

(
  "Markdown configuration with invalid math value"
  <error>
  (markdown->styled-text (markdown "text") '((math . 5)))
)

(
  "Markdown configuration with invalid math scale"
  <error>
  (markdown->styled-text (markdown "text") '((math . ((scale . 0)))))
)

(
  "Markdown configuration with unknown math key"
  <error>
  (markdown->styled-text (markdown "text") '((math . ((colour . red)))))
)

(
  "Markdown math constructor with invalid formula"
  <error>
  (math 1 #f #f)
)

(
  "Markdown math constructor with invalid source"
  <error>
  (math "x" #f 5)
)

(
  "Markdown math constructor with missing arguments"
  <error>
  (math "x" #f)
)

(
  "Markdown math block constructor with invalid formula"
  <error>
  (math-block 1)
)

(
  "Markdown syntax highlighting themes"
  (#t #t #t #t #t)
  (define md (markdown "```scheme\n(define x 1)\n```\n"))
  (define themes (syntax-highlighting-themes))
  (define (ansi theme)
    (markdown->formatted-string md 40 (list #f #f #f #f #f #f #f #f #f #f #f (list theme))))
  (define unhighlighted
    (markdown->formatted-string md 40 (list #f #f #f #f #f #f #f #f #f #f #f #f)))
  (list
    (and (member "xcode" themes) (member "markdownkit" themes) #t)
    (string? (syntax-highlighting-theme "xcode"))
    (not (syntax-highlighting-theme "no-such-theme"))
    (not (equal? (ansi "xcode") unhighlighted))
    (not (equal? (ansi "xcode") (ansi "github"))))
)
