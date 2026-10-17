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
    (not (markdown=? md1 (markdown "~one~ two" #t)))
    (and (markdown-inline? (underline '())) (markdown-inline? (strikethrough '())))
    (equal? (markdown->html md1) "<p><u>one</u> two</p>\n")
    (equal? (markdown->html md2) "<p><del>one</del> two</p>\n")
    (equal? (markdown->html md4) "<p><strong>a <del><u>b</u></del></strong> c</p>\n")
    (equal? (markdown->sxml md1) '((p (u ("one")) " two")))
    (equal? (markdown->sxml md2) '((p (del ("one")) " two")))
    (equal? (markdown->raw-string md4) "a b c"))
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
    (not (markdown=? doc1 (markdown "- [ ] open\n- [x] done" #t)))
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
    (equal? (markdown->string doc1 40) "  [ ] open\n  [x] done"))
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
    (markdown->string md 40)
    (markdown->string md 40 #t)
    (markdown->string md 40 (list #f #f #f #f #f #f #f #f #f #f #f #t 'red 'aqua)))
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
  (#t #t #t #t #t #t #t #t #t #t #t #t #t #t)
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
    (string? (markdown->html-doc md #f))
    (string? (markdown->html-doc md '((syntax-highlighting . #f) (tight-lists . #t)
                                      (version . pre-os26)))))
)

(
  "Markdown styled text"
  (#t #t #t #t #t #t)
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
    (styled-text? (markdown->styled-text md '((version . pre-os26) (tight-lists . #t)))))
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
  "Markdown configuration with invalid version"
  <error>
  (markdown->styled-text (markdown "text") '((version . future)))
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
  "Markdown syntax highlighting themes"
  (#t #t #t #t #t)
  (define md (markdown "```scheme\n(define x 1)\n```\n"))
  (define themes (syntax-highlighting-themes))
  (define (ansi theme)
    (markdown->string md 40 (list #f #f #f #f #f #f #f #f #f #f #f (list theme))))
  (define unhighlighted
    (markdown->string md 40 (list #f #f #f #f #f #f #f #f #f #f #f #f)))
  (list
    (and (member "xcode" themes) (member "markdownkit" themes) #t)
    (string? (syntax-highlighting-theme "xcode"))
    (not (syntax-highlighting-theme "no-such-theme"))
    (not (equal? (ansi "xcode") unhighlighted))
    (not (equal? (ansi "xcode") (ansi "github"))))
)
