;;; Markdown.scm
;;; Regression test data
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2019 ObjectHub. All rights reserved.
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
                       (bullet #\- #f (list (paragraph (list (text "Alpha")))))
                       (bullet #\- #t (list (paragraph (list (text "Beta"))))))))))
  (define list2-doc (document (list
                      (list-items 1 #t (list
                        (ordered 1 #\. #f (list (paragraph (list (text "Eins")))))
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
                                                     "http://lisppad.objecthub.net"
                                                     "Mac App"))))))
  (define md6 (document (list (paragraph (list (html "a href=\"http://objecthub.net\"")
                                               (text "homepage")
                                               (html "/a"))))))
  (list
    (markdown=? md1 (markdown "text `code` rest"))
    (markdown=? md2 (markdown "*one two* three"))
    (markdown=? md3 (markdown "**one two** three"))
    (markdown=? md4 (markdown "**one _two_** three"))
    (markdown=? md5 (markdown "[LispPad](http://lisppad.objecthub.net \"Mac App\")"))
    (markdown=? md6 (markdown "<a href=\"http://objecthub.net\">homepage</a>")))
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
              (bullet #\: #t (list
                (paragraph (list (text "programs used by a")))
                (list-items #f #t (list
                  (bullet #\- #t (list (paragraph (list (text "device")))))
                  (bullet #\- #t (list (paragraph (list (emph (list (text "computer")))))))))
                (paragraph (list (text "and other systems")))))
              (bullet #\: #f (list
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
