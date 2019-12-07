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
  "Markdown basics"
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
