;;; SystemSearch.scm
;;; Regression test data for library (lispkit system search)
;;;
;;; The tests only cover the query compiler and the attribute tables since search results
;;; depend on the content of the Spotlight index of the machine running the tests.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 ObjectHub. All rights reserved.
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
  "Imports and helpers"
  #t
  (import (lispkit system search)
          (lispkit date-time))
  ;; Returns #t if calling `thunk` raises an error
  (define (call-raises? thunk)
    (guard (x (#t #t)) (thunk) #f))
  (define-syntax raises?
    (syntax-rules ()
      ((_ expr) (call-raises? (lambda () expr)))))
  (define qs spotlight-query-string)
  #t
)

(
  "Text queries"
  ("** == \"invoice*\"cdw"
   "(** == \"invoice*\"cdw && ** == \"2024*\"cdw)"
   "(** == \"a*\"cdw && ** == \"b*\"cdw && ** == \"c*\"cdw)")
  (list (qs "invoice")
        (qs '(text "invoice 2024"))
        (qs '(text "a" " b  c")))
)

(
  "Name, extension, type, and tag queries"
  ("kMDItemFSName == \"*.scm\"cd"
   "(kMDItemFSName == \"*.scm\"cd || kMDItemFSName == \"*.sld\"cd)"
   "kMDItemContentTypeTree == \"com.adobe.pdf\""
   "(kMDItemContentTypeTree == \"public.image\" || kMDItemContentTypeTree == \"net.example.x\")"
   "kMDItemUserTags == \"Red\"cd")
  (list (qs '(name "*.scm"))
        (qs '(extension "scm" ".sld"))
        (qs '(type pdf))
        (qs '(type image "net.example.x"))
        (qs '(tag "Red")))
)

(
  "Comparisons"
  ("kMDItemFSSize > 1000"
   "kMDItemPixelWidth <= 2.5"
   "kMDItemTitle == \"a\\\"b\\*c\""
   "kMDItemFSInvisible != 1"
   "kMDItemContentModificationDate >= $time.iso(2025-01-02T03:04:05Z)"
   "(kMDItemFSSize >= 10 && kMDItemFSSize <= 20)"
   "(kMDItemKind == \"PDF\" || kMDItemKind == \"Folder\")"
   "kMDItemAuthors == *")
  (list (qs '(> size 1000))
        (qs '(<= pixel-width 2.5))
        (qs '(= title "a\"b*c"))
        (qs '(!= fs-invisible #t))
        (qs `(>= modified ,(date-time "UTC" 2025 1 2 3 4 5)))
        (qs '(between size 10 20))
        (qs '(in kind "PDF" "Folder"))
        (qs '(exists authors)))
)

(
  "String matching"
  ("kMDItemTitle == \"*report*\"cd"
   "kMDItemTitle == \"Rep*\"d"
   "kMDItemTitle == \"*\\*.txt\"c"
   "kMDItemTitle == \"a*b\""
   "kMDItemTitle == \"budget*\"cdw")
  (list (qs '(contains title "report"))
        (qs '(prefix title "Rep" case-sensitive))
        (qs '(suffix title "*.txt" diacritic-sensitive))
        (qs '(like title "a*b" case-sensitive diacritic-sensitive))
        (qs '(like title "budget*" word)))
)

(
  "Logical connectives"
  ("((kMDItemFSName == \"*.scm\"cd || kMDItemFSName == \"*.sld\"cd) && kMDItemFSSize < 100)"
   "!(kMDItemFSName == \"*.tmp\"cd)"
   "!(kMDItemFSSize > 1 && kMDItemFSSize < 5)"
   "(kMDItemFSSize > 1 && kMDItemFSSize < 5 && kMDItemFSSize != 3)"
   "kMDItemFSSize > 1"
   "((kMDItemFSName == 'x') || kMDItemFSSize > 2)"
   "kMDItemContentType == *")
  (list (qs '(and (or (extension "scm") (extension "sld")) (< size 100)))
        (qs '(not (name "*.tmp")))
        (qs '(not (and (> size 1) (< size 5))))
        (qs '(and (and (> size 1) (< size 5)) (!= size 3)))
        (qs '(and (> size 1)))
        (qs '(or (raw "kMDItemFSName == 'x'") (> size 2)))
        (qs #t))
)

(
  "Raw attribute keys and relative dates"
  (#t #t #t #t)
  (list (string=? (qs '(= "com_example_rating" 5)) "com_example_rating == 5")
        (string-prefix? (qs '(>= last-used (days-ago 7))) "kMDItemLastUsedDate >= $time.iso(")
        (string-prefix? (qs '(during created this-week))
                        "(kMDItemContentCreationDate >= $time.iso(")
        (string-prefix? (qs '(during created (last-days 3)))
                        "(kMDItemContentCreationDate >= $time.iso("))
)

(
  "Fixed time ranges"
  "(kMDItemDateAdded >= $time.iso(2025-01-01T00:00:00Z) && kMDItemDateAdded < $time.iso(2025-02-01T00:00:00Z))"
  (qs `(during added (,(date-time "UTC" 2025 1 1) ,(date-time "UTC" 2025 2 1))))
)

(
  "Queries for indexed items"
  ("(displayName == \"*.scm\"cd || displayName == \"*.sld\"cd)"
   "(title == \"*quokka*\"cd && domainIdentifier == \"notes\")"
   "keywords == \"alpha\"cd"
   "contentTypeTree == \"public.text\""
   "(rating >= 3 && contentDescription == \"*fish*\"cd)")
  (list (qs '(extension "scm" "sld") 'items)
        (qs '(and (contains title "quokka") (= domain "notes")) 'items)
        (qs '(tag "alpha") 'items)
        (qs '(type text) 'items)
        (qs '(and (>= rating 3) (contains description "fish")) 'items))
)

(
  "Attributes"
  (#t #t #t #t "kMDItemFSName" "kMDItemPixelWidth" "kMDItemFSName" "displayName" "ISOSpeed"
   "uniqueIdentifier" "raw")
  (list (and (memq 'pixel-width (spotlight-attributes)) #t)
        (and (memq 'fs-name (spotlight-attributes 'files)) #t)
        (and (memq 'content-description (spotlight-attributes 'items)) #t)
        (and (memq 'id (spotlight-attributes 'items)) #t)
        (spotlight-attribute-key 'name)
        (spotlight-attribute-key 'pixel-width)
        (spotlight-attribute-key 'fs-name 'files)
        (spotlight-attribute-key 'name 'items)
        (spotlight-attribute-key 'iso-speed 'items)
        (spotlight-attribute-key 'id 'items)
        (spotlight-attribute-key "raw"))
)

(
  "Malformed queries"
  (#t #t #t #t #t #t #t #t #t #t)
  (list (raises? (qs '(foo "bar")))
        (raises? (qs '(= unknown-attribute 1)))
        (raises? (qs '(= size)))
        (raises? (qs '(and)))
        (raises? (qs '(type no-such-kind)))
        (raises? (qs '(like title "x" sideways)))
        (raises? (qs '(during modified next-century)))
        (raises? (qs '(>= modified (eons-ago 3))))
        (raises? (qs '(text "   ")))
        (raises? (qs 42)))
)

(
  "Malformed searches"
  (#t #t #t #t)
  (list (raises? (spotlight-search "x" '(size bogus)))
        (raises? (spotlight-search "x" #f '((scope nowhere))))
        (raises? (spotlight-search "x" #f '((sort (size sideways)))))
        (raises? (spotlight-search-items "x" #f '((scope home)))))
)
