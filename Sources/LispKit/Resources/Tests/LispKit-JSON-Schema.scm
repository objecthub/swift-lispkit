;;; LISPKIT JSON SCHEMA REGRESSION TEST SUITE
;;;
;;; This is the test suite for library `(lispkit json schema)`.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2024 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed under the
;;; License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;;; either express or implied. See the License for the specific language governing permissions
;;; and limitations under the License.

(import (lispkit base)
        (lispkit test)
        (lispkit json)
        (lispkit json schema))

(define test-dir (asset-file-path "JSON/Tests/2020-12"))

(define (make-test-registry)
  (let ((registry (make-schema-registry)))
    (schema-registry-add-source!
      (asset-file-path "JSON/Schema/2020-12")
      "https://json-schema.org/draft/2020-12/"
      registry)
    registry
  ))

(define (execute-tests dir)
  (do ((filenames (sort string<? (directory-list dir)) (cdr filenames)))
      ((null? filenames))
    (cond
      ((string-suffix? (car filenames) ".json")
        (test-begin (string-append "LispKit JSON Schema: " (car filenames)))
        (let ((registry (make-test-registry)))
          (json-for-each-element
            (lambda (test)
              (let ((description (json->value (json-ref test 'description)))
                    (schema (json-schema (json-ref test 'schema)))
                    (tests (json-ref test 'tests))
                    (ignore (and (json-ref test 'ignore)
                                 (json->value (json-ref test 'ignore)))))
                (if (not ignore)
                  (json-for-each-element
                    (lambda (case)
                      (let ((description (json->value (json-ref case 'description)))
                            (data (json-ref case 'data))
                            (valid (json->value (json-ref case 'valid)))
                            (ignore (and (json-ref case 'ignore)
                                         (json->value (json-ref case 'ignore)))))
                        (if (not ignore)
                            (test-equal description valid (json-valid? data schema registry)))
                      )
                    )
                    tests))
              )
            )
            (load-json (file-path (car filenames) test-dir))))
        (test-end)))))

(test-begin "LispKit JSON Schema")

(execute-tests test-dir)

(test-end)
