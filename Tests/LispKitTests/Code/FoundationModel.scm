;;; FoundationModel.scm
;;; Regression test data
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
  (import (lispkit foundation-model)
          (lispkit thread future))
  (cond-expand
    ((library (lispkit foundation-model claude))
      (import (lispkit foundation-model claude)))
    (else))
  (cond-expand
    ((library (lispkit foundation-model gemini))
      (import (lispkit foundation-model gemini)))
    (else))
  (cond-expand
    ((library (lispkit foundation-model gpt))
      (import (lispkit foundation-model gpt)))
    (else))
  (cond-expand
    ((library (lispkit foundation-model tool))
      (import (lispkit foundation-model tool)))
    (else))
  ;; Returns #t if calling `thunk` raises an error
  (define (call-raises? thunk)
    (guard (x (#t #t)) (thunk) #f))
  ;; Returns the kind of the model error raised by calling `thunk`, or #f
  (define (call-model-error-kind thunk)
    (guard (x ((model-error? x) (model-error-kind x)) (#t #f)) (thunk) #f))
  ;; Evaluates to #t if evaluating `expr` raises an error
  (define-syntax raises?
    (syntax-rules ()
      ((_ expr) (call-raises? (lambda () expr)))))
  ;; Evaluates to the kind of the model error raised by `expr`
  (define-syntax model-error-raised
    (syntax-rules ()
      ((_ expr) (call-model-error-kind (lambda () expr)))))
  ;; Is the on-device language model available for the live tests?
  (define live? (language-model-available? (system-language-model)))
  ;; Evaluates `expr` only if the on-device model is available; otherwise returns `default`
  (define-syntax when-live
    (syntax-rules ()
      ((_ default expr ...) (if live? (begin expr ...) default))))
  ;; Converts a Scheme value into generated content and back, via a transcript segment
  (define (roundtrip value)
    (let ((entry (transcript-ref (make-transcript `(response (segments (structure "s" ,value)))) 0)))
      (caddr (car (cdr (assq 'segments (cdr entry)))))))
  #t
)

;;; Language models

(
  "Language model families"
  (#t system #f)
  (list (language-model? (system-language-model))
        (language-model-family (system-language-model 'content-tagging 'default))
        (language-model? "model"))
)

(
  "Language model availability"
  (#t #t)
  (list (symbol? (language-model-availability (system-language-model)))
        (boolean? (language-model-available? (system-language-model))))
)

(
  "Language model names"
  (#t #t)
  (let ((name (language-model-name (system-language-model))))
    (list (and (member name '("AFM 3 Core" "AFM 3 Core Advanced")) #t)
          (equal? name (language-model-name (system-language-model 'content-tagging 'default)))))
)

(
  "Families of language models"
  (#t #f #t #f #f #t "Apple" #f #f)
  (let ((families (available-language-model-families)))
    (list (and (memq 'system families) #t)
          (and (memq 'afm-cloud families) #t)
          (and (member (available-language-models 'system)
                      (list '() (list (language-model-name (system-language-model))))) #t)
          (available-language-models 'afm-cloud)
          (available-language-models 'unknown)
          (raises? (available-language-models "system"))
          (language-model-family-vendor 'system)
          (language-model-family-vendor 'afm-cloud)
          (language-model-family-vendor 'unknown)))
)

(
  "Unsupported model queries"
  (#f)
  (list (language-model-quota-usage (system-language-model)))
)

(
  "Private Cloud Compute is disabled by default"
  (#t #t #t)
  (list (raises? (afm-cloud-language-model))
        (raises? (limit-increase-suggestion? #f))
        (raises? (limit-increase-suggestion-show! #f)))
)

(
  "Data type support"
  (#t #t)
  (let ((m (system-language-model)))
    ;; Data attachments require macOS 27.2, which is reported as an unsupported request
    (define (supported? kind)
      (guard (x ((eq? (model-error-kind x) 'unsupported) #t))
        (boolean? (language-model-supports-data-type? m "public.png" kind))))
    (list (supported? 'attachment)
          (and (supported? 'entry)
               (raises? (language-model-supports-data-type? m "public.png" 'other)))))
)

(
  "Invalid use cases and guardrails"
  (#t #t)
  (list (raises? (system-language-model 'unknown))
        (raises? (system-language-model 'general 'none)))
)

;;; Generation schemas

(
  "Schema construction"
  (#t "Person" "Person")
  (define person
    (make-generation-schema
      '(object Person "A person"
         (name string "Full name")
         (age (integer (range 0 150)))
         (height number)
         (email (optional (string (pattern "[a-z]+@[a-z]+\\.com"))))
         (role (string (any-of "admin" "user")))
         (tags (array string 1 3)))))
  (list (generation-schema? person)
        (generation-schema-name person)
        (generation-schema-name (json->generation-schema (generation-schema->json person))))
)

(
  "Schema dependencies and references"
  ("Team" schema-error)
  (list (generation-schema-name
          (make-generation-schema '(object Team (members (array Person)) (lead (ref Person)))
                                  '(object Person (name string))))
        (model-error-raised (make-generation-schema '(object Team (members (array Person))))))
)

(
  "Enumerations and choices"
  ("Color" "Shape")
  (list (generation-schema-name (make-generation-schema '(enum Color "A color" ("red" "green"))))
        (generation-schema-name
          (make-generation-schema '(any-of Shape (ref Circle) (ref Square))
                                  '(object Circle (radius number))
                                  '(object Square (side number)))))
)

(
  "Invalid schemas"
  (#t #t #t #t #t #t #t)
  (list (raises? (make-generation-schema '(unknown Foo)))
        (raises? (make-generation-schema '(optional string)))
        (raises? (make-generation-schema '(string (pattern "[a-"))))
        (raises? (make-generation-schema '(integer (range 5 1))))
        (raises? (make-generation-schema '(enum Color ())))
        (raises? (make-generation-schema '(object Person (name))))
        (raises? (make-generation-schema 42)))
)

(
  "Schemas with explicit nil"
  ("P" "Q" #t)
  (list (generation-schema-name
          (make-generation-schema '(object P explicit-nil (name string) (nick (optional string)))))
        ;; `explicit-nil` is only special as a symbol; properties can be named like this
        (generation-schema-name (make-generation-schema '(object Q (explicit-nil string))))
        (raises? (make-generation-schema '(object R explicit-nil (name)))))
)

;;; Generated content

(
  "Generated content conversion"
  (((a . 1) (b . #(#t null "x")) (c . 2.5) (d)) () "sym" "c")
  (list (roundtrip '((a . 1) (b . #(#t null "x")) (c . 2.5) (d . ())))
        (roundtrip '())
        (roundtrip 'sym)
        (roundtrip #\c))
)

(
  "Generated content preserves property order"
  ((z y x))
  (list (map car (roundtrip '((z . 1) (y . 2) (x . 3)))))
)

;;; Prompts and attachments

(
  "Prompts"
  (#t #t #f #t)
  (list (prompt? (make-prompt "Hello" #\space '("nested" 42) 3.5))
        (prompt? (make-prompt))
        (prompt? "Hello")
        (raises? (make-prompt 'symbol)))
)

(
  "Image attachments"
  (#t #t)
  (list (attachment? (image-attachment "image.png" "photo"))
        (raises? (image-attachment 42)))
)

;;; Tools

(
  "Tool construction"
  ("getWeather" "Returns the weather" #t #t #t)
  (define weather
    (make-tool "getWeather" "Returns the weather"
               '(object WeatherArgs (city string "The city"))
               (lambda (city) "sunny")))
  (list (tool-name weather)
        (tool-description weather)
        (tool? weather)
        (generation-schema? (tool-parameters weather))
        (raises? (make-tool "" "empty" 'string (lambda (x) x))))
)

;;; Sessions and options

(
  "Session construction"
  (#t system #t 0)
  (define session (make-model-session #f (list weather) "You are a helpful assistant."))
  (list (model-session? session)
        (language-model-family (model-session-model session))
        (tool? (car (model-session-tools session)))
        (cdr (assq 'total-tokens (model-session-usage session))))
)

(
  "Session error policies"
  (revert-transcript preserve-transcript #f)
  (define s (make-model-session))
  (list (begin (model-session-error-policy-set! s 'revert-transcript) (model-session-error-policy s))
        (begin (model-session-error-policy-set! s 'preserve-transcript) (model-session-error-policy s))
        (begin (model-session-error-policy-set! s #f) (model-session-error-policy s)))
)

(
  "Invalid request options"
  (#t #t #t #t)
  (define s (make-model-session))
  (list (raises? (model-session-response s "Hi" #f '((temprature . 0.5))))
        (raises? (model-session-response s "Hi" #f '((sampling . (top-p 2.0)))))
        (raises? (model-session-response s "Hi" #f '((tool-calling . sometimes))))
        (raises? (model-session-stream s 'invalid-prompt)))
)

(
  "Rehydrating a session from a transcript"
  (3 #t)
  (define t (make-transcript '(instructions (segments "Be brief."))
                             '(prompt (segments "Hi"))
                             '(response (segments "Hello!"))))
  (define s (make-model-session #f '() t))
  (list (transcript-length (model-session-transcript s))
        (transcript? (cdr (assq 'history (model-session-properties s)))))
)

;;; Transcripts

(
  "Transcript construction and access"
  (4 (instructions prompt tool-calls response) "Hi" 2 3)
  (define t
    (make-transcript '(instructions (segments "Be brief."))
                     '((prompt (segments "Hi") (options (temperature . 0.5))))
                     '(tool-calls (calls ((tool . "getWeather") (arguments (city . "Zurich")))))
                     '(response (segments "Hello!") (metadata (source . "test")))))
  (list (transcript-length t)
        (map car (transcript->list t))
        (car (cdr (assq 'segments (cdr (transcript-ref t 1)))))
        (transcript-length (transcript-sub t 1 3))
        (transcript-length (transcript-append (transcript-sub t 0 1) (transcript-sub t 2))))
)

(
  "Transcript JSON round trip"
  (4 (instructions prompt tool-calls response) #t)
  (define t2 (json->transcript (transcript->json t)))
  (list (transcript-length t2)
        (map car (transcript->list t2))
        (equal? (transcript->list t) (transcript->list t2)))
)

(
  "Invalid transcripts"
  (#t #t #t)
  (list (raises? (make-transcript '(unknown (segments "x"))))
        (raises? (make-transcript '(prompt (segments 42.5 foo))))
        (raises? (json->transcript "{not json")))
)

(
  "Transcript metadata, signatures, and asset identifiers"
  (((k . "v")) #u8(1 2 3) ("a1" "a2") ((k . "v")) #u8(1 2 3) ("a1" "a2") #t #t)
  (define (call-meta t) (cdr (assq 'metadata (car (cdr (assq 'calls (cdr (transcript-ref t 0))))))))
  (define (signature t) (cdr (assq 'signature (cdr (transcript-ref t 1)))))
  (define (assets t) (cdr (assq 'asset-ids (cdr (transcript-ref t 2)))))
  (define t3
    (make-transcript
      '(tool-calls (calls ((id . "c1") (tool . "t") (arguments (a . 1)) (metadata (k . "v")))))
      '(reasoning (segments "thinking") (signature . #u8(1 2 3)))
      '(response (segments "done") (asset-ids "a1" "a2"))))
  (define t4 (json->transcript (transcript->json t3)))
  (list (call-meta t3) (signature t3) (assets t3)
        (call-meta t4) (signature t4) (assets t4)
        (raises? (make-transcript '(reasoning (segments "x") (signature . "no"))))
        (raises? (make-transcript '(response (segments "x") (asset-ids 42)))))
)

;;; Errors

(
  "Model errors"
  (#f #f #f)
  (list (model-error? (make-error "plain" '()))
        (model-error-kind (make-error "plain" '()))
        (model-error? 42))
)

(
  "Model error details"
  (("A" "a") ("Missing") #f #f #f #f)
  (define (call-model-error-details thunk)
    (guard (x ((model-error? x) (model-error-details x)) (#t #f)) (thunk) #f))
  (define dup (call-model-error-details
                (lambda () (make-generation-schema '(object A (a string) (a string))))))
  (define undefined (call-model-error-details
                      (lambda () (make-generation-schema '(object A (x (ref Missing)))))))
  (list (list (cdr (assq 'schema dup)) (cdr (assq 'property dup)))
        (cdr (assq 'references undefined))
        (model-error-details 42)
        (model-error-details (make-error "plain" '()))
        (model-error-explanation 42)
        (model-error-explanation (make-error "plain" '())))
)

;;; Claude

(
  "Claude models"
  (#t claude (low medium high xhigh max) #t #t)
  (cond-expand
    ((library (lispkit foundation-model claude))
      (let ((m (make-claude-model "claude-opus-5-5" "sk-test" '((effort . high)))))
        (list (and (member "claude-opus-5-5" (claude-models)) #t)
              (language-model-family m)
              (cdr (assq 'effort-levels (claude-model-capabilities m)))
              (raises? (make-claude-model "claude-haiku-4-5" "sk-test" '((effort . max))))
              (raises? (make-claude-model "claude-unknown" "sk-test")))))
    (else
      (list #t 'claude '(low medium high xhigh max) #t #t)))
)

(
  "Claude model queries"
  ("claude-haiku-4-5" "Anthropic" #t available #f #f #f #f unsupported)
  (cond-expand
    ((library (lispkit foundation-model claude))
      (let ((m (make-claude-model "claude-haiku-4-5" "sk-test")))
        (list (language-model-name m)
              (language-model-family-vendor (language-model-family m))
              (equal? (available-language-models 'claude) (claude-models))
              (language-model-availability m)
              (language-model-context-size m)
              (language-model-supported-languages m)
              (language-model-token-count m "Hello")
              (language-model-quota-usage m)
              (model-error-raised (language-model-supports-locale? m "de_CH")))))
    (else
      (list "claude-haiku-4-5" "Anthropic" #t 'available #f #f #f #f 'unsupported)))
)

;;; Gemini

(
  "Gemini models"
  (#t gemini (low medium high xhigh) unsupported-thinking-level #t #t #t #f (low high))
  (cond-expand
    ((library (lispkit foundation-model gemini))
      (let ((m (make-gemini-model "gemini-4-neon" "test-key"
                                  '((thinking-level . xhigh) (thought-summaries . #f)
                                    (built-in-tools google-search code-execution)))))
        (list (and (member "gemini-3.8-flash" (gemini-models)) #t)
              (language-model-family m)
              (cdr (assq 'thinking-levels (gemini-model-capabilities m)))
              (model-error-raised
                (make-gemini-model "gemini-3.8-flash" "test-key" '((thinking-level . xhigh))))
              (raises? (make-gemini-model "gemini-unknown" "test-key"))
              (raises? (make-gemini-model "gemini-3.8-flash" "test-key" '((bogus . 1))))
              (and (language-model? (make-gemini-model "gemini-3.8-flash" "test-key"
                                      '((retries . 4) (replay-earlier-thoughts . #t))))
                   (language-model? (make-gemini-model "gemini-3.8-flash" "test-key" '((retries . #f))))
                   (raises? (make-gemini-model "gemini-3.8-flash" "test-key" '((retries . -1)))))
              (gemini-model-capabilities "gemini-unknown")
              (cdr (assq 'thinking-levels
                         (gemini-model-capabilities
                           (make-gemini-model "gemini-custom" "test-key"
                             '((capabilities . ((thinking-levels high low)))))))))))
    (else
      (list #t 'gemini '(low medium high xhigh) 'unsupported-thinking-level #t #t #t #f '(low high))))
)

(
  "Gemini model queries"
  ("gemini-3.8-flash" "Google" #t available #f #f #f unsupported)
  (cond-expand
    ((library (lispkit foundation-model gemini))
      ;; The context size is fetched from the API and not tested here
      (let ((m (make-gemini-model "gemini-3.8-flash" "test-key")))
        (list (language-model-name m)
              (language-model-family-vendor (language-model-family m))
              (equal? (available-language-models 'gemini) (gemini-models))
              (language-model-availability m)
              (language-model-supported-languages m)
              (language-model-token-count m "Hello")
              (language-model-quota-usage m)
              (model-error-raised (language-model-supports-locale? m "de_CH")))))
    (else
      (list "gemini-3.8-flash" "Google" #t 'available #f #f #f 'unsupported)))
)

;;; GPT

(
  "GPT models"
  (#t gpt (low medium high xhigh max) medium unsupported-reasoning-effort #t #t #f (none high) none #t #t)
  (cond-expand
    ((library (lispkit foundation-model gpt))
      (let ((m (make-gpt-model "gpt-6-sol" '(api-key "sk-test" "org-1" #f)
                               '((reasoning-effort . xhigh) (reasoning-summaries . #f)
                                 (built-in-tools web-search code-interpreter)
                                 (stateful . #t)))))
        (list (and (member "gpt-5.5" (gpt-models)) #t)
              (language-model-family m)
              (cdr (assq 'reasoning-efforts (gpt-model-capabilities "gpt-6.2-sol")))
              (cdr (assq 'default-reasoning-effort (gpt-model-capabilities m)))
              (model-error-raised
                (make-gpt-model "gpt-5" "sk-test" '((reasoning-effort . xhigh))))
              (raises? (make-gpt-model "gpt-unknown" "sk-test"))
              (raises? (make-gpt-model "gpt-5.5" "sk-test" '((bogus . 1))))
              (gpt-model-capabilities "gpt-unknown")
              (cdr (assq 'reasoning-efforts
                         (gpt-model-capabilities
                           (make-gpt-model "gpt-custom" "sk-test"
                             '((capabilities . ((reasoning-efforts high none) (default-reasoning-effort . none))))))))
              (cdr (assq 'default-reasoning-effort (gpt-model-capabilities "gpt-5.4-mini")))
              (raises? (make-gpt-model "gpt-5.5" '(proxied 42)))
              (and (language-model? (make-gpt-model "gpt-6-luna" "sk-test" '((reasoning-effort . max) (retries . #f))))
                   (raises? (make-gpt-model "gpt-5.5" "sk-test" '((retries . -1))))))))
    (else
      (list #t 'gpt '(low medium high xhigh max) 'medium 'unsupported-reasoning-effort #t #t #f '(none high) 'none #t #t)))
)

(
  "GPT model queries"
  ("gpt-5.5" "OpenAI" #t available #f #f #f unsupported ())
  (cond-expand
    ((library (lispkit foundation-model gpt))
      ;; Token counts are computed by the API and not tested here
      (let* ((m (make-gpt-model "gpt-5.5" "sk-test"))
             (session (make-model-session m)))
        (list (language-model-name m)
              (language-model-family-vendor (language-model-family m))
              (equal? (available-language-models 'gpt) (gpt-models))
              (language-model-availability m)
              (language-model-context-size m)
              (language-model-supported-languages m)
              (language-model-quota-usage m)
              (model-error-raised (language-model-supports-locale? m "de_CH"))
              (gpt-tool-activities (model-session-transcript session)))))
    (else
      (list "gpt-5.5" "OpenAI" #t 'available #f #f #f 'unsupported '())))
)

;;; Live tests (they only run if the on-device model is available)

(
  "Text response"
  #t
  (define live-session (make-model-session #f '() "Answer in one short sentence."))
  (when-live #t
    (let ((r (future-get (model-session-response live-session "What is the capital of France?"))))
      (and (model-response? r)
           (string? (model-response-content r))
           (model-response-complete? r)
           (> (cdr (assq 'total-tokens (model-response-usage r))) 0)
           (transcript? (model-response-entries r)))))
)

(
  "Guided generation"
  ("Ada" #t)
  (when-live (list "Ada" #t)
    (let ((p (model-response-content
               (future-get (model-session-response live-session "Invent a person named Ada."
                             '(object Person (name string) (age (integer (range 1 99)))))))))
      (list (cdr (assq 'name p))
            (exact-integer? (cdr (assq 'age p))))))
)

(
  "Guided generation with explicit nil"
  (#t #t)
  (when-live (list #t #t)
    (let ((p (model-response-content
               (future-get (model-session-response
                             (make-model-session)
                             "Describe a person named Ada, who has no nickname."
                             '(object Person explicit-nil (name string) (nickname (optional string)))
                             '((sampling . greedy)))))))
      (list (string? (cdr (assq 'name p)))
            (and (assq 'nickname p) #t))))
)

(
  "Tool calls"
  (#t #t)
  (define tool-calls 0)
  (define temperature
    (make-tool "getTemperature" "Returns the current temperature in a city in degrees Celsius"
               '(object TemperatureArgs (city string "The name of the city"))
               (lambda (city) (set! tool-calls (+ tool-calls 1)) `((city . ,city) (celsius . 17)))))
  (when-live (list #t #t)
    (let* ((s (make-model-session #f (list temperature)
                                  "Always use the getTemperature tool for questions about temperatures."))
           (r (future-get (model-session-response s "What is the temperature in Zurich?"))))
      (list (> tool-calls 0)
            (string? (model-response-content r)))))
)

(
  "Errors raised by tools are passed through"
  ("stock service down" oops)
  (define (stock-tool proc)
    (make-tool "getStockPrice" "Returns the price of a stock"
               '(object StockArgs (ticker string)) proc))
  (when-live (list "stock service down" 'oops)
    (list (guard (e ((error-object? e) (error-object-message e)))
            (future-get (model-session-response
                          (make-model-session #f (list (stock-tool (lambda (t) (error "stock service down" t))))
                                              "Always use the getStockPrice tool for stock questions.")
                          "What is the price of AAPL?")))
          (guard (e ((symbol? e) e))
            (future-get (model-session-response
                          (make-model-session #f (list (stock-tool (lambda (t) (raise 'oops))))
                                              "Always use the getStockPrice tool for stock questions.")
                          "What is the price of AAPL?")))))
)

(
  "Streaming"
  (#t #t #t)
  (when-live (list #t #t #t)
    (let* ((stream (model-session-stream live-session "Count from one to five in words."))
           (snapshots 0))
      (response-stream-for-each (lambda (snap) (set! snapshots (+ snapshots 1))) stream)
      (list (> snapshots 0)
            (response-stream-done? stream)
            (string? (model-response-content (future-get (response-stream-result stream)))))))
)

(
  "Concurrent requests and cancellation"
  (concurrent-requests cancelled #f)
  (when-live (list 'concurrent-requests 'cancelled #f)
    (let* ((s (make-model-session))
           (f (model-session-response s "Write a 300 word essay about rivers.")))
      (list (model-error-raised (model-session-response s "Another request"))
            (begin (model-session-cancel! s)
                   (model-error-raised (future-get f)))
            (model-session-responding? s))))
)

;;; Native tools

(
  "Vision tools"
  (#t #t "getText" "readBarcodes" "read-text" "Reads the text of an image." #f #t #t)
  (cond-expand
    ((library (lispkit foundation-model tool))
      (let ((ocr (make-ocr-tool))
            (barcodes (make-barcode-reader-tool)))
        (list (tool? ocr)
              (tool? barcodes)
              (tool-name ocr)
              (tool-name barcodes)
              (tool-name (make-ocr-tool "read-text"))
              (tool-description (make-ocr-tool #f "Reads the text of an image."))
              (spotlight-search-tool? ocr)
              (generation-schema? (tool-parameters barcodes))
              (model-session? (make-model-session #f (list ocr barcodes))))))
    (else
      '(#t #t "getText" "readBarcodes" "read-text" "Reads the text of an image." #f #t #t)))
)

(
  "Spotlight search tool"
  (#t #t "spotlight_search" "search" () () #t #t #t)
  (cond-expand
    ((library (lispkit foundation-model tool))
      (let ((spotlight (make-spotlight-search-tool)))
        (list (tool? spotlight)
              (spotlight-search-tool? spotlight)
              (tool-name spotlight)
              (tool-name (make-spotlight-search-tool '((name . "search"))))
              (spotlight-search-results spotlight)
              (begin (spotlight-search-results-clear! spotlight)
                     (spotlight-search-results spotlight))
              (and (memq 'display-name (spotlight-search-attributes))
                   (memq 'content-modification-date (spotlight-search-attributes))
                   #t)
              (model-session? (make-model-session #f (list spotlight (make-ocr-tool))))
              (raises? (spotlight-search-results (make-ocr-tool))))))
    (else
      '(#t #t "spotlight_search" "search" () () #t #t #t)))
)

(
  "Spotlight search tool configurations"
  (#t #t #t #t #t #t #t #t #t #t)
  (cond-expand
    ((library (lispkit foundation-model tool))
      (list (spotlight-search-tool?
              (make-spotlight-search-tool
                '((sources core-spotlight
                           (files (scopes "/tmp") (max-results . 10)
                                  (fetch-attributes display-name "kMDItemNamedLocation")))
                  (guide focused (documents (authors author-names) (modified content-modification-date)))
                  (format . compact)
                  (max-response-size . 2000)
                  (user (display-name . "Jane Appleseed") (email-addresses "jane@example.com")))))
            (spotlight-search-tool?
              (make-spotlight-search-tool
                '((guide dynamic text-match dates (attributes title)) (user . "Jane"))))
            (spotlight-search-tool? (make-spotlight-search-tool '((guide . complete))))
            (raises? (make-spotlight-search-tool '((colour . red))))
            (raises? (make-spotlight-search-tool '((sources web))))
            (raises? (make-spotlight-search-tool '((sources (files (depth . 3))))))
            (raises? (make-spotlight-search-tool '((guide focused nowhere))))
            (raises? (make-spotlight-search-tool '((guide focused (documents (colour title))))))
            (raises? (make-spotlight-search-tool '((guide dynamic telepathy))))
            (raises? (make-spotlight-search-tool '((sources (files (fetch-attributes no-such))))))))
    (else
      '(#t #t #t #t #t #t #t #t #t #t)))
)
