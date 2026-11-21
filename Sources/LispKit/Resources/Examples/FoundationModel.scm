;;; Talk to language models via sessions, schemas, tools, and streams
;;;
;;; This code showcases the API provided by library `(lispkit foundation-model)`. It
;;; builds a small "travel assistant" on top of Apple's on-device language model:
;;; it asks plain questions, generates structured itineraries guided by a schema,
;;; lets the model look up information via tools implemented as Scheme procedures,
;;; streams a response while it is being generated, and saves and restores the
;;; conversation via its transcript. Finally, it shows how to use a Claude model
;;; via library `(lispkit foundation-model claude)`, a Gemini model via library
;;; `(lispkit foundation-model gemini)`, and a GPT model via library
;;; `(lispkit foundation-model gpt)` if API keys are available: a tool call, a
;;; follow-up question, guided generation, and streaming, plus Google Search for
;;; Gemini and a stored conversation with web search for GPT.
;;;
;;; The code requires macOS 27 or iOS 27 and a device on which Apple Intelligence
;;; is enabled.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
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

(import (lispkit base)
        (lispkit foundation-model)
        (lispkit thread future))

;;; For using the Claude demo, an Anthropic API key is needed. It can be
;;; set here or defined via environment variable "ANTHROPIC_API_KEY" or set
;;; via `(set-anthropic-api-key! "...")` which stores the key in the system
;;; keychain. Similarly, the Gemini demo needs a Gemini API key, which can
;;; be set here, via environment variable "GEMINI_API_KEY", or via
;;; `(set-gemini-api-key! "...")`, and the GPT demo needs an OpenAI API key,
;;; which can be set here, via environment variable "OPENAI_API_KEY", or via
;;; `(set-openai-api-key! "...")`.

(define anthropic-api-key #f)
(define gemini-api-key #f)
(define openai-api-key #f)

;;; Checking the model

(display "===================================")
(newline)
(define model (system-language-model))
(display "MODEL: ")
(display (language-model-name model))
(display ", availability: ")
(display (language-model-availability model))
(display ", context size: ")
(display (language-model-context-size model))
(newline)

(if (not (language-model-available? model))
    (begin
      (display "The on-device language model is not available.")
      (newline)
      (exit)))

;;; Asking questions

;; `model-session-response` returns a future; `future-get` waits for the response.
(define (ask session prompt . args)
  (model-response-content (future-get (apply model-session-response session prompt args))))

(define assistant
  (make-model-session model '() "You are a concise travel assistant. Answer in at most two sentences."))

(display "Q: What is Kyoto famous for?\nA: ")
(display (ask assistant "What is Kyoto famous for?"))
(newline)

;; The session remembers the conversation.
(display "Q: And what is the best season to visit?\nA: ")
(display (ask assistant "And what is the best season to visit?"))
(newline)

;;; Guided generation

;; A schema describes the structure of the generated content. The result is an
;; association list whose values are converted like JSON values: arrays become
;; vectors and nested objects become association lists.
(define itinerary-schema
  (make-generation-schema
    '(object Itinerary "A travel itinerary"
       (city string "The destination")
       (days (array Day 2 3) "One entry per day"))
    '(object Day
       (title string "A short title for the day")
       (activities (array string 2 3))
       (budget (integer (range 20 500)) "Budget in US dollars"))))

(define itinerary
  (ask assistant "Plan a short trip to Kyoto." itinerary-schema '((temperature . 0.3))))

(define (show-itinerary itinerary)
  (display "Itinerary for ")
  (display (cdr (assq 'city itinerary)))
  (newline)
  (vector-for-each
    (lambda (day)
      (display "  ")
      (display (cdr (assq 'title day)))
      (display " ($")
      (display (cdr (assq 'budget day)))
      (display "): ")
      (display (cdr (assq 'activities day)))
      (newline))
    (cdr (assq 'days itinerary))))

(show-itinerary itinerary)

;;; Tools

;; Tools are Scheme procedures. The model decides when to call them. The arguments
;; of the procedure correspond to the properties of the parameter schema.
(define exchange-rates '(("EUR" . 0.92) ("JPY" . 152.0) ("CHF" . 0.88)))

(define convert-tool
  (make-tool "convertCurrency"
             "Converts an amount of US dollars into another currency"
             '(object ConversionArgs
                (amount number "Amount in US dollars")
                (currency (string (any-of "EUR" "JPY" "CHF")) "Target currency"))
             (lambda (amount currency)
               (display "  [tool] converting ")
               (display amount)
               (display " USD to ")
               (display currency)
               (newline)
               `((amount . ,(* amount (cdr (assoc currency exchange-rates))))
                 (currency . ,currency)))))

(define banker
  (make-model-session model (list convert-tool)
    "Use the convertCurrency tool to convert amounts of money."))

(display "Q: How much is 250 US dollars in Japanese yen?\nA: ")
(display (ask banker "How much is 250 US dollars in Japanese yen?"))
(newline)

;;; Streaming

;; A response stream provides cumulative snapshots of the response while it is
;; being generated. `stream-answer` prints the part of each snapshot that is new.
(define (stream-answer session prompt . args)
  (let ((stream (apply model-session-stream session prompt args))
        (shown 0))
    (response-stream-for-each
      (lambda (snapshot)
        (let ((text (model-response-content snapshot)))
          (display (substring text shown (string-length text)))
          (set! shown (string-length text))))
      stream)))

(display "Streaming: ")
(stream-answer assistant "Describe a morning walk through Gion in three sentences.")
(newline)

;;; Transcripts and usage

(define usage (model-session-usage assistant))
(display "Tokens used by the assistant: ")
(display (cdr (assq 'total-tokens usage)))
(newline)

;; A transcript can be saved as JSON and used to continue the conversation later.
(define saved (transcript->json (model-session-transcript assistant)))
(define restored (make-model-session model '() (json->transcript saved)))
(display "Restored a conversation with ")
(display (transcript-length (model-session-transcript restored)))
(display " entries: ")
(display (map car (transcript->list (model-session-transcript restored))))
(newline)
(display "Q: Which city did we talk about?\nA: ")
(display (cdr (assq
                'city
                (ask restored
                     "Which city did we talk about?"
                     (make-generation-schema
                       '(object CityAnswer "A name of a city"
                          (city string "Only the name of the city. No other words.")))))))
(newline)

;;; Claude

;; Claude models are available via library `(lispkit foundation-model claude)`. A
;; Claude session works exactly like a session for the on-device model. The code
;; below reuses the currency tool, the itinerary schema, and the helpers from above.
(cond-expand
  ((library (lispkit foundation-model claude))
    (import (lispkit foundation-model claude)
            (lispkit system util))
    (define-values (get-anthropic-api-key set-anthropic-api-key!)
      (manage-credential anthropic-api-key
                         "ANTHROPIC_API_KEY"
                         "FoundationModelsDemo"
                         "anthropic-api-key"))
    (display "===================================")
    (newline)
    (if-let* ((apikey (get-anthropic-api-key))
              (claude-model (make-claude-model "claude-sonnet-5-5" apikey))
              (claude (make-model-session
                        claude-model
                        (list convert-tool)
                        (string-append "You are a concise travel assistant. "
                                       "Use the convertCurrency tool to convert amounts."))))
      (begin
        (display "MODEL: ")
        (display (language-model-name claude-model))
        (display ", availability: ")
        (display (language-model-availability claude-model))
        (display ", context size: ")
        (display (language-model-context-size claude-model))
        (newline)
        ;; A tool call
        (display "Q: How much is 80 US dollars in Swiss francs?\nClaude: ")
        (display (ask claude "How much is 80 US dollars in Swiss francs?"))
        (newline)
        ;; The session remembers the conversation, here the amount from above
        (display "Q: And how much is that in euros?\nClaude: ")
        (display (ask claude "And how much is that in euros?"))
        (newline)
        ;; Guided generation with a schema, as before. A new session is used, so that
        ;; the answer is independent of the conversation about currencies above.
        (display "Q: Plan a short trip to Lisbon.\n")
        (show-itinerary
          (ask (make-model-session claude-model '() "You are a concise travel assistant.")
               "Plan a short trip to Lisbon."
               itinerary-schema))
        ;; Streaming
        (display "Q: Describe an evening in the Alfama district in three sentences.\n")
        (display "Claude (streaming): ")
        (stream-answer claude "Describe an evening in the Alfama district in three sentences.")
        (newline)
        ;; Usage
        (let ((usage (model-session-usage claude)))
          (display "Tokens used by Claude: ")
          (display (cdr (assq 'input-tokens usage)))
          (display " input, ")
          (display (cdr (assq 'output-tokens usage)))
          (display " output")
          (newline)))
      (display "Anthropic API key not available.")))
  (else
    (display "Claude integration unavailable.")))
(newline)

;;; Gemini

;; Gemini models are available via library `(lispkit foundation-model gemini)`. As with
;; Claude, a Gemini session works exactly like a session for the on-device model.
(cond-expand
  ((library (lispkit foundation-model gemini))
    (import (lispkit foundation-model gemini)
            (lispkit system util))
    (define-values (get-gemini-api-key set-gemini-api-key!)
      (manage-credential gemini-api-key
                         "GEMINI_API_KEY"
                         "FoundationModelsDemo"
                         "gemini-api-key"))
    (display "===================================")
    (newline)
    (if-let* ((apikey (get-gemini-api-key))
              (gemini-model (make-gemini-model "gemini-3.8-flash" apikey))
              (gemini (make-model-session
                        gemini-model
                        (list convert-tool)
                        (string-append "You are a concise travel assistant. "
                                       "Use the convertCurrency tool to convert amounts."))))
      (begin
        (display "MODEL: ")
        (display (language-model-name gemini-model))
        (display ", availability: ")
        (display (language-model-availability gemini-model))
        (display ", context size: ")
        (display (language-model-context-size gemini-model))
        (newline)
        ;; A tool call
        (display "Q: How much is 120 US dollars in Japanese yen?\nGemini: ")
        (display (ask gemini "How much is 120 US dollars in Japanese yen?"))
        (newline)
        ;; The session remembers the conversation, here the amount from above
        (display "Q: And how much is that in Swiss francs?\nGemini: ")
        (display (ask gemini "And how much is that in Swiss francs?"))
        (newline)
        ;; Guided generation with the itinerary schema, in a new session
        (display "Q: Plan a short trip to Porto.\n")
        (show-itinerary
          (ask (make-model-session gemini-model '() "You are a concise travel assistant.")
               "Plan a short trip to Porto."
               itinerary-schema))
        ;; Streaming
        (display "Q: Describe a morning at the Ribeira in Porto in three sentences.\n")
        (display "Gemini (streaming): ")
        (stream-answer gemini "Describe a morning at the Ribeira in Porto in three sentences.")
        (newline)
        ;; Google Search runs on Google's side; the transcript records the searches
        (let ((searcher (make-model-session
                          (make-gemini-model "gemini-3.8-flash" apikey
                                             '((built-in-tools google-search)))
                          '()
                          "Answer in one sentence.")))
          (display "Q: Search the web: which events are planned in Porto this month?\nGemini: ")
          (display (ask searcher "Search the web: which events are planned in Porto this month?"))
          (newline)
          (for-each
            (lambda (activity)
              (display "  [")
              (display (cdr (assq 'kind activity)))
              (display "] ")
              (display (cdr (assq 'queries (cdr (assq 'arguments activity)))))
              (newline))
            (gemini-tool-activities (model-session-transcript searcher))))
        ;; Usage
        (let ((usage (model-session-usage gemini)))
          (display "Tokens used by Gemini: ")
          (display (cdr (assq 'input-tokens usage)))
          (display " input, ")
          (display (cdr (assq 'output-tokens usage)))
          (display " output, ")
          (display (cdr (assq 'reasoning-tokens usage)))
          (display " reasoning")
          (newline)))
      (display "Gemini API key not available.")))
  (else
    (display "Gemini integration unavailable.")))
(newline)

;;; GPT

;; OpenAI's GPT models are available via library `(lispkit foundation-model gpt)`. Again,
;; a GPT session works exactly like a session for the on-device model.
(cond-expand
  ((library (lispkit foundation-model gpt))
    (import (lispkit foundation-model gpt)
            (lispkit system util))
    (define-values (get-openai-api-key set-openai-api-key!)
      (manage-credential openai-api-key
                         "OPENAI_API_KEY"
                         "FoundationModelsDemo"
                         "gpt-api-key"))
    (display "===================================")
    (newline)
    (if-let* ((apikey (get-openai-api-key))
              (gpt-model (make-gpt-model "gpt-5.5" apikey))
              (gpt (make-model-session
                     gpt-model
                     (list convert-tool)
                     (string-append "You are a concise travel assistant. "
                                    "Use the convertCurrency tool to convert amounts."))))
      (begin
        (display "MODEL: ")
        (display (language-model-name gpt-model))
        (display " by ")
        (display (language-model-family-vendor (language-model-family gpt-model)))
        (display ", availability: ")
        (display (language-model-availability gpt-model))
        (newline)
        ;; A tool call
        (display "Q: How much is 60 US dollars in euros?\nGPT: ")
        (display (ask gpt "How much is 60 US dollars in euros?"))
        (newline)
        ;; The session remembers the conversation, here the amount from above
        (display "Q: And how much is that in Japanese yen?\nGPT: ")
        (display (ask gpt "And how much is that in Japanese yen?"))
        (newline)
        ;; Guided generation with the itinerary schema, in a new session
        (display "Q: Plan a short trip to Seville.\n")
        (show-itinerary
          (ask (make-model-session gpt-model '() "You are a concise travel assistant.")
               "Plan a short trip to Seville."
               itinerary-schema))
        ;; Streaming
        (display "Q: Describe an afternoon in the Alcázar of Seville in three sentences.\n")
        (display "GPT (streaming): ")
        (stream-answer gpt "Describe an afternoon in the Alcázar of Seville in three sentences.")
        (newline)
        ;; Token counts are computed by the OpenAI API
        (display "Tokens of the conversation so far: ")
        (display (language-model-token-count gpt-model (model-session-transcript gpt)))
        (newline)
        ;; A stored conversation: responses are kept by OpenAI, and each request only
        ;; sends what is new. Web search runs on OpenAI's side; the transcript records it.
        (let ((searcher (make-model-session
                          (make-gpt-model "gpt-5.5" apikey
                                          '((built-in-tools web-search) (stateful . #t)))
                          '()
                          "Answer in one sentence.")))
          (display "Q: Search the web: which exhibitions are on in Seville this month?\nGPT: ")
          (display (ask searcher "Search the web: which exhibitions are on in Seville this month?"))
          (newline)
          (for-each
            (lambda (activity)
              (display "  [")
              (display (cdr (assq 'kind activity)))
              (display "] ")
              (display (cdr (assq 'queries activity)))
              (newline))
            (gpt-tool-activities (model-session-transcript searcher)))
          (display "Q: Which of them is the most popular?\nGPT: ")
          (display (ask searcher "Which of them is the most popular?"))
          (newline))
        ;; Usage
        (let ((usage (model-session-usage gpt)))
          (display "Tokens used by GPT: ")
          (display (cdr (assq 'input-tokens usage)))
          (display " input, ")
          (display (cdr (assq 'output-tokens usage)))
          (display " output, ")
          (display (cdr (assq 'reasoning-tokens usage)))
          (display " reasoning")
          (newline)))
      (display "OpenAI API key not available.")))
  (else
    (display "GPT integration unavailable.")))
(newline)
