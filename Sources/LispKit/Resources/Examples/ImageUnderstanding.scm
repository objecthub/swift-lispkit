;;; Understand photos with the on-device language model
;;;
;;; This code showcases how Apple's on-device language model can be used together
;;; with images via libraries `(lispkit foundation-model)` and
;;; `(lispkit foundation-model tool)`. Two photos are attached to prompts: a photo
;;; of a cat and a photo of a printed book page. The program first lets the model
;;; describe the cat and answer follow-up questions about it. It then classifies
;;; both photos via guided generation. Finally, it reads the book page with the
;;; OCR tool provided by Apple: without the tool, the model can only guess the
;;; text of the page; with the tool, it answers questions about the page, summarizes
;;; it, and extracts its content into a structured form.
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
        (lispkit draw)
        (lispkit foundation-model)
        (lispkit foundation-model tool)
        (lispkit thread future))

;;; Loading the photos

(define cat-image (load-image (asset-file-path "Inga" "jpg" "Images")))
(define page-image (load-image (asset-file-path "Page" "jpg" "Images")))

;;; Checking the model

(display "===================================")
(newline)
(define model (system-language-model))
(display "MODEL: ")
(display (language-model-name model))
(display ", availability: ")
(display (language-model-availability model))
(display ", capabilities: ")
(display (language-model-capabilities model))
(newline)

(if (not (language-model-available? model))
    (begin
      (display "The on-device language model is not available.")
      (newline)
      (exit)))

(if (not (memq 'vision (language-model-capabilities model)))
    (begin
      (display "The on-device language model does not support images.")
      (newline)
      (exit)))

;; Images are passed to the model as attachments of a prompt. The label of an
;; attachment allows the model (and tools) to refer to the image. An image
;; attachment uses a fixed number of tokens, independent of the size of the image.
(display "Tokens used by a prompt with the photo of the cat: ")
(display (language-model-token-count model
           (list "Describe this photo." (image-attachment cat-image "cat"))))
(newline)

;; `model-session-response` returns a future; `future-get` waits for the response.
(define (ask session prompt . args)
  (model-response-content (future-get (apply model-session-response session prompt args))))

(define (show question answer)
  (display "Q: ")
  (display question)
  (newline)
  (display "A: ")
  (display answer)
  (newline))

;;; Describing a photo

(display "-----------------------------------")
(newline)

(define viewer
  (make-model-session model '()
    "You describe photos precisely. Answer in at most two sentences."))

;; The first prompt attaches the photo; the session remembers it, so follow-up
;; questions can refer to it.
(show "What does this photo show?"
      (ask viewer (list "What does this photo show?" (image-attachment cat-image "cat"))))
(show "What color are the cat's eyes?"
      (ask viewer "What color are the cat's eyes?"))
(show "What kind of fur does the cat have, and which breed could it be?"
      (ask viewer "What kind of fur does the cat have, and which breed could it be?"))

;;; Classifying photos

(display "-----------------------------------")
(newline)

;; A generation schema turns the model into an image classifier: the response is an
;; association list with the category of the photo and further properties.
(define classification-schema
  (make-generation-schema
    '(object Classification "A classification of a photo"
       (category (enum Category ("animal" "person" "landscape" "document" "food" "object"))
                 "The category of the main subject of the photo")
       (subject string "The main subject of the photo in a few words")
       (colors (array string 1 4) "The dominant colors of the main subject")
       (setting string "Where the photo was taken")
       (tags (array string 3 6) "Keywords describing the photo"))))

;; Every photo is classified in a fresh session so that the classifications don't
;; influence each other.
(define (classify image label)
  (ask (make-model-session model)
       (list "Classify this photo." (image-attachment image label))
       classification-schema
       '((temperature . 0.2))))

(define (show-classification label classification)
  (display label)
  (display ":")
  (newline)
  (for-each (lambda (property)
              (display "  ")
              (display (car property))
              (display ": ")
              (display (if (vector? (cdr property))
                           (vector->list (cdr property))
                           (cdr property)))
              (newline))
            classification))

(show-classification "Photo of the cat" (classify cat-image "cat"))
(show-classification "Photo of the page" (classify page-image "page"))

;;; Reading a document with the OCR tool

(display "-----------------------------------")
(newline)

;; The model sees the photo of the page, but it can't read the text reliably. When
;; asked about details, it makes up an answer.
(define guesser (make-model-session model '() "Answer concisely."))

(show "What is the caption of the figure on this page? (without OCR)"
      (ask guesser (list "What is the caption of the figure on this page?"
                         (image-attachment page-image "page"))))

;; The OCR tool reads the text of an image attached to the conversation. The model
;; refers to the image by the label of its attachment. The on-device model calls
;; the tool when it is explicitly asked to read the attached page. Once the tool
;; was called, the text of the page is part of the transcript of the session, and
;; follow-up questions are answered based on that text. Option
;; `((tool-calling . required))` is not suitable for forcing the tool call: it
;; requires a tool call in every step, so the model would call the tool over and
;; over again instead of answering.
(define reader
  (make-model-session model (list (make-ocr-tool))
    (string-append "You answer questions about photos of printed documents. Never guess "
                   "the text of a document: always read it with the getText tool first. "
                   "Answer concisely.")))

(show "Read the text of the attached page and tell me the caption of its figure."
      (ask reader (list "Read the text of the attached page and tell me the caption of its figure."
                        (image-attachment page-image "page"))))
(show "Summarize the section in two sentences."
      (ask reader "Summarize the section in two sentences."))

;; Guided generation extracts the content of the page into a structured form.
(define page-schema
  (make-generation-schema
    '(object Page "The content of a book page"
       (section-number string "The number of the section, e.g. 2.3")
       (section-title string)
       (figure-caption string "The caption of the figure on the page")
       (requirements (array string 3 6) "The requirements listed on the page"))))

(define page (ask reader "Extract the content of the page." page-schema))
(display "Section: ")
(display (cdr (assq 'section-number page)))
(display " ")
(display (cdr (assq 'section-title page)))
(newline)
(display "Figure: ")
(display (cdr (assq 'figure-caption page)))
(newline)
(display "Requirements:")
(newline)
(vector-for-each (lambda (requirement)
                   (display "  - ")
                   (display requirement)
                   (newline))
                 (cdr (assq 'requirements page)))

;; The transcript of the session shows how often the model called the OCR tool and
;; what the tool returned.
(define (tool-outputs session)
  (filter (lambda (entry) (eq? (car entry) 'tool-output))
          (transcript->list (model-session-transcript session))))

(display "The OCR tool was called ")
(display (length (tool-outputs reader)))
(display " time(s); it returned ")
(display (string-length (cadr (assq 'segments (cdar (tool-outputs reader))))))
(display " characters of text.")
(newline)
