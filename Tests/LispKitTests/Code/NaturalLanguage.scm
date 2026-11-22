;;; NaturalLanguage.scm
;;; Regression test data for library (lispkit natural-language)
;;;
;;; The results of the NaturalLanguage framework depend on the models installed on the
;;; machine running the tests. The tests therefore check robust properties of the results
;;; instead of exact scores.
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
  (import (lispkit natural-language)
          (lispkit thread future)
          (only (lispkit system) system-directory path file-exists? delete-file-or-directory))
  ;; Returns #t if calling `thunk` raises an error
  (define (call-raises? thunk)
    (guard (x (#t #t)) (thunk) #f))
  (define-syntax raises?
    (syntax-rules ()
      ((_ expr) (call-raises? (lambda () expr)))))
  (define tmp-dir (car (system-directory 'temporary)))
  (define (tmp-file name)
    (path tmp-dir name))
  (define (delete-if-exists file)
    (if (file-exists? file) (delete-file-or-directory file)))
  (define (approx= x y)
    (< (abs (- x y)) 1e-4))
  (define text "Tim Cook visited Cupertino on Monday. He was happy.\n\nThe weather was terrible!")
  #t
)

(
  "Language identification"
  (de en #f #t (de))
  (list (dominant-language "Das ist ein kleiner Test")
        (dominant-language "This is a small test")
        (dominant-language "")
        (let ((hyps (language-hypotheses "Das ist ein kleiner Test" 3)))
          (and (<= (length hyps) 3)
               (eq? (caar hyps) 'de)
               (> (cdar hyps) 0.9)))
        (map car (language-hypotheses "Das ist ein kleiner Test" 3 '(de))))
)

(
  "Tokenization"
  (("Hello" "world")
   ((0 . 5) (7 . 12))
   ("Tim" "Cook" "visited")
   ("Tim Cook visited Cupertino on Monday. " "He was happy.\n" "\n" "The weather was terrible!")
   (17 . 26)
   #f
   ())
  (list (text-tokens "Hello, world!")
        (text-token-ranges "Hello, world!" 'word)
        (text-tokens text 'word #f 0 16)
        (text-tokens text 'sentence 'en)
        (text-token-range text 20)
        (text-token-range "abc" 3)
        (text-tokens ""))
)

(
  "Tag schemes"
  (#t #t #f (language script) #t)
  (list (tag-scheme? 'lemma)
        (tag-scheme? 'sentiment-score)
        (tag-scheme? 'tense)
        (available-tag-schemes 'en 'sentence)
        (and (memq 'lexical-class (available-tag-schemes 'en)) #t))
)

(
  "Lexical classes"
  (("The" . determiner) ("cat" . noun) ("sleeps" . verb) ("." . sentence-terminator))
  (text-tags "The cat sleeps." 'lexical-class)
)

(
  "Token types and ranges"
  (((0 . 5) . word) ((5 . 6) . punctuation) ((7 . 12) . word) ((12 . 13) . punctuation))
  (text-tag-ranges "Hello, world!" 'token-type)
)

(
  "Lemmas"
  (("He" . "he") ("was" . "be") ("happy" . "happy") ("." . #f))
  (text-tags "He was happy." 'lemma)
)

(
  "Tagger options"
  (("Tim Cook" . personal-name) ("visited" . other-word) ("Cupertino" . place-name))
  (text-tags "Tim Cook visited Cupertino." 'name-type 'word
             '(omit-whitespace omit-punctuation join-names))
)

(
  "Single tags"
  (noun verb #f #t)
  (list (text-tag "Time flies" 0 'lexical-class)
        (text-tag "The cat sleeps." 8 'lexical-class)
        (text-tag "abc" 3 'lemma)
        (let ((hyps (text-tag-hypotheses "The cat sleeps." 4 'lexical-class 'word 3)))
          (and (pair? hyps) (eq? (caar hyps) 'noun))))
)

(
  "Named entities"
  (("Tim Cook" . personal-name) ("Cupertino" . place-name))
  (named-entities text)
)

(
  "Sentiment"
  (#t #t #f 2 #t)
  (list (> (text-sentiment "I love this wonderful library!") 0.0)
        (< (text-sentiment "This is a horrible, awful mess.") 0.0)
        (text-sentiment "")
        (length (text-tags "Good.\n\nBad!" 'sentiment-score 'sentence))
        (real? (cdar (text-tags text 'sentiment-score 'paragraph))))
)

(
  "Word embeddings"
  (#t word en 300 #t #f #t #t #t #t)
  (let ((e (word-embedding 'en)))
    (list (embedding? e)
          (embedding-type e)
          (embedding-language e)
          (embedding-dimension e)
          (embedding-contains? e "dog")
          (embedding-vector e "xqzzyxx")
          (= (vector-length (embedding-vector e "dog")) (embedding-dimension e))
          (< (embedding-distance e "dog" "cat") (embedding-distance e "dog" "banana"))
          (approx= (embedding-distance e "dog" "cat")
                   (cosine-distance (embedding-vector e "dog") (embedding-vector e "cat")))
          (let ((ns (embedding-neighbors e "dog" 3)))
            (and (= (length ns) 3)
                 (string? (caar ns))
                 (approx= (cdar ns) (embedding-distance e "dog" (caar ns)))))))
)

(
  "Sentence embeddings"
  (sentence #t)
  (let ((e (sentence-embedding 'en)))
    (list (embedding-type e)
          (< (embedding-distance e "The cat sits on the mat." "A kitten rests on a rug.")
             (embedding-distance e "The cat sits on the mat." "Stock prices fell sharply."))))
)

(
  "Cosine distance"
  (#t 1.0 2.0 #f)
  (list (approx= (cosine-distance #(1 2 3) #(2 4 6)) 0.0)
        (cosine-distance #(1 0) '(0 1))
        (cosine-distance #(1 0) #(-1 0))
        (cosine-distance #(0 0) #(1 0)))
)

(
  "Custom embeddings"
  (#t custom en 2 4 300 #t 2)
  (define (vec i) (vector (sin i) (cos i) (sin (* 2 i)) (cos (* 3 i))))
  (define emb-file (tmp-file "lispkit-nl-test.embedding"))
  (delete-if-exists emb-file)
  (save-embedding emb-file
                  (let loop ((i 0) (acc '()))
                    (if (= i 300)
                        acc
                        (loop (+ i 1) (cons (cons (string-append "w" (number->string i)) (vec i))
                                            acc))))
                  'en
                  2)
  (let ((e (load-embedding emb-file)))
    (delete-if-exists emb-file)
    (list (embedding? e)
          (embedding-type e)
          (embedding-language e)
          (embedding-revision e)
          (embedding-dimension e)
          (embedding-vocabulary-size e)
          (approx= (embedding-distance e "w5" "w6") (cosine-distance (vec 5) (vec 6)))
          (length (embedding-neighbors e (vec 7) 2))))
)

(
  "Contextual embeddings"
  (#t #t 4 #t)
  (let ((ce (contextual-embedding 'en)))
    (list (contextual-embedding? ce)
          (and (memq 'en (contextual-embedding-languages ce)) #t)
          (length (contextual-embedding-vectors ce "The cat sat."))
          (= (vector-length (contextual-embedding-vector ce "The cat sat."))
             (contextual-embedding-dimension ce))))
)

(
  "Gazetteers"
  (fruit color #f fruit)
  (define gaz-file (tmp-file "lispkit-nl-test.gazetteer"))
  (define g (make-gazetteer '((fruit "apple" "pear") (color "red" "green"))))
  (delete-if-exists gaz-file)
  (save-gazetteer gaz-file g)
  (let ((g2 (load-gazetteer gaz-file)))
    (delete-if-exists gaz-file)
    (list (gazetteer-label g "pear")
          (gazetteer-label g "red")
          (gazetteer-label g "car")
          (gazetteer-label g2 "apple")))
)

(
  "Taggers with gazetteers"
  (#t name-type
   (("Gandalf" . personal-name) ("ate" . other-word) ("an" . other-word) ("apple" . fruit)
    ("in" . other-word) ("Rivendell" . place-name)))
  (define t (make-tagger 'name-type #f
                         (make-gazetteer '((personal-name "Gandalf")
                                           (place-name "Rivendell")
                                           (fruit "apple")))))
  (list (tagger? t)
        (tagger-scheme t)
        (text-tags "Gandalf ate an apple in Rivendell." t 'word '(omit-whitespace omit-punctuation)))
)

(
  "Text classifiers"
  (#t classifier positive negative #t #t)
  (define data
    '((positive "I love this" "What a great day" "This is wonderful" "Fantastic work"
                "I am so happy" "Best movie ever" "Really enjoyable" "Absolutely brilliant")
      (negative "I hate this" "What a terrible day" "This is awful" "Horrible work"
                "I am so sad" "Worst movie ever" "Really boring" "Absolutely dreadful")))
  (define model-file (tmp-file "lispkit-nl-test.mlmodel"))
  (define m (future-get (train-text-classifier data)))
  (delete-if-exists model-file)
  (save-text-model model-file m)
  (let ((m2 (load-text-model model-file)))
    (delete-if-exists model-file)
    (list (text-model? m)
          (text-model-type m)
          (text-model-label m "What a wonderful movie")
          (text-model-label m2 "What an awful movie")
          (eq? (caar (text-model-label-hypotheses m "So happy")) 'positive)
          (equal? (text-tags "I love it. I hate it." (make-tagger 'sentiment-class m) 'sentence)
                  '(("I love it. " . positive) ("I hate it." . negative)))))
)

(
  "Errors"
  (#t #t #t #t #t #t #t)
  (list (raises? (text-tokens "abc" 'chapter))
        (raises? (text-tags "abc" 'lemma 'word '(omit-everything)))
        (raises? (text-tokens "abc" 'word #f 2 10))
        (raises? (embedding-dimension 'en))
        (raises? (cosine-distance #(1 2) #(1 2 3)))
        (raises? (load-text-model (tmp-file "lispkit-nl-nonexistent.mlmodel")))
        (raises? (future-get (train-text-classifier '((a "x")) 'unknown-algorithm))))
)
