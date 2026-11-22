# LispKit Natural Language

Library `(lispkit natural-language)` provides natural language processing capabilities through Apple's _NaturalLanguage_ framework. All functionality is executed on the device. The library supports:

  - _Language identification_: determining the language of a text
  - _Tokenization_: splitting a text into words, sentences, or paragraphs
  - _Tagging_: classifying the tokens of a text, e.g. into lexical classes (nouns, verbs, ...), named entities (names of people, places, and organizations), lemmas, languages, scripts, and sentiment scores
  - _Embeddings_: vectors representing the meaning of words and sentences, which can be compared via distances and used for finding semantically similar words
  - _Contextual embeddings_: vectors for the tokens of a text which take the context of the tokens into account
  - _Gazetteers_: custom dictionaries that assign labels to terms, which can be used to customize tagging
  - _Text models_: custom text classifiers and word taggers, which can be trained from Scheme data via Create ML

The following code shows a few examples:

```scheme
(import (lispkit natural-language))
(dominant-language "Das ist ein kleiner Test")  ⇒  de
(text-tags "The cat sleeps." 'lexical-class)
  ⇒  (("The" . determiner) ("cat" . noun) ("sleeps" . verb) ("." . sentence-terminator))
(named-entities "Tim Cook visited Cupertino on Monday.")
  ⇒  (("Tim Cook" . personal-name) ("Cupertino" . place-name))
(text-sentiment "I love this wonderful library!")  ⇒  1.0
(embedding-neighbors (word-embedding 'en) "dog" 3)
  ⇒  (("cat" . 0.25696967925122927) ("canine" . 0.265751288046447) ("kennel" . 0.28345779525055725))
```

The results of the procedures of this library depend on the models provided by the operating system. Models for some languages and tag schemes are downloaded on demand; procedures `request-tag-assets` and `request-contextual-embedding-assets` request such downloads.


## Conventions

**Languages** are represented by symbols denoting BCP 47 language tags, such as `en`, `de`, `fr`, `pt-BR`, or `zh-Hans`. Procedures accepting languages also accept strings. `#f` denotes an undetermined language; procedures returning languages return `#f` if the language cannot be determined. **Scripts** are represented by symbols denoting ISO 15924 script codes, such as `Latn`, `Cyrl`, or `Hani`.

**Texts** are strings. Many procedures accept optional arguments _start_ and _end_ which restrict processing to the characters of the text between index _start_ (inclusive) and index _end_ (exclusive). The default for _start_ is 0; the default for _end_ is the length of the text. **Ranges** of text, e.g. of tokens, are represented by pairs `(start . end)` of indices, like the matching specs of library `(lispkit regexp)`.

**Token units** determine the granularity at which a text is tokenized or tagged. They are represented by the symbols `word`, `sentence`, `paragraph`, and `document`. The default token unit is `word`.

**Labels** are symbols. Tags, i.e. labels determined by a tagger, which are defined by the NaturalLanguage framework are represented by the following symbols:

  - Token types: `word`, `punctuation`, `whitespace`, `other`
  - Lexical classes: `noun`, `verb`, `adjective`, `adverb`, `pronoun`, `determiner`, `particle`, `preposition`, `number`, `conjunction`, `interjection`, `classifier`, `idiom`, `other-word`, `sentence-terminator`, `open-quote`, `close-quote`, `open-parenthesis`, `close-parenthesis`, `word-joiner`, `dash`, `paragraph-break`
  - Name types: `personal-name`, `place-name`, `organization-name`

All other labels, e.g. the labels of gazetteers and text models, are symbols whose name is the label itself. Procedures accepting labels also accept strings.

**Distances** between vectors of embeddings are _cosine distances_, i.e. one minus the cosine similarity of the two vectors. Cosine distances range from 0.0 (same direction) to 2.0 (opposite direction). Vectors are represented as vectors of flonums; procedures accepting vectors also accept lists of numbers.


## Language Identification

**(dominant-language _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the most likely language of string _str_, or `#f` if the language cannot be determined.

```scheme
(dominant-language "Das ist ein kleiner Test")  ⇒  de
(dominant-language "")  ⇒  #f
```

**(language-hypotheses _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(language-hypotheses _str n_)**  
**(language-hypotheses _str n constraints_)**  
**(language-hypotheses _str n constraints hints_)**  

Returns an association list mapping the most likely languages of string _str_ to their probabilities. The list contains at most _n_ entries (default is 5), sorted by descending probability. _constraints_ is a list of languages restricting the languages that are considered; `#f` (the default) does not restrict the languages. _hints_ is an association list mapping languages to prior probabilities, which are used to guide the identification of the language, e.g. for short texts; `#f` (the default) does not provide hints.

```scheme
(language-hypotheses "Das ist ein kleiner Test" 3)
  ⇒  ((de . 0.9999737739562988) (nl . 1.5001117390056606e-05) (hu . 3.2021428069128888e-06))
(language-hypotheses "Das ist ein kleiner Test" 3 '(de))
  ⇒  ((de . 1.0))
```


## Tokenization

**(text-tokens _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(text-tokens _str unit_)**  
**(text-tokens _str unit lang_)**  
**(text-tokens _str unit lang start_)**  
**(text-tokens _str unit lang start end_)**  

Splits string _str_ between _start_ and _end_ into tokens of the given token _unit_ and returns them as a list of strings. Whitespace and punctuation are not tokens at the `word` level. _unit_ is `word` by default. _lang_ is the language of _str_; if _lang_ is `#f` (the default), the language is determined automatically.

```scheme
(text-tokens "I paid $3.50 for 2 pizzas.")
  ⇒  ("I" "paid" "3.50" "for" "2" "pizzas")
(text-tokens "Hello there! How are you?" 'sentence)
  ⇒  ("Hello there! " "How are you?")
```

**(text-token-ranges _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(text-token-ranges _str unit_)**  
**(text-token-ranges _str unit lang_)**  
**(text-token-ranges _str unit lang start_)**  
**(text-token-ranges _str unit lang start end_)**  

Like `text-tokens`, but returns the tokens as a list of ranges `(start . end)` of _str_.

```scheme
(text-token-ranges "Hello, world!")  ⇒  ((0 . 5) (7 . 12))
```

**(text-token-range _str index_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(text-token-range _str index unit_)**  
**(text-token-range _str index unit lang_)**  

Returns the range `(start . end)` of the token of the given token _unit_ which contains the character of string _str_ at _index_. `#f` is returned if there is no such token, e.g. if _index_ is the length of _str_.

```scheme
(text-token-range "Hello, world!" 9)  ⇒  (7 . 12)
```


## Tagging

A _tag scheme_ determines what tags are assigned to tokens. The following tag schemes are supported:

  - `token-type`: classifies tokens into words, punctuation, whitespace, and other tokens
  - `lexical-class`: classifies tokens by their lexical class (part of speech, type of punctuation or whitespace)
  - `name-type`: classifies names into personal names, place names, and organization names; other tokens are tagged with their token type or lexical class
  - `name-type-or-lexical-class`: combines `name-type` and `lexical-class`
  - `lemma`: tags tokens with the stem form of the word (as a string), if known
  - `language`: tags tokens with their language
  - `script`: tags tokens with their script
  - `sentiment-score`: tags sentences and paragraphs with a sentiment score, a flonum between -1.0 (negative) and 1.0 (positive); applying this scheme at the `word` level tags each token with the score of its paragraph

Tag schemes are symbols. Tag schemes that are defined by custom text models are represented by symbols with arbitrary names.

Tags are assigned by a _tagger_. Procedures that tag text accept either a tag scheme, which is used with the built-in models of the framework, or a _tagger_ object created by `make-tagger`, which combines a tag scheme with custom models and gazetteers.

The tagging procedures accept a list of _options_ customizing which tokens are returned. The following options are supported:

  - `omit-words`: omits word tokens
  - `omit-punctuation`: omits punctuation tokens
  - `omit-whitespace`: omits whitespace tokens, i.e. tokens which only consist of whitespace (also at the `sentence` and `paragraph` level)
  - `omit-other`: omits tokens of other types, e.g. symbols
  - `join-names`: joins the tokens of names, e.g. `Tim Cook`, into a single token
  - `join-contractions`: joins contractions, e.g. `wasn't`, into a single token

By default, the only option is `omit-whitespace`.

**(tag-scheme? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a symbol denoting one of the tag schemes provided by the NaturalLanguage framework; `#f` otherwise.

**(available-tag-schemes _lang_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(available-tag-schemes _lang unit_)**  

Returns a list of the tag schemes that are available for language _lang_ at the given token _unit_ (default is `word`).

```scheme
(available-tag-schemes 'en)
  ⇒  (language script token-type name-type lexical-class name-type-or-lexical-class lemma)
(available-tag-schemes 'en 'sentence)  ⇒  (language script)
```

**(request-tag-assets _lang scheme_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Requests the download of the models for tag scheme _scheme_ and language _lang_ if they are not installed yet. Returns a future which eventually refers to `#t` if the models are available, and `#f` otherwise. Procedure `future-get` of library `(lispkit thread future)` waits for the result.

**(make-tagger _scheme_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-tagger _scheme models_)**  
**(make-tagger _scheme models gazetteers_)**  

Returns a new tagger for tag scheme _scheme_ which uses the custom text models _models_ and the gazetteers _gazetteers_ for this scheme. _models_ and _gazetteers_ are either a single object or a list of objects; `#f` or `()` (the default) do not add any models or gazetteers. Labels determined by gazetteers override the tags of the models. Text classifiers used in taggers tag tokens at the `sentence` or `paragraph` level; word taggers tag tokens at the `word` level.

```scheme
(define t
  (make-tagger 'name-type #f
    (make-gazetteer '((personal-name "Gandalf") (place-name "Rivendell") (fruit "apple")))))
(text-tags "Gandalf ate an apple in Rivendell." t 'word '(omit-whitespace omit-punctuation))
  ⇒  (("Gandalf" . personal-name) ("ate" . other-word) ("an" . other-word) ("apple" . fruit)
      ("in" . other-word) ("Rivendell" . place-name))
```

**(tagger? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a tagger; `#f` otherwise.

**(tagger-scheme _tagger_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the tag scheme of _tagger_.

**(text-tags _str scheme_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(text-tags _str scheme unit_)**  
**(text-tags _str scheme unit options_)**  
**(text-tags _str scheme unit options lang_)**  
**(text-tags _str scheme unit options lang start_)**  
**(text-tags _str scheme unit options lang start end_)**  

Tags the tokens of the given token _unit_ (default is `word`) of string _str_ between _start_ and _end_ and returns an association list mapping the tokens (strings) to their tags. _scheme_ is either a tag scheme or a tagger. _options_ is a list of tagger options (default is `(omit-whitespace)`). _lang_ is the language of _str_; if _lang_ is `#f` (the default), the language is determined automatically. The tags are symbols, except for tag scheme `lemma`, whose tags are strings, and `sentiment-score`, whose tags are flonums. Tokens without a tag, e.g. punctuation for the `lemma` scheme, are mapped to `#f`.

```scheme
(text-tags "He wasn't happy." 'lemma)
  ⇒  (("He" . "he") ("was" . "be") ("n't" . "not") ("happy" . "happy") ("." . #f))
(text-tags "Tim Cook visited Cupertino." 'name-type 'word
           '(omit-whitespace omit-punctuation join-names))
  ⇒  (("Tim Cook" . personal-name) ("visited" . other-word) ("Cupertino" . place-name))
(text-tags "Good.\n\nBad!" 'sentiment-score 'sentence)
  ⇒  (("Good.\n" . 0.0) ("Bad!" . -0.6))
(text-tags "Mais le chat dort" 'lemma 'word '(omit-whitespace) 'fr)
  ⇒  (("Mais" . "mais") ("le" . "le") ("chat" . "chat") ("dort" . "dormir"))
```

**(text-tag-ranges _str scheme_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(text-tag-ranges _str scheme unit_)**  
**(text-tag-ranges _str scheme unit options_)**  
**(text-tag-ranges _str scheme unit options lang_)**  
**(text-tag-ranges _str scheme unit options lang start_)**  
**(text-tag-ranges _str scheme unit options lang start end_)**  

Like `text-tags`, but the tokens are represented by their ranges `(start . end)` in _str_.

```scheme
(text-tag-ranges "Hello, world!" 'token-type)
  ⇒  (((0 . 5) . word) ((5 . 6) . punctuation) ((7 . 12) . word) ((12 . 13) . punctuation))
```

**(text-tag _str index scheme_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(text-tag _str index scheme unit_)**  
**(text-tag _str index scheme unit lang_)**  

Returns the tag of the token of the given token _unit_ (default is `word`) which contains the character of string _str_ at _index_. _scheme_ is either a tag scheme or a tagger. `#f` is returned if the token does not have a tag.

```scheme
(text-tag "The cat sleeps." 8 'lexical-class)  ⇒  verb
```

**(text-tag-hypotheses _str index scheme_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(text-tag-hypotheses _str index scheme unit_)**  
**(text-tag-hypotheses _str index scheme unit n_)**  
**(text-tag-hypotheses _str index scheme unit n lang_)**  

Returns an association list mapping the most likely tags of the token of the given token _unit_ (default is `word`) which contains the character of string _str_ at _index_ to their probabilities. The list contains at most _n_ entries (default is 5), sorted by descending probability. _scheme_ is either a tag scheme or a tagger.

```scheme
(text-tag-hypotheses "Time flies like an arrow" 5 'lexical-class)
  ⇒  ((verb . 0.6006264639211071))
```

**(named-entities _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(named-entities _str lang_)**  

Returns an association list mapping the names of people, places, and organizations mentioned in string _str_ to their name type, i.e. `personal-name`, `place-name`, or `organization-name`.

```scheme
(named-entities "Tim Cook visited Cupertino on Monday.")
  ⇒  (("Tim Cook" . personal-name) ("Cupertino" . place-name))
```

**(text-sentiment _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(text-sentiment _str lang_)**  

Returns the sentiment score of string _str_, a flonum between -1.0 (negative) and 1.0 (positive). The sentiment score of a text with several paragraphs is the average of the sentiment scores of the paragraphs, weighted by their lengths. `#f` is returned if _str_ only consists of whitespace.

```scheme
(text-sentiment "I love this wonderful library!")  ⇒  1.0
(text-sentiment "This is a horrible, awful mess.")  ⇒  -1.0
```


## Embeddings

An _embedding_ maps strings to vectors of flonums such that strings with a similar meaning are mapped to vectors with a small distance. _Word embeddings_ map words to vectors; they have a vocabulary, i.e. a finite set of words for which they provide vectors. _Sentence embeddings_ map arbitrary strings, e.g. sentences, to vectors. The NaturalLanguage framework provides word and sentence embeddings for a number of languages. _Custom embeddings_ are word embeddings created from Scheme data via `save-embedding`.

**(embedding? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an embedding; `#f` otherwise.

**(word-embedding _lang_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(word-embedding _lang revision_)**  

Returns the word embedding for language _lang_, or `#f` if there is no word embedding for _lang_. _revision_ is the revision of the embedding; the default is the current revision.

**(sentence-embedding _lang_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(sentence-embedding _lang revision_)**  

Returns the sentence embedding for language _lang_, or `#f` if there is no sentence embedding for _lang_. _revision_ is the revision of the embedding; the default is the current revision.

**(word-embedding-revisions _lang_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the supported revisions of word embeddings for language _lang_.

**(sentence-embedding-revisions _lang_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the supported revisions of sentence embeddings for language _lang_.

**(load-embedding _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Loads a custom embedding from the file at _path_. Such files are created via `save-embedding`, or with Create ML.

**(save-embedding _path entries_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(save-embedding _path entries lang_)**  
**(save-embedding _path entries lang revision_)**  

Creates a custom embedding and writes it to the file at _path_. _entries_ is an association list mapping words (strings) to vectors. All vectors need to have the same dimension. _lang_ is the language of the embedding (default is `#f`), and _revision_ is the revision of the embedding (default is 1). The embedding is stored in a compressed form; the vectors of an embedding with a very small vocabulary are approximated.

```scheme
(save-embedding "colors.embedding"
  '(("red" . #(1.0 0.0 0.0)) ("orange" . #(0.9 0.4 0.0)) ("blue" . #(0.0 0.0 1.0)) ...))
(define colors (load-embedding "colors.embedding"))
```

**(embedding-type _embedding_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the type of _embedding_: `word`, `sentence`, or `custom`.

**(embedding-language _embedding_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the language of _embedding_, or `#f` if the language is not known.

**(embedding-revision _embedding_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the revision of _embedding_.

**(embedding-dimension _embedding_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the dimension of the vectors of _embedding_.

**(embedding-vocabulary-size _embedding_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of words in the vocabulary of _embedding_. Sentence embeddings do not have a vocabulary; their vocabulary size is 0.

**(embedding-contains? _embedding str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _embedding_ provides a vector for string _str_; `#f` otherwise.

**(embedding-vector _embedding str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the vector of string _str_ in _embedding_, or `#f` if _embedding_ does not provide a vector for _str_.

```scheme
(define we (word-embedding 'en))
(embedding-dimension we)  ⇒  300
(vector-length (embedding-vector we "king"))  ⇒  300
```

**(embedding-distance _embedding x y_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the cosine distance between _x_ and _y_ in _embedding_. _x_ and _y_ are either strings or vectors. `#f` is returned if _embedding_ does not provide a vector for a string, or if one of the vectors is zero.

```scheme
(embedding-distance we "dog" "cat")  ⇒  0.25696967925122927
(define se (sentence-embedding 'en))
(embedding-distance se "The cat sits on the mat." "A kitten rests on a rug.")
  ⇒  0.3306807084257901
(embedding-distance se "The cat sits on the mat." "Stock prices fell sharply.")
  ⇒  0.8549174812486697
```

**(embedding-neighbors _embedding x_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(embedding-neighbors _embedding x n_)**  
**(embedding-neighbors _embedding x n maxdist_)**  

Returns an association list mapping the words of the vocabulary of _embedding_ which are closest to _x_ to their cosine distance to _x_. _x_ is either a string or a vector. The list contains at most _n_ entries (default is 10), sorted by ascending distance, and only words whose distance is at most _maxdist_ (default is 2.0). The result never contains _x_ itself, nor words whose vector is identical to vector _x_. For custom embeddings, the search for neighbors is approximate. Since sentence embeddings do not have a vocabulary, `embedding-neighbors` returns `()` for them.

```scheme
(embedding-neighbors we "dog" 5)
  ⇒  (("cat" . 0.25696967925122927) ("canine" . 0.265751288046447) ("kennel" . 0.28345779525055725)
      ("puppy" . 0.31232280833188675) ("feline" . 0.3379380100102498))
```

**(cosine-distance _v1 v2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the cosine distance between the vectors _v1_ and _v2_, i.e. one minus the cosine similarity of the two vectors, or `#f` if one of the vectors is zero. _v1_ and _v2_ need to have the same dimension.

```scheme
(cosine-distance #(1 0) #(0 1))  ⇒  1.0
(cosine-distance #(1 0) #(-1 0))  ⇒  2.0
```


## Contextual Embeddings

A _contextual embedding_ maps the tokens of a text to vectors which take the context of the tokens in the text into account. For instance, the vector of the word "bank" in "river bank" differs from the vector in "bank account". Contextual embeddings are transformer models which support a set of languages written in one script. The model of a contextual embedding is loaded when it is first used. Its assets might need to be downloaded first via `request-contextual-embedding-assets`.

**(contextual-embedding? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a contextual embedding; `#f` otherwise.

**(contextual-embedding _lang_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(contextual-embedding _lang script_)**  

Returns a contextual embedding supporting language _lang_ and script _script_. If _script_ is not provided, the contextual embedding for language _lang_ is returned. If _lang_ is `#f`, a contextual embedding for _script_ is returned. `#f` is returned if there is no such contextual embedding.

```scheme
(define ce (contextual-embedding 'en))
(contextual-embedding-languages ce)
  ⇒  (cs da de en es fi fr hr hu id it nb nl pl pt ro sk sv tr vi)
(contextual-embedding #f 'Cyrl)  ⇒  #<contextual-embedding 7920b640f0: Cyrl dimension=512>
```

**(contextual-embedding-id _cembedding_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the identifier of the model of contextual embedding _cembedding_ as a string.

**(contextual-embedding-languages _cembedding_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the list of languages supported by contextual embedding _cembedding_.

**(contextual-embedding-scripts _cembedding_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the list of scripts supported by contextual embedding _cembedding_.

**(contextual-embedding-revision _cembedding_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the revision of contextual embedding _cembedding_.

**(contextual-embedding-dimension _cembedding_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the dimension of the vectors of contextual embedding _cembedding_.

**(contextual-embedding-max-length _cembedding_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the maximum number of tokens of a text which contextual embedding _cembedding_ maps to vectors. Tokens beyond this length are ignored.

**(contextual-embedding-assets? _cembedding_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the assets of contextual embedding _cembedding_ are installed on the device; `#f` otherwise.

**(request-contextual-embedding-assets _cembedding_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Requests the download of the assets of contextual embedding _cembedding_ if they are not installed yet. Returns a future which eventually refers to `#t` if the assets are available, and `#f` otherwise.

**(contextual-embedding-vectors _cembedding str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(contextual-embedding-vectors _cembedding str lang_)**  

Returns an association list mapping the ranges `(start . end)` of the tokens of string _str_ to their vectors in contextual embedding _cembedding_. _lang_ is the language of _str_; if _lang_ is `#f` (the default), the language is determined automatically.

```scheme
(map (lambda (x) (cons (car x) (vector-length (cdr x))))
     (contextual-embedding-vectors ce "The cat sat."))
  ⇒  (((0 . 3) . 512) ((4 . 7) . 512) ((8 . 11) . 512) ((11 . 12) . 512))
```

**(contextual-embedding-vector _cembedding str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(contextual-embedding-vector _cembedding str lang_)**  

Returns a single vector for string _str_, the mean of the vectors of the tokens of _str_ in contextual embedding _cembedding_. `#f` is returned if _str_ does not have any tokens. Such vectors can be compared with `cosine-distance` for determining the semantic similarity of texts.

```scheme
(define (distance a b)
  (cosine-distance (contextual-embedding-vector ce a) (contextual-embedding-vector ce b)))
(distance "The cat sits on the mat." "A kitten rests on a rug.")  ⇒  0.08386927809318256
(distance "The cat sits on the mat." "Stock prices fell sharply.")  ⇒  0.26330456843157857
```


## Gazetteers

A _gazetteer_ is a dictionary which maps terms to labels. Gazetteers are used to customize taggers (see `make-tagger`): tokens found in a gazetteer are tagged with the label of the gazetteer. Gazetteers for tag scheme `name-type` can also be used to recognize additional names: terms labeled with `personal-name`, `place-name`, or `organization-name` are tagged as names.

**(gazetteer? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a gazetteer; `#f` otherwise.

**(make-gazetteer _entries_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-gazetteer _entries lang_)**  

Returns a new gazetteer for language _lang_ (default is `#f`). _entries_ is an association list mapping labels to lists of terms (strings), i.e. it has the form `((label term ...) ...)`.

```scheme
(define g (make-gazetteer '((fruit "apple" "pear" "banana") (color "red" "green"))))
(gazetteer-label g "pear")  ⇒  fruit
(gazetteer-label g "car")  ⇒  #f
```

**(load-gazetteer _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Loads a gazetteer from the file at _path_. Such files are created via `save-gazetteer`, or with Create ML.

**(save-gazetteer _path gazetteer_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Writes _gazetteer_ to the file at _path_.

**(gazetteer-language _gazetteer_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the language of _gazetteer_, or `#f` if the language is not known.

**(gazetteer-label _gazetteer str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the label of string _str_ in _gazetteer_, or `#f` if _gazetteer_ does not contain _str_.


## Text Models

A _text model_ is either a _text classifier_, which assigns a label to a text, or a _word tagger_ (a _sequence_ model), which assigns labels to the tokens of a text. Text models are Core ML models which are created with Create ML, either via the Create ML application or, for text classifiers, via procedure `train-text-classifier`. Text models can be used directly, or in taggers (see `make-tagger`).

**(text-model? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a text model; `#f` otherwise.

**(load-text-model _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Loads a text model from the Core ML model at _path_. _path_ refers either to a compiled Core ML model (a directory with extension `.mlmodelc`), or to a Core ML model file (with extension `.mlmodel`), which is compiled before it is loaded.

**(text-model-type _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the type of text model _model_: `classifier` for text classifiers and `sequence` for word taggers.

**(text-model-language _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the language of text model _model_, or `#f` if the language is not known.

**(text-model-revision _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the revision of the NaturalLanguage framework which was used to create text model _model_.

**(text-model-label _model str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the label which text model _model_ assigns to string _str_, or `#f` if no label can be determined.

**(text-model-label-hypotheses _model str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(text-model-label-hypotheses _model str n_)**  

Returns an association list mapping the most likely labels which text classifier _model_ assigns to string _str_ to their probabilities. The list contains at most _n_ entries (default is 5), sorted by descending probability.

**(text-model-token-labels _model tokens_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the labels which word tagger _model_ assigns to the given list of tokens _tokens_ (strings).

**(train-text-classifier _data_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(train-text-classifier _data algorithm_)**  
**(train-text-classifier _data algorithm lang_)**  

Trains a text classifier with Create ML and returns a future which eventually refers to the resulting text model. _data_ is an association list mapping labels to lists of example texts (strings), i.e. it has the form `((label text ...) ...)`. Part of the data is held out for validating the classifier. _algorithm_ determines the training algorithm:

  - `max-ent`: a maximum entropy classifier (the default)
  - `crf`: a conditional random field classifier
  - `static-embedding`: transfer learning based on a static word embedding
  - `elmo-embedding`: transfer learning based on a dynamic word embedding
  - `bert-embedding`: transfer learning based on a BERT embedding
  - a string: transfer learning based on the custom embedding stored at the path specified by the string

_lang_ is the language of the texts; if _lang_ is `#f` (the default), the language is determined automatically. Create ML prints information about the training progress to standard output. Training text classifiers is not supported in the iOS simulator; there, `train-text-classifier`, `text-model-metrics`, and `save-text-model` are not defined.

```scheme
(import (lispkit thread future))
(define data
  '((positive "I love this" "What a great day" "This is wonderful" "Fantastic work"
              "I am so happy" "Best movie ever" "Really enjoyable" "Absolutely brilliant")
    (negative "I hate this" "What a terrible day" "This is awful" "Horrible work"
              "I am so sad" "Worst movie ever" "Really boring" "Absolutely dreadful")))
(define m (future-get (train-text-classifier data)))
(text-model-label m "This was a great experience")  ⇒  positive
(text-model-label-hypotheses m "This was a dreadful experience")
  ⇒  ((negative . 0.6543387599174739) (positive . 0.34566124008252597))
(text-tags "I love it. I hate it." (make-tagger 'sentiment-class m) 'sentence)
  ⇒  (("I love it. " . positive) ("I hate it." . negative))
```

**(text-model-metrics _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an association list with the accuracy of text classifier _model_ on the training data (`training-accuracy`) and on the validation data (`validation-accuracy`), if available. Accuracies are flonums between 0.0 and 1.0. `#f` is returned if _model_ was not trained with `train-text-classifier`.

```scheme
(text-model-metrics m)  ⇒  ((training-accuracy . 1.0))
```

**(save-text-model _path model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(save-text-model _path model author_)**  
**(save-text-model _path model author description_)**  
**(save-text-model _path model author description version_)**  

Writes text classifier _model_, which was trained with `train-text-classifier`, as a Core ML model file to _path_ (which should have the extension `.mlmodel`). _author_, _description_, and _version_ are strings which are stored as metadata of the model. Text models loaded from files cannot be saved.
