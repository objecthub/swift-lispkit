# LispKit Foundation Model

Library `(lispkit foundation-model)` provides access to large language models via Apple's [FoundationModels](https://developer.apple.com/documentation/foundationmodels) framework. It supports Apple's on-device language model, Apple's language model running on Private Cloud Compute, and language models provided by other libraries, such as `(lispkit foundation-model claude)`, `(lispkit foundation-model gemini)`, and `(lispkit foundation-model gpt)`, which give access to Anthropic's Claude models, Google's Gemini models, and OpenAI's GPT models. The library includes:

  - _Language models_ with information about their availability and capabilities
  - _Sessions_ which keep track of interactions with a language model and allow programs to generate text, structured content, and streams of responses
  - _Generation schemas_ for guided generation of structured content
  - _Tools_, i.e. Scheme procedures that a language model can call while generating a response
  - _Transcripts_ capturing the history of a session, which can be saved and restored

The library is only available on macOS 27 and iOS 27 or newer. Programs can check if it is available via `cond-expand`:

```scheme
(cond-expand
  ((library (lispkit foundation-model))
    (import (lispkit foundation-model)))
  (else
    (display "language models are not supported")))
```

Generating a response is an asynchronous operation. Procedures `model-session-response` and `response-stream-result` return a _future_ for the result. Library `(lispkit thread future)` provides procedure `future-get` for waiting for the result of a future. The following code creates a session for the on-device language model and asks a question:

```scheme
(import (lispkit foundation-model) (lispkit thread future))
(define session
  (make-model-session #f '() "You are a concise assistant. Answer in one sentence."))
(define response
  (future-get (model-session-response session "What is the capital of France?")))
(model-response-content response)
⇒  "Paris."
```

## Language models

Language models are represented by objects of type `language-model`. Each language model belongs to a _family_ of models and has a _name_ which identifies it within its family. Library `(lispkit foundation-model)` provides two families of language models: Apple's on-device language model (family `system`) and Apple's language model running on Private Cloud Compute (family `afm-cloud`). Other libraries can provide additional families of language models, e.g. library `(lispkit foundation-model claude)` provides Claude models (family `claude`), library `(lispkit foundation-model gemini)` provides Gemini models (family `gemini`), and library `(lispkit foundation-model gpt)` provides GPT models (family `gpt`).

The family `afm-cloud` is only provided if the application using LispKit enables Private Cloud Compute (see `afm-cloud-language-model`).

**(language-model? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a language model; `#f` otherwise.

**(system-language-model)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(system-language-model _use-case_)**  
**(system-language-model _use-case guardrails_)**  

Returns Apple's on-device language model. _use-case_ is either `general` (the default) or `content-tagging`, which specializes the model for tagging content. _guardrails_ is either `default` (the default) or `permissive-content-transformations`, which reduces the guardrails for tasks that transform user-provided content, e.g. for summarizing or rewriting text.

**(afm-cloud-language-model)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns Apple's language model running on Private Cloud Compute. Using this model requires an entitlement. Therefore, Private Cloud Compute is disabled by default. Applications that have the entitlement enable it by setting the static property `FoundationModelLibrary.enablePrivateCloudCompute` to `true` before the first LispKit context is created. If Private Cloud Compute is disabled, the family `afm-cloud` is not available and procedures `afm-cloud-language-model`, `limit-increase-suggestion?`, and `limit-increase-suggestion-show!` are not defined. Without the entitlement, the system rejects all requests, even though the model is reported as `available`; requests then fail with a model error of kind `not-permitted`.

**(language-model-family _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the family of language model _model_ as a symbol: `system`, `afm-cloud`, or a family defined by another library, such as `claude`, `gemini`, or `gpt`.

**(language-model-name _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the name of language model _model_ as a string. The name identifies the model among the models of its family:

| Family | Names |
| :--- | :--- |
| `system` | The model the device uses: `"AFM 3 Core"` or `"AFM 3 Core Advanced"` |
| `afm-cloud` | `"afm-cloud"` |
| `claude` | The model identifier, e.g. `"claude-opus-5-5"` |
| `gemini` | The model identifier, e.g. `"gemini-3.8-flash"` |
| `gpt` | The model identifier, e.g. `"gpt-5.5"` |

For Claude, Gemini, and GPT models, the name is the model identifier that was passed to `make-claude-model`, `make-gemini-model`, or `make-gpt-model`. The name of the on-device model is determined by the device, which uses either `"AFM 3 Core"` or `"AFM 3 Core Advanced"`; programs can't choose between them, and `system-language-model` always returns the model of the device. The use case and the guardrails of the on-device model are not part of its name. FoundationModels doesn't tell which model runs on Private Cloud Compute, which therefore has a single, fixed name.

```scheme
(language-model-name (system-language-model 'content-tagging))  ⇒  "AFM 3 Core Advanced"
(language-model-name (make-gemini-model "gemini-3.8-flash"))  ⇒  "gemini-3.8-flash"
```

**(available-language-model-families)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the families of language models as symbols. Besides `system` and, if Private Cloud Compute is enabled, `afm-cloud`, the list contains the families provided by libraries that have been loaded, such as `claude`, `gemini`, and `gpt`.

```scheme
(import (lispkit foundation-model claude))
(available-language-model-families)  ⇒  (claude system)
```

**(language-model-family-vendor _family_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the name of the vendor of the language models of family _family_, a symbol, as a string. `#f` is returned if _family_ is not a known family of language models (see `available-language-model-families`).

| Family | Vendor |
| :--- | :--- |
| `system` | `"Apple"` |
| `afm-cloud` | `"Apple"` |
| `claude` | `"Anthropic"` |
| `gemini` | `"Google"` |
| `gpt` | `"OpenAI"` |

```scheme
(language-model-family-vendor 'gemini)  ⇒  "Google"
(language-model-family-vendor (language-model-family (system-language-model)))  ⇒  "Apple"
```

**(available-language-models _family_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the names of the available language models of family _family_, a symbol. `#f` is returned if _family_ is not a known family of language models (see `available-language-model-families`). The library implementing a family determines what is available:

- `system`: the name of the model the device uses, if it is available (see `language-model-available?`); otherwise, the list is empty.
- `afm-cloud`: `"afm-cloud"`, if the model is available; otherwise, the list is empty. This family is only known if Private Cloud Compute is enabled.
- `claude`, `gemini`, and `gpt`: the models known to the library, i.e. the result of `claude-models`, `gemini-models`, and `gpt-models`. Whether a model can be used with a particular API key is not checked; `gemini-available-models` and `gpt-available-models` list the models offered by the Gemini and OpenAI APIs.

```scheme
(available-language-models 'system)  ⇒  ("AFM 3 Core Advanced")
(available-language-models 'gemini)
⇒  ("gemini-4-hydrogen" "gemini-4-neon" "gemini-4-argon" "gemini-4-carbon" "gemini-3.8-flash" ...)
```

The family and the name of a model are sufficient for creating a model of the same family and name. The following procedure creates a model from a family and a name. It can be applied to the names returned by `available-language-models`, or used to create a fresh model with the same family and name as an existing model _m_ via `(make-language-model (language-model-family m) (language-model-name m))`:

```scheme
(define (make-language-model family name)
  (case family
    ((system) (system-language-model))
    ((afm-cloud) (afm-cloud-language-model))
    ((claude) (make-claude-model name))
    ((gemini) (make-gemini-model name))
    ((gpt) (make-gpt-model name))
    (else (error "unknown family of language models" family))))
```

For the families `system` and `afm-cloud`, the name is not needed: there is only one model of these families on a device. Everything else that was specified when the original model was created is not carried over and needs to be passed again: the use case and guardrails of the on-device model, and the credentials and options of Claude, Gemini, and GPT models (e.g. effort levels, thinking levels, reasoning efforts, and built-in tools). Without credentials, `make-claude-model`, `make-gemini-model`, and `make-gpt-model` use the API keys in the environment variables `ANTHROPIC_API_KEY`, `GEMINI_API_KEY`, and `OPENAI_API_KEY`.

**(language-model-available? _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if language model _model_ is available on this device; `#f` otherwise.

**(language-model-availability _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the availability of language model _model_ as a symbol. `available` is returned if the model can be used. Otherwise, a symbol describing why the model is unavailable is returned: `device-not-eligible`, `apple-intelligence-not-enabled`, `model-not-ready`, or `system-not-ready`. Language models provided by `(lispkit foundation-model claude)`, `(lispkit foundation-model gemini)`, and `(lispkit foundation-model gpt)` are always reported as `available`.

**(language-model-capabilities _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of symbols describing the capabilities of language model _model_. The following capabilities are supported: `vision` (the model accepts images), `guided-generation` (the model can generate content that conforms to a generation schema), `reasoning` (the model can reason before responding), and `tool-calling` (the model can call tools).

```scheme
(language-model-capabilities (system-language-model))
⇒  (vision guided-generation tool-calling)
```

**(language-model-context-size _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the size of the context window of language model _model_ in tokens, or `#f` if the model doesn't report it. The on-device model, the Private Cloud Compute model, and Gemini models report a context size. The procedure waits for the result if it has to be fetched asynchronously, e.g. from the Gemini API.

**(language-model-supported-languages _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a sorted list of language identifiers (strings) for the languages supported by language model _model_, or `#f` if the model doesn't report its languages. Only the on-device model and the Private Cloud Compute model report them. The procedure waits for the result if it has to be fetched asynchronously.

**(language-model-supports-locale? _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(language-model-supports-locale? _model locale_)**  

Returns `#t` if language model _model_ supports the locale _locale_, a locale identifier string such as `"de_CH"`. If _locale_ is not provided, the current locale is used. Only the on-device model and the Private Cloud Compute model support this query; for other models, a model error of kind `unsupported` is raised. The procedure waits for the result if it has to be fetched asynchronously.

**(language-model-token-count _model obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(language-model-token-count _model obj kind_)**  

Returns the number of tokens that _obj_ uses when it is passed to language model _model_, or `#f` if the model can't count tokens. The on-device model and GPT models count tokens; GPT models count them via the OpenAI API. _kind_ determines how _obj_ is interpreted: `prompt` (_obj_ is a prompt), `instructions` (_obj_ are instructions), `tools` (_obj_ is a list of tools), `schema` (_obj_ is a generation schema or schema specification), or `transcript` (_obj_ is a transcript). If _kind_ is not provided, it is derived from _obj_: transcripts, generation schemas, and lists of tools are counted as such; everything else is counted as a prompt. The procedure waits for the result.

```scheme
(language-model-token-count (system-language-model) "Hello, how are you?")
⇒  7
```

**(language-model-supports-data-type? _model type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(language-model-supports-data-type? _model type kind_)**  

Returns `#t` if language model _model_ supports data of type _type_, a string describing the type of the data either as a uniform type identifier (e.g. `"public.png"`), a MIME type, or a filename extension. _kind_ is either `attachment` (data attachments in prompts and instructions; this is the default) or `entry` (data entries in transcripts). The procedure waits for the result. It requires macOS 27.2 or iOS 27.2.

```scheme
(language-model-supports-data-type? (system-language-model) "public.png")
```

**(language-model-quota-usage _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an association list describing the usage quota of language model _model_, or `#f` if the model has no usage quota. Only the Private Cloud Compute model has a usage quota. Key `status` refers to one of the symbols `below-limit`, `approaching-limit`, or `limit-reached`. Key `reset-date` refers to a date-time object (as defined by library `(lispkit date-time)`) describing when the quota gets reset, or `#f` if this is unknown. Key `limit-increase-suggestion` refers to a limit increase suggestion object if the system suggests a way of increasing the quota, and to `#f` otherwise.

**(limit-increase-suggestion? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a limit increase suggestion; `#f` otherwise. Limit increase suggestions are returned by `language-model-quota-usage` and are part of the details of errors of kind `quota-limit-reached`. This procedure is only defined if Private Cloud Compute is enabled (see `afm-cloud-language-model`).

**(limit-increase-suggestion-show! _suggestion_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Presents limit increase suggestion _suggestion_ to the user. This only has an effect in applications that can display the user interface of the system. This procedure is only defined if Private Cloud Compute is enabled (see `afm-cloud-language-model`).

## Prompts and instructions

Prompts and instructions are represented by strings, characters, numbers, images, attachments, and (nested) lists of such values. Lists are flattened and their elements are concatenated in order. Images are objects defined by library `(lispkit draw)`. They are passed to the language model as image attachments. For instance, the following list is a valid prompt that includes an image `photo`:

```scheme
(list "Describe this image in " 3 " sentences: " photo)
```

Instructions are passed to a session when it is created. They describe the role of the language model and how it should respond. Prompts are the inputs to which the language model responds.

**(make-prompt _obj ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a prompt object consisting of the prompt components _obj ..._. A prompt object can be used wherever a prompt is expected. Creating a prompt object is useful if a prompt is used several times, since it gets converted into the internal representation only once.

**(prompt? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a prompt object; `#f` otherwise.

**(image-attachment _image_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(image-attachment _image label_)**  

Returns an attachment for the given image. _image_ is either an image object as defined by library `(lispkit draw)` or a path to an image file. _label_ is an optional string that can be used to refer to the attachment in a prompt.

**(data-attachment _bytevector type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-attachment _bytevector type label_)**  

Returns an attachment for the data in _bytevector_. _type_ is a string describing the type of the data, either as a uniform type identifier (e.g. `"public.json"`), a MIME type (e.g. `"application/pdf"`), or a filename extension (e.g. `"pdf"`). _label_ is an optional string that can be used to refer to the attachment in a prompt. Data attachments require macOS 27.2 or iOS 27.2 and a language model that supports the given type of data.

**(attachment? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an attachment; `#f` otherwise.

## Generated content

Language models can generate structured content, e.g. as described by a generation schema. Generated content is represented by Scheme values following the conventions of library `(lispkit json)`:

  - _null_ is represented by the symbol `null`,
  - booleans are represented by `#t` and `#f`,
  - numbers are represented by exact integers or floating-point numbers; numbers declared as `integer` in a schema are always exact integers, numbers declared as `number` are always floating-point numbers,
  - strings are represented by strings,
  - arrays are represented by immutable vectors, and
  - objects are represented by association lists with symbols as keys. The order of the keys is the order in which the properties are declared by the schema.

When Scheme values are converted into generated content, e.g. for the results of tools, symbols other than `null` are converted into strings, characters into strings of length 1, all vectors into arrays, the empty list into an empty object, and records, hashtables, and JSON values (of library `(lispkit json)`) into objects. Entries of association lists whose value is `#<void>` are skipped.

## Generation schemas

Generation schemas describe the structure of generated content. They are used for guided generation, i.e. for generating content that conforms to the schema, and for describing the parameters of tools. Generation schemas are created from _schema specifications_, s-expressions that follow this grammar:

```
schema   ::= null
           | boolean
           | integer
           | number
           | string
           | name          ; reference to a named schema
           | (string strguide ...)
           | (integer numguide ...)
           | (number numguide ...)
           | (array schema)
           | (array schema min-count)
           | (array schema min-count max-count)
           | (object name property ...)
           | (object name description property ...)
           | (enum name (str ...))
           | (enum name description (str ...))
           | (any-of name schema ...)
           | (any-of name description schema ...)
           | (ref name)
property ::= (property-name schema)
           | (property-name schema description)
           | (property-name (optional schema))
           | (property-name (optional schema) description)
           | explicit-nil
strguide ::= (constant str)
           | (any-of str ...)
           | (pattern regex)
numguide ::= (minimum x)
           | (maximum x)
           | (range low high)
```

Names and property names are symbols or strings; descriptions are strings. Guides restrict the values that can be generated: `(constant str)` only allows string _str_, `(any-of str ...)` allows one of the given strings, and `(pattern regex)` allows strings matching the regular expression _regex_. `(minimum x)`, `(maximum x)`, and `(range low high)` restrict numbers. For arrays, _min-count_ and _max-count_ restrict the number of elements; they can be `#f` for "no restriction". Objects have a name and a list of properties. Optional properties do not need to be generated; if they are not, they are omitted from the generated content. If the symbol `explicit-nil` is one of the properties of an object, optional properties without a value are generated explicitly with value `null` instead, e.g. `(object Person explicit-nil (name string) (nickname (optional string)))`. Enumerations are named schemas for a fixed set of strings, and `any-of` defines a named schema whose values conform to one of the given schemas. Named schemas can be referred to via their name, either as a symbol or via `(ref name)`.

Wherever a generation schema is expected, a schema specification can be used instead; it is converted into a generation schema automatically.

**(make-generation-schema _schema dependency ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a generation schema for schema specification _schema_. The schema specifications _dependency ..._ define the named schemas that _schema_ refers to. An error is raised if _schema_ is malformed or refers to undefined schemas.

```scheme
(make-generation-schema
  '(object Team "A team"
     (name string)
     (members (array Person 1 5))
     (lead (optional Person) "The team lead"))
  '(object Person
     (name string "The full name")
     (age (integer (range 18 99)))
     (role (enum Role ("engineer" "designer" "manager")))))
```

**(generation-schema? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a generation schema; `#f` otherwise.

**(generation-schema-name _schema_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the name of generation schema _schema_ as a string.

**(generation-schema->json _schema_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a JSON representation of generation schema _schema_ as a string. The representation is similar to a JSON schema.

```scheme
(generation-schema->json '(object Point (x number) (y number)))
⇒  "{\"additionalProperties\":false,\"properties\":{\"x\":{\"type\":\"number\"},\"y\":{\"type\":\"number\"}},\"required\":[\"x\",\"y\"],\"title\":\"Point\",\"type\":\"object\",\"x-order\":[\"x\",\"y\"]}"
```

**(json->generation-schema _json_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a generation schema for the JSON representation _json_, which is either a string or a JSON value of library `(lispkit json)`. _json_ has the format produced by `generation-schema->json`. Content generated for schemas created by this procedure is converted without knowledge of the schema, i.e. integral numbers are always converted into exact integers and the keys of objects are sorted.

## Tools

Tools are Scheme procedures that a language model can call while it generates a response, e.g. to look up information or to perform actions. Every tool has a name, a description that tells the language model what the tool does, and a generation schema for its parameters. Tools are passed to a session when it is created. Library `(lispkit foundation-model tool)` provides tools that are implemented natively by Apple: reading text and barcodes from images, and searching the content indexed by Spotlight.

Tools implemented in Scheme are executed by a separate thread that is created for every request of a session that has such tools. Thus, tool procedures run concurrently with the code that made the request.

**(make-tool _name description parameters proc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-tool _name description parameters proc include-schema?_)**  

Returns a new tool with the given _name_ (a string or symbol) and _description_ (a string). _parameters_ is a generation schema or schema specification describing the arguments of the tool. _proc_ is the procedure implementing the tool. If _parameters_ is an object schema, _proc_ is called with one argument per property, in the order in which the properties are declared; optional properties that were not generated, or that were generated as `null`, are passed as `#f`. For all other schemas, _proc_ is called with one argument, the generated content. If _include-schema?_ is `#f`, the parameter schema is not included in the instructions of the session. The default is `#t`.

The result of _proc_ is returned to the language model. A string is returned as text, a prompt object as a prompt, and every other value as generated content. If _proc_ raises an error, the request that caused the tool call fails with the same error.

```scheme
(define weather
  (make-tool "getWeather" "Returns the current weather in a city"
    '(object WeatherArgs
       (city string "The name of the city")
       (unit (optional (string (any-of "celsius" "fahrenheit")))))
    (lambda (city unit)
      `((city . ,city) (temperature . 21) (unit . ,(or unit "celsius"))))))
```

**(tool? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a tool; `#f` otherwise.

**(tool-name _tool_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the name of _tool_ as a string.

**(tool-description _tool_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the description of _tool_.

**(tool-parameters _tool_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the generation schema describing the parameters of _tool_.

## Sessions

A session represents a conversation with a language model. It keeps track of all prompts and responses in a transcript, so that the language model can refer to earlier interactions. A session can only process one request at a time.

**(make-model-session)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-model-session _model_)**  
**(make-model-session _model tools_)**  
**(make-model-session _model tools instructions_)**  

Returns a new session for language model _model_. If _model_ is `#f` or not provided, the on-device language model is used. _tools_ is a list of tools that the language model can call. _instructions_ defines instructions (see section "Prompts and instructions") for the session. If _instructions_ is a transcript, the session continues the conversation captured by the transcript.

```scheme
(define session
  (make-model-session (system-language-model) (list weather)
    "You are a helpful assistant. Use the getWeather tool for weather questions."))
```

**(model-session? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a session; `#f` otherwise.

**(model-session-model _session_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the language model of _session_.

**(model-session-tools _session_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the list of tools of _session_.

**(model-session-prewarm _session_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(model-session-prewarm _session prefix_)**  

Loads the resources required by _session_ ahead of a request. _prefix_ is an optional prompt with which the next prompt is expected to start.

**(model-session-response _session prompt_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(model-session-response _session prompt schema_)**  
**(model-session-response _session prompt schema options_)**  

Sends _prompt_ to the language model of _session_ and returns a future for the response. The future yields a response object once the language model has responded; it raises an error if the request fails. If _schema_ is a generation schema or schema specification, the language model generates content that conforms to _schema_. Otherwise, if _schema_ is `#f` or not provided, the language model generates text. _options_ is an association list of options, as described in section "Options".

```scheme
(define r (future-get (model-session-response session "What's the weather in Zurich?")))
(model-response-content r)
⇒  "It is currently 21°C in Zurich."
(define p (future-get (model-session-response session "Invent a person named Ada."
                        '(object Person (name string) (age integer)))))
(model-response-content p)
⇒  ((name . "Ada") (age . 34))
```

**(model-session-stream _session prompt_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(model-session-stream _session prompt schema_)**  
**(model-session-stream _session prompt schema options_)**  

Sends _prompt_ to the language model of _session_ and returns a response stream which provides snapshots of the response while it is being generated. The arguments are the same as for `model-session-response`.

**(model-session-cancel! _session_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Cancels the requests of _session_ that are in progress. The futures and streams of cancelled requests fail with a model error of kind `cancelled`. Returns `#t` if a request was cancelled, `#f` otherwise. Requests are also cancelled when the evaluation is aborted, e.g. by pressing ctrl-C in the REPL.

**(model-session-responding? _session_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _session_ is currently processing a request; `#f` otherwise.

**(model-session-transcript _session_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a transcript with the full history of _session_, including instructions, prompts, tool calls, and responses.

**(model-session-usage _session_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an association list describing the accumulated number of tokens used by all responses of _session_. The keys are `input-tokens`, `cached-input-tokens`, `output-tokens`, `reasoning-tokens`, `total-tokens`, and `metadata`, whose value is an association list with additional, model-specific information.

```scheme
(model-session-usage session)
⇒  ((input-tokens . 75) (cached-input-tokens . 0) (output-tokens . 5)
    (reasoning-tokens . 0) (total-tokens . 80) (metadata))
```

**(model-session-error-policy _session_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the policy of _session_ for managing its transcript when a request fails: `revert-transcript` (entries of failed requests are removed), `preserve-transcript` (entries of failed requests are kept), or `#f` if the default policy of the language model is used.

**(model-session-error-policy-set! _session policy_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the policy of _session_ for managing its transcript when a request fails to _policy_, which is one of `revert-transcript`, `preserve-transcript`, or `#f`.

**(model-session-properties _session_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the properties of _session_ as an association list. Currently, there is only one property: `history` refers to a transcript with the entries of _session_ that are considered when generating responses.

**(model-session-property-set! _session key value_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the property _key_ of _session_ to _value_. Currently, only the `history` property can be set. _value_ is a transcript, a transcript entry, or a list of transcripts and transcript entries. This can be used, for instance, to remove old entries from the history when the context window of the language model gets exhausted.

## Options

Requests can be configured via an association list of options. Options whose value is `#f` are ignored. The following options are supported:

| Option | Value |
| :--- | :--- |
| `sampling` | `greedy` for always choosing the most likely token, `(top-k k)` or `(top-k k seed)` for choosing among the _k_ most likely tokens, or `(top-p threshold)` or `(top-p threshold seed)` for choosing among the most likely tokens whose probabilities add up to _threshold_ (between 0.0 and 1.0). |
| `temperature` | A number influencing the randomness of the response. Lower values make responses more deterministic. |
| `maximum-response-tokens` | The maximum number of tokens of the response. |
| `tool-calling` | `allowed` (the language model decides if it calls tools), `required` (the language model must call a tool), or `disallowed`. |
| `include-schema-in-prompt` | A boolean determining whether the generation schema is included in the prompt. The default is `#t` if a schema is given. |
| `reasoning-level` | `light`, `moderate`, `deep`, or a string naming a model-specific reasoning level. This is a hint for language models that support reasoning. |
| `metadata` | An association list with additional, model-specific information about the request. |

```scheme
(model-session-response session "Suggest a name for a cat." #f
  '((temperature . 0.9) (maximum-response-tokens . 20)))
```

## Responses

A response object represents the response of a language model, or a snapshot of a response that is still being generated.

**(model-response? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a response object; `#f` otherwise.

**(model-response-content _response_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the content of _response_: a string for text responses, or the generated content (see section "Generated content") if the response was generated for a schema.

**(model-response-raw-content _response_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the raw content of _response_ as generated content. For text responses, this is a string.

**(model-response-entries _response_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a transcript with the entries that were added to the transcript of the session for _response_, e.g. the prompt, tool calls, tool outputs, and the response.

**(model-response-usage _response_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an association list describing the number of tokens used for _response_. The format is the same as for `model-session-usage`.

**(model-response-complete? _response_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _response_ is complete; `#f` if it is a snapshot of a response that was only partially generated.

## Response streams

Response streams provide snapshots of a response while it is being generated. Every snapshot contains the full content generated so far. Snapshots that were not retrieved before a newer snapshot became available are skipped.

**(response-stream? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a response stream; `#f` otherwise.

**(response-stream-next _stream_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(response-stream-next _stream timeout_)**  
**(response-stream-next _stream timeout default_)**  

Waits for the next snapshot of _stream_ and returns it as a response object. The end-of-file object is returned once the response is complete and the last snapshot has been returned. If generating the response fails, an error is raised. _timeout_ is the maximum number of seconds to wait; `default` is returned if no snapshot is available within _timeout_ seconds. The default for _default_ is `#f`.

**(response-stream-result _stream_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a future for the final response of _stream_.

**(response-stream-done? _stream_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the response of _stream_ is complete and its last snapshot was returned by `response-stream-next`; `#f` otherwise.

**(response-stream-cancel! _stream_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Cancels the generation of the response of _stream_.

**(response-stream->generator _stream_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a generator, i.e. a procedure without arguments, which returns the snapshots of _stream_ one after the other, followed by the end-of-file object. Generators can be used with the procedures of library `(srfi 158)`.

**(response-stream-for-each _proc stream_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Calls _proc_ for every snapshot of _stream_ until the response is complete.

```scheme
(define stream (model-session-stream session "Count from one to five in words."))
(response-stream-for-each
  (lambda (snapshot) (display (model-response-content snapshot)) (newline))
  stream)
```

## Transcripts

A transcript is an immutable sequence of entries that captures the history of a session. There are seven kinds of entries: `instructions`, `prompt`, `tool-calls`, `tool-output`, `response`, `reasoning`, and `data`. Transcript entries are represented as lists that start with the kind of the entry, followed by an association list of properties:

```
(instructions (id . str) (segments segment ...) (tools (name description schema) ...))
(prompt (id . str) (segments segment ...) (options . options) (metadata . alist) (schema . schema))
(tool-calls (id . str) (calls ((id . str) (tool . name) (arguments . content) (metadata . alist)) ...))
(tool-output (id . str) (tool . name) (segments segment ...))
(response (id . str) (segments segment ...) (metadata . alist) (asset-ids str ...))
(reasoning (id . str) (segments segment ...) (metadata . alist) (signature . bytevector))
(data (id . str) (content-type . type) (content . bytevector) (metadata . content))
```

The properties `metadata` of tool calls, `asset-ids` of responses, and `signature` of reasoning entries are only present if they have a value.

Segments are strings (text segments), lists `(structure source content)` (generated content for a schema named _source_), or lists `(attachment label image)` and `(attachment label bytevector type)` (attachments, where _label_ is a string or `#f`). When transcript entries are created from this representation, all properties are optional and missing identifiers are generated.

The list representation does not capture opaque, model-specific information. For saving and restoring transcripts without losing information, procedures `transcript->json` and `json->transcript` should be used.

**(transcript? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a transcript; `#f` otherwise.

**(make-transcript _obj ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new transcript consisting of the entries described by _obj ..._. Each _obj_ is either a transcript entry, a transcript, or a list of transcript entries and transcripts.

```scheme
(define t (make-transcript '(instructions (segments "Be brief."))
                           '(prompt (segments "Hi"))
                           '(response (segments "Hello!"))))
(define s (make-model-session #f '() t))
```

**(transcript-length _transcript_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of entries of _transcript_.

**(transcript-ref _transcript i_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the _i_-th entry of _transcript_ in list representation.

**(transcript->list _transcript_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all entries of _transcript_ in list representation.

```scheme
(map car (transcript->list (model-session-transcript session)))
⇒  (instructions prompt tool-calls tool-output response)
```

**(transcript-append _transcript obj ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new transcript consisting of the entries of _transcript_ followed by the entries described by _obj ..._, which are transcript entries, transcripts, or lists of entries and transcripts.

**(transcript-sub _transcript start_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(transcript-sub _transcript start end_)**  

Returns a new transcript consisting of the entries of _transcript_ between index _start_ (inclusive) and index _end_ (exclusive). If _end_ is not provided, the length of _transcript_ is used.

**(transcript->json _transcript_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a JSON representation of _transcript_ as a string. This representation captures all information of _transcript_.

**(json->transcript _json_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the transcript represented by _json_, a string or a JSON value of library `(lispkit json)` in the format produced by `transcript->json`.

```scheme
(define saved (transcript->json (model-session-transcript session)))
(define restored (make-model-session #f '() (json->transcript saved)))
```

## Errors

Errors reported by language models are represented by _model errors_. Each model error has a kind, a symbol describing the cause of the error. The following kinds are defined by library `(lispkit foundation-model)`:

  - `context-size-exceeded`, `exceeded-context-window-size`: the context window of the language model is exhausted
  - `rate-limited`, `quota-limit-reached`: too many requests were made
  - `guardrail-violation`, `refusal`: the language model refused to respond
  - `unsupported-capability`, `unsupported-transcript-content`, `unsupported-guide`, `unsupported-language-or-locale`, `unsupported`: the language model does not support the request
  - `assets-unavailable`, `network-failure`, `service-unavailable`, `timeout`: the language model cannot be reached
  - `decoding-failure`, `schema-error`: generated content or a schema is invalid
  - `concurrent-requests`, `transcript-mutation-while-responding`: the session is processing another request
  - `cancelled`: the request was cancelled
  - `not-permitted`, `system-error`: the system refused or failed to process the request, e.g. because the process is not permitted to use the language model
  - `tool-call-failed`: a tool failed

Errors raised by tool procedures are not wrapped in model errors; they are passed through unchanged.

**(model-error? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a model error; `#f` otherwise.

**(model-error-kind _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the kind of model error _obj_ as a symbol, or `#f` if _obj_ is not a model error.

```scheme
(guard (e ((model-error? e) (model-error-kind e)))
  (future-get (model-session-response session "...")))
```

**(model-error-details _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the details of model error _obj_ as an association list, or `#f` if _obj_ is not a model error. The details provide additional information supplied by the language model framework. Which keys are available depends on the kind of the error; keys without a value are omitted, so the list can be empty.

| Key | Value |
| :--- | :--- |
| `debug-description` | A string with a description for developers |
| `recovery-suggestion`, `failure-reason` | Strings that explain how to recover from the error and why it occurred |
| `metadata` | An association list with metadata provided by the language model |
| `context-size`, `token-count` | The size of the context window and the number of tokens of the request (`context-size-exceeded`) |
| `reset-date` | A date-time object describing when the limit gets reset (`rate-limited`, `quota-limit-reached`) |
| `limit-increase-suggestion` | A limit increase suggestion (`quota-limit-reached`) |
| `capability` | The unsupported capability: `vision`, `guided-generation`, `reasoning`, or `tool-calling` (`unsupported-capability`) |
| `content` | A transcript with the entries that are not supported (`unsupported-transcript-content`) |
| `schema-name` | The name of the schema with the unsupported guide (`unsupported-guide`) |
| `language-code` | The unsupported language code (`unsupported-language-or-locale`) |
| `schema`, `type`, `property`, `references` | The schema, type, property, or undefined types responsible for a `schema-error` |
| `raw-content`, `underlying-error` | The content that could not be decoded and the reason (`decoding-failure`) |
| `tool` | The name of the tool that failed (`tool-call-failed`) |
| `system-errors` | A list of strings naming the domains and codes of the underlying system errors (`not-permitted`, `system-error`, and `context-size-exceeded` if the system reported the overflow) |
| `refusal` | A refusal object that is used by `model-error-explanation` (`refusal`) |

```scheme
(guard (e ((eq? (model-error-kind e) 'exceeded-context-window-size)
           (cdr (assq 'debug-description (model-error-details e)))))
  (future-get (model-session-response session very-long-prompt)))
⇒  "Content contains 40059 tokens, which exceeds the maximum allowed context size of 8192."
```

**(model-error-explanation _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

If _obj_ is a model error of kind `refusal`, this procedure asks the language model why it refused to respond and returns a future for the explanation, a string. `#f` is returned if _obj_ is not a refusal.

```scheme
(guard (e ((eq? (model-error-kind e) 'refusal)
           (future-get (model-error-explanation e))))
  (future-get (model-session-response session "...")))
```
