# LispKit Foundation Model Gemini

Library `(lispkit foundation-model gemini)` provides Google's [Gemini](https://deepmind.google/models/gemini/) models as language models for library `(lispkit foundation-model)`. It is based on the Swift package [GeminiForFoundationModels](https://github.com/objecthub/GeminiForFoundationModels), which implements the language model protocol of Apple's FoundationModels framework for Gemini. Gemini models are used exactly like the other language models supported by `(lispkit foundation-model)`: sessions, guided generation, tools, streaming, and transcripts all work the same way.

The library is only available on macOS 27 and iOS 27 or newer. Requests are sent to the Gemini API and therefore require a network connection and an API key, which can be created in [Google AI Studio](https://aistudio.google.com/apikey). Requests are stateless: nothing is stored on Google's side, and every request sends the conversation recorded in the session's transcript.

```scheme
(import (lispkit foundation-model)
        (lispkit foundation-model gemini)
        (lispkit thread future))
(define model (make-gemini-model "gemini-3.8-flash"))  ; uses GEMINI_API_KEY
(define session (make-model-session model '() "You are a concise travel assistant."))
(model-response-content
  (future-get (model-session-response session "Plan a 4-day trip to Buenos Aires.")))
```

## Gemini models

**(make-gemini-model _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-gemini-model _model auth_)**  
**(make-gemini-model _model auth options_)**  

Returns a language model of family `gemini` for the Gemini model _model_, a string or symbol with the model identifier, e.g. `"gemini-3.8-flash"`. The capabilities of the models listed by `gemini-models` are known; for other models, the capabilities need to be provided via option `capabilities`.

_auth_ defines how requests are authenticated. It has one of the following forms:

  - `(api-key key)` or simply a string _key_: requests are authenticated with the API key _key_. Since API keys can be extracted from apps, they should only be used for development and tools.
  - `(proxied ((header . value) ...))`: requests are sent to a proxy (see option `base-url`) which adds the credential. The given headers are sent with every request so that the proxy can authorize the caller.

If _auth_ is `#f` or not provided, the API key is taken from the environment variable `GEMINI_API_KEY`; a model error of kind `missing-credential` is raised if the variable is not set.

_options_ is an association list with the following options:

| Option | Value |
| :--- | :--- |
| `thinking-level` | The thinking level used for all requests: `minimal`, `low`, `medium`, `high`, or `xhigh`. The level must be supported by the model; otherwise a model error of kind `unsupported-thinking-level` is raised. Without this option, the reasoning level of requests determines the thinking level: `light` maps to `low`, `moderate` to `medium`, and `deep` to `high`. A level the model doesn't support is replaced by the closest one it does. |
| `thought-summaries` | If `#t` (the default), the model returns summaries of its thoughts, which appear as reasoning entries in transcripts. |
| `built-in-tools` | A list of tools executed by Google's servers: `google-search` (grounding with Google Search), `url-context` (reading pages at URLs mentioned in prompts), and `code-execution` (running Python code). Requests with a generation schema don't use built-in tools. |
| `replay-earlier-thoughts` | If `#t`, requests also send the thoughts of completed turns. The API only needs the thoughts of the current turn, and it bills earlier ones as input tokens, so by default (`#f`) they are left out. |
| `retries` | How often a request is sent again after a temporary failure (rate limits, exhausted quotas, overloaded servers, lost connections), before any of its response has arrived; `#f` disables retries. The default is 2. The wait is the one the server asks for, or else an increasing backoff; if the server asks for more than 20 seconds, the request fails with a model error of kind `rate-limited` instead. |
| `base-url` | The base URL of the API, e.g. the URL of a proxy. |
| `timeout` | The timeout for requests in seconds. The default is 120. |
| `capabilities` | The capabilities of a model that is not known to the library (see `gemini-model-capabilities`). |

```scheme
(make-gemini-model "gemini-4-neon" '(api-key "AIza...")
  '((thinking-level . xhigh)
    (built-in-tools . (google-search code-execution))))
(make-gemini-model "gemini-flash-latest" #f
  '((capabilities . ((thinking-levels low medium high)))))
```

**(gemini-models)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the identifiers of all Gemini models known to the library.

```scheme
(gemini-models)
⇒  ("gemini-4-hydrogen" "gemini-4-neon" "gemini-4-argon" "gemini-4-carbon" "gemini-3.8-flash" ...)
```

**(gemini-model-capabilities _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the capabilities of _model_, which is either a model identifier or a language model of family `gemini`, as an association list. `#f` is returned for unknown model identifiers. The association list has the following keys:
- `thinking-levels`: a list of the thinking levels the model accepts; an empty list means the model doesn't take a thinking level;
- `sampling-params`: the model honors the temperature and the probability threshold of the sampling mode;
- `structured-output`: the model supports guided generation;
- `image-input`, `document-input`, `audio-input`, and `video-input`: the model accepts images, PDF documents, audio, and video.

The same format is used for option `capabilities` of `make-gemini-model`. Keys that are missing there default to `#t`, except `thinking-levels`, which defaults to the empty list, and `sampling-params`, which defaults to `#f`.

```scheme
(gemini-model-capabilities "gemini-3.6-flash")
⇒  ((thinking-levels minimal low medium high) (sampling-params . #f) (structured-output . #t)
    (image-input . #t) (document-input . #t) (audio-input . #t) (video-input . #t))
```

**(gemini-available-models)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(gemini-available-models _auth_)**  

Returns a future which yields the models offered by the Gemini API. Each model is described by an association list with keys `id`, `display-name`, `description`, `input-token-limit`, `output-token-limit`, `thinking` (the model thinks), and `methods` (a list of the API methods the model supports). _auth_ is interpreted like the corresponding argument of `make-gemini-model`. Not all of the listed models can be used as language models: models for embeddings, speech, images, or live sessions are listed as well, and models of the Gemini 2.5 family are not supported by the API that `(lispkit foundation-model gemini)` uses.

```scheme
(map (lambda (m) (cdr (assq 'id m))) (future-get (gemini-available-models)))
⇒  ("gemini-2.5-flash" "gemini-2.5-pro" ... "gemini-4-neon" ...)
```

Gemini models support the model-level procedures of `(lispkit foundation-model)` as follows: `language-model-name` returns the model identifier, and `language-model-context-size` returns the input token limit listed in the model catalog of the Gemini API (it is fetched on first use, which requires a valid API key). `language-model-supported-languages`, `language-model-token-count`, and `language-model-quota-usage` return `#f`, and `language-model-supports-locale?` raises a model error of kind `unsupported`. `(available-language-models 'gemini)` returns the identifiers of the models listed by `gemini-models`.

## Transcripts

Responses of Gemini models record the calls of built-in tools and their results in their transcript entries.

**(gemini-tool-activities _transcript_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the built-in tool calls in _transcript_, in order. Each call is represented by an association list with the following keys:
- `id`;
- `kind`: one of `google-search`, `url-context`, or `code-execution`;
- `arguments`: the arguments of the call. For `google-search` they include `queries`, a vector of search queries; for `url-context`, `urls`; for `code-execution`, `language` and `code`;
- `result`: the result of the call, or `#f` if no result was recorded;
- `error`: `#t` if the tool reported an error.

Arguments and results are converted like JSON values by library `(lispkit json)`.

```scheme
(define session
  (make-model-session (make-gemini-model "gemini-3.8-flash" #f '((built-in-tools code-execution)))))
(future-get (model-session-response session "Use code execution to compute 2 to the power of 100."))
(gemini-tool-activities (model-session-transcript session))
⇒  (((id . "call_1...") (kind . code-execution)
     (arguments (language . "python") (code . "print(2**100)\n"))
     (result . "1267650600228229401496703205376\n") (error . #f)))
```

## Errors

Besides the model errors of library `(lispkit foundation-model)`, Gemini models raise model errors of the following kinds:
- `missing-credential`: there is no API key;
- `invalid-credential`: the API rejected the API key;
- `unsupported-thinking-level`: a thinking level is not supported by the model;
- `incomplete-response`: a structured response was cut off, e.g. at the output token limit.

Other errors reported by the API are raised as model errors whose kind is the API's error code, for example `not-found` for an unknown model, `invalid-request`, or `payment-required` if the prepaid credits of the API key are used up.
