# LispKit Foundation Model GPT

Library `(lispkit foundation-model gpt)` provides OpenAI's GPT models as language models for library `(lispkit foundation-model)`. It is based on the Swift package [GPTForFoundationModels](https://github.com/objecthub/GPTForFoundationModels), which implements the language model protocol of Apple's FoundationModels framework for GPT models using OpenAI's Responses API. GPT models are used exactly like the other language models supported by `(lispkit foundation-model)`: sessions, guided generation, tools, streaming, and transcripts all work the same way.

The library is only available on macOS 27 and iOS 27 or newer. Requests are sent to the OpenAI API and therefore require a network connection and an API key, which can be created on the [OpenAI platform](https://platform.openai.com/api-keys). By default, requests are stateless: nothing is stored on OpenAI's side, and every request sends the conversation recorded in the session's transcript. Alternatively, responses can be stored at OpenAI so that a request only sends what is new (see option `stateful`).

```scheme
(import (lispkit foundation-model)
        (lispkit foundation-model gpt)
        (lispkit thread future))
(define model (make-gpt-model "gpt-5.5"))  ; uses OPENAI_API_KEY
(define session (make-model-session model '() "You are a concise travel assistant."))
(model-response-content
  (future-get (model-session-response session "Plan a 4-day trip to Buenos Aires.")))
```

## GPT models

**(make-gpt-model _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-gpt-model _model auth_)**  
**(make-gpt-model _model auth options_)**  

Returns a language model of family `gpt` for the GPT model _model_, a string or symbol with the model identifier, e.g. `"gpt-5.5"`. The capabilities of the models listed by `gpt-models` are known; for other models, the capabilities need to be provided via option `capabilities`.

_auth_ defines how requests are authenticated. It has one of the following forms:

  - `(api-key key)`, `(api-key key organization)`, `(api-key key organization project)`, or simply a string _key_: requests are authenticated with the API key _key_, optionally for the given organization and project (strings or `#f`). Since API keys can be extracted from apps, they should only be used for development and tools.
  - `(proxied ((header . value) ...))`: requests are sent to a proxy (see option `base-url`) which adds the credential. The given headers are sent with every request so that the proxy can authorize the caller. This form can also be used for servers that implement OpenAI's Responses API.

If _auth_ is `#f` or not provided, the API key is taken from the environment variable `OPENAI_API_KEY`, and the organization and project from `OPENAI_ORG_ID` and `OPENAI_PROJECT_ID` if they are set; a model error of kind `missing-credential` is raised if there is no API key.

_options_ is an association list with the following options:

| Option | Value |
| :--- | :--- |
| `reasoning-effort` | The reasoning effort used for all requests: `none`, `minimal`, `low`, `medium`, `high`, or `xhigh`. The effort must be supported by the model; otherwise a model error of kind `unsupported-reasoning-effort` is raised. Without this option, the reasoning level of requests determines the effort: `light` maps to `low`, `moderate` to `medium`, and `deep` to `high`. An effort the model doesn't support is replaced by the closest one it does. Without either, the model's default effort is used. |
| `reasoning-summaries` | If `#t` (the default), the model returns summaries of its reasoning, which appear as reasoning entries in transcripts. |
| `built-in-tools` | A list of tools executed by OpenAI's servers: `web-search` (searching the web) and `code-interpreter` (running Python code). Requests with a generation schema don't use built-in tools. |
| `stateful` | If `#t`, responses are stored at OpenAI (by default for 30 days), and a request continues the previous response with only what is new since. The transcript remains authoritative: if it doesn't match the stored conversation anymore, e.g. because it was edited or the stored response expired, the whole conversation is sent. The default is `#f`. |
| `base-url` | The base URL of the API, e.g. the URL of a proxy. |
| `timeout` | The timeout for requests in seconds. The default is 120. |
| `capabilities` | The capabilities of a model that is not known to the library (see `gpt-model-capabilities`). |

```scheme
(make-gpt-model "gpt-6-sol" '(api-key "sk-...")
  '((reasoning-effort . xhigh)
    (built-in-tools . (web-search code-interpreter))
    (stateful . #t)))
(make-gpt-model "gpt-5.5-pro" #f
  '((capabilities . ((reasoning-efforts high) (default-reasoning-effort . high)))))
```

Models with a reasoning effort accept a temperature and a probability threshold for sampling only while they don't reason, i.e. for effort `none`; such sampling options are ignored otherwise.

**(gpt-models)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the identifiers of all GPT models known to the library.

```scheme
(gpt-models)
⇒  ("gpt-6.2-sol" "gpt-6.2-astra" "gpt-6.1-sol" "gpt-6.1-terra" "gpt-6-sol" ...)
```

**(gpt-model-capabilities _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the capabilities of _model_, which is either a model identifier or a language model of family `gpt`, as an association list. `#f` is returned for unknown model identifiers. The association list has the following keys:
- `reasoning-efforts`: a list of the reasoning efforts the model accepts; an empty list means the model doesn't reason;
- `default-reasoning-effort`: the effort the model uses if a request doesn't specify one, or `#f`;
- `sampling-params`: the model accepts a temperature and a probability threshold while it doesn't reason;
- `structured-output`: the model supports guided generation;
- `image-input` and `document-input`: the model accepts images and PDF documents.

The same format is used for option `capabilities` of `make-gpt-model`. Keys that are missing there default to `#t`, except `reasoning-efforts`, which defaults to the empty list, `default-reasoning-effort`, which defaults to `medium` (or the lowest effort if the model doesn't support `medium`), and `sampling-params`, which defaults to `#f`.

```scheme
(gpt-model-capabilities "gpt-5.4-mini")
⇒  ((reasoning-efforts none low medium high xhigh) (default-reasoning-effort . none)
    (sampling-params . #t) (structured-output . #t) (image-input . #t) (document-input . #t))
```

**(gpt-available-models)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(gpt-available-models _auth_)**  

Returns a future which yields the models offered by the OpenAI API, sorted by their identifiers. Each model is described by an association list with keys `id`, `created` (a date-time object as defined by library `(lispkit date-time)`), and `owned-by`. _auth_ is interpreted like the corresponding argument of `make-gpt-model`. Not all of the listed models can be used as language models: models for embeddings, images, speech, transcription, or realtime sessions are listed as well.

```scheme
(map (lambda (m) (cdr (assq 'id m))) (future-get (gpt-available-models)))
⇒  ("chat-latest" "chatgpt-image-latest" "gpt-4.1" ... "gpt-6.2-sol" ...)
```

GPT models support the model-level procedures of `(lispkit foundation-model)` as follows: `language-model-name` returns the model identifier, and `language-model-token-count` counts tokens via the OpenAI API (prompts and instructions containing prompt objects or attachments that refer to image files can't be counted; `#f` is returned for them). `language-model-context-size`, `language-model-supported-languages`, and `language-model-quota-usage` return `#f`, and `language-model-supports-locale?` raises a model error of kind `unsupported`. `(available-language-models 'gpt)` returns the identifiers of the models listed by `gpt-models`.

## Transcripts

Responses of GPT models record the calls of built-in tools in their transcript entries.

**(gpt-tool-activities _transcript_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the built-in tool calls in _transcript_, in order. Each call is represented by an association list with the following keys:
- `id`;
- `kind`: `web-search` or `code-interpreter`;
- for `web-search`: `queries`, a list of the search queries, and `sources`, a list of the URLs the search consulted;
- for `code-interpreter`: `code`, the code that was run, and `output`, the logs the code printed, or `#f`;
- `status`: the status of the call, e.g. `completed`;
- `error`: `#t` if the call failed;
- `item`: the complete call as reported by the API, converted like a JSON value by library `(lispkit json)`.

```scheme
(define session
  (make-model-session (make-gpt-model "gpt-5.4-mini" #f '((built-in-tools web-search)))))
(future-get (model-session-response session "Search the web: what is the capital of Australia?"))
(gpt-tool-activities (model-session-transcript session))
⇒  (((id . "ws_0c07...") (kind . web-search) (queries "capital of Australia")
     (sources "https://..." ...) (status . completed) (error . #f) (item ...)))
```

## Errors

Besides the model errors of library `(lispkit foundation-model)`, GPT models raise model errors of the following kinds:
- `missing-credential`: there is no API key;
- `invalid-credential`: the API rejected the API key;
- `unsupported-reasoning-effort`: a reasoning effort is not supported by the model;
- `incomplete-response`: a structured response was cut off, e.g. at the output token limit.

Prompts flagged by OpenAI's safety checks raise a model error of kind `guardrail-violation`, like content blocked by other models. The safety check of reasoning models occasionally flags harmless prompts and doesn't always fire for the same prompt, so retrying a request can succeed. Other errors reported by the API are raised as model errors whose kind is the API's error code, for example `model-not-found` for an unknown model, `insufficient-quota` if the credits of the account are used up, or `invalid-json-schema`.
