# LispKit Foundation Model Claude

Library `(lispkit foundation-model claude)` provides Anthropic's [Claude](https://www.anthropic.com/claude) models as language models for library `(lispkit foundation-model)`. It is based on the Swift package [ClaudeForFoundationModels](https://github.com/anthropics/ClaudeForFoundationModels), which implements the language model protocol of Apple's FoundationModels framework for Claude. Claude models are used exactly like the other language models supported by `(lispkit foundation-model)`: sessions, guided generation, tools, streaming, and transcripts all work the same way.

The library is only available on macOS 27 and iOS 27 or newer. Requests are sent to the Claude API and therefore require a network connection and a credential, e.g. an API key.

```scheme
(import (lispkit foundation-model)
        (lispkit foundation-model claude)
        (lispkit thread future))
(define model (make-claude-model "claude-sonnet-5-5"))  ; uses ANTHROPIC_API_KEY
(define session (make-model-session model '() "You are a concise travel assistant."))
(model-response-content
  (future-get (model-session-response session "Plan a 4-day trip to Buenos Aires.")))
```

## Claude models

**(make-claude-model _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-claude-model _model auth_)**  
**(make-claude-model _model auth options_)**  

Returns a language model of kind `claude` for the Claude model _model_, a string or symbol with the model identifier, e.g. `"claude-opus-5-5"`. The capabilities of the models listed by `claude-models` are known; for other models, the capabilities need to be provided via option `capabilities`.

_auth_ defines how requests are authenticated. It has one of the following forms:

  - `(api-key key)` or simply a string _key_: requests are authenticated with the API key _key_. Since API keys can be extracted from apps, they should only be used for development.
  - `(app-attest client-id)`: every installation of the app proves via Apple's App Attest service that it is a genuine copy of the app. _client-id_ is the client ID of the app as registered in the Anthropic console. This requires the App Attest entitlement and a physical device.
  - `(proxied ((header . value) ...))`: requests are sent to a proxy (see option `base-url`) which adds the credential. The given headers are sent with every request so that the proxy can authorize the caller.

If _auth_ is `#f` or not provided, the API key is taken from the environment variable `ANTHROPIC_API_KEY`; a model error of kind `missing-credential` is raised if the variable is not set.

_options_ is an association list with the following options:

| Option | Value |
| :--- | :--- |
| `effort` | The effort level used for all requests: `low`, `medium`, `high`, `xhigh`, or `max`. The level must be supported by the model. Without this option, the reasoning level of requests determines the effort level. |
| `fallbacks` | A list of model identifiers of up to three models that are used, in order, if the model declines a request, or `server-default` for using the fallbacks recommended by the API. |
| `server-tools` | A list of tools executed by Anthropic's servers: `(web-search filter max-uses)`, `(web-fetch filter max-uses)`, and `code-execution`. _filter_ is optional and is either `(allowing domain ...)` or `(blocking domain ...)`. _max-uses_ is an optional maximum number of uses per request. `web-search` and `web-fetch` can also be given as symbols. |
| `user-profile-id` | The identifier of the user profile on whose behalf requests are made. |
| `base-url` | The base URL of the API, e.g. the URL of a proxy. |
| `timeout` | The timeout for requests in seconds. The default is 60. |
| `capabilities` | The capabilities of a model that is not known to the library (see `claude-model-capabilities`). |

```scheme
(make-claude-model "claude-opus-5-5" '(api-key "sk-ant-...")
  '((effort . high)
    (fallbacks . ("claude-opus-4-8"))
    (server-tools . ((web-search (allowing "apple.com" "swift.org") 5) code-execution))))
```

**(claude-models)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the identifiers of all Claude models known to the library.

```scheme
(claude-models)
⇒  ("claude-haiku-5-5" "claude-sonnet-5-5" "claude-opus-5-5" "claude-fable-5-1" ...)
```

**(claude-model-capabilities _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the capabilities of _model_, which is either a model identifier or a language model of kind `claude`, as an association list. `#f` is returned for unknown model identifiers. The association list has the following keys: `sampling-params` (the model accepts sampling parameters, such as the temperature), `effort-levels` (a list of supported effort levels), `adaptive-thinking` (the model supports adaptive thinking), `structured-output` (the model supports guided generation), and `image-input` (the model accepts images). The same format is used for option `capabilities` of `make-claude-model`.

```scheme
(claude-model-capabilities "claude-opus-4-6")
⇒  ((sampling-params . #t) (effort-levels low medium high max) (adaptive-thinking . #t)
    (structured-output . #t) (image-input . #t))
```

**(claude-authenticate _model_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Authenticates the installation of the app if language model _model_ uses App Attest. This happens automatically with the first request, but can take a few seconds; calling `claude-authenticate` at launch avoids this delay. Returns a future which yields `#t` once the app is authenticated, or raises an error if authentication fails.

## Transcripts

Responses of Claude models carry additional information in their transcript entries, e.g. about server tools and fallback models. The following procedures extract this information from transcripts.

**(claude-server-tool-activities _transcript_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the server tool calls in _transcript_, in order. Each call is represented by an association list with keys `id`, `tool`, and `kind`. For kind `web-search`, there are keys `query` and `results` (a list of association lists with keys `url`, `title`, and `page-age`). For kind `web-fetch`, there are keys `url` and `document` (an association list with keys `url`, `title`, `text`, `media-type`, and `retrieved-at`). For kind `code-execution`, there are keys `code`, `stdout`, `stderr`, and `return-code`. Failed calls have key `error` with an error code. Results that have not arrived yet are missing.

**(claude-handovers _transcript i_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of handovers for the _i_-th entry of _transcript_, which needs to be a response. A handover occurs if a model declines a request and a fallback model continues the response. Each handover is represented by an association list with keys `from` (the model that declined), `to` (the fallback model), and `reason`, which is either `#f`, `(refusal category)`, or `(unrecognized type)`.

**(claude-response-model-id _transcript i_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the identifier of the model that produced the _i_-th entry of _transcript_, which needs to be a response, or `#f` if this is unknown.

## Errors

Besides the model errors of library `(lispkit foundation-model)`, Claude models raise model errors of the following kinds: `missing-credential` (there is no valid credential), `attestation-unsupported` (App Attest is not supported, e.g. on a simulator), and `attestation-failed` (App Attest failed).
