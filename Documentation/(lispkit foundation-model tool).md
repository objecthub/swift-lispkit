# LispKit Foundation Model Tool

Library `(lispkit foundation-model tool)` provides tools that are implemented natively by Apple for library `(lispkit foundation-model)`. Language models can call these tools in the same way as tools that are implemented in Scheme via `make-tool`:

  - An _OCR tool_ which reads text from images attached to a prompt
  - A _barcode reader tool_ which reads barcodes and QR codes from images attached to a prompt
  - A _Spotlight search tool_ which searches the content indexed by Spotlight, such as files, emails, or calendar events

The tools are created by procedures of this library and returned as tool objects (see `tool?` of library `(lispkit foundation-model)`). They are passed to `make-model-session` like every other tool, and they can be combined with tools implemented in Scheme. Procedures `tool-name`, `tool-description`, and `tool-parameters` return the name, the description, and the parameter schema that Apple defined for each tool.

The library is only available on macOS 27 and iOS 27 or newer.

```scheme
(import (lispkit foundation-model)
        (lispkit foundation-model tool)
        (lispkit thread future))
(define session
  (make-model-session #f (list (make-ocr-tool) (make-barcode-reader-tool))))
(model-response-content
  (future-get (model-session-response session
                (list "What is the total amount on this invoice?"
                      (image-attachment "~/Desktop/invoice.png" "invoice")))))
⇒  "The total amount on this invoice is CHF 128.50."
```

## Reading text and barcodes

The OCR tool and the barcode reader tool analyze images that are attached to a prompt of the session. A language model refers to an image by its label, so images need to be attached with a label, e.g. via `(image-attachment image label)`. The image may also have been attached to an earlier prompt of the session.

**(make-ocr-tool)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-ocr-tool _name_)**  
**(make-ocr-tool _name description_)**  

Returns a tool which reads the text of an image attached to a prompt. _name_ and _description_ are strings that override the name and description of the tool. If they are `#f` or not provided, Apple's defaults are used; the default name is `"getText"`. The tool returns the recognized text to the language model.

**(make-barcode-reader-tool)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-barcode-reader-tool _name_)**  
**(make-barcode-reader-tool _name description_)**  

Returns a tool which reads barcodes and QR codes from an image attached to a prompt. _name_ and _description_ are strings that override the name and description of the tool. If they are `#f` or not provided, Apple's defaults are used; the default name is `"readBarcodes"`. The tool returns the type and the content of the barcodes to the language model.

```scheme
(define session (make-model-session #f (list (make-barcode-reader-tool))))
(model-response-content
  (future-get (model-session-response session
                (list "Which URL does the QR code encode?"
                      (image-attachment "~/Desktop/flyer.png" "flyer")))))
⇒  "The QR code encodes the URL https://lisppad.app."
```

## Spotlight search

The Spotlight search tool lets a language model search the content indexed by Spotlight. The language model formulates a query in a query language that is described by the tool; the tool runs the query and returns the results. Results consist of items, e.g. files, together with their attributes. The tool also supports aggregations, e.g. counting items or grouping them by month.

The description of the tool explains the query language and can be large. With the default guide, the description does not fit into the context window of Apple's on-device language model. When using the on-device model, a `focused` or `dynamic` guide is needed (see option `guide` below).

Besides the results which are returned to the language model, the tool produces results for the application, e.g. all the items found by a query, or the results of pipeline stages that the language model directs to the user. These results are collected by the tool and returned by `spotlight-search-results`.

**(make-spotlight-search-tool)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-spotlight-search-tool _config_)**  

Returns a new Spotlight search tool. _config_ is an association list configuring the tool. The following keys are supported:

| Key | Value |
| :--- | :--- |
| `name` | A string overriding the name of the tool. The default name is `"spotlight_search"`. |
| `sources` | A list of sources which are searched (see below). The default is `(core-spotlight)`. |
| `guide` | Determines how the tool guides the language model in formulating queries (see below). |
| `format` | The format of the results returned to the language model: `structured` (the default) or `compact`. |
| `max-response-size` | The maximum size of the results returned to the language model. |
| `user` | The identity of the user, which is needed for queries like "emails I sent". This is either a string with the name of the user, or an association list with keys `display-name` (a string), `names`, `email-addresses`, and `phone-numbers` (lists of strings). |

A source is either a symbol or a list starting with a symbol followed by an association list of options:

  - `core-spotlight` searches the items that apps index via the Core Spotlight framework. Options: `fetch-attributes` (a list of attributes that are fetched for the items that are found), `max-results` (the maximum number of results), and, on macOS, `allow-mail` (if `#t`, emails are included; this requires an entitlement).
  - `files` searches the files indexed by Spotlight. Options: `fetch-attributes`, `max-results`, and `scopes` (a list of paths of directories to which the search is restricted).

A guide has one of the following forms:

  - `complete`: the description of the tool explains the full query language. This is the default.
  - `(focused)` or `(focused domain)`: the description is focused on searching one content domain. _domain_ is one of the symbols `items` (the default), `documents`, `communications`, `calendar`, `audio`, and `visual-media`. A domain can also be a list starting with one of these symbols followed by an association list which maps fields of the domain to lists of attributes. The fields are: `title`, `text`, `created`, and `modified` for `items`; `authors`, `keywords`, `created`, and `modified` for `documents`; `authors`, `recipients`, `sent`, `received`, and `topic` for `communications`; `organizer`, `attendees`, `location`, and `date` for `calendar`; `artist`, `album`, `transcription`, and `date` for `audio`; and `people`, `description`, `location`, and `date` for `visual-media`.
  - `(dynamic feature ...)`: the description covers the given query features: `text-match`, `similarity-match`, `numeric-match`, `dates`, `people`, and `content-type`. A feature can also be a list `(attributes attribute ...)`, which restricts queries to the given attributes.

Attributes are either symbols from the list returned by `spotlight-search-attributes`, such as `display-name` or `content-modification-date`, or strings with the names that Spotlight uses for attributes, such as `"kMDItemNamedLocation"`.

```scheme
(define spotlight
  (make-spotlight-search-tool
    '((sources (files (scopes "~/Documents") (max-results . 10)))
      (guide focused documents)
      (user . "Jane Appleseed"))))
(define session (make-model-session #f (list spotlight)))
(model-response-content
  (future-get (model-session-response session
                "Which documents about LispKit did I edit last week?")))
```

**(spotlight-search-tool? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a Spotlight search tool; `#f` otherwise.

**(spotlight-search-results _tool_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the results that Spotlight search tool _tool_ produced for the application, oldest first, since the tool was created or since `spotlight-search-results-clear!` was last called. The tool keeps the 100 most recent results. Every result is an association list with the following keys:

| Key | Value |
| :--- | :--- |
| `label` | A string labeling the result, or `#f` |
| `status` | `complete` or `partial` |
| `kind` | The kind of the result: `items`, `scored-items`, `grouped-items`, `count`, `table`, `statistic`, or `text` |
| `items` | A list of items (`items` and `scored-items`) |
| `groups` | A list of pairs `(key . items)` (`grouped-items`) |
| `count` | The number of items (`count`) |
| `columns`, `rows` | The columns of a table as pairs `(name . type)` and its rows as lists of values (`table`) |
| `name`, `value` | The name and the value of a statistic (`statistic`) |
| `text` | A string (`text`) |
| `header` | A string describing the result, or `#f` (`count`, `table`, `statistic`, and `text`) |

Items are association lists with the unique identifier of the item (key `id`), its domain identifier (key `domain`, if available), its score (key `score`, only for scored items), and all the attributes of the item that have a value, using the names returned by `spotlight-search-attributes` as keys. Which attributes are available depends on the source and on its `fetch-attributes` option. Dates are represented by date-time objects as defined by library `(lispkit date-time)`.

```scheme
(map (lambda (item) (cdr (assq 'display-name item)))
     (cdr (assq 'items (car (spotlight-search-results spotlight)))))
⇒  ("(lispkit foundation-model).md" "CHANGELOG.md" "README.md" ...)
```

**(spotlight-search-results-clear! _tool_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes all results collected by Spotlight search tool _tool_.

**(spotlight-search-attributes)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of symbols naming the Spotlight attributes supported by this library. The names are derived from the attribute names of the Core Spotlight framework, e.g. `display-name`, `content-url`, `author-names`, or `content-modification-date`.
