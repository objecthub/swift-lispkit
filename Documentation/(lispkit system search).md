# LispKit System Search

Library `(lispkit system search)` provides access to _Spotlight_, the search infrastructure of macOS and iOS. Spotlight maintains indices of metadata, such as file names, content types, creation and modification dates, authors, titles, image dimensions, and the textual content of documents. The library supports two kinds of searches:

  - **File search**: Procedure `spotlight-search` searches the Spotlight index of files. On macOS, this covers the whole file system that is indexed by Spotlight (like the `mdfind` command-line tool) and can be restricted to specific directories. On iOS, only files in the iCloud containers of the application can be searched; this requires that the application hosting LispKit has the iCloud entitlement.
  - **Item search**: Procedure `spotlight-search-items` searches the items that the application hosting LispKit added to its own Spotlight index via the _Core Spotlight_ framework. Items get added to the index via procedure `spotlight-index-item!` and removed via `spotlight-remove-items!`, `spotlight-remove-domains!`, and `spotlight-remove-all-items!`. Items indexed this way are also found by the system-wide Spotlight search of the device.

Both kinds of searches use the same s-expression based _query language_, which gets compiled into Spotlight query strings. Spotlight searches are asynchronous. All procedures that search Spotlight or change the index of the application return a _future_ which eventually contains the result of the operation. Procedure `future-get` of library `(lispkit thread future)` waits for the result and returns it. If the operation failed, `future-get` raises the corresponding error. Since futures are used, multiple searches can be executed concurrently.

```scheme
(import (lispkit system search)
        (lispkit thread future))
(future-get
  (spotlight-search '(and (type pdf) (text "invoice") (>= modified (days-ago 30)))
                    '(display-name modified)
                    '((scope "~/Documents") (sort (modified descending)) (limit . 20))))
⇒ (((path . "/Users/me/Documents/Taxes/Invoice-2026-11.pdf")
     (display-name . "Invoice-2026-11.pdf")
     (modified . #<date-time 2026-11-03T10:21:07+01:00>))
    ...)
```


## Queries

A _query_ is an s-expression describing which files or items to find. Queries refer to _attributes_ of files and items. An attribute is either specified by a symbol, such as `pixel-width` or `content-modification-date`, or by a string containing the raw Spotlight attribute key, such as `"kMDItemPixelWidth"` or `"com_example_rating"`. Attribute keys can be used to refer to attributes that are not known to the library, e.g. custom attributes. The list of supported attribute names is returned by procedure `spotlight-attributes`. Besides the attribute names listed in the [attribute table](#attributes) below, the following short aliases are supported:

| Alias | Files | Indexed items |
| :--- | :--- | :--- |
| `name` | `fs-name` | `display-name` |
| `size` | `fs-size` | `file-size` |
| `created` | `content-creation-date` | `content-creation-date` |
| `modified` | `content-modification-date` | `content-modification-date` |
| `last-used` | `last-used-date` | `last-used-date` |
| `added` | `date-added` | `added-date` |
| `content-types` | `content-type-tree` | `content-type-tree` |
| `tags` | `user-tags` | `keywords` |
| `text` | `text-content` | `text-content` |
| `description` | | `content-description` |

The query language supports the following forms:

  - **_string_**: A string is a shortcut for `(text string)`.
  - **(text _str ..._)**: Full-text search. Every word in the strings _str ..._ has to occur as a prefix of a word in some textual attribute of the file or item, such as the name, the title, the authors, or the textual content. The search ignores case and diacritics. `(text "lisp scheme")` matches files whose name or content contains the word "lisp" and a word starting with "scheme", e.g. "schemes".
  - **(and _query ..._)**: Matches files or items that match all of the given queries.
  - **(or _query ..._)**: Matches files or items that match at least one of the given queries.
  - **(not _query_)**: Matches files or items that do not match _query_.
  - **(name _pattern_)**: Matches files or items whose name matches _pattern_. In _pattern_, `*` matches an arbitrary sequence of characters. The match ignores case and diacritics. The name of a file is its file name (attribute `fs-name`); the name of an indexed item is its display name. Note that the pattern `"*"` on its own does not match any file names; query `#t` can be used to match all files instead.
  - **(extension _ext ..._)**: Matches files or items whose name has one of the file extensions _ext ..._. Extensions are strings or symbols, with or without the leading period.
  - **(type _kind ..._)**: Matches files or items whose content type conforms to one of the given kinds. A kind is either a string containing a uniform type identifier (UTI), such as `"public.jpeg"` or `"net.daringfireball.markdown"`, or one of the following symbols: `item`, `content`, `text`, `plain-text`, `source-code`, `image`, `movie`, `audio`, `pdf`, `folder`, `application`, `archive`, `presentation`, `spreadsheet`, `html`, `email`, `contact`, `calendar-event`, or `font`. Since a content type conforms to all of its supertypes, `(type image)` matches JPEG and PNG images alike.
  - **(tag _str ..._)**: Matches files that have one of the Finder tags _str ..._, or items with one of the keywords _str ..._. `*` can be used as a wildcard.
  - **(= _attr value_)**, **(!= _attr value_)**, **(< _attr value_)**, **(<= _attr value_)**, **(> _attr value_)**, **(>= _attr value_)**: Compares the value of attribute _attr_ with _value_. Comparisons of strings are exact; i.e. case-sensitive and without wildcards. Comparisons of attributes with multiple values, such as `authors` or `content-types`, succeed if one of the values matches.
  - **(between _attr lower upper_)**: Matches if the value of attribute _attr_ is between _lower_ and _upper_ (both inclusive).
  - **(in _attr value ..._)**: Matches if the value of attribute _attr_ is equal to one of _value ..._.
  - **(exists _attr_)**: Matches if attribute _attr_ has a value. Note that this does not work for attributes that are not stored in the Spotlight index, but computed from the file system, such as `fs-name` and `path`.
  - **(like _attr pattern flag ..._)**: Matches if a string value of attribute _attr_ matches _pattern_. In _pattern_, `*` matches an arbitrary sequence of characters. By default, the match ignores case and diacritics. This can be changed via the flags `case-sensitive` and `diacritic-sensitive`. With flag `word`, the pattern is matched against the individual words of the attribute value.
  - **(prefix _attr str flag ..._)**, **(suffix _attr str flag ..._)**, **(contains _attr str flag ..._)**: Matches if a string value of attribute _attr_ starts with, ends with, or contains _str_. All characters of _str_, including `*`, are matched literally. The same flags as for `like` are supported.
  - **(during _attr range_)**: Matches if the date of attribute _attr_ is within _range_. A range is one of the symbols `today`, `yesterday`, `this-week`, `last-week`, `this-month`, `last-month`, `this-year`, and `last-year`, a list `(last-days n)` referring to the last _n_ days up to now, or a list `(start end)` of two dates. A range includes its start and excludes its end.
  - **(raw _str_)**: A query in the [Spotlight query syntax](https://developer.apple.com/library/archive/documentation/Carbon/Conceptual/SpotlightQuery/Concepts/QueryFormat.html), e.g. `(raw "kMDItemFSName == 'Read*'c")`. Raw queries are not supported for file searches on iOS.
  - **#t**: Matches all files or items.

Values in comparisons are strings, numbers, booleans, and date-time objects as defined by library `(lispkit date-time)`. Furthermore, the following relative dates are supported:

  - `now`: The current date and time.
  - `today`, `yesterday`, `tomorrow`: The start of the current, previous, and next day.
  - `(minutes-ago n)`, `(hours-ago n)`, `(days-ago n)`, `(weeks-ago n)`, `(months-ago n)`, `(years-ago n)`: The current date and time minus _n_ minutes, hours, days, weeks, months, or years.

Relative dates are computed when a query gets compiled.

Here are a few examples of queries:

```scheme
;; Scheme source files modified during the last week
(and (extension "scm" "sld") (during modified (last-days 7)))
;; Images that are at least 4000 pixels wide, excluding screenshots
(and (type image) (>= pixel-width 4000) (not (name "Screenshot*")))
;; PDF documents with author "Zenger" whose title contains "LispKit"
(and (type pdf) (like authors "*zenger*") (contains title "LispKit"))
;; Files with Finder tag "Important" or "Urgent"
(tag "Important" "Urgent")
;; Movies longer than an hour
(and (type movie) (> duration-seconds 3600))
```

**(spotlight-query-string _query_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(spotlight-query-string _query source_)**  

Returns the Spotlight query string into which _query_ gets compiled. _source_ is either `files` (default) or `items`; it determines how attributes are mapped to Spotlight attribute keys. Query strings for files can be used with the `mdfind` command-line tool. `spotlight-query-string` raises an error if _query_ is malformed.

```scheme
(spotlight-query-string '(and (extension "scm") (> size 1000)))
⇒ "(kMDItemFSName == \"*.scm\"cd && kMDItemFSSize > 1000)"
(spotlight-query-string '(or (text "quokka") (= domain "notes")) 'items)
⇒ "(** == \"quokka*\"cdw || domainIdentifier == \"notes\")"
```


## Searching files

**(spotlight-search _query_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(spotlight-search _query attrs_)**  
**(spotlight-search _query attrs options_)**  

Searches the Spotlight index of files for files matching _query_ and returns a future which eventually contains the result of the search. _attrs_ determines the shape of the result:

  - If _attrs_ is `#f` or omitted, the result is a list of file paths.
  - If _attrs_ is a symbol or string referring to an attribute, the result is a list of the values of this attribute, one per file. Files that do not have a value for the attribute are represented by `#f`.
  - If _attrs_ is a list of attributes, the result is a list of association lists, one per file, mapping the attributes to their values. Each association list starts with attribute `path`. Attributes without a value are not included.

Attribute values are mapped to strings, numbers, booleans, date-time objects, bytevectors, or lists of such values.

_options_ is an association list supporting the following options:

  - `(scope s ...)`: The scopes of the search. A scope is either a path of a directory (a string), or one of the following symbols: `computer` (all local volumes), `indexed-computer` (all indexed local volumes), `home` (the home directory of the user), `network` (all mounted network volumes), `indexed-network` (all indexed network volumes), `icloud-documents` (the documents in the iCloud containers of the application), `icloud-data` (all other files in the iCloud containers of the application), and `icloud-external` (iCloud documents outside of the containers of the application that the application can access). On iOS, only the iCloud scopes are supported. The default scope is `computer` on macOS and `icloud-documents` on iOS.
  - `(sort spec ...)`: Sorts the results. A sort specification is either an attribute or a list `(attr order)` where _order_ is either `ascending` (the default) or `descending`. The first specification determines the order; the following ones are used for files that have equal values for the previous specifications.
  - `(limit . n)`: The maximum number of results. The default is 10000. `(limit . #f)` removes the limit, but this should be used with care: searches can return hundreds of thousands of files, and very long lists can exhaust the stack of LispKit. Without sort specification, the search stops as soon as _n_ files are found and the results are not ordered. With sort specification, all files are found and sorted before the first _n_ files are returned.
  - `(timeout . secs)`: The maximum time the search may take in seconds. When the timeout is reached, the search stops and the future gets the results found so far. A timeout passed to `future-get` only determines how long `future-get` waits for the result; it does not stop the search.

If the evaluation of a program is aborted while a search is running, the search is stopped as well.

```scheme
(define docs "~/Documents")
;; Paths of all Markdown files in the documents folder
(future-get (spotlight-search '(type "net.daringfireball.markdown") #f `((scope ,docs))))
;; Names and sizes of the three largest images
(future-get (spotlight-search '(type image) '(name size)
                              `((scope ,docs) (sort (size descending)) (limit . 3))))
;; Pixel widths of all images
(future-get (spotlight-search '(type image) 'pixel-width `((scope ,docs))))
;; Two searches running concurrently
(let ((scm (spotlight-search '(extension "scm") #f `((scope ,docs))))
      (pdf (spotlight-search '(type pdf) #f `((scope ,docs)))))
  (list (length (future-get scm)) (length (future-get pdf))))
```

**(spotlight-metadata _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(spotlight-metadata _path attrs_)**  

Returns the Spotlight metadata of the file at _path_ as an association list, similar to the `mdls` command-line tool. If _attrs_ is a list of attributes, only the given attributes are returned and the association list starts with attribute `path`. Otherwise, all available attributes are returned; attributes not known to the library are represented by strings containing their Spotlight attribute key. `spotlight-metadata` returns `#f` if there is no metadata for _path_. Unlike the other procedures of this library, `spotlight-metadata` is synchronous. It is only supported on macOS.

```scheme
(spotlight-metadata "~/Pictures/Billboard.jpg" '(name kind pixel-width content-types))
⇒ ((path . "/Users/me/Pictures/Billboard.jpg") (name . "Billboard.jpg")
    (kind . "JPEG image") (pixel-width . 2000)
    (content-types "public.jpeg" "public.image" "public.data" "public.item" "public.content"))
```


## Indexing and searching items

Applications can add their own content to Spotlight. Such _items_ are identified by a unique identifier (a string) and optionally belong to a _domain_ (also a string), which can be used to group related items, e.g. to remove them all at once. Items are described by attributes such as `title`, `content-description`, `keywords`, or `text-content`. Items are only visible to the application that indexed them and to the Spotlight search of the device. By default, Spotlight removes items 30 days after they were indexed.

**(spotlight-index-available?)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the device supports indexing items; `#f` otherwise.

**(spotlight-index-item! _id attrs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(spotlight-index-item! _id attrs domain_)**  
**(spotlight-index-item! _id attrs domain expiration_)**  

Adds an item with identifier _id_ to the Spotlight index of the application, or replaces the item with this identifier. _attrs_ is an association list mapping attributes (symbols) to their values. Attribute `content-type` specifies the content type of the item as a UTI string (or a file extension); the default is `"public.content"`. Values are converted according to the type of the attribute: strings, numbers, booleans, date-time objects, lists of strings or dates, bytevectors for data attributes, and URLs or file paths (strings) for URL attributes. Attributes `authors`, `primary-recipients`, `additional-recipients`, and `hidden-additional-recipients` refer to lists of persons. A person is either a name (string) or a list consisting of a name followed by email addresses. Attributes specified by strings define custom attributes; their values are strings, numbers, booleans, date-time objects, or lists of these. _domain_ is the domain of the item (a string), or `#f` if the item does not belong to a domain. _expiration_ is a date-time object specifying when Spotlight removes the item from the index, or the symbol `never`. If _expiration_ is omitted or `#f`, the item expires after 30 days.

`spotlight-index-item!` returns a future which eventually contains `#t` if the item was added successfully. It is an error if an attribute is unknown or not supported on the current system.

```scheme
(future-get
  (spotlight-index-item! "note-17"
    `((title . "Quokka observations")
      (content-description . "Notes from the trip to Rottnest Island")
      (keywords "quokka" "travel")
      (content-type . "public.plain-text")
      (rating . 4)
      (content-modification-date . ,(date-time))
      ("com_example_trip" . "Rottnest"))
    "notes"
    'never))
⇒ #t
```

**(spotlight-search-items _query_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(spotlight-search-items _query attrs_)**  
**(spotlight-search-items _query attrs options_)**  

Searches the items that the application added to its Spotlight index for items matching _query_ and returns a future which eventually contains the result of the search. _attrs_ determines the shape of the result like for `spotlight-search`. Besides the attributes of items, the pseudo attributes `id` and `domain` can be used to refer to the identifier and the domain of items. If _attrs_ is `#f` or omitted, the result is a list of item identifiers. If _attrs_ is a list, the association lists start with attribute `id`. _options_ supports the options `sort`, `limit`, and `timeout` as described for `spotlight-search`.

Queries for items can refer to the domain of items via `(= domain "...")`; the identifiers of items cannot be used in queries.

```scheme
(future-get (spotlight-search-items '(text "quokka")))
⇒ ("note-17")
(future-get (spotlight-search-items '(= domain "notes") '(title keywords rating)
                                    '((sort (rating descending)))))
⇒ (((id . "note-17") (title . "Quokka observations") (keywords "quokka" "travel") (rating . 4))
    ((id . "note-3") (title . "Packing list") (keywords "travel") (rating . 2)))
```

**(spotlight-remove-items! _id ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the items with identifiers _id ..._ from the Spotlight index of the application. Returns a future which eventually contains `#t`.

**(spotlight-remove-domains! _domain ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes all items belonging to the domains _domain ..._ from the Spotlight index of the application. Returns a future which eventually contains `#t`.

**(spotlight-remove-all-items!)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes all items from the Spotlight index of the application. Returns a future which eventually contains `#t`.


## Attributes

**(spotlight-attributes)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(spotlight-attributes _source_)**  

Returns a list of the names of all attributes supported for _source_, which is either `files` (default) or `items`. Aliases are not included.

**(spotlight-attribute-key _attr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(spotlight-attribute-key _attr source_)**  

Returns the Spotlight key for attribute _attr_ (a symbol or string) for _source_, which is either `files` (default) or `items`. For files, keys are Spotlight metadata attribute keys; for items, keys are properties of `CSSearchableItemAttributeSet`. It is an error if _attr_ is not a known attribute.

```scheme
(spotlight-attribute-key 'name)         ⇒ "kMDItemFSName"
(spotlight-attribute-key 'pixel-width)  ⇒ "kMDItemPixelWidth"
(spotlight-attribute-key 'name 'items)  ⇒ "displayName"
```

The names of the attributes of files are derived from the Spotlight metadata attribute keys by dropping the prefix `kMDItem` and converting the remaining name into lowercase words separated by hyphens, e.g. `kMDItemContentModificationDate` becomes `content-modification-date` and `kMDItemFSName` becomes `fs-name`. The most commonly used attributes of files are:

| Attribute | Description |
| :--- | :--- |
| `path` | Full path of the file |
| `fs-name` | Name of the file, including its extension |
| `display-name` | Name of the file as shown to users (localized, possibly without extension) |
| `fs-size` | Size of the file in bytes |
| `kind` | Localized description of the content type, e.g. "JPEG image" |
| `content-type` | Uniform type identifier of the content type, e.g. "public.jpeg" |
| `content-type-tree` | Content type and all its supertypes |
| `content-creation-date` | Date when the content was created |
| `content-modification-date` | Date when the content was last modified |
| `fs-creation-date` | Date when the file was created |
| `fs-content-change-date` | Date when the file was last modified |
| `last-used-date` | Date when the file was last opened |
| `date-added` | Date when the file was added to its current location |
| `user-tags` | Finder tags |
| `title`, `authors`, `subject`, `description`, `keywords`, `comment`, `copyright` | Descriptive metadata of documents |
| `text-content` | Textual content of the document (can only be searched, not retrieved) |
| `number-of-pages`, `page-width`, `page-height` | Properties of documents |
| `pixel-width`, `pixel-height`, `pixel-count`, `color-space`, `orientation` | Properties of images |
| `acquisition-make`, `acquisition-model`, `lens-model`, `iso-speed`, `f-number`, `focal-length` | Camera properties of photos |
| `latitude`, `longitude`, `altitude`, `city`, `country`, `named-location` | Location metadata |
| `duration-seconds`, `codecs`, `audio-bit-rate`, `video-bit-rate` | Properties of audio and video files |
| `album`, `authors`, `composer`, `musical-genre`, `audio-track-number`, `recording-year` | Properties of music |
| `where-froms` | URLs from which the file was downloaded |
| `cf-bundle-identifier`, `version` | Properties of applications |

The names of the attributes of items are derived from the properties of class `CSSearchableItemAttributeSet` of the Core Spotlight framework, e.g. property `contentDescription` becomes `content-description`. Commonly used attributes of items are `display-name`, `title`, `content-description`, `keywords`, `text-content`, `content-type`, `content-type-tree`, `content-creation-date`, `content-modification-date`, `rating`, `url`, `content-url`, `thumbnail-url`, `thumbnail-data`, `authors`, `start-date`, `end-date`, `due-date`, `latitude`, `longitude`, `city`, and `country`.
