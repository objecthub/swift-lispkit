# LispKit System

## File paths

Files and directories are referenced by _paths_. Paths are strings consisting of directory names separated by character `'/'` optionally followed by a file name (if the path refers to a file) and a path extension (sometimes also called _file name suffix_, if the path refers to a file). Paths are either _absolute_, if they start with character `'/'`, or they are _relative_ to some unspecified directory.

If a relative path is used to refer to a concrete directory or file, e.g. in the API provided by library `(lispkit port)`, typically the path is interpreted as relative to the path as defined by the parameter object `current-directory`, unless specified otherwise.

**current-directory** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

Defines the path referring to the _current directory_. Each LispKit virtual machine has its own current directory.

**(source-directory)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

Returns the directory in which the source file is located which is currently being compiled and executed. Typically, such source files are executed via procedure `load`.

**(home-directory)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(home-directory _username_)**  
**(home-directory _username non-sandboxed?_)**  

Returns the path of the home directory of the user identified via string _username_. If _username_ is not given or is set to `#f`, the name of the current user is used as a default. The name of the current user can be retrieved via procedure `current-user-name`. If boolean argument _non-sandboxed?_ is set to `#t`, `home-directory` will return the Unix home directory path, even if LispKit is used in a sandboxed application such as LispPad.

```scheme
(home-directory "objecthub")  ⇒  "/Users/objecthub")
```

**(system-directory _type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of paths to system directories specified via symbol _type_ for the current user. In most cases, a single value is returned. The following `type` values are supported:

  - `desktop`: The "Desktop" folder.
  - `downloads`: The "Downloads" folder.
  - `movies`: The "Movies" folder.
  - `music`: Ths "Music folder.
  - `pictures`: The "Pictures" folder.
  - `documents`: The "Documents" folder.
  - `icloud`: The "Documents" folder on iCloud.
  - `shared-public`: The "Public" folder.
  - `application-support`: The application support directory on iOS (only accessible to the application hosting LispKit)
  - `application-scripts`: The folder where AppleScript source code is stored (only available on macOS).
  - `cache`: The cache directory on iOS.
  - `temporary`: A shared temporary folder.

```scheme
(system-directory 'documents)  ⇒  ("/Users/objecthub/Documents")
(system-directory 'desktop)  ⇒  ("/Users/objecthub/Desktop")
```

**(path _path comp ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Constructs a new relative file or directory path consisting of a relative (or absolute) base path _base_ and a number of path components _comp ..._. If it is not possible to coonstruct a valid path, this procedure returns `#f`.

```scheme
(path "one" "two" "three.png")  ⇒  "one/two/three.png"
```

**(parent-path _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the parent path of _path_. The result is either a relative path if _path_ is relative, or the result is an absolute path. `parent-path` returns `#f` if _path_ is not a valid path.

```scheme
(parent-path "one/two/three.png")  ⇒  "one/two"
(parent-path "three.png")          ⇒  "."
```

**(path-components _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the individual components of a (relative or absolute) _path_ as a list of strings. Returns `#f` if _path_ is not a valid path.

```scheme
(path-components "one/two/three.png")  ⇒  ("one" "two" "three.png")
```

**(file-path _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(file-path _path base_)**   
**(file-path _path base resolve?_)**   

Constructs a new absolute file or directory path consisting of a base path _base_ and a relative file path _path_. Procedure `file-path` will also resolve symbolic links if boolean argument _resolve?_ is `#t`. The default for _resolve?_ is `#f`. Tilde is always resolved, if provided either in _path_ or _base_.

```scheme
(file-path "Photos/img.jpg" "/Users/mz/Desktop")
⇒  "/Users/mz/Desktop/Photos/img.jpg"
(file-path "~/Images/test.jpg")
⇒  "/Users/objecthub/Images/test.jpg"
(file-path "~/Images/test.jpg" "/random")
⇒  "/Users/objecthub/Images/test.jpg"
```

**(asset-file-path _name type_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(asset-file-path _name type dir_)**   

Returns a new absolute file or directory path to a LispKit _asset_. An asset is identified via a file _name_, a file _type_, and an optional directory path _dir_. _name_, _type_, and _dir_ are all strings. An asset is a file which is located directly or indirectly in one of the asset directories part of the LispKit installation. An asset has a _type_, which is the default path extension of the file (e.g. `"png"` for PNG images). If _dir_ is provided, it is a relative path to a sub-directory within a matching asset directory.

`asset-file-path` constructs a relative file path in the following way (assuming there is no existing file path extension already):

&nbsp;&nbsp;&nbsp;_dir/name.type_

It then searches the asset paths in their given order for a file matching this relative file path. Once the first matching file is found, an absolute file path for this file is returned by `asset-file-path`. If no valid (and existing) file is found, `asset-file-path` returns `#f`.

**(parent-file-path _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

If _path_ refers to a file, then `parent-file-path` returns the directory in which this file is contained. If _path_ refers to a directory, then `parent-file-path` returns the directory in which this directory is contained. The result of _parent-file-path_ is always an absolute path.

**(path-extension _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the path extension of _path_ or `#f` if there is no path extension.

```scheme
(path-extension "/foo/bar.txt")  ⇒  "txt"
(path-extension "/foo/bar")      ⇒  #f
```

**(append-path-extension _path ext opt_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Appends path extension string _ext_ to the file path _path_. The extension is added no matter whether _path_ has an extension already or not, unless _opt_ is set to `#t`, in which case extension _ext_ is only added if there is no extension already.

```scheme
(append-path-extension "/foo/bar" "txt")        ⇒  "/foo/bar.txt"
(append-path-extension "/foo/bar.txt" "mp3")    ⇒  "/foo/bar.txt.mp3"
(append-path-extension "/foo/bar.txt" "mp3" #t) ⇒  "/foo/bar.txt"
(append-path-extension "" "txt")                ⇒  #f
```

**(remove-path-extension _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the path extension of _path_ if one exists and returns the resulting path. If no path extension exists, _path_ is returned.

```scheme
(remove-path-extension "/foo/bar")         ⇒  "/foo/bar"
(remove-path-extension "/foo/bar.txt")     ⇒  "/foo/bar"
(remove-path-extension "/foo/bar.txt.mp3") ⇒  "/foo/bar.txt"
(remove-path-extension "")                 ⇒  ""
```

**(file-path-root? _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _path_ exists and corresponds to the root of the directory hierarchy. The root is typically equivalent to "/". It is an error if _path_ is not a string.


## File operations

LispKit supports ways to explore the file system, test if files or directories exist, read and write files, list directory contents, get metadata about files (e.g. file sizes), etc. Most of this functionality is provided by the libraries `(lispkit system)` and `(lispkit port)`.

**(file-exists? _filepath_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `file-exists?` procedure returns `#t` if the named file exists at the time the procedure is called, and `#f` otherwise. It is an error if _filename_ is not a string.

**(directory-exists? _dirpath_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `directory-exists?` procedure returns `#t` if the named directory exists at the time the procedure is called, and `#f` otherwise. It is an error if _filename_ is not a string.

**(file-or-directory-exists? _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `file-or-directory-exists?` procedure returns `#t` if the named file or directory exists at the time the procedure is called, and `#f` otherwise. It is an error if _filename_ is not a string.

**(file-readable? _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the file at _path_ exists and is readable; returns `#f` otherwise.

**(directory-readable? _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the directory at _path_ exists and is readable; returns `#f` otherwise.

**(file-writable? _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the file at _path_ exists and is writable; returns `#f` otherwise.

**(directory-writable? _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the directory at _path_ exists and is writable; returns `#f` otherwise.

**(file-deletable? _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the file at _path_ exists and is deletable; returns `#f` otherwise.

**(directory-deletable? _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the file at _path_ exists and is deletab; returns `#f` otherwise.

**(delete-file _filepath_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `delete-file` procedure deletes the file specified by _filepath_ if it exists and can be deleted. If the file does not exist or cannot be deleted, an error that satisfies `file-error?` is signaled. It is an error if _filepath_ is not a string.

**(delete-directory _dirpath_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `delete-directory` procedure deletes the directory specified by _dirpath_ if it exists and can be deleted. If the directory does not exist or cannot be deleted, an error that satisfies `file-error?` is signaled. It is an error if _dirpath_ is not a string.

**(delete-file-or-directory _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `delete-file-or-directory` procedure deletes the directory or file specified by _path_ if it exists and can be deleted. If _path_ neither leads to a file nor a directory or the file or directory cannot be deleted, an error that satisfies `file-error?` is signaled. It is an error if _path_ is not a string.

**(copy-file _filepath targetpath_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `copy-file` procedure copies the file specified by _filepath_ to the file specified by _targetpath_. An error satisfying `file-error?` is signaled if _filepath_ does not lead to an existing file or if a file at _targetpath_ cannot be written. It is an error if either _filepath_ or _targetpath_ are not strings.

**(copy-directory _dirpath targetpath_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `copy-directory` procedure copies the directory specified by _dirpath_ to the directory specified by _targetpath_. An error satisfying `file-error?` is signaled if _dirpath_ does not lead to an existing directory or if a directory at _targetpath_ cannot be written. It is an error if either _dirpath_ or _targetpath_ are not strings.

**(copy-file-or-directory _sourcepath targetpath_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The `copy-file-or-directory` procedure copies the file or directory specified by _sourcepath_ to the file or directory specified by _targetpath_. An error satisfying `file-error?` is signaled if _sourcepath_ does not lead to an existing file or directory, or if a file or directory at _targetpath_ cannot be written. It is an error if either _sourcepath_ or _targetpath_ are not strings.

**(move-file _filepath targetpath_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Moves the file at _filepath_ to _targetpath_. This procedure fails if _filepath_ does not reference an existing file, or if the file cannot be moved to _targetpath_. It is an error if either _filepath_ or _targetpath_ are not strings.

**(move-directory _dirpath targetpath_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Moves the directory at _dirpath_ to _targetpath_. This procedure fails if _dirpath_ does not reference an existing directory, or if the directory cannot be moved to _targetpath_. It is an error if either _dirpath_ or _targetpath_ are not strings.

**(move-file-or-directory _sourcepath targetpath_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Moves the file or directory at _sourcepath_ to _targetpath_. This procedure fails if _sourcepath_ does not reference an existing file or directory, or if the file or directory cannot be moved to _targetpath_. It is an error if either _sourcepath_ or _targetpath_ are not strings.

**(file-size _filepath_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the size of the file specificed by _filepath_ in bytes. It is an error if _filepath_ is not a string or if _filepath_ does not reference an existing file.

**(directory-list _dirpath_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of names of files and directories contained in the directory specified by _dirpath_. It is an error if _dirpath_ is not a string or if _dirpath_ does not reference an existing directory.

**(make-directory _dirpath_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a directory with path _dirpath_. If the directory exists already or if it is not possible to create a directory with path _dirpath_, `make-directory` fails with an error. It is an error if _dirpath_ is not a string.

**(open-file _filepath_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(open-file _filepath app_)**  
**(open-file _filepath app activate_)**  

Opens the file specified by _filepath_ with the application _app_. _app_ is either an application name or a file path. _activate_ is a boolean argument. If it is `#t`, it will make _app_ the frontmost application after invoking it. If _app_ is not specified, the default application for the type of the file specified by _filepath_ is used. If _activate_ is not specified, it is assumed it is `#t`. `open-file` returns `#t` if it was possible to open the file, `#f` otherwise. Example: `(open-file "/Users/objecthub/main.swift" "TextEdit")`.


## Network operations

**(open-url _url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Opens the given url in the default browser and makes the browser the frontmost application.

**(http-get _url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(http-get _url timeout_)**  

`http-get` performs an `http get` request for the given URL. `timeout` is a floating point number defining the time in seconds it should take at most for receiving a response. `http-get` returns two values: the HTTP header in form of an association list, and the content in form of a bytevector. It is an error if the `http get` request fails. Example:

```scheme
(http-get "http://github.com/objecthub")
⇒
(("Date" . "Sat, 17 Nov 2018 22:47:19 GMT")
 ("Referrer-Policy" . "origin-when-cross-origin, strict-origin-when-cross-origin")
 ("X-XSS-Protection" . "1; mode=block")
 ("Status" . "200 OK")
 ("Transfer-Encoding" . "Identity")
 ...
 ("Content-Type" . "text/html; charset=utf-8")
 ("Server" . "GitHub.com"))
#u8(10 10 60 33 68 79 67 84 89 80 69 32 104 116 109 108 62 10 60 104 116 109 108 32 108 97 110 103 61 34 101 110 34 62 10 32 32 60 104 101 97 100 62 10 32 32 32 32 60 109 101 116 97 32 99 104 97 114 115 101 116 61 34 117 116 102 ...)
```


## Time operations

**(current-second)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a floating-point number representing the current time on the International Atomic Time (TAI) scale. The value `0.0` represents midnight on January 1, 1970 TAI (equivalent to ten seconds before midnight UTC) and the value `1.0` represents one TAI second later.
**Note:** The current implementation returns the same number like `current-seconds`. This is not conforming to the R7RS spec requiring TAI scale.

**(current-jiffy)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of jiffies as a fixnum that have elapsed since an arbitrary epoch. A jiffy is a fraction of a second which is defined by the return value of the `jiffies-per-second` procedure. The starting epoch is guaranteed to be constant during a run of the program, but may vary between runs.

**(jiffies-per-second)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a fixnum representing the number of jiffies per SI second. Here is an example for how to use `jiffies-per-second`:

```scheme
(define (time-length)
  (let ((list (make-list 100000))
        (start (current-jiffy)))
    (length list)
    (/ (- (current-jiffy) start) (jiffies-per-second))))
```


## Locales

For handling locale-specific behavior, e.g. for formatting numbers and dates, library `(lispkit system)` defines a framework in which

   - regions/countries are identified via ISO 3166-1 Alpha 2-code strings,
   - languages are identified via ISO 639-1 2-letter strings, and
   - locales (i.e. combinations of regions and languages) are identified as symbols.

Library `(lispkit system)` provides functions for returning all available regions, languages, and locales. It also defines functions to map identifiers to human-readable names and to construct identifiers out of other identifiers.

**(available-regions)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of 2-letter region code identifiers (strings) for all available regions.

**(available-region? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a string matching one entry in the list of supported 2-letter region code identifiers. Otherwise, `#f` is returned.

**(region-name _ident_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(region-name _ident locale_)**  

Returns the name of the region identified by the 2-letter region code string _ident_ for the given locale _locale_. If _locale_ is not provided, the current (system-provided) locale is used.

**(region-flag _ident_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the flag of the region identified by the 2-letter region code string _ident_ as a string containing a single flag emoji.

**(region-continent _ident_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a region identifier (string) for the continent containing the region identified by the 2-letter region code string _ident_.

**(region-parent _ident_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an identifier for the parent of the region identified by the 2-letter region code string _ident_.

**(region-subregions _ident_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of identifiers (strings) for all regions contained in the region identified by the 2-letter region code string _ident_.

**(available-languages)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of 2-letter language code identifiers (strings) for all available languages.

**(available-language? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a 2-letter language code identifier string contained in the list of supported/available languages.

**(language-name _ident_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(language-name _ident locale_)** 

Returns the name of the language identified by the 2-letter language code string _ident_ for the given locale _locale_. If _locale_ is not provided, the current (system-configured) locale is used.

**(available-currencies)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of available alpha currency codes based on ISO 4217. A currency code is a 3-letter symbol.

**(available-currency? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid alpha currency code (symbol) that is contained in the list of supported/available currencies.

**(currency-name _ident_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(currency-name _ident locale_)** 

Returns the name of the currency identified by the currency identifier _ident_ for the given locale _locale_. _ident_ can either be a numeric (fixnum) or alpha (symbol or string) currency code as defined by ISO 4217. If _locale_ is not provided, the current (system-configured) locale is used.

**(currency-code _ident_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the alpha currency code of the currency identified by the currency identifier _ident_. _ident_ can either be a numeric (fixnum) or alpha (symbol or string) currency code as defined by ISO 4217. Returns `#f` if _ident_ is not a valid, available currency code symbol.

**(currency-numeric-code _ident_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the numeric currency code of the currency identified by the currency identifier _ident_. _ident_ can either be a numeric (fixnum) or alpha (symbol or string) currency code as defined by ISO 4217. Returns `#f` if _ident_ is not a valid, available currency code symbol.

**(currency-symbol _ident_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(currency-symbol _ident locale_)** 

Returns a currency symbol (e.g. "€", "$", "￡") for the currency identified by the currency identifier _ident_ for the given locale _locale_. _ident_ can either be a numeric (fixnum) or alpha (symbol or string) currency code as defined by ISO 4217. If _locale_ is not provided, the current (system-configured) locale is used. If there is no currency symbol for a given currency, then `#f` is returned.

**(available-locales)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all available locale identifiers (symbols).

**(available-locale? _locale_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the symbol _locale_ is identifying a locale supported by the operating system; returns `#f` otherwise.

**(locale)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(locale _lang_)**  
**(locale _lang country_)**  

If no argument is provided `locale` returns the current locale (symbol) as configured by the user for the operating system. If the string argument _lang_ is provided, a locale representing _lang_ (and all countries for which _lang_ is supported) is returned. If both _lang_ and string _country_ are provided, `locale` will return a symbol identifying the corresponding locale.

This function never fails if both _lang_ and _country_ are strings. It can be used for constructing canonical locale identifiers that are not supported by the underlying operating system. This can be checked with function `available-locale?`.

```scheme
(locale)           ⇒  en_US
(locale "de")      ⇒  de
(locale "en" "GB") ⇒  en_GB
(locale "en" "de") ⇒  en_DE
```

**(locale-region _locale_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the 2-letter region code string for the region targeted by the locale identifier _locale_. If _locale_ does not target a region, `locale-region` returns `#f`.

**(locale-language _locale_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the 2-letter language code string for the language targeted by the locale identifier _locale_. If _locale_ does not target a language, `locale-language` returns `#f`.

**(locale-currency _locale_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the alpha currency code as a symbol for the currency associated with the country targeted by _locale_. If _locale_ does not target a country, `locale-currency` returns `#f`.


## Execution environment

**(get-environment-variable _name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Many operating systems provide each running process with an environment consisting of _environment variables_. Both the name and value of an environment variable are represented as strings. The procedure `get-environment-variable` returns the value of the environment variable _name_, or `#f` if the named environment variable is not found. Example: `(get-environment-variable "PATH")` ⇒ `"/usr/local/bin:/usr/bin:/bin"`.

**(get-environment-variables)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the names and values of all the environment variables as an association list, where the car of each entry is the name of an environment variable and the cdr is its value, both as strings. Example: `(("USER" . "root") ("HOME" . "/"))`.

**(command-line)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the command line passed to the process as a list of strings. The first string corresponds to the command name.

**(features)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the feature identifiers which `cond-expand` treats as true. Here is an example of what `features` might return: `(modules x86-64 lispkit macosx syntax-rules complex 64bit macos little-endian dynamic-loading ratios r7rs)`. LispKit supports at least the following feature identifiers:

  * `32bit`
  * `64bit`
  * `arm`
  * `arm64`
  * `big-endian`
  * `complex`
  * `dynamic-loading`
  * `i386`
  * `ios`
  * `linux`
  * `lispkit`
  * `lisppad`
  * `little-endian`
  * `macos`
  * `macosx`
  * `modules`
  * `r7rs`
  * `ratios`
  * `repl`
  * `runloop`
  * `syntax-rules`
  * `threads`
  * `x86-64`

**(implementation-name)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the name of the Scheme implementation. For LispKit, this function returns the string "LispKit".

**(implementation-version)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the version of the Scheme implementation as a string.

**(cpu-architecture)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the CPU architecture on which this Scheme implementation is executing as a string.

**(machine-name)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a name for the particular machine on which the Scheme implementation is currently running.

**(machine-model)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an identifier for the machine on which the Scheme implementation is currently running.

**(physical-memory)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the amount of physical memory of the device executing the LispKit code in bytes.

**(memory-footprint)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the amount of memory allocated by the application executing the LispKit code in bytes.

**(system-uptime)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the uptime of the system in seconds.

**(available-network-interfaces)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(available-network-interfaces _ipv4only_)**  

Returns an association list mapping names of available network interfaces to pairs consisting of local ip addresses and network masks. When _ipv4only_ is set to `#t`, only network interfaces with IPv4 IP addresses are being returned.

```scheme
(available-network-interfaces)
⇒  (("en0" "192.168.10.175" . "255.255.255.0") ("utun24" "10.5.0.2" . "255.255.0.0"))
```

**(os-type)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the type of the operating system on which the Scheme implementation is running as a string. For macOS, this procedure returns "Darwin".

**(os-name)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the name of the operating system on which the Scheme implementation is running as a string. For macOS, this procedure returns "macOS".

**(os-version)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the build number of the operating system on which the Scheme implementation is running as a string. For macOS 10.14.1, this procedure returns "18B75".

**(os-release)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the (major) release version of the operating system on which the Scheme implementation is running as a string. For macOS 10.14.1, this procedure returns "10.14".

**(host-name)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the network host name of the computer running the LispKit interpreter as a string. This is typically the name assigned to the machine on the local network.

**(current-user-name)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the username of the user running the Scheme implementation as a string.

**(user-data _username_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns information about the user specified via _username_ in form of a list. The list provides the following information in the given order:

   1. User id (fixnum)
   2. Group id (fixnum)
   3. Username (string)
   4. Full name (string)
   5. Home directory (string)
   6. Default shell (string)

Here is an example showing the result for invocation `(user-data "objecthub")`: `(501 20 "objecthub" "Max Mustermann" "/Users/objecthub/" "/bin/bash")`.

**(terminal-size)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

If a program gets executed in a terminal window, it might be possible to determine the number of columns and rows of that window. In this case, procedure `terminal-size` returns a pair consisting of the number of columns and the number of rows (both in terms of number of characters). If this is not possible, `terminal-size` returns `#f`.


## UUIDs

**(make-uuid-string)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-uuid-string _bytevector_)**  
**(make-uuid-string _bytevector start end_)**  
**(make-uuid-string _bytevector start end_)**  

Returns a UUID string. A UUID (Universally Unique Identifier) is a 128-bit label. `make-uuid-string` returns a string with a hexadecimal representation using the 8-4-4-4-12 format. If _bytevector_ is not provided, a random UUID is returned. Otherwise, a string representation of the 16-byte bytevector between _start_ (inclusive) and _end_ (exclusive) is returned.

```scheme
(make-uuid-string) ⇒ "9AF65983-8D44-43CB-AE9B-2FF49ED898BE"
```

**(make-uuid-bytevector)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-uuid-bytevector str)**  

Returns a UUID as a bytevector. A UUID (Universally Unique Identifier) is a 128-bit label. If _str_ is not provided, a random UUID bytevector is returned. If _str_ is provided, it is assumed it is a UUID string (e.g. generated by `make-uuid-string`) and `make-uuid-bytevector` returns a 16-byte representation of this UUID as a bytevector.
