# LispKit Log

Library `(lispkit log)` defines a simple logging API for LispKit. Log entries are sent to a _logger_. A logger processes each log entry, e.g. by adding or filtering information, and eventually persists it if the severity of the log entry is at or above the level of the severity of the logger. Supported are logging to a port and into a file. The macOS IDE _LispPad_ implements a special version of `(lispkit log)` which makes log messages available in a session logging user interface supporting filtering, sorting, and exporting of log entries.

A log entry consists of the following four components: a timestamp, a severity, a sequence of tags, and a log message. Timestamps are generated via `current-second`. There are five severities, represented as symbols, supported by this library: `debug`, `info`, `warn`, `err`, and `fatal`. Also tags are represented as symbols. The sequence of tags is represented as a list of symbols. A log message is a string.

Logging functions take the logger as an optional argument. If it is not provided, the _current logger_ is chosen. The current logger is represented via the parameter object `current-logger`. The current logger is initially set to `default-logger`.

## Log severities

Log severities are represented using symbols. The following symbols are supported: `debug` (0), `info` (1), `warn` (2), `err` (3), and `fatal` (4). Each severity has an associated _severity level_ (previously listed in parenthesis for each severity). The higher the level, the more severe a logged issue.

**default-severity** <span style="float:right;text-align:rigth;">[object]</span>   

The default logging severity that is used if no severity is specified (initially `'debug`) when a new empty logger is created via procedure `logger`.

**(severity? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an object representing a log severity, `#f` otherwise. The following symbols are representing severities: `debug`, `info`, `warn`, `err`, and `fatal`. 

**(severity-\>level _sev_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the severity level of severity _sev_ as a fixnum.

**(severity-\>string _sev_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a human readable string (in English) for the default textual representation of the given severity _sev_.

## Log formatters

Log formatters are used by port and file loggers to map a structured logging request consisting of a timestamp, severity, log message, and logging tags into a string.

**default-log-formatter** <span style="float:right;text-align:rigth;">[object]</span>  

The default log formatting procedure. It is used by default when a new port or file logger gets created and no formatter procedure is provided.

**(long-log-formatter _timestamp sev message tags_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Formatter procedure using a long format.

**(short-log-formatter _timestamp sev message tags_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Formatter procedure using a short format.

## Logger objects

**default-logger** <span style="float:right;text-align:rigth;">[object]</span>  

The default logger that is initially created by the logging library. The native implementation for LispKit logs to standard out, the native implementation for LispPad logs into the session logging system of LispPad.

**current-logger** <span style="float:right;text-align:rigth;">[parameter object]</span>  

Parameter object referring to the current logger that is used as a default if no logger object is provided for a logging request. Initially `current-logger` is set to `default-logger`.

**logger-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `logger` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all logger objects.

**(logger? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a logger object, `#f` otherwise.

**(logger)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(logger _sev_)**  

Returns a new empty logger with the lowest persisted severity _sev_. The logger does not perform any logging action. If _sev_ is not provided, `default-severity` is used as a default.

**(make-logger _logproc lg_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-logger _logproc deinit lg_)**  

Returns a new logger with logging procedure _logproc_, the de-initialization thunk _deinit_, and a logger object _lg_ which can be used as a delegate and whose state will be inherited (e.g. the lowest persisted severity).

_logproc_ gets called by logging requests via procedures such as `log`, `log-debug`, etc. _logproc_ is a procedure with the following signature: `(logproc timestamp sev message tags)`. _timestamp_ is a floating-point number representing the number of seconds since 00:00 UTC on January 1, 1970 (e.g. returned by `current-second`), _sev_ is a severity, _message_ is the log message string, and _tags_ is a list of logging tags. A tag is represented as a symbol.

Procedure _deinit_ is called without parameters when the logger gets closed via _close-logger_ before the de-initialization procedure of _lg_ is called.

**(make-port-logger _port_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-port-logger _port formatter_)**  
**(make-port-logger _port formatter sev_)**  

Returns a new port logger object which forwards log messages formatted by _formatter_ to _port_ if the severity is above the lowest persisted severity _sev_.

_formatter_ is a procedure with the following signature: `(formatter timestamp sev message tags)`. _timestamp_ is a floating-point number representing the number of seconds since 00:00 UTC on January 1, 1970, _sev_ is a severity, _message_ is the log message string, and _tags_ is a list of logging tags. A tag is represented as a symbol.

**(make-file-logger _path_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-file-logger _path formatter_)**  
**(make-file-logger _path formatter sev_)**  

Returns a new file logger object which writes log messages formatted by _formatter_ into a new file at the given file path _path_ if the severity is above the lowest persisted severity _sev_.

_formatter_ is a procedure with the following signature: `(formatter timestamp sev message tags)`. _timestamp_ is a floating-point number representing the number of seconds since 00:00 UTC on January 1, 1970, _sev_ is a severity, _message_ is the log message string, and _tags_ is a list of logging tags. A tag is represented as a symbol.

**(make-tag-logger _tag lg_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new logger which includes _tag_ into the tags to log and forwards the logging request to logger _lg_.

**(make-filter-logger _filter lg_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new logger which filters logging requests via procedure _filter_ and forwards the requests which pass the filter to logger _lg_.

_filter_ is a predicate with the following signature: `(filter timestamp sev message tags)`. _timestamp_ is a floating-point number representing the number of seconds since 00:00 UTC on January 1, 1970, _sev_ is a severity, _message_ is the log message string, and _tags_ is a list of logging tags. A tag is represented as a symbol.

**(close-logger _lg_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Closes the logger _lg_ by calling the deinitialization procedures of the full logger chain of _lg_.

**(logger-addproc _lg_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the logging request procedure used by logger _lg_.

**(logger-severity _lg_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the default logging severity used by logger _lg_.

**(logger-severity-set! _lg sev_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Sets the default logging severity used by logger _lg_ to _sev_.

## Logging procedures

**(log _sev message_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(log _sev message tag_)**  
**(log _sev message lg_)**  
**(log _sev message tag lg_)**  

Logs message string _message_ with severity _sev_ into logger _lg_ with _tag_ if provided. If _lg_ is not provided, the current logger (as defined by parameter object `current-logger`) is used.

**(log-debug _message_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(log-debug _message tag_)**  
**(log-debug _message lg_)**  
**(log-debug _message tag lg_)**  

Logs message string _message_ with severity `debug` into logger _lg_ with _tag_ if provided. If _lg_ is not provided, the current logger (as defined by parameter object `current-logger`) is used.

**(log-info _message_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(log-info _message tag_)**  
**(log-info _message lg_)**  
**(log-info _message tag lg_)**  

Logs message string _message_ with severity `info` into logger _lg_ with _tag_ if provided. If _lg_ is not provided, the current logger (as defined by parameter object `current-logger`) is used.

**(log-warn _message_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(log-warn _message tag_)**  
**(log-warn _message lg_)**  
**(log-warn _message tag lg_)**  

Logs message string _message_ with severity `warn` into logger _lg_ with _tag_ if provided. If _lg_ is not provided, the current logger (as defined by parameter object `current-logger`) is used.

**(log-error _message_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(log-error _message tag_)**  
**(log-error _message lg_)**  
**(log-error _message tag lg_)**  

Logs message string _message_ with severity `error` into logger _lg_ with _tag_ if provided. If _lg_ is not provided, the current logger (as defined by parameter object `current-logger`) is used.

**(log-fatal _message_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(log-fatal _message tag_)**  
**(log-fatal _message lg_)**  
**(log-fatal _message tag lg_)**  

Logs message string _message_ with severity `fatal` into logger _lg_ with _tag_ if provided. If _lg_ is not provided, the current logger (as defined by parameter object `current-logger`) is used.

## Logging syntax

**(log-time _expr_)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(log-time _expr descr_)**  
**(log-time _expr descr tag_)**  
**(log-time _expr descr tag lg_)**  

Log the time for executing expression _expr_ into logger _lg_. _descr_ is a description string and _tag_ is a logging tag. If _lg_ is not provided, the current logger (as defined by parameter object `current-logger`) is used.

**(log-using _lg body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

Assigns _lg_ as the current logger and executed expressions _body ..._ in the context of this assignment.

**(log-into-file _filepath body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

Creates a new file logger at file path _filepath_, assigns the new file logger to parameter object `current-logger` and executes the statements _body ..._ in the context of this assignment.

**(log-with-tag _tag body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

Creates a new logger which appends _tag_ to the tags logged to `current-logger` and assigns the new logger to `current-logger`. _body ..._ gets executed in the context of this assignment.

**(log-from-severity _sev body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

Modifies the current logger setting its lowest persisted severity to _sev_ and executing _body ..._ in the context of this change. Once _body ..._ has been executed, the lowest persisted severity is set back to its original value.

**(log-dropping-below-severity _sev body ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

Creates a new logger on top of `current-logger` which filters out all logging requests with a severity level below the severity level of _sev_ and assigns the new logger to `current-logger`. _body ..._ gets executed in the context of this assignment.
