# LispKit CSV

Library `(lispkit csv)` provides a simple API for reading and writing structured data in CSV format from a text file. The API provides two different levels of abstraction: reading and writing at

1. line-level (lower-level API), and
2. record-level (higher-level API).

A text file in CSV format typically has the following structure:

```
"First name", "Last name", "Birth date"
Steve, Jobs, 1955-02-24
Bill, Gates, "1955-10-28"
"Jeff", "Bezos", "1964-01-12"
```

The first line is called the _header_. It defines the names and the order of the columns. Columns are separated by a _separator_ character (which is `,` in the example above). The _column names_ can optionally be wrapped by a _quotation_ character, which is needed if the name contains, for instance, the separator character.

Each following line defines one data record which provides values for the columns defined in the header. The values are again separated by the _separator_ character and they may be optionally wrapped by the _quotation_ character. If a value is wrapped with a quotation character, the same character can be used within the value if it is escaped. The quotation character can be escaped by a sequence of two quotation characters (e.g. if `"` is used as a quotation character, `""` encodes a single `"` character within the string value).

The client of the API decides how to handle inconsistencies between the lines, e.g. if lines have too few or too many values.


## CSV ports

Both levels use a _CSV port_ to configure the textual input/output port, the separator and quotation character.

**(csv-port? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a CSV port; returns `#f` otherwise.

**(csv-input-port? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a CSV port for reading data; returns `#f` otherwise.

**(csv-output-port? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a CSV port for writing data; returns `#f` otherwise.

**(make-csv-port)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-csv-port _tport_)**  
**(make-csv-port _tport sep_)**  
**(make-csv-port _tport sep quote_)**  

Returns a new CSV port for reading or writing data via an underlying textual port _tport_. If _tport_ is an output port, the CSV port can be used for writing. If _tport_ is an input port, the CSV port can be used for reading. The default for _tport_ is the current input port `current-input-port` exported from library `(lispkit port)`.

The separation character used by the CSV port is _sep_, the quotation character is _quote_. The default for _sep_ is `#\,` and for _quote_ the default is `#\"`.

**(csv-base-port _csvp_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the textual port on which the CSV port _csvp_ is based on.

**(csv-separator _csvp_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the separation character used by the CSV port _csvp_.

**(csv-quotechar _csvp_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the quotation character used by the CSV port _csvp_.


## Line-level API

The line-level API provides means to read a whole CSV file via `csv-read` and write data in CSV format via `csv-write`.

**(csv-read _csvp_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(csv-read _csvp readheader?_)**  

Reads from CSV port _csvp_ first the header, if `readheader?` is set to `#t`, and then all the lines until the end of the input is reached. Procedure `csv-read` returns two values: the header line (a list of strings representing the column names), and a vector of all data lines, which itself are lists of strings representing the individual field values. The default for _readheader?_ is `#t`. If _readheader?_ is set to `#f`, the first result of `csv-read` is always `#f`.

**(csv-write _csvp header lines_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Writes to CSV port _csvp_ first the _header_ (a list of strings representing the column names) unless _header_ is set to `#f`. Then procedure `csv-write` writes each line of `lines`. `lines` is a vector of lists representing the individual field values in string form.


## Record-level API

The higher level API has a notion of records. The default representation for records are association lists. The functions for reading and writing records are `csv-read-records` and `csv-write-records`:

**(csv-read-records _csvp_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(csv-read-records _csvp make-col_)**  
**(csv-read-records _csvp make-col make-record_)**  

Reads from CSV port _csvp_ first the header and then all the data lines until the end of the input is reached. Header names (strings) are mapped via procedure _make-col_ into _column identifiers_ or _column factories_ (i.e. procedures which take one argument, a column value, and they return either a representation of this column if the value is valid, or `#f` if the column value is invalid). With `make-record` a list of _column identifiers_ and _column factories_ as well as a list of column values (strings) of a data line are mapped into a record. Procedure `csv-read-records` returns a vector of records.

The default _make-col_ procedure is `make-symbol-column`. The default `make-record` function is `make-alist-record/excess`.

**(csv-write-records _csvp header records_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(csv-write-records _csvp header records col-\>str_)**  
**(csv-write-records _csvp header records col-\>str field-\>str_)**  

First writes the header to CSV port _csvp_ by mapping `header`, which is a list of column identifiers. to a list of header names using procedure _col-\>str_. Then, `csv-write-records` writes all the records from the vector _records_ by mapping each record to a data line. This is done by applying _field-\>str_ to all column identifiers for the record. _field-\>str_ takes two arguments: a column identifier and the record.

The default implementation for procedure _col-\>str_ is `symbol->string`. The default implementation for procedure _field-\>str_ is `alist-field->string`.

**(make-symbol-column _str_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a symbol representing the trimmed string _str_. If the trimmed string is empty, `make-symbol-column` returns `#t`. This procedure can be used for creating column identifers out of column names in procedure `csv-read-records`.

**(make-alist-record _cols fields_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new record given a list of column identifiers or column factories (i.e. procedures which take one argument, a column value, and they return either a representation of this column if the value is valid, or `#f` if the column value is invalid) _cols_, and a list of column values _fields_.

This procedure represents records as association lists, iterating through all _cols_ and _fields_ values. If there are more _fields_ values than _cols_ expressions, than they are skipped. If there are more _cols_ expressions than _fields_ values, `#f` is used as a default for missing _fields_ values. If a _cols_ expression is a procedure, the association entry gets created by calling the procedure with the corresponding _fields_ value. For all other _cols_ expression types, a pair is created with the _cols_ expression being the car and the _fields_ value being the cdr.

**(make-alist-record/excess _cols fields_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new record given a list of column identifiers or column factories (i.e. procedures which take one argument, a column value, and they return either a representation of this column if the value is valid, or `#f` if the column value is invalid) _cols_, and a list of column values _fields_.

This procedure represents records as association lists, iterating through all _cols_ and _fields_ values. If there are more _fields_ values than _cols_ expressions, than `#f` is used as a default _cols_ expression. If there are more _cols_ expressions than _fields_ values, `#f` is used as a default for missing _fields_ values. If a _cols_ expression is a procedure, the association entry gets created by calling the procedure with the corresponding _fields_ value. For all other _cols_ expression types, a pair is created with the _cols_ expression being the car and the _fields_ value being the cdr.

**(alist-field-\>string _record col_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the column value of column _col_ from association list _record_. `alist-field->string` assumes that _record_ is an association list whose values are strings. This is how the procedure is defined:

```scheme
(define (alist-field->string record column)
  (cdr (assq column record)))
```
