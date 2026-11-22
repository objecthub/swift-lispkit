# LispKit Data Frame

Library `(lispkit data-frame)` provides _data frames_, i.e. tables of data organized in named, typed columns, based on Apple's _TabularData_ framework. Data frames are typically read from CSV or JSON files, analyzed (e.g. by filtering, sorting, grouping and aggregating rows, joining data frames, or computing summary statistics), and written back as CSV or JSON.

Data frames are immutable. All procedures that transform data frames return new data frames and leave their arguments unchanged.

The example program `DataFrames.scm`, which is included in the LispKit distribution, provides a guided tour through the library: it analyzes the orders of a small online bookshop.

The following code shows a few examples:

```scheme
(import (lispkit data-frame))
(define df
  (csv->data-frame
    (string-append "name,city,age,score,member\n"
                   "Alice,Zurich,34,88.5,true\n"
                   "Bob,Basel,,72.0,false\n"
                   "Carol,Zurich,29,NA,true\n"
                   "Dave,Bern,41,95.25,false\n"
                   "Eve,Basel,23,64.0,true\n")))
(display (data-frame->string df))
```

This displays the following table:

```
┏━━━┳━━━━━━━━━━┳━━━━━━━━━━┳━━━━━━━┳━━━━━━━━━━┳━━━━━━━━┓
┃   ┃ name     ┃ city     ┃ age   ┃ score    ┃ member ┃
┃   ┃ <String> ┃ <String> ┃ <Int> ┃ <Double> ┃ <Bool> ┃
┡━━━╇━━━━━━━━━━╇━━━━━━━━━━╇━━━━━━━╇━━━━━━━━━━╇━━━━━━━━┩
│ 0 │ Alice    │ Zurich   │    34 │     88.5 │ true   │
│ 1 │ Bob      │ Basel    │   nil │     72.0 │ false  │
│ 2 │ Carol    │ Zurich   │    29 │      nil │ true   │
│ 3 │ Dave     │ Bern     │    41 │    95.25 │ false  │
│ 4 │ Eve      │ Basel    │    23 │     64.0 │ true   │
└───┴──────────┴──────────┴───────┴──────────┴────────┘
5 rows, 5 columns
```

```scheme
(data-frame-column df "age")  ⇒  #(34 #f 29 41 23)
(data-frame->rows
  (data-frame-filter df (lambda (age) (and age (> age 30))) '("age")))
  ⇒  (("Alice" "Zurich" 34 88.5 #t) ("Dave" "Bern" 41 95.25 #f))
(data-frame->rows
  (data-frame-aggregate df "city" '((n count) (avg-age mean age) (best max score))))
  ⇒  (("Zurich" 2 31.5 88.5) ("Basel" 2 23.0 72.0) ("Bern" 1 41.0 95.25))
```


## Conventions

**Columns** have a _name_ and a _type_. Column names are strings; procedures accepting column names also accept symbols. The following column types are supported:

  - `integer`: exact integers (fixnums)
  - `double`: flonums
  - `boolean`: booleans
  - `string`: strings
  - `date`: date-time objects of library `(lispkit date-time)`
  - `data`: bytevectors
  - `array`: lists of values (created when reading JSON arrays)
  - `object`: association lists mapping symbols to values (created when reading JSON objects)

Procedure `data-frame-column-types` returns `other` for columns of other types; such columns can be created by the TabularData framework, but their values can only be accessed in the form of strings.

**Missing values**, e.g. empty cells in CSV files or `null` in JSON data, are represented by `#f`. In boolean columns, `#f` also represents the value _false_; procedure `data-frame-missing?` distinguishes the two cases.

**Rows** are identified by their index, starting at 0. Procedures returning rows represent them as lists of values, in the order of the columns.

**Column types are inferred** when data frames are created from Scheme values, e.g. by `make-data-frame`, unless types are specified explicitly. Values `#f` are ignored for the inference. A column is an `integer` column if all its values are fixnums, a `double` column if all its values are real numbers, a `string` column if all its values are strings or symbols, a `date` column for date-time objects, a `data` column for bytevectors, and an `array` column for lists and vectors. A column only consisting of booleans is a `boolean` column, in which `#f` denotes _false_. Columns with values of different types are rejected.

**Options** of the procedures for reading and writing CSV and JSON data and for formatting data frames are association lists mapping option names (symbols) to values, e.g. `((delimiter . #\;) (header . #f))`. Options with several values are written as lists, e.g. `(missing "" "NA")`.


## Creating data frames

**data-frame-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `data-frame` type. The `type-of` procedure of library `(lispkit type)` returns a list containing this symbol for all data frames.

**(data-frame? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a data frame; `#f` otherwise.

**(make-data-frame _columns_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-data-frame _columns types_)**  

Returns a new data frame with the given _columns_. _columns_ is an association list mapping column names to lists or vectors of values, i.e. it has the form `((name value ...) ...)`. All columns need to have the same number of values. _types_ is an association list mapping column names to column types; the types of columns not listed in _types_ are inferred from their values.

```scheme
(define cities
  (make-data-frame '(("city" "Zurich" "Basel" "Geneva")
                     ("canton" "ZH" "BS" "GE")
                     ("pop" 421878 173863 203856))))
(data-frame-column-types cities)
  ⇒  (("city" . string) ("canton" . string) ("pop" . integer))
(data-frame->rows (make-data-frame '((a 1 2)) '((a . double))))
  ⇒  ((1.0) (2.0))
```

**(rows->data-frame _names rows_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(rows->data-frame _names rows types_)**  

Returns a new data frame with columns named _names_ and the given _rows_. _rows_ is a list of rows, each being a list or vector of values, one for each column. _types_ is an association list mapping column names to column types; the types of columns not listed in _types_ are inferred from their values.

```scheme
(define d (rows->data-frame '(x y) '((1 "a") (2.5 #f))))
(data-frame-column-types d)  ⇒  (("x" . double) ("y" . string))
(data-frame->rows d)  ⇒  ((1.0 "a") (2.5 #f))
```


## Reading and writing CSV

**(csv->data-frame _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(csv->data-frame _str options_)**  

Parses CSV data and returns a data frame. _str_ is either a string or a bytevector containing UTF-8 encoded text. The types of the columns are inferred from the data, unless they are specified via option `types`. _options_ is an association list supporting the following options:

  - `header`: `#t` if the first line contains the column names (default); otherwise the columns are named `Column 0`, `Column 1`, etc.
  - `delimiter`: the character separating values (default is `#\,`)
  - `columns`: a list of the names of the columns to read (default: all columns)
  - `rows`: a pair `(start . end)` defining the range of rows to read
  - `types`: an association list mapping column names to column types (`integer`, `double`, `boolean`, `string`, `date`, or `data`)
  - `missing`: a list of strings representing missing values (default: `""`, `"#N/A"`, `"#N/A N/A"`, `"#NA"`, `"N/A"`, `"NA"`, `"NULL"`, `"n/a"`, `"nil"`, `"null"`)
  - `true`: a list of strings representing _true_ (default: `"1"`, `"True"`, `"TRUE"`, `"true"`)
  - `false`: a list of strings representing _false_ (default: `"0"`, `"False"`, `"FALSE"`, `"false"`)
  - `quoting`: `#t` if values can be quoted with `"` (default)
  - `escaping`: `#t` if characters can be escaped with the escape character (default is `#f`)
  - `escape`: the escape character (default is `#\\`)
  - `ignore-empty-lines`: `#t` if empty lines are ignored (default)
  - `date-format`: a list of date formats, e.g. `"yyyy-MM-dd"`, used for recognizing dates; dates in ISO 8601 format are recognized without this option

```scheme
(define dated
  (csv->data-frame "d;v\n2024-01-15;1\n2023-06-01;2\n"
                   '((delimiter . #\;) (date-format "yyyy-MM-dd"))))
(data-frame-column-types dated)  ⇒  (("d" . date) ("v" . integer))
```

**(load-csv-data-frame _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(load-csv-data-frame _path options_)**  

Reads the CSV file at _path_ and returns a data frame. _options_ are the same as for `csv->data-frame`.

**(data-frame->csv _df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-frame->csv _df options_)**  

Returns data frame _df_ in CSV format as a string. _options_ is an association list supporting the following options:

  - `header`: `#t` if the first line contains the column names (default)
  - `delimiter`: the character separating values (default is `#\,`)
  - `missing`: the string representing missing values (default is `""`)
  - `true`: the string representing _true_ (default is `"true"`)
  - `false`: the string representing _false_ (default is `"false"`)
  - `newline`: the string separating lines (default is `"\n"`)
  - `date-format`: the format of dates, e.g. `"yyyy-MM-dd"`; the default is the ISO 8601 format

```scheme
(data-frame->csv (data-frame-head (data-frame-select-columns df '(name age)) 2)
                 '((delimiter . #\;) (missing . "NA")))
  ⇒  "name;age\nAlice;34\nBob;NA\n"
```

**(save-csv-data-frame _path df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(save-csv-data-frame _path df options_)**  

Writes data frame _df_ in CSV format to the file at _path_. _options_ are the same as for `data-frame->csv`.


## Reading and writing JSON

JSON data representing data frames consists of an array of objects. Each object corresponds to a row and maps column names to values.

**(json->data-frame _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(json->data-frame _str options_)**  

Parses JSON data and returns a data frame. _str_ is either a string or a bytevector containing UTF-8 encoded text. The columns of the resulting data frame are sorted by name. The types of the columns are inferred from the data, unless they are specified via option `types`; booleans are inferred to be integers (0 and 1), so boolean columns need to be declared explicitly. _options_ is an association list supporting the following options:

  - `columns`: a list of the names of the columns to read (default: all columns)
  - `types`: an association list mapping column names to column types (`integer`, `double`, `boolean`, `string`, `date`, `array`, or `object`)
  - `date-format`: a list of date formats used for recognizing dates

```scheme
(define j (json->data-frame "[{\"a\":1,\"b\":[1,2],\"c\":{\"x\":1}},{\"a\":2,\"b\":[],\"c\":{}}]"))
(data-frame-column-types j)  ⇒  (("a" . integer) ("b" . array) ("c" . object))
(data-frame->rows j)  ⇒  ((1 (1 2) ((x . 1))) (2 () ()))
```

**(load-json-data-frame _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(load-json-data-frame _path options_)**  

Reads the JSON file at _path_ and returns a data frame. _options_ are the same as for `json->data-frame`.

**(data-frame->json _df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-frame->json _df options_)**  

Returns data frame _df_ in JSON format as a string. Missing values are represented by `null`. _options_ is an association list supporting the following options:

  - `pretty`: `#t` if the JSON data is formatted with indentation and line breaks (default is `#f`)
  - `sort-keys`: `#t` if the keys of objects are sorted (default is `#f`)
  - `date-format`: the format of dates; the default is the ISO 8601 format

```scheme
(data-frame->json (data-frame-head (data-frame-select-columns df '(name age)) 2)
                  '((sort-keys . #t)))
  ⇒  "[{\"age\":34,\"name\":\"Alice\"},{\"age\":null,\"name\":\"Bob\"}]"
```

**(save-json-data-frame _path df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(save-json-data-frame _path df options_)**  

Writes data frame _df_ in JSON format to the file at _path_. _options_ are the same as for `data-frame->json`.


## Displaying data frames

Besides `data-frame->string`, which uses the table format of the TabularData framework, procedure `data-frame->text-table` of library `(lispkit text-table)` turns data frames into text tables, which support more layout options, e.g. different separators and edges, custom alignment and formatting of columns, and multi-line cells.

**(data-frame->string _df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-frame->string _df options_)**  

Returns a string representing data frame _df_ as a table (see the example at the beginning). Numbers and dates are formatted according to the current locale. _options_ is an association list supporting the following options:

  - `max-rows`: the maximum number of rows that are shown (default is 20)
  - `max-width`: the maximum width of the table in characters (default is 120); columns that do not fit are omitted
  - `max-cell-width`: the maximum width of a cell in characters (default is 50)
  - `types`: `#t` if the types of the columns are shown (default)
  - `row-indices`: `#t` if the row indices are shown (default)
  - `counts`: `#t` if the number of rows and columns is shown (default)


## Accessing data frames

**(data-frame-row-count _df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of rows of data frame _df_.

**(data-frame-column-count _df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of columns of data frame _df_.

**(data-frame-column-names _df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the names of the columns of data frame _df_.

**(data-frame-column-types _df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns an association list mapping the names of the columns of data frame _df_ to their types.

```scheme
(data-frame-column-types df)
  ⇒  (("name" . string) ("city" . string) ("age" . integer) ("score" . double) ("member" . boolean))
```

**(data-frame-has-column? _df name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if data frame _df_ has a column named _name_; `#f` otherwise.

**(data-frame-column _df name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new vector containing the values of column _name_ of data frame _df_.

**(data-frame-ref _df row name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the value of column _name_ in row _row_ of data frame _df_.

**(data-frame-missing? _df row name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the value of column _name_ in row _row_ of data frame _df_ is missing; `#f` otherwise.

```scheme
(data-frame-ref df 1 "member")  ⇒  #f
(data-frame-missing? df 1 "member")  ⇒  #f
(data-frame-ref df 1 "age")  ⇒  #f
(data-frame-missing? df 1 "age")  ⇒  #t
```

**(data-frame-missing-count _df name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of missing values in column _name_ of data frame _df_.

**(data-frame-distinct-values _df name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of the distinct values of column _name_ of data frame _df_ in the order in which they first appear. Missing values are not included.

```scheme
(data-frame-distinct-values df "city")  ⇒  ("Zurich" "Basel" "Bern")
```

**(data-frame-row _df row_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the values of row _row_ of data frame _df_ as a list.

**(data-frame->rows _df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the rows of data frame _df_ as a list of lists of values.

**(data-frame->columns _df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the columns of data frame _df_ as an association list mapping column names to vectors of values.

```scheme
(data-frame->columns (make-data-frame '((a 1 2) (b "x" "y"))))
  ⇒  (("a" . #(1 2)) ("b" . #("x" "y")))
```

**(data-frame-for-each _df proc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-frame-for-each _df proc names_)**  

Calls _proc_ for every row of data frame _df_, in order. _proc_ receives the values of the columns _names_ (default: all columns) as arguments.


## Transforming data frames

**(data-frame-select-columns _df names_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a data frame consisting of the columns _names_ of data frame _df_, in the order of _names_.

**(data-frame-drop-columns _df names_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a data frame consisting of the columns of data frame _df_ except for columns _names_.

**(data-frame-rename-column _df name new-name_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a data frame in which column _name_ of data frame _df_ is renamed to _new-name_.

**(data-frame-set-column _df name values_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-frame-set-column _df name values type_)**  

Returns a data frame in which column _name_ of data frame _df_ has the given _values_, a list or vector with one value per row. If _df_ has a column _name_, it is replaced; otherwise, a new column is appended. _type_ is the type of the column; if it is not provided, the type is inferred from _values_.

**(data-frame-update-column _df name proc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-frame-update-column _df name proc type_)**  

Returns a data frame in which the values of column _name_ of data frame _df_ are replaced with the results of applying _proc_ to them. _type_ is the type of the new column; if it is not provided, the type is inferred from the new values.

```scheme
(data-frame-column (data-frame-update-column df "age" (lambda (a) (and a (* a 2)))) "age")
  ⇒  #(68 #f 58 82 46)
```

**(data-frame-compute-column _df name names proc_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-frame-compute-column _df name names proc type_)**  

Returns a data frame with a column _name_ whose values are computed by _proc_. For every row, _proc_ receives the values of the columns _names_ of data frame _df_ as arguments. If _df_ has a column _name_, it is replaced; otherwise, a new column is appended. _type_ is the type of the column; if it is not provided, the type is inferred from the computed values.

```scheme
(data-frame-column
  (data-frame-compute-column df "label" '("name" "city")
                             (lambda (n c) (string-append n "@" c)))
  "label")
  ⇒  #("Alice@Zurich" "Bob@Basel" "Carol@Zurich" "Dave@Bern" "Eve@Basel")
```

**(data-frame-filter _df pred_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-frame-filter _df pred names_)**  

Returns a data frame consisting of the rows of data frame _df_ for which _pred_ returns true. For every row, _pred_ receives the values of the columns _names_ (default: all columns) as arguments.

**(data-frame-select-rows _df indices_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a data frame consisting of the rows of data frame _df_ at the given _indices_, a list or vector of row indices, in the order of _indices_.

**(data-frame-slice _df start_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-frame-slice _df start end_)**  

Returns a data frame consisting of the rows of data frame _df_ from index _start_ (inclusive) to _end_ (exclusive). The default for _end_ is the number of rows of _df_.

**(data-frame-head _df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-frame-head _df n_)**  

Returns a data frame consisting of the first _n_ rows of data frame _df_ (default is 10).

**(data-frame-tail _df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-frame-tail _df n_)**  

Returns a data frame consisting of the last _n_ rows of data frame _df_ (default is 10).

**(data-frame-sort _df keys_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a data frame with the rows of data frame _df_ sorted by _keys_. _keys_ is either a single key or a list of keys. A key is either a column name, which sorts in ascending order, or a pair `(name . order)` where _order_ is either `ascending` or `descending`. Rows which are equal with respect to the first key are sorted by the second key, etc. Missing values are sorted last. The sort is stable. Only columns of types `integer`, `double`, `boolean`, `string`, and `date` can be used as keys.

```scheme
(data-frame-column (data-frame-sort df '(("city" . ascending) ("age" . descending))) "name")
  ⇒  #("Eve" "Bob" "Dave" "Alice" "Carol")
```

**(data-frame-append _df df1 ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a data frame consisting of the rows of data frame _df_ followed by the rows of the data frames _df1 ..._. All data frames need to have columns with the same names and types; columns are matched by name.

**(data-frame-append-rows _df rows_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a data frame consisting of the rows of data frame _df_ followed by _rows_, a list of rows, each being a list or vector of values, one for each column of _df_.

**(data-frame-join _df other on_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-frame-join _df other on kind_)**  

Joins data frame _df_ with data frame _other_ and returns the resulting data frame. Rows are matched by the values of the join columns: _on_ is either a column name which exists in both data frames, or a pair `(name . other-name)` of a column of _df_ and a column of _other_. The join columns need to have the same type. _kind_ is one of `inner` (only matching rows, the default), `left` (all rows of _df_), `right` (all rows of _other_), and `full` (all rows of both data frames). The result contains the join column, followed by the other columns of _df_, prefixed with `left.`, and the other columns of _other_, prefixed with `right.`.

```scheme
(define cantons
  (make-data-frame '(("city" "Zurich" "Basel" "Geneva") ("canton" "ZH" "BS" "GE"))))
(define joined (data-frame-join df cantons "city" 'left))
(data-frame-column-names joined)
  ⇒  ("left.name" "city" "left.age" "left.score" "left.member" "right.canton")
(data-frame-column joined "right.canton")  ⇒  #("ZH" "BS" "ZH" #f "BS")
```

**(data-frame-random-split _df proportion_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-frame-random-split _df proportion seed_)**  

Splits the rows of data frame _df_ randomly into two data frames and returns them as two values. _proportion_ is the proportion of rows (between 0.0 and 1.0) that go into the first data frame. If the fixnum _seed_ is provided, the split is reproducible. This is useful, for instance, for splitting data into training data and test data.


## Aggregating and summarizing data frames

**(data-frame-aggregate _df names_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-frame-aggregate _df names aggregations_)**  

Groups the rows of data frame _df_ by the values of the columns _names_ and returns a data frame with one row per group. _names_ is either a single column name or a list of column names; for `()`, all rows form a single group. The groups appear in the order in which they first occur in _df_. The result consists of the grouping columns followed by one column per aggregation. _aggregations_ is a list of aggregations of the form `(name op column)`, where _name_ is the name of the resulting column, _op_ is the aggregation operation, and _column_ is the name of the aggregated column. Missing values are ignored by all aggregation operations. The following operations are supported:

  - `count`: the number of values of _column_; `(name count)` without a column counts the rows of the group
  - `distinct`: the number of distinct values of _column_
  - `sum`: the sum of the numeric values of _column_ (an integer for `integer` columns, a flonum otherwise)
  - `mean`: the mean of the numeric values of _column_
  - `median`: the median of the numeric values of _column_
  - `std`: the sample standard deviation of the numeric values of _column_
  - `variance`: the sample variance of the numeric values of _column_
  - `min`: the smallest value of _column_
  - `max`: the largest value of _column_
  - `first`: the first value of _column_
  - `last`: the last value of _column_

If _aggregations_ is not provided, a column `count` with the number of rows of each group is added.

```scheme
(data-frame->rows
  (data-frame-aggregate df "city"
    '((n count) (avg-age mean age) (max-score max score) (total sum age) (first-name first name))))
  ⇒  (("Zurich" 2 31.5 88.5 63 "Alice") ("Basel" 2 23.0 72.0 23 "Bob") ("Bern" 1 41.0 95.25 41 "Dave"))
(data-frame->rows (data-frame-aggregate df 'city))
  ⇒  (("Zurich" 2) ("Basel" 2) ("Bern" 1))
```

**(data-frame-summary _df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-frame-summary _df names_)**  

Returns a data frame summarizing the columns _names_ (default: all columns) of data frame _df_. The result has one row per summarized column and the following columns: `column` (the name of the column), `mean`, `std`, `min`, `max`, `median`, `Q1` and `Q3` (the first and third quartile; these statistics are only available for numeric columns), `mode` (a list of the most frequent values), `uniqueCount` (the number of distinct values), `noneCount` (the number of missing values), and `someCount` (the number of values which are not missing).

```scheme
(display (data-frame->string (data-frame-summary df '(age score))
                             '((max-width . 100))))
```
