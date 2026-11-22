# LispKit Text-Table

Library `(lispkit text-table)` provides an API for creating tables of textual content. The library supports column and cell-based text alignment, allows for multi-line rows, and supports different types of row separators.

## Overview

A text table consists of one header row followed by regular text and separator rows. As part of the header row, it is possible to specify the text alignment of the header cell, the default text alignment of the corresponding column and a minimum and maximum size of the column (in terms of characters).

The following example shows how text tables are created:

```scheme
(define tt (make-text-table
             '(("ID" center right)
               ("Name" center left)
               ("Address" center left 10 20)
               ("Approved" center center))
             double-line-sep))
(add-text-table-row! tt
  '("1" "Mark Smith"
        "2600 Windsor Road\nRedwood City, CA" "Yes"))
(add-text-table-separator! tt line-sep)
(add-text-table-row! tt
  '("2" "Emily Armstrong"
        "160 Randy Rock Way\nMountain View, CA" "No"))
(add-text-table-separator! tt line-sep)
(add-text-table-row! tt
  '("3" "Alexander Montgomery"
        "1500 Valencia Street\nSuite 100\nLos Altos, CA" "Yes"))
(add-text-table-separator! tt line-sep)
(add-text-table-row! tt
  '("4" "Myra Jones"
        "1320 Topaz Street\nPalo Alto, CA" "Yes"))
```

A displayable string representation can be generated via procedure `text-table->string`. This is what the result looks like:

```
┌────┬──────────────────────┬──────────────────────┬──────────┐
│ ID │         Name         │       Address        │ Approved │
╞════╪══════════════════════╪══════════════════════╪══════════╡
│  1 │ Mark Smith           │ 2600 Windsor Road    │   Yes    │
│    │                      │ Redwood City, CA     │          │
├────┼──────────────────────┼──────────────────────┼──────────┤
│  2 │ Emily Armstrong      │ 160 Randy Rock Way   │    No    │
│    │                      │ Mountain View, CA    │          │
├────┼──────────────────────┼──────────────────────┼──────────┤
│  3 │ Alexander Montgomery │ 1500 Valencia Street │   Yes    │
│    │                      │ Suite 100            │          │
│    │                      │ Los Altos, CA        │          │
├────┼──────────────────────┼──────────────────────┼──────────┤
│  4 │ Myra Jones           │ 1320 Topaz Street    │   Yes    │
│    │                      │ Palo Alto, CA        │          │
└────┴──────────────────────┴──────────────────────┴──────────┘
```


## API

**text-table-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `text-table` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all text table objects.

**(text-table? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a text table object; returns `#f` otherwise.

**(text-table-header? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid text table header. A text table header is a proper list of header cells, one for each column of the text table. A header cell has one of the following forms:

   - _"title"_, just specifying the column title.
   - _("title" halign)_ where _halign_ is an alignment specifier (i.e. either `left`, `right`, `center`) that declares how the title is aligned.
   - _("title" halign calign)_  where _halign_ and _calign_ are alignment specifiers. _halign_ declares how the column title is aligned, _calign_ declares how the content in the rest of the column is aligned by default.
   - _("title" halign calign min)_ where _halign_ and _calign_ are alignment specifiers and _min_ is the minimum size of the column.
   - _("title" halign calign min max)_ where _halign_ and _calign_ are alignment specifiers and _min_ is the minimum and _max_ the maximum size of the column.

**(text-table-row? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid text table row. A text table row is a proper list of row cells, one for each column of the text table. A row cell has one of the following forms:

   - _"content"_, just specifying the content of the cell.
   - _("content" align)_ or _("content" . align)_ where _align_ is an alignment specifier (i.e. either `left`, `right`, `center`) that declares how the content in the row cell is aligned.

**(make-text-table _headers_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-text-table _headers sep_)**  
**(make-text-table _headers sep edges_)**  

Returns a new text table with the given header row. _headers_ is a valid text table header, _sep_ is a separator between header and table rows (i.e. an object for which `text-table-separator?` returns `#t`) and _edges_ specifies whether the table edges are round (`round-edges`) or sharp (`sharp-edges`).

```scheme
(make-text-table
  '(("x" center right 3 5) ("f(x)" center right))
  double-line-sep)
```

**(add-text-table-row! _table row_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Adds a new row to the given text table. _row_ is a valid text table row, i.e. it is a proper list of row cells, one for each column of the text table. A row cell is either a string, a list with two elements, a string and an alignment specifier (i.e. either `left`, `right`, `center`) which declares how the content in the row cell is aligned, or a pair consisting of a string and an alignment specifier.

**(add-text-table-separator! _table_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(add-text-table-separator! _table sep_)**  

Adds a new row separator to the given table. _sep_ is a separator, i.e. it is either `space-sep`, `line-sep`, `double-line-sep`, `bold-line-sep`, `dashed-line-sep`, or `bold-dashed-line-sep`. The default for _sep_ is `line-sep`.

**(alignment-specifier? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid alignment specifier. Supported alignment specifiers are `left`, `right`, and `center`.

**left** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  
**right**  
**center**  

Corresponds to one of the three supported alignment specifiers for text tables.

**(text-table-edges? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid text table edges specifier. Supported edges specifiers are `no-edges`, `round-edges`, and `sharp-edges`.

**no-edges** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  
**round-edges**  
**sharp-edges**  

Corresponds to one of the three supported edges specifiers for text tables.

**(text-table-separator? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a valid text table separator. Supported separators are `no-sep`, `space-sep`, `line-sep`, `double-line-sep`, `bold-line-sep`, `dashed-line-sep`, `bold-dashed-line-sep`.

**no-sep** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  
**space-sep**  
**line-sep**  
**double-line-sep**  
**bold-line-sep**  
**dashed-line-sep**  
**bold-dashed-line-sep**  

Corresponds to one of the seven supported text table separators.

**(text-table-\>string _table_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(text-table-\>string _table border_)**  

Returns the given text table as a string that can be displayed. _border_ is a boolean argument specifying whether a border is printed around the table.


## Data frames

**(data-frame-\>text-table _df_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(data-frame-\>text-table _df options_)**  

Returns a new text table showing the content of data frame _df_ as defined by library `(lispkit data-frame)`. The header row consists of the names of the columns, and there is one table row for every row of _df_. Values are converted into strings: numbers via `number->string`, booleans as `true` and `false`, date-time objects via `date-time->string`, strings as they are (strings with newlines become multi-line cells), and all other values via `write`. By default, numeric columns are right-aligned, boolean columns are centered, and all other columns are left-aligned.

_options_ is an association list supporting the following options:

  - `columns`: a list of the names of the columns to show, in the order in which they are shown (default: all columns)
  - `headers`: an association list mapping column names to header cells. A header cell is either a title string or a list of the form _("title" halign calign min max)_ as accepted by `text-table-header?`. By default, the title is the column name.
  - `align`: an association list mapping column names to alignment specifiers for the cells of the columns
  - `format`: either an association list mapping column names to procedures which convert a value of the column into a string, or a procedure which receives a value and the name of its column and returns a string. Formatting procedures are not applied to missing values.
  - `missing`: the string shown for missing values (default is `""`)
  - `max-cell-width`: the maximum width of the columns (default is 100); longer content is truncated
  - `max-rows`: the maximum number of rows shown; if _df_ has more rows, a final row consisting of `…` is added (default: all rows)
  - `row-indices`: if `#t`, a first column shows the index of each row (default is `#f`)
  - `header-sep`: the separator between the header row and the other rows (default is `bold-line-sep`)
  - `row-sep`: the separator between rows (default is `no-sep`)
  - `edges`: the edges of the table (default is `sharp-edges`)

Separators and edges can be specified either by their values, e.g. `line-sep`, or by their names as symbols, e.g. `(row-sep . line-sep)`.

```scheme
(import (lispkit data-frame))
(define df
  (csv->data-frame
    (string-append "name,city,age,score,member\n"
                   "Alice,Zurich,34,88.5,true\n"
                   "Bob,Basel,,72.0,\n"
                   "Carol,Zurich,29,NA,true\n"
                   "Dave,Bern,41,95.25,false\n")))
(display (text-table->string (data-frame->text-table df)))
```

This displays the following table:

```
┌───────┬────────┬─────┬───────┬────────┐
│ name  │  city  │ age │ score │ member │
┝━━━━━━━┿━━━━━━━━┿━━━━━┿━━━━━━━┿━━━━━━━━┥
│ Alice │ Zurich │  34 │  88.5 │  true  │
│ Bob   │ Basel  │     │  72.0 │        │
│ Carol │ Zurich │  29 │       │  true  │
│ Dave  │ Bern   │  41 │ 95.25 │ false  │
└───────┴────────┴─────┴───────┴────────┘
```

Options customize the columns, the separators, and the treatment of missing values:

```scheme
(display
  (text-table->string
    (data-frame->text-table df
      '((columns name age member)
        (headers (name . "Name") ("age" "Age" left right 6))
        (missing . "–")
        (row-sep . line-sep)
        (edges . round-edges)
        (header-sep . double-line-sep)
        (max-rows . 2)
        (row-indices . #t)))))
```

```
╭───┬───────┬────────┬────────╮
│   │ Name  │ Age    │ member │
╞═══╪═══════╪════════╪════════╡
│ 0 │ Alice │     34 │  true  │
├───┼───────┼────────┼────────┤
│ 1 │ Bob   │      – │   –    │
│ … │ …     │      … │   …    │
╰───┴───────┴────────┴────────╯
```
