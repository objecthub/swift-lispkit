# LispKit SQLite

SQLite is a lightweight, embedded, relational open-source database management system. It is simple to use, requires zero configuration, is not based on a server, and manages databases directly in files.

Library `(lispkit sqlite)` provides functionality for creating, managing, and querying SQLite databases in LispKit. `(lispkit sqlite)` is a low-level library that wraps the classial C API for SQLite3. Just like in the C API, the actual SQL statements are represented as strings and compiled into statement objects that are used for executing the statements.

## Introduction

Library `(lispkit sqlite)` exports procedure `open-database` for creating new databases and connecting to existing ones. The following code will create a new database from scratch in file `~/Desktop/TestDatabase.sqlite` if that file does not exist. If the file exists, `open-database` will return a database object for accessing the database:

```scheme
(import (lispkit sqlite))
(define db (open-database "~/Desktop/TestDatabase.sqlite"))
```

A new table can be created in database _db_ with the help of an SQL `CREATE TABLE` statement. SQL statements are defined as strings and compiled into statement objects via procedure `prepare-statement`. Procedure `process-statement` is used to execute statement objects.

```scheme
(define stmt0
  (prepare-statement db
    (string-append
      "CREATE TABLE Contacts (id INTEGER PRIMARY KEY,"
      "                       name TEXT NOT NULL,"
      "                       email TEXT NOT NULL UNIQUE,"
      "                       phone TEXT);")))
(process-statement stmt0)
```

Entries can be inserted into the new table `Contacts` with a corresponding SQL statement as shown in the following listing. First, a new SQL statement is being compiled. This SQL statement contains _parameters_. These are placeholders that are defined via `?`. They can be bound to concrete values before the statement is executed using procedures `bind-parameter` and `bind-parameters`.

The SQL statement below has 4 parameters, indexed starting 1. The code below binds these parameters one by one via `bind-parameter` to concrete values before the statement is executed via `process-statement`.

```scheme
(define stmt1 (prepare-statement db "INSERT INTO Contacts VALUES (?, ?, ?, ?);"))
(bind-parameter stmt1 1 1000)
(bind-parameter stmt1 2 "Mickey Mouse")
(bind-parameter stmt1 3 "mickey@disney.net")
(bind-parameter stmt1 4 "+1 101-123-456")
(process-statement stmt1)
```

SQL statements can be reused many times. Typically, this is done by utilizing procedure `reset-statement`. If the previous execution was successful, though, this is not strictly necessary and a reset is done automatically. The code below re-applies the same statement a second time, this time using procedure `bind-parameters` to bind all parameters in one go.

```scheme
(reset-statement stmt1) ; not strictly needed here
(bind-parameters stmt1 '(1001 "Donald Duck" "donald@disney.net" "+1 101-123-456"))
(process-statement stmt1)
```

The following code shows how to query for the total number of distinct phone numbers in table `Contacts`. The first invokation of procedure `process-statement` returns `#f`, indicating that there is a result. `column-count` returns 1, which is the column containing the distinct count. The count is extracted from the statement via `column-value`. The second invokation of `process-statement` now returns `#t` as there are no further query results.

```scheme
; Count the number of distinct phone numbers.
(define stmt2 (prepare-statement db "SELECT COUNT(DISTINCT phone) FROM Contacts;"))
(process-statement stmt2) ; returns `#f`, i.e. there is a result
(display (column-count stmt2))
(newline)
(display (column-value stmt2 0))
(newline)
(process-statement stmt2) ; returns `#t`, i.e. there is no further result
```

The final example code below shows how to iterate effectively over a result table that has more than one result row.

```scheme
; Show all names and email addresses from the `Contacts` table.
(define stmt3 (prepare-statement db "SELECT name, email FROM Contacts;"))
(do ((res '() (cons (row-values stmt3) res)))
    ((process-statement stmt3) res))
```

Executing this code returns the following list:

```scheme
(("Donald Duck" "donald@disney.net") ("Mickey Mouse" "mickey@disney.net"))
```

## API

### SQLite version retrieval

**(sqlite-version)** <span style="float:right;text-align:rigth;">[procedure]</span>   

The `sqlite-version` procedure returns a string that specifies the version of the SQLite framework in use in the format "_X.Y.Z_", where _X_ is the major version number (e.g. 3 for SQLite3), Y is the minor version number, and Z is a release number.

**(sqlite-version-number)** <span style="float:right;text-align:rigth;">[procedure]</span>   

The `sqlite-version-number` procedure returns a fixnum with the value _X_*1000000 + _Y_*1000 + Z where _X_ is the major version number (e.g. 3 for SQLite3), Y is the minor version number, and Z is a release number.

### Database options

The following fixnum constants are used to specify how databases are opened or created via `make-database` and `open-database`. They can be combined by using an _inclusive or_ function such as `fxior`. For instance, `(fxior sqlite-readwrite sqlite-create)` combines the two options `sqlite-create` and `sqlite-readwrite`.

**sqlite-readonly** <span style="float:right;text-align:rigth;">[constant]</span>   

This is a fixnum value for specifying an option how databases are opened or created via `make-database` and `open-database`. With this option, the database is opened in read-only mode. If the database does not exist already, an exception is thrown.

**sqlite-readwrite** <span style="float:right;text-align:rigth;">[constant]</span>   

This is a fixnum value for specifying an option how databases are opened or created via `make-database` and `open-database`. With this option, the database is opened for reading and writing if possible, or reading only if the file cannot be written at the operating system-level. If the database does not exist already, an exception is thrown.

**sqlite-create** <span style="float:right;text-align:rigth;">[constant]</span>   

This is a fixnum value for specifying an option how databases are opened or created via `make-database` and `open-database`. This option needs to be combined with either `sqlite-readwrite` or `sqlite-readonly`. It will lead to the creation of a new database in case there is no database at the specified path.

**sqlite-default** <span style="float:right;text-align:rigth;">[constant]</span>   

This is a fixnum value for specifying an option how databases are opened or created via `make-database` and `open-database`. With this option, the database is opened for reading and writing if possible, or reading only if the file cannot be written at the operating system-level. If the database does not exist already, a new database is being created.

**sqlite-fullmutex** <span style="float:right;text-align:rigth;">[constant]</span>   

This is a fixnum value for specifying an option how databases are opened or created via `make-database` and `open-database`. With this option, the database will use the "serialized" threading mode. In this mode, multiple threads can safely attempt to use the same database connection at the same time without the need for synchronization.

**sqlite-sharedcache** <span style="float:right;text-align:rigth;">[constant]</span>   

This is a fixnum value for specifying an option how databases are opened or created via `make-database` and `open-database`. With this option, the database is opened with shared cache enabled.

**sqlite-privatecache** <span style="float:right;text-align:rigth;">[constant]</span>   

This is a fixnum value for specifying an option how databases are opened or created via `make-database` and `open-database`. With this option, the database is opened with shared cache disabled.

### Database objects

SQLite database objects are either created in memory with procedure `make-database` or they are created on disk by calling procedure `open-database`. `open-database` can also be used for opening an existing database. SQLite stores databases in regular files on disk.

**(make-database)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(make-database _options_)**  

Creates a new temporary in-memory database whose characteristics are described by _options_. _options_ is a fixnum value. If no options are specified, `sqlite-default` (= create a new read/write database in memory) is used as the default. Options are represented as fixnum values. Combinations of options are created by performing a _bitwise inclusive or_ of several option values, e.g. via `(fxior opt1 opt2)`. The following option values are predefined and can be used with `make-database`:

- `sqlite-default`: A new in-memory database is created and opened for reading and writing.
- `sqlite-fullmutex`: The database will use the "serialized" threading mode. In this mode, multiple threads can safely attempt to use the same database connection at the same time without the need for synchronization.
- `sqlite-sharedcache`: The database is opened with shared cache enabled.
- `sqlite-privatecache`: The database is opened with shared cache disabled.

**(open-database _path_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(open-database _path options_)**

Opens a database at file path _path_ whose characteristics are described by _options_. _options_ is a fixnum value. If no options are specified, `sqlite-default` (= create a new read/write database if there is not database at _path_) is used as the default. Options are represented as fixnum values. Combinations of options are created by performing a _bitwise inclusive or_ of several option values, e.g. via `(fxior opt1 opt2)`. The following option values are predefined and can be used with `open-database`:

- `sqlite-readonly`: The database is opened in read-only mode. If the database does not exist already, an exception is thrown.
- `sqlite-readwrite`: The database is opened for reading and writing if possible, or reading only if the file cannot be written at the operating system-level. If the database does not exist already, an exception is thrown.
- `sqlite-create`: This option needs to be combined with either `sqlite-readwrite` or `sqlite-readonly`. It will lead to the creation of a new database in case there is no database at the specified path.
- `sqlite-default`: The database is opened for reading and writing if possible, or reading only if the file cannot be written at the operating system-level. If the database does not exist already, a new database is being created.
- `sqlite-fullmutex`: The database will use the "serialized" threading mode. In this mode, multiple threads can safely attempt to use the same database connection at the same time without the need for synchronization.
- `sqlite-sharedcache`: The database is opened with shared cache enabled.
- `sqlite-privatecache`: The database is opened with shared cache disabled.

**(close-database _db_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Closes database _db_ and deallocates all memory related to the database. If a transaction is open at this point, the transaction is automatically rolled back.

**(sqlite-database? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a database object. Otherwise, predicate `sqlite-database?` returns `#f`.

**(database-path _db_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the file path as a string at which the database _db_ is being persisted. For in-memory databases, this procedure returns `#f`.

**(database-last-row-id _db_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Each entry in a database table (except for _WITHOUT ROWID_ tables) has a unique fixnum key called the _row id_. Procedure `database-last-row-id` returns the row id of the most recent successful insert into a table of database _db_. Inserts into _WITHOUT ROWID_ tables are not recorded. If no successful inserts into row id tables have ever occurred for an open database, then `database-last-row-id` returns zero.

**(database-last-changes _db_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

`database-last-changes` returns the number of rows modified, inserted or deleted by the most recently completed `INSERT`, `UPDATE` or `DELETE` statement on the database _db_. Executing any other type of SQL statement does not modify the value returned by `database-last-changes`.

**(database-total-changes _db_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Procedure `database-total-changes` returns the total number of rows inserted, modified or deleted by all `INSERT`, `UPDATE`, or `DELETE` statements completed since the database _db_ was opened. Executing any other type of SQL statement does not affect the value returned by `database-total-changes`.

### SQL statements

SQL statements are created with procedure `prepare-statement`. This procedure returns a statement object which encapsulates a compiled SQL query. The compiled SQL query can be executed by repeatedly calling procedure `process-statement`. As long as `process-statement` returns `#f`, a new result row can be extracted from the statement object with procedures such as `column-count`, `column-name`, `column-type`, `column-value`, `row-names`, `row-types`, `row-values`, and `row-alist`. As soon as `process-statement` returns `#t`, processing is complete. With procedure `reset-statement`, a statement object can be reset such that it can be executed again.

**(sqlite-statement? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a statement object. Otherwise, predicate `sqlite-statement?` returns `#f`.

**(prepare-statement _db str_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

To execute an SQL statement, it must first be compiled into bytecode which then gets executed, potentially multiple times, in a second step. `prepare-statement` compiles an SQL statement contained in string _str_ for execution in database _db_. It returns a _statement_ object which encapsulates the compiled query. If compilation fails, an execption is thrown.

**(parameter-count _stmt_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the number of parameters contained in statement object _stmt_. If _stmt_ contains _N_ parameters, they can be referenced by the indices 1 to _N_.

**(parameter-index _stmt name_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the index of named parameter _name_ in statement object _stmt_. _name_ is a string. The result is a positive fixnum if the named parameter exists, or `#f` if there is no parameter with name _name_.

**(parameter-name _stmt idx_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the name of the named parameter at index _idx_ in statement object _stmt_ as a string. If such a parameter does not exist, `parameter-name` returns `#f`. _idx_ is a positive fixnum.

**(bind-parameter _stmt idx val_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Binds parameter at index _idx_ to value _val_ in statement object _stmt_.

**(bind-parameters _stmt vals_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(bind-parameters _stmt vals idx_)**  

Binds the parameters starting at index _idx_ to values in list _vals_. If _idx_ is not given, 1 is used as a default. `bind-parameters` returns the tail of the list that could not be bound to parameters. _idx_ is a positive fixnum.

**(process-statement _stmt_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Procedure `process-statement` starts or proceeds executing statement _stmt_. The result of the execution step is accessible via the statement object _stmt_ and can be inspected by procedures such as `column-count`, `column-name`, `column-type`, `column-value`, `row-names`, `row-types`, `row-values`, and `row-alist`. `process-statement` returns `#f` as long as the execution is ongoing and a new resulting table row is available for inspection. When `#t` is returned, execution is complete.

**(reset-statement _stmt_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Resets the statement object _stmt_ so that it can be processed another time.

**(column-count _stmt_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

`column-count` returns the number of columns of the result of processing statement _stmt_. If _stmt_ does not yield data as a result, `column-count` returns 0.

**(column-name _stmt idx_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

`column-name` returns the name of column _idx_ of the result of executing statement _stmt_. _idx_ is a fixnum identifying the column by its 0-based index. `column-name` returns `#f` if column _idx_ does not exist.

**(column-type _stmt idx_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

`column-type` returns the type of the value at column _idx_ of the result of executing statement _stmt_. _idx_ is a fixnum identifying the column by its 0-based index. `column-type` returns `#f` if column _idx_ does not exist. Types are represented by symbols. The following types are supported:

- `sqlite-integer`: Values are fixnums
- `sqlite-float`: Values are flonums
- `sqlite-text`: Values are strings
- `sqlite-blob`: Values are bytevectors
- `sqlite-null`: There is no value (void is the only supported value)

**(column-value _stmt idx_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

`column-value` returns the value at column _idx_ of the result of executing statement _stmt_. _idx_ is a fixnum identifying the column by its 0-based index. `column-value` returns `#f` if column _idx_ does not exist.

**(row-names _stmt_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a list of all column names of the result of executing statement _stmt_.

**(row-types _stmt_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a list of all column types of the result of executing statement _stmt_.  Types are represented by symbols. The following types are supported:

- `sqlite-integer`: Values are fixnums
- `sqlite-float`: Values are flonums
- `sqlite-text`: Values are strings
- `sqlite-blob`: Values are bytevectors
- `sqlite-null`: There is no value (void is the only supported value)

**(row-values _stmt_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a list of all column values of the result of executing statement _stmt_.

**(row-alist _stmt_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns an association list associating column names with column values of the result of executing statement _stmt_.
