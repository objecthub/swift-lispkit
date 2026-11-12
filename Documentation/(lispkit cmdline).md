# LispKit Cmdline

Library `(lispkit cmdline)` supports the parsing of command-line arguments of scripts and programs. It provides two distinct solutions: one based on parsers that are described by a table of flags and handler procedures, inspired by Racket's `racket/cmdline` library, and one based on a fold-driven approach inspired by SRFI 37.

Procedure `parse-command-line` implements the table-based approach. A parser is described by a table, which is plain data (lists, strings, symbols and procedures) that is passed to `parse-command-line` as an ordinary argument. The table lists the flags of the program, tells the parser what to do for each flag, and provides the text for a generated help message.

A lower-level API is provided by procedure `args-fold`. `args-fold` is a _fold_ over the list of command-line arguments: the arguments are processed from left to right, and the procedures that handle the options thread one or more _seed_ values through the iteration.

## Table-driven command-line parsing

The following program provides an example for how to use procedure `parse-command-line`:

```scheme
(import (lispkit base) (lispkit cmdline))

(define verbose #f)
(define level 0)
(define links '())

(parse-command-line "compile" (cdr (command-line))
  `((once-each
      (("-v" "--verbose")
        ,(lambda (flag) (set! verbose #t))
        ("Compile with verbose messages")))
    (once-any
      (("-o" "--optimize-1")
        ,(lambda (flag) (set! level 1))
        ("Optimization level 1"))
      (("-O" "--optimize-2")
        ,(lambda (flag) (set! level 2))
        ("Optimization level 2")))
    (multi
      (("-l" "--link-flags")
        ,(lambda (flag lf) (set! links (cons lf links)))
        ("Add a flag for the linker" "lf"))))
  (lambda (accum . files) (for-each compile files))
  '("file" "..."))
```

Running this program with the argument `--help` displays the following help text:

```
compile [ <option> ... ] <file> <...>
 where <option> is one of
  -v, --verbose : Compile with verbose messages
/ -o, --optimize-1 : Optimization level 1
\ -O, --optimize-2 : Optimization level 2
  -l, --link-flags <lf> : Add a flag for the linker
  -h, --help : Show this help
  -- : Do not treat any remaining argument as a flag (at this level)
 Multiple single-letter flags can be combined after one `-'; for
  example: `-h-' is the same as `-h --'
```

**(parse-command-line _name argv table finish arg-help_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(parse-command-line _name argv table finish arg-help help-proc_)**  
**(parse-command-line _name argv table finish arg-help help-proc unknown-proc_)**  

Parses the command-line arguments _argv_ according to _table_ and returns the result of calling procedure _finish_.

   - _name_ is a string with the name of the program. It is used in error messages and in the help text.
   - _argv_ is a list or a vector of strings, e.g. `(cdr (command-line))`, which skips the first element of the command line, the name of the script.
   - _table_ is a list of entries, which describe the flags and the help text (see below).
   - _finish_ is a procedure which is called when all flags have been processed. Its first argument is a list of the results of the flag handlers, in the order in which the flags appeared; handler results that are the void value are left out. The remaining arguments of _finish_ are the arguments that are not flags (e.g. file names), in the order they appeared. It is an error if _finish_ does not accept this number of arguments.
   - _arg-help_ is a list of strings describing the non-flag arguments that are expected by the program. They are used in the first line of the help text.
   - _help-proc_ is a procedure which is called with the help text (a string) when the flag `-h` or `--help` is encountered. The default procedure displays the text on the current output port and calls `(exit 0)`. If a custom procedure returns, the parsing continues.
   - _unknown-proc_ is a procedure which is called with the flag string for each flag not described in _table_. Its result is treated like the result of a flag handler. The default procedure raises an error.

Processing of the arguments is as follows:

   - An argument is a _flag_ if it starts with `-` or `+`, has at least two characters, and is not a number. Numbers, such as `-5`, as well as single hyphens `-` are treated like all other arguments that are not flags.
   - Processing stops with the first argument that is not a flag. This and the remaining arguments are passed to _finish_.
   - The special argument `--` ends the flag processing. The remaining arguments are passed to _finish_ without being interpreted.
   - Short flags consisting of a single-character may be combined in a single argument, e.g. `-vl` is the same as `-v -l`. A flag with arguments consumes the arguments following it, even if they look like flags.
   - Arguments of long flags are not part of the flag. They are provided as separate arguments: `--link-flags lf`.

Errors are raised if a `once-each` flag appears more than once, if two flags of the same `once-any` group are used, if a flag does not have enough arguments, or if _finish_ does not accept the provided number of arguments.

### Tables

A _table_ is a list of entries. Each entry is a list whose first element is a symbol that determines the kind of entry: `once-each`, `once-any`, `multi`, `final`, `help-labels`, `usage-help` or `ps`. These symbols are plain data which `parse-command-line` looks at when it examines the table. They are neither bound to procedures or other values nor exported by the library, i.e. they do not need to be imported and cannot be called.

An entry of the form `(once-each spec ...)`, `(once-any spec ...)`, `(multi spec ...)` or `(final spec ...)` describes flags. A flag specification _spec_ is a list of the form `(flags handler help)`, where

   - _flags_ is a non-empty list of strings, such as `("-v" "--verbose")`. Flags are either short, starting with `-` or `+` and followed by a single character, or long, starting with `--` or `++`.
   - _handler_ is a procedure. It is called with the flag string that was used (e.g. `"--verbose"`) followed by as many arguments as the flag requires. The number of required arguments is defined by _help_. The result of the handler is added to the list that is passed to _finish_, unless it is the void value.
   - _help_ is a list of strings describing the flag. The first element is the description of the flag. It may be a list of strings, in which case the first string is shown beside the flag and the other ones are shown in lines below. The remaining elements are the names of the arguments of the flag, which are shown in the help text.

The kind of the entry determines how often the flag may be used. A flag of a `once-each` entry may appear at most once. At most one of the flags of a `once-any` entry may be used (only once). Flags of `multi` entries may appear any number of times. After a flag of a `final` entry has been processed, the parsing stops and the remaining arguments are passed to _finish_.

Three further kinds of entries add text to the generated help:

   - `(help-labels string ...)` adds the strings as lines to the list of flags in the help text, e.g. to group flags under headings.
   - `(usage-help string ...)` adds the strings as lines below the first line of the help text, i.e. before the list of flags.
   - `(ps string ...)` adds the strings as lines at the end of the help text.

The help text shows entries in the order in which they appear in _table_. The flags of a `once-any` entry are marked with `/`, `|` and `\` to indicate that they are mutually exclusive. The flags `-h`/`--help` and `--` are always added. The program fails already when `parse-command-line` is called, if the table is malformed, e.g. if a handler does not accept the number of arguments that is described by its help list.

### Examples

The following examples pass the arguments as a literal list. A real program typically passes `(cdr (command-line))` instead.

The handlers of the flags return values, which are collected and passed as a list to _finish_ in the order in which the flags appeared. The remaining arguments of _finish_ are the arguments that are not flags.

```scheme
(define table
  `((once-each
      (("-n" "--name")
        ,(lambda (flag name) (cons 'name name))
        ("Name to greet" "name"))
      (("-v" "--verbose")
        ,(lambda (flag) 'verbose)
        ("Verbose output")))))

(parse-command-line "greet" '("-n" "Ada" "-v" "x" "y") table
  (lambda (flags . rest) (list flags rest))
  '("arg" "..."))
  ⇒  (((name . "Ada") verbose) ("x" "y"))

(parse-command-line "greet" '("-vn" "Bob") table
  (lambda (flags . rest) (list flags rest))
  '("arg" "..."))
  ⇒  ((verbose (name . "Bob")) ())
```

Flag processing ends with the first argument which is not a flag, or with `--`. Everything after that is passed on unchanged:

```scheme
(parse-command-line "greet" '("--name" "Ada" "--" "-v") table
  (lambda (flags . rest) (list flags rest))
  '("arg" "..."))
  ⇒  (((name . "Ada")) ("-v"))
```

Procedure _finish_ determines how many arguments which are not flags are accepted. A program that expects exactly two file names can use a procedure with two parameters (besides the list of flag results):

```scheme
(parse-command-line "copy" '("-v" "a.txt" "b.txt") table
  (lambda (flags src dst) (list flags src dst))
  '("src" "dst"))
  ⇒  ((verbose) "a.txt" "b.txt")
```

If the number of arguments does not fit, an error is raised, for instance `copy: the wrong number of arguments were provided` for the call above with just the argument `"a.txt"`. Errors are also raised for repeated `once-each` flags (`copy: the "-v" flag can only be used once`), for unknown flags (`copy: unknown flag`) and for flags with missing arguments (`copy: the "-n" flag needs more arguments`).

Arguments such as `-5` (numbers) and `-` (a single hyphen) are not flags. They are passed on like other arguments:

```scheme
(parse-command-line "greet" '("-5" "-" "x") table
  (lambda (flags . rest) rest)
  '("arg" "..."))
  ⇒  ("-5" "-" "x")
```

The optional arguments _help-proc_ and _unknown-proc_ customize the handling of `--help` and of unknown flags. Here, unknown flags are not an error; they are recorded instead:

```scheme
(parse-command-line "greet" '("--bogus" "-x" "a") table
  (lambda (flags . rest) (list flags rest))
  '("arg" "...")
  display
  (lambda (flag . args) (list 'unknown flag)))
  ⇒  (((unknown "--bogus") (unknown "-x")) ("a"))
```

The following table uses all kinds of entries. The first element of the help list of the `-t` flag is itself a list: its first string is shown beside the flag, and the others are shown in additional lines. The flag `--rest` is a `final` flag.

```scheme
(parse-command-line "tool" '("--help")
  `((usage-help "Converts files from one format to another.")
    (help-labels "Input options:")
    (once-each
      (("-f" "--from")
        ,(lambda (flag fmt) (cons 'from fmt))
        ("Input format" "format")))
    (help-labels "Output options:")
    (once-any
      (("-j" "--json") ,(lambda (flag) 'json) ("Produce JSON"))
      (("-x" "--xml") ,(lambda (flag) 'xml) ("Produce XML"))
      (("-t" "--text")
        ,(lambda (flag) 'text)
        (("Produce plain text" "(the default)"))))
    (multi
      (("-D" "--define")
        ,(lambda (flag key) (cons 'define key))
        ("Define a variable" "key")))
    (final
      (("--rest")
        ,(lambda (flag) 'rest)
        ("Treat the remaining arguments as files")))
    (ps "See the manual for details."))
  (lambda (flags . files) (list flags files))
  '("file" "...")
  (lambda (text) (display text)))
```

This program displays the following help text:

```
tool [ <option> ... ] <file> <...>
 Converts files from one format to another.
 where <option> is one of
 Input options:
  -f, --from <format> : Input format
 Output options:
/ -j, --json : Produce JSON
| -x, --xml : Produce XML
\ -t, --text : Produce plain text
     (the default)
  -D, --define <key> : Define a variable
  --rest : Treat the remaining arguments as files
  -h, --help : Show this help
  -- : Do not treat any remaining argument as a flag (at this level)
 Multiple single-letter flags can be combined after one `-'; for
  example: `-h-' is the same as `-h --'
 See the manual for details.
```

After a `final` flag, the remaining arguments are not interpreted as flags, even if they look like flags:

```scheme
(parse-command-line "tool" '("--rest" "-j" "a")
  `((once-any (("-j") ,(lambda (flag) 'json) ("Produce JSON")))
    (final (("--rest") ,(lambda (flag) 'rest) ("Treat the rest as files"))))
  (lambda (flags . files) (list flags files))
  '("file" "..."))
  ⇒  ((rest) ("-j" "a"))
```

### Notes

   - All arguments of `parse-command-line` are positional: _name_, _argv_, _table_, _finish_ and _arg-help_ are always required.
   - Handlers receive the arguments of a flag as strings. No conversion to numbers or other types is performed; a handler has to do this itself, e.g. with `string->number`.
   - Since a table is just a list, it can be constructed programmatically, e.g. with quasiquotation as in the examples above, which is needed to embed procedures into the table.
   - When a script is run with `LispKitRepl`, the arguments of the script follow the name of the script, e.g. `LispKitRepl -d LispKit --- compile.scm -v main.scm`. The separator `---` makes sure that the options of the REPL are not mixed up with the arguments of the script. Procedure `command-line` returns the name of the script followed by its arguments, which is why the examples pass `(cdr (command-line))` to `parse-command-line`.

## Fold-based command-line parsing

In contrast to `parse-command-line`, which is driven by a table, `args-fold` is a _fold_ over the list of command-line arguments: the arguments are processed from left to right, and the procedures that handle the options thread one or more _seed_ values through the iteration. This makes it easy to collect the result in any data structure. The library follows the conventions of POSIX and GNU programs: it supports clusters of short options such as `-vo file` and long options with arguments of the form `--name=arg`.

The library exports the procedures `option`, `option?`, `option-names`, `option-required-arg?`, `option-optional-arg?`, `option-processor` and `args-fold`. These procedures are also exported by `(srfi 37)`, i.e. a program may import either `(srfi 37)` or `(lispkit cmdline)` to use them. Here is an example:

```scheme
(import (lispkit base)
        (lispkit cmdline))
(define opts
  (list (option '(#\v "verbose") #f #f
                (lambda (o n a seed) (cons 'verbose seed)))
        (option '(#\o "output") #t #f
                (lambda (o n a seed) (cons (list 'output a) seed)))))
(reverse
  (args-fold '("-v" "file1" "--output=out.txt")
             opts
             (lambda (o n a seed) (error "unknown option" n))
             (lambda (operand seed) (cons operand seed))
             '()))
  ⇒  (verbose "file1" (output "out.txt"))
```

### Options

**(option _names required-arg? optional-arg? processor_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates an option. _names_ is a list of the names of the option. A character is a short name, which is used on the command line with a single hyphen, as in `-v`. A string is a long name, which is used with two hyphens, as in `--verbose`. An option can have both short and long names.

If _required-arg?_ is true, the option requires an argument. If _optional-arg?_ is true, the option may have an argument. At most one of the two should be true.

_processor_ is a procedure which is called whenever the option is found on the command line. It receives four kinds of arguments: the option, the name that was used (a character or a string), the argument of the option (a string, or `#f` if there is none) and the current seed values, which are passed as separate arguments. The processor returns the new seed values, as multiple values if there is more than one seed.

**(option? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is an option created by `option`; otherwise `#f` is returned.

**(option-names _opt_)** <span style="float:right;text-align:rigth;">[procedure]</span>  
**(option-required-arg? _opt_)**  
**(option-optional-arg? _opt_)**  
**(option-processor _opt_)**  

Return the components of option _opt_ that were provided to `option`.

```scheme
(let ((o (option '(#\v "verbose") #f #f (lambda (o n a seed) seed))))
  (list (option? o) (option-names o) (option-required-arg? o) (option-optional-arg? o)))
  ⇒  (#t (#\v "verbose") #f #f)
```

### Processing arguments

**(args-fold _args options unrecognized-proc operand-proc seed ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Processes the list of strings _args_ from left to right according to the list of _options_, and returns the final seed values (as multiple values, if there is more than one seed). The initial seed values are _seed ..._.

   - For every option that is found, the processor of the option is called with the option, the name, the argument, and the current seed values.
   - For every option that is not in _options_, procedure _unrecognized-proc_ is called in the same way as an option processor. The first argument is a newly created option whose list of names consists of just the name that was used.
   - For every argument that is not an option (an _operand_), procedure _operand-proc_ is called with the operand and the current seed values.

All three procedures return the new seed values. Arguments are interpreted as follows:

   - The argument `--` ends the processing of options. It is neither an option nor an operand. All arguments that follow are operands, even if they start with a hyphen.
   - An argument that starts with `--` is a long option. The option name is the text after the hyphens, up to the first `=`. If there is an `=`, the text after it is the argument of the option, as in `--output=out.txt`. Otherwise, if the option requires an argument, the next argument is used as its argument, as in `--output out.txt`. An optional argument is only recognized in the form `--name=arg`.
   - An argument that starts with a single `-` and has more characters is a cluster of short options: every character is the name of an option, i.e. `-vx` is the same as `-v -x`. If an option accepts an argument and further characters follow in the same argument, the remaining characters are the argument of the option: `-oout.txt` is the same as `-o out.txt`. If no characters follow and the option requires an argument, the next argument is used.
   - All other arguments, including a single hyphen `-`, are operands. Operands and options may appear in any order.
   - If an option requires an argument but there are no more arguments, its processor is called with `#f` as the argument. The processor may report this as an error.

The following examples use a function `parse`, which collects options and operands in a list:

```scheme
(define opts
  (list (option '(#\v "verbose") #f #f
                (lambda (o n a seed) (cons 'verbose seed)))
        (option '(#\o "output") #t #f
                (lambda (o n a seed) (cons (list 'output a) seed)))
        (option '(#\c "color") #f #t
                (lambda (o n a seed) (cons (list 'color a) seed)))))

(define (parse args)
  (reverse
    (args-fold args opts
               (lambda (o n a seed) (error "unknown option" n))
               (lambda (operand seed) (cons operand seed))
               '())))

(parse '("-vo" "out.txt" "file1"))     ⇒  (verbose (output "out.txt") "file1")
(parse '("-oout.txt" "file1"))         ⇒  ((output "out.txt") "file1")
(parse '("--output" "out.txt" "a"))    ⇒  ((output "out.txt") "a")
(parse '("a" "-v" "b"))                ⇒  ("a" verbose "b")
(parse '("-v" "--" "-o" "x"))          ⇒  (verbose "-o" "x")
(parse '("-" "file1"))                 ⇒  ("-" "file1")
(parse '("--color=red"))               ⇒  ((color "red"))
(parse '("--color" "red"))             ⇒  ((color #f) "red")
(parse '("-o"))                        ⇒  ((output #f))
```

If there are several seed values, all processors and procedures take and return as many values. The following program counts the occurrences of the flag `-v` and collects the operands:

```scheme
(call-with-values
  (lambda ()
    (args-fold '("-v" "-v" "a" "b")
               (list (option '(#\v) #f #f
                             (lambda (o n a count files) (values (+ count 1) files))))
               (lambda (o n a count files) (values count files))
               (lambda (operand count files) (values count (cons operand files)))
               0 '()))
  (lambda (count files) (list count (reverse files))))
  ⇒  (2 ("a" "b"))
```
