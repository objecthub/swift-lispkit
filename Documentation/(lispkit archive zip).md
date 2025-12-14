# LispKit Archive Zip

Library `(lispkit archive zip)` provides an API for creating and managing zip archives. Zip archives are either persisted on the file system, or they are created in-memory. Zip archives can be opened either in read-only or read-write mode. They allow either files or in-memory data (in the form of bytevectors) to be included. Such _zip entries_ are either a file, a directory, or a symbolic link. In an archive, files are stored in either compressed or uncompressed form.


## Constructors

**(make-zip-archive)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(make-zip-archive _bvec_)**  
**(make-zip-archive _bvec mutable?_)**  

Procedure `make-zip-archive` creates an in-memory zip archive. If bytevector _bvec_ is provided, the zip archive is created from the given binary data, otherwise, a new empty zip archive is returned. For a zip archive created from a bytevector, parameter _mutable?_ determines if it is a read-only or read-write zip archive. In the latter case, _mutable?_ has to be set to `#t`, the default is `#f`.

**(create-zip-archive _path_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Creates a new empty read-write zip archive at the given file path.

**(open-zip-archive _path_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(open-zip-archive _path mutable?_)**  

Opens a zip archive at the given file path. By default, the zip archive is opened in read-only mode, unless _mutable?_ is set to `#t`.


## Properties of archives

**zip-archive-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `zip-archive` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all zip archive objects.

**(zip-archive? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ refers to a zip archive, otherwise `#f` is being returned.

**(zip-archive-mutable? _archive_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if the given zip archive is mutable, i.e. opened in read-write mode, `#f` otherwise.

**(zip-archive-path _archive_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Procedure `zip-archive-path` returns the file path at which _archive_ is being persisted. If _archive_ is a in-memory zip archive, then `#f` is returned.

**(zip-archive-bytevector _archive_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Procedure `zip-archive-bytevector` returns _archive_ as a bytevector. This bytevector can be written to disk or used to create a in-memory copy of the zip archive.


## Introspecting entries

Entries in zip archives are referred to via their relative file path in the archive. All procedures that provide information about a zip archive entry therefore expect two arguments: the zip archive and a file path.

**(zip-entry-count _archive_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the number of entries in _archive_.

**(zip-entries _archive_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a list of file paths for all entries of zip archive _archive_.

**(zip-entry-exists? _archive path_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _archive_ contains an entry with the given file path.

**(zip-entry-compressed? _archive path_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _archive_ contains an entry for the given file path and this entry is stored in compressed form. If _path_ does not refer to a valid entry in _archive_, the procedure `zip-entry-compressed?` fails with an error.

**(zip-entry-file? _archive path_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _archive_ contains an entry for the given file path and this entry is a file. If _path_ does not refer to a valid entry in _archive_, the procedure `zip-entry-file?` fails with an error.

**(zip-entry-directory? _archive path_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _archive_ contains an entry for the given file path and this entry is a directory. If _path_ does not refer to a valid entry in _archive_, the procedure `zip-entry-directory?` fails with an error.

**(zip-entry-symlink? _archive path_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _archive_ contains an entry for the given file path and this entry is a symbolic link. If _path_ does not refer to a valid entry in _archive_, the procedure `zip-entry-directory?` fails with an error.

**(zip-entry-compressed-size _archive path_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the size of the compressed file for the entry at the given path in bytes. If _path_ does not refer to a valid entry in _archive_, the procedure `zip-entry-compressed-size` fails with an error.

**(zip-entry-uncompressed-size _archive path_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the size of the uncompressed file for the entry at the given path in bytes. If _path_ does not refer to a valid entry in _archive_, the procedure `zip-entry-uncompressed-size` fails with an error.


## Adding and removing entries

**(add-zip-entry _archive path base_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(add-zip-entry _archive path base compressed?_)**  

Adds the file, directory, or symbolic link at _path_ relative to path _base_ to the given zip _archive_. The corresponding entry in _archive_ is identified via _path_. The file is stored in uncompressed form if _compressed?_ is set to `#f`. The default for _compressed?_ is `#t`.

**(write-zip-entry _archive path bvec_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
**(write-zip-entry _archive path bvec compressed?_)**  
**(write-zip-entry _archive path bvec compressed? time_)**  

Adds a new file entry to _archive_ at _path_ based on the content of bytevector _bvec_. The entry is stored in uncompressed form if _compressed?_ is set to `#f`. The default for _compressed?_ is `#t`. _time_ is a `date-time` object as defined by library `(lispkit date-time)` which defines the modification time of the new entry.

**(delete-zip-entry _archive path_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Deletes the entry at _path_ from _archive_. Procedure `delete-zip-entry` fails if the entry does not exist or if the archive is opened in read-only mode.

## Extracting entries

**(extract-zip-entry _archive path base_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Extracts the entry at _path_ in _archive_ and stores it on the file system at _path_ relative to path _base_.

**(read-zip-entry _archive path_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the file entry at _path_ in _archive_ in form of a bytevector. Procedure `read-zip-entry` fails if the entry does not exist or if the entry is not a file entry.
