# LispKit JSON Schema

Library `(lispkit json schema)` implements _JSON Schema_ as defined by the [2020-12 Internet Draft specification](https://datatracker.ietf.org/doc/draft-bhutton-json-schema/) for validating JSON data.   

## Overview

A JSON _schema_ is represented by the `json-schema` type. It is possible to load schema objects either from a file, decode them from a string, or from a bytevector. Schema objects have an identity, they are pre-processed and pre-validated. The identity of a JSON schema is defined by a URI in string form. Such URIs are either _absolute_ or _relative_ and it is either a _base URI_, i.e. it is referring to a top-level schema, or it is a _non-base URI_ and thus refers to a schema nested within another schema via a URI fragment.

The semantics of a schema is defined by its _dialect_. A schema dialect is again identified by a URI in string form. If schema does not define a dialect identifier, a default dialect is assumed (which is `json-draft2020-default` right now for top-level schema and the dialect of the enclosing schema for nested schema). Schema dialects are represented by a pair consisting of a meta schema URI and a list of enabled vocabularies. Right now, only meta schema `json-draft2020` is supported with dialects `json-draft2020-default` (for enabling the default set of vocabularies) and `json-draft2020-all` for enabling all standard vocabularies.

_Schema validation_ is the process of determining whether a given JSON value matches a given schema. The whole schema validation process is controlled by a `json-schema-registry` object. Schema registries define:

  - A set of supported dialects with their corresponding URI identities,
  - A default dialect (for schema resources that do not define a dialect themselves),
  - A set of known/loaded schema with their corresponding identities, and
  - Schema resources, providing means for discovering and loading schema which are not loaded yet.

Most of the schema registry functionality is about configuring registry objects by registering supported dialects, inserting available schema and setting up schema resources for automatically discovering new schema. To simplify the API, library `(lispkit json schema)` provides a parameter object `current-schema-registry` referring to the current default schema registry. For most use cases, one can simply work with this pre-initialized default by registering schema and schema resources and then using it implicitly in validation calls.

Schema validation is performed via two procedures: `json-valid?` and `json-validate` both taking four arguments: the JSON value to validate, the schema against which validation takes place, a default dialect, and a schema registry coordinating the validation process. While `json-valid?` simply returns a boolean result, `json-validate` returns a `json-validation-result` object which encapsulates the output of the validation process.

`json-validation-result` objects include information about:

  - Whether the validation process succeeded
  - Errors that were encountered during validation (if there are errors, then the validation process failed)
  - Tag annotations denoting what values were _deprecated_, _read-only_, or _write-only_.
  - Format annotations denoting violations of format constraints by string arributes (when the `format-annotation` vocabulary is enabled, then these violations are automatically turned into errors).
  - Default member values for missing object members.

## Workflow

The following code snippet showcases the typical workflow for performing schema validation with library `(lispkit json schema)`. First, a new schema registry is created. Then, a new schema object is loaded from a file and registered with the registry. Next, a JSON value that does not conform to the schema is defined and validated. The validation errors are printed. Finally, a JSON value which conforms to the schema is defined and defaults are printed out.

```scheme
(import (lispkit base) (lispkit json) (lispkit json schema))
; Make a new JSON schema registry with default dialect `json-draft2020-default`
(define registry (make-schema-registry json-draft2020-default))
; Load a JSON schema from a file
(define person-schema
  (load-json-schema (asset-file-path "person" "" "JSON/Schema/custom")))
; Register the schema with the registry
(schema-registry-register! person-schema registry)

; Define an invalid person
(define person1 (json '(
  (name . "John Doe")
  (email . #("john@doe.com" "john.doe@gmail.com")))))
; Validate `person1` and show the validation errors
(let ((res (json-validate person1 person-schema #t registry)))
  (display* "person1 valid: " (validation-result-valid? res) "\n")
  (for-each
    (lambda (err) (display* "  - " (caddr err) " at " (cdar err) "\n"))
    (validation-result-errors res)))

; Define a valid person
(define person2 (json '(
  (name . "John Doe")
  (birthday . "1983-03-19")
  (address . "12 Main Street, 17445 Noname"))))
(let ((res (json-validate person2 person-schema #t registry)))
  (display* "person2 valid: " (validation-result-valid? res) "\n")
  (for-each
    (lambda (x)
      (if (cadr x)
          (display* "  - " (car x) " exists; default: "
                    (json->string (car (caddr x))) "\n")
          (display* "  - " (car x) " does not exist; default: "
                    (json->string (car (caddr x))) "\n")))
    (validation-result-defaults res)))
```

## Using the default registry

Library `(lispkit json schema)` defines a default schema registry and initializes parameter object `current-schema-registry` with it. The default registry is configured such that

  - The schema definitions in the asset directory `JSON/Schema/2020-12` are available via the base schema identifier `https://json-schema.org/draft/2020-12`.
  - The schema definitions in the asset directory `JSON/Schema/custom` are available via the base schema identifier `https://lisppad.app/schema`.

With such a setup, it is easy to make new schema available by dropping their definition files into the asset directory `JSON/Schema/custom`.

If a different object with the same setup is needed, an equivalent schema registry can be created via `(make-schema-registry json-draft2020-default #t #t)`.

## JSON schema dialects

Meta schema and vocabularies are specified via URIs. URIs are represented as strings. A schema dialect specifier is a pair whose head refers to the URI of the meta schema and tail refers to a list of URIs for enabled vocabularies. Two dialect specifiers are predefined via the constants `json-draft2020-default` and `json-draft2020-all`.

**json-draft2020** <span style="float:right;text-align:rigth;">[constant]</span>  

URI identifying the meta schema for the [2020-12 Internet Draft specification](https://www.ietf.org/archive/id/draft-bhutton-json-schema-01.html) of JSON schema.

**json-draft2020-core** <span style="float:right;text-align:rigth;">[constant]</span>  

URI identifying the core vocabulary of the [2020-12 Internet Draft specification](https://www.ietf.org/archive/id/draft-bhutton-json-schema-01.html) of JSON schema.

**json-draft2020-applicator** <span style="float:right;text-align:rigth;">[constant]</span>  

URI identifying the applicator vocabulary of the [2020-12 Internet Draft specification](https://www.ietf.org/archive/id/draft-bhutton-json-schema-01.html) of JSON schema.

**json-draft2020-unevaluated** <span style="float:right;text-align:rigth;">[constant]</span>  

URI identifying the unevaluated vocabulary of the [2020-12 Internet Draft specification](https://www.ietf.org/archive/id/draft-bhutton-json-schema-01.html) of JSON schema.

**json-draft2020-validation** <span style="float:right;text-align:rigth;">[constant]</span>  

URI identifying the validation vocabulary of the [2020-12 Internet Draft specification](https://www.ietf.org/archive/id/draft-bhutton-json-schema-01.html) of JSON schema.

**json-draft2020-meta** <span style="float:right;text-align:rigth;">[constant]</span>  

URI identifying the meta vocabulary of the [2020-12 Internet Draft specification](https://www.ietf.org/archive/id/draft-bhutton-json-schema-01.html) of JSON schema.

**json-draft2020-format** <span style="float:right;text-align:rigth;">[constant]</span>  

URI identifying the format vocabulary of the [2020-12 Internet Draft specification](https://www.ietf.org/archive/id/draft-bhutton-json-schema-01.html) of JSON schema.

**json-draft2020-content** <span style="float:right;text-align:rigth;">[constant]</span>  

URI identifying the content vocabulary of the [2020-12 Internet Draft specification](https://www.ietf.org/archive/id/draft-bhutton-json-schema-01.html) of JSON schema.

**json-draft2020-formatval** <span style="float:right;text-align:rigth;">[constant]</span>  

URI identifying the format assertion vocabulary of the [2020-12 Internet Draft specification](https://www.ietf.org/archive/id/draft-bhutton-json-schema-01.html) of JSON schema.

**json-draft2020-deprecated** <span style="float:right;text-align:rigth;">[constant]</span>  

URI identifying the deprecated vocabulary of the [2020-12 Internet Draft specification](https://www.ietf.org/archive/id/draft-bhutton-json-schema-01.html) of JSON schema.

**json-draft2020-default** <span style="float:right;text-align:rigth;">[constant]</span>  

Specifier for the schema dialect as defined by the [2020-12 Internet Draft specification](https://www.ietf.org/archive/id/draft-bhutton-json-schema-01.html) of JSON schema, with vocabularies that are enabled by default.

**json-draft2020-all** <span style="float:right;text-align:rigth;">[constant]</span>  

Specifier for the schema dialect as defined by the [2020-12 Internet Draft specification](https://www.ietf.org/archive/id/draft-bhutton-json-schema-01.html) of JSON schema, with all standard vocabularies enabled.

**(json-schema-dialect? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a JSON schema dialect specifier; `#f` otherwise.


## JSON schema registries

**schema-registry-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `json` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all JSON values.

**current-schema-registry** <span style="float:right;text-align:rigth;">[parameter object]</span>  

This parameter object represents the current default schema registry. Procedures requiring a registry typically make the registry argument optional. If it is not provided, the result of `(current-schema-registry)` is used instead. The value of `current-schema-registry` can be overridden with `parameterize`.

**(schema-registry? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a schema registry; `#f` otherwise.

**(make-schema-registry)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-schema-registry _dialect_)**  
**(make-schema-registry _dialect meta?_)**  
**(make-schema-registry _dialect meta? custom_)**  

Returns a new schema registry with _dialect_ being the default schema dialect. If _meta?_ is provided and set to true, a data source will be configured such that the meta schema and vocabularies for the Draft 2020 standard can be referenced. If argument _custom_ is provided, it defines a base URI for schema definitions placed into the asset directory `JSON/Schema/custom`. If _custom_ is set to `#t`, the base URI `https://lisppad.app/schema/` will be used as a default.

**(schema-registry-copy)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(schema-registry-copy _registry_)**  

Returns a copy of _registry_. If argument _registry_ is not provided, a copy of the current value of parameter object `current-schema-registry` is returned.

**(schema-registry-dialects)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(schema-registry-dialects _registry_)**  

Returns a list of URIs identifying the dialects registered for _registry_. If argument _registry_ is not provided, the current value of parameter object `current-schema-registry` is used as a default.

**(schema-registry-schemas)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(schema-registry-schemas _registry_)**  

Returns a list of URIs identifying the schema registered with _registry_. If argument _registry_ is not provided, the current value of parameter object `current-schema-registry` is used as a default.

**(schema-registry-add-source! _path base_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(schema-registry-add-source! _path base registry_)**  

Adds a new data source to _registry_. A data source is defined by a directory at _path_ and a _base_ URI for schema available in the directory. If argument _registry_ is not provided, the current value of parameter object `current-schema-registry` is used as a default.

**(schema-registry-register! _expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(schema-registry-register! _expr registry_)**  

Registers either a dialect or a schema with _registry_. If _expr_ is a schema object, the schema is being registered under its identifier. If the schema does not define an identifier, an error is signaled. Alternatively, it is possible to specify a pair consisting of a URI as schema identifier and a schema object. If _expr_ specifies a dialect, then this dialect gets registered with _registry_. If argument _registry_ is not provided, the current value of parameter object `current-schema-registry` is used as a default.

**(schema-registry-ref _ident_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(schema-registry-ref _ident registry_)**  

Returns a schema object from _registry_ for the given URI specified via argument _ident_. First, `schema-registry-ref` will check if a schema object is available already for _ident_ in _registry_. If it is, the object gets returned. If it is not, then `schema-registry-ref` attempts to load the schema from the data sources of _registry_. If no matching schema is found, `#f` is returned. If argument _registry_ is not provided, the current value of parameter object `current-schema-registry` is used as a default.

## JSON schema

**json-schema-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `json-schema` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all JSON schema values.

**(json-schema? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a JSON schema; `#f` otherwise.

**(json-schema-boolean? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a boolean JSON schema, i.e. a schema that is either `#t` or `#f`; `#f` otherwise.

**(json-schema _expr_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new JSON schema object dependent on argument _expr_:

  - If _expr_ is `#t` or `#f`, a new schema object for a boolean schema is returned.
  - If _expr_ is a JSON object then it is interpreted as a representation of a schema and if the coercion to a schema suceeds, a corresponding JSON schema object is returned. Otherwise, an error is signaled.
  - If _expr_ is a JSON schema object, a new copy of this object is returned.
  - In all other cases, `(json-schema expr)` is implemented via `(json-schema (json expr))`, i.e. a new schema object is based on a coercion of _expr_ into JSON.

**(load-json-schema _path_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(load-json-schema _path uri_)**  

Loads a JSON schema from a file at _path_ and returns a new JSON schema object. _uri_ is an optional identifier for the loaded schema. If it is provided, it is used as a default for the loaded schema in case it does not define its own identifier.

**(json-schema-id _schema_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the identifier of _schema_ as a URI. If _schema_ does not define its own identifier, then `#f` is returned.

**(json-schema-meta _schema_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the identifier of the meta schema of _schema_ as a URI. If _schema_ does not define a meta schema, then `#f` is returned.

**(json-schema-title _schema_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the title of _schema_ as a string. If _schema_ does not define a title, then `#f` is returned.

**(json-schema-description _schema_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the description of _schema_ as a string. If _schema_ does not define a description, then `#f` is returned.

**(json-schema-nested _schema_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of local schema nested within _schema_. Each element of the list is a pair consisting of a JSON location and a JSON schema object. The location refers to the place within _schema_ where the nested schema was found.

**(json-schema-ref _schema ref_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a schema nested within _schema_ at the location at which JSON reference _ref_ is referring to. If that reference does not point at a location with a valid schema, then `#f` is returned.

**(json-schema-resolve _schema fragment_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a schema nested within _schema_ with the local schema identifier _fragment_ (a string). This procedure can be used to do a nested schema lookup by name.

**(json-schema-\>json _schema_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a JSON object representing _schema_.

## JSON validation

**(json-valid? _json schema_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(json-valid? _json schema dialect_)**  
**(json-valid? _json schema registry_)**  
**(json-valid? _json schema dialect registry_)**  

Returns `#t` if value _json_ conforms to _schema_. _dialect_ specifies, if provided, a default dialect overriding the default from _registry_ specifically for _schema_. If argument _registry_ is not provided, the current value of parameter object `current-schema-registry` is used instead.

**(json-validate _json schema_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(json-validate _json schema dialect_)**  
**(json-validate _json schema registry_)**  
**(json-validate _json schema dialect registry_)**  

Returns an object encapsulating validation results from the process of validating _json_ against _schema_. _dialect_ specifies, if provided, a default dialect overriding the default from _registry_ specifically for _schema_. If argument _registry_ is not provided, the current value of parameter object `current-schema-registry` is used instead.

## JSON validation results

**validation-result-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `json-validation-result` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all JSON validation result objects.

**(validation-result? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a JSON validation result object; `#f` otherwise.

**(validation-result-valid? _res_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the validation results object _res_ does not contain any validation errors; i.e. the validation succeeded. Returns `#f` otherwise.

**(validation-result-counts _res_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns three results: the number of erros in validation results object _res_, the number of tags, and the number of format constraints.

**(validation-result-errors _res_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of errors from the validation results object _res_. Each error is represented by the following data structure: `((value . value-location) (schema-rule, rule-location) message)` where _value_ is the affected JSON value at a location at which reference _value-location_ is referring to within the verified value. _schema-rule_ is a part of the schema causing the error, again with reference _rule-location_ referring to it. _message_ is an error message.

Here is some sample code displaying errors:

```scheme
(for-each
  (lambda (err)
    (let ((val (caar err))
          (val-loc (cdar err))
          (schema (caadr err))
          (schema-loc (cdadr err))
          (message (caddr err)))
      (display* "  - " message " at " val-loc "\n")))
  (validation-result-errors res))
```

**(validation-result-tags _res_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of tagged JSON values from the validation results object _res_. Each tagged value is represented by the following data structure: `((value . value-location) tag-location taglist)` where _value_ is the tagged JSON value at a location at which reference _value-location_ is referring to within the verified value. _rule-location_ refers to the tagging rule in the schema. _taglist_ is a list containing at least one of the following symbols: `deprecated`, `read-only`, and `write-only`.

**(validation-result-formats _res_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of format constraints from the validation results object _res_. Each format constraint is represented by the following data structure: `((value . value-location) format-location (message . valid?))` where _value_ is the tagged JSON value at a location at which reference _value-location_ is referring to within the verified value. _format-location_ refers to the format constraint in the schema. _message_ is a string describing the format constraint. _valid?_ is `()` if the constraint has not been checked, it is `#t` if the constraint has been checked and is valid, and it is `#f` if the constraint has been checked and is invalid.

**(validation-result-defaults _res_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of computed defaults from the validation results object _res_. Each default is represented by the following data structure: `(value-location exists? defaults)` where _value-location_ is a JSON reference referring to a member with a default. _exists?_ is a boolean value; it is `#t` if that member has a value already, or `#f` if it does not have a member value already. _defaults_ is a list of computed default JSON values for this member.
