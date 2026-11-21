# LispKit System Util

Library `(lispkit system util)` provides system-level utilities. So far, it offers a standardized way of accessing and storing credentials such as passwords, API keys, or client ids. Programs that need such a secret should not hard-code it. Instead, they can use this library to look it up in a well-defined order from the program itself, from the process environment, and from the system keychain, and to store a new value in the keychain for later use.


## Managing credentials

**(manage-credential _default_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(manage-credential _default envvar_)**  
**(manage-credential _default envvar kc_)**  
**(manage-credential _default envvar kc key_)**  
**(manage-credential _default envvar kc key label_)**  
**(manage-credential _default envvar kc key label comment_)**  

Returns two values: an _accessor_ for a credential and a _setter_ for the same credential. Both are procedures. The accessor is called without arguments and returns the credential, typically a string, or `#f` if there is none. It takes the credential from the first of the following sources that has a value:

  1. _default_, if it is not `#f`. This overrides sources 2 and 3.
  2. The value of the environment variable named _envvar_ (a string). This overrides source 3. The source is skipped if _envvar_ is `#f` or if the variable is not set.
  3. The secret stored in a keychain item. The item is described by _kc_ and _key_ (see below) and is read via `keychain-ref-string`. The source is skipped if there is no keychain or if the keychain does not contain an item for _key_.

If none of the sources provides a value, the accessor returns `#f`.

The keychain item is identified by _kc_ and _key_. _key_ is a string identifying the item within the keychain service. _kc_ describes the keychain client, as provided by library `(lispkit system keychain)`. It is one of the following:

  - `#f`: There is no keychain. Source 3 is not available and nothing is stored in a keychain.
  - A _keychain_ object, as returned by `make-keychain`.
  - A string: the name of the keychain _service_.
  - A list `(service group acc sync)`: _service_ is a string or a keychain object, and _group_, _acc_, and _sync_ are the arguments of `make-keychain`, i.e. the shared access group, the access policies, and whether items are synchronized across iCloud. Trailing elements can be omitted. `#f` selects the default for _group_ and _acc_, and items are not synchronized by default. The access policies and the synchronization behavior of the keychain client also apply to the items stored via the setter (see below). If _service_ is a keychain object, it is used as is and _group_, _acc_, and _sync_ are ignored.

A keychain is only used if _key_ is a string and a service has been specified. Unlike with `make-keychain`, `#f` for _service_ does not select the default service of the application; it means that there is no keychain. In this case, the credential is neither read from nor stored in the keychain.

_label_ is a string that is attached to the keychain item when it is stored by the setter. The default is `"net.objecthub.LispPad.credential"`. _comment_ is a string stored as the comment of the keychain item; the default is `#f`, which means that there is no comment.

The setter is called as `(setter value)` and makes _value_ the new value of the credential:

**(_setter_ _value_)**  
**(_setter_ _value label_)**  
**(_setter_ _value label comment_)**  
**(_setter_ _value label comment acc_)**  
**(_setter_ _value label comment acc sync_)**  

If there is a keychain, _value_ is stored in the keychain item for _key_ via `keychain-set-string!`, creating the item or overwriting an existing one. This is also the reason why _value_ needs to be a string in this case. Independent of that, _value_ is cached by the setter, and from now on, the accessor returns _value_ without consulting the environment variable or the keychain. Even if there is no keychain, the setter caches _value_. Such a value only lives as long as the accessor and setter.

The optional arguments _label_, _comment_, _acc_, and _sync_ of the setter apply to this call only:

  - _label_ and _comment_ override the values given to `manage-credential`. `#f` means "not specified", in which case the values given to `manage-credential` are used.
  - _acc_ specifies the access policies of the stored item. They are described in the documentation of `keychain-set!` in library `(lispkit system keychain)`. If _acc_ is omitted or `#f`, the access policies of the keychain client apply.
  - _sync_ determines whether the stored item is synchronized across iCloud (`#t`) or stored locally only (`#f`). If _sync_ is omitted or the empty list, the keychain client decides.

The values of the keychain are not cached by the accessor. If neither _default_ nor the environment variable provide a value, every call of the accessor reads the keychain. Once the setter has been called, access is fast and does not touch the keychain anymore.

`manage-credential` raises an error if it is called without any arguments or with more than six arguments.

### Examples

A credential that is only available via an environment variable:

```scheme
(define-values (api-key set-api-key!)
  (manage-credential #f "ANTHROPIC_API_KEY"))
(api-key)        ; ⇒ the value of ANTHROPIC_API_KEY, or #f if it is not set
```

The sources are tried in order. A value provided as _default_ wins over the environment, and a value that was set explicitly wins over both:

```scheme
(define-values (get set) (manage-credential #f "HOME"))
(get)            ⇒ "/Users/jane"
(set "other")
(get)            ⇒ "other"

(define-values (get set) (manage-credential "preset" "HOME"))
(get)            ⇒ "preset"

(define-values (get set) (manage-credential #f "UNKNOWN_VARIABLE"))
(get)            ⇒ #f
```

A credential that is taken from the environment or, if the environment variable is not set, from the keychain. The first line sets up the keychain service `"net.objecthub.example"` and the key `"anthropic-api-key"`. The first time a user is asked for the key, the setter stores it in the keychain for later runs:

```scheme
(define-values (get-key set-key!)
  (manage-credential #f "ANTHROPIC_API_KEY" "net.objecthub.example" "anthropic-api-key"))
(unless (get-key)
  (display "API key: ")
  (set-key! (read-line)))
(get-key)        ; ⇒ the key; from now on also available in the keychain
```

The keychain client can be configured via a list. Here, items are only accessible while the device is unlocked and are synchronized via iCloud. The label and comment help to recognize the item in the keychain:

```scheme
(define-values (get-token set-token!)
  (manage-credential #f #f
                     '("net.objecthub.example" #f when-unlocked #t)
                     "access-token"
                     "Example access token"
                     "Created by the example program"))
(set-token! "abc123")                ; stored in the keychain and synchronized
(set-token! "abc123" #f #f #f #f)    ; stored in the keychain, but only locally
```
