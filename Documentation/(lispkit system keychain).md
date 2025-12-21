# LispKit System Keychain

Library `(lispkit system keychain)` provides an API for accessing the macOS and iOS keychain. The _keychain_ allows for centrally storing small bits of confidential user data in an encrypted database. Such _keychain items_ consist of encrypted binary data as well as unencrypted meta-data. Once stored in the keychain, keychain items, by default, are only readable by the application hosting the LispKit interpreter app. While the system keychain supports different types of keychain items, library `(lispkit system keychain)` only handles _generic password_ keychain items.

Access to items in a keychain is provided by `keychain` client objects. A `keychain` client enables accessing all _keychain items_ that belong to a given _service_. The _service_ is typically a string identifier for the program storing and accessing secrets in the keychain. By default, this is the main bundle identifier of the macOS or iOS application hosting the LispKit interpreter. But the API supports specifying any arbitrary service. To share keychain items between applications, it is possible to specify a shared _access group_ identifier which can be used across applications.

`keychain` clients also specify the security level of their keychain storage. Such _access policies_ are defined via symbolic constants which can be parameterized with an _authentication prompt_ and an _authentication policy_, which again is a symbolic identifier. By default, access policy `when-unlocked` is used. It is one of the most restrictive options, providing good data protection since keychain items can only be accessed while the device is unlocked. More details on the access policy model can be found in the article ["Restricting keychain item accessibility"](https://developer.apple.com/documentation/security/restricting-keychain-item-accessibility).

Finally, each keychain object defines whether the keychain items that are accessible via this object are synchronized via iCloud and thus accessible on other systems and devices.

Keychain items accessible via a keychain client object are identified by a _key_, which is an arbitrary string. The API provides means to read and write keychain items, both data and meta-data (so called _attributes_). There is functionality for deleting keychain items as well as listing all the keys of the items belonging to the service of the keychain object. Via procedure `available-keychain-services`, all available services can be listed. Alternatively, all available service/key combinations can be returned via procedure `available-keychain-keys`.


## Keychains

**keychain-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `keychain` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all keychain objects.

**(keychain? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a keychain object; `#f` otherwise.

**(make-keychain)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-keychain _service_)**  
**(make-keychain _service group_)**  
**(make-keychain _service group acc_)**  
**(make-keychain _service group acc sync_)**  

Returns a new keychain client for the given _service_ and access _group_. _service_ is a string identifier for the program storing and accessing secrets in the keychain. By default, this is the main bundle identifier of the macOS or iOS application. A shared access _group_ identifier (a string) can be used to share keychain items across programs. `#f` specifies the default for _service_ (the application) and _group_ (none).

_acc_ specifies access policies. The following access policy specifiers are supported:

  - `symbol`: Symbols specify the [item accessibility](https://developer.apple.com/documentation/security/restricting-keychain-item-accessibility). Supported are `when-unlocked`, `after-first-unlockl`, `always`, `when-unlocked-this-device-only`, `after-first-unlock-this-device-only`, `always-this-device-only`.
  - `(prompt)`: An authentication _prompt_ (a string) is provided, which is shown to the user. Default item accessibility is used.
  - `(prompt access)`: An authentication _prompt_ (a string) is provided, which is shown to the user. _access_ specifies the item accessibility via a symbol (see previous bullet point).
  - `(prompt access policy ...)`: An authentication _prompt_ (a string) is provided, which is shown to the user. _access_ specifies the item accessibility via a symbol (see previous bullet point). _policy ..._ are access policy specifiers (symbols) which determine what authentication methods should be allowed. Supported are `user-presence`, `biometry-any`, `biometry-current-set`, `device-passcode`, `watch`, `or`, `and`, `private-key-usage`, and `application-password`.

_sync_ is a boolean argument. If set to `#t`, keychain items managed via the keychain client will be synchronized across iCloud.

**(keychain-service _keychain_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the keychain service (a string) for the given _keychain_ client.

**(keychain-access-group _keychain_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the access group identifier (a string) for the given _keychain_ client. If no access group is defined, `#f` is returned.

**(keychain-accessibility _keychain_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the item accessibility specifier for the given _keychain_ client. See `make-keychain` for the supported symbols.

**(keychain-synchronized? _keychain_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the given _keychain_ client synchronizes keychain item updates across iCloud; `#f` otherwise.


## Keychain items

**(keychain-exists? _keychain key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _keychain_ contains an item for the given _key_, `#f` otherwise. _keychain_ is a keychain client object, _key_ is a string.

**(keychain-ref _keychain key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(keychain-ref _keychain key default_)**  

With `keychain-ref` it is possible to retrieve the value set via `keychain-set!` from the item in _keychain_ identified via `key`. Such values are stored in the keychain in serialized fashion. `keychain-ref` deserializes the data and returns the result of this operation. _keychain_ is a keychain client object, _key_ is a string. If the key is unknown, _default_ is returned. If _default_ is not provided, `#f` is used.

**(keychain-ref-attributes _keychain key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns metadata associated with the item in _keychain_ identified via _key_. _keychain_ is a keychain client object, _key_ is a string. Metadata is returned in form of an association list which uses the following keys:

  - `access-group`
  - `accessibility`
  - `comment`
  - `creation-date`
  - `key`
  - `label`
  - `modification-date`
  - `service`
  - `synchronizable`
  - `value`

**(keychain-ref-data _keychain key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the item in _keychain_ identified by string _key_ as a bytevector. If the data in the item cannot be represented as a bytevector, `#f` is returned.

**(keychain-ref-string _keychain key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the item in _keychain_ identified by string _key_ as a string. If the data in the item cannot be represented by a string, `#f` is returned.

**(keychain-set! _keychain key value_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(keychain-set! _keychain key value label_)**  
**(keychain-set! _keychain key value label comment_)**  
**(keychain-set! _keychain key value label comment acc_)**  
**(keychain-set! _keychain key value label comment acc sync_)**  

Creates or overwrites an item identified via string _key_ in _keychain_ with _value_. _value_ can be any serializable expression. Optional argument _label_ defines a string label for this new keychain item (by default, _label_ is `#f`), _comment_ specifies a string comment that is stored as metadata (default is `#f`), _acc_ specifies access policies. The following access policy specifiers are supported:

  - `()` or `#f`: The _keychain_ client defines the access policies to use.
  - `symbol`: Symbols specify the [item accessibility](https://developer.apple.com/documentation/security/restricting-keychain-item-accessibility). Supported are `when-unlocked`, `after-first-unlockl`, `always`, `when-unlocked-this-device-only`, `after-first-unlock-this-device-only`, `always-this-device-only`.
  - `(prompt)`: An authentication _prompt_ (a string) is provided, which is shown to the user. Default item accessibility is used.
  - `(prompt access)`: An authentication _prompt_ (a string) is provided, which is shown to the user. _access_ specifies the item accessibility via a symbol (see previous bullet point).
  - `(prompt access policy ...)`: An authentication _prompt_ (a string) is provided, which is shown to the user. _access_ specifies the item accessibility via a symbol (see previous bullet point). _policy ..._ are access policy specifiers (symbols) which determine what authentication methods should be allowed. Supported are `user-presence`, `biometry-any`, `biometry-current-set`, `device-passcode`, `watch`, `or`, `and`, `private-key-usage`, and `application-password`.

The access policies specified via argument _acc_ override the ones defined by the _keychain_ client object. _sync_ is a boolean argument. If set to `#t`, this keychain item will be synchronized across iCloud. If set to `#f`, this keychain item will only be stored locally. By default, _sync_ is the empty list, which denotes that the _keychain_ client defines whether to sync the item or not.

**(keychain-set-data! _keychain key data_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(keychain-set-data! _keychain key data label_)**  
**(keychain-set-data! _keychain key data label comment_)**  
**(keychain-set-data! _keychain key data label comment acc_)**  
**(keychain-set-data! _keychain key data label comment acc sync_)**  

Creates or overwrites an item identified via string _key_ in _keychain_ with the binary content of bytevector _data_. Optional argument _label_ defines a string label for this new keychain item (by default, _label_ is `#f`), _comment_ specifies a string comment that is stored as metadata (default is `#f`), _acc_ specifies access policies. See the description of `keychain-set!` for the specification of access policies via argument _acc_. Finally, _sync_ is a boolean argument. If set to `#t`, this keychain item will be synchronized across iCloud. If set to `#f`, this keychain item will only be stored locally. By default, _sync_ is the empty list, which denotes that the _keychain_ client defines whether to sync the item or not.

**(keychain-set-string! _keychain key str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(keychain-set-string! _keychain key str label_)**  
**(keychain-set-string! _keychain key str label comment_)**  
**(keychain-set-string! _keychain key str label comment acc_)**  
**(keychain-set-string! _keychain key str label comment acc sync_)**  

Creates or overwrites an item identified via string _key_ in _keychain_ with the string _str_. Optional argument _label_ defines a string label for this new keychain item (by default, _label_ is `#f`), _comment_ specifies a string comment that is stored as metadata (default is `#f`), _acc_ specifies access policies. See the description of `keychain-set!` for the specification of access policies via argument _acc_. Finally, _sync_ is a boolean argument. If set to `#t`, this keychain item will be synchronized across iCloud. If set to `#f`, this keychain item will only be stored locally. By default, _sync_ is the empty list, which denotes that the _keychain_ client defines whether to sync the item or not.

**(keychain-remove! _keychain key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes the item in _keychain_ identified via string _key_. If the item does not exist, the keychain is left untouched.

**(keychain-keys _keychain_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns all the keys of items accessible via _keychain_.


## Keychain utilities

**(available-keychain-services)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns all the services available in the system keychain as a list of strings.

**(available-keychain-keys)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of service/key pairs for all items available in the system keychain.

**(make-password)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new randomly generated password.
