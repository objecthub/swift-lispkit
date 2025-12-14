# LispKit Crypto

Library `(lispkit crypto)` provides an API for cryptographic operations, including message digests, the creation and management of secure keys, and cryptographic algorithms for encrypting, decrypting, signing, and verifying messages.

The following sample code illustrates how `(lispkit crypto)` can be used to implement public key cryptography:

```scheme
(define msg (string->utf8 "This is a secret message!"))
(define privkey (make-private-key 'rsa))
(define pubkey (public-key privkey))
(define encr (encrypt pubkey 'rsa-encryption-pkcs1 msg))
(define decr (decrypt privkey 'rsa-encryption-pkcs1 encr))
(utf8->string decr)
⇒ "This is a secret message!"
```


## Hash functions

Library `(lispkit crypto)` provides a number of cryptographic hash functions, sometimes also called _message digest functions_. A cryptographic hash function maps variable-length, potentially long sequences of bytes to fixed length, relatively short hashes, also called _digests_.

**(md5 _bytevector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(md5 _bytevector start_)**  
**(md5 _bytevector start end_)**  

Implementation of the MD5 message-digest algorithm. Computes a 128-bit hash for the bytes of _bytevector_ between index _start_ (including) and _end_ (excluding) and returns the result as a bytevector. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(sha1 _bytevector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(sha1 _bytevector start_)**  
**(sha1 _bytevector start end_)**  

Implementation of the "Secure Hash Algorithm 1". Computes a 160-bit hash for the bytes of _bytevector_ between index _start_ (including) and _end_ (excluding) and returns the result as a bytevector. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(sha256 _bytevector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(sha256 _bytevector start_)**  
**(sha256 _bytevector start end_)**  

Implementation of the "Secure Hash Algorithm 2" with a 256-bit digest. Computes a 256-bit hash for the bytes of _bytevector_ between index _start_ (including) and _end_ (excluding) and returns the result as a bytevector. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(sha384 _bytevector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(sha384 _bytevector start_)**  
**(sha384 _bytevector start end_)**  

Implementation of the "Secure Hash Algorithm 2" with a 384-bit digest. Computes a 384-bit hash for the bytes of _bytevector_ between index _start_ (including) and _end_ (excluding) and returns the result as a bytevector. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(sha512 _bytevector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(sha512 _bytevector start_)**  
**(sha512 _bytevector start end_)**  

Implementation of the "Secure Hash Algorithm 2" with a 512-bit digest. Computes a 512-bit hash for the bytes of _bytevector_ between index _start_ (including) and _end_ (excluding) and returns the result as a bytevector. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.


## Secure keys

**secure-key-type-tag** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `secure-key` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all secure key objects.

**(make-private-key _cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-private-key _cs size_)**  
**(make-private-key _cs size tag_)**  

Creates a new private key for the cryptographic system _cs_ and returns it as a `secure-key` object. Cryptographic systems are identified via symbols. Currently only `rsa` is supported as value for _cs_. _size_ defines the size of the key in bits; default is 1024. _tag_ is an optional string identifying the application generating the key. If _tag_ is not provided, a random UUID string is being used as application tag.

```scheme
(make-private-key 'rsa 2048 "myapp")
⇒ #<secure-key 600000b26ce0: rsa private 2048>
```

**(public-key _key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a public key for the given private _key_ as a `secure-key` object.

```scheme
(define privkey (make-private-key 'rsa 1024 "demo app"))
(define pubkey (public-key privkey))
(display (secure-key->string pubkey)) ⇒ 
-----BEGIN RSA PUBLIC KEY-----
MIGJAoGBAPh4qcCprhnrCelHVzhvlzVBxe62qDwes3IrMNcvKAYnqgVpSvUN+RNI
AWcQPVEiIPWxMz0/75mT3jFukysGMg6LdFzjslmtgvVutUM7vqkaliGIiBp92QBa
iXjZpD33YxQvKTp7F8hv4sJwgVz4junkM11X7Wnw8R6+1l4fYCbvAgMBAAE=
-----END RSA PUBLIC KEY-----
```

**(secure-key? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a secure key; otherwise `#f` is returned.

**(secure-key-type? _key cs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the secure _key_ is suitable to be used with the cryptographic system _cs_; otherwise `#f` is returned. Cryptographic systems are identified via symbols. Currently only `rsa` is supported as value for _cs_.

**(secure-key-private? _key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the secure _key_ is a private key; otherwise `#f` is returned.

**(secure-key-public? _key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the secure _key_ is a public key; otherwise `#f` is returned.

**(secure-key-can-encrypt? _key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(secure-key-can-encrypt? _key algo_)**  

Returns `#t` if secure _key_ can be used to encrypt messages via algorithm _algo_, otherwise `#f` is returned. _algo_ is a symbol identifying the encryption algorithm (see section on Crypto Algorithms). If _algo_ is not provided, `secure-key-can-encrypt?` returns `#t` if _key_ can be used to encrypt messages in general, independent of a concrete algorithm.

**(secure-key-can-decrypt? _key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(secure-key-can-decrypt? _key algo_)**  

Returns `#t` if secure _key_ can be used to decrypt messages via algorithm _algo_, otherwise `#f` is returned. _algo_ is a symbol identifying the encryption algorithm (see section on Crypto Algorithms). If _algo_ is not provided, `secure-key-can-decrypt?` returns `#t` if _key_ can be used to decrypt messages in general, independent of a concrete algorithm.

**(secure-key-can-sign? _key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(secure-key-can-sign? _key algo_)**  

Returns `#t` if secure _key_ can be used to sign messages via algorithm _algo_, otherwise `#f` is returned. _algo_ is a symbol identifying the encryption algorithm (see section on Crypto Algorithms). If _algo_ is not provided, `secure-key-can-sign?` returns `#t` if _key_ can be used to sign messages in general, independent of a concrete algorithm.

**(secure-key-can-verify? _key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(secure-key-can-verify? _key algo_)**  

Returns `#t` if secure _key_ can be used to verify messages via algorithm _algo_, otherwise `#f` is returned. _algo_ is a symbol identifying the encryption algorithm (see section on Crypto Algorithms). If _algo_ is not provided, `secure-key-can-verify?` returns `#t` if _key_ can be used to verify messages in general, independent of a concrete algorithm.

**(secure-key-size _key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(secure-key-size _key effective_)**  

Returns the size of the given secure _key_ in bits. If _effective_ is `#f` or omitted, the total number of bits in the secure key are returned, otherwise, the effective number of bits used by this secure key are returned.

**(secure-key-block-size _key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the block length associated with the given secure _key_ in bytes. If the secure key is an RSA key, for instance, this is the size of the modulus.

**(secure-key-attributes _key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the attributes associated with the given secure _key_ as an association list. Each attribute is represented by a cons whose car is the key of an attribute and whose cdr is the corresponding value.

```scheme
(secure-key-attributes (make-private-key 'rsa))
⇒ ((unwp . 1) (priv . 0) (sens . 0) (extr . 1) (vrfy . 0)
   (encr . 0) (drve . 1) (modi . 0) (sign . 1) (vyrc . 0)
   (next . 0) (type . "42") (bsiz . 1024) (kcls . "1")
   (asen . 0) (esiz . 1024) (decr . 1) (wrap . 0)
   (class . "keys") (snrc . 0) (perm . 1))
```

**(secure-key=? _key0 key ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Defines an identity relationship for secure keys. `secure-key=?` returns `#t` if each _key ..._ is the same key object as _key0_, otherwise `#f` gets returned.

**(secure-key-data=? _key ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Defines an equivalence relationship for secure keys based on the equivalence of a serialized representation of a secure key. `secure-key-data=?` returns `#t` if each _key ..._ is representing an equivalent key with the same serialized external representation as _key0_. Otherwise, `#f` gets returned.

**(secure-key-\>bytevector _key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Serializes the given secure _key_ into a new bytevector and returns the bytevector.

**(bytevector-\>private-key _cs bytevector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(bytevector-\>private-key _cs bytevector start_)**  
**(bytevector-\>private-key _cs bytevector start end_)**  

Turns a serialized representation of a secure key in _bytevector_ between indices _start_ (inclusive) and _end_ (exclusive) into a new private key of the given cryptographic system _cs_. Cryptographic systems are identified via symbols. Currently only `rsa` is supported as value for _cs_. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(bytevector-\>public-key _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Turns a serialized representation of a secure key in _bytevector_ between indices _start_ (inclusive) and _end_ (exclusive) into a new public key of the given cryptographic system _cs_. Cryptographic systems are identified via symbols. Currently only `rsa` is supported as value for _cs_. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(secure-key-\>string _key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a PEM (Privacy Encoded Mail)-encoded string representation of the given secure _key_.

```scheme
(define pkey (make-private-key 'rsa 1024 "test"))
(display (secure-key->string (public-key pkey))) ⇒
-----BEGIN RSA PUBLIC KEY-----
MIGJAoGBALz04JfvuKHfOtKE6rYGasoebv4T54m88OR0tJ0Bb+7gUWkX8DPWEy/y
Y0m6QOUK0nvpCvNdvZq7dW2Pjnwd4Cwy9lCUGj+MTSrwqL8fM3FvbLGI6lAAPqYb
S/T9zcG/YnNSmB/A6o3EcfYi/nT0u83t6bmSwa0SHNoOQ110fm6jAgMBAAE=
-----END RSA PUBLIC KEY-----
```

**(string-\>secure-key _str_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Turns a PEM (Privacy Encoded Mail)-encoded representation of a secure key _str_ (a string) into a new secure key object and returns this object. An error is raised if it is not possible to extract a secure key supported by this library.


## Crypto algorithms

In library `(lispkit crypto)`, cryptographic algorithms are identified via interned symbols. The following cryptographic algorithms are supported:

&nbsp;&nbsp;**RSA Encryption**

   - `rsa-encryption-raw`
   - `rsa-encryption-pkcs1`

&nbsp;&nbsp;**RSA Encryption OAEP**

   - `rsa-encryption-oaep-sha1`
   - `rsa-encryption-oaep-sha256`
   - `rsa-encryption-oaep-sha384`
   - `rsa-encryption-oaep-sha512`

&nbsp;&nbsp;**RSA Encryption OAEP AESGCM**

   - `rsa-encryption-oaep-sha1-aesgcm`
   - `rsa-encryption-oaep-sha256-aesgcm`
   - `rsa-encryption-oaep-sha384-aesgcm`
   - `rsa-encryption-oaep-sha512-aesgcm`

&nbsp;&nbsp;**RSA Signature Raw**

   - `rsa-signature-raw`

&nbsp;&nbsp;**RSA Signature Digest PKCS1v15**

   - `rsa-signature-digest-pkcs1v15-raw`
   - `rsa-signature-digest-pkcs1v15-sha1`
   - `rsa-signature-digest-pkcs1v15-sha256`
   - `rsa-signature-digest-pkcs1v15-sha384`
   - `rsa-signature-digest-pkcs1v15-sha512`

&nbsp;&nbsp;**RSA Signature Message PKCS1v15**

   - `rsa-signature-message-pkcs1v15-sha1`
   - `rsa-signature-message-pkcs1v15-sha256`
   - `rsa-signature-message-pkcs1v15-sha384`
   - `rsa-signature-message-pkcs1v15-sha512`

&nbsp;&nbsp;**RSA Signature Digest PSS**

   - `rsa-signature-digest-pss-sha1`
   - `rsa-signature-digest-pss-sha256`
   - `rsa-signature-digest-pss-sha384`
   - `rsa-signature-digest-pss-sha512`

&nbsp;&nbsp;**RSA Signature Message PSS**

   - `rsa-signature-message-pss-sha1`
   - `rsa-signature-message-pss-sha256`
   - `rsa-signature-message-pss-sha384`
   - `rsa-signature-message-pss-sha512`

**(encrypt _key algo bytevector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(encrypt _key algo bytevector start_)**  
**(encrypt _key algo bytevector start end_)**  

Encrypts a message represented by _bytevector_ between indices _start_ (inclusive) and _end_ (exclusive) using the given secure _key_ and encryption algorithm _algo_. The encrypted message is returned as a new bytevector.

_algo_ is a symbol identifying the encryption algorithm. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(decrypt _key algo bytevector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(decrypt _key algo bytevector start_)**  
**(decrypt _key algo bytevector start end_)**  

Decrypts an encrypted message represented by _bytevector_ between indices _start_ (inclusive) and _end_ (exclusive) using the given secure _key_ and encryption algorithm _algo_. The decrypted message is returned as a new bytevector.

_algo_ is a symbol identifying the encryption algorithm. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(sign _key algo bytevector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(sign _key algo bytevector start_)**  
**(sign _key algo bytevector start end_)**  

Signs a message represented by _bytevector_ between indices _start_ (inclusive) and _end_ (exclusive) using the given secure _key_ and signature algorithm _algo_. The signature of the message is returned as a new bytevector.

_algo_ is a symbol identifying the signature algorithm. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.

**(verify _key algo bytevector_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(verify _key algo bytevector start_)**  
**(verify _key algo bytevector start end_)**  

Verifies a signature represented by _bytevector_ between indices _start_ (inclusive) and _end_ (exclusive) using the given secure _key_ and signature algorithm _algo_. `verify` returns `#t` if the signature could be verified, otherwise `#f` is returned.

_algo_ is a symbol identifying the signature algorithm. If _end_ is not provided, it is assumed to be the length of _bytevector_. If _start_ is not provided, it is assumed to be 0.
