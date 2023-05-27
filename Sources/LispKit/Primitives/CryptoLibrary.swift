//
//  CryptoLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 19/02/2023.
//  Copyright © 2023 ObjectHub. All rights reserved.
//
//  Licensed under the Apache License, Version 2.0 (the "License");
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
//

import Foundation
import Security
import CryptoKit

public final class CryptoLibrary: NativeLibrary {
  
  enum CryptoSystem {
    case rsa
  }
  
  /// Crypto system specifiers
  private let rsa: Symbol
  
  /// Supported algorithms
  
  // RSA Encryption
  private let rsaEncryptionRaw: Symbol
  private let rsaEncryptionPKCS1: Symbol
  // RSA Encryption OAEP
  private let rsaEncryptionOAEPSHA1: Symbol
  private let rsaEncryptionOAEPSHA256: Symbol
  private let rsaEncryptionOAEPSHA384: Symbol
  private let rsaEncryptionOAEPSHA512: Symbol
  // RSA Encryption OAEP AESGCM
  private let rsaEncryptionOAEPSHA1AESGCM: Symbol
  private let rsaEncryptionOAEPSHA256AESGCM: Symbol
  private let rsaEncryptionOAEPSHA384AESGCM: Symbol
  private let rsaEncryptionOAEPSHA512AESGCM: Symbol
  // RSA Signature Raw
  private let rsaSignatureRaw: Symbol
  // RSA Signature Digest PKCS1v15
  private let rsaSignatureDigestPKCS1Raw: Symbol
  private let rsaSignatureDigestPKCS1SHA1: Symbol
  private let rsaSignatureDigestPKCS1SHA256: Symbol
  private let rsaSignatureDigestPKCS1SHA384: Symbol
  private let rsaSignatureDigestPKCS1SHA512: Symbol
  // RSA Signature Message PKCS1v15
  private let rsaSignatureMessagePKCS1SHA1: Symbol
  private let rsaSignatureMessagePKCS1SHA256: Symbol
  private let rsaSignatureMessagePKCS1SHA384: Symbol
  private let rsaSignatureMessagePKCS1SHA512: Symbol
  // RSA Signature Digest PSS
  private let rsaSignatureDigestPSSSHA1: Symbol
  private let rsaSignatureDigestPSSSHA256: Symbol
  private let rsaSignatureDigestPSSSHA384: Symbol
  private let rsaSignatureDigestPSSSHA512: Symbol
  // RSA Signature Message PSS
  private let rsaSignatureMessagePSSSHA1: Symbol
  private let rsaSignatureMessagePSSSHA256: Symbol
  private let rsaSignatureMessagePSSSHA384: Symbol
  private let rsaSignatureMessagePSSSHA512: Symbol
  
  /// Initialize symbols.
  public required init(in context: Context) throws {
    self.rsa = context.symbols.intern("rsa")
    self.rsaEncryptionRaw = context.symbols.intern("rsa-encryption-raw")
    self.rsaEncryptionPKCS1 = context.symbols.intern("rsa-encryption-pkcs1")
    self.rsaEncryptionOAEPSHA1 = context.symbols.intern("rsa-encryption-oaep-sha1")
    self.rsaEncryptionOAEPSHA256 = context.symbols.intern("rsa-encryption-oaep-sha256")
    self.rsaEncryptionOAEPSHA384 = context.symbols.intern("rsa-encryption-oaep-sha384")
    self.rsaEncryptionOAEPSHA512 = context.symbols.intern("rsa-encryption-oaep-sha512")
    self.rsaEncryptionOAEPSHA1AESGCM = context.symbols.intern("rsa-encryption-oaep-sha1-aesgcm")
    self.rsaEncryptionOAEPSHA256AESGCM = context.symbols.intern("rsa-encryption-oaep-sha256-aesgcm")
    self.rsaEncryptionOAEPSHA384AESGCM = context.symbols.intern("rsa-encryption-oaep-sha384-aesgcm")
    self.rsaEncryptionOAEPSHA512AESGCM = context.symbols.intern("rsa-encryption-oaep-sha512-aesgcm")
    self.rsaSignatureRaw = context.symbols.intern("rsa-signature-raw")
    self.rsaSignatureDigestPKCS1Raw = context.symbols.intern("rsa-signature-digest-pkcs1v15-raw")
    self.rsaSignatureDigestPKCS1SHA1 = context.symbols.intern("rsa-signature-digest-pkcs1v15-sha1")
    self.rsaSignatureDigestPKCS1SHA256 = context.symbols.intern("rsa-signature-digest-pkcs1v15-sha256")
    self.rsaSignatureDigestPKCS1SHA384 = context.symbols.intern("rsa-signature-digest-pkcs1v15-sha384")
    self.rsaSignatureDigestPKCS1SHA512 = context.symbols.intern("rsa-signature-digest-pkcs1v15-sha512")
    self.rsaSignatureMessagePKCS1SHA1 = context.symbols.intern("rsa-signature-message-pkcs1v15-sha1")
    self.rsaSignatureMessagePKCS1SHA256 = context.symbols.intern("rsa-signature-message-pkcs1v15-sha256")
    self.rsaSignatureMessagePKCS1SHA384 = context.symbols.intern("rsa-signature-message-pkcs1v15-sha384")
    self.rsaSignatureMessagePKCS1SHA512 = context.symbols.intern("rsa-signature-message-pkcs1v15-sha512")
    self.rsaSignatureDigestPSSSHA1 = context.symbols.intern("rsa-signature-digest-pss-sha1")
    self.rsaSignatureDigestPSSSHA256 = context.symbols.intern("rsa-signature-digest-pss-sha256")
    self.rsaSignatureDigestPSSSHA384 = context.symbols.intern("rsa-signature-digest-pss-sha384")
    self.rsaSignatureDigestPSSSHA512 = context.symbols.intern("rsa-signature-digest-pss-sha512")
    self.rsaSignatureMessagePSSSHA1 = context.symbols.intern("rsa-signature-message-pss-sha1")
    self.rsaSignatureMessagePSSSHA256 = context.symbols.intern("rsa-signature-message-pss-sha256")
    self.rsaSignatureMessagePSSSHA384 = context.symbols.intern("rsa-signature-message-pss-sha384")
    self.rsaSignatureMessagePSSSHA512 = context.symbols.intern("rsa-signature-message-pss-sha512")
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "crypto"]
  }
  
  /// Declarations of the library.
  public override func declarations() {
    // Hash functions
    self.define(Procedure("md5", self.md5))
    self.define(Procedure("sha1", self.sha1))
    self.define(Procedure("sha256", self.sha256))
    self.define(Procedure("sha384", self.sha384))
    self.define(Procedure("sha512", self.sha512))
    // Secure keys
    self.define(Procedure("make-private-key", self.makePrivateKey))
    self.define(Procedure("public-key", self.publicKey))
    self.define(Procedure("secure-key?", self.isSecureKey))
    self.define(Procedure("secure-key-type?", self.isSecureKeyType))
    self.define(Procedure("secure-key-private?", self.isSecureKeyPrivate))
    self.define(Procedure("secure-key-public?", self.isSecureKeyPublic))
    self.define(Procedure("secure-key-can-encrypt?", self.secureKeyCanEncrypt))
    self.define(Procedure("secure-key-can-decrypt?", self.secureKeyCanDecrypt))
    self.define(Procedure("secure-key-can-sign?", self.secureKeyCanSign))
    self.define(Procedure("secure-key-can-verify?", self.secureKeyCanVerify))
    self.define(Procedure("secure-key-size", self.secureKeySize))
    self.define(Procedure("secure-key-block-size", self.secureKeyBlockSize))
    self.define(Procedure("secure-key-attributes", self.secureKeyAttributes))
    self.define(Procedure("secure-key=?", self.secureKeyEquals))
    self.define(Procedure("secure-key-data=?", self.secureKeyDataEquals))
    self.define(Procedure("secure-key->bytevector", self.secureKeyToBytevector))
    self.define(Procedure("bytevector->private-key", self.bytevectorToPrivateKey))
    self.define(Procedure("bytevector->public-key", self.bytevectorToPublicKey))
    self.define(Procedure("secure-key->string", self.secureKeyToString))
    self.define(Procedure("string->secure-key", self.stringToSecureKey))
    // Apply cypto algorithms
    self.define(Procedure("encrypt", self.encrypt))
    self.define(Procedure("decrypt", self.decrypt))
    self.define(Procedure("sign", self.sign))
    self.define(Procedure("verify", self.verify))
  }
  
  private func md5(expr: Expr, args: Arguments) throws -> Expr {
    let subvec = try BytevectorLibrary.subVector("md5", expr, args)
    return BytevectorLibrary.bytevector(from: Data(Insecure.MD5.hash(data: Data(subvec))))
  }
  
  private func sha1(expr: Expr, args: Arguments) throws -> Expr {
    let subvec = try BytevectorLibrary.subVector("sha1", expr, args)
    return BytevectorLibrary.bytevector(from: Data(Insecure.SHA1.hash(data: Data(subvec))))
  }
  
  private func sha256(expr: Expr, args: Arguments) throws -> Expr {
    let subvec = try BytevectorLibrary.subVector("sha256", expr, args)
    return BytevectorLibrary.bytevector(from: Data(SHA256.hash(data: Data(subvec))))
  }
  
  private func sha384(expr: Expr, args: Arguments) throws -> Expr {
    let subvec = try BytevectorLibrary.subVector("sha384", expr, args)
    return BytevectorLibrary.bytevector(from: Data(SHA384.hash(data: Data(subvec))))
  }
  
  private func sha512(expr: Expr, args: Arguments) throws -> Expr {
    let subvec = try BytevectorLibrary.subVector("sha512", expr, args)
    return BytevectorLibrary.bytevector(from: Data(SHA512.hash(data: Data(subvec))))
  }
  
  private func cryptoSystem(_ expr: Expr) throws -> CryptoSystem {
    let spec = try expr.asSymbol()
    switch spec {
      case self.rsa:
        return .rsa
      default:
        throw RuntimeError.eval(.unsupportedCryptoSystem, expr)
    }
  }
  
  private func cryptoAlgorithm(_ expr: Expr) throws -> SecKeyAlgorithm {
    let spec = try expr.asSymbol()
    switch spec {
      case self.rsaEncryptionRaw:
        return .rsaEncryptionRaw
      case self.rsaEncryptionPKCS1:
        return .rsaEncryptionPKCS1
      case self.rsaEncryptionOAEPSHA1:
        return .rsaEncryptionOAEPSHA1
      case self.rsaEncryptionOAEPSHA256:
        return .rsaEncryptionOAEPSHA256
      case self.rsaEncryptionOAEPSHA384:
        return .rsaEncryptionOAEPSHA384
      case self.rsaEncryptionOAEPSHA512:
        return .rsaEncryptionOAEPSHA512
      case self.rsaEncryptionOAEPSHA1AESGCM:
        return .rsaEncryptionOAEPSHA1AESGCM
      case self.rsaEncryptionOAEPSHA256AESGCM:
        return .rsaEncryptionOAEPSHA256AESGCM
      case self.rsaEncryptionOAEPSHA384AESGCM:
        return .rsaEncryptionOAEPSHA384AESGCM
      case self.rsaEncryptionOAEPSHA512AESGCM:
        return .rsaEncryptionOAEPSHA512AESGCM
      case self.rsaSignatureRaw:
        return .rsaSignatureRaw
      case self.rsaSignatureDigestPKCS1Raw:
        return .rsaSignatureDigestPKCS1v15Raw
      case self.rsaSignatureDigestPKCS1SHA1:
        return .rsaSignatureDigestPKCS1v15SHA1
      case self.rsaSignatureDigestPKCS1SHA256:
        return .rsaSignatureDigestPKCS1v15SHA256
      case self.rsaSignatureDigestPKCS1SHA384:
        return .rsaSignatureDigestPKCS1v15SHA384
      case self.rsaSignatureDigestPKCS1SHA512:
        return .rsaSignatureDigestPKCS1v15SHA512
      case self.rsaSignatureMessagePKCS1SHA1:
        return .rsaSignatureMessagePKCS1v15SHA1
      case self.rsaSignatureMessagePKCS1SHA256:
        return .rsaSignatureMessagePKCS1v15SHA256
      case self.rsaSignatureMessagePKCS1SHA384:
        return .rsaSignatureMessagePKCS1v15SHA384
      case self.rsaSignatureMessagePKCS1SHA512:
        return .rsaSignatureMessagePKCS1v15SHA512
      case self.rsaSignatureDigestPSSSHA1:
        return .rsaSignatureDigestPSSSHA1
      case self.rsaSignatureDigestPSSSHA256:
        return .rsaSignatureDigestPSSSHA256
      case self.rsaSignatureDigestPSSSHA384:
        return .rsaSignatureDigestPSSSHA384
      case self.rsaSignatureDigestPSSSHA512:
        return .rsaSignatureDigestPSSSHA512
      case self.rsaSignatureMessagePSSSHA1:
        return .rsaSignatureMessagePSSSHA1
      case self.rsaSignatureMessagePSSSHA256:
        return .rsaSignatureMessagePSSSHA256
      case self.rsaSignatureMessagePSSSHA384:
        return .rsaSignatureMessagePSSSHA384
      case self.rsaSignatureMessagePSSSHA512:
        return .rsaSignatureMessagePSSSHA512
      default:
        throw RuntimeError.eval(.unsupportedCryptoAlgorithm, expr)
    }
  }
  
  private func makePrivateKey(css: Expr, param0: Expr?, param1: Expr?) throws -> Expr {
    switch try self.cryptoSystem(css) {
      case .rsa:
        let keySize = try param0?.asInt(above: 512, below: 10241) ?? 1024
        guard let tagName = (try param1?.asString() ?? UUID().uuidString).data(using: .utf8) else {
          return .false
        }
        let privateKeyParams: [CFString: Any] = [
          kSecAttrIsPermanent: true,
          kSecAttrApplicationTag: tagName
        ]
        let parameters: [CFString: Any] = [
          kSecAttrKeyType: kSecAttrKeyTypeRSA,
          kSecAttrKeyClass: kSecAttrKeyClassPrivate,
          kSecAttrKeySizeInBits: keySize,
          kSecPrivateKeyAttrs: privateKeyParams
        ]
        var error: Unmanaged<CFError>?
        guard let privateKey = SecKeyCreateRandomKey(parameters as CFDictionary, &error) else {
          if let error = error?.takeRetainedValue() {
            throw error as Error
          }
          return .false
        }
        return .object(SecureKey(privateKey))
    }
  }
  
  private func publicKey(sc: Expr) throws -> Expr {
    let privateKey = try self.asSecureKey(sc).key
    guard let publicKey = SecKeyCopyPublicKey(privateKey) else {
      return .false
    }
    return .object(SecureKey(publicKey)) 
  }
  

  // Secure keys
  
  private func asSecureKey(_ expr: Expr) throws -> SecureKey {
    guard case .object(let obj) = expr, let sk = obj as? SecureKey else {
      throw RuntimeError.type(expr, expected: [SecureKey.type])
    }
    return sk
  }
  
  private func isSecureKey(_ expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is SecureKey {
      return .true
    }
    return .false
  }
  
  private func isSecureKeyPrivate(expr: Expr) throws -> Expr {
    guard let attribs = try self.asSecureKey(expr).attributes(),
          let val = attribs["kcls" as CFString] as? String else {
      return .false
    }
    return .makeBoolean(val == "1")
  }
  
  private func isSecureKeyPublic(expr: Expr) throws -> Expr {
    guard let attribs = try self.asSecureKey(expr).attributes(),
          let val = attribs["kcls" as CFString] as? String else {
      return .false
    }
    return .makeBoolean(val == "0")
  }
  
  private func isSecureKeyType(expr: Expr, css: Expr) throws -> Expr {
    guard let attribs = try self.asSecureKey(expr).attributes(),
          let val = attribs["type" as CFString] as? String else {
      return .false
    }
    switch try self.cryptoSystem(css) {
      case .rsa:
        return .makeBoolean(val == "42")
    }
  }
  
  private func secureKeyCanEncrypt(expr: Expr, algo: Expr?) throws -> Expr {
    let key = try self.asSecureKey(expr)
    if let algo = algo {
      let algorithm = try self.cryptoAlgorithm(algo)
      return .makeBoolean(SecKeyIsAlgorithmSupported(key.key, .encrypt, algorithm))
    } else {
      guard let attribs = key.attributes(),
            let val = attribs["encr" as CFString] as? Int else {
        return .false
      }
      return .makeBoolean(val == 1)
    }
  }
  
  private func secureKeyCanDecrypt(expr: Expr, algo: Expr?) throws -> Expr {
    let key = try self.asSecureKey(expr)
    if let algo = algo {
      let algorithm = try self.cryptoAlgorithm(algo)
      return .makeBoolean(SecKeyIsAlgorithmSupported(key.key, .decrypt, algorithm))
    } else {
      guard let attribs = key.attributes(),
            let val = attribs["decr" as CFString] as? Int else {
        return .false
      }
      return .makeBoolean(val == 1)
    }
  }
  
  private func secureKeyCanSign(expr: Expr, algo: Expr?) throws -> Expr {
    let key = try self.asSecureKey(expr)
    if let algo = algo {
      let algorithm = try self.cryptoAlgorithm(algo)
      return .makeBoolean(SecKeyIsAlgorithmSupported(key.key, .sign, algorithm))
    } else {
      guard let attribs = key.attributes(),
            let val = attribs["sign" as CFString] as? Int else {
        return .false
      }
      return .makeBoolean(val == 1)
    }
  }
  
  private func secureKeyCanVerify(expr: Expr, algo: Expr?) throws -> Expr {
    let key = try self.asSecureKey(expr)
    if let algo = algo {
      let algorithm = try self.cryptoAlgorithm(algo)
      return .makeBoolean(SecKeyIsAlgorithmSupported(key.key, .verify, algorithm))
    } else {
      guard let attribs = key.attributes(),
            let val = attribs["vrfy" as CFString] as? Int else {
        return .false
      }
      return .makeBoolean(val == 1)
    }
  }
  
  private func secureKeySize(expr: Expr, effective: Expr?) throws -> Expr {
    guard let attribs = try self.asSecureKey(expr).attributes() else {
      return .false
    }
    let key = (effective?.isTrue ?? false) ? "esiz" : "bsiz"
    guard let size = attribs[key as CFString] as? Int else {
      return .false
    }
    return .makeNumber(size)
  }
  
  private func secureKeyBlockSize(expr: Expr) throws -> Expr {
    let sk = try self.asSecureKey(expr).key
    return .makeNumber(SecKeyGetBlockSize(sk))
  }
  
  private func secureKeyAttributes(expr: Expr) throws -> Expr {
    guard let attribs = try self.asSecureKey(expr).attributes() else {
      return .false
    }
    var res = Expr.null
    for (key, val) in attribs {
      if let keyName = key as? String {
        var value: Expr? = nil
        if let num = val as? Int {
          value = .fixnum(Int64(num))
        } else if let str = val as? String {
          value = .makeString(str)
        }
        if let value = value {
          res = .pair(.pair(.symbol(self.context.symbols.intern(keyName)), value), res)
        }
      }
    }
    return res
  }
  
  private func secureKeyEquals(expr: Expr, args: Arguments) throws -> Expr {
    let this = try self.asSecureKey(expr)
    for arg in args {
      let that = try self.asSecureKey(arg)
      if this.key != that.key {
        return .false
      }
    }
    return .true
  }
  
  private func secureKeyDataEquals(expr: Expr, args: Arguments) throws -> Expr {
    let this = try self.asSecureKey(expr)
    var error: Unmanaged<CFError>?
    guard let thisData = SecKeyCopyExternalRepresentation(this.key, &error) as Data? else {
      if let error = error?.takeRetainedValue() {
        throw error as Error
      }
      return .false
    }
    for arg in args {
      let that = try self.asSecureKey(arg)
      guard let thatData = SecKeyCopyExternalRepresentation(that.key, &error) as Data? else {
        if let error = error?.takeRetainedValue() {
          throw error as Error
        }
        return .false
      }
      if thisData != thatData {
        return .false
      }
    }
    return .true
  }
  
  private func secureKeyToBytevector(expr: Expr) throws -> Expr {
    let key = try self.asSecureKey(expr).key
    var error: Unmanaged<CFError>?
    guard let unwrappedData = SecKeyCopyExternalRepresentation(key, &error) as Data? else {
      if let error = error?.takeRetainedValue() {
        throw error as Error
      }
      return .false
    }
    return BytevectorLibrary.bytevector(from: unwrappedData)
  }
  
  private func bytevectorToPrivateKey(css: Expr, expr: Expr, args: Arguments) throws -> Expr {
    guard case .rsa = try self.cryptoSystem(css) else {
      throw RuntimeError.eval(.unsupportedCryptoSystem, css)
    }
    let data = Data(try BytevectorLibrary.subVector("bytevector->private-key", expr, args))
    let keySize = data.count * 8
    let keyDict: [CFString: Any] = [
      kSecAttrKeyType: kSecAttrKeyTypeRSA,
      kSecAttrKeyClass: kSecAttrKeyClassPrivate,
      kSecAttrKeySizeInBits: keySize,
      kSecReturnPersistentRef: true
    ]
    var error: Unmanaged<CFError>?
    guard let key = SecKeyCreateWithData(data as CFData, keyDict as CFDictionary, &error) else {
      if let error = error?.takeRetainedValue() {
        throw error as Error
      }
      return .false
    }
    return .object(SecureKey(key))
  }
  
  private func bytevectorToPublicKey(css: Expr, expr: Expr, args: Arguments) throws -> Expr {
    guard case .rsa = try self.cryptoSystem(css) else {
      throw RuntimeError.eval(.unsupportedCryptoSystem, css)
    }
    let data = Data(try BytevectorLibrary.subVector("bytevector->public-key", expr, args))
    let keySize = data.count * 8
    let keyDict: [CFString: Any] = [
      kSecAttrKeyType: kSecAttrKeyTypeRSA,
      kSecAttrKeyClass: kSecAttrKeyClassPublic,
      kSecAttrKeySizeInBits: keySize,
      kSecReturnPersistentRef: true
    ]
    var error: Unmanaged<CFError>?
    guard let key = SecKeyCreateWithData(data as CFData, keyDict as CFDictionary, &error) else {
      if let error = error?.takeRetainedValue() {
        throw error as Error
      }
      return .false
    }
    return .object(SecureKey(key))
  }
  
  private func secureKeyToString(expr: Expr) throws -> Expr {
    func split(_ str: String, byChunksOfLength length: Int) -> [String] {
      return stride(from: 0, to: str.count, by: length).map { index -> String in
        let startIndex = str.index(str.startIndex, offsetBy: index)
        let endIndex = str.index(startIndex, offsetBy: length, limitedBy: str.endIndex) ??
        str.endIndex
        return String(str[startIndex..<endIndex])
      }
    }
    let key = try self.asSecureKey(expr).key
    guard let attributes = SecKeyCopyAttributes(key) as Dictionary<NSObject, AnyObject>?,
          let cryptoSystem = attributes["type" as CFString] as? String,
          let keyType = attributes["kcls" as CFString] as? String else {
      return .false
    }
    guard cryptoSystem == "42" else {
      throw RuntimeError.eval(.unsupportedCryptoSystem, Expr.symbol(Symbol(uninterned: "unknown")))
    }
    var error: Unmanaged<CFError>?
    guard let keyData = SecKeyCopyExternalRepresentation(key, &error) as Data? else {
      if let error = error?.takeRetainedValue() {
        throw error as Error
      }
      return .false
    }
    let pemType = keyType == "1" ? "RSA PRIVATE KEY" : "RSA PUBLIC KEY"
    let chunks = split(keyData.base64EncodedString(options: []), byChunksOfLength: 64)
    let pem = ["-----BEGIN \(pemType)-----",
               chunks.joined(separator: "\n"),
               "-----END \(pemType)-----\n"]
    return .makeString(pem.joined(separator: "\n"))
  }
  
  private func stringToSecureKey(expr: Expr) throws -> Expr {
    let pemString = try expr.asString()
    var keyType = kSecAttrKeyClassPublic
    if pemString.contains("-----BEGIN RSA PRIVATE KEY") {
      keyType = kSecAttrKeyClassPrivate
    } else if !pemString.contains("-----BEGIN RSA PUBLIC KEY") {
      return .false
    }
    let lines = pemString.components(separatedBy: "\n").filter { line in
      return !line.hasPrefix("-----BEGIN") && !line.hasPrefix("-----END")
    }
    guard lines.count != 0,
          let data = Data(base64Encoded: lines.joined(separator: ""), options: []) else {
      return .false
    }
    let keySize = data.count * 8
    let keyDict: [CFString: Any] = [
      kSecAttrKeyType: kSecAttrKeyTypeRSA,
      kSecAttrKeyClass: keyType,
      kSecAttrKeySizeInBits: keySize,
      kSecReturnPersistentRef: true
    ]
    var error: Unmanaged<CFError>?
    guard let key = SecKeyCreateWithData(data as CFData, keyDict as CFDictionary, &error) else {
      if let error = error?.takeRetainedValue() {
        throw error as Error
      }
      return .false
    }
    return .object(SecureKey(key))
  }
  
  private func encrypt(key: Expr, algo: Expr, bvec: Expr, args: Arguments) throws -> Expr {
    let sk = try self.asSecureKey(key)
    let algorithm = try self.cryptoAlgorithm(algo)
    let data = Data(try BytevectorLibrary.subVector("encrypt", bvec, args))
    guard SecKeyIsAlgorithmSupported(sk.key, .encrypt, algorithm) else {
      throw RuntimeError.eval(.keyDoesNotSupportAlgorithm, key, algo, .makeString("encryption"))
    }
    var error: Unmanaged<CFError>?
    guard let encrypted = SecKeyCreateEncryptedData(sk.key,
                                                    algorithm,
                                                    data as CFData,
                                                    &error) as Data? else {
      if let error = error?.takeRetainedValue() {
        throw error as Error
      }
      return .false
    }
    return BytevectorLibrary.bytevector(from: encrypted)
  }
  
  private func decrypt(key: Expr, algo: Expr, bvec: Expr, args: Arguments) throws -> Expr {
    let sk = try self.asSecureKey(key)
    let algorithm = try self.cryptoAlgorithm(algo)
    let encrypted = Data(try BytevectorLibrary.subVector("decrypt", bvec, args))
    guard SecKeyIsAlgorithmSupported(sk.key, .decrypt, algorithm) else {
      throw RuntimeError.eval(.keyDoesNotSupportAlgorithm, key, algo, .makeString("decryption"))
    }
    var error: Unmanaged<CFError>?
    guard let data = SecKeyCreateDecryptedData(sk.key,
                                               algorithm,
                                               encrypted as CFData,
                                               &error) as Data? else {
      if let error = error?.takeRetainedValue() {
        throw error as Error
      }
      return .false
    }
    return BytevectorLibrary.bytevector(from: data)
  }
  
  private func sign(key: Expr, algo: Expr, bvec: Expr, args: Arguments) throws -> Expr {
    let sk = try self.asSecureKey(key)
    let algorithm = try self.cryptoAlgorithm(algo)
    let data = Data(try BytevectorLibrary.subVector("sign", bvec, args))
    guard SecKeyIsAlgorithmSupported(sk.key, .sign, algorithm) else {
      throw RuntimeError.eval(.keyDoesNotSupportAlgorithm, key, algo, .makeString("signing"))
    }
    var error: Unmanaged<CFError>?
    guard let signature = SecKeyCreateSignature(sk.key,
                                                algorithm,
                                                data as CFData,
                                                &error) as Data? else {
      if let error = error?.takeRetainedValue() {
        throw error as Error
      }
      return .false
    }
    return BytevectorLibrary.bytevector(from: signature)
  }
  
  private func verify(key: Expr, algo: Expr, sig: Expr, args: Arguments) throws -> Expr {
    let sk = try self.asSecureKey(key)
    let algorithm = try self.cryptoAlgorithm(algo)
    let signature = Data(try sig.asByteVector().value)
    guard let bvec = args.first else {
      throw RuntimeError.argumentCount(of: "verify",
                                       min: 4,
                                       max: 6,
                                       args: .pair(key, .pair(algo, .pair(sig, .null))))
    }
    let data = Data(try BytevectorLibrary.subVector("sign",
                                                    bvec,
                                                    args[args.index(after: args.startIndex)...]))
    guard SecKeyIsAlgorithmSupported(sk.key, .verify, algorithm) else {
      throw RuntimeError.eval(.keyDoesNotSupportAlgorithm, key, algo, .makeString("verifying"))
    }
    var error: Unmanaged<CFError>?
    guard SecKeyVerifySignature(sk.key,
                                algorithm,
                                data as CFData,
                                signature as CFData,
                                &error) else {
      if let error = error?.takeRetainedValue() {
        throw error as Error
      }
      return .false
    }
    return .true
  }
}

public final class SecureKey: NativeObject {

  /// Type representing secure keys
  public static let type = Type.objectType(Symbol(uninterned: "secure-key"))
  
  /// The secure key
  public let key: SecKey
  
  /// Initializer
  public init(_ key: SecKey) {
    self.key = key
  }
  
  public func attributes() -> Dictionary<NSObject, AnyObject>? {
    return SecKeyCopyAttributes(self.key) as Dictionary<NSObject, AnyObject>?
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    if let attribs = self.attributes() {
      var attribStr = "?"
      if let val = attribs["type" as CFString] as? String {
        attribStr = val == "42" ? "rsa" : "unknown"
      }
      if let val = attribs["kcls" as CFString] as? String {
        attribStr += val == "1" ? " private" : " public"
      } else {
        attribStr += " ?"
      }
      if let val = attribs["bsiz" as CFString] as? Int {
        attribStr += " \(val)"
      } else {
        attribStr += " ?"
      }
      return "#<secure-key \(self.identityString): \(attribStr)>"
    } else {
      return "#<secure-key \(self.identityString)>"
    }
  }
  
  public override func unpack() -> Exprs {
    var type: Int = 0
    var kind: Int = 0
    var esize: Int = 0
    var ksize: Int = 0
    if let attribs = self.attributes() {
      if let val = attribs["type" as CFString] as? String, val == "42" {
        type = 1 // RSA
      }
      if let val = attribs["kcls" as CFString] as? String {
        kind = val == "1" ? 1 /* private */ : 2 /* public */
      }
      if let size = attribs["esiz" as CFString] as? Int {
        esize = size
      }
      if let size = attribs["bsiz" as CFString] as? Int {
        ksize = size
      }
    }
    return [.makeString(self.identityString),
            .makeNumber(type),
            .makeNumber(kind),
            .makeNumber(ksize),
            .makeNumber(esize)]
  }
}
