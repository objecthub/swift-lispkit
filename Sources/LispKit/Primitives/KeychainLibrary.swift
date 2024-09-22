//
//  KeychainLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 27/08/2024.
//  Copyright © 2024 ObjectHub. All rights reserved.
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
import KeychainAccess

public final class KeychainLibrary: NativeLibrary {
  
  // Accessibility symbols
  private let whenUnlocked: Symbol
  private let afterFirstUnlock: Symbol
  private let always: Symbol
  private let whenPasscodeSetThisDeviceOnly: Symbol
  private let whenUnlockedThisDeviceOnly: Symbol
  private let afterFirstUnlockThisDeviceOnly: Symbol
  private let alwaysThisDeviceOnly: Symbol
  
  // Accessibility policy
  private let userPresence: Symbol
  private let biometryAny: Symbol
  private let biometryCurrentSet: Symbol
  private let devicePasscode: Symbol
  private let watch: Symbol
  private let or: Symbol
  private let and: Symbol
  private let privateKeyUsage: Symbol
  private let applicationPassword: Symbol
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    self.whenUnlocked = context.symbols.intern("when-unlocked")
    self.afterFirstUnlock = context.symbols.intern("after-first-unlock")
    self.always = context.symbols.intern("always")
    self.whenPasscodeSetThisDeviceOnly = context.symbols.intern("when-passcode-set-this-device-only")
    self.whenUnlockedThisDeviceOnly = context.symbols.intern("when-unlocked-this-device-only")
    self.afterFirstUnlockThisDeviceOnly = context.symbols.intern("after-first-unlock-this-device-only")
    self.alwaysThisDeviceOnly = context.symbols.intern("always-this-device-only")
    self.userPresence = context.symbols.intern("user-presence")
    self.biometryAny = context.symbols.intern("biometry-any")
    self.biometryCurrentSet = context.symbols.intern("biometry-current-set")
    self.devicePasscode = context.symbols.intern("device-passcode")
    self.watch = context.symbols.intern("watch")
    self.or = context.symbols.intern("or")
    self.and = context.symbols.intern("and")
    self.privateKeyUsage = context.symbols.intern("private-key-usage")
    self.applicationPassword = context.symbols.intern("application-password")
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "system", "keychain"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define("keychain-type-tag", as: Future.type.objectTypeTag())
    self.define(Procedure("keychain?", self.isKeychain))
    self.define(Procedure("make-keychain", self.makeKeychain))
    self.define(Procedure("keychain-service", self.keychainService))
    self.define(Procedure("keychain-access-group", self.keychainAccessGroup))
    self.define(Procedure("keychain-accessibility", self.keychainAccessibility))
    self.define(Procedure("keychain-synchronized?", self.keychainSynchronized))
    self.define(Procedure("keychain-exists?", self.keychainExists))
    self.define(Procedure("keychain-ref", self.keychainRef))
    self.define(Procedure("keychain-ref-attributes", self.keychainRefAttributes))
    self.define(Procedure("keychain-ref-data", self.keychainRefData))
    self.define(Procedure("keychain-ref-string", self.keychainRefString))
    self.define(Procedure("keychain-set!", self.keychainSet))
    self.define(Procedure("keychain-set-data!", self.keychainSetData))
    self.define(Procedure("keychain-set-string!", self.keychainSetString))
    self.define(Procedure("keychain-remove!", self.keychainRemove))
    self.define(Procedure("keychain-keys", self.keychainKeys))
    self.define(Procedure("available-keychain-services", self.availableKeychainServices))
    self.define(Procedure("available-keychain-keys", self.availableKeychainKeys))
    self.define(Procedure("make-password", self.makePassword))
  }
  
  private func keychain(from: Expr) throws -> Keychain {
    guard case .object(let obj) = from, let keychain = obj as? NativeKeychain else {
      throw RuntimeError.type(from, expected: [NativeKeychain.type])
    }
    return keychain.keychain
  }
  
  private func accessibility(from: Expr) throws -> Accessibility {
    guard case .symbol(let sym) = from else {
      throw RuntimeError.eval(.expectedAccessibilitySpecifier, from)
    }
    switch sym {
      case self.whenUnlocked:
        return .whenUnlocked
      case self.afterFirstUnlock:
        return .afterFirstUnlock
      #if !targetEnvironment(macCatalyst)
      case self.always:
        return .always
      #endif
      case self.whenUnlockedThisDeviceOnly:
        return .whenUnlockedThisDeviceOnly
      case self.afterFirstUnlockThisDeviceOnly:
        return .afterFirstUnlock
      #if !targetEnvironment(macCatalyst)
      case self.alwaysThisDeviceOnly:
        return .alwaysThisDeviceOnly
      #endif
      default:
        throw RuntimeError.eval(.expectedAccessibilitySpecifier, from)
    }
  }
  
  private func expr(from accessibility: Accessibility) -> Expr {
    switch accessibility {
      case .whenUnlocked:
        return .symbol(self.whenUnlocked)
      case .afterFirstUnlock:
        return .symbol(self.afterFirstUnlock)
      #if !targetEnvironment(macCatalyst)
      case .always:
        return .symbol(self.always)
      #endif
      case .whenPasscodeSetThisDeviceOnly:
        return .symbol(self.whenPasscodeSetThisDeviceOnly)
      case .whenUnlockedThisDeviceOnly:
        return .symbol(self.whenUnlockedThisDeviceOnly)
      case .afterFirstUnlockThisDeviceOnly:
        return .symbol(self.afterFirstUnlockThisDeviceOnly)
      #if !targetEnvironment(macCatalyst)
      case .alwaysThisDeviceOnly:
        return .symbol(self.alwaysThisDeviceOnly)
      #endif
    }
  }
  
  private func policy(from: Expr) throws -> AuthenticationPolicy {
    var res = AuthenticationPolicy()
    var expr = from
    while case .pair(let fst, let snd) = expr {
      switch try fst.asSymbol() {
        case self.userPresence:
          res.insert(.userPresence)
        case self.biometryAny:
          res.insert(.biometryAny)
        case self.biometryCurrentSet:
          res.insert(.biometryCurrentSet)
        case self.devicePasscode:
          res.insert(.devicePasscode)
        case self.watch:
          #if os(iOS) || os(watchOS) || os(tvOS)
          throw RuntimeError.eval(.illegalPolicySpecifier, fst)
          #elseif os(macOS)
          res.insert(.watch)
          #endif
        case self.or:
          res.insert(.or)
        case self.and:
          res.insert(.and)
        case self.privateKeyUsage:
          res.insert(.privateKeyUsage)
        case self.applicationPassword:
          res.insert(.applicationPassword)
        default:
          throw RuntimeError.eval(.illegalPolicySpecifier, fst)
      }
      expr = snd
    }
    guard case .null = expr else {
      throw RuntimeError.eval(.illegalPolicySpecifier, from)
    }
    return res
  }
  
  private func expr(from policy: AuthenticationPolicy) -> Expr {
    var res = Expr.null
    if policy.contains(.userPresence) {
      res = .pair(.symbol(self.userPresence), res)
    }
    if policy.contains(.biometryAny) {
      res = .pair(.symbol(self.biometryAny), res)
    }
    if policy.contains(.biometryCurrentSet) {
      res = .pair(.symbol(self.biometryCurrentSet), res)
    }
    if policy.contains(.devicePasscode) {
      res = .pair(.symbol(self.devicePasscode), res)
    }
    #if os(macOS)
    if policy.contains(.watch) {
      res = .pair(.symbol(self.watch), res)
    }
    #endif
    if policy.contains(.or) {
      res = .pair(.symbol(self.or), res)
    }
    if policy.contains(.and) {
      res = .pair(.symbol(self.and), res)
    }
    if policy.contains(.privateKeyUsage) {
      res = .pair(.symbol(self.privateKeyUsage), res)
    }
    if policy.contains(.applicationPassword) {
      res = .pair(.symbol(self.applicationPassword), res)
    }
    return res
  }
  
  private func isKeychain(_ expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is NativeKeychain else {
      return .false
    }
    return .true
  }
  
  private func makeKeychain(_ args: Arguments) throws -> Expr {
    guard let (service, group, acc, sync) = args.optional(.false, .false, .false, .null) else {
      throw RuntimeError.argumentCount(of: "make-keychain",
                                       min: 0,
                                       max: 4,
                                       args: .makeList(args))
    }
    var keychain: Keychain
    if service.isTrue && !service.isNull && group.isTrue && !group.isNull {
      keychain = Keychain(service: try service.asString(), accessGroup: try group.asString())
    } else if service.isTrue && !service.isNull {
      keychain = Keychain(service: try service.asString())
    } else if group.isTrue && !group.isNull {
      keychain = Keychain(accessGroup: try group.asString())
    } else {
      keychain = Keychain()
    }
    if acc.isTrue && !acc.isNull {
      switch acc {
        case .pair(let prompt, .null):
          if prompt.isTrue && !prompt.isNull {
            keychain = keychain.authenticationPrompt(try prompt.asString())
          }
        case .pair(let prompt, .pair(let ac, .null)):
          if prompt.isTrue && !prompt.isNull {
            keychain = keychain.authenticationPrompt(try prompt.asString())
          }
          if ac.isTrue && !ac.isNull {
            keychain = keychain.accessibility(try self.accessibility(from: ac))
          }
        case .pair(let prompt, .pair(let ac, let policy)):
          if prompt.isTrue && !prompt.isNull {
            keychain = keychain.authenticationPrompt(try prompt.asString())
          }
          keychain = keychain.accessibility(try self.accessibility(from: ac),
                                            authenticationPolicy: try self.policy(from: policy))
        default:
          keychain = keychain.accessibility(try self.accessibility(from: acc))
      }
    }
    if !sync.isNull {
      keychain = keychain.synchronizable(acc.isTrue)
    }
    return .object(NativeKeychain(keychain: keychain))
  }
  
  private func keychainService(_ expr: Expr) throws -> Expr {
    return .makeString(try self.keychain(from: expr).service)
  }
  
  private func keychainAccessGroup(_ expr: Expr) throws -> Expr {
    if let group = try self.keychain(from: expr).accessGroup {
      return .makeString(group)
    } else {
      return .false
    }
  }
  
  private func keychainAccessibility(_ expr: Expr) throws -> Expr {
    return self.expr(from: try self.keychain(from: expr).accessibility)
  }
  
  private func keychainSynchronized(_ expr: Expr) throws -> Expr {
    return .makeBoolean(try self.keychain(from: expr).synchronizable)
  }
  
  private func keychainExists(_ expr: Expr, _ key: Expr) throws -> Expr {
    return .makeBoolean(try self.keychain(from: expr).contains(key.asString()))
  }
  
  private func keychainRef(_ expr: Expr, _ key: Expr) throws -> Expr {
    let keychain = try self.keychain(from: expr)
    if let data = try keychain.getData(key.asString())?.gunzip() {
      return try Serialization(data: data).deserialize(in: self.context)
    } else {
      return .false
    }
  }
  
  private func keychainRefAttributes(_ expr: Expr, _ key: Expr) throws -> Expr {
    let keychain = try self.keychain(from: expr)
    return try keychain.get(key.asString(), ignoringAttributeSynchronizable: true) { attributes in
      guard let attributes else {
        return .false
      }
      var res = Expr.null
      if let str = attributes.accessGroup {
        res = .pair(.pair(.symbol(self.context.symbols.intern("access-group")), .makeString(str)), res)
      }
      if let str = attributes.service {
        res = .pair(.pair(.symbol(self.context.symbols.intern("service")), .makeString(str)), res)
      }
      if let date = attributes.creationDate {
        res = .pair(.pair(.symbol(self.context.symbols.intern("creation-date")),
                          .object(NativeDateTime(
                            DateTimeLibrary.calendar.dateComponents(in: TimeZone.current, from: date)))),
                    res)
      }
      if let date = attributes.modificationDate {
        res = .pair(.pair(.symbol(self.context.symbols.intern("modification-date")),
                          .object(NativeDateTime(
                            DateTimeLibrary.calendar.dateComponents(in: TimeZone.current, from: date)))),
                    res)
      }
      if let synchronizable = attributes.synchronizable {
        res = .pair(.pair(.symbol(self.context.symbols.intern("synchronizable")),
                          .makeBoolean(synchronizable)),
                    res)
      }
      if let accessible = attributes.accessible,
         let accessibility = Accessibility(rawValue: accessible) {
        res = .pair(.pair(.symbol(self.context.symbols.intern("accessibility")),
                          self.expr(from: accessibility)), res)
      }
      if let str = attributes.comment {
        res = .pair(.pair(.symbol(self.context.symbols.intern("comment")), .makeString(str)), res)
      }
      if let str = attributes.label {
        res = .pair(.pair(.symbol(self.context.symbols.intern("label")), .makeString(str)), res)
      }
      if let data = attributes.data {
        if let str = String(data: data, encoding: .utf8) {
          res = .pair(.pair(.symbol(self.context.symbols.intern("value")),
                            .makeString(str)), res)
        } else if let unzipped = data.gunzip() {
          do {
            res = .pair(.pair(.symbol(self.context.symbols.intern("value")),
                              try Serialization(data: unzipped).deserialize(in: self.context)), res)
          } catch {
            res = .pair(.pair(.symbol(self.context.symbols.intern("value")),
                              BytevectorLibrary.bytevector(from: data)), res)
          }
        } else {
          res = .pair(.pair(.symbol(self.context.symbols.intern("value")),
                            BytevectorLibrary.bytevector(from: data)), res)
        }
      }
      if let str = attributes.account {
        res = .pair(.pair(.symbol(self.context.symbols.intern("key")), .makeString(str)), res)
      }

      return res
    }
  }
  
  private func keychainRefData(_ expr: Expr, _ key: Expr) throws -> Expr {
    let keychain = try self.keychain(from: expr)
    if let data = try keychain.getData(key.asString()) {
      return BytevectorLibrary.bytevector(from: data)
    } else {
      return .false
    }
  }
  
  private func keychainRefString(_ expr: Expr, _ key: Expr) throws -> Expr {
    let keychain = try self.keychain(from: expr)
    if let str = try keychain.get(key.asString()) {
      return .makeString(str)
    } else {
      return .false
    }
  }
  
  private func keychainForSet(_ expr: Expr, _ args: Arguments) throws -> Keychain? {
    guard let (label, comment, acc, sync) = args.optional(.false, .false, .false, .null) else {
      return nil
    }
    var keychain = try self.keychain(from: expr)
    if label.isTrue && !label.isNull {
      keychain = keychain.label(try label.asString())
    }
    if comment.isTrue && !comment.isNull {
      keychain = keychain.comment(try comment.asString())
    }
    if acc.isTrue && !acc.isNull {
      switch acc {
        case .pair(let prompt, .null):
          if prompt.isTrue && !prompt.isNull {
            keychain = keychain.authenticationPrompt(try prompt.asString())
          }
        case .pair(let prompt, .pair(let ac, .null)):
          if prompt.isTrue && !prompt.isNull {
            keychain = keychain.authenticationPrompt(try prompt.asString())
          }
          if ac.isTrue && !ac.isNull {
            keychain = keychain.accessibility(try self.accessibility(from: ac))
          }
        case .pair(let prompt, .pair(let ac, let policy)):
          if prompt.isTrue && !prompt.isNull {
            keychain = keychain.authenticationPrompt(try prompt.asString())
          }
          keychain = keychain.accessibility(try self.accessibility(from: ac),
                                            authenticationPolicy: try self.policy(from: policy))
        default:
          keychain = keychain.accessibility(try self.accessibility(from: acc))
      }
    }
    if !sync.isNull {
      keychain = keychain.synchronizable(acc.isTrue)
    }
    return keychain
  }
  
  private func keychainSet(_ expr: Expr,
                           _ key: Expr,
                           _ value: Expr,
                           _ args: Arguments) throws -> Expr {
    guard let keychain = try self.keychainForSet(expr, args) else {
      throw RuntimeError.argumentCount(of: "keychain-set!",
                                       min: 3,
                                       max: 7,
                                       args: .pair(expr, .pair(key, .pair(value, .makeList(args)))))
    }
    if let data = try value.serialization().serialize().gzip() {
      keychain[data: try key.asString()] = data
    } else {
      throw RuntimeError.eval(.cannotSerialize, value)
    }
    return .void
  }
  
  private func keychainSetData(_ expr: Expr,
                               _ key: Expr,
                               _ value: Expr,
                               _ args: Arguments) throws -> Expr {
    guard let keychain = try self.keychainForSet(expr, args) else {
      throw RuntimeError.argumentCount(of: "keychain-set-data!",
                                       min: 3,
                                       max: 7,
                                       args: .pair(expr, .pair(key, .pair(value, .makeList(args)))))
    }
    keychain[data: try key.asString()] = Data(try value.asByteVector().value)
    return .void
  }
  
  private func keychainSetString(_ expr: Expr,
                                 _ key: Expr,
                                 _ value: Expr,
                                 _ args: Arguments) throws -> Expr {
    guard let keychain = try self.keychainForSet(expr, args) else {
      throw RuntimeError.argumentCount(of: "keychain-set-string!",
                                       min: 3,
                                       max: 7,
                                       args: .pair(expr, .pair(key, .pair(value, .makeList(args)))))
    }
    keychain[string: try key.asString()] = try value.asString()
    return .void
  }
  
  private func keychainRemove(_ expr: Expr, _ key: Expr) throws -> Expr {
    let keychain = try self.keychain(from: expr)
    try keychain.remove(try key.asString())
    return .void
  }
  
  private func keychainKeys(_ expr: Expr) throws -> Expr {
    return .makeList(Exprs(try self.keychain(from: expr).allKeys().map{.makeString($0)}))
  }
  
  private func availableKeychainServices() throws -> Expr {
    var set: Set<String> = []
    Keychain.allKeys(.genericPassword).forEach { key, value in
      set.insert(key)
    }
    var res = Expr.null
    for elem in set {
      res = .pair(.makeString(elem), res)
    }
    return res
  }
  
  private func availableKeychainKeys() throws -> Expr {
    return .makeList(Exprs(Keychain.allKeys(.genericPassword).map{ key, value in
      .pair(.makeString(key), .makeString(value))
    }))
  }
  
  private func makePassword() throws -> Expr {
    if let password = SecCreateSharedWebCredentialPassword() {
      return .makeString(password as String)
    } else {
      return .false
    }
  }
}

public final class NativeKeychain: NativeObject {

  /// Type representing zip archives
  public static let type = Type.objectType(Symbol(uninterned: "keychain"))
  
  /// Keychain accessor
  fileprivate var keychain: Keychain
  
  /// Initializer
  public init(keychain: Keychain) {
    self.keychain = keychain
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    return "#<\(self.tagString)>"
  }
  
  public override var tagString: String {
    if let accessGroup = self.keychain.accessGroup {
      return "\(Self.type) \(self.keychain.service) \(accessGroup)"
    } else if self.keychain.service.isEmpty {
      return "\(Self.type) \(self.identityString)"
    } else {
      return "\(Self.type) \(self.keychain.service)"
    }
  }
  
  public override func mark(in gc: GarbageCollector) {
  }
  
  public override func unpack(in context: Context) -> Exprs {
    return [.makeString(self.identityString),
            .makeString(self.keychain.service),
            self.keychain.accessGroup == nil ? .false : .makeString(self.keychain.accessGroup!)]
  }
}
