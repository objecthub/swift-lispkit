//
//  CharSet.swift
//  LispKit
//
//  Created by Matthias Zenger on 02/01/2019.
//  Copyright © 2019 ObjectHub. All rights reserved.
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

///
/// Simple character set implementation assuming characters are UTF16 code units.
/// Immutability of character sets is implemented at the Scheme library level.
///
public final class CharSet: NativeObject {

  /// Type representing character sets
  public static let type = Type.objectType(Symbol(uninterned: "char-set"))

  /// Native character set
  public private(set) var chars: Set<UniChar>

  /// Is this character set immutable? Immutability is not enforced via this class.
  public let immutable: Bool

  /// Return native object type.
  public override var type: Type {
    return Self.type
  }

  public override init() {
    self.chars = []
    self.immutable = false
  }

  public init(immutable: Bool) {
    self.chars = []
    self.immutable = immutable
  }

  public init(copy cs: CharSet, immutable: Bool = false) {
    self.chars = cs.chars
    self.immutable = immutable
  }

  public convenience init(charsIn str: NSString, immutable: Bool = false) {
    self.init(immutable: immutable)
    self.chars.reserveCapacity(str.length)
    for i in 0..<str.length {
      self.chars.insert(str.character(at: i))
    }
  }

  public convenience init(charsIn str: String, immutable: Bool = false) {
    self.init(immutable: immutable)
    for ch in str.utf16 {
      self.chars.insert(ch)
    }
  }
  
  public override func unpack(in context: Context) -> Exprs {
    var res: Exprs = []
    res.reserveCapacity(self.chars.count)
    for ch in self.chars {
      res.append(.char(ch))
    }
    return [.makeString(self.identityString),
            .vector(Collection(kind: .immutableVector, exprs: res))]
  }
  
  public func contains(_ ch: UniChar) -> Bool {
    return self.chars.contains(ch)
  }

  public func insert(_ ch: UniChar) {
    self.chars.insert(ch)
  }

  public func insert(charsIn str: NSString) {
    for i in 0..<str.length {
      self.chars.insert(str.character(at: i))
    }
  }

  public func insert(charsIn str: String) {
    for ch in str.utf16 {
      self.chars.insert(ch)
    }
  }

  public func remove(_ ch: UniChar) {
    self.chars.remove(ch)
  }

  public func remove(charsIn str: NSString) {
    for i in 0..<str.length {
      self.chars.remove(str.character(at: i))
    }
  }

  public func remove(charsIn str: String) {
    for ch in str.utf16 {
      self.chars.remove(ch)
    }
  }

  public func formUnion(_ cs: CharSet) {
    self.chars.formUnion(cs.chars)
  }

  public func formIntersection(_ cs: CharSet) {
    self.chars.formIntersection(cs.chars)
  }

  public func formSymmetricDifference(_ cs: CharSet) {
    self.chars.formSymmetricDifference(cs.chars)
  }

  public func subtract(_ cs: CharSet) {
    self.chars.subtract(cs.chars)
  }

  public var inverted: CharSet {
    let res = CharSet()
    for ch in 0...UInt16.max {
      if !self.chars.contains(ch) {
        res.chars.insert(ch)
      }
    }
    return res
  }

  public func invert() {
    for ch in 0...UInt16.max {
      if self.chars.contains(ch) {
        self.chars.remove(ch)
      } else {
        self.chars.insert(ch)
      }
    }
  }

  public var isEmpty: Bool {
    return self.chars.isEmpty
  }

  public var count: Int {
    return self.chars.count
  }

  public func forEach(f: (UniChar) -> Void) {
    for ch in self.chars {
      f(ch)
    }
  }

  public var array: [UniChar] {
    var res: [UniChar] = []
    res.reserveCapacity(self.chars.count)
    for ch in self.chars {
      res.append(ch)
    }
    return res
  }

  public var first: UniChar? {
    for i in 0...UInt16.max {
      if self.chars.contains(i) {
        return i
      }
    }
    return nil
  }

  public func next(_ ch: UniChar) -> UniChar? {
    if ch == UInt16.max {
      return nil
    }
    for i in (ch + 1)...UInt16.max {
      if self.chars.contains(i) {
        return i
      }
    }
    return nil
  }

  public var charSetHashValue: Int {
    return self.chars.hashValue
  }

  public func isEqual(to cs: CharSet) -> Bool {
    return self.chars == cs.chars
  }

  public func isSubset(of cs: CharSet) -> Bool {
    return self.chars.isSubset(of: cs.chars)
  }

  public func isDisjoint(with cs: CharSet) -> Bool {
    return self.chars.isDisjoint(with: cs.chars)
  }

  // Definiton of default character sets

  public class func lowercaseLetters() -> CharSet {
    return CharSet.convert(cs: .lowercaseLetters)
  }

  public class func uppercaseLetters() -> CharSet {
    return CharSet.convert(cs: .uppercaseLetters)
  }

  public class func titlecaseLetters() -> CharSet {
    return CharSet.convert(cs: .capitalizedLetters)
  }

  public class func letters() -> CharSet {
    return CharSet.convert(cs: .letters)
  }

  public class func digits() -> CharSet {
    return CharSet.convert(cs: .decimalDigits)
  }

  public class func lettersAndDigits() -> CharSet {
    return CharSet.convert(cs: .alphanumerics)
  }

  public class func graphics() -> CharSet {
    return CharSet.convert(cs: .symbols,
                           target: CharSet.convert(cs: .punctuationCharacters,
                                                   target: CharSet.convert(cs: .alphanumerics)))
  }

  public class func printing() -> CharSet {
    return CharSet.convert(cs: .whitespacesAndNewlines, target: CharSet.graphics())
  }

  public class func whitespaces() -> CharSet {
    return CharSet.convert(cs: .whitespacesAndNewlines)
  }

  public class func newlines() -> CharSet {
    return CharSet.convert(cs: .newlines)
  }

  public class func blanks() -> CharSet {
    return CharSet.convert(cs: .whitespaces)
  }

  public class func controls() -> CharSet {
    return CharSet.convert(cs: .controlCharacters)
  }

  public class func punctuations() -> CharSet {
    return CharSet.convert(cs: .punctuationCharacters)
  }

  public class func symbols() -> CharSet {
    return CharSet.convert(cs: .symbols)
  }

  public class func hexdigits() -> CharSet {
    return CharSet(charsIn: "0123456789ABCDEFabcdef", immutable: true)
  }

  public class func ascii() -> CharSet {
    let cs = CharSet(immutable: true)
    for ch: UInt16 in 0..<128 {
      cs.insert(ch)
    }
    return cs
  }

  public class func full() -> CharSet {
    let cs = CharSet(immutable: true)
    cs.invert()
    return cs
  }

  class func convert(cs: CharacterSet, target: CharSet? = nil) -> CharSet {
    return Self.convert(nscs: cs as NSCharacterSet, target: target)
  }
  
  class func convert(nscs: NSCharacterSet, target: CharSet? = nil) -> CharSet {
    let res = target == nil ? CharSet(immutable: true) : target!
    for ch in 0...UInt16.max {
      if nscs.characterIsMember(ch) {
        res.insert(ch)
      }
    }
    return res
  }
}
