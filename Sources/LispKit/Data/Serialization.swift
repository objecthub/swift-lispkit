//
//  SerializedExpr.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/08/2024.
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
import NumberKit
import CBORCoding

///
/// `Serialization` structs are a container for structured serialized expressions
/// together with all the objects they refer to. They provide means to serialize
/// expressions, deserialize them, and to determine whether an expression can be
/// serialized.
///
public struct Serialization: Codable {
  let symbols: [SerializableSymbol]
  let strings: [String]
  let bytevectors: [[UInt8]]
  let vectors: [SerializableVector]
  let tables: [SerializableHashTable]
  let serialized: SerializableExpr
  
  /// Structured serialized expression
  public enum SerializableExpr: Codable {
    case undef
    case void
    case eof
    case null
    case `true`
    case `false`
    case uninit(Int)
    case symbol(Int)
    case fixnum(Int64)
    case bignum(BigInt)
    indirect case rational(SerializableExpr, SerializableExpr)
    case flonum(Double)
    case complex(Complex<Double>)
    case char(UniChar)
    case string(Int)
    case bytes(Int)
    case array(Int)
    case vector(Int)
    case table(Int)
    indirect case pair(SerializableExpr, SerializableExpr)
    indirect case values(SerializableExpr)
    indirect case tagged(SerializableExpr, SerializableExpr)
  }
  
  /// Structured serialized symbol
  public struct SerializableSymbol: Codable {
    public let identifier: String
    public let interned: Bool
  }

  /// Structured serialized vector
  public struct SerializableVector: Codable {
    public let exprs: [SerializableExpr]
    public let immutable: Bool
  }

  /// Structured serialized hashtable
  public struct SerializableHashTable: Codable {
    public enum Equivalence: Codable {
      case eq
      case eqv
      case equal
      
      public func deserialize() -> HashTable.Equivalence {
        switch self {
          case .eq:
            return .eq
          case .eqv:
            return .eqv
          case .equal:
            return .equal
        }
      }
    }
    
    public let buckets: [SerializableExpr]
    public let count: Int
    public let mutable: Bool
    public let equiv: Equivalence
  }
  
  /// Create `Serialization` by serializing expression
  init(for expr: Expr, unserializable: Expr? = nil) throws {
    let context = SerializationContext(unserializable: unserializable)
    self.serialized = try context.serialize(expr)
    self.symbols = context.symbolSequence
    self.strings = context.stringSequence
    self.bytevectors = context.bytevectorSequence
    self.vectors = context.vectorSequence
    self.tables = context.tableSequence
  }
  
  /// Create `Serialization` from a `Data` object
  public init(data: Data) throws {
    let decoder = CBORDecoder()
    self = try decoder.decode(Serialization.self, from: data)
  }
  
  /// Return a `Data` object from this serialization
  public func serialize() throws -> Data {
    let encoder = CBOREncoder()
    return try encoder.encode(self)
  }
  
  /// Can the given expression be serialized?
  public static func isSerializable(_ expr: Expr) -> Bool {
    let context = SerializationCheckContext()
    return context.isSerializable(expr)
  }
  
  /// Deserialize this serialization
  public func deserialize(in context: Context) -> Expr {
    let context = DeserializationContext(context: context, serialization: self)
    return context.deserialize()
  }
  
  /// Implements serialization process
  private final class SerializationContext {
    var unserializable: Expr?
    var symbols: [Symbol : (Int, SerializableSymbol)]
    var strings: [NSMutableString : (Int, String)]
    var bytevectors: [ByteVector : (Int, [UInt8])]
    var vectors: [Collection : (Int, SerializableVector)]
    var tables: [HashTable : (Int, SerializableHashTable)]
    
    init(unserializable: Expr?) {
      self.unserializable = unserializable
      self.symbols = [:]
      self.strings = [:]
      self.bytevectors = [:]
      self.vectors = [:]
      self.tables = [:]
    }
    
    var symbolSequence: [SerializableSymbol] {
      var res: [SerializableSymbol?] = Array(repeating: nil, count: self.symbols.count)
      for (i, vec) in self.symbols.values {
        res[i] = vec
      }
      return res.map { $0! }
    }
    
    var stringSequence: [String] {
      var res: [String?] = Array(repeating: nil, count: self.strings.count)
      for (i, vec) in self.strings.values {
        res[i] = vec
      }
      return res.map { $0! }
    }
    
    var bytevectorSequence: [[UInt8]] {
      var res: [[UInt8]?] = Array(repeating: nil, count: self.bytevectors.count)
      for (i, vec) in self.bytevectors.values {
        res[i] = vec
      }
      return res.map { $0! }
    }
    
    var vectorSequence: [SerializableVector] {
      var res: [SerializableVector?] = Array(repeating: nil, count: self.vectors.count)
      for (i, vec) in self.vectors.values {
        res[i] = vec
      }
      return res.map { $0! }
    }
    
    var tableSequence: [SerializableHashTable] {
      var res: [SerializableHashTable?] = Array(repeating: nil, count: self.tables.count)
      for (i, table) in self.tables.values {
        res[i] = table
      }
      return res.map { $0! }
    }
    
    private func serialize(_ sym: Symbol) throws -> Int {
      if let (i, _) = self.symbols[sym] {
        return i
      }
      let id = self.symbols.count
      self.symbols[sym] = (id, SerializableSymbol(identifier: sym.identifier,
                                                  interned: sym.isInterned))
      return id
    }
    
    private func serialize(_ str: NSMutableString) throws -> Int {
      if let (i, _) = self.strings[str] {
        return i
      }
      let id = self.strings.count
      self.strings[str] = (id, str as String)
      return id
    }
    
    private func serialize(_ bvec: ByteVector) throws -> Int {
      if let (i, _) = self.bytevectors[bvec] {
        return i
      }
      let id = self.bytevectors.count
      self.bytevectors[bvec] = (id, bvec.value)
      return id
    }
    
    private func serialize(_ coll: Collection) throws -> Int {
      if let (i, _) = self.vectors[coll] {
        return i
      }
      var immutable: Bool
      if case .immutableVector = coll.kind {
        immutable = true
      } else {
        immutable = false
      }
      let id = self.vectors.count
      self.vectors[coll] =
        (id, SerializableVector(exprs: [], immutable: immutable))
      self.vectors[coll] =
        (id, SerializableVector(exprs: Array(try coll.exprs.map{try self.serialize($0)}),
                                immutable: immutable))
      return id
    }
    
    private func serialize(_ table: HashTable) throws -> Int {
      if let (i, _) = self.tables[table] {
        return i
      }
      let equiv: SerializableHashTable.Equivalence
      switch table.equiv {
        case .eq:
          equiv = .eq
        case .eqv:
          equiv = .eqv
        case .equal:
          equiv = .equal
        default:
          throw RuntimeError.eval(.cannotSerialize, .table(table))
      }
      let id = self.tables.count
      self.tables[table] =
        (id, SerializableHashTable(buckets: [],
                                   count: table.count,
                                   mutable: table.mutable,
                                   equiv: equiv))
      self.tables[table] =
        (id, SerializableHashTable(buckets: Array(try table.buckets.map{try serialize($0)}),
                                   count: table.count,
                                   mutable: table.mutable,
                                   equiv: equiv))
      return id
    }
    
    func serialize(_ expr: Expr) throws -> SerializableExpr {
      switch expr {
        case .undef:
          return .undef
        case .void:
          return .void
        case .eof:
          return .eof
        case .null:
          return .null
        case .true:
          return .true
        case .false:
          return .false
        case .uninit(let sym):
          return .uninit(try self.serialize(sym))
        case .symbol(let sym):
          return .symbol(try self.serialize(sym))
        case .fixnum(let num):
          return .fixnum(num)
        case .bignum(let num):
          return .bignum(num)
        case .rational(let numer, let denom):
          return try .rational(serialize(numer), serialize(denom))
        case .flonum(let num):
          return .flonum(num)
        case .complex(let num):
          return .complex(num.value)
        case .char(let ch):
          return .char(ch)
        case .string(let str):
          return .string(try self.serialize(str))
        case .bytes(let bvec):
          return .bytes(try self.serialize(bvec))
        case .pair(let car, let cdr):
          return try .pair(serialize(car), serialize(cdr))
        case .array(let coll):
          guard case .array = coll.kind else {
            if let unserializable {
              return try self.serialize(unserializable)
            } else {
              throw RuntimeError.eval(.cannotSerialize, expr)
            }
          }
          return .array(try self.serialize(coll))
        case .vector(let coll):
          switch coll.kind {
            case .vector, .immutableVector:
              return .vector(try self.serialize(coll))
            default:
              if let unserializable {
                return try self.serialize(unserializable)
              } else {
                throw RuntimeError.eval(.cannotSerialize, expr)
              }
          }
        case .table(let table):
          switch table.equiv {
            case .eq, .eqv, .equal:
              return .table(try self.serialize(table))
            default:
              if let unserializable {
                return try self.serialize(unserializable)
              } else {
                throw RuntimeError.eval(.cannotSerialize, expr)
              }
          }
        case .values(let vals):
          return .values(try serialize(vals))
        case .tagged(let tag, let expr):
          return try .tagged(serialize(tag), serialize(expr))
        case .syntax(_, let expr):
          return try serialize(expr)
        default:
          if let unserializable {
            return try self.serialize(unserializable)
          } else {
            throw RuntimeError.eval(.cannotSerialize, expr)
          }
      }
    }
  }
  
  /// Implements process to determine whether an expression can be serialized
  private final class SerializationCheckContext {
    var vectors: Set<Collection> = []
    var tables: Set<HashTable> = []
    
    func isSerializable(_ expr: Expr) -> Bool {
      switch expr {
        case .undef, .void, .eof, .null, .true, .false, .uninit(_), .symbol(_), .fixnum(_),
            .bignum(_), .flonum(_), .complex(_), .char(_), .string(_), .bytes(_):
          return true
        case .rational(let numer, let denom):
          return numer.isSerializable && denom.isSerializable
        case .pair(let car, let cdr):
          return car.isSerializable && cdr.isSerializable
        case .array(let coll):
          if self.vectors.contains(coll) {
            return true
          }
          guard case .array = coll.kind else {
            return false
          }
          self.vectors.insert(coll)
          for expr in coll.exprs {
            if !expr.isSerializable {
              return false
            }
          }
          return true
        case .vector(let coll):
          if self.vectors.contains(coll) {
            return true
          }
          switch coll.kind {
            case .vector, .immutableVector:
              self.vectors.insert(coll)
              for expr in coll.exprs {
                if !expr.isSerializable {
                  return false
                }
              }
              return true
            default:
              return false
          }
        case .table(let table):
          if self.tables.contains(table) {
            return true
          }
          switch table.equiv {
            case .eq, .eqv, .equal:
              break
            default:
              return false
          }
          self.tables.insert(table)
          for expr in table.buckets {
            if !expr.isSerializable {
              return false
            }
          }
          return true
        case .values(let vals):
          return vals.isSerializable
        case .tagged(let tag, let expr):
          return tag.isSerializable && expr.isSerializable
        case .syntax(_, let expr):
          return expr.isSerializable
        default:
          return false
      }
    }
  }
  
  /// Implements the deserialization process
  final class DeserializationContext {
    let context: Context
    let serialization: Serialization
    var symbols: [Symbol?]
    var strings: [NSMutableString?]
    var bytevectors: [ByteVector?]
    var vectors: [Collection?]
    var tables: [HashTable?]
    
    init(context: Context, serialization: Serialization) {
      self.context = context
      self.serialization = serialization
      self.symbols = Array(repeating: nil, count: serialization.symbols.count)
      self.strings = Array(repeating: nil, count: serialization.strings.count)
      self.bytevectors = Array(repeating: nil, count: serialization.bytevectors.count)
      self.vectors = Array(repeating: nil, count: serialization.vectors.count)
      self.tables = Array(repeating: nil, count: serialization.tables.count)
    }
    
    func deserialize() -> Expr {
      return self.deserialize(self.serialization.serialized)
    }
    
    private func deserialize(symbol i: Int) -> Symbol {
      if let sym = self.symbols[i] {
        return sym
      }
      let symbol = self.serialization.symbols[i]
      let sym = symbol.interned ? self.context.symbols.intern(symbol.identifier)
                                : Symbol(uninterned: symbol.identifier)
      self.symbols[i] = sym
      return sym
    }
    
    private func deserialize(string i: Int) -> NSMutableString {
      if let str = self.strings[i] {
        return str
      }
      let str = NSMutableString(string: self.serialization.strings[i])
      self.strings[i] = str
      return str
    }
    
    private func deserialize(bytevector i: Int) -> ByteVector {
      if let bvec = self.bytevectors[i] {
        return bvec
      }
      let bvec = ByteVector(self.serialization.bytevectors[i])
      self.bytevectors[i] = bvec
      return bvec
    }
    
    private func deserialize(vector i: Int) -> Collection {
      if let coll = self.vectors[i] {
        return coll
      }
      let vector = self.serialization.vectors[i]
      if vector.immutable {
        let coll = Collection(kind: .immutableVector, exprs: [])
        self.vectors[i] = coll
        coll.exprs = Exprs(vector.exprs.map{self.deserialize($0)})
        return coll
      } else {
        let coll = Collection(kind: .vector, exprs: [])
        self.vectors[i] = coll
        coll.exprs = Exprs(vector.exprs.map{self.deserialize($0)})
        return coll
      }
    }
    
    private func deserialize(array i: Int) -> Collection {
      if let coll = self.vectors[i] {
        return coll
      }
      let vector = self.serialization.vectors[i]
      let coll = Collection(kind: .array, exprs: [])
      self.vectors[i] = coll
      coll.exprs = Exprs(vector.exprs.map{self.deserialize($0)})
      return coll
    }
    
    private func deserialize(table i: Int) -> HashTable {
      if let ht = self.tables[i] {
        return ht
      }
      let table = self.serialization.tables[i]
      let ht = HashTable(buckets: [],
                         count: table.count,
                         mutable: table.mutable,
                         equiv: table.equiv.deserialize())
      self.tables[i] = ht
      ht.buckets = Exprs(table.buckets.map{self.deserialize($0)})
      return ht
    }
    
    private func deserialize(_ expr: SerializableExpr) -> Expr {
      switch expr {
        case .undef:
          return .undef
        case .void:
          return .void
        case .eof:
          return .eof
        case .null:
          return .null
        case .true:
          return .true
        case .false:
          return .false
        case .uninit(let i):
          return .uninit(self.deserialize(symbol: i))
        case .symbol(let i):
          return .symbol(self.deserialize(symbol: i))
        case .fixnum(let num):
          return .fixnum(num)
        case .bignum(let num):
          return .bignum(num)
        case .rational(let numer, let denom):
          return .rational(deserialize(numer), deserialize(denom))
        case .flonum(let num):
          return .flonum(num)
        case .complex(let num):
          return .complex(DoubleComplex(num))
        case .char(let ch):
          return .char(ch)
        case .string(let i):
          return .string(self.deserialize(string: i))
        case .bytes(let i):
          return .bytes(self.deserialize(bytevector: i))
        case .array(let i):
          return .array(self.deserialize(array: i))
        case .vector(let i):
          return .vector(self.deserialize(vector: i))
        case .table(let i):
          return .table(self.deserialize(table: i))
        case .pair(let car, let cdr):
          return .pair(deserialize(car), deserialize(cdr))
        case .values(let exprs):
          return .values(deserialize(exprs))
        case .tagged(let tag, let expr):
          return .tagged(deserialize(tag), deserialize(expr))
      }
    }
  }
}
