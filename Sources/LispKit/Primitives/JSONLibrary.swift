//
//  JSONLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 12/05/2024.
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
import DynamicJSON

public final class JSONLibrary: NativeLibrary {
  
  private static let floatDecodingStrategy: JSONDecoder.NonConformingFloatDecodingStrategy
                       = .convertFromString(positiveInfinity: "Infinity",
                                            negativeInfinity: "-Infinity",
                                            nan: "NaN")
  
  private static let floatEncodingStrategy: JSONEncoder.NonConformingFloatEncodingStrategy
                       = .convertToString(positiveInfinity: "Infinity",
                                          negativeInfinity: "-Infinity",
                                          nan: "NaN")
  
  // Symbols
  // private let null: Symbol
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    // self.null = context.symbols.intern("null")
    try super.init(in: context)
  }
  
    /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "json"]
  }
  
    /// Dependencies of the library.
  public override func dependencies() {
    /*
     self.`import`(from: ["lispkit", "core"], "define", "define-syntax", "syntax-rules", "lambda",
     "apply", "values")
     self.`import`(from: ["lispkit", "control"], "begin", "let")
     self.`import`(from: ["lispkit", "dynamic"], "dynamic-wind")
     self.`import`(from: ["lispkit", "system"], "current-second")
     self.`import`(from: ["lispkit", "math"], "+", "-")
     self.`import`(from: ["lispkit", "list"], "cons", "map")
     */
  }
  
    /// Declarations of the library.
  public override func declarations() {
    self.define("json-type-tag", as: JSON.type.objectTypeTag())
    self.define("mutable-json-type-tag", as: MutableJSON.type.objectTypeTag())
    
    // JSON references
    self.define(Procedure("json-location?", self.isJsonLocation))
    self.define(Procedure("json-pointer?", self.isJsonPointer))
    self.define(Procedure("json-reference?", self.isJsonReference))
    self.define(Procedure("json-location", self.jsonLocation))
    self.define(Procedure("json-pointer", self.jsonPointer))
    
    // JSON values
    self.define(Procedure("json?", self.isJson))
    self.define(Procedure("json-null?", self.isJsonNull))
    self.define(Procedure("json-boolean?", self.isJsonBoolean))
    self.define(Procedure("json-number?", self.isJsonNumber))
    self.define(Procedure("json-string?", self.isJsonString))
    self.define(Procedure("json-array?", self.isJsonArray))
    self.define(Procedure("json-object?", self.isJsonObject))
    self.define(Procedure("json-refinement?", self.isJsonRefinement))
    self.define(Procedure("json", self.json))
    self.define(Procedure("string->json", self.stringToJson))
    self.define(Procedure("bytevector->json", self.bytevectorToJson))
    self.define(Procedure("load-json", self.loadJson))
    self.define(Procedure("json-children", self.jsonChildren))
    self.define(Procedure("json-ref", self.jsonRef))
    self.define(Procedure("json-select", self.jsonSelect))
    self.define(Procedure("json-update", self.jsonUpdate))
    self.define(Procedure("json-update-all", self.jsonUpdateAll))
    self.define(Procedure("json->value", self.jsonToValue))
    self.define(Procedure("json->string", self.jsonToString))
    self.define(Procedure("json->bytevector", self.jsonToBytevector))
    
    // Mutable JSON values
    self.define(Procedure("mutable-json", self.mutableJson))
    self.define(Procedure("json-set!", self.jsonSet))
    self.define(Procedure("json-apply!", self.jsonApply))
    
    // Merging JSON values
    self.define(Procedure("json-merge", self.jsonMerge))
    self.define(Procedure("json-merge-patch", self.jsonMergePatch))
    self.define(Procedure("json-override", self.jsonOverride))
    
    // JSON queries
    self.define(Procedure("json-path?", self.isJsonPath))
    self.define(Procedure("json-query", self.jsonQuery))
    self.define(Procedure("json-query-results", self.jsonQueryResults))
    self.define(Procedure("json-query-locations", self.jsonQueryLocations))
  }
  
  private func jsonOpt(from expr: Expr) -> JSON? {
    guard case .object(let obj) = expr else {
      return nil
    }
    if let json = obj as? JSON {
      return json
    } else if let mutable = obj as? MutableJSON {
      return mutable.value
    } else {
      return nil
    }
  }
  
  private func json(from expr: Expr) throws -> JSON {
    guard let json = self.jsonOpt(from: expr) else {
      throw RuntimeError.type(expr, expected: [JSON.type])
    }
    return json
  }
  
  private func mutableJson(from expr: Expr) throws -> MutableJSON {
    guard case .object(let obj) = expr,
          let mutable = obj as? MutableJSON else {
      throw RuntimeError.type(expr, expected: [MutableJSON.type])
    }
    return mutable
  }
  
  private func patch(from expr: Expr) throws -> NativeJSONPatch {
    guard case .object(let obj) = expr,
          let patch = obj as? NativeJSONPatch else {
      throw RuntimeError.type(expr, expected: [NativeJSONPatch.type])
    }
    return patch
  }
  
  private func ref(from: Expr) throws -> JSONReference {
    switch from {
      case .null:
        return JSONLocation.root
      case .fixnum(let num):
        if let index = Int(exactly: num) {
          return JSONLocation.index(.root, index)
        } else {
          throw RuntimeError.eval(.jsonReferenceExpected, from)
        }
      case .string(let str):
        return try JSON.reference(from: str as String)
      case .pair(_, _):
        var lst = from
        var location = JSONLocation.root
        while case .pair(let segment, let rest) = lst {
          switch segment {
            case .string(let str):
              location = location.select(member: str as String)
            case .fixnum(let num):
              if let index = Int(exactly: num) {
                location = location.select(index: index)
              } else {
                throw RuntimeError.eval(.jsonReferenceExpected, from)
              }
            default:
              throw RuntimeError.eval(.jsonReferenceExpected, from)
          }
          lst = rest
        }
        guard case .null = lst else {
          throw RuntimeError.eval(.jsonReferenceExpected, from)
        }
        return location
      default:
        throw RuntimeError.eval(.jsonReferenceExpected, from)
    }
  }
  
  // JSON references
  
  private func isJsonPointer(expr: Expr, strict: Expr?) throws -> Expr {
    do {
      _ = try JSONPointer(try expr.asString(), strict: strict?.isTrue ?? false)
      return .true
    } catch {
      return .false
    }
  }
  
  private func isJsonLocation(expr: Expr) throws -> Expr {
    do {
      _ = try JSONLocation(try expr.asString())
      return .true
    } catch {
      return .false
    }
  }
  
  private func isJsonReference(expr: Expr) throws -> Expr {
    switch expr {
      case .null:
        return .true
      case .fixnum(let num):
        if Int(exactly: num) == nil {
          return .false
        }
        return .true
      case .string(let str):
        do {
          _ = try JSON.reference(from: str as String)
          return .true
        } catch {
          return .false
        }
      case .pair(_, _):
        var lst = expr
        while case .pair(let car, let cdr) = lst {
          switch car {
            case .fixnum(let num):
              if Int(exactly: num) == nil {
                return .false
              }
            case .string(_):
              break
            default:
              return .false
          }
          lst = cdr
        }
        return .makeBoolean(lst == .null)
      default:
        return .false
    }
  }
  
  private func jsonLocation(expr: Expr) throws -> Expr {
    switch expr {
      case .null:
        return .makeString(JSONLocation.root.description)
      case .fixnum(let num):
        guard let index = Int(exactly: num) else {
          throw RuntimeError.eval(.jsonReferenceExpected, expr)
        }
        return .makeString(JSONLocation.index(.root, index).description)
      case .string(let str):
        let reference = try JSON.reference(from: str as String)
        if let pointer = reference as? JSONPointer {
          let locations = pointer.locations()
          if locations.count == 1 {
            return .makeString(locations[0].description)
          }
          throw RuntimeError.eval(.jsonReferenceExpected, expr)
        } else if let location = reference as? JSONLocation {
          return .makeString(location.description)
        } else {
          throw RuntimeError.eval(.jsonReferenceExpected, expr)
        }
      case .pair(_, _):
        var lst = expr
        var location = JSONLocation.root
        while case .pair(let car, let cdr) = lst {
          switch car {
            case .fixnum(let num):
              guard let index = Int(exactly: num) else {
                throw RuntimeError.eval(.jsonReferenceExpected, car)
              }
              location = location.select(index: index)
            case .string(let member):
              location = location.select(member: member as String)
            default:
              throw RuntimeError.eval(.jsonReferenceExpected, car)
          }
          lst = cdr
        }
        return .makeString(location.description)
      default:
        throw RuntimeError.eval(.jsonReferenceExpected, expr)
    }
  }
  
  private func jsonPointer(expr: Expr) throws -> Expr {
    var components: [String] = []
    switch expr {
      case .null:
        break
      case .fixnum(let num):
        components.append("\(num)")
      case .string(let str):
        let reference = try JSON.reference(from: str as String)
        if let pointer = reference as? JSONPointer {
          return .makeString(pointer.description)
        } else if let location = reference as? JSONLocation,
                  let pointer = location.pointer {
          return .makeString(pointer.description)
        } else {
          throw RuntimeError.eval(.jsonReferenceExpected, expr)
        }
      case .pair(_, _):
        var lst = expr
        while case .pair(let car, let cdr) = lst {
          switch car {
            case .fixnum(let num):
              components.append("\(num)")
            case .string(let member):
              components.append(member as String)
            default:
              throw RuntimeError.eval(.jsonReferenceExpected, car)
          }
          lst = cdr
        }
      default:
        throw RuntimeError.eval(.jsonReferenceExpected, expr)
    }
    return .makeString(JSONPointer(components: components).description)
  }
  
  // JSON values
  
  private func isJson(expr: Expr, strict: Expr?) throws -> Expr {
    guard case .object(let obj) = expr else {
      return .false
    }
    if strict?.isTrue ?? false {
      return .makeBoolean(obj is JSON)
    } else {
      return .makeBoolean(obj is JSON || obj is MutableJSON)
    }
  }
  
  private func isJsonNull(expr: Expr) throws -> Expr {
    guard let json = self.jsonOpt(from: expr),
          case .null = json else {
      return .false
    }
    return .true
  }
  
  private func isJsonBoolean(expr: Expr) throws -> Expr {
    guard let json = self.jsonOpt(from: expr),
          case .boolean(_) = json else {
      return .false
    }
    return .true
  }
  
  private func isJsonNumber(expr: Expr) throws -> Expr {
    guard let json = self.jsonOpt(from: expr) else {
      return .false
    }
    switch json {
      case .integer(_), .float(_):
        return .true
      default:
        return .false
    }
  }
  
  private func isJsonString(expr: Expr) throws -> Expr {
    guard let json = self.jsonOpt(from: expr),
          case .string(_) = json else {
      return .false
    }
    return .true
  }
  
  private func isJsonArray(expr: Expr) throws -> Expr {
    guard let json = self.jsonOpt(from: expr),
          case .array(_) = json else {
      return .false
    }
    return .true
  }
  
  private func isJsonObject(expr: Expr) throws -> Expr {
    guard let json = self.jsonOpt(from: expr),
          case .object(_) = json else {
      return .false
    }
    return .true
  }
  
  private func isJsonRefinement(expr: Expr, other: Expr) throws -> Expr {
    return .makeBoolean(try self.json(from: expr).isRefinement(of: self.json(from: other)))
  }
  
  private func toJSON(_ expr: Expr) throws -> JSON {
    switch expr {
      case .symbol(self.context.symbols.null):
        return .null
      case .null:
        return .array([])
      case .false:
        return .boolean(false)
      case .true:
        return .boolean(true)
      case .fixnum(let num):
        return .integer(num)
      case .flonum(let num):
        return .float(num)
      case .string(let str):
        return .string(str as String)
      case .vector(let col):
        switch col.kind {
          case .array, .vector, .immutableVector, .growableVector:
            var arr: [JSON] = []
            for expr in col.exprs {
              arr.append(try self.toJSON(expr))
            }
            return .array(arr)
          default:
            throw RuntimeError.eval(.cannotConvertToJSON, expr)
        }
      case .record(let record):
        guard case .record(let type) = record.kind,
              case .recordType = type.kind else {
          throw RuntimeError.eval(.cannotConvertToJSON, expr)
        }
        let syms = Collection.RecordType.fields(type)
        var dict: [String : JSON] = [:]
        for sym in syms {
          guard let index = RecordLibrary.indexOfField(sym, in: type),
                index >= 0 && index < record.exprs.count else {
            throw RuntimeError.eval(.unknownFieldOfRecordType, expr, name)
          }
          dict[sym.identifier] = try self.toJSON(record.exprs[index])
        }
        return .object(dict)
      case .pair(_, _):
        var rs = expr
        var dict: [String : JSON] = [:]
        while case .pair(.pair(let key, let value), let rest) = rs {
          switch key {
            case .symbol(let member):
              dict[member.identifier] = try self.toJSON(value)
            case .string(let member):
              dict[member as String] = try self.toJSON(value)
            default:
              throw RuntimeError.eval(.cannotConvertToJSON, key)
          }
          rs = rest
        }
        guard case .null = rs else {
          throw RuntimeError.eval(.cannotConvertToJSON, expr)
        }
        return .object(dict)
      case .table(let table):
        var dict: [String : JSON] = [:]
        for (key, value) in table.entries {
          switch key {
            case .symbol(let member):
              dict[member.identifier] = try self.toJSON(value)
            case .string(let member):
              dict[member as String] = try self.toJSON(value)
            default:
              throw RuntimeError.eval(.cannotConvertToJSON, key)
          }
        }
        return .object(dict)
      case .object(let obj):
        if let json = obj as? JSON {
          return json
        } else if let mutable = obj as? MutableJSON {
          return mutable.value
        } else {
          throw RuntimeError.eval(.cannotConvertToJSON, expr)
        }
      default:
        throw RuntimeError.eval(.cannotConvertToJSON, expr)
    }
  }
  
  private func json(expr: Expr) throws -> Expr {
    return .object(try self.toJSON(expr))
  }
  
  private func stringToJson(expr: Expr) throws -> Expr {
    return .object(try JSON(string: expr.asString(),
                            dateDecodingStrategy: .iso8601,
                            floatDecodingStrategy: Self.floatDecodingStrategy))
  }
  
  private func bytevectorToJson(expr: Expr, args: Arguments) throws -> Expr {
    let subvec = try BytevectorLibrary.subVector("bytevector->json", expr, args)
    return .object(try JSON(data: Data(subvec),
                            dateDecodingStrategy: .iso8601,
                            floatDecodingStrategy: Self.floatDecodingStrategy))
  }
  
  private func loadJson(filename: Expr) throws -> Expr {
    let path = self.context.fileHandler.path(try filename.asPath(),
                                             relativeTo: self.context.evaluator.currentDirectoryPath)
    return .object(try JSON(url: URL(fileURLWithPath: path),
                            dateDecodingStrategy: .iso8601,
                            floatDecodingStrategy: Self.floatDecodingStrategy))
  }
  
  private func jsonChildren(expr: Expr) throws -> Expr {
    let children = try self.json(from: expr).children
    var res: Expr = .null
    for child in children.reversed() {
      res = .pair(.object(child), res)
    }
    return res
  }
  
  private func jsonRef(expr: Expr, ref: Expr) throws -> Expr {
    let json = try self.json(from: expr)
    switch ref {
      case .null:
        return .object(json)
      case .string(let str):
        if let res = try json[ref: str as String] {
          return .object(res)
        } else {
          return .false
        }
      case .pair(_, _):
        var lst = ref
        var location = JSONLocation.root
        while case .pair(let segment, let rest) = lst {
          switch segment {
            case .string(let str):
              location = location.select(member: str as String)
            case .fixnum(let num):
              if let index = Int(exactly: num) {
                location = location.select(index: index)
              } else {
                return .false
              }
            default:
              return .false
          }
          lst = rest
        }
        if let res = json[ref: location] {
          return .object(res)
        } else {
          return .false
        }
      default:
        return .false
    }
  }
  
  private func jsonSelect(expr: Expr, args: Arguments) throws -> Expr {
    let json = try self.json(from: expr)
    var location = JSONLocation.root
    for arg in args {
      switch arg {
        case .fixnum(let num):
          if let index = Int(exactly: num) {
            location = location.select(index: index)
          } else {
            return .false
          }
        case .string(let member):
          location = location.select(member: member as String)
        default:
          return .false
      }
    }
    if let res = json[ref: location] {
      return .object(res)
    } else {
      return .false
    }
  }
  
  private func jsonUpdate(expr: Expr, args: Arguments) throws -> Expr {
    var json = try self.json(from: expr)
    var iter = args.makeIterator()
    while let reference = iter.next() {
      let ref = try self.ref(from: reference)
      if let expr = iter.next() {
        try json.update(ref, with: try self.json(from: expr))
      } else {
        break
      }
    }
    return .object(json)
  }
  
  private func jsonUpdateAll(expr: Expr, updates: Expr) throws -> Expr {
    var json = try self.json(from: expr)
    var lst = updates
    while case .pair(let car, let cdr) = lst {
      guard case .pair(let reference, let expr) = car else {
        throw RuntimeError.type(car, expected: [.pairType])
      }
      try json.update(try self.ref(from: reference), with: try self.json(from: expr))
      lst = cdr
    }
    guard case .null = lst else {
      throw RuntimeError.type(updates, expected: [.properListType])
    }
    return .object(json)
  }
  
  private func jsonToValue(expr: Expr) throws -> Expr {
    return try self.json(from: expr).toExpr(in: self.context)
  }
  
  private func jsonToString(expr: Expr, args: Arguments) throws -> Expr {
    guard let (pretty, sort, slashesc) = args.optional(.false, .false, .false) else {
      throw RuntimeError.argumentCount(of: "json->string", min: 1, max: 4,
                                       args: .pair(expr, .makeList(args)))
    }
    let json = try self.json(from: expr)
    var options: JSONEncoder.OutputFormatting = []
    if pretty.isTrue {
      options.insert(.prettyPrinted)
    }
    if sort.isTrue {
      options.insert(.sortedKeys)
    }
    if slashesc.isFalse {
      options.insert(.withoutEscapingSlashes)
    }
    if let str = try json.string(formatting: options,
                                 dateEncodingStrategy: .iso8601,
                                 floatEncodingStrategy: Self.floatEncodingStrategy) {
      return .makeString(str)
    } else {
      return .false
    }
  }
  
  private func jsonToBytevector(expr: Expr, args: Arguments) throws -> Expr {
    guard let (pretty, sort, slashesc) = args.optional(.false, .false, .false) else {
      throw RuntimeError.argumentCount(of: "json->bytevector", min: 1, max: 4,
                                       args: .pair(expr, .makeList(args)))
    }
    let json = try self.json(from: expr)
    var options: JSONEncoder.OutputFormatting = []
    if pretty.isTrue {
      options.insert(.prettyPrinted)
    }
    if sort.isTrue {
      options.insert(.sortedKeys)
    }
    if slashesc.isFalse {
      options.insert(.withoutEscapingSlashes)
    }
    let data = try json.data(formatting: options,
                             dateEncodingStrategy: .iso8601,
                             floatEncodingStrategy: Self.floatEncodingStrategy)
    let count = data.count
    var res = [UInt8](repeating: 0, count: count)
    data.copyBytes(to: &res, count: count)
    return .bytes(MutableBox(res))
  }
  
  // Mutable JSON values
  
  private func mutableJson(expr: Expr, forceNew: Expr?) throws -> Expr {
    if forceNew?.isTrue ?? true {
      return .object(MutableJSON(try self.toJSON(expr)))
    } else if case .object(let obj) = expr, obj is MutableJSON {
      return expr
    } else {
      return .object(MutableJSON(try self.toJSON(expr)))
    }
  }
  
  private func jsonSet(expr: Expr, reference: Expr, value: Expr) throws -> Expr {
    let mutable = try self.mutableJson(from: expr)
    let ref = try self.ref(from: reference)
    try mutable.value.update(ref, with: try self.json(from: value))
    return .void
  }
  
  private func jsonApply(expr: Expr, patch: Expr) throws -> Expr {
    let mutable = try self.mutableJson(from: expr)
    let patch = try self.patch(from: patch)
    try mutable.value.apply(patch: JSONPatch(operations: patch.value))
    return .void
  }
  
  // Merging JSON values
  
  private func jsonMerge(expr: Expr, other: Expr) throws -> Expr {
    let json = try self.json(from: expr)
    if let result = json.merging(value: try self.json(from: other)) {
      return .object(result)
    } else {
      return .false
    }
  }
  
  private func jsonMergePatch(expr: Expr, other: Expr) throws -> Expr {
    let json = try self.json(from: expr)
    return .object(json.merging(patch: try self.json(from: other)))
  }
  
  private func jsonOverride(expr: Expr, other: Expr) throws -> Expr {
    let json = try self.json(from: expr)
    return .object(json.overriding(with: try self.json(from: other)))
  }
  
  private func expr(from: JSONLocation) -> Expr {
    var res = Expr.null
    var location = from
    while true {
      switch location {
        case .root:
          return res
        case .member(let parent, let member):
          res = .pair(.makeString(member), res)
          location = parent
        case .index(let parent, let index):
          res = .pair(.fixnum(Int64(index)), res)
          location = parent
      }
    }
  }
  
  // JSON queries
  
  private func isJsonPath(expr: Expr, strict: Expr?) throws -> Expr {
    do {
      var parser = JSONPathParser(string: try expr.asString(), strict: strict?.isTrue ?? true)
      _ = try parser.parse()
      return .true
    } catch {
      return .false
    }
  }
  
  private func jsonQuery(expr: Expr, path: Expr) throws -> Expr {
    let results = try self.json(from: expr).query(path.asString())
    var res = Expr.null
    for result in results {
      res = .pair(.pair(.object(result.value), self.expr(from: result.location)), res)
    }
    return res
  }
  
  private func jsonQueryResults(expr: Expr, path: Expr) throws -> Expr {
    let results = try self.json(from: expr).query(values: path.asString())
    var res = Expr.null
    for result in results {
      res = .pair(.object(result), res)
    }
    return res
  }
  
  private func jsonQueryLocations(expr: Expr, path: Expr) throws -> Expr {
    let results = try self.json(from: expr).query(locations: path.asString())
    var res = Expr.null
    for result in results {
      res = .pair(self.expr(from: result), res)
    }
    return res
  }
}

extension JSON: CustomExpr {
  
  /// Type representing enum sets
  public static let type = Type.objectType(Symbol(uninterned: "json"))
  
  public var type: Type {
    return Self.type
  }
  
  public var string: String {
    return "#<\(self.tagString)>"
  }
  
  public var tagString: String {
    return self.jsonString(tag: "json")
  }
  
  public func jsonString(tag: String?) -> String {
    func toString(_ json: JSON) -> String {
      switch json {
        case .null:
          return "null"
        case .boolean(let bool):
          return bool ? "true" : "false"
        case .integer(let num):
          return String(num)
        case .float(let num):
          return String(num)
        case .string(let str):
          return "\"\(str)\""
        case .array(let arr):
          if arr.count == 0 {
            return "[]"
          } else if arr.count == 1 {
            return "[\(toString(arr[0]))]"
          } else {
            return "[\(toString(arr[0])),…+\(arr.count - 1)]"
          }
        case .object(let dict):
          if let attribute = dict.first {
            if dict.count == 1 {
              return "{\(attribute.key)}"
            } else {
              return "{\(attribute.key),…+\(dict.count - 1)]"
            }
          } else {
            return "{}"
          }
      }
    }
    var res: String
    switch self {
      case .array(let arr):
        if arr.count > 8 {
          res = "[" + arr.dropLast(arr.count - 8).map(toString).joined(separator: ",") +
                ",…+\(arr.count - 8)]"
        } else {
          res = "[\(arr.map(toString).joined(separator: ","))]"
        }
      case .object(let dict):
        if dict.count > 8 {
          res = "{" + dict.keys.dropLast(dict.count - 8).joined(separator: ",") +
                ",…+\(dict.count - 8)}"
        } else {
          res = "{\(dict.keys.joined(separator: ","))}"
        }
      default:
        res = toString(self)
    }
    if let tag {
      return "\(tag) \(res)"
    } else {
      return res
    }
  }
  
  public var hash: Int {
    return self.hashValue
  }
  
  public func equals(to expr: Expr) -> Bool {
    guard case .object(let obj) = expr,
          let other = obj as? JSON else {
      return false
    }
    return self == other
  }
  
  public func toExpr(in context: Context) -> Expr {
    switch self {
      case .null:
        return .symbol(context.symbols.null)
      case .boolean(let bool):
        return .makeBoolean(bool)
      case .integer(let num):
        return .fixnum(num)
      case .float(let num):
        return .flonum(num)
      case .string(let str):
        return .makeString(str)
      case .array(let arr):
        return .vector(Collection(kind: .immutableVector,
                                  exprs: Exprs(arr.map { $0.toExpr(in: context) })))
      case .object(let dict):
        return .makeList(Exprs(dict.map { (key, val) in
                 .pair(.symbol(context.symbols.intern(key)), val.toExpr(in: context))
               }))
    }
  }
  
  public func unpack(in context: Context) -> Exprs {
    return [self.toExpr(in: context)]
  }
}

public final class MutableJSON: AnyMutableNativeObject<JSON> {

  /// Type representing images
  public static let type = Type.objectType(Symbol(uninterned: "mutable-json"))

  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    return "#<mutable-json \(self.identityString) \(self.value.jsonString(tag: nil))>"
  }
  
  public override var tagString: String {
    return self.value.jsonString(tag: "mutable-json")
  }
  
  public override func unpack(in context: Context) -> Exprs {
    return [.object(self.value)]
  }
}

public final class NativeJSONPatch: AnyNativeObject<[JSONPatchOperation]> {

  /// Type representing images
  public static let type = Type.objectType(Symbol(uninterned: "json-patch"))

  public override var type: Type {
    return Self.type
  }
  
  private static func toString(operation: JSONPatchOperation) -> String {
    switch operation {
      case .add(let pointer, let value):
        return "add \(pointer) \(value.jsonString(tag: nil))"
      case .remove(let pointer):
        return "remove \(pointer)"
      case .replace(let pointer, let value):
        return "replace \(pointer) \(value.jsonString(tag: nil))"
      case .move(let pointer, let from):
        return "move \(pointer) \(from)"
      case .copy(let pointer, let from):
        return "copy \(pointer) \(from)"
      case .test(let pointer, let value):
        return "test \(pointer) \(value.jsonString(tag: nil))"
    }
  }
  
  public override var string: String {
    return "#<\(self.tagString)>"
  }
  
  public override var tagString: String {
    let ops = self.value.map(NativeJSONPatch.toString(operation:)).joined(separator: ", ")
    return "json-patch \(self.identityString) \(ops)"
  }
  
  public override func unpack(in context: Context) -> Exprs {
    return [.object(self)]
  }
}
