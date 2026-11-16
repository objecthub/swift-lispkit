//
//  YamlLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 16/11/2026.
//  Copyright © 2026 ObjectHub. All rights reserved.
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
import YamlKit

///
/// Implements library `(lispkit yaml)`, an API for representing, reading, writing and
/// manipulating YAML documents. The library is built on top of the YamlKit framework and
/// makes its three layers available to Scheme:
///
///   - _nodes_ (`yaml` objects) representing the YAML representation graph,
///   - _events_ (`yaml-event` objects) with a pull parser and a push emitter, and
///   - _text_, i.e. strings and bytevectors containing YAML documents.
///
public final class YamlLibrary: NativeLibrary {

  /// Location of an internal procedure that applies a schema procedure and catches errors
  /// raised by it. Errors need to be caught within the nested virtual machine invocation
  /// because continuations of enclosing exception handlers cannot be invoked from there.
  private var schemaCall: Int = -1

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "yaml"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"],    "define", "define-syntax", "syntax-rules", "quote")
    self.`import`(from: ["lispkit", "control"], "do", "let", "if")
    self.`import`(from: ["lispkit", "dynamic"], "assert", "guard")
    self.`import`(from: ["lispkit", "list"],    "null?", "car", "cdr", "cons")
  }

  /// Declarations of the library.
  public override func declarations() {
    // Type tags
    self.define("yaml-type-tag", as: NativeYAML.type.objectTypeTag())
    self.define("mutable-yaml-type-tag", as: MutableYAML.type.objectTypeTag())
    self.define("yaml-event-type-tag", as: NativeYAMLEvent.type.objectTypeTag())
    self.define("yaml-parser-type-tag", as: NativeYAMLParser.type.objectTypeTag())
    self.define("yaml-emitter-type-tag", as: NativeYAMLEmitter.type.objectTypeTag())

    // YAML nodes: predicates
    self.define(Procedure("yaml?", self.isYaml))
    self.define(Procedure("mutable-yaml?", self.isMutableYaml))
    self.define(Procedure("yaml-null?", self.isYamlNull))
    self.define(Procedure("yaml-boolean?", self.isYamlBoolean))
    self.define(Procedure("yaml-number?", self.isYamlNumber))
    self.define(Procedure("yaml-string?", self.isYamlString))
    self.define(Procedure("yaml-scalar?", self.isYamlScalar))
    self.define(Procedure("yaml-sequence?", self.isYamlSequence))
    self.define(Procedure("yaml-mapping?", self.isYamlMapping))
    self.define(Procedure("yaml-member?", self.isYamlMember))
    self.define(Procedure("yaml=?", self.isYamlEqual))

    // YAML nodes: construction and conversion
    self.define(Procedure("yaml", self.yamlConstr))
    self.define(Procedure("yaml->value", self.yamlToValue))
    self.define(Procedure("yaml-with-tag", self.yamlWithTag))
    self.define(Procedure("yaml-with-style", self.yamlWithStyle))
    self.define(Procedure("yaml-with-anchor", self.yamlWithAnchor))

    // YAML nodes: access
    self.define(Procedure("yaml-tag", self.yamlTag))
    self.define(Procedure("yaml-style", self.yamlStyle))
    self.define(Procedure("yaml-anchor", self.yamlAnchor))
    self.define(Procedure("yaml-mark", self.yamlMark))
    self.define(Procedure("yaml-scalar-text", self.yamlScalarText))
    self.define(Procedure("yaml-children", self.yamlChildren))
    self.define(Procedure("yaml-children-count", self.yamlChildrenCount))
    self.define(Procedure("yaml-keys", self.yamlKeys))
    self.define(Procedure("yaml-entries", self.yamlEntries))
    self.define(Procedure("yaml-ref", self.yamlRef))
    self.define(Procedure("yaml-replace", self.yamlReplace))

    // Mutable YAML nodes
    self.define(Procedure("mutable-yaml", self.mutableYaml))
    self.define(Procedure("yaml-set!", self.yamlSet))
    self.define(Procedure("yaml-append!", self.yamlAppend))
    self.define(Procedure("yaml-insert!", self.yamlInsert))
    self.define(Procedure("yaml-remove!", self.yamlRemove))

    // Reading and writing YAML text
    self.define(Procedure("string->yaml", self.stringToYaml))
    self.define(Procedure("bytevector->yaml", self.bytevectorToYaml))
    self.define(Procedure("load-yaml", self.loadYaml))
    self.define(Procedure("string->yaml-documents", self.stringToYamlDocuments))
    self.define(Procedure("bytevector->yaml-documents", self.bytevectorToYamlDocuments))
    self.define(Procedure("load-yaml-documents", self.loadYamlDocuments))
    self.define(Procedure("yaml->string", self.yamlToString))
    self.define(Procedure("yaml->bytevector", self.yamlToBytevector))
    self.define(Procedure("yaml-documents->string", self.yamlDocumentsToString))
    self.define(Procedure("yaml-documents->bytevector", self.yamlDocumentsToBytevector))
    self.define(Procedure("save-yaml", self.saveYaml))
    self.define(Procedure("save-yaml-documents", self.saveYamlDocuments))

    // YAML events
    self.define(Procedure("yaml-event?", self.isYamlEvent))
    self.define(Procedure("make-yaml-event", self.makeYamlEvent))
    self.define(Procedure("yaml-event-kind", self.yamlEventKind))
    self.define(Procedure("yaml-event-value", self.yamlEventValue))
    self.define(Procedure("yaml-event-style", self.yamlEventStyle))
    self.define(Procedure("yaml-event-tag", self.yamlEventTag))
    self.define(Procedure("yaml-event-anchor", self.yamlEventAnchor))
    self.define(Procedure("yaml-event-implicit?", self.isYamlEventImplicit))
    self.define(Procedure("yaml-event-version", self.yamlEventVersion))
    self.define(Procedure("yaml-event-tag-directives", self.yamlEventTagDirectives))
    self.define(Procedure("yaml-event-start", self.yamlEventStart))
    self.define(Procedure("yaml-event-end", self.yamlEventEnd))
    self.define(Procedure("string->yaml-events", self.stringToYamlEvents))
    self.define(Procedure("bytevector->yaml-events", self.bytevectorToYamlEvents))
    self.define(Procedure("yaml-events->string", self.yamlEventsToString))

    // YAML parsers and emitters
    self.define(Procedure("yaml-parser?", self.isYamlParser))
    self.define(Procedure("make-yaml-parser", self.makeYamlParser))
    self.define(Procedure("yaml-parser-next!", self.yamlParserNext))
    self.define(Procedure("yaml-emitter?", self.isYamlEmitter))
    self.define(Procedure("make-yaml-emitter", self.makeYamlEmitter))
    self.define(Procedure("yaml-emitter-emit!", self.yamlEmitterEmit))
    self.define(Procedure("yaml-emitter-output", self.yamlEmitterOutput))

    // Internal helper for applying schema procedures
    self.schemaCall = self.define("_yaml-schema-call", via: """
      (define (_yaml-schema-call proc text)
        (guard (e (#t (cons 'error e)))
          (cons 'ok (proc text))))
    """)
    
    // Higher-order functions
    self.define("yaml-for-each-element", via: """
      (define (yaml-for-each-element f node)
        (assert (yaml-sequence? node))
        (do ((lst (yaml-children node) (cdr lst)))
            ((null? lst))
          (f (car lst))))
    """)
    self.define("yaml-for-each-entry", via: """
      (define (yaml-for-each-entry f node)
        (assert (yaml-mapping? node))
        (do ((lst (yaml-entries node) (cdr lst)))
            ((null? lst))
          (f (car (car lst)) (cdr (car lst)))))
    """)
  }

  // MARK: - Errors

  /// Translates a YAML error into a Scheme error.
  private static func translate(_ error: YAMLError) -> RuntimeError {
    // `$` introduces placeholders in error message templates and thus needs to be escaped
    return RuntimeError.custom("yaml error",
                               error.description.replacingOccurrences(of: "$", with: "$$"),
                               [])
  }

  /// Returns an error stating that `expr` is invalid for the given reason.
  private static func invalid(_ reason: String, _ expr: Expr) -> RuntimeError {
    return RuntimeError.custom("error", "\(reason): $0", [expr])
  }

  /// Executes `body` and translates YAML errors into Scheme errors. If a schema procedure
  /// failed while `body` was executed, the error of the schema procedure is thrown.
  private func guarded<T>(_ options: Options? = nil, _ body: () throws -> T) throws -> T {
    do {
      let res = try body()
      try options?.checkSchema()
      return res
    } catch let error as YAMLError {
      try options?.checkSchema()
      throw Self.translate(error)
    }
  }

  // MARK: - Options

  /// Schema error box. It is used to propagate errors raised by schema procedures.
  private final class SchemaErrorBox {
    var error: Error? = nil
  }

  /// A YAML schema backed by a Scheme procedure. The procedure is applied to the text of a
  /// plain scalar and returns a tag, or `#f` if the core schema should be used instead.
  private struct ProcedureSchema: YAMLSchema, @unchecked Sendable {
    let procedure: Procedure
    let caller: Procedure
    let context: Context
    let box: SchemaErrorBox
    
    func tag(forPlainScalar value: String) -> YAMLTag {
      guard self.box.error == nil else {
        return .str
      }
      do {
        let args: Expr = .pair(.procedure(self.procedure), .pair(.makeString(value), .null))
        let res = try self.context.evaluator.machine.apply(.procedure(self.caller), to: args)
        guard case .pair(.symbol(let status), let result) = res else {
          throw YamlLibrary.invalid("unexpected schema result", res)
        }
        if status.identifier == "error" {
          if case .error(let error) = result {
            throw error
          }
          throw RuntimeError.uncaught(result)
        }
        switch result {
          case .false:
            return CoreSchema().tag(forPlainScalar: value)
          case .string(let str):
            return try YamlLibrary.parseTag(str as String, result)
          default:
            throw YamlLibrary.invalid("schema procedure must return a tag string or #f", result)
        }
      } catch {
        self.box.error = error
        return .str
      }
    }
  }
  
  /// Options controlling parsing and serialization.
  private struct Options {
    var parse = YAML.ParseOptions()
    var serialize = YAML.SerializeOptions()
    var sortKeys = false
    var flowStyle = false
    var box: SchemaErrorBox? = nil

    func checkSchema() throws {
      if let error = self.box?.error {
        self.box?.error = nil
        throw error
      }
    }
  }

  /// Parses an option list of the form `((name . value) ...)`.
  private func options(from expr: Expr?) throws -> Options {
    var options = Options()
    guard let expr, expr != .false else {
      return options
    }
    var lst = expr
    while case .pair(let entry, let rest) = lst {
      guard case .pair(let key, let value) = entry else {
        throw RuntimeError.type(entry, expected: [.pairType])
      }
      let name: String
      switch key {
        case .symbol(let sym):
          name = sym.identifier
        case .string(let str):
          name = str as String
        default:
          throw RuntimeError.type(key, expected: [.symbolType])
      }
      func bool() throws -> Bool {
        switch value {
          case .true:
            return true
          case .false:
            return false
          default:
            throw Self.invalid("YAML option '\(name)' requires a boolean", value)
        }
      }
      switch name {
        case "schema":
          switch value {
            case .symbol(let sym):
              switch sym.identifier {
                case "core":
                  options.parse.schema = CoreSchema()
                  options.serialize.schema = CoreSchema()
                case "json":
                  options.parse.schema = JSONSchema()
                  options.serialize.schema = JSONSchema()
                case "failsafe":
                  options.parse.schema = FailsafeSchema()
                  options.serialize.schema = FailsafeSchema()
                default:
                  throw Self.invalid("unknown YAML schema", value)
              }
            case .procedure(let proc):
              let box = options.box ?? SchemaErrorBox()
              options.box = box
              let schema = ProcedureSchema(procedure: proc,
                                           caller: self.procedure(self.schemaCall),
                                           context: self.context,
                                           box: box)
              options.parse.schema = schema
              options.serialize.schema = schema
            default:
              throw Self.invalid("YAML option 'schema' requires a symbol or procedure", value)
          }
        case "duplicate-keys":
          options.parse.allowsDuplicateKeys = try bool()
        case "merge-keys":
          options.parse.resolvesMergeKeys = try bool()
        case "max-alias-expansion":
          options.parse.maximumAliasExpansion = try value.asInt(above: 0)
        case "max-depth":
          options.parse.maximumDepth = try value.asInt(above: 1)
        case "indentation":
          options.serialize.formatting.indentation = try value.asInt(above: 2, below: 10)
        case "line-width":
          if value.isFalse {
            options.serialize.formatting.lineWidth = Int.max
          } else {
            options.serialize.formatting.lineWidth = try value.asInt(above: 1)
          }
        case "unicode":
          options.serialize.formatting.allowsUnicode = try bool()
        case "indent-sequences":
          options.serialize.formatting.indentsSequencesInMappings = try bool()
        case "explicit-start":
          options.serialize.formatting.explicitDocumentStart = try bool()
        case "sort-keys":
          options.sortKeys = try bool()
        case "flow-style":
          options.flowStyle = try bool()
        default:
          throw Self.invalid("unknown YAML option", key)
      }
      lst = rest
    }
    guard lst.isNull else {
      throw RuntimeError.type(expr, expected: [.assocListType])
    }
    return options
  }

  // MARK: - Nodes and values

  private static func wrap(_ node: YAMLNode) -> Expr {
    return .object(NativeYAML(node))
  }

  private func nodeOpt(from expr: Expr) -> YAMLNode? {
    guard case .object(let obj) = expr else {
      return nil
    }
    if let node = obj as? NativeYAML {
      return node.value
    } else if let mutable = obj as? MutableYAML {
      return mutable.value
    } else {
      return nil
    }
  }

  private func node(from expr: Expr) throws -> YAMLNode {
    guard let node = self.nodeOpt(from: expr) else {
      throw RuntimeError.type(expr, expected: [NativeYAML.type])
    }
    return node
  }

  private func mutableNode(from expr: Expr) throws -> MutableYAML {
    guard case .object(let obj) = expr, let mutable = obj as? MutableYAML else {
      throw RuntimeError.type(expr, expected: [MutableYAML.type])
    }
    return mutable
  }

  /// Parses a tag in shorthand notation (`!!str`, `!local`, `!<uri>`) or as a resolved URI.
  internal static func parseTag(_ str: String, _ expr: Expr) throws -> YAMLTag {
    if str == "!" {
      return .nonSpecific
    } else if str.hasPrefix("!!") {
      guard str.count > 2 else {
        throw Self.invalid("not a valid YAML tag", expr)
      }
      return YAMLTag(YAMLTag.yamlPrefix + str.dropFirst(2))
    } else if str.hasPrefix("!<") {
      guard str.hasSuffix(">") && str.count > 3 else {
        throw Self.invalid("not a valid YAML tag", expr)
      }
      return YAMLTag(String(str.dropFirst(2).dropLast()))
    } else if str.isEmpty {
      throw Self.invalid("not a valid YAML tag", expr)
    } else {
      return YAMLTag(str)
    }
  }

  private static func scalarStyle(from expr: Expr) throws -> ScalarStyle? {
    switch expr {
      case .false:
        return nil
      case .symbol(let sym):
        switch sym.identifier {
          case "plain":
            return .plain
          case "single-quoted":
            return .singleQuoted
          case "double-quoted":
            return .doubleQuoted
          case "literal":
            return .literal
          case "folded":
            return .folded
          default:
            throw Self.invalid("not a valid YAML scalar style", expr)
        }
      default:
        throw Self.invalid("not a valid YAML scalar style", expr)
    }
  }

  private static func collectionStyle(from expr: Expr) throws -> CollectionStyle? {
    switch expr {
      case .false:
        return nil
      case .symbol(let sym):
        switch sym.identifier {
          case "block":
            return .block
          case "flow":
            return .flow
          default:
            throw Self.invalid("not a valid YAML collection style", expr)
        }
      default:
        throw Self.invalid("not a valid YAML collection style", expr)
    }
  }

  private func styleExpr(_ style: ScalarStyle?) -> Expr {
    guard let style else {
      return .false
    }
    switch style {
      case .plain:
        return .symbol(self.context.symbols.intern("plain"))
      case .singleQuoted:
        return .symbol(self.context.symbols.intern("single-quoted"))
      case .doubleQuoted:
        return .symbol(self.context.symbols.intern("double-quoted"))
      case .literal:
        return .symbol(self.context.symbols.intern("literal"))
      case .folded:
        return .symbol(self.context.symbols.intern("folded"))
    }
  }

  private func styleExpr(_ style: CollectionStyle?) -> Expr {
    guard let style else {
      return .false
    }
    switch style {
      case .block:
        return .symbol(self.context.symbols.intern("block"))
      case .flow:
        return .symbol(self.context.symbols.intern("flow"))
    }
  }

  private static func markExpr(_ mark: Mark?) -> Expr {
    guard let mark else {
      return .false
    }
    return .makeList(.fixnum(Int64(mark.line)),
                     .fixnum(Int64(mark.column)),
                     .fixnum(Int64(mark.offset)))
  }

  /// Converts a Scheme value into a YAML mapping key.
  private static func toKey(_ expr: Expr, in context: Context) throws -> YAMLNode {
    switch expr {
      case .symbol(let sym):
        return YAMLNode(sym.identifier)
      case .string(let str):
        return YAMLNode(str as String)
      default:
        return try Self.toYAML(expr, in: context)
    }
  }

  /// Converts a Scheme value into a YAML node.
  internal static func toYAML(_ expr: Expr, in context: Context) throws -> YAMLNode {
    switch expr {
      case .symbol(context.symbols.null):
        return .null
      case .null:
        return .mapping(YAMLNode.Mapping([YAMLNode.Mapping.Entry]()))
      case .false:
        return YAMLNode(false)
      case .true:
        return YAMLNode(true)
      case .fixnum(let num):
        return YAMLNode(num)
      case .bignum(let num):
        return .scalar(YAMLNode.Scalar(num.description, tag: .int))
      case .flonum(let num):
        return YAMLNode(num)
      case .symbol(let sym):
        return YAMLNode(sym.identifier)
      case .string(let str):
        return YAMLNode(str as String)
      case .vector(let col):
        switch col.kind {
          case .array, .vector, .immutableVector, .growableVector:
            var elements: [YAMLNode] = []
            elements.reserveCapacity(col.exprs.count)
            for expr in col.exprs {
              elements.append(try Self.toYAML(expr, in: context))
            }
            return .sequence(YAMLNode.Sequence(elements))
          default:
            throw Self.invalid("cannot convert to YAML", expr)
        }
      case .record(let record):
        guard case .record(let type) = record.kind,
              case .recordType = type.kind else {
          throw Self.invalid("cannot convert to YAML", expr)
        }
        var entries: [YAMLNode.Mapping.Entry] = []
        for sym in Collection.RecordType.fields(type) {
          guard let index = RecordLibrary.indexOfField(sym, in: type),
                index >= 0 && index < record.exprs.count else {
            throw RuntimeError.eval(.unknownFieldOfRecordType, expr, .symbol(sym))
          }
          entries.append(YAMLNode.Mapping.Entry(
                           key: YAMLNode(sym.identifier),
                           value: try Self.toYAML(record.exprs[index], in: context)))
        }
        return .mapping(YAMLNode.Mapping(entries))
      case .pair(_, _):
        var entries: [YAMLNode.Mapping.Entry] = []
        var rs = expr
        while case .pair(.pair(let key, let value), let rest) = rs {
          if case .void = value {
            rs = rest
            continue
          }
          entries.append(YAMLNode.Mapping.Entry(key: try Self.toKey(key, in: context),
                                                value: try Self.toYAML(value, in: context)))
          rs = rest
        }
        guard case .null = rs else {
          throw Self.invalid("cannot convert to YAML", expr)
        }
        return .mapping(YAMLNode.Mapping(entries))
      case .table(let table):
        var entries: [YAMLNode.Mapping.Entry] = []
        for (key, value) in table.entries {
          if case .void = value {
            continue
          }
          entries.append(YAMLNode.Mapping.Entry(key: try Self.toKey(key, in: context),
                                                value: try Self.toYAML(value, in: context)))
        }
        return .mapping(YAMLNode.Mapping(entries))
      case .object(let obj):
        if let node = obj as? NativeYAML {
          return node.value
        } else if let mutable = obj as? MutableYAML {
          return mutable.value
        } else {
          throw Self.invalid("cannot convert to YAML", expr)
        }
      default:
        throw Self.invalid("cannot convert to YAML", expr)
    }
  }

  private func toYAML(_ expr: Expr) throws -> YAMLNode {
    return try Self.toYAML(expr, in: self.context)
  }

  /// Parses a boolean. In addition to the core schema spellings, the YAML 1.1 spellings are
  /// supported since schemas can map them to booleans.
  private static func parseBool(_ text: String) -> Bool? {
    if let bool = CoreSchema.parseBool(text) {
      return bool
    }
    switch text.lowercased() {
      case "yes", "y", "on", "true":
        return true
      case "no", "n", "off", "false":
        return false
      default:
        return nil
    }
  }

  /// Parses an integer in decimal, octal (`0o`) or hexadecimal (`0x`) notation.
  private static func parseInt(_ text: String) -> Expr? {
    if let num = CoreSchema.parseInteger(text, as: Int64.self) {
      return .fixnum(num)
    }
    var digits = Substring(text)
    var radix = 10
    var negative = false
    if digits.hasPrefix("0o") {
      radix = 8
      digits = digits.dropFirst(2)
    } else if digits.hasPrefix("0x") {
      radix = 16
      digits = digits.dropFirst(2)
    } else if digits.hasPrefix("-") {
      negative = true
      digits = digits.dropFirst()
    } else if digits.hasPrefix("+") {
      digits = digits.dropFirst()
    }
    guard let num = BigInt(String(digits), radix: radix) else {
      return nil
    }
    return .makeNumber(negative ? -num : num)
  }

  /// Converts a YAML node into a Scheme value.
  internal static func toValue(_ node: YAMLNode, in context: Context) -> Expr {
    switch node {
      case .scalar(let scalar):
        switch scalar.tag {
          case .null:
            return .symbol(context.symbols.null)
          case .bool:
            if let bool = Self.parseBool(scalar.value) {
              return .makeBoolean(bool)
            }
          case .int:
            if let num = Self.parseInt(scalar.value) {
              return num
            }
          case .float:
            if let num = CoreSchema.parseFloat(scalar.value) {
              return .flonum(num)
            }
          default:
            break
        }
        return .makeString(scalar.value)
      case .sequence(let sequence):
        var exprs = Exprs()
        exprs.reserveCapacity(sequence.elements.count)
        for element in sequence.elements {
          exprs.append(Self.toValue(element, in: context))
        }
        return .vector(Collection(kind: .immutableVector, exprs: exprs))
      case .mapping(let mapping):
        var entries = Exprs()
        entries.reserveCapacity(mapping.entries.count)
        for entry in mapping.entries {
          let key: Expr
          if case .scalar(let scalar) = entry.key, scalar.tag == .str {
            key = .symbol(context.symbols.intern(scalar.value))
          } else {
            key = Self.toValue(entry.key, in: context)
          }
          entries.append(.pair(key, Self.toValue(entry.value, in: context)))
        }
        return .makeList(entries)
    }
  }

  /// Returns a short description of the given node.
  private static func summary(_ node: YAMLNode, depth: Int = 0) -> String {
    func text(_ str: String) -> String {
      let line = str.replacingOccurrences(of: "\n", with: "\\n")
      return line.count > 32 ? String(line.prefix(32)) + "…" : line
    }
    switch node {
      case .scalar(let scalar):
        return scalar.tag == .str ? "\"\(text(scalar.value))\"" : text(scalar.value)
      case .sequence(let sequence):
        if sequence.isEmpty {
          return "[]"
        } else if depth >= 2 {
          return "[…]"
        }
        let items = sequence.elements.prefix(4).map { Self.summary($0, depth: depth + 1) }
        let more = sequence.count > 4 ? ",…+\(sequence.count - 4)" : ""
        return "[\(items.joined(separator: ","))\(more)]"
      case .mapping(let mapping):
        if mapping.entries.isEmpty {
          return "{}"
        } else if depth >= 2 {
          return "{…}"
        }
        let keys = mapping.entries.prefix(4).map { entry in
          if case .scalar(let scalar) = entry.key {
            return text(scalar.value)
          }
          return Self.summary(entry.key, depth: depth + 1)
        }
        let more = mapping.entries.count > 4 ? ",…+\(mapping.entries.count - 4)" : ""
        return "{\(keys.joined(separator: ","))\(more)}"
    }
  }

  fileprivate static func nodeDescription(_ node: YAMLNode) -> String {
    return Self.summary(node)
  }

  // MARK: - References

  /// A reference to a child of a YAML node.
  private enum Ref {
    case index(Int)
    case key(String)
    case node(YAMLNode)

    func keyNode() -> YAMLNode {
      switch self {
        case .index(let i):
          return YAMLNode(i)
        case .key(let str):
          return YAMLNode(str)
        case .node(let node):
          return node
      }
    }
  }

  private func ref(from expr: Expr) throws -> Ref {
    switch expr {
      case .fixnum(let num):
        guard let index = Int(exactly: num) else {
          throw Self.invalid("not a valid YAML reference", expr)
        }
        return .index(index)
      case .symbol(let sym):
        return .key(sym.identifier)
      case .string(let str):
        return .key(str as String)
      default:
        if let node = self.nodeOpt(from: expr) {
          return .node(node)
        }
        throw Self.invalid("not a valid YAML reference", expr)
    }
  }

  /// Converts a reference into a path. `()` is the empty path, lists are paths, and all
  /// other references are paths of length 1.
  private func path(from expr: Expr) throws -> [Ref] {
    switch expr {
      case .null:
        return []
      case .pair(_, _):
        var res: [Ref] = []
        var lst = expr
        while case .pair(let car, let cdr) = lst {
          res.append(try self.ref(from: car))
          lst = cdr
        }
        guard case .null = lst else {
          throw RuntimeError.type(expr, expected: [.properListType])
        }
        return res
      default:
        return [try self.ref(from: expr)]
    }
  }

  private static func index(of ref: Ref, in mapping: YAMLNode.Mapping) -> Int? {
    switch ref {
      case .index(_):
        let key = ref.keyNode()
        return mapping.entries.firstIndex { $0.key == key }
      case .key(let str):
        return mapping.entries.firstIndex { $0.key.scalar?.value == str }
      case .node(let node):
        return mapping.entries.firstIndex { $0.key == node }
    }
  }

  private static func lookup(_ node: YAMLNode, _ ref: Ref) -> YAMLNode? {
    switch node {
      case .scalar(_):
        return nil
      case .sequence(let sequence):
        if case .index(let i) = ref, sequence.elements.indices.contains(i) {
          return sequence.elements[i]
        }
        return nil
      case .mapping(let mapping):
        if let i = Self.index(of: ref, in: mapping) {
          return mapping.entries[i].value
        }
        return nil
    }
  }

  private static func notFound(_ expr: Expr) -> RuntimeError {
    return Self.invalid("cannot find YAML reference", expr)
  }

  /// Applies `body` to the node found at `path` within `node`.
  private func modify(_ node: inout YAMLNode,
                      _ path: ArraySlice<Ref>,
                      _ source: Expr,
                      _ body: (inout YAMLNode) throws -> Void) throws {
    guard let ref = path.first else {
      try body(&node)
      return
    }
    switch node {
      case .scalar(_):
        throw Self.notFound(source)
      case .sequence(var sequence):
        guard case .index(let i) = ref, sequence.elements.indices.contains(i) else {
          throw Self.notFound(source)
        }
        node = .null // release the reference to the sequence to enable in-place updates
        defer {
          node = .sequence(sequence)
        }
        try self.modify(&sequence.elements[i], path.dropFirst(), source, body)
      case .mapping(var mapping):
        guard let i = Self.index(of: ref, in: mapping) else {
          throw Self.notFound(source)
        }
        node = .null // release the reference to the mapping to enable in-place updates
        defer {
          node = .mapping(mapping)
        }
        try self.modify(&mapping.entries[i].value, path.dropFirst(), source, body)
    }
  }

  /// Sets the child referenced by `path` of `node` to `value`.
  private func set(_ node: inout YAMLNode,
                   _ path: [Ref],
                   _ value: YAMLNode,
                   _ source: Expr) throws {
    guard let last = path.last else {
      node = value
      return
    }
    try self.modify(&node, path.dropLast()[...], source) { parent in
      switch parent {
        case .scalar(_):
          throw Self.notFound(source)
        case .sequence(var sequence):
          guard case .index(let i) = last, i >= 0 && i <= sequence.elements.count else {
            throw Self.notFound(source)
          }
          parent = .null
          if i == sequence.elements.count {
            sequence.elements.append(value)
          } else {
            sequence.elements[i] = value
          }
          parent = .sequence(sequence)
        case .mapping(var mapping):
          parent = .null
          if let i = Self.index(of: last, in: mapping) {
            mapping.entries[i].value = value
          } else {
            mapping.entries.append(YAMLNode.Mapping.Entry(key: last.keyNode(), value: value))
          }
          parent = .mapping(mapping)
      }
    }
  }

  /// Removes the child referenced by `path` from `node`.
  private func remove(_ node: inout YAMLNode, _ path: [Ref], _ source: Expr) throws {
    guard let last = path.last else {
      throw Self.invalid("cannot remove the root of a YAML document", source)
    }
    try self.modify(&node, path.dropLast()[...], source) { parent in
      switch parent {
        case .scalar(_):
          throw Self.notFound(source)
        case .sequence(var sequence):
          guard case .index(let i) = last, sequence.elements.indices.contains(i) else {
            throw Self.notFound(source)
          }
          parent = .null
          sequence.elements.remove(at: i)
          parent = .sequence(sequence)
        case .mapping(var mapping):
          guard let i = Self.index(of: last, in: mapping) else {
            throw Self.notFound(source)
          }
          parent = .null
          mapping.entries.remove(at: i)
          parent = .mapping(mapping)
      }
    }
  }

  // MARK: - Predicates

  private func isYaml(expr: Expr, strict: Expr?) throws -> Expr {
    guard case .object(let obj) = expr else {
      return .false
    }
    if strict?.isTrue ?? false {
      return .makeBoolean(obj is NativeYAML)
    } else {
      return .makeBoolean(obj is NativeYAML || obj is MutableYAML)
    }
  }

  private func isMutableYaml(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr else {
      return .false
    }
    return .makeBoolean(obj is MutableYAML)
  }

  private func isYamlNull(expr: Expr) throws -> Expr {
    return .makeBoolean(self.nodeOpt(from: expr)?.isNull ?? false)
  }

  private func isYamlBoolean(expr: Expr) throws -> Expr {
    return .makeBoolean(self.nodeOpt(from: expr)?.scalar?.tag == .bool)
  }

  private func isYamlNumber(expr: Expr) throws -> Expr {
    guard let tag = self.nodeOpt(from: expr)?.scalar?.tag else {
      return .false
    }
    return .makeBoolean(tag == .int || tag == .float)
  }

  private func isYamlString(expr: Expr) throws -> Expr {
    return .makeBoolean(self.nodeOpt(from: expr)?.scalar?.tag == .str)
  }

  private func isYamlScalar(expr: Expr) throws -> Expr {
    return .makeBoolean(self.nodeOpt(from: expr)?.scalar != nil)
  }

  private func isYamlSequence(expr: Expr) throws -> Expr {
    return .makeBoolean(self.nodeOpt(from: expr)?.sequence != nil)
  }

  private func isYamlMapping(expr: Expr) throws -> Expr {
    return .makeBoolean(self.nodeOpt(from: expr)?.mapping != nil)
  }

  private func isYamlMember(expr: Expr, key: Expr) throws -> Expr {
    guard let mapping = self.nodeOpt(from: expr)?.mapping else {
      return .false
    }
    let ref = try self.ref(from: key)
    return .makeBoolean(Self.index(of: ref, in: mapping) != nil)
  }

  private func isYamlEqual(expr: Expr, args: Arguments) throws -> Expr {
    let lhs = try self.node(from: expr)
    for arg in args {
      guard try lhs == self.node(from: arg) else {
        return .false
      }
    }
    return .true
  }

  // MARK: - Construction and conversion

  private func yamlConstr(args: Arguments) throws -> Expr {
    switch args.count {
      case 0:
        return Self.wrap(.null)
      case 1:
        return Self.wrap(try self.toYAML(args.first!))
      default:
        var elements: [YAMLNode] = []
        for arg in args {
          elements.append(try self.toYAML(arg))
        }
        return Self.wrap(.sequence(YAMLNode.Sequence(elements)))
    }
  }

  private func yamlToValue(expr: Expr, def: Expr?) throws -> Expr {
    if expr.isFalse, let def {
      return def
    }
    return Self.toValue(try self.node(from: expr), in: self.context)
  }

  private func yamlWithTag(expr: Expr, tag: Expr) throws -> Expr {
    var node = try self.node(from: expr)
    node.tag = try Self.parseTag(try tag.asString(), tag)
    return Self.wrap(node)
  }

  private func yamlWithStyle(expr: Expr, style: Expr) throws -> Expr {
    switch try self.node(from: expr) {
      case .scalar(var scalar):
        scalar.style = try Self.scalarStyle(from: style)
        return Self.wrap(.scalar(scalar))
      case .sequence(var sequence):
        sequence.style = try Self.collectionStyle(from: style)
        return Self.wrap(.sequence(sequence))
      case .mapping(var mapping):
        mapping.style = try Self.collectionStyle(from: style)
        return Self.wrap(.mapping(mapping))
    }
  }

  private func yamlWithAnchor(expr: Expr, anchor: Expr) throws -> Expr {
    var node = try self.node(from: expr)
    if anchor.isFalse {
      node.anchor = nil
    } else {
      let name = try anchor.asString()
      guard !name.isEmpty else {
        throw Self.invalid("not a valid YAML anchor", anchor)
      }
      node.anchor = name
    }
    return Self.wrap(node)
  }

  // MARK: - Access

  private func yamlTag(expr: Expr) throws -> Expr {
    return .makeString(try self.node(from: expr).tag.description)
  }

  private func yamlStyle(expr: Expr) throws -> Expr {
    switch try self.node(from: expr) {
      case .scalar(let scalar):
        return self.styleExpr(scalar.style)
      case .sequence(let sequence):
        return self.styleExpr(sequence.style)
      case .mapping(let mapping):
        return self.styleExpr(mapping.style)
    }
  }

  private func yamlAnchor(expr: Expr) throws -> Expr {
    if let anchor = try self.node(from: expr).anchor {
      return .makeString(anchor)
    }
    return .false
  }

  private func yamlMark(expr: Expr) throws -> Expr {
    return Self.markExpr(try self.node(from: expr).mark)
  }

  private func yamlScalarText(expr: Expr) throws -> Expr {
    if let text = try self.node(from: expr).string {
      return .makeString(text)
    }
    return .false
  }

  private func yamlChildren(expr: Expr) throws -> Expr {
    var res: Expr = .null
    switch try self.node(from: expr) {
      case .scalar(_):
        break
      case .sequence(let sequence):
        for element in sequence.elements.reversed() {
          res = .pair(Self.wrap(element), res)
        }
      case .mapping(let mapping):
        for entry in mapping.entries.reversed() {
          res = .pair(Self.wrap(entry.value), res)
        }
    }
    return res
  }

  private func yamlChildrenCount(expr: Expr) throws -> Expr {
    switch try self.node(from: expr) {
      case .scalar(_):
        return .fixnum(0)
      case .sequence(let sequence):
        return .makeNumber(sequence.count)
      case .mapping(let mapping):
        return .makeNumber(mapping.entries.count)
    }
  }

  private func yamlKeys(expr: Expr) throws -> Expr {
    var res: Expr = .null
    if let mapping = try self.node(from: expr).mapping {
      for entry in mapping.entries.reversed() {
        res = .pair(Self.wrap(entry.key), res)
      }
    }
    return res
  }

  private func yamlEntries(expr: Expr) throws -> Expr {
    var res: Expr = .null
    if let mapping = try self.node(from: expr).mapping {
      for entry in mapping.entries.reversed() {
        res = .pair(.pair(Self.wrap(entry.key), Self.wrap(entry.value)), res)
      }
    }
    return res
  }

  private func yamlRef(expr: Expr, refs: Arguments) throws -> Expr {
    var node = try self.node(from: expr)
    for reference in refs {
      for ref in try self.path(from: reference) {
        guard let child = Self.lookup(node, ref) else {
          return .false
        }
        node = child
      }
    }
    return Self.wrap(node)
  }

  private func yamlReplace(expr: Expr, args: Arguments) throws -> Expr {
    var node = try self.node(from: expr)
    var iter = args.makeIterator()
    while let reference = iter.next() {
      guard let value = iter.next() else {
        break
      }
      try self.set(&node, try self.path(from: reference), try self.toYAML(value), reference)
    }
    return Self.wrap(node)
  }

  // MARK: - Mutable nodes

  private func mutableYaml(expr: Expr, forceNew: Expr?) throws -> Expr {
    if !(forceNew?.isTrue ?? false), case .object(let obj) = expr, obj is MutableYAML {
      return expr
    }
    return .object(MutableYAML(try self.toYAML(expr)))
  }

  private func yamlSet(expr: Expr, reference: Expr, value: Expr) throws -> Expr {
    let mutable = try self.mutableNode(from: expr)
    let value = try self.toYAML(value)
    try self.set(&mutable.value, try self.path(from: reference), value, reference)
    return .void
  }

  private func yamlAppend(expr: Expr, reference: Expr, args: Arguments) throws -> Expr {
    let mutable = try self.mutableNode(from: expr)
    var elements: [YAMLNode] = []
    for arg in args {
      elements.append(try self.toYAML(arg))
    }
    try self.modify(&mutable.value, try self.path(from: reference)[...], reference) { target in
      guard case .sequence(var sequence) = target else {
        throw Self.invalid("not a YAML sequence", reference)
      }
      target = .null
      sequence.elements.append(contentsOf: elements)
      target = .sequence(sequence)
    }
    return .void
  }

  private func yamlInsert(expr: Expr,
                          reference: Expr,
                          index: Expr,
                          args: Arguments) throws -> Expr {
    let mutable = try self.mutableNode(from: expr)
    let ind = try index.asInt(above: 0)
    var elements: [YAMLNode] = []
    for arg in args {
      elements.append(try self.toYAML(arg))
    }
    try self.modify(&mutable.value, try self.path(from: reference)[...], reference) { target in
      guard case .sequence(var sequence) = target else {
        throw Self.invalid("not a YAML sequence", reference)
      }
      guard ind <= sequence.elements.count else {
        throw RuntimeError.range(parameter: 3,
                                 of: "yaml-insert!",
                                 index,
                                 min: 0,
                                 max: Int64(sequence.elements.count))
      }
      target = .null
      sequence.elements.insert(contentsOf: elements, at: ind)
      target = .sequence(sequence)
    }
    return .void
  }

  private func yamlRemove(expr: Expr, reference: Expr, args: Arguments) throws -> Expr {
    let mutable = try self.mutableNode(from: expr)
    try self.remove(&mutable.value, try self.path(from: reference), reference)
    for ref in args {
      try self.remove(&mutable.value, try self.path(from: ref), ref)
    }
    return .void
  }

  // MARK: - Reading YAML

  private func readFile(_ filename: Expr) throws -> Data {
    let path = self.context.fileHandler.path(try filename.asPath(),
                                             relativeTo: self.context.evaluator.currentDirectoryPath)
    guard let data = FileManager.default.contents(atPath: path) else {
      throw RuntimeError.eval(.cannotOpenFile, .makeString(path))
    }
    return data
  }

  /// Writes `text`, UTF-8 encoded, to the file at path `filename`. The file is replaced
  /// atomically, i.e. an existing file stays unchanged if writing fails.
  private func writeFile(_ filename: Expr, _ text: String) throws {
    let path = self.context.fileHandler.path(try filename.asPath(),
                                             relativeTo: self.context.evaluator.currentDirectoryPath)
    do {
      try Data(text.utf8).write(to: URL(fileURLWithPath: path), options: .atomic)
    } catch {
      throw RuntimeError.os(error)
    }
  }

  private func bytes(_ expr: Expr) throws -> Data {
    return Data(try expr.asByteVector().value)
  }

  private func document(_ node: YAMLNode?) -> Expr {
    if let node {
      return Self.wrap(node)
    }
    return .false
  }

  private func documents(_ nodes: [YAMLNode]) -> Expr {
    return .makeList(Exprs(nodes.map(Self.wrap)))
  }

  private func stringToYaml(expr: Expr, options: Expr?) throws -> Expr {
    let text = try expr.asString()
    let opts = try self.options(from: options)
    return self.document(try self.guarded(opts) { try YAML.parse(text, options: opts.parse) })
  }

  private func bytevectorToYaml(expr: Expr, options: Expr?) throws -> Expr {
    let data = try self.bytes(expr)
    let opts = try self.options(from: options)
    return self.document(try self.guarded(opts) { try YAML.parse(data, options: opts.parse) })
  }

  private func loadYaml(filename: Expr, options: Expr?) throws -> Expr {
    let data = try self.readFile(filename)
    let opts = try self.options(from: options)
    return self.document(try self.guarded(opts) { try YAML.parse(data, options: opts.parse) })
  }

  private func stringToYamlDocuments(expr: Expr, options: Expr?) throws -> Expr {
    let text = try expr.asString()
    let opts = try self.options(from: options)
    return self.documents(try self.guarded(opts) {
      try YAML.parseAll(text, options: opts.parse)
    })
  }

  private func bytevectorToYamlDocuments(expr: Expr, options: Expr?) throws -> Expr {
    let data = try self.bytes(expr)
    let opts = try self.options(from: options)
    return self.documents(try self.guarded(opts) {
      try YAML.parseAll(data, options: opts.parse)
    })
  }

  private func loadYamlDocuments(filename: Expr, options: Expr?) throws -> Expr {
    let data = try self.readFile(filename)
    let opts = try self.options(from: options)
    return self.documents(try self.guarded(opts) {
      try YAML.parseAll(data, options: opts.parse)
    })
  }

  // MARK: - Writing YAML

  private static func sorted(_ node: YAMLNode) -> YAMLNode {
    switch node {
      case .scalar(_):
        return node
      case .sequence(var sequence):
        sequence.elements = sequence.elements.map(Self.sorted)
        return .sequence(sequence)
      case .mapping(var mapping):
        func sortKey(_ key: YAMLNode) -> String {
          return key.scalar?.value ?? key.description
        }
        let entries = mapping.entries.map { entry in
          YAMLNode.Mapping.Entry(key: Self.sorted(entry.key), value: Self.sorted(entry.value))
        }
        // Sort stably by key
        mapping.entries = entries.enumerated().sorted { lhs, rhs in
          let lkey = sortKey(lhs.element.key)
          let rkey = sortKey(rhs.element.key)
          return lkey == rkey ? lhs.offset < rhs.offset : lkey < rkey
        }.map { $0.element }
        return .mapping(mapping)
    }
  }

  private static func flowed(_ node: YAMLNode) -> YAMLNode {
    switch node {
      case .scalar(_):
        return node
      case .sequence(var sequence):
        sequence.elements = sequence.elements.map(Self.flowed)
        sequence.style = .flow
        return .sequence(sequence)
      case .mapping(var mapping):
        mapping.entries = mapping.entries.map { entry in
          YAMLNode.Mapping.Entry(key: Self.flowed(entry.key), value: Self.flowed(entry.value))
        }
        mapping.style = .flow
        return .mapping(mapping)
    }
  }

  private func serialize(_ nodes: [YAMLNode], _ opts: Options) throws -> String {
    var nodes = nodes
    if opts.sortKeys {
      nodes = nodes.map(Self.sorted)
    }
    if opts.flowStyle {
      nodes = nodes.map(Self.flowed)
    }
    return try self.guarded(opts) { try YAML.serialize(documents: nodes, options: opts.serialize) }
  }

  private func yamlToString(expr: Expr, options: Expr?) throws -> Expr {
    let node = try self.toYAML(expr)
    let opts = try self.options(from: options)
    return .makeString(try self.serialize([node], opts))
  }

  private func yamlToBytevector(expr: Expr, options: Expr?) throws -> Expr {
    let node = try self.toYAML(expr)
    let opts = try self.options(from: options)
    return .bytes(MutableBox(Array(try self.serialize([node], opts).utf8)))
  }

  private func saveYaml(filename: Expr, expr: Expr, options: Expr?) throws -> Expr {
    let node = try self.toYAML(expr)
    let opts = try self.options(from: options)
    try self.writeFile(filename, try self.serialize([node], opts))
    return .void
  }

  private func saveYamlDocuments(filename: Expr, expr: Expr, options: Expr?) throws -> Expr {
    let nodes = try self.nodes(from: expr)
    let opts = try self.options(from: options)
    try self.writeFile(filename, try self.serialize(nodes, opts))
    return .void
  }

  private func nodes(from expr: Expr) throws -> [YAMLNode] {
    var res: [YAMLNode] = []
    switch expr {
      case .vector(let col):
        for elem in col.exprs {
          res.append(try self.toYAML(elem))
        }
      default:
        var lst = expr
        while case .pair(let car, let cdr) = lst {
          res.append(try self.toYAML(car))
          lst = cdr
        }
        guard case .null = lst else {
          throw RuntimeError.type(expr, expected: [.properListType])
        }
    }
    return res
  }

  private func yamlDocumentsToString(expr: Expr, options: Expr?) throws -> Expr {
    let nodes = try self.nodes(from: expr)
    let opts = try self.options(from: options)
    return .makeString(try self.serialize(nodes, opts))
  }

  private func yamlDocumentsToBytevector(expr: Expr, options: Expr?) throws -> Expr {
    let nodes = try self.nodes(from: expr)
    let opts = try self.options(from: options)
    return .bytes(MutableBox(Array(try self.serialize(nodes, opts).utf8)))
  }

  // MARK: - Events

  private func event(from expr: Expr) throws -> YAMLEvent {
    guard case .object(let obj) = expr, let event = obj as? NativeYAMLEvent else {
      throw RuntimeError.type(expr, expected: [NativeYAMLEvent.type])
    }
    return event.value
  }

  private func isYamlEvent(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr else {
      return .false
    }
    return .makeBoolean(obj is NativeYAMLEvent)
  }

  private func optionalString(_ expr: Expr?) throws -> String? {
    guard let expr, !expr.isFalse else {
      return nil
    }
    return try expr.asString()
  }

  private func optionalTag(_ expr: Expr?) throws -> YAMLTag? {
    guard let expr, !expr.isFalse else {
      return nil
    }
    return try Self.parseTag(try expr.asString(), expr)
  }

  private func makeYamlEvent(kind: Expr, args: Arguments) throws -> Expr {
    let name = try kind.asSymbol().identifier
    func arity(_ min: Int, _ max: Int) throws {
      guard args.count >= min && args.count <= max else {
        throw RuntimeError.argumentCount(of: "make-yaml-event",
                                         min: min + 1,
                                         max: max + 1,
                                         args: .pair(kind, .makeList(args)))
      }
    }
    func arg(_ index: Int) -> Expr? {
      return args.count > index ? args[args.startIndex + index] : nil
    }
    let event: YAMLEvent
    switch name {
      case "stream-start":
        try arity(0, 0)
        event = YAMLEvent(.streamStart)
      case "stream-end":
        try arity(0, 0)
        event = YAMLEvent(.streamEnd)
      case "document-start":
        try arity(0, 3)
        var version: YAMLVersion? = nil
        if let ver = arg(1), !ver.isFalse {
          guard case .pair(.fixnum(let major), .fixnum(let minor)) = ver else {
            throw Self.invalid("not a valid YAML version", ver)
          }
          version = YAMLVersion(major: Int(major), minor: Int(minor))
        }
        var directives: [TagDirective] = []
        if let dirs = arg(2) {
          var lst = dirs
          while case .pair(let entry, let rest) = lst {
            guard case .pair(let handle, let prefix) = entry else {
              throw RuntimeError.type(entry, expected: [.pairType])
            }
            directives.append(TagDirective(handle: try handle.asString(),
                                           prefix: try prefix.asString()))
            lst = rest
          }
          guard lst.isNull else {
            throw RuntimeError.type(dirs, expected: [.assocListType])
          }
        }
        event = YAMLEvent(.documentStart(version: version,
                                         tagDirectives: directives,
                                         isImplicit: arg(0)?.isTrue ?? true))
      case "document-end":
        try arity(0, 1)
        event = YAMLEvent(.documentEnd(isImplicit: arg(0)?.isTrue ?? true))
      case "alias":
        try arity(1, 1)
        event = YAMLEvent(.alias(anchor: try arg(0)!.asString()))
      case "scalar":
        try arity(1, 4)
        event = YAMLEvent(.scalar(value: try arg(0)!.asString(),
                                  style: try arg(1).flatMap(Self.scalarStyle) ?? .plain,
                                  tag: try self.optionalTag(arg(2)),
                                  anchor: try self.optionalString(arg(3))))
      case "sequence-start":
        try arity(0, 3)
        event = YAMLEvent(.sequenceStart(style: try arg(0).flatMap(Self.collectionStyle) ?? .block,
                                         tag: try self.optionalTag(arg(1)),
                                         anchor: try self.optionalString(arg(2))))
      case "sequence-end":
        try arity(0, 0)
        event = YAMLEvent(.sequenceEnd)
      case "mapping-start":
        try arity(0, 3)
        event = YAMLEvent(.mappingStart(style: try arg(0).flatMap(Self.collectionStyle) ?? .block,
                                        tag: try self.optionalTag(arg(1)),
                                        anchor: try self.optionalString(arg(2))))
      case "mapping-end":
        try arity(0, 0)
        event = YAMLEvent(.mappingEnd)
      default:
        throw Self.invalid("not a valid YAML event kind", kind)
    }
    return .object(NativeYAMLEvent(event))
  }

  private func yamlEventKind(expr: Expr) throws -> Expr {
    let name: String
    switch try self.event(from: expr).kind {
      case .streamStart:
        name = "stream-start"
      case .streamEnd:
        name = "stream-end"
      case .documentStart(_, _, _):
        name = "document-start"
      case .documentEnd(_):
        name = "document-end"
      case .alias(_):
        name = "alias"
      case .scalar(_, _, _, _):
        name = "scalar"
      case .sequenceStart(_, _, _):
        name = "sequence-start"
      case .sequenceEnd:
        name = "sequence-end"
      case .mappingStart(_, _, _):
        name = "mapping-start"
      case .mappingEnd:
        name = "mapping-end"
    }
    return .symbol(self.context.symbols.intern(name))
  }

  private func yamlEventValue(expr: Expr) throws -> Expr {
    switch try self.event(from: expr).kind {
      case .scalar(let value, _, _, _):
        return .makeString(value)
      case .alias(let anchor):
        return .makeString(anchor)
      default:
        return .false
    }
  }

  private func yamlEventStyle(expr: Expr) throws -> Expr {
    switch try self.event(from: expr).kind {
      case .scalar(_, let style, _, _):
        return self.styleExpr(style)
      case .sequenceStart(let style, _, _), .mappingStart(let style, _, _):
        return self.styleExpr(style)
      default:
        return .false
    }
  }

  private func yamlEventTag(expr: Expr) throws -> Expr {
    switch try self.event(from: expr).kind {
      case .scalar(_, _, let tag, _),
           .sequenceStart(_, let tag, _),
           .mappingStart(_, let tag, _):
        if let tag {
          return .makeString(tag.description)
        }
        return .false
      default:
        return .false
    }
  }

  private func yamlEventAnchor(expr: Expr) throws -> Expr {
    switch try self.event(from: expr).kind {
      case .scalar(_, _, _, let anchor),
           .sequenceStart(_, _, let anchor),
           .mappingStart(_, _, let anchor):
        if let anchor {
          return .makeString(anchor)
        }
        return .false
      default:
        return .false
    }
  }

  private func isYamlEventImplicit(expr: Expr) throws -> Expr {
    switch try self.event(from: expr).kind {
      case .documentStart(_, _, let isImplicit), .documentEnd(let isImplicit):
        return .makeBoolean(isImplicit)
      default:
        return .false
    }
  }

  private func yamlEventVersion(expr: Expr) throws -> Expr {
    if case .documentStart(let version, _, _) = try self.event(from: expr).kind,
       let version {
      return .pair(.fixnum(Int64(version.major)), .fixnum(Int64(version.minor)))
    }
    return .false
  }

  private func yamlEventTagDirectives(expr: Expr) throws -> Expr {
    var res: Expr = .null
    if case .documentStart(_, let directives, _) = try self.event(from: expr).kind {
      for directive in directives.reversed() {
        res = .pair(.pair(.makeString(directive.handle), .makeString(directive.prefix)), res)
      }
    }
    return res
  }

  private func yamlEventStart(expr: Expr) throws -> Expr {
    return Self.markExpr(try self.event(from: expr).start)
  }

  private func yamlEventEnd(expr: Expr) throws -> Expr {
    return Self.markExpr(try self.event(from: expr).end)
  }

  private func stringToYamlEvents(expr: Expr, options: Expr?) throws -> Expr {
    let text = try expr.asString()
    let opts = try self.options(from: options)
    return try self.parseEvents(opts) {
      try YAMLParser(string: text, maximumDepth: opts.parse.maximumDepth)
    }
  }

  private func bytevectorToYamlEvents(expr: Expr, options: Expr?) throws -> Expr {
    let data = try self.bytes(expr)
    let opts = try self.options(from: options)
    return try self.parseEvents(opts) {
      try YAMLParser(data: data, maximumDepth: opts.parse.maximumDepth)
    }
  }

  private func parseEvents(_ opts: Options, _ make: () throws -> YAMLParser) throws -> Expr {
    let events = try self.guarded(opts) {
      var parser = try make()
      return try parser.parseAll()
    }
    return .makeList(Exprs(events.map { Expr.object(NativeYAMLEvent($0)) }))
  }

  private func events(from expr: Expr) throws -> [YAMLEvent] {
    var res: [YAMLEvent] = []
    switch expr {
      case .vector(let col):
        for elem in col.exprs {
          res.append(try self.event(from: elem))
        }
      default:
        var lst = expr
        while case .pair(let car, let cdr) = lst {
          res.append(try self.event(from: car))
          lst = cdr
        }
        guard case .null = lst else {
          throw RuntimeError.type(expr, expected: [.properListType])
        }
    }
    return res
  }

  private func yamlEventsToString(expr: Expr, options: Expr?) throws -> Expr {
    let events = try self.events(from: expr)
    let opts = try self.options(from: options)
    return .makeString(try self.guarded(opts) {
      try YAML.emit(events, options: opts.serialize.formatting)
    })
  }

  // MARK: - Parsers and emitters

  private func isYamlParser(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr else {
      return .false
    }
    return .makeBoolean(obj is NativeYAMLParser)
  }

  private func parser(from expr: Expr) throws -> NativeYAMLParser {
    guard case .object(let obj) = expr, let parser = obj as? NativeYAMLParser else {
      throw RuntimeError.type(expr, expected: [NativeYAMLParser.type])
    }
    return parser
  }

  private func makeYamlParser(source: Expr, options: Expr?) throws -> Expr {
    let opts = try self.options(from: options)
    switch source {
      case .string(let str):
        let text = str as String
        return .object(NativeYAMLParser(try self.guarded(opts) {
          try YAMLParser(string: text, maximumDepth: opts.parse.maximumDepth)
        }))
      case .bytes(let bytes):
        let data = Data(bytes.value)
        return .object(NativeYAMLParser(try self.guarded(opts) {
          try YAMLParser(data: data, maximumDepth: opts.parse.maximumDepth)
        }))
      default:
        throw RuntimeError.type(source, expected: [.strType, .byteVectorType])
    }
  }

  private func yamlParserNext(expr: Expr) throws -> Expr {
    let parser = try self.parser(from: expr)
    guard let event = try self.guarded(nil, { try parser.parser.next() }) else {
      return .eof
    }
    return .object(NativeYAMLEvent(event))
  }

  private func isYamlEmitter(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr else {
      return .false
    }
    return .makeBoolean(obj is NativeYAMLEmitter)
  }

  private func emitter(from expr: Expr) throws -> NativeYAMLEmitter {
    guard case .object(let obj) = expr, let emitter = obj as? NativeYAMLEmitter else {
      throw RuntimeError.type(expr, expected: [NativeYAMLEmitter.type])
    }
    return emitter
  }

  private func makeYamlEmitter(options: Expr?) throws -> Expr {
    let opts = try self.options(from: options)
    return .object(NativeYAMLEmitter(YAMLEmitter(options: opts.serialize.formatting)))
  }

  private func yamlEmitterEmit(expr: Expr, args: Arguments) throws -> Expr {
    let emitter = try self.emitter(from: expr)
    for arg in args {
      let event = try self.event(from: arg)
      try self.guarded(nil) { try emitter.emitter.emit(event) }
    }
    return .void
  }

  private func yamlEmitterOutput(expr: Expr) throws -> Expr {
    return .makeString(try self.emitter(from: expr).emitter.output)
  }
}

// MARK: - Native object types

/// An immutable YAML node.
public final class NativeYAML: AnyNativeObject<YAMLNode> {
  public static let type = Type.objectType(Symbol(uninterned: "yaml"))

  public override var type: Type {
    return Self.type
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }

  public override var tagString: String {
    return "yaml \(YamlLibrary.nodeDescription(self.value))"
  }

  public override func unpack(in context: Context) -> Exprs {
    return [.makeString(self.identityString),
            .makeString(YamlLibrary.nodeDescription(self.value))]
  }
}

/// A mutable YAML node.
public final class MutableYAML: AnyMutableNativeObject<YAMLNode> {
  public static let type = Type.objectType(Symbol(uninterned: "mutable-yaml"))

  public override var type: Type {
    return Self.type
  }

  public override var string: String {
    return "#<mutable-yaml \(self.identityString) \(YamlLibrary.nodeDescription(self.value))>"
  }

  public override var tagString: String {
    return "mutable-yaml \(YamlLibrary.nodeDescription(self.value))"
  }

  public override func equals(_ obj: NativeObject) -> Bool {
    return self === obj
  }

  public override func unpack(in context: Context) -> Exprs {
    return [.makeString(self.identityString),
            .makeString(YamlLibrary.nodeDescription(self.value))]
  }
}

/// A YAML event.
public final class NativeYAMLEvent: AnyNativeObject<YAMLEvent> {
  public static let type = Type.objectType(Symbol(uninterned: "yaml-event"))

  public override var type: Type {
    return Self.type
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }

  public override var tagString: String {
    switch self.value.kind {
      case .streamStart:
        return "yaml-event stream-start"
      case .streamEnd:
        return "yaml-event stream-end"
      case .documentStart(_, _, _):
        return "yaml-event document-start"
      case .documentEnd(_):
        return "yaml-event document-end"
      case .alias(let anchor):
        return "yaml-event alias \(anchor)"
      case .scalar(let value, _, _, _):
        let text = value.replacingOccurrences(of: "\n", with: "\\n")
        return "yaml-event scalar \"\(text.count > 32 ? String(text.prefix(32)) + "…" : text)\""
      case .sequenceStart(_, _, _):
        return "yaml-event sequence-start"
      case .sequenceEnd:
        return "yaml-event sequence-end"
      case .mappingStart(_, _, _):
        return "yaml-event mapping-start"
      case .mappingEnd:
        return "yaml-event mapping-end"
    }
  }
}

/// A pull parser producing YAML events.
public final class NativeYAMLParser: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "yaml-parser"))

  fileprivate var parser: YAMLParser

  fileprivate init(_ parser: YAMLParser) {
    self.parser = parser
    super.init()
  }

  public override var type: Type {
    return Self.type
  }
}

/// A push emitter consuming YAML events.
public final class NativeYAMLEmitter: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "yaml-emitter"))

  fileprivate var emitter: YAMLEmitter

  fileprivate init(_ emitter: YAMLEmitter) {
    self.emitter = emitter
    super.init()
  }

  public override var type: Type {
    return Self.type
  }
}
