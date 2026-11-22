//
//  FoundationModelSupport.swift
//  LispKit
//
//  Created by Matthias Zenger on 19/11/2026.
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

#if canImport(FoundationModels)

import Foundation
import CoreGraphics
import UniformTypeIdentifiers
import DynamicJSON
import FoundationModels
#if os(macOS)
import AppKit
#elseif os(iOS)
import UIKit
#endif

// MARK: - Language models

/// FoundationModels instructions; LispKit defines its own `Instructions` type for bytecode.
@available(macOS 27.0, iOS 27.0, *)
typealias FMInstructions = FoundationModels.Instructions

/// What `language-model-token-count` counts the tokens of. Prompts and
/// instructions come with their transcript segments, if they can be represented
/// as such: FoundationModels' `Prompt` and `Instructions` are opaque, and models
/// that count tokens by transcript need the segments.
@available(macOS 27.0, iOS 27.0, *)
public enum LanguageModelTokenInput: Sendable {
  case prompt(Prompt, segments: [Transcript.Segment]?)
  case instructions(FoundationModels.Instructions, segments: [Transcript.Segment]?)
  case tools([any Tool])
  case schema(GenerationSchema)
  case transcript(Transcript)
}

/// The usage quota of a language model, as returned by `language-model-quota-usage`.
@available(macOS 27.0, iOS 27.0, *)
public struct LanguageModelQuotaUsage: Sendable {
  public typealias LimitIncreaseSuggestion =
    PrivateCloudComputeLanguageModel.QuotaUsage.LimitIncreaseSuggestion

  /// One of `below-limit`, `approaching-limit`, or `limit-reached`.
  public let status: String
  public let resetDate: Date?
  public let limitIncreaseSuggestion: LimitIncreaseSuggestion?

  public init(status: String, resetDate: Date?, limitIncreaseSuggestion: LimitIncreaseSuggestion?) {
    self.status = status
    self.resetDate = resetDate
    self.limitIncreaseSuggestion = limitIncreaseSuggestion
  }
}

///
/// A language model integration. Each family of language models (the on-device model,
/// Private Cloud Compute, Claude, Gemini, ...) provides a backend, which determines the
/// results of the model-level procedures of `(lispkit foundation-model)`, such as
/// `language-model-name` or `language-model-context-size`.
///
/// All queries are asynchronous; the library waits for their results. A query returning
/// `nil` is not supported by the model: procedures return `#f` in this case, except for
/// predicates, which raise a model error of kind `unsupported`. The default implementations
/// return `nil` and report the model as `available`.
///
@available(macOS 27.0, iOS 27.0, *)
public protocol LanguageModelBackend: Sendable {

  /// The language model used for sessions.
  var model: any LanguageModel { get }

  /// The family of the model, returned as a symbol by `language-model-family`.
  var family: String { get }

  /// The name of the model, returned by `language-model-name`. It identifies the model
  /// among the models of its family, e.g. the model identifier for Claude and Gemini models.
  /// The names of the available models of a family are registered via
  /// `LanguageModelFamilies`. The default implementation returns the family.
  var name: String { get }

  /// The availability of the model: `available` or a reason why the model is unavailable.
  func availability() async throws -> String

  /// The size of the context window in tokens.
  func contextSize() async throws -> Int?

  /// The languages supported by the model.
  func supportedLanguages() async throws -> Set<Locale.Language>?

  /// Whether the model supports `locale`.
  func supportsLocale(_ locale: Locale) async throws -> Bool?

  /// The number of tokens `input` uses.
  func tokenCount(for input: LanguageModelTokenInput) async throws -> Int?

  /// The usage quota of the model.
  func quotaUsage() async throws -> LanguageModelQuotaUsage?
}

@available(macOS 27.0, iOS 27.0, *)
extension LanguageModelBackend {

  public var name: String {
    return self.family
  }

  public func availability() async throws -> String {
    return "available"
  }

  public func contextSize() async throws -> Int? {
    return nil
  }

  public func supportedLanguages() async throws -> Set<Locale.Language>? {
    return nil
  }

  public func supportsLocale(_ locale: Locale) async throws -> Bool? {
    return nil
  }

  public func tokenCount(for input: LanguageModelTokenInput) async throws -> Int? {
    return nil
  }

  public func quotaUsage() async throws -> LanguageModelQuotaUsage? {
    return nil
  }
}

/// The backend of Apple's on-device language model.
@available(macOS 27.0, iOS 27.0, *)
public struct SystemModelBackend: LanguageModelBackend {
  public let system: SystemLanguageModel

  public init(_ system: SystemLanguageModel) {
    self.system = system
  }

  public var model: any LanguageModel {
    return self.system
  }

  public var family: String {
    return "system"
  }

  /// The name of the model variant the device uses, e.g. "AFM 3 Core Advanced". The
  /// variant is determined by the device; it can't be chosen.
  public var name: String {
    return self.system.variant.displayName
  }

  public func availability() async throws -> String {
    switch self.system.availability {
      case .available:
        return "available"
      case .unavailable(let reason):
        switch reason {
          case .deviceNotEligible:
            return "device-not-eligible"
          case .appleIntelligenceNotEnabled:
            return "apple-intelligence-not-enabled"
          case .modelNotReady:
            return "model-not-ready"
          @unknown default:
            return "unavailable"
        }
    }
  }

  public func contextSize() async throws -> Int? {
    return self.system.contextSize
  }

  public func supportedLanguages() async throws -> Set<Locale.Language>? {
    return self.system.supportedLanguages
  }

  public func supportsLocale(_ locale: Locale) async throws -> Bool? {
    return self.system.supportsLocale(locale)
  }

  public func tokenCount(for input: LanguageModelTokenInput) async throws -> Int? {
    switch input {
      case .prompt(let prompt, _):
        return try await self.system.tokenCount(for: prompt)
      case .instructions(let instructions, _):
        return try await self.system.tokenCount(for: instructions)
      case .tools(let tools):
        return try await self.system.tokenCount(for: tools)
      case .schema(let schema):
        return try await self.system.tokenCount(for: schema)
      case .transcript(let transcript):
        return try await self.system.tokenCount(for: transcript)
    }
  }
}

/// The backend of Apple's language model running on Private Cloud Compute.
@available(macOS 27.0, iOS 27.0, *)
public struct PrivateCloudComputeModelBackend: LanguageModelBackend {
  public let pcc: PrivateCloudComputeLanguageModel

  public init(_ pcc: PrivateCloudComputeLanguageModel) {
    self.pcc = pcc
  }

  public var model: any LanguageModel {
    return self.pcc
  }

  public var family: String {
    return "afm-cloud"
  }

  public func availability() async throws -> String {
    switch self.pcc.availability {
      case .available:
        return "available"
      case .unavailable(let reason):
        switch reason {
          case .deviceNotEligible:
            return "device-not-eligible"
          case .systemNotReady:
            return "system-not-ready"
          @unknown default:
            return "unavailable"
        }
    }
  }

  public func contextSize() async throws -> Int? {
    return try await self.pcc.contextSize
  }

  public func supportedLanguages() async throws -> Set<Locale.Language>? {
    return try await self.pcc.supportedLanguages
  }

  public func supportsLocale(_ locale: Locale) async throws -> Bool? {
    return try await self.pcc.supportsLocale(locale)
  }

  public func quotaUsage() async throws -> LanguageModelQuotaUsage? {
    let usage = self.pcc.quotaUsage
    let status: String
    switch usage.status {
      case .belowLimit(let below):
        status = below.isApproachingLimit ? "approaching-limit" : "below-limit"
      case .limitReached(_):
        status = "limit-reached"
      @unknown default:
        status = usage.isLimitReached ? "limit-reached" : "below-limit"
    }
    return LanguageModelQuotaUsage(status: status,
                                   resetDate: usage.resetDate,
                                   limitIncreaseSuggestion: usage.limitIncreaseSuggestion)
  }
}

///
/// The registry of the families of language models. A library providing a family of
/// language models registers the family together with its vendor (returned by
/// `language-model-family-vendor`) and a procedure which returns the names of the models
/// of this family that are available (returned by `available-language-models`).
/// Registering a family again replaces the previous registration.
///
@available(macOS 27.0, iOS 27.0, *)
public enum LanguageModelFamilies {
  public typealias Provider = @Sendable () async throws -> [String]

  private struct Registration {
    let vendor: String
    let provider: Provider
  }

  private static let lock = NSLock()
  nonisolated(unsafe) private static var registrations: [String : Registration] = [:]

  /// Registers family `family` of vendor `vendor` (e.g. "Apple") with `provider` returning
  /// the names of the available models.
  public static func register(family: String,
                              vendor: String,
                              models provider: @escaping Provider) {
    Self.lock.withLock {
      Self.registrations[family] = Registration(vendor: vendor, provider: provider)
    }
  }

  /// Returns the provider of the available models of family `family`, if it is registered.
  public static func provider(for family: String) -> Provider? {
    return Self.lock.withLock {
      Self.registrations[family]?.provider
    }
  }

  /// Returns the vendor of family `family`, if it is registered.
  public static func vendor(for family: String) -> String? {
    return Self.lock.withLock {
      Self.registrations[family]?.vendor
    }
  }

  /// The registered families, sorted alphabetically.
  public static var families: [String] {
    return Self.lock.withLock {
      Self.registrations.keys.sorted()
    }
  }
}

///
/// A language model: Apple's on-device model, Private Cloud Compute, or a model provided
/// by another library, e.g. `(lispkit foundation-model claude)` or
/// `(lispkit foundation-model gemini)`. The model's backend implements the model-level
/// queries.
///
@available(macOS 27.0, iOS 27.0, *)
public final class NativeLanguageModel: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "language-model"))

  public let backend: any LanguageModelBackend
  public let label: String

  public init(_ backend: any LanguageModelBackend, label: String) {
    self.backend = backend
    self.label = label
  }

  /// The language model used for sessions.
  public var model: any LanguageModel {
    return self.backend.model
  }

  public var family: String {
    return self.backend.family
  }

  public override var type: Type {
    return Self.type
  }

  public override var tagString: String {
    return "\(Self.type) \(self.identityString): \(self.label)"
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }
}

// MARK: - Generation schemas

///
/// The shape of generated content as far as it matters for converting it into Scheme
/// values: numbers declared as integers become exact integers, and the properties of
/// objects have a declared order (used for passing tool arguments positionally).
///
@available(macOS 27.0, iOS 27.0, *)
indirect enum ContentShape {
  case integer
  case number
  case other
  case array(ContentShape)
  case object([(name: String, shape: ContentShape, optional: Bool)])
  case reference(String)
}

/// The shape of a schema and the shapes of the named schemas it refers to.
@available(macOS 27.0, iOS 27.0, *)
struct SchemaShapes {
  let root: ContentShape
  let named: [String : ContentShape]

  func resolve(_ shape: ContentShape?) -> ContentShape? {
    var shape = shape
    var depth = 0
    while case .some(.reference(let name)) = shape, depth < 32 {
      shape = self.named[name]
      depth += 1
    }
    if case .some(.reference(_)) = shape {
      return nil
    }
    return shape
  }

  /// The properties of the root schema if it is an object schema.
  var properties: [(name: String, shape: ContentShape, optional: Bool)]? {
    if case .some(.object(let props)) = self.resolve(self.root) {
      return props
    }
    return nil
  }
}

///
/// A generation schema used for guided generation and for describing tool parameters.
///
@available(macOS 27.0, iOS 27.0, *)
public final class NativeGenerationSchema: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "generation-schema"))

  public let schema: GenerationSchema
  let shapes: SchemaShapes?

  init(_ schema: GenerationSchema, shapes: SchemaShapes?) {
    self.schema = schema
    self.shapes = shapes
  }

  public override var type: Type {
    return Self.type
  }

  public override var tagString: String {
    return "\(Self.type) \(self.identityString): \(self.schema.name)"
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }
}

///
/// Compiles schema specifications, i.e. s-expressions, into dynamic generation schemas.
///
@available(macOS 27.0, iOS 27.0, *)
struct SchemaCompiler {
  let context: Context
  var named: [String : ContentShape] = [:]

  init(in context: Context) {
    self.context = context
  }

  static func invalid(_ reason: String, _ expr: Expr) -> RuntimeError {
    return RuntimeError.custom("error", "\(reason): $0", [expr])
  }

  static func name(_ expr: Expr) throws -> String {
    switch expr {
      case .symbol(let sym):
        return sym.identifier
      case .string(let str):
        return str as String
      default:
        throw RuntimeError.type(expr, expected: [.symbolType, .strType])
    }
  }

  /// Compiles a schema with its dependencies into a `NativeGenerationSchema`.
  static func schema(from root: Expr,
                     dependencies: Arguments,
                     in context: Context) throws -> NativeGenerationSchema {
    var compiler = SchemaCompiler(in: context)
    var deps: [DynamicGenerationSchema] = []
    for dep in dependencies {
      deps.append(try compiler.compile(dep).schema)
    }
    let (dynRoot, rootShape) = try compiler.compile(root)
    do {
      let schema = try GenerationSchema(root: dynRoot, dependencies: deps)
      return NativeGenerationSchema(schema, shapes: SchemaShapes(root: rootShape,
                                                                 named: compiler.named))
    } catch {
      throw ModelErrors.translate(error, in: context)
    }
  }

  /// Returns the schema denoted by `expr`, which is either a schema object or a schema
  /// specification.
  static func schema(from expr: Expr, in context: Context) throws -> NativeGenerationSchema {
    if case .object(let obj) = expr, let schema = obj as? NativeGenerationSchema {
      return schema
    }
    return try Self.schema(from: expr, dependencies: [], in: context)
  }

  /// Splits `(name [description] rest ...)` into its components.
  private func header(_ args: Expr, _ spec: Expr) throws -> (String, String?, Expr) {
    guard case .pair(let nm, let rest) = args else {
      throw Self.invalid("schema name missing", spec)
    }
    let name = try Self.name(nm)
    if case .pair(.string(let descr), let rest2) = rest {
      return (name, descr as String, rest2)
    }
    return (name, nil, rest)
  }

  private func list(_ expr: Expr, _ spec: Expr) throws -> [Expr] {
    var res: [Expr] = []
    var lst = expr
    while case .pair(let head, let tail) = lst {
      res.append(head)
      lst = tail
    }
    guard lst.isNull else {
      throw Self.invalid("malformed schema", spec)
    }
    return res
  }

  private func guides<T: Generable>(_ args: Expr,
                                    _ spec: Expr,
                                    _ convert: (Expr) throws -> T,
                                    _ make: (String, [T]) throws -> GenerationGuide<T>?)
                                    throws -> [GenerationGuide<T>] {
    var res: [GenerationGuide<T>] = []
    for guide in try self.list(args, spec) {
      guard case .pair(.symbol(let kind), let rest) = guide else {
        throw Self.invalid("invalid schema guide", guide)
      }
      let vals = try self.list(rest, guide).map(convert)
      guard let g = try make(kind.identifier, vals) else {
        throw Self.invalid("invalid schema guide", guide)
      }
      res.append(g)
    }
    return res
  }

  private func numericGuide<T: Comparable>(_ kind: String,
                                           _ vals: [T],
                                           minimum: (T) -> GenerationGuide<T>,
                                           maximum: (T) -> GenerationGuide<T>,
                                           range: (ClosedRange<T>) -> GenerationGuide<T>)
                                           -> GenerationGuide<T>? {
    switch (kind, vals.count) {
      case ("minimum", 1):
        return minimum(vals[0])
      case ("maximum", 1):
        return maximum(vals[0])
      case ("range", 2) where vals[0] <= vals[1]:
        return range(vals[0]...vals[1])
      default:
        return nil
    }
  }

  /// Compiles a schema specification.
  mutating func compile(_ spec: Expr) throws -> (schema: DynamicGenerationSchema,
                                                 shape: ContentShape) {
    switch spec {
      case .symbol(let sym):
        switch sym.identifier {
          case "string":
            return (DynamicGenerationSchema(type: String.self), .other)
          case "integer":
            return (DynamicGenerationSchema(type: Int.self), .integer)
          case "number":
            return (DynamicGenerationSchema(type: Double.self), .number)
          case "boolean":
            return (DynamicGenerationSchema(type: Bool.self), .other)
          case "null":
            return (DynamicGenerationSchema.null, .other)
          default:
            return (DynamicGenerationSchema(referenceTo: sym.identifier),
                    .reference(sym.identifier))
        }
      case .pair(.symbol(let head), let args):
        switch head.identifier {
          case "string":
            let gs = try self.guides(args, spec, { try $0.asString() }) { kind, vals in
              switch (kind, vals.count) {
                case ("constant", 1):
                  return .constant(vals[0])
                case ("any-of", let n) where n > 0:
                  return .anyOf(vals)
                case ("pattern", 1):
                  do {
                    return .pattern(try Regex(vals[0]))
                  } catch {
                    throw Self.invalid("invalid regular expression", .makeString(vals[0]))
                  }
                default:
                  return nil
              }
            }
            return (DynamicGenerationSchema(type: String.self, guides: gs), .other)
          case "integer":
            let gs = try self.guides(args, spec, { try $0.asInt(above: Int.min) }) { kind, vals in
              self.numericGuide(kind, vals, minimum: { .minimum($0) },
                                maximum: { .maximum($0) }, range: { .range($0) })
            }
            return (DynamicGenerationSchema(type: Int.self, guides: gs), .integer)
          case "number":
            let gs = try self.guides(args, spec, { try $0.asDouble(coerce: true) }) { kind, vals in
              self.numericGuide(kind, vals, minimum: { .minimum($0) },
                                maximum: { .maximum($0) }, range: { .range($0) })
            }
            return (DynamicGenerationSchema(type: Double.self, guides: gs), .number)
          case "array":
            let elems = try self.list(args, spec)
            guard elems.count >= 1 && elems.count <= 3 else {
              throw Self.invalid("invalid array schema", spec)
            }
            let (item, shape) = try self.compile(elems[0])
            let minimum = elems.count > 1 && elems[1].isTrue ? try elems[1].asInt() : nil
            let maximum = elems.count > 2 && elems[2].isTrue ? try elems[2].asInt() : nil
            if let minimum, let maximum, minimum > maximum {
              throw Self.invalid("invalid array bounds", spec)
            }
            return (DynamicGenerationSchema(arrayOf: item,
                                            minimumElements: minimum,
                                            maximumElements: maximum), .array(shape))
          case "object":
            let (name, descr, rest) = try self.header(args, spec)
            var props: [DynamicGenerationSchema.Property] = []
            var shapes: [(name: String, shape: ContentShape, optional: Bool)] = []
            var explicitNil = false
            for prop in try self.list(rest, spec) {
              // The symbol `explicit-nil` makes optional properties without value explicit
              // in generated content, i.e. they get value `null` instead of being omitted.
              if case .symbol(let sym) = prop, sym.identifier == "explicit-nil" {
                explicitNil = true
                continue
              }
              let elems = try self.list(prop, prop)
              guard elems.count >= 2 && elems.count <= 3 else {
                throw Self.invalid("invalid property", prop)
              }
              let pname = try Self.name(elems[0])
              var pspec = elems[1]
              var optional = false
              if case .pair(.symbol(let opt), .pair(let inner, .null)) = pspec,
                 opt.identifier == "optional" {
                pspec = inner
                optional = true
              }
              let pdescr = elems.count > 2 ? try elems[2].asString() : nil
              let (pschema, pshape) = try self.compile(pspec)
              props.append(DynamicGenerationSchema.Property(name: pname,
                                                            description: pdescr,
                                                            schema: pschema,
                                                            isOptional: optional))
              shapes.append((pname, pshape, optional))
            }
            let shape = ContentShape.object(shapes)
            self.named[name] = shape
            return (explicitNil ?
                      DynamicGenerationSchema(name: name,
                                              description: descr,
                                              representNilExplicitlyInGeneratedContent: true,
                                              properties: props) :
                      DynamicGenerationSchema(name: name, description: descr, properties: props),
                    shape)
          case "enum":
            let (name, descr, rest) = try self.header(args, spec)
            guard case .pair(let choices, .null) = rest else {
              throw Self.invalid("enum schema requires a list of choices", spec)
            }
            let strs = try self.list(choices, spec).map { try $0.asString() }
            guard !strs.isEmpty else {
              throw Self.invalid("enum schema without choices", spec)
            }
            self.named[name] = .other
            return (DynamicGenerationSchema(name: name, description: descr, anyOf: strs), .other)
          case "any-of":
            let (name, descr, rest) = try self.header(args, spec)
            var choices: [DynamicGenerationSchema] = []
            for choice in try self.list(rest, spec) {
              choices.append(try self.compile(choice).schema)
            }
            guard !choices.isEmpty else {
              throw Self.invalid("any-of schema without choices", spec)
            }
            self.named[name] = .other
            return (DynamicGenerationSchema(name: name, description: descr, anyOf: choices),
                    .other)
          case "ref":
            guard case .pair(let nm, .null) = args else {
              throw Self.invalid("invalid schema reference", spec)
            }
            let name = try Self.name(nm)
            return (DynamicGenerationSchema(referenceTo: name), .reference(name))
          case "optional":
            throw Self.invalid("optional schemas are only supported for properties", spec)
          default:
            throw Self.invalid("unknown schema", spec)
        }
      default:
        throw Self.invalid("invalid schema", spec)
    }
  }
}

// MARK: - Generated content

///
/// Conversions between generated content and Scheme values. The mapping follows the
/// conventions of `(lispkit json)`.
///
@available(macOS 27.0, iOS 27.0, *)
enum ContentConverter {

  static func expr(from content: GeneratedContent,
                   shape: ContentShape? = nil,
                   shapes: SchemaShapes? = nil,
                   in context: Context) -> Expr {
    let shape = shapes?.resolve(shape) ?? shape
    switch content.kind {
      case .null:
        return .symbol(context.symbols.null)
      case .bool(let bool):
        return .makeBoolean(bool)
      case .number(let num):
        switch shape {
          case .some(.number):
            return .flonum(num)
          default:
            if num.isFinite && num == num.rounded() && abs(num) < 9.0e15 {
              return .fixnum(Int64(num))
            }
            return .flonum(num)
        }
      case .string(let str):
        return .makeString(str)
      case .array(let elems):
        var elemShape: ContentShape? = nil
        if case .some(.array(let s)) = shape {
          elemShape = s
        }
        return .vector(Collection(kind: .immutableVector,
                                  exprs: Exprs(elems.map {
                                    Self.expr(from: $0, shape: elemShape, shapes: shapes,
                                              in: context)
                                  })))
      case .structure(properties: let props, orderedKeys: let keys):
        // Properties are ordered as declared by the schema, if available
        var propShapes: [String : ContentShape] = [:]
        var order: [String] = []
        if case .some(.object(let ps)) = shape {
          for p in ps {
            propShapes[p.name] = p.shape
            order.append(p.name)
          }
        }
        var res = Exprs()
        var seen = Set<String>()
        for key in order + keys + props.keys.sorted() where !seen.contains(key) {
          seen.insert(key)
          if let value = props[key] {
            res.append(.pair(.symbol(context.symbols.intern(key)),
                             Self.expr(from: value, shape: propShapes[key], shapes: shapes,
                                       in: context)))
          }
        }
        return .makeList(res)
      @unknown default:
        return .makeString(content.jsonString)
    }
  }

  static func content(from expr: Expr, in context: Context) throws -> GeneratedContent {
    switch expr {
      case .symbol(context.symbols.null):
        return GeneratedContent(kind: .null)
      case .null:
        return GeneratedContent(kind: .structure(properties: [:], orderedKeys: []))
      case .true:
        return GeneratedContent(kind: .bool(true))
      case .false:
        return GeneratedContent(kind: .bool(false))
      case .fixnum(let num):
        return GeneratedContent(kind: .number(Double(num)))
      case .bignum(_), .rational(_, _), .flonum(_):
        return GeneratedContent(kind: .number(try expr.asDouble(coerce: true)))
      case .symbol(let sym):
        return GeneratedContent(kind: .string(sym.identifier))
      case .string(let str):
        return GeneratedContent(kind: .string(str as String))
      case .char(let ch):
        return GeneratedContent(kind: .string(String(utf16CodeUnits: [ch], count: 1)))
      case .vector(let col):
        switch col.kind {
          case .array, .vector, .immutableVector, .growableVector:
            return GeneratedContent(kind: .array(try col.exprs.map {
              try Self.content(from: $0, in: context)
            }))
          default:
            break
        }
      case .pair(_, _):
        var props: [String : GeneratedContent] = [:]
        var keys: [String] = []
        var lst = expr
        while case .pair(.pair(let key, let value), let rest) = lst {
          lst = rest
          if case .void = value {
            continue
          }
          let name: String
          switch key {
            case .symbol(let sym):
              name = sym.identifier
            case .string(let str):
              name = str as String
            default:
              throw RuntimeError.eval(.cannotConvertToJSON, key)
          }
          if props[name] == nil {
            keys.append(name)
          }
          props[name] = try Self.content(from: value, in: context)
        }
        guard lst.isNull else {
          throw RuntimeError.eval(.cannotConvertToJSON, expr)
        }
        return GeneratedContent(kind: .structure(properties: props, orderedKeys: keys))
      default:
        break
    }
    return Self.content(from: try JSONLibrary.toJSON(expr, in: context))
  }

  static func content(from json: JSON) -> GeneratedContent {
    switch json {
      case .null:
        return GeneratedContent(kind: .null)
      case .boolean(let bool):
        return GeneratedContent(kind: .bool(bool))
      case .integer(let num):
        return GeneratedContent(kind: .number(Double(num)))
      case .float(let num):
        return GeneratedContent(kind: .number(num))
      case .string(let str):
        return GeneratedContent(kind: .string(str))
      case .array(let arr):
        return GeneratedContent(kind: .array(arr.map(Self.content(from:))))
      case .object(let dict):
        var props: [String : GeneratedContent] = [:]
        for (key, value) in dict {
          props[key] = Self.content(from: value)
        }
        return GeneratedContent(kind: .structure(properties: props,
                                                 orderedKeys: dict.keys.sorted()))
    }
  }

  /// Converts an association list into a metadata dictionary.
  static func metadata(from expr: Expr, in context: Context) throws -> [String : GeneratedContent] {
    guard expr.isTrue else {
      return [:]
    }
    let content = try Self.content(from: expr, in: context)
    guard case .structure(properties: let props, orderedKeys: _) = content.kind else {
      throw RuntimeError.type(expr, expected: [.assocListType])
    }
    return props
  }

  /// Converts a metadata dictionary into an association list.
  static func expr(from metadata: [String : GeneratedContent], in context: Context) -> Expr {
    var res = Exprs()
    for key in metadata.keys.sorted() {
      res.append(.pair(.symbol(context.symbols.intern(key)),
                       Self.expr(from: metadata[key]!, in: context)))
    }
    return .makeList(res)
  }
}

// MARK: - Prompts, instructions, and attachments

/// Wraps a transcript data attachment so that it can be used with `Attachment`.
@available(macOS 27.2, iOS 27.2, *)
struct RawDataAttachment: DataAttachmentRepresentable {
  let transcriptRepresentation: Transcript.DataAttachment

  init(_ attachment: Transcript.DataAttachment) {
    self.transcriptRepresentation = attachment
  }
}

///
/// An image or data attachment, optionally labeled, that can be included in prompts and
/// instructions.
///
@available(macOS 27.0, iOS 27.0, *)
public final class NativeAttachment: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "attachment"))

  let prompt: Prompt
  let instructions: FMInstructions
  /// The attachment as a transcript segment, if it is available as such.
  let segment: Transcript.Segment?
  let label: String?
  let kind: String

  init(image: CGImage, label: String?) {
    var attachment = Attachment(image)
    if let label {
      attachment = attachment.label(label)
    }
    self.prompt = Prompt(attachment)
    self.instructions = FMInstructions(attachment)
    self.segment = .attachment(Transcript.AttachmentSegment(
                                 content: .image(Transcript.ImageAttachment(image)),
                                 label: label))
    self.label = label
    self.kind = "image"
  }

  init(imageURL url: URL, label: String?) {
    var attachment = Attachment(imageURL: url)
    if let label {
      attachment = attachment.label(label)
    }
    self.prompt = Prompt(attachment)
    self.instructions = FMInstructions(attachment)
    self.segment = nil
    self.label = label
    self.kind = "image"
  }

  @available(macOS 27.2, iOS 27.2, *)
  init(data: Data, contentType: UTType, label: String?) {
    let content = Transcript.DataAttachment(contentType: contentType, content: data)
    var attachment = Attachment(RawDataAttachment(content))
    if let label {
      attachment = attachment.label(label)
    }
    self.prompt = Prompt(attachment)
    self.instructions = FMInstructions(attachment)
    self.segment = .attachment(Transcript.AttachmentSegment(content: .data(content), label: label))
    self.label = label
    self.kind = contentType.identifier
  }

  public override var type: Type {
    return Self.type
  }

  public override var tagString: String {
    if let label {
      return "\(Self.type) \(self.identityString): \(self.kind) \(label)"
    }
    return "\(Self.type) \(self.identityString): \(self.kind)"
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }
}

///
/// A prompt that was converted once from its Scheme representation.
///
@available(macOS 27.0, iOS 27.0, *)
public final class NativePrompt: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "prompt"))

  let prompt: Prompt

  init(_ prompt: Prompt) {
    self.prompt = prompt
  }

  public override var type: Type {
    return Self.type
  }
}

@available(macOS 27.0, iOS 27.0, *)
enum PromptConverter {

  static func cgImage(from expr: Expr) -> CGImage? {
    guard case .object(let obj) = expr, let image = obj as? NativeImage else {
      return nil
    }
    return image.cgImage()
  }

  static func text(from expr: Expr) -> String? {
    switch expr {
      case .string(let str):
        return str as String
      case .char(let ch):
        return String(utf16CodeUnits: [ch], count: 1)
      case .fixnum(let num):
        return String(num)
      case .flonum(let num):
        return String(num)
      case .bignum(_), .rational(_, _):
        return expr.description
      default:
        return nil
    }
  }

  static func prompt(from expr: Expr) throws -> Prompt {
    if case .object(let obj) = expr, let prompt = obj as? NativePrompt {
      return prompt.prompt
    }
    var parts: [Prompt] = []
    try Self.collect(expr, into: &parts)
    return Prompt(parts)
  }

  private static func collect(_ expr: Expr, into parts: inout [Prompt]) throws {
    if let text = Self.text(from: expr) {
      parts.append(Prompt(text))
      return
    }
    switch expr {
      case .null:
        return
      case .pair(let head, let tail):
        try Self.collect(head, into: &parts)
        try Self.collect(tail, into: &parts)
      case .object(let obj):
        if let prompt = obj as? NativePrompt {
          parts.append(prompt.prompt)
        } else if let attachment = obj as? NativeAttachment {
          parts.append(attachment.prompt)
        } else if let image = Self.cgImage(from: expr) {
          parts.append(Prompt(Attachment(image)))
        } else {
          throw RuntimeError.type(expr, expected: [.strType, NativeImage.type,
                                                   NativeAttachment.type, NativePrompt.type])
        }
      default:
        throw RuntimeError.type(expr, expected: [.strType, NativeImage.type,
                                                 NativeAttachment.type, NativePrompt.type])
    }
  }

  /// The transcript segments of a prompt or instructions given as a Scheme value, or
  /// `nil` if they can't be represented as segments (prompt objects and attachments
  /// referring to image files).
  static func segments(from expr: Expr) -> [Transcript.Segment]? {
    var segments: [Transcript.Segment] = []
    func collect(_ expr: Expr) -> Bool {
      if let text = Self.text(from: expr) {
        segments.append(.text(Transcript.TextSegment(content: text)))
        return true
      }
      switch expr {
        case .null:
          return true
        case .pair(let head, let tail):
          return collect(head) && collect(tail)
        case .object(let obj):
          if let attachment = obj as? NativeAttachment, let segment = attachment.segment {
            segments.append(segment)
            return true
          } else if let image = Self.cgImage(from: expr) {
            segments.append(.attachment(Transcript.AttachmentSegment(
                                          content: .image(Transcript.ImageAttachment(image)))))
            return true
          }
          return false
        default:
          return false
      }
    }
    return collect(expr) ? segments : nil
  }

  static func instructions(from expr: Expr) throws -> FMInstructions {
    var parts: [FMInstructions] = []
    try Self.collect(expr, into: &parts)
    return FMInstructions(parts)
  }

  private static func collect(_ expr: Expr, into parts: inout [FMInstructions]) throws {
    if let text = Self.text(from: expr) {
      parts.append(FMInstructions(text))
      return
    }
    switch expr {
      case .null:
        return
      case .pair(let head, let tail):
        try Self.collect(head, into: &parts)
        try Self.collect(tail, into: &parts)
      case .object(let obj):
        if let attachment = obj as? NativeAttachment {
          parts.append(attachment.instructions)
        } else if let image = Self.cgImage(from: expr) {
          parts.append(FMInstructions(Attachment(image)))
        } else {
          throw RuntimeError.type(expr, expected: [.strType, NativeImage.type,
                                                   NativeAttachment.type])
        }
      default:
        throw RuntimeError.type(expr, expected: [.strType, NativeImage.type,
                                                 NativeAttachment.type])
    }
  }

  static func contentType(from expr: Expr) throws -> UTType {
    let str = try expr.asString()
    if let type = UTType(str) {
      return type
    } else if let type = UTType(mimeType: str) {
      return type
    } else if let type = UTType(filenameExtension: str) {
      return type
    }
    throw SchemaCompiler.invalid("unknown content type", expr)
  }
}

// MARK: - Options

/// Options of a request, i.e. generation options, context options, and metadata.
@available(macOS 27.0, iOS 27.0, *)
struct RequestOptions {
  var generation = GenerationOptions()
  var context = ContextOptions()
  var metadata: [String : GeneratedContent] = [:]

  /// Parses an option list of the form `((name . value) ...)`.
  init(from expr: Expr?, schema: Bool, in context: Context) throws {
    if schema {
      self.context.includeSchemaInPrompt = true
    }
    guard let expr, expr.isTrue else {
      return
    }
    var lst = expr
    while case .pair(let entry, let rest) = lst {
      guard case .pair(let key, let value) = entry else {
        throw RuntimeError.type(entry, expected: [.pairType])
      }
      lst = rest
      let name = try SchemaCompiler.name(key)
      if value.isFalse && name != "include-schema-in-prompt" {
        continue
      }
      switch name {
        case "sampling":
          self.generation.samplingMode = try Self.sampling(from: value)
        case "temperature":
          self.generation.temperature = try value.asDouble(coerce: true)
        case "maximum-response-tokens":
          self.generation.maximumResponseTokens = try value.asInt(above: 1)
        case "tool-calling":
          switch try value.asSymbol().identifier {
            case "allowed":
              self.generation.toolCallingMode = .allowed
            case "required":
              self.generation.toolCallingMode = .required
            case "disallowed":
              self.generation.toolCallingMode = .disallowed
            default:
              throw SchemaCompiler.invalid("unknown tool calling mode", value)
          }
        case "include-schema-in-prompt":
          self.context.includeSchemaInPrompt = value.isTrue
        case "reasoning-level":
          switch value {
            case .symbol(let sym):
              switch sym.identifier {
                case "light":
                  self.context.reasoningLevel = .light
                case "moderate":
                  self.context.reasoningLevel = .moderate
                case "deep":
                  self.context.reasoningLevel = .deep
                default:
                  throw SchemaCompiler.invalid("unknown reasoning level", value)
              }
            case .string(let str):
              self.context.reasoningLevel = .custom(str as String)
            default:
              throw RuntimeError.type(value, expected: [.symbolType, .strType])
          }
        case "metadata":
          self.metadata = try ContentConverter.metadata(from: value, in: context)
        default:
          throw SchemaCompiler.invalid("unknown option", key)
      }
    }
    guard lst.isNull else {
      throw RuntimeError.type(expr, expected: [.assocListType])
    }
  }

  private static func seed(_ args: Expr) throws -> UInt64? {
    switch args {
      case .null:
        return nil
      case .pair(let seed, .null):
        return UInt64(try seed.asInt64())
      default:
        throw SchemaCompiler.invalid("invalid sampling mode", args)
    }
  }

  private static func sampling(from expr: Expr) throws -> GenerationOptions.SamplingMode {
    switch expr {
      case .symbol(let sym) where sym.identifier == "greedy":
        return .greedy
      case .pair(.symbol(let sym), .pair(let param, let rest)):
        switch sym.identifier {
          case "top-k":
            return .random(top: try param.asInt(above: 1), seed: try Self.seed(rest))
          case "top-p":
            let threshold = try param.asDouble(coerce: true)
            guard threshold >= 0.0 && threshold <= 1.0 else {
              throw SchemaCompiler.invalid("top-p threshold must be between 0 and 1", param)
            }
            return .random(probabilityThreshold: threshold, seed: try Self.seed(rest))
          default:
            break
        }
      default:
        break
    }
    throw SchemaCompiler.invalid("invalid sampling mode", expr)
  }

  static func expr(from generation: GenerationOptions,
                   _ ctxt: ContextOptions,
                   in context: Context) -> Expr {
    var res = Exprs()
    func add(_ key: String, _ value: Expr) {
      res.append(.pair(.symbol(context.symbols.intern(key)), value))
    }
    if let sampling = generation.samplingMode {
      switch sampling.kind {
        case .greedy:
          add("sampling", .symbol(context.symbols.intern("greedy")))
        case .randomTopK(let k, seed: let seed):
          add("sampling", .makeList(.symbol(context.symbols.intern("top-k")), .makeNumber(k),
                                    seed.map { .makeNumber($0) } ?? .null))
        case .randomProbabilityThreshold(let p, seed: let seed):
          add("sampling", .makeList(.symbol(context.symbols.intern("top-p")), .flonum(p),
                                    seed.map { .makeNumber($0) } ?? .null))
        @unknown default:
          break
      }
    }
    if let temperature = generation.temperature {
      add("temperature", .flonum(temperature))
    }
    if let tokens = generation.maximumResponseTokens {
      add("maximum-response-tokens", .makeNumber(tokens))
    }
    if let mode = generation.toolCallingMode {
      switch mode.kind {
        case .allowed:
          add("tool-calling", .symbol(context.symbols.intern("allowed")))
        case .required:
          add("tool-calling", .symbol(context.symbols.intern("required")))
        case .disallowed:
          add("tool-calling", .symbol(context.symbols.intern("disallowed")))
        @unknown default:
          break
      }
    }
    if let include = ctxt.includeSchemaInPrompt {
      add("include-schema-in-prompt", .makeBoolean(include))
    }
    if let level = ctxt.reasoningLevel {
      switch level {
        case .light:
          add("reasoning-level", .symbol(context.symbols.intern("light")))
        case .moderate:
          add("reasoning-level", .symbol(context.symbols.intern("moderate")))
        case .deep:
          add("reasoning-level", .symbol(context.symbols.intern("deep")))
        case .custom(let str):
          add("reasoning-level", .makeString(str))
        @unknown default:
          break
      }
    }
    return .makeList(res)
  }
}

// MARK: - Usage

@available(macOS 27.0, iOS 27.0, *)
enum UsageConverter {
  static func expr(from usage: LanguageModelSession.Usage, in context: Context) -> Expr {
    func entry(_ key: String, _ value: Expr) -> Expr {
      return .pair(.symbol(context.symbols.intern(key)), value)
    }
    return .makeList(
      entry("input-tokens", .makeNumber(usage.input.totalTokenCount)),
      entry("cached-input-tokens", .makeNumber(usage.input.cachedTokenCount)),
      entry("output-tokens", .makeNumber(usage.output.totalTokenCount)),
      entry("reasoning-tokens", .makeNumber(usage.output.reasoningTokenCount)),
      entry("total-tokens", .makeNumber(usage.totalTokenCount)),
      entry("metadata", ContentConverter.expr(from: usage.metadata, in: context)))
  }
}

// MARK: - Transcripts

///
/// An immutable transcript, i.e. the history of interactions with a language model.
///
@available(macOS 27.0, iOS 27.0, *)
public final class NativeTranscript: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "transcript"))

  public let transcript: Transcript

  public init(_ transcript: Transcript) {
    self.transcript = transcript
  }

  public override var type: Type {
    return Self.type
  }

  public override var tagString: String {
    return "\(Self.type) \(self.identityString): \(self.transcript.count) entries"
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }
}

@available(macOS 27.0, iOS 27.0, *)
struct TranscriptConverter {
  let context: Context

  private func sym(_ str: String) -> Expr {
    return .symbol(self.context.symbols.intern(str))
  }

  private func entry(_ kind: String, _ props: [(String, Expr)]) -> Expr {
    return .pair(self.sym(kind), .makeList(Exprs(props.map { .pair(self.sym($0.0), $0.1) })))
  }

  private func image(from cgImage: CGImage) -> Expr {
    #if os(macOS)
    return .object(NativeImage(NSImage(cgImage: cgImage, size: .zero)))
    #else
    return .object(NativeImage(UIImage(cgImage: cgImage)))
    #endif
  }

  private func bytes(_ data: Data) -> Expr {
    return .bytes(MutableBox([UInt8](data)))
  }

  // Transcript entries -> Scheme

  func expr(from segment: Transcript.Segment) -> Expr {
    switch segment {
      case .text(let text):
        return .makeString(text.content)
      case .structure(let structure):
        return .makeList(self.sym("structure"),
                         .makeString(structure.source),
                         ContentConverter.expr(from: structure.content, in: self.context))
      case .attachment(let attachment):
        let label: Expr = attachment.label.map { .makeString($0) } ?? .false
        switch attachment.content {
          case .image(let image):
            return .makeList(self.sym("attachment"), label, self.image(from: image.cgImage))
          default:
            if #available(macOS 27.2, iOS 27.2, *), case .data(let data) = attachment.content {
              return .makeList(self.sym("attachment"), label, self.bytes(data.content),
                               .makeString(data.contentType.identifier))
            }
            return .makeList(self.sym("attachment"), label)
        }
      @unknown default:
        return .makeString(segment.description)
    }
  }

  func expr(from segments: [Transcript.Segment]) -> Expr {
    return .makeList(Exprs(segments.map(self.expr(from:))))
  }

  func expr(from entry: Transcript.Entry) -> Expr {
    switch entry {
      case .instructions(let instr):
        let tools = instr.toolDefinitions.map { (def: Transcript.ToolDefinition) -> Expr in
          .makeList(.makeString(def.name), .makeString(def.description),
                    .object(NativeGenerationSchema(def.parameters, shapes: nil)))
        }
        return self.entry("instructions", [("id", .makeString(instr.id)),
                                           ("segments", self.expr(from: instr.segments)),
                                           ("tools", .makeList(Exprs(tools)))])
      case .prompt(let prompt):
        var schema: Expr = .false
        if let format = prompt.responseFormat, case .schema(let s) = format.kind {
          schema = .object(NativeGenerationSchema(s, shapes: nil))
        }
        return self.entry("prompt", [
          ("id", .makeString(prompt.id)),
          ("segments", self.expr(from: prompt.segments)),
          ("options", RequestOptions.expr(from: prompt.options, prompt.contextOptions,
                                          in: self.context)),
          ("metadata", ContentConverter.expr(from: prompt.metadata, in: self.context)),
          ("schema", schema)])
      case .toolCalls(let calls):
        let cs = calls.map { (call: Transcript.ToolCall) -> Expr in
          var props: Exprs = [.pair(self.sym("id"), .makeString(call.id)),
                              .pair(self.sym("tool"), .makeString(call.toolName)),
                              .pair(self.sym("arguments"),
                                    ContentConverter.expr(from: call.arguments,
                                                          in: self.context))]
          if !call.metadata.isEmpty {
            props.append(.pair(self.sym("metadata"),
                               ContentConverter.expr(from: call.metadata, in: self.context)))
          }
          return .makeList(props)
        }
        return self.entry("tool-calls", [("id", .makeString(calls.id)),
                                         ("calls", .makeList(Exprs(cs)))])
      case .toolOutput(let output):
        return self.entry("tool-output", [("id", .makeString(output.id)),
                                          ("tool", .makeString(output.toolName)),
                                          ("segments", self.expr(from: output.segments))])
      case .response(let response):
        var props: [(String, Expr)] = [
          ("id", .makeString(response.id)),
          ("segments", self.expr(from: response.segments)),
          ("metadata", ContentConverter.expr(from: response.metadata, in: self.context))]
        if !response.assetIDs.isEmpty {
          props.append(("asset-ids", .makeList(Exprs(response.assetIDs.map { .makeString($0) }))))
        }
        return self.entry("response", props)
      case .reasoning(let reasoning):
        var props: [(String, Expr)] = [
          ("id", .makeString(reasoning.id)),
          ("segments", self.expr(from: reasoning.segments)),
          ("metadata", ContentConverter.expr(from: reasoning.metadata, in: self.context))]
        if let signature = reasoning.signature {
          props.append(("signature", self.bytes(signature)))
        }
        return self.entry("reasoning", props)
      default:
        if #available(macOS 27.2, iOS 27.2, *), case .data(let data) = entry {
          return self.entry("data", [
            ("id", .makeString(data.id)),
            ("content-type", .makeString(data.contentType.identifier)),
            ("content", self.bytes(data.content)),
            ("metadata", ContentConverter.expr(from: data.metadata, in: self.context))])
        }
        return self.entry("unknown", [("id", .makeString(entry.id)),
                                      ("description", .makeString(entry.description))])
    }
  }

  // Scheme -> transcript entries

  private func properties(_ expr: Expr) throws -> (String, [String : Expr]) {
    guard case .pair(.symbol(let kind), let alist) = expr else {
      throw SchemaCompiler.invalid("invalid transcript entry", expr)
    }
    var props: [String : Expr] = [:]
    var lst = alist
    while case .pair(.pair(.symbol(let key), let value), let rest) = lst {
      if props[key.identifier] == nil {
        props[key.identifier] = value
      }
      lst = rest
    }
    guard lst.isNull else {
      throw SchemaCompiler.invalid("invalid transcript entry", expr)
    }
    return (kind.identifier, props)
  }

  private func list(_ expr: Expr?) throws -> [Expr] {
    guard let expr else {
      return []
    }
    var res: [Expr] = []
    var lst = expr
    while case .pair(let head, let tail) = lst {
      res.append(head)
      lst = tail
    }
    guard lst.isNull else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return res
  }

  private func string(_ expr: Expr?, default def: String? = nil) throws -> String {
    guard let expr, expr.isTrue else {
      if let def {
        return def
      }
      throw SchemaCompiler.invalid("missing transcript entry property", expr ?? .false)
    }
    return try expr.asString()
  }

  func segment(from expr: Expr) throws -> Transcript.Segment {
    if let text = PromptConverter.text(from: expr) {
      return .text(Transcript.TextSegment(content: text))
    }
    let elems = try self.list(expr)
    guard elems.count >= 2, case .symbol(let kind) = elems[0] else {
      throw SchemaCompiler.invalid("invalid transcript segment", expr)
    }
    switch kind.identifier {
      case "structure" where elems.count == 3:
        return .structure(Transcript.StructuredSegment(
                            source: try elems[1].asString(),
                            content: try ContentConverter.content(from: elems[2],
                                                                  in: self.context)))
      case "attachment" where elems.count >= 3:
        let label = elems[1].isTrue ? try elems[1].asString() : nil
        if let image = PromptConverter.cgImage(from: elems[2]), elems.count == 3 {
          return .attachment(Transcript.AttachmentSegment(
                               content: .image(Transcript.ImageAttachment(image)),
                               label: label))
        } else if #available(macOS 27.2, iOS 27.2, *),
                  case .bytes(let bvec) = elems[2], elems.count == 4 {
          let data = Transcript.DataAttachment(
                       contentType: try PromptConverter.contentType(from: elems[3]),
                       content: Data(bvec.value))
          return .attachment(Transcript.AttachmentSegment(content: .data(data), label: label))
        }
      default:
        break
    }
    throw SchemaCompiler.invalid("invalid transcript segment", expr)
  }

  private func segments(_ expr: Expr?) throws -> [Transcript.Segment] {
    return try self.list(expr).map(self.segment(from:))
  }

  func entry(from expr: Expr) throws -> Transcript.Entry {
    let (kind, props) = try self.properties(expr)
    let id = try self.string(props["id"], default: UUID().uuidString)
    let metadata = try ContentConverter.metadata(from: props["metadata"] ?? .null,
                                                 in: self.context)
    switch kind {
      case "instructions":
        var defs: [Transcript.ToolDefinition] = []
        for def in try self.list(props["tools"]) {
          let elems = try self.list(def)
          guard elems.count == 3 else {
            throw SchemaCompiler.invalid("invalid tool definition", def)
          }
          defs.append(Transcript.ToolDefinition(
                        name: try elems[0].asString(),
                        description: try elems[1].asString(),
                        parameters: try SchemaCompiler.schema(from: elems[2],
                                                              in: self.context).schema))
        }
        return .instructions(Transcript.Instructions(id: id,
                                                     segments: try self.segments(props["segments"]),
                                                     toolDefinitions: defs))
      case "prompt":
        var format: Transcript.ResponseFormat? = nil
        if let schema = props["schema"], schema.isTrue {
          format = Transcript.ResponseFormat(
                     schema: try SchemaCompiler.schema(from: schema, in: self.context).schema)
        }
        let options = try RequestOptions(from: props["options"], schema: false,
                                         in: self.context)
        return .prompt(Transcript.Prompt(id: id,
                                         metadata: metadata,
                                         segments: try self.segments(props["segments"]),
                                         options: options.generation,
                                         responseFormat: format,
                                         contextOptions: options.context))
      case "tool-calls":
        var calls: [Transcript.ToolCall] = []
        for call in try self.list(props["calls"]) {
          let (_, cprops) = try self.properties(.pair(self.sym("call"), call))
          calls.append(Transcript.ToolCall(
                         id: try self.string(cprops["id"], default: UUID().uuidString),
                         metadata: try ContentConverter.metadata(from: cprops["metadata"] ?? .null,
                                                                 in: self.context),
                         toolName: try self.string(cprops["tool"]),
                         arguments: try ContentConverter.content(from: cprops["arguments"] ?? .null,
                                                                 in: self.context)))
        }
        return .toolCalls(Transcript.ToolCalls(id: id, calls))
      case "tool-output":
        return .toolOutput(Transcript.ToolOutput(id: id,
                                                 toolName: try self.string(props["tool"]),
                                                 segments: try self.segments(props["segments"])))
      case "response":
        var response = Transcript.Response(id: id,
                                           metadata: metadata,
                                           segments: try self.segments(props["segments"]))
        response.assetIDs = try self.list(props["asset-ids"]).map { try $0.asString() }
        return .response(response)
      case "reasoning":
        var signature: Data? = nil
        if let sig = props["signature"], sig.isTrue {
          guard case .bytes(let bvec) = sig else {
            throw SchemaCompiler.invalid("signature of reasoning entry requires a bytevector", sig)
          }
          signature = Data(bvec.value)
        }
        return .reasoning(Transcript.Reasoning(id: id,
                                               metadata: metadata,
                                               segments: try self.segments(props["segments"]),
                                               signature: signature))
      case "data":
        guard #available(macOS 27.2, iOS 27.2, *) else {
          throw ModelErrors.error("unsupported", "data entries require macOS/iOS 27.2",
                                  in: self.context)
        }
        guard let content = props["content"], case .bytes(let bvec) = content else {
          throw SchemaCompiler.invalid("data entry requires a bytevector", expr)
        }
        return .data(Transcript.DataEntry(
                       id: id,
                       contentType: try PromptConverter.contentType(
                                      from: props["content-type"] ?? .false),
                       content: Data(bvec.value),
                       metadata: try ContentConverter.content(from: props["metadata"] ?? .null,
                                                              in: self.context)))
      default:
        throw SchemaCompiler.invalid("unknown transcript entry kind", expr)
    }
  }

  /// Appends the entries denoted by `expr` (a transcript, an entry, or a list of entries
  /// and transcripts) to `entries`.
  func collect(_ expr: Expr, into entries: inout [Transcript.Entry]) throws {
    switch expr {
      case .null:
        return
      case .object(let obj):
        guard let transcript = obj as? NativeTranscript else {
          throw RuntimeError.type(expr, expected: [NativeTranscript.type])
        }
        entries.append(contentsOf: transcript.transcript)
      case .pair(.symbol(_), _):
        entries.append(try self.entry(from: expr))
      case .pair(let head, let tail):
        try self.collect(head, into: &entries)
        try self.collect(tail, into: &entries)
      default:
        throw SchemaCompiler.invalid("invalid transcript entry", expr)
    }
  }
}

// MARK: - Errors

///
/// Errors raised by a Scheme tool procedure. They are passed through FoundationModels and
/// re-raised unchanged when the request fails.
///
struct SchemeRaisedError: Error, @unchecked Sendable {
  let raised: Expr
}

@available(macOS 27.0, iOS 27.0, *)
public enum ModelErrors {

  /// Translators for errors of model providers, e.g. Claude. They return the kind and message
  /// of model errors.
  nonisolated(unsafe) private static var translators: [(Error) -> (kind: String, message: String)?] = []
  private static let lock = NSLock()

  public static func register(_ translator: @escaping (Error) -> (kind: String, message: String)?) {
    Self.lock.lock()
    Self.translators.append(translator)
    Self.lock.unlock()
  }

  /// Returns a model error of the given kind. `details` is an association list with
  /// additional information about the error.
  public static func error(_ kind: String,
                           _ message: String,
                           details: Expr = .null,
                           in context: Context) -> RuntimeError {
    // The kind is the first irritant, the details (if there are any) the second.
    var irritants: [Expr] = [.symbol(context.symbols.intern(kind))]
    if !details.isNull {
      irritants.append(details)
    }
    // `$` introduces placeholders in error message templates and thus needs to be escaped
    return RuntimeError.custom("model error",
                               message.replacingOccurrences(of: "$", with: "$$"),
                               irritants)
  }

  /// Returns the kind of a model error, or `nil` if `error` is not a model error.
  public static func kind(of error: RuntimeError) -> Expr? {
    guard case .custom("model error", _) = error.descriptor, let kind = error.irritants.first else {
      return nil
    }
    return kind
  }

  /// Returns the details of a model error as an association list, or `nil` if `error` is
  /// not a model error.
  public static func details(of error: RuntimeError) -> Expr? {
    guard Self.kind(of: error) != nil else {
      return nil
    }
    return error.irritants.count > 1 ? error.irritants[1] : .null
  }

  private static func message(_ error: Error) -> String {
    if let error = error as? LocalizedError, let descr = error.errorDescription {
      return descr
    }
    return error.localizedDescription
  }

  /// Returns the kind of a known FoundationModels error.
  private static func kind(of error: Error) -> String? {
    switch error {
      case let error as LanguageModelError:
        switch error {
          case .contextSizeExceeded(_):
            return "context-size-exceeded"
          case .rateLimited(_):
            return "rate-limited"
          case .guardrailViolation(_):
            return "guardrail-violation"
          case .refusal(_):
            return "refusal"
          case .unsupportedCapability(_):
            return "unsupported-capability"
          case .unsupportedTranscriptContent(_):
            return "unsupported-transcript-content"
          case .unsupportedGenerationGuide(_):
            return "unsupported-guide"
          case .unsupportedLanguageOrLocale(_):
            return "unsupported-language-or-locale"
          case .timeout(_):
            return "timeout"
          @unknown default:
            return "language-model-error"
        }
      case let error as LanguageModelSession.GenerationError:
        switch error {
          case .exceededContextWindowSize(_):
            return "exceeded-context-window-size"
          case .assetsUnavailable(_):
            return "assets-unavailable"
          case .guardrailViolation(_):
            return "guardrail-violation"
          case .unsupportedGuide(_):
            return "unsupported-guide"
          case .unsupportedLanguageOrLocale(_):
            return "unsupported-language-or-locale"
          case .decodingFailure(_):
            return "decoding-failure"
          case .rateLimited(_):
            return "rate-limited"
          case .concurrentRequests(_):
            return "concurrent-requests"
          case .refusal(_, _):
            return "refusal"
          @unknown default:
            return "generation-error"
        }
      case let error as LanguageModelSession.Error:
        switch error {
          case .concurrentRequests:
            return "concurrent-requests"
          case .transcriptMutationWhileResponding:
            return "transcript-mutation-while-responding"
          @unknown default:
            return "session-error"
        }
      case let error as PrivateCloudComputeLanguageModel.Error:
        switch error {
          case .networkFailure(_):
            return "network-failure"
          case .quotaLimitReached(_):
            return "quota-limit-reached"
          case .serviceUnavailable(_):
            return "service-unavailable"
          @unknown default:
            return "afm-cloud-error"
        }
      case is SystemLanguageModel.Error:
        return "assets-unavailable"
      case is GenerationSchema.SchemaError:
        return "schema-error"
      case is GeneratedContent.ParsingError:
        return "decoding-failure"
      case is LanguageModelSession.ToolCallError:
        return "tool-call-failed"
      case is CancellationError:
        return "cancelled"
      default:
        return nil
    }
  }

  /// Translates a Swift error into a LispKit error.
  public static func translate(_ error: Error, in context: Context) -> Error {
    switch error {
      case is RuntimeError:
        return error
      case let error as SchemeRaisedError:
        if case .error(let err) = error.raised {
          return err
        }
        return RuntimeError.uncaught(error.raised)
      case let error as LanguageModelSession.ToolCallError:
        if error.underlyingError is SchemeRaisedError || error.underlyingError is RuntimeError {
          return Self.translate(error.underlyingError, in: context)
        }
      default:
        break
    }
    if let kind = Self.kind(of: error) {
      return Self.error(kind,
                        Self.message(error),
                        details: ModelErrorDetails.expr(for: error, in: context),
                        in: context)
    }
    Self.lock.lock()
    let translators = Self.translators
    Self.lock.unlock()
    for translator in translators {
      if let (kind, message) = translator(error) {
        return Self.error(kind, message, in: context)
      }
    }
    return RuntimeError.os(error)
  }

  /// Translates a Swift error into a value that can be stored in a future and raised by
  /// `future-get`: errors raised by Scheme tool procedures are passed through unchanged.
  static func raisable(_ error: Error, in context: Context) -> Expr {
    if let error = error as? SchemeRaisedError {
      return error.raised
    } else if let error = error as? LanguageModelSession.ToolCallError,
              let underlying = error.underlyingError as? SchemeRaisedError {
      return underlying.raised
    }
    if let error = Self.translate(error, in: context) as? RuntimeError {
      return .error(error)
    }
    return .error(RuntimeError.os(error))
  }
}

// MARK: - Error details

///
/// A refusal of a language model to respond. It can be asked to explain itself.
///
@available(macOS 27.0, iOS 27.0, *)
public final class NativeRefusal: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "model-refusal"))

  enum Source {
    case model(LanguageModelError.Refusal)
    case generation(LanguageModelSession.GenerationError.Refusal)
  }

  let source: Source

  init(_ source: Source) {
    self.source = source
  }

  /// Asks the language model to explain the refusal.
  func explanation() async throws -> String {
    switch self.source {
      case .model(let refusal):
        return try await refusal.explanation.content
      case .generation(let refusal):
        return try await refusal.explanation.content
    }
  }

  public override var type: Type {
    return Self.type
  }

  public override var tagString: String {
    return "\(Self.type) \(self.identityString)"
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }
}

///
/// A suggestion of the system for increasing the limit of Private Cloud Compute requests.
///
@available(macOS 27.0, iOS 27.0, *)
public final class NativeLimitIncreaseSuggestion: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "limit-increase-suggestion"))

  let suggestion: PrivateCloudComputeLanguageModel.QuotaUsage.LimitIncreaseSuggestion

  init(_ suggestion: PrivateCloudComputeLanguageModel.QuotaUsage.LimitIncreaseSuggestion) {
    self.suggestion = suggestion
  }

  /// Presents the suggestion to the user.
  func show() {
    let suggestion = self.suggestion
    Task { @MainActor in
      suggestion.show()
    }
  }

  public override var type: Type {
    return Self.type
  }

  public override var tagString: String {
    return "\(Self.type) \(self.identityString)"
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }
}

///
/// Extracts the details of errors of FoundationModels as association lists. The details are
/// attached to model errors.
///
@available(macOS 27.0, iOS 27.0, *)
enum ModelErrorDetails {

  struct Builder {
    let context: Context
    var items: [(String, Expr)] = []

    init(_ context: Context) {
      self.context = context
    }

    mutating func add(_ key: String, _ value: Expr) {
      self.items.append((key, value))
    }

    mutating func add(_ key: String, string: String?) {
      if let string {
        self.add(key, .makeString(string))
      }
    }

    mutating func add(_ key: String, date: Date?) {
      self.add(key, ModelErrorDetails.date(date))
    }

    mutating func add(metadata: [String : any Sendable]) {
      guard !metadata.isEmpty else {
        return
      }
      var res = Exprs()
      for key in metadata.keys.sorted() {
        res.append(.pair(.symbol(self.context.symbols.intern(key)),
                         ModelErrorDetails.value(metadata[key]!, in: self.context)))
      }
      self.add("metadata", .makeList(res))
    }

    var expr: Expr {
      return .makeList(Exprs(self.items.map {
        .pair(.symbol(self.context.symbols.intern($0.0)), $0.1)
      }))
    }
  }

  static func date(_ date: Date?) -> Expr {
    guard let date else {
      return .false
    }
    return .object(NativeDateTime(DateTimeLibrary.calendar.dateComponents(in: .current,
                                                                         from: date)))
  }

  static func value(_ value: Any, in context: Context) -> Expr {
    switch value {
      case let value as String:
        return .makeString(value)
      case let value as Bool:
        return .makeBoolean(value)
      case let value as Int:
        return .makeNumber(value)
      case let value as Double:
        return .flonum(value)
      case let value as Date:
        return Self.date(value)
      case let value as GeneratedContent:
        return ContentConverter.expr(from: value, in: context)
      default:
        return .makeString(String(describing: value))
    }
  }

  static func capabilityName(_ capability: LanguageModelCapabilities.Capability) -> String {
    switch capability {
      case .vision:
        return "vision"
      case .guidedGeneration:
        return "guided-generation"
      case .reasoning:
        return "reasoning"
      case .toolCalling:
        return "tool-calling"
      default:
        return "unknown"
    }
  }

  /// Returns the details of `error` as an association list.
  static func expr(for error: Error, in context: Context) -> Expr {
    var b = Builder(context)
    var debug: String? = nil
    var metadata: [String : any Sendable] = [:]
    switch error {
      case let error as LanguageModelError:
        switch error {
          case .contextSizeExceeded(let e):
            b.add("context-size", .makeNumber(e.contextSize))
            b.add("token-count", .makeNumber(e.tokenCount))
            debug = e.debugDescription
            metadata = e.metadata
          case .rateLimited(let e):
            b.add("reset-date", date: e.resetDate)
            debug = e.debugDescription
            metadata = e.metadata
          case .guardrailViolation(let e):
            debug = e.debugDescription
            metadata = e.metadata
          case .refusal(let e):
            b.add("refusal", .object(NativeRefusal(.model(e))))
            debug = e.debugDescription
            metadata = e.metadata
          case .unsupportedCapability(let e):
            b.add("capability", .symbol(context.symbols.intern(Self.capabilityName(e.capability))))
            debug = e.debugDescription
            metadata = e.metadata
          case .unsupportedTranscriptContent(let e):
            b.add("content", .object(NativeTranscript(Transcript(entries: e.unsupportedContent))))
            debug = e.debugDescription
            metadata = e.metadata
          case .unsupportedGenerationGuide(let e):
            b.add("schema-name", string: e.schemaName)
            debug = e.debugDescription
            metadata = e.metadata
          case .unsupportedLanguageOrLocale(let e):
            b.add("language-code", string: e.languageCode.identifier)
            debug = e.debugDescription
            metadata = e.metadata
          case .timeout(let e):
            debug = e.debugDescription
            metadata = e.metadata
          @unknown default:
            break
        }
      case let error as LanguageModelSession.GenerationError:
        switch error {
          case .exceededContextWindowSize(let c),
               .assetsUnavailable(let c),
               .guardrailViolation(let c),
               .unsupportedGuide(let c),
               .unsupportedLanguageOrLocale(let c),
               .decodingFailure(let c),
               .rateLimited(let c),
               .concurrentRequests(let c):
            debug = c.debugDescription
          case .refusal(let refusal, let c):
            b.add("refusal", .object(NativeRefusal(.generation(refusal))))
            debug = c.debugDescription
          @unknown default:
            break
        }
      case let error as PrivateCloudComputeLanguageModel.Error:
        switch error {
          case .networkFailure(let e):
            debug = e.debugDescription
          case .quotaLimitReached(let e):
            b.add("reset-date", date: e.resetDate)
            if let suggestion = e.limitIncreaseSuggestion {
              b.add("limit-increase-suggestion", .object(NativeLimitIncreaseSuggestion(suggestion)))
            }
            debug = e.debugDescription
          case .serviceUnavailable(let e):
            debug = e.debugDescription
          @unknown default:
            break
        }
      case let error as SystemLanguageModel.Error:
        if case .assetsUnavailable(let e) = error {
          debug = e.debugDescription
        }
      case let error as GenerationSchema.SchemaError:
        switch error {
          case .duplicateType(schema: let schema, type: let type, context: let c):
            b.add("schema", string: schema)
            b.add("type", string: type)
            debug = c.debugDescription
          case .duplicateProperty(schema: let schema, property: let property, context: let c):
            b.add("schema", string: schema)
            b.add("property", string: property)
            debug = c.debugDescription
          case .emptyTypeChoices(schema: let schema, context: let c):
            b.add("schema", string: schema)
            debug = c.debugDescription
          case .undefinedReferences(schema: let schema, references: let refs, context: let c):
            b.add("schema", string: schema)
            b.add("references", .makeList(Exprs(refs.map { .makeString($0) })))
            debug = c.debugDescription
          @unknown default:
            break
        }
      case let error as GeneratedContent.ParsingError:
        b.add("raw-content", string: error.rawContent)
        if let underlying = error.underlyingError {
          b.add("underlying-error", string: underlying.localizedDescription)
        }
        debug = error.debugDescription
      case let error as LanguageModelSession.ToolCallError:
        b.add("tool", string: error.tool.name)
        b.add("underlying-error", string: error.underlyingError.localizedDescription)
      default:
        break
    }
    b.add("debug-description", string: debug)
    b.add(metadata: metadata)
    if let error = error as? LocalizedError {
      b.add("recovery-suggestion", string: error.recoverySuggestion)
      b.add("failure-reason", string: error.failureReason)
    }
    return b.expr
  }
}

// MARK: - Tools

///
/// A tool, i.e. a Scheme procedure with a name, a description, and a parameter schema,
/// that a language model can call.
///
@available(macOS 27.0, iOS 27.0, *)
public final class NativeTool: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "tool"))

  let name: String
  let descr: String
  let parameters: NativeGenerationSchema
  let procedure: Procedure
  let includesSchema: Bool

  init(name: String,
       description: String,
       parameters: NativeGenerationSchema,
       procedure: Procedure,
       includesSchema: Bool) {
    self.name = name
    self.descr = description
    self.parameters = parameters
    self.procedure = procedure
    self.includesSchema = includesSchema
  }

  public override var type: Type {
    return Self.type
  }

  public override var tagString: String {
    return "\(Self.type) \(self.identityString): \(self.name)"
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }

  public override func mark(in gc: GarbageCollector) {
    gc.mark(self.procedure)
  }
}

///
/// A tool invocation that is waiting to be executed by a Scheme worker thread.
///
@available(macOS 27.0, iOS 27.0, *)
final class ToolInvocation: NativeObject, @unchecked Sendable {
  static let type = Type.objectType(Symbol(uninterned: "tool-invocation"))

  let tool: NativeTool
  let arguments: GeneratedContent
  private let lock = NSLock()
  private var continuation: CheckedContinuation<Prompt, Error>?

  init(tool: NativeTool, arguments: GeneratedContent, continuation: CheckedContinuation<Prompt, Error>) {
    self.tool = tool
    self.arguments = arguments
    self.continuation = continuation
  }

  /// Resumes the waiting tool call. Only the first call has an effect.
  func resume(with result: Result<Prompt, Error>) {
    self.lock.lock()
    let continuation = self.continuation
    self.continuation = nil
    self.lock.unlock()
    continuation?.resume(with: result)
  }

  override var type: Type {
    return Self.type
  }
}

///
/// The dispatcher of a request queues up tool invocations. They are executed by a Scheme
/// worker thread which retrieves them via `next`.
///
@available(macOS 27.0, iOS 27.0, *)
final class ToolDispatcher: NativeObject, @unchecked Sendable {
  static let type = Type.objectType(Symbol(uninterned: "tool-dispatcher"))

  private let condition = NSCondition()
  private var queue: [ToolInvocation] = []
  private var closed = false

  /// Submits an invocation; fails if the dispatcher is closed.
  func submit(_ invocation: ToolInvocation) {
    self.condition.lock()
    if self.closed {
      self.condition.unlock()
      invocation.resume(with: .failure(CancellationError()))
      return
    }
    self.queue.append(invocation)
    self.condition.broadcast()
    self.condition.unlock()
  }

  /// Closes the dispatcher. Pending invocations are cancelled.
  func close() {
    self.condition.lock()
    self.closed = true
    let pending = self.queue
    self.queue.removeAll()
    self.condition.broadcast()
    self.condition.unlock()
    for invocation in pending {
      invocation.resume(with: .failure(CancellationError()))
    }
  }

  /// Returns the next invocation, or `nil` if the dispatcher was closed. Waiting gets
  /// aborted if the evaluation of the current thread gets aborted.
  func next(in context: Context) throws -> ToolInvocation? {
    self.condition.lock()
    defer {
      self.condition.unlock()
    }
    while self.queue.isEmpty && !self.closed {
      if context.evaluator.threads.current?.value.machine?.abortionRequested ?? false {
        throw RuntimeError.abortion()
      }
      _ = self.condition.wait(until: Date(timeIntervalSinceNow: 0.1))
    }
    if self.queue.isEmpty {
      return nil
    }
    return self.queue.removeFirst()
  }

  override var type: Type {
    return Self.type
  }
}

/// Connects the tools of a session with the dispatcher of the request that is currently
/// being processed.
@available(macOS 27.0, iOS 27.0, *)
final class ToolHub: @unchecked Sendable {
  private let lock = NSLock()
  private var dispatcher: ToolDispatcher? = nil

  var current: ToolDispatcher? {
    get {
      self.lock.lock()
      defer { self.lock.unlock() }
      return self.dispatcher
    }
    set {
      self.lock.lock()
      self.dispatcher = newValue
      self.lock.unlock()
    }
  }
}

/// The FoundationModels tool implementation for Scheme tools.
@available(macOS 27.0, iOS 27.0, *)
struct SchemeTool: Tool, @unchecked Sendable {
  let tool: NativeTool
  let hub: ToolHub

  var name: String {
    return self.tool.name
  }

  var description: String {
    return self.tool.descr
  }

  var parameters: GenerationSchema {
    return self.tool.parameters.schema
  }

  var includesSchemaInInstructions: Bool {
    return self.tool.includesSchema
  }

  @concurrent func call(arguments: GeneratedContent) async throws -> Prompt {
    guard let dispatcher = self.hub.current else {
      throw CancellationError()
    }
    let tool = self.tool
    var invocation: ToolInvocation? = nil
    return try await withTaskCancellationHandler {
      try await withCheckedThrowingContinuation { continuation in
        let inv = ToolInvocation(tool: tool, arguments: arguments, continuation: continuation)
        invocation = inv
        dispatcher.submit(inv)
      }
    } onCancel: {
      invocation?.resume(with: .failure(CancellationError()))
    }
  }
}

// MARK: - Sessions, requests, and responses

///
/// A language model session.
///
@available(macOS 27.0, iOS 27.0, *)
public final class NativeModelSession: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "model-session"))

  let session: LanguageModelSession
  let model: NativeLanguageModel
  let tools: [NativeTool]
  let hub: ToolHub
  private let lock = NSLock()
  private var requests: [ObjectIdentifier : ModelRequest] = [:]

  /// The transcript error handling policy as set via the Scheme API.
  var errorPolicy: String?

  init(session: LanguageModelSession, model: NativeLanguageModel, tools: [NativeTool], hub: ToolHub) {
    self.session = session
    self.model = model
    self.tools = tools
    self.hub = hub
    if let policy = session.transcriptErrorHandlingPolicy {
      let descr = String(describing: policy).lowercased()
      self.errorPolicy = descr.contains("revert") ? "revert-transcript"
                       : descr.contains("preserve") ? "preserve-transcript" : nil
    } else {
      self.errorPolicy = nil
    }
  }

  /// Is a request of this session in progress?
  var busy: Bool {
    self.lock.lock()
    defer { self.lock.unlock() }
    return !self.requests.isEmpty
  }

  func add(_ request: ModelRequest) {
    self.lock.lock()
    self.requests[ObjectIdentifier(request)] = request
    self.lock.unlock()
  }

  func remove(_ request: ModelRequest) {
    self.lock.lock()
    self.requests[ObjectIdentifier(request)] = nil
    self.lock.unlock()
  }

  func cancel() -> Bool {
    self.lock.lock()
    let requests = Array(self.requests.values)
    self.lock.unlock()
    for request in requests {
      request.cancel()
    }
    return !requests.isEmpty
  }

  public override var type: Type {
    return Self.type
  }

  public override var tagString: String {
    return "\(Self.type) \(self.identityString): \(self.model.label)"
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }

  public override func mark(in gc: GarbageCollector) {
    for tool in self.tools {
      tool.mark(in: gc)
    }
  }
}

///
/// A request that is in progress. It owns the Swift task that is waiting for the
/// language model and the dispatcher for tool calls.
///
@available(macOS 27.0, iOS 27.0, *)
final class ModelRequest: @unchecked Sendable {
  private let lock = NSLock()
  private var task: Task<Void, Never>? = nil
  private var cancelled = false
  let dispatcher: ToolDispatcher?
  weak var session: NativeModelSession?
  unowned let context: Context

  init(session: NativeModelSession, context: Context) {
    self.session = session
    self.context = context
    self.dispatcher = session.tools.isEmpty ? nil : ToolDispatcher()
  }

  /// Starts the request; `body` is executed by a Swift task.
  func start(_ body: @escaping @Sendable () async -> Void) {
    self.session?.hub.current = self.dispatcher
    self.session?.add(self)
    self.context.evaluator.registerAbortHandler(for: self) { [weak self] in
      self?.cancel()
    }
    let task = Task {
      await body()
    }
    self.lock.lock()
    self.task = task
    let cancelled = self.cancelled
    self.lock.unlock()
    if cancelled {
      task.cancel()
    }
  }

  /// Must be called by `body` when the request is finished.
  func finish() {
    self.dispatcher?.close()
    if let session = self.session {
      if session.hub.current === self.dispatcher {
        session.hub.current = nil
      }
      session.remove(self)
    }
    self.context.evaluator.removeAbortHandler(for: self)
  }

  func cancel() {
    self.lock.lock()
    self.cancelled = true
    let task = self.task
    self.lock.unlock()
    task?.cancel()
    self.dispatcher?.close()
  }
}

///
/// A response of a language model, or a snapshot of a response that is being streamed.
///
@available(macOS 27.0, iOS 27.0, *)
public final class NativeModelResponse: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "model-response"))

  enum Content {
    case text(String)
    case generated(GeneratedContent)
  }

  let content: Content
  let rawContent: GeneratedContent
  let entries: [Transcript.Entry]
  let usage: LanguageModelSession.Usage
  let shapes: SchemaShapes?
  let snapshot: Bool

  init(content: Content,
       rawContent: GeneratedContent,
       entries: ArraySlice<Transcript.Entry>,
       usage: LanguageModelSession.Usage,
       shapes: SchemaShapes?,
       snapshot: Bool) {
    self.content = content
    self.rawContent = rawContent
    self.entries = Array(entries)
    self.usage = usage
    self.shapes = shapes
    self.snapshot = snapshot
  }

  convenience init(_ response: LanguageModelSession.Response<String>) {
    self.init(content: .text(response.content),
              rawContent: response.rawContent,
              entries: response.transcriptEntries,
              usage: response.usage,
              shapes: nil,
              snapshot: false)
  }

  convenience init(_ response: LanguageModelSession.Response<GeneratedContent>,
                   shapes: SchemaShapes?) {
    self.init(content: .generated(response.content),
              rawContent: response.rawContent,
              entries: response.transcriptEntries,
              usage: response.usage,
              shapes: shapes,
              snapshot: false)
  }

  convenience init(_ snapshot: LanguageModelSession.ResponseStream<String>.Snapshot) {
    self.init(content: .text(snapshot.content),
              rawContent: snapshot.rawContent,
              entries: snapshot.transcriptEntries,
              usage: snapshot.usage,
              shapes: nil,
              snapshot: true)
  }

  convenience init(_ snapshot: LanguageModelSession.ResponseStream<GeneratedContent>.Snapshot,
                   shapes: SchemaShapes?) {
    self.init(content: .generated(snapshot.content),
              rawContent: snapshot.rawContent,
              entries: snapshot.transcriptEntries,
              usage: snapshot.usage,
              shapes: shapes,
              snapshot: true)
  }

  /// A final response with the content of this snapshot.
  var final: NativeModelResponse {
    return NativeModelResponse(content: self.content,
                               rawContent: self.rawContent,
                               entries: self.entries[...],
                               usage: self.usage,
                               shapes: self.shapes,
                               snapshot: false)
  }

  var isComplete: Bool {
    return !self.snapshot || self.rawContent.isComplete
  }

  func contentExpr(in context: Context) -> Expr {
    switch self.content {
      case .text(let str):
        return .makeString(str)
      case .generated(let content):
        return ContentConverter.expr(from: content, shape: self.shapes?.root,
                                     shapes: self.shapes, in: context)
    }
  }

  public override var type: Type {
    return Self.type
  }

  public override var tagString: String {
    return "\(Self.type) \(self.identityString)\(self.isComplete ? "" : " (partial)")"
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }
}

///
/// A stream of responses. A Swift task consumes the response stream of FoundationModels
/// and keeps the most recent snapshot. Since snapshots are cumulative, older snapshots
/// that were not retrieved can be dropped.
///
@available(macOS 27.0, iOS 27.0, *)
public final class NativeResponseStream: NativeObject, @unchecked Sendable {
  public static let type = Type.objectType(Symbol(uninterned: "response-stream"))

  private let condition = NSCondition()
  private var latest: NativeModelResponse? = nil
  private var delivered = true
  private var done = false
  private var error: Error? = nil
  let result = Future(external: false)
  var request: ModelRequest? = nil

  func update(_ snapshot: NativeModelResponse) {
    self.condition.lock()
    self.latest = snapshot
    self.delivered = false
    self.condition.broadcast()
    self.condition.unlock()
  }

  func finish(with error: Error?, in context: Context) {
    self.condition.lock()
    self.done = true
    self.error = error
    let latest = self.latest
    self.condition.broadcast()
    self.condition.unlock()
    if let error {
      _ = try? self.result.setResult(in: context,
                                     to: ModelErrors.raisable(error, in: context),
                                     raise: true)
    } else if let latest {
      _ = try? self.result.setResult(in: context, to: .object(latest.final))
    } else {
      _ = try? self.result.setResult(in: context,
                                     to: .error(ModelErrors.error("empty-response",
                                                                  "no response was received",
                                                                  in: context)),
                                     raise: true)
    }
  }

  var isDone: Bool {
    self.condition.lock()
    defer { self.condition.unlock() }
    return self.done && self.delivered
  }

  enum Next {
    case snapshot(NativeModelResponse)
    case failure(Error)
    case end
    case timeout
  }

  /// Returns the next snapshot. Waiting gets aborted if the evaluation of the current
  /// thread gets aborted.
  func next(in context: Context, timeout: TimeInterval?) throws -> Next {
    let deadline = timeout.map { Date(timeIntervalSinceNow: $0) }
    self.condition.lock()
    defer {
      self.condition.unlock()
    }
    while self.delivered && !self.done {
      if context.evaluator.threads.current?.value.machine?.abortionRequested ?? false {
        self.request?.cancel()
        throw RuntimeError.abortion()
      }
      if let deadline, Date() > deadline {
        return .timeout
      }
      let limit = Date(timeIntervalSinceNow: 0.1)
      _ = self.condition.wait(until: deadline.map { min($0, limit) } ?? limit)
    }
    if !self.delivered, let latest = self.latest {
      self.delivered = true
      return .snapshot(latest)
    } else if let error = self.error {
      return .failure(error)
    } else {
      return .end
    }
  }

  public override var type: Type {
    return Self.type
  }
}

#endif
