//
//  FoundationModelGeminiLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 21/11/2026.
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

#if canImport(FoundationModels) && canImport(GeminiForFoundationModels)

import Foundation
import FoundationModels
import GeminiForFoundationModels
import DynamicJSON

///
/// The backend of Gemini models. The name of a Gemini model is its model identifier.
/// The context size is the input token limit listed in the API's model catalog; it is
/// fetched on first use and then cached. Gemini models don't report supported languages
/// or token counts.
///
@available(macOS 27.0, iOS 27.0, *)
public final class GeminiModelBackend: LanguageModelBackend, @unchecked Sendable {
  public let gemini: GeminiLanguageModel
  public let auth: GeminiAuth

  /// The cached input token limit; `.some(nil)` if the catalog doesn't list one.
  private var inputTokenLimit: Int?? = nil
  private let lock = NSLock()

  public init(_ gemini: GeminiLanguageModel, auth: GeminiAuth) {
    self.gemini = gemini
    self.auth = auth
  }

  public var model: any LanguageModel {
    return self.gemini
  }

  public var family: String {
    return "gemini"
  }

  public var name: String {
    return self.gemini.model.id
  }

  public func contextSize() async throws -> Int? {
    if let limit = self.lock.withLock({ self.inputTokenLimit }) {
      return limit
    }
    let models = try await GeminiModel.available(auth: self.auth,
                                                 baseURL: self.gemini.baseURL,
                                                 timeout: self.gemini.timeout)
    let limit = models.first { $0.id == self.gemini.model.id }?.inputTokenLimit
    self.lock.withLock { self.inputTokenLimit = .some(limit) }
    return limit
  }
}

///
/// Implements library `(lispkit foundation-model gemini)`. It provides Google's Gemini
/// models as language models for `(lispkit foundation-model)`, based on the
/// GeminiForFoundationModels package.
///
@available(macOS 27.0, iOS 27.0, *)
public final class FoundationModelGeminiLibrary: NativeLibrary {

  /// Is the model family registered?
  nonisolated(unsafe) private static var modelFamilyRegistered = false
  private static let lock = NSLock()

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "foundation-model", "gemini"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "foundation-model"], "language-model?")
  }

  /// Declarations of the library.
  public override func declarations() {
    Self.registerModelFamily()
    self.define(Procedure("make-gemini-model", self.makeGeminiModel))
    self.define(Procedure("gemini-models", self.geminiModels))
    self.define(Procedure("gemini-model-capabilities", self.geminiModelCapabilities))
    self.define(Procedure("gemini-available-models", self.geminiAvailableModels))
    self.define(Procedure("gemini-tool-activities", self.geminiToolActivities))
  }
  
  private static func registerModelFamily() {
    Self.lock.lock()
    defer {
      Self.lock.unlock()
    }
    guard !Self.modelFamilyRegistered else {
      return
    }
    Self.modelFamilyRegistered = true
    ModelErrors.register { error in
      if let error = error as? GeminiError {
        let message = error.errorDescription ?? error.localizedDescription
        switch error {
          case .missingCredential:
            return ("missing-credential", message)
          case .invalidCredential:
            return ("invalid-credential", message)
          case .unsupportedThinkingLevel(_, _):
            return ("unsupported-thinking-level", message)
          case .incompleteResponse:
            return ("incomplete-response", message)
          @unknown default:
            return ("gemini-error", message)
        }
      } else if let error = error as? GeminiAPIError {
        return (error.code.replacingOccurrences(of: "_", with: "-"), error.message)
      }
      return nil
    }
    LanguageModelFamilies.register(family: "gemini", vendor: "Google") {
      GeminiModel.knownModels.map(\.id)
    }
  }

  // MARK: - Helpers

  private func sym(_ str: String) -> Expr {
    return .symbol(self.context.symbols.intern(str))
  }

  private func entry(_ key: String, _ value: Expr) -> Expr {
    return .pair(self.sym(key), value)
  }

  private func invalid(_ reason: String, _ expr: Expr) -> RuntimeError {
    return RuntimeError.custom("error", "\(reason): $0", [expr])
  }

  private func list(_ expr: Expr) throws -> [Expr] {
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

  private func identifier(_ expr: Expr) throws -> String {
    switch expr {
      case .symbol(let sym):
        return sym.identifier
      case .string(let str):
        return str as String
      default:
        throw RuntimeError.type(expr, expected: [.symbolType, .strType])
    }
  }

  private func thinkingLevel(_ expr: Expr) throws -> GeminiModel.ThinkingLevel {
    guard let level = GeminiModel.ThinkingLevel(rawValue: try self.identifier(expr)) else {
      throw self.invalid("unknown thinking level", expr)
    }
    return level
  }

  private func capabilities(from expr: Expr) throws -> GeminiModel.Capabilities {
    var caps = GeminiModel.Capabilities()
    for entry in try self.list(expr) {
      guard case .pair(let key, let value) = entry else {
        throw RuntimeError.type(entry, expected: [.pairType])
      }
      switch try self.identifier(key) {
        case "thinking-levels":
          caps.thinkingLevels = Set(try self.list(value).map(self.thinkingLevel))
        case "sampling-params":
          caps.samplingParams = value.isTrue
        case "structured-output":
          caps.structuredOutput = value.isTrue
        case "image-input":
          caps.imageInput = value.isTrue
        case "document-input":
          caps.documentInput = value.isTrue
        case "audio-input":
          caps.audioInput = value.isTrue
        case "video-input":
          caps.videoInput = value.isTrue
        default:
          throw self.invalid("unknown Gemini model capability", key)
      }
    }
    return caps
  }

  private func expr(from caps: GeminiModel.Capabilities) -> Expr {
    return .makeList(
      self.entry("thinking-levels",
                 .makeList(Exprs(GeminiModel.ThinkingLevel.allCases.filter {
                   caps.thinkingLevels.contains($0)
                 }.map { self.sym($0.rawValue) }))),
      self.entry("sampling-params", .makeBoolean(caps.samplingParams)),
      self.entry("structured-output", .makeBoolean(caps.structuredOutput)),
      self.entry("image-input", .makeBoolean(caps.imageInput)),
      self.entry("document-input", .makeBoolean(caps.documentInput)),
      self.entry("audio-input", .makeBoolean(caps.audioInput)),
      self.entry("video-input", .makeBoolean(caps.videoInput)))
  }

  private func model(_ expr: Expr, capabilities: GeminiModel.Capabilities?) throws -> GeminiModel {
    let id = try self.identifier(expr)
    if let capabilities {
      return GeminiModel(id: id, capabilities: capabilities)
    } else if let model = GeminiModel.named(id) {
      return model
    }
    throw self.invalid("unknown Gemini model; specify its capabilities", expr)
  }

  private func auth(from expr: Expr?) throws -> GeminiAuth {
    guard let expr, expr.isTrue else {
      guard let key = ProcessInfo.processInfo.environment["GEMINI_API_KEY"], !key.isEmpty else {
        throw ModelErrors.error("missing-credential",
                                "no Gemini credential; GEMINI_API_KEY is not set",
                                in: self.context)
      }
      return .apiKey(key)
    }
    switch expr {
      case .string(let str):
        return .apiKey(str as String)
      case .pair(.symbol(let kind), .pair(let arg, .null)):
        switch kind.identifier {
          case "api-key":
            return .apiKey(try arg.asString())
          case "proxied":
            var headers: [String : String] = [:]
            for header in try self.list(arg) {
              guard case .pair(let name, let value) = header else {
                throw RuntimeError.type(header, expected: [.pairType])
              }
              headers[try self.identifier(name)] = try value.asString()
            }
            return .proxied(headers: headers)
          default:
            break
        }
      default:
        break
    }
    throw self.invalid("invalid Gemini authentication", expr)
  }

  private func builtInTool(from expr: Expr) throws -> GeminiBuiltInTool {
    switch try self.identifier(expr) {
      case "google-search":
        return .googleSearch
      case "url-context":
        return .urlContext
      case "code-execution":
        return .codeExecution
      default:
        throw self.invalid("unknown built-in tool", expr)
    }
  }

  private func geminiModel(from expr: Expr) throws -> GeminiLanguageModel {
    guard case .object(let obj) = expr,
          let model = obj as? NativeLanguageModel,
          let gemini = model.model as? GeminiLanguageModel else {
      throw RuntimeError.type(expr, expected: [NativeLanguageModel.type])
    }
    return gemini
  }

  /// Converts JSON text into a Scheme value following the conventions of `(lispkit json)`.
  private func value(fromJSON text: String) -> Expr {
    guard let json = try? JSON(string: text) else {
      return .makeString(text)
    }
    return json.toExpr(in: self.context)
  }

  // MARK: - Models

  private func makeGeminiModel(id: Expr, auth: Expr?, options: Expr?) throws -> Expr {
    var capabilities: GeminiModel.Capabilities? = nil
    var thinkingLevel: GeminiModel.ThinkingLevel? = nil
    var thoughtSummaries = true
    var builtInTools = Set<GeminiBuiltInTool>()
    var replaysEarlierThoughts = false
    var retry = GeminiRetryPolicy.default
    var baseURL = GeminiLanguageModel.defaultBaseURL
    var timeout: TimeInterval = 120
    if let options, options.isTrue {
      for option in try self.list(options) {
        guard case .pair(let key, let value) = option else {
          throw RuntimeError.type(option, expected: [.pairType])
        }
        let name = try self.identifier(key)
        if name == "thought-summaries" {
          thoughtSummaries = value.isTrue
          continue
        } else if name == "replay-earlier-thoughts" {
          replaysEarlierThoughts = value.isTrue
          continue
        } else if name == "retries" {
          retry = value.isFalse ? .disabled
                                : GeminiRetryPolicy(maxRetries: try value.asInt(above: 0, below: 100))
          continue
        } else if value.isFalse {
          continue
        }
        switch name {
          case "capabilities":
            capabilities = try self.capabilities(from: value)
          case "thinking-level":
            thinkingLevel = try self.thinkingLevel(value)
          case "built-in-tools":
            builtInTools = Set(try self.list(value).map(self.builtInTool(from:)))
          case "base-url":
            guard let url = URL(string: try value.asString()) else {
              throw self.invalid("invalid base URL", value)
            }
            baseURL = url
          case "timeout":
            timeout = try value.asDouble(coerce: true)
          default:
            throw self.invalid("unknown Gemini model option", key)
        }
      }
    }
    let model = try self.model(id, capabilities: capabilities)
    do {
      let auth = try self.auth(from: auth)
      let gemini = try GeminiLanguageModel(name: model,
                                           auth: auth,
                                           thinkingLevel: thinkingLevel,
                                           thoughtSummaries: thoughtSummaries,
                                           builtInTools: builtInTools,
                                           replaysEarlierThoughts: replaysEarlierThoughts,
                                           retry: retry,
                                           baseURL: baseURL,
                                           timeout: timeout)
      return .object(NativeLanguageModel(GeminiModelBackend(gemini, auth: auth),
                                         label: "gemini \(model.id)"))
    } catch let error as GeminiError {
      throw ModelErrors.error("unsupported-thinking-level",
                              error.errorDescription ?? "unsupported thinking level",
                              in: self.context)
    }
  }

  private func geminiModels() -> Expr {
    return .makeList(Exprs(GeminiModel.knownModels.map { .makeString($0.id) }))
  }

  private func geminiModelCapabilities(expr: Expr) throws -> Expr {
    if case .object(_) = expr {
      return self.expr(from: try self.geminiModel(from: expr).model.capabilities)
    }
    guard let model = GeminiModel.named(try self.identifier(expr)) else {
      return .false
    }
    return self.expr(from: model.capabilities)
  }

  private func geminiAvailableModels(auth: Expr?) throws -> Expr {
    let auth = try self.auth(from: auth)
    let future = Future(external: false)
    let context = self.context
    Task {
      do {
        let models = try await GeminiModel.available(auth: auth)
        let res: Expr = .makeList(Exprs(models.map { info in
          .makeList(self.entry("id", .makeString(info.id)),
                    self.entry("display-name", .makeString(info.displayName)),
                    self.entry("description", .makeString(info.description)),
                    self.entry("input-token-limit",
                               info.inputTokenLimit.map { .fixnum(Int64($0)) } ?? .false),
                    self.entry("output-token-limit",
                               info.outputTokenLimit.map { .fixnum(Int64($0)) } ?? .false),
                    self.entry("thinking", .makeBoolean(info.thinking)),
                    self.entry("methods",
                               .makeList(Exprs(info.supportedGenerationMethods.map {
                                 .makeString($0)
                               }))))
        }))
        _ = try? future.setResult(in: context, to: res)
      } catch {
        _ = try? future.setResult(in: context,
                                  to: ModelErrors.raisable(error, in: context),
                                  raise: true)
      }
    }
    return .object(future)
  }

  // MARK: - Transcripts

  private func expr(from activity: GeminiToolActivity) -> Expr {
    let kind: Expr
    switch activity.kind {
      case .googleSearch:
        kind = self.sym("google-search")
      case .urlContext:
        kind = self.sym("url-context")
      case .codeExecution:
        kind = self.sym("code-execution")
      case .other(let name):
        kind = self.sym(name.replacingOccurrences(of: "_", with: "-"))
      @unknown default:
        kind = self.sym("unknown")
    }
    return .makeList(self.entry("id", .makeString(activity.id)),
                     self.entry("kind", kind),
                     self.entry("arguments", self.value(fromJSON: activity.arguments)),
                     self.entry("result", activity.result.map { self.value(fromJSON: $0) } ?? .false),
                     self.entry("error", .makeBoolean(activity.isError)))
  }

  private func geminiToolActivities(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let t = obj as? NativeTranscript else {
      throw RuntimeError.type(expr, expected: [NativeTranscript.type])
    }
    return .makeList(Exprs(GeminiToolActivity.activities(in: t.transcript).map(self.expr(from:))))
  }
}

#endif
