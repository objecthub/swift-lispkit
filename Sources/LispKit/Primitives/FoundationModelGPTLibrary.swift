//
//  FoundationModelGPTLibrary.swift
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

#if canImport(FoundationModels) && canImport(GPTForFoundationModels)

import Foundation
import FoundationModels
import GPTForFoundationModels
import DynamicJSON

///
/// The backend of GPT models. The name of a GPT model is its model identifier. Tokens are
/// counted by the API; GPT models don't report a context size or supported languages.
///
@available(macOS 27.0, iOS 27.0, *)
public struct GPTModelBackend: LanguageModelBackend {
  public let gpt: GPTLanguageModel

  public init(_ gpt: GPTLanguageModel) {
    self.gpt = gpt
  }

  public var model: any LanguageModel {
    return self.gpt
  }

  public var family: String {
    return "gpt"
  }

  public var name: String {
    return self.gpt.model.id
  }

  public func tokenCount(for input: LanguageModelTokenInput) async throws -> Int? {
    switch input {
      case .prompt(_, let segments):
        guard let segments else {
          return nil
        }
        return try await self.gpt.tokenCount(forPrompt: segments)
      case .instructions(_, let segments):
        guard let segments else {
          return nil
        }
        return try await self.gpt.tokenCount(forInstructions: segments)
      case .tools(let tools):
        return try await self.gpt.tokenCount(for: tools)
      case .schema(let schema):
        return try await self.gpt.tokenCount(for: schema)
      case .transcript(let transcript):
        return try await self.gpt.tokenCount(for: transcript)
    }
  }
}

///
/// Implements library `(lispkit foundation-model gpt)`. It provides OpenAI's GPT models
/// as language models for `(lispkit foundation-model)`, based on the
/// GPTForFoundationModels package.
///
@available(macOS 27.0, iOS 27.0, *)
public final class FoundationModelGPTLibrary: NativeLibrary {

  /// Is the error translator registered?
  nonisolated(unsafe) private static var translatorRegistered = false
  private static let lock = NSLock()

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "foundation-model", "gpt"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "foundation-model"], "language-model?")
  }

  /// Declarations of the library.
  public override func declarations() {
    Self.registerErrorTranslator()
    LanguageModelFamilies.register(family: "gpt", vendor: "OpenAI") {
      GPTModel.knownModels.map(\.id)
    }
    self.define(Procedure("make-gpt-model", self.makeGPTModel))
    self.define(Procedure("gpt-models", self.gptModels))
    self.define(Procedure("gpt-model-capabilities", self.gptModelCapabilities))
    self.define(Procedure("gpt-available-models", self.gptAvailableModels))
    self.define(Procedure("gpt-tool-activities", self.gptToolActivities))
  }

  private static func registerErrorTranslator() {
    Self.lock.lock()
    defer { Self.lock.unlock() }
    guard !Self.translatorRegistered else {
      return
    }
    Self.translatorRegistered = true
    ModelErrors.register { error in
      if let error = error as? GPTError {
        let message = error.errorDescription ?? error.localizedDescription
        switch error {
          case .missingCredential:
            return ("missing-credential", message)
          case .invalidCredential:
            return ("invalid-credential", message)
          case .unsupportedReasoningEffort(_, _):
            return ("unsupported-reasoning-effort", message)
          case .incompleteResponse(_):
            return ("incomplete-response", message)
          @unknown default:
            return ("gpt-error", message)
        }
      } else if let error = error as? GPTAPIError {
        return (error.code.replacingOccurrences(of: "_", with: "-"), error.message)
      }
      return nil
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

  private func reasoningEffort(_ expr: Expr) throws -> GPTModel.ReasoningEffort {
    guard let effort = GPTModel.ReasoningEffort(rawValue: try self.identifier(expr)) else {
      throw self.invalid("unknown reasoning effort", expr)
    }
    return effort
  }

  private func capabilities(from expr: Expr) throws -> GPTModel.Capabilities {
    var caps = GPTModel.Capabilities()
    for entry in try self.list(expr) {
      guard case .pair(let key, let value) = entry else {
        throw RuntimeError.type(entry, expected: [.pairType])
      }
      switch try self.identifier(key) {
        case "reasoning-efforts":
          caps.reasoningEfforts = Set(try self.list(value).map(self.reasoningEffort))
        case "default-reasoning-effort":
          caps.defaultReasoningEffort = value.isFalse ? nil : try self.reasoningEffort(value)
        case "sampling-params":
          caps.samplingParams = value.isTrue
        case "structured-output":
          caps.structuredOutput = value.isTrue
        case "image-input":
          caps.imageInput = value.isTrue
        case "document-input":
          caps.documentInput = value.isTrue
        default:
          throw self.invalid("unknown GPT model capability", key)
      }
    }
    if caps.defaultReasoningEffort == nil && !caps.reasoningEfforts.isEmpty {
      caps.defaultReasoningEffort = caps.reasoningEfforts.contains(.medium)
        ? .medium : caps.reasoningEfforts.sorted().first
    }
    return caps
  }

  private func expr(from caps: GPTModel.Capabilities) -> Expr {
    return .makeList(
      self.entry("reasoning-efforts",
                 .makeList(Exprs(GPTModel.ReasoningEffort.allCases.filter {
                   caps.reasoningEfforts.contains($0)
                 }.map { self.sym($0.rawValue) }))),
      self.entry("default-reasoning-effort",
                 caps.defaultReasoningEffort.map { self.sym($0.rawValue) } ?? .false),
      self.entry("sampling-params", .makeBoolean(caps.samplingParams)),
      self.entry("structured-output", .makeBoolean(caps.structuredOutput)),
      self.entry("image-input", .makeBoolean(caps.imageInput)),
      self.entry("document-input", .makeBoolean(caps.documentInput)))
  }

  private func model(_ expr: Expr, capabilities: GPTModel.Capabilities?) throws -> GPTModel {
    let id = try self.identifier(expr)
    if let capabilities {
      return GPTModel(id: id, capabilities: capabilities)
    } else if let model = GPTModel.named(id) {
      return model
    }
    throw self.invalid("unknown GPT model; specify its capabilities", expr)
  }

  private func environment(_ name: String) -> String? {
    guard let value = ProcessInfo.processInfo.environment[name], !value.isEmpty else {
      return nil
    }
    return value
  }

  private func auth(from expr: Expr?) throws -> GPTAuth {
    guard let expr, expr.isTrue else {
      guard let key = self.environment("OPENAI_API_KEY") else {
        throw ModelErrors.error("missing-credential",
                                "no OpenAI credential; OPENAI_API_KEY is not set",
                                in: self.context)
      }
      return .apiKey(key,
                     organization: self.environment("OPENAI_ORG_ID"),
                     project: self.environment("OPENAI_PROJECT_ID"))
    }
    switch expr {
      case .string(let str):
        return .apiKey(str as String)
      case .pair(.symbol(let kind), let args):
        let args = try self.list(args)
        switch kind.identifier {
          case "api-key" where args.count >= 1 && args.count <= 3:
            let organization = args.count > 1 && args[1].isTrue ? try args[1].asString() : nil
            let project = args.count > 2 && args[2].isTrue ? try args[2].asString() : nil
            return .apiKey(try args[0].asString(), organization: organization, project: project)
          case "proxied" where args.count == 1:
            var headers: [String : String] = [:]
            for header in try self.list(args[0]) {
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
    throw self.invalid("invalid OpenAI authentication", expr)
  }

  private func builtInTool(from expr: Expr) throws -> GPTBuiltInTool {
    switch try self.identifier(expr) {
      case "web-search":
        return .webSearch
      case "code-interpreter":
        return .codeInterpreter
      default:
        throw self.invalid("unknown built-in tool", expr)
    }
  }

  private func gptModel(from expr: Expr) throws -> GPTLanguageModel {
    guard case .object(let obj) = expr,
          let model = obj as? NativeLanguageModel,
          let gpt = model.model as? GPTLanguageModel else {
      throw RuntimeError.type(expr, expected: [NativeLanguageModel.type])
    }
    return gpt
  }

  /// Converts JSON text into a Scheme value following the conventions of `(lispkit json)`.
  private func value(fromJSON text: String) -> Expr {
    guard let json = try? JSON(string: text) else {
      return .makeString(text)
    }
    return json.toExpr(in: self.context)
  }

  private func date(_ date: Date) -> Expr {
    return .object(NativeDateTime(DateTimeLibrary.calendar.dateComponents(in: .current, from: date)))
  }

  // MARK: - Models

  private func makeGPTModel(id: Expr, auth: Expr?, options: Expr?) throws -> Expr {
    var capabilities: GPTModel.Capabilities? = nil
    var reasoningEffort: GPTModel.ReasoningEffort? = nil
    var reasoningSummaries = true
    var builtInTools = Set<GPTBuiltInTool>()
    var state = GPTConversationState.stateless
    var baseURL = GPTLanguageModel.defaultBaseURL
    var timeout: TimeInterval = 120
    if let options, options.isTrue {
      for option in try self.list(options) {
        guard case .pair(let key, let value) = option else {
          throw RuntimeError.type(option, expected: [.pairType])
        }
        let name = try self.identifier(key)
        switch name {
          case "reasoning-summaries":
            reasoningSummaries = value.isTrue
            continue
          case "stateful":
            state = value.isTrue ? .stored : .stateless
            continue
          default:
            if value.isFalse {
              continue
            }
        }
        switch name {
          case "capabilities":
            capabilities = try self.capabilities(from: value)
          case "reasoning-effort":
            reasoningEffort = try self.reasoningEffort(value)
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
            throw self.invalid("unknown GPT model option", key)
        }
      }
    }
    let model = try self.model(id, capabilities: capabilities)
    let auth = try self.auth(from: auth)
    do {
      let gpt = try GPTLanguageModel(name: model,
                                     auth: auth,
                                     reasoningEffort: reasoningEffort,
                                     reasoningSummaries: reasoningSummaries,
                                     builtInTools: builtInTools,
                                     state: state,
                                     baseURL: baseURL,
                                     timeout: timeout)
      return .object(NativeLanguageModel(GPTModelBackend(gpt), label: "gpt \(model.id)"))
    } catch let error as GPTError {
      throw ModelErrors.translate(error, in: self.context)
    }
  }

  private func gptModels() -> Expr {
    return .makeList(Exprs(GPTModel.knownModels.map { .makeString($0.id) }))
  }

  private func gptModelCapabilities(expr: Expr) throws -> Expr {
    if case .object(_) = expr {
      return self.expr(from: try self.gptModel(from: expr).model.capabilities)
    }
    guard let model = GPTModel.named(try self.identifier(expr)) else {
      return .false
    }
    return self.expr(from: model.capabilities)
  }

  private func gptAvailableModels(auth: Expr?) throws -> Expr {
    let auth = try self.auth(from: auth)
    let future = Future(external: false)
    let context = self.context
    Task {
      do {
        let models = try await GPTModel.available(auth: auth)
        let res: Expr = .makeList(Exprs(models.map { info in
          .makeList(self.entry("id", .makeString(info.id)),
                    self.entry("created", info.created.map(self.date) ?? .false),
                    self.entry("owned-by", .makeString(info.ownedBy)))
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

  private func expr(from activity: GPTToolActivity) -> Expr {
    var props: Exprs = [self.entry("id", .makeString(activity.id))]
    switch activity.kind {
      case .webSearch:
        props.append(self.entry("kind", self.sym("web-search")))
        props.append(self.entry("queries", .makeList(Exprs(activity.queries.map { .makeString($0) }))))
        props.append(self.entry("sources", .makeList(Exprs(activity.sources.map { .makeString($0) }))))
      case .codeInterpreter:
        props.append(self.entry("kind", self.sym("code-interpreter")))
        props.append(self.entry("code", activity.code.map { .makeString($0) } ?? .false))
        props.append(self.entry("output", activity.output.map { .makeString($0) } ?? .false))
      case .other(let name):
        props.append(self.entry("kind", self.sym(name.replacingOccurrences(of: "_", with: "-"))))
      @unknown default:
        props.append(self.entry("kind", self.sym("unknown")))
    }
    props.append(self.entry("status", self.sym(activity.status)))
    props.append(self.entry("error", .makeBoolean(activity.isError)))
    props.append(self.entry("item", self.value(fromJSON: activity.item)))
    return .makeList(props)
  }

  private func gptToolActivities(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let t = obj as? NativeTranscript else {
      throw RuntimeError.type(expr, expected: [NativeTranscript.type])
    }
    return .makeList(Exprs(GPTToolActivity.activities(in: t.transcript).map(self.expr(from:))))
  }
}

#endif
