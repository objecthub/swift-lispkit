//
//  FoundationModelClaudeLibrary.swift
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

#if canImport(FoundationModels) && canImport(ClaudeForFoundationModels)

import Foundation
import FoundationModels
import ClaudeForFoundationModels

///
/// The backend of Claude models. The name of a Claude model is its model identifier.
/// Claude models don't report a context size, supported languages, or token counts.
///
@available(macOS 27.0, iOS 27.0, *)
public struct ClaudeModelBackend: LanguageModelBackend {
  public let claude: ClaudeLanguageModel

  public init(_ claude: ClaudeLanguageModel) {
    self.claude = claude
  }

  public var model: any LanguageModel {
    return self.claude
  }

  public var family: String {
    return "claude"
  }

  public var name: String {
    return self.claude.model.id
  }
}

///
/// Implements library `(lispkit foundation-model claude)`. It provides Anthropic's Claude
/// models as language models for `(lispkit foundation-model)`, based on the
/// ClaudeForFoundationModels package.
///
@available(macOS 27.0, iOS 27.0, *)
public final class FoundationModelClaudeLibrary: NativeLibrary {

  /// Models known to ClaudeForFoundationModels.
  private static let knownModels: [ClaudeModel] = [
    .haiku5_5, .sonnet5_5, .opus5_5, .fable5_1, .fable5, .opus5, .sonnet5,
    .opus4_8, .opus4_7, .opus4_6, .sonnet4_6, .haiku4_5
  ]

  /// Is the model family registered?
  nonisolated(unsafe) private static var modelFamilyRegistered = false
  private static let lock = NSLock()

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "foundation-model", "claude"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "foundation-model"], "language-model?")
  }

  /// Declarations of the library.
  public override func declarations() {
    Self.registerModelFamily()
    self.define(Procedure("make-claude-model", self.makeClaudeModel))
    self.define(Procedure("claude-models", self.claudeModels))
    self.define(Procedure("claude-model-capabilities", self.claudeModelCapabilities))
    self.define(Procedure("claude-authenticate", self.claudeAuthenticate))
    self.define(Procedure("claude-server-tool-activities", self.claudeServerToolActivities))
    self.define(Procedure("claude-handovers", self.claudeHandovers))
    self.define(Procedure("claude-response-model-id", self.claudeResponseModelId))
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
      guard let error = error as? ClaudeError else {
        return nil
      }
      let message = error.errorDescription ?? error.localizedDescription
      switch error {
        case .missingCredential:
          return ("missing-credential", message)
        case .attestationUnsupported:
          return ("attestation-unsupported", message)
        case .attestationFailed:
          return ("attestation-failed", message)
        @unknown default:
          return ("claude-error", message)
      }
    }
    LanguageModelFamilies.register(family: "claude", vendor: "Anthropic") {
      Self.knownModels.map(\.id)
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

  private func effort(_ expr: Expr) throws -> ClaudeModel.Effort {
    guard let effort = ClaudeModel.Effort(rawValue: try self.identifier(expr)) else {
      throw self.invalid("unknown effort level", expr)
    }
    return effort
  }

  private func capabilities(from expr: Expr) throws -> ClaudeModel.Capabilities {
    var caps = ClaudeModel.Capabilities()
    for entry in try self.list(expr) {
      guard case .pair(let key, let value) = entry else {
        throw RuntimeError.type(entry, expected: [.pairType])
      }
      switch try self.identifier(key) {
        case "sampling-params":
          caps.samplingParams = value.isTrue
        case "effort-levels":
          caps.effortLevels = Set(try self.list(value).map(self.effort))
        case "adaptive-thinking":
          caps.adaptiveThinking = value.isTrue
        case "structured-output":
          caps.structuredOutput = value.isTrue
        case "image-input":
          caps.imageInput = value.isTrue
        default:
          throw self.invalid("unknown Claude model capability", key)
      }
    }
    return caps
  }

  private func expr(from caps: ClaudeModel.Capabilities) -> Expr {
    let order: [ClaudeModel.Effort] = [.low, .medium, .high, .xhigh, .max]
    return .makeList(
      self.entry("sampling-params", .makeBoolean(caps.samplingParams)),
      self.entry("effort-levels",
                 .makeList(Exprs(order.filter { caps.effortLevels.contains($0) }.map {
                   self.sym($0.rawValue)
                 }))),
      self.entry("adaptive-thinking", .makeBoolean(caps.adaptiveThinking)),
      self.entry("structured-output", .makeBoolean(caps.structuredOutput)),
      self.entry("image-input", .makeBoolean(caps.imageInput)))
  }

  private func knownModel(_ id: String) -> ClaudeModel? {
    return Self.knownModels.first { $0.id == id }
  }

  private func model(_ expr: Expr, capabilities: ClaudeModel.Capabilities?) throws -> ClaudeModel {
    let id = try self.identifier(expr)
    if let capabilities {
      return ClaudeModel(id: id, capabilities: capabilities)
    } else if let model = self.knownModel(id) {
      return model
    }
    throw self.invalid("unknown Claude model; specify its capabilities", expr)
  }

  private func auth(from expr: Expr?) throws -> AuthMode {
    guard let expr, expr.isTrue else {
      guard let key = ProcessInfo.processInfo.environment["ANTHROPIC_API_KEY"], !key.isEmpty else {
        throw ModelErrors.error("missing-credential",
                                "no Claude credential; ANTHROPIC_API_KEY is not set",
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
          case "app-attest":
            return .appAttest(clientID: try arg.asString())
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
    throw self.invalid("invalid Claude authentication", expr)
  }

  private func domainFilter(_ expr: Expr) throws -> ClaudeServerTool.DomainFilter {
    guard case .pair(.symbol(let kind), let domains) = expr else {
      throw self.invalid("invalid domain filter", expr)
    }
    let ds = try self.list(domains).map { try $0.asString() }
    switch kind.identifier {
      case "allowing":
        return .allowing(ds)
      case "blocking":
        return .blocking(ds)
      default:
        throw self.invalid("invalid domain filter", expr)
    }
  }

  private func serverTool(from expr: Expr) throws -> ClaudeServerTool {
    var name: String
    var args: [Expr] = []
    switch expr {
      case .symbol(let sym):
        name = sym.identifier
      case .pair(.symbol(let sym), let rest):
        name = sym.identifier
        args = try self.list(rest)
      default:
        throw self.invalid("invalid server tool", expr)
    }
    var domains = ClaudeServerTool.DomainFilter.unrestricted
    var maxUses: Int? = nil
    for arg in args {
      if case .pair(_, _) = arg {
        domains = try self.domainFilter(arg)
      } else if arg.isTrue {
        maxUses = try arg.asInt(above: 1)
      }
    }
    switch name {
      case "web-search":
        return .webSearch(domains: domains, maxUses: maxUses)
      case "web-fetch":
        return .webFetch(domains: domains, maxUses: maxUses)
      case "code-execution" where args.isEmpty:
        return .codeExecution
      default:
        throw self.invalid("invalid server tool", expr)
    }
  }

  private func claudeModel(from expr: Expr) throws -> ClaudeLanguageModel {
    guard case .object(let obj) = expr,
          let model = obj as? NativeLanguageModel,
          let claude = model.model as? ClaudeLanguageModel else {
      throw RuntimeError.type(expr, expected: [NativeLanguageModel.type])
    }
    return claude
  }

  private func response(_ transcript: Expr, _ index: Expr) throws -> Transcript.Response {
    guard case .object(let obj) = transcript, let t = obj as? NativeTranscript else {
      throw RuntimeError.type(transcript, expected: [NativeTranscript.type])
    }
    let i = try index.asInt(below: t.transcript.count)
    guard case .response(let response) = t.transcript[i] else {
      throw self.invalid("transcript entry is not a response", index)
    }
    return response
  }

  // MARK: - Models

  private func makeClaudeModel(id: Expr, auth: Expr?, options: Expr?) throws -> Expr {
    var capabilities: ClaudeModel.Capabilities? = nil
    var effort: ClaudeModel.Effort? = nil
    var fallbacks = ClaudeFallbacks.models([])
    var serverTools = Set<ClaudeServerTool>()
    var userProfileID: String? = nil
    var baseURL = ClaudeLanguageModel.defaultBaseURL
    var timeout: TimeInterval = 60
    var fallbackIds: [Expr] = []
    if let options, options.isTrue {
      for option in try self.list(options) {
        guard case .pair(let key, let value) = option else {
          throw RuntimeError.type(option, expected: [.pairType])
        }
        if value.isFalse {
          continue
        }
        switch try self.identifier(key) {
          case "capabilities":
            capabilities = try self.capabilities(from: value)
          case "effort":
            effort = try self.effort(value)
          case "fallbacks":
            if case .symbol(let sym) = value, sym.identifier == "server-default" {
              fallbacks = .serverDefault
            } else {
              fallbackIds = try self.list(value)
            }
          case "server-tools":
            serverTools = Set(try self.list(value).map(self.serverTool(from:)))
          case "user-profile-id":
            userProfileID = try value.asString()
          case "base-url":
            guard let url = URL(string: try value.asString()) else {
              throw self.invalid("invalid base URL", value)
            }
            baseURL = url
          case "timeout":
            timeout = try value.asDouble(coerce: true)
          default:
            throw self.invalid("unknown Claude model option", key)
        }
      }
    }
    let model = try self.model(id, capabilities: capabilities)
    if !fallbackIds.isEmpty {
      fallbacks = .models(try fallbackIds.map { try self.model($0, capabilities: nil) })
    }
    // `ClaudeLanguageModel` checks the effort level with a precondition
    if let effort, !model.capabilities.effortLevels.contains(effort) {
      throw self.invalid("effort level not supported by \(model.id)", self.sym(effort.rawValue))
    }
    let claude = ClaudeLanguageModel(name: model,
                                     auth: try self.auth(from: auth),
                                     userProfileID: userProfileID,
                                     fixedEffort: effort,
                                     fallbacks: fallbacks,
                                     serverTools: serverTools,
                                     baseURL: baseURL,
                                     timeout: timeout)
    return .object(NativeLanguageModel(ClaudeModelBackend(claude), label: "claude \(model.id)"))
  }

  private func claudeModels() -> Expr {
    return .makeList(Exprs(Self.knownModels.map { .makeString($0.id) }))
  }

  private func claudeModelCapabilities(expr: Expr) throws -> Expr {
    if case .object(_) = expr {
      return self.expr(from: try self.claudeModel(from: expr).model.capabilities)
    }
    guard let model = self.knownModel(try self.identifier(expr)) else {
      return .false
    }
    return self.expr(from: model.capabilities)
  }

  private func claudeAuthenticate(expr: Expr) throws -> Expr {
    let claude = try self.claudeModel(from: expr)
    let future = Future(external: false)
    let context = self.context
    Task {
      do {
        try await claude.authenticateIfNeeded()
        _ = try? future.setResult(in: context, to: .true)
      } catch {
        _ = try? future.setResult(in: context,
                                  to: ModelErrors.raisable(error, in: context),
                                  raise: true)
      }
    }
    return .object(future)
  }

  // MARK: - Transcripts

  private func expr(from activity: ClaudeServerToolActivity) -> Expr {
    var props: [Expr] = [self.entry("id", .makeString(activity.id)),
                         self.entry("tool", .makeString(activity.toolName))]
    func failure(_ code: String) -> Expr {
      return self.entry("error", .makeString(code))
    }
    switch activity.content {
      case .webSearch(let search):
        props.append(self.entry("kind", self.sym("web-search")))
        props.append(self.entry("query", .makeString(search.query)))
        switch search.outcome {
          case .some(.results(let hits)):
            props.append(self.entry("results", .makeList(Exprs(hits.map { hit in
              .makeList(self.entry("url", .makeString(hit.url.absoluteString)),
                        self.entry("title", .makeString(hit.title)),
                        self.entry("page-age", hit.pageAge.map { .makeString($0) } ?? .false))
            }))))
          case .some(.failure(errorCode: let code)):
            props.append(failure(code))
          default:
            break
        }
      case .webFetch(let fetch):
        props.append(self.entry("kind", self.sym("web-fetch")))
        props.append(self.entry("url", .makeString(fetch.url.absoluteString)))
        switch fetch.outcome {
          case .some(.document(let doc)):
            props.append(self.entry("document", .makeList(
              self.entry("url", doc.url.map { .makeString($0.absoluteString) } ?? .false),
              self.entry("title", doc.title.map { .makeString($0) } ?? .false),
              self.entry("text", doc.text.map { .makeString($0) } ?? .false),
              self.entry("media-type", doc.mediaType.map { .makeString($0) } ?? .false),
              self.entry("retrieved-at", doc.retrievedAt.map { .makeString($0) } ?? .false))))
          case .some(.failure(errorCode: let code)):
            props.append(failure(code))
          default:
            break
        }
      case .codeExecution(let exec):
        props.append(self.entry("kind", self.sym("code-execution")))
        props.append(self.entry("code", .makeString(exec.code)))
        switch exec.outcome {
          case .some(.output(let output)):
            props.append(self.entry("stdout", output.stdout.map { .makeString($0) } ?? .false))
            props.append(self.entry("stderr", output.stderr.map { .makeString($0) } ?? .false))
            props.append(self.entry("return-code",
                                    output.returnCode.map { .makeNumber($0) } ?? .false))
          case .some(.failure(errorCode: let code)):
            props.append(failure(code))
          default:
            break
        }
      case .unrecognized(let unrecognized):
        props.append(self.entry("kind", self.sym("unrecognized")))
        props.append(self.entry("result-type",
                                unrecognized.resultType.map { .makeString($0) } ?? .false))
      @unknown default:
        props.append(self.entry("kind", self.sym("unrecognized")))
    }
    return .makeList(Exprs(props))
  }

  private func claudeServerToolActivities(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let t = obj as? NativeTranscript else {
      throw RuntimeError.type(expr, expected: [NativeTranscript.type])
    }
    return .makeList(Exprs(t.transcript.claudeServerToolActivities.map(self.expr(from:))))
  }

  private func claudeHandovers(expr: Expr, index: Expr) throws -> Expr {
    let response = try self.response(expr, index)
    return .makeList(Exprs(response.claudeHandovers.map { handover in
      var reason: Expr = .false
      switch handover.reason {
        case .some(.refusal(category: let category)):
          reason = .makeList(self.sym("refusal"), category.map { .makeString($0) } ?? .false)
        case .some(.unrecognized(type: let type)):
          reason = .makeList(self.sym("unrecognized"), .makeString(type))
        default:
          break
      }
      return .makeList(self.entry("from", .makeString(handover.fromModelID)),
                       self.entry("to", .makeString(handover.toModelID)),
                       self.entry("reason", reason))
    }))
  }

  private func claudeResponseModelId(expr: Expr, index: Expr) throws -> Expr {
    return try self.response(expr, index).claudeModelID.map { .makeString($0) } ?? .false
  }
}

#endif
