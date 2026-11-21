//
//  FoundationModelLibrary.swift
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
import DynamicJSON
import FoundationModels

///
/// Implements library `(lispkit foundation-model)`, an API for language models based on
/// Apple's FoundationModels framework. It supports Apple's on-device model, the model
/// provided via Private Cloud Compute, and models provided by other libraries, such as
/// `(lispkit foundation-model claude)` and `(lispkit foundation-model gemini)`. The library supports sessions, text and guided
/// generation, streaming, tool calling with Scheme procedures, and transcripts.
///
/// Requests are asynchronous: `model-session-response` returns a future and
/// `model-session-stream` returns a response stream. Tools are executed by a Scheme thread
/// that is started for every request that might call a tool.
///
@available(macOS 27.0, iOS 27.0, *)
public final class FoundationModelLibrary: NativeLibrary {

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "foundation-model"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"],    "define", "lambda", "quote", "apply", "not")
    self.`import`(from: ["lispkit", "control"], "do", "let", "if")
    self.`import`(from: ["lispkit", "dynamic"], "guard")
    self.`import`(from: ["lispkit", "list"],    "car", "cdr", "cadr", "cddr", "cons")
    self.`import`(from: ["lispkit", "port"],    "eof-object?")
    self.`import`(from: ["lispkit", "thread"],  "make-thread", "thread-start!")
  }

  /// Declarations of the library.
  public override func declarations() {
    // Families of language models: Apple's models are available if the device can use them;
    // the name of the on-device model is the model variant the device uses
    LanguageModelFamilies.register(family: "system", vendor: "Apple") {
      let backend = SystemModelBackend(.default)
      return try await backend.availability() == "available" ? [backend.name] : []
    }
    LanguageModelFamilies.register(family: "private-cloud-compute", vendor: "Apple") {
      let backend = PrivateCloudComputeModelBackend(PrivateCloudComputeLanguageModel())
      return try await backend.availability() == "available" ? [backend.name] : []
    }
    // Language models
    self.define(Procedure("language-model?", self.isLanguageModel))
    self.define(Procedure("system-language-model", self.systemLanguageModel))
    self.define(Procedure("private-cloud-compute-language-model",
                          self.privateCloudComputeLanguageModel))
    self.define(Procedure("language-model-family", self.languageModelFamily))
    self.define(Procedure("language-model-name", self.languageModelName))
    self.define(Procedure("available-language-model-families", self.availableLanguageModelFamilies))
    self.define(Procedure("language-model-family-vendor", self.languageModelFamilyVendor))
    self.define(Procedure("available-language-models", self.availableLanguageModels))
    self.define(Procedure("language-model-available?", self.isLanguageModelAvailable))
    self.define(Procedure("language-model-availability", self.languageModelAvailability))
    self.define(Procedure("language-model-capabilities", self.languageModelCapabilities))
    self.define(Procedure("language-model-context-size", self.languageModelContextSize))
    self.define(Procedure("language-model-supported-languages",
                          self.languageModelSupportedLanguages))
    self.define(Procedure("language-model-supports-locale?", self.languageModelSupportsLocale))
    self.define(Procedure("language-model-token-count", self.languageModelTokenCount))
    self.define(Procedure("language-model-quota-usage", self.languageModelQuotaUsage))
    self.define(Procedure("language-model-supports-data-type?", self.languageModelSupportsDataType))
    self.define(Procedure("limit-increase-suggestion?", self.isLimitIncreaseSuggestion))
    self.define(Procedure("limit-increase-suggestion-show!", self.limitIncreaseSuggestionShow))

    // Prompts and attachments
    self.define(Procedure("prompt?", self.isPrompt))
    self.define(Procedure("make-prompt", self.makePrompt))
    self.define(Procedure("attachment?", self.isAttachment))
    self.define(Procedure("image-attachment", self.imageAttachment))
    self.define(Procedure("data-attachment", self.dataAttachment))

    // Generation schemas
    self.define(Procedure("generation-schema?", self.isGenerationSchema))
    self.define(Procedure("make-generation-schema", self.makeGenerationSchema))
    self.define(Procedure("generation-schema-name", self.generationSchemaName))
    self.define(Procedure("generation-schema->json", self.generationSchemaToJson))
    self.define(Procedure("json->generation-schema", self.jsonToGenerationSchema))

    // Tools
    self.define(Procedure("tool?", self.isTool))
    self.define(Procedure("make-tool", self.makeTool))
    self.define(Procedure("tool-name", self.toolName))
    self.define(Procedure("tool-description", self.toolDescription))
    self.define(Procedure("tool-parameters", self.toolParameters))

    // Sessions
    self.define(Procedure("model-session?", self.isModelSession))
    self.define(Procedure("make-model-session", self.makeModelSession))
    self.define(Procedure("model-session-model", self.modelSessionModel))
    self.define(Procedure("model-session-tools", self.modelSessionTools))
    self.define(Procedure("model-session-prewarm", self.modelSessionPrewarm))
    self.define(Procedure("_model-session-response", self.modelSessionResponse))
    self.define(Procedure("_model-session-stream", self.modelSessionStream))
    self.define(Procedure("model-session-cancel!", self.modelSessionCancel))
    self.define(Procedure("model-session-transcript", self.modelSessionTranscript))
    self.define(Procedure("model-session-usage", self.modelSessionUsage))
    self.define(Procedure("model-session-responding?", self.isModelSessionResponding))
    self.define(Procedure("model-session-error-policy", self.modelSessionErrorPolicy))
    self.define(Procedure("model-session-error-policy-set!", self.modelSessionErrorPolicySet))
    self.define(Procedure("model-session-properties", self.modelSessionProperties))
    self.define(Procedure("model-session-property-set!", self.modelSessionPropertySet))

    // Responses
    self.define(Procedure("model-response?", self.isModelResponse))
    self.define(Procedure("model-response-content", self.modelResponseContent))
    self.define(Procedure("model-response-raw-content", self.modelResponseRawContent))
    self.define(Procedure("model-response-entries", self.modelResponseEntries))
    self.define(Procedure("model-response-usage", self.modelResponseUsage))
    self.define(Procedure("model-response-complete?", self.isModelResponseComplete))

    // Response streams
    self.define(Procedure("response-stream?", self.isResponseStream))
    self.define(Procedure("response-stream-next", self.responseStreamNext))
    self.define(Procedure("response-stream-result", self.responseStreamResult))
    self.define(Procedure("response-stream-done?", self.isResponseStreamDone))
    self.define(Procedure("response-stream-cancel!", self.responseStreamCancel))

    // Transcripts
    self.define(Procedure("transcript?", self.isTranscript))
    self.define(Procedure("make-transcript", self.makeTranscript))
    self.define(Procedure("transcript-length", self.transcriptLength))
    self.define(Procedure("transcript-ref", self.transcriptRef))
    self.define(Procedure("transcript->list", self.transcriptToList))
    self.define(Procedure("transcript-append", self.transcriptAppend))
    self.define(Procedure("transcript-sub", self.transcriptSub))
    self.define(Procedure("transcript->json", self.transcriptToJson))
    self.define(Procedure("json->transcript", self.jsonToTranscript))

    // Errors
    self.define(Procedure("model-error?", self.isModelError))
    self.define(Procedure("model-error-kind", self.modelErrorKind))
    self.define(Procedure("model-error-details", self.modelErrorDetails))
    self.define(Procedure("model-error-explanation", self.modelErrorExplanation))

    // Tool execution
    self.define(Procedure("_model-tool-next!", self.modelToolNext))
    self.define(Procedure("_model-tool-reply!", self.modelToolReply))
    self.define("_model-tool-loop", via: """
      (define (_model-tool-loop dispatcher)
        (do ((next (_model-tool-next! dispatcher) (_model-tool-next! dispatcher)))
            ((not next))
          (_model-tool-reply! (car next)
            (guard (e (#t (cons #f e)))
              (cons #t (apply (cadr next) (cddr next)))))))
    """)
    self.define("_model-tool-worker", via: """
      (define (_model-tool-worker res)
        (if (cdr res)
            (thread-start! (make-thread (lambda () (_model-tool-loop (cdr res))) "model tools")))
        (car res))
    """)
    self.define("model-session-response", via: """
      (define (model-session-response session prompt . args)
        (_model-tool-worker (apply _model-session-response session prompt args)))
    """)
    self.define("model-session-stream", via: """
      (define (model-session-stream session prompt . args)
        (_model-tool-worker (apply _model-session-stream session prompt args)))
    """)
    self.define("response-stream->generator", via: """
      (define (response-stream->generator stream)
        (lambda () (response-stream-next stream)))
    """)
    self.define("response-stream-for-each", via: """
      (define (response-stream-for-each proc stream)
        (do ((next (response-stream-next stream) (response-stream-next stream)))
            ((eof-object? next))
          (proc next)))
    """)
  }

  // MARK: - Helpers

  private func sym(_ str: String) -> Expr {
    return .symbol(self.context.symbols.intern(str))
  }

  private func error(_ kind: String, _ message: String) -> RuntimeError {
    return ModelErrors.error(kind, message, in: self.context)
  }

  private func unsupported(_ what: String, _ model: NativeLanguageModel) -> RuntimeError {
    return self.error("unsupported", "\(what) is not supported by \(model.label)")
  }

  private func object<T: NativeObject>(_ expr: Expr, _ type: T.Type, _ tpe: Type) throws -> T {
    guard case .object(let obj) = expr, let res = obj as? T else {
      throw RuntimeError.type(expr, expected: [tpe])
    }
    return res
  }

  private func model(from expr: Expr) throws -> NativeLanguageModel {
    return try self.object(expr, NativeLanguageModel.self, NativeLanguageModel.type)
  }

  private func session(from expr: Expr) throws -> NativeModelSession {
    return try self.object(expr, NativeModelSession.self, NativeModelSession.type)
  }

  private func tool(from expr: Expr) throws -> NativeTool {
    return try self.object(expr, NativeTool.self, NativeTool.type)
  }

  private func response(from expr: Expr) throws -> NativeModelResponse {
    return try self.object(expr, NativeModelResponse.self, NativeModelResponse.type)
  }

  private func stream(from expr: Expr) throws -> NativeResponseStream {
    return try self.object(expr, NativeResponseStream.self, NativeResponseStream.type)
  }

  private func transcript(from expr: Expr) throws -> Transcript {
    return try self.object(expr, NativeTranscript.self, NativeTranscript.type).transcript
  }

  private func schema(from expr: Expr) throws -> NativeGenerationSchema {
    return try self.object(expr, NativeGenerationSchema.self, NativeGenerationSchema.type)
  }

  private func isObject<T>(_ expr: Expr, _ type: T.Type) -> Expr {
    guard case .object(let obj) = expr, obj is T else {
      return .false
    }
    return .true
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

  private func string(fromJson expr: Expr) throws -> String {
    if case .object(let obj) = expr, let json = obj as? JSON {
      return String(decoding: try JSONEncoder().encode(json), as: UTF8.self)
    }
    return try expr.asString()
  }

  /// Runs `operation` asynchronously and waits for its result. Waiting can be aborted.
  private func wait<T>(_ operation: @escaping @Sendable () async throws -> T) throws -> T {
    let outcome = AsyncOutcome<T, Error>()
    let task = Task {
      do {
        outcome.store(.success(try await operation()))
      } catch {
        outcome.store(.failure(error))
      }
    }
    let result: Result<T, Error>?
    do {
      result = try outcome.waitForResult(in: self.context)
    } catch {
      task.cancel()
      throw error
    }
    switch result {
      case .some(.success(let value)):
        return value
      case .some(.failure(let error)):
        throw ModelErrors.translate(error, in: self.context)
      case .none:
        throw RuntimeError.abortion()
    }
  }

  // MARK: - Language models

  private func isLanguageModel(expr: Expr) -> Expr {
    return self.isObject(expr, NativeLanguageModel.self)
  }

  private func systemLanguageModel(useCase: Expr?, guardrails: Expr?) throws -> Expr {
    var uc = SystemLanguageModel.UseCase.general
    var gr = SystemLanguageModel.Guardrails.default
    var label = "system"
    if let useCase, useCase.isTrue {
      switch try useCase.asSymbol().identifier {
        case "general":
          break
        case "content-tagging":
          uc = .contentTagging
          label += " content-tagging"
        default:
          throw SchemaCompiler.invalid("unknown use case", useCase)
      }
    }
    if let guardrails, guardrails.isTrue {
      switch try guardrails.asSymbol().identifier {
        case "default":
          break
        case "permissive-content-transformations":
          gr = .permissiveContentTransformations
          label += " permissive"
        default:
          throw SchemaCompiler.invalid("unknown guardrails", guardrails)
      }
    }
    let model = (useCase == nil && guardrails == nil) ? SystemLanguageModel.default
                                                      : SystemLanguageModel(useCase: uc,
                                                                            guardrails: gr)
    return .object(NativeLanguageModel(SystemModelBackend(model), label: label))
  }

  private func privateCloudComputeLanguageModel() -> Expr {
    return .object(NativeLanguageModel(
      PrivateCloudComputeModelBackend(PrivateCloudComputeLanguageModel()),
      label: "private-cloud-compute"))
  }

  private func languageModelFamily(expr: Expr) throws -> Expr {
    return self.sym(try self.model(from: expr).family)
  }

  private func languageModelName(expr: Expr) throws -> Expr {
    return .makeString(try self.model(from: expr).backend.name)
  }

  private func availableLanguageModelFamilies() -> Expr {
    return .makeList(Exprs(LanguageModelFamilies.families.map { self.sym($0) }))
  }

  private func languageModelFamilyVendor(family: Expr) throws -> Expr {
    guard let vendor = LanguageModelFamilies.vendor(for: try family.asSymbol().identifier) else {
      return .false
    }
    return .makeString(vendor)
  }

  private func availableLanguageModels(family: Expr) throws -> Expr {
    guard let provider = LanguageModelFamilies.provider(for: try family.asSymbol().identifier) else {
      return .false
    }
    return .makeList(Exprs(try self.wait(provider).map { .makeString($0) }))
  }

  private func availability(_ model: NativeLanguageModel) throws -> String {
    let backend = model.backend
    return try self.wait { try await backend.availability() }
  }

  private func isLanguageModelAvailable(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.availability(try self.model(from: expr)) == "available")
  }

  private func languageModelAvailability(expr: Expr) throws -> Expr {
    return self.sym(try self.availability(try self.model(from: expr)))
  }

  private func languageModelCapabilities(expr: Expr) throws -> Expr {
    let capabilities = try self.model(from: expr).model.capabilities
    var res = Exprs()
    for (capability, name) in [(LanguageModelCapabilities.Capability.vision, "vision"),
                               (.guidedGeneration, "guided-generation"),
                               (.reasoning, "reasoning"),
                               (.toolCalling, "tool-calling")] {
      if capabilities.contains(capability) {
        res.append(self.sym(name))
      }
    }
    return .makeList(res)
  }

  private func languageModelContextSize(expr: Expr) throws -> Expr {
    let backend = try self.model(from: expr).backend
    guard let size = try self.wait({ try await backend.contextSize() }) else {
      return .false
    }
    return .makeNumber(size)
  }

  private func languageModelSupportedLanguages(expr: Expr) throws -> Expr {
    let backend = try self.model(from: expr).backend
    guard let languages = try self.wait({ try await backend.supportedLanguages() }) else {
      return .false
    }
    return .makeList(Exprs(languages.map { $0.minimalIdentifier }.sorted().map {
      .makeString($0)
    }))
  }

  private func languageModelSupportsLocale(expr: Expr, locale: Expr?) throws -> Expr {
    let model = try self.model(from: expr)
    let backend = model.backend
    let loc = (locale == nil || locale!.isFalse) ? Locale.current
                                                 : Locale(identifier: try locale!.asString())
    guard let res = try self.wait({ try await backend.supportsLocale(loc) }) else {
      throw self.unsupported("locale support", model)
    }
    return .makeBoolean(res)
  }

  private func languageModelTokenCount(expr: Expr, value: Expr, kind: Expr?) throws -> Expr {
    let backend = try self.model(from: expr).backend
    let input: LanguageModelTokenInput
    var k: String
    if let kind, kind.isTrue {
      k = try kind.asSymbol().identifier
    } else if case .object(let obj) = value, obj is NativeTranscript {
      k = "transcript"
    } else if case .object(let obj) = value, obj is NativeGenerationSchema {
      k = "schema"
    } else if case .pair(.object(let obj), _) = value, obj is NativeTool {
      k = "tools"
    } else {
      k = "prompt"
    }
    switch k {
      case "prompt":
        input = .prompt(try PromptConverter.prompt(from: value))
      case "instructions":
        input = .instructions(try PromptConverter.instructions(from: value))
      case "tools":
        let hub = ToolHub()
        input = .tools(try self.list(value).map {
          SchemeTool(tool: try self.tool(from: $0), hub: hub)
        })
      case "schema":
        input = .schema(try SchemaCompiler.schema(from: value, in: self.context).schema)
      case "transcript":
        input = .transcript(try self.transcript(from: value))
      default:
        throw SchemaCompiler.invalid("unknown token count kind", kind ?? .false)
    }
    guard let count = try self.wait({ try await backend.tokenCount(for: input) }) else {
      return .false
    }
    return .makeNumber(count)
  }

  private func languageModelQuotaUsage(expr: Expr) throws -> Expr {
    let backend = try self.model(from: expr).backend
    guard let usage = try self.wait({ try await backend.quotaUsage() }) else {
      return .false
    }
    var reset: Expr = .false
    if let date = usage.resetDate {
      reset = .object(NativeDateTime(DateTimeLibrary.calendar.dateComponents(in: .current,
                                                                            from: date)))
    }
    var suggestion: Expr = .false
    if let s = usage.limitIncreaseSuggestion {
      suggestion = .object(NativeLimitIncreaseSuggestion(s))
    }
    return .makeList(.pair(self.sym("status"), self.sym(usage.status)),
                     .pair(self.sym("reset-date"), reset),
                     .pair(self.sym("limit-increase-suggestion"), suggestion))
  }

  private func languageModelSupportsDataType(expr: Expr, type: Expr, kind: Expr?) throws -> Expr {
    guard #available(macOS 27.2, iOS 27.2, *) else {
      throw self.error("unsupported", "data types require macOS/iOS 27.2")
    }
    let model = try self.model(from: expr).model
    let type = try PromptConverter.contentType(from: type)
    let attachment: Bool
    if let kind, kind.isTrue {
      switch try kind.asSymbol().identifier {
        case "attachment":
          attachment = true
        case "entry":
          attachment = false
        default:
          throw SchemaCompiler.invalid("unknown data type kind", kind)
      }
    } else {
      attachment = true
    }
    return .makeBoolean(try self.wait {
      attachment ? try await model.supportsDataAttachmentType(type)
                 : try await model.supportsDataEntryType(type)
    })
  }

  // MARK: - Prompts and attachments

  private func isPrompt(expr: Expr) -> Expr {
    return self.isObject(expr, NativePrompt.self)
  }

  private func makePrompt(args: Arguments) throws -> Expr {
    return .object(NativePrompt(try PromptConverter.prompt(from: .makeList(args))))
  }

  private func isAttachment(expr: Expr) -> Expr {
    return self.isObject(expr, NativeAttachment.self)
  }

  private func imageAttachment(image: Expr, label: Expr?) throws -> Expr {
    let lbl = (label == nil || label!.isFalse) ? nil : try label!.asString()
    if let cgImage = PromptConverter.cgImage(from: image) {
      return .object(NativeAttachment(image: cgImage, label: lbl))
    }
    switch image {
      case .string(_):
        let path = try image.asAbsolutePath(in: self.context)
        return .object(NativeAttachment(imageURL: URL(fileURLWithPath: path), label: lbl))
      case .object(let obj):
        if let attachment = obj as? NativeAttachment, attachment.kind == "image" {
          return image
        }
      default:
        break
    }
    throw RuntimeError.type(image, expected: [NativeImage.type, .strType])
  }

  private func dataAttachment(bytes: Expr, contentType: Expr, label: Expr?) throws -> Expr {
    guard #available(macOS 27.2, iOS 27.2, *) else {
      throw self.error("unsupported", "data attachments require macOS/iOS 27.2")
    }
    let lbl = (label == nil || label!.isFalse) ? nil : try label!.asString()
    return .object(NativeAttachment(data: Data(try bytes.asByteVector().value),
                                    contentType: try PromptConverter.contentType(from: contentType),
                                    label: lbl))
  }

  // MARK: - Generation schemas

  private func isGenerationSchema(expr: Expr) -> Expr {
    return self.isObject(expr, NativeGenerationSchema.self)
  }

  private func makeGenerationSchema(root: Expr, deps: Arguments) throws -> Expr {
    return .object(try SchemaCompiler.schema(from: root, dependencies: deps, in: self.context))
  }

  private func generationSchemaName(expr: Expr) throws -> Expr {
    return .makeString(try SchemaCompiler.schema(from: expr, in: self.context).schema.name)
  }

  private func generationSchemaToJson(expr: Expr) throws -> Expr {
    let schema = try SchemaCompiler.schema(from: expr, in: self.context).schema
    let encoder = JSONEncoder()
    encoder.outputFormatting = [.sortedKeys]
    do {
      return .makeString(String(decoding: try encoder.encode(schema), as: UTF8.self))
    } catch {
      throw ModelErrors.translate(error, in: self.context)
    }
  }

  private func jsonToGenerationSchema(expr: Expr) throws -> Expr {
    let str = try self.string(fromJson: expr)
    do {
      let schema = try JSONDecoder().decode(GenerationSchema.self, from: Data(str.utf8))
      return .object(NativeGenerationSchema(schema, shapes: nil))
    } catch {
      throw self.error("schema-error", "invalid generation schema: \(error.localizedDescription)")
    }
  }

  // MARK: - Tools

  private func isTool(expr: Expr) -> Expr {
    return self.isObject(expr, NativeTool.self)
  }

  private func makeTool(args: Arguments) throws -> Expr {
    guard args.count >= 4 && args.count <= 5 else {
      throw RuntimeError.argumentCount(of: "make-tool", min: 4, max: 5, args: .makeList(args))
    }
    let a = Array(args)
    let name = try SchemaCompiler.name(a[0])
    guard !name.isEmpty else {
      throw SchemaCompiler.invalid("tool name must not be empty", a[0])
    }
    return .object(NativeTool(name: name,
                              description: try a[1].asString(),
                              parameters: try SchemaCompiler.schema(from: a[2], in: self.context),
                              procedure: try a[3].asProcedure(),
                              includesSchema: a.count < 5 || a[4].isTrue))
  }

  private func toolName(expr: Expr) throws -> Expr {
    return .makeString(try self.tool(from: expr).name)
  }

  private func toolDescription(expr: Expr) throws -> Expr {
    return .makeString(try self.tool(from: expr).descr)
  }

  private func toolParameters(expr: Expr) throws -> Expr {
    return .object(try self.tool(from: expr).parameters)
  }

  // MARK: - Sessions

  private func isModelSession(expr: Expr) -> Expr {
    return self.isObject(expr, NativeModelSession.self)
  }

  private func makeModelSession(args: Arguments) throws -> Expr {
    guard args.count <= 3 else {
      throw RuntimeError.argumentCount(of: "make-model-session", min: 0, max: 3,
                                       args: .makeList(args))
    }
    let a = Array(args)
    let model: NativeLanguageModel
    if a.count > 0 && a[0].isTrue {
      model = try self.model(from: a[0])
    } else {
      model = NativeLanguageModel(SystemModelBackend(SystemLanguageModel.default),
                                  label: "system")
    }
    var tools: [NativeTool] = []
    if a.count > 1 && a[1].isTrue {
      tools = try self.list(a[1]).map(self.tool(from:))
    }
    let hub = ToolHub()
    let swiftTools: [any Tool] = tools.map { SchemeTool(tool: $0, hub: hub) }
    let session: LanguageModelSession
    if a.count > 2, case .object(let obj) = a[2], let transcript = obj as? NativeTranscript {
      session = LanguageModelSession(model: model.model,
                                     tools: swiftTools,
                                     transcript: transcript.transcript)
    } else if a.count > 2 && a[2].isTrue {
      session = LanguageModelSession(model: model.model,
                                     tools: swiftTools,
                                     instructions: try PromptConverter.instructions(from: a[2]))
    } else {
      session = LanguageModelSession(model: model.model,
                                     tools: swiftTools,
                                     instructions: nil as FMInstructions?)
    }
    return .object(NativeModelSession(session: session, model: model, tools: tools, hub: hub))
  }

  private func modelSessionModel(expr: Expr) throws -> Expr {
    return .object(try self.session(from: expr).model)
  }

  private func modelSessionTools(expr: Expr) throws -> Expr {
    return .makeList(Exprs(try self.session(from: expr).tools.map { .object($0) }))
  }

  private func modelSessionPrewarm(expr: Expr, prefix: Expr?) throws -> Expr {
    let session = try self.session(from: expr)
    if let prefix, prefix.isTrue {
      session.session.prewarm(promptPrefix: try PromptConverter.prompt(from: prefix))
    } else {
      session.session.prewarm(promptPrefix: nil)
    }
    return .void
  }

  /// The components of a request.
  private struct RequestComponents: @unchecked Sendable {
    let session: NativeModelSession
    let prompt: Prompt
    let schema: NativeGenerationSchema?
    let options: RequestOptions

    var metadata: [String : any ConvertibleToGeneratedContent] {
      return self.options.metadata.mapValues { $0 as any ConvertibleToGeneratedContent }
    }
  }

  private func requestComponents(_ expr: Expr,
                                 _ prompt: Expr,
                                 _ schema: Expr?,
                                 _ options: Expr?) throws -> RequestComponents {
    let session = try self.session(from: expr)
    var schemaObj: NativeGenerationSchema? = nil
    if let schema, schema.isTrue {
      schemaObj = try SchemaCompiler.schema(from: schema, in: self.context)
    }
    let components = RequestComponents(session: session,
                                       prompt: try PromptConverter.prompt(from: prompt),
                                       schema: schemaObj,
                                       options: try RequestOptions(from: options,
                                                                   schema: schemaObj != nil,
                                                                   in: self.context))
    if session.busy {
      throw self.error("concurrent-requests",
                       "the session is already responding to another request")
    }
    return components
  }

  private func requestResult(_ request: ModelRequest, _ result: Expr) -> Expr {
    if let dispatcher = request.dispatcher {
      return .pair(result, .object(dispatcher))
    }
    return .pair(result, .false)
  }

  private func modelSessionResponse(expr: Expr,
                                    prompt: Expr,
                                    schema: Expr?,
                                    options: Expr?) throws -> Expr {
    let req = try self.requestComponents(expr, prompt, schema, options)
    let request = ModelRequest(session: req.session, context: self.context)
    let future = Future(external: false)
    let context = self.context
    request.start {
      do {
        let response: NativeModelResponse
        if let schema = req.schema {
          response = NativeModelResponse(
            try await req.session.session.respond(to: req.prompt,
                                                  schema: schema.schema,
                                                  options: req.options.generation,
                                                  contextOptions: req.options.context,
                                                  metadata: req.metadata),
            shapes: schema.shapes)
        } else {
          response = NativeModelResponse(
            try await req.session.session.respond(to: req.prompt,
                                                  options: req.options.generation,
                                                  contextOptions: req.options.context,
                                                  metadata: req.metadata))
        }
        request.finish()
        _ = try? future.setResult(in: context, to: .object(response))
      } catch {
        request.finish()
        _ = try? future.setResult(in: context,
                                  to: ModelErrors.raisable(error, in: context),
                                  raise: true)
      }
    }
    return self.requestResult(request, .object(future))
  }

  private func modelSessionStream(expr: Expr,
                                  prompt: Expr,
                                  schema: Expr?,
                                  options: Expr?) throws -> Expr {
    let req = try self.requestComponents(expr, prompt, schema, options)
    let request = ModelRequest(session: req.session, context: self.context)
    let stream = NativeResponseStream()
    stream.request = request
    let context = self.context
    request.start {
      do {
        if let schema = req.schema {
          let responses = req.session.session.streamResponse(to: req.prompt,
                                                             schema: schema.schema,
                                                             options: req.options.generation,
                                                             contextOptions: req.options.context,
                                                             metadata: req.metadata)
          for try await snapshot in responses {
            stream.update(NativeModelResponse(snapshot, shapes: schema.shapes))
          }
        } else {
          let responses = req.session.session.streamResponse(to: req.prompt,
                                                             options: req.options.generation,
                                                             contextOptions: req.options.context,
                                                             metadata: req.metadata)
          for try await snapshot in responses {
            stream.update(NativeModelResponse(snapshot))
          }
        }
        // A cancelled stream may end without an error
        try Task.checkCancellation()
        request.finish()
        stream.finish(with: nil, in: context)
      } catch {
        request.finish()
        stream.finish(with: error, in: context)
      }
    }
    return self.requestResult(request, .object(stream))
  }

  private func modelSessionCancel(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.session(from: expr).cancel())
  }

  private func modelSessionTranscript(expr: Expr) throws -> Expr {
    return .object(NativeTranscript(try self.session(from: expr).session.transcript))
  }

  private func modelSessionUsage(expr: Expr) throws -> Expr {
    return UsageConverter.expr(from: try self.session(from: expr).session.usage, in: self.context)
  }

  private func isModelSessionResponding(expr: Expr) throws -> Expr {
    let session = try self.session(from: expr)
    return .makeBoolean(session.busy || session.session.isResponding)
  }

  private func modelSessionErrorPolicy(expr: Expr) throws -> Expr {
    let session = try self.session(from: expr)
    if let policy = session.errorPolicy {
      return self.sym(policy)
    }
    return .false
  }

  private func modelSessionErrorPolicySet(expr: Expr, policy: Expr) throws -> Expr {
    let session = try self.session(from: expr)
    if policy.isFalse {
      session.session.transcriptErrorHandlingPolicy = nil
      session.errorPolicy = nil
      return .void
    }
    switch try policy.asSymbol().identifier {
      case "revert-transcript":
        session.session.transcriptErrorHandlingPolicy = .revertTranscript
        session.errorPolicy = "revert-transcript"
      case "preserve-transcript":
        session.session.transcriptErrorHandlingPolicy = .preserveTranscript
        session.errorPolicy = "preserve-transcript"
      default:
        throw SchemaCompiler.invalid("unknown transcript error handling policy", policy)
    }
    return .void
  }

  private func modelSessionProperties(expr: Expr) throws -> Expr {
    let session = try self.session(from: expr)
    let history = Transcript(entries: session.session.properties.history)
    return .makeList(.pair(self.sym("history"), .object(NativeTranscript(history))))
  }

  private func modelSessionPropertySet(expr: Expr, key: Expr, value: Expr) throws -> Expr {
    let session = try self.session(from: expr)
    switch try key.asSymbol().identifier {
      case "history":
        var entries: [Transcript.Entry] = []
        try TranscriptConverter(context: self.context).collect(value, into: &entries)
        var history = Transcript.HistoryView()
        history.append(contentsOf: entries)
        session.session.properties.history = history
      default:
        throw SchemaCompiler.invalid("unknown session property", key)
    }
    return .void
  }

  // MARK: - Responses

  private func isModelResponse(expr: Expr) -> Expr {
    return self.isObject(expr, NativeModelResponse.self)
  }

  private func modelResponseContent(expr: Expr) throws -> Expr {
    return try self.response(from: expr).contentExpr(in: self.context)
  }

  private func modelResponseRawContent(expr: Expr) throws -> Expr {
    let response = try self.response(from: expr)
    return ContentConverter.expr(from: response.rawContent,
                                 shape: response.shapes?.root,
                                 shapes: response.shapes,
                                 in: self.context)
  }

  private func modelResponseEntries(expr: Expr) throws -> Expr {
    return .object(NativeTranscript(Transcript(entries: try self.response(from: expr).entries)))
  }

  private func modelResponseUsage(expr: Expr) throws -> Expr {
    return UsageConverter.expr(from: try self.response(from: expr).usage, in: self.context)
  }

  private func isModelResponseComplete(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.response(from: expr).isComplete)
  }

  // MARK: - Response streams

  private func isResponseStream(expr: Expr) -> Expr {
    return self.isObject(expr, NativeResponseStream.self)
  }

  private func responseStreamNext(expr: Expr, timeout: Expr?, def: Expr?) throws -> Expr {
    let stream = try self.stream(from: expr)
    var limit: TimeInterval? = nil
    if let timeout, timeout.isTrue {
      limit = try timeout.asDouble(coerce: true)
    }
    switch try stream.next(in: self.context, timeout: limit) {
      case .snapshot(let snapshot):
        return .object(snapshot)
      case .failure(let error):
        throw ModelErrors.translate(error, in: self.context)
      case .end:
        return .eof
      case .timeout:
        return def ?? .false
    }
  }

  private func responseStreamResult(expr: Expr) throws -> Expr {
    return .object(try self.stream(from: expr).result)
  }

  private func isResponseStreamDone(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.stream(from: expr).isDone)
  }

  private func responseStreamCancel(expr: Expr) throws -> Expr {
    try self.stream(from: expr).request?.cancel()
    return .void
  }

  // MARK: - Transcripts

  private func isTranscript(expr: Expr) -> Expr {
    return self.isObject(expr, NativeTranscript.self)
  }

  private func makeTranscript(args: Arguments) throws -> Expr {
    var entries: [Transcript.Entry] = []
    let converter = TranscriptConverter(context: self.context)
    for arg in args {
      try converter.collect(arg, into: &entries)
    }
    return .object(NativeTranscript(Transcript(entries: entries)))
  }

  private func transcriptLength(expr: Expr) throws -> Expr {
    return .makeNumber(try self.transcript(from: expr).count)
  }

  private func transcriptRef(expr: Expr, index: Expr) throws -> Expr {
    let transcript = try self.transcript(from: expr)
    let i = try index.asInt(below: transcript.count)
    return TranscriptConverter(context: self.context).expr(from: transcript[i])
  }

  private func transcriptToList(expr: Expr) throws -> Expr {
    let converter = TranscriptConverter(context: self.context)
    return .makeList(Exprs(try self.transcript(from: expr).map(converter.expr(from:))))
  }

  private func transcriptAppend(expr: Expr, args: Arguments) throws -> Expr {
    var entries = Array(try self.transcript(from: expr))
    let converter = TranscriptConverter(context: self.context)
    for arg in args {
      try converter.collect(arg, into: &entries)
    }
    return .object(NativeTranscript(Transcript(entries: entries)))
  }

  private func transcriptSub(expr: Expr, start: Expr, end: Expr?) throws -> Expr {
    let entries = Array(try self.transcript(from: expr))
    let e = (end == nil || end!.isFalse) ? entries.count
                                         : try end!.asInt(below: entries.count + 1)
    let s = try start.asInt(below: e + 1)
    return .object(NativeTranscript(Transcript(entries: entries[s..<e])))
  }

  private func transcriptToJson(expr: Expr) throws -> Expr {
    let transcript = try self.transcript(from: expr)
    do {
      return .makeString(String(decoding: try JSONEncoder().encode(transcript), as: UTF8.self))
    } catch {
      throw ModelErrors.translate(error, in: self.context)
    }
  }

  private func jsonToTranscript(expr: Expr) throws -> Expr {
    let str = try self.string(fromJson: expr)
    do {
      return .object(NativeTranscript(try JSONDecoder().decode(Transcript.self,
                                                               from: Data(str.utf8))))
    } catch {
      throw self.error("decoding-failure", "invalid transcript: \(error.localizedDescription)")
    }
  }

  // MARK: - Errors

  private func isModelError(expr: Expr) -> Expr {
    guard case .error(let err) = expr else {
      return .false
    }
    return .makeBoolean(ModelErrors.kind(of: err) != nil)
  }

  private func modelErrorKind(expr: Expr) -> Expr {
    guard case .error(let err) = expr else {
      return .false
    }
    return ModelErrors.kind(of: err) ?? .false
  }

  private func modelErrorDetails(expr: Expr) -> Expr {
    guard case .error(let err) = expr else {
      return .false
    }
    return ModelErrors.details(of: err) ?? .false
  }

  private func modelErrorExplanation(expr: Expr) throws -> Expr {
    guard case .error(let err) = expr, let details = ModelErrors.details(of: err) else {
      return .false
    }
    // Look for the refusal in the details of the error
    var lst = details
    var refusal: NativeRefusal? = nil
    while case .pair(.pair(_, .object(let obj)), let rest) = lst {
      if let r = obj as? NativeRefusal {
        refusal = r
        break
      }
      lst = rest
    }
    guard let refusal else {
      return .false
    }
    let future = Future(external: false)
    let context = self.context
    Task {
      do {
        let text = try await refusal.explanation()
        _ = try? future.setResult(in: context, to: .makeString(text))
      } catch {
        _ = try? future.setResult(in: context,
                                  to: ModelErrors.raisable(error, in: context),
                                  raise: true)
      }
    }
    return .object(future)
  }

  private func isLimitIncreaseSuggestion(expr: Expr) -> Expr {
    return self.isObject(expr, NativeLimitIncreaseSuggestion.self)
  }

  private func limitIncreaseSuggestionShow(expr: Expr) throws -> Expr {
    let suggestion = try self.object(expr,
                                     NativeLimitIncreaseSuggestion.self,
                                     NativeLimitIncreaseSuggestion.type)
    suggestion.show()
    return .void
  }

  // MARK: - Tool execution

  private func modelToolNext(expr: Expr) throws -> Expr {
    let dispatcher = try self.object(expr, ToolDispatcher.self, ToolDispatcher.type)
    guard let invocation = try dispatcher.next(in: self.context) else {
      return .false
    }
    let tool = invocation.tool
    let shapes = tool.parameters.shapes
    var args = Exprs()
    if let props = shapes?.properties,
       case .structure(properties: let values, orderedKeys: _) = invocation.arguments.kind {
      for prop in props {
        // Missing optional properties, and optional properties that are explicitly `null`
        // (see `explicit-nil` for object schemas), are passed as `#f`.
        if let value = values[prop.name], !(prop.optional && value.kind == .null) {
          args.append(ContentConverter.expr(from: value, shape: prop.shape, shapes: shapes,
                                            in: self.context))
        } else {
          args.append(.false)
        }
      }
    } else {
      args.append(ContentConverter.expr(from: invocation.arguments, shape: shapes?.root,
                                        shapes: shapes, in: self.context))
    }
    return .pair(.object(invocation), .pair(.procedure(tool.procedure), .makeList(args)))
  }

  private func modelToolReply(expr: Expr, result: Expr) throws -> Expr {
    let invocation = try self.object(expr, ToolInvocation.self, ToolInvocation.type)
    guard case .pair(let ok, let value) = result else {
      invocation.resume(with: .failure(SchemeRaisedError(raised: result)))
      return .void
    }
    guard ok.isTrue else {
      invocation.resume(with: .failure(SchemeRaisedError(raised: value)))
      return .void
    }
    do {
      if case .string(let str) = value {
        invocation.resume(with: .success(Prompt(str as String)))
      } else if case .object(let obj) = value, let prompt = obj as? NativePrompt {
        invocation.resume(with: .success(prompt.prompt))
      } else {
        invocation.resume(with: .success(Prompt(try ContentConverter.content(from: value,
                                                                             in: self.context))))
      }
    } catch {
      invocation.resume(with: .failure(SchemeRaisedError(raised: .error(
        (error as? RuntimeError) ?? RuntimeError.os(error)))))
    }
    return .void
  }
}

#endif
