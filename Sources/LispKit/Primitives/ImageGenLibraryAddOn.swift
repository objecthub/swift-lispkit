//
//  ImageGenLibraryAddOn.swift
//  LispKit
//
//  Created by Matthias Zenger on 19/10/2026.
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
import MarkdownKit

///
/// `ImageGenLibraryAddOn` adds procedures `mermaid->image` and `latex-math->image` to library
/// `(lispkit draw)` for both macOS and iOS: the platform-specific libraries only register the
/// procedures. Diagrams and formulas are drawn by `MarkdownUtil`, using the renderers of the
/// configuration of library `(lispkit markdown)`. The renderers are looked up once, when the
/// add-on gets created, i.e. when one of its procedures is called for the first time.
///
final class ImageGenLibraryAddOn {
  private weak let context: Context?
  private let util: MarkdownUtil
  
  init(context: Context) {
    self.context = context
    // Look up resource for the configuration of the Markdown library
    if let resource = context.configs.resource(for: MarkdownLibrary.Config.self) {
      self.util = MarkdownUtil(mermaidRenderer: resource.mermaidRenderer,
                               mathRenderer: resource.mathRenderer)
    } else {
      self.util = MarkdownUtil(mermaidRenderer: MermaidRenderer.shared,
                               mathRenderer: MathRenderer.shared)
    }
  }
  
  // MARK: Mermaid diagrams
  
  /// Keys of the association lists configuring Mermaid diagrams
  private static let mermaidKeys: Set<String> = [
    "theme", "theme-variables", "font-family", "scale", "width", "margin", "max-width",
    "spacing", "timeout"
  ]

  /// Draws the Mermaid diagram `source`, an optional configuration `config` (an association
  /// list), and returns the diagram as an image object.
  func mermaidImage(source: Expr, config: Expr?) throws -> Expr {
    let source = try source.asString()
    let config = try self.mermaidConfig(config)
    let outcome = Outcome<HRImage, MermaidError>()
    self.util.mermaidImage(source, config: config) { result in
      outcome.store(result)
    }
    // MarkdownKit reports a timeout by itself and does so on the main thread. If that thread is
    // blocked, e.g. because it waits for this thread, the result never arrives; the deadline
    // makes sure that this thread does not wait forever.
    guard let result = try self.wait(for: outcome, upTo: config.timeout + 10.0) else {
      throw RuntimeError.eval(.cannotRenderMermaidDiagram,
                              .makeString(MermaidError.timedOut.localizedDescription))
    }
    switch result {
      case .success(let image):
        return .object(NativeImage(image))
      case .failure(let error):
        throw RuntimeError.eval(.cannotRenderMermaidDiagram,
                                .makeString(error.localizedDescription))
    }
  }

  private func mermaidConfig(_ expr: Expr?) throws -> MermaidConfig {
    let entries = try ConfigEntries(expr, allowed: ImageGenLibraryAddOn.mermaidKeys,
                                    error: .notAValidMermaidConfig)
    var config = MermaidConfig.default
    if let name = try entries.symbol("theme", in: ["default", "neutral", "dark", "forest", "base"]),
       let theme = MermaidConfig.Theme(rawValue: name) {
      config.theme = theme
    }
    if let variables = try entries.strings("theme-variables") {
      config.themeVariables = variables
    }
    if let fontFamily = try entries.string("font-family") {
      config.fontFamily = fontFamily
    }
    if let scale = try entries.length("scale") {
      config.scale = scale
    }
    if let width = try entries.length("width") {
      config.width = width
    }
    if let (_, margin) = entries["margin"] {
      // `#f` keeps the margins that Mermaid adds around its diagrams
      config.margin = margin.isFalse ? nil : try entries.length("margin", zero: true)
    }
    if let maxWidth = try entries.length("max-width") {
      config.maxWidth = maxWidth
    }
    if let spacing = try entries.length("spacing", zero: true) {
      config.spacing = spacing
    }
    if let timeout = try entries.length("timeout") {
      config.timeout = TimeInterval(timeout)
    }
    return config
  }

  // MARK: LaTeX formulas

  /// Keys of the association lists configuring formulas
  private static let mathKeys: Set<String> = [
    "scale", "spacing", "max-width", "timeout", "macros", "max-expand", "max-size",
    "min-rule-thickness", "max-length"
  ]

  /// Draws the formula `tex`. The arguments `args` are optional: `display?`, `font-size`, `color`
  /// and a configuration `config` (an association list). `#f` stands for the default.
  func mathImage(tex: Expr, args: Arguments) throws -> Expr {
    guard let (displayArg, sizeArg, colorArg, configArg) =
            args.optional(Expr.false, Expr.false, Expr.false, Expr.false) else {
      throw RuntimeError.argumentCount(of: "latex-math->image",
                                       min: 1,
                                       max: 5,
                                       args: .pair(tex, .makeList(args)))
    }
    let tex = try tex.asString()
    let display = displayArg.isTrue
    var fontSize: Float = 14.0
    if sizeArg.isTrue {
      let size = try sizeArg.asDouble(coerce: true)
      guard size >= 1.0 && size <= 1000.0 else {
        throw RuntimeError.range(parameter: 3, of: "latex-math->image", sizeArg, min: 1, max: 1000)
      }
      fontSize = Float(size)
    }
    var color: HRColor? = nil
    if colorArg.isTrue {
      guard case .object(let obj) = colorArg, let col = obj as? Color else {
        throw RuntimeError.type(colorArg, expected: [Color.type])
      }
      color = col.nsColor
    }
    let config = try ImageGenLibraryAddOn.mathConfig(configArg, error: .notAValidMathConfig)
    let outcome = Outcome<HRImage, MathError>()
    self.util.mathImage(tex,
                        display: display,
                        fontSize: fontSize,
                        color: color,
                        config: config) { result in
      outcome.store(result)
    }
    // A timeout of at most 0 means that there is no limit
    guard let result = try self.wait(for: outcome,
                                     upTo: config.timeout > 0.0 ? config.timeout + 10.0 : nil) else {
      throw RuntimeError.eval(.cannotRenderLatexMath,
                              .makeString(MathError.timedOut.localizedDescription))
    }
    switch result {
      case .success(let image):
        return .object(NativeImage(image))
      case .failure(let error):
        throw RuntimeError.eval(.cannotRenderLatexMath, .makeString(error.localizedDescription))
    }
  }

  /// Maps a configuration of formulas, i.e. an association list with the keys in `mathKeys`, to
  /// a `MathConfig`. `error` is the error that is signaled for an invalid configuration. This
  /// function is also used by library `(lispkit markdown)` for key `math` of its configurations.
  static func mathConfig(_ expr: Expr?, error: EvalError) throws -> MathConfig {
    let entries = try ConfigEntries(expr, allowed: ImageGenLibraryAddOn.mathKeys, error: error)
    var config = MathConfig()
    if let scale = try entries.length("scale") {
      config.scale = scale
    }
    if let spacing = try entries.length("spacing", zero: true) {
      config.spacing = spacing
    }
    if let maxWidth = try entries.length("max-width") {
      config.maxWidth = maxWidth
    }
    if let timeout = try entries.length("timeout", zero: true) {
      config.timeout = TimeInterval(timeout)
    }
    if let macros = try entries.strings("macros") {
      config.katex.macros = macros
    }
    if let maxExpand = try entries.int("max-expand") {
      config.katex.maxExpand = maxExpand
    }
    if let maxSize = try entries.double("max-size") {
      config.katex.maxSize = maxSize
    }
    if let minRuleThickness = try entries.double("min-rule-thickness") {
      config.katex.minRuleThickness = minRuleThickness
    }
    if let maxLength = try entries.int("max-length") {
      config.katex.maxLength = maxLength
    }
    return config
  }

  // MARK: Waiting for results

  /// The outcome of an image that gets rendered asynchronously.
  private final class Outcome<Value, Failure: Error>: @unchecked Sendable {
    private let lock = NSLock()
    private let semaphore = DispatchSemaphore(value: 0)
    private var result: Result<Value, Failure>?

    func store(_ result: Result<Value, Failure>) {
      self.lock.lock()
      self.result = result
      self.lock.unlock()
      self.semaphore.signal()
    }

    var current: Result<Value, Failure>? {
      self.lock.lock()
      defer { self.lock.unlock() }
      return self.result
    }

    func wait(upTo seconds: TimeInterval) {
      _ = self.semaphore.wait(timeout: .now() + seconds)
    }
  }

  /// Waits for the outcome of rendering an image, until it is available (the result of the
  /// function), the evaluation gets aborted (an error is thrown), or the time `limit` in seconds
  /// is up (`nil`). There is no time limit if `limit` is `nil`. The run loop is kept running
  /// if this is the main thread, as WebKit needs it for drawing.
  private func wait<Value, Failure: Error>(for outcome: Outcome<Value, Failure>,
                                            upTo limit: TimeInterval?) throws
                                            -> Result<Value, Failure>? {
    let deadline = limit.map { Date(timeIntervalSinceNow: $0) }
    while outcome.current == nil {
      if self.context?.evaluator.threads.current?.value.machine?.abortionRequested ?? false {
        throw RuntimeError.abortion()
      }
      if let deadline, Date() > deadline {
        return nil
      }
      if Thread.isMainThread {
        if !RunLoop.current.run(mode: .default, before: Date(timeIntervalSinceNow: 0.05)) {
          Thread.sleep(forTimeInterval: 0.005)
        }
      } else {
        outcome.wait(upTo: 0.05)
      }
    }
    return outcome.current
  }

  // MARK: Configurations

  /// The entries of a configuration, i.e. an association list with symbol keys. The first entry
  /// for a key is used. `#f` and `()` stand for no entries. The functions of this structure
  /// return `nil` if an entry is missing or its value is `#f`.
  private struct ConfigEntries {
    private let entries: [String : (entry: Expr, value: Expr)]
    private let error: EvalError

    init(_ config: Expr?, allowed: Set<String>, error: EvalError) throws {
      self.error = error
      var entries: [String : (entry: Expr, value: Expr)] = [:]
      if let config, !config.isFalse {
        var list = config
        while case .pair(let entry, let rest) = list {
          guard case .pair(.symbol(let sym), let value) = entry,
                allowed.contains(sym.identifier) else {
            throw RuntimeError.eval(error, entry)
          }
          if entries[sym.identifier] == nil {
            entries[sym.identifier] = (entry, value)
          }
          list = rest
        }
        guard list.isNull else {
          throw RuntimeError.eval(error, config)
        }
      }
      self.entries = entries
    }

    subscript(_ key: String) -> (entry: Expr, value: Expr)? {
      return self.entries[key]
    }

    func string(_ key: String) throws -> String? {
      guard let (entry, value) = self.entries[key], !value.isFalse else {
        return nil
      }
      guard case .string(let str) = value else {
        throw RuntimeError.eval(self.error, entry)
      }
      return str as String
    }

    func double(_ key: String) throws -> Double? {
      guard let (entry, value) = self.entries[key], !value.isFalse else {
        return nil
      }
      switch value {
        case .fixnum(let n):
          return Double(n)
        case .flonum(let x):
          return x
        default:
          throw RuntimeError.eval(self.error, entry)
      }
    }

    /// Returns a positive number, or a non-negative one if `zero` is true.
    func length(_ key: String, zero: Bool = false) throws -> CGFloat? {
      guard let x = try self.double(key) else {
        return nil
      }
      guard x > 0.0 || (zero && x == 0.0) else {
        throw RuntimeError.eval(self.error, self.entries[key]!.entry)
      }
      return CGFloat(x)
    }

    /// Returns a positive integer.
    func int(_ key: String) throws -> Int? {
      guard let (entry, value) = self.entries[key], !value.isFalse else {
        return nil
      }
      guard case .fixnum(let n) = value, n > 0, let res = Int(exactly: n) else {
        throw RuntimeError.eval(self.error, entry)
      }
      return res
    }

    /// Returns the name of the symbol if it is one of `names`.
    func symbol(_ key: String, in names: Set<String>) throws -> String? {
      guard let (entry, value) = self.entries[key], !value.isFalse else {
        return nil
      }
      guard case .symbol(let sym) = value, names.contains(sym.identifier) else {
        throw RuntimeError.eval(self.error, entry)
      }
      return sym.identifier
    }

    /// Returns a dictionary for an association list mapping symbols or strings to strings.
    func strings(_ key: String) throws -> [String : String]? {
      guard let (entry, value) = self.entries[key], !value.isFalse else {
        return nil
      }
      var res: [String : String] = [:]
      var list = value
      while case .pair(.pair(let name, .string(let str)), let rest) = list {
        switch name {
          case .symbol(let sym):
            res[sym.identifier] = str as String
          case .string(let name):
            res[name as String] = str as String
          default:
            throw RuntimeError.eval(self.error, entry)
        }
        list = rest
      }
      guard list.isNull else {
        throw RuntimeError.eval(self.error, entry)
      }
      return res
    }
  }
}
