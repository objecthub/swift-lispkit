//
//  MarkdownLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 24/10/2019.
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
import MarkdownKit
import CommandLineKit

public final class MarkdownLibrary: NativeLibrary {
  
  // Global configuration defaults for all `MarkdownLibrary` instances
  
  /// Specification of the global library configuration; these define defaults for all
  /// library instantiation.
  public struct Config: LibraryConfig, Equatable, Sendable {
    
    /// This is what the config creates in the current context
    public struct Resource {
      public let syntaxHighlighter: SyntaxHighlighter?
      public let mermaidRenderer: MermaidRenderer?
      public let mathRenderer: MathRenderer?
    }
    
    // Default syntax highlighter configuration
    
    /// The URL of the JavaScript source code; if this is `nil`, the `themeDirectory`
    /// URL won't be used.
    public let highlightJSURL: URL?
    public let themeDirectory: URL?
    
    // Default Mermaid renderer configuration
    
    /// The URL of the Mermaid source code; if this is `nil`, the other Mermaid-related
    /// options are being ignored.
    public let mermaidJSURL: URL?
    public let cachesScript: Bool?
    public let idleTime: TimeInterval?
    public let maxCachedDiagrams: Int?
    public let maxCacheBytes: Int?
    
    /// The URL of the KaTeX foler; if this is `nil`, the other KaTeX-related
    /// options are being ignored
    public let katexURL: URL?
    public let katexIdleTime: TimeInterval?
    public let maxCachedFormulas: Int?
    public let maxKatexCacheBytes: Int?
    
    /// Initializer
    public init(highlightJSURL: URL? = nil,
                themeDirectory: URL? = nil,
                mermaidJSURL: URL? = nil,
                cachesScript: Bool? = nil,
                idleTime: TimeInterval? = nil,
                maxCachedDiagrams: Int? = nil,
                maxCacheBytes: Int? = nil,
                katexURL: URL? = nil,
                katexIdleTime: TimeInterval? = nil,
                maxCachedFormulas: Int? = nil,
                maxKatexCacheBytes: Int? = nil) {
      self.highlightJSURL = highlightJSURL
      self.themeDirectory = themeDirectory
      self.mermaidJSURL = mermaidJSURL
      self.cachesScript = cachesScript
      self.idleTime = idleTime
      self.maxCachedDiagrams = maxCachedDiagrams
      self.maxCacheBytes = maxCacheBytes
      self.katexURL = katexURL
      self.katexIdleTime = katexIdleTime
      self.maxCachedFormulas = maxCachedFormulas
      self.maxKatexCacheBytes = maxKatexCacheBytes
    }
    
    /// Returns the URL of `highlight.min.js` if available
    private func highlightURL(in context: Context) -> URL? {
      if let highlightJSURL {
        return highlightJSURL
      } else if let path = context.fileHandler.assetFilePath(forFile: "highlight.min",
                                                             ofType: "js",
                                                             inFolder: "JavaScript/Highlight") {
        return URL(fileURLWithPath: path)
      } else {
        return nil
      }
    }
    
    /// Returns the URL of the themes directory for the syntax highlighter if available;
    /// it requires that theme `MarkdownKit` is present. This is the default.
    private func highlightThemesURL(in context: Context) -> URL? {
      if let themeDirectory {
        return themeDirectory
      } else if let c = context.fileHandler.assetFilePath(forFile: "markdownkit",
                                                          ofType: "css",
                                                          inFolder: "JavaScript/Highlight/Themes") {
        return URL(fileURLWithPath: c).deletingLastPathComponent()
      } else {
        return nil
      }
    }
    
    /// Returns the URL of `mermaid.min.js` if available
    private func mermaidURL(in context: Context) -> URL? {
      if let mermaidJSURL {
        return mermaidJSURL
      } else if let path = context.fileHandler.assetFilePath(forFile: "mermaid.min",
                                                             ofType: "js",
                                                             inFolder: "JavaScript/Mermaid") {
        return URL(fileURLWithPath: path)
      } else {
        return nil
      }
    }
    
    /// Returns the URL of `katex.min.js` if available
    private func katexURL(in context: Context) -> URL? {
      if let katexURL {
        return katexURL
      } else if let path = context.fileHandler.assetFilePath(forFile: "katex.min",
                                                             ofType: "js",
                                                             inFolder: "JavaScript/KaTeX") {
        return URL(fileURLWithPath: path).deletingLastPathComponent()
      } else {
        return nil
      }
    }
    
    /// Turns this configuration into a resource for the current context
    public func evaluate(in context: Context) -> Resource {
      var highlighter: SyntaxHighlighter? = nil
      if let highlightURL = self.highlightURL(in: context) {
        highlighter = SyntaxHighlighter(highlightJSURL: highlightURL,
                                        themeDirectory: self.highlightThemesURL(in: context))
      }
      var mermaidRenderer: MermaidRenderer? = nil
      if let mermaidURL = self.mermaidURL(in: context) {
        mermaidRenderer = MermaidRenderer(
          mermaidJSURL: mermaidURL,
          cachesScript: self.cachesScript ?? true,
          idleTime: self.idleTime ?? MermaidRenderer.defaultIdleTime,
          maxCachedDiagrams: self.maxCachedDiagrams ?? MermaidRenderer.defaultMaxCachedDiagrams,
          maxCacheBytes: self.maxCacheBytes ?? MermaidRenderer.defaultMaxCacheBytes)
      }
      var mathRenderer: MathRenderer? = nil
      if let katexURL = self.katexURL(in: context) {
        mathRenderer = MathRenderer(
          katexFolder: katexURL,
          idleTime: self.katexIdleTime ?? MathRenderer.defaultIdleTime,
          maxCachedFormulas: self.maxCachedFormulas ?? MathRenderer.defaultMaxCachedFormulas,
          maxCacheBytes: self.maxKatexCacheBytes ?? MathRenderer.defaultMaxCacheBytes)
      }
      return Resource(syntaxHighlighter: highlighter ?? SyntaxHighlighter.proxy,
                      mermaidRenderer: mermaidRenderer ?? MermaidRenderer.shared,
                      mathRenderer: mathRenderer ?? MathRenderer.shared)
    }
  }
  
  // Type tags
  
  private let blockTypeTag = Symbol(uninterned: "block")
  private let listItemTypeTag = Symbol(uninterned: "list-item")
  private let inlineTypeTag = Symbol(uninterned: "inline")
  
  // Datatype elements
  
  public let blockType: Expr
  private let document: Symbol
  private let blockquote: Symbol
  private let listItems: Symbol
  private let paragraph: Symbol
  private let heading: Symbol
  private let indentedCode: Symbol
  private let fencedCode: Symbol
  private let htmlBlock: Symbol
  private let referenceDef: Symbol
  private let thematicBreak: Symbol
  private let mathBlock: Symbol
  private let table: Symbol
  private let definitionList: Symbol
  
  public let listItemType: Expr
  private let bullet: Symbol
  private let ordered: Symbol
  private let task: Symbol

  public let inlineType: Expr
  private let text: Symbol
  private let code: Symbol
  private let emph: Symbol
  private let strong: Symbol
  private let underline: Symbol
  private let strikethrough: Symbol
  private let link: Symbol
  private let autoLink: Symbol
  private let emailAutoLink: Symbol
  private let image: Symbol
  private let html: Symbol
  private let math: Symbol
  private let lineBreak: Symbol
  
  private let left: Symbol
  private let right: Symbol
  private let center: Symbol
  
  /// Syntax highlighter used by this library instance (no synchronization needed; the
  /// protocol ensures that this variable is set once)
  private var syntaxHighlighter: SyntaxHighlighter? = SyntaxHighlighter.proxy
  
  /// Mermaid renderer used by this library instance (no synchronization needed; the
  /// protocol ensures that this variable is set once)
  private var mermaidRenderer: MermaidRenderer? = MermaidRenderer.shared
  
  /// Math renderer used by this library instance (no synchronization needed; the
  /// protocol ensures that this variable is set once)
  private var mathRenderer: MathRenderer? = MathRenderer.shared
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    self.blockType = .pair(.symbol(self.blockTypeTag), .null)
    self.document = context.symbols.intern("document")
    self.blockquote = context.symbols.intern("blockquote")
    self.listItems = context.symbols.intern("list-items")
    self.paragraph = context.symbols.intern("paragraph")
    self.heading = context.symbols.intern("heading")
    self.indentedCode = context.symbols.intern("indented-code")
    self.fencedCode = context.symbols.intern("fenced-code")
    self.htmlBlock = context.symbols.intern("html-block")
    self.referenceDef = context.symbols.intern("reference-def")
    self.thematicBreak = context.symbols.intern("thematic-break")
    self.mathBlock = context.symbols.intern("math-block")
    self.table = context.symbols.intern("table")
    self.definitionList = context.symbols.intern("definition-list")
    self.listItemType = .pair(.symbol(self.listItemTypeTag), .null)
    self.bullet = context.symbols.intern("bullet")
    self.ordered = context.symbols.intern("ordered")
    self.task = context.symbols.intern("task")
    self.inlineType = .pair(.symbol(self.inlineTypeTag), .null)
    self.text = context.symbols.intern("text")
    self.code = context.symbols.intern("code")
    self.emph = context.symbols.intern("emph")
    self.strong = context.symbols.intern("strong")
    self.underline = context.symbols.intern("underline")
    self.strikethrough = context.symbols.intern("strikethrough")
    self.link = context.symbols.intern("link")
    self.autoLink = context.symbols.intern("auto-link")
    self.emailAutoLink = context.symbols.intern("email-auto-link")
    self.image = context.symbols.intern("image")
    self.html = context.symbols.intern("html")
    self.math = context.symbols.intern("math")
    self.lineBreak = context.symbols.intern("line-break")
    self.left = context.symbols.intern("l")
    self.right = context.symbols.intern("r")
    self.center = context.symbols.intern("c")
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "markdown"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    // Look up resource for the configuration of this library
    if let resource = self.context.configs.resource(for: Config.self) {
      self.syntaxHighlighter = resource.syntaxHighlighter
      self.mermaidRenderer = resource.mermaidRenderer
      self.mathRenderer = resource.mathRenderer
    }
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define("block-type-tag", as: .symbol(self.blockTypeTag))
    self.define("list-item-type-tag", as: .symbol(self.listItemTypeTag))
    self.define("inline-type-tag", as: .symbol(self.inlineTypeTag))
    self.define(Procedure("markdown-blocks?", isMarkdownBlocks))
    self.define(Procedure("markdown-block?", isMarkdownBlock))
    self.define(Procedure("markdown-block=?", markdownBlockEquals))
    self.define(Procedure("document", document))
    self.define(Procedure("blockquote", blockquote))
    self.define(Procedure("list-items", listItems))
    self.define(Procedure("paragraph", paragraph))
    self.define(Procedure("heading", heading))
    self.define(Procedure("indented-code", indentedCode))
    self.define(Procedure("fenced-code", fencedCode))
    self.define(Procedure("html-block", htmlBlock))
    self.define(Procedure("reference-def", referenceDef))
    self.define(Procedure("thematic-break", thematicBreak))
    self.define(Procedure("math-block", mathBlock))
    self.define(Procedure("table", table))
    self.define(Procedure("definition-list", definitionList))
    self.define(Procedure("markdown-list?", isMarkdownList))
    self.define(Procedure("markdown-list-item?", isMarkdownListItem))
    self.define(Procedure("markdown-list-item=?", markdownListItemEquals))
    self.define(Procedure("bullet", bullet))
    self.define(Procedure("ordered", ordered))
    self.define(Procedure("task", task))
    self.define(Procedure("markdown-text?", isMarkdownText))
    self.define(Procedure("markdown-inline?", isMarkdownInline))
    self.define(Procedure("markdown-inline=?",markdownInlineEquals))
    self.define(Procedure("text", text))
    self.define(Procedure("code", code))
    self.define(Procedure("emph", emph))
    self.define(Procedure("strong", strong))
    self.define(Procedure("underline", underline))
    self.define(Procedure("strikethrough", strikethrough))
    self.define(Procedure("link", link))
    self.define(Procedure("auto-link", autoLink))
    self.define(Procedure("email-auto-link", emailAutoLink))
    self.define(Procedure("image", image))
    self.define(Procedure("html", html))
    self.define(Procedure("math", math))
    self.define(Procedure("line-break", lineBreak))
    self.define(Procedure("markdown", markdown))
    self.define(Procedure("markdown?", isMarkdown))
    self.define(Procedure("markdown=?", markdownEquals))
    self.define(Procedure("markdown->string", markdownToString))
    self.define(Procedure("markdown-style-config", markdownStyleConfig))
    self.define(Procedure("markdown->styled-text", markdownToStyledText))
    self.define(Procedure("markdown->html-doc", markdownToHtmlDoc))
    self.define(Procedure("markdown->html", markdownToHtml))
    self.define(Procedure("markdown->sxml", markdownToSxml))
    self.define(Procedure("markdown->debug-string", markdownToDebugString))
    self.define(Procedure("markdown->raw-string", markdownToRawString))
    self.define(Procedure("blocks->html", blocksToHtml))
    self.define(Procedure("blocks->sxml", blocksToSxml))
    self.define(Procedure("blocks->raw-string", blocksToRawString))
    self.define(Procedure("text->html", textToHtml))
    self.define(Procedure("text->sxml", textToSxml))
    self.define(Procedure("text->string", textToString))
    self.define(Procedure("text->raw-string", textToRawString))
    self.define(Procedure("syntax-highlighting-theme", syntaxHighlightingTheme))
    self.define(Procedure("syntax-highlighting-themes", syntaxHighlightingThemes))
  }
  
  private func makeCase(_ type: Expr, _ sym: Symbol, _ exprs: Expr...) -> Expr {
    return .tagged(type, .pair(.symbol(sym), .makeList(Exprs(exprs))))
  }
  
  private func makeCase(_ type: Expr,
                        _ sym: Symbol,
                        _ n: Int,
                        _ args: Arguments) throws -> Expr {
    if case .some(.symbol(InternalLibrary.deconstructionTag)) = args.first {
      guard args.count == 2,
            case .tagged(type, let payload) = args[args.index(after: args.startIndex)],
            case .pair(.symbol(sym), let res) = payload else {
        return .false
      }
      return res
    }
    guard args.count == n else {
      throw RuntimeError.argumentCount(num: n, args: .makeList(args))
    }
    return .tagged(type, .pair(.symbol(sym), .makeList(args)))
  }
  
  private func checkBlocks(_ expr: Expr) throws {
    var list = expr
    while case .pair(let block, let rest) = list {
      try self.checkBlock(block)
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.custom("type error",
                                "markdown block lists must be proper lists: \(expr)", [])
    }
  }
  
  private func checkBlock(_ expr: Expr) throws {
    guard case .tagged(self.blockType, _) = expr else {
      throw RuntimeError.custom("type error", "expected value of type block; received \(expr)", [])
    }
  }
  
  private func checkListItems(_ expr: Expr) throws {
    var list = expr
    while case .pair(let item, let rest) = list {
      try self.checkListItem(item)
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.custom("type error",
                                "markdown list items must be proper lists: \(expr)", [])
    }
  }
  
  private func checkListItem(_ expr: Expr) throws {
    guard case .tagged(self.listItemType, _) = expr else {
      throw RuntimeError.custom("type error",
                                "expected value of type list-item; received \(expr)", [])
    }
  }
  
  private func checkDefinitions(_ expr: Expr) throws {
    var list = expr
    while case .pair(let def, let rest) = list {
      try self.checkDefinition(def)
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.custom("type error",
                                "not a proper list of markdown definitions: \(expr)", [])
    }
  }
  
  private func checkDefinition(_ expr: Expr) throws {
    guard case .pair(let item, let blocks) = expr else {
      throw RuntimeError.custom("type error", "not a valid markdown definition: \(expr)", [])
    }
    try self.checkText(item)
    try self.checkListItems(blocks)
  }
  
  private func checkAlignments(_ expr: Expr) throws {
    var list = expr
  outer: while case .pair(let alignment, let rest) = list {
    if alignment.isTrue {
      guard case .symbol(let sym) = alignment else {
        break
      }
      switch sym {
        case self.left, self.right, self.center:
          break
        default:
          break outer
      }
    }
    list = rest
  }
    guard list.isNull else {
      throw RuntimeError.custom("type error",
                                "not a valid list of markdown table column alignments: \(expr)", [])
    }
  }
  
  private func checkRows(_ expr: Expr) throws {
    var list = expr
    while case .pair(let row, let rest) = list {
      try self.checkRow(row)
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.custom("type error",
                                "not a proper list of markdown table rows: \(expr)", [])
    }
  }
  
  private func checkRow(_ expr: Expr) throws {
    var list = expr
    while case .pair(let cell, let rest) = list {
      try self.checkText(cell)
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.custom("type error", "not a valid markdown table row: \(expr)", [])
    }
  }
  
  private func checkText(_ expr: Expr) throws {
    var list = expr
    while case .pair(let fragment, let rest) = list {
      guard case .tagged(self.inlineType, _) = fragment else {
        break
      }
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.custom("type error", "markdown text must be represented by a proper " +
                                "list of inline elements: \(expr)", [])
    }
  }
  
  private func checkLines(_ expr: Expr) throws {
    var list = expr
    while case .pair(let line, let rest) = list {
      guard case .string(_) = line else {
        break
      }
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.custom("type error", "markdown text lines must be represented by a " +
                                "proper list of strings: \(expr)", [])
    }
  }
  
  private func isMarkdownBlocks(_ expr: Expr) -> Expr {
    var list = expr
    while case .pair(.tagged(self.blockType, _), let rest) = list {
      list = rest
    }
    return .makeBoolean(list == .null)
  }
  
  private func isMarkdownBlock(_ expr: Expr) -> Expr {
    guard case .tagged(self.blockType, _) = expr else {
      return .false
    }
    return .true
  }
  
  private func markdownBlockEquals(_ lhs: Expr, _ rhs: Expr) throws -> Expr {
    let lhsMd = try self.internMarkdown(block: lhs)
    let rhsMd = try self.internMarkdown(block: rhs)
    return .makeBoolean(lhsMd == rhsMd)
  }
  
  private func document(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.blockType, self.document, 1, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    try self.checkBlocks(args.first!)
    return res
  }
  
  private func blockquote(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.blockType, self.blockquote, 1, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    try self.checkBlocks(args.first!)
    return res
  }
  
  private func listItems(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.blockType, self.listItems, 3, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    if args.first?.isTrue ?? false {
      _ = try args.first?.asInt()
    }
    try self.checkListItems(args[args.index(args.startIndex, offsetBy: 2)])
    return res
  }
  
  private func paragraph(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.blockType, self.paragraph, 1, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    try self.checkText(args.first!)
    return res
  }
  
  private func heading(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.blockType, self.heading, 2, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    _ = try args.first?.asInt()
    try self.checkText(args[args.index(args.startIndex, offsetBy: 1)])
    return res
  }
  
  private func indentedCode(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.blockType, self.indentedCode, 1, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    try self.checkLines(args.first!)
    return res
  }
  
  private func fencedCode(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.blockType, self.fencedCode, 2, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    if args.first?.isTrue ?? false {
      _ = try args.first!.asString()
    }
    try self.checkLines(args[args.index(args.startIndex, offsetBy: 1)])
    return res
  }
  
  private func htmlBlock(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.blockType, self.htmlBlock, 1, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    try self.checkLines(args.first!)
    return res
  }
  
  private func referenceDef(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.blockType, self.referenceDef, 3, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    _ = try args.first?.asString()
    _ = try args[args.index(args.startIndex, offsetBy: 1)].asString()
    try self.checkLines(args[args.index(args.startIndex, offsetBy: 2)])
    return res
  }
  
  private func thematicBreak(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.blockType, self.thematicBreak, 0, args)
  }
  
  private func table(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.blockType, self.table, 3, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    try self.checkRow(args.first!)
    try self.checkAlignments(args[args.index(args.startIndex, offsetBy: 1)])
    try self.checkRows(args[args.index(args.startIndex, offsetBy: 2)])
    return res
  }
  
  private func definitionList(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.blockType, self.definitionList, 1, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    try self.checkDefinitions(args.first!)
    return res
  }
  
  private func isMarkdownList(_ expr: Expr) -> Expr {
    var list = expr
    while case .pair(.tagged(self.listItemType, _), let rest) = list {
      list = rest
    }
    return .makeBoolean(list == .null)
  }
  
  private func isMarkdownListItem(_ expr: Expr) -> Expr {
    guard case .tagged(self.listItemType, _) = expr else {
      return .false
    }
    return .true
  }
  
  private func markdownListItemEquals(_ lhs: Expr, _ rhs: Expr) throws -> Expr {
    let lhsMd = try self.internMarkdown(item: lhs)
    let rhsMd = try self.internMarkdown(item: rhs)
    return .makeBoolean(lhsMd == rhsMd)
  }
  
  private func bullet(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.listItemType, self.bullet, 3, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    _ = try args.first?.asUniChar()
    try self.checkBlocks(args[args.index(args.startIndex, offsetBy: 2)])
    return res
  }
  
  private func ordered(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.listItemType, self.ordered, 4, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    _ = try args.first?.asInt()
    _ = try args[args.index(args.startIndex, offsetBy: 1)].asUniChar()
    try self.checkBlocks(args[args.index(args.startIndex, offsetBy: 3)])
    return res
  }

  private func task(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.listItemType, self.task, 2, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    let item = args[args.index(args.startIndex, offsetBy: 1)]
    guard case .tagged(self.listItemType, .pair(.symbol(let sym), _)) = item,
          sym == self.bullet || sym == self.ordered else {
      throw RuntimeError.eval(.notAValidTask, item)
    }
    return res
  }

  private func isMarkdownText(_ expr: Expr) -> Expr {
    var list = expr
    while case .pair(.tagged(self.inlineType, _), let rest) = list {
      list = rest
    }
    return .makeBoolean(list == .null)
  }
  
  private func isMarkdownInline(_ expr: Expr) -> Expr {
    guard case .tagged(self.inlineType, _) = expr else {
      return .false
    }
    return .true
  }
  
  private func markdownInlineEquals(_ lhs: Expr, _ rhs: Expr) throws -> Expr {
    let lhsMd = try self.internMarkdown(fragment: lhs)
    let rhsMd = try self.internMarkdown(fragment: rhs)
    return .makeBoolean(lhsMd == rhsMd)
  }
  
  private func text(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.inlineType, self.text, 1, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    _ = try args.first?.asString()
    return res
  }
  
  private func code(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.inlineType, self.code, 1, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    _ = try args.first?.asString()
    return res
  }
  
  private func emph(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.inlineType, self.emph, 1, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    try self.checkText(args.first!)
    return res
  }
  
  private func strong(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.inlineType, self.strong, 1, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    try self.checkText(args.first!)
    return res
  }

  private func underline(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.inlineType, self.underline, 1, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    try self.checkText(args.first!)
    return res
  }

  private func strikethrough(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.inlineType, self.strikethrough, 1, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    try self.checkText(args.first!)
    return res
  }

  private func link(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.inlineType, self.link, 3, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    try self.checkText(args.first!)
    let uri = args[args.index(args.startIndex, offsetBy: 1)]
    if uri.isTrue {
      _ = try uri.asString()
    }
    let title = args[args.index(args.startIndex, offsetBy: 2)]
    if title.isTrue {
      _ = try title.asString()
    }
    return res
  }
  
  private func autoLink(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.inlineType, self.autoLink, 1, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    _ = try args.first?.asString()
    return res
  }
  
  private func emailAutoLink(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.inlineType, self.emailAutoLink, 1, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    _ = try args.first?.asString()
    return res
  }
  
  private func image(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.inlineType, self.image, 3, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    try self.checkText(args.first!)
    let uri = args[args.index(args.startIndex, offsetBy: 1)]
    if uri.isTrue {
      _ = try uri.asString()
    }
    let title = args[args.index(args.startIndex, offsetBy: 2)]
    if title.isTrue {
      _ = try title.asString()
    }
    return res
  }
  
  private func html(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.inlineType, self.html, 1, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    _ = try args.first?.asString()
    return res
  }
  
  private func lineBreak(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.inlineType, self.lineBreak, 1, args)
  }
  
  private func mathBlock(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.blockType, self.mathBlock, 1, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    _ = try args.first?.asString()
    return res
  }
  
  private func math(_ args: Arguments) throws -> Expr {
    let res = try self.makeCase(self.inlineType, self.math, 3, args)
    guard case .tagged(_, _) = res else {
      return res
    }
    _ = try args.first?.asString()
    let source = args[args.index(args.startIndex, offsetBy: 2)]
    if source.isTrue {
      _ = try source.asString()
    }
    return res
  }
  
  private func markdown(_ str: Expr, _ dialect: Expr?) throws -> Expr {
    let parser: MarkdownParser
    switch dialect {
      case .none, .false:
        parser = MarkdownParser.standard
      case .symbol(let sym):
        switch sym.identifier {
          case "standard":
            parser = MarkdownParser.standard
          case "extended":
            parser = ExtendedMarkdownParser.standard
          case "full":
            parser = FullMarkdownParser.standard
          case "math":
            parser = MathMarkdownParser.standard
          case "latex":
            parser = LaTeXMarkdownParser.standard
          default:
            throw RuntimeError.eval(.notAValidMarkdownDialect, .symbol(sym))
        }
      case .some(let expr):
        parser = LaTeXMarkdownParser.standard
    }
    guard let res = self.externMarkdown(parser.parse(try str.asString())) else {
      return .false
    }
    return res
  }
  
  private func isMarkdown(_ expr: Expr) throws -> Expr {
    guard case .tagged(self.blockType, .pair(.symbol(self.document), _)) = expr else {
      return .false
    }
    return (try? self.internMarkdown(block: expr)) == nil ? .false : .true
  }
  
  private func markdownEquals(_ lhs: Expr, _ rhs: Expr) throws -> Expr {
    let lhsMd = try self.internMarkdown(block: lhs)
    let rhsMd = try self.internMarkdown(block: rhs)
    return .makeBoolean(lhsMd == rhsMd)
  }
  
  /// Convert an `Expr` to `TextProperties`.
  /// The format can be:
  /// - #f or () for empty properties
  /// - A symbol representing a predefined style (e.g., 'bold, 'italic, 'red, etc.)
  /// - A list of up to 3 elements: (textColor backgroundColor styles...)
  ///   where textColor and backgroundColor are symbols or #f,
  ///   and styles is a list of style symbols
  private func asTextProperties(_ expr: Expr) throws -> TextProperties? {
    switch expr {
      case .false:
          return nil
      case .null:
        return .empty
      case .symbol(let sym):
        // Check for predefined styles
        switch sym.identifier {
          case "default":
            return .default
          case "bold":
            return .bold
          case "dim":
            return .dim
          case "italic":
            return .italic
          case "underline":
            return .underline
          case "blink":
            return .blink
          case "swap":
            return .swap
          case "strikethrough":
            return .strikethrough
          case "default-color":
            return .defaultColor
          case "black":
            return .black
          case "maroon":
            return .maroon
          case "green":
            return .green
          case "olive":
            return .olive
          case "navy":
            return .navy
          case "purple":
            return .purple
          case "teal":
            return .teal
          case "silver":
            return .silver
          case "grey":
            return .grey
          case "red":
            return .red
          case "lime":
            return .lime
          case "yellow":
            return .yellow
          case "blue":
            return .blue
          case "fuchsia":
            return .fuchsia
          case "aqua":
            return .aqua
          case "white":
            return .white
          default:
            throw RuntimeError.eval(.notAValidTextProperty, expr)
        }
      case .pair(let textColorExpr, let rest):
        // Parse as (textColor backgroundColor styles...)
        let textColor = try self.asTextColor(textColorExpr)
        var backgroundColor: BackgroundColor? = nil
        var textStyles: Set<CommandLineKit.TextStyle> = []
        switch rest {
          case .null:
            break
          case .pair(let bgExpr, let styleRest):
            // Parse background color
            backgroundColor = try self.asBackgroundColor(bgExpr)
            // Parse remaining elements as text styles
            var styleList = styleRest
            while case .pair(let styleExpr, let next) = styleList {
              if let style = try self.asTextStyle(styleExpr) {
                textStyles.insert(style)
              }
              styleList = next
            }
          default:
            throw RuntimeError.eval(.notAValidTextProperty, expr)
        }
        return TextProperties(textColor: textColor,
                             backgroundColor: backgroundColor,
                             textStyles: textStyles)
      default:
        throw RuntimeError.eval(.notAValidTextProperty, expr)
    }
  }
  
  /// Convert an `Expr` to `TextColor`
  private func asTextColor(_ expr: Expr) throws -> TextColor? {
    switch expr {
      case .false:
        return nil
      case .symbol(let sym):
        switch sym.identifier {
          case "default":
            return .default
          case "black":
            return .black
          case "maroon":
            return .maroon
          case "green":
            return .green
          case "olive":
            return .olive
          case "navy":
            return .navy
          case "purple":
            return .purple
          case "teal":
            return .teal
          case "silver":
            return .silver
          case "grey":
            return .grey
          case "red":
            return .red
          case "lime":
            return .lime
          case "yellow":
            return .yellow
          case "blue":
            return .blue
          case "fuchsia":
            return .fuchsia
          case "aqua":
            return .aqua
          case "white":
            return .white
          default:
            break
        }
      case .string(let mstr):
        let str: String = mstr.hasPrefix("#") ? mstr.substring(from: 1) : (mstr as String)
        if let num = Int64(str as String, radix: 16) {
          if str.count <= 3 {
            return TextColor(rgb: (Double((num & 0xf00) >> 8) / 15.0,
                                   Double((num & 0xf0) >> 4) / 15.0,
                                   Double(num & 0xf) / 15.0), fullColorSupport: true)
          } else {
            return TextColor(rgb: (Double((num & 0xff0000) >> 16) / 255.0,
                                   Double((num & 0xff00) >> 8) / 255.0,
                                   Double(num & 0xff) / 255.0), fullColorSupport: true)
          }
        }
      case .object(let obj):
        if let color = obj as? Color {
          return TextColor(rgb: (color.red, color.green, color.blue),
                                 fullColorSupport: true)
        }
      case .fixnum(let x):
        if x >= UInt8.min && x <= UInt8.max {
          return .extended(UInt8(x))
        }
      default:
        break
    }
    throw RuntimeError.eval(.notAValidTextColor, expr)
  }
  
  /// Convert an `Expr` to `BackgroundColor`
  private func asBackgroundColor(_ expr: Expr) throws -> BackgroundColor? {
    switch expr {
      case .false:
        return nil
      case .symbol(let sym):
        switch sym.identifier {
          case "default":
            return .default
          case "black":
            return .black
          case "white":
            return .white
          case "red":
            return .red
          case "green":
            return .green
          case "yellow":
            return .yellow
          case "blue":
            return .blue
          case "magenta":
            return .magenta
          case "cyan":
            return .cyan
          case "light-black":
            return .lightBlack
          case "light-red":
            return .lightRed
          case "light-green":
            return .lightGreen
          case "light-yellow":
            return .lightYellow
          case "light-blue":
            return .lightBlue
          case "light-magenta":
            return .lightMagenta
          case "light-cyan":
            return .lightCyan
          case "light-white":
            return .lightWhite
          default:
            break
        }
      case .string(let mstr):
        let str: String = mstr.hasPrefix("#") ? mstr.substring(from: 1) : (mstr as String)
        if let num = Int64(str as String, radix: 16) {
          if str.count <= 3 {
            return BackgroundColor(rgb: (Double((num & 0xf00) >> 8) / 15.0,
                                         Double((num & 0xf0) >> 4) / 15.0,
                                         Double(num & 0xf) / 15.0), fullColorSupport: true)
          } else {
            return BackgroundColor(rgb: (Double((num & 0xff0000) >> 16) / 255.0,
                                         Double((num & 0xff00) >> 8) / 255.0,
                                         Double(num & 0xff) / 255.0), fullColorSupport: true)
          }
        }
      case .object(let obj):
        if let color = obj as? Color {
          return BackgroundColor(rgb: (color.red, color.green, color.blue),
                                 fullColorSupport: true)
        }
      case .fixnum(let x):
        if x >= UInt8.min && x <= UInt8.max {
          return .extended(UInt8(x))
        }
      default:
        break
    }
    throw RuntimeError.eval(.notAValidBackgroundColor, expr)
  }
  
  /// Convert an `Expr` to `TextStyle`
  private func asTextStyle(_ expr: Expr) throws -> CommandLineKit.TextStyle? {
    switch expr {
      case .false:
        return nil
      case .symbol(let sym):
        switch sym.identifier {
          case "default":
            return .default
          case "bold":
            return .bold
          case "dim":
            return .dim
          case "italic":
            return .italic
          case "underline":
            return .underline
          case "blink":
            return .blink
          case "swap":
            return .swap
          case "strikethrough":
            return .strikethrough
          default:
            break
        }
      default:
        break
    }
    throw RuntimeError.eval(.notAValidTextStyle, expr)
  }
  
  /// Convert a list `Expr` to an array of `TextProperties` (for header properties)
  private func asTextPropertiesList(_ expr: Expr) throws -> [TextProperties] {
    var result: [TextProperties] = []
    var list = expr
    while case .pair(let head, let tail) = list {
      result.append(try self.asTextProperties(head) ?? .empty)
      list = tail
    }
    return result
  }
  
  /// Create a `TerminalGenerator` with configuration extracted from the `ansi` parameter.
  /// The `ansi` parameter is expected to be a list with the following structure:
  /// (headerProperties linkProperties codeProperties codeBlockBorderProperties
  ///  codeBlockLangProperties emphasisProperties strongProperties defTermProperties
  ///  defDescrProperties blockquoteProperties breakProperties syntaxHighlighting
  ///  underlineProperties strikethroughProperties)
  private func terminalGenerator(numColumns: Int, ansi: Expr) throws -> TerminalGenerator {
    var list = ansi
    // Extract headerProperties (list of TextProperties)
    var headerProperties: [TextProperties]? = nil
    if case .pair(let head, let tail) = list {
      if head.isTrue {
        headerProperties = try self.asTextPropertiesList(head)
      }
      list = tail
    }
    // Extract linkProperties
    var linkProperties: TextProperties? = nil
    if case .pair(let head, let tail) = list {
      linkProperties = try self.asTextProperties(head)
      list = tail
    }
    // Extract codeProperties
    var codeProperties: TextProperties? = nil
    if case .pair(let head, let tail) = list {
      codeProperties = try self.asTextProperties(head)
      list = tail
    }
    // Extract codeBlockBorderProperties
    var codeBlockBorderProperties: TextProperties? = nil
    if case .pair(let head, let tail) = list {
      codeBlockBorderProperties = try self.asTextProperties(head)
      list = tail
    }
    // Extract codeBlockLangProperties
    var codeBlockLangProperties: TextProperties? = nil
    if case .pair(let head, let tail) = list {
      codeBlockLangProperties = try self.asTextProperties(head)
      list = tail
    }
    // Extract emphasisProperties
    var emphasisProperties: TextProperties? = nil
    if case .pair(let head, let tail) = list {
      emphasisProperties = try self.asTextProperties(head)
      list = tail
    }
    // Extract strongProperties
    var strongProperties: TextProperties? = nil
    if case .pair(let head, let tail) = list {
      strongProperties = try self.asTextProperties(head)
      list = tail
    }
    // Extract defTermProperties
    var defTermProperties: TextProperties? = nil
    if case .pair(let head, let tail) = list {
      defTermProperties = try self.asTextProperties(head)
      list = tail
    }
    // Extract defDescrProperties
    var defDescrProperties: TextProperties? = nil
    if case .pair(let head, let tail) = list {
      defDescrProperties = try self.asTextProperties(head)
      list = tail
    }
    // Extract blockquoteProperties
    var blockquoteProperties: TextProperties? = nil
    if case .pair(let head, let tail) = list {
      blockquoteProperties = try self.asTextProperties(head)
      list = tail
    }
    // Extract breakProperties
    var breakProperties: TextProperties? = nil
    if case .pair(let head, let tail) = list {
      breakProperties = try self.asTextProperties(head)
      list = tail
    }
    // Extract syntaxHighlighting configuration
    var syntaxHighlighting: TerminalGenerator.SyntaxHighlightingConfig? = .default
    if case .pair(let head, let tail) = list {
      list = tail
      if head.isFalse {
        syntaxHighlighting = nil
      } else if case .pair(let themeExpr, let hlTail) = head {
        var theme: String = TerminalGenerator.SyntaxHighlightingConfig.default.theme
        var ignoreSyntacticIssues: Bool = true
        var ignoredLanguages: Set<String> = []
        var highlightIndentedCodeBlocks: Bool = true
        var fullColorSupport: Bool = true
        if themeExpr.isTrue {
          theme = try themeExpr.asString()
        }
        var hlList = hlTail
        if case .pair(let issuesExpr, let hlNext) = hlList {
          if !issuesExpr.isNull {
            ignoreSyntacticIssues = issuesExpr.isTrue
          }
          hlList = hlNext
        }
        if case .pair(let langsExpr, let hlNext) = hlList {
          if langsExpr.isTrue {
            var langList = langsExpr
            while case .pair(let lang, let next) = langList {
              ignoredLanguages.insert(try lang.asString())
              langList = next
            }
          }
          hlList = hlNext
        }
        if case .pair(let indentedExpr, let hlNext) = hlList {
          if !indentedExpr.isNull {
            highlightIndentedCodeBlocks = indentedExpr.isTrue
          }
          hlList = hlNext
        }
        if case .pair(let fullColorExpr, _) = hlList {
          if !fullColorExpr.isNull {
            fullColorSupport = fullColorExpr.isTrue
          }
        }
        syntaxHighlighting = TerminalGenerator.SyntaxHighlightingConfig(
                                theme: theme,
                                ignoreSyntacticIssues: ignoreSyntacticIssues,
                                ignoredLanguages: ignoredLanguages,
                                highlightIndentedCodeBlocks: highlightIndentedCodeBlocks,
                                fullColorSupport: fullColorSupport)
      }
    }
    // Extract underlineProperties
    var underlineProperties: TextProperties? = nil
    if case .pair(let head, let tail) = list {
      underlineProperties = try self.asTextProperties(head)
      list = tail
    }
    // Extract strikethroughProperties
    var strikethroughProperties: TextProperties? = nil
    if case .pair(let head, let tail) = list {
      strikethroughProperties = try self.asTextProperties(head)
      list = tail
    }
    return TerminalGenerator(
             numColumns: numColumns,
             headerProperties: headerProperties,
             linkProperties: linkProperties,
             codeProperties: codeProperties,
             codeBlockBorderProperties: codeBlockBorderProperties,
             codeBlockLangProperties: codeBlockLangProperties,
             emphasisProperties: emphasisProperties,
             strongProperties: strongProperties,
             underlineProperties: underlineProperties,
             strikethroughProperties: strikethroughProperties,
             defTermProperties: defTermProperties,
             defDescrProperties: defDescrProperties,
             blockquoteProperties: blockquoteProperties,
             breakProperties: breakProperties,
             syntaxHighlighting: syntaxHighlighting,
             syntaxHighlighter: self.syntaxHighlighter)
  }
  
  private func markdownToString(_ md: Expr, _ width: Expr?, _ ansi: Expr?) throws -> Expr {
    let md = try self.internMarkdown(block: md)
    let maxCols: Int
    if let width, width.isTrue {
      maxCols = try width.asInt(above: 0, below: 10000)
    } else if let size = Sysctl.terminalSize {
      maxCols = size.cols
    } else {
      maxCols = 80
    }
    if let ansi, ansi.isTrue {
      if case .true = ansi {
        return .makeString(TerminalGenerator(numColumns: maxCols,
                                             syntaxHighlighter: self.syntaxHighlighter)
                             .generate(doc: md).encodedString)
      } else {
        let generator = try self.terminalGenerator(numColumns: maxCols, ansi: ansi)
        return .makeString(generator.generate(doc: md).encodedString)
      }
    } else {
      return .makeString(StringGenerator(numColumns: maxCols).generate(doc: md))
    }
  }
  
  // MARK: - Configuration of attributed string generators
  
  private typealias RenderingOptions = AttributedStringGenerator.RenderingOptions
  private typealias ImageAccess = AttributedStringGenerator.ImageAccess
  private typealias ConfigEntries = [String : (entry: Expr, value: Expr)]
  
  /// Keys of the association lists configuring attributed string generators
  private static let configKeys: Set<String> = [
    "style", "font-size", "font-family", "font-color", "line-spacing",
    "code-font-size", "code-font-family", "code-font-color",
    "code-block-font-size", "code-block-font-color", "code-block-line-spacing",
    "code-block-background", "border-color", "blockquote-color", "header-font",
    "h1-color", "h2-color", "h3-color", "h4-color",
    "h1-font-size", "h2-font-size", "h3-font-size", "h4-font-size",
    "syntax-highlighting", "tight-lists", "align-list-markers",
    "max-image-width", "max-image-height", "custom-style", "image-base-url",
    "preset", "base-url", "timeout", "local-images", "remote-images", "image-extensions",
    "safe-mode", "text-size-multiplier", "mermaid", "math"
  ]
  
  /// Keys of the association lists configuring the rendering of Mermaid diagrams
  private static let mermaidKeys: Set<String> = [
    "theme", "theme-variables", "font-family", "scale", "width", "margin", "max-width",
    "spacing", "timeout"
  ]
  
  /// Keys of the association lists configuring syntax highlighting
  private static let syntaxHighlightingKeys: Set<String> = [
    "theme", "ignore-syntax-errors", "ignored-languages", "highlight-indented-code-blocks"
  ]
  
  /// Extracts the entries of a configuration, i.e. an association list with symbol keys, for a
  /// given set of allowed keys. If a key occurs more than once, the first occurrence is used.
  /// `#f` and `()` stand for empty configurations.
  private func configEntries(_ config: Expr?, allowed: Set<String>) throws -> ConfigEntries {
    guard let config = config, !config.isFalse else {
      return [:]
    }
    var entries: ConfigEntries = [:]
    var list = config
    while case .pair(let entry, let rest) = list {
      guard case .pair(.symbol(let sym), let value) = entry, allowed.contains(sym.identifier) else {
        throw RuntimeError.eval(.notAValidMarkdownConfig, entry)
      }
      if entries[sym.identifier] == nil {
        entries[sym.identifier] = (entry, value)
      }
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.eval(.notAValidMarkdownConfig, config)
    }
    return entries
  }
  
  /// Returns the string value for `key`; `nil` if the key is missing or its value is `#f`.
  private func configString(_ entries: ConfigEntries, _ key: String) throws -> String? {
    guard let (entry, value) = entries[key], !value.isFalse else {
      return nil
    }
    guard case .string(let str) = value else {
      throw RuntimeError.eval(.notAValidMarkdownConfig, entry)
    }
    return str as String
  }
  
  /// Returns the number for `key`; `nil` if the key is missing or its value is `#f`.
  private func configDouble(_ entries: ConfigEntries, _ key: String) throws -> Double? {
    guard let (entry, value) = entries[key], !value.isFalse else {
      return nil
    }
    switch value {
      case .fixnum(let n):
        return Double(n)
      case .flonum(let x):
        return x
      default:
        throw RuntimeError.eval(.notAValidMarkdownConfig, entry)
    }
  }
  
  /// Returns the boolean for `key`; `nil` if the key is missing.
  private func configBool(_ entries: ConfigEntries, _ key: String) throws -> Bool? {
    guard let (entry, value) = entries[key] else {
      return nil
    }
    switch value {
      case .true:
        return true
      case .false:
        return false
      default:
        throw RuntimeError.eval(.notAValidMarkdownConfig, entry)
    }
  }
  
  /// Returns the list of strings for `key`; `nil` if the key is missing or its value is `#f`.
  private func configStrings(_ entries: ConfigEntries, _ key: String) throws -> [String]? {
    guard let (entry, value) = entries[key], !value.isFalse else {
      return nil
    }
    var res: [String] = []
    var list = value
    while case .pair(.string(let str), let rest) = list {
      res.append(str as String)
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.eval(.notAValidMarkdownConfig, entry)
    }
    return res
  }
  
  /// Returns the name of the symbol for `key` if it is one of `names`; `nil` if the key is
  /// missing or its value is `#f`.
  private func configSymbol(_ entries: ConfigEntries,
                            _ key: String,
                            in names: Set<String>) throws -> String? {
    guard let (entry, value) = entries[key], !value.isFalse else {
      return nil
    }
    guard case .symbol(let sym) = value, names.contains(sym.identifier) else {
      throw RuntimeError.eval(.notAValidMarkdownConfig, entry)
    }
    return sym.identifier
  }
  
  /// Maps a string to a URL. Strings starting with a scheme like `http:` are interpreted as
  /// URLs, all other strings as paths (relative to the current directory) of directories.
  private func asURL(_ str: String) -> URL {
    if let url = URL(string: str),
       let scheme = url.scheme?.lowercased(),
       ["http", "https", "file"].contains(scheme) {
      return url
    }
    return URL(fileURLWithPath: self.context.fileHandler.path(
                                  str,
                                  relativeTo: self.context.evaluator.currentDirectoryPath),
               isDirectory: true)
  }
  
  private func configURL(_ entries: ConfigEntries, _ key: String) throws -> URL? {
    return try self.configString(entries, key).map { self.asURL($0) }
  }
  
  /// Returns the image access for `key`: `none`, `any`, or a string for restricting access to
  /// a directory (local images) or to a web site (remote images).
  private func configImageAccess(_ entries: ConfigEntries,
                                 _ key: String,
                                 remote: Bool) throws -> ImageAccess? {
    guard let (entry, value) = entries[key], !value.isFalse else {
      return nil
    }
    switch value {
      case .symbol(let sym) where sym.identifier == "none":
        return ImageAccess.none
      case .symbol(let sym) where sym.identifier == "any":
        return ImageAccess.any
      case .string(let str):
        if remote {
          guard let url = URL(string: str as String),
                let scheme = url.scheme?.lowercased(),
                scheme == "http" || scheme == "https" else {
            throw RuntimeError.eval(.notAValidMarkdownConfig, entry)
          }
          return ImageAccess.within(url)
        } else {
          return ImageAccess.within(self.asURL(str as String))
        }
      default:
        throw RuntimeError.eval(.notAValidMarkdownConfig, entry)
    }
  }
  
  /// Maps the value of the entry `syntax-highlighting` of a configuration to a configuration of
  /// syntax highlighting. `#t` and `()` stand for the configuration `style` has, or the default
  /// configuration if `style` does not highlight code; an association list refines it.
  private func syntaxHighlightingConfig(
                 _ entry: Expr,
                 _ value: Expr,
                 style: AttributedStringGenerator.Style) throws
                 -> AttributedStringGenerator.SyntaxHighlightingConfig? {
    let hl = style.syntaxHighlighting ?? AttributedStringGenerator.SyntaxHighlightingConfig.default
    switch value {
      case .false:
        return nil
      case .true, .null:
        return hl
      default:
        let entries = try self.configEntries(value,
                                             allowed: MarkdownLibrary.syntaxHighlightingKeys)
        return .init(
          theme: try self.configString(entries, "theme") ?? hl.theme,
          ignoreSyntacticIssues: try self.configBool(entries, "ignore-syntax-errors") ??
                                 hl.ignoreSyntacticIssues,
          ignoredLanguages: Set(try self.configStrings(entries, "ignored-languages") ??
                                Array(hl.ignoredLanguages)),
          highlightIndentedCodeBlocks: try self.configBool(entries,
                                                           "highlight-indented-code-blocks") ??
                                       hl.highlightIndentedCodeBlocks)
    }
  }
  
  /// Returns a positive number (or a non-negative one if `zero` is true) for `key` of `entries`.
  private func configLength(_ entries: ConfigEntries, _ key: String, zero: Bool = false) throws -> CGFloat? {
    guard let x = try self.configDouble(entries, key) else {
      return nil
    }
    guard x > 0.0 || (zero && x == 0.0) else {
      throw RuntimeError.eval(.notAValidMarkdownConfig, entries[key]!.entry)
    }
    return CGFloat(x)
  }
  
  /// Maps the value of the entry `mermaid` of a configuration to the configuration of the
  /// renderer of Mermaid diagrams. `#f` stands for no diagrams, `#t` and `()` for the default
  /// configuration, and an association list with the keys in `MarkdownLibrary.mermaidKeys`
  /// refines the default configuration.
  private func mermaidConfig(_ value: Expr) throws -> MermaidConfig? {
    switch value {
      case .false:
        return nil
      case .true, .null:
        return MermaidConfig.default
      default:
        let entries = try self.configEntries(value, allowed: MarkdownLibrary.mermaidKeys)
        var config = MermaidConfig.default
        if let name = try self.configSymbol(entries, "theme",
                                            in: ["default", "neutral", "dark", "forest", "base"]),
           let theme = MermaidConfig.Theme(rawValue: name) {
          config.theme = theme
        }
        if let (entry, value) = entries["theme-variables"], !value.isFalse {
          var variables: [String : String] = [:]
          var list = value
          while case .pair(.pair(let key, .string(let str)), let rest) = list {
            switch key {
              case .symbol(let sym):
                variables[sym.identifier] = str as String
              case .string(let name):
                variables[name as String] = str as String
              default:
                throw RuntimeError.eval(.notAValidMarkdownConfig, entry)
            }
            list = rest
          }
          guard list.isNull else {
            throw RuntimeError.eval(.notAValidMarkdownConfig, entry)
          }
          config.themeVariables = variables
        }
        if let fontFamily = try self.configString(entries, "font-family") {
          config.fontFamily = fontFamily
        }
        if let scale = try self.configLength(entries, "scale") {
          config.scale = scale
        }
        if let width = try self.configLength(entries, "width") {
          config.width = width
        }
        if let (_, margin) = entries["margin"] {
          // `#f` keeps the margins that Mermaid adds around its diagrams
          config.margin = margin.isFalse ? nil : try self.configLength(entries, "margin", zero: true)
        }
        if let maxWidth = try self.configLength(entries, "max-width") {
          config.maxWidth = maxWidth
        }
        if let spacing = try self.configLength(entries, "spacing", zero: true) {
          config.spacing = spacing
        }
        if let timeout = try self.configLength(entries, "timeout") {
          config.timeout = TimeInterval(timeout)
        }
        return config
    }
  }
  
  /// Maps the value of the entry `math` of a configuration to the configuration of the
  /// renderer of formulas. `#f` stands for formulas shown as code, `#t` and `()` for the default
  /// configuration, and an association list refines the default configuration (see
  /// `ImageGenLibraryAddOn.mathConfig`).
  private func mathConfig(_ value: Expr) throws -> MathConfig? {
    switch value {
      case .false:
        return nil
      case .true, .null:
        return MathConfig()
      default:
        return try ImageGenLibraryAddOn.mathConfig(value, error: .notAValidMarkdownConfig)
    }
  }
  
  /// Creates an attributed string generator from a configuration, i.e. an association list with
  /// symbol keys (see `MarkdownLibrary.configKeys`). The first entry for a key is used. All
  /// entries are optional; `#f` and `()` stand for the default configuration.
  ///
  /// Diagrams (entry `mermaid`) and formulas (entry `math`, enabled by default) are only
  /// enabled if `diagrams` is true; they are drawn when rendering attributed strings, but cannot
  /// be included in HTML documents.
  private func attributedStringGenerator(config: Expr?,
                                         diagrams: Bool) throws -> AttributedStringGenerator {
    let entries = try self.configEntries(config, allowed: MarkdownLibrary.configKeys)
    // Appearance: a preset, which the entries of the configuration refine
    var style: AttributedStringGenerator.Style
    switch try self.configSymbol(entries, "style", in: ["default", "light", "dark"]) {
      case .some("default"):
        style = .default
      case .some("dark"):
        style = .dark
      default:
        // Preset `light` is used if there is no preset
        style = .light
    }
    func float(_ key: String) throws -> Float? {
      return try self.configDouble(entries, key).map { Float($0) }
    }
    if let size = try float("font-size") {
      style.fontSize = size
    }
    if let family = try self.configString(entries, "font-family") {
      style.fontFamily = family
    }
    if let color = try self.configString(entries, "font-color") {
      style.fontColor = color
    }
    if let spacing = try float("line-spacing") {
      style.lineSpacing = spacing
    }
    if let size = try float("code-font-size") {
      style.codeFontSize = size
    }
    if let family = try self.configString(entries, "code-font-family") {
      style.codeFontFamily = family
    }
    if let color = try self.configString(entries, "code-font-color") {
      style.codeFontColor = color
    }
    if let size = try float("code-block-font-size") {
      style.codeBlockFontSize = size
    }
    if let color = try self.configString(entries, "code-block-font-color") {
      style.codeBlockFontColor = color
    }
    if let spacing = try float("code-block-line-spacing") {
      style.codeBlockLineSpacing = spacing
    }
    if let color = try self.configString(entries, "code-block-background") {
      style.codeBlockBackground = color
    }
    if let color = try self.configString(entries, "border-color") {
      style.borderColor = color
    }
    if let color = try self.configString(entries, "blockquote-color") {
      style.blockquoteColor = color
    }
    if let font = try self.configString(entries, "header-font") {
      style.headerFont = font
    }
    if let color = try self.configString(entries, "h1-color") {
      style.h1Color = color
    }
    if let size = try float("h1-font-size") {
      style.h1FontSize = size
    }
    if let color = try self.configString(entries, "h2-color") {
      style.h2Color = color
    }
    if let size = try float("h2-font-size") {
      style.h2FontSize = size
    }
    if let color = try self.configString(entries, "h3-color") {
      style.h3Color = color
    }
    if let size = try float("h3-font-size") {
      style.h3FontSize = size
    }
    if let color = try self.configString(entries, "h4-color") {
      style.h4Color = color
    }
    if let size = try float("h4-font-size") {
      style.h4FontSize = size
    }
    if let (entry, value) = entries["syntax-highlighting"] {
      style.syntaxHighlighting = try self.syntaxHighlightingConfig(entry, value, style: style)
    }
    if let width = try self.configString(entries, "max-image-width") {
      style.maxImageWidth = width
    }
    if let height = try self.configString(entries, "max-image-height") {
      style.maxImageHeight = height
    }
    if let css = try self.configString(entries, "custom-style") {
      style.customStyle = css
    }
    var options: AttributedStringGenerator.Options = []
    if try self.configBool(entries, "tight-lists") ?? false {
      options.insert(.tightLists)
    }
    let alignListMarkers = try self.configBool(entries, "align-list-markers") ?? false
    let imageBaseUrl = try self.configURL(entries, "image-base-url")
    // Mermaid diagrams
    var mermaid: MermaidConfig? = nil
    if let (_, value) = entries["mermaid"] {
      mermaid = try self.mermaidConfig(value)
    }
    // Formulas, which are typeset unless the configuration says otherwise
    var math: MathConfig? = MathConfig()
    if let (_, value) = entries["math"] {
      math = try self.mathConfig(value)
    }
    // Rendering options
    let preset: RenderingOptions
    switch try self.configSymbol(entries, "preset", in: ["trusted", "untrusted"]) {
      case .some("untrusted"):
        preset = .untrusted
      default:
        preset = .trusted
    }
    var timeout = preset.timeout
    if let (entry, value) = entries["timeout"] {
      if value.isFalse {
        timeout = nil
      } else if let seconds = try self.configDouble(entries, "timeout"), seconds > 0.0 {
        timeout = seconds
      } else {
        throw RuntimeError.eval(.notAValidMarkdownConfig, entry)
      }
    }
    var textSizeMultiplier = preset.textSizeMultiplier
    if let multiplier = try self.configDouble(entries, "text-size-multiplier") {
      guard multiplier > 0.0 else {
        throw RuntimeError.eval(.notAValidMarkdownConfig, entries["text-size-multiplier"]!.entry)
      }
      textSizeMultiplier = multiplier
    }
    let renderingOptions = RenderingOptions(
      baseUrl: try self.configURL(entries, "base-url") ?? preset.baseUrl,
      timeout: timeout,
      localImages: try self.configImageAccess(entries, "local-images", remote: false) ??
                   preset.localImages,
      remoteImages: try self.configImageAccess(entries, "remote-images", remote: true) ??
                    preset.remoteImages,
      imageExtensions: try self.configStrings(entries, "image-extensions").map { Set($0) } ??
                       preset.imageExtensions,
      safeMode: try self.configBool(entries, "safe-mode") ?? preset.safeMode,
      textSizeMultiplier: textSizeMultiplier)
    if diagrams, let math {
      return MathAttributedStringGenerator(version: .OS26,
                                           options: options,
                                           style: style,
                                           syntaxHighlighter: self.syntaxHighlighter,
                                           mermaid: mermaid,
                                           mermaidRenderer: self.mermaidRenderer,
                                           math: math,
                                           mathRenderer: self.mathRenderer,
                                           imageBaseUrl: imageBaseUrl,
                                           alignListMarkers: alignListMarkers,
                                           renderingOptions: renderingOptions)
    } else {
      return AttributedStringGenerator(version: .OS26,
                                       options: options,
                                       style: style,
                                       syntaxHighlighter: self.syntaxHighlighter,
                                       mermaid: diagrams ? mermaid : nil,
                                       mermaidRenderer: self.mermaidRenderer,
                                       imageBaseUrl: imageBaseUrl,
                                       alignListMarkers: alignListMarkers,
                                       renderingOptions: renderingOptions)
    }
  }
  
  // MARK: - Converting positional style arguments into configurations
  
  private func configSymbol(_ name: String) -> Expr {
    return .symbol(self.context.symbols.intern(name))
  }
  
  /// Maps a list `(v0 v1 ...)` of up to `keys.count` values, where `#f` stands for "not
  /// specified", to the entries for `keys`. The first value needs to be a number if
  /// `numeric` is true; all other values need to be strings.
  private func legacyEntries(_ expr: Expr,
                             keys: [String],
                             numeric: Bool) throws -> [(String, Expr)] {
    guard !expr.isFalse else {
      return []
    }
    var res: [(String, Expr)] = []
    var list = expr
    var i = 0
    while case .pair(let value, let rest) = list {
      guard i < keys.count else {
        throw RuntimeError.eval(.notAValidMarkdownConfig, expr)
      }
      if !value.isFalse {
        switch value {
          case .fixnum(_), .flonum(_):
            guard numeric && i == 0 else {
              throw RuntimeError.eval(.notAValidMarkdownConfig, expr)
            }
          case .string(_):
            guard !(numeric && i == 0) else {
              throw RuntimeError.eval(.notAValidMarkdownConfig, expr)
            }
          default:
            throw RuntimeError.eval(.notAValidMarkdownConfig, expr)
        }
        res.append((keys[i], value))
      }
      list = rest
      i += 1
    }
    guard list.isNull else {
      throw RuntimeError.eval(.notAValidMarkdownConfig, expr)
    }
    return res
  }
  
  /// Maps the legacy argument for syntax highlighting, a list
  /// `(theme ignore-syntax-errors? ignored-languages highlight-indented-code-blocks?)`, to
  /// configuration entries. In the legacy format, `#f` stands for the default configuration,
  /// whereas `#t` and `()` disable syntax highlighting.
  private func legacySyntaxEntries(_ expr: Expr) throws -> [(String, Expr)] {
    switch expr {
      case .false:
        return []
      case .true, .null:
        return [("syntax-highlighting", .false)]
      case .pair(_, _):
        var res: [(String, Expr)] = []
        var list = expr
        var i = 0
        while case .pair(let value, let rest) = list {
          switch i {
            case 0:
              if !value.isFalse {
                guard case .string(_) = value else {
                  throw RuntimeError.eval(.notAValidMarkdownConfig, expr)
                }
                res.append(("theme", value))
              }
            case 1:
              if !value.isNull {
                res.append(("ignore-syntax-errors", .makeBoolean(value.isTrue)))
              }
            case 2:
              if !value.isFalse {
                var langs = value
                while case .pair(.string(_), let next) = langs {
                  langs = next
                }
                guard langs.isNull else {
                  throw RuntimeError.eval(.notAValidMarkdownConfig, expr)
                }
                res.append(("ignored-languages", value))
              }
            case 3:
              if !value.isNull {
                res.append(("highlight-indented-code-blocks", .makeBoolean(value.isTrue)))
              }
            default:
              throw RuntimeError.eval(.notAValidMarkdownConfig, expr)
          }
          list = rest
          i += 1
        }
        guard list.isNull else {
          throw RuntimeError.eval(.notAValidMarkdownConfig, expr)
        }
        return [("syntax-highlighting",
                 .makeList(Exprs(res.map { .pair(self.configSymbol($0.0), $0.1) })))]
      default:
        throw RuntimeError.eval(.notAValidMarkdownConfig, expr)
    }
  }
  
  /// Implements `markdown-style-config`, which maps the positional arguments for configuring
  /// the style of attributed strings and HTML documents to a configuration.
  private func markdownStyleConfig(_ args: Arguments) throws -> Expr {
    guard args.count <= 5 else {
      throw RuntimeError.argumentCount(min: 0, max: 5, args: .makeList(args))
    }
    let args = Array(args)
    var entries: [(String, Expr)] = []
    if args.count > 0 {
      entries += try self.legacyEntries(args[0],
                                        keys: ["font-size", "font-family", "font-color"],
                                        numeric: true)
    }
    if args.count > 1 {
      entries += try self.legacyEntries(args[1],
                                        keys: ["code-font-size",
                                               "code-font-family",
                                               "code-font-color"],
                                        numeric: true)
    }
    if args.count > 2 {
      entries += try self.legacyEntries(args[2],
                                        keys: ["code-block-font-size",
                                               "code-block-font-color",
                                               "code-block-background"],
                                        numeric: true)
    }
    if args.count > 3 {
      entries += try self.legacyEntries(args[3],
                                        keys: ["border-color", "blockquote-color",
                                               "h1-color", "h2-color", "h3-color", "h4-color"],
                                        numeric: false)
    }
    if args.count > 4 {
      entries += try self.legacySyntaxEntries(args[4])
    }
    return .makeList(Exprs(entries.map { .pair(self.configSymbol($0.0), $0.1) }))
  }
  
  // MARK: - Generating HTML documents and styled text
  
  private func markdownToHtmlDoc(md: Expr, config: Expr?) throws -> Expr {
    let attribStrGen = try self.attributedStringGenerator(config: config, diagrams: false)
    return .makeString(
      attribStrGen.generateHtml(
        attribStrGen.htmlGenerator().generate(doc: try self.internMarkdown(block: md))))
  }
  
  /// The outcome of rendering an attributed string asynchronously.
  private final class RenderingOutcome: @unchecked Sendable {
    private let lock = NSLock()
    private let semaphore = DispatchSemaphore(value: 0)
    private var result: Result<NSAttributedString, AttributedStringGenerator.RenderingError>?
    
    func store(_ result: Result<NSAttributedString, AttributedStringGenerator.RenderingError>) {
      self.lock.lock()
      self.result = result
      self.lock.unlock()
      self.semaphore.signal()
    }
    
    var current: Result<NSAttributedString, AttributedStringGenerator.RenderingError>? {
      self.lock.lock()
      defer { self.lock.unlock() }
      return self.result
    }
    
    func wait(upTo seconds: TimeInterval) {
      _ = self.semaphore.wait(timeout: .now() + seconds)
    }
  }
  
  /// Collects the sources of the Mermaid diagrams in a block, i.e. the content of fenced code
  /// blocks whose info string starts with the word `mermaid`.
  private func collectMermaidSources(in block: Block, into sources: inout Set<String>) {
    switch block {
      case .document(let blocks),
           .blockquote(let blocks),
           .list(_, _, let blocks),
           .listItem(_, _, let blocks):
        for block in blocks {
          self.collectMermaidSources(in: block, into: &sources)
        }
      case .definitionList(let definitions):
        for definition in definitions {
          for block in definition.descriptions {
            self.collectMermaidSources(in: block, into: &sources)
          }
        }
      case .fencedCode(let info, let lines):
        if info?.split(whereSeparator: \.isWhitespace).first == "mermaid" {
          sources.insert(lines.joined())
        }
      default:
        break
    }
  }
  
  /// Renders a Markdown document asynchronously into an attributed string and waits for the
  /// result. The HTML importer delivers its result on the main thread. If this is the main
  /// thread, the run loop gets executed while waiting; otherwise, the calling thread just waits.
  private func renderAttributedString(_ doc: Block,
                                      with generator: AttributedStringGenerator) throws
                                      -> NSAttributedString {
    let outcome = RenderingOutcome()
    generator.generateAsync(doc: doc) { result in
      outcome.store(result)
    }
    // MarkdownKit reports a timeout after `timeout` plus a grace period of five seconds, but it
    // does so on the main thread. If that thread is blocked, e.g. because it waits for this
    // thread, the result never arrives; the deadline makes sure this thread does not wait forever.
    // Diagrams get rendered first, which takes up to the timeout of the diagrams each.
    var diagrams = Set<String>()
    if generator.mermaidConfig != nil {
      self.collectMermaidSources(in: doc, into: &diagrams)
    }
    let diagramTime = Double(diagrams.count) * ((generator.mermaidConfig?.timeout ?? 0.0) + 5.0)
    // Formulas are rendered in one batch, which takes up to the timeout of the formulas (if any)
    let formulaTimeout = (generator as? MathAttributedStringGenerator)?.mathConfig.timeout ?? 0.0
    let formulaTime = formulaTimeout > 0.0 ? formulaTimeout + 5.0 : 0.0
    let deadline = generator.renderingOptions.timeout.map {
      Date(timeIntervalSinceNow: $0 + 10.0 + diagramTime + formulaTime)
    }
    while outcome.current == nil {
      if self.context.evaluator.threads.current?.value.machine?.abortionRequested ?? false {
        throw RuntimeError.abortion()
      }
      if let deadline, Date() > deadline {
        throw RuntimeError.eval(
          .cannotRenderMarkdown,
          .makeString(AttributedStringGenerator.RenderingError.timedOut.localizedDescription))
      }
      if Thread.isMainThread {
        if !RunLoop.current.run(mode: .default, before: Date(timeIntervalSinceNow: 0.05)) {
          Thread.sleep(forTimeInterval: 0.005)
        }
      } else {
        outcome.wait(upTo: 0.05)
      }
    }
    switch outcome.current! {
      case .success(let astr):
        return astr
      case .failure(let error):
        throw RuntimeError.eval(.cannotRenderMarkdown, .makeString(error.localizedDescription))
    }
  }
  
  private func markdownToStyledText(md: Expr, config: Expr?) throws -> Expr {
    let attribStrGen = try self.attributedStringGenerator(config: config, diagrams: true)
    let astr = try self.renderAttributedString(try self.internMarkdown(block: md),
                                               with: attribStrGen)
    return .object(StyledText(NSMutableAttributedString(attributedString: astr)))
  }
  
  private func markdownToHtml(_ md: Expr) throws -> Expr {
    return .makeString(HtmlGenerator.standard.generate(doc: try self.internMarkdown(block: md)))
  }
  
  private func markdownToSxml(_ md: Expr) throws -> Expr {
    let gen = SXMLGenerator(context: self.context)
    return gen.generate(block: try self.internMarkdown(block: md))
  }
  
  private func markdownToDebugString(_ md: Expr) throws -> Expr {
    return .makeString(try self.internMarkdown(block: md).description)
  }
  
  private func markdownToRawString(_ md: Expr) throws -> Expr {
    return .makeString(try self.internMarkdown(block: md).string)
  }
  
  private func blocksToHtml(_ expr: Expr, tight: Expr?) throws -> Expr {
    let tight = tight?.isTrue ?? false
    switch expr {
      case .pair(_ , _):
        return .makeString(HtmlGenerator.standard.generate(
                 blocks: try self.internMarkdown(blocks: expr),
                 parent: .none,
                 tight: tight))
      case .tagged(self.blockType, _):
        return .makeString(HtmlGenerator.standard.generate(
                 block: try self.internMarkdown(block: expr),
                 parent: .none,
                 tight: tight))
      default:
        throw RuntimeError.custom(
          "type error", "blocks->html expects argument of type block or list of block; " +
          "received \(expr)", [])
    }
  }
  
  private func blocksToSxml(_ expr: Expr, tight: Expr?) throws -> Expr {
    let gen = SXMLGenerator(context: self.context)
    let tight = tight?.isTrue ?? false
    switch expr {
      case .pair(_ , _):
        return gen.generate(blocks: try self.internMarkdown(blocks: expr), tight: tight)
      case .tagged(self.blockType, _):
        return gen.generate(block: try self.internMarkdown(block: expr), tight: tight)
      default:
        throw RuntimeError.custom(
          "type error", "blocks->sxml expects argument of type block or list of block; " +
          "received \(expr)", [])
    }
  }
  
  private func blocksToRawString(_ expr: Expr) throws -> Expr {
    switch expr {
      case .pair(_ , _):
        return .makeString(try self.internMarkdown(blocks: expr).string)
      case .tagged(self.blockType, _):
        return .makeString(try self.internMarkdown(block: expr).string)
      default:
        throw RuntimeError.custom(
          "type error", "blocks->raw-string expects argument of type block or list of block; " +
          "received \(expr)", [])
    }
  }
  
  private func textToHtml(_ expr: Expr) throws -> Expr {
    switch expr {
      case .pair(_ , _):
        return .makeString(HtmlGenerator.standard.generate(text:
                             try self.internMarkdown(text: expr)))
      case .tagged(self.inlineType, _):
        return .makeString(HtmlGenerator.standard.generate(textFragment:
                             try self.internMarkdown(fragment: expr)))
      default:
        throw RuntimeError.custom(
          "type error", "text->html expects argument of type inline or list of inline; " +
          "received \(expr)", [])
    }
  }
  
  private func textToSxml(_ expr: Expr) throws -> Expr {
    let gen = SXMLGenerator(context: self.context)
    switch expr {
      case .pair(_ , _):
        return gen.generate(text: try self.internMarkdown(text: expr))
      case .tagged(self.inlineType, _):
        return gen.generate(textFragment: try self.internMarkdown(fragment: expr))
      default:
        throw RuntimeError.custom(
          "type error", "text->sxml expects argument of type inline or list of inline; " +
          "received \(expr)", [])
    }
  }
  
  private func textToString(_ expr: Expr) throws -> Expr {
    switch expr {
      case .pair(_ , _):
        return .makeString(try self.internMarkdown(text: expr).description)
      case .tagged(self.inlineType, _):
        return .makeString(try self.internMarkdown(fragment: expr).description)
      default:
        throw RuntimeError.custom(
          "type error", "inline->string expects argument of type inline or list of inlines; " +
          "received \(expr)", [])
    }
  }

  private func textToRawString(_ expr: Expr) throws -> Expr {
    switch expr {
      case .pair(_ , _):
        return .makeString(try self.internMarkdown(text: expr).rawDescription)
      case .tagged(self.inlineType, _):
        return .makeString(try self.internMarkdown(fragment: expr).rawDescription)
      default:
        throw RuntimeError.custom(
          "type error", "inline->raw-string expects argument of type inline or list of inlines; " +
          "received \(expr)", [])
    }
  }

  private func internMarkdown(blocks expr: Expr) throws -> Blocks {
    var list = expr
    var blocks = Blocks()
    while case .pair(let block, let rest) = list {
      blocks.append(try self.internMarkdown(block: block))
      list = rest
    }
    guard case .null = list else {
      throw RuntimeError.custom("type error",
                                "markdown block lists must be proper lists: \(expr)", [])
    }
    return blocks
  }

  private func internMarkdown(block expr: Expr) throws -> Block {
    guard case .tagged(self.blockType, .pair(.symbol(let sym), let args)) = expr else {
      throw RuntimeError.custom("type error",
                                "expected value of type block; received \(expr)",
                                [])
    }
    switch sym {
      case self.document:
        if case .pair(let fst, .null) = args {
          return .document(try self.internMarkdown(blocks: fst))
        }
      case self.blockquote:
        if case .pair(let fst, .null) = args {
          return .blockquote(try self.internMarkdown(blocks: fst))
        }
      case self.listItems:
        if case .pair(let start, .pair(let tight, .pair(let items, .null))) = args {
          return .list(start.isFalse ? nil : try start.asInt(),
                       tight.isTrue,
                       try self.internMarkdown(items: items))
        }
      case self.paragraph:
        if case .pair(let fst, .null) = args {
          return .paragraph(try self.internMarkdown(text: fst))
        }
      case self.heading:
        if case .pair(let level, .pair(let text, .null)) = args {
          return .heading(try level.asInt(), try self.internMarkdown(text: text))
        }
      case self.indentedCode:
        if case .pair(let lines, .null) = args {
          return .indentedCode(try self.internMarkdown(lines: lines))
        }
      case self.fencedCode:
        if case .pair(let lang, .pair(let lines, .null)) = args {
          return .fencedCode(lang.isFalse ? nil : try lang.asString(),
                             try self.internMarkdown(lines: lines))
        }
      case self.htmlBlock:
        if case .pair(let lines, .null) = args {
          return .htmlBlock(try self.internMarkdown(lines: lines))
        }
      case self.referenceDef:
        if case .pair(let label, .pair(let dest, .pair(let title, .null))) = args {
          return .referenceDef(try label.asString() ,
                               Substring(try dest.asString()),
                               try self.internMarkdown(lines: title))
        }
      case self.thematicBreak:
        if case .null = args {
          return .thematicBreak
        }
      case self.table:
        if case .pair(let header, .pair(let alignments, .pair(let rows, .null))) = args {
          return .table(try self.internMarkdown(row: header),
                        try self.internMarkdown(alignments: alignments),
                        try self.internMarkdown(rows: rows))
        }
      case self.definitionList:
        if case .pair(let definitions, .null) = args {
          return .definitionList(try self.internMarkdown(definitions: definitions))
        }
      case self.mathBlock:
        if case .pair(let tex, .null) = args {
          return .custom(MathBlock(tex: try tex.asString()))
        }
      default:
        break
    }
    throw RuntimeError.custom("eval error", "malformed expression of type block: \(expr)", [])
  }

  private func internMarkdown(items expr: Expr) throws -> Blocks {
    var list = expr
    var items = Blocks()
    while case .pair(let item, let rest) = list {
      items.append(try self.internMarkdown(item: item))
      list = rest
    }
    guard case .null = list else {
      throw RuntimeError.custom("type error",
                                "markdown list item lists must be proper lists \(expr)", [])
    }
    return items
  }

  private func internMarkdown(item expr: Expr) throws -> Block {
    guard case .tagged(self.listItemType, .pair(.symbol(let sym), let args)) = expr else {
      throw RuntimeError.eval(.notAValidListItem, expr)
    }
    switch sym {
      case self.bullet:
        if case .pair(let char, .pair(let tight, .pair(let blocks, .null))) = args {
          return .listItem(.bullet(Character(unicodeScalar(try char.asUniChar()))),
                           tight.isTrue ? (tight.isNull ? .initial : .tight) : .loose,
                           try self.internMarkdown(blocks: blocks))
        }
      case self.ordered:
        if case .pair(let start, .pair(let char,
                                       .pair(let tight, .pair(let blocks, .null)))) = args {
          return .listItem(.ordered(try start.asInt(),
                                    Character(unicodeScalar(try char.asUniChar()))),
                           tight.isTrue ? (tight.isNull ? .initial : .tight) : .loose,
                           try self.internMarkdown(blocks: blocks))
        }
      case self.task:
        if case .pair(let checked, .pair(let item, .null)) = args,
           case .listItem(let type, let density, let blocks) = try self.internMarkdown(item: item),
           !type.isTask {
          return .listItem(.task(type, checked: checked.isTrue), density, blocks)
        }
      default:
        break
    }
    throw RuntimeError.eval(.notAValidListItem, expr)
  }
  
  private func internMarkdown(definitions expr: Expr) throws -> Definitions {
    var list = expr
    var definitions = Definitions()
    while case .pair(let def, let rest) = list {
      definitions.append(try self.internMarkdown(definition: def))
      list = rest
    }
    guard case .null = list else {
      throw RuntimeError.custom("type error",
                                "list of markdown definitions must be a proper list: \(expr)", [])
    }
    return definitions
  }
  
  private func internMarkdown(definition expr: Expr) throws -> MarkdownKit.Definition {
    guard case .pair(let item, let blocks) = expr else {
      throw RuntimeError.custom("type error", "invalid markdown definition: \(expr)", [])
    }
    return MarkdownKit.Definition(item: try self.internMarkdown(text: item),
                                  descriptions: try self.internMarkdown(items: blocks))
  }
  
  private func internMarkdown(alignments expr: Expr) throws -> Alignments {
    var list = expr
    var alignments = Alignments()
    while case .pair(let alignment, let rest) = list {
      if case .symbol(let sym) = alignment {
        switch sym {
          case self.left:
            alignments.append(.left)
          case self.right:
            alignments.append(.right)
          case self.center:
            alignments.append(.center)
          default:
            alignments.append(.undefined)
        }
      } else {
        alignments.append(.undefined)
      }
      list = rest
    }
    guard case .null = list else {
      throw RuntimeError.custom("type error",
                                "markdown table row must be a proper list: \(expr)", [])
    }
    return alignments
  }
  
  private func internMarkdown(rows expr: Expr) throws -> Rows {
    var list = expr
    var rows = Rows()
    while case .pair(let row, let rest) = list {
      rows.append(try self.internMarkdown(row: row))
      list = rest
    }
    guard case .null = list else {
      throw RuntimeError.custom("type error",
                                "list of markdown table rows must be a proper list: \(expr)", [])
    }
    return rows
  }
  
  private func internMarkdown(row expr: Expr) throws -> Row {
    var list = expr
    var row = Row()
    while case .pair(let cell, let rest) = list {
      row.append(try self.internMarkdown(text: cell))
      list = rest
    }
    guard case .null = list else {
      throw RuntimeError.custom("type error",
                                "markdown table row must be a proper list: \(expr)", [])
    }
    return row
  }

  private func internMarkdown(text expr: Expr) throws -> Text {
    var list = expr
    var text = Text()
    while case .pair(let fragment, let rest) = list {
      text.append(fragment: try self.internMarkdown(fragment: fragment))
      list = rest
    }
    guard case .null = list else {
      throw RuntimeError.custom("type error",
                                "markdown inline text lists must be proper lists: \(expr)", [])
    }
    return text
  }

  private func internMarkdown(fragment expr: Expr) throws -> TextFragment {
    guard case .tagged(self.inlineType,
                       .pair(.symbol(let sym), .pair(let fst, let args))) = expr else {
      throw RuntimeError.custom("type error",
                                "expected value of type inline; received \(expr)",
                                [])
    }
    switch sym {
      case self.text:
        return .text(Substring(try fst.asString()))
      case self.code:
        return .code(Substring(try fst.asString()))
      case self.emph:
        return .emph(try self.internMarkdown(text: fst))
      case self.strong:
        return .strong(try self.internMarkdown(text: fst))
      case self.underline:
        return .underline(try self.internMarkdown(text: fst))
      case self.strikethrough:
        return .strikethrough(try self.internMarkdown(text: fst))
      case self.link:
        if case .pair(let uri, .pair(let title, .null)) = args {
          return .link(try self.internMarkdown(text: fst),
                       uri.isFalse ? nil : try uri.asString(),
                       title.isFalse ? nil : try title.asString())
        }
      case self.autoLink:
        return .autolink(.uri, Substring(try fst.asString()))
      case self.emailAutoLink:
        return .autolink(.email, Substring(try fst.asString()))
      case self.image:
        if case .pair(let uri, let title) = args {
          switch title {
            case .false, .null, .pair(.false, _), .pair(.null, _):
              return .image(try self.internMarkdown(text: fst),
                            uri.isFalse ? nil : try uri.asString(),
                            nil)
            case .string(let str), .pair(.string(let str), _):
              return .image(try self.internMarkdown(text: fst),
                            uri.isFalse ? nil : try uri.asString(),
                            str as String)
            default:
              break
          }
        }
      case self.html:
        return .html(Substring(try fst.asString()))
      case self.math:
        if case .pair(let display, .pair(let source, .null)) = args {
          return .custom(MathFragment(tex: try fst.asString(),
                                      display: display.isTrue,
                                      source: source.isFalse ? nil : try source.asString()))
        }
      case self.lineBreak:
        return fst.isFalse ? .softLineBreak : .hardLineBreak
      default:
        break
    }
    throw RuntimeError.custom("eval error", "malformed expression of type inline: \(expr)", [])
  }

  private func internMarkdown(lines expr: Expr) throws -> Lines {
    var list = expr
    var lines = Lines()
    while case .pair(let str, let rest) = list {
      lines.append(Substring(try str.asString()))
      list = rest
    }
    guard case .null = list else {
      throw RuntimeError.custom("type error",
                                "markdown string lists must be proper lists: \(expr)", [])
    }
    return lines
  }

  private func externMarkdown(_ blocks: Blocks) -> Expr {
    var exprs: Exprs = []
    for block in blocks {
      if let expr = self.externMarkdown(block) {
        exprs.append(expr)
      }
    }
    return .makeList(exprs)
  }
  
  private func externDensity(_ density: ListDensity) -> Expr {
    switch density {
      case .initial:
        return .null
      case .loose:
        return .false
      case .tight:
        return .true
    }
  }

  private func externMarkdown(_ block: Block) -> Expr? {
    switch block {
      case .document(let blocks):
        return self.makeCase(self.blockType,
                             self.document,
                             self.externMarkdown(blocks))
      case .blockquote(let blocks):
        return self.makeCase(self.blockType,
                             self.blockquote,
                             self.externMarkdown(blocks))
      case .list(let start, let tight, let blocks):
        if let start = start {
          return self.makeCase(self.blockType,
                               self.listItems,
                               .makeNumber(start),
                               .makeBoolean(tight),
                               self.externMarkdown(blocks))
        } else {
          return self.makeCase(self.blockType,
                              self.listItems,
                              .false,
                              .makeBoolean(tight),
                              self.externMarkdown(blocks))
        }
      case .listItem(let type, let tight, let blocks):
        return self.externMarkdown(type, tight, blocks)
      case .paragraph(let text):
        return self.makeCase(self.blockType,
                             self.paragraph,
                             self.externMarkdown(text))
      case .heading(let level, let text):
        return self.makeCase(self.blockType,
                             self.heading,
                             .makeNumber(level),
                             self.externMarkdown(text))
      case .indentedCode(let lines):
        return self.makeCase(self.blockType,
                             self.indentedCode,
                             self.externMarkdown(lines))
      case .fencedCode(let lang, let lines):
        return self.makeCase(self.blockType,
                             self.fencedCode,
                             self.externMarkdown(lang),
                             self.externMarkdown(lines))
      case .htmlBlock(let lines):
        return self.makeCase(self.blockType,
                             self.htmlBlock,
                             self.externMarkdown(lines))
      case .referenceDef(let label, let dest, let title):
        return self.makeCase(self.blockType,
                             self.referenceDef,
                             self.externMarkdown(label),
                             self.externMarkdown(String(dest)),
                             self.externMarkdown(title))
      case .thematicBreak:
        return self.makeCase(self.blockType, self.thematicBreak)
      case .table(let header, let alignments, let rows):
        return self.makeCase(self.blockType,
                             self.table,
                             self.externMarkdown(header),
                             self.externMarkdown(alignments),
                             self.externMarkdown(rows))
      case .definitionList(let definitions):
        return self.makeCase(self.blockType,
                             self.definitionList,
                             self.externMarkdown(definitions))
      case .custom(let custom):
        guard let math = custom as? MathBlock else {
          return nil
        }
        return self.makeCase(self.blockType, self.mathBlock, .makeString(math.tex))
    }
  }
  
  private func externMarkdown(_ type: ListType,
                              _ density: ListDensity,
                              _ blocks: Blocks) -> Expr {
    switch type {
      case .bullet(let ch):
        return self.makeCase(self.listItemType,
                             self.bullet,
                             .char(ch.utf16.first!),
                             self.externDensity(density),
                             self.externMarkdown(blocks))
      case .ordered(let n, let ch):
        return self.makeCase(self.listItemType,
                             self.ordered,
                             .makeNumber(n),
                             .char(ch.utf16.first!),
                             self.externDensity(density),
                             self.externMarkdown(blocks))
      case .task(let marker, let checked):
        return self.makeCase(self.listItemType,
                             self.task,
                             .makeBoolean(checked),
                             self.externMarkdown(marker.marker, density, blocks))
    }
  }

  private func externMarkdown(_ definitions: Definitions) -> Expr {
    var exprs: Exprs = []
    for definition in definitions {
      exprs.append(self.externMarkdown(definition))
    }
    return .makeList(exprs)
  }
  
  private func externMarkdown(_ definition: MarkdownKit.Definition) -> Expr {
    return .pair(self.externMarkdown(definition.item),
                 self.externMarkdown(definition.descriptions))
  }
  
  private func externMarkdown(_ alignments: Alignments) -> Expr {
    var exprs: Exprs = []
    for alignment in alignments {
      switch alignment {
        case .left:
          exprs.append(.symbol(self.left))
        case .right:
          exprs.append(.symbol(self.right))
        case .center:
          exprs.append(.symbol(self.center))
        case .undefined:
          exprs.append(.false)
      }
    }
    return .makeList(exprs)
  }
  
  private func externMarkdown(_ rows: Rows) -> Expr {
    var exprs: Exprs = []
    for row in rows {
      exprs.append(self.externMarkdown(row))
    }
    return .makeList(exprs)
  }
  
  private func externMarkdown(_ row: Row) -> Expr {
    var exprs: Exprs = []
    for cell in row {
      exprs.append(self.externMarkdown(cell))
    }
    return .makeList(exprs)
  }
  
  private func externMarkdown(_ text: Text) -> Expr {
    var exprs: Exprs = []
    for fragment in text {
      if let expr = self.externMarkdown(fragment) {
        exprs.append(expr)
      }
    }
    return .makeList(exprs)
  }

  private func externMarkdown(_ fragment: TextFragment) -> Expr? {
    switch fragment {
      case .text(let str):
        return self.makeCase(self.inlineType, self.text, .makeString(String(str)))
      case .code(let str):
        return self.makeCase(self.inlineType, self.code, .makeString(String(str)))
      case .emph(let text):
        return self.makeCase(self.inlineType, self.emph, self.externMarkdown(text))
      case .strong(let text):
        return self.makeCase(self.inlineType, self.strong, self.externMarkdown(text))
      case .underline(let text):
        return self.makeCase(self.inlineType, self.underline, self.externMarkdown(text))
      case .strikethrough(let text):
        return self.makeCase(self.inlineType, self.strikethrough, self.externMarkdown(text))
      case .link(let text, let uri, let title):
        return self.makeCase(self.inlineType,
                             self.link,
                             self.externMarkdown(text),
                             self.externMarkdown(uri),
                             self.externMarkdown(title))
      case .autolink(.uri, let uri):
        return self.makeCase(self.inlineType, self.autoLink, .makeString(String(uri)))
      case .autolink(.email, let email):
        return self.makeCase(self.inlineType, self.emailAutoLink, .makeString(String(email)))
      case .image(let text, let uri, let title):
        return self.makeCase(self.inlineType,
                             self.image,
                             self.externMarkdown(text),
                             self.externMarkdown(uri),
                             self.externMarkdown(title))
      case .html(let str):
        return self.makeCase(self.inlineType, self.html, .makeString(String(str)))
      case .delimiter(_, _, _):
        return self.makeCase(self.inlineType, self.text, .makeString(fragment.description))
      case .softLineBreak:
        return self.makeCase(self.inlineType, self.lineBreak, .false)
      case .hardLineBreak:
        return self.makeCase(self.inlineType, self.lineBreak, .true)
      case .custom(let custom):
        guard let math = custom as? MathFragment else {
          return nil
        }
        return self.makeCase(self.inlineType,
                             self.math,
                             .makeString(math.tex),
                             .makeBoolean(math.display),
                             .makeString(math.source))
    }
  }
  
  private func externMarkdown(_ lines: Lines) -> Expr {
    var exprs: Exprs = []
    for line in lines {
      exprs.append(.makeString(String(line)))
    }
    return .makeList(exprs)
  }

  private func externMarkdown(_ str: String?) -> Expr {
    if let str = str {
      return .makeString(str)
    } else {
      return .false
    }
  }
  
  private func syntaxHighlightingTheme(expr: Expr) throws -> Expr {
    let nameOrContent = try expr.asString()
    // Try to load the theme
    if let highlighter = self.syntaxHighlighter,
       let themeDirectoryURL = highlighter.themeDirectoryURL {
      let themeURL = themeDirectoryURL.appendingPathComponent(nameOrContent)
                                      .appendingPathExtension("css")
      if let content = try? String(contentsOf: themeURL) {
        return .makeString(content)
      }
    }
    // If we couldn't load a theme file, check if the input itself is valid CSS
    return SyntaxHighlighter.isValidCSS(nameOrContent) ? expr : .false
  }
  
  private func syntaxHighlightingThemes() -> Expr {
    if let highlighter = self.syntaxHighlighter {
      var res = Expr.null
      for theme in highlighter.availableThemes {
        res = .pair(.makeString(theme), res)
      }
      return res
    } else {
      return .false
    }
  }
}

open class SXMLGenerator {
  var context: Context
  
  public init(context: Context) {
    self.context = context
  }
  
  private func symbol(_ str: String) -> Expr {
    return .symbol(self.context.symbols.intern(str))
  }
  
  private func attribs(_ attribs: [String : Expr]) -> Expr {
    var res = Expr.null
    for (key, value) in attribs {
      res = .pair(.pair(self.symbol(key), .pair(value, .null)), res)
    }
    return .pair(self.symbol("@"), res)
  }
  
  /// Mirrors `HtmlGenerator.generate(taskCheckbox:)`: `<input checked="" disabled="" type="checkbox">`
  open func checkbox(checked: Bool) -> Expr {
    var attribs = Expr.null
    attribs = .pair(.pair(self.symbol("type"), .pair(.makeString("checkbox"), .null)), attribs)
    attribs = .pair(.pair(self.symbol("disabled"), .pair(.makeString(""), .null)), attribs)
    if checked {
      attribs = .pair(.pair(self.symbol("checked"), .pair(.makeString(""), .null)), attribs)
    }
    return .pair(self.symbol("input"), .pair(.pair(self.symbol("@"), attribs), .null))
  }

  open func generate(blocks: Blocks, tight: Bool = false) -> Expr {
    var res = Expr.null
    for block in blocks.reversed() {
      res = .pair(self.generate(block: block, tight: tight), res)
    }
    return res
  }

  open func generate(block: Block, tight: Bool = false) -> Expr {
    switch block {
      case .document(let blocks):
        return self.generate(blocks: blocks)
      case .blockquote(let blocks):
        return .pair(self.symbol("blockquote"), self.generate(blocks: blocks))
      case .list(let start, let tight, let blocks):
        if let startNumber = start {
          return .pair(self.symbol("ol"),
                       .pair(self.attribs(["start" : .makeNumber(startNumber)]),
                             self.generate(blocks: blocks, tight: tight)))
        } else {
          return .pair(self.symbol("ul"), self.generate(blocks: blocks, tight: tight))
        }
      case .listItem(let type, _, let blocks):
        // The checkbox of a task list item is the first thing in its first paragraph
        let checkbox: Exprs = type.checked.map { [self.checkbox(checked: $0), .makeString(" ")] } ?? []
        if tight, let text = blocks.text {
          return .pair(self.symbol("li"),
                       checkbox.reversed().reduce(self.generate(text: text)) { .pair($1, $0) })
        } else if !checkbox.isEmpty, case .paragraph(let text) = blocks.first {
          let first = Expr.pair(self.symbol("p"),
                                checkbox.reversed().reduce(self.generate(text: text)) { .pair($1, $0) })
          return .pair(self.symbol("li"),
                       .pair(first, self.generate(blocks: Blocks(blocks.dropFirst()))))
        } else {
          return .pair(self.symbol("li"),
                       checkbox.reversed().reduce(self.generate(blocks: blocks)) { .pair($1, $0) })
        }
      case .paragraph(let text):
        return .pair(self.symbol("p"), self.generate(text: text))
      case .heading(let n, let text):
        let tag = "h\(n > 0 && n < 7 ? n : 1)"
        return .pair(self.symbol(tag), self.generate(text: text))
      case .indentedCode(let lines):
        return .pair(self.symbol("pre"),
                     .pair(.pair(self.symbol("code"),
                                 .pair(.pair(self.symbol("@raw"),
                                             .pair(.makeString(self.generate(lines: lines).encodingPredefinedXmlEntities()),
                                                   .null)),
                                       .null)),
                           .null))
      case .fencedCode(let lang, let lines):
        if let language = lang {
          return .pair(self.symbol("pre"),
                       .pair(.pair(self.symbol("code"),
                                   .pair(.pair(self.symbol("@"),
                                               .pair(.pair(self.symbol("class"),
                                                           .pair(.makeString(language), .null)),
                                                     .null)),
                                         .pair(.pair(self.symbol("@raw"),
                                                     .pair(.makeString(self.generate(lines: lines, separator: "").encodingPredefinedXmlEntities()),
                                                           .null)),
                                               .null))),
                             .null))
        } else {
          return .pair(self.symbol("pre"),
                       .pair(.pair(self.symbol("code"),
                                   .pair(.pair(self.symbol("@raw"),
                                               .pair(.makeString(self.generate(lines: lines, separator: "").encodingPredefinedXmlEntities()),
                                                     .null)),
                                         .null)),
                             .null))
        }
      case .htmlBlock(let lines):
        return .pair(self.symbol("@raw"),
                     .pair(.makeString(self.generate(lines: lines)),
                           .null))
      case .referenceDef(_, _, _):
        return .null
      case .thematicBreak:
        return .pair(self.symbol("hr"), .null)
      case .table(let header, let align, let rows):
        var tagsuffix: [[String : Expr]] = []
        for a in align {
          switch a {
            case .undefined:
              tagsuffix.append([:])
            case .left:
              tagsuffix.append(["align" : .makeString("left")])
            case .right:
              tagsuffix.append(["align" : .makeString("right")])
            case .center:
              tagsuffix.append(["align" : .makeString("center")])
          }
        }
        var hd: Exprs = []
        var i = 0
        for head in header {
          if i < tagsuffix.count {
            hd.append(.pair(self.symbol("th"), .pair(self.attribs(tagsuffix[i]), self.generate(text: head))))
          } else {
            hd.append(.pair(self.symbol("th"), self.generate(text: head)))
          }
          i += 1
        }
        var bd: Exprs = []
        for row in rows {
          var r: Exprs = []
          i = 0
          for cell in row {
            if i < tagsuffix.count {
              r.append(.pair(self.symbol("td"), .pair(self.attribs(tagsuffix[i]), self.generate(text: cell))))
            } else {
              r.append(.pair(self.symbol("td"), self.generate(text: cell)))
            }
            i += 1
          }
          bd.append(.pair(self.symbol("tr"), .makeList(r)))
        }
        return .pair(self.symbol("table"),
                     .pair(.pair(self.symbol("thead"),
                                 .pair(.pair(self.symbol("tr"), .makeList(hd)), .null)),
                           .pair(.pair(self.symbol("tbody"), .makeList(bd)),
                                 .null)))
      case .definitionList(let defs):
        var ds = Expr.null
        for def in defs.reversed() {
          for descr in def.descriptions.reversed() {
            if case .listItem(_, _, let blocks) = descr {
              if blocks.count == 1,
                 case .paragraph(let text) = blocks.first! {
                ds = .pair(.pair(self.symbol("dd"), self.generate(text: text)), ds)
              } else {
                ds = .pair(.pair(self.symbol("dd"), self.generate(blocks: blocks)), ds)
              }
            }
          }
          ds = .pair(.pair(self.symbol("dt"), self.generate(text: def.item)), ds)
        }
        return .pair(self.symbol("dl"), ds)
      case .custom(let customBlock):
        return .pair(self.symbol("@raw"),
                     .pair(.makeString(customBlock.generateHtml(via: HtmlGenerator.standard, tight: tight)),
                           .null))
    }
  }

  open func generate(text: Text) -> Expr {
    var res = Expr.null
    for fragment in text.reversed() {
      res = .pair(self.generate(textFragment: fragment), res)
    }
    return res
  }

  open func generate(textFragment fragment: TextFragment) -> Expr {
    switch fragment {
      case .text(let str):
        return .makeString(String(str).decodingNamedCharacters())
      case .code(let str):
        return .pair(self.symbol("code"), .pair(.makeString(String(str)), .null))
      case .emph(let text):
        return .pair(self.symbol("em"), .pair(self.generate(text: text), .null))
      case .strong(let text):
        return .pair(self.symbol("strong"), .pair(self.generate(text: text), .null))
      case .underline(let text):
        return .pair(self.symbol("u"), .pair(self.generate(text: text), .null))
      case .strikethrough(let text):
        return .pair(self.symbol("del"), .pair(self.generate(text: text), .null))
      case .link(let text, let uri, let title):
        let titleAttr: Expr = title == nil ? .null : .pair(.pair(self.symbol("title"),
                                                                 .pair(.makeString(title!), .null)),
                                                           .null)
        return .pair(self.symbol("a"),
                     .pair(.pair(self.symbol("@"),
                                 .pair(.pair(self.symbol("href"),
                                             .pair(.makeString(uri ?? ""), .null)),
                                       titleAttr)),
                           .pair(self.generate(text: text), .null)))
      case .autolink(let type, let str):
        switch type {
          case .uri:
            return .pair(self.symbol("a"),
                         .pair(.pair(self.symbol("@"),
                                     .pair(.pair(self.symbol("href"),
                                                 .pair(.makeString(String(str)), .null)),
                                           .null)),
                               .pair(.makeString(String(str)), .null)))
          case .email:
            return .pair(self.symbol("a"),
                         .pair(.pair(self.symbol("@"),
                                     .pair(.pair(self.symbol("href"),
                                                 .pair(.makeString("mailto:" + str), .null)),
                                           .null)),
                               .pair(.makeString(String(str)), .null)))
        }
      case .image(let text, let uri, let title):
        if let uri = uri {
          var attr: [String : Expr]  = [
            "src" : .makeString(uri),
            "alt" : .makeString(text.rawDescription)
          ]
          if let title {
            attr["title"] = .makeString(title)
          }
          return .pair(self.symbol("img"), .pair(self.attribs(attr), .null))
        } else {
          return self.generate(text: text)
        }
      case .html(let tag):
        return .pair(self.symbol("@raw"), .pair(.makeString("<\(tag.description)>"), .null))
      case .delimiter(let ch, let n, _):
        var res = String(ch)
        for _ in 1..<n {
          res.append(ch)
        }
        return .makeString(res)
      case .softLineBreak:
        return .makeString("\n")
      case .hardLineBreak:
        return .pair(self.symbol("br"), .null)
      case .custom(let customTextFragment):
        return .pair(self.symbol("@raw"),
                     .pair(.makeString(customTextFragment.generateHtml(via: HtmlGenerator.standard)),
                           .null))
    }
  }

  open func generate(lines: Lines, separator: String = "\n") -> String {
    var res = ""
    for line in lines {
      if res.isEmpty {
        res = String(line)
      } else {
        res += separator + line
      }
    }
    return res
  }
}
