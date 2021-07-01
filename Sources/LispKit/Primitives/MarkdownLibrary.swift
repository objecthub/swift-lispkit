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

public final class MarkdownLibrary: NativeLibrary {

  public let blockType: Tuple
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
  private let table: Symbol
  private let definitionList: Symbol

  public let listItemType: Tuple
  private let bullet: Symbol
  private let ordered: Symbol

  public let inlineType: Tuple
  private let text: Symbol
  private let code: Symbol
  private let emph: Symbol
  private let strong: Symbol
  private let link: Symbol
  private let autoLink: Symbol
  private let emailAutoLink: Symbol
  private let image: Symbol
  private let html: Symbol
  private let lineBreak: Symbol
  
  private let left: Symbol
  private let right: Symbol
  private let center: Symbol
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    self.blockType = Tuple(.symbol(context.symbols.intern("block")), .null)
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
    self.table = context.symbols.intern("table")
    self.definitionList = context.symbols.intern("definition-list")
    self.listItemType = Tuple(.symbol(context.symbols.intern("list-item")), .null)
    self.bullet = context.symbols.intern("bullet")
    self.ordered = context.symbols.intern("ordered")
    self.inlineType = Tuple(.symbol(context.symbols.intern("inline")), .null)
    self.text = context.symbols.intern("text")
    self.code = context.symbols.intern("code")
    self.emph = context.symbols.intern("emph")
    self.strong = context.symbols.intern("strong")
    self.link = context.symbols.intern("link")
    self.autoLink = context.symbols.intern("auto-link")
    self.emailAutoLink = context.symbols.intern("email-auto-link")
    self.image = context.symbols.intern("image")
    self.html = context.symbols.intern("html")
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
  }

  /// Declarations of the library.
  public override func declarations() {
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
    self.define(Procedure("table", table))
    self.define(Procedure("definition-list", definitionList))
    self.define(Procedure("markdown-list?", isMarkdownList))
    self.define(Procedure("markdown-list-item?", isMarkdownListItem))
    self.define(Procedure("markdown-list-item=?", markdownListItemEquals))
    self.define(Procedure("bullet", bullet))
    self.define(Procedure("ordered", ordered))
    self.define(Procedure("markdown-text?", isMarkdownText))
    self.define(Procedure("markdown-inline?", isMarkdownInline))
    self.define(Procedure("markdown-inline=?",markdownInlineEquals))
    self.define(Procedure("text", text))
    self.define(Procedure("code", code))
    self.define(Procedure("emph", emph))
    self.define(Procedure("strong", strong))
    self.define(Procedure("link", link))
    self.define(Procedure("auto-link", autoLink))
    self.define(Procedure("email-auto-link", emailAutoLink))
    self.define(Procedure("image", image))
    self.define(Procedure("html", html))
    self.define(Procedure("line-break", lineBreak))
    self.define(Procedure("markdown", markdown))
    self.define(Procedure("markdown?", isMarkdown))
    self.define(Procedure("markdown=?", markdownEquals))
    self.define(Procedure("markdown->html", markdownToHtml))
    self.define(Procedure("blocks->html", blocksToHtml))
    self.define(Procedure("text->html", textToHtml))
    self.define(Procedure("markdown->html-doc", markdownToHtmlDoc))
    self.define(Procedure("text->string", textToString))
    self.define(Procedure("text->raw-string", textToRawString))
  }

  private func makeCase(_ type: Tuple, _ sym: Symbol, _ exprs: Expr...) -> Expr {
    return .tagged(.mpair(type), .pair(.symbol(sym), .makeList(Exprs(exprs))))
  }

  private func makeCase(_ type: Tuple,
                        _ sym: Symbol,
                        _ n: Int,
                        _ args: Arguments) throws -> Expr {
    if case .some(.symbol(InternalLibrary.deconstructionTag)) = args.first {
      guard args.count == 2,
            case .tagged(.mpair(type), let payload) = args[args.index(after: args.startIndex)],
            case .pair(.symbol(sym), let res) = payload else {
        return .false
      }
      return res
    }
    guard args.count == n else {
      throw RuntimeError.argumentCount(num: n, args: .makeList(args))
    }
    return .tagged(.mpair(type), .pair(.symbol(sym), .makeList(args)))
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
    guard case .tagged(.mpair(self.blockType), _) = expr else {
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
    guard case .tagged(.mpair(self.listItemType), _) = expr else {
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
      guard case .tagged(.mpair(self.inlineType), _) = fragment else {
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
    while case .pair(.tagged(.mpair(self.blockType), _), let rest) = list {
      list = rest
    }
    return .makeBoolean(list == .null)
  }

  private func isMarkdownBlock(_ expr: Expr) -> Expr {
    guard case .tagged(.mpair(self.blockType), _) = expr else {
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
    while case .pair(.tagged(.mpair(self.listItemType), _), let rest) = list {
      list = rest
    }
    return .makeBoolean(list == .null)
  }

  private func isMarkdownListItem(_ expr: Expr) -> Expr {
    guard case .tagged(.mpair(self.listItemType), _) = expr else {
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

  private func isMarkdownText(_ expr: Expr) -> Expr {
    var list = expr
    while case .pair(.tagged(.mpair(self.inlineType), _), let rest) = list {
      list = rest
    }
    return .makeBoolean(list == .null)
  }

  private func isMarkdownInline(_ expr: Expr) -> Expr {
    guard case .tagged(.mpair(self.inlineType), _) = expr else {
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

  private func markdown(_ str: Expr, _ extended: Expr?) throws -> Expr {
    let parser = (extended?.isTrue ?? false) ? ExtendedMarkdownParser.standard
                                             : MarkdownParser.standard
    guard let res = self.externMarkdown(parser.parse(try str.asString())) else {
      return .false
    }
    return res
  }
  
  private func isMarkdown(_ expr: Expr) throws -> Expr {
    guard case .tagged(.mpair(self.blockType), .pair(.symbol(self.document), _)) = expr else {
      return .false
    }
    return (try? self.internMarkdown(block: expr)) == nil ? .false : .true
  }

  private func markdownEquals(_ lhs: Expr, _ rhs: Expr) throws -> Expr {
    let lhsMd = try self.internMarkdown(block: lhs)
    let rhsMd = try self.internMarkdown(block: rhs)
    return .makeBoolean(lhsMd == rhsMd)
  }

  private func markdownToHtml(_ md: Expr) throws -> Expr {
    return .makeString(HtmlGenerator.standard.generate(doc: try self.internMarkdown(block: md)))
  }
  
  private func blocksToHtml(_ expr: Expr, tight: Expr?) throws -> Expr {
    let tight = tight?.isTrue ?? false
    switch expr {
      case .pair(_ , _):
        return .makeString(HtmlGenerator.standard.generate(blocks:
                             try self.internMarkdown(blocks: expr), tight: tight))
      case .tagged(.mpair(self.blockType), _):
        return .makeString(HtmlGenerator.standard.generate(block:
                             try self.internMarkdown(block: expr), tight: tight))
      default:
        throw RuntimeError.custom(
          "type error", "blocks->html expects argument of type block or list of block; " +
          "received \(expr)", [])
    }
  }
  
  private func textToHtml(_ expr: Expr) throws -> Expr {
    switch expr {
      case .pair(_ , _):
        return .makeString(HtmlGenerator.standard.generate(text:
                             try self.internMarkdown(text: expr)))
      case .tagged(.mpair(self.inlineType), _):
        return .makeString(HtmlGenerator.standard.generate(textFragment:
                             try self.internMarkdown(fragment: expr)))
      default:
        throw RuntimeError.custom(
          "type error", "text->html expects argument of type inline or list of inline; " +
          "received \(expr)", [])
    }
  }
  
  private func markdownToHtmlDoc(md: Expr, args: Arguments) throws -> Expr {
    var fontSize: Float = 14.0
    var fontFamily = "\"Times New Roman\",Times,serif"
    var fontColor = MarkdownKit.mdDefaultColor
    var codeFontSize: Float = 13.0
    var codeFontFamily = "\"Consolas\",\"Andale Mono\",\"Courier New\",Courier,monospace"
    var codeFontColor = MarkdownKit.mdDefaultColor
    var codeBlockFontSize: Float = 12.0
    var codeBlockFontColor = MarkdownKit.mdDefaultColor
    var codeBlockBackground = MarkdownKit.mdDefaultBackgroundColor
    var borderColor = "#bbb"
    var blockquoteColor = "#99c"
    var h1Color = MarkdownKit.mdDefaultColor
    var h2Color = MarkdownKit.mdDefaultColor
    var h3Color = MarkdownKit.mdDefaultColor
    var h4Color = MarkdownKit.mdDefaultColor
    if args.count > 0 {
      (fontSize, fontFamily, fontColor) = try self.asSizeFontColor(args.first!,
                                                                   defaultSize: fontSize,
                                                                   defaultFont: fontFamily,
                                                                   defaultColor: fontColor)
    }
    if args.count > 1 {
      (codeFontSize, codeFontFamily, codeFontColor) =
        try self.asSizeFontColor(args[args.startIndex + 1],
                                 defaultSize: codeFontSize,
                                 defaultFont: codeFontFamily,
                                 defaultColor: codeFontColor)
    }
    if args.count > 2 {
      (codeBlockFontSize, codeBlockFontColor, codeBlockBackground) =
        try self.asSizeFontColor(args[args.startIndex + 2],
                                 defaultSize: codeBlockFontSize,
                                 defaultFont: codeBlockFontColor,
                                 defaultColor: codeBlockBackground)
    }
    if args.count > 3 {
      var list = args[args.startIndex + 3]
      if case .pair(let head, let tail) = list {
        if !head.isFalse {
          borderColor = try head.asString()
        }
        list = tail
      }
      if case .pair(let head, let tail) = list {
        if !head.isFalse {
          blockquoteColor = try head.asString()
        }
        list = tail
      }
      if case .pair(let head, let tail) = list {
        if !head.isFalse {
          h1Color = try head.asString()
        }
        list = tail
      }
      if case .pair(let head, let tail) = list {
        if !head.isFalse {
          h2Color = try head.asString()
        }
        list = tail
      }
      if case .pair(let head, let tail) = list {
        if !head.isFalse {
          h3Color = try head.asString()
        }
        list = tail
      }
      if case .pair(let head, _) = list {
        if !head.isFalse {
          h4Color = try head.asString()
        }
      }
    }
    let attribStrGen = AttributedStringGenerator(fontSize: fontSize,
                                                 fontFamily: fontFamily,
                                                 fontColor: fontColor,
                                                 codeFontSize: codeFontSize,
                                                 codeFontFamily: codeFontFamily,
                                                 codeFontColor: codeFontColor,
                                                 codeBlockFontSize: codeBlockFontSize,
                                                 codeBlockFontColor: codeBlockFontColor,
                                                 codeBlockBackground: codeBlockBackground,
                                                 borderColor: borderColor,
                                                 blockquoteColor: blockquoteColor,
                                                 h1Color: h1Color,
                                                 h2Color: h2Color,
                                                 h3Color: h3Color,
                                                 h4Color: h4Color)
    return .makeString(
              attribStrGen.generateHtml(
                attribStrGen.htmlGenerator.generate(doc: try self.internMarkdown(block: md))))
  }
  
  private func asSizeFontColor(_ expr: Expr,
                               defaultSize: Float,
                               defaultFont: String,
                               defaultColor: String) throws -> (Float, String, String) {
    guard !expr.isFalse else {
      return (defaultSize, defaultFont, defaultColor)
    }
    switch expr {
      case .pair(let size, .pair(let font, .pair(let color, .null))):
        return (size.isFalse ? defaultSize : Float(try size.asDouble(coerce: true)),
                font.isFalse ? defaultFont : try font.asString(),
                color.isFalse ? defaultColor : try color.asString())
      case .pair(let size, .pair(let font, .null)):
        return (size.isFalse ? defaultSize : Float(try size.asDouble(coerce: true)),
                font.isFalse ? defaultFont : try font.asString(),
                defaultColor)
      case .pair(let size, .null):
        return (size.isFalse ? defaultSize : Float(try size.asDouble(coerce: true)),
                defaultFont,
                defaultColor)
      case .null:
        return (defaultSize, defaultFont, defaultColor)
      default:
        throw RuntimeError.custom("type error", "triple (float, font, color) needs to be " +
                                                "represented by a list of up to three elements", [])
    }
  }

  private func textToString(_ expr: Expr) throws -> Expr {
    switch expr {
      case .pair(_ , _):
        return .makeString(try self.internMarkdown(text: expr).description)
      case .tagged(.mpair(self.inlineType), _):
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
      case .tagged(.mpair(self.inlineType), _):
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
    guard case .tagged(.mpair(self.blockType), .pair(.symbol(let sym), let args)) = expr else {
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
    guard case .tagged(.mpair(self.listItemType), .pair(.symbol(let sym), let args)) = expr else {
      throw RuntimeError.custom("type error",
                                "expected value of type list-item; received \(expr)",
                                [])
    }
    switch sym {
      case self.bullet:
        if case .pair(let char, .pair(let tight, .pair(let blocks, .null))) = args {
          return .listItem(.bullet(Character(unicodeScalar(try char.asUniChar()))),
                           tight.isTrue,
                           try self.internMarkdown(blocks: blocks))
        }
      case self.ordered:
        if case .pair(let start, .pair(let char,
                                       .pair(let tight, .pair(let blocks, .null)))) = args {
          return .listItem(.ordered(try start.asInt(),
                                    Character(unicodeScalar(try char.asUniChar()))),
                         tight.isTrue,
                         try self.internMarkdown(blocks: blocks))
      }
      default:
        break
    }
    throw RuntimeError.custom("eval error", "malformed expression of type list-item: \(expr)", [])
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
    guard case .tagged(.mpair(self.inlineType),
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
          return .image(try self.internMarkdown(text: fst),
                        uri.isFalse ? nil : try uri.asString(),
                        title.isFalse ? nil : try title.asString())
        }
      case self.html:
        return .html(Substring(try fst.asString()))
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
      case .listItem(.bullet(let ch), let tight, let blocks):
        return self.makeCase(self.listItemType,
                             self.bullet,
                             .char(ch.utf16.first!),
                             .makeBoolean(tight),
                             self.externMarkdown(blocks))
      case .listItem(.ordered(let n, let ch), let tight, let blocks):
        return self.makeCase(self.listItemType,
                             self.ordered,
                             .makeNumber(n),
                             .char(ch.utf16.first!),
                             .makeBoolean(tight),
                             self.externMarkdown(blocks))
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
      default:
        return nil
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
      case .custom(_):
        return nil
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
}
