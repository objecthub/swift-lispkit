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
    self.lineBreak = context.symbols.intern("lineBreak")
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
    self.define(Procedure("markdown-block?", isMarkdownBlock))
    self.define(Procedure("markdown-block-valid?", isMarkdownBlockValid))
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
    self.define(Procedure("markdown-list-item?", isMarkdownListItem))
    self.define(Procedure("markdown-list-item-valid?", isMarkdownListItemValid))
    self.define(Procedure("markdown-list-item=?", markdownListItemEquals))
    self.define(Procedure("bullet", bullet))
    self.define(Procedure("ordered", ordered))
    self.define(Procedure("markdown-inline?", isMarkdownInline))
    self.define(Procedure("markdown-inline-valid?", isMarkdownInlineValid))
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
    self.define(Procedure("lineBreak", lineBreak))
    self.define(Procedure("markdown", markdown))
    self.define(Procedure("markdown?", isMarkdown))
    self.define(Procedure("markdown-valid?", isMarkdownValid))
    self.define(Procedure("markdown=?", markdownEquals))
    self.define(Procedure("markdown->html", markdownToHtml))
    self.define(Procedure("inline->string", inlineToString))
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

  private func isMarkdownBlock(_ expr: Expr) -> Expr {
    guard case .tagged(.mpair(let tuple), _) = expr, tuple === self.blockType else {
      return .false
    }
    return .true
  }

  private func isMarkdownBlockValid(_ expr: Expr) -> Expr {
    return (try? self.internMarkdown(block: expr)) == nil ? .false : .true
  }

  private func markdownBlockEquals(_ lhs: Expr, _ rhs: Expr) throws -> Expr {
    let lhsMd = try self.internMarkdown(block: lhs)
    let rhsMd = try self.internMarkdown(block: rhs)
    return .makeBoolean(lhsMd == rhsMd)
  }

  private func document(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.blockType, self.document, 1, args)
  }

  private func blockquote(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.blockType, self.blockquote, 1, args)
  }

  private func listItems(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.blockType, self.listItems, 3, args)
  }

  private func paragraph(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.blockType, self.paragraph, 1, args)
  }

  private func heading(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.blockType, self.heading, 2, args)
  }

  private func indentedCode(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.blockType, self.indentedCode, 1, args)
  }

  private func fencedCode(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.blockType, self.fencedCode, 2, args)
  }

  private func htmlBlock(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.blockType, self.htmlBlock, 1, args)
  }

  private func referenceDef(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.blockType, self.referenceDef, 3, args)
  }

  private func thematicBreak(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.blockType, self.thematicBreak, 0, args)
  }

  private func isMarkdownListItem(_ expr: Expr) -> Expr {
    guard case .tagged(.mpair(let tuple), _) = expr, tuple === self.listItemType else {
      return .false
    }
    return .true
  }

  private func isMarkdownListItemValid(_ expr: Expr) -> Expr {
    return (try? self.internMarkdown(item: expr)) == nil ? .false : .true
  }

  private func markdownListItemEquals(_ lhs: Expr, _ rhs: Expr) throws -> Expr {
    let lhsMd = try self.internMarkdown(item: lhs)
    let rhsMd = try self.internMarkdown(item: rhs)
    return .makeBoolean(lhsMd == rhsMd)
  }

  private func bullet(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.listItemType, self.bullet, 3, args)
  }

  private func ordered(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.listItemType, self.ordered, 4, args)
  }

  private func isMarkdownInline(_ expr: Expr) -> Expr {
    guard case .tagged(.mpair(let tuple), _) = expr, tuple === self.inlineType else {
      return .false
    }
    return .true
  }

  private func isMarkdownInlineValid(_ expr: Expr) -> Expr {
    return (try? self.internMarkdown(fragment: expr)) == nil ? .false : .true
  }

  private func markdownInlineEquals(_ lhs: Expr, _ rhs: Expr) throws -> Expr {
    let lhsMd = try self.internMarkdown(fragment: lhs)
    let rhsMd = try self.internMarkdown(fragment: rhs)
    return .makeBoolean(lhsMd == rhsMd)
  }

  private func text(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.inlineType, self.text, 1, args)
  }

  private func code(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.inlineType, self.code, 1, args)
  }

  private func emph(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.inlineType, self.emph, 1, args)
  }

  private func strong(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.inlineType, self.strong, 1, args)
  }

  private func link(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.inlineType, self.link, 3, args)
  }

  private func autoLink(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.inlineType, self.autoLink, 1, args)
  }

  private func emailAutoLink(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.inlineType, self.emailAutoLink, 1, args)
  }

  private func image(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.inlineType, self.image, 3, args)
  }

  private func html(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.inlineType, self.html, 1, args)
  }

  private func lineBreak(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.inlineType, self.lineBreak, 1, args)
  }

  private func markdown(_ str: Expr) throws -> Expr {
    guard let res = self.externMarkdown(MarkdownParser.standard.parse(try str.asString())) else {
      return .false
    }
    return res
  }

  private func isMarkdown(_ expr: Expr) throws -> Expr {
    guard case .tagged(.mpair(self.blockType), .pair(.symbol(self.document), _)) = expr else {
      return .false
    }
    return .true
  }

  private func isMarkdownValid(_ expr: Expr) throws -> Expr {
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

  private func inlineToString(_ expr: Expr) throws -> Expr {
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

  private func externMarkdown(_ block: Block) -> Expr? {
    switch block {
      case .document(let blocks):
        return self.makeCase(self.blockType, self.document, self.externMarkdown(blocks))
      case .blockquote(let blocks):
        return self.makeCase(self.blockType, self.blockquote, self.externMarkdown(blocks))
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
        return self.makeCase(self.blockType, self.paragraph, self.externMarkdown(text))
      case .heading(let level, let text):
        return self.makeCase(self.blockType,
                             self.heading,
                             .makeNumber(level),
                             self.externMarkdown(text))
      case .indentedCode(let lines):
        return self.makeCase(self.blockType, self.indentedCode, self.externMarkdown(lines))
      case .fencedCode(let lang, let lines):
        return self.makeCase(self.blockType,
                             self.fencedCode,
                             self.externMarkdown(lang),
                             self.externMarkdown(lines))
      case .htmlBlock(let lines):
        return self.makeCase(self.blockType, self.htmlBlock, self.externMarkdown(lines))
      case .referenceDef(let label, let dest, let title):
        return self.makeCase(self.blockType,
                             self.referenceDef,
                             self.externMarkdown(label),
                             self.externMarkdown(String(dest)),
                             self.externMarkdown(title))
      case .thematicBreak:
        return self.makeCase(self.blockType, self.thematicBreak)
    }
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

  private func externMarkdown(_ fragment: TextFragment) -> Expr {
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
    }
  }

  private func externMarkdown(_ text: Text) -> Expr {
    var exprs: Exprs = []
    for fragment in text {
      exprs.append(self.externMarkdown(fragment))
    }
    return .makeList(exprs)
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
