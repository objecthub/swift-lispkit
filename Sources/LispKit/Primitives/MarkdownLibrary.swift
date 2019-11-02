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
  private let listBlock: Symbol
  private let paragraph: Symbol
  private let heading: Symbol
  private let indentedCode: Symbol
  private let fencedCode: Symbol
  private let htmlBlock: Symbol
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
    self.listBlock = context.symbols.intern("list-block")
    self.paragraph = context.symbols.intern("paragraph")
    self.heading = context.symbols.intern("heading")
    self.indentedCode = context.symbols.intern("indented-code")
    self.fencedCode = context.symbols.intern("fenced-code")
    self.htmlBlock = context.symbols.intern("html-block")
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
    self.define(Procedure("document", document))
    self.define(Procedure("blockquote", blockquote))
    self.define(Procedure("list-block", listBlock))
    self.define(Procedure("paragraph", paragraph))
    self.define(Procedure("heading", heading))
    self.define(Procedure("indented-code", indentedCode))
    self.define(Procedure("fenced-code", fencedCode))
    self.define(Procedure("html-block", htmlBlock))
    self.define(Procedure("thematic-break", thematicBreak))
    self.define(Procedure("markdown-list-item?", isMarkdownListItem))
    self.define(Procedure("bullet", bullet))
    self.define(Procedure("ordered", ordered))
    self.define(Procedure("markdown-inline?", isMarkdownInline))
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
    self.define(Procedure("parse-markdown", parseMarkdown))
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

  private func document(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.blockType, self.document, 1, args)
  }

  private func blockquote(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.blockType, self.blockquote, 1, args)
  }

  private func listBlock(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.blockType, self.listBlock, 3, args)
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

  private func thematicBreak(_ args: Arguments) throws -> Expr {
    return try self.makeCase(self.blockType, self.thematicBreak, 0, args)
  }

  private func isMarkdownListItem(_ expr: Expr) -> Expr {
    guard case .tagged(.mpair(let tuple), _) = expr, tuple === self.listItemType else {
      return .false
    }
    return .true
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

  private func parseMarkdown(_ str: Expr) throws -> Expr {
    guard let res = self.externMarkdown(MarkdownParser.standard.parse(try str.asString())) else {
      return .false
    }
    return res
  }

  private func markdownToHtml(_ str: Expr) throws -> Expr {
    return .null
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

  private func internMarkdown(text expr: Expr) throws -> Text {
    var list = expr
    var text = Text()
    while case .pair(let fragment, let rest) = list {
      text.append(fragment: try self.internMarkdown(fragment: fragment))
      list = rest
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
        if case .pair(let uri, let title) = args {
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
    throw RuntimeError.custom("internal error", "malformed expression of type inline: \(expr)", [])
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
                               self.listBlock,
                               .makeNumber(start),
                               .makeBoolean(tight),
                               self.externMarkdown(blocks))
        } else {
          return self.makeCase(self.blockType,
                              self.listBlock,
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
                             self.bullet,
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
      case .referenceDef(_, _, _):
        return nil
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
