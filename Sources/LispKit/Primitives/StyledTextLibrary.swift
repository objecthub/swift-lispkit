//
//  StyledTextLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 14/05/2022.
//  Copyright © 2022 ObjectHub. All rights reserved.
//

import Foundation
#if os(iOS) || os(watchOS) || os(tvOS)
import UIKit
#elseif os(macOS)
import AppKit
#endif

///
/// This class implements the library `(lispkit styled-text)`. 
///
public final class StyledTextLibrary: NativeLibrary {
  
  /// Symbols used in enumeration values
  
  // Text style attributes
  private let backgroundColor: Symbol
  private let baselineOffset: Symbol
  private let expansion: Symbol
  private let font: Symbol
  private let foregroundColor: Symbol
  private let kern: Symbol
  private let ligature: Symbol
  private let link: Symbol
  private let obliqueness: Symbol
  private let paragraphStyle: Symbol
  private let shadow: Symbol
  private let strikethroughColor: Symbol
  private let strokeColor: Symbol
  private let strokeWidth: Symbol
  private let superscript: Symbol
  private let underlineColor: Symbol
  private let writingDirection: Symbol
  
  // Text block style attributes
  private let width: Symbol
  private let height: Symbol
  private let margin: Symbol
  private let border: Symbol
  private let padding: Symbol
  private let borderColor: Symbol
  private let verticalAlignment: Symbol
  
  // Paragraph style attributes
  private let alignment: Symbol
  private let firstHeadIndent: Symbol
  private let headIndent: Symbol
  private let tailIndent: Symbol
  private let lineHeightMultiple: Symbol
  private let maximumLineHeight: Symbol
  private let minimumLineHeight: Symbol
  private let lineSpacing: Symbol
  private let paragraphSpacing: Symbol
  private let paragraphSpacingBefore: Symbol
  private let tabInterval: Symbol
  private let textFitMode: Symbol
  private let pushOutLineBreak: Symbol
  private let hyphenationFactor: Symbol
  
  // Text alignment
  private let left: Symbol
  private let right: Symbol
  private let center: Symbol
  private let justified: Symbol
  private let natural: Symbol
  
  // Vertical alignment
  private let top: Symbol
  private let bottom: Symbol
  private let middle: Symbol
  private let baseline: Symbol
  
  // Text fit mode
  private let wordWrap: Symbol
  private let charWrap: Symbol
  private let clip: Symbol
  private let truncate: Symbol
  private let truncateHead: Symbol
  private let truncateTail: Symbol
  
  // Writing direction
  private let leftToRight: Symbol
  private let rightToLeft: Symbol
  
  // Supported document types
  private let docDoc: Symbol
  private let wordDoc: Symbol
  private let rtfDoc: Symbol
  private let rtfdDoc: Symbol
  private let plainDoc: Symbol
  
  // Percentage
  private let percent: Symbol
  
  /// Initialize drawing library, in particular its parameter objects.
  public required init(in context: Context) throws {
    self.backgroundColor = context.symbols.intern("background-color")
    self.baselineOffset = context.symbols.intern("baseline-offset")
    self.expansion = context.symbols.intern("expansion")
    self.font = context.symbols.intern("font")
    self.foregroundColor = context.symbols.intern("foreground-color")
    self.kern = context.symbols.intern("kern")
    self.ligature = context.symbols.intern("ligature")
    self.link = context.symbols.intern("link")
    self.obliqueness = context.symbols.intern("obliqueness")
    self.paragraphStyle = context.symbols.intern("paragraph-style")
    self.shadow = context.symbols.intern("shadow")
    self.strikethroughColor = context.symbols.intern("strikethrough-color")
    self.strokeColor = context.symbols.intern("stroke-color")
    self.strokeWidth = context.symbols.intern("stroke-width")
    self.superscript = context.symbols.intern("superscript")
    self.underlineColor = context.symbols.intern("underline-color")
  
    self.width = context.symbols.intern("width")
    self.height = context.symbols.intern("height")
    self.margin = context.symbols.intern("margin")
    self.border = context.symbols.intern("border")
    self.padding = context.symbols.intern("padding")
    self.borderColor = context.symbols.intern("border-color")
    self.verticalAlignment = context.symbols.intern("vertical-alignment")
    
    self.alignment = context.symbols.intern("alignment")
    self.firstHeadIndent = context.symbols.intern("first-head-indent")
    self.headIndent = context.symbols.intern("head-indent")
    self.tailIndent = context.symbols.intern("tail-indent")
    self.lineHeightMultiple = context.symbols.intern("line-height-multiple")
    self.maximumLineHeight = context.symbols.intern("max-line-height")
    self.minimumLineHeight = context.symbols.intern("min-line-height")
    self.lineSpacing = context.symbols.intern("line-spacing")
    self.paragraphSpacing = context.symbols.intern("paragraph-spacing-after")
    self.paragraphSpacingBefore = context.symbols.intern("paragraph-spacing-before")
    self.tabInterval = context.symbols.intern("tab-interval")
    self.textFitMode = context.symbols.intern("text-fit-mode")
    self.pushOutLineBreak = context.symbols.intern("push-out-line-break")
    self.hyphenationFactor = context.symbols.intern("hypenation-factor")
    self.writingDirection = context.symbols.intern("writing-direction")
    
    self.left = context.symbols.intern("left")
    self.right = context.symbols.intern("right")
    self.center = context.symbols.intern("center")
    self.justified = context.symbols.intern("justified")
    self.natural = context.symbols.intern("natural")
    
    self.top = context.symbols.intern("top")
    self.bottom = context.symbols.intern("bottom")
    self.middle = context.symbols.intern("middle")
    self.baseline = context.symbols.intern("baseline")
    
    self.wordWrap = context.symbols.intern("word-wrap")
    self.charWrap = context.symbols.intern("char-wrap")
    self.clip = context.symbols.intern("clip")
    self.truncate = context.symbols.intern("truncate")
    self.truncateHead = context.symbols.intern("truncate-head")
    self.truncateTail = context.symbols.intern("truncate-tail")
    
    self.leftToRight = context.symbols.intern("left-to-right")
    self.rightToLeft = context.symbols.intern("right-to-left")
    
    self.docDoc = context.symbols.intern("doc")
    self.wordDoc = context.symbols.intern("docx")
    self.rtfDoc = context.symbols.intern("rtf")
    self.rtfdDoc = context.symbols.intern("rtfd")
    self.plainDoc = context.symbols.intern("plain")
    
    self.percent = context.symbols.intern("%")
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "styled-text"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"],    "define", "define-syntax", "syntax-rules")
    self.`import`(from: ["lispkit", "control"], "let")
    self.`import`(from: ["lispkit", "dynamic"], "parameterize")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    // Styled text
    self.define(Procedure("styled-text?", isStyledText))
    self.define(Procedure("styled-text", styledText))
    self.define(Procedure("make-styled-text", makeStyledText))
    self.define(Procedure("load-styled-text", loadStyledText))
    self.define(Procedure("copy-styled-text", copyStyledText))
    self.define(Procedure("save-styled-text", saveStyledText))
    self.define(Procedure("bytevector->styled-text", bytevectorToStyledText))
    self.define(Procedure("styled-text=?", styledTextEquals))
    self.define(Procedure("styled-text-string", styledTextString))
    self.define(Procedure("styled-text-insert!", styledTextInsert))
    self.define(Procedure("styled-text-append!", styledTextAppend))
    self.define(Procedure("styled-text-ref", styledTextRef))
    self.define(Procedure("styled-text-set!", styledTextSet))
    self.define(Procedure("styled-text-add!", styledTextAdd))
    self.define(Procedure("styled-text-remove!", styledTextRemove))
    self.define(Procedure("styled-text-attribute", styledTextAttribute))
    self.define(Procedure("styled-text-attributes", styledTextAttributes))
    self.define(Procedure("styled-text->bytevector", styledTextToBytevector))
    
    // Text styles
    self.define(Procedure("text-style?", isTextStyle))
    self.define(Procedure("make-text-style", makeTextStyle))
    self.define(Procedure("copy-text-style", copyTextStyle))
    self.define(Procedure("text-style-empty?", textStyleEmpty))
    self.define(Procedure("text-style-ref", textStyleRef))
    self.define(Procedure("text-style-set!", textStyleSet))
    self.define(Procedure("text-style-merge!", textStyleMerge))
    self.define(Procedure("text-style-remove!", textStyleRemove))
    self.define(Procedure("text-style-attributes", textStyleAttributes))
    
    // Text block styles
    self.define(Procedure("percent", percentage))
    self.define(Procedure("percent?", isPercentage))
    self.define(Procedure("text-block-style?", isTextBlockStyle))
    self.define(Procedure("make-text-block-style", makeTextBlockStyle))
    self.define(Procedure("copy-text-block-style", copyTextBlockStyle))
    self.define(Procedure("text-block-style=?", textBlockStyleEquals))
    self.define(Procedure("text-block-style-ref", textBlockStyleRef))
    self.define(Procedure("text-block-style-set!", textBlockStyleSet))
    
    // Paragraph styles
    self.define(Procedure("paragraph-style?", isParagraphStyle))
    self.define(Procedure("make-paragraph-style", makeParagraphStyle))
    self.define(Procedure("copy-paragraph-style", copyParagraphStyle))
    self.define(Procedure("paragraph-style=?", paragraphStyleEquals))
    self.define(Procedure("paragraph-style-ref", paragraphStyleRef))
    self.define(Procedure("paragraph-style-set!", paragraphStyleSet))
    self.define(Procedure("paragraph-style-tabstops", paragraphStyleTabstops))
    self.define(Procedure("paragraph-style-tabstop-add!", paragraphStyleTabstopAdd))
    self.define(Procedure("paragraph-style-tabstop-remove!", paragraphStyleTabstopRemove))
  }
  
  // MARK: - Utilities
  
  private func color(from expr: Expr) throws -> Color {
    guard case .object(let obj) = expr, let color = obj as? Color else {
      throw RuntimeError.type(expr, expected: [Color.type])
    }
    return color
  }
  
  private func font(from expr: Expr) throws -> Font {
    guard case .object(let obj) = expr, let fontBox = obj as? NativeFont else {
      throw RuntimeError.type(expr, expected: [NativeFont.type])
    }
    return fontBox.value
  }
  
  private func styledText(from expr: Expr) throws -> StyledText {
    guard case .object(let obj) = expr, let str = obj as? StyledText else {
      throw RuntimeError.type(expr, expected: [StyledText.type])
    }
    return str
  }
  
  private func textStyle(from expr: Expr) throws -> TextStyle {
    guard case .object(let obj) = expr, let style = obj as? TextStyle else {
      throw RuntimeError.type(expr, expected: [TextStyle.type])
    }
    return style
  }
  
  private func textBlockStyle(from expr: Expr) throws -> TextBlockStyle {
    guard case .object(let obj) = expr, let style = obj as? TextBlockStyle else {
      throw RuntimeError.type(expr, expected: [TextBlockStyle.type])
    }
    return style
  }
  
  private func paragraphStyle(from expr: Expr) throws -> ParagraphStyle {
    guard case .object(let obj) = expr, let style = obj as? ParagraphStyle else {
      throw RuntimeError.type(expr, expected: [ParagraphStyle.type])
    }
    return style
  }
  
  private func bytevector(from data: Data) -> Expr {
    let count = data.count
    var res = [UInt8](repeating: 0, count: count)
    data.copyBytes(to: &res, count: count)
    return .bytes(MutableBox(res))
  }
  
  private func percentageOrPoints(from expr: Expr) throws -> TextBlockStyle.Value {
    switch expr {
      case .false:
        return .undefined
      case .pair(.flonum(let x), .symbol(self.percent)):
        return .percentage(x)
      default:
        return .points(try expr.asDouble(coerce: true))
    }
  }
  
  private func verticalAlignment(from sym: Symbol) -> TextBlockStyle.VerticalAlignment? {
    switch sym {
      case self.top:
        return .top
      case self.bottom:
        return .bottom
      case self.middle:
        return .middle
      case self.baseline:
        return .baseline
      default:
        return nil
    }
  }
  
  private func documentType(from sym: Symbol,
                            supportRtfd: Bool = false) -> NSAttributedString.DocumentType? {
    switch sym {
      case self.plainDoc:
        return .plain
      case self.rtfDoc:
        return .rtf
      case self.rtfdDoc:
        return supportRtfd ? .rtfd : nil
      #if os(macOS)
      case self.docDoc:
        return .docFormat
      case self.wordDoc:
        return .officeOpenXML
        // return .wordML
      #endif
      default:
        return nil
    }
  }
  
  // MARK: - Styled text
  
  private func isStyledText(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is StyledText {
      return .true
    }
    return .false
  }
  
  private func styledText(expr: Expr, args: Arguments) throws -> Expr {
    let str = try expr.asString()
    guard let (font, color, pstyle) = args.optional(.false, .false, .false) else {
      throw RuntimeError.argumentCount(of: "styled-text", min: 1, max: 4,
                                       args: .pair(expr, .makeList(args)))
    }
    let tstyle = TextStyle()
    if font.isTrue {
      if color.isFalse && pstyle.isFalse,
         case .object(let o) = font,
         let ts = o as? TextStyle {
        return .object(StyledText(NSMutableAttributedString(string: str,
                                                            attributes: ts.attributes)))
      } else {
        tstyle.attributes[.font] = try self.font(from: font)
      }
    }
    if color.isTrue {
      tstyle.attributes[.foregroundColor] = try self.color(from: color).nsColor
    }
    if pstyle.isTrue {
      tstyle.attributes[.paragraphStyle] = try self.paragraphStyle(from: pstyle).value
    }
    return .object(StyledText(NSMutableAttributedString(string: str,
                                                        attributes: tstyle.attributes)))
  }
  
  private func makeStyledText(obj: Expr, args: Arguments) throws -> Expr {
    switch obj {
      case .string(let str):
        let tstyle = TextStyle()
        var i = args.startIndex
        while i < args.endIndex {
          guard case .symbol(let keyword) = args[i],
                keyword.identifier.hasSuffix(":") else {
            throw RuntimeError.eval(.expectedTextStyleAttributeKeyword, args[i])
          }
          i = args.index(after: i)
          if i < args.endIndex {
            var tattr = keyword.identifier
            tattr.removeLast()
            try self.textStyleSet(tstyle: tstyle,
                                  tattr: self.context.symbols.intern(tattr),
                                  value: args[i])
            i = args.index(after: i)
          } else {
            break
          }
        }
        return .object(StyledText(NSMutableAttributedString(string: str as String,
                                                            attributes: tstyle.attributes)))
      case .object(let o):
        if let image = o as? NativeImage {
          guard args.isEmpty else {
            throw RuntimeError.eval(.textStyleNotApplicableToImage, obj)
          }
          let attachment = NSTextAttachment()
          attachment.image = image.value
          return .object(StyledText(NSMutableAttributedString(attachment: attachment)))
        } else {
          throw RuntimeError.eval(.cannotMakeStyledText, obj)
        }
      default:
        throw RuntimeError.eval(.cannotMakeStyledText, obj)
    }
  }
  
  private func loadStyledText(filename: Expr, format: Expr) throws -> Expr {
    let path = self.context.fileHandler.path(try filename.asPath(),
                                             relativeTo: self.context.evaluator.currentDirectoryPath)
    if let doctype = self.documentType(from: try format.asSymbol(), supportRtfd: true) {
      let astr = try NSMutableAttributedString(url: URL(fileURLWithPath: path),
                                               options: [.documentType : doctype],
                                               documentAttributes: nil)
      return .object(StyledText(astr))
    } else {
      throw RuntimeError.eval(.unsupportedDocType, format)
    }
  }
  
  private func copyStyledText(text: Expr, start: Expr?, end: Expr?) throws -> Expr {
    let astring = try self.styledText(from: text).value
    if let end = try end?.asInt(below: astring.length + 1) {
      let start = try start!.asInt(below: end + 1)
      return .object(
        StyledText(NSMutableAttributedString(attributedString:
            astring.attributedSubstring(from: NSRange(location: start, length: end - start)))))
    } else if let start = try start?.asInt(below: astring.length + 1) {
      return .object(StyledText(NSMutableAttributedString(attributedString:
                       astring.attributedSubstring(from: NSRange(location: start,
                                                                 length: astring.length - start)))))
    } else {
      return .object(StyledText(NSMutableAttributedString(attributedString: astring)))
    }
  }
  
  private func saveStyledText(filename: Expr, text: Expr, format: Expr) throws -> Expr {
    let path = self.context.fileHandler.path(try filename.asPath(),
                                             relativeTo: self.context.evaluator.currentDirectoryPath)
    let astr = try self.styledText(from: text).value
    if let doctype = self.documentType(from: try format.asSymbol(), supportRtfd: true) {
      let fileWrapper = try? astr.fileWrapper(from: NSMakeRange(0, astr.length),
                                              documentAttributes: [.documentType : doctype])
      try fileWrapper?.write(to: URL(fileURLWithPath: path),
                             options: .atomic,
                             originalContentsURL: nil)
    } else {
      throw RuntimeError.eval(.unsupportedDocType, format)
    }
    return .void
  }
  
  private func bytevectorToStyledText(expr: Expr, format: Expr, args: Arguments) throws -> Expr {
    let subvec = try BytevectorLibrary.subVector("bytevector->styled-text", expr, args)
    if let docType = self.documentType(from: try format.asSymbol()) {
      let astr = try NSMutableAttributedString(data: Data(subvec),
                                               options: [
                                                .documentType: docType,
                                                /* .characterEncoding: String.Encoding.utf8 */],
                                               documentAttributes: nil)
      return .object(StyledText(astr))
    } else {
      throw RuntimeError.eval(.unsupportedDocType, format)
    }
  }
  
  private func styledTextEquals(expr: Expr, args: Arguments) throws -> Expr {
    let text = try self.styledText(from: expr).value
    for arg in args {
      if !text.isEqual(to: try self.styledText(from: arg).value) {
        return .false
      }
    }
    return .true
  }
  
  private func styledTextString(text: Expr) throws -> Expr {
    return .string(try self.styledText(from: text).value.mutableString)
  }
  
  private func styledTextInsert(text: Expr, istr: Expr, s: Expr?, e: Expr?) throws -> Expr {
    let str = try self.styledText(from: text).value
    let start: Int
    let end: Int
    if let e = e {
      end = try e.asInt(below: str.length + 1)
      start = try s?.asInt(below: end + 1) ?? end
    } else {
      start = try s?.asInt(below: str.length + 1) ?? 0
      end = start
    }
    switch istr {
      case .false:
        str.deleteCharacters(in: NSMakeRange(start, end - start))
      case .string(let s):
        if s.length == 0 {
          str.deleteCharacters(in: NSMakeRange(start, end - start))
        } else {
          str.replaceCharacters(in: NSMakeRange(start, end - start), with: s as String)
        }
      case .object(let obj):
        if let image = obj as? NativeImage {
          let attachment = NSTextAttachment()
          attachment.image = image.value
          str.replaceCharacters(in: NSMakeRange(start, end - start),
                                with: NSMutableAttributedString(attachment: attachment))
        } else if let s = obj as? StyledText {
          str.replaceCharacters(in: NSMakeRange(start, end - start), with: s.value)
        } else {
          throw RuntimeError.eval(.cannotMakeStyledText, istr)
        }
      default:
        throw RuntimeError.type(istr, expected: [StyledText.type, .strType])
    }
    return .void
  }
  
  private func styledTextAppend(text: Expr, args: Arguments) throws -> Expr {
    let str = try self.styledText(from: text).value
    for arg in args {
      switch arg {
        case .string(let s):
          str.replaceCharacters(in: NSMakeRange(str.length, 0), with: s as String)
        case .object(let o):
          if let image = o as? NativeImage {
            let attachment = NSTextAttachment()
            attachment.image = image.value
            str.append(NSMutableAttributedString(attachment: attachment))
          } else if let s = o as? StyledText {
            str.append(s.value)
          } else {
            throw RuntimeError.eval(.cannotMakeStyledText, arg)
          }
        default:
          throw RuntimeError.type(arg, expected: [StyledText.type, .strType])
      }
    }
    return .void
  }
  
  private func styledTextRef(text: Expr, index: Expr) throws -> Expr {
    let str = try self.styledText(from: text).value
    let idx = try index.asInt(below: str.length + 1)
    var range = NSRange(location: 0, length: 0)
    let res = TextStyle()
    res.attributes = str.attributes(at: idx, effectiveRange: &range)
    return .object(res)
  }
  
  private func styledTextSet(text: Expr, start: Expr, end: Expr, args: Arguments) throws -> Expr {
    guard let (fst, snd) = args.optional(.false, .undef) else {
      throw RuntimeError.argumentCount(of: "styled-text-set!", min: 3, max: 5,
                                       args: .pair(text, .pair(start, .pair(end, .makeList(args)))))
    }
    let str = try self.styledText(from: text).value
    let e = end.isTrue ? try end.asInt(below: str.length + 1) : str.length
    let s = start.isTrue ? try start.asInt(below: e + 1) : 0
    if snd.isUndef {
      str.setAttributes(try self.textStyle(from: fst).attributes,
                        range: NSRange(location: s, length: e - s))
    } else {
      let tstyle = TextStyle()
      try self.textStyleSet(tstyle: tstyle, tattr: try fst.asSymbol(), value: snd)
      str.setAttributes(tstyle.attributes, range: NSRange(location: s, length: e - s))
    }
    return .void
  }
  
  private func styledTextAdd(text: Expr, start: Expr, end: Expr, args: Arguments) throws -> Expr {
    guard let (fst, snd) = args.optional(.false, .undef) else {
      throw RuntimeError.argumentCount(of: "styled-text-add!", min: 3, max: 5,
                                       args: .pair(text, .pair(start, .pair(end, .makeList(args)))))
    }
    let str = try self.styledText(from: text).value
    let e = end.isTrue ? try end.asInt(below: str.length + 1) : str.length
    let s = start.isTrue ? try start.asInt(below: e + 1) : 0
    if snd.isUndef {
      str.addAttributes(try self.textStyle(from: fst).attributes,
                        range: NSRange(location: s, length: e - s))
    } else {
      let tstyle = TextStyle()
      try self.textStyleSet(tstyle: tstyle, tattr: try fst.asSymbol(), value: snd)
      str.addAttributes(tstyle.attributes, range: NSRange(location: s, length: e - s))
    }
    return .void
  }
  
  private func styledTextRemove(text: Expr, start: Expr, end: Expr, attrib: Expr) throws -> Expr {
    let str = try self.styledText(from: text).value
    let e = end.isTrue ? try end.asInt(below: str.length + 1) : str.length
    let s = start.isTrue ? try start.asInt(below: e + 1) : 0
    if let key = self.attributeKey(from: try attrib.asSymbol()) {
      str.removeAttribute(key, range: NSRange(location: s, length: e - s))
    } else {
      throw RuntimeError.eval(.unknownTextStyleAttribute, attrib)
    }
    return .void
  }
  
  private func styledTextAttribute(text: Expr, index: Expr, sym: Expr, args: Arguments) throws -> Expr {
    guard let (start, end) = args.optional(.false, .false) else {
      throw RuntimeError.argumentCount(of: "styled-text-attribute", min: 3, max: 5,
                                       args: .pair(text, .pair(index, .pair(sym, .makeList(args)))))
    }
    let str = try self.styledText(from: text).value
    let idx = try index.asInt(below: str.length + 1)
    guard let key = self.attributeKey(from: try sym.asSymbol()) else {
      throw RuntimeError.eval(.unknownTextStyleAttribute, sym)
    }
    let s = start.isTrue ? try start.asInt(below: idx + 1) : 0
    let e = end.isTrue ? try end.asInt(above: s, below: str.length + 1) : str.length
    var range = NSRange(location: 0, length: 0)
    if let value = str.attribute(key,
                                 at: idx,
                                 longestEffectiveRange: &range,
                                 in: NSRange(location: s, length: e - s)),
       let res = try self.textStyleValue(key: key, value: value) {
      return .values(.pair(res, .pair(.pair(.fixnum(Int64(range.location)),
                                            .fixnum(Int64(range.location + range.length))), .null)))
    } else {
      return .false
    }
  }
  
  private func styledTextAttributes(text: Expr, index: Expr, args: Arguments) throws -> Expr {
    guard let (start, end) = args.optional(.false, .false) else {
      throw RuntimeError.argumentCount(of: "styled-text-attributes", min: 2, max: 4,
                                       args: .pair(text, .pair(index, .makeList(args))))
    }
    let str = try self.styledText(from: text).value
    let idx = try index.asInt(below: str.length + 1)
    let s = start.isTrue ? try start.asInt(below: idx + 1) : 0
    let e = end.isTrue ? try end.asInt(above: s, below: str.length + 1) : str.length
    var range = NSRange(location: 0, length: 0)
    let res = TextStyle()
    res.attributes = str.attributes(at: idx,
                                    longestEffectiveRange: &range,
                                    in: NSRange(location: s, length: e - s))
    return .values(.pair(.object(res),
                         .pair(.pair(.fixnum(Int64(range.location)),
                                     .fixnum(Int64(range.location + range.length))),
                               .null)))
  }
  
  private func styledTextToBytevector(text: Expr,
                                      format: Expr,
                                      start: Expr?,
                                      end: Expr?) throws -> Expr {
    let str = try self.styledText(from: text).value
    let e = (end?.isTrue ?? false) ? try end!.asInt(below: str.length + 1) : str.length
    let s = (start?.isTrue ?? false) ? try start!.asInt(below: e + 1) : 0
    if let docType = self.documentType(from: try format.asSymbol()) {
      let data = try str.data(from: NSRange(location: s, length: e - s),
                              documentAttributes: [
                                .documentType: docType,
                                /* .characterEncoding: String.Encoding.utf8 */])
      return self.bytevector(from: data)
    } else {
      throw RuntimeError.eval(.unsupportedDocType, format)
    }
  }
  
  // MARK: - Text style
  
  private func isTextStyle(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is TextStyle {
      return .true
    }
    return .false
  }
  
  private func textStyleEmpty(style: Expr) throws -> Expr {
    return .makeBoolean(try self.textStyle(from: style).attributes.isEmpty)
  }
  
  private func makeTextStyle(args: Arguments) throws -> Expr {
    let tstyle = TextStyle()
    var i = args.startIndex
    while i < args.endIndex {
      guard case .symbol(let keyword) = args[i],
            keyword.identifier.hasSuffix(":") else {
        throw RuntimeError.eval(.expectedTextStyleAttributeKeyword, args[i])
      }
      i = args.index(after: i)
      if i < args.endIndex {
        var tattr = keyword.identifier
        tattr.removeLast()
        try self.textStyleSet(tstyle: tstyle,
                              tattr: self.context.symbols.intern(tattr),
                              value: args[i])
        i = args.index(after: i)
      } else {
        break
      }
    }
    return .object(tstyle)
  }
  
  private func copyTextStyle(style: Expr) throws -> Expr {
    let newStyle = TextStyle()
    newStyle.attributes = try self.textStyle(from: style).attributes
    return .object(newStyle)
  }
  
  private func textStyleValue(key: NSAttributedString.Key, value val: Any) throws -> Expr? {
    switch key {
      case .backgroundColor,
           .foregroundColor,
           .strikethroughColor,
           .strokeColor,
           .underlineColor:
        if let value = val as? NativeColor {
          return .object(Color(value))
        } else if CFGetTypeID(val as CFTypeRef) == CGColor.typeID, // What a mess...
                  let col = Color(val as! CGColor) {
          return .object(col)
        } else {
          return .false
        }
      case .baselineOffset,
           .expansion,
           .kern,
           .obliqueness,
           .strokeWidth:
        if let value = val as? NSNumber {
          return .flonum(value.doubleValue)
        } else {
          return .false
        }
      case .ligature:
        return ((val as? NSNumber)?.intValue ?? 0) == 0 ? .false : .true
      case .font:
        if let value = val as? NativeFont {
          return .object(value)
        } else {
          return .false
        }
      case .link:
        if let url = val as? NSURL {
          return .makeString(url.description)
        } else if let str = val as? NSString {
          return .makeString(str as String)
        } else {
          return .false
        }
      case .paragraphStyle:
        if let pstyle = val as? NSParagraphStyle {
          let style = NSMutableParagraphStyle()
          style.setParagraphStyle(pstyle)
          return .object(ParagraphStyle(style))
        } else {
          return .false
        }
      case .shadow:
        if let shadow = val as? NSShadow {
          let col: Expr
          #if os(iOS) || os(watchOS) || os(tvOS)
          if CFGetTypeID(shadow.shadowColor as CFTypeRef) == CGColor.typeID, // What a mess...
             let c = Color(shadow.shadowColor as! CGColor) {
            col = .pair(.object(c), .null)
          } else {
            col = .null
          }
          #elseif os(macOS)
          if let color = shadow.shadowColor {
            col = .pair(.object(Color(color)), .null)
          } else {
            col = .null
          }
          #endif
          return .pair(.pair(.flonum(shadow.shadowOffset.width),
                             .flonum(shadow.shadowOffset.height)),
                       .pair(.flonum(shadow.shadowBlurRadius), col))
        } else {
          return .false
        }
      #if os(macOS)
      case .superscript:
        if let value = val as? NSNumber {
          return .fixnum(Int64(value.intValue))
        } else {
          return .false
        }
      #endif
      default:
        return nil
    }
  }
  
  private func textStyleRef(style: Expr, attr: Expr) throws -> Expr {
    let tstyle = try self.textStyle(from: style)
    guard let key = self.attributeKey(from: try attr.asSymbol()) else {
      throw RuntimeError.eval(.unknownTextStyleAttribute, attr)
    }
    guard let val = tstyle.attributes[key] else {
      return .false
    }
    if let value = try self.textStyleValue(key: key, value: val) {
      return value
    } else {
      throw RuntimeError.eval(.unknownTextStyleAttribute, attr)
    }
  }
  
  private func textStyleSet(tstyle: TextStyle,
                            tattr: Symbol,
                            value: Expr) throws {
    guard let key = self.attributeKey(from: tattr) else {
      throw RuntimeError.eval(.unknownTextStyleAttribute, .symbol(tattr))
    }
    switch key {
      case .backgroundColor,
           .foregroundColor,
           .strikethroughColor,
           .strokeColor,
           .underlineColor:
        tstyle.attributes[key] = try self.color(from: value).nsColor
      case .baselineOffset,
           .expansion,
           .kern,
           .obliqueness,
           .strokeWidth:
        tstyle.attributes[key] = NSNumber(value: try value.asDouble(coerce: true))
      case .ligature:
        tstyle.attributes[key] = NSNumber(value: value.isFalse ? 0 : 1)
      case .font:
        tstyle.attributes[key] = try self.font(from: value)
      case .link:
        tstyle.attributes[key] = NSString(string: try value.asString())
      case .paragraphStyle:
        let pstyle = NSMutableParagraphStyle()
        pstyle.setParagraphStyle(try self.paragraphStyle(from: value).value)
        tstyle.attributes[key] = pstyle
      case .shadow:
        switch value {
          case .pair(let size, .pair(let radius, .null)):
            guard case .pair(.flonum(let w), .flonum(let h)) = size else {
              throw RuntimeError.eval(.invalidSize, size)
            }
            let s = NSShadow()
            s.shadowBlurRadius = CGFloat(try radius.asDouble(coerce: true))
            s.shadowColor = Color.black.nsColor
            #if os(iOS) || os(watchOS) || os(tvOS)
            s.shadowOffset = CGSize(width: w, height: h)
            #elseif os(macOS)
            s.shadowOffset = NSSize(width: w, height: h)
            #endif
            tstyle.attributes[key] = s
          case .pair(let size, .pair(let radius, .pair(let col, .null))):
            guard case .pair(.flonum(let w), .flonum(let h)) = size else {
              throw RuntimeError.eval(.invalidSize, size)
            }
            let s = NSShadow()
            s.shadowBlurRadius = CGFloat(try radius.asDouble(coerce: true))
            s.shadowColor = try self.color(from: col).nsColor
            #if os(iOS) || os(watchOS) || os(tvOS)
            s.shadowOffset = CGSize(width: w, height: h)
            #elseif os(macOS)
            s.shadowOffset = NSSize(width: w, height: h)
            #endif
            tstyle.attributes[key] = s
          default:
            throw RuntimeError.eval(.invalidShadowSpec, value)
        }
      #if os(macOS)
      case .superscript:
        tstyle.attributes[key] = NSNumber(value: try value.asInt(above: -101, below: 101))
      #endif
      default:
        throw RuntimeError.eval(.unknownTextStyleAttribute, .symbol(tattr))
    }
  }
  
  private func textStyleSet(style: Expr, attr: Expr, value: Expr) throws -> Expr {
    try self.textStyleSet(tstyle: try self.textStyle(from: style),
                          tattr: try attr.asSymbol(),
                          value: value)
    return .void
  }
  
  private func textStyleMerge(style: Expr, args: Arguments) throws -> Expr {
    let tstyle = try self.textStyle(from: style)
    for arg in args {
      let astyle = try self.textStyle(from: arg)
      for (key, val) in astyle.attributes {
        tstyle.attributes[key] = val
      }
    }
    return .void
  }
  
  private func textStyleRemove(style: Expr, attr: Expr) throws -> Expr {
    let tstyle = try self.textStyle(from: style)
    if let key = self.attributeKey(from: try attr.asSymbol()) {
      tstyle.attributes.removeValue(forKey: key)
    } else {
      throw RuntimeError.eval(.unknownTextStyleAttribute, attr)
    }
    return .void
  }
  
  private func textStyleAttributes(style: Expr) throws -> Expr {
    let tstyle = try self.textStyle(from: style)
    var res = Expr.null
    for (key, val) in tstyle.attributes {
      if let keySym = self.symbol(from: key),
         let valExpr = try self.textStyleValue(key: key, value: val) {
        res = .pair(.pair(.symbol(keySym), valExpr), res)
      }
    }
    return res
  }
  
  private func attributeKey(from sym: Symbol) -> NSAttributedString.Key? {
    switch sym {
      case self.backgroundColor:
        return .backgroundColor
      case self.baselineOffset:
        return .baselineOffset
      case self.expansion:
        return .expansion
      case self.font:
        return .font
      case self.foregroundColor:
        return .foregroundColor
      case self.kern:
        return .kern
      case self.ligature:
        return .ligature
      case self.link:
        return .link
      case self.obliqueness:
        return .obliqueness
      case self.paragraphStyle:
        return .paragraphStyle
      case self.shadow:
        return .shadow
      case self.strikethroughColor:
        return .strikethroughColor
      case self.strokeColor:
        return .strokeColor
      case self.strokeWidth:
        return .strokeWidth
      case self.underlineColor:
        return .underlineColor
      #if os(macOS)
      case self.superscript:
        return .superscript
      #endif
      default:
        return nil
    }
  }
  
  private func symbol(from attrib: NSAttributedString.Key) -> Symbol? {
    switch attrib {
      case .backgroundColor:
        return self.backgroundColor
      case .baselineOffset:
        return self.baselineOffset
      case .expansion:
        return self.expansion
      case .font:
        return self.font
      case .foregroundColor:
        return self.foregroundColor
      case .kern:
        return self.kern
      case .ligature:
        return self.ligature
      case .link:
        return self.link
      case .obliqueness:
        return self.obliqueness
      case .paragraphStyle:
        return self.paragraphStyle
      case .shadow:
        return self.shadow
      case .strikethroughColor:
        return self.strikethroughColor
      case .strokeColor:
        return self.strokeColor
      case .strokeWidth:
        return self.strokeWidth
      case .underlineColor:
        return self.underlineColor
      #if os(macOS)
      case .superscript:
        return self.superscript
      #endif
      default:
        return nil
    }
  }
  
  // MARK: - Text alignment
  
  private func textAlignment(from expr: Expr) throws -> NSTextAlignment {
    if expr.isFalse {
      return .natural
    }
    switch try expr.asSymbol() {
      case self.left:
        return .left
      case self.right:
        return .right
      case self.center:
        return .center
      case self.justified:
        return .justified
      case self.natural:
        return .natural
      default:
        throw RuntimeError.eval(.unknownTextAlignment, expr)
    }
  }
  
  private func expr(from alignment: NSTextAlignment) -> Expr {
    switch alignment {
      case .left:
        return .symbol(self.left)
      case .right:
        return .symbol(self.right)
      case .center:
        return .symbol(self.center)
      case .justified:
        return .symbol(self.justified)
      case .natural:
        return .symbol(self.natural)
      @unknown default:
        return .false
    }
  }
  
  // MARK: - Text block styles
  
  private func percentage(expr: Expr) throws -> Expr {
    return .pair(.flonum(try expr.asDouble(coerce: true)), .symbol(self.percent))
  }
  
  private func isPercentage(obj: Expr) throws -> Expr {
    guard case .pair(.flonum(_), .symbol(self.percent)) = obj else {
      return .false
    }
    return .true
  }
  
  private func isTextBlockStyle(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is TextBlockStyle {
      return .true
    }
    return .false
  }
  
  private func makeTextBlockStyle(args: Arguments) throws -> Expr {
    let tbstyle = TextBlockStyle()
    var i = args.startIndex
    while i < args.endIndex {
      guard case .symbol(let keyword) = args[i],
            keyword.identifier.hasSuffix(":") else {
        throw RuntimeError.eval(.expectedTextBlockStyleAttribute, args[i])
      }
      i = args.index(after: i)
      if i < args.endIndex {
        var tbattr = keyword.identifier
        tbattr.removeLast()
        try self.textBlockStyleSet(tbstyle: tbstyle,
                                   tbattr: self.context.symbols.intern(tbattr),
                                   value: args[i])
        i = args.index(after: i)
      } else {
        break
      }
    }
    return .object(tbstyle)
  }
  
  private func copyTextBlockStyle(style: Expr) throws -> Expr {
    let newStyle = TextBlockStyle()
    newStyle.copy(from: try self.textBlockStyle(from: style))
    return .object(newStyle)
  }
  
  private func textBlockStyleEquals(style: Expr, args: Arguments) throws -> Expr {
    let tbstyle = try self.textBlockStyle(from: style)
    for arg in args {
      if !tbstyle.equals(to: try self.textBlockStyle(from: arg)) {
        return .false
      }
    }
    return .true
  }
  
  private func valueExpr(_ value: TextBlockStyle.Value) -> Expr {
    switch value {
      case .undefined:
        return .false
      case .points(let x):
        return .flonum(x)
      case .percentage(let x):
        return .pair(.flonum(x), .symbol(self.percent))
    }
  }
  
  private func offsetsExpr(_ off: TextBlockStyle.Offsets) -> Expr {
    if off.left == off.right && off.left == off.top && off.left == off.bottom {
      return self.valueExpr(off.left)
    } else {
      return .pair(self.valueExpr(off.left),
                   .pair(self.valueExpr(off.right),
                         .pair(self.valueExpr(off.top),
                               .pair(self.valueExpr(off.bottom), .null))))
    }
  }
  
  private func textBlockStyleRef(style: Expr, attr: Expr) throws -> Expr {
    let tbstyle = try self.textBlockStyle(from: style)
    let tbattr = try attr.asSymbol()
    switch tbattr {
      case self.width:
        if tbstyle.width.minimum == .undefined && tbstyle.width.maximum == .undefined {
          return self.valueExpr(tbstyle.width.value)
        } else {
          return .pair(self.valueExpr(tbstyle.width.minimum),
                       self.valueExpr(tbstyle.width.maximum))
        }
      case self.height:
        if tbstyle.height.minimum == .undefined && tbstyle.height.maximum == .undefined {
          return self.valueExpr(tbstyle.height.value)
        } else {
          return .pair(self.valueExpr(tbstyle.height.minimum),
                       self.valueExpr(tbstyle.height.maximum))
        }
      case self.margin:
        return self.offsetsExpr(tbstyle.margin)
      case self.border:
        return self.offsetsExpr(tbstyle.border)
      case self.padding:
        return self.offsetsExpr(tbstyle.padding)
      case self.backgroundColor:
        if let col = tbstyle.backgroundColor {
          return .object(col)
        } else {
          return .false
        }
      case self.borderColor:
        if tbstyle.borderColorLeft == tbstyle.borderColorRight &&
           tbstyle.borderColorLeft == tbstyle.borderColorTop &&
           tbstyle.borderColorLeft == tbstyle.borderColorBottom {
          if let col = tbstyle.borderColorLeft {
            return .object(col)
          } else {
            return .false
          }
        } else {
          let left = tbstyle.borderColorLeft == nil ? Expr.false : .object(tbstyle.borderColorLeft!)
          let right = tbstyle.borderColorRight == nil ? Expr.false
                                                      : .object(tbstyle.borderColorRight!)
          let top = tbstyle.borderColorTop == nil ? Expr.false : .object(tbstyle.borderColorTop!)
          let bottom = tbstyle.borderColorBottom == nil ? Expr.false
                                                        : .object(tbstyle.borderColorBottom!)
          return .pair(left, .pair(right, .pair(top, .pair(bottom, .null))))
        }
      case self.verticalAlignment:
        switch tbstyle.verticalAlignment {
          case .undefined:
            return .false
          case .top:
            return .symbol(self.top)
          case .middle:
            return .symbol(self.middle)
          case .bottom:
            return .symbol(self.bottom)
          case .baseline:
            return .symbol(self.baseline)
        }
      default:
        throw RuntimeError.eval(.unknownTextBlockStyleAttribute, attr)
    }
  }
  
  private func textBlockStyleSet(tbstyle: TextBlockStyle,
                                 tbattr: Symbol,
                                 value: Expr) throws {
    switch tbattr {
      case self.width:
        switch value {
          case .false:
            tbstyle.width.reset()
          case .pair(.flonum(let x), .symbol(self.percent)):
            tbstyle.width.value = .percentage(x)
            tbstyle.width.minimum = .undefined
            tbstyle.width.maximum = .undefined
          case .pair(let min, let max):
            tbstyle.width.value = .undefined
            tbstyle.width.minimum = try self.percentageOrPoints(from: min)
            tbstyle.width.maximum = try self.percentageOrPoints(from: max)
          default:
            tbstyle.width.value = .points(try value.asDouble(coerce: true))
            tbstyle.width.minimum = .undefined
            tbstyle.width.maximum = .undefined
        }
      case self.height:
        switch value {
          case .false:
            tbstyle.height.reset()
          case .pair(.flonum(let x), .symbol(self.percent)):
            tbstyle.height.value = .percentage(x)
            tbstyle.height.minimum = .undefined
            tbstyle.height.maximum = .undefined
          case .pair(let min, let max):
            tbstyle.height.value = .undefined
            tbstyle.height.minimum = try self.percentageOrPoints(from: min)
            tbstyle.height.maximum = try self.percentageOrPoints(from: max)
          default:
            tbstyle.height.value = .points(try value.asDouble(coerce: true))
            tbstyle.height.minimum = .undefined
            tbstyle.height.maximum = .undefined
        }
      case self.margin:
        switch value {
          case .false:
            tbstyle.margin.reset()
          case .pair(.flonum(let x), .symbol(self.percent)):
            tbstyle.margin.set(.percentage(x))
          case .pair(let left, .pair(let right, .pair(let top, .pair(let bottom, .null)))):
            tbstyle.margin.left = try self.percentageOrPoints(from: left)
            tbstyle.margin.right = try self.percentageOrPoints(from: right)
            tbstyle.margin.top = try self.percentageOrPoints(from: top)
            tbstyle.margin.bottom = try self.percentageOrPoints(from: bottom)
          case .pair(let horiz, .pair(let vert, .null)):
            tbstyle.margin.set(try self.percentageOrPoints(from: horiz),
                               try self.percentageOrPoints(from: vert))
          default:
            tbstyle.margin.set(.points(try value.asDouble(coerce: true)))
        }
      case self.border:
        switch value {
          case .false:
            tbstyle.border.reset()
          case .pair(.flonum(let x), .symbol(self.percent)):
            tbstyle.border.set(.percentage(x))
          case .pair(let left, .pair(let right, .pair(let top, .pair(let bottom, .null)))):
            tbstyle.border.left = try self.percentageOrPoints(from: left)
            tbstyle.border.right = try self.percentageOrPoints(from: right)
            tbstyle.border.top = try self.percentageOrPoints(from: top)
            tbstyle.border.bottom = try self.percentageOrPoints(from: bottom)
          case .pair(let horiz, .pair(let vert, .null)):
            tbstyle.border.set(try self.percentageOrPoints(from: horiz),
                               try self.percentageOrPoints(from: vert))
          default:
            tbstyle.border.set(.points(try value.asDouble(coerce: true)))
        }
      case self.padding:
        switch value {
          case .false:
            tbstyle.padding.reset()
          case .pair(.flonum(let x), .symbol(self.percent)):
            tbstyle.padding.set(.percentage(x))
          case .pair(let left, .pair(let right, .pair(let top, .pair(let bottom, .null)))):
            tbstyle.padding.left = try self.percentageOrPoints(from: left)
            tbstyle.padding.right = try self.percentageOrPoints(from: right)
            tbstyle.padding.top = try self.percentageOrPoints(from: top)
            tbstyle.padding.bottom = try self.percentageOrPoints(from: bottom)
          case .pair(let horiz, .pair(let vert, .null)):
            tbstyle.padding.set(try self.percentageOrPoints(from: horiz),
                                try self.percentageOrPoints(from: vert))
          default:
            tbstyle.padding.set(.points(try value.asDouble(coerce: true)))
        }
      case self.backgroundColor:
        tbstyle.backgroundColor = value.isTrue ? try self.color(from: value) : nil
      case self.borderColor:
        switch value {
          case .false:
            tbstyle.setBorderColor(nil)
          case .pair(let left, .pair(let right, .pair(let top, .pair(let bottom, .null)))):
            tbstyle.borderColorLeft = left.isTrue ? try self.color(from: left) : nil
            tbstyle.borderColorRight = right.isTrue ? try self.color(from: right) : nil
            tbstyle.borderColorTop = top.isTrue ? try self.color(from: top) : nil
            tbstyle.borderColorBottom = bottom.isTrue ? try self.color(from: bottom) : nil
          case .pair(let horiz, .pair(let vert, .null)):
            tbstyle.setBorderColor(horiz.isTrue ? try self.color(from: horiz) : nil,
                                   vert.isTrue ? try self.color(from: vert) : nil)
          default:
            tbstyle.setBorderColor(value.isTrue ? try self.color(from: value) : nil)
        }
      case self.verticalAlignment:
        guard let alignment = self.verticalAlignment(from: try value.asSymbol()) else {
          throw RuntimeError.eval(.unknownTextAlignment, value)
        }
        tbstyle.verticalAlignment = alignment
      default:
        throw RuntimeError.eval(.unknownTextBlockStyleAttribute, .symbol(tbattr))
    }
  }
  
  private func textBlockStyleSet(style: Expr, attr: Expr, value: Expr) throws -> Expr {
    try self.textBlockStyleSet(tbstyle: try self.textBlockStyle(from: style),
                               tbattr: try attr.asSymbol(),
                               value: value)
    return .void
  }
  
  // MARK: - Paragraph styles
  
  private func isParagraphStyle(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is ParagraphStyle {
      return .true
    }
    return .false
  }
  
  private func makeParagraphStyle(args: Arguments) throws -> Expr {
    let pstyle = NSMutableParagraphStyle()
    var i = args.startIndex
    while i < args.endIndex {
      guard case .symbol(let keyword) = args[i],
            keyword.identifier.hasSuffix(":") else {
        throw RuntimeError.eval(.expectedParagraphStyleAttributeKeyword, args[i])
      }
      i = args.index(after: i)
      if i < args.endIndex {
        var pattr = keyword.identifier
        pattr.removeLast()
        try self.paragraphStyleSet(pstyle: pstyle,
                                   pattr: self.context.symbols.intern(pattr),
                                   value: args[i])
        i = args.index(after: i)
      } else {
        break
      }
    }
    return .object(ParagraphStyle(pstyle))
  }
  
  private func copyParagraphStyle(style: Expr) throws -> Expr {
    let newStyle = NSMutableParagraphStyle()
    newStyle.setParagraphStyle(try self.paragraphStyle(from: style).value)
    return .object(ParagraphStyle(newStyle))
  }
  
  private func paragraphStyleEquals(style: Expr, args: Arguments) throws -> Expr {
    let pstyle = try self.paragraphStyle(from: style).value
    for arg in args {
      #if os(iOS) || os(watchOS) || os(tvOS)
      if !pstyle.isEqual(try self.paragraphStyle(from: arg).value) {
        return .false
      }
      #elseif os(macOS)
      if !pstyle.isEqual(to: try self.paragraphStyle(from: arg).value) {
        return .false
      }
      #endif
    }
    return .true
  }
  
  private func paragraphStyleRef(style: Expr, attr: Expr) throws -> Expr {
    let pstyle = try self.paragraphStyle(from: style).value
    let pattr = try attr.asSymbol()
    switch pattr {
      case self.alignment:
        return self.expr(from: pstyle.alignment)
      case self.firstHeadIndent:
        return .flonum(pstyle.firstLineHeadIndent)
      case self.headIndent:
        return .flonum(pstyle.headIndent)
      case self.tailIndent:
        return .flonum(pstyle.tailIndent)
      case self.lineHeightMultiple:
        return .flonum(pstyle.lineHeightMultiple)
      case self.maximumLineHeight:
        return .flonum(pstyle.maximumLineHeight)
      case self.minimumLineHeight:
        return .flonum(pstyle.minimumLineHeight)
      case self.lineSpacing:
        return .flonum(pstyle.lineSpacing)
      case self.paragraphSpacing:
        return .flonum(pstyle.paragraphSpacing)
      case self.paragraphSpacingBefore:
        return .flonum(pstyle.paragraphSpacingBefore)
      case self.tabInterval:
        return .flonum(pstyle.defaultTabInterval)
      case self.textFitMode:
        switch pstyle.lineBreakMode {
          case .byWordWrapping:
            return .symbol(self.wordWrap)
          case .byCharWrapping:
            return .symbol(self.charWrap)
          case .byClipping:
            return .symbol(self.clip)
          case .byTruncatingHead:
            return .symbol(self.truncateHead)
          case .byTruncatingTail:
            return .symbol(self.truncateTail)
          case .byTruncatingMiddle:
            return .symbol(self.truncate)
          @unknown default:
            return .false
        }
      case self.pushOutLineBreak:
        return .makeBoolean(pstyle.lineBreakStrategy.contains(.pushOut))
      case self.hyphenationFactor:
        return .flonum(Double(pstyle.hyphenationFactor))
      case self.writingDirection:
        switch pstyle.baseWritingDirection {
          case .natural:
            return .symbol(self.natural)
          case .leftToRight:
            return .symbol(self.leftToRight)
          case .rightToLeft:
            return .symbol(self.rightToLeft)
          @unknown default:
            return .false
        }
      default:
        throw RuntimeError.eval(.unknownParagraphStyleAttribute, attr)
    }
  }
  
  private func paragraphStyleSet(pstyle: NSMutableParagraphStyle,
                                 pattr: Symbol,
                                 value: Expr) throws {
    switch pattr {
      case self.alignment:
        pstyle.alignment = try self.textAlignment(from: value)
      case self.firstHeadIndent:
        pstyle.firstLineHeadIndent = try value.asDouble(coerce: true)
      case self.headIndent:
        pstyle.headIndent = try value.asDouble(coerce: true)
      case self.tailIndent:
        pstyle.tailIndent = try value.asDouble(coerce: true)
      case self.lineHeightMultiple:
        pstyle.lineHeightMultiple = try value.asDouble(coerce: true)
      case self.maximumLineHeight:
        pstyle.maximumLineHeight = try value.asDouble(coerce: true)
      case self.minimumLineHeight:
        pstyle.minimumLineHeight = try value.asDouble(coerce: true)
      case self.lineSpacing:
        pstyle.lineSpacing = try value.asDouble(coerce: true)
      case self.paragraphSpacing:
        pstyle.paragraphSpacing = try value.asDouble(coerce: true)
      case self.paragraphSpacingBefore:
        pstyle.paragraphSpacingBefore = try value.asDouble(coerce: true)
      case self.tabInterval:
        pstyle.defaultTabInterval = try value.asDouble(coerce: true)
      case self.textFitMode:
        let sym = try value.asSymbol()
        switch sym {
          case self.wordWrap:
            pstyle.lineBreakMode = .byWordWrapping
          case self.charWrap:
            pstyle.lineBreakMode = .byCharWrapping
          case self.clip:
            pstyle.lineBreakMode = .byClipping
          case self.truncateHead:
            pstyle.lineBreakMode = .byTruncatingHead
          case self.truncateTail:
            pstyle.lineBreakMode = .byTruncatingTail
          case self.truncate:
            pstyle.lineBreakMode = .byTruncatingMiddle
          default:
            throw RuntimeError.eval(.unknownTextFitMode, .symbol(sym))
        }
      case self.pushOutLineBreak:
        if value.isTrue {
          pstyle.lineBreakStrategy.insert(.pushOut)
        } else {
          pstyle.lineBreakStrategy.remove(.pushOut)
        }
      case self.hyphenationFactor:
        pstyle.hyphenationFactor = Float(try value.asDouble(coerce: true))
      case self.writingDirection:
        let sym = try value.asSymbol()
        switch sym {
          case self.natural:
            pstyle.baseWritingDirection = .natural
          case self.leftToRight:
            pstyle.baseWritingDirection = .leftToRight
          case self.rightToLeft:
            pstyle.baseWritingDirection = .rightToLeft
          default:
            throw RuntimeError.eval(.unknownWritingDirection, .symbol(sym))
        }
      default:
        throw RuntimeError.eval(.unknownParagraphStyleAttribute, .symbol(pattr))
    }
  }
  
  private func paragraphStyleSet(style: Expr, attr: Expr, value: Expr) throws -> Expr {
    try self.paragraphStyleSet(pstyle: try self.paragraphStyle(from: style).value,
                               pattr: try attr.asSymbol(),
                               value: value)
    return .void
  }
  
  private func paragraphStyleTabstops(style: Expr) throws -> Expr {
    let pstyle = try self.paragraphStyle(from: style).value
    var res = Expr.null
    for tabstop in pstyle.tabStops.reversed() {
      let opt: Expr
      if let nscs = tabstop.options[.columnTerminators] as? NSCharacterSet {
        opt = .pair(.object(CharSet.convert(nscs: nscs)), .null)
      } else {
        opt = .null
      }
      res = .pair(.pair(.flonum(tabstop.location),
                        .pair(self.expr(from: tabstop.alignment), opt)), res)
    }
    return res
  }
  
  private func paragraphStyleTabstopAdd(style: Expr,
                                        loc: Expr,
                                        align: Expr?,
                                        cset: Expr?) throws -> Expr {
    let pstyle = try self.paragraphStyle(from: style).value
    let location = try loc.asDouble(coerce: true)
    let alignment = try self.textAlignment(from: align ?? .symbol(self.left))
    if let cset = cset {
      guard case .object(let obj) = cset, let cs = obj as? CharSet else {
        throw RuntimeError.type(cset, expected: [CharSet.type])
      }
      let nscs = NSCharacterSet(charactersIn: String(utf16CodeUnits: cs.array, count: cs.count))
      pstyle.addTabStop(NSTextTab(textAlignment: alignment,
                                  location: location,
                                  options: [.columnTerminators : nscs]))
    } else {
      pstyle.addTabStop(NSTextTab(textAlignment: alignment, location: location))
    }
    return .void
  }
  
  private func paragraphStyleTabstopRemove(style: Expr,
                                           loc: Expr,
                                           align: Expr?) throws -> Expr {
    let pstyle = try self.paragraphStyle(from: style).value
    let location = try loc.asDouble(coerce: true)
    let alignment = try self.textAlignment(from: align ?? .symbol(self.left))
    pstyle.removeTabStop(NSTextTab(textAlignment: alignment, location: location))
    return .void
  }
}

public final class StyledText: AnyNativeObject<NSMutableAttributedString> {

  /// Type representing attributed strings
  public static let type = Type.objectType(Symbol(uninterned: "styled-text"))

  public override var type: Type {
    return StyledText.type
  }
}

public final class TextBlockStyle: NativeObject {
  
  public enum VerticalAlignment: Equatable {
    case undefined
    case top
    case middle
    case bottom
    case baseline
  }
  
  public enum Value: Equatable {
    case undefined
    case points(CGFloat)
    case percentage(CGFloat)
  }

  public struct Dimension: Equatable {
    var value: Value = .undefined
    var minimum: Value = .undefined
    var maximum: Value = .undefined
    
    mutating func reset() {
      self.value = .undefined
      self.minimum = .undefined
      self.maximum = .undefined
    }
  }

  public struct Offsets: Equatable {
    var top: Value = .undefined
    var bottom: Value = .undefined
    var left: Value = .undefined
    var right: Value = .undefined
    
    mutating func reset() {
      self.top = .undefined
      self.bottom = .undefined
      self.left = .undefined
      self.right = .undefined
    }
    
    mutating func set(_ val: Value) {
      self.top = val
      self.bottom = val
      self.left = val
      self.right = val
    }
    
    mutating func set(_ horiz: Value, _ vert: Value) {
      self.top = vert
      self.bottom = vert
      self.left = horiz
      self.right = horiz
    }
  }
  
  /// Type representing text block styles.
  public static let type = Type.objectType(Symbol(uninterned: "text-block-style"))
  
  /// Text block style attributes.
  public var width = Dimension()
  public var height = Dimension()
  public var margin = Offsets()
  public var border = Offsets()
  public var padding = Offsets()
  
  public var verticalAlignment: VerticalAlignment = .undefined
  public var backgroundColor: Color? = nil
  public var borderColorTop: Color? = nil
  public var borderColorBottom: Color? = nil
  public var borderColorLeft: Color? = nil
  public var borderColorRight: Color? = nil
  
  /// Return the native object type.
  public override var type: Type {
    return Self.type
  }
  
  public func setBorderColor(_ col: Color?) {
    self.borderColorTop = col
    self.borderColorBottom = col
    self.borderColorLeft = col
    self.borderColorRight = col
  }
  
  public func setBorderColor(_ horiz: Color?, _ vert: Color?) {
    self.borderColorTop = vert
    self.borderColorBottom = vert
    self.borderColorLeft = horiz
    self.borderColorRight = horiz
  }
  
  public func copy(from other: TextBlockStyle) {
    self.width = other.width
    self.height = other.height
    self.margin = other.margin
    self.border = other.border
    self.padding = other.padding
    self.verticalAlignment = other.verticalAlignment
    self.backgroundColor = other.backgroundColor
    self.borderColorTop = other.borderColorTop
    self.borderColorBottom = other.borderColorBottom
    self.borderColorLeft = other.borderColorLeft
    self.borderColorRight = other.borderColorRight
  }
  
  public func equals(to other: TextBlockStyle) -> Bool {
    return self.width == other.width &&
           self.height == other.height &&
           self.margin == other.margin &&
           self.border == other.border &&
           self.padding == other.padding &&
           self.verticalAlignment == other.verticalAlignment &&
           self.backgroundColor == other.backgroundColor &&
           self.borderColorTop == other.borderColorTop &&
           self.borderColorBottom == other.borderColorBottom &&
           self.borderColorLeft == other.borderColorLeft &&
           self.borderColorRight == other.borderColorRight
  }
  
  // Unfortunately, tables are only supported in macOS (and iOS still does not allow
  // tables in attributed strings and RTF documents)
  #if os(macOS)
  public func initialize(_ block: NSTextBlock) {
    func setValue(_ val: Value, for dim: NSTextBlock.Dimension) {
      switch val {
        case .undefined:
          break
        case .points(let x):
          block.setValue(x, type: .absoluteValueType, for: dim)
        case .percentage(let x):
          block.setValue(x, type: .percentageValueType, for: dim)
      }
    }
    func setWidth(_ val: Value, for layer: NSTextBlock.Layer, edge: NSRectEdge) {
      switch val {
        case .undefined:
          break
        case .points(let x):
          block.setWidth(x, type: .absoluteValueType, for: layer, edge: edge)
        case .percentage(let x):
          block.setWidth(x, type: .percentageValueType, for: layer, edge: edge)
      }
    }
    func setOffsets(_ offsets: Offsets, for layer: NSTextBlock.Layer) {
      setWidth(offsets.left, for: layer, edge: .minX)
      setWidth(offsets.right, for: layer, edge: .maxX)
      setWidth(offsets.top, for: layer, edge: .minY)
      setWidth(offsets.bottom, for: layer, edge: .maxY)
    }
    setValue(self.width.value, for: .width)
    setValue(self.width.minimum, for: .minimumWidth)
    setValue(self.width.maximum, for: .maximumWidth)
    setValue(self.height.value, for: .height)
    setValue(self.height.minimum, for: .minimumHeight)
    setValue(self.height.maximum, for: .maximumHeight)
    setOffsets(self.margin, for: .margin)
    setOffsets(self.border, for: .border)
    setOffsets(self.padding, for: .padding)
    switch self.verticalAlignment {
      case .undefined:
        break
      case .top:
        block.verticalAlignment = .topAlignment
      case .bottom:
        block.verticalAlignment = .bottomAlignment
      case .middle:
        block.verticalAlignment = .middleAlignment
      case .baseline:
        block.verticalAlignment = .baselineAlignment
    }
    if let color = self.backgroundColor?.nsColor {
      block.backgroundColor = color
    }
    if let color = self.borderColorTop?.nsColor {
      block.setBorderColor(color, for: .minY)
    }
    if let color = self.borderColorBottom?.nsColor {
      block.setBorderColor(color, for: .maxY)
    }
    if let color = self.borderColorLeft?.nsColor {
      block.setBorderColor(color, for: .minX)
    }
    if let color = self.borderColorRight?.nsColor {
      block.setBorderColor(color, for: .maxX)
    }
  }
  #endif
}

public final class TextStyle: NativeObject {

  /// Type representing text styles.
  public static let type = Type.objectType(Symbol(uninterned: "text-style"))
  
  /// Text style attributes.
  public var attributes: [NSAttributedString.Key : Any] = [:]
  
  /// Return the native object type.
  public override var type: Type {
    return Self.type
  }
  
  private func keyName(for attrib: NSAttributedString.Key) -> String? {
    switch attrib {
      case .backgroundColor:
        return "background-color"
      case .baselineOffset:
        return "baseline-offset"
      case .expansion:
        return "expansion"
      case .font:
        return "font"
      case .foregroundColor:
        return "foreground-color"
      case .kern:
        return "kern"
      case .ligature:
        return "ligature"
      case .link:
        return "link"
      case .obliqueness:
        return "obliqueness"
      case .paragraphStyle:
        return "paragraph-style"
      case .shadow:
        return "shadow"
      case .strikethroughColor:
        return "strikethrough-color"
      case .strokeColor:
        return "stroke-color"
      case .strokeWidth:
        return "stroke-width"
      case .underlineColor:
        return "underline-color"
      #if os(macOS)
      case .superscript:
        return "superscript"
      #endif
      default:
        return nil
    }
  }
  
  /// Return string representation of native object.
  public override var string: String {
    var res = ""
    var opt = false
    for (key, _) in self.attributes {
      if let keyName = self.keyName(for: key) {
        if !res.isEmpty {
          res += ","
        }
        res += keyName
      } else {
        opt = true
      }
    }
    if opt {
      if !res.isEmpty {
        res += ","
      }
      res += "*"
    }
    return "#<text-style \(res)>"
  }
}

public final class ParagraphStyle: AnyNativeObject<NSMutableParagraphStyle> {

  /// Type representing paragraph styles
  public static let type = Type.objectType(Symbol(uninterned: "paragraph-style"))

  public override var type: Type {
    return ParagraphStyle.type
  }
}
