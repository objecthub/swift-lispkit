//
//  RegExpLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 12/01/2019.
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

public final class RegexpLibrary: NativeLibrary {
  
  /// Regular expression pattern matching options
  private let caseInsensitive: Symbol
  private let allowComments: Symbol
  private let ignoreMeta: Symbol
  private let dotMatchesLineSeparators: Symbol
  private let anchorsMatchLines: Symbol
  private let unixOnlyLineSeparators: Symbol
  private let unicodeWords: Symbol
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    self.caseInsensitive = context.symbols.intern("case-insensitive")
    self.allowComments = context.symbols.intern("allow-comments")
    self.ignoreMeta = context.symbols.intern("ignore-meta")
    self.dotMatchesLineSeparators = context.symbols.intern("dot-matches-line-separator")
    self.anchorsMatchLines = context.symbols.intern("anchors-match-lines")
    self.unixOnlyLineSeparators = context.symbols.intern("unix-only-line-separators")
    self.unicodeWords = context.symbols.intern("unicode-words")
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "regexp"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "control"], "let", "let-optionals", "cond", "if")
    self.`import`(from: ["lispkit", "core"],    "define", "lambda", "and")
    self.`import`(from: ["lispkit", "list"],    "cdar")
    self.`import`(from: ["lispkit", "math"],    "fx1+", "fx<", "fx=")
    self.`import`(from: ["lispkit", "string"],  "string-length")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define("regexp-type-tag", as: NativeRegularExpression.type.objectTypeTag())
    self.define(Procedure("regexp?", isRegexp))
    self.define(Procedure("regexp", regexp))
    self.define(Procedure("regexp-pattern", regexpPattern))
    self.define(Procedure("regexp-capture-groups", regexpCaptureGroups))
    self.define(Procedure("escape-regexp-pattern", escapeRegexpPattern))
    self.define(Procedure("escape-regexp-template", escapeRegexpTemplate))
    self.define(Procedure("regexp-matches", regexpMatches))
    self.define(Procedure("regexp-matches?", isRegexpMatches))
    self.define(Procedure("regexp-search", regexpSearch))
    self.define(Procedure("regexp-search-all", regexpSearchAll))
    self.define(Procedure("regexp-extract", regexpExtract))
    self.define(Procedure("regexp-split", regexpSplit))
    self.define(Procedure("regexp-partition", regexpPartition))
    self.define(Procedure("regexp-replace", regexpReplace))
    self.define(Procedure("regexp-replace!", regexpReplaceDestructive))
    self.define("regexp-fold", via: """
      (define (regexp-fold rx kons knil str . args)
        (let-optionals args ((finish (lambda (from md str acc) acc))
                             (start 0)
                             (end (string-length str)))
          (let lp ((i start)
                   (from start)
                   (acc knil))
            (cond
              ((and (fx< i end) (regexp-search rx str i end))
                => (lambda (md)
                     (let ((j (cdar md)))
                       (lp (if (and (fx= i j) (fx< j end)) (fx1+ j) j)
                           j
                           (kons from md str acc)))))
              (else
                (finish from #f str acc))))))
    """)
  }
  
  private func isRegexp(_ expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is NativeRegularExpression else {
      return .false
    }
    return .true
  }
  
  private func regexp(_ expr: Expr, _ args: Arguments) throws -> Expr {
    var options: NSRegularExpression.Options = []
    for arg in args {
      guard case .symbol(let sym) = arg else {
        throw RuntimeError.eval(.invalidRegexpMatchingOption, arg)
      }
      switch sym {
        case self.caseInsensitive:
          options.insert(.caseInsensitive)
        case self.allowComments:
          options.insert(.allowCommentsAndWhitespace)
        case self.ignoreMeta:
          options.insert(.ignoreMetacharacters)
        case self.dotMatchesLineSeparators:
          options.insert(.dotMatchesLineSeparators)
        case self.anchorsMatchLines:
          options.insert(.anchorsMatchLines)
        case self.unixOnlyLineSeparators:
          options.insert(.useUnixLineSeparators)
        case self.unicodeWords:
          options.insert(.useUnicodeWordBoundaries)
        default:
          throw RuntimeError.eval(.invalidRegexpMatchingOption, arg)
      }
    }
    return .object(NativeRegularExpression(try NSRegularExpression(pattern: expr.asString(),
                                                                   options: options)))
  }
  
  private func regexpPattern(_ expr: Expr) throws -> Expr {
    return .makeString(try self.asRegexp(expr).pattern)
  }
  
  private func regexpCaptureGroups(_ expr: Expr) throws -> Expr {
    return .fixnum(Int64(try self.asRegexp(expr).numberOfCaptureGroups))
  }
  
  private func escapeRegexpPattern(_ expr: Expr) throws -> Expr {
    return .makeString(NSRegularExpression.escapedPattern(for: try expr.asString()))
  }
  
  private func escapeRegexpTemplate(_ expr: Expr) throws -> Expr {
    return .makeString(NSRegularExpression.escapedTemplate(for: try expr.asString()))
  }
  
  private func regexpMatches(_ expr: Expr,
                             _ str: Expr,
                             _ start: Expr?,
                             _ end: Expr?) throws -> Expr {
    let re = try self.asRegexp(expr)
    let ms = try str.asMutableStr()
    let end = try (end ?? Expr.fixnum(Int64(ms.length))).asInt(below: ms.length + 1)
    let start = try (start ?? .fixnum(0)).asInt(below: end + 1)
    guard end > start else {
      return .false
    }
    guard let match = re.firstMatch(in: ms as String,
                                    options: .withoutAnchoringBounds,
                                    range: NSRange(location: start, length: end - start)) else {
      return .false
    }
    guard match.range.location == start && match.range.length == (end - start) else {
      return .false
    }
    return self.expr(from: match)
  }
  
  private func isRegexpMatches(_ expr: Expr,
                               _ str: Expr,
                               _ start: Expr?,
                               _ end: Expr?) throws -> Expr {
    let re = try self.asRegexp(expr)
    let ms = try str.asMutableStr()
    let end = try (end ?? Expr.fixnum(Int64(ms.length))).asInt(below: ms.length + 1)
    let start = try (start ?? .fixnum(0)).asInt(below: end + 1)
    guard end > start else {
      return .false
    }
    guard let match = re.firstMatch(in: ms as String,
                                    options: .withoutAnchoringBounds,
                                    range: NSRange(location: start, length: end - start)) else {
      return .false
    }
    guard match.range.location == start && match.range.length == (end - start) else {
      return .false
    }
    return .true
  }
  
  private func regexpSearch(_ expr: Expr,
                            _ str: Expr,
                            _ start: Expr?,
                            _ end: Expr?) throws -> Expr {
    let re = try self.asRegexp(expr)
    let ms = try str.asMutableStr()
    let end = try (end ?? Expr.fixnum(Int64(ms.length))).asInt(below: ms.length + 1)
    let start = try (start ?? .fixnum(0)).asInt(below: end + 1)
    guard end > start else {
      return .false
    }
    guard let match = re.firstMatch(in: ms as String,
                                    options: .withoutAnchoringBounds,
                                    range: NSRange(location: start, length: end - start)) else {
      return .false
    }
    return self.expr(from: match)
  }
  
  private func regexpSearchAll(_ expr: Expr,
                               _ str: Expr,
                               _ start: Expr?,
                               _ end: Expr?) throws -> Expr {
    let re = try self.asRegexp(expr)
    let ms = try str.asMutableStr()
    let end = try (end ?? Expr.fixnum(Int64(ms.length))).asInt(below: ms.length + 1)
    let start = try (start ?? .fixnum(0)).asInt(below: end + 1)
    guard end > start else {
      return .false
    }
    let matches = re.matches(in: ms as String,
                             options: .withoutAnchoringBounds,
                             range: NSRange(location: start, length: end - start))
    var res = Expr.null
    for match in matches.reversed() {
      res = .pair(self.expr(from: match), res)
    }
    return res
  }
  
  private func regexpExtract(_ expr: Expr,
                             _ str: Expr,
                             _ start: Expr?,
                             _ end: Expr?) throws -> Expr {
    let re = try self.asRegexp(expr)
    let ms = try str.asMutableStr()
    let end = try (end ?? Expr.fixnum(Int64(ms.length))).asInt(below: ms.length + 1)
    let start = try (start ?? .fixnum(0)).asInt(below: end + 1)
    guard end > start else {
      return .null
    }
    let matches = re.matches(in: ms as String,
                             options: .withoutAnchoringBounds,
                             range: NSRange(location: start, length: end - start))
    var res = Expr.null
    for match in matches.reversed() {
      if match.range.length > 0 {
        res = .pair(.makeString(ms.substring(with: match.range)), res)
      }
    }
    return res
  }
  
  private func regexpSplit(_ expr: Expr,
                           _ str: Expr,
                           _ start: Expr?,
                           _ end: Expr?) throws -> Expr {
    let re = try self.asRegexp(expr)
    let ms = try str.asMutableStr()
    let end = try (end ?? Expr.fixnum(Int64(ms.length))).asInt(below: ms.length + 1)
    let start = try (start ?? .fixnum(0)).asInt(below: end + 1)
    guard end > start else {
      return .pair(.makeString(ms as String), .null)
    }
    let matches = re.matches(in: ms as String,
                             options: .withoutAnchoringBounds,
                             range: NSRange(location: start, length: end - start))
    var res = Expr.null
    var last = ms.length
    for match in matches.reversed() {
      if match.range.length > 0 {
        res = .pair(.makeString(ms.substring(with:
          NSRange(location: match.range.location + match.range.length,
                  length: last - match.range.location - match.range.length))), res)
        last = match.range.location
      }
    }
    res = .pair(.makeString(ms.substring(with: NSRange(location: 0, length: last))), res)
    return res
  }
  
  private func regexpPartition(_ expr: Expr,
                               _ str: Expr,
                               _ start: Expr?,
                               _ end: Expr?) throws -> Expr {
    let re = try self.asRegexp(expr)
    let ms = try str.asMutableStr()
    let end = try (end ?? Expr.fixnum(Int64(ms.length))).asInt(below: ms.length + 1)
    let start = try (start ?? .fixnum(0)).asInt(below: end + 1)
    guard end > start else {
      return .pair(.makeString(ms as String), .null)
    }
    let matches = re.matches(in: ms as String,
                             options: .withoutAnchoringBounds,
                             range: NSRange(location: start, length: end - start))
    var res = Expr.null
    var last = ms.length
    for match in matches.reversed() {
      if match.range.length > 0 {
        res = .pair(.makeString(ms.substring(with:
          NSRange(location: match.range.location + match.range.length,
                  length: last - match.range.location - match.range.length))), res)
        res = .pair(.makeString(ms.substring(with: match.range)), res)
        last = match.range.location
      }
    }
    res = .pair(.makeString(ms.substring(with: NSRange(location: 0, length: last))), res)
    return res
  }
  
  private func regexpReplace(_ expr: Expr,
                             _ str: Expr,
                             _ templ: Expr,
                             _ start: Expr?,
                             _ end: Expr?) throws -> Expr {
    let re = try self.asRegexp(expr)
    let ms = try str.asMutableStr()
    let end = try (end ?? Expr.fixnum(Int64(ms.length))).asInt(below: ms.length + 1)
    let start = try (start ?? .fixnum(0)).asInt(below: end + 1)
    guard end > start else {
      return .makeString(ms as String)
    }
    let res = re.stringByReplacingMatches(in: ms as String,
                                          options: .withoutAnchoringBounds,
                                          range: NSRange(location: start, length: end - start),
                                          withTemplate: try templ.asString())
    return .makeString(res)
  }
  
  private func regexpReplaceDestructive(_ expr: Expr,
                                        _ str: Expr,
                                        _ templ: Expr,
                                        _ start: Expr?,
                                        _ end: Expr?) throws -> Expr {
    let re = try self.asRegexp(expr)
    let ms = try str.asMutableStr()
    let end = try (end ?? Expr.fixnum(Int64(ms.length))).asInt(below: ms.length + 1)
    let start = try (start ?? .fixnum(0)).asInt(below: end + 1)
    guard end > start else {
      return .makeString(ms as String)
    }
    let res = re.replaceMatches(in: ms,
                                options: .withoutAnchoringBounds,
                                range: NSRange(location: start, length: end - start),
                                withTemplate: try templ.asString())
    return .makeNumber(res)
  }
  
  private func expr(from match: NSTextCheckingResult) -> Expr {
    var res = Expr.null
    for i in (0..<match.numberOfRanges).reversed() {
      let range = match.range(at: i)
      res = .pair(.pair(.fixnum(Int64(range.location)),
                        .fixnum(Int64(range.location + range.length))),
                  res)
    }
    return res
  }
  
  private func asRegexp(_ expr: Expr) throws -> NSRegularExpression {
    guard case .object(let obj) = expr, let box = obj as? NativeRegularExpression else {
      throw RuntimeError.type(expr, expected: [NativeRegularExpression.type])
    }
    return box.value
  }
}

public final class NativeRegularExpression: AnyNativeObject<NSRegularExpression> {

  /// Type representing regular expressions.
  public static let type = Type.objectType(Symbol(uninterned: "regexp"))

  public override var type: Type {
    return NativeRegularExpression.type
  }

  public override var string: String {
    return "#<regexp \"\(Expr.escapeStr(self.value.pattern))\">"
  }
  
  public override func unpack(in context: Context) -> Exprs {
    return [.makeString(Expr.escapeStr(self.value.pattern))]
  }
}
