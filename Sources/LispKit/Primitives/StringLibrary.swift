//
//  StringLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 22/01/2016.
//  Copyright © 2016, 2017 ObjectHub. All rights reserved.
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

public final class StringLibrary: NativeLibrary {
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "string"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "control"], "let", "let*", "do", "unless", "when", "if")
    self.`import`(from: ["lispkit", "core"],    "define", "set!", "or", "not", "apply")
    self.`import`(from: ["lispkit", "list"],    "cons", "null?")
    self.`import`(from: ["lispkit", "math"],    "fx1+", "fx1-", "fx=", "fx>", "fx<", "fx<=", "fx>=")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("string?", isString))
    self.define(Procedure("string-empty?", isStringEmpty))
    self.define(Procedure("make-string", makeString))
    self.define(Procedure("read-file", readFile))
    self.define(Procedure("write-file", writeFile))
    self.define(Procedure("string", string))
    self.define(Procedure("string-ref", stringRef))
    self.define(Procedure("string-set!", stringSet))
    self.define(Procedure("string-length", stringLength))
    self.define(Procedure("string-append", stringAppend))
    self.define(Procedure("string-concatenate", stringConcatenate))
    self.define(Procedure("string=?", stringEquals))
    self.define(Procedure("string<?", stringLessThan))
    self.define(Procedure("string<=?", stringLessThanEquals))
    self.define(Procedure("string>?", stringGreaterThan))
    self.define(Procedure("string>=?", stringGreaterThanEquals))
    self.define(Procedure("string-ci=?", stringCiEquals))
    self.define(Procedure("string-ci<?", stringCiLessThan))
    self.define(Procedure("string-ci<=?", stringCiLessThanEquals))
    self.define(Procedure("string-ci>?", stringCiGreaterThan))
    self.define(Procedure("string-ci>=?", stringCiGreaterThanEquals))
    self.define(Procedure("string-contains?", stringContains))
    self.define(Procedure("string-prefix?", stringPrefix))
    self.define(Procedure("string-suffix?", stringSuffix))
    self.define(Procedure("string-upcase", stringUpcase))
    self.define(Procedure("string-downcase", stringDowncase))
    self.define(Procedure("string-titlecase", stringTitlecase))
    self.define(Procedure("string-foldcase", stringFoldcase))
    self.define(Procedure("string-normalize-diacritics", stringNormalizeDiacritics))
    self.define(Procedure("string-normalize-separators", stringNormalizeSeparators))
    self.define(Procedure("string-decode-named-chars", stringDecodeNamedChars))
    self.define(Procedure("string-encode-named-chars", stringEncodeNamedChars))
    self.define(Procedure("object->string", objectToString))
    self.define(Procedure("list->string", listToString))
    self.define(Procedure("string->list", stringToList))
    self.define(Procedure("substring", substring))
    self.define(Procedure("string-contains", stringContainsIndex))
    self.define(Procedure("string-replace!", stringReplace))
    self.define(Procedure("string-replace-first!", stringReplaceFirst))
    self.define(Procedure("string-insert!", stringReplaceRange))
    self.define(Procedure("string-append!", stringAppendEnd))
    self.define(Procedure("string-copy", stringCopy))
    self.define(Procedure("string-copy!", stringInsert))
    self.define(Procedure("string-fill!", stringFill))
    self.define(Procedure("string-split", stringSplit))
    self.define(Procedure("string-trim", stringTrim))
    self.define(Procedure("string-pad-left", stringPadLeft))
    self.define(Procedure("string-pad-right", stringPadRight))
    self.define(Procedure("string-pad-center", stringPadCenter))
    self.define(Procedure("_string-list-ref", stringListRef))
    self.define(Procedure("_string-list-length", stringListLength))
    self.define("string-map", via:
      "(define (string-map f xs . xss)",
      "  (let* ((strs (cons xs xss))",
      "         (len (_string-list-length strs))",
      "         (res (make-string len)))",
      "    (if (null? xss)",
      "        (do ((i 0 (fx1+ i)))",
      "            ((fx>= i len) res)",
      "          (string-set! res i (f (string-ref xs i))))",
      "        (do ((i 0 (fx1+ i)))",
      "            ((fx>= i len) res)",
      "          (string-set! res i (apply f (_string-list-ref i strs)))))))")
    self.define("string-for-each", via:
      "(define (string-for-each f xs . xss)",
      "  (let* ((strs (cons xs xss))",
      "         (len (_string-list-length strs)))",
      "    (do ((i 0 (fx1+ i)))",
      "         ((fx>= i len))",
      "      (apply f (_string-list-ref i strs)))))")
  }
  
  private func isString(_ expr: Expr) -> Expr {
    if case .string(_) = expr {
      return .true
    }
    return .false
  }

  private func isStringEmpty(_ expr: Expr) throws -> Expr {
    guard case .string(let str) = expr else {
      throw RuntimeError.type(expr, expected: [.strType])
    }
    return str.length == 0 ? .true : .false
  }
  
  func makeString(_ k: Expr, ch: Expr?) throws -> Expr {
    let n = try k.asInt()
    if n == 0 {
      return .string(NSMutableString())
    } else {
      let uniChars = Array<UniChar>(repeating: try ch?.asUniChar() ?? UniChar(" "),
                                    count: n)
      return .string(NSMutableString(string: String(utf16CodeUnits: uniChars,
                                                    count: uniChars.count)))
    }
  }

  func readFile(_ path: Expr) throws -> Expr {
    var enc: UInt = 0
    return .string(try NSMutableString(contentsOfFile: try path.asPath(),
                                       usedEncoding: &enc))
  }

  func writeFile(_ path: Expr, _ str: Expr) throws -> Expr {
    let path = try path.asPath()
    let str = try str.asMutableStr()
    do {
      try str.write(toFile: path, atomically: false, encoding: String.Encoding.utf8.rawValue)
      return .true
    } catch {
      return .false
    }
  }
  
  func string(_ exprs: Arguments) throws -> Expr {
    var uniChars: [UniChar] = []
    for expr in exprs {
      uniChars.append(try expr.asUniChar())
    }
    return .string(NSMutableString(string: String(utf16CodeUnits: uniChars, count: uniChars.count)))
  }
  
  func stringLength(_ expr: Expr) throws -> Expr {
    return .fixnum(Int64(try expr.asString().utf16.count))
  }
  
  func stringListLength(_ strings: Expr) throws -> Expr {
    var n = Int.max
    var list = strings
    while case .pair(let expr, let next) = list {
      let str = try expr.asString().utf16
      if str.count < n {
        n = str.count
      }
      list = next
    }
    guard list.isNull else {
      throw RuntimeError.type(strings, expected: [.properListType])
    }
    return .fixnum(Int64(n))
  }
  
  func stringRef(_ expr: Expr, _ index: Expr) throws -> Expr {
    let str = try expr.asString().utf16
    let k = try index.asInt()
    let i = str.index(str.startIndex, offsetBy: k)
    guard i < str.endIndex else {
      throw RuntimeError.range(parameter: 2,
                               of: "string-ref",
                               index,
                               min: 0,
                               max: Int64(str.count - 1))
    }
    return .char(str[i])
  }
  
  func stringListRef(_ index: Expr, _ strings: Expr) throws -> Expr {
    let k = try index.asInt()
    var res = Exprs()
    var list = strings
    while case .pair(let expr, let next) = list {
      let str = try expr.asString().utf16
      let i = str.index(str.startIndex, offsetBy: k)
      guard i < str.endIndex else {
        return .false
      }
      res.append(.char(str[i]))
      list = next
    }
    guard list.isNull else {
      throw RuntimeError.type(strings, expected: [.properListType])
    }
    return .makeList(res)
  }
  
  func stringSet(_ expr: Expr, _ index: Expr, char: Expr) throws -> Expr {
    let str = try expr.asMutableStr()
    str.replaceCharacters(in: NSRange(location: try index.asInt(below: str.length), length: 1),
                          with: String(utf16CodeUnits: [try char.asUniChar()], count: 1))
    return .void
  }
  
  func stringAppend(_ exprs: Arguments) throws -> Expr {
    let str = NSMutableString()
    for expr in exprs {
      str.append(try expr.asString())
    }
    return .string(str)
  }
  
  func stringConcatenate(_ expr: Expr, _ sep: Expr?) throws -> Expr {
    let res = NSMutableString()
    let separator = sep == nil ? "" : try sep!.charOrString()
    var list = expr
    while case .pair(let str, let next) = list {
      res.append(try str.asString())
      if case .pair(_, _) = next {
        res.append(separator)
      }
      list = next
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return .string(res)
  }
  
  func stringEquals(_ expr: Expr, _ args: Arguments) throws -> Expr {
    let str = try expr.asString()
    for arg in args {
      guard try str == arg.asString() else {
        return .false
      }
    }
    return .true
  }
  
  func stringLessThan(_ expr: Expr, _ args: Arguments) throws -> Expr {
    var str = try expr.asString()
    for arg in args {
      let next = try arg.asString()
      guard str < next else {
        return .false
      }
      str = next
    }
    return .true
  }
  
  func stringLessThanEquals(_ expr: Expr, _ args: Arguments) throws -> Expr {
    var str = try expr.asString()
    for arg in args {
      let next = try arg.asString()
      guard str <= next else {
        return .false
      }
      str = next
    }
    return .true
  }
  
  func stringGreaterThan(_ expr: Expr, _ args: Arguments) throws -> Expr {
    var str = try expr.asString()
    for arg in args {
      let next = try arg.asString()
      guard str > next else {
        return .false
      }
      str = next
    }
    return .true
  }
  
  func stringGreaterThanEquals(_ expr: Expr, _ args: Arguments) throws -> Expr {
    var str = try expr.asString()
    for arg in args {
      let next = try arg.asString()
      guard str >= next else {
        return .false
      }
      str = next
    }
    return .true
  }
  
  func stringCiEquals(_ expr: Expr, _ args: Arguments) throws -> Expr {
    let str = try expr.asString().lowercased()
    for arg in args {
      guard try str == arg.asString().lowercased() else {
        return .false
      }
    }
    return .true
  }
  
  func stringCiLessThan(_ expr: Expr, _ args: Arguments) throws -> Expr {
    var str = try expr.asString().lowercased()
    for arg in args {
      let next = try arg.asString().lowercased()
      guard str < next else {
        return .false
      }
      str = next
    }
    return .true
  }
  
  func stringCiLessThanEquals(_ expr: Expr, _ args: Arguments) throws -> Expr {
    var str = try expr.asString().lowercased()
    for arg in args {
      let next = try arg.asString().lowercased()
      guard str <= next else {
        return .false
      }
      str = next
    }
    return .true
  }
  
  func stringCiGreaterThan(_ expr: Expr, _ args: Arguments) throws -> Expr {
    var str = try expr.asString().lowercased()
    for arg in args {
      let next = try arg.asString().lowercased()
      guard str > next else {
        return .false
      }
      str = next
    }
    return .true
  }
  
  func stringCiGreaterThanEquals(_ expr: Expr, _ args: Arguments) throws -> Expr {
    var str = try expr.asString().lowercased()
    for arg in args {
      let next = try arg.asString().lowercased()
      guard str >= next else {
        return .false
      }
      str = next
    }
    return .true
  }
  
  func stringContains(_ expr: Expr, _ other: Expr) throws -> Expr {
    return .makeBoolean(try expr.asString().contains(try other.asString()))
  }
  
  func stringSuffix(_ expr: Expr, _ other: Expr) throws -> Expr {
    return .makeBoolean(try expr.asString().hasSuffix(try other.asString()))
  }
  
  func stringPrefix(_ expr: Expr, _ other: Expr) throws -> Expr {
    return .makeBoolean(try expr.asString().hasPrefix(try other.asString()))
  }
  
  func stringUpcase(_ expr: Expr) throws -> Expr {
    return .string(NSMutableString(string: try expr.asMutableStr().uppercased))
  }
  
  func stringDowncase(_ expr: Expr) throws -> Expr {
    return .string(NSMutableString(string: try expr.asMutableStr().lowercased))
  }
  
  func stringTitlecase(_ expr: Expr) throws -> Expr {
    return .string(NSMutableString(string: try expr.asMutableStr().capitalized))
  }
  
  func stringFoldcase(_ expr: Expr) throws -> Expr {
    return .string(NSMutableString(
      string: try expr.asMutableStr().folding(options: [.caseInsensitive], locale: nil)))
  }
  
  func stringNormalizeDiacritics(_ expr: Expr) throws -> Expr {
    return .string(NSMutableString(
      string: try expr.asMutableStr().folding(options: [.diacriticInsensitive, .widthInsensitive],
                                              locale: nil)))
  }
  
  func stringNormalizeSeparators(_ expr: Expr, _ sep: Expr?, _ sepChars: Expr?) throws -> Expr {
    let separator = sep == nil ? " " : try sep!.charOrString()
    let charSet: CharacterSet
    if case .some(.object(let obj)) = sepChars, let cs = obj as? CharSet {
      charSet = CharacterSet(charactersIn: String(utf16CodeUnits: cs.array, count: cs.count))
    } else if let tcs = sepChars {
      charSet = CharacterSet(charactersIn: try tcs.asString())
    } else {
      charSet = .whitespacesAndNewlines
    }
    let components = try expr.asString().components(separatedBy: charSet)
    return .string(NSMutableString(string:
                     components.filter { !$0.isEmpty }.joined(separator: separator)))
  }
  
  func stringDecodeNamedChars(_ expr: Expr) throws -> Expr {
    return .string(NSMutableString(string: try expr.asString().decodingNamedCharacters()))
  }
  
  func stringEncodeNamedChars(_ expr: Expr, requiredOnly: Expr?) throws -> Expr {
    let str = try expr.asString()
    if requiredOnly?.isTrue ?? false {
      return .string(NSMutableString(string: str.encodingPredefinedXmlEntities()))
    } else {
      return .string(NSMutableString(string: str.encodingNamedCharacters()))
    }
  }
  
  func stringToList(_ expr: Expr, args: Arguments) throws -> Expr {
    let str = try expr.asString().utf16
    guard let (s, e) = args.optional(Expr.makeNumber(0), Expr.makeNumber(str.count)) else {
      throw RuntimeError.argumentCount(of: "string->list",
                                       min: 1,
                                       max: 3,
                                       args: .pair(expr, .makeList(args)))
    }
    var end = try e.asInt(below: str.count + 1)
    let start = try s.asInt(below: end + 1)
    var i = str.index(str.startIndex, offsetBy: end)
    var res = Expr.null
    while end > start {
      i = str.index(before: i)
      res = .pair(.char(str[i]), res)
      end -= 1
    }
    return res
  }
  
  func objectToString(_ expr: Expr, write: Expr?) throws -> Expr {
    return .makeString(expr.toString(escape: write?.isTrue ?? false))
  }
  
  func listToString(_ expr: Expr) throws -> Expr {
    var list = expr
    var uniChars: [UniChar] = []
    while case .pair(let ch, let next) = list {
      uniChars.append(try ch.asUniChar())
      list = next
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return .string(NSMutableString(string: String(utf16CodeUnits: uniChars, count: uniChars.count)))
  }
  
  func substring(_ expr: Expr, _ s: Expr, _ e: Expr) throws -> Expr {
    let str = try expr.asMutableStr()
    let end = try e.asInt(below: str.length + 1)
    let start = try s.asInt(below: end + 1)
    return .string(NSMutableString(string: str.substring(with: NSRange(location: start,
                                                                       length: end - start))))
  }
  
  func stringContainsIndex(_ expr: Expr, _ sub: Expr, _ args: Arguments) throws -> Expr {
    let str = try expr.asMutableStr()
    let from = try sub.asString()
    guard let (s, e) = args.optional(Expr.makeNumber(0), Expr.makeNumber(str.length)) else {
      throw RuntimeError.argumentCount(of: "string-contains",
                                       min: 2,
                                       max: 4,
                                       args: .pair(expr, .pair(sub, .makeList(args))))
    }
    let end = try e.asInt(below: str.length + 1)
    let start = try s.asInt(below: end + 1)
    let range = str.range(of: from, options: [], range: NSMakeRange(start, end - start))
    if range.location == NSNotFound {
      return .false
    } else {
      return .makeNumber(range.location)
    }
  }
  
  func stringReplace(_ expr: Expr, _ sub: Expr, _ repl: Expr, _ args: Arguments) throws -> Expr {
    let str = try expr.asMutableStr()
    let from = try sub.asString()
    let to = try repl.asString()
    guard let (s, e) = args.optional(Expr.makeNumber(0), Expr.makeNumber(str.length)) else {
      throw RuntimeError.argumentCount(of: "string-replace!",
                                       min: 3,
                                       max: 5,
                                       args: .pair(expr, .pair(sub, .pair(repl, .makeList(args)))))
    }
    let end = try e.asInt(below: str.length + 1)
    let start = try s.asInt(below: end + 1)
    let num = str.replaceOccurrences(of: from,
                                     with: to,
                                     options: [],
                                     range: NSMakeRange(start, end - start))
    return .makeNumber(num)
  }
  
  func stringReplaceFirst(_ expr: Expr,
                          _ sub: Expr,
                          _ repl: Expr,
                          _ args: Arguments) throws -> Expr {
    let str = try expr.asMutableStr()
    let from = try sub.asString()
    let to = try repl.asString()
    guard let (s, e) = args.optional(Expr.makeNumber(0), Expr.makeNumber(str.length)) else {
      throw RuntimeError.argumentCount(of: "string-replace-first!",
                                       min: 3,
                                       max: 5,
                                       args: .pair(expr, .pair(sub, .pair(repl, .makeList(args)))))
    }
    let end = try e.asInt(below: str.length + 1)
    let start = try s.asInt(below: end + 1)
    let range = str.range(of: from, options: [], range: NSMakeRange(start, end - start))
    if range.location == NSNotFound {
      return .false
    } else {
      str.replaceCharacters(in: range, with: to)
      return .makeNumber(range.location)
    }
  }
  
  func stringReplaceRange(_ expr: Expr, _ repl: Expr, _ s: Expr?, _ e: Expr?) throws -> Expr {
    let str = try expr.asMutableStr()
    let to = try repl.asString()
    let start: Int
    let end: Int
    if let e = e {
      end = try e.asInt(below: str.length + 1)
      start = try s?.asInt(below: end + 1) ?? end
    } else {
      start = try s?.asInt(below: str.length + 1) ?? 0
      end = start
    }
    str.replaceCharacters(in: NSMakeRange(start, end - start), with: to)
    return .void
  }

  func stringAppendEnd(_ expr: Expr, _ args: Arguments) throws -> Expr {
    let str = try expr.asMutableStr()
    for arg in args {
      str.append(try arg.asString())
    }
    return .void
  }
  
  func stringCopy(_ expr: Expr, args: Arguments) throws -> Expr {
    let str = try expr.asMutableStr()
    guard let (s, e) = args.optional(Expr.makeNumber(0), Expr.makeNumber(str.length)) else {
      throw RuntimeError.argumentCount(of: "string-copy",
                                       min: 1,
                                       max: 3,
                                       args: .pair(expr, .makeList(args)))
    }
    let end = try e.asInt(below: str.length + 1)
    let start = try s.asInt(below: end + 1)
    // short-cut if this is a full copy
    if start == 0 && end == str.length {
      return .string(NSMutableString(string: str as String))
    }
    // extract substring to copy
    return .string(NSMutableString(string: str.substring(with: NSRange(location: start,
                                                                       length: end - start))))
  }
  
  func stringInsert(_ expr: Expr, _ index: Expr, _ from: Expr, args: Arguments) throws -> Expr {
    let target = try expr.asMutableStr()
    let str = try from.asString().utf16
    guard let (s, e) = args.optional(Expr.makeNumber(0), Expr.makeNumber(str.count)) else {
      throw RuntimeError.argumentCount(
        of: "string-copy!",
        min: 3,
        max: 5,
        args: .pair(expr, .pair(index, .pair(from, .makeList(args)))))
    }
    let end = try e.asInt(below: str.count + 1)
    var ei = str.index(str.startIndex, offsetBy: end)
    let si = str.index(str.startIndex, offsetBy: try s.asInt(below: end + 1))
    let loc = try index.asInt(below: target.length + 1)
    ei = min(ei, str.index(si, offsetBy: target.length - loc))
    var uniChars: [UniChar] = []
    var n = 0
    for ch in str[si..<ei] {
      uniChars.append(ch)
      n += 1
    }
    target.replaceCharacters(in: NSMakeRange(loc, min(n, target.length - loc)),
                             with: String(utf16CodeUnits: uniChars, count: uniChars.count))
    return .void
  }
  
  func stringFill(_ expr: Expr, _ ch: Expr, _ args: Arguments) throws -> Expr {
    let str = try expr.asMutableStr()
    guard let (s, e) = args.optional(Expr.makeNumber(0), Expr.makeNumber(str.length)) else {
      throw RuntimeError.argumentCount(of: "string-fill!",
                                       min: 2,
                                       max: 4,
                                       args: .pair(expr, .pair(ch, .makeList(args))))
    }
    let end = try e.asInt(below: str.length + 1)
    let start = try s.asInt(below: end + 1)
    if start < end {
      let uniChars = Array<UniChar>(repeating: try ch.asUniChar(), count: end - start)
      str.replaceCharacters(in: NSRange(location: start, length: end - start),
                            with: String(utf16CodeUnits: uniChars, count: uniChars.count))
    }
    return .void
  }
  
  private func stringSplit(_ expr: Expr, _ separator: Expr, _ allowEmpty: Expr?) throws -> Expr {
    let str = try expr.asMutableStr()
    let sep: String
    switch separator {
      case .string(let str):
        sep = str as String
      default:
        sep = try self.charAsString(separator)
    }
    let components = str.components(separatedBy: sep)
    let includeEmpty = allowEmpty == nil ? true : !allowEmpty!.isFalse
    var res = Exprs()
    for component in components {
      if includeEmpty || !component.isEmpty {
        res.append(.makeString(component))
      }
    }
    return .makeList(res)
  }
  
  private func stringTrim(_ expr: Expr, _ trimChars: Expr?) throws -> Expr {
    let str = try expr.asMutableStr()
    if case .some(.object(let obj)) = trimChars, let cs = obj as? CharSet {
      let chars = String(utf16CodeUnits: cs.array, count: cs.count)
      return .makeString(str.trimmingCharacters(in: CharacterSet(charactersIn: chars)))
    } else if let tcs = trimChars {
      return .makeString(str.trimmingCharacters(in: CharacterSet(charactersIn: try tcs.asString())))
    } else {
      return .makeString(str.trimmingCharacters(in: CharacterSet.whitespacesAndNewlines))
    }
  }
  
  private func stringPadLeft(_ expr: Expr,
                             _ padChar: Expr,
                             _ length: Expr,
                             _ forceLength: Expr?) throws -> Expr {
    let str = try expr.asMutableStr()
    let char = try self.charAsString(padChar)
    let len = try length.asInt()
    let force = forceLength?.isTrue ?? false
    guard str.length <= len else {
      return force ? .makeString(str.substring(from: str.length - len)) : expr
    }
    var res = String(repeating: char.first ?? " ", count: len - str.length)
    res.append(str as String)
    return .makeString(res)
  }
  
  private func stringPadRight(_ expr: Expr,
                              _ padChar: Expr,
                              _ length: Expr,
                              _ forceLength: Expr?) throws -> Expr {
    let str = try expr.asMutableStr()
    let char = try self.charAsString(padChar)
    let len = try length.asInt()
    let force = forceLength?.isTrue ?? false
    guard str.length <= len else {
      return force ? .makeString(str.substring(to: len)) : expr
    }
    return .makeString(str.appending(
             String(repeating: char.first ?? " ", count: len - str.length)))
  }

  private func stringPadCenter(_ expr: Expr,
                               _ padChar: Expr,
                               _ length: Expr,
                               _ forceLength: Expr?) throws -> Expr {
    let str = try expr.asMutableStr()
    let char = try self.charAsString(padChar)
    let len = try length.asInt()
    let force = forceLength?.isTrue ?? false
    guard str.length <= len else {
      return force ? .makeString(str.substring(to: len)) : expr
    }
    let left = (len - str.length) / 2
    let right = len - str.length - left
    var res = String(repeating: char.first ?? " ", count: left)
    res.append(str as String)
    if right > 0 {
      res.append(String(repeating: char.first ?? " ", count: right))
    }
    return .makeString(res)
  }
  
  private func charAsString(_ expr: Expr) throws -> String {
    switch expr {
      case .char(let c):
        return String(unicodeScalar(c))
      default:
        return try expr.asString()
    }
  }
}
