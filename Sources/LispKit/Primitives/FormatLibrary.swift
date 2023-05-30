//
//  FormatLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 11/05/2023.
//  Copyright © 2023 ObjectHub. All rights reserved.
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
import CLFormat

public final class FormatLibrary: NativeLibrary {
  
  /// Imported native library
  private unowned var typeLibrary: TypeLibrary!
  
  /// The implementation of the s-expr directive
  private let sexprDirSpec: SExprDirectiveSpecifier
  
  /// Configuration of the `clformat` function
  private let clFormatConfig: CLFormatConfig
  
  /// Base configuration of the `format` procedure
  private let baseFormatConfig: FormatConfig
  
  /// Exported parameter objects
  public let formatConfigParam: Procedure
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "format"]
  }
  
  /// Initialize format library.
  public required init(in context: Context) throws {
    let formatConfig = FormatConfig()
    formatConfig.locale = Locale.current
    formatConfig.tabWidth = 8
    formatConfig.lineWidth = 80
    let sexprDirSpec =  SExprDirectiveSpecifier()
    var clFormatConfig = CLFormatConfig.standard
    clFormatConfig.setArgumentFactory(makeArguments: FormatArguments.init)
    clFormatConfig.parse("s", "S", appending: sexprDirSpec)
    clFormatConfig.parse("`") { parser, parameters, modifiers in
      _ = try parser.nextChar()
      let (control, directive) = try parser.parse()
      guard let directive = directive,
            directive.specifier.identifier == LispKitDirectiveSpecifier.unwrapEnd.identifier,
            parameters.parameterCount == 0,
            !modifiers.contains(.at),
            !modifiers.contains(.colon),
            !modifiers.contains(.plus) else {
        throw CLControlError.malformedDirectiveSyntax("unwrap", "~`...~‘")
      }
      return .append(parameters, modifiers, LispKitDirectiveSpecifier.unwrap(control))
    }
    clFormatConfig.parse("‘") { parser, parameters, modifiers in
      guard parameters.parameterCount == 0,
            !modifiers.contains(.at),
            !modifiers.contains(.colon),
            !modifiers.contains(.plus) else {
        throw CLControlError.malformedDirective("~\(modifiers))‘")
      }
      return .exit(parameters, modifiers, LispKitDirectiveSpecifier.unwrapEnd)
    }
    self.sexprDirSpec = sexprDirSpec
    self.clFormatConfig = clFormatConfig
    self.baseFormatConfig = formatConfig
    self.formatConfigParam = Procedure(.null, .object(formatConfig))
    try super.init(in: context)
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "type"], "type-of")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define("base-format-config", as: .object(self.baseFormatConfig))
    self.define("current-format-config", as: self.formatConfigParam)
    self.define("format-config-type-tag", as: FormatConfig.type.objectTypeTag())
    self.define(Procedure("format", self.format))
    self.define(Procedure("format-config?", self.isFormatConfig))
    self.define(Procedure("format-config", self.formatConfigConstr))
    self.define(Procedure("make-format-config", self.makeFormatConfig))
    self.define(Procedure("copy-format-config", self.copyFormatConfig))
    self.define(Procedure("format-config-locale", self.formatConfigLocale))
    self.define(Procedure("format-config-locale-set!", self.formatConfigLocaleSet))
    self.define(Procedure("format-config-tabwidth", self.formatConfigTabWidth))
    self.define(Procedure("format-config-tabwidth-set!", self.formatConfigTabWidthSet))
    self.define(Procedure("format-config-linewidth", self.formatConfigLineWidth))
    self.define(Procedure("format-config-linewidth-set!", self.formatConfigLineWidthSet))
    self.define(Procedure("format-config-control-set!", self.formatConfigControlSet))
    self.define(Procedure("format-config-controls", self.formatConfigControls))
    self.define(Procedure("format-config-parent", self.formatConfigParent))
  }
  
  /// Initializations of the library.
  public override func initializations() {
    self.typeLibrary = self.nativeLibrary(TypeLibrary.self)
    self.sexprDirSpec.typeLibrary = self.typeLibrary
  }
  
  public func defaultFormatConfig() throws -> FormatConfig {
    guard let value = self.context.evaluator.getParam(self.formatConfigParam) else {
      throw RuntimeError.eval(.invalidDefaultFormatConfig, .false)
    }
    guard case .object(let obj) = value, let config = obj as? FormatConfig else {
      throw RuntimeError.eval(.invalidDefaultFormatConfig, value)
    }
    return config
  }
  
  public func formatConfig(from expr: Expr?) throws -> FormatConfig {
    guard let expr = expr else {
      return try self.defaultFormatConfig()
    }
    switch expr {
      case .true:
        return try self.defaultFormatConfig()
      case .object(let obj):
        if let conf = obj as? FormatConfig {
          return conf
        }
      default:
        break
    }
    throw RuntimeError.type(expr, expected: [FormatConfig.type])
  }
  
  private func format(_ args: Arguments) throws -> Expr {
    var output: TextOutput? = nil
    var fconfig: FormatConfig
    var arguments: [Any?] = []
    var iterator = args.makeIterator()
    var arg = iterator.next()
    if case .some(.port(let port)) = arg {
      guard port.isOpen else {
        throw RuntimeError.eval(.portClosed, .port(port))
      }
      guard case .textOutputPort(let out) = port.kind else {
        throw RuntimeError.type(.port(port), expected: [.textInputPortType])
      }
      output = out
      arg = iterator.next()
    }
    if case .some(.object(let obj)) = arg, let outerConf = obj as? FormatConfig {
      fconfig = FormatConfig(outerConfig: outerConf)
    } else {
      fconfig = FormatConfig(outerConfig: try self.defaultFormatConfig())
    }
    if case .some(.symbol(let sym)) = arg {
      fconfig.locale = Locale(identifier: sym.identifier)
      arg = iterator.next()
    }
    if case .some(.fixnum(let num)) = arg {
      guard let tsize = Int(exactly: num), tsize > 0, tsize <= 1000 else {
        throw RuntimeError.range(arg!, min: 1, max: 1000)
      }
      fconfig.tabWidth = tsize
      arg = iterator.next()
    }
    if case .some(.fixnum(let num)) = arg {
      guard let len = Int(exactly: num), len > 0, len < Int.max - 100 else {
        throw RuntimeError.range(arg!, min: 1, max: Int64(Int.max))
      }
      fconfig.lineWidth = len
      arg = iterator.next()
    }
    guard case .some(.string(let control)) = arg else {
      throw RuntimeError(SourcePosition.unknown,
                         ErrorDescriptor.eval(.controlStringMissing),
                         Array(args))
    }
    while let arg = iterator.next() {
      arguments.append(arg)
    }
    var clFormatConfig = self.clFormatConfig
    clFormatConfig.environment["formatConfig"] = fconfig
    let res = try clformat(control as String,
                           config: clFormatConfig,
                           locale: fconfig.getLocale(),
                           tabsize: fconfig.getTabWidth(),
                           linewidth: fconfig.getLineWidth(),
                           arguments: arguments)
    if let output = output {
      guard output.writeString(res) else {
        throw RuntimeError.eval(.cannotWriteToPort, .port(Port(output: output)))
      }
      return .void
    } else {
      return .makeString(res)
    }
  }
  
  private func isFormatConfig(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is FormatConfig else {
      return .false
    }
    return .true
  }
  
  private func formatConfigConstr(args: Arguments) throws -> Expr {
    guard let (loc, twidth, lwidth, outer) =
            args.optional(.false, .false, .false, .true) else {
      throw RuntimeError.argumentCount(of: "format-config", min: 0, max: 4, args: .makeList(args))
    }
    let fconf = FormatConfig(outerConfig: outer.isTrue ? try self.formatConfig(from: outer) : nil)
    if loc.isTrue {
      fconf.locale = Locale(identifier: try loc.asSymbol().identifier)
    }
    if twidth.isTrue {
      fconf.tabWidth = try twidth.asInt(above: 1, below: 1000)
    }
    if lwidth.isTrue {
      fconf.lineWidth = try twidth.asInt(above: 1, below: Int.max - 100)
    }
    return .object(fconf)
  }
  
  private func makeFormatConfig(outer: Expr, args: Arguments) throws -> Expr {
    let fconf = FormatConfig(outerConfig: outer.isTrue ? try self.formatConfig(from: outer) : nil)
    guard let (loc, twidth, lwidth) =
            args.optional(.false, .false, .false) else {
      throw RuntimeError.argumentCount(of: "make-format-config",
                                       min: 1,
                                       max: 4,
                                       args: .pair(outer, .makeList(args)))
    }
    if loc.isTrue {
      fconf.locale = Locale(identifier: try loc.asSymbol().identifier)
    }
    if twidth.isTrue {
      fconf.tabWidth = try twidth.asInt(above: 1, below: 1000)
    }
    if lwidth.isTrue {
      fconf.lineWidth = try twidth.asInt(above: 1, below: Int.max - 100)
    }
    return .object(fconf)
  }
  
  private func copyFormatConfig(config: Expr, collapse: Expr?) throws -> Expr {
    if collapse?.isFalse ?? true {
      return .object(FormatConfig(copy: try self.formatConfig(from: config)))
    } else {
      return .object(FormatConfig(collapse: try self.formatConfig(from: config)))
    }
  }
  
  private func formatConfigLocale(config: Expr?) throws -> Expr {
    let fconf = try self.formatConfig(from: config)
    return .symbol(self.context.symbols.intern(fconf.getLocale().identifier))
  }
  
  private func formatConfigLocaleSet(fst: Expr, snd: Expr?) throws -> Expr {
    let fconf = try self.formatConfig(from: snd == nil ? nil : fst)
    let locale = snd == nil ? fst : snd!
    if locale.isFalse {
      fconf.locale = nil
    } else {
      fconf.locale = Locale(identifier: try locale.asSymbol().identifier)
    }
    return .void
  }
  
  private func formatConfigTabWidth(config: Expr?) throws -> Expr {
    return .makeNumber(try self.formatConfig(from: config).getTabWidth())
  }
  
  private func formatConfigTabWidthSet(fst: Expr, snd: Expr?) throws -> Expr {
    let fconf = try self.formatConfig(from: snd == nil ? nil : fst)
    let tabWidth = snd == nil ? fst : snd!
    if tabWidth.isFalse {
      fconf.tabWidth = nil
    } else {
      fconf.tabWidth = try tabWidth.asInt(above: 1, below: 1000)
    }
    return .void
  }
  
  private func formatConfigLineWidth(config: Expr?) throws -> Expr {
    return .makeNumber(try self.formatConfig(from: config).getLineWidth())
  }
  
  private func formatConfigLineWidthSet(fst: Expr, snd: Expr?) throws -> Expr {
    let fconf = try self.formatConfig(from: snd == nil ? nil : fst)
    let lineWidth = snd == nil ? fst : snd!
    if lineWidth.isFalse {
      fconf.lineWidth = nil
    } else {
      fconf.lineWidth = try lineWidth.asInt(above: 1, below: 1000)
    }
    return .void
  }
  
  private func formatConfigControlSet(fst: Expr, snd: Expr, trd: Expr?, fth: Expr?) throws -> Expr {
    let fconf: FormatConfig
    let typeExpr: Expr
    let controlExpr: Expr
    let configExpr: Expr?
    // If the second argument is a string or false, we use the default config
    switch snd {
      case .false, .string(_):
        fconf = try self.formatConfig(from: nil)
        typeExpr = fst
        controlExpr = snd
        configExpr = trd
      default:
        fconf = try self.formatConfig(from: fst)
        typeExpr = snd
        guard let trd = trd else {
          return .null // TODO: return error
        }
        controlExpr = trd
        configExpr = fth
    }
    let typeTag: Symbol
    // Allow record types as type tags
    if case .record(let record) = typeExpr, case .recordType = record.kind {
      typeTag = try record.exprs[0].asSymbol()
    // Assume this is a type tag
    } else {
      typeTag = try typeExpr.asSymbol()
    }
    if controlExpr.isFalse {
      fconf.removeFormat(typeTag)
    } else if let configExpr = configExpr {
      guard case .object(let obj) = configExpr, let conf = obj as? FormatConfig else {
        throw RuntimeError.type(configExpr, expected: [FormatConfig.type])
      }
      guard conf.outerConfig == nil else {
        return .null // TODO: return error
      }
      fconf.format(typeTag,
                   with: try CLControl(string: controlExpr.asString(), config: self.clFormatConfig),
                   in: conf)
    } else {
      fconf.format(typeTag,
                   with: try CLControl(string: controlExpr.asString(), config: self.clFormatConfig),
                   in: nil)
    }
    return .void
  }
  
  private func formatConfigControls(config: Expr?, all: Expr?) throws -> Expr {
    let fconf = try self.formatConfig(from: config)
    var res: Expr = .null
    if all?.isTrue ?? true {
      var fc: FormatConfig? = fconf
      var typeTags: Set<Symbol> = []
      while let conf = fc {
        for (typeTag, _) in conf.controlDict {
          typeTags.insert(typeTag)
        }
        fc = conf.outerConfig
      }
      for typeTag in typeTags {
        res = .pair(.symbol(typeTag), res)
      }
    } else {
      for (typeTag, _) in fconf.controlDict {
        res = .pair(.symbol(typeTag), res)
      }
    }
    return res
  }
  
  private func formatConfigParent(config: Expr?) throws -> Expr {
    let fconf = try self.formatConfig(from: config)
    if let parent = fconf.outerConfig {
      return .object(parent)
    } else {
      return .false
    }
  }
}

public final class FormatConfig: NativeObject {
  
  public struct FormatControl {
    let control: CLControl
    let env: FormatConfig?
  }
  
  /// Type representing format configurations
  public static let type = Type.objectType(Symbol(uninterned: "format-config"))
  
  /// The locale to be used
  public var locale: Locale?
  
  /// The width of a tab character
  public var tabWidth: Int?
  
  /// The width of a line
  public var lineWidth: Int?
  
  /// Control dictionary for ~S directives
  public private(set) var controlDict: [Symbol : FormatControl]
  
  /// Outer configuration
  public let outerConfig: FormatConfig?
  
  public init(outerConfig: FormatConfig? = nil) {
    self.locale = nil
    self.tabWidth = nil
    self.lineWidth = nil
    self.controlDict = [:]
    self.outerConfig = outerConfig
  }
  
  public init(copy: FormatConfig, outer: FormatConfig? = nil) {
    self.locale = copy.locale
    self.tabWidth = copy.tabWidth
    self.lineWidth = copy.lineWidth
    self.controlDict = copy.controlDict
    self.outerConfig = outer ?? copy.outerConfig
  }
  
  public init(collapse: FormatConfig) {
    self.locale = collapse.getLocale()
    self.tabWidth = collapse.getTabWidth()
    self.lineWidth = collapse.getLineWidth()
    self.controlDict = collapse.controlDict
    self.outerConfig = nil
    var outer = collapse.outerConfig
    while let fconf = outer {
      self.controlDict.merge(fconf.controlDict) { (current, _) in current }
      outer = fconf.outerConfig
    }
  }
  
  public func rebase(with: FormatConfig) throws -> FormatConfig {
    guard self.outerConfig == nil else {
      throw RuntimeError.eval(.cannotUseFormatConfigAsLayer, .object(self), .object(with))
    }
    return FormatConfig(copy: self, outer: with)
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    var res: String = "#<format-config \(self.identityString)"
    if let outer = self.outerConfig {
      res += " extending \(outer.identityString)"
    }
    guard self.locale != nil ||
          self.tabWidth != nil ||
          self.lineWidth != nil ||
          self.controlDict.count > 0 else {
      return res + ">"
    }
    var sep = ": "
    if let locale = self.locale {
      res += "\(sep)locale = \(locale.identifier)"
      sep = "; "
    }
    if let tabWidth = self.tabWidth {
      res += "\(sep)tab-width = \(tabWidth)"
      sep = "; "
    }
    if let lineWidth = self.lineWidth {
      res += "\(sep)line-width = \(lineWidth)"
      sep = "; "
    }
    sep = "\(sep)controls = "
    var i = 0
    for (type, _) in self.controlDict {
      if i >= 20 {
        return res + ", …>"
      }
      res += "\(sep)\(type)"
      sep = ", "
      i += 1
    }
    return res + ">"
  }
  
  public override func unpack() -> Exprs {
    var exprs: Exprs = []
    for (typeTag, _) in self.controlDict {
      exprs.append(.symbol(typeTag))
    }
    return [.makeString(self.identityString),
            self.outerConfig == nil ? .false : .makeString(self.outerConfig!.identityString),
            self.locale == nil ? .false : .makeString(self.locale!.identifier),
            self.tabWidth == nil ? .false : .makeNumber(self.tabWidth!),
            self.lineWidth == nil ? .false : .makeNumber(self.lineWidth!),
            .vector(Collection(kind: .immutableVector, exprs: exprs))]
  }
  
  public func getLocale() -> Locale {
    return self.locale ?? self.outerConfig?.getLocale() ?? Locale.current
  }
  
  public func getTabWidth() -> Int {
    return self.tabWidth ?? self.outerConfig?.getTabWidth() ?? 8
  }
  
  public func getLineWidth() -> Int {
    return self.lineWidth ?? self.outerConfig?.getLineWidth() ?? 80
  }
  
  public func format(_ type: Symbol, with control: CLControl, in env: FormatConfig?) {
    self.controlDict[type] = FormatControl(control: control, env: env)
  }
  
  public func removeFormat(_ type: Symbol) {
    self.controlDict.removeValue(forKey: type)
  }
  
  public func control(for type: Symbol) -> FormatControl? {
    return self.controlDict[type] ?? self.outerConfig?.control(for: type)
  }
}

class FormatArguments: CLFormat.Arguments {
  
  /// Is this object nil?
  public override func isNil(_ obj: Any?) -> Bool {
    guard let obj = obj else {
      return true
    }
    guard let obj = obj as? Expr, case .false = obj else {
      return false
    }
    return true
  }
  
  /// Is this object a value?
  public override func isValue(_ obj: Any?) -> Bool {
    guard let obj = obj else {
      return false
    }
    guard let obj = obj as? Expr, case .false = obj else {
      return true
    }
    return false
  }
  
  /// Coerce object to a boolean value, if possible.
  public override func coerceToBool(_ obj: Any) -> Bool? {
    if let obj = obj as? Expr {
      guard case .null = obj else {
        return true
      }
      return false
    } else {
      return nil
    }
  }

  /// Coerce object to a Number value, if possible.
  public override func coerceToNumber(_ obj: Any) -> Number? {
    guard let expr = obj as? Expr else {
      return super.coerceToNumber(obj)
    }
    switch expr {
      case .fixnum(let num):
        return Number(num)
      case .bignum(_):
        return nil
      case .rational(_, _):
        guard let dbl = try? expr.asDouble(coerce: true) else {
          return nil
        }
        return Number(dbl)
      case .flonum(let num):
        return Number(num)
      case .complex(let c):
        guard let dbl = c.value.realValue else {
          return nil
        }
        return Number(dbl)
      default:
        return nil
    }
  }
  
  /// Coerce object to an integer, if possible.
  public override func coerceToInt(_ obj: Any) -> Int? {
    guard let expr = obj as? Expr else {
      return super.coerceToInt(obj)
    }
    guard case .fixnum(let num) = expr else {
      return nil
    }
    return super.coerceToInt(num)
  }
  
  /// Coerce object to a character, if possible.
  public override func coerceToCharacter(_ obj: Any) -> Character? {
    guard let expr = obj as? Expr else {
      return super.coerceToCharacter(obj)
    }
    switch expr {
      case .char(let ch):
        return Character(unicodeScalar(ch))
      case .string(let mstr):
        let str = mstr as String
        guard str.count == 1 else {
          return nil
        }
        return str.first!
      default:
        return nil
    }
  }
  
  /// Coerce object to a string, if possible.
  public override func coerceToString(_ obj: Any) -> String? {
    guard let expr = obj as? Expr else {
      return super.coerceToString(obj)
    }
    guard case .string(let str) = expr else {
      return nil
    }
    return str as String
  }
  
  /// Coerce object to an array, if possible.
  public override func coerceToArray(_ obj: Any, capAt: Int) -> [Any?]? {
    guard let expr = obj as? Expr else {
      return super.coerceToArray(obj, capAt: capAt)
    }
    switch expr {
      case .null, .false:
        return []
      case .pair(let car, let cdr):
        var list = cdr
        var itercap = capAt
        var newargs: [Any?] = [car]
        while itercap > 0, case .pair(let head, let tail) = list {
          newargs.append(head)
          list = tail
          itercap -= 1
        }
        return newargs
      case .vector(let col):
        guard col.isVector else {
          return nil
        }
        var i = 0
        var newargs: [Any?] = []
        while i < capAt && i < col.exprs.count {
          newargs.append(col.exprs[i])
          i += 1
        }
        return newargs
      default:
        return nil
    }
  }
  
  /// Coerce object to a parameter, if possible.
  public override func coerceToParameter(_ obj: Any) -> Parameter? {
    guard let expr = obj as? Expr, case .false = expr else {
      return super.coerceToParameter(obj)
    }
    return Parameter.none
  }
}

public enum LispKitDirectiveSpecifier: DirectiveSpecifier {
  case unwrap(CLControl)
  case unwrapEnd
  
  public var identifier: Character {
    switch self {
      case .unwrap(_):
        return "`"
      case .unwrapEnd:
        return "'"
    }
  }
  
  public func apply(context: CLFormat.Context,
                    parameters: CLFormat.Parameters,
                    modifiers: CLFormat.Modifiers,
                    arguments: CLFormat.Arguments) throws -> CLFormat.Instruction {
    switch self {
      case .unwrap(let control):
        let unpacked: [Any?]
        if let arg = try arguments.next() {
          if let expr = arg as? Expr {
            unpacked = SExprDirectiveSpecifier.unpack(expr)
          } else if let x = arg as? CustomStringConvertible {
            unpacked = [Expr.makeString(x.description)]
          } else {
            unpacked = [Expr.makeString("\(arg)")]
          }
        } else {
          unpacked = [Expr.false]
        }
        let args = context.config.makeArguments(locale: arguments.locale,
                                                tabsize: arguments.tabsize,
                                                linewidth: arguments.linewidth,
                                                args: unpacked)
        let str = try control.format(with: args, in: context).string
        return .append(StandardDirectiveSpecifier.pad(string: str,
                                                      left: modifiers.contains(.at),
                                                      right: !modifiers.contains(.at),
                                                      padchar: try parameters.character(3) ?? " ",
                                                      ellipsis: try parameters.character(5) ?? "…",
                                                      mincol: try parameters.number(0) ?? 0,
                                                      colinc: try parameters.number(1) ?? 1,
                                                      minpad: try parameters.number(2) ?? 0,
                                                      maxcol: try parameters.number(4)))
      default:
        throw CLFormatError.unsupportedDirective("~\(self.identifier)")
    }
  }
  
  public var description: String {
    switch self {
      case .unwrap(let control):
        return "`" + control.description + "'"
      default:
        return String(self.identifier)
    }
  }
}

public class SExprDirectiveSpecifier: DirectiveSpecifier {
  internal weak var typeLibrary: TypeLibrary?
  
  public init(typeLibrary: TypeLibrary? = nil) {
    self.typeLibrary = typeLibrary
  }
  
  public var identifier: Character {
    return "S"
  }
  
  public static func unpack(_ expr: Expr, in context: Context? = nil) -> [Any?] {
    switch expr {
      case .rational(let num, let denom):
        return [num, denom]
      case .complex(let cpl):
        return [Expr.flonum(cpl.value.re), Expr.flonum(cpl.value.im)]
      case .symbol(let sym):
        return [Expr.makeString(sym.identifier),
                Expr.makeString(sym.description)]
      case .bytes(let bvec):
        var exprs: Exprs = []
        for x in bvec.value {
          exprs.append(.fixnum(Int64(x)))
        }
        return [Expr.makeString(bvec.identityString),
                Expr.vector(Collection(kind: .immutableVector, exprs: exprs))]
      case .box(let x):
        return [Expr.makeString(x.identityString), x.value]
      case .mpair(let x):
        return [Expr.makeString(x.identityString), x.fst, x.snd]
      case .table(let ht):
        var exprs: Exprs = []
        for bucket in ht.buckets {
          var current = bucket
          while case .pair(.pair(let key, let value), let next) = current {
            exprs.append(.pair(key, .pair(value, .null)))
            current = next
          }
        }
        return [Expr.makeString(ht.identityString),
                Expr.vector(Collection(kind: .immutableVector, exprs: exprs))]
      case .env(let ev):
        var exprs: Exprs = []
        for (sym, val) in ev.bindings {
          let suffix: Expr
          switch val {
            case .undefined:
              suffix = .pair(.fixnum(0), .null)
            case .mutable(let n):
              suffix = .pair(.fixnum(1), .pair(.fixnum(Int64(n)), .null))
            case .mutableImport(let n):
              suffix = .pair(.fixnum(2), .pair(.fixnum(Int64(n)), .null))
            case .immutableImport(let n):
              suffix = .pair(.fixnum(3), .pair(.fixnum(Int64(n)), .null))
          }
          exprs.append(.pair(.symbol(sym), suffix))
        }
        switch ev.kind {
          case .library(let name):
            return [Expr.makeString(ev.identityString),
                    Expr.fixnum(0),
                    name,
                    Expr.vector(Collection(kind: .immutableVector, exprs: exprs))]
          case .program(let file):
            return [Expr.makeString(ev.identityString),
                    Expr.fixnum(1),
                    Expr.makeString(file),
                    Expr.vector(Collection(kind: .immutableVector, exprs: exprs))]
          case .repl:
            return [Expr.makeString(ev.identityString),
                    Expr.fixnum(2),
                    Expr.vector(Collection(kind: .immutableVector, exprs: exprs))]
          case .custom:
            return [Expr.makeString(ev.identityString),
                    Expr.fixnum(3),
                    Expr.vector(Collection(kind: .immutableVector, exprs: exprs))]
        }
      case .record(let col):
        if case .record(let icoll) = col.kind,
           case .recordType = icoll.kind {
          var res: [Any?] = [Expr.makeString(col.identityString)]
          for x in col.exprs {
            res.append(x)
          }
          return res
        }
      case .tagged(.pair(.symbol(_), _), let repr):
        return [repr]
      case .tagged(.object(_), let repr):
        return [repr]
      case .object(let obj):
        let exprs = obj.unpack()
        var res: [Any?] = []
        for x in exprs {
          res.append(x)
        }
        return res
      case .error(let err):
        let filePath: Expr
        if let path = context?.sources.sourcePath(for: err.pos.sourceId) {
          filePath = .makeString(path)
        } else {
          filePath = .false
        }
        let position = Expr.pair(filePath,
                           .pair(err.pos.lineIsUnknown ? .false : .fixnum(Int64(err.pos.line)),
                           .pair(err.pos.columnIsUnknown ? .false : .fixnum(Int64(err.pos.column)),
                           .null)))
        let typeId: Int
        switch err.descriptor {
          case .lexical(_):
            typeId = 0
          case .syntax(_):
            typeId = 1
          case .type(_, _):
            typeId = 2
          case .range(_, _, _, _):
            typeId = 3
          case .argumentCount(_, _, _):
            typeId = 4
          case .eval(_):
            typeId = 5
          case .os(_):
            typeId = 6
          case .abortion:
            typeId = 7
          case .uncaught:
            typeId = 8
          case .custom(_, _):
            typeId = 9
        }
        let type = Expr.pair(.makeNumber(typeId),
                       .pair(.makeString(err.descriptor.typeDescription),
                       .null))
        var usedIrritants = Set<Int>()
        var message = err.replacePlaceholders(in: err.descriptor.messageTemplate,
                                              with: err.irritants,
                                              recordingUsage: &usedIrritants)
        var irritants: Exprs = []
        for index in err.irritants.indices {
          if !usedIrritants.contains(index) {
            irritants.append(err.irritants[index])
          }
        }
        var callTrace: Exprs = []
        if let trace = err.callTrace {
          for call in err.callTrace ?? [] {
            callTrace.append(.makeString(call))
          }
        } else if let stackTrace = err.stackTrace {
          for proc in stackTrace {
            callTrace.append(.makeString(proc.name))
          }
        }
        return [position,
                Expr.makeNumber(typeId),
                Expr.makeString(err.descriptor.typeDescription),
                Expr.makeString(message),
                Expr.vector(Collection(kind: .immutableVector, exprs: irritants)),
                err.library ?? Expr.false,
                Expr.vector(Collection(kind: .immutableVector, exprs: callTrace))]
      default:
        break
    }
    return [expr]
  }
  
  public func apply(context: CLFormat.Context,
                    parameters: CLFormat.Parameters,
                    modifiers: CLFormat.Modifiers,
                    arguments: CLFormat.Arguments) throws -> CLFormat.Instruction {
    let str: String
    if let arg = try arguments.next() {
      if let expr = arg as? Expr,
         case .some(.symbol(let typeSym)) = self.typeLibrary?.typeOf(expr: expr),
         let formatConfig = context.config.environment["formatConfig"] as? FormatConfig,
         let formatControl = formatConfig.control(for: typeSym) {
        let unpacked = Self.unpack(expr)
        let args = context.config.makeArguments(locale: arguments.locale,
                                                tabsize: arguments.tabsize,
                                                linewidth: arguments.linewidth,
                                                args: unpacked)
        var config = context.config
        if let env = formatControl.env {
          config.environment["formatConfig"] = try env.rebase(with: formatConfig)
        }
        str = try formatControl.control.format(with: args, in: context.reconfig(config)).string
      } else if let x = arg as? CustomStringConvertible {
        str = x.description
      } else {
        str = "\(arg)"
      }
    } else {
      str = context.config.nilRepresentation
    }
    return .append(StandardDirectiveSpecifier.pad(string: str,
                                                  left: modifiers.contains(.at),
                                                  right: !modifiers.contains(.at),
                                                  padchar: try parameters.character(3) ?? " ",
                                                  ellipsis: try parameters.character(5) ?? "…",
                                                  mincol: try parameters.number(0) ?? 0,
                                                  colinc: try parameters.number(1) ?? 1,
                                                  minpad: try parameters.number(2) ?? 0,
                                                  maxcol: try parameters.number(4)))
  }
  
  public var description: String {
    return String(self.identifier)
  }
}

