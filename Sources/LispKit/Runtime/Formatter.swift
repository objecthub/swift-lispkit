//
//  Formatter.swift
//  LispKit
//
//  Created by Matthias Zenger on 02/06/2023.
//  Copyright © 2023 ObjectHub. All rights reserved.
//

import Foundation
import CLFormat

open class Formatter {
  
  /// The context of this virtual machine
  public unowned let context: Context
  
  /// The implementation of the s-expr directive
  private let sexprDirSpec: SExprDirectiveSpecifier
  
  /// Configuration of the `clformat` function
  public let clFormatConfig: CLFormatConfig
  
  /// Base configuration of the `format` procedure
  public let baseFormatConfig: FormatConfig
  
  /// Configuration of the `format` procedure for usage by REPLs
  public let replFormatConfig: FormatConfig
  
  /// Exported parameter objects
  public let formatConfigParam: Procedure
  
  /// Constructor of the formatter
  public init(for context: Context) {
    self.context = context
    let formatConfig = FormatConfig()
    formatConfig.locale = Locale.current
    formatConfig.tabWidth = 8
    formatConfig.lineWidth = 80
    let sexprDirSpec =  SExprDirectiveSpecifier(context: context)
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
    self.replFormatConfig = FormatConfig(outerConfig: formatConfig)
    self.formatConfigParam = Procedure(.null, .object(formatConfig))
  }
  
  open func format(_ control: String,
                   config: FormatConfig? = nil,
                   locale: Locale? = nil,
                   tabsize: Int? = nil,
                   linewidth: Int? = nil,
                   arguments: [Expr]) throws -> String {
    let fconfig = FormatConfig(outerConfig: config ?? self.defaultFormatConfig())
    if let locale = locale {
      fconfig.locale = locale
    }
    if let tabsize = tabsize {
      fconfig.tabWidth = tabsize
    }
    if let linewidth = linewidth {
      fconfig.lineWidth = linewidth
    }
    var clFormatConfig = self.clFormatConfig
    clFormatConfig.environment["formatConfig"] = fconfig
    return try clformat(control as String,
                        config: clFormatConfig,
                        locale: fconfig.getLocale(),
                        tabsize: fconfig.getTabWidth(),
                        linewidth: fconfig.getLineWidth(),
                        arguments: arguments)
  }
  
  public func defaultFormatConfig() -> FormatConfig {
    guard let value = self.context.evaluator.getParam(self.formatConfigParam) else {
      return self.baseFormatConfig
    }
    guard case .object(let obj) = value, let config = obj as? FormatConfig else {
      return self.baseFormatConfig
    }
    return config
  }
}

public final class FormatConfig: NativeObject {
  
  public static let empty = FormatConfig(outerConfig: nil)
  
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
  
  public init(collapse: FormatConfig, outer: FormatConfig? = nil) {
    self.locale = collapse.getLocale()
    self.tabWidth = collapse.getTabWidth()
    self.lineWidth = collapse.getLineWidth()
    self.controlDict = collapse.controlDict
    self.outerConfig = outer
    var parent = collapse.outerConfig
    while let fconf = parent {
      self.controlDict.merge(fconf.controlDict) { (current, _) in current }
      parent = fconf.outerConfig
    }
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

public class SExprDirectiveSpecifier: DirectiveSpecifier {
  public unowned let context: Context
  
  public init(context: Context) {
    self.context = context
  }
  
  public var identifier: Character {
    return "S"
  }
  
  public func apply(context: CLFormat.Context,
                    parameters: CLFormat.Parameters,
                    modifiers: CLFormat.Modifiers,
                    arguments: CLFormat.Arguments) throws -> CLFormat.Instruction {
    let str: String
    if let arg = try arguments.next() {
      if let expr = arg as? Expr,
         case .some(let typeSym) = expr.typeTag(in: self.context),
         let formatConfig = context.config.environment["formatConfig"] as? FormatConfig,
         let formatControl = formatConfig.control(for: typeSym) {
        var unpacked: [Any?] = []
        for arg in expr.unpack(in: self.context) ?? [expr] {
          unpacked.append(arg)
        }
        let args = context.config.makeArguments(locale: arguments.locale,
                                                tabsize: arguments.tabsize,
                                                linewidth: arguments.linewidth,
                                                args: unpacked)
        var config = context.config
        if let env = formatControl.env {
          config.environment["formatConfig"] = FormatConfig(collapse: env, outer: formatConfig)
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
            var args: [Any?] = []
            for arg in expr.unpack() ?? [expr] {
              args.append(arg)
            }
            unpacked = args
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