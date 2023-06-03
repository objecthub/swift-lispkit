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
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "format"]
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define("base-format-config", as: .object(self.context.formatter.baseFormatConfig))
    self.define("current-format-config", as: self.context.formatter.formatConfigParam)
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
  
  public func defaultFormatConfig() throws -> FormatConfig {
    guard let value =
                self.context.evaluator.getParam(self.context.formatter.formatConfigParam) else {
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
    var clFormatConfig = self.context.formatter.clFormatConfig
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
          throw RuntimeError.eval(.controlStringMissing, .pair(fst, .pair(snd, .null)))
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
        throw RuntimeError.eval(.cannotUseFormatConfigWithParent, .object(conf), typeExpr)
      }
      fconf.format(typeTag,
                   with: try CLControl(string: controlExpr.asString(),
                                       config: self.context.formatter.clFormatConfig),
                   in: conf)
    } else {
      fconf.format(typeTag,
                   with: try CLControl(string: controlExpr.asString(),
                                       config: self.context.formatter.clFormatConfig),
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
