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
  
  /// Configure the `clformat` function
  private let clformatConfig: CLFormatConfig = {
    var config = CLFormatConfig.standard
    config.makeArguments = FormatArguments.init
    config.parse("s", "S", appending: LispKitDirectiveSpecifier.sexpr)
    return config
  }()
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "format"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("format", format))
  }
  
  private func format(_ args: Arguments) throws -> Expr {
    var output: TextOutput? = nil
    var locale: Locale? = nil
    var tabsize = 4
    var linelen = 80
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
    if case .some(.symbol(let sym)) = arg {
      locale = Locale(identifier: sym.identifier)
      arg = iterator.next()
    }
    if case .some(.fixnum(let num)) = arg {
      guard let tsize = Int(exactly: num), tsize > 0, tsize <= 1000 else {
        throw RuntimeError.range(arg!, min: 1, max: 1000)
      }
      tabsize = tsize
      arg = iterator.next()
    }
    if case .some(.fixnum(let num)) = arg {
      guard let len = Int(exactly: num), len > 0 else {
        throw RuntimeError.range(arg!, min: 1, max: Int64(Int.max))
      }
      linelen = len
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
    let res = try clformat(control as String,
                           config: self.clformatConfig,
                           locale: locale,
                           tabsize: tabsize,
                           linewidth: linelen,
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
}

class FormatArguments: CLFormat.Arguments {
  
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
        while i < capAt {
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
  case sexpr
  
  public var identifier: Character {
    switch self {
      case .sexpr:
        return "S"
    }
  }
  
  public func apply(context: CLFormat.Context,
                    parameters: CLFormat.Parameters,
                    modifiers: CLFormat.Modifiers,
                    arguments: CLFormat.Arguments) throws -> CLFormat.Instruction {
    switch self {
      case .sexpr:
        return try StandardDirectiveSpecifier.write.apply(context: context,
                                                          parameters: parameters,
                                                          modifiers: modifiers,
                                                          arguments: arguments)
        /* let str: String
        if let arg = try arguments.next() {
          if let x = arg as? CustomStringConvertible {
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
                                                      maxcol: try parameters.number(4))) */
    }
  }
  
  public var description: String {
    return String(self.identifier)
  }
}
