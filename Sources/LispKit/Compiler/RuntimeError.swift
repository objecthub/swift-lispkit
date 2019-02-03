//
//  RuntimeError.swift
//  LispKit
//
//  Created by Matthias Zenger on 20/11/2015.
//  Copyright © 2015-2018 ObjectHub. All rights reserved.
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

///
/// Class `RuntimeError` defines a universal representation of errors in LispKit. A runtime
/// error consists of the following components:
///    - `pos`: The position of the error in some source code (if available)
///    - `descriptor`: A structured descriptor of the error
///    - `irritants`: An array of expressions that the descriptor may refer to
///    - `stackTrace`: An optional stack trace in terms of the invoked LispKit functions
///
public class RuntimeError: Error, Hashable, CustomStringConvertible {
  public let pos: SourcePosition
  public let descriptor: ErrorDescriptor
  public let irritants: [Expr]
  public private(set) var library: Expr?
  public private(set) var stackTrace: [Procedure]?
  
  private init(_ pos: SourcePosition,
               _ descriptor: ErrorDescriptor,
               _ irritants: [Expr],
               _ stackTrace: [Procedure]? = nil) {
    self.pos = pos
    self.descriptor = descriptor
    self.irritants = irritants
    self.stackTrace = nil
  }
  
  public class func lexical(_ error: LexicalError,
                            at pos: SourcePosition = SourcePosition.unknown) -> RuntimeError {
    return RuntimeError(pos, ErrorDescriptor.lexical(error), [])
  }
  
  public class func syntax(_ error: SyntaxError,
                           at pos: SourcePosition = SourcePosition.unknown) -> RuntimeError {
    return RuntimeError(pos, ErrorDescriptor.syntax(error), [])
  }
  
  public class func type(_ expr: Expr,
                         expected: Set<Type>,
                         at pos: SourcePosition = SourcePosition.unknown) -> RuntimeError {
    return RuntimeError(pos, ErrorDescriptor.type(expr.type, expected), [expr])
  }
  
  public class func range(parameter: Int? = nil,
                          of: String? = nil,
                          _ expr: Expr,
                          min: Int64 = Int64.min,
                          max: Int64 = Int64.max,
                          at pos: SourcePosition = SourcePosition.unknown) -> RuntimeError {
    return RuntimeError(pos, ErrorDescriptor.range(of, parameter, min, max), [expr])
  }
  
  public class func argumentCount(of: String? = nil,
                                  min: Int = 0,
                                  max: Int = Int.max,
                                  expr: Expr,
                                  at pos: SourcePosition = SourcePosition.unknown) -> RuntimeError {
    guard case .pair(let fun, let args) = expr else {
      return RuntimeError.argumentCount(of: of, min: min, max: max, args: expr, at: pos)
    }
    if of == nil, case .symbol(let sym) = fun {
      return RuntimeError.argumentCount(
               of: sym.description, min: min, max: max, args: args, at: pos)
    }
    return RuntimeError.argumentCount(of: of, min: min, max: max, args: args, at: pos)
  }
  
  public class func argumentCount(of: String? = nil,
                                  min: Int = 0,
                                  max: Int = Int.max,
                                  args: Expr,
                                  at pos: SourcePosition = SourcePosition.unknown) -> RuntimeError {
    return RuntimeError(pos,
                        ErrorDescriptor.argumentCount(of, min, max),
                        [.makeNumber(args.toExprs().0.count), args])
  }
  
  public class func argumentCount(of: String? = nil,
                                  num: Int,
                                  expr: Expr,
                                  at pos: SourcePosition = SourcePosition.unknown) -> RuntimeError {
    guard case .pair(let fun, let args) = expr else {
      return RuntimeError.argumentCount(of: of, num: num, args: expr, at: pos)
    }
    if of == nil, case .symbol(let sym) = fun {
      return RuntimeError.argumentCount(of: sym.description, num: num, args: args, at: pos)
    }
    return RuntimeError.argumentCount(of: of, num: num, args: args, at: pos)
  }
  
  public class func argumentCount(of: String? = nil,
                                  num: Int,
                                  args: Expr,
                                  at pos: SourcePosition = SourcePosition.unknown) -> RuntimeError {
    return RuntimeError(pos,
                        ErrorDescriptor.argumentCount(of, num, num),
                        [.makeNumber(args.toExprs().0.count), args])
  }
  
  public class func eval(_ error: EvalError,
                         _ irritants: Expr...,
                         at pos: SourcePosition = SourcePosition.unknown) -> RuntimeError {
    return RuntimeError(pos, ErrorDescriptor.eval(error), irritants)
  }
  
  public class func os(_ error: NSError,
                       at pos: SourcePosition = SourcePosition.unknown) -> RuntimeError {
    return RuntimeError(SourcePosition.unknown, ErrorDescriptor.os(error), [])
  }
  
  public class func abortion(at pos: SourcePosition = SourcePosition.unknown,
                             stackTrace: [Procedure]? = nil) -> RuntimeError {
    return RuntimeError(pos, ErrorDescriptor.abortion, [], stackTrace)
  }
  
  public class func custom(_ kind: String,
                           _ template: String,
                           _ irritants: [Expr],
                           at pos: SourcePosition = SourcePosition.unknown) -> RuntimeError {
    return RuntimeError(SourcePosition.unknown, ErrorDescriptor.custom(kind, template), irritants)
  }
  
  public func at(_ pos: SourcePosition) -> RuntimeError {
    return RuntimeError(pos, self.descriptor, self.irritants, self.stackTrace)
  }
  
  @discardableResult public func attach(stackTrace: [Procedure]) -> RuntimeError {
    if self.stackTrace == nil {
      self.stackTrace = stackTrace
    }
    return self
  }
  
  @discardableResult public func attach(library: Expr) -> RuntimeError {
    if self.library == nil {
      self.library = library
    }
    return self
  }
  
  public func hash(into hasher: inout Hasher) {
    for irritant in self.irritants {
      hasher.combine(irritant)
    }
    hasher.combine(self.descriptor)
    hasher.combine(self.pos)
  }
  
  public var message: String {
    var usedIrritants = Set<Int>()
    return self.replacePlaceholders(in: self.descriptor.messageTemplate,
                                    with: self.irritants,
                                    recordingUsage: &usedIrritants)
  }
  
  public var description: String {
    var usedIrritants = Set<Int>()
    let message = self.replacePlaceholders(in: self.descriptor.messageTemplate,
                                           with: self.irritants,
                                           recordingUsage: &usedIrritants)
    var builder = StringBuilder(prefix: "[\(self.descriptor.typeDescription)] \(message)",
                                postfix: "",
                                separator: ", ",
                                initial: ": ")
    for index in self.irritants.indices {
      if !usedIrritants.contains(index) {
        builder.append(self.irritants[index].description)
      }
    }
    return builder.description
  }
  
  public var inlineDescription: String {
    var usedIrritants = Set<Int>()
    let message = self.replacePlaceholders(in: self.descriptor.messageTemplate,
                                           with: self.irritants,
                                           recordingUsage: &usedIrritants)
    return "\(self.descriptor.typeDescription): \(message)"
  }
  
  public func printableDescription(context: Context,
                                   typeOpen: String = "[",
                                   typeClose: String = "] ",
                                   irritantHeader: String? = "irritants: ",
                                   irritantSeparator: String = ", ",
                                   positionHeader: String? = "at: ",
                                   libraryHeader: String? = "library: ",
                                   stackTraceHeader: String? = "stack trace: ",
                                   stackTraceSeparator: String = ", ") -> String {
    var usedIrritants = Set<Int>()
    let message = self.replacePlaceholders(in: self.descriptor.messageTemplate,
                                           with: self.irritants,
                                           recordingUsage: &usedIrritants)
    var builder = StringBuilder(
          prefix: "\(typeOpen)\(self.descriptor.typeDescription)\(typeClose)\(message)",
          postfix: "",
          separator: "\n",
          initial: "")
    if let irritantHeader = irritantHeader {
      var irritantBuilder = StringBuilder(prefix: "",
                                          postfix: "",
                                          separator: irritantSeparator,
                                          initial: irritantHeader)
      for index in self.irritants.indices {
        if !usedIrritants.contains(index) {
          irritantBuilder.append(self.irritants[index].description)
        }
      }
      builder.append(irritantBuilder.description)
    }
    if let positionHeader = positionHeader, !self.pos.isUnknown {
      if let filename = context.sources.sourcePath(for: pos.sourceId) {
        builder.append("\(positionHeader)\(self.pos.description):\(filename)")
      } else {
        builder.append("\(positionHeader)\(self.pos.description)")
      }
    }
    if let libraryHeader = libraryHeader,
       let libraryName = self.library?.description {
      builder.append("\(libraryHeader)\(libraryName)")
    }
    if let stackTraceHeader = stackTraceHeader,
       let stackTrace = self.stackTrace {
      builder = StringBuilder(prefix: builder.description,
                              postfix: "",
                              separator: stackTraceSeparator,
                              initial: "\n\(stackTraceHeader)")
      for proc in stackTrace {
        builder.append(proc.name)
      }
    }
    return builder.description
  }
  
  public func mark(_ tag: UInt8) {
    for irritant in self.irritants {
      irritant.mark(tag)
    }
    if let stackTrace = self.stackTrace {
      for procedure in stackTrace {
        procedure.mark(tag)
      }
    }
  }
  
  /// This method assumes the string contains variables of the form `$n` where `n` is a
  /// variable index into the array `values`. It replaces occurences of `$n` with the value at
  /// index `n`. If there is no such value or the index is not well-formed, the variable
  /// reference remains in the string.
  /// It is possible to terminate parsing the index `n` by using "~". For instance, "$0~1" gets
  /// expanded into "zero1" assuming that "zero" is the value for variable `0`.
  /// It is possible to escape both "$" and "~" by prefixing the characters with "$". For
  /// instance, "$$0" gets expanded into "$0".
  /// It is possible to use placeholders of the form `$,n` for embedding a value using
  /// an unescaped string representation of the value.
  private func replacePlaceholders(in template: String,
                                   with values: [Expr],
                                   recordingUsage used: inout Set<Int>) -> String {
    var res: String = ""
    var variable: String = ""
    var parsingVariable = false
    var embedVariable = false
    for ch in template {
      if parsingVariable {
        switch ch {
          case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9":
            variable.append(ch)
            continue
          case "~":
            if variable.isEmpty {
              if embedVariable {
                res.append("$,~")
              } else {
                res.append(ch)
              }
              parsingVariable = false
              embedVariable = false
              continue
            }
          case "$":
            if variable.isEmpty {
              if embedVariable {
                res.append("$,$")
              } else {
                res.append(ch)
              }
              parsingVariable = false
              embedVariable = false
              continue
            }
          case ",":
            if variable.isEmpty {
              if embedVariable {
                res.append("$,,")
                parsingVariable = false
                embedVariable = false
              } else {
                embedVariable = true
              }
              continue
            }
          default:
            if variable.isEmpty {
              res.append("$")
              if embedVariable {
                res.append(",")
              }
              res.append(ch)
              parsingVariable = false
              embedVariable = false
              continue
            }
            break
        }
        let varNum = Int(variable)
        if varNum != nil && varNum! >= 0 && varNum! < values.count {
          if embedVariable {
            res.append(values[varNum!].unescapedDescription)
          } else {
            res.append(values[varNum!].description)
          }
          used.insert(varNum!)
          variable = ""
          parsingVariable = false
          embedVariable = false
        } else {
          res.append("$")
          if embedVariable {
            res.append(",")
          }
          res.append(variable)
          variable = ""
          parsingVariable = false
          embedVariable = false
          if ch == "~" {
            res.append("~")
            continue
          }
        }
        if ch == "$" {
          parsingVariable = true
        } else if ch != "~" {
          res.append(ch)
        }
      } else if ch == "$" {
        parsingVariable = true
      } else {
        res.append(ch)
      }
    }
    if parsingVariable {
      let varNum = Int(variable)
      if varNum != nil && varNum! >= 0 && varNum! < values.count {
        var value = values[varNum!]
        if embedVariable {
          switch value {
            case .pair(_, _):
              var builder = StringBuilder(prefix: "", postfix: "", separator: ", ")
              while case .pair(let car, let cdr) = value {
                builder.append(car.unescapedDescription)
                value = cdr
              }
              res.append(builder.description)
              if !value.isNull {
                res.append(" (")
                res.append(value.unescapedDescription)
                res.append(")")
              }
            default:
              res.append(value.unescapedDescription)
          }
        } else {
          res.append(value.description)
        }
        used.insert(varNum!)
      } else {
        res.append("$")
        if embedVariable {
          res.append(",")
        }
        res.append(variable)
      }
    }
    return res
  }
  
  public static func ==(_ lhs: RuntimeError, _ rhs: RuntimeError) -> Bool {
    return lhs.pos == rhs.pos &&
           lhs.descriptor == rhs.descriptor &&
           lhs.irritants == rhs.irritants &&
           (lhs.stackTrace == nil && rhs.stackTrace == nil ||
            lhs.stackTrace != nil && rhs.stackTrace != nil && lhs.stackTrace! == rhs.stackTrace!)
  }
}


///
/// An `ErrorDescriptor` value describes an error in a structured way. Error descriptors may
/// not encapsulate expressions, but their textual description may refer to them via the reference
/// `$n` or `$,n` (see method `replacePlaceholders` above).
///
public enum ErrorDescriptor: Hashable {
  case lexical(LexicalError)
  case syntax(SyntaxError)
  case type(Type, Set<Type>)
  case range(String?, Int?, Int64, Int64)
  case argumentCount(String?, Int, Int)
  case eval(EvalError)
  case os(NSError)
  case abortion
  case custom(String, String)
  
  
  public var isFileError: Bool {
    guard case .eval(let err) = self else {
      return false
    }
    switch err {
      case .cannotOpenFile, .cannotOpenUrl, .cannotWriteToPort:
        return true
      default:
        return false
    }
  }
  
  public var isReadError: Bool {
    switch self {
      case .lexical(_), .syntax(_):
        return true
      default:
        return false
    }
  }
  
  public var typeDescription: String {
    switch self {
      case .lexical(_):
        return "lexical error"
      case .syntax(_):
        return "syntax error"
      case .type(_, _):
        return "type error"
      case .range(_, _, _, _):
        return "range error"
      case .argumentCount(_, _, _):
        return "argument count error"
      case .eval(_):
        return "eval error"
      case .os(_):
        return "os error"
      case .abortion:
        return "abortion"
      case .custom(let type, _):
        return type
    }
  }
  
  public var messageTemplate: String {
    switch self {
      case .lexical(let error):
        return error.message
      case .syntax(let error):
        return error.message
      case .type(let found, let expected):
        guard expected.count > 0 else {
          return "unexpected expression $0"
        }
        var tpe: Type? = nil
        var res = ""
        for type in expected {
          if let t = tpe {
            res += (res.isEmpty ? "" : ", ") + t.description
          }
          tpe = type
        }
        if res.isEmpty {
          res = "a " + tpe!.description
        } else {
          res = "either a " + res + " or " + tpe!.description
        }
        return "$0 is of type \(found.description), but is required to be \(res) value"
      case .range(let fun, let par, let low, let high):
        if let fun = fun, let par = par {
          if low == Int64.min {
            if high == 0 {
              return "expected argument \(par) of function \(fun) to be a negative integer " +
                     "value; is $0 instead"
            } else {
              return "expected argument \(par) of function \(fun) to be an integer value less " +
                     "than equals \(high); is $0 instead"
            }
          } else if high == Int64.max {
            if low == 0 {
              return "expected argument \(par) of function \(fun) to be a positive integer " +
                     "value; is $0 instead"
            } else {
              return "expected argument \(par) of function \(fun) to be an integer value greater " +
                     "than equals \(low); is $0 instead"
            }
          } else {
            return "expected argument \(par) of function \(fun) to be an integer value within " +
                   "the range [\(low), \(high)]; is $0 instead"
          }
        } else if low == Int64.min {
          if high == 0 {
            return "expected $0 to be a negative integer value"
          } else {
            return "expected $0 to be an integer value less than equals \(high)"
          }
        } else if high == Int64.max {
          if low == 0 {
            return "expected $0 to be a positive integer value"
          } else {
            return "expected $0 to be an integer value greater than equals \(low)"
          }
        } else {
          return "expected $0 to be an integer value within the range [\(low), \(high)]"
        }
      case .argumentCount(let fun, let min, let max):
        if let fun = fun {
          if min == max {
            return "\(fun) expects \(self.arguments(min)), but received $0 arguments: $1"
          } else if max == Int.max {
            return "\(fun) expects at least \(self.arguments(min)), but received only " +
                   "$0 arguments: $1"
          } else {
            return "\(fun) expects between \(min) and \(self.arguments(max)), but received $0 " +
                   "arguments: $1"
          }
        } else {
          if min == max {
            return "function expects \(self.arguments(min)), but received $0 arguments: $1"
          } else if max == Int.max {
            return "function expects at least \(self.arguments(min)), but received only " +
                   "$0 arguments: $1"
          } else {
            return "function expects between \(min) and \(self.arguments(max)), but received " +
                   "$0 arguments: $1"
          }
        }
      case .eval(let error):
        return error.message
      case .os(let error):
        if let underlying = error.userInfo[NSUnderlyingErrorKey] as? NSError {
          let prefix = underlying.localizedFailureReason ?? underlying.localizedDescription
          return "\(prefix): \(error.localizedDescription) (\(error.code))"
        } else {
          return "\(error.localizedDescription) (\(error.code))"
        }
      case .abortion:
        return "abortion"
      case .custom(_, let message):
        return message
    }
  }
  
  private func arguments(_ n: Int) -> String {
    if n == 1 {
      return "1 argument"
    } else {
      return "\(n) arguments"
    }
  }
  
  public func hash(into hasher: inout Hasher) {
    switch self {
      case .lexical(let error):
        hasher.combine(error)
      case .syntax(let error):
        hasher.combine(1)
        hasher.combine(error)
      case .type(let found, let expected):
        hasher.combine(2)
        hasher.combine(found)
        hasher.combine(expected)
      case .range(let fun, let argn, let low, let high):
        hasher.combine(3)
        hasher.combine(fun)
        hasher.combine(argn)
        hasher.combine(low)
        hasher.combine(high)
      case .argumentCount(let fun, let min, let max):
        hasher.combine(4)
        hasher.combine(fun)
        hasher.combine(min)
        hasher.combine(max)
      case .eval(let error):
        hasher.combine(5)
        hasher.combine(error)
      case .os(let error):
        hasher.combine(6)
        hasher.combine(error)
      case .abortion:
        hasher.combine(7)
      case .custom(let kind, let message):
        hasher.combine(8)
        hasher.combine(kind)
        hasher.combine(message)
    }
  }
  
  public static func ==(_ lhs: ErrorDescriptor, _ rhs: ErrorDescriptor) -> Bool {
    switch (lhs, rhs) {
      case (.lexical(let lerr), .lexical(let rerr)):
        return lerr == rerr
      case (.syntax(let lerr), .syntax(let rerr)):
        return lerr == rerr
      case (.type(let lfound, let lexp), .type(let rfound, let rexp)):
          return lfound == rfound && lexp == rexp
      case (.range(let lfun, let largn, let llow, let lhigh),
            .range(let rfun, let rargn, let rlow, let rhigh)):
        return lfun == rfun && largn == rargn && llow == rlow && lhigh == rhigh
      case (.argumentCount(let lfun, let lmin, let lmax),
            .argumentCount(let rfun, let rmin, let rmax)):
        return lfun == rfun && lmin == rmin && lmax == rmax
      case (.eval(let lerr), .eval(let rerr)):
        return lerr == rerr
      case (.os(let lerr), .os(let rerr)):
        return lerr == rerr
      case (.abortion, .abortion):
        return true
      case (.custom(let lkind, let lmessage), .custom(let rkind, let rmessage)):
        return lkind == rkind && lmessage == rmessage
     default:
        return false
    }
  }
}

