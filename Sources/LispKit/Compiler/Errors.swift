//
//  Errors.swift
//  LispKit
//
//  Created by Matthias Zenger on 20/11/2015.
//  Copyright © 2016 ObjectHub. All rights reserved.
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
/// Definition of the LispKit error protocol. A `LispError` object provides the following
/// components:
///    - An error type: Every implementation of `LispError` implements errors of a certain type.
///    - An error message: A string representation of the error.
///    - The error irritants: These are LispKit expressions which are associated with the error.
///      The error message might refer to the irritants.
///
public protocol LispError: Error, CustomStringConvertible {
  var root: LispError { get }
  var type: LispErrorType { get }
  var message: String { get }
  var irritants: Exprs { get }
  func equals(_ other: LispError) -> Bool
}

/// This extension defines a few default implementations for `LispError` implementations.
public extension LispError {
  
  public var root: LispError {
    return self
  }
  
  public var hashValue: Int {
    var res = 0
    for irritant in self.irritants {
      res = res &* 31 &+ irritant.hashValue
    }
    res = res &* 31 &+ self.message.hashValue
    return res &* 31 &+ self.type.hashValue
  }
  
  public var description: String {
    var res = "\(self.type) | \(self.message)"
    if self.irritants.count > 0 {
      res += ": "
      var sep = ""
      for expr in self.irritants {
        res += sep
        res += expr.description
        sep = ", "
      }
    }
    return res
  }
  
  public static func ==(lhs: LispError, rhs: LispError) -> Bool {
    return lhs.equals(rhs)
  }
}


///
/// Every `LispError` implementation has a unique type associated. This enumeration lists
/// all available types.
///
public enum LispErrorType: Hashable, CustomStringConvertible {
  case lexicalError
  case syntaxError
  case evalError
  case osError
  case abortionError
  case customError(kind: String)
  
  public var hashValue: Int {
    switch self {
      case .lexicalError:
        return 0
      case .syntaxError:
        return 1
      case .evalError:
        return 2
      case .osError:
        return 3
      case .abortionError:
        return 4
      case .customError(let kind):
        return (kind.hashValue &* 31) &+ 5
    }
  }
  
  public var description: String {
    switch self {
      case .lexicalError:
        return "lexical error"
      case .syntaxError:
        return "syntax error"
      case .evalError:
        return "evaluation error"
      case .osError:
        return "os error"
      case .abortionError:
        return "abortion"
      case .customError(let kind):
        return kind
    }
  }
}

public func ==(lhs: LispErrorType, rhs: LispErrorType) -> Bool {
  switch (lhs, rhs) {
    case (.lexicalError, .lexicalError),
         (.syntaxError, .syntaxError),
         (.evalError, .evalError),
         (.osError, .osError),
         (.abortionError, .abortionError):
      return true
    case (.customError(let lkind), .customError(let rkind)):
      return lkind == rkind
    default:
      return false
  }
}


///
/// Class `AnyError` wraps `LispError` implementations. The main motivation for using this class
/// is related to the high cost of storing references to protocols (especially in the definition
/// of the `Expr` enumeration). Another reason is that Swift 4 does not allow protocols to
/// implement Hashable (which, in practice, needs to be attached to a class, struct or enum).
///
public class AnyError: LispError, Hashable, CustomStringConvertible {
  private let error: LispError
  
  public init(_ error: LispError) {
    if let anyError = error as? AnyError {
      self.error = anyError.error
    } else {
      self.error = error
    }
  }
  
  public var root: LispError {
    return self.error.root
  }
  
  public var type: LispErrorType {
    return error.type
  }
  
  public var message: String {
    return error.message
  }
  
  public var irritants: Exprs {
    return error.irritants
  }
  
  public var stackTrace: [Procedure] {
    return []
  }
  
  public func equals(_ other: LispError) -> Bool {
    if let that = other as? AnyError {
      return self.error.equals(that.error)
    } else {
      return false
    }
  }
  
  public var printableDescription: String {
    var builder = StringBuilder(prefix: "\(self.type): \(self.message)",
                                separator: ", ",
                                initial: "\nirritants: ")
    for expr in self.irritants {
      builder.append(expr.description)
    }
    return builder.description
  }
}

public func ==(lhs: AnyError, rhs: AnyError) -> Bool {
  return lhs.equals(rhs)
}

///
/// Class `ContextualError` wraps `LispError` implementations adding a stack trace.
///
public final class ContextualError: AnyError {
  public let procedures: [Procedure]
  
  public init(_ error: LispError, _ stackTrace: [Procedure]) {
    self.procedures = stackTrace
    super.init(error)
  }
  
  public override var stackTrace: [Procedure] {
    return self.procedures
  }
  
  public override var printableDescription: String {
    var builder = StringBuilder(prefix: super.printableDescription,
                                separator: ", ",
                                initial: "\nstack trace: ")
    for proc in self.stackTrace {
      builder.append(proc.name)
    }
    return builder.description
  }
}


///
/// Enumeration `LexicalError` represents lexical parsing errors emitted by the
/// scanner.
///
public enum LexicalError: Int, LispError, Hashable {
  case empty
  case malformedIdentifier
  case brokenIdentifierEncoding
  case brokenNumberEncoding
  case numberExpected
  case malformedFloatLiteral
  case malformedComplexLiteral
  case malformedStringLiteral
  case malformedCharacterLiteral
  case unknownCharacterLiteral
  case incompleteCharacterLiteral
  case illegalCharacter
  case illegalHexCharacter
  case illegalEscapeSequence
  case illegalEndOfLine
  case tokenNotYetSupported
  case divisionByZero
  case exactComplexNumbersUnsupported
  case unknownDirective
  
  public var type: LispErrorType {
    return .lexicalError
  }
  
  public var message: String {
    switch self {
      case .empty:
        return "no input available"
      case .malformedIdentifier:
        return "malformed identifier"
      case .brokenIdentifierEncoding:
        return "broken identifier encoding"
      case .brokenNumberEncoding:
        return "broken number encoding"
      case .numberExpected:
        return "expected a number"
      case .malformedFloatLiteral:
        return "malformed floating point number literal"
      case .malformedComplexLiteral:
        return "malformed complex number literal"
      case .malformedStringLiteral:
        return "malformed string literal"
      case .malformedCharacterLiteral:
        return "malformed character literal"
      case .unknownCharacterLiteral:
        return "unknown character literal"
      case .incompleteCharacterLiteral:
        return "incomplete character literal"
      case .illegalCharacter:
        return "illegal character"
      case .illegalHexCharacter:
        return "illegal hex character"
      case .illegalEscapeSequence:
        return "illegal escape sequence"
      case .illegalEndOfLine:
        return "illegal end of line"
      case .tokenNotYetSupported:
        return "token not yet supported"
      case .divisionByZero:
        return "division by zero"
      case .exactComplexNumbersUnsupported:
        return "exact complex numnbers are not supported"
      case .unknownDirective:
        return "unknown directive"
    }
  }
  
  public var irritants: Exprs {
    return noExprs
  }
  
  public func equals(_ error: LispError) -> Bool {
    if let other = error as? LexicalError {
      return self.rawValue == other.rawValue
    }
    return false
  }
}


///
/// Enumeration `SyntaxError` represents syntactical errors emitted by the parser.
///
public enum SyntaxError: Int, LispError, Hashable {
  case empty
  case closingParenthesisMissing
  case unexpectedClosingParenthesis
  case unexpectedDot
  case notAByteValue
  case syntaxNotYetSupported
  
  public var type: LispErrorType {
    return .syntaxError
  }
  
  public var message: String {
    switch self {
      case .empty:
        return "empty input"
      case .closingParenthesisMissing:
        return "closing parenthesis missing"
      case .unexpectedClosingParenthesis:
        return "unexpected closing parenthesis"
      case .unexpectedDot:
        return "unexpected dot"
      case .notAByteValue:
        return "bytevector element not a byte"
      case .syntaxNotYetSupported:
        return "syntax not yet supported"
    }
  }
  
  public var irritants: Exprs {
    return noExprs
  }
  
  public func equals(_ error: LispError) -> Bool {
    if let other = error as? SyntaxError {
      return self.rawValue == other.rawValue
    }
    return false
  }
}


///
/// Enumeration `EvalError` represents errors occuring during the evaluation or compilation
/// of LispKit expressions.
///
public enum EvalError: LispError, Hashable {
  case illegalKeywordUsage(Expr)
  case illegalFormalParameter(Expr)
  case illegalFormalRestParameter(Expr)
  case divisionByZero
  case unboundVariable(Symbol)
  case variableUndefined(Symbol?)
  case variableNotYetInitialized(Symbol?)
  case malformedCaseLambda(Expr)
  case malformedArgumentList(Expr)
  case malformedDefinition(Expr)
  case malformedTransformer(Expr)
  case malformedSyntaxRule(Expr)
  case malformedSyntaxRulePattern(error: Expr?, pattern: Expr)
  case malformedSyntaxRuleLiterals(Expr)
  case invalidContextInQuasiquote(Symbol, Expr)
  case macroMismatchedRepetitionPatterns(Symbol)
  case malformedBindings(Expr?, Expr)
  case malformedTest(Expr)
  case malformedCondClause(Expr)
  case malformedCondExpandClause(Expr)
  case malformedCaseClause(Expr)
  case duplicateBinding(Symbol, Expr)
  case indexOutOfBounds(Int64, Int64)
  case parameterOutOfBounds(String, Int, Int64, Int64, Int64)
  case nonApplicativeValue(Expr)
  case illegalRadix(Expr)
  case typeError(Expr, Set<Type>)
  case argumentError(f: Expr, args: Expr)
  case argumentCountError(formals: Int, args: Expr)
  case noMatchingCase(args: Expr, proc: Expr)
  case leastArgumentCountError(formals: Int, args: Expr)
  case multiValueCountError(expected: Int, found: Expr)
  case outOfScope(Expr)
  case defineInLocalEnv(signature: Expr, definition: Expr, group: BindingGroup)
  case importInLocalEnv(Expr, BindingGroup)
  case importInLibrary(Expr)
  case malformedImportSet(Expr)
  case erroneousRedefinition(Symbol, Library)
  case cannotExpandImportSet(ImportSet)
  case defineSyntaxInLocalEnv(keyword: Symbol, definition: Expr, group: BindingGroup)
  case targetBytevectorTooSmall(Expr)
  case cannotOpenFile(String)
  case cannotOpenUrl(String)
  case invalidUrl(Expr)
  case cannotWriteToPort(Expr)
  case illegalContinuationApplication(Procedure, Int)
  case attemptToModifyImmutableData(Expr)
  case unknownFieldOfRecordType(Expr, Symbol)
  case fieldCountError(Int, Expr)
  case malformedLibraryDefinition(decls: Expr)
  case malformedLibraryName(name: Expr)
  case uninitializedExports([Symbol], Expr)
  case unknownFile(String)
  case unknownDirectory(String)
  case cannotDecodeBytevector(Expr)
  case cannotEncodeBytevector(Expr)
  case invalidDateTime(Expr)
  case invalidTimeZone(Expr)
  
  public var type: LispErrorType {
    return .evalError
  }
  
  public var message: String {
    switch self {
      case .illegalKeywordUsage(let expr):
        return "syntactic keywords may not be used as expressions: \(expr)"
      case .illegalFormalParameter(let expr):
        return "illegal formal parameter: \(expr)"
      case .illegalFormalRestParameter(let expr):
        return "illegal formal rest parameter: \(expr)"
      case .divisionByZero:
        return "division by zero"
      case .unboundVariable(let sym):
        return "unbound variable `\(sym)`"
      case .variableUndefined(let sym):
        guard let sym = sym else {
          return "variable undefined"
        }
        return "variable `\(sym)` undefined"
      case .variableNotYetInitialized(let sym):
        guard let sym = sym else {
          return "variable not yet initialized"
        }
        return "variable `\(sym)` not yet initialized"
      case .malformedCaseLambda(let expr):
        return "malformed lambda case list: \(expr)"
      case .malformedArgumentList(let args):
        return "malformed argument list: \(args)"
      case .malformedDefinition(let def):
        return "malformed definition: \(def)"
      case .malformedTransformer(let transformer):
        return "malformed transformer: \(transformer)"
      case .malformedSyntaxRule(let rule):
        return "not a valid syntax rule: \(rule)"
      case .malformedSyntaxRulePattern(let error, let pattern):
        if let errPat = error {
          return "illegal pattern \(errPat) in syntax rule pattern: \(pattern)"
        } else {
          return "malformed syntax rule pattern: \(pattern)"
        }
      case .malformedSyntaxRuleLiterals(let literals):
        return "malformed list of syntax rule literals: \(literals)"
      case .invalidContextInQuasiquote(let op, let expr):
        return "\(op) in invalid context within quasiquote: \(expr)"
      case .macroMismatchedRepetitionPatterns(let sym):
        return "macro could not be expanded: mismatched repetition patterns: \(sym)"
      case .malformedBindings(nil, let bindingList):
        return "malformed list of bindings: \(bindingList)"
      case .malformedBindings(.some(let binding), _):
        return "malformed binding: \(binding)"
      case .malformedTest(let expr):
        return "malformed test expression: \(expr)"
      case .malformedCondClause(let clause):
        return "malformed clause in cond form: \(clause)"
      case .malformedCondExpandClause(let clause):
        return "malformed clause in cond-expand form: \(clause)"
      case .malformedCaseClause(let clause):
        return "malformed clause in case form: \(clause)"
      case .duplicateBinding(let sym, let expr):
        return "symbol \(sym) bound multiple times in \(expr)"
      case .indexOutOfBounds(let index, let max):
        if index < 0 {
          return "index \(index) is not a non-negative exact integer"
        } else {
          return "index \(index) is out of bounds [0..\(max)]"
        }
      case .parameterOutOfBounds(let fun, let n, let actual, let min, let max):
        return "parameter \(n) of \(fun) out of bounds [\(min)..\(max)]: \(actual)"
      case .nonApplicativeValue(let expr):
        return "cannot apply arguments to: \(expr)"
      case .illegalRadix(let expr):
        return "illegal radix \(expr)"
      case .typeError(let expr, let expected):
        guard expected.count > 0 else {
          return "unexpected expression \(expr)"
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
        return "expected \(expr) to be \(res) value vs. a value of type \(expr.type)"
      case .argumentError(let f, let args):
        return "wrong number of arguments for \(f): \(args)"
      case .argumentCountError(let formals, let args):
        let (exprs, _) = args.toExprs()
        return "expected \(formals), but received \(exprs.count) arguments: \(args)"
      case .noMatchingCase(let args, let proc):
        return "arguments \(args) not matching any case of \(proc)"
      case .leastArgumentCountError(let formals, let args):
        let (exprs, _) = args.toExprs()
        return "expected at least \(formals), but received only \(exprs.count) arguments: \(args)"
      case .multiValueCountError(let expected, let found):
        return "expected \(expected) values to be returned, but found: \(found)"
      case .outOfScope(let syntax):
        return "out of scope evaluation of \(syntax)"
      case .defineInLocalEnv(let sig, _, _):
        return "definition of \(sig) in local environment"
      case .importInLocalEnv(let importSet, _):
        return "import of \(importSet) in local environment"
      case .importInLibrary(let e):
        return "illegal import in library \(e)"
      case .malformedImportSet(let e):
        return "malformed import set \(e)"
      case .erroneousRedefinition(let sym, let lib):
        return "attempted to redefine \(sym) with definition from \(lib)"
      case .cannotExpandImportSet(let i):
        return "cannot expand import set \(i)"
      case .defineSyntaxInLocalEnv(let sym, _, _):
        return "syntax definition of \(sym) in local environment"
      case .targetBytevectorTooSmall(let bvec):
        return "target bytevector too small: \(bvec)"
      case .cannotOpenFile(let filename):
        return "cannot open file '\(filename)'"
      case .cannotOpenUrl(let url):
        return "cannot open URL '\(url)'"
      case .invalidUrl(let expr):
        return "invalid URL: '\(expr)'"
      case .cannotWriteToPort(let port):
        return "cannot write to port \(port)"
      case .illegalContinuationApplication(let proc, let rid):
        return "continuation application in wrong context (\(proc) in \(rid))"
      case .attemptToModifyImmutableData(let expr):
        return "illegal attempt to modify immutable data structure: \(expr)"
      case .unknownFieldOfRecordType(let type, let field):
        return "unknown field \(field) of record type \(type)"
      case .fieldCountError(let expected, let values):
        return "expected values for \(expected) fields, received: \(values)"
      case .malformedLibraryDefinition(let decls):
        return "malformed library definition: \(decls)"
      case .malformedLibraryName(let name):
        return "malformed library name: \(name)"
      case .uninitializedExports(let exports, let lib):
        guard !exports.isEmpty else {
          return "library \(lib) does not initialize all exported definitions"
        }
        let defstr = exports.count == 1 ? " definition " : " definitions "
        var builder = StringBuilder(prefix: "library \(lib) does not initialize exported\(defstr)",
                                    postfix: "",
                                    separator: ", ")
        for sym in exports {
          builder.append(sym.rawIdentifier)
        }
        return builder.description
      case .unknownFile(let path):
        return "file \"\(path)\" unknown or a directory"
      case .unknownDirectory(let path):
        return "directory \"\(path)\" unknown or a file"
      case .cannotDecodeBytevector(let expr):
        return "unable to decode \(expr) into bytevector"
      case .cannotEncodeBytevector(let expr):
        return "unable to encode bytevector \(expr)"
      case .invalidDateTime(let expr):
        return "invalid/incomplete date time component list: \(expr)"
      case .invalidTimeZone(let expr):
        return "invalid/incomplete time zone identifier: \(expr)"
    }
  }
  
  public var irritants: Exprs {
    return noExprs
  }
  
  public func equals(_ error: LispError) -> Bool {
    if let other = error as? EvalError {
      switch (self, other) {
        case (.illegalKeywordUsage(let expr1), .illegalKeywordUsage(let expr2)):
          return expr1 == expr2
        case (.illegalFormalParameter(let expr1), .illegalFormalParameter(let expr2)):
          return expr1 == expr2
        case (.illegalFormalRestParameter(let expr1), .illegalFormalRestParameter(let expr2)):
          return expr1 == expr2
        case (.divisionByZero, .divisionByZero):
          return true
        case (.unboundVariable(let sym1), .unboundVariable(let sym2)):
          return sym1 == sym2
        case (.variableUndefined(let sym1), .variableUndefined(let sym2)):
          return sym1 == sym2
        case (.variableNotYetInitialized(let sym1), .variableNotYetInitialized(let sym2)):
          return sym1 == sym2
        case (.malformedCaseLambda(let e1), .malformedCaseLambda(let e2)):
          return e1 == e2
        case (.malformedArgumentList(let a1), .malformedArgumentList(let a2)):
          return a1 == a2
        case (.malformedDefinition(let def1), .malformedDefinition(let def2)):
          return def1 == def2
        case (.malformedTransformer(let trans1), .malformedTransformer(let trans2)):
          return trans1 == trans2
        case (.malformedSyntaxRule(let rule1), .malformedSyntaxRule(let rule2)):
          return rule1 == rule2
        case (.malformedSyntaxRulePattern(let error1, let pat1),
              .malformedSyntaxRulePattern(let error2, let pat2)):
          return error1 == error2 && pat1 == pat2
        case (.malformedSyntaxRuleLiterals(let lit1), .malformedSyntaxRuleLiterals(let lit2)):
          return lit1 == lit2
        case (.invalidContextInQuasiquote(let op1, let expr1),
              .invalidContextInQuasiquote(let op2, let expr2)):
          return op1 == op2 && expr1 == expr2
        case (.macroMismatchedRepetitionPatterns(let sym1),
              .macroMismatchedRepetitionPatterns(let sym2)):
          return sym1 == sym2
        case (.malformedBindings(let b1, let l1), .malformedBindings(let b2, let l2)):
          return b1 == b2 && l1 == l2
        case (.malformedTest(let expr1), .malformedTest(let expr2)):
          return expr1 == expr2
        case (.malformedCondClause(let clause1), .malformedCondClause(let clause2)):
          return clause1 == clause2
        case (.malformedCondExpandClause(let clause1), .malformedCondExpandClause(let clause2)):
          return clause1 == clause2
        case (.malformedCaseClause(let clause1), .malformedCaseClause(let clause2)):
          return clause1 == clause2
        case (.duplicateBinding(let sym1, let expr1), .duplicateBinding(let sym2, let expr2)):
          return sym1 == sym2 && expr1 == expr2
        case (.indexOutOfBounds(let i1, let m1), .indexOutOfBounds(let i2, let m2)):
          return i1 == i2 && m1 == m2
        case (.parameterOutOfBounds(let fun1, let n1, let actual1, let min1, let max1),
              .parameterOutOfBounds(let fun2, let n2, let actual2, let min2, let max2)):
          return fun1 == fun2 && n1 == n2 && actual1 == actual2 && min1 == min2 && max1 == max2
        case (.nonApplicativeValue(let expr1), .nonApplicativeValue(let expr2)):
          return expr1 == expr2
        case (.illegalRadix(let expr1), .illegalRadix(let expr2)):
          return expr1 == expr2
        case (.typeError(let expr1, let exp1), .typeError(let expr2, let exp2)):
          return expr1 == expr2 && exp1 == exp2
        case (.argumentError(let f1, let a1), .argumentError(let f2, let a2)):
          return f1 == f2 && a1 == a2
        case (.argumentCountError(let fml1, let a1), .argumentCountError(let fml2, let a2)):
          return fml1 == fml2 && a1 == a2
        case (.noMatchingCase(let a1, let p1), .noMatchingCase(let a2, let p2)):
          return a1 == a2 && p1 == p2
        case (.leastArgumentCountError(let fml1, let a1),
              .leastArgumentCountError(let fml2, let a2)):
          return fml1 == fml2 && a1 == a2
        case (.multiValueCountError(let e1, let f1), .multiValueCountError(let e2, let f2)):
          return e1 == e2 && f1 == f2
        case (.outOfScope(let e1), .outOfScope(let e2)):
          return e1 == e2
        case (.defineInLocalEnv(let sig1, let def1, let g1),
              .defineInLocalEnv(let sig2, let def2, let g2)):
          return sig1 == sig2 && def1 == def2 && g1 == g2
        case (.importInLocalEnv(let is1, let g1), .importInLocalEnv(let is2, let g2)):
          return is1 == is2 && g1 == g2
        case (.importInLibrary(let e1), .importInLibrary(let e2)):
          return e1 == e2
        case (.malformedImportSet(let e1), .malformedImportSet(let e2)):
          return e1 == e2
        case (.erroneousRedefinition(let s1, let l1), .erroneousRedefinition(let s2, let l2)):
          return s1 == s2 && l1 == l2
        case (.cannotExpandImportSet(let i1), .cannotExpandImportSet(let i2)):
          return i1 == i2
        case (.defineSyntaxInLocalEnv(let sym1, let def1, let g1),
              .defineSyntaxInLocalEnv(let sym2, let def2, let g2)):
          return sym1 == sym2 && def1 == def2 && g1 == g2
        case (.targetBytevectorTooSmall(let bvec1), .targetBytevectorTooSmall(let bvec2)):
          return bvec1 == bvec2
        case (.cannotOpenFile(let f1), .cannotOpenFile(let f2)):
          return f1 == f2
        case (.cannotOpenUrl(let u1), .cannotOpenUrl(let u2)):
          return u1 == u2
        case (.invalidUrl(let e1), .invalidUrl(let e2)):
          return e1 == e2
        case (.cannotWriteToPort(let p1), .cannotWriteToPort(let p2)):
          return p1 == p2
        case (.illegalContinuationApplication(let p1, let rid1),
              .illegalContinuationApplication(let p2, let rid2)):
          return p1 == p2 && rid1 == rid2
        case (.attemptToModifyImmutableData(let e1), .attemptToModifyImmutableData(let e2)):
          return e1 == e2
        case (.unknownFieldOfRecordType(let t1, let f1), .unknownFieldOfRecordType(let t2, let f2)):
          return t1 == t2 && f1 == f2
        case (.fieldCountError(let e1, let v1), .fieldCountError(let e2, let v2)):
          return e1 == e2 && v1 == v2
        case (.malformedLibraryDefinition(let d1), .malformedLibraryDefinition(let d2)):
          return d1 == d2
        case (.malformedLibraryName(let n1), .malformedLibraryName(let n2)):
          return n1 == n2
        case (.uninitializedExports(let e1, let l1), .uninitializedExports(let e2, let l2)):
          guard l1 == l2 && e1.count == e2.count else {
            return false
          }
          for i in e1.indices {
            guard e1[i] === e2[i] else {
              return false
            }
          }
          return true
        case (.unknownFile(let p1), .unknownFile(let p2)):
          return p1 == p2
        case (.unknownDirectory(let p1), .unknownDirectory(let p2)):
          return p1 == p2
        case (.cannotDecodeBytevector(let e1), .cannotDecodeBytevector(let e2)):
          return e1 == e2
        case (.cannotEncodeBytevector(let e1), .cannotEncodeBytevector(let e2)):
          return e1 == e2
        case (.invalidDateTime(let e1), .invalidDateTime(let e2)):
          return e1 == e2
        case (.invalidTimeZone(let e1), .invalidTimeZone(let e2)):
          return e1 == e2
        default:
          return false
      }
    }
    return false
  }
  
  public static func assert(_ args: Arguments, count: Int) throws {
    guard args.count == count else {
      throw EvalError.argumentCountError(formals: count, args: .makeList(args))
    }
  }
}

public func ==(lhs: EvalError, rhs: EvalError) -> Bool {
  return lhs.equals(rhs)
}


///
/// Class `OsError` wraps system-level errors (represented as `NSError`) and maps them to
/// the LispKit error protocol.
///
public final class OsError: LispError, Hashable {
  let nsError: NSError
  
  public init(_ nsError: NSError) {
    self.nsError = nsError
  }
  
  public var type: LispErrorType {
    return .osError
  }
  
  public var message: String {
    return "\(self.nsError.localizedFailureReason ?? self.nsError.localizedDescription) " +
           "(\(self.nsError.domain), \(self.nsError.code))"
  }
  
  public var irritants: Exprs {
    return noExprs
  }
  
  public func equals(_ error: LispError) -> Bool {
    guard let other = error as? OsError else {
      return false
    }
    return self.nsError == other.nsError
  }
}

public func ==(lhs: OsError, rhs: OsError) -> Bool {
  return lhs.equals(rhs)
}


///
/// An abortion error is created by a user to terminate long-running evaluations
///
public enum AbortionError: Int, LispError, Hashable {
  case value
  
  public var type: LispErrorType {
    return .abortionError
  }
  
  public var message: String {
    return "aborted evaluation"
  }
  
  public var irritants: Exprs {
    return noExprs
  }
  
  public func equals(_ error: LispError) -> Bool {
    return error.type == .abortionError
  }
}


///
/// Custom errors are created via the LispKit functions `make-error` and (indirectly) `error`.
/// The semantics of these errors are defined by the programmer.
///
public struct CustomError: LispError, Hashable {
  public let kind: String
  public let message: String
  public let irritants: Exprs
  
  public var type: LispErrorType {
    return .customError(kind: kind)
  }
  
  public func equals(_ error: LispError) -> Bool {
    if let other = error as? CustomError {
      return self.kind == other.kind &&
             self.message == other.message &&
             self.irritants == other.irritants
    }
    return false
  }
}

public func ==(lhs: CustomError, rhs: CustomError) -> Bool {
  return lhs.equals(rhs)
}

