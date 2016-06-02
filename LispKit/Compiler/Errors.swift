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


public class AnyError: LispError, CustomStringConvertible {
  var error: LispError
  
  public init(_ error: LispError) {
    self.error = error
  }
  
  public var type: LispErrorType {
    return error.type
  }
  
  public var kind: String {
    return error.kind
  }
  
  public var message: String {
    return error.message
  }
  
  public var irritants: Exprs {
    return error.irritants
  }
  
  public func equals(other: LispError) -> Bool {
    return error.equals(other)
  }
}

public protocol LispError: ErrorType, CustomStringConvertible {
  var type: LispErrorType { get }
  var kind: String { get }
  var message: String { get }
  var irritants: Exprs { get }
  func equals(other: LispError) -> Bool
}

public extension LispError {
  
  public var hashValue: Int {
    var res = 0
    for irritant in self.irritants {
      res = res &* 31 &+ irritant.hashValue
    }
    res = res &* 31 &+ self.message.hashValue
    res = res &* 31 &+ self.kind.hashValue
    return res &* 31 &+ self.type.hashValue
  }
  
  public var description: String {
    var res = "#<\(self.kind)| \(self.message)"
    if self.irritants.count > 0 {
      res += ": "
      var sep = ""
      for expr in self.irritants {
        res += sep
        res += expr.description
        sep = ", "
      }
    }
    return res + ">"
  }
}

public func ==(lhs: LispError, rhs: LispError) -> Bool {
  return lhs.equals(rhs)
}

public enum LispErrorType: Int, Hashable, CustomStringConvertible {
  case LexicalError
  case SyntaxError
  case EvalError
  case OsError
  case CustomError
  
  public var description: String {
    switch self {
      case LexicalError:
        return "lexical error"
      case SyntaxError:
        return "syntax error"
      case EvalError:
        return "eval error"
      case OsError:
        return "os error"
      case CustomError:
        return "custom error"
    }
  }
}


/// Enumeration `LexicalError` represents lexical parsing errors emitted by the
/// scanner.
public enum LexicalError: Int, LispError {
  case Empty
  case MalformedIdentifier
  case BrokenIdentifierEncoding
  case BrokenNumberEncoding
  case NumberExpected
  case MalformedFloatLiteral
  case MalformedComplexLiteral
  case MalformedStringLiteral
  case MalformedCharacterLiteral
  case UnknownCharacterLiteral
  case IncompleteCharacterLiteral
  case IllegalCharacter
  case IllegalHexCharacter
  case IllegalEscapeSequence
  case IllegalEndOfLine
  case TokenNotYetSupported
  case DivisionByZero
  case ExactComplexNumbersUnsupported
  
  public var kind: String {
    return "lexical error"
  }
  
  public var message: String {
    switch self {
      case Empty:
        return "no input available"
      case MalformedIdentifier:
        return "malformed identifier"
      case BrokenIdentifierEncoding:
        return "broken identifier encoding"
      case BrokenNumberEncoding:
        return "broken number encoding"
      case .NumberExpected:
        return "expected a number"
      case MalformedFloatLiteral:
        return "malformed floating point number literal"
      case MalformedComplexLiteral:
        return "malformed complex number literal"
      case MalformedStringLiteral:
        return "malformed string literal"
      case MalformedCharacterLiteral:
        return "malformed character literal"
      case UnknownCharacterLiteral:
        return "unknown character literal"
      case IncompleteCharacterLiteral:
        return "incomplete character literal"
      case IllegalCharacter:
        return "illegal character"
      case IllegalHexCharacter:
        return "illegal hex character"
      case IllegalEscapeSequence:
        return "illegal escape sequence"
      case IllegalEndOfLine:
        return "illegal end of line"
      case TokenNotYetSupported:
        return "token not yet supported"
      case DivisionByZero:
        return "division by zero"
      case .ExactComplexNumbersUnsupported:
        return "exact complex numnbers are not supported"
    }
  }
  
  public var irritants: Exprs {
    return NO_EXPRS
  }
  
  public var type: LispErrorType {
    return .LexicalError
  }
  
  public func equals(error: LispError) -> Bool {
    if let other = error as? LexicalError {
      return self.rawValue == other.rawValue
    }
    return false
  }
}

public enum SyntaxError: Int, LispError {
  case Empty
  case ClosingParenthesisMissing
  case UnexpectedClosingParenthesis
  case UnexpectedDot
  case SyntaxNotYetSupported
  
  public var kind: String {
    return "syntax error"
  }
  
  public var message: String {
    switch self {
      case Empty:
        return "empty input"
      case ClosingParenthesisMissing:
        return "closing parenthesis missing"
      case UnexpectedClosingParenthesis:
        return "unexpected closing parenthesis"
      case UnexpectedDot:
        return "unexpected dot"
      case SyntaxNotYetSupported:
        return "syntax not yet supported"
    }
  }
  
  public var irritants: Exprs {
    return NO_EXPRS
  }
  
  public var type: LispErrorType {
    return .SyntaxError
  }
  
  public func equals(error: LispError) -> Bool {
    if let other = error as? SyntaxError {
      return self.rawValue == other.rawValue
    }
    return false
  }
}

public enum EvalError: LispError {
  case IllegalKeywordUsage(Expr)
  case IllegalFormalParameter(Expr)
  case IllegalFormalRestParameter(Expr)
  case DivisionByZero
  case UnboundVariable(Symbol)
  case VariableNotYetInitialized(Symbol?)
  case MalformedArgumentList(Expr)
  case MalformedDefinition(Expr)
  case MalformedTransformer(Expr)
  case MalformedSyntaxRule(Expr)
  case MalformedSyntaxRulePattern(error: Expr?, pattern: Expr)
  case MalformedSyntaxRuleLiterals(Expr)
  case InvalidContextInQuasiquote(Symbol, Expr)
  case MacroMismatchedRepetitionPatterns(Symbol)
  case MalformedBindings(Expr?, Expr)
  case MalformedTest(Expr)
  case MalformedCondClause(Expr)
  case MalformedCaseClause(Expr)
  case DuplicateBinding(Symbol, Expr)
  case IndexOutOfBounds(Int64, Int64, Expr)
  case ParameterOutOfBounds(String, Int, Int64, Int64, Int64)
  case NonApplicativeValue(Expr)
  case IllegalRadix(Expr)
  case TypeError(Expr, Set<Type>)
  case ArgumentError(f: Expr, args: Expr)
  case ArgumentCountError(formals: Int, args: Expr)
  case LeastArgumentCountError(formals: Int, args: Expr)
  case OutOfScope(Expr)
  case DefineInLocalEnv(signature: Expr, definition: Expr, group: BindingGroup)
  case DefineSyntaxInLocalEnv(keyword: Symbol, definition: Expr, group: BindingGroup)
  
  public var kind: String {
    return "eval error"
  }
  
  public var message: String {
    switch self {
      case IllegalKeywordUsage(let expr):
        return "syntactic keywords may not be used as expressions: \(expr)"
      case IllegalFormalParameter(let expr):
        return "illegal formal parameter: \(expr)"
      case IllegalFormalRestParameter(let expr):
        return "illegal formal rest parameter: \(expr)"
      case DivisionByZero:
        return "division by zero"
      case UnboundVariable(let sym):
        return "unbound variable: \(sym)"
      case VariableNotYetInitialized(let sym):
        return "variable \(sym?.description ?? "") not yet initialized"
      case MalformedArgumentList(let args):
        return "malformed argument list: \(args)"
      case MalformedDefinition(let def):
        return "malformed definition: \(def)"
      case MalformedTransformer(let transformer):
        return "malformed transformer: \(transformer)"
      case MalformedSyntaxRule(let rule):
        return "not a valid syntax rule: \(rule)"
      case MalformedSyntaxRulePattern(let error, let pattern):
        if let errPat = error {
          return "illegal pattern \(errPat) in syntax rule pattern: \(pattern)"
        } else {
          return "malformed syntax rule pattern: \(pattern)"
        }
      case MalformedSyntaxRuleLiterals(let literals):
        return "malformed list of syntax rule literals: \(literals)"
      case InvalidContextInQuasiquote(let op, let expr):
        return "\(op) in invalid context within quasiquote: \(expr)"
      case MacroMismatchedRepetitionPatterns(let sym):
        return "macro could not be expanded: mismatched repetition patterns: \(sym)"
      case MalformedBindings(nil, let bindingList):
        return "malformed list of bindings: \(bindingList)"
      case MalformedBindings(.Some(let binding), _):
        return "malformed binding: \(binding)"
      case MalformedTest(let expr):
        return "malformed test expression: \(expr)"
      case MalformedCondClause(let clause):
        return "malformed clause in cond form: \(clause)"
      case MalformedCaseClause(let clause):
        return "malformed clause in case form: \(clause)"
      case DuplicateBinding(let sym, let expr):
        return "symbol \(sym) bound multiple times in \(expr)"
      case IndexOutOfBounds(let index, let max, let expr):
        if index < 0 && max < 0 {
          return "index \(index) must not be negative when accessing \(expr)"
        } else {
          return "index \(index) out of bounds [0..\(max)] when accessing \(expr)"
        }
      case ParameterOutOfBounds(let fun, let n, let actual, let min, let max):
        return "parameter \(n) of \(fun) out of bounds [\(min)..\(max)]: \(actual)"
      case NonApplicativeValue(let expr):
        return "cannot apply arguments to: \(expr)"
      case IllegalRadix(let expr):
        return "illegal radix \(expr)"
      case TypeError(let expr, let expected):
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
      case ArgumentError(let f, let args):
        return "wrong number of arguments for \(f): \(args)"
      case ArgumentCountError(let formals, let args):
        let (exprs, _) = args.toExprs()
        return "expected \(formals), but received \(exprs.count) arguments: \(args)"
      case LeastArgumentCountError(let formals, let args):
        let (exprs, _) = args.toExprs()
        return "expected at least \(formals), but received only \(exprs.count) arguments: \(args)"
      case OutOfScope(let syntax):
        return "out of scope evaluation of \(syntax)"
      case DefineInLocalEnv(let sig, _, _):
        return "definition of \(sig) in local environment"
      case DefineSyntaxInLocalEnv(let sym, _, _):
        return "syntax definition of \(sym) in local environment"
    }
  }
  
  public var irritants: Exprs {
    return NO_EXPRS
  }
  
  public var type: LispErrorType {
    return .EvalError
  }
  
  public func equals(error: LispError) -> Bool {
    if let other = error as? EvalError {
      switch (self, other) {
        case (IllegalKeywordUsage(let expr1), IllegalKeywordUsage(let expr2)):
          return expr1 == expr2
        case (IllegalFormalParameter(let expr1), IllegalFormalParameter(let expr2)):
          return expr1 == expr2
        case (IllegalFormalRestParameter(let expr1), IllegalFormalRestParameter(let expr2)):
          return expr1 == expr2
        case (DivisionByZero, DivisionByZero):
          return true
        case (UnboundVariable(let sym1), UnboundVariable(let sym2)):
          return sym1 == sym2
        case (VariableNotYetInitialized(let sym1), VariableNotYetInitialized(let sym2)):
          return sym1 == sym2
        case (MalformedArgumentList(let a1), MalformedArgumentList(let a2)):
          return a1 == a2
        case (MalformedDefinition(let def1), MalformedDefinition(let def2)):
          return def1 == def2
        case (MalformedTransformer(let trans1), MalformedTransformer(let trans2)):
          return trans1 == trans2
        case (MalformedSyntaxRule(let rule1), MalformedSyntaxRule(let rule2)):
          return rule1 == rule2
        case (MalformedSyntaxRulePattern(let error1, let pat1),
              MalformedSyntaxRulePattern(let error2, let pat2)):
          return error1 == error2 && pat1 == pat2
        case (MalformedSyntaxRuleLiterals(let lit1), MalformedSyntaxRuleLiterals(let lit2)):
          return lit1 == lit2
        case (InvalidContextInQuasiquote(let op1, let expr1),
              InvalidContextInQuasiquote(let op2, let expr2)):
          return op1 == op2 && expr1 == expr2
        case (MacroMismatchedRepetitionPatterns(let sym1),
              MacroMismatchedRepetitionPatterns(let sym2)):
          return sym1 == sym2
        case (MalformedBindings(let b1, let l1), MalformedBindings(let b2, let l2)):
          return b1 == b2 && l1 == l2
        case (MalformedTest(let expr1), MalformedTest(let expr2)):
          return expr1 == expr2
        case (MalformedCondClause(let clause1), MalformedCondClause(let clause2)):
          return clause1 == clause2
        case (MalformedCaseClause(let clause1), MalformedCaseClause(let clause2)):
          return clause1 == clause2
        case (DuplicateBinding(let sym1, let expr1), DuplicateBinding(let sym2, let expr2)):
          return sym1 == sym2 && expr1 == expr2
        case (IndexOutOfBounds(let i1, let m1, let expr1),
              IndexOutOfBounds(let i2, let m2, let expr2)):
          return i1 == i2 && m1 == m2 && expr1 == expr2
        case (ParameterOutOfBounds(let fun1, let n1, let actual1, let min1, let max1),
              ParameterOutOfBounds(let fun2, let n2, let actual2, let min2, let max2)):
          return fun1 == fun2 && n1 == n2 && actual1 == actual2 && min1 == min2 && max1 == max2
        case (NonApplicativeValue(let expr1), NonApplicativeValue(let expr2)):
          return expr1 == expr2
        case (IllegalRadix(let expr1), IllegalRadix(let expr2)):
          return expr1 == expr2
        case (TypeError(let expr1, let exp1), TypeError(let expr2, let exp2)):
          return expr1 == expr2 && exp1 == exp2
        case (ArgumentError(let f1, let a1), ArgumentError(let f2, let a2)):
          return f1 == f2 && a1 == a2
        case (ArgumentCountError(let fml1, let a1), ArgumentCountError(let fml2, let a2)):
          return fml1 == fml2 && a1 == a2
        case (LeastArgumentCountError(let fml1, let a1),
              LeastArgumentCountError(let fml2, let a2)):
          return fml1 == fml2 && a1 == a2
        case (OutOfScope(let e1), OutOfScope(let e2)):
          return e1 == e2
        case (DefineInLocalEnv(let sig1, let def1, let g1),
              DefineInLocalEnv(let sig2, let def2, let g2)):
          return sig1 == sig2 && def1 == def2 && g1 == g2
        case (DefineSyntaxInLocalEnv(let sym1, let def1, let g1),
              DefineSyntaxInLocalEnv(let sym2, let def2, let g2)):
          return sym1 == sym2 && def1 == def2 && g1 == g2
        default:
          return false
      }
    }
    return false
  }
}

public class OsError: LispError {
  let nsError: NSError
  
  public init(_ nsError: NSError) {
    self.nsError = nsError
  }
  
  public var kind: String {
    return type.description
  }
  
  public var message: String {
    return "\(self.nsError.localizedFailureReason ?? self.nsError.localizedDescription) " +
           "(\(self.nsError.domain), \(self.nsError.code))"
  }
  
  public var irritants: Exprs {
    return NO_EXPRS
  }
  
  public var type: LispErrorType {
    return .OsError
  }
  
  public func equals(error: LispError) -> Bool {
    guard let other = error as? OsError else {
      return false
    }
    return self.nsError == other.nsError
  }
}

public struct CustomError: LispError {
  public let kind: String
  public let message: String
  public let irritants: Exprs
  
  public var type: LispErrorType {
    return .CustomError
  }
  
  public func equals(error: LispError) -> Bool {
    if let other = error as? CustomError {
      return self.kind == other.kind &&
             self.message == other.message &&
             self.irritants == other.irritants
    }
    return false
  }
}
