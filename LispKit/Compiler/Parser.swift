//
//  Parser.swift
//  LispKit
//
//  Created by Matthias Zenger on 14/11/2015.
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

/// 
/// Implementation of a simple Lisp parser. The parser requires access to a scanner and
/// a symbol table for creating `Symbol` objects.
/// 
public class Parser {
  private var symbols: SymbolTable
  private var scanner: Scanner
  
  public convenience init(symbols: SymbolTable, src: String) {
    self.init(symbols: symbols, scanner: Scanner(string: src))
  }
  
  public convenience init(symbols: SymbolTable, input: TextInput) {
    self.init(symbols: symbols, scanner: Scanner(input: input))
  }
  
  public init(symbols: SymbolTable, scanner: Scanner) {
    self.symbols = symbols
    self.scanner = scanner
  }
  
  public var finished: Bool {
    return !self.scanner.hasNext()
  }
  
  public func parse(prescan: Bool = true) throws -> Expr {
    var res: Expr
    let token = self.scanner.token
    switch token.kind {
      case .ERROR:
        let lexicalError = token.errorVal!
        self.scanner.next()
        throw lexicalError
      case .EOF_TOKEN:
        throw SyntaxError.Empty
      case .IDENT:
        res = .Sym(self.symbols.intern(token.strVal))
      case .TRUELIT:
        res = .True
      case .FALSELIT:
        res = .False
      case .INT:
        res = Expr.Fixnum(token.intVal)
      case .BIGINT:
        res = Expr.Bignum(token.bigIntVal).normalized
      case .RAT:
        res = Expr.Rat(token.ratVal).normalized
      case .BIGRAT:
        res = Expr.Bigrat(token.bigRatVal).normalized
      case .FLOAT:
        res = Expr.Flonum(token.floatVal)
      case .COMPLEX:
        res = Expr.Complexnum(token.complexVal).normalized
      case .CHAR:
        res = .Char(UInt16(token.intVal))
      case .STRING:
        res = .Str(NSMutableString(string: token.strVal))
      case .LPAREN:
        self.scanner.next()
        var exprs = Exprs()
        while !self.scanner.hasToken(.EOF_TOKEN, .RPAREN, .DOT) {
          exprs.append(try self.parse())
        }
        if self.scanner.hasToken(.DOT) {
          self.scanner.next()
          res = Expr.List(exprs, append: try self.parse())
        } else {
          res = Expr.List(exprs)
        }
        if !self.scanner.hasToken(.RPAREN) {
          throw SyntaxError.ClosingParenthesisMissing
        }
      case .RPAREN:
        self.scanner.next()
        throw SyntaxError.UnexpectedClosingParenthesis
      case .HASHLPAREN:
        self.scanner.next()
        var exprs = Exprs()
        while !self.scanner.hasToken(.EOF_TOKEN, .RPAREN) {
          exprs.append(try self.parse())
        }
        guard self.scanner.hasToken(.RPAREN) else {
          throw SyntaxError.ClosingParenthesisMissing
        }
        res = .Vec(Vector(exprs))
      case .U8LPAREN:
        self.scanner.next()
        var bytes = [UInt8]()
        while self.scanner.hasToken(.INT) {
          let number = self.scanner.token.intVal
          guard number >= 0 && number <= 255 else {
            throw SyntaxError.NotAByteValue
          }
          bytes.append(UInt8(number))
          self.scanner.next()
        }
        guard self.scanner.hasToken(.RPAREN) else {
          throw SyntaxError.ClosingParenthesisMissing
        }
        res = .ByteVec(MutableBox(bytes))
      case .QUOTE:
        self.scanner.next()
        return Expr.List(.Sym(symbols.QUOTE), try self.parse())
      case .BACKQUOTE:
        self.scanner.next()
        return Expr.List(.Sym(symbols.QUASIQUOTE), try self.parse())
      case .COMMA:
        self.scanner.next()
        return Expr.List(.Sym(symbols.UNQUOTE), try self.parse())
      case .COMMAAT:
        self.scanner.next()
        return Expr.List(.Sym(symbols.UNQUOTESPLICING), try self.parse())
      case .DOT:
        self.scanner.next()
        throw SyntaxError.UnexpectedDot
    }
    if prescan {
      self.scanner.next()
    }
    return res
  }
}

