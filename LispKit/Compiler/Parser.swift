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
public final class Parser {
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
  
  public func parse(_ prescan: Bool = true) throws -> Expr {
    var res: Expr
    let token = self.scanner.token
    switch token.kind {
      case .error:
        let lexicalError = token.errorVal!
        self.scanner.next()
        throw lexicalError
      case .eof:
        throw SyntaxError.empty
      case .ident:
        res = .symbol(self.symbols.intern(token.strVal))
      case .truelit:
        res = .true
      case .falselit:
        res = .false
      case .int:
        res = Expr.fixnum(token.intVal)
      case .bigint:
        res = Expr.bignum(token.bigIntVal).normalized
      case .rat:
        res = Expr.rational(.fixnum(token.ratVal.numerator),
                            .fixnum(token.ratVal.denominator)).normalized
      case .bigrat:
        res = Expr.rational(.bignum(token.bigRatVal.numerator),
                            .bignum(token.bigRatVal.denominator)).normalized
      case .float:
        res = Expr.flonum(token.floatVal)
      case .complex:
        res = Expr.complex(ImmutableBox(token.complexVal)).normalized
      case .char:
        res = .char(UInt16(token.intVal))
      case .string:
        res = .string(NSMutableString(string: token.strVal))
      case .lparen:
        self.scanner.next()
        var exprs = Exprs()
        while !self.scanner.hasToken(.eof, .rparen, .dot) {
          exprs.append(try self.parse())
        }
        if self.scanner.hasToken(.dot) {
          self.scanner.next()
          res = Expr.makeList(exprs, append: try self.parse())
        } else {
          res = Expr.makeList(exprs)
        }
        if !self.scanner.hasToken(.rparen) {
          throw SyntaxError.closingParenthesisMissing
        }
      case .rparen:
        self.scanner.next()
        throw SyntaxError.unexpectedClosingParenthesis
      case .hashlparen:
        self.scanner.next()
        var exprs = Exprs()
        while !self.scanner.hasToken(.eof, .rparen) {
          exprs.append(try self.parse())
        }
        guard self.scanner.hasToken(.rparen) else {
          throw SyntaxError.closingParenthesisMissing
        }
        res = .vector(Collection(kind: .immutableVector, exprs: exprs))
      case .u8LPAREN:
        self.scanner.next()
        var bytes = [UInt8]()
        while self.scanner.hasToken(.int) {
          let number = self.scanner.token.intVal
          guard number >= 0 && number <= 255 else {
            throw SyntaxError.notAByteValue
          }
          bytes.append(UInt8(number))
          self.scanner.next()
        }
        guard self.scanner.hasToken(.rparen) else {
          throw SyntaxError.closingParenthesisMissing
        }
        res = .bytes(MutableBox(bytes))
      case .quote:
        self.scanner.next()
        return Expr.makeList(.symbol(symbols.quote), try self.parse())
      case .backquote:
        self.scanner.next()
        return Expr.makeList(.symbol(symbols.quasiquote), try self.parse())
      case .comma:
        self.scanner.next()
        return Expr.makeList(.symbol(symbols.unquote), try self.parse())
      case .commaat:
        self.scanner.next()
        return Expr.makeList(.symbol(symbols.unquoteSplicing), try self.parse())
      case .dot:
        self.scanner.next()
        throw SyntaxError.unexpectedDot
    }
    if prescan {
      self.scanner.next()
    }
    return res
  }
}

