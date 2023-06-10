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

import Foundation

///
/// Implementation of a simple Scheme parser. The parser requires access to a scanner and
/// a symbol table for creating `Symbol` objects.
///
public final class Parser {
  
  /// Symbol table used for instantiating symbols
  private let symbols: SymbolTable
  
  /// Scanner used for lexical parsing
  private let scanner: Scanner
  
  /// Id of source code
  internal let sourceId: UInt16
  
  public convenience init(symbols: SymbolTable,
                          input: TextInput,
                          sourceId: UInt16 = SourceManager.unknownSourceId,
                          foldCase: Bool = false) {
    self.init(symbols: symbols,
              scanner: Scanner(input: input, foldCase: foldCase),
              sourceId: sourceId)
  }
  
  public init(symbols: SymbolTable, scanner: Scanner, sourceId: UInt16) {
    self.symbols = symbols
    self.scanner = scanner
    self.sourceId = sourceId
  }
  
  /// Returns true if the scanner has reached the end of the input.
  public var finished: Bool {
    return !self.scanner.hasNext()
  }
  
  /// Parses the next expression.
  public func parse(_ prescan: Bool = true) throws -> Expr {
    var res: Expr
    let token = self.scanner.token
    let pos = self.sourcePosition
    switch token.kind {
      case .undef:
        res = .undef
      case .error:
        let lexicalError = token.errorVal!
        self.scanner.next()
        throw RuntimeError.lexical(lexicalError, at: pos)
      case .eof:
        throw RuntimeError.syntax(.empty, at: pos)
      case .hashsemi:
        self.scanner.next()
        _ = try self.parse()
        return try self.parse(prescan)
      case .ident:
        res = .symbol(
          self.symbols.intern(self.scanner.foldCase ? token.strVal.lowercased() : token.strVal))
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
          throw RuntimeError.syntax(.closingParenthesisMissing, at: self.sourcePosition)
        }
      case .rparen:
        self.scanner.next()
        throw RuntimeError.syntax(.unexpectedClosingParenthesis, at: pos)
      case .hashlparen:
        self.scanner.next()
        var exprs = Exprs()
        while !self.scanner.hasToken(.eof, .rparen) {
          exprs.append(try self.parse().datum)
        }
        guard self.scanner.hasToken(.rparen) else {
          throw RuntimeError.syntax(.closingParenthesisMissing, at: self.sourcePosition)
        }
        res = .vector(Collection(kind: .immutableVector, exprs: exprs))
      case .hashglparen:
        self.scanner.next()
        var exprs = Exprs()
        while !self.scanner.hasToken(.eof, .rparen) {
          exprs.append(try self.parse().datum)
        }
        guard self.scanner.hasToken(.rparen) else {
          throw RuntimeError.syntax(.closingParenthesisMissing, at: self.sourcePosition)
        }
        res = .vector(Collection(kind: .growableVector, exprs: exprs))
      case .u8lparen:
        self.scanner.next()
        var bytes = [UInt8]()
        while self.scanner.hasToken(.int) {
          let number = self.scanner.token.intVal
          guard number >= 0 && number <= 255 else {
            throw RuntimeError.syntax(.notAByteValue, at: self.sourcePosition)
          }
          bytes.append(UInt8(number))
          self.scanner.next()
        }
        guard self.scanner.hasToken(.rparen) else {
          throw RuntimeError.syntax(.closingParenthesisMissing, at: self.sourcePosition)
        }
        res = .bytes(MutableBox(bytes))
      case .quote:
        self.scanner.next()
        return Expr.makeList(.syntax(pos, .symbol(symbols.quote)), try self.parse())
      case .backquote:
        self.scanner.next()
        return Expr.makeList(.syntax(pos, .symbol(symbols.quasiquote)), try self.parse())
      case .comma:
        self.scanner.next()
        return Expr.makeList(.syntax(pos, .symbol(symbols.unquote)), try self.parse())
      case .commaat:
        self.scanner.next()
        return Expr.makeList(.syntax(pos, .symbol(symbols.unquoteSplicing)), try self.parse())
      case .dot:
        self.scanner.next()
        throw RuntimeError.syntax(.unexpectedDot, at: pos)
    }
    if prescan {
      self.scanner.next()
    }
    return .syntax(pos, res)
  }
  
  /// Returns the source position of the current token.
  private var sourcePosition: SourcePosition {
    return SourcePosition(self.sourceId, self.scanner.token.pos.line, self.scanner.token.pos.col)
  }
}
