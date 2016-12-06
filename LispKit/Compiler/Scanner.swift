//
//  Scanner.swift
//  LispKit
//
//  Created by Matthias Zenger on 09/11/2015.
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

import NumberKit

/// 
/// Class `Scanner` implements a lexical analyzer for Scheme. The class uses the utf16 view
/// of the string for parsing purposes.
/// 
public final class Scanner {
  
  /// Input of source code
  private let input: TextInput
  
  /// Buffer for characters read during one invocation of `next`
  private var buffer: ScanBuffer
  
  /// Last scanned character
  internal var ch: UniChar
  
  /// Position of last scanned character
  private var lpos: Position
  
  /// Next position
  internal var pos: Position
  
  /// Last scanned token
  internal var token: Token
  
  /// Creates a new scanner for the given string.
  public convenience init(string: String, prescan: Bool = true) {
    self.init(input: TextInput(string: string), prescan: prescan)
  }
  
  /// Creates a new scanner for the given string.
  public init(input: TextInput, prescan: Bool = true) {
    self.input = input
    self.buffer = ScanBuffer()
    self.pos = Position(1, 1)
    self.lpos = Position(0, 0)
    self.ch = SPACE_CH
    self.token = Token(
      pos: Position(0, 0),
      kind: .error,
      strVal: "",
      intVal: 0,
      bigIntVal: 0,
      ratVal: 0,
      bigRatVal: 0,
      floatVal: 0.0,
      complexVal: 0.0,
      errorVal: LexicalError.empty)
    if prescan {
      self.next()
    }
  }
  
  /// Returns true if the current token has one of the given token kinds.
  public func hasToken(_ kind: TokenKind...) -> Bool {
    for k in kind {
      if self.token.kind == k {
        return true
      }
    }
    return false
  }
  
  /// Returns true if there is another token available. The token can be accessed via the
  /// `token` property.
  public func hasNext() -> Bool {
    return self.token.kind != .eof
  }
  
  /// Parses the next token.
  public func next() {
    while self.ch != EOF_CH {
      // skip whitespace
      self.skipSpace()
      self.token.reset(self.lpos)
      // handle end of file
      if self.ch == EOF_CH {
        break
      }
      // reset buffer
      self.buffer.reset()
      // handle identifiers
      if isInitialIdent(self.ch) {
        self.scanIdent()
        return
      }
      // handle +/- prefixes
      if self.ch == PLUS_CH || self.ch == MINUS_CH {
        let neg = self.ch == MINUS_CH
        self.nextCh()
        // signed numbers
        if isDigit(self.ch) {
          self.scanNumber(10, neg: neg, dot: false)
        // signed numbers of the form +/-.X
        } else if self.ch == DOT_CH {
          self.nextCh()
          if isDotSubsequent(self.ch) {
            self.scanIdent()
          } else if isDigit(self.ch) {
            self.scanNumber(10, neg: neg, dot: true)
          } else {
            self.token.kind = .ident
            self.token.strVal = self.buffer.stringValue
          }
        // identifiers starting with +/-, infinity or NaN
        } else if isSubsequentIdent(self.ch) || self.ch == PLUS_CH || self.ch == MINUS_CH {
          self.nextCh()
          while isSubsequentIdent(self.ch) && self.ch != PLUS_CH && self.ch != MINUS_CH {
            self.nextCh()
          }
          var realPart: Double?
          switch self.buffer.stringValue.lowercased() {
            case "-inf.0":
              realPart = -Double.infinity
            case "+inf.0":
              realPart = Double.infinity
            case "-nan.0", "+nan.0":
              realPart = Double.nan
            default:
              realPart = nil
              while isSubsequentIdent(self.ch) {
                self.nextCh()
              }
              self.token.kind = .ident
              self.token.strVal = self.buffer.stringValue.lowercased()
          }
          // check if infinity/NaN is part of a complex number
          if let realPart = realPart {
            switch self.ch {
              case PLUS_CH:
                self.nextCh()
                return scanImaginaryPart(realPart, neg: false)
              case MINUS_CH:
                self.nextCh()
                return scanImaginaryPart(realPart, neg: true)
              default:
                self.token.kind = .float
                self.token.floatVal = realPart
            }
          }
        } else {
          self.token.kind = .ident
          self.token.strVal = self.buffer.stringValue
        }
        return
      }
      // handle unsigned numbers
      if isDigit(self.ch) {
        self.scanNumber(10, neg: false, dot: false)
        return
      }
      // handle others
      switch self.ch {
        case BAR_CH:
          self.scanDelimitedIdent()
          return
        case DOT_CH:
          self.nextCh()
          if isDotSubsequent(self.ch) {
            self.scanIdent()
          } else if isDigit(self.ch) {
            self.scanNumber(10, neg: false, dot: true)
          } else {
            self.token.kind = .dot
          }
          return
        case LPAREN_CH:
          self.token.kind = .lparen
          self.nextCh()
          return
        case RPAREN_CH:
          self.token.kind = .rparen
          self.nextCh()
          return
        case Q_CH, OPENQ_CH, CLOSEQ_CH:
          self.token.kind = .quote
          self.nextCh()
          return
        case BQ_CH:
          self.token.kind = .backquote
          self.nextCh()
          return
        case DQ_CH:
          self.scanString()
          return
        case COMMA_CH:
          self.nextCh()
          if self.ch == AT_CH {
            self.nextCh()
            self.token.kind = .commaat
          } else {
            self.token.kind = .comma
          }
          return
        case HASH_CH:
          self.nextCh()
          switch self.ch {
            case LE_CH:
              self.nextCh()
              self.scanGeneralNumber(exact: true)
            case LI_CH:
              self.nextCh()
              self.scanGeneralNumber(exact: false)
            case B_CH:
              self.nextCh()
              self.scanSignedNumber(2)
            case O_CH:
              self.nextCh()
              self.scanSignedNumber(8)
            case D_CH:
              self.nextCh()
              self.scanSignedNumber(10)
            case X_CH:
              self.nextCh()
              self.scanSignedNumber(16)
            case U_CH:
              self.nextCh()
              guard self.ch == EIGHT_CH else {
                self.signal(.incompleteCharacterLiteral)
                return
              }
              self.nextCh()
              guard self.ch == LPAREN_CH else {
                self.signal(.incompleteCharacterLiteral)
                return
              }
              self.nextCh()
              self.token.kind = .u8LPAREN
            case BAR_CH:
              self.nextCh()
              var bar = false
              while (!bar || self.ch != HASH_CH) && self.ch != EOF_CH {
                bar = self.ch == BAR_CH
                self.nextCh()
              }
              if self.ch != EOF_CH {
                self.nextCh()
              }
              continue
            case LPAREN_CH:
              self.nextCh()
              self.token.kind = .hashlparen
            case BS_CH:
              self.nextCh()
              self.scanCharacterLiteral()
            case EOF_CH:
              self.signal(.incompleteCharacterLiteral)
              return
            default:
              while self.ch >= LA_CH && self.ch <= LZ_CH || self.ch >= UA_CH && self.ch <= UZ_CH {
                self.nextCh()
              }
              let s = self.buffer.stringStartingAt(1)
              switch s.lowercased() {
                case "t":
                  self.token.kind = .truelit
                case "true":
                  self.token.kind = .truelit
                case "f":
                  self.token.kind = .falselit
                case "false":
                  self.token.kind = .falselit
                default:
                  self.signal(.unknownCharacterLiteral)
                  return
              }
          }
          return
        default:
          self.nextCh()
          self.signal(.illegalCharacter)
      }
    }
    self.token.kind = .eof
  }
  
  /// Signals a lexical error
  private func signal(_ error: LexicalError) {
    self.token.kind = .error
    self.token.errorVal = error
  }
  
  /// Reads the next character and makes it available via the `ch` property.
  private func nextCh() {
    // Check if we reached EOF already
    guard self.ch != EOF_CH else {
      return
    }
    // Store last position
    self.lpos = self.pos
    // Read next character and terminate if there is none available
    guard let c = self.input.read() else {
      self.ch = EOF_CH
      self.buffer.append(self.ch)
      return
    }
    // Handle potential line breaks
    switch c {
      case RET_CH:
        self.ch = EOL_CH
        self.pos.col = 1
        self.pos.line += 1
        if let next = self.input.peek(), next == EOL_CH {
          _ = self.input.read()
        }
      case EOL_CH:
        self.ch = EOL_CH
        self.pos.col = 1
        self.pos.line += 1
      default:
        self.ch = c
        self.pos.col += 1
    }
    // Write new character into buffer
    self.buffer.append(self.ch)
  }
  
  /// Finds the next non-whitespace character.
  internal func skipSpace() {
    self.skipComment()
    while isSpace(self.ch) {
      self.nextCh()
      while isSpace(self.ch) {
        self.nextCh()
      }
      self.skipComment()
    }
  }
  
  /// Skips consecutive comment lines
  private func skipComment() {
    while self.ch == SEMI_CH {
      self.nextCh()
      while self.ch != EOL_CH && self.ch != EOF_CH {
        self.nextCh()
      }
    }
  }
  
  /// Scans the next characters as an identifier.
  private func scanIdent() {
    self.nextCh()
    while isSubsequentIdent(self.ch) {
      self.nextCh()
    }
    self.token.kind = .ident
    self.token.strVal = self.buffer.stringValue.lowercased()
  }
  
  /// Scans a character literal
  private func scanCharacterLiteral() {
    switch self.ch {
      case EOF_CH:
        self.signal(.malformedCharacterLiteral)
      case X_CH:
        self.nextCh()
        if let ch = self.scanHexNumber(2) {
          self.token.kind = .char
          self.token.intVal = ch
        } else if self.ch >= LA_CH && self.ch <= LZ_CH || self.ch >= UA_CH && self.ch <= UZ_CH {
          self.signal(.unknownCharacterLiteral)
        } else {
          self.token.kind = .char
          self.token.intVal = Int64(X_CH)
        }
      case U_CH:
        self.nextCh()
        if let ch = self.scanHexNumber(4) {
          self.token.kind = .char
          self.token.intVal = ch
        } else if self.ch >= LA_CH && self.ch <= LZ_CH || self.ch >= UA_CH && self.ch <= UZ_CH {
          self.signal(.unknownCharacterLiteral)
        } else {
          self.token.kind = .char
          self.token.intVal = Int64(U_CH)
        }
      case LA_CH...LZ_CH, UA_CH...UZ_CH:
        self.nextCh()
        while self.ch >= LA_CH && self.ch <= LZ_CH || self.ch >= UA_CH && self.ch <= UZ_CH {
          self.nextCh()
        }
        let s = self.buffer.stringStartingAt(2)
        self.token.kind = .char
        if (s.utf16.count == 1) {
          self.token.intVal = Int64(s.utf16.first!)
        } else {
          switch s {
            case "alarm":
              self.token.intVal = 7
            case "backspace":
              self.token.intVal = 8
            case "delete":
              self.token.intVal = 127
            case "escape":
              self.token.intVal = 27
            case "newline":
              self.token.intVal = 10
            case "null":
              self.token.intVal = 0
            case "page":
              self.token.intVal = 12
            case "return":
              self.token.intVal = 13
            case "space":
              self.token.intVal = 32
            case "tab":
              self.token.intVal = 9
            case "vtab":
              self.token.intVal = 11
            default:
              self.signal(.unknownCharacterLiteral)
          }
        }
      default:
        if self.ch > 0xd7ff {
          self.signal(.unknownCharacterLiteral)
        } else {
          self.token.kind = .char
          self.token.intVal = Int64(self.ch)
          self.nextCh()
        }
    }
  }
  
  /// Scans a hex number with a given number of digits.
  private func scanHexNumber(_ maxDigits: Int) -> Int64? {
    guard isDigitForRadix(self.ch, 16) else {
      return nil
    }
    var i = maxDigits
    var res: Int64 = 0
    while i > 0 && isDigitForRadix(self.ch, 16) {
      guard case (let x, false) = Int64.multiplyWithOverflow(res, 16),
            case (let y, false) = Int64.addWithOverflow(x, Int64(digitVal(self.ch))) else {
        return nil
      }
      res = y
      self.nextCh()
      i -= 1
    }
    return res
  }
  
  /// Scans an exact or inexact number
  private func scanGeneralNumber(exact: Bool? = nil) {
    if self.ch == HASH_CH {
      self.nextCh()
      switch self.ch {
        case B_CH:
          self.nextCh()
          self.scanSignedNumber(2)
        case O_CH:
          self.nextCh()
          self.scanSignedNumber(8)
        case D_CH:
          self.nextCh()
          self.scanSignedNumber(10)
        case X_CH:
          self.nextCh()
          self.scanSignedNumber(16)
        default:
          self.signal(LexicalError.numberExpected)
      }
    } else {
      self.scanSignedNumber(10)
    }
    if let exact = exact {
      if exact {
        switch self.token.kind {
          case .float:
            self.token.kind = .rat
            self.token.ratVal = MathLibrary.approximate(self.token.floatVal)
            self.token.floatVal = 0.0
          case .complex:
            self.signal(LexicalError.exactComplexNumbersUnsupported)
          default:
            break
        }
      } else {
        switch self.token.kind {
          case .int:
            self.token.kind = .float
            self.token.floatVal = Double(self.token.intVal)
            self.token.intVal = 0
          case .bigint:
            self.token.kind = .float
            self.token.floatVal = self.token.bigIntVal.doubleValue
            self.token.bigIntVal = 0
          case .rat:
            self.token.kind = .float
            self.token.floatVal = Double(self.token.ratVal.numerator) /
                                  Double(self.token.ratVal.denominator)
            self.token.ratVal = 0
          case .bigrat:
            self.token.kind = .float
            self.token.floatVal = self.token.bigRatVal.numerator.doubleValue /
                                  self.token.bigRatVal.denominator.doubleValue
            self.token.bigRatVal = 0
          default:
            break
        }
      }
    }
  }
  
  /// Scans the next characters as a signed integer or floating point number.
  internal func scanSignedNumber(_ radix: Int) {
    switch self.ch {
      case MINUS_CH:
        self.nextCh()
        self.scanNumber(radix, neg: true, dot: false)
      case PLUS_CH:
        self.nextCh()
        fallthrough
      default:
        self.scanNumber(radix, neg: false, dot: false)
    }
  }
  
  /// Scans the next characters as an unsigned integer or floating point number.
  private func scanNumber(_ radix: Int, neg: Bool, dot: Bool) {
    var digits: [UInt8] = []
    var isFloat = dot
    let start = self.buffer.index - 1
    if !isFloat {
      if radix != 10 || self.ch != DOT_CH {
        while isDigitForRadix(self.ch, radix) {
          digits.append(UInt8(digitVal(self.ch)))
          self.nextCh()
        }
      }
      if radix == 10 && self.ch == DOT_CH {
        isFloat = true
        self.nextCh()
      }
    }
    if isFloat {
      while isDigit(self.ch) {
        self.nextCh()
      }
      if self.ch == LE_CH || self.ch == UE_CH {
        self.nextCh()
        if self.ch == PLUS_CH || self.ch == MINUS_CH {
          self.nextCh()
        }
        while isDigit(self.ch) {
          self.nextCh()
        }
      }
      let s = self.buffer.stringStartingAt(start)
      if let dbl = Double(dot ? "." + s : s) {
        switch self.ch {
          case PLUS_CH:
            self.nextCh()
            return scanImaginaryPart(neg ? -dbl : dbl, neg: false)
          case MINUS_CH:
            self.nextCh()
            return scanImaginaryPart(neg ? -dbl : dbl, neg: true)
          default:
            self.token.kind = .float
            self.token.floatVal = neg ? -dbl : dbl
        }
      } else {
        self.signal(.malformedFloatLiteral)
      }
    } else {
      let numer = BigInt(digits: digits, negative: neg, base: BigInt.base(of: radix))
      switch self.ch {
        case SLASH_CH:
          self.nextCh()
          digits = []
          while isDigitForRadix(self.ch, radix) {
            digits.append(UInt8(digitVal(self.ch)))
            self.nextCh()
          }
          guard digits.count > 0 else {
            self.scanIdent()
            return
          }
          let denom = BigInt(digits: digits, negative: false, base: BigInt.base(of: radix))
          guard denom != 0 else {
            self.signal(LexicalError.divisionByZero)
            return
          }
          if let n = numer.intValue, let d = denom.intValue {
            self.token.kind = .rat
            self.token.ratVal = Rational(n, d)
          } else {
            self.token.kind = .bigrat
            self.token.bigRatVal = Rational(numer, denom)
          }
        case PLUS_CH:
          self.nextCh()
          return scanImaginaryPart(numer.doubleValue, neg: false)
        case MINUS_CH:
          self.nextCh()
          return scanImaginaryPart(numer.doubleValue, neg: true)
        default:
          if let i = numer.intValue {
            self.token.kind = .int
            self.token.intVal = i
          } else {
            self.token.kind = .bigint
            self.token.bigIntVal = numer
          }
        }
    }
  }
  
  /// Scans the next characters as an unsigned floating point number representing the
  /// imaginary part of a complex number
  private func scanImaginaryPart(_ realPart: Double, neg: Bool) {
    let start = self.buffer.index - 1
    guard self.ch != LI_CH && self.ch != N_CH else {
      self.scanIdent()
      let s = self.buffer.stringStartingAt(start)
      switch s {
        case "inf.0i":
          self.token.kind = .complex
          self.token.complexVal = Complex(realPart, neg ? -Double.infinity : Double.infinity)
          self.token.strVal = ""
        case "nan.0i":
          self.token.kind = .complex
          self.token.complexVal = Complex(realPart, Double.nan)
          self.token.strVal = ""
        default:
          self.signal(.malformedComplexLiteral)
      }
      return
    }
    while isDigitForRadix(self.ch, 10) {
      self.nextCh()
    }
    if self.ch == DOT_CH {
      self.nextCh()
      while isDigit(self.ch) {
        self.nextCh()
      }
      if self.ch == LE_CH || self.ch == UE_CH {
        self.nextCh()
        if self.ch == PLUS_CH || self.ch == MINUS_CH {
          self.nextCh()
        }
        while isDigit(self.ch) {
          self.nextCh()
        }
      }
    }
    if self.ch == LI_CH || self.ch == UI_CH {
      let s = self.buffer.stringStartingAt(start)
      self.nextCh()
      if let dbl = Double(s) {
        self.token.kind = .complex
        self.token.complexVal = Complex(realPart, neg ? -dbl : dbl)
      } else {
        self.signal(.malformedFloatLiteral)
      }
    } else {
      self.signal(.malformedComplexLiteral)
    }
  }
  
  /// Scans the next characters as a string literal.
  private func scanString() {
    switch self.scanCharSequenceUntil(DQ_CH) {
      case .success(let str):
        self.token.kind = .string
        self.token.strVal = str
      case .malformed:
        self.signal(.malformedStringLiteral)
      case .illegalEscapeSequence:
        self.signal(.illegalEscapeSequence)
      case .illegalEndOfLine:
        self.signal(.illegalEndOfLine)
      case .illegalHexChar:
        self.signal(.illegalHexCharacter)
      case .unsupported:
        self.signal(.tokenNotYetSupported)
    }
  }
  
  /// Scans the next characters as an identifier.
  private func scanDelimitedIdent() {
    switch self.scanCharSequenceUntil(BAR_CH) {
      case .success(let str):
        self.token.kind = .ident
        self.token.strVal = str.lowercased()
      case .malformed:
        self.signal(.malformedIdentifier)
      case .illegalEscapeSequence:
        self.signal(.illegalEscapeSequence)
      case .illegalEndOfLine:
        self.signal(.illegalEndOfLine)
      case .illegalHexChar:
        self.signal(.illegalHexCharacter)
      case .unsupported:
        self.signal(.tokenNotYetSupported)
    }
  }
  
  /// Result type of `scanCharSequenceUntil`.
  private enum CharSequenceResult {
    case success(String)
    case malformed
    case illegalEscapeSequence
    case illegalEndOfLine
    case illegalHexChar
    case unsupported
  }
  
  /// Scans the next characters until the given terminator character and returns the character
  /// sequence as a string.
  private func scanCharSequenceUntil(_ terminator: UniChar) -> CharSequenceResult {
    var uniChars: [UniChar] = []
    self.nextCh()
    while self.ch != terminator {
      if self.ch == EOF_CH {
        return .malformed
      } else if self.ch == BS_CH {
        self.nextCh()
        switch self.ch {
          case BS_CH:
            self.nextCh();
            uniChars.append(BS_CH)
          case DQ_CH:
            self.nextCh();
            uniChars.append(DQ_CH)
          case X_CH:
            self.nextCh();
            guard let ch = self.scanHexChar() else {
              return .illegalHexChar
            }
            uniChars.append(ch)
          case LA_CH: // alarm
            self.nextCh();
            uniChars.append(7)
          case B_CH: // backspace
            self.nextCh();
            uniChars.append(8)
          case T_CH: // tab
            self.nextCh();
            uniChars.append(9)
          case N_CH: // linefeed
            self.nextCh();
            uniChars.append(10)
          case V_CH: // vertical tab
            self.nextCh();
            uniChars.append(11)
          case F_CH: // formfeed
            self.nextCh();
            uniChars.append(12)
          case R_CH: // return
            self.nextCh();
            uniChars.append(13)
          case LE_CH: // escape
            self.nextCh();
            uniChars.append(27)
          case EOL_CH:
            self.nextCh();
          case RET_CH:
            self.nextCh()
            if self.ch == EOL_CH {
              self.nextCh()
            }
          default:
            return .illegalEscapeSequence
        }
      } else if self.ch == EOL_CH || self.ch == RET_CH {
        self.nextCh()
        return .illegalEndOfLine
      } else {
        uniChars.append(self.ch)
        self.nextCh()
      }
    }
    self.nextCh()
    return .success(String(utf16CodeUnits: uniChars, count: uniChars.count))
  }
  
  /// Scans a hex number with a given number of digits.
  private func scanHexChar() -> UniChar? {
    guard isDigitForRadix(self.ch, 16) else {
      return nil
    }
    var res: UInt16 = 0
    while isDigitForRadix(self.ch, 16) {
      guard case (let x, false) = UInt16.multiplyWithOverflow(res, 16),
            case (let y, false) = UInt16.addWithOverflow(x, UInt16(digitVal(self.ch))) else {
        return nil
      }
      res = y
      self.nextCh()
    }
    guard self.ch == SEMI_CH else {
      return nil
    }
    self.nextCh()
    return res
  }
}

/// Struct `Position` represents a position in the scanned string in terms of a line and
/// column number.
public struct Position: CustomStringConvertible {
  public var line: UInt
  public var col: UInt
  
  init(_ line: UInt, _ col: UInt) {
    self.line = line
    self.col = col
  }
  
  public var description: String {
    return self.line == 0 ? "" : (self.col == 0 ? "\(self.line)" : "\(self.line):\(self.col)")
  }
}

/// Struct `Token` represents a lexical token. Class `Scanner` generates a sequence of such
/// tokens.
public struct Token: CustomStringConvertible {
  public var pos: Position
  public var kind: TokenKind
  public var strVal: String
  public var intVal: Int64
  public var bigIntVal: BigInt
  public var ratVal: Rational<Int64>
  public var bigRatVal: Rational<BigInt>
  public var floatVal: Double
  public var complexVal: Complex<Double>
  public var errorVal: LexicalError?
  
  public var description: String {
    switch self.kind {
      case .error     : return "<error: \(self.errorVal)>"
      case .eof       : return "<eof>"
      case .ident     : return self.strVal
      case .truelit   : return "#t"
      case .falselit  : return "#f"
      case .int       : return self.intVal.description
      case .bigint    : return self.bigIntVal.description
      case .rat       : return self.ratVal.description
      case .bigrat    : return self.bigRatVal.description
      case .float     : return self.floatVal.description
      case .complex   : return self.complexVal.description
      case .char      :
        switch self.intVal {
          case   7: return "#\\alarm"
          case   8: return "#\\backspace"
          case 127: return "#\\delete"
          case  27: return "#\\escape"
          case  10: return "#\\newline"
          case   0: return "#\\null"
          case  13: return "#\\return"
          case  32: return "#\\space"
          case   9: return "#\\tab"
          default : return "#\\\(self.strVal)"
        }
      case .string    : return "\"\(self.strVal)\""
      case .lparen    : return "("
      case .rparen    : return ")"
      case .hashlparen: return "#("
      case .u8LPAREN  : return "#u8("
      case .quote     : return "'"
      case .backquote : return "`"
      case .comma     : return ","
      case .commaat   : return ",@"
      case .dot       : return "."
    }
  }
  
  mutating func reset(_ pos: Position) {
    self.pos = pos
    self.kind = .error
    self.strVal = ""
    self.intVal = 0
    self.bigIntVal = 0
    self.ratVal = 0
    self.bigRatVal = 0
    self.floatVal = 0.0
    self.complexVal = 0
    self.errorVal = nil
  }
}

public enum TokenKind: Int, CustomStringConvertible {
  case error
  case eof
  case ident
  case truelit
  case falselit
  case int
  case bigint
  case rat
  case bigrat
  case float
  case complex
  case char
  case string
  case lparen
  case rparen
  case hashlparen
  case u8LPAREN
  case quote
  case backquote
  case comma
  case commaat
  case dot
  
  public var description: String {
    switch self {
      case .error     : return "ERROR"
      case .eof       : return "EOF"
      case .ident     : return "IDENT"
      case .truelit   : return "TRUELIT"
      case .falselit  : return "FALSELIT"
      case .int       : return "INT"
      case .bigint    : return "BIGINT"
      case .rat       : return "RAT"
      case .bigrat    : return "BIGRAT"
      case .float     : return "FLOAT"
      case .complex   : return "COMPLEX"
      case .char      : return "CHAR"
      case .string    : return "STRING"
      case .lparen    : return "LPAREN"
      case .rparen    : return "RPAREN"
      case .hashlparen: return "HASHLPAREN"
      case .u8LPAREN  : return "U8LPAREN"
      case .quote     : return "quote"
      case .backquote : return "BACKQUOTE"
      case .comma     : return "COMMA"
      case .commaat   : return "COMMAAT"
      case .dot       : return "DOT"
    }
  }
}

func UniChar(_ str: String) -> UniChar {
  return str.utf16.first!
}

let EOF_CH: UniChar    = 0
let EOL_CH             = UniChar("\n")
let RET_CH             = UniChar("\r")
let TAB_CH             = UniChar("\t")
let SPACE_CH           = UniChar(" ")
let ZERO_CH            = UniChar("0")
let B_CH               = UniChar("b")
let D_CH               = UniChar("d")
let O_CH               = UniChar("o")
let LA_CH              = UniChar("a")
let UA_CH              = UniChar("A")
let LZ_CH              = UniChar("z")
let UZ_CH              = UniChar("Z")
let DQ_CH              = UniChar("\"")
let BQ_CH              = UniChar("`")
let Q_CH               = UniChar("\'")
let OPENQ_CH: UniChar  = 8216
let CLOSEQ_CH: UniChar = 8217
let DOT_CH             = UniChar(".")
let COMMA_CH           = UniChar(",")
let SEMI_CH            = UniChar(";")
let AT_CH              = UniChar("@")
let HASH_CH            = UniChar("#")
let MINUS_CH           = UniChar("-")
let PLUS_CH            = UniChar("+")
let LE_CH              = UniChar("e")
let UE_CH              = UniChar("E")
let LI_CH              = UniChar("i")
let UI_CH              = UniChar("I")
let BS_CH              = UniChar("\\")
let SLASH_CH           = UniChar("/")
let BAR_CH             = UniChar("|")
let X_CH               = UniChar("x")
let T_CH               = UniChar("t")
let R_CH               = UniChar("r")
let N_CH               = UniChar("n")
let V_CH               = UniChar("v")
let F_CH               = UniChar("f")
let U_CH               = UniChar("u")
let EIGHT_CH           = UniChar("8")
let LPAREN_CH          = UniChar("(")
let RPAREN_CH          = UniChar(")")

let WHITESPACES        = CharacterSet.whitespaces
let WHITESPACES_NL     = CharacterSet.whitespacesAndNewlines
let CONTROL_CHARS      = CharacterSet.controlCharacters
let ILLEGAL_CHARS      = CharacterSet.illegalCharacters
let MODIFIER_CHARS     = CharacterSet.nonBaseCharacters
let DIGITS             = CharacterSet(charactersIn: "0123456789")
let LHEXDIGITS         = CharacterSet(charactersIn: "abcdef")
let UHEXDIGITS         = CharacterSet(charactersIn: "ABCDEF")
let LETTERS            = CharacterSet.letters
let UPPER_LETTERS      = CharacterSet.uppercaseLetters
let LOWER_LETTERS      = CharacterSet.lowercaseLetters
let INITIALS           = CharacterSet(charactersIn: "!$%&*/:<=>?^_~")
let SUBSEQUENTS        = CharacterSet(charactersIn: "+-.@")
let SIGNSUBSEQUENTS    = CharacterSet(charactersIn: "+-@")

func isSpace(_ ch: UniChar) -> Bool {
  return WHITESPACES.contains(UnicodeScalar(ch)!)
}

func isLetter(_ ch: UniChar) -> Bool {
  return LETTERS.contains(UnicodeScalar(ch)!)
}

func isDigit(_ ch: UniChar) -> Bool {
  return DIGITS.contains(UnicodeScalar(ch)!)
}

func isDigitForRadix(_ ch: UniChar, _ radix: Int) -> Bool {
  return digitVal(ch) < radix
}

func isInitialIdent(_ ch: UniChar) -> Bool {
  return LETTERS.contains(UnicodeScalar(ch)!) ||
         INITIALS.contains(UnicodeScalar(ch)!)
}

func isSubsequentIdent(_ ch: UniChar) -> Bool {
  return isInitialIdent(ch) ||
         isDigit(ch) ||
         SUBSEQUENTS.contains(UnicodeScalar(ch)!)
}

func isSignSubsequent(_ ch: UniChar) -> Bool {
  return isInitialIdent(ch) ||
         SIGNSUBSEQUENTS.contains(UnicodeScalar(ch)!)
}

func isDotSubsequent(_ ch: UniChar) -> Bool {
  return isSignSubsequent(ch) ||
         ch == DOT_CH
}

func digitVal(_ ch: UniChar) -> Int {
  if DIGITS.contains(UnicodeScalar(ch)!) {
    return Int(ch - ZERO_CH)
  } else if LHEXDIGITS.contains(UnicodeScalar(ch)!) {
    return Int(ch - LA_CH + 10)
  } else if UHEXDIGITS.contains(UnicodeScalar(ch)!) {
    return Int(ch - UA_CH + 10)
  } else {
    return 16
  }
}

func unicodeScalar(_ ch: UniChar) -> UnicodeScalar {
  return UnicodeScalar(ch) ?? UnicodeScalar(0)
}
