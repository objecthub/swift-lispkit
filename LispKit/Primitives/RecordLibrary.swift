//
//  RecordLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 12/08/2016.
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
/// Record library: based on R7RS spec.
/// 
public final class RecordLibrary: NativeLibrary {
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "record"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "base"], "define", "define-syntax", "syntax-rules",
                                             "lambda", "quote", "symbol->string")
    self.`import`(from: ["lispkit", "control"], "let", "begin")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("record?", isRecord))
    self.define(Procedure("record-type?", isRecordType))
    self.define(Procedure("record-type", recordType))
    self.define(Procedure("make-record-type", makeRecordType))
    self.define(Procedure("record-type-name", recordTypeName))
    self.define(Procedure("record-type-field-names", recordTypeFieldNames))
    self.define(Procedure("record-type-field-index", recordTypeFieldIndex))
    self.define(Procedure("make-record", makeRecord))
    self.define(Procedure("record-ref", recordRef))
    self.define(Procedure("record-set!", recordSet))
    self.define("record-constructor", via:
      "(define (record-constructor type fields)",
      "  (let ((indices (record-type-field-index type fields)))",
      "    (lambda args",
      "      (let ((record (make-record type)))",
      "        (record-set! record indices args) record))))")
    self.define("record-predicate", via:
      "(define (record-predicate type) (lambda (x) (record? x type)))")
    self.define("record-field-accessor", via:
      "(define (record-field-accessor type field)",
      "  (let ((index (record-type-field-index type field)))",
      "    (lambda (record) (record-ref record index))))")
    self.define("record-field-mutator", via:
      "(define (record-field-mutator type field)",
      "  (let ((index (record-type-field-index type field)))",
      "    (lambda (record value) (record-set! record index value))))")
    self.define("define-record-field", via:
      "(define-syntax define-record-field",
      "  (syntax-rules ()",
      "    ((_ type field accessor)",
      "      (define accessor (record-field-accessor type 'field)))",
      "    ((_ type field accessor mutator)",
      "      (begin",
      "        (define accessor (record-field-accessor type 'field))",
      "        (define mutator (record-field-mutator type 'field))))))")
    self.define("define-record-type", via:
      "(define-syntax define-record-type",
      "  (syntax-rules ()",
      "    ((_ type (constr cfield ...) pred (field accessor . mutator) ...)",
      "      (begin",
      "        (define type (make-record-type (symbol->string 'type) '(cfield ...)))",
      "        (define constr (record-constructor type '(cfield ...)))",
      "        (define pred (record-predicate type))",
      "        (define-record-field type field accessor . mutator) ...))))")
  }
  
  func isRecord(_ expr: Expr, rtype: Expr?) -> Expr {
    guard case .record(let record) = expr else {
      return .false
    }
    guard let rectype = rtype else {
      return .true
    }
    guard case .record(let exprtype) = record.kind else {
      return .false
    }
    guard case .record(let type) = rectype,
          case .recordType = type.kind else {
      return .false
    }
    return .makeBoolean(type === exprtype)
  }
  
  func isRecordType(_ expr: Expr) -> Expr {
    guard case .record(let record) = expr,
          case .recordType = record.kind else {
      return .false
    }
    return .true
  }
  
  func recordType(_ expr: Expr) -> Expr {
    guard case .record(let record) = expr,
          case .record(let type) = record.kind else {
      return .false
    }
    return .record(type)
  }
  
  func makeRecordType(_ name: Expr, fields: Expr) throws -> Expr {
    // Check that first argument is a string
    let _ = try name.asMutableStr()
    // Check that second argument is a proper list of symbols
    var numFields = 0
    var current = fields
    while case .pair(let sym, let next) = current {
      guard case .symbol(_) = sym else {
        throw EvalError.typeError(sym, [.symbolType])
      }
      numFields += 1
      current = next
    }
    guard case .null = current else {
      throw EvalError.typeError(fields, [.properListType])
    }
    // Return record type
    return .record(Collection(kind: .recordType, exprs: [name, .makeNumber(numFields), fields]))
  }
  
  func recordTypeName(_ expr: Expr) -> Expr {
    guard case .record(let record) = expr,
          case .recordType = record.kind else {
      return .false
    }
    return record.exprs[0]
  }
  
  func recordTypeFieldNames(_ expr: Expr) -> Expr {
    guard case .record(let record) = expr,
          case .recordType = record.kind else {
      return .false
    }
    return record.exprs[2]
  }
  
  func recordTypeFieldIndex(_ expr: Expr, name: Expr) throws -> Expr {
    let record = try expr.recordAsCollection()
    guard case .recordType = record.kind else {
      return .false
    }
    switch name {
      case .symbol(let field):
        guard let index = self.indexOfField(field, in: record.exprs[2]) else {
          throw EvalError.unknownFieldOfRecordType(expr, field)
        }
        return .makeNumber(index)
      case .pair(_, _):
        var indices = Exprs()
        var current = name
        while case .pair(let sym, let next) = current {
          let field = try sym.asSymbol()
          guard let index = self.indexOfField(field, in: record.exprs[2]) else {
            throw EvalError.unknownFieldOfRecordType(expr, field)
          }
          indices.append(.makeNumber(index))
          current = next
        }
        guard current.isNull else {
          throw EvalError.typeError(name, [.properListType])
        }
        return .makeList(indices)
      default:
        throw EvalError.typeError(name, [.symbolType])
    }
  }
  
  fileprivate func indexOfField(_ field: Symbol, in fields: Expr) -> Int? {
    var index = 0
    var current = fields
    while case .pair(.symbol(let sym), let next) = current {
      if sym == field {
        return index
      }
      index += 1
      current = next
    }
    return nil
  }
  
  func makeRecord(_ expr: Expr) throws -> Expr {
    let type = try expr.recordAsCollection()
    guard case .recordType = type.kind else {
      return .false
    }
    guard case .fixnum(let size) = type.exprs[1] else {
      preconditionFailure("broken record type encoding: \(type)")
    }
    return .record(
      Collection(kind: .record(type), exprs: Array(repeating: .undef, count: Int(size))))
  }
  
  func recordRef(_ expr: Expr, index: Expr) throws -> Expr {
    let record = try expr.recordAsCollection()
    let idx = try index.asInt()
    guard idx >= 0 && idx < record.exprs.count else {
      throw EvalError.indexOutOfBounds(Int64(idx), Int64(record.exprs.count - 1), expr)
    }
    return record.exprs[idx]
  }
  
  func recordSet(_ expr: Expr, index: Expr, value: Expr) throws -> Expr {
    let record = try expr.recordAsCollection()
    guard case .record(_) = record.kind else {
      throw EvalError.attemptToModifyImmutableData(expr)
    }
    switch index {
      case .fixnum(let idx):
        guard idx >= 0 && idx < Int64(record.exprs.count) else {
          throw EvalError.indexOutOfBounds(idx, Int64(record.exprs.count - 1), expr)
        }
        // Set record field value. Guarantee that cells for which `record-set!` is called are
        // managed by a managed object pool.
        (value.isAtom ? record : self.context.objects.manage(record)).exprs[Int(idx)] = value
      case .pair(_, _):
        var allSimple = true
        var numFields = 0
        var currentIndex = index
        var currentValue = value
        while case .pair(.fixnum(let idx), let nextIndex) = currentIndex {
          guard idx >= 0 && idx < Int64(record.exprs.count) else {
            throw EvalError.indexOutOfBounds(idx, Int64(record.exprs.count - 1), expr)
          }
          guard case .pair(let v, let nextValue) = currentValue else {
            while case .pair(_, let nextIndex) = currentIndex {
              numFields += 1
              currentIndex = nextIndex
            }
            throw EvalError.fieldCountError(numFields, value)
          }
          record.exprs[Int(idx)] = v
          // Guarantee that we manage this record by a managed object pool if there was at least
          // one field value which was not simple
          if allSimple && !v.isAtom {
            _ = self.context.objects.manage(record)
            allSimple = false
          }
          numFields += 1
          currentIndex = nextIndex
          currentValue = nextValue
        }
        guard currentIndex.isNull else {
          throw EvalError.typeError(index, [.properListType])
        }
        guard currentValue.isNull else {
          throw EvalError.fieldCountError(numFields, value)
        }
      default:
        throw EvalError.typeError(index, [.exactIntegerType])
    }
    return .void
  }
}
