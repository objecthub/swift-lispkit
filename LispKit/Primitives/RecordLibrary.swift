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
public final class RecordLibrary: Library {
  
  public override func export() {
    define(Procedure("record?", isRecord))
    define(Procedure("record-type?", isRecordType))
    define(Procedure("record-type", recordType))
    define(Procedure("make-record-type", makeRecordType))
    define(Procedure("record-type-name", recordTypeName))
    define(Procedure("record-type-field-names", recordTypeFieldNames))
    define(Procedure("record-type-field-index", recordTypeFieldIndex))
    define(Procedure("make-record", makeRecord))
    define(Procedure("record-ref", recordRef))
    define(Procedure("record-set!", recordSet))
    define("record-constructor", compile:
      "(lambda (type fields)" +
      "  (let ((indices (record-type-field-index type fields)))" +
      "    (lambda args" +
      "      (let ((record (make-record type)))" +
      "        (record-set! record indices args) record))))")
    define("record-predicate", compile: "(lambda (type) (lambda (x) (record? x type)))")
    define("record-field-accessor", compile:
      "(lambda (type field)" +
      "  (let ((index (record-type-field-index type field)))" +
      "    (lambda (record) (record-ref record index))))")
    define("record-field-mutator", compile:
      "(lambda (type field)" +
      "  (let ((index (record-type-field-index type field)))" +
      "    (lambda (record value) (record-set! record index value))))")
    define("define-record-field", syntax:
      "(syntax-rules ()" +
      "  ((_ type field accessor)" +
      "    (define accessor (record-field-accessor type 'field)))" +
      "  ((_ type field accessor mutator)" +
      "    (begin" +
      "      (define accessor (record-field-accessor type 'field))" +
      "      (define mutator (record-field-mutator type 'field)))))")
    define("define-record-type", syntax:
      "(syntax-rules ()" +
      "  ((_ type (constr cfield ...) pred (field accessor . mutator) ...)" +
      "    (begin" +
      "      (define type (make-record-type (symbol->string 'type) '(cfield ...)))" +
      "      (define constr (record-constructor type '(cfield ...)))" +
      "      (define pred (record-predicate type))" +
      "      (define-record-field type field accessor . mutator) ...)))")
  }
  
  func isRecord(expr: Expr, rtype: Expr?) -> Expr {
    guard case .Record(let record) = expr else {
      return .False
    }
    guard let rectype = rtype else {
      return .True
    }
    guard case .Record(let exprtype) = record.kind else {
      return .False
    }
    guard case .Record(let type) = rectype, .RecordType = type.kind else {
      return .False
    }
    return .Boolean(type === exprtype)
  }
  
  func isRecordType(expr: Expr) -> Expr {
    guard case .Record(let record) = expr, .RecordType = record.kind else {
      return .False
    }
    return .True
  }
  
  func recordType(expr: Expr) -> Expr {
    guard case .Record(let record) = expr, .Record(let type) = record.kind else {
      return .False
    }
    return .Record(type)
  }
  
  func makeRecordType(name: Expr, fields: Expr) throws -> Expr {
    // Check that first argument is a string
    let _ = try name.asMutableStr()
    // Check that second argument is a proper list of symbols
    var numFields = 0
    var current = fields
    while case .Pair(let sym, let next) = current {
      guard case .Sym(_) = sym else {
        throw EvalError.TypeError(sym, [.SymbolType])
      }
      numFields += 1
      current = next
    }
    guard case .Null = current else {
      throw EvalError.TypeError(fields, [.ProperListType])
    }
    // Return record type
    return .Record(Collection(kind: .RecordType, exprs: [name, .Number(numFields), fields]))
  }
  
  func recordTypeName(expr: Expr) -> Expr {
    guard case .Record(let record) = expr, .RecordType = record.kind else {
      return .False
    }
    return record.exprs[0]
  }
  
  func recordTypeFieldNames(expr: Expr) -> Expr {
    guard case .Record(let record) = expr, .RecordType = record.kind else {
      return .False
    }
    return record.exprs[2]
  }
  
  func recordTypeFieldIndex(expr: Expr, name: Expr) throws -> Expr {
    let record = try expr.asRecord()
    guard case .RecordType = record.kind else {
      return .False
    }
    switch name {
      case .Sym(let field):
        guard let index = self.indexOfField(field, in: record.exprs[2]) else {
          throw EvalError.UnknownFieldOfRecordType(expr, field)
        }
        return .Number(index)
      case .Pair(_, _):
        var indices = Exprs()
        var current = name
        while case .Pair(let sym, let next) = current {
          let field = try sym.asSymbol()
          guard let index = self.indexOfField(field, in: record.exprs[2]) else {
            throw EvalError.UnknownFieldOfRecordType(expr, field)
          }
          indices.append(.Number(index))
          current = next
        }
        guard current.isNull else {
          throw EvalError.TypeError(name, [.ProperListType])
        }
        return .List(indices)
      default:
        throw EvalError.TypeError(name, [.SymbolType])
    }
  }
  
  private func indexOfField(field: Symbol, in fields: Expr) -> Int? {
    var index = 0
    var current = fields
    while case .Pair(.Sym(let sym), let next) = current {
      if sym == field {
        return index
      }
      index += 1
      current = next
    }
    return nil
  }
  
  func makeRecord(expr: Expr) throws -> Expr {
    let type = try expr.asRecord()
    guard case .RecordType = type.kind else {
      return .False
    }
    guard case .Fixnum(let size) = type.exprs[1] else {
      preconditionFailure("broken record type encoding: \(type)")
    }
    return .Record(
      Collection(kind: .Record(type), exprs: Array(count: Int(size), repeatedValue: .Undef)))
  }
  
  func recordRef(expr: Expr, index: Expr) throws -> Expr {
    let record = try expr.asRecord()
    let idx = try index.asInt()
    guard idx >= 0 && idx < record.exprs.count else {
      throw EvalError.IndexOutOfBounds(Int64(idx), Int64(record.exprs.count - 1), expr)
    }
    return record.exprs[idx]
  }
  
  func recordSet(expr: Expr, index: Expr, value: Expr) throws -> Expr {
    let record = try expr.asRecord()
    guard case .Record(_) = record.kind else {
      throw EvalError.AttemptToModifyImmutableData(expr)
    }
    switch index {
      case .Fixnum(let idx):
        guard idx >= 0 && idx < Int64(record.exprs.count) else {
          throw EvalError.IndexOutOfBounds(idx, Int64(record.exprs.count - 1), expr)
        }
        // Set record field value. Guarantee that cells for which `record-set!` is called are
        // managed by a managed object pool.
        (value.isSimple ? record : self.context.objects.manage(record)).exprs[Int(idx)] = value
      case .Pair(_, _):
        var allSimple = true
        var numFields = 0
        var currentIndex = index
        var currentValue = value
        while case .Pair(.Fixnum(let idx), let nextIndex) = currentIndex {
          guard idx >= 0 && idx < Int64(record.exprs.count) else {
            throw EvalError.IndexOutOfBounds(idx, Int64(record.exprs.count - 1), expr)
          }
          guard case .Pair(let v, let nextValue) = currentValue else {
            while case .Pair(_, let nextIndex) = currentIndex {
              numFields += 1
              currentIndex = nextIndex
            }
            throw EvalError.FieldCountError(numFields, value)
          }
          record.exprs[Int(idx)] = v
          // Guarantee that we manage this record by a managed object pool if there was at least
          // one field value which was not simple
          if allSimple && !v.isSimple {
            self.context.objects.manage(record)
            allSimple = false
          }
          numFields += 1
          currentIndex = nextIndex
          currentValue = nextValue
        }
        guard currentIndex.isNull else {
          throw EvalError.TypeError(index, [.ProperListType])
        }
        guard currentValue.isNull else {
          throw EvalError.FieldCountError(numFields, value)
        }
      default:
        throw EvalError.TypeError(index, [.IntegerType])
    }
    return .Void
  }
}
