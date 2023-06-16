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
    self.`import`(from: ["lispkit", "core"],    "define", "define-syntax", "syntax-rules",
                                                "lambda", "quote", "void", "symbol->string")
    self.`import`(from: ["lispkit", "control"], "let", "begin")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("record?", isRecord))
    self.define(Procedure("record-type?", isRecordType))
    self.define(Procedure("record-type", recordType))
    self.define(Procedure("make-record-type", makeRecordType))
    self.define(Procedure("record-type-tag", recordTypeId))
    self.define(Procedure("record-type-name", recordTypeName))
    self.define(Procedure("record-type-parent", recordTypeParent))
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
      "        (record-set! record indices args type) record))))")
    self.define("_define-record-constructor", via:
      "(define-syntax _define-record-constructor",
      "  (syntax-rules ()",
      "    ((_ name type fields)",
      "      (define name",
      "        (let ((indices (record-type-field-index type fields))(t type))",
      "          (define (name . args)",
      "            (record-set! (make-record t) indices args t)) name)))))")
    self.define("record-predicate", via:
      "(define (record-predicate type) (lambda (x) (record? x type)))")
    self.define("record-field-accessor", via:
      "(define (record-field-accessor type field)",
      "  (let ((index (record-type-field-index type field)))",
      "    (lambda (record) (record-ref record index type))))")
    self.define("record-field-mutator", via:
      "(define (record-field-mutator type field)",
      "  (let ((index (record-type-field-index type field)))",
      "    (lambda (record value) (record-set! record index value type))))")
    self.define("_define-record-field", via:
      "(define-syntax _define-record-field",
      "  (syntax-rules ()",
      "    ((_ type field accessor)",
      "      (define accessor (let ((index (record-type-field-index type 'field))(t type))",
      "                         (define (accessor record) (record-ref record index t))",
      "                         accessor)))",
      "    ((_ type field accessor mutator)",
      "      (begin",
      "        (define accessor (let ((index (record-type-field-index type 'field))(t type))",
      "                           (define (accessor record) (record-ref record index t))",
      "                           accessor))",
      "        (define mutator (let ((index (record-type-field-index type 'field))(t type))",
      "                          (define (mutator record value)",
      "                            (record-set! record index value t))",
      "                          mutator))))))")
    self.define("_define-record-type", via:
      "(define-syntax _define-record-type",
      "  (syntax-rules ()",
      "    ((_ type parent #f args #f (field accessor . mutator) ...)",
      "      (begin",
      "        (define type (make-record-type (symbol->string 'type) '(field ...) parent))",
      "        (_define-record-field type field accessor . mutator) ...))",
      "    ((_ type parent constr args #f (field accessor . mutator) ...)",
      "      (begin",
      "        (define type (make-record-type (symbol->string 'type) '(field ...) parent))",
      "        (_define-record-constructor constr type args)",
      "        (_define-record-field type field accessor . mutator) ...))",
      "    ((_ type parent #f args pred (field accessor . mutator) ...)",
      "      (begin",
      "        (define type (make-record-type (symbol->string 'type) '(field ...) parent))",
      "        (define pred (let ((t type)) (define (pred x) (record? x t)) pred))",
      "        (_define-record-field type field accessor . mutator) ...))",
      "    ((_ type parent constr args pred (field accessor . mutator) ...)",
      "      (begin",
      "        (define type (make-record-type (symbol->string 'type) '(field ...) parent))",
      "        (_define-record-constructor constr type args)",
      "        (define pred (let ((t type)) (define (pred x) (record? x t)) pred))",
      "        (_define-record-field type field accessor . mutator) ...))))")
    self.define("define-record-type", via:
      "(define-syntax define-record-type",
      "  (syntax-rules ()",
      "    ((_ (type parent) #f #f field ...)",
      "      (_define-record-type type parent #f #f #f field ...))",
      "    ((_ (type parent) (constr cfield ...) #f field ...)",
      "      (_define-record-type type parent constr (quote (cfield ...)) #f field ...))",
      "    ((_ (type parent) #f pred field ...)",
      "      (_define-record-type type parent #f #f pred field ...))",
      "    ((_ (type parent) (constr cfield ...) pred field ...)",
      "      (_define-record-type type parent constr (quote (cfield ...)) pred field ...))",
      "    ((_ (type parent) constr pred field ...)",
      "      (_define-record-type type parent constr",
      "                           (record-type-field-names type #t) pred field ...))",
      "    ((_ (type) constr pred field ...)",
      "      (define-record-type (type #f) constr pred field ...))",
      "    ((_ type constr pred field ...)",
      "      (define-record-type (type #f) constr pred field ...))))")
  }
  
  private func isSubtypeOf(_ subType: Collection, _ superType: Collection) -> Bool {
    var recordType = subType
    while superType !== recordType {
      guard case .record(let coll) = recordType.exprs[Collection.RecordType.parent.rawValue],
            case .recordType = coll.kind else {
        return false
      }
      recordType = coll
    }
    return true
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
    return .makeBoolean(self.isSubtypeOf(exprtype, type))
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
  
  func makeRecordType(name: Expr, fields: Expr, parent: Expr?) throws -> Expr {
    // Check that first argument is a string
    let str = try name.asString()
    // Check that second argument is a proper list of symbols
    var numFields = 0
    var current = fields
    while case .pair(let sym, let next) = current {
      guard case .symbol(_) = sym else {
        throw RuntimeError.type(sym, expected: [.symbolType])
      }
      numFields += 1
      current = next
    }
    guard case .null = current else {
      throw RuntimeError.type(fields, expected: [.properListType])
    }
    if let parent = parent, parent != .false {
      guard case .record(let coll) = parent,
            case .recordType = coll.kind,
            let parentFields = try? coll.exprs[
                                 Collection.RecordType.totalFieldCount.rawValue].asInt() else {
        throw RuntimeError.type(parent, expected: [.recordType])
      }
      return .record(Collection(kind: .recordType,
                                exprs: [.symbol(Symbol(uninterned: str)),
                                        parent,
                                        .makeNumber(numFields + parentFields),
                                        .makeNumber(numFields),
                                        fields]))
    } else {
      return .record(Collection(kind: .recordType,
                                exprs: [.symbol(Symbol(uninterned: str)),
                                        .false,
                                        .makeNumber(numFields),
                                        .makeNumber(numFields),
                                        fields]))
    }
  }
  
  func recordTypeId(_ expr: Expr) -> Expr {
    guard case .record(let record) = expr,
          case .recordType = record.kind else {
      return .false
    }
    return record.exprs[Collection.RecordType.typeTag.rawValue]
  }
  
  func recordTypeName(_ expr: Expr) -> Expr {
    guard case .record(let record) = expr,
          case .recordType = record.kind else {
      return .false
    }
    return .makeString(
             record.exprs[Collection.RecordType.typeTag.rawValue].unescapedDescription)
  }
  
  func recordTypeParent(_ expr: Expr) -> Expr {
    guard case .record(let record) = expr,
          case .recordType = record.kind else {
      return .false
    }
    return record.exprs[Collection.RecordType.parent.rawValue]
  }
  
  func recordTypeFieldNames(_ expr: Expr, _ all: Expr?) -> Expr {
    guard case .record(let record) = expr,
          case .recordType = record.kind else {
      return .false
    }
    if all?.isTrue ?? false {
      let syms = Collection.RecordType.fields(record)
      var allFields = Expr.null
      for sym in syms.reversed() {
        allFields = .pair(.symbol(sym), allFields)
      }
      return allFields
    } else {
      return record.exprs[Collection.RecordType.fields.rawValue]
    }
  }
  
  func recordTypeFieldIndex(_ expr: Expr, name: Expr) throws -> Expr {
    let record = try expr.recordAsCollection()
    guard case .recordType = record.kind else {
      return .false
    }
    switch name {
      case .symbol(let field):
        guard let index = self.indexOfField(field, in: record) else {
          throw RuntimeError.eval(.unknownFieldOfRecordType, expr, name)
        }
        return .makeNumber(index)
      case .null:
        return .null
      case .pair(_, _):
        var indices = Exprs()
        var current = name
        while case .pair(let sym, let next) = current {
          let field = try sym.asSymbol()
          guard let index = self.indexOfField(field, in: record) else {
            throw RuntimeError.eval(.unknownFieldOfRecordType, expr, sym)
          }
          indices.append(.makeNumber(index))
          current = next
        }
        guard current.isNull else {
          throw RuntimeError.type(name, expected: [.properListType])
        }
        return .makeList(indices)
      default:
        throw RuntimeError.type(name, expected: [.symbolType])
    }
  }
  
  private func indexOfField(_ field: Symbol, in recordType: Collection) -> Int? {
    guard case .recordType = recordType.kind,
          let total = try? recordType.exprs[Collection.RecordType.totalFieldCount.rawValue].asInt(),
          let count = try? recordType.exprs[
                             Collection.RecordType.fieldCount.rawValue].asInt() else {
      return nil
    }
    var index = total - count
    var current = recordType.exprs[Collection.RecordType.fields.rawValue]
    while case .pair(.symbol(let sym), let next) = current {
      if sym == field {
        return index
      }
      index += 1
      current = next
    }
    if let parent = try? recordType.exprs[
                           Collection.RecordType.parent.rawValue].recordAsCollection() {
      return self.indexOfField(field, in: parent)
    } else {
      return nil
    }
  }
  
  func makeRecord(_ expr: Expr) throws -> Expr {
    let type = try expr.recordAsCollection()
    guard case .recordType = type.kind,
          let total = try? type.exprs[
                             Collection.RecordType.totalFieldCount.rawValue].asInt() else {
      return .false
    }
    return .record(
      Collection(kind: .record(type), exprs: Exprs(repeating: .undef, count: total)))
  }
  
  func recordRef(_ expr: Expr, _ index: Expr, _ type: Expr?) throws -> Expr {
    let record = try expr.recordAsCollection()
    let idx = try index.asInt()
    if let type = type {
      guard case .record(let tpe) = type,
            case .recordType = tpe.kind else {
        throw RuntimeError.eval(.expectedRecordToAccessField, type, expr)
      }
      guard case .record(let exprtype) = record.kind,
            self.isSubtypeOf(exprtype, tpe) else {
        throw RuntimeError.eval(.expectedRecordToAccessField,
                                tpe.exprs[Collection.RecordType.typeTag.rawValue],
                                expr)
      }
    }
    guard idx >= 0 && idx < record.exprs.count else {
      throw RuntimeError.range(parameter: 2,
                               of: "record-ref",
                               index, min: 0,
                               max: Int64(record.exprs.count - 1))
    }
    return record.exprs[idx]
  }
  
  func recordSet(_ expr: Expr, _ index: Expr, _ value: Expr, _ type: Expr?) throws -> Expr {
    let record = try expr.recordAsCollection()
    guard case .record(let exprtype) = record.kind else {
      throw RuntimeError.eval(.attemptToModifyImmutableData, expr)
    }
    if let type = type {
      guard case .record(let tpe) = type,
            case .recordType = tpe.kind else {
        throw RuntimeError.eval(.expectedRecordToAccessField, type, expr)
      }
      guard self.isSubtypeOf(exprtype, tpe) else {
        throw RuntimeError.eval(.expectedRecordToAccessField,
                                tpe.exprs[Collection.RecordType.typeTag.rawValue],
                                expr)
      }
    }
    switch index {
      case .fixnum(let idx):
        guard idx >= 0 && idx < Int64(record.exprs.count) else {
          throw RuntimeError.range(parameter: 2,
                                   of: "record-set!",
                                   index,
                                   min: 0,
                                   max: Int64(record.exprs.count - 1))
        }
        // Set record field value. Guarantee that cells for which `record-set!` is called are
        // managed by a managed object pool.
        (value.isAtom ? record : self.context.objects.manage(record)).exprs[Int(idx)] = value
      case .null:
        guard value.isNull else {
          throw RuntimeError.eval(.fieldCountError, .makeNumber(0), value)
        }
        return expr
      case .pair(_, _):
        var allSimple = true
        var numFields = 0
        var currentIndex = index
        var currentValue = value
        while case .pair(.fixnum(let idx), let nextIndex) = currentIndex {
          guard idx >= 0 && idx < Int64(record.exprs.count) else {
            throw RuntimeError.range(parameter: 2,
                                     of: "record-set!",
                                     index,
                                     min: 0,
                                     max: Int64(record.exprs.count - 1))
          }
          guard case .pair(let v, let nextValue) = currentValue else {
            while case .pair(_, let nextIndex) = currentIndex {
              numFields += 1
              currentIndex = nextIndex
            }
            throw RuntimeError.eval(.fieldCountError, .makeNumber(numFields), value)
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
          throw RuntimeError.type(index, expected: [.properListType])
        }
        guard currentValue.isNull else {
          throw RuntimeError.eval(.fieldCountError, .makeNumber(numFields), value)
        }
        return expr
      default:
        throw RuntimeError.type(index, expected: [.fixnumType])
    }
    return .void
  }
}
