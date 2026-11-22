//
//  DataFrameLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/11/2026.
//  Copyright © 2026 ObjectHub. All rights reserved.
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
import TabularData

///
/// Library `(lispkit data-frame)` provides data frames, i.e. tables of typed columns,
/// based on Apple's TabularData framework. Data frames are immutable: all operations
/// return new data frames. Missing values are represented by `#f`.
///
public final class DataFrameLibrary: NativeLibrary {

  /// Column types supported by this library.
  fileprivate enum Kind: String, CaseIterable {
    case integer
    case double
    case boolean
    case string
    case date
    case data
    case array
    case object
    case other

    init(of column: AnyColumn) {
      let type = column.wrappedElementType
      if type == Int.self {
        self = .integer
      } else if type == Double.self {
        self = .double
      } else if type == Bool.self {
        self = .boolean
      } else if type == String.self {
        self = .string
      } else if type == Date.self {
        self = .date
      } else if type == Data.self {
        self = .data
      } else if type == [Any?].self {
        self = .array
      } else if type == [String : Any?].self {
        self = .object
      } else {
        self = .other
      }
    }

    var csvType: CSVType? {
      switch self {
        case .integer: return .integer
        case .double: return .double
        case .boolean: return .boolean
        case .string: return .string
        case .date: return .date
        case .data: return .data
        default: return nil
      }
    }

    var jsonType: JSONType? {
      switch self {
        case .integer: return .integer
        case .double: return .double
        case .boolean: return .boolean
        case .string: return .string
        case .date: return .date
        case .array: return .array
        case .object: return .object
        default: return nil
      }
    }
  }

  /// Aggregation operations
  fileprivate enum Aggregation: String {
    case count
    case sum
    case mean
    case min
    case max
    case median
    case std
    case variance
    case first
    case last
    case distinct
  }

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "data-frame"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"], "define", "lambda", "quote", "apply", "not", "and", "or")
    self.`import`(from: ["lispkit", "control"], "if", "let", "let*", "do")
    self.`import`(from: ["lispkit", "list"], "cons", "car", "pair?", "reverse", "map", "list")
    self.`import`(from: ["lispkit", "vector"], "vector-ref", "vector-length", "vector-map",
                                               "list->vector")
    self.`import`(from: ["lispkit", "math"], "fx1+", "fx<", "fx=")
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define("data-frame-type-tag", as: NativeDataFrame.type.objectTypeTag())
    self.define(Procedure("data-frame?", self.isDataFrame))
    // Creating data frames
    self.define(Procedure("make-data-frame", self.makeDataFrame))
    self.define(Procedure("rows->data-frame", self.rowsToDataFrame))
    // Reading and writing
    self.define(Procedure("csv->data-frame", self.csvToDataFrame))
    self.define(Procedure("load-csv-data-frame", self.loadCsvDataFrame))
    self.define(Procedure("data-frame->csv", self.dataFrameToCsv))
    self.define(Procedure("save-csv-data-frame", self.saveCsvDataFrame))
    self.define(Procedure("json->data-frame", self.jsonToDataFrame))
    self.define(Procedure("load-json-data-frame", self.loadJsonDataFrame))
    self.define(Procedure("data-frame->json", self.dataFrameToJson))
    self.define(Procedure("save-json-data-frame", self.saveJsonDataFrame))
    self.define(Procedure("data-frame->string", self.dataFrameToString))
    // Accessing data frames
    self.define(Procedure("data-frame-row-count", self.dataFrameRowCount))
    self.define(Procedure("data-frame-column-count", self.dataFrameColumnCount))
    self.define(Procedure("data-frame-column-names", self.dataFrameColumnNames))
    self.define(Procedure("data-frame-column-types", self.dataFrameColumnTypes))
    self.define(Procedure("data-frame-has-column?", self.dataFrameHasColumn))
    self.define(Procedure("data-frame-column", self.dataFrameColumn))
    self.define(Procedure("data-frame-ref", self.dataFrameRef))
    self.define(Procedure("data-frame-missing?", self.dataFrameMissing))
    self.define(Procedure("data-frame-missing-count", self.dataFrameMissingCount))
    self.define(Procedure("data-frame-distinct-values", self.dataFrameDistinctValues))
    self.define(Procedure("data-frame-row", self.dataFrameRow))
    self.define(Procedure("data-frame->rows", self.dataFrameToRows))
    self.define(Procedure("data-frame->columns", self.dataFrameToColumns))
    // Transforming data frames
    self.define(Procedure("data-frame-select-columns", self.dataFrameSelectColumns))
    self.define(Procedure("data-frame-drop-columns", self.dataFrameDropColumns))
    self.define(Procedure("data-frame-rename-column", self.dataFrameRenameColumn))
    self.define(Procedure("data-frame-set-column", self.dataFrameSetColumn))
    self.define(Procedure("data-frame-select-rows", self.dataFrameSelectRows))
    self.define(Procedure("data-frame-slice", self.dataFrameSlice))
    self.define(Procedure("data-frame-head", self.dataFrameHead))
    self.define(Procedure("data-frame-tail", self.dataFrameTail))
    self.define(Procedure("data-frame-sort", self.dataFrameSort))
    self.define(Procedure("data-frame-append", self.dataFrameAppend))
    self.define(Procedure("data-frame-append-rows", self.dataFrameAppendRows))
    self.define(Procedure("data-frame-join", self.dataFrameJoin))
    self.define(Procedure("data-frame-aggregate", self.dataFrameAggregate))
    self.define(Procedure("data-frame-summary", self.dataFrameSummary))
    self.define(Procedure("data-frame-random-split", self.dataFrameRandomSplit))
    // Procedures implemented in Scheme; they call procedures provided by the user
    self.define("data-frame-filter", via: """
      (define (data-frame-filter df pred . args)
        (let* ((names (if (pair? args) (car args) (data-frame-column-names df)))
               (cols (map (lambda (name) (data-frame-column df name)) names))
               (n (data-frame-row-count df)))
          (do ((i 0 (fx1+ i))
               (res '() (if (apply pred (map (lambda (col) (vector-ref col i)) cols))
                            (cons i res)
                            res)))
              ((fx= i n) (data-frame-select-rows df (reverse res))))))
    """)
    self.define("data-frame-for-each", via: """
      (define (data-frame-for-each df proc . args)
        (let* ((names (if (pair? args) (car args) (data-frame-column-names df)))
               (cols (map (lambda (name) (data-frame-column df name)) names))
               (n (data-frame-row-count df)))
          (do ((i 0 (fx1+ i)))
              ((fx= i n))
            (apply proc (map (lambda (col) (vector-ref col i)) cols)))))
    """)
    self.define("data-frame-update-column", via: """
      (define (data-frame-update-column df name proc . args)
        (apply data-frame-set-column df name (vector-map proc (data-frame-column df name)) args))
    """)
    self.define("data-frame-compute-column", via: """
      (define (data-frame-compute-column df name names proc . args)
        (let* ((cols (map (lambda (name) (data-frame-column df name)) names))
               (n (data-frame-row-count df)))
          (do ((i 0 (fx1+ i))
               (res '() (cons (apply proc (map (lambda (col) (vector-ref col i)) cols)) res)))
              ((fx= i n)
               (apply data-frame-set-column df name (list->vector (reverse res)) args)))))
    """)
  }

  // MARK: - Errors

  private func error(_ reason: String, _ expr: Expr) -> RuntimeError {
    return RuntimeError.custom("error", "\(reason): $0", [expr])
  }

  // MARK: - Conversions: Scheme to Swift

  private func dataFrame(_ expr: Expr) throws -> DataFrame {
    guard case .object(let obj) = expr, let df = obj as? NativeDataFrame else {
      throw RuntimeError.type(expr, expected: [NativeDataFrame.type])
    }
    return df.frame
  }

  private func symbol(_ str: String) -> Expr {
    return .symbol(self.context.symbols.intern(str))
  }

  private func name(_ expr: Expr) throws -> String {
    switch expr {
      case .symbol(let sym):
        return sym.identifier
      case .string(let str):
        return str as String
      default:
        throw RuntimeError.type(expr, expected: [.strType, .symbolType])
    }
  }

  /// Returns the names in `expr`, which is either a single name or a list of names.
  private func names(_ expr: Expr) throws -> [String] {
    switch expr {
      case .symbol(_), .string(_):
        return [try self.name(expr)]
      default:
        var res: [String] = []
        var list = expr
        while case .pair(let name, let rest) = list {
          res.append(try self.name(name))
          list = rest
        }
        guard list.isNull else {
          throw RuntimeError.type(expr, expected: [.properListType])
        }
        return res
    }
  }

  private func elements(_ expr: Expr) throws -> Exprs {
    switch expr {
      case .vector(let col):
        return col.exprs
      case .null, .pair(_, _):
        var res = Exprs()
        var list = expr
        while case .pair(let x, let rest) = list {
          res.append(x)
          list = rest
        }
        guard list.isNull else {
          throw RuntimeError.type(expr, expected: [.properListType])
        }
        return res
      default:
        throw RuntimeError.type(expr, expected: [.properListType, .vectorType])
    }
  }

  private func kind(_ expr: Expr) throws -> Kind {
    guard let kind = Kind(rawValue: try self.name(expr)), kind != .other else {
      throw self.error("unknown column type", expr)
    }
    return kind
  }

  private func path(_ expr: Expr) throws -> String {
    return self.context.fileHandler.path(try expr.asPath(),
                                         relativeTo: self.context.evaluator.currentDirectoryPath)
  }

  private func character(_ expr: Expr) throws -> Character {
    switch expr {
      case .char(let ch):
        guard let scalar = UnicodeScalar(ch) else {
          throw self.error("invalid character", expr)
        }
        return Character(scalar)
      case .string(let str):
        let s = str as String
        guard s.count == 1, let ch = s.first else {
          throw self.error("expected a single character", expr)
        }
        return ch
      default:
        throw RuntimeError.type(expr, expected: [.charType, .strType])
    }
  }

  private func strings(_ expr: Expr) throws -> [String] {
    if case .string(let str) = expr {
      return [str as String]
    }
    return try self.elements(expr).map { try $0.asString() }
  }

  /// Converts an association list of options into a dictionary.
  private func options(_ expr: Expr?) throws -> [String : Expr] {
    var res: [String : Expr] = [:]
    guard let expr, expr.isTrue else {
      return res
    }
    var list = expr
    while case .pair(let entry, let rest) = list {
      switch entry {
        case .pair(let key, let value):
          res[try self.name(key)] = value
        case .symbol(let sym):
          res[sym.identifier] = .true
        default:
          throw self.error("invalid option", entry)
      }
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.assocListType])
    }
    return res
  }

  private func checkOptions(_ options: [String : Expr], _ valid: Set<String>) throws {
    for key in options.keys where !valid.contains(key) {
      throw self.error("unknown option", self.symbol(key))
    }
  }

  private func dateFormatter(_ format: String) -> DateFormatter {
    let formatter = DateFormatter()
    formatter.locale = Locale(identifier: "en_US_POSIX")
    formatter.dateFormat = format
    return formatter
  }

  /// Converts a Scheme value into a Swift value of a column of the given kind.
  private func value(_ expr: Expr, kind: Kind) throws -> Any? {
    if expr.isFalse && kind != .boolean {
      return nil
    }
    switch kind {
      case .integer:
        guard case .fixnum(let num) = expr else {
          throw RuntimeError.type(expr, expected: [.exactIntegerType])
        }
        return Int(num)
      case .double:
        return try expr.asDouble(coerce: true)
      case .boolean:
        return expr.isTrue
      case .string:
        return try self.name(expr)
      case .date:
        guard case .object(let obj) = expr,
              let dt = obj as? NativeDateTime,
              let date = dt.value.date else {
          throw RuntimeError.type(expr, expected: [NativeDateTime.type])
        }
        return date
      case .data:
        return Data(try expr.asByteVector().value)
      case .array:
        return try self.elements(expr).map { try self.any($0) }
      case .object:
        var dict: [String : Any?] = [:]
        var list = expr
        while case .pair(.pair(let key, let value), let rest) = list {
          dict[try self.name(key)] = try self.any(value)
          list = rest
        }
        guard list.isNull else {
          throw RuntimeError.type(expr, expected: [.assocListType])
        }
        return dict
      case .other:
        throw self.error("unsupported column type", expr)
    }
  }

  /// Converts a Scheme value into a Swift value of an array or object column.
  private func any(_ expr: Expr) throws -> Any? {
    switch expr {
      case .false:
        return nil
      case .true:
        return true
      case .fixnum(let num):
        return Int(num)
      case .bignum(_), .rational(_, _), .flonum(_):
        return try expr.asDouble(coerce: true)
      case .string(_), .symbol(_):
        return try self.name(expr)
      case .bytes(let bv):
        return Data(bv.value)
      case .null:
        return [Any?]()
      case .pair(.pair(.symbol(_), _), _), .pair(.pair(.string(_), _), _):
        return try self.value(expr, kind: .object)
      case .pair(_, _), .vector(_):
        return try self.value(expr, kind: .array)
      case .object(let obj) where obj is NativeDateTime:
        return try self.value(expr, kind: .date)
      default:
        throw self.error("value cannot be stored in a data frame", expr)
    }
  }

  /// Determines the column type for the given Scheme values.
  private func inferKind(_ values: Exprs, _ name: String) throws -> Kind {
    var kind: Kind? = nil
    var booleans = true
    for value in values {
      let k: Kind
      switch value {
        case .false:
          continue
        case .true:
          k = .boolean
        case .fixnum(_):
          k = .integer
        case .bignum(_), .rational(_, _), .flonum(_):
          k = .double
        case .string(_), .symbol(_):
          k = .string
        case .bytes(_):
          k = .data
        case .null, .pair(_, _), .vector(_):
          k = .array
        case .object(let obj) where obj is NativeDateTime:
          k = .date
        default:
          throw self.error("value cannot be stored in a data frame", value)
      }
      if k != .boolean {
        booleans = false
      }
      if let current = kind, current != k {
        if (current == .integer && k == .double) || (current == .double && k == .integer) {
          kind = .double
        } else {
          throw self.error("values of column \(name) have incompatible types", value)
        }
      } else {
        kind = k
      }
    }
    if booleans {
      return .boolean
    }
    return kind ?? .boolean
  }

  /// Creates a column with the given name, kind, and Scheme values.
  private func column(_ name: String, _ kind: Kind, _ values: Exprs) throws -> AnyColumn {
    switch kind {
      case .integer:
        return Column<Int>(name: name, contents: try values.map {
          try self.value($0, kind: kind) as? Int }).eraseToAnyColumn()
      case .double:
        return Column<Double>(name: name, contents: try values.map {
          try self.value($0, kind: kind) as? Double }).eraseToAnyColumn()
      case .boolean:
        return Column<Bool>(name: name, contents: try values.map {
          try self.value($0, kind: kind) as? Bool }).eraseToAnyColumn()
      case .string:
        return Column<String>(name: name, contents: try values.map {
          try self.value($0, kind: kind) as? String }).eraseToAnyColumn()
      case .date:
        return Column<Date>(name: name, contents: try values.map {
          try self.value($0, kind: kind) as? Date }).eraseToAnyColumn()
      case .data:
        return Column<Data>(name: name, contents: try values.map {
          try self.value($0, kind: kind) as? Data }).eraseToAnyColumn()
      case .array:
        return Column<[Any?]>(name: name, contents: try values.map {
          try self.value($0, kind: kind) as? [Any?] }).eraseToAnyColumn()
      case .object:
        return Column<[String : Any?]>(name: name, contents: try values.map {
          try self.value($0, kind: kind) as? [String : Any?] }).eraseToAnyColumn()
      case .other:
        throw self.error("unsupported column type", self.symbol(kind.rawValue))
    }
  }

  /// Returns a dictionary mapping column names to kinds.
  private func types(_ expr: Expr?) throws -> [String : Kind] {
    var res: [String : Kind] = [:]
    guard let expr, expr.isTrue else {
      return res
    }
    var list = expr
    while case .pair(.pair(let name, let type), let rest) = list {
      res[try self.name(name)] = try self.kind(type)
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.assocListType])
    }
    return res
  }

  private func makeFrame(_ columns: [(String, Exprs)], _ types: [String : Kind]) throws -> DataFrame {
    var frame = DataFrame()
    var names = Set<String>()
    let count = columns.first?.1.count ?? 0
    for (name, values) in columns {
      guard !names.contains(name) else {
        throw self.error("duplicate column name", .makeString(name))
      }
      guard values.count == count else {
        throw self.error("columns differ in length", .makeString(name))
      }
      names.insert(name)
      let kind = try types[name] ?? self.inferKind(values, name)
      frame.append(column: try self.column(name, kind, values))
    }
    return frame
  }

  private func columnIndex(_ frame: DataFrame, _ expr: Expr) throws -> Int {
    let name = try self.name(expr)
    guard let index = frame.indexOfColumn(name) else {
      throw self.error("unknown column", expr)
    }
    return index
  }

  private func rowIndex(_ frame: DataFrame, _ expr: Expr) throws -> Int {
    return try expr.asInt(above: 0, below: frame.rows.count)
  }

  // MARK: - Conversions: Swift to Scheme

  /// Converts a value of a data frame into a Scheme value.
  private func expr(_ value: Any?) -> Expr {
    guard let value else {
      return .false
    }
    switch value {
      case let x as Int:
        return .makeNumber(x)
      case let x as Double:
        return .makeNumber(x)
      case let x as Bool:
        return .makeBoolean(x)
      case let x as String:
        return .makeString(x)
      case let x as Date:
        return .object(NativeDateTime(DateTimeLibrary.calendar.dateComponents(in: TimeZone.current,
                                                                              from: x)))
      case let x as Data:
        return .bytes(MutableBox([UInt8](x)))
      case let x as [Any?]:
        return .makeList(Exprs(x.map(self.expr)))
      case let x as [String : Any?]:
        var res = Exprs()
        for (k, v) in x.sorted(by: { $0.key < $1.key }) {
          res.append(.pair(self.symbol(k), self.expr(v)))
        }
        return .makeList(res)
      case let x as Int64:
        return .makeNumber(x)
      case let x as Int32:
        return .makeNumber(Int(x))
      default:
        // Nested optionals
        let mirror = Mirror(reflecting: value)
        if mirror.displayStyle == .optional {
          if let child = mirror.children.first {
            return self.expr(child.value)
          }
          return .false
        }
        return .makeString(String(describing: value))
    }
  }

  private func columnVector(_ column: AnyColumn) -> Expr {
    var exprs = Exprs()
    exprs.reserveCapacity(column.count)
    for i in 0..<column.count {
      exprs.append(self.expr(column[i]))
    }
    return .vector(Collection(kind: .vector, exprs: exprs))
  }

  private func row(_ frame: DataFrame, _ i: Int) -> Expr {
    return .makeList(Exprs(frame.columns.map { self.expr($0[i]) }))
  }

  private func wrap(_ frame: DataFrame) -> Expr {
    return .object(NativeDataFrame(frame))
  }

  // MARK: - Creating data frames

  private func isDataFrame(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is NativeDataFrame {
      return .true
    }
    return .false
  }

  private func makeDataFrame(columns: Expr, types: Expr?) throws -> Expr {
    var cols: [(String, Exprs)] = []
    var list = columns
    while case .pair(let entry, let rest) = list {
      guard case .pair(let name, let values) = entry else {
        throw RuntimeError.type(entry, expected: [.pairType])
      }
      cols.append((try self.name(name), try self.elements(values)))
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.type(columns, expected: [.assocListType])
    }
    return self.wrap(try self.makeFrame(cols, try self.types(types)))
  }

  private func rowsToDataFrame(names: Expr, rows: Expr, types: Expr?) throws -> Expr {
    let names = try self.names(names)
    var values = [Exprs](repeating: [], count: names.count)
    for row in try self.elements(rows) {
      let vals = try self.elements(row)
      guard vals.count == names.count else {
        throw self.error("row does not match the number of columns", row)
      }
      for (i, v) in vals.enumerated() {
        values[i].append(v)
      }
    }
    return self.wrap(try self.makeFrame(Array(zip(names, values)), try self.types(types)))
  }

  // MARK: - CSV

  private func csvReadingOptions(_ expr: Expr?) throws -> (CSVReadingOptions,
                                                            [String]?,
                                                            Range<Int>?,
                                                            [String : CSVType]) {
    let opts = try self.options(expr)
    try self.checkOptions(opts, ["header", "delimiter", "columns", "rows", "types", "missing",
                                 "true", "false", "quoting", "escaping", "escape",
                                 "ignore-empty-lines", "date-format"])
    // The delimiter and the escape character can only be set by the initializer
    var options = CSVReadingOptions(
      delimiter: try opts["delimiter"].map(self.character) ?? Character(","),
      escapeCharacter: try opts["escape"].map(self.character) ?? Character("\\"))
    if let x = opts["header"] {
      options.hasHeaderRow = x.isTrue
    }
    if let x = opts["missing"] {
      options.nilEncodings = Set(try self.strings(x))
    }
    if let x = opts["true"] {
      options.trueEncodings = Set(try self.strings(x))
    }
    if let x = opts["false"] {
      options.falseEncodings = Set(try self.strings(x))
    }
    if let x = opts["quoting"] {
      options.usesQuoting = x.isTrue
    }
    if let x = opts["escaping"] {
      options.usesEscaping = x.isTrue
    }
    if let x = opts["ignore-empty-lines"] {
      options.ignoresEmptyLines = x.isTrue
    }
    if let x = opts["date-format"] {
      for format in try self.strings(x) {
        let formatter = self.dateFormatter(format)
        options.dateParsers.append({ str in formatter.date(from: str) })
      }
    }
    var columns: [String]? = nil
    if let x = opts["columns"] {
      columns = try self.names(x)
    }
    var rows: Range<Int>? = nil
    if let x = opts["rows"] {
      guard case .pair(let s, let e) = x else {
        throw self.error("invalid row range", x)
      }
      let start = try s.asInt(above: 0)
      rows = start..<(try e.asInt(above: start))
    }
    var types: [String : CSVType] = [:]
    for (name, kind) in try self.types(opts["types"]) {
      guard let type = kind.csvType else {
        throw self.error("unsupported CSV column type", self.symbol(kind.rawValue))
      }
      types[name] = type
    }
    return (options, columns, rows, types)
  }

  private func csvWritingOptions(_ expr: Expr?) throws -> CSVWritingOptions {
    let opts = try self.options(expr)
    try self.checkOptions(opts, ["header", "delimiter", "missing", "true", "false", "newline",
                                 "date-format"])
    var options = CSVWritingOptions()
    if let x = opts["header"] {
      options.includesHeader = x.isTrue
    }
    if let x = opts["delimiter"] {
      options.delimiter = try self.character(x)
    }
    if let x = opts["missing"] {
      options.nilEncoding = try x.asString()
    }
    if let x = opts["true"] {
      options.trueEncoding = try x.asString()
    }
    if let x = opts["false"] {
      options.falseEncoding = try x.asString()
    }
    if let x = opts["newline"] {
      options.newline = try x.asString()
    }
    if let x = opts["date-format"] {
      let formatter = self.dateFormatter(try x.asString())
      options.dateFormatter = { date in formatter.string(from: date) }
    }
    return options
  }

  private func data(_ expr: Expr) throws -> Data {
    switch expr {
      case .string(let str):
        return Data((str as String).utf8)
      case .bytes(let bv):
        return Data(bv.value)
      default:
        throw RuntimeError.type(expr, expected: [.strType, .byteVectorType])
    }
  }

  private func guarded<T>(_ f: () throws -> T) throws -> T {
    do {
      return try f()
    } catch let error as RuntimeError {
      throw error
    } catch {
      throw RuntimeError.os(error)
    }
  }

  private func csvToDataFrame(expr: Expr, options: Expr?) throws -> Expr {
    let data = try self.data(expr)
    let (opts, columns, rows, types) = try self.csvReadingOptions(options)
    return self.wrap(try self.guarded {
      try DataFrame(csvData: data, columns: columns, rows: rows, types: types, options: opts)
    })
  }

  private func loadCsvDataFrame(path: Expr, options: Expr?) throws -> Expr {
    let url = URL(fileURLWithPath: try self.path(path))
    let (opts, columns, rows, types) = try self.csvReadingOptions(options)
    return self.wrap(try self.guarded {
      try DataFrame(contentsOfCSVFile: url, columns: columns, rows: rows, types: types, options: opts)
    })
  }

  private func dataFrameToCsv(expr: Expr, options: Expr?) throws -> Expr {
    let frame = try self.dataFrame(expr)
    let opts = try self.csvWritingOptions(options)
    let data = try self.guarded { try frame.csvRepresentation(options: opts) }
    return .makeString(String(decoding: data, as: UTF8.self))
  }

  private func saveCsvDataFrame(path: Expr, expr: Expr, options: Expr?) throws -> Expr {
    let url = URL(fileURLWithPath: try self.path(path))
    let frame = try self.dataFrame(expr)
    let opts = try self.csvWritingOptions(options)
    try self.guarded { try frame.writeCSV(to: url, options: opts) }
    return .void
  }

  // MARK: - JSON

  private func jsonReadingOptions(_ expr: Expr?) throws -> (JSONReadingOptions,
                                                             [String]?,
                                                             [String : JSONType]) {
    let opts = try self.options(expr)
    try self.checkOptions(opts, ["columns", "types", "date-format"])
    var options = JSONReadingOptions()
    if let x = opts["date-format"] {
      for format in try self.strings(x) {
        let formatter = self.dateFormatter(format)
        options.dateParsers.append({ str in formatter.date(from: str) })
      }
    }
    var columns: [String]? = nil
    if let x = opts["columns"] {
      columns = try self.names(x)
    }
    var types: [String : JSONType] = [:]
    for (name, kind) in try self.types(opts["types"]) {
      guard let type = kind.jsonType else {
        throw self.error("unsupported JSON column type", self.symbol(kind.rawValue))
      }
      types[name] = type
    }
    return (options, columns, types)
  }

  private func jsonWritingOptions(_ expr: Expr?) throws -> JSONWritingOptions {
    let opts = try self.options(expr)
    try self.checkOptions(opts, ["pretty", "sort-keys", "date-format"])
    var options = JSONWritingOptions()
    if let x = opts["pretty"] {
      options.prettyPrint = x.isTrue
    }
    if let x = opts["sort-keys"] {
      options.sortKeys = x.isTrue
    }
    if let x = opts["date-format"] {
      let formatter = self.dateFormatter(try x.asString())
      options.dateFormatter = { date in formatter.string(from: date) }
    }
    return options
  }

  private func jsonToDataFrame(expr: Expr, options: Expr?) throws -> Expr {
    let data = try self.data(expr)
    let (opts, columns, types) = try self.jsonReadingOptions(options)
    return self.wrap(try self.guarded {
      try DataFrame(jsonData: data, columns: columns, types: types, options: opts)
    })
  }

  private func loadJsonDataFrame(path: Expr, options: Expr?) throws -> Expr {
    let url = URL(fileURLWithPath: try self.path(path))
    let (opts, columns, types) = try self.jsonReadingOptions(options)
    return self.wrap(try self.guarded {
      try DataFrame(contentsOfJSONFile: url, columns: columns, types: types, options: opts)
    })
  }

  private func dataFrameToJson(expr: Expr, options: Expr?) throws -> Expr {
    let frame = try self.dataFrame(expr)
    let opts = try self.jsonWritingOptions(options)
    let data = try self.guarded { try frame.jsonRepresentation(options: opts) }
    return .makeString(String(decoding: data, as: UTF8.self))
  }

  private func saveJsonDataFrame(path: Expr, expr: Expr, options: Expr?) throws -> Expr {
    let url = URL(fileURLWithPath: try self.path(path))
    let frame = try self.dataFrame(expr)
    let opts = try self.jsonWritingOptions(options)
    try self.guarded { try frame.writeJSON(to: url, options: opts) }
    return .void
  }

  // MARK: - Formatting

  private func dataFrameToString(expr: Expr, options: Expr?) throws -> Expr {
    let frame = try self.dataFrame(expr)
    let opts = try self.options(options)
    try self.checkOptions(opts, ["max-rows", "max-width", "max-cell-width", "types",
                                 "row-indices", "counts"])
    var formatting = FormattingOptions(maximumLineWidth: 120)
    if let x = opts["max-rows"] {
      formatting.maximumRowCount = try x.asInt(above: 0)
    }
    if let x = opts["max-width"] {
      formatting.maximumLineWidth = try x.asInt(above: 1)
    }
    if let x = opts["max-cell-width"] {
      formatting.maximumCellWidth = try x.asInt(above: 1)
    }
    if let x = opts["types"] {
      formatting.includesColumnTypes = x.isTrue
    }
    if let x = opts["row-indices"] {
      formatting.includesRowIndices = x.isTrue
    }
    if let x = opts["counts"] {
      formatting.includesRowAndColumnCounts = x.isTrue
    }
    return .makeString(frame.description(options: formatting))
  }

  // MARK: - Accessing data frames

  private func dataFrameRowCount(expr: Expr) throws -> Expr {
    return .makeNumber(try self.dataFrame(expr).rows.count)
  }

  private func dataFrameColumnCount(expr: Expr) throws -> Expr {
    return .makeNumber(try self.dataFrame(expr).columns.count)
  }

  private func dataFrameColumnNames(expr: Expr) throws -> Expr {
    return .makeList(Exprs(try self.dataFrame(expr).columns.map { .makeString($0.name) }))
  }

  private func dataFrameColumnTypes(expr: Expr) throws -> Expr {
    return .makeList(Exprs(try self.dataFrame(expr).columns.map {
      .pair(.makeString($0.name), self.symbol(Kind(of: $0).rawValue))
    }))
  }

  private func dataFrameHasColumn(expr: Expr, name: Expr) throws -> Expr {
    return .makeBoolean(try self.dataFrame(expr).indexOfColumn(try self.name(name)) != nil)
  }

  private func dataFrameColumn(expr: Expr, name: Expr) throws -> Expr {
    let frame = try self.dataFrame(expr)
    return self.columnVector(frame.columns[try self.columnIndex(frame, name)])
  }

  private func dataFrameRef(expr: Expr, row: Expr, name: Expr) throws -> Expr {
    let frame = try self.dataFrame(expr)
    let column = frame.columns[try self.columnIndex(frame, name)]
    return self.expr(column[try self.rowIndex(frame, row)])
  }

  private func dataFrameMissing(expr: Expr, row: Expr, name: Expr) throws -> Expr {
    let frame = try self.dataFrame(expr)
    let column = frame.columns[try self.columnIndex(frame, name)]
    return .makeBoolean(column.isNil(at: try self.rowIndex(frame, row)))
  }

  private func dataFrameMissingCount(expr: Expr, name: Expr) throws -> Expr {
    let frame = try self.dataFrame(expr)
    return .makeNumber(frame.columns[try self.columnIndex(frame, name)].missingCount)
  }

  private func dataFrameDistinctValues(expr: Expr, name: Expr) throws -> Expr {
    let frame = try self.dataFrame(expr)
    let column = frame.columns[try self.columnIndex(frame, name)]
    var seen = Set<GroupKey>()
    var res = Exprs()
    for i in 0..<column.count where !column.isNil(at: i) {
      if seen.insert(GroupKey([column[i]])).inserted {
        res.append(self.expr(column[i]))
      }
    }
    return .makeList(res)
  }

  private func dataFrameRow(expr: Expr, row: Expr) throws -> Expr {
    let frame = try self.dataFrame(expr)
    return self.row(frame, try self.rowIndex(frame, row))
  }

  private func dataFrameToRows(expr: Expr) throws -> Expr {
    let frame = try self.dataFrame(expr)
    var res = Exprs()
    res.reserveCapacity(frame.rows.count)
    for i in 0..<frame.rows.count {
      res.append(self.row(frame, i))
    }
    return .makeList(res)
  }

  private func dataFrameToColumns(expr: Expr) throws -> Expr {
    let frame = try self.dataFrame(expr)
    return .makeList(Exprs(frame.columns.map { .pair(.makeString($0.name), self.columnVector($0)) }))
  }

  // MARK: - Transforming data frames

  private func dataFrameSelectColumns(expr: Expr, names: Expr) throws -> Expr {
    let frame = try self.dataFrame(expr)
    var columns: [AnyColumn] = []
    var seen = Set<String>()
    for name in try self.names(names) {
      guard let index = frame.indexOfColumn(name) else {
        throw self.error("unknown column", .makeString(name))
      }
      guard seen.insert(name).inserted else {
        throw self.error("duplicate column name", .makeString(name))
      }
      columns.append(frame.columns[index])
    }
    return self.wrap(DataFrame(columns: columns))
  }

  private func dataFrameDropColumns(expr: Expr, names: Expr) throws -> Expr {
    var frame = try self.dataFrame(expr)
    for name in try self.names(names) {
      guard frame.indexOfColumn(name) != nil else {
        throw self.error("unknown column", .makeString(name))
      }
      _ = frame.removeColumn(name)
    }
    return self.wrap(frame)
  }

  private func dataFrameRenameColumn(expr: Expr, old: Expr, new: Expr) throws -> Expr {
    let frame = try self.dataFrame(expr)
    let index = try self.columnIndex(frame, old)
    let newName = try self.name(new)
    if let other = frame.indexOfColumn(newName), other != index {
      throw self.error("duplicate column name", new)
    }
    var columns = frame.columns
    columns[index].name = newName
    return self.wrap(DataFrame(columns: columns))
  }

  private func dataFrameSetColumn(expr: Expr, name: Expr, values: Expr, type: Expr?) throws -> Expr {
    let frame = try self.dataFrame(expr)
    let columnName = try self.name(name)
    let vals = try self.elements(values)
    if !frame.columns.isEmpty && vals.count != frame.rows.count {
      throw self.error("number of values does not match the number of rows", values)
    }
    let kind = try (type == nil || type!.isFalse) ? self.inferKind(vals, columnName)
                                                   : self.kind(type!)
    let column = try self.column(columnName, kind, vals)
    var columns = frame.columns
    if let index = frame.indexOfColumn(columnName) {
      columns[index] = column
    } else {
      columns.append(column)
    }
    return self.wrap(DataFrame(columns: columns))
  }

  /// Returns a data frame consisting of the rows of `frame` at `indices`.
  private func rows(_ frame: DataFrame, _ indices: [Int]) -> DataFrame {
    var columns: [AnyColumn] = []
    for column in frame.columns {
      var col = column.prototype.makeColumn(capacity: indices.count)
      for i in indices {
        col.append(column[i])
      }
      columns.append(col)
    }
    return DataFrame(columns: columns)
  }

  private func dataFrameSelectRows(expr: Expr, indices: Expr) throws -> Expr {
    let frame = try self.dataFrame(expr)
    let rows = try self.elements(indices).map { try self.rowIndex(frame, $0) }
    return self.wrap(self.rows(frame, rows))
  }

  private func dataFrameSlice(expr: Expr, start: Expr, end: Expr?) throws -> Expr {
    let frame = try self.dataFrame(expr)
    let count = frame.rows.count
    let e = (end == nil || end!.isFalse) ? count : try end!.asInt(above: 0, below: count + 1)
    let s = try start.asInt(above: 0, below: e + 1)
    return self.wrap(DataFrame(frame[s..<e]))
  }

  private func dataFrameHead(expr: Expr, n: Expr?) throws -> Expr {
    let frame = try self.dataFrame(expr)
    return self.wrap(DataFrame(frame.prefix(try n?.asInt(above: 0) ?? 10)))
  }

  private func dataFrameTail(expr: Expr, n: Expr?) throws -> Expr {
    let frame = try self.dataFrame(expr)
    return self.wrap(DataFrame(frame.suffix(try n?.asInt(above: 0) ?? 10)))
  }

  /// Compares two values of a column. Returns `nil` if the values cannot be compared.
  private func compare(_ lhs: Any, _ rhs: Any) -> Int? {
    func cmp<T: Comparable>(_ x: T, _ y: T) -> Int {
      return x < y ? -1 : (x == y ? 0 : 1)
    }
    switch (lhs, rhs) {
      case (let x as Int, let y as Int):
        return cmp(x, y)
      case (let x as Double, let y as Double):
        return cmp(x, y)
      case (let x as String, let y as String):
        return cmp(x, y)
      case (let x as Date, let y as Date):
        return cmp(x, y)
      case (let x as Bool, let y as Bool):
        return cmp(x ? 1 : 0, y ? 1 : 0)
      default:
        return nil
    }
  }

  private func dataFrameSort(expr: Expr, keys: Expr) throws -> Expr {
    let frame = try self.dataFrame(expr)
    var sortKeys: [(AnyColumn, Bool)] = []
    let keyList: Exprs
    switch keys {
      case .symbol(_), .string(_), .pair(.symbol(_), .symbol(_)), .pair(.string(_), .symbol(_)):
        keyList = [keys]
      default:
        keyList = try self.elements(keys)
    }
    for key in keyList {
      switch key {
        case .pair(let name, .symbol(let order)):
          let ascending: Bool
          switch order.identifier {
            case "ascending":
              ascending = true
            case "descending":
              ascending = false
            default:
              throw self.error("invalid sort order", .symbol(order))
          }
          sortKeys.append((frame.columns[try self.columnIndex(frame, name)], ascending))
        default:
          sortKeys.append((frame.columns[try self.columnIndex(frame, key)], true))
      }
    }
    for (column, _) in sortKeys {
      switch Kind(of: column) {
        case .integer, .double, .string, .date, .boolean:
          break
        default:
          throw self.error("column cannot be sorted", .makeString(column.name))
      }
    }
    // Missing values are sorted last; the sort is stable
    let indices = (0..<frame.rows.count).sorted { i, j in
      for (column, ascending) in sortKeys {
        switch (column[i], column[j]) {
          case (nil, nil):
            continue
          case (nil, _):
            return false
          case (_, nil):
            return true
          case (.some(let x), .some(let y)):
            let c = self.compare(x, y) ?? 0
            if c != 0 {
              return ascending ? c < 0 : c > 0
            }
        }
      }
      return i < j
    }
    return self.wrap(self.rows(frame, indices))
  }

  /// Appends the rows of `other` to `frame`. Columns are matched by name.
  private func append(_ frame: DataFrame, _ other: DataFrame, _ expr: Expr) throws -> DataFrame {
    guard frame.columns.count == other.columns.count else {
      throw self.error("data frames have different columns", expr)
    }
    var columns: [AnyColumn] = []
    for column in frame.columns {
      guard let index = other.indexOfColumn(column.name) else {
        throw self.error("data frames have different columns", .makeString(column.name))
      }
      let otherColumn = other.columns[index]
      guard column.wrappedElementType == otherColumn.wrappedElementType else {
        throw self.error("columns have different types", .makeString(column.name))
      }
      var col = column
      col.append(contentsOf: otherColumn)
      columns.append(col)
    }
    return DataFrame(columns: columns)
  }

  private func dataFrameAppend(expr: Expr, args: Arguments) throws -> Expr {
    var frame = try self.dataFrame(expr)
    for arg in args {
      frame = try self.append(frame, try self.dataFrame(arg), arg)
    }
    return self.wrap(frame)
  }

  private func dataFrameAppendRows(expr: Expr, rows: Expr) throws -> Expr {
    let frame = try self.dataFrame(expr)
    var values = [Exprs](repeating: [], count: frame.columns.count)
    for row in try self.elements(rows) {
      let vals = try self.elements(row)
      guard vals.count == frame.columns.count else {
        throw self.error("row does not match the number of columns", row)
      }
      for (i, v) in vals.enumerated() {
        values[i].append(v)
      }
    }
    var columns: [AnyColumn] = []
    for (i, column) in frame.columns.enumerated() {
      let kind = Kind(of: column)
      guard kind != .other else {
        throw self.error("unsupported column type", .makeString(column.name))
      }
      var col = column
      col.append(contentsOf: try self.column(column.name, kind, values[i]))
      columns.append(col)
    }
    return self.wrap(DataFrame(columns: columns))
  }

  private func dataFrameJoin(expr: Expr, other: Expr, on: Expr, kind: Expr?) throws -> Expr {
    let left = try self.dataFrame(expr)
    let right = try self.dataFrame(other)
    let leftName: String
    let rightName: String
    if case .pair(let l, let r) = on {
      leftName = try self.name(l)
      rightName = try self.name(r)
    } else {
      leftName = try self.name(on)
      rightName = leftName
    }
    guard let li = left.indexOfColumn(leftName) else {
      throw self.error("unknown column", .makeString(leftName))
    }
    guard let ri = right.indexOfColumn(rightName) else {
      throw self.error("unknown column", .makeString(rightName))
    }
    guard left.columns[li].wrappedElementType == right.columns[ri].wrappedElementType else {
      throw self.error("join columns have different types", on)
    }
    switch Kind(of: left.columns[li]) {
      case .integer, .double, .string, .date, .boolean, .data:
        break
      default:
        throw self.error("unsupported join column type", on)
    }
    let joinKind: JoinKind
    switch try kind.map(self.name) ?? "inner" {
      case "inner":
        joinKind = .inner
      case "left":
        joinKind = .left
      case "right":
        joinKind = .right
      case "full":
        joinKind = .full
      default:
        throw self.error("invalid join kind", kind!)
    }
    return self.wrap(left.joined(right, on: (left: leftName, right: rightName), kind: joinKind))
  }

  private func number(_ value: Any?) -> Double? {
    switch value {
      case let x as Int:
        return Double(x)
      case let x as Double:
        return x
      default:
        return nil
    }
  }

  private func aggregate(_ op: Aggregation,
                         _ column: AnyColumn?,
                         _ rows: [Int],
                         _ name: String) throws -> Any? {
    guard let column else {
      return rows.count
    }
    let values = rows.compactMap { column[$0] }
    switch op {
      case .count:
        return values.count
      case .distinct:
        return Set(values.map { GroupKey([$0]) }).count
      case .first:
        return values.first
      case .last:
        return values.last
      case .min, .max:
        var res: Any? = nil
        for value in values {
          if let current = res {
            guard let c = self.compare(value, current) else {
              throw self.error("values cannot be compared", .makeString(name))
            }
            if (op == .min && c < 0) || (op == .max && c > 0) {
              res = value
            }
          } else {
            res = value
          }
        }
        return res
      case .sum:
        if Kind(of: column) == .integer {
          var sum = 0
          for case let x as Int in values {
            let (res, overflow) = sum.addingReportingOverflow(x)
            guard !overflow else {
              throw self.error("integer overflow in sum of column", .makeString(name))
            }
            sum = res
          }
          return sum
        }
        return try self.numbers(values, name).reduce(0.0, +)
      case .mean:
        let nums = try self.numbers(values, name)
        return nums.isEmpty ? nil : nums.reduce(0.0, +) / Double(nums.count)
      case .median:
        let nums = try self.numbers(values, name).sorted()
        guard !nums.isEmpty else {
          return nil
        }
        let mid = nums.count / 2
        return nums.count % 2 == 1 ? nums[mid] : (nums[mid - 1] + nums[mid]) / 2.0
      case .std, .variance:
        let nums = try self.numbers(values, name)
        guard nums.count > 1 else {
          return nil
        }
        let mean = nums.reduce(0.0, +) / Double(nums.count)
        let variance = nums.reduce(0.0) { $0 + ($1 - mean) * ($1 - mean) } / Double(nums.count - 1)
        return op == .std ? variance.squareRoot() : variance
    }
  }

  private func numbers(_ values: [Any], _ name: String) throws -> [Double] {
    return try values.map {
      guard let x = self.number($0) else {
        throw self.error("column is not numeric", .makeString(name))
      }
      return x
    }
  }

  private func dataFrameAggregate(expr: Expr, groups: Expr, aggregations: Expr?) throws -> Expr {
    let frame = try self.dataFrame(expr)
    let groupColumns = try self.names(groups).map { name in
      guard let index = frame.indexOfColumn(name) else {
        throw self.error("unknown column", .makeString(name))
      }
      return frame.columns[index]
    }
    // Parse aggregations of the form (name op) or (name op column)
    var specs: [(String, Aggregation, AnyColumn?)] = []
    if let aggregations, aggregations.isTrue {
      for spec in try self.elements(aggregations) {
        let parts = try self.elements(spec)
        guard parts.count == 2 || parts.count == 3 else {
          throw self.error("invalid aggregation", spec)
        }
        guard let op = Aggregation(rawValue: try self.name(parts[1])) else {
          throw self.error("unknown aggregation", parts[1])
        }
        var column: AnyColumn? = nil
        if parts.count == 3 {
          column = frame.columns[try self.columnIndex(frame, parts[2])]
        } else if op != .count {
          throw self.error("aggregation requires a column", spec)
        }
        specs.append((try self.name(parts[0]), op, column))
      }
    } else {
      specs.append(("count", .count, nil))
    }
    // Group rows in the order in which the groups first appear
    var keys: [GroupKey] = []
    var groupRows: [GroupKey : [Int]] = [:]
    for i in 0..<frame.rows.count {
      let key = GroupKey(groupColumns.map { $0[i] })
      if groupRows[key] == nil {
        keys.append(key)
        groupRows[key] = [i]
      } else {
        groupRows[key]!.append(i)
      }
    }
    var columns: [AnyColumn] = []
    var names = Set<String>()
    for column in groupColumns {
      var col = column.prototype.makeColumn(capacity: keys.count)
      for key in keys {
        col.append(column[groupRows[key]!.first!])
      }
      names.insert(column.name)
      columns.append(col)
    }
    for (name, op, column) in specs {
      guard names.insert(name).inserted else {
        throw self.error("duplicate column name", .makeString(name))
      }
      var values: [Any?] = []
      for key in keys {
        values.append(try self.aggregate(op, column, groupRows[key]!, column?.name ?? name))
      }
      switch op {
        case .count, .distinct:
          columns.append(Column<Int>(name: name, contents: values.map { $0 as? Int })
                           .eraseToAnyColumn())
        case .mean, .median, .std, .variance:
          columns.append(Column<Double>(name: name, contents: values.map { $0 as? Double })
                           .eraseToAnyColumn())
        case .sum where Kind(of: column!) == .integer:
          columns.append(Column<Int>(name: name, contents: values.map { $0 as? Int })
                           .eraseToAnyColumn())
        case .sum:
          columns.append(Column<Double>(name: name, contents: values.map { $0 as? Double })
                           .eraseToAnyColumn())
        case .min, .max, .first, .last:
          var col = column!.prototype.makeColumn(capacity: values.count)
          col.name = name
          for value in values {
            col.append(value)
          }
          columns.append(col)
      }
    }
    return self.wrap(DataFrame(columns: columns))
  }

  private func dataFrameSummary(expr: Expr, names: Expr?) throws -> Expr {
    var frame = try self.dataFrame(expr)
    if let names, names.isTrue {
      let selected = try self.names(names)
      for name in selected where frame.indexOfColumn(name) == nil {
        throw self.error("unknown column", .makeString(name))
      }
      frame = frame.selecting(columnNames: selected)
    }
    return self.wrap(frame.summary())
  }

  private func dataFrameRandomSplit(expr: Expr, proportion: Expr, seed: Expr?) throws -> Expr {
    let frame = try self.dataFrame(expr)
    let p = try proportion.asDouble(coerce: true)
    guard p >= 0.0 && p <= 1.0 else {
      throw self.error("proportion not between 0 and 1", proportion)
    }
    let s = (seed == nil || seed!.isFalse) ? nil : try seed!.asInt()
    let (first, second) = frame.randomSplit(by: p, seed: s)
    return .values(.pair(self.wrap(DataFrame(first)), .pair(self.wrap(DataFrame(second)), .null)))
  }
}

/// Hashable representation of a list of values of a data frame, used for grouping.
fileprivate struct GroupKey: Hashable {
  let values: [AnyHashable?]

  init(_ values: [Any?]) {
    self.values = values.map { value in
      guard let value else {
        return nil
      }
      if let hashable = value as? AnyHashable {
        return hashable
      }
      return AnyHashable(String(describing: value))
    }
  }
}

///
/// Data frames: tables of typed columns.
///
public final class NativeDataFrame: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "data-frame"))

  public let frame: DataFrame

  public init(_ frame: DataFrame) {
    self.frame = frame
  }

  public override var type: Type {
    return Self.type
  }

  public override var tagString: String {
    return "\(Self.type) \(self.identityString): \(self.frame.rows.count) rows, " +
           "\(self.frame.columns.count) columns"
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }

  public override var hash: Int {
    return self.frame.hashValue
  }

  public override func equals(_ obj: NativeObject) -> Bool {
    guard let other = obj as? NativeDataFrame else {
      return false
    }
    return self.frame == other.frame
  }
}
