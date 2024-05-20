//
//  SQLiteLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 10/04/2020.
//  Copyright © 2020 ObjectHub. All rights reserved.
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
import SQLiteExpress

public final class SQLiteLibrary: NativeLibrary {
  
  // Type representations
  private let integerType: Symbol
  private let floatType: Symbol
  private let textType: Symbol
  private let blobType: Symbol
  private let nullType: Symbol
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    self.integerType = context.symbols.intern("sqlite-integer")
    self.floatType = context.symbols.intern("sqlite-float")
    self.textType = context.symbols.intern("sqlite-text")
    self.blobType = context.symbols.intern("sqlite-blob")
    self.nullType = context.symbols.intern("sqlite-null")
    try super.init(in: context)
  }

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "sqlite"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define("sqlite-database-type-tag", as: SQLiteDB.type.objectTypeTag())
    self.define("sqlite-statement-type-tag", as: SQLiteStmt.type.objectTypeTag())
    self.defineOpenOption("sqlite-readonly", as: .readOnly)
    self.defineOpenOption("sqlite-readwrite", as: .readWrite)
    self.defineOpenOption("sqlite-create", as: .create)
    self.defineOpenOption("sqlite-nomutex", as: .noMutex)
    self.defineOpenOption("sqlite-fullmutex", as: .fullMutex)
    self.defineOpenOption("sqlite-sharedcache", as: .sharedCache)
    self.defineOpenOption("sqlite-privatecache", as: .privateCache)
    self.defineOpenOption("sqlite-protect-complete", as: .protectionComplete)
    self.defineOpenOption("sqlite-protect-completeunlessopen", as: .protectionCompleteUnlessOpen)
    self.defineOpenOption("sqlite-protect-completeuntilfirstuserauth",
                          as: .protectionCompleteUntilFirstUserAuth)
    self.defineOpenOption("sqlite-protect-none", as: .protectionNone)
    self.defineOpenOption("sqlite-default", as: .default)
    self.define(Procedure("sqlite-version", sqliteVersion))
    self.define(Procedure("sqlite-version-number", sqliteVersionNumber))
    self.define(Procedure("sqlite-database?", sqliteDatabase))
    self.define(Procedure("make-database", makeDatabase))
    self.define(Procedure("open-database", openDatabase))
    self.define(Procedure("close-database", closeDatabase))
    self.define(Procedure("database-path", databasePath))
    self.define(Procedure("database-last-row-id", databaseLastRowId))
    self.define(Procedure("database-last-changes", databaseLastChanges))
    self.define(Procedure("database-total-changes", databaseTotalChanges))
    self.define(Procedure("sqlite-statement?", sqliteStatement))
    self.define(Procedure("prepare-statement", prepareStatement))
    self.define(Procedure("process-statement", processStatement))
    self.define(Procedure("reset-statement", resetStatement))
    self.define(Procedure("column-count", columnCount))
    self.define(Procedure("column-name", columnName))
    self.define(Procedure("column-type", columnType))
    self.define(Procedure("column-value", columnValue))
    self.define(Procedure("row-names", rowNames))
    self.define(Procedure("row-types", rowTypes))
    self.define(Procedure("row-values", rowValues))
    self.define(Procedure("row-alist", rowAlist))
    self.define(Procedure("parameter-count", parameterCount))
    self.define(Procedure("parameter-index", parameterIndex))
    self.define(Procedure("parameter-name", parameterName))
    self.define(Procedure("bind-parameter", bindParameter))
    self.define(Procedure("bind-parameters", bindParameters))
  }
  
  private func sqliteVersion() -> Expr {
    return .makeString(SQLiteDatabase.version ?? "\(SQLiteDatabase.versionNumber)")
  }
  
  private func sqliteVersionNumber() -> Expr {
    return .makeNumber(SQLiteDatabase.versionNumber)
  }
  
  private func sqliteDatabase(_ expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is SQLiteDB {
      return .true
    }
    return .false
  }
  
  private func makeDatabase(options: Expr?) throws -> Expr {
    let openOptions = try SQLiteDatabase.OpenOptions(rawValue:
                            Int32(truncatingIfNeeded: options?.asInt() ?? 6))
    return .object(SQLiteDB(try SQLiteDatabase(options: openOptions, extendedErrors: true)))
  }
  
  private func openDatabase(path: Expr, options: Expr?) throws -> Expr {
    let openOptions = try SQLiteDatabase.OpenOptions(rawValue:
                            Int32(truncatingIfNeeded: options?.asInt() ?? 6))
    return .object(SQLiteDB(try SQLiteDatabase(path: path.asPath(),
                                               options: openOptions,
                                               extendedErrors: true)))
  }
  
  private func closeDatabase(db: Expr) throws -> Expr {
    try self.database(from: db).db.close()
    return .void
  }
  
  private func databasePath(db: Expr) throws -> Expr {
    guard let path = try self.database(from: db).path else {
      return .false
    }
    return .makeString(path)
  }
  
  private func databaseLastRowId(db: Expr) throws -> Expr {
    return .fixnum(try self.database(from: db).db.lastRowId)
  }
  
  private func databaseLastChanges(db: Expr) throws -> Expr {
    return .makeNumber(try self.database(from: db).db.lastChanges)
  }
  
  private func databaseTotalChanges(db: Expr) throws -> Expr {
    return .makeNumber(try self.database(from: db).db.totalChanges)
  }
  
  private func sqliteStatement(_ expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is SQLiteStmt {
      return .true
    }
    return .false
  }
  
  private func prepareStatement(db: Expr, expr: Expr) throws -> Expr {
    return .object(SQLiteStmt(try self.database(from: db).db.prepare(sql: expr.asString())))
  }
  
  private func processStatement(stmt: Expr) throws -> Expr {
    return .makeBoolean(try self.statement(from: stmt).stmt.step())
  }
  
  private func resetStatement(stmt: Expr) throws -> Expr {
    try self.statement(from: stmt).stmt.reset()
    return .void
  }
  
  private func columnCount(stmt: Expr) throws -> Expr {
    return .makeNumber(try self.statement(from: stmt).stmt.columnCount)
  }
  
  private func columnName(stmt: Expr, idx: Expr) throws -> Expr {
    let stmt = try self.statement(from: stmt).stmt
    guard let name = try stmt.name(ofColumn: idx.asInt()) else {
      return .false
    }
    return .makeString(name)
  }
  
  private func columnTypeExpr(for stmt: SQLiteStatement, at i: Int) throws -> Expr {
    guard let type = try stmt.type(ofColumn: i) else {
      return .false
    }
    switch type {
      case .integer:
        return .symbol(self.integerType)
      case .float:
        return .symbol(self.floatType)
      case .text:
        return .symbol(self.textType)
      case .blob:
        return .symbol(self.blobType)
      case .null:
        return .symbol(self.nullType)
    }
  }
  
  private func columnType(stmt: Expr, idx: Expr) throws -> Expr {
    return try self.columnTypeExpr(for: self.statement(from: stmt).stmt, at: idx.asInt())
  }
  
  private func columnValueExpr(for stmt: SQLiteStatement, at i: Int) throws -> Expr {
    guard let type = try stmt.type(ofColumn: i) else {
       return .false
     }
     switch type {
       case .integer:
         return .fixnum(try stmt.int(atColumn: i))
       case .float:
         return .flonum(try stmt.float(atColumn: i))
       case .text:
         guard let str = try stmt.text(atColumn: i) else {
           return .false
         }
         return .makeString(str)
       case .blob:
         guard let data = try stmt.blob(atColumn: i) else {
           return .false
         }
         let count = data.count
         var res = [UInt8](repeating: 0, count: count)
         data.copyBytes(to: &res, count: count)
         return .bytes(MutableBox(res))
       case .null:
         return .null
     }
  }
  
  private func columnValue(stmt: Expr, idx: Expr) throws -> Expr {
    return try self.columnValueExpr(for: self.statement(from: stmt).stmt, at: idx.asInt())
  }
  
  private func rowNames(stmt: Expr) throws -> Expr {
    let stmt = try self.statement(from: stmt).stmt
    var i = stmt.columnCount
    var res = Expr.null
    while i > 0 {
      i -= 1
      if let name = try stmt.name(ofColumn: i) {
        res = .pair(.makeString(name), res)
      } else {
        res = .pair(.false, res)
      }
    }
    return res
  }
  
  private func rowTypes(stmt: Expr) throws -> Expr {
    let stmt = try self.statement(from: stmt).stmt
    var i = stmt.columnCount
    var res = Expr.null
    while i > 0 {
      i -= 1
      res = .pair(try self.columnTypeExpr(for: stmt, at: i), res)
    }
    return res
  }
  
  private func rowValues(stmt: Expr) throws -> Expr {
    let stmt = try self.statement(from: stmt).stmt
    var i = stmt.columnCount
    var res = Expr.null
    while i > 0 {
      i -= 1
      res = .pair(try self.columnValueExpr(for: stmt, at: i), res)
    }
    return res
  }
  
  private func rowAlist(stmt: Expr) throws -> Expr {
    let stmt = try self.statement(from: stmt).stmt
    var i = stmt.columnCount
    var res = Expr.null
    while i > 0 {
      i -= 1
      let name: Expr
      if let str = try stmt.name(ofColumn: i) {
        name = .makeString(str)
      } else {
        name = .makeNumber(i)
      }
      res = .pair(.pair(name, try self.columnValueExpr(for: stmt, at: i)), res)
    }
    return res
  }
  
  private func parameterCount(stmt: Expr) throws -> Expr {
    return .makeNumber(try self.statement(from: stmt).stmt.paramCount)
  }
  
  private func parameterIndex(stmt: Expr, name: Expr) throws -> Expr {
    return .makeNumber(try self.statement(from: stmt).stmt.paramIndex(name.asString()))
  }
  
  private func parameterName(stmt: Expr, idx: Expr) throws -> Expr {
    guard let name = try self.statement(from: stmt).stmt.paramName(idx.asInt()) else {
      return .false
    }
    return .makeString(name)
  }
  
  private func bind(_ expr: Expr, to i: Int, in stmt: SQLiteStatement) throws {
    switch expr {
      case .fixnum(let x):
        try stmt.bind(integer: x, toParam: i)
      case .flonum(let x):
        try stmt.bind(float: x, toParam: i)
      case .string(let str):
        try stmt.bind(text: str as String, toParam: i)
      case .bytes(let bvec):
        try stmt.bind(blob: Data(bvec.value), toParam: i)
      case .null:
        try stmt.bindNull(toParam: i)
      default:
        throw RuntimeError.eval(.unableToBindValue, expr, .fixnum(Int64(i)))
    }
  }
  
  private func bindParameter(stmt: Expr, idx: Expr, value: Expr) throws -> Expr {
    let stmt = try self.statement(from: stmt).stmt
    let i = try idx.asInt()
    try self.bind(value, to: i, in: stmt)
    return .void
  }
  
  private func bindParameters(stmt: Expr, values: Expr, idx: Expr?) throws -> Expr {
    let stmt = try self.statement(from: stmt).stmt
    var i = try idx?.asInt() ?? 1
    let max = stmt.paramCount
    var list = values
    while i > 0 && i <= max {
      guard case .pair(let value, let rest) = list else {
        break
      }
      try self.bind(value, to: i, in: stmt)
      list = rest
      i += 1
    }
    return list
  }
  
  private func defineOpenOption(_ name: String, as opt: SQLiteDatabase.OpenOptions) {
    self.define(name, as: .fixnum(Int64(opt.rawValue)), mutable: false, export: true)
  }
  
  private func database(from expr: Expr) throws -> SQLiteDB {
    guard case .object(let obj) = expr, let db = obj as? SQLiteDB else {
      throw RuntimeError.type(expr, expected: [SQLiteDB.type])
    }
    return db
  }
  
  private func statement(from expr: Expr) throws -> SQLiteStmt {
    guard case .object(let obj) = expr, let stmt = obj as? SQLiteStmt else {
      throw RuntimeError.type(expr, expected: [SQLiteStmt.type])
    }
    return stmt
  }
}

public final class SQLiteDB: NativeObject {

  /// Type representing SQLite3 databases
  public static let type = Type.objectType(Symbol(uninterned: "sqlite-database"))
  
  /// The SQLite3 database connection
  public let db: SQLiteDatabase
  
  /// File path where database gets persisted; this is `nil` for in-memory databases
  public let path: String?
  
  /// Initializer copying another drawing.
  public init(_ db: SQLiteDatabase, at path: String? = nil) {
    self.db = db
    self.path = path
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    let pathString = self.path == nil ? "" : " " + self.path!
    return "#<sqlite-database\(pathString) \(self.identityString)>"
  }
  
  public override func unpack(in context: Context) -> Exprs {
    return [.makeString(self.identityString),
            self.path == nil ? .false : .makeString(self.path!),
            .makeNumber(self.db.totalChanges)]
  }
}

public final class SQLiteStmt: NativeObject {
  
  /// Type representing SQLite3 statements
  public static let type = Type.objectType(Symbol(uninterned: "sqlite-statement"))
  
  /// The SQLite3 statements
  public let stmt: SQLiteStatement
  
  /// Initializer copying another drawing.
  public init(_ stmt: SQLiteStatement) {
    self.stmt = stmt
  }
  
  public override var type: Type {
    return Self.type
  }
}
