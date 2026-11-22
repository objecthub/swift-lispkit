//
//  SpotlightSearchLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 22/11/2026.
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

#if canImport(CoreSpotlight)

import Foundation
import CoreSpotlight
import UniformTypeIdentifiers

///
/// Implements library `(lispkit system search)`, which provides access to Spotlight:
/// searching files via the Spotlight metadata index (`NSMetadataQuery`), searching and
/// maintaining the items that the application indexed itself (CoreSpotlight). Queries
/// are expressed in an s-expression based query language which gets compiled into
/// Spotlight query strings (or predicates on iOS). All searches and index updates are
/// asynchronous and return futures.
///
public final class SpotlightSearchLibrary: NativeLibrary {

  /// Default for the maximum number of search results. Without a limit, a search can easily
  /// return hundreds of thousands of results.
  public static let defaultLimit = 10_000

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "system", "search"]
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("spotlight-search", self.spotlightSearch))
    self.define(Procedure("spotlight-search-items", self.spotlightSearchItems))
    self.define(Procedure("spotlight-metadata", self.spotlightMetadata))
    self.define(Procedure("spotlight-query-string", self.spotlightQueryString))
    self.define(Procedure("spotlight-attributes", self.spotlightAttributes))
    self.define(Procedure("spotlight-attribute-key", self.spotlightAttributeKey))
    self.define(Procedure("spotlight-index-available?", self.spotlightIndexAvailable))
    self.define(Procedure("spotlight-index-item!", self.spotlightIndexItem))
    self.define(Procedure("spotlight-remove-items!", self.spotlightRemoveItems))
    self.define(Procedure("spotlight-remove-domains!", self.spotlightRemoveDomains))
    self.define(Procedure("spotlight-remove-all-items!", self.spotlightRemoveAllItems))
  }

  // MARK: - Helpers

  private func sym(_ str: String) -> Expr {
    return .symbol(self.context.symbols.intern(str))
  }

  private func invalid(_ reason: String, _ expr: Expr) -> RuntimeError {
    return RuntimeError.custom("error", "\(reason): $0", [expr])
  }

  private func list(_ expr: Expr) throws -> [Expr] {
    var res: [Expr] = []
    var lst = expr
    while case .pair(let head, let tail) = lst {
      res.append(head)
      lst = tail
    }
    guard lst.isNull else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return res
  }

  private func identifier(_ expr: Expr) throws -> String {
    switch expr {
      case .symbol(let sym):
        return sym.identifier
      case .string(let str):
        return str as String
      default:
        throw RuntimeError.type(expr, expected: [.symbolType, .strType])
    }
  }

  /// Returns the entries `(key . value)` of association list `expr`.
  private func options(_ expr: Expr?) throws -> [(String, Expr)] {
    guard let expr, expr.isTrue else {
      return []
    }
    return try self.list(expr).map { option in
      guard case .pair(let key, let value) = option else {
        throw RuntimeError.type(option, expected: [.pairType])
      }
      return (try self.identifier(key), value)
    }
  }

  private func source(from expr: Expr?) throws -> SpotlightSource {
    guard let expr, expr.isTrue else {
      return .files
    }
    switch try self.identifier(expr) {
      case "files":
        return .files
      case "items":
        return .items
      default:
        throw self.invalid("unknown Spotlight source", expr)
    }
  }

  private func date(from expr: Expr) throws -> Date {
    guard case .object(let obj) = expr,
          let dt = obj as? NativeDateTime,
          let date = dt.value.date else {
      throw RuntimeError.type(expr, expected: [NativeDateTime.type])
    }
    return date
  }

  // MARK: - Attributes

  /// Resolves the attribute `expr` (a symbol or a string with the raw key) into the key used
  /// in queries for `source`.
  private func attributeKey(_ expr: Expr, _ source: SpotlightSource) throws -> String {
    switch expr {
      case .symbol(let sym):
        guard let key = SpotlightSearchAttributes.key(for: sym.identifier, source) else {
          throw self.invalid("unknown Spotlight attribute", expr)
        }
        return key
      case .string(let str):
        return str as String
      default:
        throw RuntimeError.type(expr, expected: [.symbolType, .strType])
    }
  }

  private func spotlightAttributes(source: Expr?) throws -> Expr {
    let names = SpotlightSearchAttributes.names(try self.source(from: source))
    return .makeList(Exprs(names.map(self.sym)))
  }

  private func spotlightAttributeKey(attr: Expr, source: Expr?) throws -> Expr {
    return .makeString(try self.attributeKey(attr, try self.source(from: source)))
  }

  /// Parses the requested result attributes. Returns the requested attributes and their
  /// keys plus a flag determining whether a single value (`true`) or association lists
  /// (`false`) are returned per result.
  private func requested(_ expr: Expr?,
                         _ source: SpotlightSource) throws -> SpotlightProjection {
    let primary = source == .files ? "path" : "id"
    guard let expr, expr.isTrue else {
      return SpotlightProjection(attributes: [(self.sym(primary),
                                               try self.attributeKey(self.sym(primary), source))],
                                 single: true)
    }
    if case .symbol(_) = expr {
      return SpotlightProjection(attributes: [(expr, try self.attributeKey(expr, source))],
                                 single: true)
    }
    var attributes: [(Expr, String)] = []
    for attr in try self.list(expr) {
      attributes.append((attr, try self.attributeKey(attr, source)))
    }
    let primaryKey = try self.attributeKey(self.sym(primary), source)
    if !attributes.contains(where: { $0.1 == primaryKey }) {
      attributes.insert((self.sym(primary), primaryKey), at: 0)
    }
    return SpotlightProjection(attributes: attributes, single: false)
  }

  // MARK: - Query language

  private func query(from expr: Expr, _ source: SpotlightSource) throws -> SpotlightQuery {
    switch expr {
      case .string(let str):
        return try self.textQuery([str as String], expr)
      case .true:
        return .exists(try self.attributeKey(self.sym("content-type"), source))
      case .pair(.symbol(let sym), let rest):
        let args = try self.list(rest)
        func arity(_ n: Int, _ m: Int? = nil) throws {
          guard args.count >= n && args.count <= (m ?? n) else {
            throw self.invalid("wrong number of arguments in Spotlight query", expr)
          }
        }
        func key(_ i: Int) throws -> String {
          return try self.attributeKey(args[i], source)
        }
        switch sym.identifier {
          case "and", "or":
            try arity(1, Int.max)
            let conjunction = sym.identifier == "and"
            var qs: [SpotlightQuery] = []
            for arg in args {
              switch try self.query(from: arg, source) {
                case .and(let nested) where conjunction:
                  qs.append(contentsOf: nested)
                case .or(let nested) where !conjunction:
                  qs.append(contentsOf: nested)
                case let q:
                  qs.append(q)
              }
            }
            if qs.count == 1 {
              return qs[0]
            }
            return conjunction ? .and(qs) : .or(qs)
          case "not":
            try arity(1)
            return .not(try self.query(from: args[0], source))
          case "text":
            try arity(1, Int.max)
            return try self.textQuery(try args.map { try $0.asString() }, expr)
          case "name":
            try arity(1)
            return .match(try self.attributeKey(self.sym("name"), source),
                          .like, try args[0].asString(), SpotlightMatchFlags.insensitive)
          case "extension":
            try arity(1, Int.max)
            let name = try self.attributeKey(self.sym("name"), source)
            return self.disjunction(try args.map { ext in
              var str = try self.identifier(ext)
              if !str.hasPrefix(".") {
                str = "." + str
              }
              return .match(name, .suffix, str, SpotlightMatchFlags.insensitive)
            })
          case "type":
            try arity(1, Int.max)
            let tree = try self.attributeKey(self.sym("content-types"), source)
            return self.disjunction(try args.map { kind in
              return .compare(tree, .eq, .string(try self.contentType(kind)))
            })
          case "tag":
            try arity(1, Int.max)
            let tags = try self.attributeKey(self.sym("tags"), source)
            return self.disjunction(try args.map { tag in
              return .match(tags, .like, try tag.asString(), SpotlightMatchFlags.insensitive)
            })
          case "=", "!=", "<", "<=", ">", ">=":
            try arity(2)
            let op: SpotlightOperator
            switch sym.identifier {
              case "=":  op = .eq
              case "!=": op = .ne
              case "<":  op = .lt
              case "<=": op = .le
              case ">":  op = .gt
              default:   op = .ge
            }
            return .compare(try key(0), op, try self.value(from: args[1]))
          case "between":
            try arity(3)
            let k = try key(0)
            return .and([.compare(k, .ge, try self.value(from: args[1])),
                         .compare(k, .le, try self.value(from: args[2]))])
          case "in":
            try arity(2, Int.max)
            let k = try key(0)
            return self.disjunction(try args.dropFirst().map {
              .compare(k, .eq, try self.value(from: $0))
            })
          case "exists":
            try arity(1)
            return .exists(try key(0))
          case "like", "prefix", "suffix", "contains":
            try arity(2, 5)
            let kind: SpotlightMatchKind
            switch sym.identifier {
              case "like":   kind = .like
              case "prefix": kind = .prefix
              case "suffix": kind = .suffix
              default:       kind = .contains
            }
            var flags = SpotlightMatchFlags.insensitive
            for flag in args.dropFirst(2) {
              switch try self.identifier(flag) {
                case "case-sensitive":
                  flags.remove(.caseInsensitive)
                case "diacritic-sensitive":
                  flags.remove(.diacriticInsensitive)
                case "word":
                  flags.insert(.word)
                default:
                  throw self.invalid("unknown Spotlight match flag", flag)
              }
            }
            return .match(try key(0), kind, try args[1].asString(), flags)
          case "during":
            try arity(2)
            let k = try key(0)
            let (lower, upper) = try self.range(from: args[1])
            return .and([.compare(k, .ge, .date(lower)), .compare(k, .lt, .date(upper))])
          case "raw":
            try arity(1)
            return .raw(try args[0].asString())
          default:
            throw self.invalid("unknown Spotlight query operator", .symbol(sym))
        }
      default:
        throw self.invalid("invalid Spotlight query", expr)
    }
  }

  private func disjunction(_ qs: [SpotlightQuery]) -> SpotlightQuery {
    return qs.count == 1 ? qs[0] : .or(qs)
  }

  private func textQuery(_ strs: [String], _ expr: Expr) throws -> SpotlightQuery {
    var words: [SpotlightQuery] = []
    for str in strs {
      for word in str.split(whereSeparator: { $0.isWhitespace || $0.isNewline }) {
        words.append(.text(String(word)))
      }
    }
    guard !words.isEmpty else {
      throw self.invalid("Spotlight text query without words", expr)
    }
    return words.count == 1 ? words[0] : .and(words)
  }

  private func contentType(_ expr: Expr) throws -> String {
    switch expr {
      case .string(let str):
        return str as String
      case .symbol(let sym):
        guard let uti = SpotlightSearchAttributes.kinds[sym.identifier] else {
          throw self.invalid("unknown content kind", expr)
        }
        return uti
      default:
        throw RuntimeError.type(expr, expected: [.symbolType, .strType])
    }
  }

  private func value(from expr: Expr) throws -> SpotlightValue {
    let calendar = Calendar.current
    switch expr {
      case .string(let str):
        return .string(str as String)
      case .fixnum(let num):
        return .integer(num)
      case .flonum(let num):
        return .double(num)
      case .bignum(_), .rational(_, _):
        return .double(try expr.asDouble(coerce: true))
      case .true:
        return .boolean(true)
      case .false:
        return .boolean(false)
      case .object(_):
        return .date(try self.date(from: expr))
      case .symbol(let sym):
        let today = calendar.startOfDay(for: Date())
        switch sym.identifier {
          case "now":
            return .date(Date())
          case "today":
            return .date(today)
          case "yesterday":
            return .date(calendar.date(byAdding: .day, value: -1, to: today)!)
          case "tomorrow":
            return .date(calendar.date(byAdding: .day, value: 1, to: today)!)
          default:
            throw self.invalid("unknown Spotlight query value", expr)
        }
      case .pair(.symbol(let sym), .pair(let arg, .null)):
        let component: Calendar.Component
        switch sym.identifier {
          case "minutes-ago": component = .minute
          case "hours-ago":   component = .hour
          case "days-ago":    component = .day
          case "weeks-ago":   component = .weekOfYear
          case "months-ago":  component = .month
          case "years-ago":   component = .year
          default:
            throw self.invalid("unknown Spotlight query value", expr)
        }
        let n = try arg.asInt(above: Int.min, below: Int.max)
        return .date(calendar.date(byAdding: component, value: -n, to: Date())!)
      default:
        throw self.invalid("invalid Spotlight query value", expr)
    }
  }

  /// Returns a time interval `[lower, upper)` for a range specification.
  private func range(from expr: Expr) throws -> (Date, Date) {
    let calendar = Calendar.current
    let now = Date()
    func interval(_ component: Calendar.Component, _ offset: Int) -> (Date, Date) {
      let date = calendar.date(byAdding: component, value: offset, to: now)!
      let iv = calendar.dateInterval(of: component, for: date)!
      return (iv.start, iv.end)
    }
    switch expr {
      case .symbol(let sym):
        switch sym.identifier {
          case "today":      return interval(.day, 0)
          case "yesterday":  return interval(.day, -1)
          case "this-week":  return interval(.weekOfYear, 0)
          case "last-week":  return interval(.weekOfYear, -1)
          case "this-month": return interval(.month, 0)
          case "last-month": return interval(.month, -1)
          case "this-year":  return interval(.year, 0)
          case "last-year":  return interval(.year, -1)
          default:
            throw self.invalid("unknown Spotlight time range", expr)
        }
      case .pair(.symbol(let sym), .pair(let arg, .null)) where sym.identifier == "last-days":
        let n = try arg.asInt(above: 0, below: Int.max)
        return (calendar.date(byAdding: .day, value: -n, to: now)!, now)
      case .pair(let lower, .pair(let upper, .null)):
        guard case .date(let lo) = try self.value(from: lower),
              case .date(let hi) = try self.value(from: upper) else {
          throw self.invalid("Spotlight time range requires dates", expr)
        }
        return (lo, hi)
      default:
        throw self.invalid("invalid Spotlight time range", expr)
    }
  }

  private func spotlightQueryString(query: Expr, source: Expr?) throws -> Expr {
    let source = try self.source(from: source)
    return .makeString(try self.query(from: query, source).queryString)
  }

  // MARK: - File search

  private func spotlightSearch(query: Expr, attrs: Expr?, options: Expr?) throws -> Expr {
    let q = try self.query(from: query, .files)
    let projection = try self.requested(attrs, .files)
    var scopes: [Any] = []
    var sort: [(String, Bool)] = []
    var limit: Int? = Self.defaultLimit
    var timeout: Double? = nil
    for (key, value) in try self.options(options) {
      switch key {
        case "scope":
          scopes = try self.list(value).map(self.scope(from:))
        case "sort":
          sort = try self.sortSpecs(value, .files)
        case "limit":
          limit = value.isFalse ? nil : try value.asInt(above: 0, below: Int.max)
        case "timeout":
          timeout = value.isFalse ? nil : try value.asDouble(coerce: true)
        default:
          throw self.invalid("unknown Spotlight search option", self.sym(key))
      }
    }
    if scopes.isEmpty {
      #if os(macOS)
      scopes = [NSMetadataQueryLocalComputerScope]
      #else
      scopes = [NSMetadataQueryUbiquitousDocumentsScope]
      #endif
    }
    #if os(macOS)
    guard let predicate = NSPredicate(fromMetadataQueryString: q.queryString) else {
      throw self.invalid("invalid Spotlight query", query)
    }
    #else
    let predicate = try q.predicate()
    #endif
    let future = Future(external: false)
    let search = SpotlightFileSearch(predicate: predicate,
                                     scopes: scopes,
                                     sort: sort,
                                     limit: limit,
                                     timeout: timeout,
                                     future: future,
                                     context: self.context) { items in
      return .makeList(Exprs(items.map { item in
        projection.result { key in item.value(forAttribute: key) }
      }))
    }
    search.start()
    return .object(future)
  }

  private func scope(from expr: Expr) throws -> Any {
    switch expr {
      case .symbol(let sym):
        switch sym.identifier {
          #if os(macOS)
          case "computer":
            return NSMetadataQueryLocalComputerScope
          case "indexed-computer":
            return NSMetadataQueryIndexedLocalComputerScope
          case "home":
            return NSMetadataQueryUserHomeScope
          case "network":
            return NSMetadataQueryNetworkScope
          case "indexed-network":
            return NSMetadataQueryIndexedNetworkScope
          #endif
          case "icloud-documents":
            return NSMetadataQueryUbiquitousDocumentsScope
          case "icloud-data":
            return NSMetadataQueryUbiquitousDataScope
          case "icloud-external":
            return NSMetadataQueryAccessibleUbiquitousExternalDocumentsScope
          default:
            throw self.invalid("unknown or unsupported Spotlight search scope", expr)
        }
      case .string(_):
        let path = self.context.fileHandler.path(try expr.asPath(),
                                                 relativeTo: self.context.evaluator.currentDirectoryPath)
        return URL(fileURLWithPath: path, isDirectory: true)
      default:
        throw RuntimeError.type(expr, expected: [.symbolType, .strType])
    }
  }

  private func sortSpecs(_ expr: Expr, _ source: SpotlightSource) throws -> [(String, Bool)] {
    return try self.list(expr).map { spec in
      if case .pair(let attr, .pair(let order, .null)) = spec {
        switch try self.identifier(order) {
          case "ascending":
            return (try self.attributeKey(attr, source), true)
          case "descending":
            return (try self.attributeKey(attr, source), false)
          default:
            throw self.invalid("unknown sort order", order)
        }
      }
      return (try self.attributeKey(spec, source), true)
    }
  }

  // MARK: - File metadata

  private func spotlightMetadata(path: Expr, attrs: Expr?) throws -> Expr {
    #if os(macOS)
    let url = URL(fileURLWithPath:
                    self.context.fileHandler.path(try path.asPath(),
                                                  relativeTo: self.context.evaluator.currentDirectoryPath))
    guard let item = NSMetadataItem(url: url) else {
      return .false
    }
    if let attrs, attrs.isTrue {
      return try self.requested(attrs, .files).result { key in item.value(forAttribute: key) }
    }
    var entries = Exprs()
    for key in item.attributes.sorted() where !key.hasPrefix("_") {
      if let value = item.value(forAttribute: key),
         let expr = SpotlightValues.expr(from: value) {
        let name: Expr = SpotlightSearchAttributes.name(for: key, .files).map(self.sym)
                         ?? .makeString(key)
        entries.append(.pair(name, expr))
      }
    }
    return .makeList(entries)
    #else
    throw RuntimeError.custom("error", "spotlight-metadata is not supported on this platform", [])
    #endif
  }

  // MARK: - Item search

  private func spotlightSearchItems(query: Expr, attrs: Expr?, options: Expr?) throws -> Expr {
    let q = try self.query(from: query, .items)
    let projection = try self.requested(attrs, .items)
    var sort: [(String, Bool)] = []
    var limit: Int? = Self.defaultLimit
    var timeout: Double? = nil
    for (key, value) in try self.options(options) {
      switch key {
        case "sort":
          sort = try self.sortSpecs(value, .items)
        case "limit":
          limit = value.isFalse ? nil : try value.asInt(above: 0, below: Int.max)
        case "timeout":
          timeout = value.isFalse ? nil : try value.asDouble(coerce: true)
        default:
          throw self.invalid("unknown Spotlight search option", self.sym(key))
      }
    }
    var fetch = Set<String>()
    for key in projection.attributes.map({ $0.1 }) + sort.map({ $0.0 }) {
      if key != SpotlightSearchAttributes.itemIdentifier &&
          key != SpotlightSearchAttributes.itemDomain {
        fetch.insert(key)
      }
    }
    let context = CSSearchQueryContext()
    context.fetchAttributes = Array(fetch)
    let future = Future(external: false)
    let search = SpotlightItemSearch(query: CSSearchQuery(queryString: q.queryString,
                                                          queryContext: context),
                                     sort: sort,
                                     limit: limit,
                                     timeout: timeout,
                                     future: future,
                                     context: self.context) { items in
      return .makeList(Exprs(items.map { item in
        projection.result { key in SpotlightSearchAttributes.value(of: key, in: item) }
      }))
    }
    search.start()
    return .object(future)
  }

  // MARK: - Indexing

  private func spotlightIndexAvailable() -> Expr {
    return .makeBoolean(CSSearchableIndex.isIndexingAvailable())
  }

  private func spotlightIndexItem(id: Expr,
                                  attrs: Expr,
                                  domain: Expr?,
                                  expiration: Expr?) throws -> Expr {
    var entries: [(Expr, Expr)] = []
    for entry in try self.list(attrs) {
      guard case .pair(let key, let value) = entry else {
        throw RuntimeError.type(entry, expected: [.pairType])
      }
      entries.append((key, value))
    }
    var contentType = UTType.content
    for (key, value) in entries {
      if case .symbol(let sym) = key, sym.identifier == "content-type" {
        let str = try value.asString()
        guard let type = UTType(str) ?? UTType(filenameExtension: str) else {
          throw self.invalid("unknown content type", value)
        }
        contentType = type
      }
    }
    let set = CSSearchableItemAttributeSet(contentType: contentType)
    for (key, value) in entries {
      switch key {
        case .symbol(let sym):
          guard sym.identifier != "content-type" else {
            continue
          }
          guard let attribute = SpotlightSearchAttributes.item(for: sym.identifier) else {
            throw self.invalid("unknown Spotlight attribute", key)
          }
          guard set.responds(to: NSSelectorFromString(attribute.setter)) else {
            throw self.invalid("Spotlight attribute not supported on this system", key)
          }
          set.setValue(try self.object(from: value, attribute.type), forKey: attribute.property)
        case .string(let name):
          guard let customKey = CSCustomAttributeKey(keyName: name as String) else {
            throw self.invalid("invalid custom Spotlight attribute", key)
          }
          guard let obj = try self.object(from: value, nil) as? (NSSecureCoding & NSObjectProtocol) else {
            throw self.invalid("unsupported value for custom Spotlight attribute", value)
          }
          set.setValue(obj, forCustomKey: customKey)
        default:
          throw RuntimeError.type(key, expected: [.symbolType, .strType])
      }
    }
    let item = CSSearchableItem(uniqueIdentifier: try id.asString(),
                                domainIdentifier: (domain?.isTrue ?? false) ? try domain!.asString()
                                                                             : nil,
                                attributeSet: set)
    if let expiration, expiration.isTrue {
      if case .symbol(let sym) = expiration, sym.identifier == "never" {
        item.expirationDate = .distantFuture
      } else {
        item.expirationDate = try self.date(from: expiration)
      }
    }
    let future = Future(external: false)
    let context = self.context
    CSSearchableIndex.default().indexSearchableItems([item]) { error in
      SpotlightIndexing.complete(future, error, in: context)
    }
    return .object(future)
  }

  private func spotlightRemoveItems(_ args: Arguments) throws -> Expr {
    let ids = try args.map { try $0.asString() }
    let future = Future(external: false)
    let context = self.context
    CSSearchableIndex.default().deleteSearchableItems(withIdentifiers: ids) { error in
      SpotlightIndexing.complete(future, error, in: context)
    }
    return .object(future)
  }

  private func spotlightRemoveDomains(_ args: Arguments) throws -> Expr {
    let domains = try args.map { try $0.asString() }
    let future = Future(external: false)
    let context = self.context
    CSSearchableIndex.default().deleteSearchableItems(withDomainIdentifiers: domains) { error in
      SpotlightIndexing.complete(future, error, in: context)
    }
    return .object(future)
  }

  private func spotlightRemoveAllItems() -> Expr {
    let future = Future(external: false)
    let context = self.context
    CSSearchableIndex.default().deleteAllSearchableItems { error in
      SpotlightIndexing.complete(future, error, in: context)
    }
    return .object(future)
  }

  /// Converts `expr` into an object suitable for an attribute of type `type`. If `type` is
  /// `nil`, the type is derived from `expr`.
  private func object(from expr: Expr, _ type: SpotlightValueType?) throws -> Any {
    switch type {
      case .string:
        return try expr.asString()
      case .strings:
        if case .string(let str) = expr {
          return [str as String]
        }
        return try self.list(expr).map { try $0.asString() }
      case .number:
        return try self.number(from: expr)
      case .date:
        return try self.date(from: expr)
      case .dates:
        return try self.list(expr).map(self.date(from:))
      case .url:
        let str = try expr.asString()
        if str.hasPrefix("/") || str.hasPrefix("~") {
          return URL(fileURLWithPath: self.context.fileHandler.path(str,
                       relativeTo: self.context.evaluator.currentDirectoryPath))
        }
        guard let url = URL(string: str) else {
          throw self.invalid("invalid URL", expr)
        }
        return url
      case .data:
        guard case .bytes(let bv) = expr else {
          throw RuntimeError.type(expr, expected: [.byteVectorType])
        }
        return Data(bv.value)
      case .persons:
        return try self.list(expr).map { person in
          switch person {
            case .string(let name):
              return CSPerson(displayName: name as String, handles: [],
                              handleIdentifier: "emailAddresses")
            case .pair(let name, let handles):
              return CSPerson(displayName: try name.asString(),
                              handles: try self.list(handles).map { try $0.asString() },
                              handleIdentifier: "emailAddresses")
            default:
              throw RuntimeError.type(person, expected: [.strType, .pairType])
          }
        }
      case .none:
        switch expr {
          case .string(let str):
            return str as String
          case .fixnum(_), .bignum(_), .rational(_, _), .flonum(_), .true, .false:
            return try self.number(from: expr)
          case .object(_):
            return try self.date(from: expr)
          case .pair(_, _), .null:
            return try self.list(expr).map { try self.object(from: $0, nil) }
          default:
            throw self.invalid("unsupported Spotlight attribute value", expr)
        }
    }
  }

  private func number(from expr: Expr) throws -> NSNumber {
    switch expr {
      case .true:
        return NSNumber(value: true)
      case .false:
        return NSNumber(value: false)
      case .fixnum(let num):
        return NSNumber(value: num)
      default:
        return NSNumber(value: try expr.asDouble(coerce: true))
    }
  }
}

// MARK: - Query representation

enum SpotlightSource {
  case files
  case items
}

enum SpotlightValueType {
  case string
  case strings
  case number
  case date
  case dates
  case url
  case data
  case persons
}

enum SpotlightOperator: String {
  case eq = "=="
  case ne = "!="
  case lt = "<"
  case le = "<="
  case gt = ">"
  case ge = ">="

  var predicateType: NSComparisonPredicate.Operator {
    switch self {
      case .eq: return .equalTo
      case .ne: return .notEqualTo
      case .lt: return .lessThan
      case .le: return .lessThanOrEqualTo
      case .gt: return .greaterThan
      case .ge: return .greaterThanOrEqualTo
    }
  }
}

enum SpotlightMatchKind {
  case like
  case prefix
  case suffix
  case contains
}

struct SpotlightMatchFlags: OptionSet {
  let rawValue: Int
  static let caseInsensitive = SpotlightMatchFlags(rawValue: 1)
  static let diacriticInsensitive = SpotlightMatchFlags(rawValue: 2)
  static let word = SpotlightMatchFlags(rawValue: 4)
  static let insensitive: SpotlightMatchFlags = [.caseInsensitive, .diacriticInsensitive]

  var modifiers: String {
    return (self.contains(.caseInsensitive) ? "c" : "") +
           (self.contains(.diacriticInsensitive) ? "d" : "") +
           (self.contains(.word) ? "w" : "")
  }

  var predicateOptions: NSComparisonPredicate.Options {
    var res: NSComparisonPredicate.Options = []
    if self.contains(.caseInsensitive) {
      res.insert(.caseInsensitive)
    }
    if self.contains(.diacriticInsensitive) {
      res.insert(.diacriticInsensitive)
    }
    return res
  }
}

enum SpotlightValue {
  case string(String)
  case integer(Int64)
  case double(Double)
  case boolean(Bool)
  case date(Date)

  private static let isoFormatter: ISO8601DateFormatter = {
    let formatter = ISO8601DateFormatter()
    formatter.formatOptions = [.withInternetDateTime]
    formatter.timeZone = TimeZone(identifier: "UTC")
    return formatter
  }()

  var queryString: String {
    switch self {
      case .string(let str):
        return "\"" + SpotlightQuery.escape(str, wildcards: true) + "\""
      case .integer(let num):
        return String(num)
      case .double(let num):
        return String(num)
      case .boolean(let bool):
        return bool ? "1" : "0"
      case .date(let date):
        return "$time.iso(\(Self.isoFormatter.string(from: date)))"
    }
  }

  var object: Any {
    switch self {
      case .string(let str):
        return str
      case .integer(let num):
        return NSNumber(value: num)
      case .double(let num):
        return NSNumber(value: num)
      case .boolean(let bool):
        return NSNumber(value: bool)
      case .date(let date):
        return date
    }
  }
}

indirect enum SpotlightQuery {
  case and([SpotlightQuery])
  case or([SpotlightQuery])
  case not(SpotlightQuery)
  case compare(String, SpotlightOperator, SpotlightValue)
  case match(String, SpotlightMatchKind, String, SpotlightMatchFlags)
  case exists(String)
  case text(String)
  case raw(String)

  /// Escapes `"` and `\` in `str`. If `wildcards` is set, `*` gets escaped too.
  static func escape(_ str: String, wildcards: Bool) -> String {
    var res = ""
    for ch in str {
      switch ch {
        case "\"", "\\":
          res.append("\\")
        case "*" where wildcards:
          res.append("\\")
        default:
          break
      }
      res.append(ch)
    }
    return res
  }

  /// The Spotlight query string representing this query.
  var queryString: String {
    switch self {
      case .and(let qs):
        return "(" + qs.map(\.queryString).joined(separator: " && ") + ")"
      case .or(let qs):
        return "(" + qs.map(\.queryString).joined(separator: " || ") + ")"
      case .not(let q):
        return "!" + q.parenthesizedQueryString
      case .compare(let key, let op, let value):
        return "\(key) \(op.rawValue) \(value.queryString)"
      case .match(let key, let kind, let str, let flags):
        let pattern: String
        switch kind {
          case .like:
            pattern = SpotlightQuery.escape(str, wildcards: false)
          case .prefix:
            pattern = SpotlightQuery.escape(str, wildcards: true) + "*"
          case .suffix:
            pattern = "*" + SpotlightQuery.escape(str, wildcards: true)
          case .contains:
            pattern = "*" + SpotlightQuery.escape(str, wildcards: true) + "*"
        }
        return "\(key) == \"\(pattern)\"\(flags.modifiers)"
      case .exists(let key):
        return "\(key) == *"
      case .text(let word):
        return "** == \"\(SpotlightQuery.escape(word, wildcards: true))*\"cdw"
      case .raw(let str):
        return "(" + str + ")"
    }
  }

  private var parenthesizedQueryString: String {
    switch self {
      case .and(_), .or(_), .raw(_):
        return self.queryString
      default:
        return "(" + self.queryString + ")"
    }
  }

  /// An `NSPredicate` representing this query; used on platforms that do not support
  /// Spotlight query strings for `NSMetadataQuery`.
  func predicate() throws -> NSPredicate {
    switch self {
      case .and(let qs):
        return NSCompoundPredicate(andPredicateWithSubpredicates: try qs.map { try $0.predicate() })
      case .or(let qs):
        return NSCompoundPredicate(orPredicateWithSubpredicates: try qs.map { try $0.predicate() })
      case .not(let q):
        return NSCompoundPredicate(notPredicateWithSubpredicate: try q.predicate())
      case .compare(let key, let op, let value):
        return NSComparisonPredicate(leftExpression: NSExpression(forKeyPath: key),
                                     rightExpression: NSExpression(forConstantValue: value.object),
                                     modifier: .direct,
                                     type: op.predicateType,
                                     options: [])
      case .match(let key, let kind, let str, let flags):
        let type: NSComparisonPredicate.Operator
        var pattern = str
        switch kind {
          case .like:
            type = .like
            pattern = str.replacingOccurrences(of: "?", with: "\\?")
          case .prefix:
            type = .beginsWith
          case .suffix:
            type = .endsWith
          case .contains:
            type = .contains
        }
        return NSComparisonPredicate(leftExpression: NSExpression(forKeyPath: key),
                                     rightExpression: NSExpression(forConstantValue: pattern),
                                     modifier: .direct,
                                     type: type,
                                     options: flags.predicateOptions)
      case .exists(let key):
        return NSComparisonPredicate(leftExpression: NSExpression(forKeyPath: key),
                                     rightExpression: NSExpression(forConstantValue: nil),
                                     modifier: .direct,
                                     type: .notEqualTo,
                                     options: [])
      case .text(let word):
        return try SpotlightQuery.or([
          .match("kMDItemFSName", .contains, word, .insensitive),
          .match("kMDItemTextContent", .contains, word, .insensitive)
        ]).predicate()
      case .raw(_):
        throw RuntimeError.custom("error", "raw Spotlight queries are not supported on this platform", [])
    }
  }
}

/// Determines which attributes are returned per search result and how.
struct SpotlightProjection {

  /// The requested attributes (as given by the user) with their keys.
  let attributes: [(Expr, String)]

  /// If set, there is exactly one attribute and its value is returned directly.
  let single: Bool

  func result(_ value: (String) -> Any?) -> Expr {
    if self.single {
      return value(self.attributes[0].1).flatMap(SpotlightValues.expr(from:)) ?? .false
    }
    var entries = Exprs()
    for (attr, key) in self.attributes {
      if let val = value(key), let expr = SpotlightValues.expr(from: val) {
        entries.append(.pair(attr, expr))
      }
    }
    return .makeList(entries)
  }
}

// MARK: - Value conversion

enum SpotlightValues {

  /// Converts a value of a Spotlight attribute into an expression.
  static func expr(from value: Any) -> Expr? {
    switch value {
      case let str as String:
        return .makeString(str)
      case let num as NSNumber:
        if CFGetTypeID(num) == CFBooleanGetTypeID() {
          return .makeBoolean(num.boolValue)
        } else if CFNumberIsFloatType(num) {
          return .flonum(num.doubleValue)
        } else {
          return .makeNumber(num.int64Value)
        }
      case let date as Date:
        return .object(NativeDateTime(DateTimeLibrary.calendar.dateComponents(in: .current,
                                                                             from: date)))
      case let url as URL:
        return .makeString(url.isFileURL ? url.path : url.absoluteString)
      case let data as Data:
        return .bytes(MutableBox([UInt8](data)))
      case let person as CSPerson:
        return person.displayName.map { .makeString($0) }
                 ?? person.handles.first.map { .makeString($0) }
      case let array as [Any]:
        return .makeList(Exprs(array.compactMap(Self.expr(from:))))
      default:
        return .makeString(String(describing: value))
    }
  }

  /// Compares two attribute values for sorting; values that are missing come last.
  static func ordered(_ lhs: Any?, _ rhs: Any?, ascending: Bool) -> Bool? {
    guard let lhs else {
      return rhs == nil ? nil : false
    }
    guard let rhs else {
      return true
    }
    let res: ComparisonResult
    switch (lhs, rhs) {
      case (let l as String, let r as String):
        res = l.localizedStandardCompare(r)
      case (let l as NSNumber, let r as NSNumber):
        res = l.compare(r)
      case (let l as Date, let r as Date):
        res = l.compare(r)
      default:
        res = String(describing: lhs).compare(String(describing: rhs))
    }
    switch res {
      case .orderedSame:
        return nil
      case .orderedAscending:
        return ascending
      case .orderedDescending:
        return !ascending
    }
  }
}

// MARK: - Search execution

///
/// Base class for running asynchronous Spotlight searches. A search is executed on a
/// serial dispatch queue. A timer checks regularly whether the evaluator requested to
/// abort and whether the timeout was reached. A search keeps itself alive until it is
/// finished and the result was written into the future.
///
class SpotlightSearch: @unchecked Sendable {
  let queue = DispatchQueue(label: "net.objecthub.lispkit.spotlight")
  let future: Future
  let context: Context
  let limit: Int?
  let deadline: Date?
  private var timer: DispatchSourceTimer? = nil
  private var finished = false
  private var keepAlive: SpotlightSearch? = nil

  init(limit: Int?, timeout: Double?, future: Future, context: Context) {
    self.limit = limit
    self.deadline = timeout.map { Date(timeIntervalSinceNow: $0) }
    self.future = future
    self.context = context
  }

  /// Starts the search.
  final func start() {
    self.keepAlive = self
    let timer = DispatchSource.makeTimerSource(queue: self.queue)
    timer.schedule(deadline: .now() + 0.2, repeating: 0.2)
    timer.setEventHandler { [unowned self] in
      if self.context.evaluator.isAbortionRequested() {
        self.finish(error: RuntimeError.abortion())
      } else if let deadline = self.deadline, Date() >= deadline {
        self.finish(error: nil)
      }
    }
    self.timer = timer
    timer.resume()
    self.queue.async {
      self.begin()
    }
  }

  /// Finishes the search. Must be called on `queue`.
  final func finish(error: Error?) {
    guard !self.finished else {
      return
    }
    self.finished = true
    self.timer?.cancel()
    self.timer = nil
    let result: Expr
    if let error = error as? RuntimeError {
      self.end()
      result = .error(error)
    } else if let error {
      self.end()
      result = .error(RuntimeError.os(error))
    } else {
      result = self.end()
    }
    _ = try? self.future.setResult(in: self.context, to: result, raise: error != nil)
    self.keepAlive = nil
  }

  /// Starts the actual search. Called on `queue`.
  func begin() {
  }

  /// Stops the search and returns the results. Called on `queue`.
  @discardableResult func end() -> Expr {
    return .null
  }
}

///
/// A file search via `NSMetadataQuery`.
///
final class SpotlightFileSearch: SpotlightSearch, @unchecked Sendable {
  let query = NSMetadataQuery()
  let operations = OperationQueue()
  let sorted: Bool
  let convert: ([NSMetadataItem]) -> Expr
  private var observers: [NSObjectProtocol] = []

  init(predicate: NSPredicate,
       scopes: [Any],
       sort: [(String, Bool)],
       limit: Int?,
       timeout: Double?,
       future: Future,
       context: Context,
       convert: @escaping ([NSMetadataItem]) -> Expr) {
    self.sorted = !sort.isEmpty
    self.convert = convert
    super.init(limit: limit, timeout: timeout, future: future, context: context)
    self.operations.maxConcurrentOperationCount = 1
    self.operations.underlyingQueue = self.queue
    self.query.operationQueue = self.operations
    self.query.predicate = predicate
    self.query.searchScopes = scopes
    self.query.sortDescriptors = sort.map { NSSortDescriptor(key: $0.0, ascending: $0.1) }
  }

  override func begin() {
    let center = NotificationCenter.default
    self.observers.append(center.addObserver(forName: .NSMetadataQueryDidFinishGathering,
                                             object: self.query,
                                             queue: self.operations) { [unowned self] _ in
      self.finish(error: nil)
    })
    self.observers.append(center.addObserver(forName: .NSMetadataQueryGatheringProgress,
                                             object: self.query,
                                             queue: self.operations) { [unowned self] _ in
      if !self.sorted, let limit = self.limit, self.query.resultCount >= limit {
        self.finish(error: nil)
      }
    })
    if !self.query.start() {
      self.finish(error: RuntimeError.custom("error", "unable to start Spotlight query", []))
    }
  }

  override func end() -> Expr {
    self.query.disableUpdates()
    for observer in self.observers {
      NotificationCenter.default.removeObserver(observer)
    }
    self.observers.removeAll()
    var items: [NSMetadataItem] = []
    let n = min(self.query.resultCount, self.limit ?? Int.max)
    items.reserveCapacity(n)
    for i in 0..<n {
      if let item = self.query.result(at: i) as? NSMetadataItem {
        items.append(item)
      }
    }
    let res = self.convert(items)
    self.query.stop()
    return res
  }
}

///
/// A search of the items indexed by the application via `CSSearchQuery`.
///
final class SpotlightItemSearch: SpotlightSearch, @unchecked Sendable {
  let query: CSSearchQuery
  let sort: [(String, Bool)]
  let convert: ([CSSearchableItem]) -> Expr
  private var items: [CSSearchableItem] = []

  init(query: CSSearchQuery,
       sort: [(String, Bool)],
       limit: Int?,
       timeout: Double?,
       future: Future,
       context: Context,
       convert: @escaping ([CSSearchableItem]) -> Expr) {
    self.query = query
    self.sort = sort
    self.convert = convert
    super.init(limit: limit, timeout: timeout, future: future, context: context)
  }

  override func begin() {
    self.query.foundItemsHandler = { [unowned self] items in
      self.queue.async {
        self.items.append(contentsOf: items)
        if self.sort.isEmpty, let limit = self.limit, self.items.count >= limit {
          self.finish(error: nil)
        }
      }
    }
    self.query.completionHandler = { [unowned self] error in
      self.queue.async {
        self.finish(error: error)
      }
    }
    self.query.start()
  }

  override func end() -> Expr {
    self.query.cancel()
    var items = self.items
    if !self.sort.isEmpty {
      items.sort { lhs, rhs in
        for (key, ascending) in self.sort {
          if let res = SpotlightValues.ordered(SpotlightSearchAttributes.value(of: key, in: lhs),
                                               SpotlightSearchAttributes.value(of: key, in: rhs),
                                               ascending: ascending) {
            return res
          }
        }
        return false
      }
    }
    if let limit = self.limit, items.count > limit {
      items.removeLast(items.count - limit)
    }
    return self.convert(items)
  }
}

enum SpotlightIndexing {
  static func complete(_ future: Future, _ error: Error?, in context: Context) {
    if let error {
      _ = try? future.setResult(in: context, to: .error(RuntimeError.os(error)), raise: true)
    } else {
      _ = try? future.setResult(in: context, to: .true)
    }
  }
}

// MARK: - Attribute tables

enum SpotlightSearchAttributes {

  /// Pseudo attribute keys for the identifier and domain of indexed items.
  static let itemIdentifier = "uniqueIdentifier"
  static let itemDomain = "domainIdentifier"

  struct ItemAttribute {
    let name: String
    let property: String
    let getter: String
    let type: SpotlightValueType

    var setter: String {
      return "set" + self.property.prefix(1).uppercased() + self.property.dropFirst() + ":"
    }
  }

  /// Short names for commonly used attributes of files.
  static let fileAliases: [String : String] = [
    "name": "fs-name",
    "size": "fs-size",
    "created": "content-creation-date",
    "modified": "content-modification-date",
    "last-used": "last-used-date",
    "added": "date-added",
    "content-types": "content-type-tree",
    "tags": "user-tags",
    "text": "text-content",
  ]

  /// Short names for commonly used attributes of indexed items.
  static let itemAliases: [String : String] = [
    "name": "display-name",
    "size": "file-size",
    "created": "content-creation-date",
    "modified": "content-modification-date",
    "last-used": "last-used-date",
    "added": "added-date",
    "content-types": "content-type-tree",
    "tags": "keywords",
    "text": "text-content",
    "description": "content-description",
  ]

  /// Content kinds for query form `type`.
  static let kinds: [String : String] = [
    "item": "public.item",
    "content": "public.content",
    "text": "public.text",
    "plain-text": "public.plain-text",
    "source-code": "public.source-code",
    "image": "public.image",
    "movie": "public.movie",
    "audio": "public.audio",
    "pdf": "com.adobe.pdf",
    "folder": "public.folder",
    "application": "com.apple.application",
    "archive": "public.archive",
    "presentation": "public.presentation",
    "spreadsheet": "public.spreadsheet",
    "html": "public.html",
    "email": "public.email-message",
    "contact": "public.contact",
    "calendar-event": "public.calendar-event",
    "font": "public.font",
  ]

  static let fileKeys: [String : String] = {
    var res: [String : String] = [:]
    for (name, key) in Self.fileTable {
      res[name] = key
    }
    return res
  }()

  static let fileNames: [String : String] = {
    var res: [String : String] = [:]
    for (name, key) in Self.fileTable {
      res[key] = name
    }
    return res
  }()

  static let itemsByName: [String : ItemAttribute] = {
    var res: [String : ItemAttribute] = [:]
    for (name, property, getter, type) in Self.itemTable {
      res[name] = ItemAttribute(name: name, property: property, getter: getter, type: type)
    }
    return res
  }()

  static let itemsByProperty: [String : ItemAttribute] = {
    var res: [String : ItemAttribute] = [:]
    for attr in Self.itemsByName.values {
      res[attr.property] = attr
    }
    return res
  }()

  /// Returns the names of all supported attributes for `source`.
  static func names(_ source: SpotlightSource) -> [String] {
    switch source {
      case .files:
        return Self.fileTable.map { $0.0 }
      case .items:
        return ["id", "domain"] + Self.itemTable.map { $0.0 }
    }
  }

  /// Returns the key of the attribute with the given name for `source`.
  static func key(for name: String, _ source: SpotlightSource) -> String? {
    switch source {
      case .files:
        return Self.fileKeys[Self.fileAliases[name] ?? name]
      case .items:
        switch name {
          case "id":
            return Self.itemIdentifier
          case "domain":
            return Self.itemDomain
          default:
            return Self.item(for: name)?.property
        }
    }
  }

  /// Returns the attribute name for the given key.
  static func name(for key: String, _ source: SpotlightSource) -> String? {
    switch source {
      case .files:
        return Self.fileNames[key]
      case .items:
        return Self.itemsByProperty[key]?.name
    }
  }

  /// Returns the attribute of indexed items with the given name or alias.
  static func item(for name: String) -> ItemAttribute? {
    return Self.itemsByName[Self.itemAliases[name] ?? name]
  }

  /// Returns the value of the attribute with key `key` of indexed item `item`.
  static func value(of key: String, in item: CSSearchableItem) -> Any? {
    switch key {
      case Self.itemIdentifier:
        return item.uniqueIdentifier
      case Self.itemDomain:
        return item.domainIdentifier
      default:
        let set = item.attributeSet
        if let attr = Self.itemsByProperty[key] {
          guard set.responds(to: NSSelectorFromString(attr.getter)) else {
            return nil
          }
          return set.value(forKey: attr.property)
        } else if set.responds(to: NSSelectorFromString(key)) {
          return set.value(forKey: key)
        } else if let custom = CSCustomAttributeKey(keyName: key) {
          return set.value(forCustomKey: custom)
        }
        return nil
    }
  }

  /// Attributes of files: names and keys.
  static let fileTable: [(String, String)] = [
    ("acquisition-make", "kMDItemAcquisitionMake"),
    ("acquisition-model", "kMDItemAcquisitionModel"),
    ("album", "kMDItemAlbum"),
    ("alternate-names", "kMDItemAlternateNames"),
    ("altitude", "kMDItemAltitude"),
    ("aperture", "kMDItemAperture"),
    ("apple-loop-descriptors", "kMDItemAppleLoopDescriptors"),
    ("apple-loops-key-filter-type", "kMDItemAppleLoopsKeyFilterType"),
    ("apple-loops-loop-mode", "kMDItemAppleLoopsLoopMode"),
    ("apple-loops-root-key", "kMDItemAppleLoopsRootKey"),
    ("application-categories", "kMDItemApplicationCategories"),
    ("attribute-change-date", "kMDItemAttributeChangeDate"),
    ("audiences", "kMDItemAudiences"),
    ("audio-bit-rate", "kMDItemAudioBitRate"),
    ("audio-channel-count", "kMDItemAudioChannelCount"),
    ("audio-encoding-application", "kMDItemAudioEncodingApplication"),
    ("audio-sample-rate", "kMDItemAudioSampleRate"),
    ("audio-track-number", "kMDItemAudioTrackNumber"),
    ("author-addresses", "kMDItemAuthorAddresses"),
    ("author-email-addresses", "kMDItemAuthorEmailAddresses"),
    ("authors", "kMDItemAuthors"),
    ("bits-per-sample", "kMDItemBitsPerSample"),
    ("cf-bundle-identifier", "kMDItemCFBundleIdentifier"),
    ("camera-owner", "kMDItemCameraOwner"),
    ("city", "kMDItemCity"),
    ("codecs", "kMDItemCodecs"),
    ("color-space", "kMDItemColorSpace"),
    ("comment", "kMDItemComment"),
    ("composer", "kMDItemComposer"),
    ("contact-keywords", "kMDItemContactKeywords"),
    ("content-creation-date", "kMDItemContentCreationDate"),
    ("content-modification-date", "kMDItemContentModificationDate"),
    ("content-rating", "kMDItemContentRating"),
    ("content-type", "kMDItemContentType"),
    ("content-type-tree", "kMDItemContentTypeTree"),
    ("contributors", "kMDItemContributors"),
    ("copyright", "kMDItemCopyright"),
    ("country", "kMDItemCountry"),
    ("coverage", "kMDItemCoverage"),
    ("creator", "kMDItemCreator"),
    ("date-added", "kMDItemDateAdded"),
    ("delivery-type", "kMDItemDeliveryType"),
    ("description", "kMDItemDescription"),
    ("director", "kMDItemDirector"),
    ("display-name", "kMDItemDisplayName"),
    ("downloaded-date", "kMDItemDownloadedDate"),
    ("due-date", "kMDItemDueDate"),
    ("duration-seconds", "kMDItemDurationSeconds"),
    ("exif-gps-version", "kMDItemEXIFGPSVersion"),
    ("exif-version", "kMDItemEXIFVersion"),
    ("editors", "kMDItemEditors"),
    ("email-addresses", "kMDItemEmailAddresses"),
    ("encoding-applications", "kMDItemEncodingApplications"),
    ("executable-architectures", "kMDItemExecutableArchitectures"),
    ("executable-platform", "kMDItemExecutablePlatform"),
    ("exposure-mode", "kMDItemExposureMode"),
    ("exposure-program", "kMDItemExposureProgram"),
    ("exposure-time-seconds", "kMDItemExposureTimeSeconds"),
    ("exposure-time-string", "kMDItemExposureTimeString"),
    ("f-number", "kMDItemFNumber"),
    ("fs-content-change-date", "kMDItemFSContentChangeDate"),
    ("fs-creation-date", "kMDItemFSCreationDate"),
    ("fs-has-custom-icon", "kMDItemFSHasCustomIcon"),
    ("fs-invisible", "kMDItemFSInvisible"),
    ("fs-is-extension-hidden", "kMDItemFSIsExtensionHidden"),
    ("fs-is-stationery", "kMDItemFSIsStationery"),
    ("fs-label", "kMDItemFSLabel"),
    ("fs-name", "kMDItemFSName"),
    ("fs-node-count", "kMDItemFSNodeCount"),
    ("fs-owner-group-id", "kMDItemFSOwnerGroupID"),
    ("fs-owner-user-id", "kMDItemFSOwnerUserID"),
    ("fs-size", "kMDItemFSSize"),
    ("finder-comment", "kMDItemFinderComment"),
    ("flash-on-off", "kMDItemFlashOnOff"),
    ("focal-length", "kMDItemFocalLength"),
    ("focal-length-35mm", "kMDItemFocalLength35mm"),
    ("fonts", "kMDItemFonts"),
    ("gps-area-information", "kMDItemGPSAreaInformation"),
    ("gps-dop", "kMDItemGPSDOP"),
    ("gps-date-stamp", "kMDItemGPSDateStamp"),
    ("gps-dest-bearing", "kMDItemGPSDestBearing"),
    ("gps-dest-distance", "kMDItemGPSDestDistance"),
    ("gps-dest-latitude", "kMDItemGPSDestLatitude"),
    ("gps-dest-longitude", "kMDItemGPSDestLongitude"),
    ("gps-differential", "kMDItemGPSDifferental"),
    ("gps-map-datum", "kMDItemGPSMapDatum"),
    ("gps-measure-mode", "kMDItemGPSMeasureMode"),
    ("gps-processing-method", "kMDItemGPSProcessingMethod"),
    ("gps-status", "kMDItemGPSStatus"),
    ("gps-track", "kMDItemGPSTrack"),
    ("genre", "kMDItemGenre"),
    ("html-content", "kMDItemHTMLContent"),
    ("has-alpha-channel", "kMDItemHasAlphaChannel"),
    ("headline", "kMDItemHeadline"),
    ("iso-speed", "kMDItemISOSpeed"),
    ("identifier", "kMDItemIdentifier"),
    ("image-direction", "kMDItemImageDirection"),
    ("information", "kMDItemInformation"),
    ("instant-message-addresses", "kMDItemInstantMessageAddresses"),
    ("instructions", "kMDItemInstructions"),
    ("is-application-managed", "kMDItemIsApplicationManaged"),
    ("is-general-midi-sequence", "kMDItemIsGeneralMIDISequence"),
    ("is-likely-junk", "kMDItemIsLikelyJunk"),
    ("key-signature", "kMDItemKeySignature"),
    ("keywords", "kMDItemKeywords"),
    ("kind", "kMDItemKind"),
    ("label-id", "kMDItemLabelID"),
    ("label-icon", "kMDItemLabelIcon"),
    ("label-kind", "kMDItemLabelKind"),
    ("label-uuid", "kMDItemLabelUUID"),
    ("languages", "kMDItemLanguages"),
    ("last-used-date", "kMDItemLastUsedDate"),
    ("latitude", "kMDItemLatitude"),
    ("layer-names", "kMDItemLayerNames"),
    ("lens-model", "kMDItemLensModel"),
    ("logical-size", "kMDItemLogicalSize"),
    ("longitude", "kMDItemLongitude"),
    ("lyricist", "kMDItemLyricist"),
    ("max-aperture", "kMDItemMaxAperture"),
    ("media-extensions", "kMDItemMediaExtensions"),
    ("media-types", "kMDItemMediaTypes"),
    ("metering-mode", "kMDItemMeteringMode"),
    ("musical-genre", "kMDItemMusicalGenre"),
    ("musical-instrument-category", "kMDItemMusicalInstrumentCategory"),
    ("musical-instrument-name", "kMDItemMusicalInstrumentName"),
    ("named-location", "kMDItemNamedLocation"),
    ("number-of-pages", "kMDItemNumberOfPages"),
    ("organizations", "kMDItemOrganizations"),
    ("orientation", "kMDItemOrientation"),
    ("original-format", "kMDItemOriginalFormat"),
    ("original-source", "kMDItemOriginalSource"),
    ("page-height", "kMDItemPageHeight"),
    ("page-width", "kMDItemPageWidth"),
    ("participants", "kMDItemParticipants"),
    ("path", "kMDItemPath"),
    ("performers", "kMDItemPerformers"),
    ("phone-numbers", "kMDItemPhoneNumbers"),
    ("physical-size", "kMDItemPhysicalSize"),
    ("pixel-count", "kMDItemPixelCount"),
    ("pixel-height", "kMDItemPixelHeight"),
    ("pixel-width", "kMDItemPixelWidth"),
    ("producer", "kMDItemProducer"),
    ("profile-name", "kMDItemProfileName"),
    ("projects", "kMDItemProjects"),
    ("publishers", "kMDItemPublishers"),
    ("recipient-addresses", "kMDItemRecipientAddresses"),
    ("recipient-email-addresses", "kMDItemRecipientEmailAddresses"),
    ("recipients", "kMDItemRecipients"),
    ("recording-date", "kMDItemRecordingDate"),
    ("recording-year", "kMDItemRecordingYear"),
    ("red-eye-on-off", "kMDItemRedEyeOnOff"),
    ("resolution-height-dpi", "kMDItemResolutionHeightDPI"),
    ("resolution-width-dpi", "kMDItemResolutionWidthDPI"),
    ("rights", "kMDItemRights"),
    ("security-method", "kMDItemSecurityMethod"),
    ("speed", "kMDItemSpeed"),
    ("star-rating", "kMDItemStarRating"),
    ("state-or-province", "kMDItemStateOrProvince"),
    ("streamable", "kMDItemStreamable"),
    ("subject", "kMDItemSubject"),
    ("support-file-type", "kMDItemSupportFileType"),
    ("tempo", "kMDItemTempo"),
    ("text-content", "kMDItemTextContent"),
    ("theme", "kMDItemTheme"),
    ("time-signature", "kMDItemTimeSignature"),
    ("timestamp", "kMDItemTimestamp"),
    ("title", "kMDItemTitle"),
    ("total-bit-rate", "kMDItemTotalBitRate"),
    ("url", "kMDItemURL"),
    ("use-count", "kMDItemUseCount"),
    ("used-dates", "kMDItemUsedDates"),
    ("user-tags", "kMDItemUserTags"),
    ("version", "kMDItemVersion"),
    ("video-bit-rate", "kMDItemVideoBitRate"),
    ("where-froms", "kMDItemWhereFroms"),
    ("white-balance", "kMDItemWhiteBalance"),
    ("xmp-credit", "kMDItemXMPCredit"),
    ("xmp-digital-source-type", "kMDItemXMPDigitalSourceType"),
  ]

  /// Attributes of indexed items: names, properties, getters, and value types.
  static let itemTable: [(String, String, String, SpotlightValueType)] = [
    ("subject", "subject", "subject", .string),
    ("theme", "theme", "theme", .string),
    ("content-description", "contentDescription", "contentDescription", .string),
    ("identifier", "identifier", "identifier", .string),
    ("audiences", "audiences", "audiences", .strings),
    ("file-size", "fileSize", "fileSize", .number),
    ("page-count", "pageCount", "pageCount", .number),
    ("page-width", "pageWidth", "pageWidth", .number),
    ("page-height", "pageHeight", "pageHeight", .number),
    ("security-method", "securityMethod", "securityMethod", .string),
    ("creator", "creator", "creator", .string),
    ("encoding-applications", "encodingApplications", "encodingApplications", .strings),
    ("kind", "kind", "kind", .string),
    ("font-names", "fontNames", "fontNames", .strings),
    ("due-date", "dueDate", "dueDate", .date),
    ("completion-date", "completionDate", "completionDate", .date),
    ("start-date", "startDate", "startDate", .date),
    ("end-date", "endDate", "endDate", .date),
    ("important-dates", "importantDates", "importantDates", .dates),
    ("all-day", "allDay", "allDay", .number),
    ("display-name", "displayName", "displayName", .string),
    ("alternate-names", "alternateNames", "alternateNames", .strings),
    ("path", "path", "path", .string),
    ("content-url", "contentURL", "contentURL", .url),
    ("thumbnail-url", "thumbnailURL", "thumbnailURL", .url),
    ("thumbnail-data", "thumbnailData", "thumbnailData", .data),
    ("dark-thumbnail-url", "darkThumbnailURL", "darkThumbnailURL", .url),
    ("related-unique-identifier", "relatedUniqueIdentifier", "relatedUniqueIdentifier", .string),
    ("weak-related-unique-identifier", "weakRelatedUniqueIdentifier", "weakRelatedUniqueIdentifier", .string),
    ("metadata-modification-date", "metadataModificationDate", "metadataModificationDate", .date),
    ("content-type", "contentType", "contentType", .string),
    ("content-type-tree", "contentTypeTree", "contentTypeTree", .strings),
    ("keywords", "keywords", "keywords", .strings),
    ("title", "title", "title", .string),
    ("version", "version", "version", .string),
    ("user-created", "userCreated", "isUserCreated", .number),
    ("user-owned", "userOwned", "isUserOwned", .number),
    ("user-curated", "userCurated", "isUserCurated", .number),
    ("ranking-hint", "rankingHint", "rankingHint", .number),
    ("domain-identifier", "domainIdentifier", "domainIdentifier", .string),
    ("transcribed-text-content", "transcribedTextContent", "transcribedTextContent", .string),
    ("supports-phone-call", "supportsPhoneCall", "supportsPhoneCall", .number),
    ("supports-navigation", "supportsNavigation", "supportsNavigation", .number),
    ("container-title", "containerTitle", "containerTitle", .string),
    ("container-display-name", "containerDisplayName", "containerDisplayName", .string),
    ("container-identifier", "containerIdentifier", "containerIdentifier", .string),
    ("container-order", "containerOrder", "containerOrder", .number),
    ("provider-data-type-identifiers", "providerDataTypeIdentifiers", "providerDataTypeIdentifiers", .strings),
    ("provider-file-type-identifiers", "providerFileTypeIdentifiers", "providerFileTypeIdentifiers", .strings),
    ("provider-in-place-file-type-identifiers", "providerInPlaceFileTypeIdentifiers", "providerInPlaceFileTypeIdentifiers", .strings),
    ("pixel-height", "pixelHeight", "pixelHeight", .number),
    ("pixel-width", "pixelWidth", "pixelWidth", .number),
    ("pixel-count", "pixelCount", "pixelCount", .number),
    ("color-space", "colorSpace", "colorSpace", .string),
    ("bits-per-sample", "bitsPerSample", "bitsPerSample", .number),
    ("flash-on", "flashOn", "isFlashOn", .number),
    ("focal-length", "focalLength", "focalLength", .number),
    ("focal-length-35mm", "focalLength35mm", "isFocalLength35mm", .number),
    ("acquisition-make", "acquisitionMake", "acquisitionMake", .string),
    ("acquisition-model", "acquisitionModel", "acquisitionModel", .string),
    ("camera-owner", "cameraOwner", "cameraOwner", .string),
    ("lens-model", "lensModel", "lensModel", .string),
    ("iso-speed", "ISOSpeed", "ISOSpeed", .number),
    ("orientation", "orientation", "orientation", .number),
    ("layer-names", "layerNames", "layerNames", .strings),
    ("white-balance", "whiteBalance", "whiteBalance", .number),
    ("aperture", "aperture", "aperture", .number),
    ("profile-name", "profileName", "profileName", .string),
    ("resolution-width-dpi", "resolutionWidthDPI", "resolutionWidthDPI", .number),
    ("resolution-height-dpi", "resolutionHeightDPI", "resolutionHeightDPI", .number),
    ("exposure-mode", "exposureMode", "exposureMode", .number),
    ("exposure-time", "exposureTime", "exposureTime", .number),
    ("exif-version", "EXIFVersion", "EXIFVersion", .string),
    ("exif-gps-version", "EXIFGPSVersion", "EXIFGPSVersion", .string),
    ("has-alpha-channel", "hasAlphaChannel", "hasAlphaChannel", .number),
    ("red-eye-on", "redEyeOn", "isRedEyeOn", .number),
    ("metering-mode", "meteringMode", "meteringMode", .string),
    ("max-aperture", "maxAperture", "maxAperture", .number),
    ("f-number", "fNumber", "fNumber", .number),
    ("exposure-program", "exposureProgram", "exposureProgram", .string),
    ("exposure-time-string", "exposureTimeString", "exposureTimeString", .string),
    ("editors", "editors", "editors", .strings),
    ("participants", "participants", "participants", .strings),
    ("projects", "projects", "projects", .strings),
    ("content-sources", "contentSources", "contentSources", .strings),
    ("comment", "comment", "comment", .string),
    ("copyright", "copyright", "copyright", .string),
    ("last-used-date", "lastUsedDate", "lastUsedDate", .date),
    ("content-creation-date", "contentCreationDate", "contentCreationDate", .date),
    ("content-modification-date", "contentModificationDate", "contentModificationDate", .date),
    ("added-date", "addedDate", "addedDate", .date),
    ("downloaded-date", "downloadedDate", "downloadedDate", .date),
    ("duration", "duration", "duration", .number),
    ("contact-keywords", "contactKeywords", "contactKeywords", .strings),
    ("codecs", "codecs", "codecs", .strings),
    ("media-types", "mediaTypes", "mediaTypes", .strings),
    ("streamable", "streamable", "isStreamable", .number),
    ("total-bit-rate", "totalBitRate", "totalBitRate", .number),
    ("video-bit-rate", "videoBitRate", "videoBitRate", .number),
    ("audio-bit-rate", "audioBitRate", "audioBitRate", .number),
    ("delivery-type", "deliveryType", "deliveryType", .number),
    ("organizations", "organizations", "organizations", .strings),
    ("role", "role", "role", .string),
    ("languages", "languages", "languages", .strings),
    ("rights", "rights", "rights", .string),
    ("publishers", "publishers", "publishers", .strings),
    ("contributors", "contributors", "contributors", .strings),
    ("coverage", "coverage", "coverage", .strings),
    ("rating", "rating", "rating", .number),
    ("rating-description", "ratingDescription", "ratingDescription", .string),
    ("play-count", "playCount", "playCount", .number),
    ("information", "information", "information", .string),
    ("director", "director", "director", .string),
    ("producer", "producer", "producer", .string),
    ("genre", "genre", "genre", .string),
    ("performers", "performers", "performers", .strings),
    ("original-format", "originalFormat", "originalFormat", .string),
    ("original-source", "originalSource", "originalSource", .string),
    ("local", "local", "isLocal", .number),
    ("content-rating", "contentRating", "contentRating", .number),
    ("url", "URL", "URL", .url),
    ("audio-sample-rate", "audioSampleRate", "audioSampleRate", .number),
    ("audio-channel-count", "audioChannelCount", "audioChannelCount", .number),
    ("tempo", "tempo", "tempo", .number),
    ("key-signature", "keySignature", "keySignature", .string),
    ("time-signature", "timeSignature", "timeSignature", .string),
    ("audio-encoding-application", "audioEncodingApplication", "audioEncodingApplication", .string),
    ("composer", "composer", "composer", .string),
    ("lyricist", "lyricist", "lyricist", .string),
    ("album", "album", "album", .string),
    ("artist", "artist", "artist", .string),
    ("audio-track-number", "audioTrackNumber", "audioTrackNumber", .number),
    ("recording-date", "recordingDate", "recordingDate", .date),
    ("musical-genre", "musicalGenre", "musicalGenre", .string),
    ("general-midi-sequence", "generalMIDISequence", "isGeneralMIDISequence", .number),
    ("musical-instrument-category", "musicalInstrumentCategory", "musicalInstrumentCategory", .string),
    ("musical-instrument-name", "musicalInstrumentName", "musicalInstrumentName", .string),
    ("account-identifier", "accountIdentifier", "accountIdentifier", .string),
    ("account-handles", "accountHandles", "accountHandles", .strings),
    ("html-content-data", "HTMLContentData", "HTMLContentData", .data),
    ("text-content", "textContent", "textContent", .string),
    ("authors", "authors", "authors", .persons),
    ("primary-recipients", "primaryRecipients", "primaryRecipients", .persons),
    ("additional-recipients", "additionalRecipients", "additionalRecipients", .persons),
    ("hidden-additional-recipients", "hiddenAdditionalRecipients", "hiddenAdditionalRecipients", .persons),
    ("mailbox-identifiers", "mailboxIdentifiers", "mailboxIdentifiers", .strings),
    ("author-names", "authorNames", "authorNames", .strings),
    ("recipient-names", "recipientNames", "recipientNames", .strings),
    ("author-email-addresses", "authorEmailAddresses", "authorEmailAddresses", .strings),
    ("recipient-email-addresses", "recipientEmailAddresses", "recipientEmailAddresses", .strings),
    ("author-addresses", "authorAddresses", "authorAddresses", .strings),
    ("recipient-addresses", "recipientAddresses", "recipientAddresses", .strings),
    ("phone-numbers", "phoneNumbers", "phoneNumbers", .strings),
    ("email-addresses", "emailAddresses", "emailAddresses", .strings),
    ("instant-message-addresses", "instantMessageAddresses", "instantMessageAddresses", .strings),
    ("likely-junk", "likelyJunk", "isLikelyJunk", .number),
    ("headline", "headline", "headline", .string),
    ("instructions", "instructions", "instructions", .string),
    ("thoroughfare", "thoroughfare", "thoroughfare", .string),
    ("sub-thoroughfare", "subThoroughfare", "subThoroughfare", .string),
    ("postal-code", "postalCode", "postalCode", .string),
    ("city", "city", "city", .string),
    ("state-or-province", "stateOrProvince", "stateOrProvince", .string),
    ("country", "country", "country", .string),
    ("fully-formatted-address", "fullyFormattedAddress", "fullyFormattedAddress", .string),
    ("altitude", "altitude", "altitude", .number),
    ("latitude", "latitude", "latitude", .number),
    ("longitude", "longitude", "longitude", .number),
    ("speed", "speed", "speed", .number),
    ("timestamp", "timestamp", "timestamp", .date),
    ("image-direction", "imageDirection", "imageDirection", .number),
    ("named-location", "namedLocation", "namedLocation", .string),
    ("gps-track", "GPSTrack", "GPSTrack", .number),
    ("gps-status", "GPSStatus", "GPSStatus", .string),
    ("gps-measure-mode", "GPSMeasureMode", "GPSMeasureMode", .string),
    ("gps-dop", "GPSDOP", "GPSDOP", .number),
    ("gps-map-datum", "GPSMapDatum", "GPSMapDatum", .string),
    ("gps-dest-latitude", "GPSDestLatitude", "GPSDestLatitude", .number),
    ("gps-dest-longitude", "GPSDestLongitude", "GPSDestLongitude", .number),
    ("gps-dest-bearing", "GPSDestBearing", "GPSDestBearing", .number),
    ("gps-dest-distance", "GPSDestDistance", "GPSDestDistance", .number),
    ("gps-processing-method", "GPSProcessingMethod", "GPSProcessingMethod", .string),
    ("gps-area-information", "GPSAreaInformation", "GPSAreaInformation", .string),
    ("gps-date-stamp", "GPSDateStamp", "GPSDateStamp", .date),
    ("gps-differential", "GPSDifferental", "GPSDifferental", .number),
  ]
}

#endif
