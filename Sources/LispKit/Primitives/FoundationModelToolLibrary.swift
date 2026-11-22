//
//  FoundationModelToolLibrary.swift
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

#if canImport(FoundationModels) && canImport(Vision) && canImport(CoreSpotlight)

import Foundation
import FoundationModels
import Vision
import CoreSpotlight

///
/// Implements library `(lispkit foundation-model tool)`, which provides the tools that
/// Apple ships with FoundationModels for use with library `(lispkit foundation-model)`:
/// `OCRTool` and `BarcodeReaderTool` (from Vision), which read text and barcodes from
/// images attached to the prompt, and `SpotlightSearchTool` (from CoreSpotlight), which
/// searches the content indexed by Spotlight.
///
@available(macOS 27.0, iOS 27.0, *)
public final class FoundationModelToolLibrary: NativeLibrary {

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "foundation-model", "tool"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "foundation-model"], "tool?")
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("make-ocr-tool", self.makeOCRTool))
    self.define(Procedure("make-barcode-reader-tool", self.makeBarcodeReaderTool))
    self.define(Procedure("make-spotlight-search-tool", self.makeSpotlightSearchTool))
    self.define(Procedure("spotlight-search-tool?", self.isSpotlightSearchTool))
    self.define(Procedure("spotlight-search-results", self.spotlightSearchResults))
    self.define(Procedure("spotlight-search-results-clear!", self.spotlightSearchResultsClear))
    self.define(Procedure("spotlight-search-attributes", self.spotlightSearchAttributes))
  }

  // MARK: - Helpers

  private func sym(_ str: String) -> Expr {
    return .symbol(self.context.symbols.intern(str))
  }

  private func entry(_ key: String, _ value: Expr) -> Expr {
    return .pair(self.sym(key), value)
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
  private func options(_ expr: Expr) throws -> [(String, Expr)] {
    return try self.list(expr).map { option in
      guard case .pair(let key, let value) = option else {
        throw RuntimeError.type(option, expected: [.pairType])
      }
      return (try self.identifier(key), value)
    }
  }

  private func string(_ expr: Expr?) throws -> String? {
    guard let expr, expr.isTrue else {
      return nil
    }
    return try expr.asString()
  }

  // MARK: - Vision tools

  private func makeOCRTool(name: Expr?, description: Expr?) throws -> Expr {
    return .object(NativeTool(native: OCRTool(name: try self.string(name),
                                              description: try self.string(description))))
  }

  private func makeBarcodeReaderTool(name: Expr?, description: Expr?) throws -> Expr {
    return .object(NativeTool(native: BarcodeReaderTool(name: try self.string(name),
                                                        description: try self.string(description))))
  }

  // MARK: - Spotlight search tool

  private func makeSpotlightSearchTool(config: Expr?) throws -> Expr {
    var configuration = SpotlightSearchTool.Configuration(sources: [.coreSpotlight])
    var level: SpotlightSearchTool.GuidanceLevel? = nil
    var format: SpotlightSearchTool.FormatLevel? = nil
    var name: String? = nil
    if let config, config.isTrue {
      for (key, value) in try self.options(config) {
        switch key {
          case "name":
            name = try value.asString()
          case "sources":
            configuration.sources = try self.list(value).map(self.searchSource(from:))
          case "guide":
            level = try self.guidanceLevel(from: value)
          case "format":
            switch try self.identifier(value) {
              case "structured":
                format = .structured
              case "compact":
                format = .compact
              default:
                throw self.invalid("unknown Spotlight search format", value)
            }
          case "max-response-size":
            configuration.maximumResponseSize = value.isFalse ? nil
                                              : try value.asInt(above: 1, below: Int.max)
          case "user":
            configuration.contactResolver = try self.contactResolver(from: value)
          default:
            throw self.invalid("unknown Spotlight search tool option", self.sym(key))
        }
      }
    }
    if level != nil || format != nil {
      configuration.guide = SpotlightSearchTool.Guide(level: level ?? .complete,
                                                      format: format ?? .structured)
    }
    var tool = SpotlightSearchTool(configuration: configuration)
    if let name {
      tool.name = name
    }
    return .object(NativeSpotlightSearchTool(tool))
  }

  private func searchSource(from expr: Expr) throws -> SearchSource {
    let kind: String
    let options: [(String, Expr)]
    if case .pair(let head, let tail) = expr {
      kind = try self.identifier(head)
      options = try self.options(tail)
    } else {
      kind = try self.identifier(expr)
      options = []
    }
    switch kind {
      case "core-spotlight":
        var source = CoreSpotlightSource()
        for (key, value) in options {
          switch key {
            case "fetch-attributes":
              source.fetchAttributes = try self.attributes(from: value)
            case "max-results":
              source.maximumResultCount = value.isFalse ? nil
                                        : try value.asInt(above: 1, below: Int.max)
            case "allow-mail":
              #if os(macOS)
              source.sourceOptions = value.isTrue ? .allowMail : []
              #endif
            default:
              throw self.invalid("unknown Spotlight source option", self.sym(key))
          }
        }
        return .coreSpotlight(source)
      case "files":
        var source = FileSource()
        for (key, value) in options {
          switch key {
            case "fetch-attributes":
              source.fetchAttributes = try self.attributes(from: value)
            case "max-results":
              source.maximumResultCount = value.isFalse ? nil
                                        : try value.asInt(above: 1, below: Int.max)
            case "scopes":
              source.scopes = try self.list(value).map { scope in
                let path = self.context.fileHandler.path(try scope.asPath(),
                                                         relativeTo: self.context.evaluator.currentDirectoryPath)
                return URL(fileURLWithPath: path, isDirectory: true)
              }
            default:
              throw self.invalid("unknown Spotlight source option", self.sym(key))
          }
        }
        return .files(source)
      default:
        throw self.invalid("unknown Spotlight source", expr)
    }
  }

  private func guidanceLevel(from expr: Expr) throws -> SpotlightSearchTool.GuidanceLevel {
    let kind: String
    var args: [Expr] = []
    if case .pair(let head, let tail) = expr {
      kind = try self.identifier(head)
      args = try self.list(tail)
    } else {
      kind = try self.identifier(expr)
    }
    switch kind {
      case "complete":
        return .complete
      case "focused":
        guard args.count <= 1 else {
          throw self.invalid("focused guide expects at most one content domain", expr)
        }
        return .focused(args.isEmpty ? .items : try self.contentDomain(from: args[0]))
      case "dynamic":
        var profile = SpotlightSearchTool.GuidanceProfile()
        for arg in args {
          if case .pair(let head, let tail) = arg {
            guard try self.identifier(head) == "attributes" else {
              throw self.invalid("unknown dynamic guide feature", arg)
            }
            profile.attributes = try self.attributes(from: tail)
            continue
          }
          switch try self.identifier(arg) {
            case "text-match":
              profile.textMatch = true
            case "similarity-match":
              profile.similarityMatch = true
            case "numeric-match":
              profile.numericMatch = true
            case "dates":
              profile.dates = true
            case "people":
              profile.people = true
            case "content-type":
              profile.contentType = true
            default:
              throw self.invalid("unknown dynamic guide feature", arg)
          }
        }
        return .dynamic(profile)
      default:
        throw self.invalid("unknown Spotlight search guide", expr)
    }
  }

  private func contentDomain(from expr: Expr) throws -> SpotlightSearchTool.ContentDomain {
    let kind: String
    var fields: [String : [SearchableItemAttribute]] = [:]
    var configured = false
    if case .pair(let head, let tail) = expr {
      kind = try self.identifier(head)
      configured = true
      for field in try self.list(tail) {
        guard case .pair(let key, let value) = field else {
          throw RuntimeError.type(field, expected: [.pairType])
        }
        fields[try self.identifier(key)] = try self.attributes(from: value)
      }
    } else {
      kind = try self.identifier(expr)
    }
    func field(_ name: String, _ allowed: [String]) throws -> [SearchableItemAttribute]? {
      if let unknown = fields.keys.first(where: { !allowed.contains($0) }) {
        throw self.invalid("unknown field of content domain \(kind)", self.sym(unknown))
      }
      return fields[name]
    }
    switch kind {
      case "items":
        guard configured else { return .items }
        let a = ["title", "text", "created", "modified"]
        return .items(.init(title: try field("title", a), text: try field("text", a),
                            created: try field("created", a), modified: try field("modified", a)))
      case "documents":
        guard configured else { return .documents }
        let a = ["authors", "keywords", "created", "modified"]
        return .documents(.init(authors: try field("authors", a),
                                keywords: try field("keywords", a),
                                created: try field("created", a),
                                modified: try field("modified", a)))
      case "communications":
        guard configured else { return .communications }
        let a = ["authors", "recipients", "sent", "received", "topic"]
        return .communications(.init(authors: try field("authors", a),
                                     recipients: try field("recipients", a),
                                     sent: try field("sent", a),
                                     received: try field("received", a),
                                     topic: try field("topic", a)))
      case "calendar":
        guard configured else { return .calendar }
        let a = ["organizer", "attendees", "location", "date"]
        return .calendar(.init(organizer: try field("organizer", a),
                               attendees: try field("attendees", a),
                               location: try field("location", a),
                               date: try field("date", a)))
      case "audio":
        guard configured else { return .audio }
        let a = ["artist", "album", "transcription", "date"]
        return .audio(.init(artist: try field("artist", a), album: try field("album", a),
                            transcription: try field("transcription", a),
                            date: try field("date", a)))
      case "visual-media":
        guard configured else { return .visualMedia }
        let a = ["people", "description", "location", "date"]
        return .visualMedia(.init(people: try field("people", a),
                                  description: try field("description", a),
                                  location: try field("location", a),
                                  date: try field("date", a)))
      default:
        throw self.invalid("unknown content domain", expr)
    }
  }

  /// Attributes are either symbols from `spotlight-search-attributes` or strings with
  /// Spotlight attribute names, such as "kMDItemNamedLocation".
  private func attributes(from expr: Expr) throws -> [SearchableItemAttribute] {
    return try self.list(expr).map { attr in
      switch attr {
        case .symbol(let sym):
          guard let attribute = SpotlightAttributes.byName[sym.identifier]?.attribute else {
            throw self.invalid("unknown Spotlight attribute", attr)
          }
          return attribute
        case .string(let str):
          return SearchableItemAttribute(rawValue: str as String)
        default:
          throw RuntimeError.type(attr, expected: [.symbolType, .strType])
      }
    }
  }

  private func contactResolver(from expr: Expr) throws -> (any ContactResolver)? {
    if expr.isFalse {
      return nil
    }
    if case .string(let name) = expr {
      return SpotlightUser(contact: ResolvedContact(displayName: name as String))
    }
    var contact: ResolvedContact? = nil
    var names: [String] = []
    var emails: [String] = []
    var phones: [String] = []
    for (key, value) in try self.options(expr) {
      switch key {
        case "display-name":
          contact = ResolvedContact(displayName: try value.asString())
        case "names":
          names = try self.list(value).map { try $0.asString() }
        case "email-addresses":
          emails = try self.list(value).map { try $0.asString() }
        case "phone-numbers":
          phones = try self.list(value).map { try $0.asString() }
        default:
          throw self.invalid("unknown Spotlight user property", self.sym(key))
      }
    }
    guard var contact else {
      throw self.invalid("Spotlight user requires a display name", expr)
    }
    contact.names = names
    contact.emailAddresses = emails
    contact.phoneNumbers = phones
    return SpotlightUser(contact: contact)
  }

  private func spotlightTool(from expr: Expr) throws -> NativeSpotlightSearchTool {
    guard case .object(let obj) = expr, let tool = obj as? NativeSpotlightSearchTool else {
      throw self.invalid("not a Spotlight search tool", expr)
    }
    return tool
  }

  private func isSpotlightSearchTool(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is NativeSpotlightSearchTool else {
      return .false
    }
    return .true
  }

  private func spotlightSearchResults(expr: Expr) throws -> Expr {
    let replies = try self.spotlightTool(from: expr).replies
    return .makeList(Exprs(replies.map(self.expr(from:))))
  }

  private func spotlightSearchResultsClear(expr: Expr) throws -> Expr {
    try self.spotlightTool(from: expr).clear()
    return .void
  }

  private func spotlightSearchAttributes() -> Expr {
    return .makeList(Exprs(SpotlightAttributes.all.map { self.sym($0.name) }))
  }

  // MARK: - Converting search results

  private func expr(from reply: SpotlightSearchTool.SearchReply) -> Expr {
    var entries = Exprs()
    entries.append(self.entry("label", reply.label.map { .makeString($0) } ?? .false))
    entries.append(self.entry("status", self.sym(reply.status == .complete ? "complete"
                                                                           : "partial")))
    switch reply.content {
      case .items(let items):
        entries.append(self.entry("kind", self.sym("items")))
        entries.append(self.entry("items", .makeList(Exprs(items.map { self.expr(from: $0) }))))
      case .scoredItems(let items):
        entries.append(self.entry("kind", self.sym("scored-items")))
        entries.append(self.entry("items", .makeList(Exprs(items.map {
          self.expr(from: $0.item, score: $0.score)
        }))))
      case .groupedItems(let groups):
        entries.append(self.entry("kind", self.sym("grouped-items")))
        let keys = groups.keys.sorted { $0.rawValue < $1.rawValue }
        entries.append(self.entry("groups", .makeList(Exprs(keys.map { key in
          .pair(.makeString(key.rawValue),
                .makeList(Exprs(groups[key]!.map { self.expr(from: $0) })))
        }))))
      case .count(let count):
        entries.append(self.entry("kind", self.sym("count")))
        entries.append(self.entry("header", count.header.map { .makeString($0) } ?? .false))
        entries.append(self.entry("count", .makeNumber(count.value)))
      case .table(let table):
        entries.append(self.entry("kind", self.sym("table")))
        entries.append(self.entry("header", table.header.map { .makeString($0) } ?? .false))
        entries.append(self.entry("columns", .makeList(Exprs(table.columns.map { column in
          .pair(.makeString(column.name), self.sym(self.name(of: column.type)))
        }))))
        entries.append(self.entry("rows", .makeList(Exprs(table.rows.map { row in
          .makeList(Exprs(row.values.map(self.expr(from:))))
        }))))
      case .statistic(let statistic):
        entries.append(self.entry("kind", self.sym("statistic")))
        entries.append(self.entry("header", statistic.header.map { .makeString($0) } ?? .false))
        entries.append(self.entry("name", .makeString(statistic.name)))
        entries.append(self.entry("value", .flonum(statistic.value)))
      case .text(let text):
        entries.append(self.entry("kind", self.sym("text")))
        entries.append(self.entry("header", text.header.map { .makeString($0) } ?? .false))
        entries.append(self.entry("text", .makeString(text.body)))
      @unknown default:
        entries.append(self.entry("kind", self.sym("unknown")))
    }
    return .makeList(entries)
  }

  private func name(of type: SearchResultsTable.ValueType) -> String {
    switch type {
      case .string:
        return "string"
      case .integer:
        return "integer"
      case .double:
        return "double"
      case .date:
        return "date"
      case .boolean:
        return "boolean"
      @unknown default:
        return "unknown"
    }
  }

  private func expr(from value: SearchResultsTable.Value) -> Expr {
    switch value {
      case .string(let str):
        return .makeString(str)
      case .integer(let num):
        return .makeNumber(num)
      case .double(let num):
        return .flonum(num)
      case .date(let date):
        return ModelErrorDetails.date(date)
      case .boolean(let bool):
        return .makeBoolean(bool)
      case .none:
        return .false
      @unknown default:
        return .false
    }
  }

  /// Converts a searchable item into an association list with the identifier of the item,
  /// its domain identifier (if available), its score (for scored items), and all attributes
  /// listed by `spotlight-search-attributes` that have a value.
  private func expr(from item: SearchableItem, score: Double? = nil) -> Expr {
    let item = item.item
    var entries = Exprs()
    entries.append(self.entry("id", .makeString(item.uniqueIdentifier)))
    if let domain = item.domainIdentifier {
      entries.append(self.entry("domain", .makeString(domain)))
    }
    if let score {
      entries.append(self.entry("score", .flonum(score)))
    }
    let set = item.attributeSet
    for attr in SpotlightAttributes.all {
      if let value = SpotlightAttributes.value(of: attr.property, in: set),
         let expr = self.expr(fromAttributeValue: value) {
        entries.append(self.entry(attr.name, expr))
      }
    }
    return .makeList(entries)
  }

  private func expr(fromAttributeValue value: Any) -> Expr? {
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
        return ModelErrorDetails.date(date)
      case let url as URL:
        return .makeString(url.isFileURL ? url.path : url.absoluteString)
      case let data as Data:
        return .bytes(MutableBox([UInt8](data)))
      case let person as CSPerson:
        return person.displayName.map { .makeString($0) }
                 ?? person.handles.first.map { .makeString($0) }
      case let array as [Any]:
        return .makeList(Exprs(array.compactMap(self.expr(fromAttributeValue:))))
      default:
        return .makeString(String(describing: value))
    }
  }
}

///
/// A Spotlight search tool. Besides the results returned to the language model, the
/// Spotlight search tool produces results for the application (e.g. the items found by
/// pipeline stages that output to the user). They are collected and returned by
/// `spotlight-search-results`.
///
@available(macOS 27.0, iOS 27.0, *)
final class NativeSpotlightSearchTool: NativeTool, @unchecked Sendable {

  /// The maximum number of replies that are kept.
  static let maxReplies = 100

  private let lock = NSLock()
  private var collected: [SpotlightSearchTool.SearchReply] = []
  private var task: Task<Void, Never>? = nil

  init(_ tool: SpotlightSearchTool) {
    super.init(native: tool)
    self.task = Task { [weak self] in
      for await reply in tool.searchResults {
        guard let self else {
          return
        }
        self.add(reply)
      }
    }
  }

  deinit {
    self.task?.cancel()
  }

  private func add(_ reply: SpotlightSearchTool.SearchReply) {
    self.lock.lock()
    self.collected.append(reply)
    if self.collected.count > Self.maxReplies {
      self.collected.removeFirst(self.collected.count - Self.maxReplies)
    }
    self.lock.unlock()
  }

  /// The replies collected so far, oldest first.
  var replies: [SpotlightSearchTool.SearchReply] {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    return self.collected
  }

  /// Removes all collected replies.
  func clear() {
    self.lock.lock()
    self.collected.removeAll()
    self.lock.unlock()
  }
}

/// Resolves the identity of the user for the Spotlight search tool, e.g. for finding
/// messages sent by or to the user.
@available(macOS 27.0, iOS 27.0, *)
struct SpotlightUser: ContactResolver {
  let contact: ResolvedContact

  func userIdentity() -> ResolvedContact {
    return self.contact
  }
}

///
/// The Spotlight attributes that are supported by `(lispkit foundation-model tool)`: their
/// Scheme names, the corresponding attributes, and the properties of attribute sets that
/// provide their values.
///
@available(macOS 27.0, iOS 27.0, *)
enum SpotlightAttributes {

  struct Attribute {
    let name: String
    let attribute: SearchableItemAttribute
    let property: String
  }

  static let all: [Attribute] = Self.table.map {
    Attribute(name: $0.0, attribute: $0.1, property: $0.2)
  }

  static let byName: [String : Attribute] = {
    var res: [String : Attribute] = [:]
    for attr in Self.all {
      res[attr.name] = attr
    }
    return res
  }()

  /// Returns the value of the attribute set property `property`, or `nil` if the attribute
  /// set doesn't have such a property or the property has no value.
  static func value(of property: String, in set: CSSearchableItemAttributeSet) -> Any? {
    guard set.responds(to: NSSelectorFromString(property)) else {
      return nil
    }
    return set.value(forKey: property)
  }

  private static let table: [(String, SearchableItemAttribute, String)] = [
    ("display-name", .displayName, "displayName"),
    ("alternate-names", .alternateNames, "alternateNames"),
    ("path", .path, "path"),
    ("content-url", .contentURL, "contentURL"),
    ("thumbnail-url", .thumbnailURL, "thumbnailURL"),
    ("thumbnail-data", .thumbnailData, "thumbnailData"),
    ("dark-thumbnail-url", .darkThumbnailURL, "darkThumbnailURL"),
    ("related-unique-identifier", .relatedUniqueIdentifier, "relatedUniqueIdentifier"),
    ("weak-related-unique-identifier", .weakRelatedUniqueIdentifier, "weakRelatedUniqueIdentifier"),
    ("metadata-modification-date", .metadataModificationDate, "metadataModificationDate"),
    ("content-type", .contentType, "contentType"),
    ("content-type-tree", .contentTypeTree, "contentTypeTree"),
    ("keywords", .keywords, "keywords"),
    ("title", .title, "title"),
    ("text-content", .textContent, "textContent"),
    ("version", .version, "version"),
    ("user-created", .userCreated, "userCreated"),
    ("user-owned", .userOwned, "userOwned"),
    ("user-curated", .userCurated, "userCurated"),
    ("ranking-hint", .rankingHint, "rankingHint"),
    ("domain-identifier", .domainIdentifier, "domainIdentifier"),
    ("supports-phone-call", .supportsPhoneCall, "supportsPhoneCall"),
    ("supports-navigation", .supportsNavigation, "supportsNavigation"),
    ("container-title", .containerTitle, "containerTitle"),
    ("container-display-name", .containerDisplayName, "containerDisplayName"),
    ("container-identifier", .containerIdentifier, "containerIdentifier"),
    ("container-order", .containerOrder, "containerOrder"),
    ("subject", .subject, "subject"),
    ("theme", .theme, "theme"),
    ("content-description", .contentDescription, "contentDescription"),
    ("identifier", .identifier, "identifier"),
    ("audiences", .audiences, "audiences"),
    ("file-size", .fileSize, "fileSize"),
    ("page-count", .pageCount, "pageCount"),
    ("page-width", .pageWidth, "pageWidth"),
    ("page-height", .pageHeight, "pageHeight"),
    ("security-method", .securityMethod, "securityMethod"),
    ("creator", .creator, "creator"),
    ("encoding-applications", .encodingApplications, "encodingApplications"),
    ("kind", .kind, "kind"),
    ("font-names", .fontNames, "fontNames"),
    ("due-date", .dueDate, "dueDate"),
    ("completion-date", .completionDate, "completionDate"),
    ("start-date", .startDate, "startDate"),
    ("end-date", .endDate, "endDate"),
    ("important-dates", .importantDates, "importantDates"),
    ("all-day", .allDay, "allDay"),
    ("account-identifier", .accountIdentifier, "accountIdentifier"),
    ("account-handles", .accountHandles, "accountHandles"),
    ("mailbox-identifiers", .mailboxIdentifiers, "mailboxIdentifiers"),
    ("author-names", .authorNames, "authorNames"),
    ("recipient-names", .recipientNames, "recipientNames"),
    ("author-email-addresses", .authorEmailAddresses, "authorEmailAddresses"),
    ("recipient-email-addresses", .recipientEmailAddresses, "recipientEmailAddresses"),
    ("author-addresses", .authorAddresses, "authorAddresses"),
    ("recipient-addresses", .recipientAddresses, "recipientAddresses"),
    ("phone-numbers", .phoneNumbers, "phoneNumbers"),
    ("email-addresses", .emailAddresses, "emailAddresses"),
    ("instant-message-addresses", .instantMessageAddresses, "instantMessageAddresses"),
    ("likely-junk", .likelyJunk, "likelyJunk"),
    ("editors", .editors, "editors"),
    ("participants", .participants, "participants"),
    ("projects", .projects, "projects"),
    ("content-sources", .contentSources, "contentSources"),
    ("comment", .comment, "comment"),
    ("copyright", .copyright, "copyright"),
    ("last-used-date", .lastUsedDate, "lastUsedDate"),
    ("content-creation-date", .contentCreationDate, "contentCreationDate"),
    ("content-modification-date", .contentModificationDate, "contentModificationDate"),
    ("added-date", .addedDate, "addedDate"),
    ("downloaded-date", .downloadedDate, "downloadedDate"),
    ("duration", .duration, "duration"),
    ("contact-keywords", .contactKeywords, "contactKeywords"),
    ("codecs", .codecs, "codecs"),
    ("media-types", .mediaTypes, "mediaTypes"),
    ("streamable", .streamable, "streamable"),
    ("total-bit-rate", .totalBitRate, "totalBitRate"),
    ("video-bit-rate", .videoBitRate, "videoBitRate"),
    ("audio-bit-rate", .audioBitRate, "audioBitRate"),
    ("delivery-type", .deliveryType, "deliveryType"),
    ("organizations", .organizations, "organizations"),
    ("role", .role, "role"),
    ("languages", .languages, "languages"),
    ("rights", .rights, "rights"),
    ("publishers", .publishers, "publishers"),
    ("contributors", .contributors, "contributors"),
    ("coverage", .coverage, "coverage"),
    ("rating", .rating, "rating"),
    ("rating-description", .ratingDescription, "ratingDescription"),
    ("play-count", .playCount, "playCount"),
    ("information", .information, "information"),
    ("director", .director, "director"),
    ("producer", .producer, "producer"),
    ("genre", .genre, "genre"),
    ("performers", .performers, "performers"),
    ("original-format", .originalFormat, "originalFormat"),
    ("original-source", .originalSource, "originalSource"),
    ("local", .local, "local"),
    ("content-rating", .contentRating, "contentRating"),
    ("url", .url, "url"),
    ("audio-sample-rate", .audioSampleRate, "audioSampleRate"),
    ("audio-channel-count", .audioChannelCount, "audioChannelCount"),
    ("tempo", .tempo, "tempo"),
    ("key-signature", .keySignature, "keySignature"),
    ("time-signature", .timeSignature, "timeSignature"),
    ("audio-encoding-application", .audioEncodingApplication, "audioEncodingApplication"),
    ("composer", .composer, "composer"),
    ("lyricist", .lyricist, "lyricist"),
    ("album", .album, "album"),
    ("artist", .artist, "artist"),
    ("audio-track-number", .audioTrackNumber, "audioTrackNumber"),
    ("recording-date", .recordingDate, "recordingDate"),
    ("musical-genre", .musicalGenre, "musicalGenre"),
    ("general-midi-sequence", .generalMIDISequence, "generalMIDISequence"),
    ("musical-instrument-category", .musicalInstrumentCategory, "musicalInstrumentCategory"),
    ("musical-instrument-name", .musicalInstrumentName, "musicalInstrumentName"),
    ("pixel-height", .pixelHeight, "pixelHeight"),
    ("pixel-width", .pixelWidth, "pixelWidth"),
    ("pixel-count", .pixelCount, "pixelCount"),
    ("color-space", .colorSpace, "colorSpace"),
    ("bits-per-sample", .bitsPerSample, "bitsPerSample"),
    ("flash-on", .flashOn, "flashOn"),
    ("focal-length", .focalLength, "focalLength"),
    ("focal-length-35mm-equivalent", .focalLength35mmEquivalent, "focalLength35mmEquivalent"),
    ("acquisition-make", .acquisitionMake, "acquisitionMake"),
    ("acquisition-model", .acquisitionModel, "acquisitionModel"),
    ("camera-owner", .cameraOwner, "cameraOwner"),
    ("lens-model", .lensModel, "lensModel"),
    ("iso-speed", .isoSpeed, "isoSpeed"),
    ("orientation", .orientation, "orientation"),
    ("layer-names", .layerNames, "layerNames"),
    ("white-balance", .whiteBalance, "whiteBalance"),
    ("aperture", .aperture, "aperture"),
    ("profile-name", .profileName, "profileName"),
    ("resolution-width-dpi", .resolutionWidthDPI, "resolutionWidthDPI"),
    ("resolution-height-dpi", .resolutionHeightDPI, "resolutionHeightDPI"),
    ("exposure-mode", .exposureMode, "exposureMode"),
    ("exposure-time", .exposureTime, "exposureTime"),
    ("exif-version", .exifVersion, "exifVersion"),
    ("exif-gps-version", .exifGPSVersion, "exifGPSVersion"),
    ("has-alpha-channel", .hasAlphaChannel, "hasAlphaChannel"),
    ("red-eye-on", .redEyeOn, "redEyeOn"),
    ("metering-mode", .meteringMode, "meteringMode"),
    ("maximum-aperture", .maximumAperture, "maximumAperture"),
    ("f-number", .fNumber, "fNumber"),
    ("exposure-program", .exposureProgram, "exposureProgram"),
    ("exposure-time-string", .exposureTimeString, "exposureTimeString"),
    ("headline", .headline, "headline"),
    ("instructions", .instructions, "instructions"),
    ("thoroughfare", .thoroughfare, "thoroughfare"),
    ("sub-thoroughfare", .subThoroughfare, "subThoroughfare"),
    ("postal-code", .postalCode, "postalCode"),
    ("city", .city, "city"),
    ("state-or-province", .stateOrProvince, "stateOrProvince"),
    ("country", .country, "country"),
    ("fully-formatted-address", .fullyFormattedAddress, "fullyFormattedAddress"),
    ("altitude", .altitude, "altitude"),
    ("latitude", .latitude, "latitude"),
    ("longitude", .longitude, "longitude"),
    ("speed", .speed, "speed"),
    ("timestamp", .timestamp, "timestamp"),
    ("image-direction", .imageDirection, "imageDirection"),
    ("named-location", .namedLocation, "namedLocation"),
    ("gps-track", .gpsTrack, "gpsTrack"),
    ("gps-status", .gpsStatus, "gpsStatus"),
    ("gps-measure-mode", .gpsMeasureMode, "gpsMeasureMode"),
    ("gps-dilution-of-precision", .gpsDilutionOfPrecision, "gpsDilutionOfPrecision"),
    ("gps-map-datum", .gpsMapDatum, "gpsMapDatum"),
    ("gps-destination-latitude", .gpsDestinationLatitude, "gpsDestinationLatitude"),
    ("gps-destination-longitude", .gpsDestinationLongitude, "gpsDestinationLongitude"),
    ("gps-destination-bearing", .gpsDestinationBearing, "gpsDestinationBearing"),
    ("gps-destination-distance", .gpsDestinationDistance, "gpsDestinationDistance"),
    ("gps-processing-method", .gpsProcessingMethod, "gpsProcessingMethod"),
    ("gps-area-information", .gpsAreaInformation, "gpsAreaInformation"),
    ("gps-date-stamp", .gpsDateStamp, "gpsDateStamp"),
    ("gps-differential", .gpsDifferential, "gpsDifferential"),
  ]
}

#endif
