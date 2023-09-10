//
//  TarArchiveLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 01/09/2023.
//  Copyright © 2023 ObjectHub. All rights reserved.
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
import SWCompression

public final class TarArchiveLibrary: NativeLibrary {
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    try super.init(in: context)
  }

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "archive", "tar"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"], "load")
  }

  /// Declarations of the library.
  public override func declarations() {
    // Constructors
    self.define(Procedure("make-tar-archive", self.makeTarArchive))
    
    // Reading 
    self.define(Procedure("load-tar-archive", self.loadTarArchive))
    self.define(Procedure("save-tar-archive", self.saveTarArchive))
    
    // Archive-level data
    self.define("tar-archive-type-tag", as: TarArchive.type.objectTypeTag())
    self.define(Procedure("tar-archive?", self.isTarArchive))
    self.define(Procedure("tar-archive-path", self.tarArchivePath))
    self.define(Procedure("tar-archive-bytevector", self.tarArchiveBytevector))
    
    // Looking up entries
    self.define(Procedure("tar-entry-count", self.tarEntryCount))
    self.define(Procedure("tar-entries", self.tarEntries))
    self.define(Procedure("tar-entry-exists?", self.tarEntryExists))
    self.define(Procedure("tar-entry-file?", self.tarEntryFile))
    self.define(Procedure("tar-entry-directory?", self.tarEntryDirectory))
    self.define(Procedure("tar-entry-symlink?", self.tarEntrySymLink))
    self.define(Procedure("tar-entry-linked", self.tarEntryLinked))
    self.define(Procedure("tar-entry-creation-date", self.tarEntryCreationDate))
    self.define(Procedure("tar-entry-modification-date", self.tarEntryModificationDate))
    self.define(Procedure("tar-entry-permissions", self.tarEntryPermissions))
    self.define(Procedure("tar-entry-size", self.tarEntrySize))
    self.define(Procedure("tar-entry-data", self.tarEntryData))
    
    // Adding and removing entries
    self.define(Procedure("add-tar-entry!", self.addTarEntry))
    self.define(Procedure("set-tar-entry!", self.setTarEntry))
    self.define(Procedure("delete-tar-entry!", self.deleteTarEntry))
    
    // Extracting entries
    self.define(Procedure("extract-tar-entry", self.extractTarEntry))
    self.define(Procedure("get-tar-entry", self.getTarEntry))
  }
  
  private func url(from path: Expr) throws -> URL {
    return URL(fileURLWithPath:
                self.context.fileHandler.path(
                  try path.asPath(), relativeTo: self.context.evaluator.currentDirectoryPath))
  }
  
  private func url(from url: URL) throws -> URL {
    return URL(fileURLWithPath:
                self.context.fileHandler.path(
                  url.absoluteURL.path, relativeTo: self.context.evaluator.currentDirectoryPath))
  }
  
  private func archive(from expr: Expr) throws -> TarArchive {
    guard case .object(let obj) = expr, let archive = obj as? TarArchive else {
      throw RuntimeError.type(expr, expected: [TarArchive.type])
    }
    return archive
  }
  
  private func entry(from archive: Expr, at path: Expr) throws -> TarEntry {
    guard let entry = try self.archive(from: archive).get(path: try path.asString()) else {
      throw RuntimeError.eval(.tarArchiveEntryDoesNotExist, archive, path)
    }
    return entry
  }
  
  private func makeTarArchive(_ args: Arguments) throws -> Expr {
    if let bvec = args.first {
      let bvector = try BytevectorLibrary.subVector(
                          "make-tar-archive",
                          bvec,
                          args[args.index(after: args.startIndex)..<args.endIndex])
      let entries = try TarContainer.open(container: Data(bvector))
      return .object(TarArchive(entries: entries))
    } else {
      return .object(TarArchive(entries: []))
    }
  }
  
  private func loadTarArchive(_ path: Expr) throws -> Expr {
    let url = try self.url(from: path)
    let entries = try TarContainer.open(container: try Data(contentsOf: url))
    return .object(TarArchive(url: url, entries: entries))
  }
  
  private func saveTarArchive(_ expr: Expr, _ path: Expr?) throws -> Expr {
    let archive = try self.archive(from: expr)
    let data = TarContainer.create(from: archive.entries)
    if let path = path {
      try data.write(to: self.url(from: path))
    } else if let url = archive.url {
      try data.write(to: url)
    } else {
      throw RuntimeError.eval(.cannotWriteTarArchive, expr)
    }
    return .void
  }
  
  private func isTarArchive(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is TarArchive {
      return .true
    }
    return .false
  }
  
  private func tarArchivePath(_ expr: Expr) throws -> Expr {
    guard let path = try self.archive(from: expr).url?.path else {
      return .false
    }
    return .makeString(path)
  }
  
  private func tarArchiveBytevector(_ expr: Expr) throws -> Expr {
    let archive = try self.archive(from: expr)
    let data = TarContainer.create(from: archive.entries) as NSData
    var res = [UInt8](repeating: 0, count: data.count)
    data.getBytes(&res, length: data.count)
    return .bytes(MutableBox(res))
  }
  
  private func tarEntryCount(_ expr: Expr) throws -> Expr {
    return .makeNumber(try self.archive(from: expr).entries.count)
  }
  
  private func tarEntries(_ expr: Expr, _ prefixPath: Expr?) throws -> Expr {
    let archive = try self.archive(from: expr)
    var res = Exprs()
    if let prefixPath = prefixPath {
      let prefix = try prefixPath.asString()
      for entry in archive.entries {
        if entry.info.name.hasPrefix(prefix) {
          res.append(.makeString(entry.info.name))
        }
      }
    } else {
      for entry in archive.entries {
        res.append(.makeString(entry.info.name))
      }
    }
    return .makeList(res)
  }
  
  private func tarEntryExists(_ expr: Expr, _ p: Expr, _ prefix: Expr?) throws -> Expr {
    let archive = try self.archive(from: expr)
    let path = try p.asString()
    for entry in archive.entries {
      if prefix?.isTrue ?? false {
        if entry.info.name.hasPrefix(path) {
          return .true
        }
      } else if entry.info.name == path {
        return .true
      }
    }
    return .false
  }
  
  private func tarEntryFile(_ expr: Expr, _ path: Expr) throws -> Expr {
    let entry = try self.entry(from: expr, at: path)
    return .makeBoolean(entry.info.type == .regular)
  }
  
  private func tarEntryDirectory(_ expr: Expr, _ path: Expr) throws -> Expr {
    let entry = try self.entry(from: expr, at: path)
    return .makeBoolean(entry.info.type == .directory)
  }
  
  private func tarEntrySymLink(_ expr: Expr, _ path: Expr) throws -> Expr {
    let entry = try self.entry(from: expr, at: path)
    return .makeBoolean(entry.info.type == .symbolicLink)
  }
  
  private func tarEntryLinked(_ expr: Expr, _ path: Expr) throws -> Expr {
    let entry = try self.entry(from: expr, at: path)
    if entry.info.type == .symbolicLink && !entry.info.linkName.isEmpty {
      return .makeString(entry.info.linkName)
    } else {
      return .false
    }
  }
  
  private func tarEntryCreationDate(_ expr: Expr, _ path: Expr) throws -> Expr {
    let entry = try self.entry(from: expr, at: path)
    if let date = entry.info.creationTime {
      return .object(
        NativeDateTime(DateTimeLibrary.calendar.dateComponents(in: TimeZone.current, from: date)))
    } else {
      return .false
    }
  }
  
  private func tarEntryModificationDate(_ expr: Expr, _ path: Expr) throws -> Expr {
    let entry = try self.entry(from: expr, at: path)
    if let date = entry.info.modificationTime ?? entry.info.creationTime {
      return .object(
        NativeDateTime(DateTimeLibrary.calendar.dateComponents(in: TimeZone.current, from: date)))
    } else {
      return .false
    }
  }
  
  private func tarEntryPermissions(_ expr: Expr, _ path: Expr) throws -> Expr {
    let entry = try self.entry(from: expr, at: path)
    if let permissions = entry.info.permissions?.rawValue, let p = Int64(exactly: permissions) {
      return .fixnum(p)
    } else {
      return .false
    }
  }
  
  private func tarEntrySize(_ expr: Expr, _ path: Expr) throws -> Expr {
    let entry = try self.entry(from: expr, at: path)
    if let size = entry.info.size ?? entry.data?.count {
      return .makeNumber(size)
    } else {
      return .false
    }
  }
  
  private func tarEntryData(_ expr: Expr, _ path: Expr) throws -> Expr {
    let entry = try self.entry(from: expr, at: path)
    if let data = entry.data as NSData? {
      var res = [UInt8](repeating: 0, count: data.count)
      data.getBytes(&res, length: data.count)
      return .bytes(MutableBox(res))
    } else {
      return .false
    }
  }
  
  private func addTarEntry(_ expr: Expr,
                           _ path: Expr,
                           _ source: Expr,
                           _ rec: Expr?) throws -> Expr {
    let archive = try self.archive(from: expr)
    let skipped = archive.add(entries: try self.createTarEntries(for: try path.asString(),
                                                                 at: try self.url(from: source),
                                                                 rec: rec?.isTrue ?? true))
    var res = Expr.null
    for path in skipped {
      res = .pair(.makeString(path), res)
    }
    return res
  }
  
  private func directoryName(_ path: String) -> String {
    return path.hasSuffix("/") ? path : path + "/"
  }
  
  public func createTarEntries(for path: String,
                               at source: URL,
                               rec recursively: Bool = false,
                               modificationDate modDate: Date? = nil) throws -> [TarEntry] {
    let fileManager = self.context.fileHandler.fileManager
    let fileAttributes = try fileManager.attributesOfItem(atPath: source.absoluteURL.path)
    var name = path
    let entryType: ContainerEntryType
    if let typeFromAttributes = fileAttributes[.type] as? FileAttributeType {
      switch typeFromAttributes {
        case .typeBlockSpecial:
          entryType = .blockSpecial
        case .typeCharacterSpecial:
          entryType = .characterSpecial
        case .typeDirectory:
          name = self.directoryName(name)
          entryType = .directory
        case .typeRegular:
          entryType = .regular
        case .typeSocket:
          entryType = .socket
        case .typeSymbolicLink:
          entryType = .symbolicLink
        case .typeUnknown:
          entryType = .unknown
        default:
          entryType = .unknown
      }
    } else {
      entryType = .unknown
    }
    var info = TarEntryInfo(name: name, type: entryType)
    info.creationTime = fileAttributes[.creationDate] as? Date
    info.groupID = (fileAttributes[.groupOwnerAccountID] as? NSNumber)?.intValue
    info.ownerGroupName = fileAttributes[.groupOwnerAccountName] as? String
    info.modificationTime = modDate ?? (fileAttributes[.modificationDate] as? Date)
    info.ownerID = (fileAttributes[.ownerAccountID] as? NSNumber)?.intValue
    info.ownerUserName = fileAttributes[.ownerAccountName] as? String
    if let posixPermissions = (fileAttributes[.posixPermissions] as? NSNumber)?.intValue {
      info.permissions = Permissions(rawValue: UInt32(truncatingIfNeeded: posixPermissions))
    }
    let entryData: Data?
    if entryType == .symbolicLink {
      entryData = nil
      info.linkName = try fileManager.destinationOfSymbolicLink(atPath: source.absoluteURL.path)
    } else if entryType != .directory {
      entryData = try Data(contentsOf: source)
    } else {
      entryData = nil
    }
    let entry = TarEntry(info: info, data: entryData)
    var entries = [TarEntry]()
    entries.append(entry)
    if entryType == .directory && recursively {
      for subPath in try fileManager.contentsOfDirectory(atPath: source.absoluteURL.path) {
        entries.append(contentsOf:
          try self.createTarEntries(for: (path as NSString).appendingPathComponent(subPath),
                                    at: source.appendingPathComponent(subPath),
                                    modificationDate: modDate))
      }
    }
    return entries
  }
  
  private func attributes(for entry: TarEntry, directory: Bool? = nil) -> [FileAttributeKey : Any]? {
    var attributes: [FileAttributeKey : Any] = [:]
    if let directory = directory {
      if directory {
        attributes[.type] = FileAttributeType.typeDirectory
      } else {
        attributes[.type] = FileAttributeType.typeRegular
      }
    }
    if let creationDate = entry.info.creationTime {
      attributes[.creationDate] = creationDate as NSDate
    }
    if let modificationDate = entry.info.modificationTime {
      attributes[.modificationDate] = modificationDate as NSDate
    }
    if let permissions = entry.info.permissions {
      attributes[.posixPermissions] = permissions.rawValue
    } else {
      attributes[.posixPermissions] = 0o777
    }
    if let ownerId = entry.info.ownerID {
      attributes[.ownerAccountID] = NSNumber(integerLiteral: ownerId)
    }
    if let ownerName = entry.info.ownerUserName {
      attributes[.ownerAccountName] = ownerName as NSString
    }
    if let groupId = entry.info.groupID {
      attributes[.groupOwnerAccountID] = NSNumber(integerLiteral: groupId)
    }
    if let groupName = entry.info.ownerGroupName {
      attributes[.groupOwnerAccountName] = groupName as NSString
    }
    if attributes.isEmpty {
      return nil
    } else {
      return attributes
    }
  }
  
  private func setTarEntry(_ expr: Expr,
                           _ path: Expr,
                           _ bvec: Expr,
                           _ modDate: Expr?,
                           _ perm: Expr?) throws -> Expr {
    let archive = try self.archive(from: expr)
    let name = try path.asString()
    let creationDate = Date()
    let modificationDate: Date
    if let modDate = modDate, modDate.isTrue {
      guard case .object(let obj) = modDate,
            let box = obj as? NativeDateTime,
            let d = box.value.date else {
        throw RuntimeError.type(modDate, expected: [NativeDateTime.type])
      }
      modificationDate = d
    } else {
      modificationDate = creationDate
    }
    let permissions: UInt32?
    if let perm = perm {
      permissions = UInt32(exactly: try perm.asInt(above: 0, below: 0o777 + 1))
    } else {
      permissions = nil
    }
    let entry: TarEntry
    switch bvec {
      case .bytes(let bvec):
        var info = TarEntryInfo(name: name, type: .regular)
        info.creationTime = creationDate
        info.modificationTime = modificationDate
        info.permissions = Permissions(rawValue: permissions ?? 0o644)
        entry = TarEntry(info: info, data: Data(bvec.value))
      case .string(_):
        var info = TarEntryInfo(name: name, type: .symbolicLink)
        info.creationTime = creationDate
        info.modificationTime = modificationDate
        info.permissions = Permissions(rawValue: permissions ?? 0o755)
        info.linkName = try bvec.asPath()
        entry = TarEntry(info: info, data: nil)
      case .null:
        var info = TarEntryInfo(name: self.directoryName(name), type: .directory)
        info.creationTime = creationDate
        info.modificationTime = modificationDate
        info.permissions = Permissions(rawValue: permissions ?? 0o755)
        entry = TarEntry(info: info, data: nil)
      default:
        throw RuntimeError.type(bvec, expected: [.nullType, .strType, .byteVectorType])
    }
    if archive.add(entry: entry) {
      return .null
    } else {
      return .pair(.makeString(name), .null)
    }
  }
  
  private func deleteTarEntry(_ expr: Expr, _ path: Expr, _ prefix: Expr?) throws -> Expr {
    let archive = try self.archive(from: expr)
    archive.delete(path: try path.asString(), prefix: prefix?.isTrue ?? false)
    return .void
  }
  
  private func extractTarEntry(_ expr: Expr,
                               _ path: Expr,
                               _ dest: Expr,
                               _ prefix: Expr?) throws -> Expr {
    let archive = try self.archive(from: expr)
    let destination = try self.url(from: dest)
    var entries: [TarEntry] = []
    if prefix?.isTrue ?? false {
      entries.append(contentsOf: archive.get(prefix: try path.asString()))
    } else {
      guard let entry = archive.get(path: try path.asString()) else {
        return .false
      }
      entries.append(entry)
    }
    var succ = Expr.null
    var fail = Expr.null
    for entry in entries {
      switch entry.info.type {
        case .regular:
          guard let data = entry.data else {
            return .false
          }
          let fileUrl = destination.appendingPathComponent(entry.info.name)
          let dirUrl = fileUrl.deletingLastPathComponent()
          if !self.context.fileHandler.isDirectory(atPath: dirUrl.absoluteURL.path) {
            try self.context.fileHandler.fileManager.createDirectory(
                  at: dirUrl,
                  withIntermediateDirectories: true,
                  attributes: self.attributes(for: entry, directory: true))
          }
          if self.context.fileHandler.fileManager.createFile(
              atPath: fileUrl.absoluteURL.path,
              contents: data,
              attributes: self.attributes(for: entry, directory: false)) {
            succ = .pair(.makeString(entry.info.name), succ)
          } else {
            fail = .pair(.makeString(entry.info.name), fail)
          }
        case .symbolicLink:
          guard !entry.info.linkName.isEmpty else {
            return .false
          }
          let link = destination.appendingPathComponent(entry.info.name)
          let dest = URL(fileURLWithPath: entry.info.linkName, relativeTo: destination)
          try self.context.fileHandler.fileManager.createSymbolicLink(at: link,
                                                                      withDestinationURL: dest)
          succ = .pair(.makeString(entry.info.name), succ)
        case .directory:
          let dir = destination.appendingPathComponent(entry.info.name)
          try self.context.fileHandler.fileManager.createDirectory(
                at: dir,
                withIntermediateDirectories: true,
                attributes: self.attributes(for: entry))
          succ = .pair(.makeString(entry.info.name), succ)
        default:
          fail = .pair(.makeString(entry.info.name), fail)
      }
    }
    return .values(.pair(succ, .pair(fail, .null)))
  }
  
  private func getTarEntry(_ expr: Expr, _ path: Expr) throws -> Expr {
    let entry = try self.entry(from: expr, at: path)
    switch entry.info.type {
      case .regular:
        guard let data = entry.data else {
          return .true
        }
        let count = data.count
        var arr = [UInt8](repeating: 0, count: count)
        data.copyBytes(to: &arr, count: count)
        return .bytes(MutableBox(arr))
      case .symbolicLink:
        guard !entry.info.linkName.isEmpty else {
          return .true
        }
        return .makeString(entry.info.linkName)
      case .directory:
        return .null
      default:
        return .true
    }
  }
}

public final class TarArchive: NativeObject {

  /// Type representing tar archives
  public static let type = Type.objectType(Symbol(uninterned: "tar-archive"))
  
  /// The URL of this archive
  public let url: URL?
  
  /// The tar entries in this archive
  public private(set) var entries: [TarEntry]
  
  /// Initializer
  public init(url: URL? = nil, entries: [TarEntry]) {
    self.url = url
    self.entries = entries
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    if let url = self.url {
      return "#<tar-archive \(url.path)>"
    } else {
      return super.string
    }
  }
  
  public func get(path: String) -> TarEntry? {
    for entry in self.entries {
      if entry.info.name == path {
        return entry
      }
    }
    return nil
  }
  
  public func get(prefix: String) -> [TarEntry] {
    var res: [TarEntry] = []
    for entry in self.entries {
      if entry.info.name.hasPrefix(prefix) {
        res.append(entry)
      }
    }
    return res
  }
  
  public func add(entry: TarEntry) -> Bool {
    if self.get(path: entry.info.name) == nil {
      self.entries.append(entry)
      return true
    } else {
      return false
    }
  }
  
  public func add(entries: [TarEntry]) -> [String] {
    var res: [String] = []
    for entry in entries {
      if !self.add(entry: entry) {
        res.append(entry.info.name)
      }
    }
    return res
  }
  
  public func delete(path: String, prefix: Bool = false) {
    self.entries.removeAll { entry in
      return prefix && entry.info.name.hasPrefix(path) || !prefix && (entry.info.name == path)
    }
  }
  
  public func delete(prefix: String) {
    self.entries.removeAll { entry in
      return entry.info.name.hasPrefix(prefix)
    }
  }
}
