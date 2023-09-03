//
//  ZipArchiveLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 15/01/2021.
//  Copyright © 2021 ObjectHub. All rights reserved.
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
    self.define(Procedure("read-tar-archive", self.readTarArchive))
    self.define(Procedure("write-tar-archive", self.writeTarArchive))
    
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
    self.define(Procedure("tar-entry-mod-date", self.tarEntryModificationDate))
    self.define(Procedure("tar-entry-size", self.tarEntrySize))
    self.define(Procedure("tar-entry-data", self.tarEntryData))
    
    // Extracting entries
    /*
    self.define(Procedure("read-zip-entry", self.readZipEntry))
    
    // Adding entries
    self.define(Procedure("add-zip-entry", self.addZipEntry))
    self.define(Procedure("write-zip-entry", self.writeZipEntry))
    
    // Deleting entries
    self.define(Procedure("delete-zip-entry", self.deleteZipEntry))
    */
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
    guard let entry = try self.archive(from: archive).get(try path.asString()) else {
      throw RuntimeError.eval(.zipArchiveEntryDoesNotExist, archive, path) // TODO: fix
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
  
  private func readTarArchive(_ path: Expr) throws -> Expr {
    let url = try self.url(from: path)
    let data = try Data(contentsOf: url)
    let entries = try TarContainer.open(container: data)
    return .object(TarArchive(url: url, entries: entries))
  }
  
  private func writeTarArchive(_ expr: Expr, _ path: Expr?) throws -> Expr {
    let archive = try self.archive(from: expr)
    let data = TarContainer.create(from: archive.entries)
    if let path = path {
      try data.write(to: self.url(from: path))
    } else if let url = archive.url {
      try data.write(to: url)
    } else {
      return .false // TODO: throw error
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
  
  private func tarEntries(_ expr: Expr) throws -> Expr {
    let archive = try self.archive(from: expr)
    var res = Exprs()
    for entry in archive.entries {
      res.append(.makeString(entry.info.name))
    }
    return .makeList(res)
  }
  
  private func tarEntryExists(_ expr: Expr, _ p: Expr) throws -> Expr {
    let archive = try self.archive(from: expr)
    let path = try p.asString()
    for entry in archive.entries {
      if entry.info.name == path {
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
  
  private func tarEntryModificationDate(_ expr: Expr, _ path: Expr) throws -> Expr {
    let entry = try self.entry(from: expr, at: path)
    if let date = entry.info.modificationTime ?? entry.info.creationTime {
      return .object(
        NativeDateTime(DateTimeLibrary.calendar.dateComponents(in: TimeZone.current, from: date)))
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
}

public final class TarArchive: NativeObject {

  /// Type representing zip archives
  public static let type = Type.objectType(Symbol(uninterned: "tar-archive"))
  
  /// The URL of this archive
  public let url: URL?
  
  /// The tar entries in this archive
  public let entries: [TarEntry]
  
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
  
  public func get(_ path: String) -> TarEntry? {
    for entry in self.entries {
      if entry.info.name == path {
        return entry
      }
    }
    return nil
  }
}
