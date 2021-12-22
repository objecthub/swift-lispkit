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
import ZIPFoundation

public final class ZipArchiveLibrary: NativeLibrary {
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    try super.init(in: context)
  }

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "archive", "zip"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"], "load")
  }

  /// Declarations of the library.
  public override func declarations() {
    // Constructors
    self.define(Procedure("make-zip-archive", self.makeZipArchive))
    self.define(Procedure("create-zip-archive", self.createZipArchive))
    self.define(Procedure("open-zip-archive", self.openZipArchive))
    
    // Archive-level data
    self.define(Procedure("zip-archive?", self.isZipArchive))
    self.define(Procedure("zip-archive-mutable?", self.zipArchiveMutable))
    self.define(Procedure("zip-archive-path", self.zipArchivePath))
    self.define(Procedure("zip-archive-bytevector", self.zipArchiveBytevector))
    
    // Looking up entries
    self.define(Procedure("zip-entry-count", self.zipEntryCount))
    self.define(Procedure("zip-entries", self.zipEntries))
    self.define(Procedure("zip-entry-exists?", self.zipEntryExists))
    self.define(Procedure("zip-entry-compressed?", self.zipEntryCompressed))
    self.define(Procedure("zip-entry-file?", self.zipEntryFile))
    self.define(Procedure("zip-entry-directory?", self.zipEntryDirectory))
    self.define(Procedure("zip-entry-symlink?", self.zipEntrySymLink))
    self.define(Procedure("zip-entry-compressed-size", self.zipArchiveEntryCompSize))
    self.define(Procedure("zip-entry-uncompressed-size", self.zipArchiveEntryUncompSize))
    
    // Extracting entries
    self.define(Procedure("extract-zip-entry", self.extractZipEntry))
    self.define(Procedure("read-zip-entry", self.readZipEntry))
    
    // Adding entries
    self.define(Procedure("add-zip-entry", self.addZipEntry))
    self.define(Procedure("write-zip-entry", self.writeZipEntry))
    
    // Deleting entries
    self.define(Procedure("delete-zip-entry", self.deleteZipEntry))
  }
  
  private func url(from path: Expr) throws -> URL {
    return URL(fileURLWithPath:
                self.context.fileHandler.path(
                  try path.asPath(), relativeTo: self.context.evaluator.currentDirectoryPath))
  }
  
  private func archive(from expr: Expr) throws -> Archive {
    guard case .object(let obj) = expr, let zarchive = obj as? ZipArchive else {
      throw RuntimeError.type(expr, expected: [ZipArchive.type])
    }
    return zarchive.archive
  }
  
  private func entry(from zarchive: Expr, at path: Expr) throws -> Entry {
    guard let entry = try self.archive(from: zarchive)[try path.asString()] else {
      throw RuntimeError.eval(.zipArchiveEntryDoesNotExist, zarchive, path)
    }
    return entry
  }
  
  private func makeZipArchive(_ expr: Expr?, _ update: Expr?) throws -> Expr {
    guard let bvector = try expr?.asByteVector() else {
      guard let archive = Archive(accessMode: .create) else {
        throw RuntimeError.eval(.cannotCreateInMemoryZipArchive)
      }
      return .object(ZipArchive(archive))
    }
    guard let archive = Archive(data: Data(bvector.value),
                                accessMode: (update ?? .false).isTrue ? .update : .read) else {
      throw RuntimeError.eval(.cannotMakeInMemoryZipArchiveFromData, expr!)
    }
    return .object(ZipArchive(archive))
  }
  
  private func createZipArchive(_ expr: Expr) throws -> Expr {
    guard let archive = Archive(url: try self.url(from: expr), accessMode: .create) else {
      throw RuntimeError.eval(.cannotCreateZipArchive, expr)
    }
    return .object(ZipArchive(archive))
  }
  
  private func openZipArchive(_ expr: Expr, _ update: Expr?) throws -> Expr {
    guard let archive = Archive(url: try self.url(from: expr),
                                accessMode: (update ?? .false).isTrue ? .update : .read) else {
      throw RuntimeError.eval(.cannotOpenZipArchive, expr)
    }
    return .object(ZipArchive(archive))
  }
  
  private func isZipArchive(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is ZipArchive {
      return .true
    }
    return .false
  }
  
  private func zipArchiveMutable(_ expr: Expr) throws -> Expr {
    return .makeBoolean(try self.archive(from: expr).accessMode != .read)
  }
  
  private func zipArchivePath(_ expr: Expr) throws -> Expr {
    let path = try self.archive(from: expr).url.absoluteURL.path
    return path.isEmpty ? .false : .makeString(try self.archive(from: expr).url.absoluteURL.path)
  }

  private func zipArchiveBytevector(_ expr: Expr) throws -> Expr {
    guard let data = try self.archive(from: expr).data as NSData? else {
      return .false
    }
    var res = [UInt8](repeating: 0, count: data.count)
    data.getBytes(&res, length: data.count)
    return .bytes(MutableBox(res))
  }
  
  private func zipEntryCount(_ expr: Expr) throws -> Expr {
    let archive = try self.archive(from: expr)
    var n = 0
    for _ in archive {
      n += 1
    }
    return .makeNumber(n)
  }
  
  private func zipEntries(_ expr: Expr) throws -> Expr {
    let archive = try self.archive(from: expr)
    var res = Exprs()
    for entry in archive {
      res.append(.makeString(entry.path))
    }
    return .makeList(res)
  }
  
  private func zipEntryExists(_ expr: Expr, _ p: Expr) throws -> Expr {
    let archive = try self.archive(from: expr)
    let path = try p.asString()
    for entry in archive {
      if entry.path == path {
        return .true
      }
    }
    return .false
  }
  
  private func zipEntryCompressed(_ expr: Expr, _ path: Expr) throws -> Expr {
    let entry = try self.entry(from: expr, at: path)
    return .makeBoolean(entry.compressedSize < entry.uncompressedSize)
  }
  
  private func zipEntryFile(_ expr: Expr, _ path: Expr) throws -> Expr {
    return .makeBoolean(try self.entry(from: expr, at: path).type == .file)
  }
  
  private func zipEntryDirectory(_ expr: Expr, _ path: Expr) throws -> Expr {
    return .makeBoolean(try self.entry(from: expr, at: path).type == .directory)
  }
  
  private func zipEntrySymLink(_ expr: Expr, _ path: Expr) throws -> Expr {
    return .makeBoolean(try self.entry(from: expr, at: path).type == .symlink)
  }
  
  private func zipArchiveEntryCompSize(_ expr: Expr, _ path: Expr) throws -> Expr {
    return .makeNumber(try self.entry(from: expr, at: path).compressedSize)
  }
  
  private func zipArchiveEntryUncompSize(_ expr: Expr, _ path: Expr) throws -> Expr {
    return .makeNumber(try self.entry(from: expr, at: path).uncompressedSize)
  }
  
  private func extractZipEntry(_ expr: Expr, _ path: Expr, _ dest: Expr) throws -> Expr {
    let archive = try self.archive(from: expr)
    let zpath = try path.asString()
    guard let entry = archive[zpath] else {
      throw RuntimeError.eval(.zipArchiveEntryDoesNotExist, expr, path)
    }
    _ = try archive.extract(entry,
                            to: URL(fileURLWithPath: zpath, relativeTo: self.url(from: dest)),
                            skipCRC32: true)
    return .void
  }
  
  private func readZipEntry(_ expr: Expr, _ path: Expr) throws -> Expr {
    let archive = try self.archive(from: expr)
    guard let entry = archive[try path.asString()] else {
      throw RuntimeError.eval(.zipArchiveEntryDoesNotExist, expr, path)
    }
    do {
      var data = Data()
      _ = try archive.extract(entry, skipCRC32: false) { newData in
        data.append(newData)
      }
      let count = data.count
      var arr = [UInt8](repeating: 0, count: count)
      data.copyBytes(to: &arr, count: count)
      return .bytes(MutableBox(arr))
    } catch {
      return .false
    }
  }
  
  private func addZipEntry(_ expr: Expr,
                           _ path: Expr,
                           _ source: Expr,
                           _ compressed: Expr?) throws -> Expr {
    let archive = try self.archive(from: expr)
    try archive.addEntry(with: try path.asString(),
                         relativeTo: try self.url(from: source),
                         compressionMethod: (compressed ?? .true).isTrue ? .deflate : .none)
    return .void
  }
  
  private func writeZipEntry(_ expr: Expr,
                             _ path: Expr,
                             _ bvec: Expr,
                             _ compressed: Expr?,
                             _ modDate: Expr?) throws -> Expr {
    let archive = try self.archive(from: expr)
    var date = Date()
    if let modificationDate = modDate {
      guard case .object(let obj) = modificationDate,
            let box = obj as? NativeDateTime,
            let d = box.value.date else {
        throw RuntimeError.type(modificationDate, expected: [NativeDateTime.type])
      }
      date = d
    }
    switch bvec {
      case .bytes(let bvec):
        let data = Data(bvec.value)
        guard let size = UInt32(exactly: data.count) else {
          return .false
        }
        do {
          try archive.addEntry(with: try path.asString(),
                               type: .file,
                               uncompressedSize: size,
                               modificationDate: date,
                               compressionMethod: (compressed ?? .true).isTrue ? .deflate : .none) {
            position, size in data.subdata(in: position..<position+size)
          }
          return .true
        } catch {
          return .false
        }
      case .string(_):
        let sourceUrl = try self.url(from: bvec)
        do {
          try self.addZipEntry(to: archive,
                               with: try path.asString(),
                               from: sourceUrl,
                               compressed: (compressed ?? .true).isTrue,
                               modificationDate: modDate == nil ? nil : date)
          return .true
        } catch {
          return .false
        }
      default:
        throw RuntimeError.type(bvec, expected: [.strType, .byteVectorType])
    }
  }
  
  private func deleteZipEntry(_ expr: Expr, _ path: Expr) throws -> Expr {
    let archive = try self.archive(from: expr)
    guard let entry = archive[try path.asString()] else {
      throw RuntimeError.eval(.zipArchiveEntryDoesNotExist, expr, path)
    }
    guard archive.accessMode != .read else {
      throw RuntimeError.eval(.cannotMutateZipArchive, expr)
    }
    try archive.remove(entry)
    return .void
  }
  
  public func addZipEntry(to archive: Archive,
                          with path: String,
                          from source: URL,
                          compressed: Bool = true,
                          modificationDate modDate: Date? = nil) throws {
    guard let (type, date, size, permissions) =
            try self.context.fileHandler.itemProperties(for: source) else {
      throw RuntimeError.eval(.unknownFileOrDirectory, .makeString(source.path))
    }
    guard type == .symbolicLink || FileManager.default.isReadableFile(atPath: source.path) else {
      throw RuntimeError.eval(.cannotOpenFile, .makeString(source.path))
    }
    switch type {
      case .file:
        let compMethod = compressed ? CompressionMethod.deflate : CompressionMethod.none
        let data = try Data(contentsOf: source)
        try archive.addEntry(with: path,
                             type: .file,
                             uncompressedSize: size,
                             modificationDate: modDate ?? date,
                             permissions: permissions,
                             compressionMethod: compMethod) { _, _ in return data }
      case .directory:
        try archive.addEntry(with: path.hasSuffix("/") ? path : path + "/",
                             type: .directory,
                             uncompressedSize: size,
                             modificationDate: modDate ?? date,
                             permissions: permissions,
                             compressionMethod: .none) { _, _ in Data() }
      case .symbolicLink:
        try archive.addEntry(with: path,
                             type: .symlink,
                             uncompressedSize: size,
                             modificationDate: modDate ?? date,
                             permissions: permissions,
                             compressionMethod: .none) {  _, _ in
          let fsrepr = FileManager.default.fileSystemRepresentation(withPath:
                         try FileManager.default.destinationOfSymbolicLink(atPath: source.path))
          return Data(buffer: UnsafeBufferPointer(start: fsrepr,
                                                  count: Int(strlen(fsrepr))))
        }
    }
  }
}

public final class ZipArchive: NativeObject {

  /// Type representing zip archives
  public static let type = Type.objectType(Symbol(uninterned: "zip-archive"))
  
  /// The zip archive
  public let archive: Archive
  
  /// Initializer
  public init(_ archive: Archive) {
    self.archive = archive
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    if self.archive.url.path.isEmpty {
      return super.string
    } else {
      return "#<zip-archive \(self.archive.url.path)>"
    }
  }
}
