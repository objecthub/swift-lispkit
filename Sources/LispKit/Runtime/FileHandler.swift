//
//  FileHandler.swift
//  LispKit
//
//  Created by Matthias Zenger on 03/12/2016.
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

import Foundation

///
/// The `FileHandler` class is used to load LispKit source files (suffix ".scm"),
/// LispKit library definition files (suffix ".sld")  as well as assets (i.e. data that
/// is not executable). Furthermore, the class provides a high-level file interface which
/// is used by the LispKit core library.
/// 
public final class FileHandler {
  private let fileManager: FileManager
  public var searchUrls: [URL]
  public var librarySearchUrls: [URL]
  public var assetSearchUrls: [URL]
  
  public var currentDirectoryPath: String {
    get {
      return self.fileManager.currentDirectoryPath
    }
    set {
      self.fileManager.changeCurrentDirectoryPath(newValue)
    }
  }
  
  public init(includeInternalResources: Bool = true,
              includeDocumentPath: String? = "LispKit") {
    self.fileManager = FileManager.default
    self.searchUrls = []
    self.librarySearchUrls = []
    self.assetSearchUrls = []
    if includeInternalResources {
      if let url = Context.bundle?.resourceURL?
                                  .appendingPathComponent("LispKit/Resources", isDirectory: true) {
        if self.isDirectory(atPath: url.path) {
          self.searchUrls.append(url.absoluteURL)
        }
      }
      if let url =
          Context.bundle?.resourceURL?
                         .appendingPathComponent("LispKit/Resources/Libraries", isDirectory: true) {
        if self.isDirectory(atPath: url.path) {
          self.librarySearchUrls.append(url.absoluteURL)
        }
      }
      if let url =
          Context.bundle?.resourceURL?
                         .appendingPathComponent("LispKit/Resources/Assets", isDirectory: true) {
        if self.isDirectory(atPath: url.path) {
          self.assetSearchUrls.append(url.absoluteURL)
        }
      }
    }
    if let docPath = includeDocumentPath, !docPath.isEmpty {
      for url in self.fileManager.urls(for: .documentDirectory, in: .userDomainMask) {
        let rootUrl = url.appendingPathComponent(docPath, isDirectory: true)
        if self.isDirectory(atPath: rootUrl.path) {
          self.searchUrls.append(rootUrl)
        }
        let libUrl = rootUrl.appendingPathComponent("Libraries", isDirectory: true)
        if self.isDirectory(atPath: libUrl.path) {
          self.librarySearchUrls.append(libUrl)
        }
        let assetUrl = rootUrl.appendingPathComponent("Assets", isDirectory: true)
        if self.isDirectory(atPath: assetUrl.path) {
          self.librarySearchUrls.append(assetUrl)
        }
      }
    }
  }
  
  public func addSearchPath(_ path: String) -> Bool {
    guard self.isDirectory(atPath: path) else {
      return false
    }
    self.searchUrls.append(URL(fileURLWithPath: path, isDirectory: true))
    return true
  }
  
  public func prependSearchPath(_ path: String) -> Bool {
    guard self.isDirectory(atPath: path) else {
      return false
    }
    self.searchUrls.insert(URL(fileURLWithPath: path, isDirectory: true), at: 0)
    return true
  }
  
  public func addLibrarySearchPath(_ path: String) -> Bool {
    guard self.isDirectory(atPath: path) else {
      return false
    }
    self.librarySearchUrls.append(URL(fileURLWithPath: path, isDirectory: true))
    return true
  }
  
  public func prependLibrarySearchPath(_ path: String) -> Bool {
    guard self.isDirectory(atPath: path) else {
      return false
    }
    self.librarySearchUrls.insert(URL(fileURLWithPath: path, isDirectory: true), at: 0)
    return true
  }
  
  public func addAssetSearchPath(_ path: String) -> Bool {
    guard self.isDirectory(atPath: path) else {
      return false
    }
    self.assetSearchUrls.append(URL(fileURLWithPath: path, isDirectory: true))
    return true
  }
  
  public func prependAssetSearchPath(_ path: String) -> Bool {
    guard self.isDirectory(atPath: path) else {
      return false
    }
    self.assetSearchUrls.insert(URL(fileURLWithPath: path, isDirectory: true), at: 0)
    return true
  }
  
  public func filePath(forFile name: String, relativeTo root: String? = nil) -> String? {
    return self.searchFile(withName: name,
                           ofType: "scm",
                           relativeTo: root,
                           findIn: self.searchUrls)
  }
  
  public func libraryFilePath(forFile name: String, relativeTo root: String? = nil) -> String? {
    return self.searchFile(withName: name,
                           ofType: "sld",
                           relativeTo: root,
                           findIn: self.librarySearchUrls)
  }
  
  public func assetFilePath(forFile name: String,
                            ofType type: String,
                            inFolder folder: String? = nil,
                            relativeTo root: String? = nil) -> String? {
    var name = name
    if let folder = folder {
      let folderUrl = URL(fileURLWithPath: folder, isDirectory: true)
      name = folderUrl.appendingPathComponent(name).relativePath
    }
    return self.searchFile(withName: name,
                           ofType: type,
                           relativeTo: root,
                           findIn: self.assetSearchUrls)
  }
  
  private func searchFile(withName name: String,
                          ofType type: String,
                          relativeTo root: String? = nil,
                          findIn urls: [URL]) -> String? {
    // Compute suffix
    let suffix = "." + type
    // If there is a name in the current path (either with or without suffix), then return it.
    if self.isFile(atPath: name, relativeTo: root) {
      return self.path(name, relativeTo: root)
    } else if !name.hasSuffix(suffix) && self.isFile(atPath: name + suffix, relativeTo: root) {
      return self.path(name + suffix, relativeTo: root)
    }
    // Search through all search paths ignoring the suffix.
    for url in urls {
      let path = url.appendingPathComponent(name, isDirectory: false).path
      if self.isFile(atPath: path) {
        return path
      }
    }
    // If the file doesn't end with the suffix, search through all search paths including
    // the suffix.
    if !name.hasSuffix(suffix) {
      let nameOfType = name + suffix
      for url in urls {
        let path = url.appendingPathComponent(nameOfType, isDirectory: false).path
        if self.isFile(atPath: path) {
          return path
        }
      }
    }
    return nil
  }
  
  public func path(_ path: String, relativeTo root: String? = nil) -> String {
    if let root = root {
      return URL(fileURLWithPath: path,
                 relativeTo: URL(fileURLWithPath: root, isDirectory: true)).absoluteURL.path
    } else {
      return URL(fileURLWithPath: path,
                 relativeTo: URL(fileURLWithPath: self.currentDirectoryPath, isDirectory: true))
             .absoluteURL.path
    }
  }
  
  public func directory(_ path: String, relativeTo root: String? = nil) -> String {
    if let root = root {
      return URL(fileURLWithPath: path,
                 relativeTo: URL(fileURLWithPath: root,
                                 isDirectory: true)).deletingLastPathComponent()
             .absoluteURL.path
    } else {
      return URL(fileURLWithPath: path,
                 relativeTo: URL(fileURLWithPath: self.currentDirectoryPath,
                                 isDirectory: true))
             .deletingLastPathComponent().absoluteURL.path
    }
  }
  
  public func fileSize(atPath path: String, relativeTo root: String? = nil) -> Int64? {
    let filePath = self.path(path, relativeTo: root)
    var isDir: ObjCBool = false
    guard self.fileManager.fileExists(atPath: filePath, isDirectory: &isDir) else {
      return nil
    }
    guard !isDir.boolValue else {
      return nil
    }
    do {
      let attr = try self.fileManager.attributesOfItem(atPath: filePath)
      return attr[FileAttributeKey.size] as? Int64
    } catch {
      return nil
    }
  }
  
  public func itemExists(atPath path: String, relativeTo root: String? = nil) -> Bool {
    return self.fileManager.fileExists(atPath: self.path(path, relativeTo: root))
  }
  
  public func isFile(atPath path: String, relativeTo root: String? = nil) -> Bool {
    var isDir: ObjCBool = false
    guard self.fileManager.fileExists(atPath: self.path(path, relativeTo: root),
                                      isDirectory: &isDir) else {
      return false
    }
    return !isDir.boolValue
  }
  
  public func isDirectory(atPath path: String, relativeTo root: String? = nil) -> Bool {
    var isDir: ObjCBool = false
    guard self.fileManager.fileExists(atPath: self.path(path, relativeTo: root),
                                      isDirectory: &isDir) else {
      return false
    }
    return isDir.boolValue
  }
  
  public func contentsOfDirectory(atPath path: String,
                                  relativeTo root: String? = nil) throws -> [String] {
    return try self.fileManager.contentsOfDirectory(atPath: self.path(path, relativeTo: root))
  }
  
  public func makeDirectory(atPath path: String,
                            relativeTo root: String? = nil) throws {
    try self.fileManager.createDirectory(atPath: self.path(path, relativeTo: root),
                                         withIntermediateDirectories: false,
                                         attributes: nil)
  }
  
  public func deleteItem(atPath path: String, relativeTo root: String? = nil) throws {
    try self.fileManager.removeItem(atPath: self.path(path, relativeTo: root))
  }
  
  public func copyItem(atPath path: String,
                       toPath dest: String,
                       relativeTo root: String? = nil) throws {
    try self.fileManager.copyItem(atPath: self.path(path, relativeTo: root),
                                  toPath: self.path(dest, relativeTo: root))
  }
  
  public func moveItem(atPath path: String,
                       toPath dest: String,
                       relativeTo root: String? = nil) throws {
    try self.fileManager.moveItem(atPath: self.path(path, relativeTo: root),
                                  toPath: self.path(dest, relativeTo: root))
  }
}
