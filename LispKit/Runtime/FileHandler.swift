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
/// The `FileHandler` class is used to load LispKit source files (suffix ".scm") as well as
/// LispKit library definition files (suffix ".sld"). Furthermore, the class provides a
/// high-level file interface which used by the LispKit core library.
/// 
public final class FileHandler {
  private let fileManager: FileManager
  public var searchUrls: [URL]
  public var librarySearchUrls: [URL]
  
  public init(includeInternalResources: Bool = true,
              includeDocumentPath: String? = "LispKit") {
    self.fileManager = FileManager.default
    self.searchUrls = []
    self.librarySearchUrls = []
    if includeInternalResources {
      let bundle = Bundle(identifier: "net.objecthub.LispKit")
      if let url = bundle?.resourceURL?
                          .appendingPathComponent("LispKit/Resources", isDirectory: true) {
        if self.isDirectory(atPath: url.path) {
          self.searchUrls.append(url.absoluteURL)
        }
      }
      if let url =
          bundle?.resourceURL?
                 .appendingPathComponent("LispKit/Resources/Libraries", isDirectory: true) {
        if self.isDirectory(atPath: url.path) {
          self.librarySearchUrls.append(url.absoluteURL)
        }
      }
    }
    if let docPath = includeDocumentPath {
      for url in self.fileManager.urls(for: .documentDirectory, in: .userDomainMask) {
        let rootUrl = url.appendingPathComponent(docPath, isDirectory: true)
        if self.isDirectory(atPath: rootUrl.path) {
          self.searchUrls.append(rootUrl)
        }
        let libUrl = rootUrl.appendingPathComponent("Libraries", isDirectory: true)
        if self.isDirectory(atPath: libUrl.path) {
          self.librarySearchUrls.append(libUrl)
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
  
  public func addLibrarySearchPath(_ path: String) -> Bool {
    guard self.isDirectory(atPath: path) else {
      return false
    }
    self.librarySearchUrls.append(URL(fileURLWithPath: path, isDirectory: true))
    return true
  }
  
  public func filePath(forFile name: String) -> String? {
    return self.searchFile(withName: name, ofType: "scm", findIn: self.searchUrls)
  }
  
  public func libraryFilePath(forFile name: String) -> String? {
    return self.searchFile(withName: name, ofType: "sld", findIn: self.librarySearchUrls)
  }
  
  private func searchFile(withName name: String,
                          ofType type: String,
                          findIn urls: [URL]) -> String? {
    // Compute suffix
    let suffix = "." + type
    // If there is a name in the current path (either with or without suffix), then return it.
    if self.isFile(atPath: name) {
      return name
    } else if !name.hasSuffix(suffix) && self.isFile(atPath: name + suffix) {
      return name + suffix
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
  
  public func fileExists(atPath path: String) -> Bool {
    return self.fileManager.fileExists(atPath: path)
  }
  
  public func isFile(atPath path: String) -> Bool {
    var isDir: ObjCBool = false
    guard self.fileManager.fileExists(atPath: path, isDirectory: &isDir) else {
      return false
    }
    return !isDir.boolValue
  }
  
  public func isDirectory(atPath path: String) -> Bool {
    var isDir: ObjCBool = false
    guard self.fileManager.fileExists(atPath: path, isDirectory: &isDir) else {
      return false
    }
    return isDir.boolValue
  }
  
  public func deleteFile(atPath path: String) throws {
    return try self.fileManager.removeItem(atPath: path)
  }
  
  public func contentsOfDirectory(atPath path: String) throws -> [String] {
    return try self.fileManager.contentsOfDirectory(atPath: path)
  }
}
