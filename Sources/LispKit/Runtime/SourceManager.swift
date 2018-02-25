//
//  SourceManager.swift
//  LispKit
//
//  Created by Matthias Zenger on 24.02.18.
//  Copyright © 2018 ObjectHub. All rights reserved.
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

public final class SourceManager {
  internal var sourceUrls: [URL]
  private var sourceIds: [URL: UInt16]
  
  init() {
    self.sourceUrls = [URL(string: "console")!]
    self.sourceIds = [:]
  }
  
  public static let consoleSourceId: UInt16 = 0
  public static let unknownSourceId: UInt16 = UInt16.max
  
  public func sourceId(for url: URL) -> UInt16? {
    return self.sourceIds[self.absoluteUrl(url)]
  }
  
  public func sourceId(for path: String) -> UInt16? {
    return self.sourceIds[self.absoluteUrl(path)]
  }
  
  public func obtainSourceId(for path: String) -> UInt16 {
    return self.obtainSourceId(for: URL(fileURLWithPath: path))
  }
  
  public func obtainSourceId(for url: URL) -> UInt16 {
    let canonicalUrl = self.absoluteUrl(url)
    if let id = self.sourceIds[canonicalUrl] {
      return id
    } else if self.sourceUrls.count == UInt16.max {
      return SourceManager.unknownSourceId
    } else {
      let newSourceId = UInt16(self.sourceUrls.count)
      self.sourceUrls.append(canonicalUrl)
      self.sourceIds[canonicalUrl] = newSourceId
      return newSourceId
    }
  }
  
  public func sourceUrl(for sourceId: UInt16) -> URL? {
    guard sourceId != SourceManager.unknownSourceId else {
      return nil
    }
    return self.sourceUrls[Int(sourceId)]
  }
  
  public func readSource(for sourceId: UInt16) throws -> String {
    guard let sourceUrl = self.sourceUrl(for: sourceId) else {
      throw EvalError.unknownFile("<path for source id = \(sourceId)>")
    }
    return try String(contentsOf: sourceUrl)
  }
  
  public func readSource(for url: URL) throws -> String {
    return try self.readSource(for: self.obtainSourceId(for: url))
  }
  
  public func readSource(for path: String) throws -> String {
    return try self.readSource(for: self.obtainSourceId(for: path))
  }
  
  private func absoluteUrl(_ url: URL) -> URL {
    return url.standardizedFileURL.absoluteURL
  }
  
  private func absoluteUrl(_ path: String) -> URL {
    return self.absoluteUrl(URL(fileURLWithPath: path))
  }
}
