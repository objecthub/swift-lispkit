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
  private var sources: [String]
  private var sourceIds: [String: UInt16]
  
  init() {
    self.sources = ["console"]
    self.sourceIds = [:]
  }
  
  public static let consoleSourceId: UInt16 = 0
  public static let unknownSourceId: UInt16 = UInt16.max
  
  public func sourceId(for path: String) -> UInt16? {
    return self.sourceIds[path]
  }
  
  public func obtainSourceId(for path: String) -> UInt16 {
    if let id = self.sourceId(for: path) {
      return id
    } else if self.sources.count == UInt16.max {
      return SourceManager.unknownSourceId
    } else {
      self.sources.append(path)
      return UInt16(self.sources.count - 1)
    }
  }
  
  public func sourcePath(for sourceId: UInt16) -> String? {
    guard sourceId != SourceManager.unknownSourceId else {
      return nil
    }
    return self.sources[Int(sourceId)]
  }
  
  public func readSource(for sourceId: UInt16) throws -> String {
    guard let sourcePath = self.sourcePath(for: sourceId) else {
      throw EvalError.unknownFile("<path for source id = \(sourceId)>")
    }
    return try String(contentsOfFile: sourcePath, encoding: String.Encoding.utf8)
  }
  
  public func readSource(for filepath: String) throws -> String {
    return try self.readSource(for: self.obtainSourceId(for: filepath))
  }
}
