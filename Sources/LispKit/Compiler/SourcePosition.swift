//
//  SourcePosition.swift
//  LispKit
//
//  Created by Matthias Zenger on 24/02/2018.
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

///
/// Struct `Position` represents a position in the scanned string in terms of a line and
/// column number.
///
public struct SourcePosition: Equatable, Hashable, CustomStringConvertible {
  public let sourceId: UInt16
  public let line: UInt16
  public let column: UInt16
  
  public static let unknownLine: UInt16 = 0
  public static let unknownColumn: UInt16 = 0
  public static let unknown: SourcePosition = SourcePosition(SourceManager.unknownSourceId,
                                                             SourcePosition.unknownLine,
                                                             SourcePosition.unknownColumn)
  
  public init(_ sourceId: UInt16, _ line: UInt16, _ column: UInt16) {
    self.sourceId = sourceId
    self.line = line
    self.column = line == SourcePosition.unknownLine ? SourcePosition.unknownColumn : column
  }
  
  public init(_ sourceId: UInt16, _ line: UInt, _ column: UInt) {
    self.init(sourceId,
              line > UInt16.max ? SourcePosition.unknownLine : UInt16(line),
              column > UInt16.max ? SourcePosition.unknownColumn : UInt16(column))
  }
  
  public init(_ sourceId: UInt16, _ pos: Position) {
    self.init(sourceId,
              pos.line > UInt16.max ? SourcePosition.unknownLine : UInt16(pos.line),
              pos.col > UInt16.max ? SourcePosition.unknownColumn : UInt16(pos.col))
  }
  
  public var hashValue: Int {
    return ((self.sourceId.hashValue &* 31) &+ self.line.hashValue) &* 31 &+ self.column.hashValue
  }
  
  public var isUnknown: Bool {
    return self.sourceIsUnknown && self.lineIsUnknown
  }
  
  public var lineIsUnknown: Bool {
    return self.column == SourcePosition.unknownLine
  }
  
  public var columnIsUnknown: Bool {
    return self.column == SourcePosition.unknownColumn
  }
  
  public var sourceIsUnknown: Bool {
    return self.sourceId == SourceManager.unknownSourceId
  }
  
  public func fullDescription(_ sourceManager: SourceManager) -> String {
    guard let sourceUrl = sourceManager.sourceUrl(for: self.sourceId) else {
      return self.description
    }
    return "\(sourceUrl.lastPathComponent) \(self.description)"
  }
  
  public var description: String {
    return self.isUnknown ? ""
      : (self.columnIsUnknown ? "\(self.line)" : "\(self.line):\(self.column)")
  }
  
  public static func ==(lhs: SourcePosition, rhs: SourcePosition) -> Bool {
    return lhs.sourceId == rhs.sourceId &&
      lhs.line == rhs.line &&
      lhs.column == rhs.column
  }
}

