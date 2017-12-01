//
//  MultiMap.swift
//  LispKit
//
//  Created by Matthias Zenger on 18/09/2016.
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

/// Minimalistic multi map implementation
public struct MultiMap<Key: Hashable, Value>: CustomStringConvertible {
  private var map: [Key : [Value]]
  
  public init() {
    self.map = [:]
  }
  
  public var keys: Dictionary<Key, [Value]>.Keys {
    return map.keys
  }
  
  public func hasValues(for key: Key) -> Bool {
    return self.map[key] != nil
  }
  
  public func values(for key: Key) -> [Value] {
    return self.map[key] ?? []
  }
  
  public mutating func insert(_ key: Key, mapsTo value: Value) {
    if self.map[key] == nil {
      self.map[key] = [value]
    } else {
      self.map[key]!.append(value)
    }
  }
  
  public var description: String {
    var builder = StringBuilder(prefix: "{", postfix: "}", separator: ", ")
    for (key, value) in self.map {
      builder.append("\(key) → \(value)")
    }
    return builder.description
  }
}
