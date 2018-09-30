//
//  Owners.swift
//  LispKit
//
//  Created by Matthias Zenger on 01/07/2018.
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
/// Data structure for managing dependencies allowing objects to track their
/// owners; i.e. other objects that refer to them.
///
public struct Owners<T: Reference>: Sequence {
  
  fileprivate struct Entry: Hashable {
    let hash: Int
    weak var owner: T?
    
    init(_ owner: T) {
      self.hash = Int(bitPattern: owner.identity)
      self.owner = owner
    }
    
    func hash(into hasher: inout Hasher) {
      hasher.combine(self.hash)
    }
    
    static func ==(lhs: Owners<T>.Entry, rhs: Owners<T>.Entry) -> Bool {
      return lhs.owner === rhs.owner && lhs.hashValue == rhs.hashValue
    }
  }
  
  private var entries: Set<Entry>
  
  public init() {
    self.entries = Set<Entry>()
  }

  public mutating func include(_ owner: T) {
    self.entries.insert(Entry(owner))
  }
  
  public func contains(_ owner: T) -> Bool {
    return self.entries.contains(Entry(owner))
  }
  
  public func makeIterator() -> OwnersIterator<T> {
    return OwnersIterator(self.entries.makeIterator())
  }
  
  public mutating func compact() {
    for entry in self.entries {
      if entry.owner == nil {
        self.entries.remove(entry)
      }
    }
  }
}

public struct OwnersIterator<T: Reference>: IteratorProtocol {
  private var entryIterator: SetIterator<Owners<T>.Entry>
  
  fileprivate init(_ entryIterator: SetIterator<Owners<T>.Entry>) {
    self.entryIterator = entryIterator
  }
  
  public mutating func next() -> T? {
    while true {
      guard let entry = self.entryIterator.next() else {
        return nil
      }
      if let owner = entry.owner {
        return owner
      }
    }
  }
}
