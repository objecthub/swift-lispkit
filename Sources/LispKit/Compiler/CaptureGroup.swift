//
//  CaptureGroup.swift
//  LispKit
//
//  Created by Matthias Zenger on 04/03/2016.
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


public final class CaptureGroup: CustomStringConvertible {
  internal unowned let owner: Compiler
  internal let parent: CaptureGroup?
  private var captures: [Definition: Capture]

  public final class Capture {
    public let index: Int
    public unowned var origin: BindingGroup

    fileprivate init(index: Int, origin: BindingGroup) {
      self.index = index
      self.origin = origin
    }
  }

  public init(owner: Compiler, parent: CaptureGroup? = nil) {
    self.owner = owner
    self.parent = parent
    self.captures = [Definition: Capture]()
  }

  public func capture(_ def: Definition, from: BindingGroup) -> Int {
    if let capture = self.captures[def] {
      return capture.index
    } else {
      let capture = Capture(index: self.captures.count, origin: from)
      self.captures[def] = capture
      return capture.index
    }
  }

  public func captureFor(_ def: Definition) -> Capture? {
    return self.captures[def]
  }

  public var count: Int {
    return self.captures.count
  }

  public var definitions: [Definition?] {
    var seq = [Definition?](repeating: nil, count: self.captures.count)
    for (def, capture) in self.captures {
      seq[capture.index] = def
    }
    return seq
  }

  public var description: String {
    var seq = self.definitions
    var builder = StringBuilder()
    for index in seq.indices {
      builder.append(index, width: 5, alignRight: true)
      builder.append(": ")
      if let def = seq[index], let capture = self.captures[def] {
        builder.append(capture.origin.symbol(at: def.index)?.description ?? "<unknown symbol>")
        builder.append(" (\(def))")
      } else {
        builder.append("<undef>")
      }
      builder.appendNewline()
    }
    return builder.description
  }
}
