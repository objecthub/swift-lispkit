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


public class CaptureGroup: CustomStringConvertible {
  public unowned let owner: Compiler
  public let parent: CaptureGroup?
  public private(set) var captures: [Definition: Capture]
  
  public class Capture {
    public let index: Int
    public unowned var origin: BindingGroup
    
    private init(index: Int, origin: BindingGroup) {
      self.index = index
      self.origin = origin
    }
  }
  
  public init(owner: Compiler, parent: CaptureGroup? = nil) {
    self.owner = owner
    self.parent = parent
    self.captures = [Definition: Capture]()
  }
  
  public func capture(def: Definition, from: BindingGroup) -> Int {
    if let capture = self.captures[def] {
      return capture.index
    } else {
      let capture = Capture(index: self.captures.count, origin: from)
      self.captures[def] = capture
      return capture.index
    }
  }
  
  public var count: Int {
    return self.captures.count
  }
  
  public var definitions: [Definition?] {
    var seq = [Definition?](count: self.captures.count, repeatedValue: nil)
    for (def, capture) in self.captures {
      seq[capture.index] = def
    }
    return seq
  }
  
  private func indexToStr(index: Int) -> String {
    switch index {
    case 0...9:
      return "    \(index)"
    case 10...99:
      return "   \(index)"
    case 100...999:
      return "  \(index)"
    case 1000...9999:
      return " \(index)"
    default:
      return "\(index)"
    }
  }
  
  public var description: String {
    var seq = self.definitions
    var res = ""
    for index in seq.indices {
      res += "\(indexToStr(index)): "
      if let def = seq[index], capture = self.captures[def] {
        if let sym = capture.origin.symbolAtIndex(def.index) {
          res += sym.description
        } else {
          res += "<unknown symbol>"
        }
        res += " (\(def))"
      } else {
        res += "<undef>"
      }
      res += "\n"
    }
    return res
  }
}
