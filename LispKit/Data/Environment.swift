//
//  Environment.swift
//  LispKit
//
//  Created by Matthias Zenger on 16/09/2016.
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

public class Environment {
  
  public enum LocationRef {
    case undefined
    case mutable(Int)
    case mutableImport(Int)
    case immutableImport(Int)
  }
  
  private var bindings: [Symbol : LocationRef]
  
  public init() {
    self.bindings = [:]
  }
  
  public subscript(sym: Symbol) -> LocationRef {
    get {
      return self.bindings[sym] ?? .undefined
    }
    set {
      if case .undefined = newValue {
        preconditionFailure("cannot set binding in environment to undefined")
      }
      self.bindings[sym] = newValue
    }
  }
}
