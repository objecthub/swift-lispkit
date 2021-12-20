//
//  Registers.swift
//  LispKit
//
//  Created by Matthias Zenger on 20/12/2021.
//  Copyright © 2021 ObjectHub. All rights reserved.
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
import Atomics

///
/// `Registers` collects all register values in a single struct; it includes:
///    - `rid`: a registers id
///    - `code`: a reference to the code being executed
///    - `captured`: a list of captured expressions
///    - `ip`: the instruction pointer
///    - `fp`: the frame pointer
///    - `initialFp`: the initial frame pointer
///
struct Registers {
  let rid: Int
  var code: Code
  var captured: Exprs
  var ip: Int
  var fp: Int
  let initialFp: Int
  
  /// Atomic counter for managing register ids
  private static let nextRid = ManagedAtomic<Int>(0)
  
  init(code: Code, captured: Exprs, fp: Int, root: Bool) {
    if root {
      self.rid = 0
    } else {
      self.rid = Registers.nextRid.wrappingIncrementThenLoad(ordering: .relaxed)
    }
    self.code = code
    self.captured = captured
    self.ip = 0
    self.fp = fp
    self.initialFp = fp
  }
  
  mutating func use(code: Code, captured: Exprs, fp: Int) {
    self.code = code
    self.captured = captured
    self.ip = 0
    self.fp = fp
  }
  
  var topLevel: Bool {
    return self.fp == self.initialFp
  }
  
  var isInitialized: Bool {
    return self.rid == 0 && self.code.instructions.count > 0
  }

  public func mark(in gc: GarbageCollector) {
    gc.mark(self.code)
    for i in self.captured.indices {
      gc.markLater(self.captured[i])
    }
  }
}
