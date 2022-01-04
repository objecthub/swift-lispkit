//
//  MethodProfiler.swift
//  LispKit
//
//  Created by Matthias Zenger on 01/01/2022.
//  Copyright © 2022 ObjectHub. All rights reserved.
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

public final class MethodProfiler {
  let className: String
  var open: [Double] = []
  var stats: [String : (Int, Double)] = [:]
  
  init(_ className: String) {
    self.className = className
  }
  
  func enter() {
    open.append(Timer.absoluteTimeInSec)
  }
  
  func exit(_ name: String) {
    let time = Timer.absoluteTimeInSec - open.last!
    open.removeLast()
    if let (count, average) = stats[name] {
      stats[name] = (count + 1, (average * Double(count) + time)/Double(count + 1))
    } else {
      stats[name] = (1, time)
    }
  }
  
  public func printStats() {
    Swift.print("==== \(self.className) ================")
    for (name, (count, average)) in self.stats {
      Swift.print("\(name),\(count),\(average),\(average * Double(count))")
    }
  }
}
