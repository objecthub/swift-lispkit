//
//  String.swift
//  LispKit
//
//  Created by Matthias Zenger on 06/03/2022.
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

extension String {
  
  enum TruncPosition {
    case front
    case middle
    case end
  }
  
  func truncated(limit: Int, pos: TruncPosition = .end, terminator: String = "…") -> String {
    guard self.count > limit else {
      return self
    }
    switch pos {
      case .front:
        return terminator + self.suffix(limit)
      case .middle:
        let numFrontChars = Int(ceil(Float(limit - terminator.count)/2.0))
        let numTailChars = Int(floor(Float(limit - terminator.count)/2.0))
        return "\(self.prefix(numFrontChars))\(terminator)\(self.suffix(numTailChars))"
      case .end:
        return self.prefix(limit) + terminator
    }
  }
}
