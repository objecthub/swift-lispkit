//
//  Data.swift
//  LispKit
//
//  Created by Matthias Zenger on 18/02/2023.
//  Copyright © 2023 ObjectHub. All rights reserved.
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

extension Data {
  
  func hexEncodedString(upperCase: Bool = false) -> String {
    let utf8Digits = Array((upperCase ? "0123456789ABCDEF" : "0123456789abcdef").utf8)
    return String(unsafeUninitializedCapacity: 2 * self.count) { (ptr) -> Int in
      var p = ptr.baseAddress!
      for byte in self {
        p[0] = utf8Digits[Int(byte / 16)]
        p[1] = utf8Digits[Int(byte % 16)]
        p += 2
      }
      return 2 * self.count
    }
  }
  
}
