//
//  ConsoleOutput.swift
//  LispKitRepl iOS
//
//  Created by Matthias Zenger on 24/04/2021.
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

struct ConsoleOutput: Identifiable, Equatable {
  
  enum Kind: Equatable {
    case command
    case output
    case result
    case error(String?)
    case info
  }
  
  let id = UUID()
  let kind: Kind
  var text: String
  
  var isError: Bool {
    guard case .error(_) = self.kind else {
      return false
    }
    return true
  }
}
