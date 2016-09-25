//
//  Reference.swift
//  LispKit
//
//  Created by Matthias Zenger on 06/03/2016.
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

///
/// A `Reference` is a hashable object whose identity is used for the definition of
/// equality and the hash value.
///
open class Reference: Hashable {
  
  public var identity: UInt {
    return UInt(bitPattern: ObjectIdentifier(self))
  }
  
  public var identityString: String {
    return String(self.identity, radix: 16)
  }
  
  public var hashValue: Int {
    return ObjectIdentifier(self).hashValue
  }
}

/// Equality based on the identity of references
public func ==(lhs: Reference, rhs: Reference) -> Bool {
  return lhs === rhs
}
