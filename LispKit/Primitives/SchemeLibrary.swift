//
//  SchemeLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/01/2016.
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


public final class SchemeLibrary: NativeLibrary {
  
  public override func export() {
    // The current library mechanism requires ControlFlowLibrary to come first (as it's used
    // by native implementations of some functions in other libraries)
    include(ControlFlowLibrary.self)
    include(BaseLibrary.self)
    include(BoxLibrary.self)
    include(HashTableLibrary.self)
    include(DynamicControlLibrary.self)
    include(MathLibrary.self)
    include(ListLibrary.self)
    include(VectorLibrary.self)
    include(RecordLibrary.self)
    include(BytevectorLibrary.self)
    include(CharacterLibrary.self)
    include(StringLibrary.self)
    include(PortLibrary.self)
  }
}
