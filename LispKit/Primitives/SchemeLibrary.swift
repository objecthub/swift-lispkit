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
  
  /// Name of the library.
  public override class var name: [String] {
    return ["scheme", "base"]
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.include(ControlFlowLibrary.name)
    self.include(BaseLibrary.name)
    self.include(BoxLibrary.name)
    self.include(HashTableLibrary.name)
    self.include(DynamicControlLibrary.name)
    self.include(MathLibrary.name)
    self.include(ListLibrary.name)
    self.include(VectorLibrary.name)
    self.include(RecordLibrary.name)
    self.include(BytevectorLibrary.name)
    self.include(CharacterLibrary.name)
    self.include(StringLibrary.name)
    self.include(PortLibrary.name)
  }
}
