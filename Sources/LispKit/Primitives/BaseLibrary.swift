//
//  BaseLibrary.swift
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


public final class BaseLibrary: NativeLibrary {

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "base"]
  }

  /// Exported definitions.
  public override func reexports() throws {
    try self.exportAll()
  }

  /// Dependencies of the library.
  public override func dependencies() {
    self.import(from: CoreLibrary.name)
    self.import(from: ControlFlowLibrary.name)
    self.import(from: SystemLibrary.name)
    self.import(from: BoxLibrary.name)
    self.import(from: MathLibrary.name)
    self.import(from: ListLibrary.name)
    self.import(from: HashTableLibrary.name)
    self.import(from: DynamicControlLibrary.name)
    self.import(from: TypeLibrary.name)
    self.import(from: VectorLibrary.name)
    self.import(from: RecordLibrary.name)
    self.import(from: BytevectorLibrary.name)
    self.import(from: CharLibrary.name)
    self.import(from: StringLibrary.name)
    self.import(from: PortLibrary.name)
  }
}
