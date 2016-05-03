//
//  Library.swift
//  LispKit
//
//  Created by Matthias Zenger on 22/01/2016.
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
/// `Library` defines a framework for defining builtin functionality for LispKit in a modular
/// fashion. Concrete implementations subclass `Library` and override the `export` method with
/// declarations of constants, procedures, and special forms.
/// 
public class Library {
  
  /// The context into which this library gets exported
  let context: Context
  
  /// Initializer
  public required init(_ context: Context) {
    self.context = context
    self.export()
  }
  
  /// The `export` method needs to be overridden in subclasses of `Library`. These
  /// overriding implementations declare all bindings that are exported by the library.
  public func export() {
    // This method needs to be overridden for concrete primitive libraries
  }
  
  public func define(name: String, _ proc: Procedure) {
    self.context.systemScope[self.context.symbols.intern(name)] = .Proc(proc)
  }
  
  public func define(name: String, _ special: SpecialForm) {
    self.context.systemScope[self.context.symbols.intern(name)] = .Special(special)
  }
  
  // Sublibrary declaration
  
  func include(lib: Library.Type) {
    let _ = lib.init(self.context)
  }
}
