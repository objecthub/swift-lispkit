//
//  main.swift
//  LispKitRepl
//
//  Created by Matthias Zenger on 14/04/2016.
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

import LispKit


// Create console
let console = CommandLineConsole()

// Create LispKit context
let context = Context(console: console)
do {
  try context.environment.import(SchemeLibrary.name)
} catch let error {
  preconditionFailure("cannot import (base scheme): \(error.localizedDescription)")
}

// Load standard Prelude
if let preludePath = Bundle(identifier: "net.objecthub.LispKit")?.path(
  forResource: "Prelude", ofType: "scm", inDirectory: "LispKit/Resources") {
  do {
    _ = try context.machine.eval(file: preludePath, in: context.global)
  } catch let error {
    preconditionFailure("cannot evaluate prelude: \(error.localizedDescription)")
  }
}

// Print header
console.print("\(AppInfo.name) \(AppInfo.version) (\(AppInfo.buildDate) \(AppInfo.buildTime))\n")
console.print("\(AppInfo.copyright)\n")

// Enter read-eval-print loop
console.print("⟹ ")
while let line = console.read() {
  guard line != "exit" else {
    break
  }
  let res = context.machine.evalOnTopLevel(str: line, in: context.global)
  if res != Expr.void {
    console.print(res.description + "\n")
  }
  console.print("⟹ ")
}
