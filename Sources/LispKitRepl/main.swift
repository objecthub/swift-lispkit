//
//  main.swift
//  LispKitRepl
//
//  Created by Matthias Zenger on 14/04/2016.
//  Copyright © 2016, 2017 ObjectHub. All rights reserved.
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

// Create LispKit context and import (base scheme)
let context = Context(console: console)
do {
  try context.environment.import(SchemeLibrary.name)
} catch let error {
  preconditionFailure("cannot import (base scheme): \(error.localizedDescription)")
}

// Load standard Prelude
if let preludePath = Context.defaultPreludePath {
  do {
    _ = try context.machine.eval(file: preludePath, in: context.global)
  } catch let error {
    preconditionFailure("cannot evaluate prelude: \(error.localizedDescription)")
  }
}

// Install error handler
if let dynamicLib = context.libraries.lookup("lispkit", "dynamic") as? DynamicControlLibrary,
   let raiseProc = dynamicLib.raiseProc {
  context.machine.raiseProc = raiseProc
}

// Print header
console.print("\(AppInfo.name) \(AppInfo.version)\(AppInfo.buildAnnotation)\n")
console.print("\(AppInfo.copyright)\n")

// Enter read-eval-print loop
var buffer = ""
console.print(AppInfo.prompt)
while let line = console.read() {
  buffer += line
  let res = context.machine.onTopLevelDo {
    return try context.machine.eval(str: buffer, in: context.global)
  }
  // Exit loop if the machine has executed the `exit` function
  if context.machine.exitTriggered {
    if res != .true {
      console.print("abnormal exit: \(res.description)\n")
    }
    break
  // If closing parenthesis are missing, keep on reading
  } else if case .error(let err) = res,
            let syntaxError = err.root as? SyntaxError,
            syntaxError == SyntaxError.closingParenthesisMissing {
    continue
  // For multiple values being returned, print each value on a separate line
  } else if case .values(let expr) = res {
    var next = expr
    while case .pair(let x, let rest) = next {
      console.print("\(x.description)\n")
      next = rest
    }
  // For errors print the error message
  } else if case .error(let err) = res {
    console.print("\(err.printableDescription)\n")
  // For non-void results, print result
  } else if res != .void {
    console.print("\(res.description)\n")
  }
  // Print prompt and empty buffer
  console.print(AppInfo.prompt)
  buffer = ""
}
