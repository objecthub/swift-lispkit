//
//  main.swift
//  LispKitRepl
//
//  Created by Matthias Zenger on 14/04/2016.
//  Copyright © 2016-2018 ObjectHub. All rights reserved.
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
import LispKit
import CommandLineKit

// Define and parse command-line arguments
var flags = Flags()
let filePaths  = flags.strings("f", "filepath",
                               description: "Adds file path in which programs are searched for.")
let libPaths   = flags.strings("l", "libpath",
                               description: "Adds file path in which libraries are searched for.")
let heapSize   = flags.int("h", "heapsize",
                           description: "initial capacity of the heap",
                           value: 1000)
let importLibs = flags.strings("i", "import",
                               description: "Imports library automatically after startup.")
let prelude    = flags.string("p", "prelude",
                              description: "Path to prelude file which gets executed after " +
                                           "loading all provided libraries.")
let prompt     = flags.string("r", "prompt",
                              description: "String used as prompt in REPL.",
                              value: AppInfo.prompt)
let basic      = flags.option("b", "basic",
                              description: "Use basic line reader only.")
let strict     = flags.option("s", "strict",
                              description: "In strict mode, initialization warnings terminate " +
                                           "the application.")
let quiet      = flags.option("q", "quiet",
                              description: "In quiet mode, optional messages are not printed.")
let help       = flags.option("h", "help",
                              description: "Show description of usage and options of this tools.")

do {
  try flags.parse()
} catch let error as FlagError {
  let flagname = error.flag?.longName ?? error.flag?.shortName?.description
  var message = error.flag == nil ? "error parsing flags: " : "error parsing flag `\(flagname!)`: "
  switch error.kind {
    case .unknownFlag(let str):
      message += "unknown flag `\(str)`"
    case .missingValue:
      message += "missing value"
    case .malformedValue(let str):
      message += "malformed value `\(str)`"
    case .illegalFlagCombination(let str):
      message += "illegal flag combination with `\(str)`"
    case .tooManyValues(let str):
      message += "too many values from `\(str)`"
  }
  print(message)
  exit(1)
}

// If help flag was provided, print usage description and exit tool
if help.wasSet {
  print(flags.usageDescription(usageName: TextStyle.bold.properties.apply(to: "usage:"),
                               synopsis: "[<option> ...] [--] [<program> <arg> ...]",
                               usageStyle: TextProperties.none,
                               optionsName: TextStyle.bold.properties.apply(to: "options:"),
                               flagStyle: TextStyle.italic.properties))
  exit(0)
}

// Instantiate line reader and console
let ln = basic.wasSet ? nil : LineReader()
let console = CommandLineConsole()

// Define prompt/read logic based on `console`
func readCommand(withPrompt: Bool = true) -> String? {
  if let ln = ln {
    do {
      return try ln.readLine(prompt: withPrompt ? (prompt.value ?? "> ") : "",
                             maxCount: 4000,
                             strippingNewline: true,
                             promptProperties: TextProperties(.blue, nil, .bold),
                             readProperties: TextProperties(.black, nil),
                             parenProperties: TextProperties(.red, nil, .bold))
    } catch LineReaderError.CTRLC {
      console.print("\nterminated\n")
      return nil
    } catch {
      console.print("\(error.localizedDescription)\n")
      return nil
    }
  } else {
    if withPrompt {
      console.print(prompt.value ?? "> ")
    }
    return console.read()
  }
}

// Define how optional messages and errors are printed
func printOpt(_ message: String) {
  if !quiet.wasSet {
    print(message)
  }
}

func printError(if issue: Bool = true, _ message: String) {
  if issue {
    printOpt(message)
    if strict.wasSet {
      exit(1)
    }
  }
}

// Create LispKit context
var cmdLineArgs = flags.parameters.isEmpty ? [CommandLine.arguments.first!] : flags.parameters
#if SPM
  let context = Context(console: console,
                        implementationName: "LispKit",
                        implementationVersion: "1.4.0",
                        commandLineArguments: cmdLineArgs)
#else
  let context = Context(console: console,
                        commandLineArguments: cmdLineArgs)
#endif

// Set up LispKit engine
for p in filePaths.value {
  printError(if: !context.fileHandler.addSearchPath(p), "cannot add search path: \(p)")
}
for p in libPaths.value {
  printError(if: !context.fileHandler.addLibrarySearchPath(p), "cannot add library path: \(p)")
}
if let capacity = heapSize.value {
  context.heap.reserveCapacity(capacity)
}

// Guarantee that (lispkit dynamic) is imported
do {
  try context.environment.import(["lispkit", "dynamic"])
} catch let error as RuntimeError {
  print("cannot import core lispkit libraries: \(error.message)")
  exit(1)
} catch let error as NSError {
  print("cannot import core lispkit libraries: \(error.localizedDescription)")
  exit(1)
} catch {
  print("cannot import core lispkit libraries")
  exit(1)
}

// Install error handler
if let dynamicLib = context.libraries.lookup("lispkit", "dynamic") as? DynamicControlLibrary,
  let raiseProc = dynamicLib.raiseProc {
  context.machine.raiseProc = raiseProc
}

// Import initial libraries
for lib in importLibs.value {
  // Remove outer parenthesis if needed; this is to allow users to provide a more readable
  // library name (same expression as within the repl)
  var initialLib = lib
  if initialLib.first == "(" && initialLib.last == ")" {
    initialLib.removeFirst()
    initialLib.removeLast()
  }
  var name: [String] = []
  for s in initialLib.split(separator: " ") {
    name.append(String(s))
  }
  do {
    try context.environment.import(name)
  } catch let error as RuntimeError {
    printError("error importing (\(initialLib)): \(error.message)")
  } catch let error as NSError {
    printError("error importing (\(initialLib)): \(error.localizedDescription)")
  } catch {
    printError("error importing (\(initialLib))")
  }
}

// Load prelude
print(Context.defaultPreludePath)
if let ppath = prelude.value ?? (flags.parameters.isEmpty ? Context.defaultPreludePath : nil) {
  do {
    _ = try context.machine.eval(file: ppath, in: context.global)
  } catch let error as RuntimeError {
    printError("cannot evaluate prelude \(ppath): \(error.message)")
  } catch let error as NSError {
    printError("cannot evaluate prelude \(ppath): \(error.localizedDescription)")
  } catch {
    printError("cannot evaluate prelude \(ppath)")
  }
}

func printResult(_ res: Expr) {
  // For multiple values being returned, print each value on a separate line
  if case .values(let expr) = res {
    var next = expr
    while case .pair(let x, let rest) = next {
      console.print("\(x.description)\n")
      next = rest
    }
  // For errors print the error message
  } else if case .error(let err) = res {
    console.print("\(err.printableDescription())\n")
  // For non-void results, print result
  } else if res != .void {
    console.print("\(res.description)\n")
  }
}

// Distinguish interactive usage (via REPL) from non-interactive usage
if let program = flags.parameters.first {
  do {
    let res = try context.machine.eval(file: program, in: context.global)
    if context.machine.exitTriggered {
      if res != .true {
        print("abnormal exit: \(res.description)\n")
        exit(1)
      }
    } else {
      printResult(res)
      if case .error(_) = res {
        exit(1)
      }
    }
  } catch let error as RuntimeError {
    print("cannot execute \(program): \(error.message)")
    exit(1)
  } catch let error as NSError {
    print("cannot execute \(program): \(error.localizedDescription)")
    exit(1)
  } catch {
    print("cannot execute \(program)")
    exit(1)
  }
} else {
  // Print header
  let props = Terminal.fullColorSupport ? TextStyle.bold.properties : TextProperties.none
  printOpt(props.apply(to: "\(AppInfo.name) \(AppInfo.version)\(AppInfo.buildAnnotation)"))
  printOpt(props.apply(to: "\(AppInfo.copyright)"))
  // Enter read-eval-print loop
  var buffer = ""
  while let line = readCommand(withPrompt: buffer.isEmpty) {
    buffer += line + " "
    let res = context.machine.onTopLevelDo {
      return try context.machine.eval(str: buffer, in: context.global)
    }
    // Exit loop if the machine has executed the `exit` function
    if context.machine.exitTriggered {
      if res != .true {
        print("abnormal exit: \(res.description)\n")
        exit(1)
      }
      break
      // If closing parenthesis are missing, keep on reading
    } else if case .error(let err) = res,
      case .syntax(.closingParenthesisMissing) = err.descriptor {
      continue
      // Else print result
    } else {
      printResult(res)
    }
    // Store buffer in the history of the line reader
    ln?.addHistory(buffer.trimmingCharacters(in: CharacterSet.whitespacesAndNewlines))
    // Empty buffer
    buffer = ""
  }
}
exit(0)
