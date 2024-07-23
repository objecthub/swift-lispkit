//
//  LispKitRepl.swift
//  LispKit
//
//  Created by Matthias Zenger on 12/07/2024.
//  Copyright © 2024 ObjectHub. All rights reserved.
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
import LispKitTools

@main
struct LispKitRepl {
  static func main() {
    let repl = LispKitTools.LispKitRepl(name: AppInfo.name,
                                        version: AppInfo.version,
                                        build: AppInfo.buildAnnotation,
                                        copyright: AppInfo.copyright,
                                        prompt: AppInfo.prompt)
    let features = ["repl"]
    guard repl.flagsValid() else {
      exit(1)
    }
    if repl.shouldRunRepl() {
      if LispKitContext.bundle == nil {
        guard repl.configurationSuccessfull(implementationName: "LispKit",
                                            implementationVersion: "2.4.0",
                                            includeInternalResources: false,
                                            defaultDocDirectory: "LispKit",
                                            features: features) else {
          exit(1)
        }
      } else {
        guard repl.configurationSuccessfull(features: features) else {
          exit(1)
        }
      }
      // Execute the read-eval-print loop in a new thread
      if repl.runloop.wasSet {
        repl.toolMessage = "[enabled run loop]"
        let main = Thread {
          let succeeded = repl.run()
          repl.release()
          exit(succeeded ? 0 : 1)
        }
        // Set stack size of interpreter thread (12 MByte by default)
        main.stackSize = (repl.systemStackSize.value ?? (12 * 1024)) * 1024
        main.qualityOfService = .userInitiated
        main.start()
        RunLoop.current.run()
      } else {
        let succeeded = repl.run()
        repl.release()
        exit(succeeded ? 0 : 1)
      }
    }
  }
}
