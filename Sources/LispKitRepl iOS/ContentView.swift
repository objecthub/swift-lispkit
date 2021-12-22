//
//  ContentView.swift
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

import SwiftUI

struct ContentView: View {
  private static let toolbarItemSize: CGFloat = 19
  
  @EnvironmentObject var interpreter: Interpreter
  @State private var consoleInput = ""
  @State private var showAbortAlert = false
  @State private var showResetActionSheet = false
  
  var body: some View {
    NavigationView {
      VStack(alignment: .leading, spacing: 0) {
        ConsoleView(
            font: .system(.caption, design: .monospaced),
            infoFont: .system(.footnote),
            action: {
              let old = self.consoleInput
              self.consoleInput = ""
              let input: String
              if self.interpreter.isReady {
                input = ContentView.canonicalizeInput(old)
                self.interpreter.consoleContent.append(ConsoleOutput(kind: .command, text: input))
              } else {
                input = old
              }
              self.interpreter.evaluate(input, reset: {
                self.consoleInput = old
                self.interpreter.consoleContent.removeLast()
              })
            },
            content: $interpreter.consoleContent,
            input: $consoleInput,
            readingStatus: $interpreter.readingStatus,
            ready: $interpreter.isReady)
        Spacer()
      }
      .navigationBarTitle("LispPadRepl", displayMode: .inline)
      .navigationBarItems(
        trailing: HStack(alignment: .center, spacing: 16) {
          if self.interpreter.isReady {
            Button(action: {
              self.showResetActionSheet = true
            }) {
              Image(systemName: "trash")
                .font(Font.system(size: ContentView.toolbarItemSize, weight: .light))
            }
            .actionSheet(isPresented: $showResetActionSheet) {
              ActionSheet(
                title: Text("Reset"),
                message: Text("Discard console output and reset Scheme interpreter?"),
                buttons: [.default(Text("Reset console"), action: {
                            self.interpreter.consoleContent.removeAll()
                          }),
                          .destructive(Text("Reset interpreter"), action: {
                            _ = self.interpreter.reset()
                          }),
                          .destructive(Text("Reset console & interpreter"), action: {
                            self.interpreter.consoleContent.removeAll()
                            _ = self.interpreter.reset()
                          }),
                          .cancel()])
            }
          } else {
            Button(action: {
              self.showAbortAlert = true
            }) {
              Image(systemName: "stop.circle")
                .font(Font.system(size: ContentView.toolbarItemSize, weight: .light))
                .foregroundColor(.red)
            }
            .alert(isPresented: $showAbortAlert) {
              Alert(title: Text("Abort evaluation?"),
                    primaryButton: .cancel(),
                    secondaryButton: .destructive(Text("Abort"), action: {
                      self.interpreter.context?.evaluator.abort()
                    }))
            }
          }
        })
    }
    .navigationViewStyle(StackNavigationViewStyle())
  }
  
  static func canonicalizeInput(_ input: String) -> String {
    let str = input.trimmingCharacters(in: .whitespacesAndNewlines)
    var res = ""
    for ch in str {
      switch ch {
        case "‘":
          res += "'"
        case "“", "”", "„":
          res += "\""
        default:
          res.append(ch)
      }
    }
    return res
  }
}

struct ContentView_Previews: PreviewProvider {
  @StateObject static var interpreter = Interpreter()
  static var previews: some View {
    ContentView()
      .environmentObject(Self.interpreter)
  }
}
