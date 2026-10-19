//
//  MacroTests.swift
//  LispKitTests
//
//  Created by Matthias Zenger on 07/05/2016.
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

import XCTest
import LispKit

///
/// This test case class implements all regression tests stored in group
/// `LispKitTests/Code`. Standard out contains information about the progress of the
/// regression test.
///
class RegressionTests: LispKitTestCase {
  
  func testDefinitions() {
    self.execute(file: "Definitions")
  }
  
  func testControlFlow() {
    self.execute(file: "ControlFlow")
  }

  func testSyntaxRules() {
    self.execute(file: "SyntaxRules")
  }

  func testLocalSyntaxRules() {
    self.execute(file: "LocalSyntaxRules")
  }
  
  func testCallCC() {
    self.execute(file: "CallCC")
  }
  
  func testDynamicWind() {
    self.execute(file: "DynamicWind")
  }
  
  func testParameters() {
    self.execute(file: "Parameters")
  }
  
  func testDelayedEvaluation() {
    self.execute(file: "DelayedEvaluation")
  }
  
  func testLightweightTypes() {
    self.execute(file: "LightweightTypes")
  }
  
  func testProcedures() {
    self.execute(file: "Procedures")
  }
  
  func testMath() {
    self.execute(file: "Math")
  }
  
  func testVectors() {
    self.execute(file: "Vectors")
  }
  
  func testHashTables() {
    self.execute(file: "HashTables")
  }
  
  func testPorts() {
    self.execute(file: "Ports")
  }
  
  func testRecords() {
    self.execute(file: "Records")
  }
  
  func testLibraries() {
    self.execute(file: "Libraries")
  }
  
  func testDatatypes() {
    self.execute(file: "Datatypes")
  }
  
  func testEnvironments() {
    self.execute(file: "Environments")
  }

  func testMarkdown() {
    self.execute(file: "Markdown")
  }
  
  func testMarkdownLibraryConfigDefaults() {
    // The default configuration finds the scripts and themes that come with LispKit
    let (highlighter, renderer) = MarkdownLibrary.Config().apply(in: self.context!)
    XCTAssertNotNil(highlighter)
    XCTAssertGreaterThan(highlighter?.availableThemes.count ?? 0, 0)
    XCTAssertNotNil(renderer)
  }
  
  func testMarkdownLibraryConfigOverride() throws {
    // A configured theme directory replaces the default one
    let dir = FileManager.default.temporaryDirectory
                .appendingPathComponent("LispKitThemes-\(UUID().uuidString)", isDirectory: true)
    try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: dir) }
    try ".hljs { color: #333; }".write(to: dir.appendingPathComponent("custom.css"),
                                       atomically: true,
                                       encoding: .utf8)
    let original = MarkdownLibrary.config
    MarkdownLibrary.config = MarkdownLibrary.Config(themeDirectory: dir)
    defer { MarkdownLibrary.config = original }
    XCTAssertEqual(self.eval("(import (lispkit markdown)) (syntax-highlighting-themes)"),
                   self.value("(\"custom\")"))
    XCTAssertEqual(self.eval("(syntax-highlighting-theme \"custom\")"),
                   self.value("\".hljs { color: #333; }\""))
  }
  
  func testMakeBitmapFromDrawing() {
    XCTAssertEqual(self.eval("""
      (import (lispkit draw))
      (define (tile) (drawing (fill (rectangle (point 0 0) (size 20 10)))))
      (list (bitmap-pixels (make-bitmap (tile) (size 40 20)))
            (bitmap-pixels (make-bitmap (tile) (size 40 20) 144))
            (bitmap-ppi (make-bitmap (tile) (size 40 20) 144)))
      """), self.value("((40 . 20) (80 . 40) 144.0)"))
    // A drawing needs a size
    self.assertError("(make-bitmap (tile))")
    self.assertError("(make-bitmap (tile) #f 144)")
    // The object to draw has to be a drawing or an image
    self.assertError("(make-bitmap 42 (size 40 20))")
    // There are at least 1 and at most 4 arguments
    self.assertError("(make-bitmap)")
    self.assertError("(make-bitmap (tile) (size 40 20) 72 0 #f)")
  }

  func testMakeBitmapFromImage() {
    XCTAssertEqual(self.eval("""
      (import (lispkit draw))
      (define (tile) (drawing (fill (rectangle (point 0 0) (size 20 10)))))
      (define source (make-bitmap (tile) (size 40 20) 144))
      (list (bitmap-size source) (bitmap-pixels source) (bitmap-ppi source))
      """), self.value("((40.0 . 20.0) (80 . 40) 144.0)"))
    // Without size and ppi, the bitmap has the size and resolution of the image
    XCTAssertEqual(self.eval("""
      (define (describe b) (list (bitmap-size b) (bitmap-pixels b) (bitmap-ppi b)))
      (describe (make-bitmap source))
      """), self.value("((40.0 . 20.0) (80 . 40) 144.0)"))
    // #f selects the default size or ppi
    XCTAssertEqual(self.eval("(describe (make-bitmap source #f 72))"),
                   self.value("((40.0 . 20.0) (40 . 20) 72.0)"))
    XCTAssertEqual(self.eval("(describe (make-bitmap source (size 10 5) #f))"),
                   self.value("((10.0 . 5.0) (20 . 10) 144.0)"))
    // Size and ppi can be chosen freely, and the interpolation quality is optional
    XCTAssertEqual(self.eval("(describe (make-bitmap source (size 10 5) 288))"),
                   self.value("((10.0 . 5.0) (40 . 20) 288.0)"))
    XCTAssertEqual(self.eval("(describe (make-bitmap source (size 10 5) 72 1))"),
                   self.value("((10.0 . 5.0) (10 . 5) 72.0)"))
    XCTAssertEqual(self.eval("(describe (make-bitmap source #f #f 3))"),
                   self.value("((40.0 . 20.0) (80 . 40) 144.0)"))
    // The content of the image is neither flipped nor moved
    XCTAssertEqual(self.eval("""
      (define direct (make-bitmap (tile) (size 40 20)))
      (equal? (bitmap->bytevector direct 'png)
              (bitmap->bytevector (make-bitmap (make-bitmap (tile) (size 40 20))) 'png))
      """), .true)
    XCTAssertEqual(self.eval("""
      (equal? (bitmap->bytevector direct 'png)
              (bitmap->bytevector (make-bitmap (make-bitmap (tile) (size 40 20)) (size 40 20) 72) 'png))
      """), .true)
    XCTAssertEqual(self.eval("""
      (equal? (bitmap->bytevector (make-bitmap (tile) (size 40 20) 144) 'png)
              (bitmap->bytevector (make-bitmap source) 'png))
      """), .true)
    // Interpolation quality, size and ppi must be in range
    self.assertError("(make-bitmap source #f #f 5)")
    self.assertError("(make-bitmap source (size 0 5))")
    self.assertError("(make-bitmap source #f 0)")
  }
  
  private func assertError(_ code: String, file: StaticString = #filePath, line: UInt = #line) {
    if case .error(_) = self.eval(code) {
      return
    }
    XCTFail("expected error: \(code)", file: file, line: line)
  }

  func testSQLite() {
    self.execute(file: "SQLite")
  }
  
  func testLogic() {
    self.execute(file: "Logic")
  }
  
  func testFormat() {
    self.execute(file: "Format")
  }
  
  func testJSON() {
    self.execute(file: "JSON")
  }
  
  func testSRFI19() {
    self.execute(file: "SRFI19")
  }
  
  func testSRFI35() {
    self.execute(file: "SRFI35")
  }
  
  func testSRFI69() {
    self.execute(file: "SRFI69")
  }
  
  func testSRFI113() {
    self.execute(file: "SRFI113")
  }
  
  func testSRFI121() {
    self.execute(file: "SRFI121")
  }
  
  func testSRFI132() {
    self.execute(file: "SRFI132")
  }
  
  func testSRFI133() {
    self.execute(file: "SRFI133")
  }
  
  func testSRFI134() {
    self.execute(file: "SRFI134")
  }
  
  func testSRFI135() {
    self.execute(file: "SRFI135")
  }
  
  func testSRFI152() {
    self.execute(file: "SRFI152")
  }
}
