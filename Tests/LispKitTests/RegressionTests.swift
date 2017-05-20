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
  
  func testHashTables() {
    self.execute(file: "HashTables")
  }
  
  func testRecords() {
    self.execute(file: "Records")
  }
  
  func testPorts() {
    self.execute(file: "Ports")
  }
  
  func testSRFI19() {
    self.execute(file: "SRFI19")
  }
  
  func testSRFI121() {
    self.execute(file: "SRFI121")
  }
  
  func testSRFI132() {
    self.execute(file: "SRFI132")
  }
  
  func testSRFI134() {
    self.execute(file: "SRFI134")
  }
}
