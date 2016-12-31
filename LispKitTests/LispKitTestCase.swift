//
//  LispKitTestCase.swift
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
@testable import LispKit

///
/// Class `LispKitTestCase` implements a base class for functional LispKit tests.
/// Each instance of `LispKitTestCase` maintains a LispKit virtual machine that is
/// being re-created from scratch for every invocation of test functions. Each
/// `LispKitTestCase` provides functionality for parsing LispKit expressions and
/// evaluating them.
/// 
/// There is also support for data-driven regression tests. These regression tests
/// are specified in .scm files that are stored in the bundle associated with the
/// `LispKitTests` target of this project. Such regression test files have the
/// following structure:
///
///    RegressionTests  =  RegressionTest RegressionTests
///    RegressionTest   =  '(' TestName TargetValue SourceExpr ')'
///    TestName         =  String
///    TargetValue      =  LispKitExpr
///    SourceExpr       =  LispKitExpr
/// 
/// Method `loadTests` reads such a file and parses its content into individual `Test`
/// struct objects. Method `executeTests` iterates through all tests and executes each
/// in the following way:
///    1. The source expression is being evaluates
///    2. The result of the evaluation is compared to the target value
///    3. If the two values are not equivalent using `equals?`, the test fails
///
open class LispKitTestCase: XCTestCase {
  
  /// Default console implementation
  public let console = CommandLineConsole()
  
  /// LispKit context object; created on demand before every test method is executed
  public var context: Context? = nil
  
  /// Representation of an individual regression test
  public struct Test {
    let description: String
    let source: Expr
    let target: Expr
  }
  
  override open func setUp() {
    super.setUp()
    self.context = Context(console: console)
    do {
      try self.context?.environment.import(SchemeLibrary.name)
    } catch {
      preconditionFailure("cannot import (base scheme) into test context")
    }
  }
  
  override open func tearDown() {
    self.context = nil
    super.tearDown()
  }
  
  public func eval(_ string: String) -> Expr {
    return self.context!.machine.evalOnTopLevel(str: string, in: context!.global)
  }
  
  public func value(_ str: String) -> Expr {
    do {
      return try Parser(symbols: self.context!.symbols, src: str).parse()
    } catch {
      preconditionFailure("malformed expression: \(str)")
    }
  }
  
  public func assertStackEmpty(after: String? = nil) {
    if let after = after {
      XCTAssertEqual(self.context!.machine.sp, 0, "stack not empty: \(after)")
    } else {
      XCTAssertEqual(self.context!.machine.sp, 0, "stack not empty")
    }
  }
  
  public func loadTestCode(from filename: String) -> String? {
    let bundle = Bundle(for: type(of: self))
    if let path = bundle.path(forResource: filename, ofType: "scm") {
      do {
        return try String(contentsOfFile: path, encoding: String.Encoding.utf8)
      } catch {
        return nil
      }
    }
    return nil
  }
  
  public func loadTests(from filename: String) -> [Test] {
    guard let code = loadTestCode(from: filename) else {
      preconditionFailure("cannot open test file: \(filename)")
    }
    do {
      let parser = Parser(symbols: self.context!.symbols, src: code)
      var res = [Test]()
      while !parser.finished {
        let spec = try parser.parse()
        guard case .pair(.string(let descr), .pair(let target, let source)) = spec else {
          preconditionFailure("malformed test spec in file \(filename): \(spec)")
        }
        res.append(Test(description: descr.description, source: source, target: target))
      }
      return res
    } catch let error {
      preconditionFailure("error while loading tests \(filename): \(error)")
    }
  }
  
  public func execute(tests: [Test]) {
    for test in tests {
      print("-----------------------")
      print("source: \(test.source)")
      print("target: \(test.target)")
      let res = self.context!.machine.evalOnTopLevel(exprs: test.source, in: context!.global)
      print("result: \(res)")
      XCTAssertEqual(res, test.target, test.description)
      assertStackEmpty(after: test.description)
    }
  }
  
  public func execute(file filename: String) {
    self.execute(tests: self.loadTests(from: filename))
  }
}
