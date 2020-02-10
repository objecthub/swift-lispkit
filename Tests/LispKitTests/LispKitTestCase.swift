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
  public let terminal = CommandLineDelegate()
  
  /// LispKit context object; created on demand before every test method is executed
  public var context: Context? = nil
  
  /// Representation of an individual regression test
  public struct Test {
    let description: String
    let sourceId: UInt16
    let source: Expr
    let target: Expr
  }
  
  override open func setUp() {
    super.setUp()
    #if SPM
      self.context = Context(delegate: self.terminal)
      let root = URL(fileURLWithPath: "Sources/LispKit/Resources", isDirectory: true)
      _ = self.context?.fileHandler.prependSearchPath(root.path)
      _ = self.context?.fileHandler.prependLibrarySearchPath(root
                                     .appendingPathComponent("Libraries", isDirectory: true).path)
      do {
        try self.context?.environment.import(BaseLibrary.name)
      } catch {
        preconditionFailure("cannot import (lispkit base) into test context")
      }
    #else
      self.context = Context(delegate: terminal)
      do {
        try self.context?.environment.import(BaseLibrary.name)
      } catch {
        preconditionFailure("cannot import (lispkit base) into test context")
      }
    #endif
  }
  
  override open func tearDown() {
    self.context = nil
    super.tearDown()
  }
  
  public func eval(_ string: String) -> Expr {
    return self.context!.machine.onTopLevelDo {
      return try self.context!.machine.eval(str: string,
                                            sourceId: SourceManager.consoleSourceId,
                                            in: context!.global)
    }
  }
  
  public func value(_ str: String) -> Expr {
    do {
      let input = TextInput(string: str)
      return try Parser(symbols: self.context!.symbols, input: input).parse().datum
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
  
  public func loadTestCode(from filename: String) -> (UInt16, String)? {
    #if SPM
      let path = "Tests/LispKitTests/Code/\(filename).scm"
      do {
        return try self.context?.sources.readSource(for: path)
      } catch {
        return nil
      }
    #else
      let bundle = Bundle(for: type(of: self))
      if let path = bundle.path(forResource: filename, ofType: "scm") {
        do {
          return try self.context?.sources.readSource(for: path)
        } catch {
          return nil
        }
      }
    #endif
    return nil
  }
  
  public func loadTests(from filename: String) -> [Test] {
    guard let (sourceId, code) = self.loadTestCode(from: filename) else {
      preconditionFailure("cannot open test file: \(filename)")
    }
    do {
      let input = TextInput(string: code)
      let parser = Parser(symbols: self.context!.symbols, input: input, sourceId: sourceId)
      var res = [Test]()
      while !parser.finished {
        let spec = try parser.parse().datum // TODO: remove .datum
        guard case .pair(.string(let descr), .pair(let target, let source)) = spec else {
          preconditionFailure("malformed test spec in file \(filename): \(spec)")
        }
        res.append(Test(description: descr.description,
                        sourceId: sourceId,
                        source: source,
                        target: target))
      }
      return res
    } catch let error {
      preconditionFailure("error while loading tests \(filename): \(error)")
    }
  }
  
  public func execute(tests: [Test]) {
    for test in tests {
      print("───────────────────────────────────────────────────────────────────────")
      print("✅ \(test.description)")
      print("expected: \(test.target)")
      let res = self.context!.machine.onTopLevelDo {
        return try self.context!.machine.eval(exprs: test.source, in: context!.global)
      }
      print("computed: \(res)")
      if case .symbol(let sym) = test.target, sym.rawIdentifier == "<error>" {
        if case .error(_) = res {
          // expected error
        } else {
          XCTFail("🛑 \(test.description)")
        }
      } else {
        XCTAssertEqual(res, test.target, "🛑 \(test.description)")
      }
      assertStackEmpty(after: test.description)
    }
  }
  
  public func execute(file filename: String) {
    self.execute(tests: self.loadTests(from: filename))
  }
}
