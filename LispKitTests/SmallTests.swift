//
//  LispKitTests.swift
//  LispKitTests
//
//  Created by Matthias Zenger on 14/01/2016.
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


class SmallTests: LispKitTestCase {
  
  func testPlus() {
    XCTAssertEqual(self.eval("(+ 1 2 3 4)"), self.value("10"))
    assertStackEmpty()
  }
  
  func testCapturesInLetrec() {
    XCTAssertEqual(
      self.eval("(define (foo x) " +
                "  (letrec ((mult (lambda (n) (* n x y))) (y (+ x x))) (+ x y (mult 3))))" +
                "(foo 12)"), self.value("900"))
    assertStackEmpty()
  }

}
