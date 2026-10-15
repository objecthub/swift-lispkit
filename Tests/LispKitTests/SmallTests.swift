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

  /// Aborting threads while they are blocked on (or just waking up from) a shared queue
  /// reads `EvalThread.waitingOn` concurrently with the threads updating it.
  func testAbortThreadsBlockedOnSharedQueue() {
    XCTAssertEqual(
      self.eval("(import (lispkit thread) (lispkit thread shared-queue)) " +
                "(define (abort-round) " +
                "  (let* ((q (make-shared-queue 50)) " +
                "         (ws (map (lambda (k) " +
                "                    (make-thread " +
                "                      (lambda () " +
                "                        (do ((x (shared-queue-dequeue/wait! q) " +
                "                                (shared-queue-dequeue/wait! q))) " +
                "                            ((not x)))))) " +
                "                  '(1 2 3 4))) " +
                "         (prod (spawn (lambda () " +
                "                        (do ((j 0 (+ j 1))) ((= j 4000)) " +
                "                          (shared-queue-enqueue/wait! q j 0.5)))))) " +
                "    (for-each thread-start! ws) " +
                "    (thread-sleep! 0.002) " +
                "    (for-each thread-terminate! ws) " +
                "    (shared-queue-close! q) " +
                "    (thread-terminate! prod))) " +
                "(do ((i 0 (+ i 1))) ((= i 100)) (abort-round)) " +
                "'done"), self.value("done"))
  }

  /// Starting threads concurrently reads the number of threads, while threads are being
  /// registered and removed.
  func testStartThreadsConcurrently() {
    XCTAssertEqual(
      self.eval("(import (lispkit thread)) " +
                "(define (spawn-round) " +
                "  (let ((spawners " +
                "          (map (lambda (k) " +
                "                 (spawn (lambda () " +
                "                          (do ((j 0 (+ j 1))) ((= j 60)) " +
                "                            (thread-start! (make-thread (lambda () 'x))))))) " +
                "               '(1 2 3 4)))) " +
                "    (for-each thread-join! spawners))) " +
                "(do ((i 0 (+ i 1))) ((= i 100)) (spawn-round)) " +
                "'done"), self.value("done"))
  }

}
