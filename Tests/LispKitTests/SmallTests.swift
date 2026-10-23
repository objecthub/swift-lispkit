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

  /// Bootstraps the context the way the context of a REPL is bootstrapped. Without this,
  /// errors thrown by native procedures cannot be caught by Scheme code.
  private func bootstrapContext() {
    do {
      try self.context?.bootstrap()
    } catch {
      preconditionFailure("cannot bootstrap test context: \(error)")
    }
  }

  /// Errors thrown by the virtual machine and by native procedures were raised only after
  /// all frames of the evaluation were gone. A handler that dealt with such an error at the
  /// top level thus ended the evaluation of all expressions that followed. They need to be
  /// evaluated, just like after errors raised by `error` and `raise`.
  func testHandlingThrownErrorsAtTopLevel() {
    self.bootstrapContext()
    XCTAssertEqual(
      self.eval("(define log '()) " +
                "(define (note x) (set! log (cons x log))) " +
                "(note (guard (e (#t 'type-error)) (car 5))) " +
                "(note (guard (e (#t 'arity-error)) ((lambda (a) a) 1 2))) " +
                "(note (guard (e (#t 'unbound-variable)) undefined-variable)) " +
                "(note (guard (e (#t 'range-error)) (vector-ref (vector 1 2) 7))) " +
                "(note (guard (e (#t 'error)) (error \"error\"))) " +
                "(note (guard (e (#t 'raise)) (raise 'x))) " +
                "(reverse log)"),
      self.value("(type-error arity-error unbound-variable range-error error raise)"))
    assertStackEmpty()
  }

  /// A handler that deals with an error thrown by the virtual machine at the top level needs
  /// to see the same dynamic environment as before: it runs after the `after` thunks of
  /// `dynamic-wind` that are exited by the handler, and a handler that returns for such an
  /// error leads to a secondary error that the outer handler gets.
  func testHandlingThrownErrorsInDynamicContext() {
    self.bootstrapContext()
    XCTAssertEqual(
      self.eval("(define log '()) " +
                "(define (note x) (set! log (cons x log))) " +
                "(guard (e (#t (note 'handler))) " +
                "  (dynamic-wind " +
                "    (lambda () (note 'in)) " +
                "    (lambda () (car 5)) " +
                "    (lambda () (note 'out)))) " +
                "(note (guard (e (#t 'outer)) " +
                "        (with-exception-handler " +
                "          (lambda (e) (note 'inner) 42) " +
                "          (lambda () (+ 1 (car 5)))))) " +
                "(reverse log)"),
      self.value("(in out handler inner outer)"))
    assertStackEmpty()
  }

  /// An error that is not handled at the top level still ends the evaluation. This also holds
  /// for errors that follow a handled error, and for errors in nested evaluations, which are
  /// handled by handlers outside of the nested evaluation.
  func testUncaughtThrownErrorsEndEvaluation() {
    self.bootstrapContext()
    let res = self.eval("(define n 0) " +
                        "(guard (e (#t #f)) (car 5)) " +
                        "(set! n 1) " +
                        "(car 9) " +
                        "(set! n 2)")
    guard case .error(_) = res else {
      XCTFail("uncaught error did not end the evaluation: \(res)")
      return
    }
    assertStackEmpty()
    XCTAssertEqual(self.eval("n"), self.value("1"))
    XCTAssertEqual(
      self.eval("(define m 0) " +
                "(define r (guard (e (#t 'caught)) " +
                "           (eval '(begin (set! m 1) (car 5) (set! m 2)) (interaction-environment)))) " +
                "(list r m)"),
      self.value("(caught 1)"))
    assertStackEmpty()
  }

}
