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

  /// `exit` ends the evaluation of all expressions that follow it, including the ones
  /// following the expression that calls `exit` in a program. The result of the evaluation
  /// is the value that was passed to `exit`.
  func testExitEndsEvaluation() {
    self.bootstrapContext()
    XCTAssertEqual(
      self.eval("(define n 0) (set! n 1) (exit 5) (set! n 2) 'unreachable"), self.value("5"))
    XCTAssertTrue(self.context!.evaluator.exitTriggered)
    assertStackEmpty()
    XCTAssertEqual(self.eval("n"), self.value("1"))
    XCTAssertFalse(self.context!.evaluator.exitTriggered)
    // Without argument, `exit` stands for success
    XCTAssertEqual(self.eval("(define n 0) (begin (set! n 1) (exit) (set! n 2)) (set! n 3)"),
                   .true)
    XCTAssertEqual(self.eval("n"), self.value("1"))
    // Calling `exit` in a procedure or in a handler works as well
    XCTAssertEqual(
      self.eval("(define (stop) (set! n 4) (exit 6) (set! n 5)) " +
                "(guard (e (#t (stop))) (car 'x)) (set! n 7)"), self.value("6"))
    XCTAssertEqual(self.eval("n"), self.value("4"))
    assertStackEmpty()
  }

  /// `exit` runs the `after` procedures of all outstanding calls of `dynamic-wind`.
  func testExitRunsDynamicWindAfterProcedures() {
    XCTAssertEqual(
      self.eval("(define log '()) " +
                "(define (note x) (set! log (cons x log))) " +
                "(dynamic-wind " +
                "  (lambda () (note 'in)) " +
                "  (lambda () (dynamic-wind " +
                "               (lambda () (note 'in2)) " +
                "               (lambda () (exit 3)) " +
                "               (lambda () (note 'out2)))) " +
                "  (lambda () (note 'out))) " +
                "(note 'after)"), self.value("3"))
    XCTAssertEqual(self.eval("(reverse log)"), self.value("(in in2 out2 out)"))
    assertStackEmpty()
  }

  /// The exit status for the argument of `exit` and `emergency-exit` follows R7RS.
  func testExitStatus() {
    XCTAssertEqual(Evaluator.exitStatus(of: nil), 0)
    XCTAssertEqual(Evaluator.exitStatus(of: .true), 0)
    XCTAssertEqual(Evaluator.exitStatus(of: .false), 1)
    XCTAssertEqual(Evaluator.exitStatus(of: .fixnum(0)), 0)
    XCTAssertEqual(Evaluator.exitStatus(of: .fixnum(3)), 3)
    XCTAssertEqual(Evaluator.exitStatus(of: .fixnum(-1)), -1)
    XCTAssertNil(Evaluator.exitStatus(of: self.value("foo")))
    XCTAssertNil(Evaluator.exitStatus(of: .makeString("done")))
    XCTAssertNil(Evaluator.exitStatus(of: .flonum(1.5)))
  }

  /// `exit` terminates the whole program, also if it is called on a thread other than the main
  /// thread: the evaluation of the main thread gets aborted, even if the main thread is
  /// blocked, and the result of the evaluation is the object that was passed to `exit`.
  func testExitOnThreadEndsEvaluation() {
    let start = Date()
    XCTAssertEqual(
      self.eval("(import (lispkit thread)) (define n 0) " +
                "(define t (go (thread-sleep! 0.1) (set! n 1) (exit 7) (set! n 2))) " +
                "(thread-sleep! 20) (set! n 3) 'unreachable"), self.value("7"))
    XCTAssertLessThan(Date().timeIntervalSince(start), 10.0, "main thread was not woken up")
    XCTAssertTrue(self.context!.evaluator.exitTriggered)
    assertStackEmpty()
    XCTAssertEqual(self.eval("n"), self.value("1"))
    // `exit` on a thread runs the `after` procedures of the thread, and it ends a main thread
    // that is joining the thread
    XCTAssertEqual(
      self.eval("(define log '()) " +
                "(define t (go (dynamic-wind " +
                "                (lambda () (set! log (cons 'in log))) " +
                "                (lambda () (exit)) " +
                "                (lambda () (set! log (cons 'out log)))))) " +
                "(thread-join! t) (set! log (cons 'after log))"), .true)
    XCTAssertEqual(self.eval("(reverse log)"), self.value("(in out)"))
    assertStackEmpty()
  }

  /// `shape-contains?` takes both coordinates of the points of a list into account, just like
  /// it does for a single point. For lists of points, it used to ignore the y-coordinates.
  func testShapeContainsListOfPoints() {
    XCTAssertEqual(
      self.eval("(import (lispkit draw)) " +
                "(define square (closed-polygon '((50.0 . 50.0) (300.0 . 50.0) " +
                "                                 (300.0 . 300.0) (50.0 . 300.0)))) " +
                "(define low (closed-polygon '((50.0 . -50.0) (300.0 . -50.0) " +
                "                              (300.0 . 50.0) (50.0 . 50.0)))) " +
                "(define inside '((100.0 . 100.0) (200.0 . 100.0) (200.0 . 200.0) (100.0 . 200.0))) " +
                "(define partly '((100.0 . 100.0) (400.0 . 100.0))) " +
                "(define outside '((400.0 . 400.0) (500.0 . 500.0))) " +
                "(list (shape-contains? square '(100.0 . 100.0)) " +
                "      (shape-contains? square '(100.0 . 400.0)) " +
                "      (shape-contains? square inside) " +
                "      (shape-contains? square partly) " +
                "      (shape-contains? square partly #t) " +
                "      (shape-contains? square outside #t) " +
                "      (shape-contains? low '((100.0 . 200.0))) " +
                "      (shape-contains? low '((100.0 . 0.0))) " +
                "      (shape-contains? square '()))"),
      self.value("(#t #f #t #f #t #f #f #t #t)"))
  }

}
