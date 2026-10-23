//
//  HTTPServerTests.swift
//  LispKitTests
//
//  Created by Matthias Zenger on 16/10/2026.
//  Copyright © 2026 ObjectHub. All rights reserved.
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
import Foundation
@testable import LispKit

///
/// Tests for library `(lispkit http server)`. Every test starts an HTTP server on a free port of
/// the loopback address and sends requests to it via sockets. The requests are written by hand,
/// such that the tests control exactly what the server receives (e.g. paths with `%2e%2e`).
///
class HTTPServerTests: LispKitTestCase {

  /// The context of a test is bootstrapped, as the context of a REPL is. Without this, errors
  /// thrown by native procedures cannot be caught by Scheme code (`guard` or `try`), which
  /// the worker threads of an HTTP server rely on.
  override func setUp() {
    super.setUp()
    do {
      try self.context?.bootstrap()
    } catch {
      preconditionFailure("cannot bootstrap test context: \(error)")
    }
  }
  
  // MARK: - A minimal HTTP client

  struct Response {
    let status: Int
    /// The headers of the response; the names are in lower case
    let headers: [String : String]
    let body: [UInt8]

    var text: String {
      return String(decoding: self.body, as: UTF8.self)
    }

    func header(_ name: String) -> String? {
      return self.headers[name.lowercased()]
    }
  }

  struct TestError: Error, CustomStringConvertible {
    let description: String
  }

  /// A connection to a server on the loopback address, via which several requests can be sent.
  final class Client {
    /// Describes why the last attempt to connect, send or read failed
    static var lastFailure = ""
    
    private let fd: Int32
    private var buffer: [UInt8] = []

    init?(port: Int, timeout: Int = 10) {
      let fd = socket(AF_INET, SOCK_STREAM, 0)
      guard fd >= 0 else {
        return nil
      }
      var address = sockaddr_in()
      address.sin_len = UInt8(MemoryLayout<sockaddr_in>.size)
      address.sin_family = sa_family_t(AF_INET)
      address.sin_port = UInt16(port).bigEndian
      address.sin_addr.s_addr = inet_addr("127.0.0.1")
      let connected = withUnsafePointer(to: &address) {
        $0.withMemoryRebound(to: sockaddr.self, capacity: 1) {
          connect(fd, $0, socklen_t(MemoryLayout<sockaddr_in>.size))
        }
      }
      guard connected == 0 else {
        Client.lastFailure = "cannot connect to port \(port): errno \(errno)"
        Darwin.close(fd)
        return nil
      }
      var time = timeval(tv_sec: timeout, tv_usec: 0)
      setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &time, socklen_t(MemoryLayout<timeval>.size))
      var one: Int32 = 1
      setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &one, socklen_t(MemoryLayout<Int32>.size))
      self.fd = fd
    }

    deinit {
      Darwin.close(self.fd)
    }

    /// Writes a request.
    @discardableResult
    func send(method: String = "GET",
              path: String,
              headers: [String : String] = [:],
              body: [UInt8] = [],
              close: Bool = false) -> Bool {
      var request = "\(method) \(path) HTTP/1.1\r\nHost: localhost\r\n"
      if close {
        request += "Connection: close\r\n"
      }
      for (name, value) in headers {
        request += "\(name): \(value)\r\n"
      }
      if !body.isEmpty {
        request += "Content-Length: \(body.count)\r\n"
      }
      request += "\r\n"
      return self.send(bytes: Array(request.utf8) + body)
    }

    @discardableResult
    func send(bytes: [UInt8]) -> Bool {
      var offset = 0
      while offset < bytes.count {
        let written = bytes[offset...].withUnsafeBytes { write(self.fd, $0.baseAddress, $0.count) }
        guard written > 0 else {
          return false
        }
        offset += written
      }
      return true
    }

    /// Reads more bytes into the buffer. Returns false if the connection was closed by the
    /// server or if there was no data within the timeout.
    private func fill() -> Bool {
      var chunk = [UInt8](repeating: 0, count: 65536)
      let count = read(self.fd, &chunk, chunk.count)
      guard count > 0 else {
        Client.lastFailure = "read returned \(count) (errno \(errno)), buffered: " +
                             "\(String(decoding: self.buffer.prefix(200), as: UTF8.self))"
        return false
      }
      self.buffer.append(contentsOf: chunk[0..<count])
      return true
    }

    /// Returns the index of the first occurrence of `pattern` in the buffer.
    private func index(of pattern: [UInt8]) -> Int? {
      guard self.buffer.count >= pattern.count else {
        return nil
      }
      for i in 0...(self.buffer.count - pattern.count) {
        if self.buffer[i] == pattern[0] && Array(self.buffer[i..<i + pattern.count]) == pattern {
          return i
        }
      }
      return nil
    }

    /// Reads the response to a request with the given method.
    func readResponse(to method: String = "GET") -> Response? {
      var end = self.index(of: [13, 10, 13, 10])
      while end == nil {
        guard self.fill() else {
          return nil
        }
        end = self.index(of: [13, 10, 13, 10])
      }
      let head = String(decoding: self.buffer[0..<end!], as: UTF8.self)
      self.buffer.removeSubrange(0..<end! + 4)
      var lines = head.components(separatedBy: "\r\n")
      let statusLine = lines.removeFirst().split(separator: " ")
      guard statusLine.count >= 2, let status = Int(statusLine[1]) else {
        return nil
      }
      var headers: [String : String] = [:]
      for line in lines {
        if let colon = line.firstIndex(of: ":") {
          let name = line[..<colon].trimmingCharacters(in: .whitespaces).lowercased()
          headers[name] = line[line.index(after: colon)...].trimmingCharacters(in: .whitespaces)
        }
      }
      var body: [UInt8] = []
      if method == "HEAD" || status == 204 || status == 304 || (100..<200).contains(status) {
        // responses without a body
      } else if headers["transfer-encoding"]?.lowercased().contains("chunked") ?? false {
        guard let chunked = self.readChunkedBody() else {
          return nil
        }
        body = chunked
      } else if let length = headers["content-length"].flatMap({ Int($0) }) {
        while self.buffer.count < length {
          guard self.fill() else {
            return nil
          }
        }
        body = Array(self.buffer[0..<length])
        self.buffer.removeSubrange(0..<length)
      } else {
        while self.fill() {
        }
        body = self.buffer
        self.buffer = []
      }
      return Response(status: status, headers: headers, body: body)
    }

    private func readChunkedBody() -> [UInt8]? {
      var result: [UInt8] = []
      while true {
        var lineEnd = self.index(of: [13, 10])
        while lineEnd == nil {
          guard self.fill() else {
            return nil
          }
          lineEnd = self.index(of: [13, 10])
        }
        let sizeLine = String(decoding: self.buffer[0..<lineEnd!], as: UTF8.self)
        guard let size = Int(sizeLine.split(separator: ";").first.map(String.init) ?? "",
                             radix: 16) else {
          return nil
        }
        self.buffer.removeSubrange(0..<lineEnd! + 2)
        if size == 0 {
          // Skip the trailer up to the final empty line
          while true {
            var end = self.index(of: [13, 10])
            while end == nil {
              guard self.fill() else {
                return nil
              }
              end = self.index(of: [13, 10])
            }
            self.buffer.removeSubrange(0..<end! + 2)
            if end! == 0 {
              return result
            }
          }
        }
        while self.buffer.count < size + 2 {
          guard self.fill() else {
            return nil
          }
        }
        result.append(contentsOf: self.buffer[0..<size])
        self.buffer.removeSubrange(0..<size + 2)
      }
    }

    /// Returns true if the server has closed the connection (without sending anything else).
    func closedByServer() -> Bool {
      return self.buffer.isEmpty && !self.fill()
    }

    /// Sends a single request via a new connection, which is closed afterwards, and returns the
    /// response.
    static func fetch(port: Int,
                      method: String = "GET",
                      path: String,
                      headers: [String : String] = [:],
                      body: [UInt8] = []) -> Response? {
      guard let client = Client(port: port) else {
        return nil
      }
      guard client.send(method: method, path: path, headers: headers, body: body, close: true) else {
        return nil
      }
      return client.readResponse(to: method)
    }
  }

  // MARK: - Starting and stopping servers

  /// The virtual machine of the evaluator session a test is running in
  private var machine: VirtualMachine? = nil
  
  /// Runs a test in a single session of the evaluator. `LispKitTestCase.eval` terminates all
  /// threads (like the ones running HTTP servers) as soon as it returns, so the code of a test
  /// that starts servers needs to run within one session, in which it evaluates Scheme code
  /// via `scheme`. The threads are terminated when the session ends.
  private func inEvaluator(_ body: () throws -> Void) {
    _ = self.context!.evaluator.execute { machine in
      self.machine = machine
      defer {
        self.machine = nil
      }
      do {
        try body()
      } catch {
        XCTFail("\(error)")
      }
      return .void
    }
  }
  
  /// Evaluates Scheme code in the current session of the evaluator.
  private func scheme(_ code: String) -> Expr {
    guard let machine = self.machine else {
      preconditionFailure("not running in a session of the evaluator")
    }
    do {
      return try machine.eval(str: code, sourceId: SourceManager.consoleSourceId,
                              in: self.context!.global)
    } catch let error as RuntimeError {
      return .error(error)
    } catch {
      return .error(RuntimeError.os(error))
    }
  }
  
  /// Waits at most 10 seconds until the Scheme expression `code` evaluates to true.
  @discardableResult
  private func wait(until code: String) -> Bool {
    for _ in 0..<200 {
      if self.scheme(code) == .true {
        return true
      }
      Thread.sleep(forTimeInterval: 0.05)
    }
    return false
  }

  /// Defines an HTTP server in variable `server` by evaluating `definitions` after the server
  /// was created, and starts it on a separate thread. `address` is the Scheme expression for
  /// the address argument of `http-server-start!`. Returns the port of the running server.
  private func startServer(_ definitions: String = "",
                           workers: Int = 2,
                           queueSize: Int = 10,
                           address: String = "#f") throws -> Int {
    let result = self.scheme(#"""
      (import (lispkit base) (lispkit string) (lispkit thread) (lispkit thread shared-queue)
              (lispkit box) (lispkit json) (lispkit http server))
      (define server (make-http-server \#(queueSize) 4))
      \#(definitions)
      (define starter (spawn (thunk (http-server-start! server 0 \#(address) \#(workers)))))
      'started
      """#)
    guard result == self.value("started") else {
      throw TestError(description: "cannot define server: \(result)")
    }
    return try self.portOfRunningServer()
  }

  private func portOfRunningServer() throws -> Int {
    guard self.wait(until: "(http-server-running? server)") else {
      let outcome = self.scheme("(guard (e (#t (if (error-object? e) (error-object-message e) e))) " +
                              "(thread-join! starter 1.0) 'finished)")
      throw TestError(description: "server did not start: starter = \(outcome)")
    }
    guard case .fixnum(let port) = self.scheme("(http-server-port server)") else {
      throw TestError(description: "server has no port: \(self.scheme("(http-server-port server)"))")
    }
    return Int(port)
  }

  private func stopServer() {
    _ = self.scheme("(http-server-stop! server)")
    _ = self.scheme("(wait-threads-terminated 5)")
  }

  // MARK: - Tests

  func testBasicRequests() {
    self.inEvaluator {
      let port = try self.startServer(#"""
        (http-server-register! server "GET" "/hello/:name"
          (lambda (r)
            (make-srv-response 200 '(("X-Custom" . "yes"))
              (string-append "Hello " (srv-request-path-param r "name") "!"))))
        (http-server-register! server "GET" "/address"
          (lambda (r) (make-srv-response 200 #f (or (srv-request-address r) "unknown"))))
        (http-server-register! server "GET" "/error" (lambda (r) (error "boom")))
        (http-server-register! server "GET" "/noresponse" (lambda (r) 42))
        (http-server-register-default! server
          (lambda (r) (srv-response-not-found "custom not found")))
        """#)
      defer {
        self.stopServer()
      }
      // A regular request
      let hello = try XCTUnwrap(Client.fetch(port: port, path: "/hello/world"))
      XCTAssertEqual(hello.status, 200)
      XCTAssertEqual(hello.text, "Hello world!")
      XCTAssertEqual(hello.header("X-Custom"), "yes")
      XCTAssertTrue(hello.header("Content-Type")?.hasPrefix("text/plain") ?? false)
      XCTAssertNotNil(hello.header("Date"))
      // The address of the client
      XCTAssertEqual(try XCTUnwrap(Client.fetch(port: port, path: "/address")).text, "127.0.0.1")
      // HEAD is answered by the handler for GET, without a body
      let head = try XCTUnwrap(Client.fetch(port: port, method: "HEAD", path: "/hello/world"))
      XCTAssertEqual(head.status, 200)
      XCTAssertEqual(head.body.count, 0)
      XCTAssertEqual(head.header("Content-Length"), "12")
      // OPTIONS and unsupported methods are answered by the server
      let options = try XCTUnwrap(Client.fetch(port: port, method: "OPTIONS", path: "/hello/world"))
      XCTAssertEqual(options.status, 204)
      XCTAssertTrue(options.header("Allow")?.contains("GET") ?? false)
      let post = try XCTUnwrap(Client.fetch(port: port, method: "POST", path: "/hello/world"))
      XCTAssertEqual(post.status, 405)
      XCTAssertTrue(post.header("Allow")?.contains("GET") ?? false)
      // Unknown routes are handled by the default handler
      let missing = try XCTUnwrap(Client.fetch(port: port, path: "/nowhere"))
      XCTAssertEqual(missing.status, 404)
      XCTAssertEqual(missing.text, "custom not found")
      // Errors in handlers are turned into responses
      let error = try XCTUnwrap(Client.fetch(port: port, path: "/error"))
      XCTAssertEqual(error.status, 500)
      XCTAssertTrue(error.text.contains("boom"))
      let noResponse = try XCTUnwrap(Client.fetch(port: port, path: "/noresponse"))
      XCTAssertEqual(noResponse.status, 500)
      XCTAssertTrue(noResponse.text.contains("did not return a response"))
      // Accessing the server
      XCTAssertEqual(self.scheme("(list (http-server-running? server) (http-server-ipv4 server) " +
                               "(http-server-timeout server) (http-server-num-workers server))"),
                     self.value("(#t #t 2.0 2)"))
      XCTAssertEqual(self.scheme("(begin (http-server-timeout-set! server #f) " +
                               "(http-server-timeout server))"), .false)
      XCTAssertEqual(self.scheme("(begin (http-server-timeout-set! server 0.5) " +
                               "(http-server-timeout server))"), self.value("0.5"))
    }
  }

  func testPathAndQueryOfRequests() {
    self.inEvaluator {
      let port = try self.startServer(#"""
        (define (show x) (object->string x))
        (http-server-register! server "GET" "/echo/:name"
          (lambda (r)
            (make-srv-response 200 #f
              (string-append
                "path=" (srv-request-path r) "\n"
                "decoded=" (srv-request-path r #t) "\n"
                "query=" (srv-request-query r) "\n"
                "path+query=" (srv-request-query r #t) "\n"
                "x=" (show (srv-request-query-param r "x")) "\n"
                "x decoded=" (show (srv-request-query-param r "x" #t)) "\n"
                "y=" (show (srv-request-query-param r "y")) "\n"
                "y decoded=" (show (srv-request-query-param r "y" #t)) "\n"
                "z=" (show (assoc "z" (srv-request-query-params r))) "\n"
                "w decoded=" (show (assoc "w" (srv-request-query-params r #t))) "\n"
                "count=" (number->string (length (srv-request-query-params r))) "\n"))))
        """#)
      defer {
        self.stopServer()
      }
      // Encoded characters, repeated parameters, a plus (%2B) versus a space (+)
      let first = try XCTUnwrap(Client.fetch(
        port: port, path: "/echo/caf%C3%A9?x=a%2Bb&x=a+b&y=%c3%a9&z&w="))
      XCTAssertEqual(first.status, 200)
      XCTAssertEqual(first.text, """
        path=/echo/caf%C3%A9
        decoded=/echo/café
        query=?x=a%2Bb&x=a+b&y=%c3%a9&z&w=
        path+query=/echo/caf%C3%A9?x=a%2Bb&x=a+b&y=%c3%a9&z&w=
        x=(a%2Bb a+b)
        x decoded=(a+b a b)
        y=(%c3%a9)
        y decoded=(é)
        z=(z . )
        w decoded=(w . )
        count=5

        """)
      // An encoded slash is not a separator, and there is no query
      let second = try XCTUnwrap(Client.fetch(port: port, path: "/echo/a%2Fb"))
      XCTAssertEqual(second.text, """
        path=/echo/a%2Fb
        decoded=/echo/a/b
        query=
        path+query=/echo/a%2Fb
        x=()
        x decoded=()
        y=()
        y decoded=()
        z=#f
        w decoded=#f
        count=0

        """)
      // Values are decoded once
      let third = try XCTUnwrap(Client.fetch(port: port, path: "/echo/%252e%252e?x=%252e"))
      XCTAssertTrue(third.text.contains("path=/echo/%252e%252e\n"))
      XCTAssertTrue(third.text.contains("decoded=/echo/%2e%2e\n"))
      XCTAssertTrue(third.text.contains("query=?x=%252e\n"))
      XCTAssertTrue(third.text.contains("x=(%252e)\n"))
      XCTAssertTrue(third.text.contains("x decoded=(%2e)\n"))
    }
  }

  func testPathParameters() {
    self.inEvaluator {
      let port = try self.startServer(#"""
        (define (show x) (object->string x))
        (define (describe r key)
          (make-srv-response 200 #f
            (string-append "default=" (show (srv-request-path-param r key))
                           " raw=" (show (srv-request-path-param r key #f #f))
                           " decoded=" (show (srv-request-path-param r key #f #t))
                           " all=" (show (srv-request-path-params r))
                           " all-decoded=" (show (srv-request-path-params r #t)))))
        (http-server-register! server "GET" "/p/:name" (lambda (r) (describe r "name")))
        (http-server-register! server "GET" "/q/::path" (lambda (r) (describe r "path")))
        (http-server-register! server "GET" "/f/::path/edit" (lambda (r) (describe r "path")))
        (http-server-register! server "GET" "/set/:name"
          (lambda (r)
            (srv-request-path-param-set! r "name" "x%2Fy%20z")
            (srv-request-path-param-set! r "other" "new")
            (let ((result (string-append
                            "name=" (show (srv-request-path-param r "name"))
                            " decoded=" (show (srv-request-path-param r "name" #f #t))
                            " other=" (show (srv-request-path-param r "other" #f #t)))))
              (srv-request-path-param-remove! r "name")
              (make-srv-response 200 #f
                (string-append result
                               " removed=" (show (srv-request-path-param r "name"))
                               "/" (show (srv-request-path-param r "name" 'none #t)))))))
        (http-server-register! server "GET" "/badset/:name"
          (lambda (r)
            (srv-request-path-param-set! r "name" "100%")
            (make-srv-response 200 #f "unreachable")))
        """#)
      defer {
        self.stopServer()
      }
      func check(_ path: String, _ expected: String, line: UInt = #line) throws {
        let response = try XCTUnwrap(Client.fetch(port: port, path: path), line: line)
        XCTAssertEqual(response.status, 200, path, line: line)
        XCTAssertEqual(response.text, expected, path, line: line)
      }
      // Variables of a single path segment
      try check("/p/caf%C3%A9",
                "default=caf%C3%A9 raw=caf%C3%A9 decoded=café " +
                "all=((name . caf%C3%A9)) all-decoded=((name . café))")
      try check("/p/a%2Fb",
                "default=a%2Fb raw=a%2Fb decoded=a/b all=((name . a%2Fb)) all-decoded=((name . a/b))")
      try check("/p/a%20b%2Bc?x=1",
                "default=a%20b%2Bc raw=a%20b%2Bc decoded=a b+c " +
                "all=((name . a%20b%2Bc)) all-decoded=((name . a b+c))")
      try check("/p/%252e%252e",
                "default=%252e%252e raw=%252e%252e decoded=%2e%2e " +
                "all=((name . %252e%252e)) all-decoded=((name . %2e%2e))")
      // Path variables: an encoded slash can still be told from a path separator
      try check("/q/caf%C3%A9/x%20y",
                "default=caf%C3%A9/x%20y raw=caf%C3%A9/x%20y decoded=café/x y " +
                "all=((path . caf%C3%A9/x%20y)) all-decoded=((path . café/x y))")
      try check("/q/a%2Fb/c",
                "default=a%2Fb/c raw=a%2Fb/c decoded=a/b/c " +
                "all=((path . a%2Fb/c)) all-decoded=((path . a/b/c))")
      try check("/q/%252e%252e/z",
                "default=%252e%252e/z raw=%252e%252e/z decoded=%2e%2e/z " +
                "all=((path . %252e%252e/z)) all-decoded=((path . %2e%2e/z))")
      try check("/q", "default= raw= decoded= all=((path . )) all-decoded=((path . ))")
      // A path variable followed by more segments
      try check("/f/a%2Fb/c/edit",
                "default=a%2Fb/c raw=a%2Fb/c decoded=a/b/c " +
                "all=((path . a%2Fb/c)) all-decoded=((path . a/b/c))")
      // Setting and removing path parameters (values are set in encoded form)
      try check("/set/orig", "name=x%2Fy%20z decoded=x/y z other=new removed=#f/none")
      let bad = try XCTUnwrap(Client.fetch(port: port, path: "/badset/orig"), "no response to /badset: \(Client.lastFailure)")
      XCTAssertEqual(bad.status, 500)
      XCTAssertTrue(bad.text.contains("not a valid percent-encoded string"))
    }
  }

  func testFormBodies() {
    self.inEvaluator {
      let port = try self.startServer(#"""
        (define (show x) (object->string x))
        (http-server-register! server "POST" "/form"
          (lambda (r)
            (let ((attrs (srv-request-form-attributes r)))
              (make-srv-response 200 #f
                (string-append "a=" (show (cdr (assoc "a" attrs)))
                               " b=" (show (cdr (assoc "b" attrs)))
                               " c=" (show (cdr (assoc "c" attrs)))
                               " f=" (show (cdr (assoc "f" attrs)))
                               " count=" (number->string (length attrs)))))))
        (http-server-register! server "POST" "/multipart"
          (lambda (r)
            (let ((parts (srv-request-form-multiparts r)))
              (make-srv-response 200 #f
                (apply string-append
                       "parts=" (number->string (length parts))
                       (map (lambda (p) (string-append " [" (srv-multipart-body->string p) "]"))
                            parts))))))
        """#)
      defer {
        self.stopServer()
      }
      // URL-encoded forms: "+" is a space, "%2B" a plus, and a value can contain "="
      let form = try XCTUnwrap(Client.fetch(
        port: port, method: "POST", path: "/form",
        headers: ["Content-Type": "application/x-www-form-urlencoded"],
        body: Array("a=x+y&b=%C3%BC%2B&c=d=e&f".utf8)))
      XCTAssertEqual(form.status, 200)
      XCTAssertEqual(form.text, "a=x y b=ü+ c=d=e f= count=4")
      // Multipart forms
      let boundary = ["Content-Type": "multipart/form-data; boundary=XX"]
      let part = { (name: String, value: String) in
        "--XX\r\nContent-Disposition: form-data; name=\"\(name)\"\r\n\r\n\(value)\r\n"
      }
      let wellFormed = try XCTUnwrap(Client.fetch(
        port: port, method: "POST", path: "/multipart", headers: boundary,
        body: Array((part("a", "value1") + part("b", "value2") + "--XX--\r\n").utf8)))
      XCTAssertEqual(wellFormed.text, "parts=2 [value1] [value2]")
      // A body without closing boundary does not have any parts
      let malformed = try XCTUnwrap(Client.fetch(
        port: port, method: "POST", path: "/multipart", headers: boundary,
        body: Array(part("a", "value1").utf8)))
      XCTAssertEqual(malformed.text, "parts=0")
    }
  }

  func testJSONResponses() {
    self.inEvaluator {
      let port = try self.startServer(#"""
        (define sample
          '(("name" . "Zoë \"quoted\"") ("n" . 42) ("pi" . 3.5) ("ok" . #t) ("nothing" . null)
            ("list" . #(1 2 3)) ("nested" . (("a" . 1) ("b" . #(#t #f))))))
        (define (respond body) (lambda (r) (make-srv-response 200 #f body)))
        (http-server-register! server "GET" "/json/object" (respond (json sample)))
        (http-server-register! server "GET" "/json/mutable" (respond (mutable-json sample)))
        (http-server-register! server "GET" "/json/vector" (respond (vector 1 2.5 "three" #t)))
        (http-server-register! server "GET" "/json/number" (respond 42))
        (http-server-register! server "GET" "/json/string"
          (lambda (r) (make-srv-response 201 '(("X-Test" . "1")) (json "just a string"))))
        """#)
      defer {
        self.stopServer()
      }
      func fetchJSON(_ path: String, status: Int = 200, line: UInt = #line) throws -> Any {
        let response = try XCTUnwrap(Client.fetch(port: port, path: path), line: line)
        XCTAssertEqual(response.status, status, path, line: line)
        XCTAssertEqual(response.header("Content-Type"), "application/json", path, line: line)
        return try JSONSerialization.jsonObject(with: Data(response.body),
                                                options: [.fragmentsAllowed])
      }
      let expected: NSDictionary = [
        "name": "Zoë \"quoted\"", "n": 42, "pi": 3.5, "ok": true, "nothing": NSNull(),
        "list": [1, 2, 3], "nested": ["a": 1, "b": [true, false]]
      ]
      XCTAssertEqual(try fetchJSON("/json/object") as? NSDictionary, expected)
      XCTAssertEqual(try fetchJSON("/json/mutable") as? NSDictionary, expected)
      XCTAssertEqual(try fetchJSON("/json/vector") as? NSArray, [1, 2.5, "three", true])
      XCTAssertEqual(try fetchJSON("/json/number") as? NSNumber, 42)
      XCTAssertEqual(try fetchJSON("/json/string", status: 201) as? String, "just a string")
      XCTAssertEqual(try XCTUnwrap(Client.fetch(port: port, path: "/json/string")).header("X-Test"),
                     "1")
    }
  }

  func testHTMLResponses() {
    self.inEvaluator {
      let port = try self.startServer(#"""
        (import (lispkit markdown))
        (define (respond body) (lambda (r) (make-srv-response 200 #f body)))
        (http-server-register! server "GET" "/html/document"
          (respond '(html (head (title "Title")) (body (p "text")))))
        (http-server-register! server "GET" "/html/fragment" (respond '(p "text " (b "bold"))))
        (http-server-register! server "GET" "/html/nodes" (respond '((h3 "one") (h3 "two"))))
        (http-server-register! server "GET" "/html/forced"
          (lambda (r)
            (let ((response (make-srv-response 200)))
              (srv-response-body-set! response '(html (body (p "text"))) #t)
              response)))
        (http-server-register! server "GET" "/html/markdown" (respond (markdown "*text*")))
        """#)
      defer {
        self.stopServer()
      }
      func fetchHTML(_ path: String, line: UInt = #line) throws -> String {
        let response = try XCTUnwrap(Client.fetch(port: port, path: path), line: line)
        XCTAssertEqual(response.status, 200, path, line: line)
        XCTAssertTrue(response.header("Content-Type")?.hasPrefix("text/html") ?? false, path,
                      line: line)
        return response.text
      }
      func countOfHTMLElements(in html: String) -> Int {
        return html.components(separatedBy: "<html").count - 1
      }
      // A complete document is sent as it is
      let document = try fetchHTML("/html/document")
      XCTAssertEqual(document, "<html><head><title>Title</title></head><body><p>text</p></body></html>")
      // Everything else, i.e. fragments and markdown, is wrapped into a document...
      let fragment = try fetchHTML("/html/fragment")
      XCTAssertTrue(fragment.hasPrefix("<html>"), fragment)
      XCTAssertTrue(fragment.contains("<body><p>text <b>bold</b></p></body>"), fragment)
      XCTAssertEqual(countOfHTMLElements(in: fragment), 1, fragment)
      let nodes = try fetchHTML("/html/nodes")
      XCTAssertTrue(nodes.contains("<body><h3>one</h3><h3>two</h3></body>"), nodes)
      XCTAssertEqual(countOfHTMLElements(in: nodes), 1, nodes)
      let markdown = try fetchHTML("/html/markdown")
      XCTAssertTrue(markdown.contains("<body><p><em>text</em></p>"), markdown)
      XCTAssertEqual(countOfHTMLElements(in: markdown), 1, markdown)
      // ...as is a document if the last argument of `srv-response-body-set!` is `#t`
      let forced = try fetchHTML("/html/forced")
      XCTAssertTrue(forced.contains("<body><html><body><p>text</p></body></html></body>"), forced)
    }
  }

  func testSharingFilesAndDirectories() {
    self.inEvaluator {
      let fileManager = FileManager.default
      let root = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)
        .appendingPathComponent("lispkit-http-tests-\(UUID().uuidString)", isDirectory: true)
        .resolvingSymlinksInPath()
      let www = root.appendingPathComponent("www", isDirectory: true)
      defer {
        try? fileManager.removeItem(at: root)
      }
      try fileManager.createDirectory(at: www.appendingPathComponent("sub", isDirectory: true),
                                      withIntermediateDirectories: true)
      try "hello a\n".write(to: www.appendingPathComponent("a.txt"), atomically: true, encoding: .utf8)
      try "cafe\n".write(to: www.appendingPathComponent("caf é.txt"), atomically: true, encoding: .utf8)
      try "<p>sub index</p>\n".write(to: www.appendingPathComponent("sub/index.html"),
                                     atomically: true, encoding: .utf8)
      try "b file\n".write(to: www.appendingPathComponent("sub/b.txt"), atomically: true, encoding: .utf8)
      try "SECRET\n".write(to: root.appendingPathComponent("secret.txt"), atomically: true, encoding: .utf8)
      try fileManager.createSymbolicLink(atPath: www.appendingPathComponent("link-out").path,
                                         withDestinationPath: "../secret.txt")
      try fileManager.createSymbolicLink(atPath: www.appendingPathComponent("link-in").path,
                                         withDestinationPath: "a.txt")
      let port = try self.startServer(#"""
        (http-server-register! server "/public/::path" (share-directory-handler "\#(www.path)"))
        (http-server-register! server "/files/::path" (browse-directory-handler "\#(www.path)"))
        (http-server-register! server "/one" (share-file-handler "\#(www.path)/a.txt"))
        """#)
      defer {
        self.stopServer()
      }
      func status(_ path: String, line: UInt = #line) throws -> Int {
        let response = try XCTUnwrap(Client.fetch(port: port, path: path), line: line)
        XCTAssertFalse(response.text.contains("SECRET"), "\(path) leaks the secret", line: line)
        return response.status
      }
      // Files below the directory
      let file = try XCTUnwrap(Client.fetch(port: port, path: "/public/a.txt"))
      XCTAssertEqual(file.status, 200)
      XCTAssertEqual(file.text, "hello a\n")
      XCTAssertNotNil(file.header("ETag"))
      XCTAssertEqual(try XCTUnwrap(Client.fetch(port: port, path: "/public/caf%20%C3%A9.txt")).text,
                     "cafe\n")
      XCTAssertEqual(try XCTUnwrap(Client.fetch(port: port, path: "/public/link-in")).text,
                     "hello a\n")
      XCTAssertEqual(try XCTUnwrap(Client.fetch(port: port, path: "/one")).text, "hello a\n")
      // Range requests and conditional requests
      let range = try XCTUnwrap(Client.fetch(port: port, path: "/public/a.txt",
                                             headers: ["Range": "bytes=0-4"]))
      XCTAssertEqual(range.status, 206)
      XCTAssertEqual(range.text, "hello")
      let etag = try XCTUnwrap(file.header("ETag"))
      XCTAssertEqual(try XCTUnwrap(Client.fetch(port: port, path: "/public/a.txt",
                                                headers: ["If-None-Match": etag])).status, 304)
      // Directories
      let redirect = try XCTUnwrap(Client.fetch(port: port, path: "/public/sub"))
      XCTAssertEqual(redirect.status, 301)
      XCTAssertEqual(redirect.header("Location"), "/public/sub/")
      XCTAssertEqual(try XCTUnwrap(Client.fetch(port: port, path: "/public/sub/")).text,
                     "<p>sub index</p>\n")
      XCTAssertEqual(try status("/public/nope.txt"), 404)
      // Requests leaving the directory are refused (also in encoded form)
      for path in ["/public/%2e%2e/secret.txt", "/public/%2E%2E/secret.txt",
                   "/public/../secret.txt", "/public/sub/../../secret.txt",
                   "/public/..%2fsecret.txt", "/public/%2e%2e%2fsecret.txt",
                   "/public/sub/%2e%2e/%2e%2e/secret.txt", "/public/a.txt%00.png",
                   "/public/link-out", "/files/%2e%2e/secret.txt", "/files/%2e%2e/",
                   "/files/link-out"] {
        XCTAssertEqual(try status(path), 403, path)
      }
      // A double-encoded path is decoded only once, so it names a file that does not exist
      XCTAssertEqual(try status("/public/%252e%252e/secret.txt"), 404)
      // Listing a directory
      let listing = try XCTUnwrap(Client.fetch(port: port, path: "/files/"))
      XCTAssertEqual(listing.status, 200)
      XCTAssertTrue(listing.text.contains("href=\"/files/a.txt\""), listing.text)
      // The file system may store the name decomposed (e + combining accent)
      XCTAssertTrue(listing.text.contains("href=\"/files/caf%20%C3%A9.txt\"") ||
                    listing.text.contains("href=\"/files/caf%20e%CC%81.txt\""), listing.text)
      XCTAssertEqual(try XCTUnwrap(Client.fetch(port: port, path: "/files/sub/b.txt")).text, "b file\n")
    }
  }

  func testPersistentConnections() {
    self.inEvaluator {
      let port = try self.startServer(#"""
        (http-server-register! server "GET" "/ping" (lambda (r) (make-srv-response 200 #f "pong")))
        """#, workers: 2)
      defer {
        self.stopServer()
      }
      // Several requests via the same connection
      let client = try XCTUnwrap(Client(port: port))
      for _ in 0..<3 {
        XCTAssertTrue(client.send(path: "/ping"))
        let response = try XCTUnwrap(client.readResponse())
        XCTAssertEqual(response.status, 200)
        XCTAssertEqual(response.text, "pong")
      }
      // Idle connections do not occupy worker threads (there are more than there are workers)
      let idle = (0..<8).compactMap { _ in Client(port: port) }
      XCTAssertEqual(idle.count, 8)
      XCTAssertTrue(client.send(path: "/ping"))
      XCTAssertEqual(try XCTUnwrap(client.readResponse()).text, "pong")
      XCTAssertEqual(try XCTUnwrap(Client.fetch(port: port, path: "/ping")).text, "pong")
      // The server closes a connection if the client asks for it
      let closing = try XCTUnwrap(Client(port: port))
      XCTAssertTrue(closing.send(path: "/ping", close: true))
      XCTAssertEqual(try XCTUnwrap(closing.readResponse()).header("Connection"), "close")
      XCTAssertTrue(closing.closedByServer())
    }
  }

  func testOverloadedServerRefusesRequests() {
    self.inEvaluator {
      // One worker, a queue for two requests, and no waiting for room in the queue; the handler
      // for "/slow" blocks until a token is available in `gate`
      let port = try self.startServer(#"""
        (define gate (make-shared-queue))
        (define entered (make-atomic-box 0))
        (http-server-timeout-set! server #f)
        (http-server-register! server "GET" "/slow"
          (lambda (r)
            (atomic-box-inc+mul! entered 1)
            (shared-queue-dequeue/wait! gate)
            (make-srv-response 200 #f "done")))
        (http-server-register! server "GET" "/fast" (lambda (r) (make-srv-response 200 #f "fast")))
        """#, workers: 1, queueSize: 2)
      defer {
        _ = self.scheme("(shared-queue-close! gate)")
        self.stopServer()
      }
      // The worker takes the first request, and the next two requests fill the queue
      let first = try XCTUnwrap(Client(port: port))
      XCTAssertTrue(first.send(path: "/slow"))
      XCTAssertTrue(self.wait(until: "(= (atomic-box-ref entered) 1)"), "worker did not start")
      let second = try XCTUnwrap(Client(port: port))
      XCTAssertTrue(second.send(path: "/slow"))
      let third = try XCTUnwrap(Client(port: port))
      XCTAssertTrue(third.send(path: "/slow"))
      XCTAssertTrue(self.wait(until: "(if (string-contains (object->string server) \"requests=2\") " +
                                     "#t #f)"),
                    "requests were not queued")
      // Any further request is refused right away
      let refused = try XCTUnwrap(Client.fetch(port: port, path: "/fast"))
      XCTAssertEqual(refused.status, 503)
      XCTAssertEqual(refused.header("Retry-After"), "1")
      // The requests that were accepted are answered
      XCTAssertEqual(self.scheme("(begin (shared-queue-enqueue! gate 1) (shared-queue-enqueue! gate 2) " +
                               "(shared-queue-enqueue! gate 3) 'released)"), self.value("released"))
      for client in [first, second, third] {
        let response = try XCTUnwrap(client.readResponse())
        XCTAssertEqual(response.status, 200)
        XCTAssertEqual(response.text, "done")
      }
      XCTAssertEqual(try XCTUnwrap(Client.fetch(port: port, path: "/fast")).text, "fast")
    }
  }

  func testStoppingAndRestartingServers() {
    self.inEvaluator {
      let port1 = try self.startServer(#"""
        (http-server-register! server "GET" "/ping" (lambda (r) (make-srv-response 200 #f "pong")))
        """#)
      defer {
        self.stopServer()
      }
      XCTAssertEqual(try XCTUnwrap(Client.fetch(port: port1, path: "/ping")).text, "pong")
      XCTAssertEqual(self.scheme("(http-server-num-workers server)"), .fixnum(2))
      // Stopping closes the connections and ends the worker threads
      let connection = try XCTUnwrap(Client(port: port1))
      XCTAssertTrue(connection.send(path: "/ping"))
      XCTAssertEqual(try XCTUnwrap(connection.readResponse()).text, "pong")
      self.stopServer()
      XCTAssertTrue(self.wait(until: "(not (http-server-running? server))"))
      XCTAssertTrue(self.wait(until: "(= (http-server-num-workers server) 0)"))
      XCTAssertEqual(self.scheme("(http-server-port server)"), .false)
      XCTAssertTrue(connection.closedByServer())
      // The same server can be started again
      XCTAssertEqual(self.scheme("(begin (define starter (spawn (thunk " +
                               "(http-server-start! server 0 #f 3)))) 'started)"),
                     self.value("started"))
      let port2 = try self.portOfRunningServer()
      XCTAssertEqual(try XCTUnwrap(Client.fetch(port: port2, path: "/ping")).text, "pong")
      XCTAssertEqual(self.scheme("(http-server-num-workers server)"), .fixnum(3))
    }
  }

  func testListenAddresses() {
    self.inEvaluator {
      // The loopback address is the default; the other addresses include the loopback address
      for (address, ipv4) in [("", true), ("#f", true), ("\"127.0.0.1\"", true),
                              ("\"0.0.0.0\"", true), ("#t", false)] {
        let port = try self.startServer(#"""
          (http-server-register! server "GET" "/ping" (lambda (r) (make-srv-response 200 #f "pong")))
          """#, address: address.isEmpty ? "#f" : address)
        defer {
          self.stopServer()
        }
        XCTAssertEqual(try XCTUnwrap(Client.fetch(port: port, path: "/ping"),
                                     "address \(address)").text, "pong", "address \(address)")
        XCTAssertEqual(self.scheme("(http-server-ipv4 server)"), .makeBoolean(ipv4), "address \(address)")
      }
    }
  }

  func testFailingToStartServers() {
    self.inEvaluator {
      let port = try self.startServer()
      defer {
        self.stopServer()
      }
      func errorMessage(of code: String) -> String? {
        if case .error(let error) = self.scheme(code) {
          return error.message
        }
        return nil
      }
      // The port is in use: the error says why, and no worker threads are left
      let busy = errorMessage(of: "(begin (define busy (make-http-server 10 4)) " +
                                  "(http-server-start! busy \(port)))")
      XCTAssertTrue(busy?.contains("Address already in use") ?? false, busy ?? "no error")
      XCTAssertTrue(self.wait(until: "(= (http-server-num-workers busy) 0)"))
      XCTAssertEqual(self.scheme("(http-server-running? busy)"), .false)
      // The address is not an address
      let invalid = errorMessage(of: "(http-server-start! busy 0 \"not-an-ip\")")
      XCTAssertTrue(invalid?.contains("invalid listen address") ?? false, invalid ?? "no error")
      XCTAssertTrue(self.wait(until: "(= (http-server-num-workers busy) 0)"))
      // The address has the wrong type
      let wrongType = errorMessage(of: "(http-server-start! busy 0 42)")
      XCTAssertTrue(wrongType?.contains("42") ?? false, wrongType ?? "no error")
      XCTAssertTrue(self.wait(until: "(= (http-server-num-workers busy) 0)"))
    }
  }
}
