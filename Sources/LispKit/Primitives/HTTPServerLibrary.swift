//
//  HTTPServer.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/07/2024.
//  Copyright © 2024 ObjectHub. All rights reserved.
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

import Foundation
import NanoHTTP
import DynamicJSON

public final class HTTPServerLibrary: NativeLibrary {
  
  // Factory for types used in this library
  public static var libraryConfig: HTTPServerConfig = LispKitHTTPServerConfig()
  
  // Flow identifiers
  // private let codeGrant: Symbol
  
  /// Initialize symbols.
  public required init(in context: Context) throws {
    // self.codeGrant = context.symbols.intern("code-grant")
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "http", "server"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"], "define", "lambda", "case-lambda", "or", "quote",
                                             "not", "eq?")
    self.`import`(from: ["lispkit", "control"], "let-optionals", "let", "cond", "do", "if")
    self.`import`(from: ["lispkit", "dynamic"], "try", "error-object->string")
    self.`import`(from: ["lispkit", "thread"], "current-thread", "make-thread", "thread-yield!",
                                               "thread-start!")
    self.`import`(from: ["lispkit", "thread", "shared-queue"], "shared-queue-dequeue/wait!")
    self.`import`(from: ["lispkit", "string"], "string-append")
    self.`import`(from: ["lispkit", "math"], "+", "-", ">=", "number->string")
    self.`import`(from: ["lispkit", "list"], "pair?", "cons", "car", "cdr", "fold-left")
    self.`import`(from: ["lispkit", "sxml"], "sxml->html")
    self.`import`(from: ["lispkit", "markdown"], "markdown?", "markdown->html",
                                                 "markdown-block?", "markdown-blocks?", "blocks->html",
                                                 "markdown-inline?", "markdown-text?", "text->html")
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define("http-server-type-tag", as: HTTPServer.type.objectTypeTag())
    self.define("srv-request-type-tag", as: HTTPRequest.type.objectTypeTag())
    self.define("srv-multipart-type-tag", as: HTTPMultiPart.type.objectTypeTag())
    self.define("srv-response-type-tag", as: HTTPResponse.type.objectTypeTag())
    self.define(Procedure("http-server?", self.isHttpServer))
    self.define(Procedure("make-http-server", self.makeHttpServer))
    self.define(Procedure("http-server-running?", self.isHttpServerRunning))
    self.define(Procedure("http-server-port", self.httpServerPort))
    self.define(Procedure("http-server-open-connections", self.httpServerOpenConnections))
    self.define(Procedure("http-server-routes", self.httpServerRoutes))
    self.define(Procedure("http-server-handlers", self.httpServerHandlers))
    self.define(Procedure("http-server-log-severity", self.httpServerLogSeverity))
    self.define(Procedure("http-server-log-severity-set!", self.httpServerLogSeveritySet))
    self.define(Procedure("http-server-timeout", self.httpServerTimeout))
    self.define(Procedure("http-server-timeout-set!", self.httpServerTimeoutSet))
    self.define(Procedure("http-server-num-workers", self.httpServerNumWorkers))
    self.define(Procedure("http-server-log", self.httpServerLog))
    self.define(Procedure("http-server-register!", self.httpServerRegister))
    self.define(Procedure("http-server-register-default!", self.httpServerRegisterDefault))
    self.define(Procedure("_http-server-start!", self.httpServerStart), export: false)
    self.define(Procedure("http-server-stop!", self.httpServerStop))
    self.define(Procedure("_http-server-reset!", self.httpServerReset), export: false)
    self.define(Procedure("_http-server-attach-worker!", self.httpServerAttachWorker), export: false)
    self.define(Procedure("_http-server-remove-worker!", self.httpServerRemoveWorker), export: false)
    self.define(Procedure("http-server-register-middleware!", self.httpServerRegisterMiddleware))
    self.define(Procedure("_http-server-middleware", self.httpServerMiddleware), export: false)
    self.define(Procedure("srv-request?", self.isSrvRequest))
    self.define(Procedure("srv-request-server", self.serverRequestServer))
    self.define(Procedure("srv-request-method", self.serverRequestMethod))
    self.define(Procedure("srv-request-path", self.serverRequestPath))
    self.define(Procedure("srv-request-query", self.serverRequestQuery))
    self.define(Procedure("srv-request-query-param", self.serverRequestQueryParam))
    self.define(Procedure("srv-request-query-params", self.serverRequestQueryParams))
    self.define(Procedure("srv-request-path-param", self.serverRequestPathParam))
    self.define(Procedure("srv-request-path-param-set!", self.serverRequestPathParamSet))
    self.define(Procedure("srv-request-path-param-remove!", self.serverRequestPathParamRemove))
    self.define(Procedure("srv-request-path-params", self.serverRequestPathParams))
    self.define(Procedure("srv-request-header", self.serverRequestHeader))
    self.define(Procedure("srv-request-header-set!", self.serverRequestHeaderSet))
    self.define(Procedure("srv-request-header-remove!", self.serverRequestHeaderRemove))
    self.define(Procedure("srv-request-headers", self.serverRequestHeaders))
    self.define(Procedure("srv-request-body", self.serverRequestBody))
    self.define(Procedure("srv-request-body->string", self.serverRequestBodyToString))
    self.define(Procedure("srv-request-form-attributes", self.serverRequestFormAttributes))
    self.define(Procedure("srv-request-form-multiparts", self.serverRequestFormMultiparts))
    self.define(Procedure("srv-request-address", self.serverRequestAddress))
    self.define(Procedure("srv-request-address", self.serverRequestAddress))
    self.define(Procedure("_srv-request-send-response", self.serverRequestSendResponse), export: false)
    self.define(Procedure("srv-multipart?", self.isServerMultipart))
    self.define(Procedure("srv-multipart-headers", self.serverMultipartHeaders))
    self.define(Procedure("srv-multipart-header", self.serverMultipartHeader))
    self.define(Procedure("srv-multipart-body", self.serverMultipartBody))
    self.define(Procedure("srv-response?", self.isSrvResponse))
    self.define(Procedure("_make-srv-response", self.makeSrvResponse), export: false)
    self.define(Procedure("srv-response-created", self.srvResponseCreated))
    self.define(Procedure("srv-response-accepted", self.srvResponseAccepted))
    self.define(Procedure("srv-response-moved-permanently", self.srvResponseMovedPermanently))
    self.define(Procedure("srv-response-moved-temporarily", self.srvResponseMovedTemporarily))
    self.define(Procedure("srv-response-status-code", self.srvResponseStatusCode))
    self.define(Procedure("srv-response-status-code-set!", self.srvResponseStatusCodeSet))
    self.define(Procedure("srv-response-headers", self.srvResponseHeaders))
    self.define(Procedure("srv-response-header", self.srvResponseHeader))
    self.define(Procedure("srv-response-header-set!", self.srvResponseHeaderSet))
    self.define(Procedure("srv-response-header-remove!", self.srvResponseHeaderRemove))
    self.define(Procedure("_srv-response-body-set!", self.srvResponseBodySet), export: false)
    self.define(Procedure("srv-response-body-html-set!", self.srvResponseBodyHtmlSet))
    self.define(Procedure("parse-http-header-value", self.parseHttpHeaderValue))
    self.define(Procedure("http-header-param", self.httpHeaderParam))
    self.define(Procedure("share-file-handler", self.shareFileHandler))
    self.define(Procedure("share-directory-handler", self.shareDirectoryHandler))
    self.define(Procedure("browse-directory-handler", self.browseDirectoryHandler))
    self.define("srv-response-body-set!", via:
      "(define (srv-response-body-set! resp body . args)",
      "  (let-optionals args ((last #f))",
      "    (cond ((markdown? body)",
      "            (srv-response-body-html-set! resp (markdown->html body) #t))",
      "          ((or (markdown-block? body) (markdown-blocks? body))",
      "            (srv-response-body-html-set! resp (blocks->html body) #t))",
      "          ((or (markdown-inline? body) (markdown-text? body))",
      "            (srv-response-body-html-set! resp (text->html body) #t))",
      "          ((pair? body)",
      "            (srv-response-body-html-set! resp (sxml->html body) (or last (eq? (car body) 'html))))",
      "          (else",
      "            (_srv-response-body-set! resp body last)))))")
    self.define("make-srv-response", via:
      "(define (make-srv-response . args)",
      "  (let-optionals args ((status 200)(headers #f)(body #f)(ct #f))",
      "    (let ((response (_make-srv-response status headers #f ct)))",
      "      (srv-response-body-set! response body ct)",
      "      response)))")
    self.define("srv-response-ok", via:
      "(define srv-response-ok (case-lambda",
      "    ((body) (make-srv-response 200 #f body))",
      "    ((headers body) (make-srv-response 200 headers body))))")
    self.define("srv-response-bad-request", via:
      "(define srv-response-bad-request (case-lambda",
      "    (() (make-srv-response 400 #f #f))",
      "    ((body) (make-srv-response 400 #f body))",
      "    ((headers body) (make-srv-response 400 headers body))))")
    self.define("srv-response-unauthorized", via:
      "(define srv-response-unauthorized (case-lambda",
      "    (() (make-srv-response 401 #f #f))",
      "    ((body) (make-srv-response 401 #f body))",
      "    ((headers body) (make-srv-response 401 headers body))))")
    self.define("srv-response-forbidden", via:
      "(define srv-response-forbidden (case-lambda",
      "    (() (make-srv-response 403 #f #f))",
      "    ((body) (make-srv-response 403 #f body))",
      "    ((headers body) (make-srv-response 403 headers body))))")
    self.define("srv-response-not-found", via:
      "(define srv-response-not-found (case-lambda",
      "    (() (make-srv-response 404 #f \"not found\"))",
      "    ((body) (make-srv-response 404 #f body))",
      "    ((headers body) (make-srv-response 404 headers body))))")
    self.define("srv-response-method-not-allowed", via:
      "(define srv-response-method-not-allowed (case-lambda",
      "    (() (make-srv-response 405 #f #f))",
      "    ((body) (make-srv-response 405 #f body))",
      "    ((headers body) (make-srv-response 405 headers body))))")
    self.define("srv-response-not-acceptable", via:
      "(define srv-response-not-acceptable (case-lambda",
      "    (() (make-srv-response 406 #f #f))",
      "    ((body) (make-srv-response 406 #f body))",
      "    ((headers body) (make-srv-response 406 headers body))))")
    self.define("srv-response-internal-server-error", via:
      "(define srv-response-internal-server-error (case-lambda",
      "    (() (make-srv-response 500 #f \"internal server error\"))",
      "    ((body) (make-srv-response 500 #f body))",
      "    ((headers body) (make-srv-response 500 headers body))))")
    self.define("srv-response-not-implemented", via:
      "(define srv-response-not-implemented (case-lambda",
      "    (() (make-srv-response 501 #f \"not implemented\"))",
      "    ((body) (make-srv-response 501 #f body))",
      "    ((headers body) (make-srv-response 501 headers body))))")
    self.define("_make-worker", via:
      "(define (_make-worker server queue name)",
      "  (lambda ()",
      "    (http-server-log server 0 \"worker\" \"started \" name \"/\" (http-server-num-workers server))",
      "    (_http-server-attach-worker! server (current-thread))",
      "    (do ((next (shared-queue-dequeue/wait! queue) (shared-queue-dequeue/wait! queue)))",
      "        ((not next))",
      "      (http-server-log server 0 \"worker\" name \" received request\")",
      "      (do ((con next",
      "             (_srv-request-send-response",
      "               (cdr con)",
      "               (try (lambda ()",
      "                      (or (fold-left (lambda (z x) (or z (x (cdr con)))) #f",
      "                            (_http-server-middleware server))",
      "                          ((car con) (cdr con))))",
      "                    (lambda (e)",
      "                      (http-server-log server 3 \"worker/err\" name \" handling: \"",
      "                        (srv-request-method (cdr con)) \" \" (srv-request-path (cdr con))",
      "                        \"\\n\" (error-object->string e 'printable))",
      "                      (make-srv-response 500 #f",
      "                        (string-append \"request handler: \" (error-object->string e 'printable)))))",
      "               #f)))",
      "          ((not con))",
      "        (http-server-log server 0 \"worker\" name \": \"",
      "          (srv-request-method (cdr con)) \" \" (srv-request-path (cdr con))))",
      "      (http-server-log server 0 \"worker\" name \" idle\"))",
      "    (_http-server-remove-worker! server (current-thread))",
      "    (http-server-log server 0 \"worker\" \"closed \" name \"/\" (http-server-num-workers server))))")
    self.define("http-server-start!", via:
      "(define http-server-start!",
      "  (case-lambda",
      "    ((server port)",
      "      (http-server-start! server port #f))",
      "    ((server port forceIPv4)",
      "      (http-server-start! server port forceIPv4 3))",
      "    ((server port forceIPv4 num)",
      "      (http-server-start! server port forceIPv4 num \"worker \"))",
      "    ((server port forceIPv4 num prefix)",
      "      (do ((queue (_http-server-reset! server))",
      "           (i 0 (+ i 1)))",
      "          ((>= i num))",
      "        (thread-start! (make-thread",
      "          (_make-worker server queue (string-append prefix (number->string i))))))",
      "      (_http-server-start! server port forceIPv4)",
      "      (thread-yield!))))")
  }
  
  /// Initializations of the library.
  public override func initializations() {
  }
  
  private func httpServer(from expr: Expr) throws -> HTTPServer {
    guard case .object(let obj) = expr, let server = obj as? HTTPServer else {
      throw RuntimeError.type(expr, expected: [HTTPServer.type])
    }
    return server
  }
  
  private func httpServerRequest(from expr: Expr) throws -> HTTPServerRequest {
    guard case .object(let obj) = expr, let request = obj as? HTTPServerRequest else {
      throw RuntimeError.type(expr, expected: [HTTPServerRequest.type])
    }
    return request
  }
  
  private func multiPart(from expr: Expr) throws -> HTTPMultiPart {
    guard case .object(let obj) = expr, let multipart = obj as? HTTPMultiPart else {
      throw RuntimeError.type(expr, expected: [HTTPMultiPart.type])
    }
    return multipart
  }
  
  private func httpServerResponse(from expr: Expr) throws -> HTTPServerResponse {
    guard case .object(let obj) = expr, let request = obj as? HTTPServerResponse else {
      throw RuntimeError.type(expr, expected: [HTTPServerResponse.type])
    }
    return request
  }
  
  private func isHttpServer(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is HTTPServer else {
      return .false
    }
    return .true
  }
  
  // HTTP server functionality
  
  private func makeHttpServer(maxLength: Expr?, logging: Expr?) throws -> Expr {
    let length = maxLength == nil ?
                   10 : (maxLength!.isFalse ? 1000 : try maxLength!.asInt(above: 2, below: 1000))
    return .object(HTTPServer(context: self.context,
                              queueLength: length,
                              requestEnteringTimeout: 2.0,
                              minLogSeverity: try logging?.asInt(above: 0, below: 6) ?? 1))
  }
  
  private func isHttpServerRunning(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.httpServer(from: expr).operating)
  }
  
  private func httpServerPort(expr: Expr) throws -> Expr {
    let server = try self.httpServer(from: expr)
    do {
      return .makeNumber(try server.port())
    } catch {
      return .false
    }
  }
  
  private func isHttpServerIPv4(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.httpServer(from: expr).isIPv4())
  }
  
  private func httpServerOpenConnections(expr: Expr) throws -> Expr {
    return .makeNumber(try self.httpServer(from: expr).openConnections)
  }
  
  private func httpServerRoutes(expr: Expr) throws -> Expr {
    let routes = try self.httpServer(from: expr).routes
    var res = Expr.null
    for route in routes {
      if route.isEmpty {
        res = .pair(.makeString("/"), res)
      } else {
        res = .pair(.makeString(route), res)
      }
    }
    return res
  }
  
  private func httpServerHandlers(expr: Expr) throws -> Expr {
    return .makeList(try self.httpServer(from: expr).handlers.exprs)
  }
  
  private func httpServerReset(expr: Expr) throws -> Expr {
    return .object(try self.httpServer(from: expr).resetIfNeeded(in: self.context))
  }
  
  private func httpServerLogSeverity(expr: Expr) throws -> Expr {
    return .makeNumber(try self.httpServer(from: expr).minLogSeverity)
  }
  
  private func httpServerLogSeveritySet(expr: Expr, value: Expr) throws -> Expr {
    try self.httpServer(from: expr).minLogSeverity = try value.asInt(above: 0, below: 6)
    return .void
  }
  
  private func httpServerTimeout(expr: Expr) throws -> Expr {
    return .makeNumber(try self.httpServer(from: expr).requestEnteringTimeout)
  }
  
  private func httpServerTimeoutSet(expr: Expr, value: Expr) throws -> Expr {
    try self.httpServer(from: expr).requestEnteringTimeout = try value.asDouble(coerce: true)
    return .void
  }
  
  private func httpServerLog(expr: Expr, level: Expr, tag: Expr, args: Arguments) throws -> Expr {
    let server = try self.httpServer(from: expr)
    let tag = tag.isFalse ? nil : try tag.asString()
    var str = ""
    for arg in args {
      str = str + arg.unescapedDescription
    }
    server.log(level: try level.asInt(above: 0, below: 6), tag: tag, str)
    return .void
  }
  
  private func httpServerRegister(expr: Expr, fst: Expr, snd: Expr, thd: Expr?) throws -> Expr {
    if let handler = thd {
      _ = try handler.asProcedure()
      try self.httpServer(from: expr).register(method: try fst.asString().uppercased(),
                                               path: try snd.asString(),
                                               handler: handler)
    } else {
      _ = try snd.asProcedure()
      try self.httpServer(from: expr).register(method: nil,
                                               path: try fst.asString(),
                                               handler: snd)
    }
    return .void
  }
  
  private func httpServerRegisterDefault(expr: Expr, handler: Expr) throws -> Expr {
    _ = try handler.asProcedure()
    try self.httpServer(from: expr).registerNotFound(handler: handler)
    return .void
  }
  
  private func httpServerStart(expr: Expr, port: Expr, forceIPv4: Expr?) throws -> Expr {
    let server = try self.httpServer(from: expr)
    try server.start(UInt16(port.asInt(above: 0, below: Int(UInt16.max))),
                     forceIPv4: forceIPv4?.isTrue ?? false,
                     priority: nil,
                     handlerPriority: nil)
    return .void
  }
  
  private func httpServerStop(expr: Expr, force: Expr?) throws -> Expr {
    let server = try self.httpServer(from: expr)
    server.stop()
    if force?.isTrue ?? false {
      try server.queue.abort(in: self.context)
    } else {
      try server.queue.close(in: self.context)
    }
    return .void
  }
  
  private func httpServerNumWorkers(expr: Expr) throws -> Expr {
    return .makeNumber(try self.httpServer(from: expr).numWorkers)
  }
  
  private func httpServerAttachWorker(expr: Expr, worker: Expr) throws -> Expr {
    guard case .object(let obj) = worker, let thread = obj as? NativeThread else {
      throw RuntimeError.type(worker, expected: [NativeThread.type])
    }
    try self.httpServer(from: expr).register(worker: thread)
    return .void
  }
  
  private func httpServerRemoveWorker(expr: Expr, worker: Expr) throws -> Expr {
    guard case .object(let obj) = worker, let thread = obj as? NativeThread else {
      throw RuntimeError.type(worker, expected: [NativeThread.type])
    }
    try self.httpServer(from: expr).remove(worker: thread)
    return .void
  }
  
  private func httpServerRegisterMiddleware(expr: Expr, processor: Expr) throws -> Expr {
    let proc = try processor.asProcedure()
    try self.httpServer(from: expr).register(middlewareHandler: .procedure(proc))
    return .void
  }
  
  private func httpServerMiddleware(expr: Expr) throws -> Expr {
    return .makeList(try self.httpServer(from: expr).middlewareHandlers.exprs)
  }
  
  // HTTP server request functionality
  
  private func isSrvRequest(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is HTTPServerRequest else {
      return .false
    }
    return .true
  }
  
  private func serverRequestServer(expr: Expr) throws -> Expr {
    guard let obj = try self.httpServerRequest(from: expr).connection.server,
          let server = obj as? HTTPServer else {
      return .false
    }
    return .object(server)
  }
  
  private func serverRequestMethod(expr: Expr) throws -> Expr {
    return .makeString(try self.httpServerRequest(from: expr).connection.request.method.uppercased())
  }
  
  private func serverRequestPath(expr: Expr) throws -> Expr {
    return .makeString(try self.httpServerRequest(from: expr).connection.request.path)
  }
  
  private func serverRequestQuery(expr: Expr) throws -> Expr {
    return .makeString(try self.httpServerRequest(from: expr).pathAndParams)
  }
  
  private func serverRequestPathParam(expr: Expr, name: Expr, default: Expr?) throws -> Expr {
    let name = try name.asString()
    guard let res = try self.httpServerRequest(from: expr).connection.request.params[name] else {
      return `default` ?? .false
    }
    return .makeString(res)
  }
  
  private func serverRequestPathParamSet(expr: Expr, name: Expr, value: Expr) throws -> Expr {
    let name = try name.asString()
    try self.httpServerRequest(from: expr).connection.request.params[name] = value.asString()
    return .void
  }
  
  private func serverRequestPathParamRemove(expr: Expr, name: Expr) throws -> Expr {
    let name = try name.asString()
    try self.httpServerRequest(from: expr).connection.request.params.removeValue(forKey: name)
    return .void
  }
  
  private func serverRequestPathParams(expr: Expr) throws -> Expr {
    let params = try self.httpServerRequest(from: expr).connection.request.params
    var res = Expr.null
    for (name, value) in params {
      res = .pair(.pair(.makeString(name), .makeString(value)), res)
    }
    return res
  }
  
  private func serverRequestQueryParam(expr: Expr, name: Expr) throws -> Expr {
    let params = try self.httpServerRequest(from: expr).connection.request.queryParams
    let name = try name.asString()
    var res = Expr.null
    for (param, value) in params.reversed() {
      if param == name {
        res = .pair(.makeString(value), res)
      }
    }
    return res
  }
  
  private func serverRequestQueryParams(expr: Expr) throws -> Expr {
    let attributes = try self.httpServerRequest(from: expr).connection.request.queryParams
    var res = Expr.null
    for (name, value) in attributes {
      res = .pair(.pair(.makeString(name), .makeString(value)), res)
    }
    return res
  }
  
  private func serverRequestHeader(expr: Expr, name: Expr, default: Expr?) throws -> Expr {
    let name = try name.asString()
    guard let res = try self.httpServerRequest(from: expr).connection.request.header(name) else {
      return `default` ?? .false
    }
    return .makeString(res)
  }
  
  private func serverRequestHeaderSet(expr: Expr, name: Expr, value: Expr) throws -> Expr {
    let name = try name.asString()
    try self.httpServerRequest(from: expr).connection.request.setHeader(name, to: value.toString())
    return .void
  }
  
  private func serverRequestHeaderRemove(expr: Expr, name: Expr) throws -> Expr {
    let name = try name.asString()
    try self.httpServerRequest(from: expr).connection.request.removeHeader(name)
    return .void
  }
  
  private func serverRequestHeaders(expr: Expr) throws -> Expr {
    let request = try self.httpServerRequest(from: expr).connection.request
    var res = Expr.null
    for header in request.availableHeaders {
      if let value = request.header(header) {
        res = .pair(.pair(.makeString(header), .makeString(value)), res)
      }
    }
    return res
  }
  
  private func serverRequestBody(expr: Expr) throws -> Expr {
    return .bytes(MutableBox(try self.httpServerRequest(from: expr).connection.request.body))
  }
  
  private func serverRequestBodyToString(expr: Expr) throws -> Expr {
    guard let str = String(bytes: try self.httpServerRequest(from: expr).connection.request.body,
                           encoding: .utf8) else {
      return .false
    }
    return .makeString(str)
  }
  
  private func serverRequestFormAttributes(expr: Expr) throws -> Expr {
    let attributes = try self.httpServerRequest(from: expr).connection.request.parseUrlencodedForm()
    var res = Expr.null
    for (name, value) in attributes {
      res = .pair(.pair(.makeString(name), .makeString(value)), res)
    }
    return res
  }
  
  private func serverRequestFormMultiparts(expr: Expr) throws -> Expr {
    let multiparts =
      try self.httpServerRequest(from: expr).connection.request.parseMultiPartFormData().map {
        Expr.object(HTTPMultiPart(multipart: $0))
      }
    return .makeList(Exprs(multiparts))
  }
  
  private func serverRequestAddress(expr: Expr) throws -> Expr {
    guard let str = try self.httpServerRequest(from: expr).connection.request.address else {
      return .false
    }
    return .makeString(str)
  }
  
  private func serverRequestSendResponse(expr: Expr,
                                         response: Expr,
                                         dontKeepAlive: Expr?) throws -> Expr {
    let connection = try self.httpServerRequest(from: expr).connection
    let response = try self.httpServerResponse(from: response).response
    guard let res = connection.send(response, dontKeepAlive: dontKeepAlive?.isTrue ?? false) else {
      return .false
    }
    var new = res.handler(res.request)
    if let handler = res.request.custom[HTTPServer.handlerKey] as? Expr {
      res.request.custom.removeValue(forKey: HTTPServer.handlerKey)
      return .pair(handler, .object(HTTPServerRequest(connection: res)))
    } else {
      new.body = .text("internal server error: could not connect to handler")
      _ = res.send(new, dontKeepAlive: true)
      return .false
    }
  }
  
  // HTTP server request multi-part functionality
  
  private func isServerMultipart(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is HTTPMultiPart else {
      return .false
    }
    return .true
  }
  
  private func serverMultipartHeaders(expr: Expr) throws -> Expr {
    let headers = try self.multiPart(from: expr).multipart.headers
    var res = Expr.null
    for (key, value) in headers {
      res = .pair(.pair(.makeString(key), .makeString(value)), res)
    }
    return res
  }
  
  private func serverMultipartHeader(expr: Expr, name: Expr, default: Expr?) throws -> Expr {
    let name = try name.asString().lowercased()
    guard let res = try self.multiPart(from: expr).multipart.headers[name] else {
      return `default` ?? .false
    }
    return .makeString(res)
  }
  
  private func serverMultipartBody(expr: Expr) throws -> Expr {
    return .bytes(MutableBox(try self.multiPart(from: expr).multipart.body))
  }
  
  // HTTP server response functionality
  
  private func isSrvResponse(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is HTTPServerResponse else {
      return .false
    }
    return .true
  }
  
  private func headers(from hdrs: Expr?) throws -> [String : String] {
    var headers: [String : String] = [:]
    if hdrs?.isTrue ?? false {
      var list = hdrs!
      while case .pair(.pair(let car, let cdr), let next) = list {
        headers[try car.asString()] = try cdr.asString()
        list = next
      }
      guard case .null = list else {
        throw RuntimeError.type(hdrs!, expected: [.properListType])
      }
    }
    return headers
  }
  
  private func makeSrvResponse(args: Arguments) throws -> Expr {
    guard let (statusCode, hdrs, body, ct) =
            args.optional(.makeNumber(500), .false, .false, .false) else {
      throw RuntimeError.argumentCount(of: "shared-queue-enqueue/wait!",
                                       min: 2,
                                       max: 5,
                                       args: .makeList(args))
    }
    return .object(HTTPServerResponse(response:
             NanoHTTPResponse(statusCode: try statusCode.asInt(above: 0, below: 1000),
                              headers: try self.headers(from: hdrs),
                              body: try self.srvResponseBody(from: body,
                                                             contentType: ct.isFalse ? nil : ct))))
  }
  
  private func srvResponseOk(fst: Expr?, snd: Expr?) throws -> Expr {
    let (body, headers) = try self.bodyAndHeader(fst, snd)
    return .object(HTTPServerResponse(response: .ok(headers: headers, body)))
  }
  
  private func srvResponseCreated() -> Expr {
    return .object(HTTPServerResponse(response: .created()))
  }
  
  private func srvResponseAccepted() -> Expr {
    return .object(HTTPServerResponse(response: .accepted()))
  }
  
  private func srvResponseMovedPermanently(expr: Expr, hdrs: Expr?) throws -> Expr {
    return .object(HTTPServerResponse(response: .movedPermanently(try expr.asString(),
                                                                  try self.headers(from: hdrs))))
  }
  
  private func srvResponseMovedTemporarily(expr: Expr, hdrs: Expr?) throws -> Expr {
    return .object(HTTPServerResponse(response: .movedTemporarily(try expr.asString(),
                                                                  try self.headers(from: hdrs))))
  }
  
  private func bodyAndHeader(_ fst: Expr?, _ snd: Expr?) throws -> (NanoHTTPResponse.Body,
                                                                    [String : String]) {
    if let snd {
      return (try self.srvResponseBody(from: fst!, contentType: nil), try self.headers(from: snd))
    } else if let fst {
      return (try self.srvResponseBody(from: fst, contentType: nil), [:])
    } else {
      return (.empty, [:])
    }
  }
  
  private func srvResponseStatusCode(expr: Expr) throws -> Expr {
    return .makeNumber(try self.httpServerResponse(from: expr).response.statusCode)
  }
  
  private func srvResponseStatusCodeSet(expr: Expr, code: Expr) throws -> Expr {
    try self.httpServerResponse(from: expr).response.statusCode = code.asInt(above: 0, below: 1000)
    return .void
  }
  
  private func srvResponseHeaders(expr: Expr) throws -> Expr {
    let headers = try self.httpServerResponse(from: expr).response.headers
    var res = Expr.null
    for (key, value) in headers {
      res = .pair(.pair(.makeString(key), .makeString(value)), res)
    }
    return res
  }
  
  private func srvResponseHeader(expr: Expr, name: Expr, default: Expr?) throws -> Expr {
    if let res = try self.httpServerResponse(from: expr).response.headers[name.asString()] {
      return .makeString(res)
    }
    return `default` ?? .false
  }
  
  private func srvResponseHeaderSet(expr: Expr, name: Expr, value: Expr) throws -> Expr {
    try self.httpServerResponse(from: expr).response.headers[name.asString()] = value.asString()
    return .void
  }
  
  private func srvResponseHeaderRemove(expr: Expr, name: Expr) throws -> Expr {
    try self.httpServerResponse(from: expr).response.headers.removeValue(forKey: name.asString())
    return .void
  }
  
  private func srvResponseBody(from obj: Expr, contentType: Expr?) throws -> NanoHTTPResponse.Body {
    switch obj {
      case .false:
        return .empty
      case .string(_):
        return .text(try obj.asString())
      case .bytes(let bv):
        return .data(Data(bv.value), contentType: try contentType?.asString())
      case .object(let obj):
        if let json = obj as? JSON {
          return .json(json)
        } else if let mutable = obj as? MutableJSON {
          return .json(mutable.value)
        } else if let str = obj as? StyledText {
          let data = try str.value.data(
                       from: NSRange(location: 0, length: str.value.length),
                       documentAttributes:[.documentType: NSAttributedString.DocumentType.html,
                                           .characterEncoding: String.Encoding.utf8.rawValue])
          if let res = String(data: data, encoding: String.Encoding.utf8) {
            return .htmlBody(res)
          } else {
            fallthrough
          }
        } else {
          fallthrough
        }
      default:
        return .json(try JSONLibrary.toJSON(obj, in: self.context))
    }
  }
  
  private func srvResponseBodySet(expr: Expr, obj: Expr, contentType: Expr?) throws -> Expr {
    try self.httpServerResponse(from: expr).response.body =
      self.srvResponseBody(from: obj, contentType: contentType)
    return .void
  }
  
  private func srvResponseBodyHtmlSet(expr: Expr, html: Expr, justBody: Expr?) throws -> Expr {
    if justBody?.isTrue ?? false {
      try self.httpServerResponse(from: expr).response.body = .htmlBody(html.asString())
    } else {
      try self.httpServerResponse(from: expr).response.body = .html(html.asString())
    }
    return .void
  }
  
  private func parseHttpHeaderValue(expr: Expr) throws -> Expr {
    return .makeList(Exprs(universalHeaderValues(try expr.asString())))
  }
  
  private func httpHeaderParam(expr: Expr, name: Expr) throws -> Expr {
    let name = try name.asString().lowercased()
    var list = expr
    while case .pair(let element, let next) = list {
      if case .pair(.string(let str), let value) = element, (str as String) == name {
        return value
      }
      list = next
    }
    return .false
  }
  
  private func parseComponents(_ value: String) -> [String] {
    var components: [String] = []
    var inString = false
    var start = value.startIndex
    var current = value.startIndex
    while current < value.endIndex {
      switch value[current] {
        case "\"":
          inString = !inString
          current = value.index(after: current)
        case ",":
          if !inString {
            let component = String(value[start..<current])
                              .trimmingCharacters(in: .whitespacesAndNewlines)
            if !component.isEmpty {
              components.append(String(value[start..<current]))
            }
            current = value.index(after: current)
            start = current
          }
        default:
          current = value.index(after: current)
      }
    }
    let component = String(value[start..<current])
                      .trimmingCharacters(in: .whitespacesAndNewlines)
    if !component.isEmpty {
      components.append(String(value[start..<current]))
    }
    return components
  }

  private func parseHeaderValue(_ value: String) -> Expr {
    var components: Exprs = []
    var inString = false
    var equals: String.Index? = nil
    var start = value.startIndex
    var current = value.startIndex
    while current < value.endIndex {
      switch value[current] {
        case "\"":
          inString = !inString
          current = value.index(after: current)
        case "=":
          if !inString && equals == nil {
            equals = current
          }
          current = value.index(after: current)
        case ";":
          if !inString {
            var done = false
            if let equals {
              let name = String(value[start..<equals])
                           .trimmingCharacters(in: .whitespacesAndNewlines)
              if !name.isEmpty {
                let value = String(value[value.index(after: equals)..<current])
                              .trimmingCharacters(in: .whitespacesAndNewlines)
                components.append(.pair(.makeString(name.unquote().lowercased()),
                                        .makeString(value.unquote())))
                done = true
              }
            }
            if !done {
              let token = String(value[start..<current])
                            .trimmingCharacters(in: .whitespacesAndNewlines)
              if !token.isEmpty {
                components.append(.makeString(token.unquote()))
              }
            }
            current = value.index(after: current)
            start = current
            equals = nil
          }
        default:
          current = value.index(after: current)
      }
    }
    if let equals {
      let name = String(value[start..<equals])
                   .trimmingCharacters(in: .whitespacesAndNewlines)
      if !name.isEmpty {
        let value = String(value[value.index(after: equals)..<current])
                      .trimmingCharacters(in: .whitespacesAndNewlines)
        components.append(.pair(.makeString(name.unquote().lowercased()),
                                .makeString(value.unquote())))
        return .makeList(components)
      }
    }
    let token = String(value[start..<current]).trimmingCharacters(in: .whitespacesAndNewlines)
    if !token.isEmpty {
      components.append(.makeString(token.unquote()))
    }
    return .makeList(components)
  }

  private func universalHeaderValues(_ value: String) -> [Expr] {
    return parseComponents(value).map(parseHeaderValue)
  }
  
  private func shareFileHandler(expr: Expr) throws -> Expr {
    let handler = share(file: try expr.asAbsolutePath(in: self.context))
    return .procedure(Procedure("share-file") { (request: Expr) in
      let request = try self.httpServerRequest(from: request)
      let response = handler(request.connection.request)
      if response.statusCode == 200,
         let obj = request.connection.server,
         let server = obj as? HTTPServer,
         let (_, value) = request.connection.request.params.first,
         let path = value.removingPercentEncoding {
        server.log(level: 1, tag: "worker/share", "sharing file \"\(path)\"")
      }
      return .object(HTTPServerResponse(response: response))
    })
  }
  
  private func shareDirectoryHandler(expr: Expr) throws -> Expr {
    let handler = share(directory: try expr.asAbsolutePath(in: self.context))
    return .procedure(Procedure("share-directory") { (request: Expr) in
      let request = try self.httpServerRequest(from: request)
      let response = handler(request.connection.request)
      if response.statusCode == 200,
         let obj = request.connection.server,
         let server = obj as? HTTPServer,
         let (_, value) = request.connection.request.params.first,
         let path = value.removingPercentEncoding {
        server.log(level: 1, tag: "worker/share", "sharing directory \"\(path)\"")
      }
      return .object(HTTPServerResponse(response: response))
    })
  }
  
  private func browseDirectoryHandler(expr: Expr) throws -> Expr {
    let handler = browse(directory: try expr.asAbsolutePath(in: self.context))
    return .procedure(Procedure("browse-directory") { (request: Expr) in
      let request = try self.httpServerRequest(from: request)
      let response = handler(request.connection.request)
      if response.statusCode == 200,
         let obj = request.connection.server,
         let server = obj as? HTTPServer,
         let (_, value) = request.connection.request.params.first,
         let path = value.removingPercentEncoding {
        server.log(level: 1, tag: "worker/share", "browsing directory \"\(path)\"")
      }
      return .object(HTTPServerResponse(response: response))
    })
  }
}

///
/// Represents a HTTP server request object.
///
public final class HTTPServerRequest: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "srv-request"))

  public let connection: NanoHTTPConnection
  
  init(connection: NanoHTTPConnection) {
    self.connection = connection
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    return "#<\(self.tagString)>"
  }
  
  public var pathAndParams: String {
    var components = URLComponents()
    components.path = self.connection.request.path
    if !self.connection.request.queryParams.isEmpty {
      components.queryItems = self.connection.request.queryParams.map { k, v in
        URLQueryItem(name: k, value: v)
      }
    }
    return components.url?.absoluteString ?? self.connection.request.path
  }
  
  public override var tagString: String {
    let p = self.connection.request.params.map{ k, v in "\(k)=\(v)" }.joined(separator: " ")
    var res = "\(self.type): \(self.connection.request.method) " +
              "\(self.pathAndParams), params=(\(p)), " +
              "headers=\(self.connection.request.availableHeaders.count)"
    if !self.connection.request.body.isEmpty {
      res += ", body=\(self.connection.request.body.count)"
    }
    return res
  }
  
  public override func unpack(in context: Context) -> Exprs {
    let p = self.connection.request.params.map{ k, v in Expr.pair(.makeString(k), .makeString(v)) }
    return [.makeString(self.identityString),
            .makeString(self.connection.request.method),
            .makeString(self.pathAndParams),
            .makeList(Exprs(p)),
            .makeNumber(self.connection.request.availableHeaders.count),
            .makeNumber(self.connection.request.body.count)]
  }
}

///
/// Represents a HTTP server response object.
///
public final class HTTPServerResponse: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "srv-response"))

  public var response: NanoHTTPResponse
  
  init(response: NanoHTTPResponse) {
    self.response = response
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    return "#<\(self.tagString)>"
  }
  
  private var bodyType: String {
    switch self.response.body {
      case .empty:
        return "empty"
      case .text(_):
        return "text"
      case .html(_):
        return "html"
      case .json(_):
        return "json"
      case .data(_, _):
        return "data"
      case .custom(_, _):
        return "custom"
      case .socket(_):
        return "socket"
    }
  }
  
  public override var tagString: String {
    return "\(self.type): status=\(self.response.statusCode), " +
           "headers=\(self.response.headers.count), body=\(self.bodyType)"
  }
  
  public override func unpack(in context: Context) -> Exprs {
    return [.makeString(self.identityString),
            .makeNumber(self.response.statusCode),
            .makeNumber(self.response.headers.count),
            .makeString(self.bodyType)]
  }
}

///
/// Represents a HTTP server response object.
///
public final class HTTPMultiPart: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "srv-multipart"))

  public var multipart: NanoHTTPRequest.MultiPart
  
  init(multipart: NanoHTTPRequest.MultiPart) {
    self.multipart = multipart
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    return "#<\(self.tagString)>"
  }
  
  public override var tagString: String {
    return "\(self.type) \(self.identityString)"
  }
  
  public override func unpack(in context: Context) -> Exprs {
    return [.makeString(self.identityString),
            .makeNumber(self.multipart.headers.count),
            .makeNumber(self.multipart.body.count)]
  }
}

///
/// Represents a HTTP server object.
///
open class HTTPServer: NanoHTTPServer, CustomExpr {
  public static let handlerKey = "http-server/handler"
  public static let type = Type.objectType(Symbol(uninterned: "http-server"))
  
  public weak var context: Context?
  public let lock: NSLock
  public private(set) var queue: SharedQueue
  public let handlers: Collection
  public let middlewareHandlers: Collection
  public let workers: Collection
  public var requestEnteringTimeout: TimeInterval
  public var minLogSeverity: Int // 0 = debug, 1 = info, 2 = warn, 3 = err, 4 = fatal
  
  public init(context: Context,
              queueLength: Int,
              requestEnteringTimeout: TimeInterval = 2.0,
              minLogSeverity: Int = 1) {
    self.context = context
    self.lock = NSLock()
    self.queue = SharedQueue(external: true, maxLength: queueLength)
    self.handlers = Collection(kind: .growableVector)
    self.middlewareHandlers = Collection(kind: .growableVector)
    self.workers = Collection(kind: .growableVector)
    self.requestEnteringTimeout = requestEnteringTimeout
    self.minLogSeverity = minLogSeverity
    context.objects.manage(self.handlers)
    context.objects.manage(self.workers)
    super.init()
    self.setUpMiddleware()
  }
  
  open func setUpMiddleware() {
    self.middleware.append { [weak self] request in
      self?.log(level: 1, tag: "req", "\(request.method.uppercased()) \(request.path) (\(request.address ?? "?"))")
      return nil
    }
  }
  
  open func register(method: String?, path: String, handler: Expr) {
    self.handlers.exprs.append(.pair(.makeString(path), handler))
    self.router.register(method?.uppercased(), path: path) { request in
      let response = NanoHTTPResponse(statusCode: 500, body: .text("unknown internal server error"))
      request.custom[Self.handlerKey] = handler
      return response
    }
  }
  
  open func registerNotFound(handler: Expr) {
    self.handlers.exprs.append(.pair(.false, handler))
    self.notFoundHandler = { request in
      let response = NanoHTTPResponse(statusCode: 404, body: .text("not found"))
      request.custom[Self.handlerKey] = handler
      return response
    }
  }
  
  open override func listen(priority: DispatchQoS.QoSClass?) {
    let port = (try? self.port()) ?? -1
    self.log(level: 1, tag: "server", "server started for port \(port)")
    super.listen(priority: priority)
    self.log(level: 1, tag: "server", "server stopped for port \(port)")
  }
  
  open override func handle(connection: NanoHTTPConnection) {
    var response = connection.handler(connection.request)
    if let handler = connection.request.custom[Self.handlerKey] as? Expr,
       let context = self.context {
      let handlingRequest = Expr.pair(handler, .object(HTTPServerRequest(connection: connection)))
      connection.request.custom.removeValue(forKey: Self.handlerKey)
      do {
        if try self.queue.enqueue(in: context,
                                  values: [handlingRequest],
                                  timeout: self.requestEnteringTimeout,
                                  push: false,
                                  wait: true,
                                  close: false) {
          // Successfully entered the request into the request handling queue; exit
          return
        }
        response.body = .text("overloaded: could not invoke request handler")
      } catch let e {
        response.body = .text("internal server error: \(e.localizedDescription)")
      }
    } else {
      response.body = .text("internal server error: could not connect to handler")
    }
    // Default: send error as it wasn't possible to invoke the handler
    _ = connection.send(response, dontKeepAlive: true)
  }
  
  open func log(level: Int, tag: String? = nil, _ str: String) {
    let tag = tag == nil ? "http" : "http/\(tag!)"
    if level >= self.minLogSeverity {
      self.context?.delegate?.print("[\(tag)] \(str)\n")
    }
  }
  
  open override func log(_ str: String) {
    self.log(level: 3, tag: "server", str)
  }
  
  public var numWorkers: Int {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    return self.workers.exprs.count
  }
  
  open func register(worker: NativeThread) throws {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    for expr in self.workers.exprs {
      guard case .object(let obj) = expr, let thread = obj as? NativeThread else {
        continue
      }
      if thread === worker {
        return
      }
    }
    self.workers.exprs.append(.object(worker))
  }
  
  open func remove(worker: NativeThread) throws {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    self.workers.exprs.removeAll { expr in
      guard case .object(let obj) = expr, let thread = obj as? NativeThread else {
        return false
      }
      return thread === worker
    }
  }
  
  open func register(middlewareHandler handler: Expr) throws {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    self.middlewareHandlers.exprs.append(handler)
  }
  
  open var middlewareHandlerSequence: Exprs {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    return self.middlewareHandlers.exprs
  }
  
  open func resetIfNeeded(in context: Context) throws -> SharedQueue {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    if try self.queue.isClosed(in: context) {
      self.queue = SharedQueue(external: true, maxLength: self.queue.maxLength)
    }
    for expr in self.workers.exprs {
      guard case .object(let obj) = expr, let thread = obj as? NativeThread else {
        continue
      }
      _ = thread.value.abort()
    }
    self.workers.exprs.removeAll()
    return self.queue
  }
  
  public var type: Type {
    return Self.type
  }
  
  public final var identity: UInt {
    return UInt(bitPattern: ObjectIdentifier(self))
  }
  
  public final var identityString: String {
    return String(self.identity, radix: 16)
  }
  
  public final func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }
  
  public var hash: Int {
    return self.hashValue
  }
  
  public var tagString: String {
    var res = "\(Self.type) \(self.identityString): handlers=\(self.handlers.exprs.count), "
    if let context, let reqcount = try? self.queue.length(in: context)  {
      res += "requests=\(reqcount), "
    }
    res += "timeout=\(self.requestEnteringTimeout), severity=\(self.minLogSeverity)"
    return res
  }
  
  public func equals(to expr: Expr) -> Bool {
    guard case .object(let obj) = expr,
          let other = obj as? HTTPServer else {
      return false
    }
    return self == other
  }
  
  public func mark(in gc: GarbageCollector) {
    self.queue.mark(in: gc)
    gc.mark(self.handlers)
    gc.mark(self.middlewareHandlers)
    gc.mark(self.workers)
  }
  
  public func unpack(in context: Context) -> Exprs {
    return [.makeString(self.identityString),
            .object(self.queue),
            .vector(self.handlers),
            .makeNumber(self.requestEnteringTimeout),
            .makeNumber(self.minLogSeverity)]
  }
  
  public static func ==(lhs: HTTPServer, rhs: HTTPServer) -> Bool {
    return lhs === rhs
  }
}

public protocol HTTPServerConfig {
  func createServer(context: Context,
                    queueLength: Int,
                    requestEnteringTimeout: TimeInterval) -> HTTPServer
}

public struct LispKitHTTPServerConfig: HTTPServerConfig {
  public func createServer(context: Context,
                           queueLength: Int,
                           requestEnteringTimeout: TimeInterval) -> HTTPServer {
    return HTTPServer(context: context,
                      queueLength: queueLength,
                      requestEnteringTimeout: requestEnteringTimeout)
  }
}
