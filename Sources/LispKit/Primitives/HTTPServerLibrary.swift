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
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("http-server?", self.isHttpServer))
    self.define(Procedure("make-http-server", self.makeHttpServer))
    self.define(Procedure("http-server-running?", self.isHttpServerRunning))
    self.define(Procedure("http-server-port", self.httpServerPort))
    self.define(Procedure("http-server-open-connections", self.httpServerOpenConnections))
    self.define(Procedure("http-server-routes", self.httpServerRoutes))
    self.define(Procedure("http-server-handlers", self.httpServerHandlers))
    self.define(Procedure("http-server-request-queue", self.httpServerRequestQueue))
    self.define(Procedure("http-server-log-severity", self.httpServerLogSeverity))
    self.define(Procedure("http-server-log-severity-set!", self.httpServerLogSeveritySet))
    self.define(Procedure("http-server-timeout", self.httpServerTimeout))
    self.define(Procedure("http-server-timeout-set!", self.httpServerTimeoutSet))
    self.define(Procedure("http-server-register!", self.httpServerRegister))
    self.define(Procedure("http-server-register-default!", self.httpServerRegisterDefault))
    self.define(Procedure("http-server-start!", self.httpServerStart))
    self.define(Procedure("http-server-stop!", self.httpServerStop))
    self.define(Procedure("srv-request?", self.isSrvRequest))
    self.define(Procedure("srv-request-method", self.serverRequestMethod))
    self.define(Procedure("srv-request-path", self.serverRequestPath))
    self.define(Procedure("srv-request-query-param-ref", self.serverRequestQueryParamRef))
    self.define(Procedure("srv-request-query-params", self.serverRequestQueryParams))
    self.define(Procedure("srv-request-path-param-ref", self.serverRequestPathParamRef))
    self.define(Procedure("srv-request-path-param-set!", self.serverRequestPathParamSet))
    self.define(Procedure("srv-request-path-param-remove!", self.serverRequestPathParamRemove))
    self.define(Procedure("srv-request-path-params", self.serverRequestPathParams))
    self.define(Procedure("srv-request-header-ref", self.serverRequestHeaderRef))
    self.define(Procedure("srv-request-header-set!", self.serverRequestHeaderSet))
    self.define(Procedure("srv-request-header-remove!", self.serverRequestHeaderRemove))
    self.define(Procedure("srv-request-headers", self.serverRequestHeaders))
    self.define(Procedure("srv-request-body", self.serverRequestBody))
    self.define(Procedure("srv-request-body->string", self.serverRequestBodyToString))
    self.define(Procedure("srv-request-form-attributes", self.serverRequestFormAttributes))
    self.define(Procedure("srv-request-address", self.serverRequestAddress))
    self.define(Procedure("srv-request-address", self.serverRequestAddress))
    self.define(Procedure("srv-request-log", self.serverRequestLog))
    self.define(Procedure("srv-request-send-response", self.serverRequestSendResponse))
    self.define(Procedure("srv-response?", self.isSrvResponse))
    self.define(Procedure("make-srv-response", self.makeSrvResponse))
    self.define(Procedure("srv-response-status-code", self.srvResponseStatusCode))
    self.define(Procedure("srv-response-status-code-set!", self.srvResponseStatusCodeSet))
    self.define(Procedure("srv-response-headers", self.srvResponseHeaders))
    self.define(Procedure("srv-response-header", self.srvResponseHeader))
    self.define(Procedure("srv-response-header-set!", self.srvResponseHeaderSet))
    self.define(Procedure("srv-response-header-remove!", self.srvResponseHeaderRemove))
    self.define(Procedure("srv-response-body-set!", self.srvResponseBodySet))
    self.define(Procedure("srv-response-body-html-set!", self.srvResponseBodyHtmlSet))
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
    return .object(HTTPServer(context: self.context,
                              queueLength: try maxLength?.asInt(above: 2, below: 1000) ?? 10,
                              queueCapacity: nil,
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
      res = .pair(.makeString(route), res)
    }
    return res
  }
  
  private func httpServerHandlers(expr: Expr) throws -> Expr {
    return .makeList(try self.httpServer(from: expr).handlers.exprs)
  }
  
  private func httpServerRequestQueue(expr: Expr) throws -> Expr {
    return .object(try self.httpServer(from: expr).queue)
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
  
  private func httpServerRegister(expr: Expr, method: Expr, path: Expr, handler: Expr) throws -> Expr {
    _ = try handler.asProcedure()
    try self.httpServer(from: expr).register(method: try method.asString(),
                                             path: try path.asString(),
                                             handler: handler)
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
  
  private func httpServerStop(expr: Expr) throws -> Expr {
    try self.httpServer(from: expr).stop()
    return .void
  }
  
  // HTTP server request functionality
  
  private func isSrvRequest(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is HTTPServerRequest else {
      return .false
    }
    return .true
  }
  
  private func serverRequestMethod(expr: Expr) throws -> Expr {
    return .makeString(try self.httpServerRequest(from: expr).connection.request.method)
  }
  
  private func serverRequestPath(expr: Expr) throws -> Expr {
    return .makeString(try self.httpServerRequest(from: expr).connection.request.path)
  }
  
  private func serverRequestPathParamRef(expr: Expr, name: Expr, default: Expr?) throws -> Expr {
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
  
  private func serverRequestQueryParamRef(expr: Expr, name: Expr) throws -> Expr {
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
  
  private func serverRequestHeaderRef(expr: Expr, name: Expr, default: Expr?) throws -> Expr {
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
  
  private func serverRequestAddress(expr: Expr) throws -> Expr {
    guard let str = try self.httpServerRequest(from: expr).connection.request.address else {
      return .false
    }
    return .makeString(str)
  }
  
  private func serverRequestLog(expr: Expr, level: Expr, args: Arguments) throws -> Expr {
    let request = try self.httpServerRequest(from: expr)
    if let x = request.connection.server, let server = x as? HTTPServer {
      var str = ""
      for arg in args {
        str = try str + arg.asString()
      }
      server.log(level: try level.asInt(above: 0, below: 6), str)
    }
    return .void
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
  
  // HTTP server response functionality
  
  private func isSrvResponse(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is HTTPServerResponse else {
      return .false
    }
    return .true
  }
  
  private func makeSrvResponse(args: Arguments) throws -> Expr {
    guard let (statusCode, hdrs, body, ct) =
            args.optional(.makeNumber(500), .false, .false, .false) else {
      throw RuntimeError.argumentCount(of: "shared-queue-enqueue/wait!",
                                       min: 2,
                                       max: 5,
                                       args: .makeList(args))
    }
    var headers: [String : String] = [:]
    if hdrs.isTrue {
      var list = hdrs
      while case .pair(.pair(let car, let cdr), let next) = list {
        headers[try car.asString()] = try cdr.asString()
        list = next
      }
      guard case .null = list else {
        throw RuntimeError.type(hdrs, expected: [.properListType])
      }
    }
    return .object(HTTPServerResponse(response:
             NanoHTTPResponse(statusCode: try statusCode.asInt(above: 0, below: 1000),
                              headers: headers,
                              body: try self.srvResponseBody(from: body,
                                                             contentType: ct.isFalse ? nil : ct))))
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
  
  private func srvResponseHeader(expr: Expr, name: Expr) throws -> Expr {
    if let res = try self.httpServerResponse(from: expr).response.headers[name.asString()] {
      return .makeString(res)
    }
    return .false
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
  
  public override var tagString: String {
    return "\(self.type) \(self.identityString)"
  }
  
  public override func unpack(in context: Context) -> Exprs {
    return [.makeString(self.identityString)]
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
  
  public override var tagString: String {
    return "\(self.type) \(self.identityString)"
  }
  
  public override func unpack(in context: Context) -> Exprs {
    return [.makeString(self.identityString)]
  }
}

///
/// Represents a HTTP server object.
///
open class HTTPServer: NanoHTTPServer, CustomExpr {
  public static let handlerKey = "http-server/handler"
  public static let type = Type.objectType(Symbol(uninterned: "http-server"))
  
  public weak var context: Context?
  public let queue: SharedQueue
  public let handlers: Collection
  public var requestEnteringTimeout: TimeInterval
  public var minLogSeverity: Int // 0 = debug, 1 = info, 2 = warn, 3 = err, 4 = fatal
  
  public init(context: Context,
              queueLength: Int,
              queueCapacity: Int? = nil,
              requestEnteringTimeout: TimeInterval = 2.0,
              minLogSeverity: Int = 1) {
    self.context = context
    self.queue = SharedQueue(external: true, maxLength: queueLength, capacity: queueCapacity)
    self.handlers = Collection(kind: .growableVector)
    self.requestEnteringTimeout = requestEnteringTimeout
    self.minLogSeverity = minLogSeverity
    context.objects.manage(self.handlers)
    super.init()
    self.setUpMiddleware()
  }
  
  open func setUpMiddleware() {
    self.middleware.append { [weak self] request in
      self?.log(level: 1, "\(request.address ?? "?") -> \(request.method) \(request.path)")
      return nil
    }
  }
  
  open func register(method: String, path: String, handler: Expr) {
    self.handlers.exprs.append(.pair(.makeString(path), handler))
    self.router.register(method, path: path) { request in
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
    self.log(level: 1, "http server started for port \(port)")
    super.listen(priority: priority)
    self.log(level: 1, "http server stopped for port \(port)")
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
  
  open func log(level: Int, _ str: String) {
    if level >= self.minLogSeverity {
      self.log(str + "\n")
    }
  }
  
  open override func log(_ str: String) {
    self.context?.delegate?.print(str)
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
                    queueCapacity: Int?,
                    requestEnteringTimeout: TimeInterval) -> HTTPServer
}

public struct LispKitHTTPServerConfig: HTTPServerConfig {
  public func createServer(context: Context,
                           queueLength: Int,
                           queueCapacity: Int?,
                           requestEnteringTimeout: TimeInterval) -> HTTPServer {
    return HTTPServer(context: context,
                      queueLength: queueLength,
                      queueCapacity: queueCapacity,
                      requestEnteringTimeout: requestEnteringTimeout)
  }
}
