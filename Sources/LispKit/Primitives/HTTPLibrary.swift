//
//  HTTPLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 24/06/2024.
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

public final class HTTPLibrary: NativeLibrary {
  
  // Parameter objects
  public let sessionParam: Procedure
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    self.sessionParam = Procedure(.null,
                                  .object(HTTPSession(session:
                                                        URLSession(configuration: .default))))
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "http"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    // self.`import`(from: ["lispkit", "core"], "load")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    // Type tags
    self.define("http-session-type-tag", as: HTTPSession.type.objectTypeTag())
    self.define("http-request-type-tag", as: HTTPRequest.type.objectTypeTag())
    self.define("http-response-type-tag", as: HTTPResponse.type.objectTypeTag())

    // Parameter objects
    self.define("current-http-session", as: self.sessionParam)
    
    // HTTP sessions
    self.define(Procedure("http-session?", self.isHttpSession))
    self.define(Procedure("make-http-session", self.makeHttpSession))
    self.define(Procedure("http-session-copy", self.httpSessionCopy))
    self.define(Procedure("http-session-timeout", self.httpSessionTimeout))
    self.define(Procedure("http-session-send-cookies?", self.httpSessionSendCookies))
    self.define(Procedure("http-session-cache-policy", self.httpSessionCachePolicy))
    self.define(Procedure("http-session-max-connections", self.httpSessionMaxConnections))
    self.define(Procedure("http-session-use-pipelining?", self.httpSessionUsePipelining))
    self.define(Procedure("http-session-allow-cellular?", self.httpSessionAllowCellular))
    self.define(Procedure("http-session-tasks", self.httpSessionTasks))
    self.define(Procedure("http-session-flush!", self.httpSessionFlush))
    self.define(Procedure("http-session-reset!", self.httpSessionReset))
    self.define(Procedure("http-session-finish!", self.httpSessionFinish))
    self.define(Procedure("http-session-cancel!", self.httpSessionCancel))
    self.define(Procedure("http-session-send", self.httpSessionSend))
    
    // HTTP requests
    self.define(Procedure("http-request?", self.isHttpRequest))
    self.define(Procedure("make-http-request", self.makeHttpRequest))
    self.define(Procedure("http-request-copy", self.httpRequestCopy))
    self.define(Procedure("http-request-url", self.httpRequestUrl))
    self.define(Procedure("http-request-method", self.httpRequestMethod))
    self.define(Procedure("http-request-headers", self.httpRequestHeaders))
    self.define(Procedure("http-request-header", self.httpRequestHeader))
    self.define(Procedure("http-request-header-set!", self.httpRequestHeaderSet))
    self.define(Procedure("http-request-header-remove!", self.httpRequestHeaderRemove))
    self.define(Procedure("http-request-content", self.httpRequestContent))
    self.define(Procedure("http-request-content-set!", self.httpRequestContentSet))
    self.define(Procedure("http-request-timeout", self.httpRequestTimeout))
    self.define(Procedure("http-request-timeout-set!", self.httpRequestTimeoutSet))
    self.define(Procedure("http-request-send-cookies", self.httpRequestSendCookies))
    self.define(Procedure("http-request-cache-policy", self.httpRequestCachePolicy))
    self.define(Procedure("http-request-use-pipelining", self.httpRequestUsePipelining))
    self.define(Procedure("http-request-allow-cellular", self.httpRequestAllowCellular))
    
    // HTTP responses
    self.define(Procedure("http-response?", self.isHttpResponse))
    self.define(Procedure("http-response-content", self.httpResponseContent))
    self.define(Procedure("http-response-status-code", self.httpResponseStatusCode))
    self.define(Procedure("http-response-mime-type", self.httpResponseMimeType))
    self.define(Procedure("http-response-encoding", self.httpResponseEncoding))
    self.define(Procedure("http-response-url", self.httpResponseUrl))
    self.define(Procedure("http-response-headers", self.httpResponseHeaders))
    self.define(Procedure("http-response-header", self.httpResponseHeader))
    
    // Utility
    self.define(Procedure("http-status-code->string", self.httpStatusCodeToString))
  }
  
  private func session(from: Expr?) throws -> HTTPSession {
    guard var expr = from ?? self.context.evaluator.getParam(self.sessionParam) else {
      throw RuntimeError.eval(.undefinedDefaultHttpSession)
    }
    if case .true = expr {
      expr = self.context.evaluator.getParam(self.sessionParam) ?? expr
    }
    guard case .object(let obj) = expr, let session = obj as? HTTPSession else {
      throw RuntimeError.type(expr, expected: [HTTPSession.type])
    }
    return session
  }
  
  private func request(from: Expr) throws -> HTTPRequest {
    guard case .object(let obj) = from, let request = obj as? HTTPRequest else {
      throw RuntimeError.type(from, expected: [HTTPRequest.type])
    }
    return request
  }
  
  private func response(from: Expr) throws -> HTTPResponse {
    guard case .object(let obj) = from, let session = obj as? HTTPResponse else {
      throw RuntimeError.type(from, expected: [HTTPResponse.type])
    }
    return session
  }
  
  private func isHttpSession(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is HTTPSession else {
      return .false
    }
    return .true
  }
  
  public static func cachePolicy(from: Expr) throws -> URLRequest.CachePolicy {
    guard case .symbol(let sym) = from else {
      throw RuntimeError.eval(.unknownCachePolicy, from)
    }
    switch sym.identifier {
      case "use-protocol-cache-policy":
        return .useProtocolCachePolicy
      case "reload-ignoring-local-cache":
        return .reloadIgnoringLocalCacheData
      case "reload-ignoring-local-remote-cache":
        return .reloadIgnoringLocalAndRemoteCacheData
      case "return-cache-data-else-load":
        return .returnCacheDataElseLoad
      case "return-cache-data-dont-load":
        return .returnCacheDataDontLoad
      case "reload-revalidating-cache":
        return .reloadRevalidatingCacheData
      default:
        throw RuntimeError.eval(.unknownCachePolicy, from)
    }
  }
  
  public static func string(forCachePolicy policy: URLRequest.CachePolicy) -> String {
    switch policy {
      case .useProtocolCachePolicy:
        return "use-protocol-cache-policy"
      case .reloadIgnoringLocalCacheData:
        return "reload-ignoring-local-cache"
      case .reloadIgnoringLocalAndRemoteCacheData:
        return "reload-ignoring-local-remote-cache"
      case .returnCacheDataElseLoad:
        return "return-cache-data-else-load"
      case .returnCacheDataDontLoad:
        return "return-cache-data-dont-load"
      case .reloadRevalidatingCacheData:
        return "reload-revalidating-cache"
      @unknown default:
        return "unknown"
    }
  }
  
  public static func overrideConfig(_ config: inout URLSessionConfiguration,
                                    with iter: inout IndexingIterator<Arguments>) throws {
    if let timeout = iter.next(), timeout != .false && timeout != .null {
      config.timeoutIntervalForRequest = try timeout.asDouble(coerce: true)
    }
    if let cookies = iter.next(), cookies != .null {
      config.httpShouldSetCookies = cookies.isTrue
    }
    if let cache = iter.next(), cache != .false && cache != .null {
      config.requestCachePolicy = try Self.cachePolicy(from: cache)
    }
    if let maxconnections = iter.next(), maxconnections != .false && maxconnections != .null {
      config.httpMaximumConnectionsPerHost = try maxconnections.asInt(above: 1, below: 100000)
    }
    if let pipelining = iter.next(), pipelining != .null {
      config.httpShouldUsePipelining = pipelining.isTrue
    }
    if let cellular = iter.next(), cellular != .null {
      config.allowsCellularAccess = cellular.isTrue
    }
  }
  
  private func makeHttpSession(args: Arguments) throws -> Expr {
    var iter = args.makeIterator()
    var config: URLSessionConfiguration = .default
    if let prototype = iter.next() {
      switch prototype {
        case .false:
          break
        case .true:
          config = .ephemeral
        case .object(let obj):
          if let session = obj as? HTTPSession {
            config = session.configuration
            break
          }
          fallthrough
        default:
          throw RuntimeError.type(prototype, expected: [HTTPSession.type])
      }
    }
    try Self.overrideConfig(&config, with: &iter)
    guard iter.next() == nil else {
      throw RuntimeError.argumentCount(of: "make-http-session",
                                       min: 0,
                                       max: 6,
                                       args: .makeList(args))
    }
    return .object(HTTPSession(session: URLSession(configuration: config)))
  }
  
  private func httpSessionCopy(expr: Expr?) throws -> Expr {
    return .object(try self.session(from: expr).copy())
  }
  
  private func httpSessionTimeout(expr: Expr?) throws -> Expr {
    let session = try self.session(from: expr)
    return .makeNumber(session.configuration.timeoutIntervalForRequest)
  }
  
  private func httpSessionSendCookies(expr: Expr?) throws -> Expr {
    let session = try self.session(from: expr)
    return .makeBoolean(session.configuration.httpShouldSetCookies)
  }
  
  private func httpSessionCachePolicy(expr: Expr?) throws -> Expr {
    let session = try self.session(from: expr)
    return .symbol(self.context.symbols.intern(
                     Self.string(forCachePolicy: session.configuration.requestCachePolicy)))
  }
  
  private func httpSessionMaxConnections(expr: Expr?) throws -> Expr {
    let session = try self.session(from: expr)
    return .makeNumber(session.configuration.httpMaximumConnectionsPerHost)
  }
  
  private func httpSessionUsePipelining(expr: Expr?) throws -> Expr {
    let session = try self.session(from: expr)
    return .makeBoolean(session.configuration.httpShouldUsePipelining)
  }
  
  private func httpSessionAllowCellular(expr: Expr?) throws -> Expr {
    let session = try self.session(from: expr)
    return .makeBoolean(session.configuration.allowsCellularAccess)
  }
  
  private func httpSessionTasks(expr: Expr?) throws -> Expr {
    let session = try self.session(from: expr)
    let result = Future(external: false)
    session.session.getAllTasks { tasks in
      do {
        _ = try result.setResult(in: self.context, to: .makeNumber(tasks.count))
      } catch {}
    }
    return .object(result)
  }
  
  private func httpSessionFlush(expr: Expr?) throws -> Expr {
    let session = try self.session(from: expr)
    let result = Future(external: false)
    session.session.flush {
      do {
        _ = try result.setResult(in: self.context, to: .true)
      } catch {}
    }
    return .object(result)
  }
  
  private func httpSessionReset(expr: Expr?) throws -> Expr {
    let session = try self.session(from: expr)
    let result = Future(external: false)
    session.session.reset {
      do {
        _ = try result.setResult(in: self.context, to: .true)
      } catch {}
    }
    return .object(result)
  }
  
  private func httpSessionFinish(expr: Expr?) throws -> Expr {
    try self.session(from: expr).session.finishTasksAndInvalidate()
    return .void
  }
  
  private func httpSessionCancel(expr: Expr?) throws -> Expr {
    try self.session(from: expr).session.invalidateAndCancel()
    return .void
  }
  
  fileprivate static func headers(from: Expr) throws -> [String : String] {
    var header: [String : String] = [:]
    var expr = from
    while case .pair(.pair(let key, let value), let rest) = expr {
      header[try key.asString()] = try value.asString()
      expr = rest
    }
    guard case .null = expr else {
      throw RuntimeError.eval(.invalidHttpHeaderSpec, from, expr)
    }
    return header
  }
  
  private func responseHandler(_ f: Future) -> (Data?, URLResponse?, (any Error)?) -> Void {
    return { data, response, error in
      do {
        if let error = error {
          _ = try f.setResult(in: self.context, to: .error(RuntimeError.os(error)), raise: true)
        } else if let data = data, let httpResponse = response as? HTTPURLResponse {
          _ = try f.setResult(in: self.context,
                                   to: .object(HTTPResponse(response: httpResponse, body: data)),
                                   raise: false)
        } else {
          _ = try f.setResult(in: self.context,
                                   to: .error(RuntimeError.eval(.serverError)),
                                   raise: true)
        }
      } catch let error {
        do {
          _ = try f.setResult(in: self.context,
                              to: .error(RuntimeError.eval(.unableToReturnResultViaFuture,
                                                           .object(f),
                                                           .error(RuntimeError.os(error)))),
                              raise: true)
        } catch {}
      }
    }
  }
  
  private func httpSessionSend(request: Expr, expr: Expr?) throws -> Expr {
    let request = try self.request(from: request)
    let session = try self.session(from: expr)
    let result = Future(external: false)
    switch request.method {
      case "GET", "HEAD", "POST", "PUT", "DELETE", "CONNECT", "OPTIONS", "TRACE", "PATCH":
        session.dataTask(with: session.request(from: request),
                         completionHandler: self.responseHandler(result)).resume()
      default:
        throw RuntimeError.eval(.unsupportedHttpMethod, .makeString(request.method))
    }
    return .object(result)
  }
  
  // HTTP Request
  
  private func isHttpRequest(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is HTTPRequest else {
      return .false
    }
    return .true
  }
  
  private func makeHttpRequest(url: Expr, method: Expr, args: Arguments) throws -> Expr {
    return .object(try HTTPRequest(url: try url.asURL(),
                                   method: try method.asString().uppercased(),
                                   args: args))
  }
  
  private func httpRequestCopy(expr: Expr, args: Arguments) throws -> Expr {
    let request = try self.request(from: expr).copy()
    var iter = args.makeIterator()
    if let hdrs = iter.next(), hdrs != .false {
      request.headers = try HTTPLibrary.headers(from: hdrs)
    }
    if let tout = iter.next(), tout != .false && tout != .null {
      request.timeoutIntervalForRequest = try tout.asDouble(coerce: true)
    }
    if let ckies = iter.next(), ckies != .null {
      request.httpShouldSetCookies = ckies.isTrue
    }
    if let cche = iter.next(), cche != .false && cche != .null {
      request.requestCachePolicy = try HTTPLibrary.cachePolicy(from: cche)
    }
    if let pipelining = iter.next(), pipelining != .null {
      request.httpShouldUsePipelining = pipelining.isTrue
    }
    if let cellular = iter.next(), cellular != .null {
      request.allowsCellularAccess = cellular.isTrue
    }
    return .object(request)
  }
  
  private func httpRequestUrl(expr: Expr) throws -> Expr {
    return .makeString(try self.request(from: expr).url.absoluteString)
  }
  
  private func httpRequestMethod(expr: Expr) throws -> Expr {
    return .makeString(try self.request(from: expr).method)
  }
  
  private func httpRequestHeaders(expr: Expr) throws -> Expr {
    let headers = try self.request(from: expr).headers
    var res = Expr.null
    for (key, value) in headers {
      res = .pair(.pair(.makeString(key), .makeString(value)), res)
    }
    return res
  }
  
  private func httpRequestHeader(expr: Expr, key: Expr) throws -> Expr {
    if let value = try self.request(from: expr).headers[try key.asString()] {
      return .makeString(value)
    } else {
      return .false
    }
  }
  
  private func httpRequestHeaderSet(expr: Expr, key: Expr, value: Expr) throws -> Expr {
    let val: String
    switch value {
      case .symbol(let sym):
        val = sym.identifier
      case .fixnum(let num):
        val = num.description
      case .flonum(let num):
        val = num.description
      case .true:
        val = "?1"
      case .false:
        val = "?0"
      case .null:
        val = ""
      default:
        val = try value.asString()
    }
    try self.request(from: expr).headers[try key.asString()] = val
    return .void
  }
  
  private func httpRequestHeaderRemove(expr: Expr, key: Expr) throws -> Expr {
    if let old = try self.request(from: expr).headers.removeValue(forKey: key.asString()) {
      return .makeString(old)
    } else {
      return .false
    }
  }
  
  private func httpRequestContent(expr: Expr) throws -> Expr {
    let request = try self.request(from: expr)
    if let body = request.body {
      return .bytes(MutableBox(body))
    } else {
      return .false
    }
  }
  
  private func httpRequestContentSet(expr: Expr, body: Expr, args: Arguments) throws -> Expr {
    try self.request(from: expr).body =
      try BytevectorLibrary.subVector("http-request-content-set!", body, args)
    return .void
  }
  
  private func httpRequestTimeout(expr: Expr) throws -> Expr {
    if let timeout = try self.request(from: expr).timeoutIntervalForRequest {
      return .makeNumber(timeout)
    } else {
      return .false
    }
  }
  
  private func httpRequestTimeoutSet(expr: Expr, tout: Expr) throws -> Expr {
    try self.request(from: expr).timeoutIntervalForRequest = try tout.asDouble(coerce: true)
    return .void
  }
  
  private func httpRequestSendCookies(expr: Expr) throws -> Expr {
    if let cookies = try self.request(from: expr).httpShouldSetCookies {
      return .makeBoolean(cookies)
    } else {
      return .null
    }
  }
  
  private func httpRequestCachePolicy(expr: Expr) throws -> Expr {
    if let cache = try self.request(from: expr).requestCachePolicy {
      return .symbol(self.context.symbols.intern(HTTPLibrary.string(forCachePolicy: cache)))
    } else {
      return .null
    }
  }
  
  private func httpRequestUsePipelining(expr: Expr) throws -> Expr {
    if let pipelining = try self.request(from: expr).httpShouldUsePipelining {
      return .makeBoolean(pipelining)
    } else {
      return .null
    }
  }
  
  private func httpRequestAllowCellular(expr: Expr) throws -> Expr {
    if let cellular = try self.request(from: expr).allowsCellularAccess {
      return .makeBoolean(cellular)
    } else {
      return .null
    }
  }
  
  // HTTP Response
  
  private func isHttpResponse(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is HTTPResponse else {
      return .false
    }
    return .true
  }
  
  private func httpResponseContent(expr: Expr) throws -> Expr {
    return try self.response(from: expr).body
  }
  
  private func httpResponseStatusCode(expr: Expr) throws -> Expr {
    return .makeNumber(try self.response(from: expr).response.statusCode)
  }
  
  private func httpResponseMimeType(expr: Expr) throws -> Expr {
    guard let mimeType = try self.response(from: expr).response.mimeType else {
      return .false
    }
    return .makeString(mimeType)
  }
  
  private func httpResponseEncoding(expr: Expr) throws -> Expr {
    guard let encoding = try self.response(from: expr).response.textEncodingName else {
      return .false
    }
    return .makeString(encoding)
  }
  
  private func httpResponseUrl(expr: Expr) throws -> Expr {
    guard let url = try self.response(from: expr).response.url else {
      return .false
    }
    return .makeString(url.absoluteString)
  }
  
  private func decodeHeaderValue(_ value: Any) -> Expr? {
    if let str = value as? String {
      return .makeString(str)
    } else if let num = value as? Int {
      return .makeNumber(num)
    } else if let bool = value as? Bool {
      return .makeBoolean(bool)
    } else if let real = value as? Double {
      return .makeNumber(real)
    } else {
      return nil
    }
  }
  
  private func decodeHeaderValueAsStr(_ value: Any) -> Expr? {
    if let str = value as? String {
      return .makeString(str)
    } else if let num = value as? Int {
      return .makeString(num.description)
    } else if let bool = value as? Bool {
      return .makeString(bool ? "?1" : "?0")
    } else if let real = value as? Double {
      return .makeString(real.description)
    } else {
      return nil
    }
  }
  
  private func httpResponseHeaders(expr: Expr, asString: Expr?) throws -> Expr {
    let strval = asString?.isTrue ?? false
    let headers = try self.response(from: expr).response.allHeaderFields
    var res = Expr.null
    for (key, value) in headers {
      if let keystr = key as? String,
         let valexpr = strval ? self.decodeHeaderValueAsStr(value) : self.decodeHeaderValue(value) {
        res = .pair(.pair(.makeString(keystr), valexpr), res)
      }
    }
    return res
  }
  
  private func httpResponseHeader(expr: Expr, key: Expr, asString: Expr?) throws -> Expr {
    let strval = asString?.isTrue ?? false
    guard let value = try self.response(from: expr).response.allHeaderFields[key.asString()],
          let res = strval ? self.decodeHeaderValueAsStr(value) : self.decodeHeaderValue(value) else {
      return .false
    }
    return res
  }
  
  private func httpStatusCodeToString(expr: Expr) throws -> Expr {
    return .makeString(HTTPURLResponse.localizedString(
                         forStatusCode: try expr.asInt(above: 0, below: 1000)))
  }
}

public final class HTTPRequest: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "http-request"))
  
  public let url: URL
  public let method: String
  public var headers: [String : String]
  public var body: [UInt8]? = nil
  public var timeoutIntervalForRequest: TimeInterval? = nil
  public var httpShouldSetCookies: Bool? = nil
  public var requestCachePolicy: URLRequest.CachePolicy? = nil
  public var httpShouldUsePipelining: Bool? = nil
  public var allowsCellularAccess: Bool? = nil
  
  public init(url: URL, method: String, args: Arguments) throws {
    switch method {
      case "GET", "HEAD", "POST", "PUT", "DELETE", "CONNECT", "OPTIONS", "TRACE", "PATCH":
        break
      default:
        throw RuntimeError.eval(.unsupportedHttpMethod, .makeString(method))
    }
    var iter = args.makeIterator()
    self.url = url
    self.method = method
    if let hdrs = iter.next(), hdrs != .false {
      self.headers = try HTTPLibrary.headers(from: hdrs)
    } else {
      self.headers = [:]
    }
    if let tout = iter.next(), tout != .false && tout != .null {
      self.timeoutIntervalForRequest = try tout.asDouble(coerce: true)
    }
    if let ckies = iter.next(), ckies != .null {
      self.httpShouldSetCookies = ckies.isTrue
    }
    if let cche = iter.next(), cche != .false && cche != .null {
      self.requestCachePolicy = try HTTPLibrary.cachePolicy(from: cche)
    }
    if let pipelining = iter.next(), pipelining != .null {
      self.httpShouldUsePipelining = pipelining.isTrue
    }
    if let cellular = iter.next(), cellular != .null {
      self.allowsCellularAccess = cellular.isTrue
    }
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    return "#<\(self.tagString)>"
  }
  
  public override var tagString: String {
    var res = "\(Self.type) \(self.identityString): url=\"\(self.url.absoluteString)\", method=\(self.method)"
    if self.headers.count > 0 {
      res += ", headers=\(headers.count)"
    }
    if let data = self.body {
      res += ", content=\(data.count)"
    }
    if let timeout = self.timeoutIntervalForRequest {
      res += ", timeout=\(timeout)"
    }
    if let cookies = self.httpShouldSetCookies {
      res += ", cookies=\(cookies ? "#t" : "#f")"
    }
    if let cache = self.requestCachePolicy {
      res += ", cache=\(HTTPLibrary.string(forCachePolicy: cache))"
    }
    if let pipelining = self.httpShouldUsePipelining {
      res += ", pipelining=\(pipelining ? "#t" : "#f")"
    }
    if let cellular = self.allowsCellularAccess {
      res += ", cellular=\(cellular ? "#t" : "#f")"
    }
    return res
  }
  
  public override func unpack(in context: Context) -> Exprs {
    let content: Expr = self.body == nil ? .false : .makeNumber(self.body!.count)
    let timeout: Expr = self.timeoutIntervalForRequest == nil ? .false : .makeNumber(self.timeoutIntervalForRequest!)
    let cookies: Expr = self.httpShouldSetCookies == nil ? .null : .makeBoolean(self.httpShouldSetCookies!)
    let cache: Expr = self.requestCachePolicy == nil ? . false : .makeString(HTTPLibrary.string(forCachePolicy: self.requestCachePolicy!))
    let pipelining: Expr = self.httpShouldUsePipelining == nil ? .null : .makeBoolean(self.httpShouldUsePipelining!)
    let cellular: Expr = self.allowsCellularAccess == nil ? .null : .makeBoolean(self.allowsCellularAccess!)
    return [.makeString(self.identityString),
            .makeString(self.url.absoluteString),
            .makeString(self.method),
            .makeNumber(self.headers.count),
            content,
            timeout,
            cookies,
            cache,
            pipelining,
            cellular]
  }
  
  public func copy() throws -> HTTPRequest {
    let res = try HTTPRequest(url: self.url, method: self.method, args: [])
    res.headers = self.headers
    res.body = self.body
    res.timeoutIntervalForRequest = self.timeoutIntervalForRequest
    res.httpShouldSetCookies = self.httpShouldSetCookies
    res.requestCachePolicy = self.requestCachePolicy
    res.httpShouldUsePipelining = self.httpShouldUsePipelining
    res.allowsCellularAccess = self.allowsCellularAccess
    return res
  }
}

public final class HTTPResponse: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "http-response"))

  public let response: HTTPURLResponse
  public let body: Expr
  
  public init(response: HTTPURLResponse, body: Data) {
    var data = [UInt8](repeating: 0, count: body.count)
    (body as NSData).getBytes(&data, length: body.count)
    self.body = .bytes(MutableBox(data))
    self.response = response
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    return "#<\(self.tagString)>"
  }
  
  public override var tagString: String {
    var res = "\(Self.type): status=\(self.response.statusCode)"
    if let mimeType = self.response.mimeType {
      res += ", mime=\(mimeType)"
    }
    if let encoding = self.response.textEncodingName {
      res += ", encoding=\(encoding)"
    }
    if self.response.allHeaderFields.count > 0 {
      res += ", headers=\(self.response.allHeaderFields.count)"
    }
    res += ", size=\(self.response.expectedContentLength)"
    return res
  }
  
  public override func unpack(in context: Context) -> Exprs {
    let mime: Expr
    if let mimeType = self.response.mimeType {
      mime = .makeString(mimeType)
    } else {
      mime = .false
    }
    let encoding: Expr
    if let enc = self.response.textEncodingName {
      encoding = .makeString(enc)
    } else {
      encoding = .false
    }
    return [.makeString(self.identityString),
            .makeNumber(self.response.statusCode),
            mime,
            encoding,
            .makeNumber(self.response.allHeaderFields.count),
            .makeNumber(self.response.expectedContentLength)]
  }
}

public final class HTTPSession: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "http-session"))

  public let session: URLSession
  
  public init(session: URLSession) {
    self.session = session
  }
  
  public convenience init(_ httpSession: HTTPSession) {
    self.init(session: URLSession(configuration: httpSession.configuration))
  }
  
  public init(ephemeral: Bool = false,
              cellularAccess: Bool = true,
              requestTimeout: TimeInterval = 60.0,
              sendCookies: Bool = true,
              cachePolicy: URLRequest.CachePolicy = .useProtocolCachePolicy,
              maxConnectionsPerHost: Int = 6,
              usePipelining: Bool = false) {
    let config: URLSessionConfiguration = ephemeral ? .ephemeral : .default
    config.allowsCellularAccess = cellularAccess
    config.timeoutIntervalForRequest = requestTimeout
    config.httpShouldSetCookies = sendCookies
    config.requestCachePolicy = cachePolicy
    config.httpMaximumConnectionsPerHost = maxConnectionsPerHost
    config.httpShouldUsePipelining = usePipelining
    self.session = URLSession(configuration: config)
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    return "#<\(self.tagString)>"
  }
  
  public override var tagString: String {
    return "\(Self.type) \(self.identityString): " +
           "timeout=\(self.session.configuration.timeoutIntervalForRequest), " +
           "cookies=\(self.session.configuration.httpShouldSetCookies ? "#t" : "#f"), " +
           "cache=\(HTTPLibrary.string(forCachePolicy: self.session.configuration.requestCachePolicy)), " +
           "maxconnect=\(self.session.configuration.httpMaximumConnectionsPerHost), " +
           "pipelining=\(self.session.configuration.httpShouldUsePipelining ? "#t" : "#f"), " +
           "cellular=\(self.session.configuration.allowsCellularAccess ? "#t" : "#f")"
  }
  
  public var configuration: URLSessionConfiguration {
    return self.session.configuration
  }
  
  public func copy() -> HTTPSession {
    return HTTPSession(session: self.session.mutableCopy() as! URLSession)
  }
  
  public override func unpack(in context: Context) -> Exprs {
    return [.makeString(self.identityString),
            .makeNumber(self.session.configuration.timeoutIntervalForRequest),
            .makeBoolean(self.session.configuration.httpShouldSetCookies),
            .makeString(HTTPLibrary.string(forCachePolicy: self.session.configuration.requestCachePolicy)),
            .makeNumber(self.session.configuration.httpMaximumConnectionsPerHost),
            .makeBoolean(self.session.configuration.httpShouldUsePipelining),
            .makeBoolean(self.session.configuration.allowsCellularAccess)]
  }
  
  public func request(from req: HTTPRequest) -> URLRequest {
    var request = URLRequest(url: req.url)
    request.httpMethod = req.method
    for (key, value) in req.headers {
      request.setValue(value, forHTTPHeaderField: key)
    }
    if let data = req.body {
      request.httpBody = Data(data)
    }
    request.timeoutInterval = req.timeoutIntervalForRequest ??
                              self.session.configuration.timeoutIntervalForRequest
    request.httpShouldHandleCookies = req.httpShouldSetCookies ??
                                      self.session.configuration.httpShouldSetCookies
    request.cachePolicy = req.requestCachePolicy ??
                          self.session.configuration.requestCachePolicy
    request.httpShouldUsePipelining = req.httpShouldUsePipelining ??
                                      self.session.configuration.httpShouldUsePipelining
    request.allowsCellularAccess = req.allowsCellularAccess ??
                                   self.session.configuration.allowsCellularAccess
    return request
  }
  
  public func dataTask(with request: URLRequest,
                       completionHandler: @escaping (Data?, URLResponse?, (any Error)?) -> Void)
                -> URLSessionDataTask {
    return self.session.dataTask(with: request, completionHandler: completionHandler)
  }
  
  public func uploadTask(with request: URLRequest,
                         from data: Data? = nil,
                         completionHandler: @escaping (Data?, URLResponse?, (any Error)?) -> Void)
                -> URLSessionUploadTask {
    return self.session.uploadTask(with: request, from: data, completionHandler: completionHandler)
  }
}

