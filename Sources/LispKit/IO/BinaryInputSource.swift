//
//  BinaryInputSource.swift
//  LispKit
//
//  Created by Matthias Zenger on 01/11/2017.
//  Copyright © 2017 ObjectHub. All rights reserved.
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

public protocol BinaryInputSource {
  init?(url: URL)
  func open()
  func close()
  var hasBytesAvailable: Bool { get }
  func read(_ buffer: UnsafeMutablePointer<UInt8>, maxLength len: Int) -> Int
}

extension InputStream: BinaryInputSource {
}

public final class HTTPInputStream: BinaryInputSource {

  /// A condition object used to synchronize changes between tasks and the input stream object
  fileprivate let condition = NSCondition()

  /// The target URL of the request
  public let url: URL

  /// The network task associated with this input stream
  private var task: URLSessionTask? = nil

  /// The HTTP response header object once it was received
  fileprivate var response: HTTPURLResponse? = nil

  /// Data of the body of the HTTP response
  public fileprivate(set) var data: Data

  public private(set) var readIndex: Data.Index

  /// Number of bytes that were received from the HTTP GET request
  public fileprivate(set) var bytesReceived: Int64 = 0

  /// If an error was encountered, it is being stored here
  public fileprivate(set) var error: Error? = nil

  /// Returns `false` as long as this input stream is connected to a network task.
  public fileprivate(set) var completed: Bool = false

  private static let sessionDelegate = SessionDelegate()

  /// The default request timeout
  public static let defaultTimeout: Double = 60.0

  public private(set) static var session: URLSession = {
    let configuration = URLSessionConfiguration.default
    configuration.requestCachePolicy = .useProtocolCachePolicy
    configuration.timeoutIntervalForRequest = TimeInterval(HTTPInputStream.defaultTimeout)
    return URLSession(configuration: configuration,
                      delegate: HTTPInputStream.sessionDelegate,
                      delegateQueue: nil)
  }()

  public init?(url: URL) {
    guard let scheme = url.scheme, scheme == "http" || scheme == "https" else {
      return nil
    }
    self.url = url
    self.data = Data()
    self.readIndex = self.data.startIndex
  }

  public func open() {
    self.open(timeout: HTTPInputStream.defaultTimeout)
  }

  public func open(timeout: Double) {
    guard self.task == nil && !self.completed else {
      return
    }
    var request = URLRequest(url: self.url)
    request.httpMethod = "GET"
    request.cachePolicy = .useProtocolCachePolicy
    request.timeoutInterval = TimeInterval(timeout)
    request.httpShouldHandleCookies = false
    let task = HTTPInputStream.session.dataTask(with: request)
    HTTPInputStream.sessionDelegate.registerTask(task, forStream: self)
    self.task = task
    task.resume()
  }

  public func close() {
    if !self.completed {
      if let task = self.task {
        task.cancel()
        self.task = nil
      } else {
        self.completed = true
      }
    }
  }

  public func read(_ buffer: UnsafeMutablePointer<UInt8>, maxLength len: Int) -> Int {
    self.condition.lock()
    while !self.completed && self.readIndex == self.data.endIndex {
      self.condition.wait()
    }
    let bytesRead = min(len, self.data.endIndex - self.readIndex)
    let nextReadIndex = self.data.index(self.readIndex, offsetBy: bytesRead)
    self.data.copyBytes(to: buffer, from: self.readIndex ..< nextReadIndex)
    self.readIndex = nextReadIndex
    self.condition.unlock()
    return bytesRead
  }

  public var hasBytesAvailable: Bool {
    return !self.completed || self.readIndex < self.data.endIndex
  }

  /// Waits until a response was received (or the request was terminated)
  public func waitForResponse() {
    self.condition.lock()
    while self.response == nil && !self.completed {
      self.condition.wait()
    }
    self.condition.unlock()
  }

  /// Waits until all data was received (or the request was terminated)
  public func waitForData() {
    self.condition.lock()
    while !self.completed {
      self.condition.wait()
    }
    self.condition.unlock()
  }

  /// The status code of the HTTP response
  public var statusCode: Int? {
    return self.response?.statusCode
  }

  /// A textual description of the status code of the HTTP response
  public var statusCodeDescription: String? {
    guard let statusCode = self.statusCode else {
      return nil
    }
    return HTTPURLResponse.localizedString(forStatusCode: statusCode)
  }

  /// The number of bytes that are supposed to be read
  public var expectedContentLength: Int64? {
    return self.response?.expectedContentLength
  }

  /// The name of the text encoding.
  public var textEncodingName: String? {
    return self.response?.textEncodingName
  }

  /// The mime type name.
  public var mimeType: String? {
    return self.response?.mimeType
  }

  /// The URL of the response.
  public var responseUrl: URL? {
    return self.response?.url
  }

  /// All header fields of the HTTP response.
  public var headerFields: [String : String]? {
    guard let response = self.response else {
      return nil
    }
    var res: [String : String] = [:]
    for (key, value) in response.allHeaderFields {
      if let str = value as? String {
        res[key.description] = str
      }
    }
    return res
  }
}

fileprivate class SessionDelegate: NSObject,
                                   URLSessionDelegate,
                                   URLSessionTaskDelegate,
                                   URLSessionDataDelegate {

  private var inputStreamForTask: [Int : HTTPInputStream] = [:]

  public func registerTask(_ task: URLSessionTask, forStream stream: HTTPInputStream) {
    self.inputStreamForTask[task.taskIdentifier] = stream
  }

  /// This callback is the last one done for each task. Mark the
  public func urlSession(_ session: URLSession,
                         task: URLSessionTask,
                         didCompleteWithError error: Error?) {
    guard let inputStream = self.inputStreamForTask[task.taskIdentifier] else {
      return
    }
    inputStream.condition.lock()
    inputStream.error = inputStream.error ?? error
    inputStream.completed = true
    self.inputStreamForTask.removeValue(forKey: task.taskIdentifier)
    inputStream.condition.signal()
    inputStream.condition.unlock()
  }

  public func urlSession(_ session: URLSession,
                         dataTask task: URLSessionDataTask,
                         didReceive response: URLResponse,
                         completionHandler: @escaping (URLSession.ResponseDisposition) -> Void) {
    guard let inputStream = self.inputStreamForTask[task.taskIdentifier] else {
      completionHandler(.allow)
      return
    }
    inputStream.condition.lock()
    if let response = response as? HTTPURLResponse {
      inputStream.response = response
      inputStream.condition.signal()
    }
    inputStream.condition.unlock()
    completionHandler(.allow)
  }

  public func urlSession(_ session: URLSession,
                         dataTask task: URLSessionDataTask,
                         didReceive data: Data) {
    guard let inputStream = self.inputStreamForTask[task.taskIdentifier] else {
      return
    }
    inputStream.data.append(data)
    inputStream.bytesReceived += Int64(data.count)
  }
}
