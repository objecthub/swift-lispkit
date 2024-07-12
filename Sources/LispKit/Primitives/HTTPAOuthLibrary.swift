//
//  HTTPAOuthLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 03/07/2024.
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
import OAuth2


public final class HTTPOAuthLibrary: NativeLibrary {
  
  /// Manager of ongoing authentication request flows
  public static var authRequestManager = AuthenticatedRequestManager()
  
  /// Factory for creating embedded auth configurators
  public static var libraryConfig: HTTPOAuthConfig.Type = LispKitHTTPOAuthConfig.self
  
  // The configurator for customizable parts of the library
  public let configuration: HTTPOAuthConfig
  
  // Flow identifiers
  private let codeGrant: Symbol
  private let codeGrantBasicAuth: Symbol
  private let codeGrantNoTokenType: Symbol
  private let codeGrantAzure: Symbol
  private let codeGrantFacebook: Symbol
  private let codeGrantLinkedIn: Symbol
  private let implicitGrant: Symbol
  private let implicitGrantQueryParams: Symbol
  private let clientCredentials: Symbol
  private let clientCredentialsReddit: Symbol
  private let passwordGrant: Symbol
  private let deviceGrant: Symbol
  private let userCode: Symbol
  private let verificationUrl: Symbol
  private let verificationUrlComplete: Symbol
  private let expiresIn: Symbol
  private let interval: Symbol
  private let deviceCode: Symbol
  
  /// Initialize symbols.
  public required init(in context: Context) throws {
    self.configuration = HTTPOAuthLibrary.libraryConfig.init(context: context)
    self.codeGrant = context.symbols.intern("code-grant")
    self.codeGrantBasicAuth = context.symbols.intern("code-grant-basic-auth")
    self.codeGrantNoTokenType = context.symbols.intern("code-grant-no-token-type")
    self.codeGrantAzure = context.symbols.intern("code-grant-azure")
    self.codeGrantFacebook = context.symbols.intern("code-grant-facebook")
    self.codeGrantLinkedIn = context.symbols.intern("code-grant-linkedin")
    self.implicitGrant = context.symbols.intern("implicit-grant")
    self.implicitGrantQueryParams = context.symbols.intern("implicit-grant-query-params")
    self.clientCredentials = context.symbols.intern("client-credentials")
    self.clientCredentialsReddit = context.symbols.intern("client-credentials-reddit")
    self.passwordGrant = context.symbols.intern("password-grant")
    self.deviceGrant = context.symbols.intern("device-grant")
    self.userCode = context.symbols.intern("user-code")
    self.verificationUrl = context.symbols.intern("verification-url")
    self.verificationUrlComplete = context.symbols.intern("verification-url-complete")
    self.expiresIn = context.symbols.intern("expires-in")
    self.interval = context.symbols.intern("interval")
    self.deviceCode = context.symbols.intern("device-code")
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "http", "oauth"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define("oauth2-type-tag", as: OAuth2Protocol.type.objectTypeTag())
    self.define("oauth2-session-type-tag", as: OAuth2Session.type.objectTypeTag())
    self.define(Procedure("oauth2?", self.isOAuth2))
    self.define(Procedure("make-oauth2", self.makeOAuth2))
    self.define(Procedure("oauth2-flow", self.oauth2Flow))
    self.define(Procedure("oauth2-settings", self.oauth2Settings))
    self.define(Procedure("oauth2-setting", self.oauth2Setting))
    self.define(Procedure("oauth2-unexpired-access-token?", self.oauth2UnexpiredAccessToken))
    self.define(Procedure("oauth2-access-token", self.oauth2AccessToken))
    self.define(Procedure("oauth2-refresh-token", self.oauth2RefreshToken))
    self.define(Procedure("oauth2-forget-tokens!", self.oauth2ForgetTokens))
    self.define(Procedure("oauth2-cancel-requests!", self.oauth2ForgetTokens))
    self.define(Procedure("oauth2-request-codes", self.oauth2RequestCodes))
    self.define(Procedure("oauth2-authorize!", self.oauth2Authorize))
    self.define(Procedure("http-request-sign!", self.httpRequestSign))
    self.define(Procedure("oauth2-session?", self.isOAuth2Session))
    self.define(Procedure("make-oauth2-session", self.makeOAuth2Session))
    self.define(Procedure("oauth2-session-oauth2", self.oauth2SessionOAuth2))
    self.define(Procedure("oauth2-session-http-session", self.oauth2SessionHttpSession))
    self.define(Procedure("oauth2-session-send", self.oauth2SessionSend))
  }
  
  /// Initializations of the library.
  public override func initializations() {
  }
  
  private func oauth2(from: Expr) throws -> OAuth2Protocol {
    guard case .object(let obj) = from, let res = obj as? OAuth2Protocol else {
      throw RuntimeError.type(from, expected: [OAuth2Protocol.type])
    }
    return res
  }
  
  private func oauth2Session(from: Expr) throws -> OAuth2Session {
    guard case .object(let obj) = from, let res = obj as? OAuth2Session else {
      throw RuntimeError.type(from, expected: [OAuth2Session.type])
    }
    return res
  }
  
  private func request(from: Expr) throws -> HTTPRequest {
    guard case .object(let obj) = from, let request = obj as? HTTPRequest else {
      throw RuntimeError.type(from, expected: [HTTPRequest.type])
    }
    return request
  }
  
  private func isOAuth2(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is OAuth2Protocol else {
      return .false
    }
    return .true
  }
  
  private func makeOAuth2(flow: Expr, settngs: Expr) throws -> Expr {
    let flow = try flow.asSymbol()
    let settings = try self.oauth2Settings(from: settngs)
    let oauth2: OAuth2
    switch flow {
      case self.codeGrant:
        oauth2 = OAuth2CodeGrant(settings: settings)
      case self.codeGrantBasicAuth:
        oauth2 = OAuth2CodeGrantBasicAuth(settings: settings)
      case self.codeGrantNoTokenType:
        oauth2 = OAuth2CodeGrantNoTokenType(settings: settings)
      case self.codeGrantAzure:
        if let resource = settings["resource"] as? String {
          oauth2 = OAuth2CodeGrantAzure(settings: settings, resource: resource)
        } else {
          throw RuntimeError.custom("error", "OAuth2 flow 'code-grant-azure' requires 'resource' setting", [settngs])
        }
      case self.codeGrantFacebook:
        oauth2 = OAuth2CodeGrantFacebook(settings: settings)
      case self.codeGrantLinkedIn:
        oauth2 = OAuth2CodeGrantLinkedIn(settings: settings)
      case self.implicitGrant:
        oauth2 = OAuth2ImplicitGrant(settings: settings)
      case self.implicitGrantQueryParams:
        oauth2 = OAuth2ImplicitGrantWithQueryParams(settings: settings)
      case self.clientCredentials:
        oauth2 = OAuth2ClientCredentials(settings: settings)
      case self.clientCredentialsReddit:
        oauth2 = OAuth2ClientCredentialsReddit(settings: settings)
      case self.passwordGrant:
        oauth2 = OAuth2PasswordGrant(settings: settings)
      case self.deviceGrant:
        oauth2 = OAuth2DeviceGrantLK(settings: settings)
      default:
        throw RuntimeError.custom("error", "unknown flow identifier", [.symbol(flow)])
    }
    if let value = settings["auth_embedded"], (value as? Bool) ?? false {
      self.configuration.configureEmbeddedAuth(oauth2: oauth2)
    }
    if let level = settings["log"] as? Int64 {
      let minLevel: OAuth2LogLevel
      switch level {
        case 0: // debug
          minLevel = .trace
        case 1: // info
          minLevel = .debug
        case 2: // warn
          minLevel = .warn
        default: // switch logging off (5)
          minLevel = .off
      }
      oauth2.logger = self.configuration.createLogger(level: minLevel)
    }
    return .object(OAuth2Protocol(flow: flow, oauth2: oauth2))
  }
  
  private func oauth2Flow(expr: Expr) throws -> Expr {
    return .symbol(try self.oauth2(from: expr).flow)
  }
  
  private func oauth2Settings(expr: Expr) throws -> Expr {
    return self.oauth2SettingsAsExpr(from: try self.oauth2(from: expr).oauth2)
  }
  
  private func oauth2Setting(expr: Expr, key: Expr) throws -> Expr {
    let oauth2 = try self.oauth2(from: expr).oauth2
    let config = oauth2.clientConfig
    let name: String
    switch key {
      case .symbol(let sym):
        name = sym.identifier
      case .string(let str):
        name = str as String
      default:
        throw RuntimeError.type(key, expected: [.symbolType, .strType])
    }
    func make(_ str: String?) -> Expr {
      return str == nil ? .false : .makeString(str!)
    }
    func make(_ bool: Bool) -> Expr {
      return .makeBoolean(bool)
    }
    func make(_ num: Int) -> Expr {
      return .makeNumber(num)
    }
    func make(_ strs: [String]?) -> Expr {
      return strs == nil ? .false : .vector(Collection(kind: .immutableVector,
                                                       exprs: Exprs(strs!.map { .makeString($0) })))
    }
    func make(_ dict: [String : String]?) -> Expr {
      if let dict {
        var res: Expr = .null
        for (key, value) in dict {
          res = .pair(.pair(.makeString(key), .makeString(value)), res)
        }
        return res
      } else {
        return .false
      }
    }
    switch name {
      case "client_id":
        return make(config.clientId)
      case "client_secret":
        return make(config.clientSecret)
      case "authorize_uri":
        return make(config.authorizeURL.absoluteString)
      case "token_uri":
        return make(config.tokenURL?.absoluteString)
      case "refresh_uri":
        return make(config.refreshURL?.absoluteString)
      case "redirect_uris":
        return make(config.redirectURLs)
      case "scope":
        return make(config.scope)
      case "custom_user_agent":
        return make(config.customUserAgent)
      case "client_name":
        return make(config.clientName)
      case "registration_uri":
        return make(config.registrationURL?.absoluteString)
      case "logo_uri":
        return make(config.logoURL?.absoluteString)
      case "keychain":
        return make(oauth2.useKeychain)
      case "keychain_access_mode":
        return make(oauth2.keychainAccessMode as String)
      case "keychain_access_group":
        return make(oauth2.keychainAccessGroup)
      case "keychain_account_for_client_credentials":
        return make(oauth2.keychainAccountForClientCredentials)
      case "keychain_account_for_tokens":
        return make(oauth2.keychainAccountForTokens)
      case "secret_in_body":
        return make(config.secretInBody)
      case "parameters":
        return make(config.customParameters)
      case "token_assume_unexpired":
        return make(config.accessTokenAssumeUnexpired)
      case "use_pkce":
        return make(config.useProofKeyForCodeExchange)
      case "log":
        if let logger = oauth2.logger {
          return make(logger.level.rawValue >= 3 ? 5 : logger.level.rawValue)
        }
        return .false
      case "auth_embedded":
        return make(oauth2.authConfig.authorizeEmbedded)
      case "username":
        if let pwgrant = oauth2 as? OAuth2PasswordGrant {
          return make(pwgrant.username)
        }
        return .false
      case "password":
        if let pwgrant = oauth2 as? OAuth2PasswordGrant {
          return make(pwgrant.password)
        }
        return .false
      case "device_id":
        if let reddit = oauth2 as? OAuth2ClientCredentialsReddit {
          return make(reddit.deviceId)
        }
        return .false
      default:
        throw RuntimeError.custom("error", "unknown oauth2 setting: ", [key])
    }
  }
  
  private func oauth2UnexpiredAccessToken(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.oauth2(from: expr).oauth2.hasUnexpiredAccessToken())
  }
  
  /*
   - client_id (String)
   - client_secret (String), usually only needed for code grant
   - authorize_uri (URL-String)
   - token_uri (URL-String), if omitted the authorize_uri will be used to obtain tokens
   - refresh_uri (URL-String), if omitted the token_uri will be used to obtain tokens
   - redirect_uris (Array of URL-Strings)
   - scope (String)
   - custom_user_agent (String)
   
   - client_name (String)
   - registration_uri (URL-String)
   - logo_uri (URL-String)
   
   - keychain (Bool, true by default, applies to using the system keychain)
   - keychain_access_mode (String, value for keychain kSecAttrAccessible attribute, kSecAttrAccessibleWhenUnlocked by default)
   - keychain_access_group (String, value for keychain kSecAttrAccessGroup attribute, nil by default)
   - keychain_account_for_client_credentials(String, "clientCredentials" by default)
   - keychain_account_for_tokens(String, "currentTokens" by default)
   - secret_in_body (Bool, false by default, forces the flow to use the request body for the client secret)
   - parameters ([String: String], custom request parameters to be added during authorization)
   - token_assume_unexpired (Bool, true by default, whether to use access tokens that do not come with an "expires_in" parameter)
   - use_pkce (Bool, false by default)
   
   - auth_embedded (Bool, false by default): Use embedded authorization mode
   - username (String): Used by password grant flows
   - password (String): Used by password grant flows
   - resource (String): For code-grant-azure
   - basic (String): For code-grant-basic-auth
   - device_id (String): For client-credentials-reddit
   */
  private func oauth2SettingsAsExpr(from oauth2: OAuth2) -> Expr {
    var settings: Exprs = []
    func add(_ key: String, _ value: String?) {
      if let value {
        settings.append(.pair(.symbol(self.context.symbols.intern(key)), .makeString(value)))
      }
    }
    func add(_ key: String, _ value: Bool) {
      settings.append(.pair(.symbol(self.context.symbols.intern(key)), value ? .true : .false))
    }
    func add(_ key: String, _ value: Int) {
      settings.append(.pair(.symbol(self.context.symbols.intern(key)), .makeNumber(value)))
    }
    func add(_ key: String, _ arr: [String]?) {
      if let arr {
        settings.append(.pair(.symbol(self.context.symbols.intern(key)),
                              .vector(Collection(kind: .immutableVector,
                                                 exprs: Exprs(arr.map { .makeString($0) })))))
      }
    }
    func add(_ key: String, _ dict: [String : String]?) {
      if let dict {
        var res: Expr = .null
        for (key, value) in dict {
          res = .pair(.pair(.makeString(key), .makeString(value)), res)
        }
        settings.append(.pair(.symbol(self.context.symbols.intern(key)), res))
      }
    }
    let config = oauth2.clientConfig
    add("client_id", config.clientId)
    add("client_secret", config.clientSecret)
    add("authorize_uri", config.authorizeURL.absoluteString)
    add("token_uri", config.tokenURL?.absoluteString)
    add("refresh_uri", config.refreshURL?.absoluteString)
    add("redirect_uris", config.redirectURLs)
    add("scope", config.scope)
    add("custom_user_agent", config.customUserAgent)
    add("client_name", config.clientName)
    add("registration_uri", config.registrationURL?.absoluteString)
    add("logo_uri", config.logoURL?.absoluteString)
    add("keychain", oauth2.useKeychain)
    add("keychain_access_mode", oauth2.keychainAccessMode as String)
    add("keychain_access_group", oauth2.keychainAccessGroup)
    add("keychain_account_for_client_credentials", oauth2.keychainAccountForClientCredentials)
    add("keychain_account_for_tokens", oauth2.keychainAccountForTokens)
    add("secret_in_body", config.secretInBody)
    add("parameters", config.customParameters)
    add("token_assume_unexpired", config.accessTokenAssumeUnexpired)
    add("use_pkce", config.useProofKeyForCodeExchange)
    if let logger = oauth2.logger {
      add("log", logger.level.rawValue >= 3 ? 5 : logger.level.rawValue)
    }
    add("auth_embedded", oauth2.authConfig.authorizeEmbedded)
    if let pwgrant = oauth2 as? OAuth2PasswordGrant {
      add("username", pwgrant.username)
      add("password", pwgrant.password)
    }
    if let reddit = oauth2 as? OAuth2ClientCredentialsReddit {
      add("device_id", reddit.deviceId)
    }
    return .makeList(settings)
  }
  
  private func oauth2Settings(from expr: Expr) throws -> OAuth2JSON {
    var settings: OAuth2JSON = [:]
    var list = expr
    while case .pair(.pair(let str, let value), let rest) = list {
      let key: String
      switch str {
        case .string(let s):
          key = s as String
        case .symbol(let s):
          key = s.identifier
        default:
          throw RuntimeError.custom("error",
                                    "invalid oauth2 settings key (needs to be a symbol or string): $0",
                                    [str])
      }
      switch value {
        case .null:
          settings[key] = nil
        case .false:
          settings[key] = false
        case .true:
          settings[key] = true
        case .fixnum(let num):
          settings[key] = num
        case .flonum(let num):
          settings[key] = num
        case .string(let str):
          settings[key] = str as String
        case .vector(let col):
          switch col.kind {
            case .array, .vector, .immutableVector, .growableVector:
              var arr: [String] = []
              for expr in col.exprs {
                arr.append(try expr.asString())
              }
              settings[key] = arr
            default:
              throw RuntimeError.custom("error", "invalid oauth2 settings value: $0", [value])
          }
        case .pair(_, _):
          var rs = value
          var dict: [String : String] = [:]
          while case .pair(.pair(let key, let value), let rest) = rs {
            switch key {
              case .symbol(let member):
                dict[member.identifier] = try value.asString()
              case .string(let member):
                dict[member as String] = try value.asString()
              default:
                throw RuntimeError.custom(
                        "error",
                        "invalid oauth2 settings dictionary key (needs to be a symbol or string): $0",
                        [key])
            }
            rs = rest
          }
          guard case .null = rs else {
            throw RuntimeError.custom("error",
                                      "invalid oauth2 settings dictionary data structure: $0",
                                      [rs])
          }
          settings[key] = dict
        default:
          throw RuntimeError.custom("error", "invalid oauth2 settings value: $0", [value])
      }
      list = rest
    }
    guard case .null = list else {
      throw RuntimeError.custom("error",
                                "invalid oauth2 settings dictionary data structure: $0",
                                [list])
    }
    if let level = settings["log"] as? Int64, level >= 0 || level < 3 {
      settings["verbose"] = true
    } else {
      settings["verbose"] = false
    }
    return settings
  }
  
  private func oauth2Params(from: Expr?) throws -> OAuth2StringDict? {
    guard var list = from else {
      return nil
    }
    var dict: OAuth2StringDict = [:]
    while case .pair(.pair(let key, let value), let rest) = list {
      dict[try key.asString()] = try value.asString()
      list = rest
    }
    guard case .null = list else {
      throw RuntimeError.type(from!, expected: [.properListType])
    }
    return dict
  }
  
  private func oauth2AccessToken(expr: Expr) throws -> Expr {
    if let token = try self.oauth2(from: expr).oauth2.accessToken {
      return .makeString(token)
    }
    return .false
  }
  
  private func oauth2RefreshToken(expr: Expr) throws -> Expr {
    if let token = try self.oauth2(from: expr).oauth2.refreshToken {
      return .makeString(token)
    }
    return .false
  }
  
  private func oauth2ForgetTokens(expr: Expr) throws -> Expr {
    try self.oauth2(from: expr).oauth2.forgetTokens()
    return .void
  }
  
  private func expr(from obj: Any) -> Expr? {
    if let str = obj as? String {
      return .makeString(str)
    } else if let num = obj as? Int {
      return .makeNumber(num)
    } else if let num = obj as? Double {
      return .makeNumber(num)
    } else if let bool = obj as? Bool {
      return .makeBoolean(bool)
    } else if let arr = obj as? [Any] {
      return .vector(Collection(kind: .immutableVector,
                                exprs: Exprs(arr.map { self.expr(from: $0) ?? .false })))
    } else if let dict = obj as? [String : Any] {
      var res = Expr.null
      for (key, obj) in dict {
        if let value = self.expr(from: obj) {
          res = .pair(.pair(.makeString(key), value), res)
        }
      }
      return res
    } else {
      return nil
    }
  }
  
  private func params(from params: OAuth2JSON?) -> Expr? {
    guard let params else {
      return nil
    }
    var res = Expr.null
    for (key, obj) in params {
      if let value = self.expr(from: obj) {
        res = .pair(.pair(.makeString(key), value), res)
      }
    }
    return res
  }
  
  private func oauth2RequestCodes(expr: Expr, nonTextual: Expr?, params: Expr?) throws -> Expr {
    let oauth2 = try self.oauth2(from: expr)
    guard let client = oauth2.oauth2 as? OAuth2DeviceGrantLK else {
      throw RuntimeError.custom("error", "expecting oauth2 client for the device-grant flow: ", [expr])
    }
    let params = params == nil ? [:] : try self.oauth2Params(from: params!)
    let f = Future(external: false)
    HTTPOAuthLibrary.authRequestManager.register(oauth2: client, result: f, in: self.context)
    client.start(useNonTextualTransmission: nonTextual?.isTrue ?? false, params: params, queue: nil) { codes, error in
      defer {
        HTTPOAuthLibrary.authRequestManager.unregister(future: f, in: self.context)
      }
      do {
        if let error {
          _ = try f.setResult(in: self.context, to: .error(RuntimeError.os(error)), raise: true)
        } else if let codes {
          var res = Expr.null
          res = .pair(.pair(.symbol(self.interval), .makeNumber(codes.interval)), res)
          res = .pair(.pair(.symbol(self.deviceCode), .makeString(codes.deviceCode)), res)
          if let url = codes.verificationUrlComplete {
            res = .pair(.pair(.symbol(self.verificationUrlComplete), .makeString(url.absoluteString)), res)
          }
          res = .pair(.pair(.symbol(self.verificationUrl), .makeString(codes.verificationUrl.absoluteString)), res)
          res = .pair(.pair(.symbol(self.expiresIn), .makeNumber(codes.expiresIn)), res)
          res = .pair(.pair(.symbol(self.userCode), .makeString(codes.userCode)), res)
          _ = try f.setResult(in: self.context, to: res, raise: false)
        } else {
          _ = try f.setResult(in: self.context,
                                   to: .error(RuntimeError.eval(.serverError)),
                                   raise: true)
        }
      } catch {
        do {
          _ = try f.setResult(in: self.context,
                                   to: .error(RuntimeError.eval(.serverError, .object(f))),
                                   raise: true)
        } catch {}
      }
    }
    return .object(f)
  }
  
  private func authorizeHandler(_ f: Future) -> (OAuth2JSON?, Error?) -> Void {
    return { params, error in
      defer {
        HTTPOAuthLibrary.authRequestManager.unregister(future: f, in: self.context)
      }
      do {
        if let error {
          _ = try f.setResult(in: self.context, to: .error(RuntimeError.os(error)), raise: true)
        } else if let params = self.params(from: params) {
          _ = try f.setResult(in: self.context, to: params, raise: false)
        } else {
          _ = try f.setResult(in: self.context,
                                   to: .error(RuntimeError.eval(.serverError)),
                                   raise: true)
        }
      } catch {
        do {
          _ = try f.setResult(in: self.context,
                                   to: .error(RuntimeError.eval(.serverError, .object(f))),
                                   raise: true)
        } catch {}
      }
    }
  }
  
  private func oauth2Authorize(expr: Expr) throws -> Expr {
    let oauth2 = try self.oauth2(from: expr)
    let f = Future(external: false)
    HTTPOAuthLibrary.authRequestManager.register(oauth2: oauth2.oauth2, result: f, in: self.context)
    if let oauth2DeviceGrant = oauth2.oauth2 as? OAuth2DeviceGrantLK {
      if oauth2DeviceGrant.hasUnexpiredAccessToken() {
        var params = Expr.null
        params = .pair(.pair(.makeString("token_type"), .makeString("bearer")), params)
        if let scope = oauth2DeviceGrant.scope {
          params = .pair(.pair(.makeString("scope"), .makeString(scope)), params)
        }
        if let accessToken = oauth2DeviceGrant.accessToken {
          params = .pair(.pair(.makeString("access_token"), .makeString(accessToken)), params)
        }
        _ = try f.setResult(in: self.context, to: params, raise: false)
      } else if let deviceCode = oauth2DeviceGrant.deviceCode {
        let callback = self.authorizeHandler(f)
        oauth2DeviceGrant.getDeviceAccessToken(deviceCode: deviceCode,
                                               interval: oauth2DeviceGrant.pollingInterval,
                                               queue: .global(qos: .default)) { params, error in
          if let params {
            oauth2DeviceGrant.didAuthorize(withParameters: params)
          } else if let error {
            oauth2DeviceGrant.didFail(with: error.asOAuth2Error)
          }
          callback(params, error)
        }
      } else {
        throw RuntimeError.custom("error", "OAuth2 device grant client did not yet receive device code: ", [expr])
      }
    } else {
      oauth2.oauth2.authorize(callback: self.authorizeHandler(f))
    }
    return .object(f)
  }
  
  private func oauth2CancelRequests(expr: Expr?, timeout: Expr?) throws -> Expr {
    if let expr {
      guard case .object(let obj) = expr else {
        throw RuntimeError.type(expr, expected: [OAuth2Protocol.type])
      }
      let timeout: TimeInterval = try timeout?.asDouble(coerce: true) ?? 0.0
      if let oauth2 = obj as? OAuth2Protocol {
        HTTPOAuthLibrary.authRequestManager.cancel(timeout: timeout,
                                                   oauth2: oauth2.oauth2,
                                                   in: self.context)
      } else if let future = obj as? Future {
        HTTPOAuthLibrary.authRequestManager.cancel(timeout: timeout,
                                                   future: future,
                                                   in: self.context)
      } else {
        throw RuntimeError.type(expr, expected: [OAuth2Protocol.type])
      }
    } else {
      HTTPOAuthLibrary.authRequestManager.cancel(timeout: 0.0, in: self.context)
    }
    return .void
  }
  
  private func httpRequestSign(request: Expr, oauth: Expr) throws -> Expr {
    let request = try self.request(from: request)
    if let token = try self.oauth2(from: oauth).oauth2.accessToken {
      request.headers["Authorization"] = "Bearer \(token)"
      return .true
    } else {
      return .false
    }
  }
  
  private func isOAuth2Session(expr: Expr, args: Arguments) throws -> Expr {
    guard case .object(let obj) = expr, obj is OAuth2Session else {
      return .false
    }
    return .true
  }
  
  // Configuration arguments in addition to http-session:
  //   - `host`: Provide `host` if 301 and 302 redirects should be followed automatically,
  //      as long as they appear on the same host.
  //   - `intercept403`: If set to true, a 403 is treated as a 401. (e.g. needed for Google)
  private func makeOAuth2Session(expr: Expr, args: Arguments) throws -> Expr {
    let oauth2 = try self.oauth2(from: expr)
    var intercept403 = false
    var host: String? = nil
    var config: URLSessionConfiguration = .default
    var iter = args.makeIterator()
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
          } else if let session = obj as? OAuth2Session {
            config = session.configuration
            intercept403 = session.loader.alsoIntercept403
            break
          }
          fallthrough
        default:
          throw RuntimeError.type(prototype,
                                  expected: [.booleanType, HTTPSession.type, OAuth2Session.type])
      }
    }
    if let h = iter.next(), h != .false && h != .null {
      host = try h.asString()
    }
    if let intercept = iter.next(), intercept != .null {
      intercept403 = intercept.isTrue
    }
    try HTTPLibrary.overrideConfig(&config, with: &iter)
    guard iter.next() == nil else {
      throw RuntimeError.argumentCount(of: "make-oauth2-session",
                                       min: 0,
                                       max: 6,
                                       args: .makeList(args))
    }
    let session = OAuth2Session(oauth2: oauth2,
                                host: host,
                                intercept403: intercept403,
                                config: config)
    return .object(session)
  }
  
  private func oauth2SessionOAuth2(expr: Expr) throws -> Expr {
    return .object(try self.oauth2Session(from: expr).oauth2)
  }
  
  private func oauth2SessionHttpSession(expr: Expr) throws -> Expr {
    return .object(HTTPSession(session: try self.oauth2Session(from: expr).loader.session))
  }
  
  private func responseHandler(_ f: Future) -> (OAuth2Response) -> Void {
    return { resp in
      defer {
        HTTPOAuthLibrary.authRequestManager.unregister(future: f, in: self.context)
      }
      do {
        if let error = resp.error {
          _ = try f.setResult(in: self.context, to: .error(RuntimeError.os(error)), raise: true)
        } else if let data = resp.data {
          _ = try f.setResult(in: self.context,
                                   to: .object(HTTPResponse(response: resp.response, body: data)),
                                   raise: false)
        } else {
          _ = try f.setResult(in: self.context,
                                   to: .error(RuntimeError.eval(.serverError)),
                                   raise: true)
        }
      } catch {
        do {
          _ = try f.setResult(in: self.context,
                                   to: .error(RuntimeError.eval(.serverError, .object(f))),
                                   raise: true)
        } catch {}
      }
    }
  }
  
  private func oauth2SessionSend(request: Expr, expr: Expr) throws -> Expr {
    let request = try self.request(from: request)
    let session = try self.oauth2Session(from: expr)
    let result = Future(external: false)
    switch request.method {
      case "GET", "HEAD", "POST", "PUT", "DELETE", "CONNECT", "OPTIONS", "TRACE", "PATCH":
        HTTPOAuthLibrary.authRequestManager.register(oauth2: session.loader.oauth2,
                                                  result: result,
                                                  in: self.context)
        session.loader.perform(request: session.request(from: request),
                               callback: self.responseHandler(result))
      default:
        throw RuntimeError.eval(.unsupportedHttpMethod, .makeString(request.method))
    }
    return .object(result)
  }
  
}

public final class OAuth2Protocol: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "oauth2"))
  
  fileprivate let flow: Symbol
  fileprivate let oauth2: OAuth2
  
  public init(flow: Symbol, oauth2: OAuth2) {
    self.flow = flow
    self.oauth2 = oauth2
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    return "#<\(self.tagString)>"
  }
  
  public override var tagString: String {
    return "\(Self.type) \(self.identityString): \(flow) \(self.oauth2.authURL.absoluteString)"
  }
  
  public override func unpack(in context: Context) -> Exprs {
    return [.makeString(self.identityString),
            .symbol(self.flow),
            .makeString(self.oauth2.authURL.absoluteString)]
  }
}

public final class OAuth2Session: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "oauth2-session"))

  fileprivate let oauth2: OAuth2Protocol
  fileprivate let loader: OAuth2DataLoader
  
  public init(oauth2: OAuth2Protocol,
              host: String?,
              intercept403: Bool,
              config: URLSessionConfiguration) {
    self.oauth2 = oauth2
    self.loader = OAuth2DataLoader(oauth2: oauth2.oauth2, host: host)
    self.loader.alsoIntercept403 = intercept403
    self.loader.sessionConfiguration = config
  }
  
  public convenience init(oauth2: OAuth2Protocol,
                          host: String? = nil,
                          intercept403: Bool = false,
                          ephemeral: Bool = false,
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
    self.init(oauth2: oauth2, host: host, intercept403: intercept403, config: config)
  }
  
  public override var type: Type {
    return Self.type
  }
  
  public override var string: String {
    return "#<\(self.tagString)>"
  }
  
  public override var tagString: String {
    let host = (self.loader.sessionDelegate as? OAuth2DataLoaderSessionTaskDelegate)?.host
    return
      "\(Self.type) \(self.identityString): " +
      "host=\(host == nil ? "#f" : host!), " +
      "intercept403=\(self.loader.alsoIntercept403 ? "#t" : "#f"), " +
      "timeout=\(self.configuration.timeoutIntervalForRequest), " +
      "cookies=\(self.configuration.httpShouldSetCookies ? "#t" : "#f"), " +
      "cache=\(HTTPLibrary.string(forCachePolicy: self.configuration.requestCachePolicy)), " +
      "maxconnect=\(self.configuration.httpMaximumConnectionsPerHost), " +
      "pipelining=\(self.configuration.httpShouldUsePipelining ? "#t" : "#f"), " +
      "cellular=\(self.configuration.allowsCellularAccess ? "#t" : "#f")"
  }
  
  public var configuration: URLSessionConfiguration {
    return self.loader.sessionConfiguration!
  }
  
  public override func unpack(in context: Context) -> Exprs {
    let host = (self.loader.sessionDelegate as? OAuth2DataLoaderSessionTaskDelegate)?.host
    return [.makeString(self.identityString),
            host == nil ? .false : .makeString(host!),
            .makeBoolean(self.loader.alsoIntercept403),
            .makeNumber(self.configuration.timeoutIntervalForRequest),
            .makeBoolean(self.configuration.httpShouldSetCookies),
            .makeString(HTTPLibrary.string(forCachePolicy: self.configuration.requestCachePolicy)),
            .makeNumber(self.configuration.httpMaximumConnectionsPerHost),
            .makeBoolean(self.configuration.httpShouldUsePipelining),
            .makeBoolean(self.configuration.allowsCellularAccess)]
  }
  
  public func request(from req: HTTPRequest) -> URLRequest {
    var request = self.loader.oauth2.request(forURL: req.url)
    request.httpMethod = req.method
    for (key, value) in req.headers {
      request.setValue(value, forHTTPHeaderField: key)
    }
    if let data = req.body {
      request.httpBody = Data(data)
    }
    request.timeoutInterval = req.timeoutIntervalForRequest ??
                              self.loader.sessionConfiguration!.timeoutIntervalForRequest
    request.httpShouldHandleCookies = req.httpShouldSetCookies ??
                                      self.loader.sessionConfiguration!.httpShouldSetCookies
    request.cachePolicy = req.requestCachePolicy ??
                          self.loader.sessionConfiguration!.requestCachePolicy
    request.httpShouldUsePipelining = req.httpShouldUsePipelining ??
                                      self.loader.sessionConfiguration!.httpShouldUsePipelining
    request.allowsCellularAccess = req.allowsCellularAccess ??
                                   self.loader.sessionConfiguration!.allowsCellularAccess
    return request
  }
}

public struct AuthenticatedRequestManager {
  public class RequestInProgress {
    var initiated: Date
    var redirected: Date?
    weak var oauth2: OAuth2?
    weak var result: Future?
    
    init(oauth2: OAuth2?, result: Future?) {
      self.initiated = .now
      self.redirected = nil
      self.oauth2 = oauth2
      self.result = result
    }
  }
  
  private let lock = NSLock()
  public var inProgress: [RequestInProgress] = []
  
  public mutating func register(oauth2: OAuth2?, result: Future?, in context: Context) {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    var i = 0
    while i < self.inProgress.count {
      let container = self.inProgress[i]
      if container.oauth2 == nil || container.result == nil {
        _ = try? container.result?.setResult(
          in: context,
          to: .error(RuntimeError.custom("timeout", "OAuth2 authentication canceled", [])),
          raise: true)
        self.inProgress.remove(at: i)
      } else {
        i += 1
      }
    }
    self.inProgress.append(RequestInProgress(oauth2: oauth2, result: result))
  }
  
  public mutating func unregister(oauth2: OAuth2, in context: Context) {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    var i = 0
    while i < self.inProgress.count {
      let container = self.inProgress[i]
      if container.oauth2 === oauth2 || container.oauth2 == nil || container.result == nil {
        _ = try? container.result?.setResult(
          in: context,
          to: .error(RuntimeError.custom("timeout", "OAuth2 authentication canceled", [])),
          raise: true)
        self.inProgress.remove(at: i)
      } else {
        i += 1
      }
    }
  }
  
  public mutating func unregister(future: Future, in context: Context) {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    var i = 0
    while i < self.inProgress.count {
      let container = self.inProgress[i]
      if container.result === future {
        self.inProgress.remove(at: i)
        return
      } else if container.oauth2 == nil || container.result == nil {
        _ = try? container.result?.setResult(
          in: context,
          to: .error(RuntimeError.custom("timeout", "OAuth2 authentication canceled", [])),
          raise: true)
        self.inProgress.remove(at: i)
      } else {
        i += 1
      }
    }
  }
  
  public mutating func cancel(timeout: TimeInterval,
                       oauth2: OAuth2? = nil,
                       future: Future? = nil,
                       in context: Context) {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    var i = 0
    while i < self.inProgress.count {
      let container = self.inProgress[i]
      if container.oauth2 == nil ||
         container.result == nil ||
         ((oauth2 == nil || container.oauth2 === oauth2!) &&
          (future == nil || container.result == future!) &&
          container.initiated.addingTimeInterval(timeout) < .now) {
        _ = try? container.result?.setResult(
          in: context,
          to: .error(RuntimeError.custom("timeout", "OAuth2 authentication canceled", [])),
          raise: true)
        self.inProgress.remove(at: i)
      } else {
        i += 1
      }
    }
  }
  
  public mutating func redirect(url: URL) {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    var i = 0
    while i < self.inProgress.count {
      do {
        try self.inProgress[i].oauth2?.handleRedirectURL(url)
        self.inProgress[i].redirected = .now
        return
      } catch {}
      i += 1
    }
  }
}

public protocol HTTPOAuthConfig {
  init(context: Context)
  func configureEmbeddedAuth(oauth2: OAuth2)
  func createLogger(level: OAuth2LogLevel) -> OAuth2Logger
}

public final class LispKitHTTPOAuthConfig: HTTPOAuthConfig {
  public init(context: LispKit.Context) {
  }
  
  public func configureEmbeddedAuth(oauth2: OAuth2) {
    oauth2.authConfig.authorizeEmbedded = true
    oauth2.authConfig.authorizeContext = nil
    oauth2.authConfig.ui.useSafariView = false
    oauth2.authConfig.ui.useAuthenticationSession = true
  }
  
  public func createLogger(level: OAuth2LogLevel) -> OAuth2Logger {
    return OAuth2DebugLogger(level)
  }
}
