//
//  URLLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 26/10/2024.
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

///
/// URL library
///
/// URLs are represented as strings in LispKit. This library provides functionality
/// for handling strings containing URLs.
///
public final class URLLibrary: NativeLibrary {
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "url"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("url?", self.isUrl))
    self.define(Procedure("make-url", self.makeUrl))
    self.define(Procedure("url", self.url))
    self.define(Procedure("url-copy", self.urlCopy))
    self.define(Procedure("url-scheme", self.urlScheme))
    self.define(Procedure("url-authority", self.urlAuthority))
    self.define(Procedure("url-user", self.urlUser))
    self.define(Procedure("url-password", self.urlPassword))
    self.define(Procedure("url-host", self.urlHost))
    self.define(Procedure("url-port", self.urlPort))
    self.define(Procedure("url-path", self.urlPath))
    self.define(Procedure("url-query", self.urlQuery))
    self.define(Procedure("url-query-items", self.urlQueryItems))
    self.define(Procedure("url-fragment", self.urlFragment))
    self.define(Procedure("url-format", urlFormat))
    self.define(Procedure("url-parse", urlParse))
    self.define(Procedure("file-url", self.fileUrl))
    self.define(Procedure("file-url?", self.isFileUrl))
    self.define(Procedure("file-url-standardize", self.fileUrlStandardize))
    self.define(Procedure("url-decode", urlDecode))
    self.define(Procedure("url-encode", urlEncode))
  }

  private func makeUrl(_ proto: Expr, _ args: Arguments) throws -> Expr {
    if proto.isFalse {
      return try self.url(URLComponents(), verify: false, args: args)
    } else if case .string(let proto) = proto,
              let comp = URLComponents(string: proto as String) {
      return try self.url(comp, verify: false, args: args)
    } else {
      throw RuntimeError.eval(.urlPrototypeInvalid, proto, .makeString("make-url"))
    }
  }
  
  private func url(_ args: Arguments) throws -> Expr {
    return try self.url(URLComponents(), verify: true, args: args)
  }
  
  private func urlCopy(_ expr: Expr, _ args: Arguments) throws -> Expr {
    if case .string(let proto) = expr,
       let comp = URLComponents(string: proto as String) {
      return try self.url(comp, verify: true, args: args)
    } else {
      throw RuntimeError.eval(.urlPrototypeInvalid, expr, .makeString("url-copy"))
    }
  }
  
  private func url(_ cmp: URLComponents, verify: Bool, args: Arguments) throws -> Expr {
    var components = cmp
    var iter = args.makeIterator()
    if let arg = iter.next(), arg.isTrue {
      components.scheme = arg == .true ? nil : try arg.asString()
    }
    if let arg = iter.next(), arg.isTrue {
      switch arg {
        case .false:
          break
        case .true:
          components.user = nil
          components.password = nil
          components.host = nil
          components.port = nil
        case .string(let str):
          components.host = str as String
          components.port = nil
        case .pair(.false, .null):
          break
        case .pair(.true, .null):
          components.host = nil
          components.port = nil
        case .pair(let host, .null):
          components.host = try host.asString()
          components.port = nil
        case .pair(.false, .pair(.false, .null)):
          break
        case .pair(.false, .pair(.true, .null)):
          components.port = nil
        case .pair(.true, .pair(.true, .null)):
          components.host = nil
          components.port = nil
        case .pair(.string(let host), .pair(.fixnum(let port), .null)):
          components.host = host as String
          components.port = try (Expr.fixnum(port)).asInt(above: 0, below: 65536)
        case .pair(let user, .pair(.false, .pair(.false, .null))):
          if user.isTrue {
            components.user = user == .true ? nil : try user.asString()
          }
        case .pair(let user, .pair(.false, .pair(.true, .null))):
          if user.isTrue {
            components.user = user == .true ? nil : try user.asString()
          }
          components.port = nil
        case .pair(let user, .pair(.true, .pair(.true, .null))):
          if user.isTrue {
            components.user = user == .true ? nil : try user.asString()
          }
          components.host = nil
          components.port = nil
        case .pair(let user, .pair(.string(let host), .pair(.fixnum(let port), .null))):
          if user.isTrue {
            components.user = user == .true ? nil : try user.asString()
          }
          components.host = host as String
          components.port = try (Expr.fixnum(port)).asInt(above: 0, below: 65536)
        case .pair(_, .pair(_, .pair(.true, .pair(.fixnum(_), .null)))):
          throw RuntimeError.eval(.urlAuthorityError, arg)
        case .pair(let user, .pair(let pass, .pair(let host, .pair(let port, .null)))):
          if user.isTrue {
            components.user = user == .true ? nil : try user.asString()
          }
          if pass.isTrue {
            components.password = pass == .true ? nil : try pass.asString()
          }
          if host.isTrue {
            components.host = host == .true ? nil : try host.asString()
            if host == .true {
              components.port = nil
            }
          }
          if port.isTrue {
            components.port = port == .true ? nil : try port.asInt(above: 0, below: 65536)
            if components.host == nil && components.port != nil {
              throw RuntimeError.eval(.urlAuthorityError, arg)
            }
          }
        default:
          throw RuntimeError.eval(.urlAuthorityError, arg)
      }
    }
    if let arg = iter.next(), arg.isTrue {
      components.path = try arg.asString()
    }
    if let arg = iter.next(), arg.isTrue {
      if case .true = arg {
        components.query = nil
      } else if case .string(let str) = arg {
        components.query = str as String
      } else {
        var params: [URLQueryItem] = []
        var list = arg
        while case .pair(.pair(let key, let value), let rest) = list {
          params.append(URLQueryItem(name: try key.asString(),
                                     value: value.isNull || value.isFalse ? nil : try value.asString()))
          list = rest
        }
        guard case .null = list else {
          throw RuntimeError.type(arg, expected: [.properListType])
        }
        components.queryItems = params
      }
    }
    if let arg = iter.next(), arg.isTrue {
      components.fragment = arg == .true ? nil : try arg.asString()
    }
    if verify {
      if let str = components.url?.absoluteString {
        guard let url = URL(string: str),
              url.isFileURL || (url.host != nil && url.scheme != nil) else {
          return .false
        }
        return .makeString(str)
      } else {
        return .false
      }
    } else {
      if let str = components.string {
        return .makeString(str)
      } else {
        return .false
      }
    }
  }
  
  private func url(from expr: Expr) throws -> URL {
    guard case .string(let str) = expr,
          let url = URL(string: str as String),
          url.isFileURL || (url.host != nil && url.scheme != nil) else {
      throw RuntimeError.type(expr, expected: [.urlType])
    }
    return url
  }
  
  private func isUrl(_ expr: Expr) throws -> Expr {
    guard case .string(let str) = expr,
          let url = URL(string: str as String),
          url.isFileURL || (url.host != nil && url.scheme != nil) else {
      return .false
    }
    return .true
  }
  
  private func urlScheme(_ expr: Expr) throws -> Expr {
    if let res = try self.url(from: expr).scheme {
      return .makeString(res)
    } else {
      return .false
    }
  }
  
  private func urlAuthority(_ expr: Expr, _ percentEncoded: Expr?) throws -> Expr {
    func mkstr(_ str: String?) -> Expr {
      if let str {
        return .makeString(str)
      }
      return .false
    }
    func mknum(_ num: Int?) -> Expr {
      if let num {
        return .fixnum(Int64(num))
      }
      return .false
    }
    let pe = percentEncoded?.isTrue ?? false
    let url = try self.url(from: expr)
    return .makeList(mkstr(url.user(percentEncoded: pe)),
                     mkstr(url.password(percentEncoded: pe)),
                     mkstr(url.host(percentEncoded: pe)),
                     mknum(url.port))
  }
  
  private func urlUser(_ expr: Expr, _ percentEncoded: Expr?) throws -> Expr {
    let url = try self.url(from: expr)
    if let percentEncoded {
      if let res = url.user(percentEncoded: percentEncoded.isTrue) {
        return .makeString(res)
      } else {
        return .false
      }
    } else if let res = url.user() {
      return .makeString(res)
    } else {
      return .false
    }
  }
  
  private func urlPassword(_ expr: Expr, _ percentEncoded: Expr?) throws -> Expr {
    let url = try self.url(from: expr)
    if let percentEncoded {
      if let res = url.password(percentEncoded: percentEncoded.isTrue) {
        return .makeString(res)
      } else {
        return .false
      }
    } else if let res = url.password() {
      return .makeString(res)
    } else {
      return .false
    }
  }
  
  private func urlHost(_ expr: Expr, _ percentEncoded: Expr?) throws -> Expr {
    let url = try self.url(from: expr)
    if let percentEncoded {
      if let res = url.host(percentEncoded: percentEncoded.isTrue), !res.isEmpty {
        return .makeString(res)
      } else {
        return .false
      }
    } else if let res = url.host(), !res.isEmpty {
      return .makeString(res)
    } else {
      return .false
    }
  }
  
  private func urlPort(_ expr: Expr) throws -> Expr {
    if let res = try self.url(from: expr).port {
      return .makeNumber(res)
    } else {
      return .false
    }
  }
  
  private func urlPath(_ expr: Expr, _ percentEncoded: Expr?) throws -> Expr {
    let url = try self.url(from: expr)
    if let percentEncoded {
      return .makeString(url.path(percentEncoded: percentEncoded.isTrue))
    } else {
      return .makeString(url.path())
    }
  }
  
  private func urlQuery(_ expr: Expr, _ percentEncoded: Expr?) throws -> Expr {
    let url = try self.url(from: expr)
    if let percentEncoded {
      if let res = url.query(percentEncoded: percentEncoded.isTrue) {
        return .makeString(res)
      } else {
        return .false
      }
    } else if let res = url.query() {
      return .makeString(res)
    } else {
      return .false
    }
  }
  
  private func urlQueryItems(_ expr: Expr) throws -> Expr {
    guard let items = URLComponents(url: try self.url(from: expr),
                                    resolvingAgainstBaseURL: true)?.queryItems else {
      return .false
    }
    var res = Expr.null
    for item in items.reversed() {
      res = .pair(.pair(.makeString(item.name),
                        item.value == nil ? .false : .makeString(item.value!)), res)
    }
    return res
  }
  
  private func urlFragment(_ expr: Expr, _ percentEncoded: Expr?) throws -> Expr {
    let url = try self.url(from: expr)
    if let percentEncoded {
      if let res = url.fragment(percentEncoded: percentEncoded.isTrue) {
        return .makeString(res)
      } else {
        return .false
      }
    } else if let res = url.fragment() {
      return .makeString(res)
    } else {
      return .false
    }
  }
  
  private func urlDecode(_ expr: Expr, _ force: Expr?) throws -> Expr {
    if let res = try expr.asString().removingPercentEncoding {
      return .makeString(res)
    } else if force?.isTrue ?? false {
      return expr
    } else {
      return .false
    }
  }
  
  private func urlEncode(_ expr: Expr, _ allowed: Expr?, _ force: Expr?) throws -> Expr {
    var allowedChars: CharacterSet = .urlQueryAllowed
    var encodeAmp = true
    if let allowed {
      switch allowed {
        case .false:
          allowedChars = CharacterSet()
          encodeAmp = false
        case .true:
          break
        case .symbol(let sym):
          switch sym.identifier {
            case "fragment":
              allowedChars = .urlFragmentAllowed
            case "host":
              allowedChars = .urlHostAllowed
            case "password":
              allowedChars = .urlPasswordAllowed
            case "path":
              allowedChars = .urlPathAllowed
            case "query":
              allowedChars = .urlQueryAllowed
            case "user":
              allowedChars = .urlUserAllowed
            default:
              return .false
          }
        case .string(let str):
          allowedChars = CharacterSet(charactersIn: str as String)
          encodeAmp = !allowedChars.contains("&")
        case .pair(.string(let str), .null):
          allowedChars = CharacterSet(charactersIn: str as String).inverted
          encodeAmp = !allowedChars.contains("&")
        default:
          guard case .object(let obj) = allowed, let cs = obj as? CharSet else {
            throw RuntimeError.type(expr, expected: [CharSet.type])
          }
          allowedChars = CharacterSet(charactersIn: String(utf16CodeUnits: cs.array, count: cs.count))
          encodeAmp = !allowedChars.contains("&")
      }
    }
    let res = try expr.asString().addingPercentEncoding(withAllowedCharacters: allowedChars)
    if let res = encodeAmp ? (res?.replacingOccurrences(of: "&", with: "%26")) : res {
      return .makeString(res)
    } else if force?.isTrue ?? false {
      return expr
    } else {
      return .false
    }
  }
  
  private func urlFormat(_ expr: Expr, _ args: Arguments) throws -> Expr {
    let url = try self.url(from: expr)
    func cond(_ expr: Expr, for c: URL.FormatStyle.Component) throws -> (Bool, URL.FormatStyle.Component) {
      switch expr {
        case .null, .true:
          return (true, c)
        case .false:
          return (false, c)
        default:
          let ident = try expr.asSymbol().identifier
          switch ident {
            case "scheme":
              return (true, .scheme)
            case "user":
              return (true, .user)
            case "password":
              return (true, .password)
            case "host":
              return (true, .host)
            case "port":
              return (true, .port)
            case "path":
              return (true, .path)
            case "query":
              return (true, .query)
            case "fragment":
              return (true, .fragment)
            case "scheme?":
              return (false, .scheme)
            case "user?":
              return (false, .user)
            case "password?":
              return (false, .password)
            case "host?":
              return (false, .host)
            case "port?":
              return (false, .port)
            case "path?":
              return (false, .path)
            case "query?":
              return (false, .query)
            case "fragment?":
              return (false, .fragment)
            default:
              throw RuntimeError.eval(.urlFormatConstraintError, expr)
          }
      }
    }
    func config(_ expr: Expr, for comp: URL.FormatStyle.Component) throws ->
           URL.FormatStyle.ComponentDisplayOption {
      switch expr {
        case .false:
          return .never
        case .true:
          return .always
        case .pair(_ , _):
          var set: Set<String> = []
          var list = expr
          while case .pair(let x, let rest) = list {
            set.insert(try x.asString())
            list = rest
          }
          let (omit, comp) = try cond(list, for: comp)
          if omit {
            return .omitWhen(comp, matches: set)
          } else {
            return .displayWhen(comp, matches: set)
          }
        default:
          return .omitWhen(comp, matches: [try expr.asString()])
      }
    }
    func configHost(_ expr: Expr) throws -> URL.FormatStyle.HostDisplayOption {
      switch expr {
        case .false:
          return .never
        case .true:
          return .always
        case .pair(_ , _):
          var set: Set<String> = []
          var list = expr
          while case .pair(let x, let rest) = list {
            set.insert(try x.asString())
            list = rest
          }
          let (omit, comp) = try cond(list, for: .host)
          if omit {
            return .omitWhen(comp, matches: set)
          } else {
            return .displayWhen(comp, matches: set)
          }
        default:
          return .omitWhen(.host, matches: [try expr.asString()])
      }
    }
    var style = URL.FormatStyle()
    var iter = args.makeIterator()
    if let arg = iter.next(), !arg.isNull {
      style = style.scheme(try config(arg, for: .scheme))
    }
    if let arg = iter.next(), !arg.isNull {
      style = style.user(try config(arg, for: .user))
    }
    if let arg = iter.next(), !arg.isNull {
      style = style.password(try config(arg, for: .password))
    }
    if let arg = iter.next(), !arg.isNull {
      style = style.host(try configHost(arg))
    }
    if let arg = iter.next(), !arg.isNull {
      style = style.port(try config(arg, for: .port))
    }
    if let arg = iter.next(), !arg.isNull {
      style = style.path(try config(arg, for: .path))
    }
    if let arg = iter.next(), !arg.isNull {
      style = style.query(try config(arg, for: .query))
    }
    if let arg = iter.next(), !arg.isNull {
      style = style.fragment(try config(arg, for: .fragment))
    }
    return .makeString(style.format(url))
  }
  
  private func urlParse(_ expr: Expr, _ args: Arguments) throws -> Expr {
    var style = URL.ParseStrategy()
    var iter = args.makeIterator()
    func config(_ expr: Expr) throws -> URL.ParseStrategy.ComponentParseStrategy<String> {
      switch expr {
        case .false:
          return .optional
        case .true:
          return .required
        default:
          return .defaultValue(try expr.asString())
      }
    }
    if let arg = iter.next(), !arg.isNull {
      style = style.scheme(try config(arg))
    }
    if let arg = iter.next(), !arg.isNull {
      style = style.user(try config(arg))
    }
    if let arg = iter.next(), !arg.isNull {
      style = style.password(try config(arg))
    }
    if let arg = iter.next(), !arg.isNull {
      style = style.host(try config(arg))
    }
    if let arg = iter.next(), !arg.isNull {
      switch arg {
        case .false:
          style = style.port(.optional)
        case .true:
          style = style.port(.required)
        default:
          style = style.port(.defaultValue(try arg.asInt(above: 0, below: 65536)))
      }
    }
    if let arg = iter.next(), !arg.isNull {
      style = style.path(try config(arg))
    }
    if let arg = iter.next(), !arg.isNull {
      style = style.query(try config(arg))
    }
    if let arg = iter.next(), !arg.isNull {
      style = style.fragment(try config(arg))
    }
    if let url = try? style.parse(expr.asString().trimmingCharacters(in: .whitespacesAndNewlines)) {
      return .makeString(url.absoluteString)
    } else {
      return .false
    }
  }
  
  private func fileUrl(_ expr: Expr, _ relativeTo: Expr?, _ expand: Expr?) throws -> Expr {
    var url: URL
    if let relativeTo, relativeTo.isTrue {
      if case .true = relativeTo {
        url = URL(fileURLWithPath: try expr.asString(),
                  relativeTo: URL(fileURLWithPath: self.context.evaluator.currentDirectoryPath))
      } else if let root =  URL(string: try relativeTo.asString()), root.isFileURL {
        url = URL(fileURLWithPath: try expr.asString(), relativeTo: root)
      } else {
        return .false
      }
    } else {
      url = URL(fileURLWithPath: try expr.asString(),
                relativeTo: URL(fileURLWithPath: self.context.evaluator.currentDirectoryPath))
    }
    return .makeString((expand?.isTrue ?? false) ? url.resolvingSymlinksInPath().absoluteString
                                                 : url.absoluteString)
  }
  
  private func isFileUrl(_ expr: Expr, _ dirPath: Expr?) throws -> Expr {
    guard case .string(let str) = expr,
          let url = URL(string: str as String),
          url.isFileURL else {
      return .false
    }
    if let dirPath {
      if dirPath.isTrue {
        return .makeBoolean(url.hasDirectoryPath)
      } else {
        return .makeBoolean(!url.hasDirectoryPath)
      }
    } else {
      return .true
    }
  }
  
  private func fileUrlStandardize(_ expr: Expr, _ resolveSymLinks: Expr?) throws -> Expr {
    if var url = URL(string: try expr.asString()), url.isFileURL {
      if resolveSymLinks?.isTrue ?? false {
        url.resolveSymlinksInPath()
      }
      return .makeString(url.standardizedFileURL.absoluteString)
    } else {
      return .false
    }
  }
}
