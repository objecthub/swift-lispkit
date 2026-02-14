//
//  DrawWebLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 09/06/2025.
//  Copyright © 2025 ObjectHub. All rights reserved.
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
import WebKit

public final class DrawWebLibrary: NativeLibrary {
  
  // Crop modes
  private let all: Symbol
  private let trim: Symbol
  private let inset: Symbol
  private let insetTrimmed: Symbol
  private let rect: Symbol
  private let rectTrimmed: Symbol
  
  /// Initialize symbols.
  public required init(in context: Context) throws {
    self.all = context.symbols.intern("all")
    self.trim = context.symbols.intern("trim")
    self.inset = context.symbols.intern("inset")
    self.insetTrimmed = context.symbols.intern("inset-trimmed")
    self.rect = context.symbols.intern("rect")
    self.rectTrimmed = context.symbols.intern("rect-trimmed")
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "draw", "web"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
  }
  
  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("make-web-client", self.makeWebClient))
    self.define(Procedure("web-client?", self.isWebClient))
    self.define(Procedure("web-client-busy?", self.isWebClientBusy))
    self.define(Procedure("web-client-snapshot-html", self.webClientSnapshotHtml))
    self.define(Procedure("web-client-snapshot-data", self.webClientSnapshotData))
    self.define(Procedure("web-client-snapshot-file", self.webClientSnapshotFile))
    self.define(Procedure("web-client-snapshot-url", self.webClientSnapshotUrl))
    self.define(Procedure("web-client-pdf-snapshot-html", self.webClientSnapshotHtmlPdf))
    self.define(Procedure("web-client-pdf-snapshot-data", self.webClientSnapshotDataPdf))
    self.define(Procedure("web-client-pdf-snapshot-file", self.webClientSnapshotFilePdf))
    self.define(Procedure("web-client-pdf-snapshot-url", self.webClientSnapshotUrlPdf))
  }
  
  /// Initializations of the library.
  public override func initializations() {
  }
  
  private func webClient(from: Expr) throws -> WebClient {
    guard case .object(let obj) = from, let webClient = obj as? WebClient else {
      throw RuntimeError.type(from, expected: [WebClient.type])
    }
    return webClient
  }
  
  private func cropMode(from: Expr) throws -> WebClient.CropMode {
    switch from {
      case .symbol(self.all):
        return .all
      case .symbol(self.trim):
        return .trim
      case .pair(.symbol(self.inset),
                 .pair(let top, .pair(let right, .pair(let bottom, .pair(let left, .null))))):
        return .inset(top: try top.asDouble(coerce: true),
                      right: try right.asDouble(coerce: true),
                      bottom: try bottom.asDouble(coerce: true),
                      left: try left.asDouble(coerce: true))
      case .pair(.symbol(self.insetTrimmed),
                 .pair(let top, .pair(let right, .pair(let bottom, .pair(let left, .null))))):
        return .insetTrimmed(top: try top.asDouble(coerce: true),
                             right: try right.asDouble(coerce: true),
                             bottom: try bottom.asDouble(coerce: true),
                             left: try left.asDouble(coerce: true))
      case .pair(.symbol(self.rect),
                 .pair(let x, .pair(let y, .pair(let width, .pair(let height, .null))))),
          .pair(.symbol(self.rect), .pair(.pair(let x, let y), .pair(let width, let height))):
        return .rect(x: try x.asPositiveDouble(),
                     y: try y.asPositiveDouble(),
                     width: try width.asPositiveDouble(),
                     height: try height.asPositiveDouble())
      case .pair(.symbol(self.rectTrimmed),
                 .pair(let x, .pair(let y, .pair(let width, .pair(let height, .null))))),
          .pair(.symbol(self.rectTrimmed), .pair(.pair(let x, let y), .pair(let width, let height))):
        return .rectTrimmed(x: try x.asPositiveDouble(),
                            y: try y.asPositiveDouble(),
                            width: try width.asPositiveDouble(),
                            height: try height.asPositiveDouble())
      case .pair(.pair(let x, let y), .pair(let width, let height)):
        return .rect(x: try x.asPositiveDouble(),
                     y: try y.asPositiveDouble(),
                     width: try width.asPositiveDouble(),
                     height: try height.asPositiveDouble())
      default:
        throw RuntimeError.eval(.invalidCropMode, from)
    }
  }
  
  private func makeWebClient(width: Expr, args: Arguments) throws -> Expr {
    guard let (scripts, vp, appName, d, h, prefs) =
            args.optional(.null, .false, .false, .false, .false, .null) else {
      throw RuntimeError.argumentCount(of: "make-web-client",
                                       min: 1,
                                       max: 5,
                                       args: .pair(width, .makeList(args)))
    }
    let width = try width.asPositiveDouble()
    let applicationName = appName.isTrue ? try appName.asString() : nil
    let delay: TimeInterval? = d.isTrue ? try d.asPositiveDouble() : nil
    let height: WebClient.Correction
    switch h {
      case .false, .null, .pair(.false, .null):
        height = .none
      case .pair(let x, .null):
        height = .relative(try x.asDouble(coerce: true))
      default:
        height = .absolute(try h.asPositiveDouble())
    }
    var userScripts: [(source: String, injection: WKUserScriptInjectionTime, main: Bool)] = []
    var viewPort: Expr = vp
    var ignoresViewportScaleLimits = false
    if case .pair(let viewp, let ignore) = vp {
      ignoresViewportScaleLimits = ignore.isTrue
      viewPort = viewp
    }
    switch viewPort {
      case .false:
        let viewport = """
          var meta = document.createElement('meta');
          meta.setAttribute('name', 'viewport');
          meta.setAttribute('content', 'width=\(width), initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0, user-scalable=no');
          document.getElementsByTagName('head')[0].appendChild(meta);
        """
        userScripts.append((source: viewport, injection: .atDocumentStart, main: true))
      case .true:
        let viewport = """
          var meta = document.createElement('meta');
          meta.setAttribute('name', 'viewport');
          meta.setAttribute('content', 'width=\(width)');
          document.getElementsByTagName('head')[0].appendChild(meta);
        """
        userScripts.append((source: viewport, injection: .atDocumentStart, main: true))
        let transparency = """
          var meta = document.createElement('meta');
          meta.setAttribute('name', 'transparent');
          meta.setAttribute('content', 'true');
          document.getElementsByTagName('head')[0].appendChild(meta);
        """
        userScripts.append((source: transparency, injection: .atDocumentStart, main: false))
      case .null:
        break
      default:
        let viewport = "var meta = document.createElement('meta');\n" +
        "meta.setAttribute('name', 'viewport');\n" +
        "meta.setAttribute('content', '\(try viewPort.asString())');\n" +
        "document.getElementsByTagName('head')[0].appendChild(meta);"
        userScripts.append((source: viewport, injection: .atDocumentStart, main: true))
    }
    var list = scripts
    while case .pair(let script, let rest) = list {
      let userScript: (source: String, injection: WKUserScriptInjectionTime, main: Bool)
      switch script {
        case .string(let str):
          userScript = (source: str as String, injection: .atDocumentEnd, main: true)
        case .pair(let str, .null):
          userScript = (source: try str.asString(), injection: .atDocumentEnd, main: true)
        case .pair(let str, .pair(let beg, .null)):
          userScript = (source: try str.asString(),
                        injection: beg.isTrue ? .atDocumentStart : .atDocumentEnd,
                        main: true)
        case .pair(let str, .pair(let beg, .pair(let all, .null))):
          userScript = (source: try str.asString(),
                        injection: beg.isTrue ? .atDocumentStart : .atDocumentEnd,
                        main: all.isFalse)
        default:
          throw RuntimeError.eval(.invalidUserScript, script)
      }
      userScripts.append(userScript)
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.type(scripts, expected: [.properListType])
    }
    var prefFeatures: [String : Bool] = [
      "allowFileAccessFromFileURLs" : true
    ]
    var features: [String : Bool] = [
      "allowUniversalAccessFromFileURLs" : true,
      "sward".reversed() + "background".capitalized : false
    ]
    var contentMode = WKWebpagePreferences.ContentMode.desktop
    list = prefs
    while case .pair(let feature, let rest) = list {
      switch feature {
        case .symbol(let sym):
          let ident = sym.rawIdentifier
          switch ident {
            case "desktop":
              contentMode = .desktop
            case "mobile":
              contentMode = .mobile
            case "recommended":
              contentMode = .recommended
            default:
              features[sym.rawIdentifier] = true
          }
        case .string(let str):
          features[str as String] = true
        case .pair(.symbol(let sym), let val):
          var ident = sym.rawIdentifier
          if ident.hasPrefix(":") {
            ident.removeFirst()
            prefFeatures[ident] = val.isTrue
          } else {
            features[ident] = val.isTrue
          }
        case .pair(.string(let str), let val):
          var ident = str as String
          if ident.hasPrefix(":") {
            ident.removeFirst()
            prefFeatures[ident] = val.isTrue
          } else {
            features[ident] = val.isTrue
          }
        default:
          throw RuntimeError.type(feature, expected: [.symbolType, .strType, .pairType])
      }
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.type(prefs, expected: [.properListType])
    }
    return DispatchQueue.main.sync {
      let config = WKWebViewConfiguration()
      if appName.isTrue {
        config.applicationNameForUserAgent = applicationName
      }
      // Default configurations
      config.limitsNavigationsToAppBoundDomains = false
      config.suppressesIncrementalRendering = true
      #if os(iOS) || os(watchOS) || os(tvOS)
      config.ignoresViewportScaleLimits = ignoresViewportScaleLimits
      // config.preferences.shouldPrintBackgrounds = false
      #endif
      // Set web page rendering preferences
      let prefs = WKWebpagePreferences()
      prefs.preferredContentMode = contentMode
      prefs.allowsContentJavaScript = true
      config.defaultWebpagePreferences = prefs
      for (key, val) in prefFeatures {
        config.preferences.setValue(val, forKey: key)
      }
      for (key, val) in features {
        config.setValue(val, forKey: key)
      }
      let contentController = WKUserContentController()
      for (source, injection, main) in userScripts {
        contentController.addUserScript(WKUserScript(source: source,
                                                     injectionTime: injection,
                                                     forMainFrameOnly: main))
      }
      config.userContentController = contentController
      return .object(WebClient(width: width,
                               height: height,
                               config: config,
                               delay: delay))
    }
  }
  
  private func isWebClient(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, obj is WebClient else {
      return .false
    }
    return .true
  }
  
  private func isWebClientBusy(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.webClient(from: expr).isBusy)
  }
  
  private func fail(future: Future, with error: Error) {
    do {
      _ = try future.setResult(in: context,
                               to: .error(RuntimeError.eval(.unableToReturnResultViaFuture,
                                                            .object(future),
                                                            .error(RuntimeError.os(error)))),
                               raise: true)
    } catch {}
  }
  
  private func sizeOverrides(from expr: Expr?) throws -> (CGFloat?, WebClient.Correction?) {
    if let expr {
      switch expr {
        case .false:
          return (nil, nil)
        case .pair(let x, .pair(let y, .null)):
          return (x.isFalse ? nil : try x.asPositiveDouble(),
                  y.isFalse ? nil : .relative(try y.asDouble(coerce: true)))
        case .pair(let x, let y):
          return (x.isFalse ? nil : try x.asPositiveDouble(),
                  y.isFalse ? nil : .absolute(try y.asPositiveDouble()))
        default:
          return (try expr.asPositiveDouble(), nil)
      }
    } else {
      return (nil, nil)
    }
  }
  
  private func webClientSnapshotHtml(expr: Expr,
                                     content: Expr,
                                     third: Expr,
                                     fourth: Expr?,
                                     width: Expr?) throws -> Expr {
    let wc = try self.webClient(from: expr)
    let str = try content.asString()
    let source: WebClient.ContentSource
    let crop: WebClient.CropMode
    let w: CGFloat?
    let h: WebClient.Correction?
    if let fourth {
      switch third {
        case .false:
          source = .html(content: str, baseURL: nil)
          crop = try self.cropMode(from: fourth)
          (w, h) = try self.sizeOverrides(from: width)
        case .string(let baseUrl):
          source = .html(content: str, baseURL: URL(string: baseUrl as String))
          crop = try self.cropMode(from: fourth)
          (w, h) = try self.sizeOverrides(from: width)
        default:
          source = .html(content: str, baseURL: nil)
          crop = try self.cropMode(from: third)
          (w, h) = try self.sizeOverrides(from: fourth)
      }
    } else {
      source = .html(content: str, baseURL: nil)
      crop = try self.cropMode(from: third)
      (w, h) = (nil, nil)
    }
    let result = Future(external: false)
    let context = self.context
    wc.image(source: source, crop: crop, width: w, height: h) { res in
      switch res {
        case .success(let image):
          do {
            _ = try result.setResult(in: context, to: .object(image), raise: false)
          } catch let error {
            self.fail(future: result, with: error)
          }
        case .failure(let error):
          self.fail(future: result, with: error)
      }
    }
    return .object(result)
  }
  
  private func webClientSnapshotData(expr: Expr,
                                     data: Expr,
                                     mt: Expr,
                                     args: Arguments) throws -> Expr {
    guard let (enc, base, crp, width) = args.optional(.makeString("UTF-8"),
                                                      .makeString("http://localhost"),
                                                      .symbol(self.all),
                                                      .false) else {
      throw RuntimeError.argumentCount(of: "web-client-snapshot-data",
                                       min: 3,
                                       max: 7,
                                       args: .pair(expr, .pair(data, .pair(mt, .makeList(args)))))
    }
    let wc = try self.webClient(from: expr)
    let baseUrl: URL
    if let url = URL(string: try base.asString()) {
      baseUrl = url
    } else {
      throw RuntimeError.eval(.invalidUrl, base)
    }
    let source: WebClient.ContentSource = .data(data: Data(try data.asByteVector().value),
                                                mimeType: try mt.asString(),
                                                charEnc: try enc.asString(),
                                                baseURL: baseUrl)
    let crop: WebClient.CropMode = try self.cropMode(from: crp)
    let (w, h) = try self.sizeOverrides(from: width)
    let result = Future(external: false)
    let context = self.context
    wc.image(source: source, crop: crop, width: w, height: h) { res in
      switch res {
        case .success(let image):
          do {
            _ = try result.setResult(in: context, to: .object(image), raise: false)
          } catch let error {
            self.fail(future: result, with: error)
          }
        case .failure(let error):
          self.fail(future: result, with: error)
      }
    }
    return .object(result)
  }
  
  private func webClientSnapshotFile(expr: Expr,
                                     path: Expr,
                                     dir: Expr,
                                     crop: Expr?,
                                     width: Expr?) throws -> Expr {
    let wc = try self.webClient(from: expr)
    let url = URL(filePath: self.context.fileHandler.path(try path.asPath(),
                              relativeTo: self.context.evaluator.currentDirectoryPath))
    let dir = URL(filePath: self.context.fileHandler.path(try dir.asPath(),
                              relativeTo: self.context.evaluator.currentDirectoryPath),
                  directoryHint: .isDirectory)
    let source: WebClient.ContentSource = .file(url: url, allowReadAccessTo: dir)
    let crop = (crop == nil) ? .all : try self.cropMode(from: crop!)
    let (width, height) = try self.sizeOverrides(from: width)
    let result = Future(external: false)
    let context = self.context
    wc.image(source: source, crop: crop, width: width, height: height) { res in
      switch res {
        case .success(let image):
          do {
            _ = try result.setResult(in: context, to: .object(image), raise: false)
          } catch let error {
            self.fail(future: result, with: error)
          }
        case .failure(let error):
          self.fail(future: result, with: error)
      }
    }
    return .object(result)
  }
  
  private func webClientSnapshotUrl(expr: Expr, req: Expr, crop: Expr?, w: Expr?) throws -> Expr {
    let wc = try self.webClient(from: expr)
    let request: URLRequest
    switch req {
      case .object(let obj):
        if let req = obj as? HTTPRequest {
          request = req.request()
        } else {
          fallthrough
        }
      default:
        request = URLRequest(url: try req.asURL())
    }
    let source: WebClient.ContentSource = .url(request: request)
    let crop = (crop == nil) ? .all : try self.cropMode(from: crop!)
    let (width, height) = try self.sizeOverrides(from: w)
    let result = Future(external: false)
    let context = self.context
    wc.image(source: source, crop: crop, width: width, height: height) { res in
      switch res {
        case .success(let image):
          do {
            _ = try result.setResult(in: context, to: .object(image), raise: false)
          } catch let error {
            self.fail(future: result, with: error)
          }
        case .failure(let error):
          self.fail(future: result, with: error)
      }
    }
    return .object(result)
  }
  
  private func webClientSnapshotHtmlPdf(expr: Expr,
                                        content: Expr,
                                        third: Expr?,
                                        fourth: Expr?) throws -> Expr {
    let wc = try self.webClient(from: expr)
    let str = try content.asString()
    let source: WebClient.ContentSource
    let crop: WebClient.CropMode
    if let fourth {
      source = .html(content: str,
                     baseURL: third!.isFalse ? nil : URL(string: try third!.asString()))
      crop = fourth.isFalse ? .all : try self.cropMode(from: fourth)
    } else if let third {
      switch third {
        case .false:
          source = .html(content: str, baseURL: nil)
          crop = .all
        case .string(let url):
          source = .html(content: str, baseURL: URL(string: url as String))
          crop = .all
        default:
          source = .html(content: str, baseURL: nil)
          crop = try self.cropMode(from: third)
      }
    } else {
      source = .html(content: str, baseURL: nil)
      crop = .all
    }
    let result = Future(external: false)
    let context = self.context
    wc.pdf(source: source, crop: crop, transparent: true) { res in
      switch res {
        case .success(let data):
          do {
            _ = try result.setResult(in: context,
                                     to: BytevectorLibrary.bytevector(from: data),
                                     raise: false)
          } catch let error {
            self.fail(future: result, with: error)
          }
        case .failure(let error):
          self.fail(future: result, with: error)
      }
    }
    return .object(result)
  }
  
  private func webClientSnapshotDataPdf(expr: Expr,
                                        data: Expr,
                                        mt: Expr,
                                        args: Arguments) throws -> Expr {
    guard let (enc, base, crp) = args.optional(.makeString("UTF-8"),
                                               .makeString("http://localhost"),
                                               .symbol(self.all)) else {
      throw RuntimeError.argumentCount(of: "web-client-pdf-snapshot-data",
                                       min: 3,
                                       max: 6,
                                       args: .pair(expr, .pair(data, .pair(mt, .makeList(args)))))
    }
    let wc = try self.webClient(from: expr)
    let baseUrl: URL
    if let url = URL(string: try base.asString()) {
      baseUrl = url
    } else {
      throw RuntimeError.eval(.invalidUrl, base)
    }
    let source: WebClient.ContentSource = .data(data: Data(try data.asByteVector().value),
                                                mimeType: try mt.asString(),
                                                charEnc: try enc.asString(),
                                                baseURL: baseUrl)
    let crop: WebClient.CropMode = try self.cropMode(from: crp)
    let result = Future(external: false)
    let context = self.context
    wc.pdf(source: source, crop: crop, transparent: true) { res in
      switch res {
        case .success(let data):
          do {
            _ = try result.setResult(in: context,
                                     to: BytevectorLibrary.bytevector(from: data),
                                     raise: false)
          } catch let error {
            self.fail(future: result, with: error)
          }
        case .failure(let error):
          self.fail(future: result, with: error)
      }
    }
    return .object(result)
  }
  
  private func webClientSnapshotFilePdf(expr: Expr,
                                        path: Expr,
                                        dir: Expr,
                                        crop: Expr?) throws -> Expr {
    let wc = try self.webClient(from: expr)
    let url = URL(filePath: self.context.fileHandler.path(try path.asPath(),
                              relativeTo: self.context.evaluator.currentDirectoryPath))
    let dir = URL(filePath: self.context.fileHandler.path(try dir.asPath(),
                              relativeTo: self.context.evaluator.currentDirectoryPath),
                  directoryHint: .isDirectory)
    let source: WebClient.ContentSource = .file(url: url, allowReadAccessTo: dir)
    let crop = (crop?.isFalse ?? true) ? .all : try self.cropMode(from: crop!)
    let result = Future(external: false)
    let context = self.context
    wc.pdf(source: source, crop: crop, transparent: true) { res in
      switch res {
        case .success(let data):
          do {
            _ = try result.setResult(in: context,
                                     to: BytevectorLibrary.bytevector(from: data),
                                     raise: false)
          } catch let error {
            self.fail(future: result, with: error)
          }
        case .failure(let error):
          self.fail(future: result, with: error)
      }
    }
    return .object(result)
  }
  
  private func webClientSnapshotUrlPdf(expr: Expr, req: Expr, crop: Expr?) throws -> Expr {
    let wc = try self.webClient(from: expr)
    let request: URLRequest
    switch req {
      case .object(let obj):
        if let req = obj as? HTTPRequest {
          request = req.request()
        } else {
          fallthrough
        }
      default:
        request = URLRequest(url: try req.asURL())
    }
    let source: WebClient.ContentSource = .url(request: request)
    let crop = (crop == nil) ? .all : try self.cropMode(from: crop!)
    let result = Future(external: false)
    let context = self.context
    wc.pdf(source: source, crop: crop, transparent: true) { res in
      switch res {
        case .success(let data):
          do {
            _ = try result.setResult(in: context,
                                     to: BytevectorLibrary.bytevector(from: data),
                                     raise: false)
          } catch let error {
            self.fail(future: result, with: error)
          }
        case .failure(let error):
          self.fail(future: result, with: error)
      }
    }
    return .object(result)
  }
}

public class WebClient: NSObject, WKNavigationDelegate, CustomExpr {
  public static let type = Type.objectType(Symbol(uninterned: "web-client"))
  
  public enum ContentSource {
    case html(content: String, baseURL: URL?)
    case data(data: Data, mimeType: String, charEnc: String, baseURL: URL)
    case file(url: URL, allowReadAccessTo: URL)
    case url(request: URLRequest)
  }
  
  public enum CropMode {
    case all
    case trim
    case inset(top: CGFloat, right: CGFloat, bottom: CGFloat, left: CGFloat)
    case insetTrimmed(top: CGFloat, right: CGFloat, bottom: CGFloat, left: CGFloat)
    case rect(x: CGFloat, y: CGFloat, width: CGFloat, height: CGFloat)
    case rectTrimmed(x: CGFloat, y: CGFloat, width: CGFloat, height: CGFloat)
    case compute(proc: (WKWebView, Context) -> CGRect)
    
    func rect(in webView: WKWebView, context: Context) -> CGRect {
      switch self {
        case .all:
          return webView.bounds
        case .trim:
          return context.rect(in: webView)
        case .insetTrimmed(top: let top, right: let right, bottom: let bottom, left: let left):
          if case .parameters(left: let x, top: let y, width: let w, height: let h) = context {
            return CGRect(x: x + left,
                          y: y + top,
                          width: max(0.0, w - x - left - right),
                          height: max(0.0, h - 2 * y - top - bottom))
          } else {
            fallthrough
          }
        case .inset(top: let top, right: let right, bottom: let bottom, left: let left):
          return CGRect(x: webView.bounds.origin.x + left,
                        y: webView.bounds.origin.y + top,
                        width: max(0.0, webView.bounds.width - webView.bounds.origin.x - left - right),
                        height: max(0.0, webView.bounds.height - webView.bounds.origin.y - top - bottom))
        case .rectTrimmed(x: let x, y: let y, width: let width, height: let height):
          if case .parameters(left: let left, top: let top, width: _, height: _) = context {
            return CGRect(x: x + left, y: y + top, width: max(0.0, width), height: max(0.0, height))
          } else {
            fallthrough
          }
        case .rect(x: let x, y: let y, width: let width, height: let height):
          return CGRect(x: x, y: y, width: max(0.0, width), height: max(0.0, height))
        case .compute(proc: let proc):
          return proc(webView, context)
      }
    }
  }
  
  private enum ExportMethod {
    case image(crop: CropMode,
               width: CGFloat?,
               height: Correction?,
               handler: (Result<NativeImage, Error>) -> Void)
    case pdf(crop: CropMode, transparent: Bool, handler: (Result<Data, Error>) -> Void)
    
    func height(detected: CGFloat, computed: CGFloat) -> CGFloat {
      switch self {
        case .image(crop: _, width: _, height: let height, handler: _):
          if let height {
            switch height {
              case .none:
                return detected
              case .relative(let x):
                return detected + x
              case .absolute(let x):
                return x
            }
          } else {
            return computed
          }
        case .pdf(crop: _, transparent: _, handler: _):
          return computed
      }
    }
  }
  
  public enum ExportError: Error {
    case noResponse
    case simultaneousUse
    case unableToCreatePng
    case unableToCreatePdf
  }
  
  public enum Context {
    case parameters(left: CGFloat, top: CGFloat, width: CGFloat, height: CGFloat)
    case error(any Error)
    
    func rect(in webView: WKWebView) -> CGRect {
      switch self {
        case .parameters(left: let left, top: let top, width: let width, height: let height):
          return CGRect(x: left, y: top, width: width - left, height: height - 2 * top)
        case .error(_):
          return webView.bounds
      }
    }
  }
  
  public enum Correction {
    case none
    case relative(CGFloat)
    case absolute(CGFloat)
    
    var expr: Expr {
      switch self {
        case .none:
          return .false
        case .relative(let x):
          return .makeNumber(x)
        case .absolute(let x):
          return .pair(.makeNumber(x), .null)
      }
    }
    
    func compute(detected: CGFloat) -> CGFloat {
      switch self {
        case .none:
          return detected
        case .relative(let x):
          return detected + x
        case .absolute(let x):
          return x
      }
    }
  }
  
  private let webView: WKWebView
  private let width: CGFloat
  private let height: Correction
  private var delay: TimeInterval?
  private var method: ExportMethod?
  private var queue: [(ContentSource, ExportMethod)]
  
  public init(width: CGFloat,
              height: Correction,
              config: WKWebViewConfiguration = WKWebViewConfiguration(),
              delay: TimeInterval? = nil) {
    self.width = width
    self.height = height
    self.webView = WKWebView(frame: CGRect(x: 0, y: 0, width: width, height: 0),
                             configuration: config)
    #if os(iOS) || os(watchOS) || os(tvOS)
    self.webView.setMinimumViewportInset(.zero, maximumViewportInset: .zero)
    self.webView.scrollView.contentInsetAdjustmentBehavior = .never
    #elseif os(macOS)
    self.webView.setValue(false, forKey: "sward".reversed() + "background".capitalized)
    self.webView.setMinimumViewportInset(NSEdgeInsetsZero, maximumViewportInset: NSEdgeInsetsZero)
    #endif
    self.method = nil
    self.queue = []
    self.delay = delay
    super.init()
    self.webView.navigationDelegate = self
  }
  
  public var type: Type {
    return Self.type
  }
  
  public var tagString: String {
    let res = "web-client \(self.identityString): width=\(self.width)"
    return delay == nil ? res : (res + ", delay=\(self.delay!)")
  }
  
  public var identity: UInt {
    return UInt(bitPattern: ObjectIdentifier(self))
  }
  
  public var identityString: String {
    return String(self.identity, radix: 16)
  }
  
  public func equals(to expr: Expr) -> Bool {
    guard case .object(let obj) = expr,
          let other = obj as? WebClient else {
      return false
    }
    return self === other
  }
  
  public func unpack(in context: LispKit.Context) -> Exprs {
    return [.makeString(self.identityString),
            .makeNumber(self.width),
            self.height.expr,
            delay == nil ? .false : .makeNumber(self.delay!)]
  }
  
  private func nextSnapshot() {
    if self.queue.isEmpty {
      self.method = nil
    } else {
      let (source, method) = self.queue.removeFirst()
      self.method = method
      self.webView.frame = CGRect(x: 0, y: 0, width: self.width, height: 0)
      switch source {
        case .html(content: let content, baseURL: let baseURL):
          self.webView.loadHTMLString(content, baseURL: baseURL)
        case .data(data: let data, mimeType: let mime, charEnc: let enc, baseURL: let baseURL):
          self.webView.load(data, mimeType: mime, characterEncodingName: enc, baseURL: baseURL)
        case .file(url: let url, allowReadAccessTo: let allowReadAccessTo):
          self.webView.loadFileURL(url, allowingReadAccessTo: allowReadAccessTo)
        case .url(request: let request):
          self.webView.load(request)
      }
    }
  }
  
  private func configure(source: ContentSource, method: ExportMethod) {
    return Self.onMainThread {
      if self.method == nil {
        self.queue.append((source, method))
        self.nextSnapshot()
      } else {
        self.queue.append((source, method))
      }
    }
  }
  
  public var isBusy: Bool {
    return Self.onMainThread {
      return self.method != nil
    }
  }
  
  public func pdf(source: ContentSource,
                  crop: CropMode = .all,
                  transparent: Bool = true,
                  handler: @escaping (Result<Data, Error>) -> Void) {
    self.configure(source: source,
                   method: .pdf(crop: crop, transparent: transparent, handler: { result in
                     handler(result)
                     self.nextSnapshot()
                   }))
  }
  
  public func image(source: ContentSource,
                    crop: CropMode = .all,
                    width: CGFloat? = nil,
                    height: Correction? = nil,
                    handler: @escaping (Result<NativeImage, Error>) -> Void) {
    self.configure(source: source,
                   method: .image(crop: crop, width: width, height: height, handler: { result in
                     handler(result)
                     self.nextSnapshot()
                   }))
  }
  
  public func webView(_ webView: WKWebView, didFail: WKNavigation!, withError error: any Error) {
    switch self.method {
      case .none:
        break
      case .pdf(crop: _, transparent: _, handler: let completionHandler):
        completionHandler(.failure(error))
      case .image(crop: _, width: _, height: _, handler: let completionHandler):
        completionHandler(.failure(error))
    }
  }
  
  public func webView(_ webView: WKWebView, didCommit navigation: WKNavigation!) {
    // Don't do anything
  }
  
  public func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
    let callback = { (context: Context) in
      Self.doOnMainThread {
        switch self.method {
          case .none:
            break
          case .image(crop: let crop,
                      width: let widthOverride,
                      height: _,
                      handler: let completionHandler):
            switch context {
              case .error(let error):
                completionHandler(.failure(error))
              case .parameters(left: _, top: _, width: _, height: _):
                let snapshotConfig = WKSnapshotConfiguration()
                if case .all = crop {
                  snapshotConfig.rect = .null
                } else {
                  snapshotConfig.rect = crop.rect(in: webView, context: context)
                }
                snapshotConfig.afterScreenUpdates = true
                if let widthOverride {
                  snapshotConfig.snapshotWidth = NSNumber(value: max(0.0, widthOverride).native)
                }
                webView.takeSnapshot(with: snapshotConfig) { image, error in
                  if let image {
                    completionHandler(.success(NativeImage(image)))
                  } else {
                    completionHandler(.failure(error ?? ExportError.noResponse))
                  }
                }
            }
          case .pdf(crop: let crop, transparent: _, handler: let completionHandler):
            switch context {
              case .error(let error):
                completionHandler(.failure(error))
              case .parameters(left: _, top: _, width: _, height: _):
                let pdfConfig = WKPDFConfiguration()
                if case .all = crop {
                  pdfConfig.rect = nil
                } else {
                  pdfConfig.rect = crop.rect(in: webView, context: context)
                }
                webView.createPDF(configuration: pdfConfig, completionHandler: completionHandler)
            }
        }
      }
    }
    self.executeDelayed {
      webView.evaluateJavaScript("document.readyState", completionHandler: { (complete, error) in
        if complete != nil {
          let dimensions = PageDimensions()
          webView.evaluateJavaScript("document.body.children[0].offsetTop",
                                     completionHandler: dimensions.setTop)
          webView.evaluateJavaScript("document.body.children[0].offsetLeft",
                                     completionHandler: dimensions.setLeft)
          webView.evaluateJavaScript("document.body.scrollWidth",
                                     completionHandler: dimensions.setWidth)
          webView.evaluateJavaScript("document.body.scrollHeight",
                                     completionHandler: { (height, error) in
            do {
              if let ht = height as? CGFloat {
                let hc = self.height.compute(detected: ht)
                let h = self.method?.height(detected: ht, computed: hc) ?? hc
                let (left, top, width) = try dimensions.dimensions
                webView.frame = CGRect(x: 0, y: 0, width: webView.frame.width, height: h + 2 * top)
                callback(.parameters(left: left, top: top, width: width, height: h))
              } else if case .absolute(let hc) = self.height {
                let h = self.method?.height(detected: hc, computed: hc) ?? hc
                let (left, top, width) = try dimensions.dimensions
                webView.frame = CGRect(x: 0, y: 0, width: webView.frame.width, height: h + 2 * top)
                callback(.parameters(left: left, top: top, width: width, height: h))
              } else {
                callback(.error(error ?? ExportError.noResponse))
              }
            } catch let error {
              callback(.error(error))
            }
          })
        } else {
          callback(.error(error ?? ExportError.noResponse))
        }
      })
    }
  }
  
  private func executeDelayed(_ execute: @escaping () -> Void) {
    if let delay {
      DispatchQueue.main.asyncAfter(deadline: .now() + delay, execute: execute)
    } else {
      execute()
    }
  }
  
  private static func doOnMainThread(_ execute: @escaping () -> Void) {
    if Thread.isMainThread {
      execute()
    } else {
      DispatchQueue.main.async {
        execute()
      }
    }
  }
  
  private static func onMainThread<T>(_ execute: @escaping () -> T) -> T {
    if Thread.isMainThread {
      return execute()
    } else {
      return DispatchQueue.main.sync {
        return execute()
      }
    }
  }
}

internal class PageDimensions: CustomStringConvertible {
  private let condition: NSCondition = NSCondition()
  private var leftValue: CGFloat? = nil
  private var topValue: CGFloat? = nil
  private var widthValue: CGFloat? = nil
  private var errorValue: (any Error)? = nil
  
  internal var dimensions: (left: CGFloat, top: CGFloat, width: CGFloat) {
    get throws {
      self.condition.lock()
      defer { self.condition.unlock() }
      while (((self.widthValue == nil) || (self.leftValue == nil) || (self.topValue == nil)) &&
             (self.errorValue == nil)) {
        self.condition.wait()
      }
      if let errorValue {
        throw errorValue
      }
      return (left: self.leftValue!, top: self.topValue!, width: self.widthValue!)
    }
  }
  
  internal func setLeft(_ newValue: Any, _ error: (any Error)?) {
    self.condition.lock()
    defer { self.condition.unlock() }
    if let value = newValue as? CGFloat {
      self.leftValue = value
    } else {
      self.errorValue = error ?? WebClient.ExportError.noResponse
    }
    self.condition.broadcast()
  }
  
  internal func setTop(_ newValue: Any, _ error: (any Error)?) {
    self.condition.lock()
    defer { self.condition.unlock() }
    if let value = newValue as? CGFloat {
      self.topValue = value
    } else {
      self.errorValue = error ?? WebClient.ExportError.noResponse
    }
    self.condition.broadcast()
  }
  
  internal func setWidth(_ newValue: Any, _ error: (any Error)?) {
    self.condition.lock()
    defer { self.condition.unlock() }
    if let value = newValue as? CGFloat {
      self.widthValue = value
    } else {
      self.errorValue = error ?? WebClient.ExportError.noResponse
    }
    self.condition.broadcast()
  }
  
  var description: String {
    return "{ left = \(leftValue ?? -1.0), top = \(topValue ?? -1.0)" +
           ", width = \(widthValue ?? -1.0), error = \(errorValue?.localizedDescription ?? "none")}"
  }
}
