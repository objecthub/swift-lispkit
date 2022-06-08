//
//  SystemLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 10/12/2016.
//  Copyright © 2016-2022 ObjectHub. All rights reserved.
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

#if os(iOS) || os(watchOS) || os(tvOS)
import UIKit
#elseif os(macOS)
import Cocoa
#endif

///
/// System library: LispKit-specific library providing access to operation system-level
/// functionality
///
public final class SystemLibrary: NativeLibrary {
  
  /// Set of all available locales.
  private let locales = Set<String>(Locale.availableIdentifiers)

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "system"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define("current-directory", as: self.context.evaluator.currentDirectoryProc)
    self.define(SpecialForm("source-directory", self.compileSourceDirectory))
    self.define(Procedure("home-directory", self.homeDirectory))
    self.define(Procedure("system-directory", self.systemDirectory))
    self.define(Procedure("path", self.path))
    self.define(Procedure("parent-path", self.parentPath))
    self.define(Procedure("path-extension", self.pathExtension))
    self.define(Procedure("append-path-extension", self.appendPathExtension))
    self.define(Procedure("remove-path-extension", self.removePathExtension))
    self.define(Procedure("path-components", self.pathComponents))
    self.define(Procedure("file-path", self.filePath))
    self.define(Procedure("asset-file-path", self.assetFilePath))
    self.define(Procedure("parent-file-path", self.parentFilePath))
    self.define(Procedure("file-path-root?", self.filePathRoot))
    self.define(Procedure("file-exists?", self.fileExists))
    self.define(Procedure("file-readable?", self.fileReadable))
    self.define(Procedure("file-writable?", self.fileWritable))
    self.define(Procedure("file-deletable?", self.fileDeletable))
    self.define(Procedure("directory-exists?", self.directoryExists))
    self.define(Procedure("directory-readable?", self.directoryReadable))
    self.define(Procedure("directory-writable?", self.directoryWritable))
    self.define(Procedure("directory-deletable?", self.directoryDeletable))
    self.define(Procedure("file-or-directory-exists?", self.fileOrDirectoryExists))
    self.define(Procedure("delete-file", self.deleteFile))
    self.define(Procedure("delete-directory", self.deleteDirectory))
    self.define(Procedure("delete-file-or-directory", self.deleteFileOrDirectory))
    self.define(Procedure("copy-file", self.copyFile))
    self.define(Procedure("copy-directory", self.copyDirectory))
    self.define(Procedure("copy-file-or-directory", self.copyFileOrDirectory))
    self.define(Procedure("move-file", self.copyFile))
    self.define(Procedure("move-directory", self.copyDirectory))
    self.define(Procedure("move-file-or-directory", self.copyFileOrDirectory))
    self.define(Procedure("file-size", self.fileSize))
    self.define(Procedure("directory-list", self.directoryList))
    self.define(Procedure("make-directory", self.makeDirectory))
    self.define(Procedure("open-file", self.openFile))
    self.define(Procedure("get-environment-variable", self.getEnvironmentVariable))
    self.define(Procedure("get-environment-variables", self.getEnvironmentVariables))
    self.define(Procedure("command-line", self.commandLine))
    self.define(Procedure("current-second", self.currentSecond))
    self.define(Procedure("current-jiffy", self.currentJiffy))
    self.define(Procedure("jiffies-per-second", self.jiffiesPerSecond))
    self.define(Procedure("available-regions", self.availableRegions))
    self.define(Procedure("region-name", self.regionName))
    self.define(Procedure("available-languages", self.availableLanguages))
    self.define(Procedure("language-name", self.languageName))
    self.define(Procedure("available-locales", self.availableLocales))
    self.define(Procedure("available-locale?", self.isAvailableLocale))
    self.define(Procedure("locale", self.locale))
    self.define(Procedure("locale-region", self.localeRegion))
    self.define(Procedure("locale-language", self.localeLanguage))
    self.define(Procedure("locale-currency", self.localeCurrency))
    self.define(Procedure("features", self.features))
    self.define(Procedure("implementation-name", self.implementationName))
    self.define(Procedure("implementation-version", self.implementationVersion))
    self.define(Procedure("cpu-architecture", self.cpuArchitecture))
    self.define(Procedure("machine-name", self.machineName))
    self.define(Procedure("machine-model", self.machineModel))
    self.define(Procedure("physical-memory", self.physicalMemory))
    self.define(Procedure("memory-footprint", self.memoryFootprint))
    self.define(Procedure("system-uptime", self.systemUptime))
    self.define(Procedure("os-type", self.osType))
    self.define(Procedure("os-version", self.osVersion))
    self.define(Procedure("os-name", self.osName))
    self.define(Procedure("os-release", self.osRelease))
    self.define(Procedure("host-name", self.hostName))
    self.define(Procedure("current-user-name", self.currentUserName))
    self.define(Procedure("user-data", self.userData))
    self.define(Procedure("open-url", self.openUrl))
    self.define(Procedure("http-get", httpGet))
  }
  
  private func compileSourceDirectory(compiler: Compiler,
                                      expr: Expr,
                                      env: Env,
                                      tail: Bool) throws -> Bool {
    guard case .pair(_, .null) = expr else {
      throw RuntimeError.argumentCount(of: "source-directory", num: 0, expr: expr)
    }
    try compiler.pushValue(.makeString(compiler.sourceDirectory))
    return false
  }
  
  private func homeDirectory(user: Expr?, ignoreSandbox: Expr?) throws -> Expr {
    if (user ?? .false).isTrue,
       let user = try user?.asString() {
      if (ignoreSandbox ?? .false).isTrue,
         let pw = getpwnam(user),
         let home = pw.pointee.pw_dir {
       return .makeString(FileManager.default.string(withFileSystemRepresentation: home,
                                                     length: Int(strlen(home))))
      } else if let path = NSHomeDirectoryForUser(user) {
        return .makeString(path)
      } else {
        return .false
      }
    } else if (ignoreSandbox ?? .false).isTrue,
           let pw = getpwuid(getuid()),
           let home = pw.pointee.pw_dir {
      return .makeString(FileManager.default.string(withFileSystemRepresentation: home,
                                                    length: Int(strlen(home))))
    } else {
      return .makeString(NSHomeDirectory())
    }
  }
  
  private func systemDirectory(type: Expr) throws -> Expr {
    var searchPathDir: FileManager.SearchPathDirectory
    switch try type.asSymbol().rawIdentifier {
      case "desktop":
        searchPathDir = .desktopDirectory
      case "downloads":
        searchPathDir = .downloadsDirectory
      case "movies":
        searchPathDir = .moviesDirectory
      case "music":
        searchPathDir = .musicDirectory
      case "pictures":
        searchPathDir = .picturesDirectory
      case "documents":
        searchPathDir = .documentDirectory
      case "shared-public":
        searchPathDir = .sharedPublicDirectory
      case "application-support":
        searchPathDir = .applicationSupportDirectory
      #if os(macOS)
      case "application-scripts":
        searchPathDir = .applicationScriptsDirectory
      #endif
      case "cache":
        searchPathDir = .cachesDirectory
      case "temporary":
        return .pair(.makeString(NSTemporaryDirectory()), .null)
      case "icloud":
        if let url = FileManager.default.url(forUbiquityContainerIdentifier: nil)?
                                        .appendingPathComponent("Documents")
                                        .resolvingSymlinksInPath() {
          return .pair(.makeString(url.absoluteURL.path), .null)
        }
        return .null
      default:
        throw RuntimeError.eval(.unknownSystemDirectory, type)
    }
    let dirs = NSSearchPathForDirectoriesInDomains(searchPathDir, .userDomainMask, true)
    var res = Expr.null
    for dir in dirs.reversed() {
      res = .pair(.makeString(dir), res)
    }
    return res
  }
  
  private func path(expr: Expr, args: Arguments) throws -> Expr {
    var res = URL(fileURLWithPath: try expr.asPath(), isDirectory: true)
    for arg in args {
      res.appendPathComponent(try arg.asString())
    }
    return .makeString(res.relativePath)
  }

  private func parentPath(expr: Expr) throws -> Expr {
    let base = URL(fileURLWithPath: try expr.asPath())
    return .makeString(base.deletingLastPathComponent().relativePath)
  }

  private func pathExtension(expr: Expr) throws -> Expr {
    let ext = URL(fileURLWithPath: try expr.asPath()).pathExtension
    return ext.isEmpty ? .false : .makeString(ext)
  }

  private func appendPathExtension(expr: Expr, ext: Expr, optionally: Expr?) throws -> Expr {
    let path = try expr.asPath()
    if path.isEmpty {
      return .false
    } else if optionally?.isTrue ?? false {
      let url = URL(fileURLWithPath: path)
      if url.pathExtension.isEmpty {
        return .makeString(url.appendingPathExtension(try ext.asString()).relativePath)
      } else {
        return .makeString(url.relativePath)
      }
    } else {
      let url = URL(fileURLWithPath: path).appendingPathExtension(try ext.asString())
      return .makeString(url.relativePath)
    }
  }

  private func removePathExtension(expr: Expr) throws -> Expr {
    let path = try expr.asPath()
    if path.isEmpty {
      return expr
    } else {
      let url = URL(fileURLWithPath: path).deletingPathExtension()
      return .makeString(url.relativePath)
    }
  }

  private func pathComponents(expr: Expr) throws -> Expr {
    let url = URL(fileURLWithPath: try expr.asPath())
    var res = Expr.null
    for component in url.pathComponents.reversed() {
      res = .pair(.makeString(component), res)
    }
    return res
  }

  private func filePath(expr: Expr, base: Expr?, resolve: Expr?) throws -> Expr {
    var root = self.context.evaluator.currentDirectoryPath
    if (base ?? .false).isTrue,
       let base = try base?.asPath() {
      root = self.context.fileHandler.path(base,
                                           relativeTo: self.context.evaluator.currentDirectoryPath)
    }
    let relativePath = NSString(string: try expr.asString()).expandingTildeInPath
    let path = self.context.fileHandler.path(relativePath,
                                             relativeTo: root,
                                             resolveSymLinks: resolve?.isTrue ?? false)
    return .makeString(path)
  }

  private func assetFilePath(_ expr: Expr, _ type: Expr, _ dir: Expr?) throws -> Expr {
    if let filename = self.context.fileHandler.assetFilePath(
                        forFile: try expr.asString(),
                        ofType: try type.asString(),
                        inFolder: try dir?.asPath(),
                        relativeTo: self.context.evaluator.currentDirectoryPath) {
      return .makeString(filename)
    } else {
      return .false
    }
  }

  private func parentFilePath(expr: Expr) throws -> Expr {
    return .makeString(
      self.context.fileHandler.directory(try expr.asString(),
                                         relativeTo: self.context.evaluator.currentDirectoryPath))
  }

  private func filePathRoot(expr: Expr) throws -> Expr {
    return .makeBoolean(
      self.context.fileHandler.path(try expr.asString(),
                                    relativeTo: self.context.evaluator.currentDirectoryPath) == "/")
  }
  
  private func fileExists(expr: Expr) throws -> Expr {
    return .makeBoolean(
      self.context.fileHandler.isFile(atPath: try expr.asPath(),
                                      relativeTo: self.context.evaluator.currentDirectoryPath))
  }

  private func fileReadable(expr: Expr) throws -> Expr {
    let path = try expr.asPath()
    return .makeBoolean(
      self.context.fileHandler.isFile(atPath: path,
                                      relativeTo: self.context.evaluator.currentDirectoryPath) &&
      self.context.fileHandler.itemReadable(atPath: path,
                                            relativeTo: self.context.evaluator.currentDirectoryPath))
  }

  private func fileWritable(expr: Expr) throws -> Expr {
    let path = try expr.asPath()
    return .makeBoolean(
      self.context.fileHandler.isFile(atPath: path,
                                      relativeTo: self.context.evaluator.currentDirectoryPath) &&
      self.context.fileHandler.itemWritable(atPath: path,
                                            relativeTo: self.context.evaluator.currentDirectoryPath))
  }

  private func fileDeletable(expr: Expr) throws -> Expr {
    let path = try expr.asPath()
    return .makeBoolean(
      self.context.fileHandler.isFile(atPath: path,
                                      relativeTo: self.context.evaluator.currentDirectoryPath) &&
      self.context.fileHandler.itemDeletable(atPath: path,
                                             relativeTo: self.context.evaluator.currentDirectoryPath))
  }

  private func directoryExists(expr: Expr) throws -> Expr {
    return .makeBoolean(
      self.context.fileHandler.isDirectory(atPath: try expr.asPath(),
                                           relativeTo: self.context.evaluator.currentDirectoryPath))
  }

  private func directoryReadable(expr: Expr) throws -> Expr {
    let path = try expr.asPath()
    return .makeBoolean(
      self.context.fileHandler.isDirectory(atPath: path,
                                           relativeTo: self.context.evaluator.currentDirectoryPath) &&
      self.context.fileHandler.itemReadable(atPath: path,
                                            relativeTo: self.context.evaluator.currentDirectoryPath))
  }

  private func directoryWritable(expr: Expr) throws -> Expr {
    let path = try expr.asPath()
    return .makeBoolean(
      self.context.fileHandler.isDirectory(atPath: path,
                                           relativeTo: self.context.evaluator.currentDirectoryPath) &&
      self.context.fileHandler.itemWritable(atPath: path,
                                            relativeTo: self.context.evaluator.currentDirectoryPath))
  }

  private func directoryDeletable(expr: Expr) throws -> Expr {
    let path = try expr.asPath()
    return .makeBoolean(
      self.context.fileHandler.isDirectory(atPath: path,
                                           relativeTo: self.context.evaluator.currentDirectoryPath) &&
      self.context.fileHandler.itemDeletable(atPath: path,
                                             relativeTo: self.context.evaluator.currentDirectoryPath))
  }

  private func fileOrDirectoryExists(expr: Expr) throws -> Expr {
    return .makeBoolean(
      self.context.fileHandler.itemExists(atPath: try expr.asPath(),
                                          relativeTo: self.context.evaluator.currentDirectoryPath))
  }

  private func deleteFile(expr: Expr) throws -> Expr {
    let path = try expr.asPath()
    if self.context.fileHandler.isFile(atPath: path,
                                       relativeTo: self.context.evaluator.currentDirectoryPath) {
      try self.context.fileHandler.deleteItem(atPath: path,
                                              relativeTo: self.context.evaluator.currentDirectoryPath)
      return .void
    } else {
      throw RuntimeError.eval(.unknownFile, expr)
    }
  }

  private func deleteDirectory(expr: Expr) throws -> Expr {
    let path = try expr.asPath()
    if self.context.fileHandler.isDirectory(atPath: path,
                                            relativeTo: self.context.evaluator.currentDirectoryPath) {
      try self.context.fileHandler.deleteItem(atPath: path,
                                              relativeTo: self.context.evaluator.currentDirectoryPath)
      return .void
    } else {
      throw RuntimeError.eval(.unknownDirectory, expr)
    }
  }

  private func deleteFileOrDirectory(expr: Expr) throws -> Expr {
    try self.context.fileHandler.deleteItem(atPath: try expr.asPath(),
                                            relativeTo: self.context.evaluator.currentDirectoryPath)
    return .void
  }

  private func copyFile(fromPath: Expr, toPath: Expr) throws -> Expr {
    let path = try fromPath.asPath()
    if self.context.fileHandler.isFile(atPath: path,
                                       relativeTo: self.context.evaluator.currentDirectoryPath) {
      try self.context.fileHandler.copyItem(atPath: path,
                                            toPath: try toPath.asPath(),
                                            relativeTo: self.context.evaluator.currentDirectoryPath)
      return .void
    } else {
      throw RuntimeError.eval(.unknownFile, fromPath)
    }
  }

  private func copyDirectory(fromPath: Expr, toPath: Expr) throws -> Expr {
    let path = try fromPath.asPath()
    if self.context.fileHandler.isDirectory(atPath: path,
                                            relativeTo: self.context.evaluator.currentDirectoryPath) {
      try self.context.fileHandler.copyItem(atPath: path,
                                            toPath: try toPath.asPath(),
                                            relativeTo: self.context.evaluator.currentDirectoryPath)
      return .void
    } else {
      throw RuntimeError.eval(.unknownDirectory, fromPath)
    }
  }


  private func copyFileOrDirectory(fromPath: Expr, toPath: Expr) throws -> Expr {
    try self.context.fileHandler.copyItem(atPath: try fromPath.asPath(),
                                          toPath: try toPath.asPath(),
                                          relativeTo: self.context.evaluator.currentDirectoryPath)
    return .void
  }

  private func moveFile(fromPath: Expr, toPath: Expr) throws -> Expr {
    let path = try fromPath.asPath()
    if self.context.fileHandler.isFile(atPath: path,
                                       relativeTo: self.context.evaluator.currentDirectoryPath) {
      try self.context.fileHandler.moveItem(atPath: path,
                                            toPath: try toPath.asPath(),
                                            relativeTo: self.context.evaluator.currentDirectoryPath)
      return .void
    } else {
      throw RuntimeError.eval(.unknownFile, fromPath)
    }
  }

  private func moveDirectory(fromPath: Expr, toPath: Expr) throws -> Expr {
    let path = try fromPath.asPath()
    if self.context.fileHandler.isDirectory(atPath: path,
                                            relativeTo: self.context.evaluator.currentDirectoryPath) {
      try self.context.fileHandler.moveItem(atPath: path,
                                            toPath: try toPath.asPath(),
                                            relativeTo: self.context.evaluator.currentDirectoryPath)
      return .void
    } else {
      throw RuntimeError.eval(.unknownDirectory, fromPath)
    }
  }


  private func moveFileOrDirectory(fromPath: Expr, toPath: Expr) throws -> Expr {
    try self.context.fileHandler.moveItem(atPath: try fromPath.asPath(),
                                          toPath: try toPath.asPath(),
                                          relativeTo: self.context.evaluator.currentDirectoryPath)
    return .void
  }

  private func fileSize(expr: Expr) throws -> Expr {
    guard let size = self.context.fileHandler.fileSize(
                       atPath: try expr.asPath(),
                       relativeTo: self.context.evaluator.currentDirectoryPath) else {
      throw RuntimeError.eval(.unknownFile, expr)
    }
    return .fixnum(size)
  }
  
  private func directoryList(expr: Expr) throws -> Expr {
    let contents = try self.context.fileHandler.contentsOfDirectory(
      atPath: try expr.asPath(), relativeTo: self.context.evaluator.currentDirectoryPath)
    var res = Expr.null
    for item in contents {
      res = .pair(.makeString(item), res)
    }
    return res
  }

  private func makeDirectory(expr: Expr) throws -> Expr {
    try self.context.fileHandler.makeDirectory(
      atPath: try expr.asPath(), relativeTo: self.context.evaluator.currentDirectoryPath)
    return .void
  }

  private func openFile(expr: Expr, withApp: Expr?, deactivate: Expr?) throws -> Expr {
    let path = self.context.fileHandler.path(try expr.asPath(),
                                             relativeTo: self.context.evaluator.currentDirectoryPath)
    if let app = withApp {
      #if os(iOS) || os(watchOS) || os(tvOS)
      return .false
      #elseif os(macOS)
      if let deactivate = deactivate {
        return .makeBoolean(NSWorkspace.shared.openFile(path,
                                                        withApplication: try app.asString(),
                                                        andDeactivate: deactivate.isTrue))
      } else {
        return .makeBoolean(NSWorkspace.shared.openFile(path,
                                                        withApplication: try app.asString()))
      }
      #endif
    } else {
      #if os(iOS) || os(watchOS) || os(tvOS)
      if path.starts(with: "/var/"),  // This is a hack! If I just would know how to avoid it...
         let url = URL(string: "shareddocuments:///private\(path)"),
         UIApplication.shared.canOpenURL(url) {
        DispatchQueue.main.async {
          UIApplication.shared.open(url)
        }
        return .true
      } else if let url = URL(string: "shareddocuments://\(path)"),
                UIApplication.shared.canOpenURL(url) {
        DispatchQueue.main.async {
          UIApplication.shared.open(url)
        }
        return .true
      } else {
        return .false
      }
      #elseif os(macOS)
      return .makeBoolean(NSWorkspace.shared.openFile(path))
      #endif
    }
  }
  
  private func getEnvironmentVariable(expr: Expr) throws -> Expr {
    let name = try expr.asString()
    guard let value = ProcessInfo.processInfo.environment[name] else {
      return .false
    }
    return .makeString(value)
  }

  private func getEnvironmentVariables() -> Expr {
    var alist = Expr.null
    for (name, value) in ProcessInfo.processInfo.environment {
      alist = .pair(.pair(.makeString(name), .makeString(value)), alist)
    }
    return alist
  }

  private func commandLine() -> Expr {
    let args = self.context.commandLineArguments.reversed()
    var res = Expr.null
    for arg in args {
      res = .pair(.makeString(arg), res)
    }
    return res
  }

  private func currentSecond() -> Expr {
    return .flonum(Timer.currentTimeInSec)
  }

  private func currentJiffy() -> Expr {
    var time = timeval(tv_sec: 0, tv_usec: 0)
    gettimeofday(&time, nil)
    return .fixnum(Int64(time.tv_sec) * 1000 + Int64(time.tv_usec / 1000))
  }

  private func jiffiesPerSecond() -> Expr {
    return .fixnum(1000)
  }

  private func availableRegions() -> Expr {
    var res = Expr.null
    for reg in Locale.isoRegionCodes.reversed() {
      res = .pair(.makeString(reg), res)
    }
    return res
  }

  private func regionName(_ expr: Expr, _ locale: Expr?) throws -> Expr {
    let region = try expr.asString()
    guard let name = try self.asLocale(locale).localizedString(forRegionCode: region) else {
      return .false
    }
    return .makeString(name)
  }

  private func availableLanguages() -> Expr {
    var res = Expr.null
    for lang in Locale.isoLanguageCodes.reversed() {
      res = .pair(.makeString(lang), res)
    }
    return res
  }

  private func languageName(_ expr: Expr, _ locale: Expr?) throws -> Expr {
    let lang = try expr.asString()
    guard let name = try self.asLocale(locale).localizedString(forLanguageCode: lang) else {
      return .false
    }
    return .makeString(name)
  }

  private func availableLocales() -> Expr {
    var res = Expr.null
    for locale in self.locales.sorted().reversed() {
      res = .pair(.symbol(self.context.symbols.intern(locale)), res)
    }
    return res
  }

  private func isAvailableLocale(_ obj: Expr) throws -> Expr {
    return .makeBoolean(self.locales.contains(try obj.asSymbol().identifier))
  }

  private func locale(_ language: Expr?, _ country: Expr?) throws -> Expr {
    guard let lang = language else {
      return .symbol(self.context.symbols.intern(Locale.current.identifier))
    }
    var components: [String : String] = ["kCFLocaleLanguageCodeKey" : try lang.asString()]
    if let country = country {
      components["kCFLocaleCountryCodeKey"] = try country.asString()
    }
    return .symbol(self.context.symbols.intern(Locale.identifier(fromComponents: components)))
  }

  private func localeRegion(_ expr: Expr) throws -> Expr {
    guard let region = Locale(identifier: try expr.asSymbol().identifier).regionCode else {
      return .false
    }
    return .makeString(region)
  }

  private func localeLanguage(_ expr: Expr) throws -> Expr {
    guard let language = Locale(identifier: try expr.asSymbol().identifier).languageCode else {
      return .false
    }
    return .makeString(language)
  }

  private func localeCurrency(_ expr: Expr) throws -> Expr {
    guard let currency = Locale(identifier: try expr.asSymbol().identifier).currencyCode else {
      return .false
    }
    return .makeString(currency)
  }

  private func features() -> Expr {
    var res: Expr = .null
    for feature in self.context.features {
      res = .pair(.symbol(self.context.symbols.intern(feature)), res)
    }
    return res
  }

  private func implementationName() -> Expr {
    if let name = self.context.implementationName {
      return .makeString(name)
    } else {
      return .false
    }
  }

  private func implementationVersion() -> Expr {
    if let version = self.context.implementationVersion {
      return .makeString(version)
    } else {
      return .false
    }
  }

  private func cpuArchitecture() -> Expr {
    return .makeString(Sysctl.machine)
  }

  private func machineName() -> Expr {
    return .makeString(Sysctl.hostName)
  }

  private func machineModel() -> Expr {
    return .makeString(Sysctl.model)
  }
  
  private func physicalMemory() -> Expr {
    return .makeNumber(ProcessInfo.processInfo.physicalMemory)
  }
  
  private func memoryFootprint() -> Expr {
    let TASK_VM_INFO_COUNT = mach_msg_type_number_t(MemoryLayout<task_vm_info_data_t>.size /
                                                    MemoryLayout<integer_t>.size)
    let TASK_VM_INFO_REV1_COUNT = mach_msg_type_number_t(MemoryLayout.offset(of:
                          \task_vm_info_data_t.min_address)! / MemoryLayout<integer_t>.size)
    var info = task_vm_info_data_t()
    var count = TASK_VM_INFO_COUNT
    let kr = withUnsafeMutablePointer(to: &info) { infoPtr in
      infoPtr.withMemoryRebound(to: integer_t.self, capacity: Int(count)) { intPtr in
        task_info(mach_task_self_, task_flavor_t(TASK_VM_INFO), intPtr, &count)
      }
    }
    guard kr == KERN_SUCCESS, count >= TASK_VM_INFO_REV1_COUNT else {
      return .false
    }
    return .makeNumber(UInt64(info.phys_footprint)) // returns number of bytes
  }
  
  private func systemUptime() -> Expr {
    return .makeNumber(ProcessInfo.processInfo.systemUptime)
  }

  private func osType() -> Expr {
    return .makeString(Sysctl.osType)
  }

  private func osVersion() -> Expr {
    return .makeString(Sysctl.osVersion)
  }

  private func osName() -> Expr {
    #if os(macOS)
      return .makeString("macOS")
    #elseif os(iOS)
      return .makeString("iOS")
    #elseif os(Linux)
      return .makeString("Linux")
    #endif
  }

  private func osRelease() -> Expr {
    return .makeString("\(ProcessInfo.processInfo.operatingSystemVersion.majorVersion)." +
                       "\(ProcessInfo.processInfo.operatingSystemVersion.minorVersion)")
  }
  
  private func hostName() -> Expr {
    return .makeString(ProcessInfo.processInfo.hostName)
  }
  
  private func currentUserName(full: Expr?) -> Expr {
    if full?.isTrue ?? false {
      return .makeString(NSFullUserName())
    } else {
      return .makeString(NSUserName())
    }
  }

  private func userData(_ expr: Expr) throws -> Expr {
    let username = try expr.asString()
    guard let pw = getpwnam(username) else {
      return .false
    }
    let uid = pw.pointee.pw_uid
    let gid = pw.pointee.pw_gid
    let name = String(cString: pw.pointee.pw_name)
    let dir = NSHomeDirectoryForUser(username) ?? String(cString: pw.pointee.pw_dir)
    let gecos = String(cString: pw.pointee.pw_gecos)
    let shell = String(cString: pw.pointee.pw_shell)
    return .pair(.makeNumber(Int64(uid)),
                 .pair(.makeNumber(Int64(gid)),
                       .pair(.makeString(name),
                             .pair(.makeString(gecos),
                                   .pair(.makeString(dir),
                                         .pair(.makeString(shell), .null))))))
  }

  private func openUrl(_ expr: Expr) throws -> Expr {
    #if os(iOS) || os(watchOS) || os(tvOS)
    DispatchQueue.main.async {
      do {
        UIApplication.shared.open(try expr.asURL())
      } catch {
      }
    }
    return .true
    #elseif os(macOS)
    return .makeBoolean(NSWorkspace.shared.open(try expr.asURL()))
    #endif
  }
  
  private func httpGet(_ expr: Expr, _ tout: Expr?) throws -> Expr {
    let url = try expr.asURL()
    let timeout = try tout?.asDouble(coerce: true) ?? HTTPInputStream.defaultTimeout
    guard let stream = HTTPInputStream(url: url) else {
      throw RuntimeError.eval(.cannotOpenUrl, .makeString(url.description))
    }
    stream.openConnection(timeout: timeout)
    stream.waitForResponse()
    guard let input = BinaryInput(source: stream,
                                  url: url,
                                  abortionCallback: self.context.evaluator.isAbortionRequested) else {
      throw RuntimeError.eval(.cannotOpenUrl, .makeString(url.description))
    }
    var response = Expr.null
    guard let headerFields = stream.headerFields else {
      throw RuntimeError.eval(.cannotOpenUrl, .makeString(url.description))
    }
    for (key, value) in headerFields {
      response = .pair(.pair(.makeString(key), .makeString(value)), response)
    }
    stream.waitForData()
    let bytes: [UInt8]
    if let bs = input.readMany(Int.max) {
      bytes = bs
    } else {
      bytes = []
    }
    return .values(.pair(response, .pair(.bytes(MutableBox(bytes)), .null)))
  }

  private func asLocale(_ expr: Expr?) throws -> Locale {
    guard let locale = expr else {
      return Locale.current
    }
    return Locale(identifier: try locale.asSymbol().identifier)
  }
}
