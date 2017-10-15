//
//  Feature.swift
//  LispKit
//
//  Created by Matthias Zenger on 15/10/2017.
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

public enum Feature: String {
  case lispkit = "lispkit"
  case r7rs = "r7rs"
  case ratios = "ratios"
  case complex = "complex"
  case syntaxRules = "syntax-rules"
  case littleEndian = "little-endian"
  case bigEndian = "big-endian"
  case dynamicLoading = "dynamic-loading"
  case modules = "modules"
  case bit32 = "32bit"
  case bit64 = "64bit"
  case macos = "macos"
  case macosx = "macosx"
  case ios = "ios"
  case linux = "linux"
  case i386 = "i386"
  case x8664 = "x86-64"
  case arm64 = "arm64"
  case arm = "arm"
  
  public static func isSupported(_ featureName: String) -> Bool {
    return Feature(rawValue: featureName) != nil
  }
  
  public static let supported: Set<Feature> = {
    var set = Set<Feature>()
    set.insert(.lispkit)
    set.insert(.r7rs)
    set.insert(.ratios)
    set.insert(.complex)
    set.insert(.syntaxRules)
    set.insert(.dynamicLoading)
    set.insert(.modules)
    set.insert(CFByteOrderGetCurrent() == 1 ? .littleEndian : .bigEndian)
    #if arch(i386)
      set.insert(.i386)
      set.insert(.bit32)
    #elseif arch(x86_64)
      set.insert(.x8664)
      set.insert(.bit64)
    #elseif arch(arm)
      set.insert(.arm)
      set.insert(.bit32)
    #elseif arch(arm64)
      set.insert(.arm64)
      set.insert(.bit64)
    #endif
    #if os(macOS)
      set.insert(.macos)
      set.insert(.macosx)
    #elseif os(iOS)
      set.insert(.ios)
    #elseif os(Linux)
      set.insert(.linux)
    #endif
    return set
  }()
}
