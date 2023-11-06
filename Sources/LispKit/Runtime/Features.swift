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
#if os(iOS) || os(watchOS) || os(tvOS)
import UIKit
#endif

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
  case threads = "threads"
  case bit32 = "32bit"
  case bit64 = "64bit"
  case macos = "macos"
  case macosx = "macosx"
  case ios = "ios"
  case ipados = "ipados"
  case linux = "linux"
  case i386 = "i386"
  case x8664 = "x86-64"
  case arm64 = "arm64"
  case arm = "arm"
  
  public static let supported: Set<String> = {
    var set = Set<String>()
    set.insert(Feature.lispkit.rawValue)
    set.insert(Feature.r7rs.rawValue)
    set.insert(Feature.ratios.rawValue)
    set.insert(Feature.complex.rawValue)
    set.insert(Feature.syntaxRules.rawValue)
    set.insert(Feature.dynamicLoading.rawValue)
    set.insert(Feature.modules.rawValue)
    set.insert(Feature.threads.rawValue)
    set.insert(CFByteOrderGetCurrent() == 1 ? Feature.littleEndian.rawValue
                                            : Feature.bigEndian.rawValue)
    #if arch(i386)
      set.insert(Feature.i386.rawValue)
      set.insert(Feature.bit32.rawValue)
    #elseif arch(x86_64)
      set.insert(Feature.x8664.rawValue)
      set.insert(Feature.bit64.rawValue)
    #elseif arch(arm)
      set.insert(Feature.arm.rawValue)
      set.insert(Feature.bit32.rawValue)
    #elseif arch(arm64)
      set.insert(Feature.arm64.rawValue)
      set.insert(Feature.bit64.rawValue)
    #endif
    #if os(macOS)
      set.insert(Feature.macos.rawValue)
      set.insert(Feature.macosx.rawValue)
    #elseif os(iOS)
      if UIDevice.current.userInterfaceIdiom == .pad {
        set.insert(Feature.ipados.rawValue)
      } else {
        set.insert(Feature.ios.rawValue)
      }
    #elseif os(Linux)
      set.insert(Feature.linux.rawValue)
    #endif
    return set
  }()
}
