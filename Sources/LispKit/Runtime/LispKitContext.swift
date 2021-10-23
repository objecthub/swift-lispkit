//
//  LispKitContext.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/10/2021.
//  Copyright © 2021 ObjectHub. All rights reserved.
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

public final class LispKitContext: Context {
  
  /// Bundle of the LispKit module
  public static let bundle = Bundle(identifier: "net.objecthub.LispKit")
  
  /// Name of the LispKit implementation
  public static let implementationName =
    LispKitContext.bundle?.infoDictionary?["CFBundleDisplayName"] as? String
  
  /// Version of the LispKit implementation
  public static let implementationVersion =
    LispKitContext.bundle?.infoDictionary?["CFBundleShortVersionString"] as? String
  
  /// LispKit root directory in the bundle
  public static let rootDirectory = "Root/LispKit"
  
  /// Path to default prelude file. Set it to the prelude provided by the bundle, if this exists,
  /// or fall back to the LispKit directory contained in the Documents folder.
  public static let defaultPreludePath =
    LispKitContext.bundle?.path(forResource: "Prelude",
                                ofType: "scm",
                                inDirectory: LispKitContext.rootDirectory) ??
      URL(fileURLWithPath: "LispKit/Prelude.scm",
          relativeTo: URL(fileURLWithPath: NSSearchPathForDirectoriesInDomains(
                                             .documentDirectory,
                                             .userDomainMask,
                                             true)[0])).absoluteURL.path
  
  public override init(delegate: ContextDelegate,
                       implementationName: String? = nil,
                       implementationVersion: String? = nil,
                       commandLineArguments: [String]? = nil,
                       initialHomePath: String? = nil,
                       includeInternalResources: Bool = true,
                       includeDocumentPath: String? = "LispKit",
                       assetPath: String? = nil,
                       gcDelay: Double = 5.0,
                       features: [String] = []) {
    super.init(delegate: delegate,
               implementationName: implementationName ?? LispKitContext.implementationName,
               implementationVersion: implementationVersion ?? LispKitContext.implementationVersion,
               commandLineArguments: commandLineArguments ?? CommandLine.arguments,
               initialHomePath: initialHomePath,
               includeInternalResources: includeInternalResources,
               includeDocumentPath: includeDocumentPath,
               assetPath: assetPath,
               gcDelay: gcDelay,
               features: features)
  }
}
