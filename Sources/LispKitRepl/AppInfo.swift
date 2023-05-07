//
//  AppInfo.swift
//  LispKit
//
//  Created by Matthias Zenger on 05/05/2016.
//  Copyright © 2016-2023 ObjectHub. All rights reserved.
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
/// Struct `AppInfo` provides meta-information on the application and build-related
/// information.
///
public struct AppInfo {
  
  // Name of the application
  public static let name = "LispKit Shell"
  
  // Version of the application
  public static let version =
    (Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String) ??
    "2.3.2"
  
  // Copyright message
  public static let copyright =
    (Bundle.main.infoDictionary?["NSHumanReadableCopyright"] as? String) ??
    "Copyright © 2016–2023 Matthias Zenger. All rights reserved."
  
  #if SPM
    public static let prompt = "> "
  #else
    public static let prompt = "➤ "
  #endif
  
  // Build date/time
  #if SPM
    public static let buildDate = "2023"
    public static let buildTime = "?"
    public static let buildAnnotation = ""
  #else
    public static let buildDate = { () -> String in
      let dateFormatter = DateFormatter()
      dateFormatter.dateFormat = "yyyy-MM-dd"
      return dateFormatter.string(from: getBuildDate())
    }()
    public static let buildTime = getBuildTime() ?? ""
    public static let buildAnnotation = " (\(AppInfo.buildDate) \(AppInfo.buildTime))"
  #endif
}
