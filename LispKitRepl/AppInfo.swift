//
//  AppInfo.swift
//  LispKit
//
//  Created by Matthias Zenger on 05/05/2016.
//  Copyright © 2016 ObjectHub. All rights reserved.
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
  public static let name = Bundle.main.infoDictionary!["CFBundleName"] as! String
  
  // Version of the application
  public static let version =
      Bundle.main.infoDictionary!["CFBundleShortVersionString"] as! String
  
  // Copyright message
  public static let copyright =
      Bundle.main.infoDictionary!["NSHumanReadableCopyright"] as! String
  
  // Build date
  public static let buildDate = { () -> String in
      let dateFormatter = DateFormatter()
      dateFormatter.dateFormat = "yyyy-MM-dd"
      return dateFormatter.string(from: getBuildDate())
    }()
  
  // Build time
  public static let buildTime = getBuildTime() ?? ""
}
