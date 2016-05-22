//
//  AppInfo.swift
//  LispKit
//
//  Created by Matthias Zenger on 05/05/2016.
//  Copyright © 2016 ObjectHub. All rights reserved.
//

import Foundation

///
/// Struct `AppInfo` provides meta-information on the application and build-related
/// information.
///
public struct AppInfo {
  
  // Name of the application
  public static let name = NSBundle.mainBundle().infoDictionary!["CFBundleName"] as! String
  
  // Version of the application
  public static let version =
      NSBundle.mainBundle().infoDictionary!["CFBundleShortVersionString"] as! String
  
  // Copyright message
  public static let copyright =
      NSBundle.mainBundle().infoDictionary!["NSHumanReadableCopyright"] as! String
  
  // Build date
  public static let buildDate = { () -> String in
      let dateFormatter = NSDateFormatter()
      dateFormatter.dateFormat = "yyyy-MM-dd"
      return dateFormatter.stringFromDate(getBuildDate())
    }()
  
  // Build time
  public static let buildTime = getBuildTime()
}
