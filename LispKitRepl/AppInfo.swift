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
  public static let name = NSBundle.mainBundle().infoDictionary!["CFBundleName"] as! String
  public static let version =
      NSBundle.mainBundle().infoDictionary!["CFBundleShortVersionString"] as! String
  public static let copyright =
      NSBundle.mainBundle().infoDictionary!["NSHumanReadableCopyright"] as! String
  public static let buildDate = { () -> String in
      let dateFormatter = NSDateFormatter()
      dateFormatter.dateFormat = "yyyy-MM-dd"
      return dateFormatter.stringFromDate(getBuildDate())
    }()
  public static let buildTime = getBuildTime()
}
