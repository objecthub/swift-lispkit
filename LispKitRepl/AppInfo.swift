//
//  AppInfo.swift
//  LispKit
//
//  Created by Matthias Zenger on 05/05/2016.
//  Copyright © 2016 ObjectHub. All rights reserved.
//

import Foundation

///
/// Struct `AppInfo` provides meta-information on the application and build timing.
///
public struct AppInfo {
  public let name = NSBundle.mainBundle().infoDictionary!["CFBundleName"] as! String
  public let version =
      NSBundle.mainBundle().infoDictionary!["CFBundleShortVersionString"] as! String
  public let copyright =
      NSBundle.mainBundle().infoDictionary!["NSHumanReadableCopyright"] as! String
  public let buildDate = { () -> String in
      let dateFormatter = NSDateFormatter()
      dateFormatter.dateFormat = "yyyy-MM-dd"
      return dateFormatter.stringFromDate(getBuildDate())
    }()
  public let buildTime = getBuildTime()
}
