//
//  LispKitReplOAuthConfig.swift
//  LispKit
//
//  Created by Matthias Zenger on 08/07/2024.
//  Copyright © 2024 ObjectHub. All rights reserved.
//

import OAuth2
import LispKit
import UIKit

public final class LispKitReplOAuthConfig: HTTPOAuthConfig {
  
  public init(context: Context) {
  }
  
  public func configureEmbeddedAuth(oauth2: OAuth2) {
    oauth2.authConfig.authorizeEmbedded = true
    oauth2.authConfig.ui.useSafariView = false
    oauth2.authConfig.ui.useAuthenticationSession = true
    if Thread.isMainThread {
      oauth2.authConfig.authorizeContext = UIApplication.shared.keyWindowPresentedController
    } else {
      DispatchQueue.main.sync {
        oauth2.authConfig.authorizeContext = UIApplication.shared.keyWindowPresentedController
      }
    }
  }
  
  public func createLogger(level: OAuth2LogLevel) -> OAuth2Logger {
    return OAuth2DebugLogger(level)
  }
}
